/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2002  Guido Masarotto and Brian Ripley
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

/*--- Device Driver for Windows; this file started from
 *  ../unix/X11/devX11.c --
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Graphics.h>
#include <Rdevices.h>
#include <stdio.h>
#include "opt.h"
#include "graphapp/ga.h"
#include "graphapp/stdimg.h"
#include "console.h"
#include "rui.h"
#include "devga.h"		/* 'Public' routines from here */
#include "windows.h"

extern console RConsole;
extern Rboolean AllDevicesKilled;

/* a colour used to represent the background on png if transparent
   NB: used as RGB and BGR
*/
#define PNG_TRANS 0xd6d3d6

/* these really are globals: per machine, not per window */
static double user_xpinch = 0.0, user_ypinch = 0.0;

void GAsetunits(double xpinch, double ypinch)
{
    user_xpinch = xpinch;
    user_ypinch = ypinch;
}

static rgb GArgb(int color, double gamma)
{
    int r, g, b;
    if (gamma != 1) {
	r = (int) (255 * pow(R_RED(color) / 255.0, gamma));
	g = (int) (255 * pow(R_GREEN(color) / 255.0, gamma));
	b = (int) (255 * pow(R_BLUE(color) / 255.0, gamma));
    } else {
	r = R_RED(color);
	g = R_GREEN(color);
	b = R_BLUE(color);
    }
    return rgb(r, g, b);
}



	/********************************************************/
	/* This device driver has been documented so that it be	*/
	/* used as a template for new drivers 			*/
	/********************************************************/

#define MM_PER_INCH	25.4	/* mm -> inch conversion */

#define TRACEDEVGA(a)
#define NOBM(a) if(xd->kind==SCREEN){a;}
#define CLIP if (xd->clip.width>0) gsetcliprect(_d,xd->clip)
#define DRAW(a) {drawing _d=xd->gawin;CLIP;a;NOBM(_d=xd->bm;CLIP;a;)}
#define SHOW  gbitblt(xd->gawin,xd->bm,pt(0,0),getrect(xd->bm));

#define SF 20  /* scrollbar resolution */

        /********************************************************/
	/* Each driver can have its own device-specic graphical */
	/* parameters and resources.  these should be wrapped	*/
	/* in a structure (like the gadesc structure below)    */
	/* and attached to the overall device description via 	*/
	/* the dd->deviceSpecific pointer			*/
	/* NOTE that there are generic graphical parameters	*/
	/* which must be set by the device driver, but are	*/
	/* common to all device types (see Graphics.h)		*/
	/* so go in the GPar structure rather than this device- */
	/* specific structure					*/
	/********************************************************/

enum DeviceKinds {SCREEN=0, PRINTER, METAFILE, PNG, JPEG, BMP};

typedef struct {
    /* R Graphics Parameters */
    /* local device copy so that we can detect */
    /* when parameter changes */
    int   col;			   /* Color */
    int   bg;			   /* Background */
    int   fontface;		   /* Typeface */
    int   fontsize, basefontsize;  /* Size in points */
    double fontangle;

    /* devga Driver Specific */
    /* parameters with copy per devga device */

    enum DeviceKinds kind;
    int   windowWidth;		/* Window width (pixels) */
    int   windowHeight;		/* Window height (pixels) */
    int   showWidth;		/* device width (pixels) */
    int   showHeight;		/* device height (pixels) */
    int   origWidth, origHeight, xshift, yshift;
    Rboolean resize;		/* Window resized */
    window gawin;		/* Graphics window */
  /*FIXME: we should have union for this stuff and
    maybe change gawin to canvas*/
  /* SCREEN section*/
    popup locpopup, grpopup;
    button  stoploc;
    menubar mbar, mbarloc;
    menu  msubsave;
    menuitem mpng, mbmp, mjpeg50, mjpeg75, mjpeg100;
    menuitem mps, mpdf, mwm, mclpbm, mclpwm, mprint, mclose;
    menuitem mrec, madd, mreplace, mprev, mnext, mclear, msvar, mgvar;
    menuitem mR, mfit, mfix;
    Rboolean recording, replaying, needsave;
    bitmap bm;
  /* PNG and JPEG section */
    FILE *fp;
    char filename[512];
    int quality;
    int npage;
    double w, h;
  /* Used to rescale font size so that bitmap devices have 72dpi */
    int truedpi, wanteddpi;
    rgb   fgcolor;		/* Foreground color */
    rgb   bgcolor;		/* Background color */
    rgb   canvascolor;		/* Canvas color */
    rgb   outcolor;		/* Outside canvas color */
    rect  clip;			/* The clipping rectangle */
    Rboolean usefixed;
    font  fixedfont;
    font  font;
    Rboolean locator;
    int clicked; /* {0,1,2} */
    int	px, py, lty, lwd;
    int resizing; /* {1,2,3} */
    double rescale_factor;
    int fast; /* Use fast fixed-width lines? */
    unsigned int pngtrans; /* what PNG_TRANS get mapped to */
} gadesc;

rect getregion(gadesc *xd)
{
    rect r = getrect(xd->bm);
    r.x += max(0, xd->xshift);
    r.y += max(0, xd->yshift);
    r.width = min(r.width, xd->showWidth);
    r.height = min(r.height, xd->showHeight);
    return r;
}

	/********************************************************/
	/* There are a number of actions that every device 	*/
	/* driver is expected to perform (even if, in some	*/
	/* cases it does nothing - just so long as it doesn't 	*/
	/* crash !).  this is how the graphics engine interacts */
	/* with each device. Each action will be documented 	*/
	/* individually. 					*/
	/* hooks for these actions must be set up when the 	*/
	/* device is first created				*/
	/********************************************************/

	/* Device Driver Actions */

static void GA_Activate(NewDevDesc *dd);
static void GA_Circle(double x, double y, double r,
		      int col, int fill, double gamma, int lty, double lwd,
		      NewDevDesc *dd);
static void GA_Clip(double x0, double x1, double y0, double y1,
		     NewDevDesc *dd);
static void GA_Close(NewDevDesc *dd);
static void GA_Deactivate(NewDevDesc *dd);
static void GA_Hold(NewDevDesc *dd);
static Rboolean GA_Locator(double *x, double *y, NewDevDesc *dd);
static void GA_Line(double x1, double y1, double x2, double y2,
		    int col, double gamma, int lty, double lwd,
		    NewDevDesc *dd);
static void GA_MetricInfo(int c, int font, double cex, double ps,
			  double* ascent, double* descent,
			  double* width, NewDevDesc *dd);
static void GA_Mode(int mode, NewDevDesc *dd);
static void GA_NewPage(int fill, double gamma, NewDevDesc *dd);
static void GA_Polygon(int n, double *x, double *y,
		       int col, int fill, double gamma, int lty, double lwd,
		       NewDevDesc *dd);
static void GA_Polyline(int n, double *x, double *y,
			int col, double gamma, int lty, double lwd,
			NewDevDesc *dd);
static void GA_Rect(double x0, double y0, double x1, double y1,
		    int col, int fill, double gamma, int lty, double lwd,
		    NewDevDesc *dd);
static void GA_Size(double *left, double *right,
		    double *bottom, double *top,
		    NewDevDesc *dd);
static void GA_Resize(NewDevDesc *dd);
static double GA_StrWidth(char *str, int font,
			  double cex, double ps, NewDevDesc *dd);
static void GA_Text(double x, double y, char *str,
		    double rot, double hadj,
		    int col, double gamma, int font, double cex, double ps,
		    NewDevDesc *dd);
static Rboolean GA_Open(NewDevDesc*, gadesc*, char*, double, double,
			Rboolean, int, int, double);

	/********************************************************/
	/* end of list of required device driver actions 	*/
	/********************************************************/

	/* Support Routines */

static double pixelHeight(drawing  d);
static double pixelWidth(drawing d);
static void SetColor(int, double, NewDevDesc *);
static void SetFont(int, int, double, NewDevDesc *);
static void SetLinetype(int, double, NewDevDesc *);
static int Load_Rbitmap_Dll();
void UnLoad_Rbitmap_Dll();
static void SaveAsPng(NewDevDesc *dd, char *fn);
static void SaveAsJpeg(NewDevDesc *dd, int quality, char *fn);
static void SaveAsBmp(NewDevDesc *dd, char *fn);
static void SaveAsBitmap(NewDevDesc *dd);

static void PrivateCopyDevice(NewDevDesc *dd, NewDevDesc *ndd, char *name)
{
    GEDevDesc* gdd;
    int saveDev = curDevice();
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    gsetcursor(xd->gawin, WatchCursor);
    gsetVar(install(".Device"),
	    mkString(name), R_NilValue);
    gdd = GEcreateDevDesc(ndd);
    addDevice((DevDesc*) gdd);
    GEcopyDisplayList(devNumber((DevDesc*) dd));
    KillDevice((DevDesc*) gdd);
    selectDevice(saveDev);
/*    KillDevice(GetDevice(devNumber((DevDesc*) ndd))); */
    gsetcursor(xd->gawin, ArrowCursor);
    show(xd->gawin);
}

static void SaveAsWin(NewDevDesc *dd, char *display)
{
    NewDevDesc *ndd = (NewDevDesc *) calloc(1, sizeof(NewDevDesc));
    GEDevDesc* gdd = (GEDevDesc*) GetDevice(devNumber((DevDesc*) dd));
    if (!ndd) {
	R_ShowMessage("No enough memory to copy graphics window");
	return;
    }
    if(!R_CheckDeviceAvailableBool()) {
	free(ndd);
	R_ShowMessage("No device available to copy graphics window");
	return;
    }

    ndd->displayList = R_NilValue;
    if (GADeviceDriver(ndd, display,
		       fromDeviceWidth(toDeviceWidth(1.0, GE_NDC, gdd),
				       GE_INCHES, gdd),
		       fromDeviceHeight(toDeviceHeight(-1.0, GE_NDC, gdd),
					GE_INCHES, gdd),
		       ((gadesc*) dd->deviceSpecific)->basefontsize,
		       0, 1, White, 1))
        PrivateCopyDevice(dd, ndd, display);
}


static void SaveAsPostscript(NewDevDesc *dd, char *fn)
{
    SEXP s = findVar(install(".PostScript.Options"), R_GlobalEnv);
    NewDevDesc *ndd = (NewDevDesc *) calloc(1, sizeof(NewDevDesc));
    GEDevDesc* gdd = (GEDevDesc*) GetDevice(devNumber((DevDesc*) dd));
    char family[256], encoding[256], paper[256], bg[256], fg[256],
	**afmpaths = NULL;

    if (!ndd) {
	R_ShowMessage("Not enough memory to copy graphics window");
	return;
    }
    if(!R_CheckDeviceAvailableBool()) {
	free(ndd);
	R_ShowMessage("No device available to copy graphics window");
	return;
    }

    ndd->displayList = R_NilValue;

    /* Set default values... */
    strcpy(family, "Helvetica");
    strcpy(encoding, "ISOLatin1.enc");
    strcpy(paper, "default");
    strcpy(bg, "transparent");
    strcpy(fg, "black");
    /* and then try to get it from .PostScript.Options */
    if ((s!=R_UnboundValue) && (s!=R_NilValue)) {
	SEXP names = getAttrib(s, R_NamesSymbol);
	int i,done;
	for (i=0, done=0; (done<4) && (i<length(s)) ; i++) {
	    if(!strcmp("family", CHAR(STRING_ELT(names, i)))) {
		strcpy(family, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)));
		done += 1;
	    }
	    if(!strcmp("paper", CHAR(STRING_ELT(names, i)))) {
		strcpy(paper, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)));
		done += 1;
	    }
	    if(!strcmp("bg", CHAR(STRING_ELT(names, i)))) {
		strcpy(bg, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)));
		done += 1;
	    }
	    if(!strcmp("fg", CHAR(STRING_ELT(names, i)))) {
		strcpy(fg, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)));
		done += 1;
	    }
	}
    }
    if (PSDeviceDriver((DevDesc *) ndd,
		       fn, paper, family, afmpaths, encoding, bg, fg,
		       fromDeviceWidth(toDeviceWidth(1.0, GE_NDC, gdd),
				       GE_INCHES, gdd),
		       fromDeviceHeight(toDeviceHeight(-1.0, GE_NDC, gdd),
					GE_INCHES, gdd),
		       (double)0, ((gadesc*) dd->deviceSpecific)->basefontsize,
		       0, 1, 0, "", "R Graphics Output"))
	/* horizontal=F, onefile=F, pagecentre=T, print.it=F */
	PrivateCopyDevice(dd, ndd, "postscript");
}


static void SaveAsPDF(NewDevDesc *dd, char *fn)
{
    SEXP s = findVar(install(".PostScript.Options"), R_GlobalEnv);
    NewDevDesc *ndd = (NewDevDesc *) calloc(1, sizeof(NewDevDesc));
    GEDevDesc* gdd = (GEDevDesc*) GetDevice(devNumber((DevDesc*) dd));
    char family[256], encoding[256], bg[256], fg[256];

    if (!ndd) {
	R_ShowMessage("Not enough memory to copy graphics window");
	return;
    }
    if(!R_CheckDeviceAvailableBool()) {
	free(ndd);
	R_ShowMessage("No device available to copy graphics window");
	return;
    }

    ndd->displayList = R_NilValue;

    /* Set default values... */
    strcpy(family, "Helvetica");
    strcpy(encoding, "ISOLatin1.enc");
    strcpy(bg, "transparent");
    strcpy(fg, "black");
    /* and then try to get it from .PostScript.Options */
    if ((s!=R_UnboundValue) && (s!=R_NilValue)) {
	SEXP names = getAttrib(s, R_NamesSymbol);
	int i,done;
	for (i=0, done=0; (done<4) && (i<length(s)) ; i++) {
	    if(!strcmp("family", CHAR(STRING_ELT(names, i)))) {
		strcpy(family, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)));
		done += 1;
	    }
	    if(!strcmp("bg", CHAR(STRING_ELT(names, i)))) {
		strcpy(bg, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)));
		done += 1;
	    }
	    if(!strcmp("fg", CHAR(STRING_ELT(names, i)))) {
		strcpy(fg, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)));
		done += 1;
	    }
	}
    }
    if (PDFDeviceDriver((DevDesc *) ndd, fn, family, encoding, bg, fg,
			fromDeviceWidth(toDeviceWidth(1.0, GE_NDC, gdd),
					GE_INCHES, gdd),
			fromDeviceHeight(toDeviceHeight(-1.0, GE_NDC, gdd),
					 GE_INCHES, gdd),
			((gadesc*) dd->deviceSpecific)->basefontsize, 
			1, "R Graphics Output"))
	PrivateCopyDevice(dd, ndd, "PDF");
}



			/* Pixel Dimensions (Inches) */
static double pixelWidth(drawing obj)
{
    return ((double) 1) / devicepixelsx(obj);
}

static double pixelHeight(drawing obj)
{
    return ((double) 1) / devicepixelsy(obj);
}

			/* Font information array. */
			/* Point sizes: 6-24 */
			/* Faces: plain, bold, oblique, bold-oblique */
			/* Symbol may be added later */

#define NFONT 19
#define MAXFONT 32
static int fontnum;
static int fontinitdone = 0;/* in {0,1,2} */
static char *fontname[MAXFONT];
static int fontstyle[MAXFONT];

static void RStandardFonts()
{
    int   i;

    for (i = 0; i < 4; i++)
	fontname[i] = "Times New Roman";
    fontname[4] = "Symbol";
    fontstyle[0] = fontstyle[4] = Plain;
    fontstyle[1] = Bold;
    fontstyle[2] = Italic;
    fontstyle[3] = BoldItalic;
    fontnum = 5;
    fontinitdone = 2;		/* =fontinit done & fontname must not be
				   free-ed */
}


static void RFontInit()
{
    int   i, notdone;
    char *opt[2];
    char  oops[256];

    sprintf(oops, "%s/Rdevga", getenv("R_USER"));
    notdone = 1;
    fontnum = 0;
    fontinitdone = 1;
    if (!optopenfile(oops)) {
	sprintf(oops, "%s/etc/Rdevga", getenv("R_HOME"));
	if (!optopenfile(oops)) {
	    RStandardFonts();
	    notdone = 0;
	}
    }
    while (notdone) {
	oops[0] = '\0';
	notdone = optread(opt, ':');
	if (notdone == 1)
	    sprintf(oops, "[%s] Error at line %d.", optfile(), optline());
	else if (notdone == 2) {
	    fontname[fontnum] = strdup(opt[0]);
	    if (!fontname[fontnum])
		strcpy(oops, "Insufficient memory. ");
	    else {
		if (!strcmpi(opt[1], "plain"))
		    fontstyle[fontnum] = Plain;
		else if (!strcmpi(opt[1], "bold"))
		    fontstyle[fontnum] = Bold;
		else if (!strcmpi(opt[1], "italic"))
		    fontstyle[fontnum] = Italic;
		else if (!strcmpi(opt[1], "bold&italic"))
		    fontstyle[fontnum] = BoldItalic;
		else
		    sprintf(oops, "Unknown style at line %d. ", optline());
		fontnum += 1;
	    }
	}
	if (oops[0]) {
	    optclosefile();
	    strcat(oops, optfile());
	    strcat(oops, " will be ignored.");
	    R_ShowMessage(oops);
	    for (i = 0; i < fontnum; i++) free(fontname);
	    RStandardFonts();
	    notdone = 0;
	}
	if (fontnum == MAXFONT) {
	    optclosefile();
	    notdone = 0;
	}
    }
}



static int SetBaseFont(gadesc *xd)
{
    xd->fontface = 1;
    xd->fontsize = xd->basefontsize;
    xd->fontangle = 0.0;
    xd->usefixed = FALSE;
    xd->font = gnewfont(xd->gawin, fontname[0], fontstyle[0],
			MulDiv(xd->fontsize, xd->wanteddpi, xd->truedpi), 0.0);
    if (!xd->font) {
	xd->usefixed= TRUE;
	xd->font = xd->fixedfont = FixedFont;
	if (!xd->fixedfont)
	    return 0;
    }
    return 1;
}



/* Set the font size and face */
/* If the font of this size and at that the specified */
/* rotation is not present it is loaded. */
/* 0 = plain text, 1 = bold */
/* 2 = oblique, 3 = bold-oblique */

#define SMALLEST 2
#define LARGEST 100

static void SetFont(int face, int size, double rot, NewDevDesc *dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (face < 1 || face > fontnum)
	face = 1;
    if (size < SMALLEST) size = SMALLEST;
    if (size > LARGEST) size = LARGEST;
    size = MulDiv(size, xd->wanteddpi, xd->truedpi);
    if (!xd->usefixed &&
	(size != xd->fontsize || face != xd->fontface ||
	 rot != xd->fontangle)) {
	 del(xd->font); doevent();
	 xd->font = gnewfont(xd->gawin,
			    fontname[face - 1], fontstyle[face - 1],
			    size, rot);
	if (xd->font) {
	    xd->fontface = face;
	    xd->fontsize = size;
	    xd->fontangle = rot;
	} else {
	    SetBaseFont(xd);
	}
    }
}


static void SetColor(int color, double gamma, NewDevDesc *dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (color != xd->col) {
	xd->col = color;
	xd->fgcolor = GArgb(color, gamma);
    }
}


/*
 *	Some Notes on Line Textures
 *
 *	Line textures are stored as an array of 4-bit integers within
 *	a single 32-bit word.  These integers contain the lengths of
 *	lines to be drawn with the pen alternately down and then up.
 *	The device should try to arrange that these values are measured
 *	in points if possible, although pixels is ok on most displays.
 *
 *	If newlty contains a line texture description it is decoded
 *	as follows:
 *
 *		ndash = 0;
 *		for(i=0 ; i<8 && newlty&15 ; i++) {
 *			dashlist[ndash++] = newlty&15;
 *			newlty = newlty>>4;
 *		}
 *		dashlist[0] = length of pen-down segment
 *		dashlist[1] = length of pen-up segment
 *		etc
 *
 *	An integer containing a zero terminates the pattern.  Hence
 *	ndash in this code fragment gives the length of the texture
 *	description.  If a description contains an odd number of
 *	elements it is replicated to create a pattern with an
 *	even number of elements.  (If this is a pain, do something
 *	different its not crucial).
 *
 *	27/5/98 Paul - change to allow lty and lwd to interact:
 *	the line texture is now scaled by the line width so that,
 *	for example, a wide (lwd=2) dotted line (lty=2) has bigger
 *	dots which are more widely spaced.  Previously, such a line
 *	would have "dots" which were wide, but not long, nor widely
 *	spaced.
 */

static void SetLinetype(int newlty, double nlwd, NewDevDesc *dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    int   newlwd;

    newlwd = nlwd;
    if (newlwd < 1)
	newlwd = 1;
    xd->lty = newlty;
    xd->lwd = newlwd;
}



/* Callback functions */


static void HelpResize(window w, rect r)
{
    if (AllDevicesKilled) return;
    {
	NewDevDesc *dd = (NewDevDesc *) getdata(w);
	gadesc *xd = (gadesc *) dd->deviceSpecific;

	if (r.width) {
	    if ((xd->windowWidth != r.width) ||
		((xd->windowHeight != r.height))) {
		xd->windowWidth = r.width;
		xd->windowHeight = r.height;
		xd->resize = TRUE;
	    }
	}
    }
}

static void HelpClose(window w)
{
    if (AllDevicesKilled) return;
    {
	NewDevDesc *dd = (NewDevDesc *) getdata(w);
	KillDevice(GetDevice(devNumber((DevDesc*) dd)));
    }
}

static void HelpExpose(window w, rect r)
{
    if (AllDevicesKilled) return;
    {
	NewDevDesc *dd = (NewDevDesc *) getdata(w);
	GEDevDesc* gdd = (GEDevDesc*) GetDevice(devNumber((DevDesc*) dd));
	gadesc *xd = (gadesc *) dd->deviceSpecific;

	if (xd->resize) {
	    GA_Resize(dd);
	    xd->replaying = TRUE;
	    GEplayDisplayList(gdd);
	    xd->replaying = FALSE;
	    R_ProcessEvents();
	} else
	    SHOW;
    }
}

static void HelpMouseClick(window w, int button, point pt)
{
    if (AllDevicesKilled) return;
    {
	NewDevDesc *dd = (NewDevDesc *) getdata(w);
	gadesc *xd = (gadesc *) dd->deviceSpecific;

	if (!xd->locator)
	    return;
	if (button & LeftButton) {
	    gabeep();
	    xd->clicked = 1;
	    xd->px = pt.x;
	    xd->py = pt.y;
	} else
	    xd->clicked = 2;
    }
}

static void menustop(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    if (!xd->locator)
	return;
    xd->clicked = 2;
}

void  fixslash(char *);

static void menufilebitmap(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    char *fn;
    if (m==xd->mpng) {
      setuserfilter("Png files (*.png)\0*.png\0\0");
      fn = askfilesave("Portable network graphics file", "");
    } else if (m==xd->mbmp) {
      setuserfilter("Windows bitmap files (*.bmp)\0*.bmp\0\0");
      fn = askfilesave("Windows bitmap file", "");
    } else {
      setuserfilter("Jpeg files (*.jpeg,*jpg)\0*.jpeg;*.jpg\0\0");
      fn = askfilesave("Jpeg file", "");
    }
    if (!fn) return;
    fixslash(fn);
    gsetcursor(xd->gawin, WatchCursor);
    show(xd->gawin);
    if (m==xd->mpng) SaveAsPng(dd, fn);
    else if (m==xd->mbmp) SaveAsBmp(dd, fn);
    else if (m==xd->mjpeg50) SaveAsJpeg(dd, 50, fn);
    else if (m==xd->mjpeg75) SaveAsJpeg(dd, 75, fn);
    else SaveAsJpeg(dd, 100, fn);
    gsetcursor(xd->gawin, ArrowCursor);
    show(xd->gawin);
}


static void menups(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    char  *fn;

    setuserfilter("Postscript files (*.ps)\0*.ps\0All files (*.*)\0*.*\0\0");
    fn = askfilesave("Postscript file", "");
    if (!fn) return;
    fixslash(fn);
    SaveAsPostscript(dd, fn);
}


static void menupdf(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    char  *fn;

    setuserfilter("PDF files (*.pdf)\0*.pdf\0All files (*.*)\0*.*\0\0");
    fn = askfilesave("PDF file", "");
    if (!fn) return;
    fixslash(fn);
    SaveAsPDF(dd, fn);
}


static void menuwm(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    char  display[512], *fn;

    setuserfilter("Enhanced metafiles (*.emf)\0*.emf\0All files (*.*)\0*.*\0\0");
    fn = askfilesave("Enhanced metafiles", "");
    if (!fn) return;
    fixslash(fn);
    sprintf(display, "win.metafile:%s", fn);
    SaveAsWin(dd, display);
}


static void menuclpwm(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    SaveAsWin(dd, "win.metafile");
}

static void menuclpbm(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    show(xd->gawin);
    gsetcursor(xd->gawin, WatchCursor);
    copytoclipboard(xd->bm);
    gsetcursor(xd->gawin, ArrowCursor);
}

static void menuprint(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    SaveAsWin(dd, "win.print:");
}



static void menuclose(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    HelpClose(xd->gawin);
}

/* plot history */


#ifdef PLOTHISTORY

extern SEXP savedSnapshot;

/* NB: this puts .SavedPlots in package:base */
#define GROWTH 4
#define GETDL SEXP vDL=findVar(install(".SavedPlots"), R_GlobalEnv)
#define SETDL defineVar(install(".SavedPlots"), vDL, R_GlobalEnv)
/* altered in 1.4.0, as incompatible format */
#define PLOTHISTORYMAGIC 31416
#define pMAGIC      (INTEGER(VECTOR_ELT(vDL, 0))[0])
#define pNUMPLOTS   (INTEGER(VECTOR_ELT(vDL, 1))[0])
#define pMAXPLOTS   (INTEGER(VECTOR_ELT(vDL, 2))[0])
#define pCURRENTPOS (INTEGER(VECTOR_ELT(vDL, 3))[0])
#define pHISTORY    (VECTOR_ELT(vDL, 4))
#define SET_pHISTORY(v)    (SET_VECTOR_ELT(vDL, 4, v))
#define pCURRENT    (VECTOR_ELT(pHISTORY, pCURRENTPOS))
#define pCURRENTdl  (VECTOR_ELT(pCURRENT, 0))
#define pCURRENTgp  (INTEGER(VECTOR_ELT(pCURRENT, 1)))
#define pCURRENTsnapshot (VECTOR_ELT(pCURRENT, 0))
#define pCHECK      if ((TYPEOF(vDL)!=VECSXP)||\
                       (TYPEOF(VECTOR_ELT(vDL, 0))!=INTSXP) ||\
                       (LENGTH(VECTOR_ELT(vDL, 0))!=1) ||\
                       (pMAGIC != PLOTHISTORYMAGIC)) {\
                       R_ShowMessage("Plot history seems corrupted");\
                       return;}
#define pMOVE(a) {pCURRENTPOS+=a;\
                  if(pCURRENTPOS<0) pCURRENTPOS=0;\
                  if(pCURRENTPOS>pNUMPLOTS-1) pCURRENTPOS=pNUMPLOTS-1;\
                  Replay(dd,vDL);SETDL;}
#define pEXIST ((vDL!=R_UnboundValue) && (vDL!=R_NilValue))
#define pMUSTEXIST if(!pEXIST){R_ShowMessage("No plot history!");return;}




static SEXP NewPlotHistory(int n)
{
    SEXP  vDL, class;
    int   i;

    PROTECT(vDL = allocVector(VECSXP, 5));
    for (i = 0; i < 4; i++)
	PROTECT(SET_VECTOR_ELT(vDL, i, allocVector(INTSXP, 1)));
    PROTECT(SET_pHISTORY (allocVector(VECSXP, n)));
    pMAGIC = PLOTHISTORYMAGIC;
    pNUMPLOTS = 0;
    pMAXPLOTS = n;
    pCURRENTPOS = -1;
    for (i = 0; i < n; i++)
	SET_VECTOR_ELT(pHISTORY, i, R_NilValue);
    PROTECT(class = allocVector(STRSXP, 1));
    SET_STRING_ELT(class, 0, mkChar("SavedPlots"));
    classgets(vDL, class);
    SETDL;
    UNPROTECT(7);
    return vDL;
}

static SEXP GrowthPlotHistory(SEXP vDL)
{
    SEXP  vOLD;
    int   i, oldNPlots, oldCurrent;

    PROTECT(vOLD = pHISTORY);
    oldNPlots = pNUMPLOTS;
    oldCurrent = pCURRENTPOS;
    PROTECT(vDL = NewPlotHistory(pMAXPLOTS + GROWTH));
    for (i = 0; i < oldNPlots; i++)
	SET_VECTOR_ELT(pHISTORY, i, VECTOR_ELT(vOLD, i));
    pNUMPLOTS = oldNPlots;
    pCURRENTPOS = oldCurrent;
    SETDL;
    UNPROTECT(2);
    return vDL;
}

static void AddtoPlotHistory(SEXP snapshot, int replace)
{
    int   where;
    SEXP  class;

    GETDL;
/*    if (dl == R_NilValue) {
	R_ShowMessage("Display list is void!");
	return;
	} */
    if (!pEXIST)
	vDL = NewPlotHistory(GROWTH);
    else if (!replace && (pNUMPLOTS == pMAXPLOTS))
	vDL = GrowthPlotHistory(vDL);
    PROTECT(vDL);
    pCHECK;
    if (replace)
	where = pCURRENTPOS;
    else
	where = pNUMPLOTS;
    PROTECT(snapshot);
    PROTECT(class = allocVector(STRSXP, 1));
    SET_STRING_ELT(class, 0, mkChar("recordedplot"));
    classgets(snapshot, class);
    SET_VECTOR_ELT(pHISTORY, where, snapshot);
    pCURRENTPOS = where;
    if (!replace) pNUMPLOTS += 1;
    SETDL;
    UNPROTECT(3);
}


static void Replay(NewDevDesc *dd, SEXP vDL)
{
    GEDevDesc *gdd = (GEDevDesc *) GetDevice(devNumber((DevDesc*) dd));
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    xd->replaying = TRUE;
    gsetcursor(xd->gawin, WatchCursor);
    GEplaySnapshot(pCURRENT, gdd);
    xd->replaying = FALSE;
    gsetcursor(xd->gawin, ArrowCursor);
}

static void menurec(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (xd->recording) {
	xd->recording = FALSE;
	uncheck(m);
    } else {
	xd->recording = TRUE;
	check(m);
    }
}


static void menuadd(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    GEDevDesc *gdd = (GEDevDesc *) GetDevice(devNumber((DevDesc*) dd));
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    AddtoPlotHistory(GEcreateSnapshot(gdd), 0);
    xd->needsave = FALSE;
}

static void menureplace(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    GEDevDesc *gdd = (GEDevDesc *) GetDevice(devNumber((DevDesc*) dd));

    GETDL;
    pMUSTEXIST;
    pCHECK;
    if (pCURRENTPOS < 0) {
	R_ShowMessage("No plot to replace!");
	return;
    }
    AddtoPlotHistory(GEcreateSnapshot(gdd), 1);
}

static void menunext(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    GETDL;
    if (xd->needsave) return;
    pMUSTEXIST;
    pCHECK;
    if (pCURRENTPOS != (pNUMPLOTS - 1)) pMOVE(1);
}

static void menuprev(control m)
{
    NewDevDesc *dd = (NewDevDesc*) getdata(m);
    GEDevDesc *gdd = (GEDevDesc *) GetDevice(devNumber((DevDesc*) dd));
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    GETDL;
    pMUSTEXIST;
    pCHECK;
    if (pNUMPLOTS) {
	if (xd->recording && xd->needsave && (dd->displayList != R_NilValue)) {
	    AddtoPlotHistory(GEcreateSnapshot(gdd), 0);
	    xd->needsave = FALSE;
	}
	pMOVE((xd->needsave) ? 0 : -1);
    }
}

static void menuclear(control m)
{
    defineVar(install(".SavedPlots"), R_NilValue, R_GlobalEnv);
}

static void menugvar(control m)
{
    SEXP  vDL;
    char *v = askstring("Variable name", "");
    NewDevDesc *dd = (NewDevDesc *) getdata(m);

    if (!v)
	return;
    vDL = findVar(install(v), R_GlobalEnv);
    if (!pEXIST || !pNUMPLOTS) {
	R_ShowMessage("Variable doesn't exist or doesn't contain any plots!");
	return;
    }
    pCHECK;
    pCURRENTPOS = 0;
    Replay(dd, vDL);
    SETDL;
}

static void menusvar(control m)
{
    char *v;

    GETDL;
    pMUSTEXIST;
    pCHECK;
    v = askstring("Name of variable to save to", "");
    if (!v)
	return;
    defineVar(install(v), vDL, R_GlobalEnv);
}
#endif

static void menuconsole(control m)
{
   show(RConsole);
}

static void menuR(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    check(xd->mR);
    uncheck(xd->mfix);
    uncheck(xd->mfit);
    xd->resizing = 1;
    xd->resize = TRUE;
    HelpExpose(m, getrect(xd->gawin));
}

static void menufit(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    uncheck(xd->mR);
    check(xd->mfit);
    uncheck(xd->mfix);
    xd->resizing = 2;
    xd->resize = TRUE;
    HelpExpose(m, getrect(xd->gawin));
}

static void menufix(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    uncheck(xd->mR);
    uncheck(xd->mfit);
    check(xd->mfix);
    xd->resizing = 3;
    xd->resize = TRUE;
    HelpExpose(m, getrect(xd->gawin));
}

static void CHelpKeyIn(control w, int key)
{
#ifdef PLOTHISTORY
    NewDevDesc *dd = (NewDevDesc *) getdata(w);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (xd->replaying) return;
    switch (key) {
      case INS:
	menuadd(xd->madd);
	break;
      case PGUP:
	menuprev(xd->mprev);
	break;
      case PGDN:
	menunext(xd->mnext);
	break;
    }
#endif
}

static void NHelpKeyIn(control w,int key)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(w);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (xd->replaying) return;
    if (ggetkeystate() != CtrlKey)
	return;
    key = 'A' + key - 1;
    if (key == 'C')
	menuclpbm(xd->mclpbm);
    if (dd->displayList == R_NilValue)
	return;
    if (key == 'W')
	menuclpwm(xd->mclpwm);
    if (key == 'P')
	menuprint(xd->mprint);
}

static void mbarf(control m)
{
#ifdef PLOTHISTORY
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    GETDL;
    if (pEXIST && !xd->replaying) {
	enable(xd->mnext);
	enable(xd->mprev);
	if ((pCURRENTPOS >= 0) && (dd->displayList != R_NilValue))
	    enable(xd->mreplace);
	else
	    disable(xd->mreplace);
	enable(xd->msvar);
	enable(xd->mclear);
    } else {
	disable(xd->mnext);
	disable(xd->mprev);
	disable(xd->mreplace);
	disable(xd->msvar);
	disable(xd->mclear);
    }
    if (!xd->replaying)
	enable(xd->mgvar);
    else
	disable(xd->mgvar);
    if ((dd->displayList != R_NilValue) && !xd->replaying) {
	enable(xd->madd);
	enable(xd->mprint);
	enable(xd->mpng);
	enable(xd->mbmp);
	enable(xd->mjpeg50);
	enable(xd->mjpeg75);
	enable(xd->mjpeg100);
	enable(xd->mwm);
	enable(xd->mps);
	enable(xd->mpdf);
	enable(xd->mclpwm);
	enable(xd->mclpbm);
    } else {
	disable(xd->madd);
	disable(xd->mprint);
	disable(xd->msubsave);
	disable(xd->mpng);
	disable(xd->mbmp);
	disable(xd->mjpeg50);
	disable(xd->mjpeg75);
	disable(xd->mjpeg100);
	disable(xd->mwm);
	disable(xd->mps);
	disable(xd->mpdf);
	disable(xd->mclpwm);
	disable(xd->mclpbm);
    }
    draw(xd->mbar);
#endif
}


        /********************************************************/
	/* device_Open is not usually called directly by the 	*/
	/* graphics engine;  it is usually only called from 	*/
	/* the device-driver entry point.			*/
	/* this function should set up all of the device-	*/
	/* specific resources for a new device			*/
	/* this function is given a new	structure for device-	*/
	/* specific information AND it must FREE the structure 	*/
	/* if anything goes seriously wrong			*/
	/* NOTE that it is perfectly acceptable for this 	*/
	/* function to set generic graphics parameters too	*/
	/* (i.e., override the generic parameter settings	*/
	/* which GInit sets up) all at the author's own risk	*/
	/* of course :)						*/
	/********************************************************/

#define MCHECK(m) {if(!(m)) {del(xd->gawin); return 0;}}

static void devga_sbf(control c, int pos)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(c);
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    if (pos < 0) {
	pos = -pos-1;
	pos = min(pos*SF, (xd->origWidth - xd->windowWidth + SF-1));
	xd->xshift = -pos;
    } else {
	pos = min(pos*SF, (xd->origHeight - xd->windowHeight + SF-1));
	xd->yshift = -pos;
    }
    xd->resize = 1;
    HelpExpose(c, getrect(xd->gawin));
}


static int
setupScreenDevice(NewDevDesc *dd, gadesc *xd, double w, double h,
		  Rboolean recording, int resize)
{
    menu  m;
    int   iw, ih;
    double dw, dw0, dh, d;

    xd->kind = SCREEN;
    if (R_finite(user_xpinch) && user_xpinch > 0.0)
	dw = dw0 = (int) (w * user_xpinch);
    else
	dw = dw0 = (int) (w / pixelWidth(NULL));
    if (R_finite(user_ypinch) && user_ypinch > 0.0)
	dh = (int) (w * user_ypinch);
    else
	dh = (int) (h / pixelHeight(NULL));
    if (resize != 3) {
	if ((dw / devicewidth(NULL)) > 0.85) {
	    d = dh / dw;
	    dw = 0.85 * devicewidth(NULL);
	    dh = d * dw;
	}
	if ((dh / deviceheight(NULL)) > 0.85) {
	    d = dw / dh;
	    dh = 0.85 * deviceheight(NULL);
	    dw = d * dh;
	}
    } else {
	dw = min(dw, 0.85*devicewidth(NULL));
	dh = min(dh, 0.85*deviceheight(NULL));
    }
    iw = dw + 0.5;
    ih = dh + 0.5;
    if (resize == 2) xd->rescale_factor = dw/dw0;
    if (!(xd->gawin = newwindow("R Graphics",
				rect(devicewidth(NULL) - iw - 25, 0, iw, ih),
				Document | StandardWindow | Menubar |
				VScrollbar | HScrollbar))) {
	return 0;
    }
    gchangescrollbar(xd->gawin, VWINSB, 0, ih/SF-1, ih/SF, 0);
    gchangescrollbar(xd->gawin, HWINSB, 0, iw/SF-1, iw/SF, 0);

    addto(xd->gawin);
    gsetcursor(xd->gawin, ArrowCursor);
    if (ismdi() && (RguiMDI & RW_TOOLBAR)) {
	int btsize = 24;
	rect r = rect(2, 2, btsize, btsize);
	control bt, tb;

	MCHECK(tb = newtoolbar(btsize + 4));
	gsetcursor(tb, ArrowCursor);
	addto(tb);

	MCHECK(bt = newtoolbutton(cam_image, r, menuclpwm));
	MCHECK(addtooltip(bt, "Copy to the clipboard as a metafile"));
	gsetcursor(bt, ArrowCursor);
	setdata(bt, (void *) dd);
	r.x += (btsize + 6);

	MCHECK(bt = newtoolbutton(print_image, r, menuprint));
	MCHECK(addtooltip(bt, "Print"));
	gsetcursor(bt, ArrowCursor);
	setdata(bt, (void *) dd);
	r.x += (btsize + 6);

	MCHECK(bt = newtoolbutton(console_image, r, menuconsole));
	MCHECK(addtooltip(bt, "Return focus to console"));
	gsetcursor(bt, ArrowCursor);
	setdata(bt, (void *) dd);
	r.x += (btsize + 6);

	MCHECK(xd->stoploc = newtoolbutton(stop_image, r, menustop));
	MCHECK(addtooltip(xd->stoploc, "Stop locator"));
	gsetcursor(bt, ArrowCursor);
	setdata(xd->stoploc,(void *) dd);
	hide(xd->stoploc);
    } else
	xd->stoploc = NULL;

    /* First we prepare 'locator' menubar and popup */
    addto(xd->gawin);
    MCHECK(xd->mbarloc = newmenubar(NULL));
    MCHECK(newmenu("Stop"));
    MCHECK(m = newmenuitem("Stop locator", 0, menustop));
    setdata(m, (void *) dd);
    MCHECK(xd->locpopup = newpopup(NULL));
    MCHECK(m = newmenuitem("Stop", 0, menustop));
    setdata(m, (void *) dd);
    MCHECK(newmenuitem("Continue", 0, NULL));

    /* Normal menubar */
    MCHECK(xd->mbar = newmenubar(mbarf));
    MCHECK(m = newmenu("File"));
    MCHECK(xd->msubsave = newsubmenu(m, "Save as"));
    MCHECK(xd->mwm = newmenuitem("Metafile...", 0, menuwm));
    MCHECK(xd->mps = newmenuitem("Postscript...", 0, menups));
    MCHECK(xd->mpdf = newmenuitem("PDF...", 0, menupdf));
    MCHECK(xd->mpng = newmenuitem("Png...", 0, menufilebitmap));
    MCHECK(xd->mbmp = newmenuitem("Bmp...", 0, menufilebitmap));
    MCHECK(newsubmenu(xd->msubsave,"Jpeg"));
    MCHECK(xd->mjpeg50 = newmenuitem("50% quality...", 0, menufilebitmap));
    MCHECK(xd->mjpeg75 = newmenuitem("75% quality...", 0, menufilebitmap));
    MCHECK(xd->mjpeg100 = newmenuitem("100% quality...", 0, menufilebitmap));
    MCHECK(newsubmenu(m, "Copy to the clipboard"));
    MCHECK(xd->mclpbm = newmenuitem("as a Bitmap\tCTRL+C", 0, menuclpbm));
    MCHECK(xd->mclpwm = newmenuitem("as a Metafile\tCTRL+W", 0, menuclpwm));
    addto(m);
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(xd->mprint = newmenuitem("Print...\tCTRL+P", 0, menuprint));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(xd->mclose = newmenuitem("close Device", 0, menuclose));
#ifdef PLOTHISTORY
    MCHECK(newmenu("History"));
    MCHECK(xd->mrec = newmenuitem("Recording", 0, menurec));
    if(recording) check(xd->mrec);
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(xd->madd = newmenuitem("Add\tINS", 0, menuadd));
    MCHECK(xd->mreplace = newmenuitem("Replace", 0, menureplace));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(xd->mprev = newmenuitem("Previous\tPgUp", 0, menuprev));
    MCHECK(xd->mnext = newmenuitem("Next\tPgDown", 0, menunext));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(xd->msvar = newmenuitem("Save to variable...", 0, menusvar));
    MCHECK(xd->mgvar = newmenuitem("Get from variable...", 0, menugvar));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(xd->mclear = newmenuitem("Clear history", 0, menuclear));
#endif
    MCHECK(newmenu("Resize"));
    MCHECK(xd->mR = newmenuitem("R mode", 0, menuR));
    if(resize == 1) check(xd->mR);
    MCHECK(xd->mfit = newmenuitem("Fit to window", 0, menufit));
    if(resize == 2) check(xd->mfit);
    MCHECK(xd->mfix = newmenuitem("Fixed size", 0, menufix));
    if(resize == 3) check(xd->mfix);
    newmdimenu();

    /* Normal popup */
    MCHECK(xd->grpopup = newpopup(NULL));
    MCHECK(m = newmenuitem("Copy as metafile", 0, menuclpwm));
    setdata(m, (void *) dd);
    MCHECK(m = newmenuitem("Copy as bitmap", 0, menuclpbm));
    setdata(m, (void *) dd);
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(m = newmenuitem("Save as metafile...", 0, menuwm));
    setdata(m, (void *) dd);
    MCHECK(m = newmenuitem("Save as postscript...", 0, menups));
    setdata(m, (void *) dd);
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(m = newmenuitem("Print...", 0, menuprint));
    setdata(m, (void *) dd);
    gchangepopup(xd->gawin, xd->grpopup);

    MCHECK(xd->bm = newbitmap(getwidth(xd->gawin), getheight(xd->gawin),
			      getdepth(xd->gawin)));
    gfillrect(xd->gawin, xd->outcolor, getrect(xd->gawin));
    gfillrect(xd->bm, xd->outcolor, getrect(xd->bm));
    addto(xd->gawin);
    setdata(xd->mbar, (void *) dd);
    setdata(xd->mpng, (void *) dd);
    setdata(xd->mbmp, (void *) dd);
    setdata(xd->mjpeg50, (void *) dd);
    setdata(xd->mjpeg75, (void *) dd);
    setdata(xd->mjpeg100, (void *) dd);
    setdata(xd->mps, (void *) dd);
    setdata(xd->mpdf, (void *) dd);
    setdata(xd->mwm, (void *) dd);
    setdata(xd->mclpwm, (void *) dd);
    setdata(xd->mclpbm, (void *) dd);
    setdata(xd->mprint, (void *) dd);
    setdata(xd->mclose, (void *) dd);
    setdata(xd->mrec, (void *) dd);
    setdata(xd->mprev, (void *) dd);
    setdata(xd->mnext, (void *) dd);
    setdata(xd->mgvar, (void *) dd);
    setdata(xd->madd, (void *) dd);
    setdata(xd->mreplace, (void *) dd);
    setdata(xd->mR, (void *) dd);
    setdata(xd->mfit, (void *) dd);
    setdata(xd->mfix, (void *) dd);
    show(xd->gawin); /* twice, for a Windows bug */
    show(xd->gawin);
    BringToTop(xd->gawin);
    sethit(xd->gawin, devga_sbf);
    setresize(xd->gawin, HelpResize);
    setredraw(xd->gawin, HelpExpose);
    setmousedown(xd->gawin, HelpMouseClick);
    setkeydown(xd->gawin, NHelpKeyIn);
    setkeyaction(xd->gawin, CHelpKeyIn);
    setclose(xd->gawin, HelpClose);
    xd->recording = recording;
    xd->replaying = FALSE;
    xd->resizing = resize;

    return 1;
}

static Rboolean GA_Open(NewDevDesc *dd, gadesc *xd, char *dsp,
			double w, double h, Rboolean recording,
			int resize, int canvascolor, double gamma)
{
    rect  rr;
    char buf[512];

    if (!fontinitdone)
	RFontInit();

    /* Foreground and Background Colors */
    xd->bg = dd->startfill = 0xffffffff; /* transparent */
    xd->col = dd->startcol = R_RGB(0, 0, 0);

    xd->fgcolor = Black;
    xd->bgcolor = xd->canvascolor = GArgb(canvascolor, gamma);
    xd->outcolor = myGetSysColor(COLOR_APPWORKSPACE);
    xd->rescale_factor = 1.0;
    xd->fast = 1;  /* Use `cosmetic pens' if available */
    xd->xshift = xd->yshift = 0;
    xd->npage = 0;
    if (!dsp[0]) {
	if (!setupScreenDevice(dd, xd, w, h, recording, resize))
	    return FALSE;
    } else if (!strncmp(dsp, "win.print:", 10)) {
	xd->kind = PRINTER;
	xd->gawin = newprinter(MM_PER_INCH * w, MM_PER_INCH * h, &dsp[10]);
	if (!xd->gawin)
	    return FALSE;
    } else if (!strncmp(dsp, "png:", 4) || !strncmp(dsp,"bmp:", 4)) {
	if(R_OPAQUE(canvascolor))
	    xd->bg = dd->startfill = GArgb(canvascolor, 1.0);
	else
	    xd->bg = dd->startfill = canvascolor;
	/* was R_RGB(255, 255, 255); white */
        xd->kind = (dsp[0]=='p') ? PNG : BMP;
	strcpy(xd->filename, dsp+4);
	if (!Load_Rbitmap_Dll()) {
	    warning("Unable to load Rbitmap.dll");
	    return FALSE;
	}
	/*
	  Observe that given actual graphapp implementation 256 is
	  irrelevant,i.e., depth of the bitmap is the one of graphic card
	  if required depth > 1
	*/
	if ((xd->gawin = newbitmap(w, h, 256)) == NULL) {
	    warning("Unable to allocate bitmap");
	    return FALSE;
	}
	sprintf(buf, xd->filename, 1);
	if ((xd->fp = fopen(buf, "wb")) == NULL) {
	    del(xd->gawin);
	    warning("Unable to open file `%s' for writing", buf);
	    return FALSE;
	}
    } else if (!strncmp(dsp, "jpeg:", 5)) {
        char *p = strchr(&dsp[5], ':');
	xd->bg = dd->startfill = GArgb(canvascolor, 1.0);
        xd->kind = JPEG;
	if (!p) return FALSE;
	if (!Load_Rbitmap_Dll()) {
	    warning("Unable to load Rbitmap.dll");
	    return FALSE;
	}
	*p = '\0';
	xd->quality = atoi(&dsp[5]);
	*p = ':' ;
	strcpy(xd->filename, p+1);
	if((xd->gawin = newbitmap(w, h, 256)) == NULL) {
	    warning("Unable to allocate bitmap");
	    return FALSE;
	}
	sprintf(buf, xd->filename, 1);
	if ((xd->fp = fopen(buf, "wb")) == NULL) {
	    del(xd->gawin);
	    warning("Unable to open file `%s' for writing", buf);
	    return FALSE;
	}
    } else {
	/*
	 * win.metafile[:] in memory (for the clipboard)
	 * win.metafile:filename
	 * anything else return FALSE
	 */
	char  *s = "win.metafile";
	int   ls = strlen(s);
	int   ld = strlen(dsp);

	if (ls > ld)
	    return FALSE;
	if (strncmp(dsp, s, ls) || (dsp[ls] && (dsp[ls] != ':')))
	    return FALSE;
	strcpy(xd->filename, (ld > ls) ? &dsp[ls + 1] : "");
	sprintf(buf, xd->filename, 1);
	xd->w = MM_PER_INCH * w;
	xd->h =  MM_PER_INCH * h;
	xd->gawin = newmetafile(buf, MM_PER_INCH * w, MM_PER_INCH * h);
	xd->kind = METAFILE;
	xd->fast = 0; /* use scalable line widths */
	if (!xd->gawin) {
	    if(ld > ls)
		warning("Unable to open metafile `%s' for writing", buf);
	    else
		warning("Unable to open clipboard to write metafile");
	    return FALSE;
	}
    }
    xd->truedpi = devicepixelsy(xd->gawin);
    if ((xd->kind == PNG) || (xd->kind == JPEG) || (xd->kind == BMP))
      xd->wanteddpi = 72 ;
    else
      xd->wanteddpi = xd->truedpi;
    if (!SetBaseFont(xd)) {
	warning("can't find any fonts");
	del(xd->gawin);
	if (xd->kind == SCREEN) del(xd->bm);
	return FALSE;
    }
    rr = getrect(xd->gawin);
    xd->origWidth = xd->showWidth = xd->windowWidth = rr.width;
    xd->origHeight = xd->showHeight = xd->windowHeight = rr.height;
    xd->clip = rr;
    setdata(xd->gawin, (void *) dd);
    xd->needsave = FALSE;
    return TRUE;
}

	/********************************************************/
	/* device_StrWidth should return the width of the given */
	/* string in DEVICE units (GStrWidth is responsible for */
	/* converting from DEVICE to whatever units the user 	*/
	/* asked for						*/
	/********************************************************/

static double GA_StrWidth(char *str, int font,
			      double cex, double ps, NewDevDesc *dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    double a;
    int   size = cex * ps + 0.5;

    SetFont(font, size, 0.0, dd);
    a = (double) gstrwidth(xd->gawin, xd->font, str);
    return a;
}


	/********************************************************/
	/* device_MetricInfo should return height, depth, and 	*/
	/* width information for the given character in DEVICE	*/
	/* units (GMetricInfo does the necessary conversions)	*/
	/* This is used for formatting mathematical expressions	*/
	/********************************************************/

	/* Character Metric Information */
	/* Passing c == 0 gets font information */

static void GA_MetricInfo(int c, int font, double cex, double ps,
			      double* ascent, double* descent,
			      double* width, NewDevDesc *dd)
{
    int   a, d, w;
    int   size = cex * ps + 0.5;
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    SetFont(font, size, 0.0, dd);
    gcharmetric(xd->gawin, xd->font, c, &a, &d, &w);
    *ascent = (double) a;
    *descent = (double) d;
    *width = (double) w;
}

	/********************************************************/
	/* device_Clip is given the left, right, bottom, and 	*/
	/* top of a rectangle (in DEVICE coordinates).  it 	*/
	/* should have the side-effect that subsequent output	*/
	/* is clipped to the given rectangle			*/
	/********************************************************/

static void GA_Clip(double x0, double x1, double y0, double y1, NewDevDesc *dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    xd->clip = rcanon(rpt(pt(x0, y0), pt(x1, y1)));
    xd->clip.width  += 1;
    xd->clip.height += 1;
}

	/********************************************************/
	/* device_Resize is called whenever the device is 	*/
	/* resized.  the function must update the GPar 		*/
	/* parameters (left, right, bottom, and top) for the 	*/
	/* new device size					*/
	/* this is not usually called directly by the graphics	*/
	/* engine because the detection of device resizes	*/
	/* (e.g., a window resize) are usually detected by	*/
	/* device-specific code	(see R_ProcessEvents in ./system.c)*/
	/********************************************************/

static void GA_Size(double *left, double *right,
		     double *bottom, double *top,
		     NewDevDesc *dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    int   iw, ih;

    iw = xd->windowWidth;
    ih = xd->windowHeight;
    *left = 0.0;
    *top = 0.0;
    *right = iw;
    *bottom = ih;
}

static void GA_Resize(NewDevDesc *dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    SEXP scale;
    PROTECT(scale = allocVector(REALSXP, 1));
    if (xd->resize) {
	int   iw, ih, iw0 = dd->right - dd->left,
	    ih0 = dd->bottom - dd->top;
	double fw, fh, rf, shift;

	iw = xd->windowWidth;
	ih = xd->windowHeight;
	if(xd->resizing == 1) {
	    /* last mode might have been 3, so remove scrollbars */
	    gchangescrollbar(xd->gawin, VWINSB, 0, ih/SF-1, ih/SF, 0);
	    gchangescrollbar(xd->gawin, HWINSB, 0, iw/SF-1, iw/SF, 0);
	    dd->left = 0.0;
	    dd->top = 0.0;
	    dd->right = iw;
	    dd->bottom = ih;
	    xd->showWidth = iw;
	    xd->showHeight =  ih;
	} else if (xd->resizing == 2) {
	    /* last mode might have been 3, so remove scrollbars */
	    gchangescrollbar(xd->gawin, VWINSB, 0, ih/SF-1, ih/SF, 0);
	    gchangescrollbar(xd->gawin, HWINSB, 0, iw/SF-1, iw/SF, 0);
	    fw = (iw + 0.5)/(iw0 + 0.5);
	    fh = (ih + 0.5)/(ih0 + 0.5);
	    rf = min(fw, fh);
	    xd->rescale_factor *= rf;
	    xd->wanteddpi = xd->rescale_factor * xd->truedpi;
	    /* dd->ps *= rf; dd->cra[0] *= rf; dd->cra[1] *= rf; */
	    REAL(scale)[0] = rf;
	    GEHandleEvent(GE_ScalePS, dd, scale);
	    if (fw < fh) {
		dd->left = 0.0;
		xd->showWidth = dd->right = iw;
		xd->showHeight =  ih0*fw;
		shift = (ih - xd->showHeight)/2.0;
		dd->top = shift;
		dd->bottom = ih0*fw + shift;
		xd->xshift = 0; xd->yshift = shift;
	    } else {
		dd->top = 0.0;
		xd->showHeight = dd->bottom = ih;
		xd->showWidth = iw0*fh;
		shift = (iw - xd->showWidth)/2.0;
		dd->left = shift;
		dd->right = iw0*fh + shift;
		xd->xshift = shift; xd->yshift = 0;
	    }
	    xd->clip = getregion(xd);
	} else if (xd->resizing == 3) {
	    if(iw0 < iw) shift = (iw - iw0)/2.0;
	    else shift = min(0, xd->xshift);
	    dd->left = shift;
	    dd->right = iw0 + shift;
	    xd->xshift = shift;
	    gchangescrollbar(xd->gawin, HWINSB, max(-shift,0)/SF,
			     xd->origWidth/SF - 1, xd->windowWidth/SF, 0);
	    if(ih0 < ih) shift = (ih - ih0)/2.0;
	    else shift = min(0, xd->yshift);
	    dd->top = shift;
	    dd->bottom = ih0 + shift;
	    xd->yshift = shift;
	    gchangescrollbar(xd->gawin, VWINSB, max(-shift,0)/SF,
			     xd->origHeight/SF - 1, xd->windowHeight/SF, 0);
	    xd->showWidth = xd->origWidth + min(0, xd->xshift);
	    xd->showHeight = xd->origHeight + min(0,  xd->yshift);
	}
	xd->resize = FALSE;
	if (xd->kind == SCREEN) {
	    del(xd->bm);
	    xd->bm = newbitmap(iw, ih, getdepth(xd->gawin));
	    if (!xd->bm) {
		R_ShowMessage("Insufficient memory for resize. Killing device");
		KillDevice(GetDevice(devNumber((DevDesc*) dd)));
	    }
	    gfillrect(xd->gawin, xd->outcolor, getrect(xd->gawin));
	    gfillrect(xd->bm, xd->outcolor, getrect(xd->bm));
	}
    }
    UNPROTECT(1);
}

	/********************************************************/
	/* device_NewPage is called whenever a new plot requires*/
	/* a new page.  a new page might mean just clearing the	*/
	/* device (as in this case) or moving to a new page	*/
	/* (e.g., postscript)					*/
	/********************************************************/

static void GA_NewPage(int fill, double gamma, NewDevDesc *dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    char buf[512];

    xd->npage++;
    if ((xd->kind == PRINTER) && xd->needsave)
	nextpage(xd->gawin);
    if ((xd->kind == METAFILE) && xd->needsave) {
	if (strlen(xd->filename) == 0)
	    error("A clipboard metafile can store only one figure.");
	else {
	    del(xd->gawin);
	    sprintf(buf, xd->filename, xd->npage);
	    xd->gawin = newmetafile(buf, xd->w, xd->h);
	}
    }
    if ((xd->kind == PNG || xd->kind == JPEG || xd->kind == BMP)
	&& xd->needsave) {
	SaveAsBitmap(dd);
	sprintf(buf, xd->filename, xd->npage);
	if ((xd->fp = fopen(buf, "wb")) == NULL)
	    error("Unable to open file `%s' for writing", buf);
    }
    if (xd->kind == SCREEN) {
#ifdef PLOTHISTORY
	if (xd->recording && xd->needsave)
	    AddtoPlotHistory(dd->savedSnapshot, 0);
	if (xd->replaying)
	    xd->needsave = FALSE;
	else
	    xd->needsave = TRUE;
#endif
    }
    xd->bg = fill;
    if (!R_OPAQUE(xd->bg))
	xd->bgcolor = xd->canvascolor;
    else
	xd->bgcolor = GArgb(xd->bg, gamma);
    if (xd->kind != SCREEN) {
	xd->needsave = TRUE;
	xd->clip = getrect(xd->gawin);
	if(R_OPAQUE(xd->bg) || xd->kind == PNG ||
	   xd->kind == BMP || xd->kind == JPEG )
	    DRAW(gfillrect(_d, R_OPAQUE(xd->bg) ? xd->bgcolor : PNG_TRANS,
			   xd->clip));
	if(xd->kind == PNG) xd->pngtrans = ggetpixel(xd->gawin, pt(0,0));
    } else {
	xd->clip = getregion(xd);
	DRAW(gfillrect(_d, xd->bgcolor, xd->clip));
    }
}

	/********************************************************/
	/* device_Close is called when the device is killed	*/
	/* this function is responsible for destroying any 	*/
	/* device-specific resources that were created in	*/
	/* device_Open and for FREEing the device-specific	*/
	/* parameters structure					*/
	/********************************************************/

static void GA_Close(NewDevDesc *dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (xd->kind==SCREEN) {
	hide(xd->gawin);
	del(xd->bm);
    } else if ((xd->kind == PNG) || (xd->kind == JPEG) || (xd->kind == BMP)) {
      SaveAsBitmap(dd);
    }
    del(xd->font);
    del(xd->gawin);
/*
 * this is needed since the GraphApp delayed clean-up
 * ,i.e, I want free all resources NOW
*/
    doevent();
    free(xd);
}

	/********************************************************/
	/* device_Activate is called when a device becomes the 	*/
	/* active device.  in this case it is used to change the*/
	/* title of a window to indicate the active status of 	*/
	/* the device to the user.  not all device types will 	*/
	/* do anything						*/
	/********************************************************/

static unsigned char title[20] = "R Graphics";

static void GA_Activate(NewDevDesc *dd)
{
    char  t[50];
    char  num[3];
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (xd->replaying || (xd->kind!=SCREEN))
	return;
    strcpy(t, (char *) title);
    strcat(t, ": Device ");
    sprintf(num, "%i", devNumber((DevDesc*) dd) + 1);
    strcat(t, num);
    strcat(t, " (ACTIVE)");
    settext(xd->gawin, t);
}

	/********************************************************/
	/* device_Deactivate is called when a device becomes	*/
	/* inactive.  in this case it is used to change the 	*/
	/* title of a window to indicate the inactive status of */
	/* the device to the user.  not all device types will	*/
	/* do anything						*/
	/********************************************************/

static void GA_Deactivate(NewDevDesc *dd)
{
    char  t[50];
    char  num[3];
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (xd->replaying || (xd->kind != SCREEN))
	return;
    strcpy(t, (char *) title);
    strcat(t, ": Device ");
    sprintf(num, "%i", devNumber((DevDesc*) dd) + 1);
    strcat(t, num);
    strcat(t, " (inactive)");
    settext(xd->gawin, t);
}

	/********************************************************/
	/* device_Rect should have the side-effect that a 	*/
	/* rectangle is drawn with the given locations for its 	*/
	/* opposite corners.  the border of the rectangle	*/
	/* should be in the given "fg" colour and the rectangle	*/
	/* should be filled with the given "bg" colour		*/
	/* if "fg" is NA_INTEGER then no border should be drawn */
	/* if "bg" is NA_INTEGER then the rectangle should not 	*/
	/* be filled						*/
	/* the locations are in an arbitrary coordinate system	*/
	/* and this function is responsible for converting the	*/
	/* locations to DEVICE coordinates using GConvert	*/
	/********************************************************/

static void GA_Rect(double x0, double y0, double x1, double y1,
		     int col, int fill, double gamma, int lty, double lwd,
		     NewDevDesc *dd)
{
    int   tmp;
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    rect  r;

    /* These in-place conversions are ok */
    TRACEDEVGA("rect");

    if (x0 > x1) {
	tmp = x0;
	x0 = x1;
	x1 = tmp;
    }
    if (y0 > y1) {
	tmp = y0;
	y0 = y1;
	y1 = tmp;
    }
    r = rect((int) x0, (int) y0, (int) x1 - (int) x0, (int) y1 - (int) y0);
    if (R_OPAQUE(fill)) {
	SetColor(fill, gamma, dd);
	DRAW(gfillrect(_d, xd->fgcolor, r));

    }
    if (R_OPAQUE(col)) {
	SetColor(col, gamma, dd);
	SetLinetype(lty, lwd, dd);
	DRAW(gdrawrect(_d, xd->lwd, xd->lty, xd->fgcolor, r, 0));
    }
}

	/********************************************************/
	/* device_Circle should have the side-effect that a	*/
	/* circle is drawn, centred at the given location, with */
	/* the given radius.  the border of the circle should be*/
	/* drawn in the given "col", and the circle should be	*/
	/* filled with the given "border" colour.		*/
	/* if "col" is NA_INTEGER then no border should be drawn*/
	/* if "border" is NA_INTEGER then the circle should not */
	/* be filled						*/
	/* the location is in arbitrary coordinates and the 	*/
	/* function is responsible for converting this to	*/
	/* DEVICE coordinates.  the radius is given in DEVICE	*/
	/* coordinates						*/
	/********************************************************/

static void GA_Circle(double x, double y, double r,
		       int col, int fill, double gamma, int lty, double lwd,
		       NewDevDesc *dd)
{
    int   ir, ix, iy;
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    rect  rr;

    TRACEDEVGA("circle");
#ifdef OLD
    ir = ceil(r);
#else
    ir = floor(r + 0.5);
#endif
    if (ir < 1) ir = 1;
    /* In-place conversion ok */

    ix = (int) x;
    iy = (int) y;
    rr = rect(ix - ir, iy - ir, 2 * ir, 2 * ir);
    if (R_OPAQUE(fill)) {
	SetColor(fill, gamma, dd);
	DRAW(gfillellipse(_d, xd->fgcolor, rr));
    }
    if (R_OPAQUE(col)) {
	SetLinetype(lty, lwd, dd);
	SetColor(col, gamma, dd);
	DRAW(gdrawellipse(_d, xd->lwd, xd->fgcolor, rr, 0));
    }
}

	/********************************************************/
	/* device_Line should have the side-effect that a single*/
	/* line is drawn (from x1,y1 to x2,y2)			*/
	/* x1, y1, x2, and y2 are in arbitrary coordinates and	*/
	/* the function is responsible for converting them to	*/
	/* DEVICE coordinates using GConvert			*/
	/********************************************************/

static void GA_Line(double x1, double y1, double x2, double y2,
		     int col, double gamma, int lty, double lwd,
		     NewDevDesc *dd)
{
    int   xx1, yy1, xx2, yy2;
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    /* In-place conversion ok */
    TRACEDEVGA("line");
    xx1 = (int) x1;
    yy1 = (int) y1;
    xx2 = (int) x2;
    yy2 = (int) y2;

    SetColor(col, gamma, dd),
    SetLinetype(lty, lwd, dd);
    if (R_OPAQUE(xd->fgcolor))
	DRAW(gdrawline(_d, xd->lwd, xd->lty, xd->fgcolor,
		       pt(xx1, yy1), pt(xx2, yy2), 0));
}

	/********************************************************/
	/* device_Polyline should have the side-effect that a	*/
	/* series of line segments are drawn using the given x	*/
	/* and y values						*/
	/* the x and y values are in arbitrary coordinates and	*/
	/* the function is responsible for converting them to	*/
	/* DEVICE coordinates using GConvert			*/
	/********************************************************/

static void GA_Polyline(int n, double *x, double *y,
			    int col, double gamma, int lty, double lwd,
			    NewDevDesc *dd)
{
    char *vmax = vmaxget();
    point *p = (point *) R_alloc(n, sizeof(point));
    double devx, devy;
    int   i;
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    TRACEDEVGA("pl");
    for (i = 0; i < n; i++) {
	devx = x[i];
	devy = y[i];
	p[i].x = (int) devx;
	p[i].y = (int) devy;
    }
    if (R_OPAQUE(col)) {
	SetColor(col, gamma, dd);
	SetLinetype(lty, lwd, dd);
	DRAW(gdrawpolyline(_d, xd->lwd, xd->lty, xd->fgcolor, p, n, 0, 0));
    }
    vmaxset(vmax);
}

	/********************************************************/
	/* device_Polygon should have the side-effect that a 	*/
	/* polygon is drawn using the given x and y values	*/
	/* the polygon border should be drawn in the "fg" 	*/
	/* colour and filled with the "bg" colour		*/
	/* if "fg" is NA_INTEGER don't draw the border		*/
	/* if "bg" is NA_INTEGER don't fill the polygon		*/
	/* the x and y values are in arbitrary coordinates and 	*/
	/* the function is responsible for converting them to 	*/
	/* DEVICE coordinates using GConvert			*/
	/********************************************************/

static void GA_Polygon(int n, double *x, double *y,
			int col, int fill, double gamma, int lty, double lwd,
			NewDevDesc *dd)
{
    char *vmax = vmaxget();
    point *points;
    double devx, devy;
    int   i;
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    TRACEDEVGA("plg");
    points = (point *) R_alloc(n , sizeof(point));
    if (!points)
	return;
    for (i = 0; i < n; i++) {
	devx = x[i];
	devy = y[i];
	points[i].x = (int) (devx);
	points[i].y = (int) (devy);
    }
    if (R_OPAQUE(fill)) {
	SetColor(fill, gamma, dd);
	DRAW(gfillpolygon(_d, xd->fgcolor, points, n));
    }
    if (R_OPAQUE(col)) {
	SetColor(col, gamma, dd);
	SetLinetype(lty, lwd, dd);
	DRAW(gdrawpolygon(_d, xd->lwd, xd->lty, xd->fgcolor, points, n, 0 ));
    }
    vmaxset(vmax);
}


	/********************************************************/
	/* device_Text should have the side-effect that the 	*/
	/* given text is drawn at the given location		*/
	/* the text should be rotated according to rot (degrees)*/
	/* the location is in an arbitrary coordinate system	*/
	/* and this function is responsible for converting the	*/
	/* location to DEVICE coordinates using GConvert	*/
	/********************************************************/


static void GA_Text(double x, double y, char *str,
		     double rot, double hadj,
		     int col, double gamma, int font, double cex, double ps,
		     NewDevDesc *dd)
{
    int   size;
    double pixs, xl, yl, rot1;
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    size = cex * ps + 0.5;
    pixs = - 1;
    xl = 0.0;
    yl = -pixs;
    rot1 = rot * DEG2RAD;
    x += -xl * cos(rot1) + yl * sin(rot1);
    y -= -xl * sin(rot1) - yl * cos(rot1);
    SetFont(font, size, rot, dd);
    SetColor(col, gamma, dd);
    if (R_OPAQUE(xd->fgcolor)) {
#ifdef NOCLIPTEXT
	gsetcliprect(xd->gawin, getrect(xd->gawin));
	gdrawstr1(xd->gawin, xd->font, xd->fgcolor, pt(x, y), str, hadj);
	if (xd->kind==SCREEN) {
	    gsetcliprect(xd->bm, getrect(xd->bm));
	    gdrawstr1(xd->bm, xd->font, xd->fgcolor, pt(x, y), str, hadj);
	}
#else
	DRAW(gdrawstr1(_d, xd->font, xd->fgcolor, pt(x, y), str, hadj));
#endif
    }
}

	/********************************************************/
	/* device_Locator should return the location of the next*/
	/* mouse click (in DEVICE coordinates;  GLocator is	*/
	/* responsible for any conversions)			*/
	/* not all devices will do anything (e.g., postscript)	*/
	/********************************************************/

static Rboolean GA_Locator(double *x, double *y, NewDevDesc *dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (xd->kind != SCREEN)
	return FALSE;
    xd->locator = TRUE;
    xd->clicked = 0;
    show(xd->gawin);
    addto(xd->gawin);
    gchangemenubar(xd->mbarloc);
    if (xd->stoploc) {
      show(xd->stoploc);
      show(xd->gawin);
    }
    gchangepopup(xd->gawin, xd->locpopup);
    gsetcursor(xd->gawin, CrossCursor);
    setstatus("Locator is active");
    while (!xd->clicked) {
      /*	SHOW;*/
        WaitMessage();
	R_ProcessEvents();
    }
    addto(xd->gawin);
    gchangemenubar(xd->mbar);
    if (xd->stoploc) {
      hide(xd->stoploc);
      show(xd->gawin);
    }
    gsetcursor(xd->gawin, ArrowCursor);
    gchangepopup(xd->gawin, xd->grpopup);
    addto(xd->gawin);
    setstatus("R Graphics");
    xd->locator = FALSE;
    if (xd->clicked == 1) {
	*x = xd->px;
	*y = xd->py;
	return TRUE;
    } else
	return FALSE;
}

	/********************************************************/
	/* device_Mode is called whenever the graphics engine 	*/
	/* starts drawing (mode=1) or stops drawing (mode=1)	*/
	/* the device is not required to do anything		*/
	/********************************************************/

/* Set Graphics mode - not needed for X11 */
static void GA_Mode(int mode, NewDevDesc *dd)
{
}

	/********************************************************/
	/* i don't know what this is for and i can't find it	*/
	/* being used anywhere, but i'm loath to kill it in	*/
	/* case i'm missing something important			*/
	/********************************************************/

/* Hold the Picture Onscreen - not needed for X11 */
static void GA_Hold(NewDevDesc *dd)
{
}


	/********************************************************/
	/* the device-driver entry point is given a device 	*/
	/* description structure that it must set up.  this 	*/
	/* involves several important jobs ...			*/
	/* (1) it must ALLOCATE a new device-specific parameters*/
	/* structure and FREE that structure if anything goes	*/
	/* wrong (i.e., it won't report a successful setup to	*/
	/* the graphics engine (the graphics engine is NOT	*/
	/* responsible for allocating or freeing device-specific*/
	/* resources or parameters)				*/
	/* (2) it must initialise the device-specific resources */
	/* and parameters (mostly done by calling device_Open)	*/
	/* (3) it must initialise the generic graphical 	*/
	/* parameters that are not initialised by GInit (because*/
	/* only the device knows what values they should have)	*/
	/* see Graphics.h for the official list of these	*/
	/* (4) it may reset generic graphics parameters that	*/
	/* have already been initialised by GInit (although you	*/
	/* should know what you are doing if you do this)	*/
	/* (5) it must attach the device-specific parameters	*/
        /* structure to the device description structure	*/
	/* e.g., dd->deviceSpecific = (void *) xd;		*/
	/* (6) it must FREE the overall device description if 	*/
	/* it wants to bail out to the top-level		*/
	/* the graphics engine is responsible for allocating 	*/
	/* the device description and freeing it in most cases	*/
	/* but if the device driver freaks out it needs to do 	*/
	/* the clean-up itself					*/
	/********************************************************/



Rboolean GADeviceDriver(NewDevDesc *dd, char *display, double width,
			double height, double pointsize,
			Rboolean recording, int resize, int canvas,
			double gamma)
{
    /* if need to bail out with some sort of "error" then */
    /* must free(dd) */

    int   ps;
    gadesc *xd;
    rect  rr;
    int a=0, d=0, w=0;

    /* allocate new device description */
    if (!(xd = (gadesc *) malloc(sizeof(gadesc))))
	return FALSE;

    /* from here on, if need to bail out with "error", must also */
    /* free(xd) */

    /* Font will load at first use  */

    ps = pointsize;
    if (ps < 6 || ps > 24)
	ps = 12;
    ps = 2 * (ps / 2);
    xd->fontface = -1;
    xd->fontsize = -1;
    xd->basefontsize = ps ;
    dd->startfont = 1;
    dd->startps = ps;
    dd->startlty = LTY_SOLID;
    dd->startgamma = gamma;

    /* Start the Device Driver and Hardcopy.  */

    if (!GA_Open(dd, xd, display, width, height, recording, resize, canvas,
		 gamma)) {
	free(xd);
	return FALSE;
    }
    dd->deviceSpecific = (void *) xd;
    /* Set up Data Structures  */

    dd->newDevStruct = 1;

    dd->open = GA_Open;
    dd->close = GA_Close;
    dd->activate = GA_Activate;
    dd->deactivate = GA_Deactivate;
    dd->size = GA_Size;
    dd->newPage = GA_NewPage;
    dd->clip = GA_Clip;
    dd->strWidth = GA_StrWidth;
    dd->text = GA_Text;
    dd->rect = GA_Rect;
    dd->circle = GA_Circle;
    dd->line = GA_Line;
    dd->polyline = GA_Polyline;
    dd->polygon = GA_Polygon;
    dd->locator = GA_Locator;
    dd->mode = GA_Mode;
    dd->hold = GA_Hold;
    dd->metricInfo = GA_MetricInfo;

    /* set graphics parameters that must be set by device driver */
    /* Window Dimensions in Pixels */
    rr = getrect(xd->gawin);
    dd->left = (xd->kind == PRINTER) ? rr.x : 0;	/* left */
    dd->right = dd->left + rr.width;	/* right */
    dd->top = (xd->kind == PRINTER) ? rr.y : 0;	/* top */
    dd->bottom = dd->top + rr.height;	/* bottom */

    if (resize == 3) { /* might have got a shrunken window */
	int iw = width/pixelWidth(NULL) + 0.5,
	    ih = height/pixelHeight(NULL) + 0.5;
	xd->origWidth = dd->right = iw;
	xd->origHeight = dd->bottom = ih;
    }

    /* Nominal Character Sizes in Pixels */
    gcharmetric(xd->gawin, xd->font, -1, &a, &d, &w);
    dd->cra[0] = w * xd->rescale_factor;
    dd->cra[1] = (a + d) * xd->rescale_factor;
    /* Set basefont to full size: now allow for initial re-scale */
    xd->wanteddpi = xd->truedpi * xd->rescale_factor;

    /* Character Addressing Offsets */
    /* These are used to plot a single plotting character */
    /* so that it is exactly over the plotting point */

    dd->xCharOffset = 0.50;
    dd->yCharOffset = 0.40;
    dd->yLineBias = 0.1;

    /* Inches per raster unit */

    if (R_finite(user_xpinch) && user_xpinch > 0.0)
	dd->ipr[0] = 1.0/user_xpinch;
    else
	dd->ipr[0] = pixelWidth(xd->gawin);
    if (R_finite(user_ypinch) && user_ypinch > 0.0)
	dd->ipr[1] = 1.0/user_ypinch;
    else
	dd->ipr[1] = pixelHeight(xd->gawin);

    /* Device capabilities */
    /* Clipping is problematic for X11 */
    /* Graphics is clipped, text is not */

    dd->canResizePlot= TRUE;
    dd->canChangeFont= FALSE;
    dd->canRotateText= TRUE;
    dd->canResizeText= TRUE;
    dd->canClip= TRUE;
    dd->canHAdj = 1; /* 0, 0.5, 1 */
    dd->canChangeGamma = TRUE;

    /* initialise device description (most of the work */
    /* has been done in GA_Open) */

    xd->resize = (resize == 3);
    xd->locator = FALSE;
    dd->displayListOn = TRUE;
    if (RConsole && (xd->kind!=SCREEN)) show(RConsole);
    return TRUE;
}

SEXP do_saveDevga(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP filename, type;
    char *fn, *tp, display[512];
    int device;
    NewDevDesc* dd;

    checkArity(op, args);
    device = asInteger(CAR(args));
    if(device < 1 || device > NumDevices())
	errorcall(call, "invalid device number");
    dd = ((GEDevDesc*) GetDevice(device - 1))->dev;
    if(!dd) errorcall(call, "invalid device");
    filename = CADR(args);
    if (!isString(filename) || LENGTH(filename) != 1)
	errorcall(call, "invalid filename argument");
    fn = CHAR(STRING_ELT(filename, 0));
    fixslash(fn);
    type = CADDR(args);
    if (!isString(type) || LENGTH(type) != 1)
	errorcall(call, "invalid type argument");
    tp = CHAR(STRING_ELT(type, 0));

    if(!strcmp(tp, "png")) {
	SaveAsPng(dd, fn);
    } else if (!strcmp(tp,"bmp")) {
        SaveAsBmp(dd,fn);
    } else if(!strcmp(tp, "jpeg") || !strcmp(tp,"jpg")) {
      /*Default quality suggested in libjpeg*/
        SaveAsJpeg(dd, 75, fn);
    } else if (!strcmp(tp, "wmf")) {
	sprintf(display, "win.metafile:%s", fn);
	SaveAsWin(dd, display);
    } else if (!strcmp(tp, "ps")) {
	SaveAsPostscript(dd, fn);
    } else if (!strcmp(tp, "pdf")) {
	SaveAsPDF(dd, fn);
    } else
	errorcall(call, "unknown type");
    return R_NilValue;
}


/* Rbitmap  */
#define BITMAP_DLL_NAME "\\BIN\\RBITMAP.DLL\0"
typedef int (*R_SaveAsBitmap)();
static R_SaveAsBitmap R_SaveAsPng, R_SaveAsJpeg, R_SaveAsBmp;
static int RbitmapAlreadyLoaded = 0;
static HINSTANCE hRbitmapDll;

static int Load_Rbitmap_Dll()
{
    if (!RbitmapAlreadyLoaded) {
	char szFullPath[PATH_MAX];
	strcpy(szFullPath, R_HomeDir());
	strcat(szFullPath, BITMAP_DLL_NAME);
	if (((hRbitmapDll = LoadLibrary(szFullPath)) != NULL) &&
	    ((R_SaveAsPng=
	      (R_SaveAsBitmap)GetProcAddress(hRbitmapDll, "R_SaveAsPng"))
	     != NULL) &&
	    ((R_SaveAsBmp=
	      (R_SaveAsBitmap)GetProcAddress(hRbitmapDll, "R_SaveAsBmp"))
	     != NULL) &&
	    ((R_SaveAsJpeg=
	      (R_SaveAsBitmap)GetProcAddress(hRbitmapDll, "R_SaveAsJpeg"))
	     != NULL)) {
	    RbitmapAlreadyLoaded = 1;
	} else {
	    if (hRbitmapDll != NULL) FreeLibrary(hRbitmapDll);
	    RbitmapAlreadyLoaded= -1;
	}
    }
    return (RbitmapAlreadyLoaded>0);
}

void UnLoad_Rbitmap_Dll()
{
    if (RbitmapAlreadyLoaded) FreeLibrary(hRbitmapDll);
    RbitmapAlreadyLoaded = 0;
}

static unsigned long privategetpixel(void *d,int i, int j)
{
    return ggetpixel((bitmap)d,pt(j,i));
}

/* This is the device version */
static void SaveAsBitmap(NewDevDesc *dd)
{
    rect r;
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    r = ggetcliprect(xd->gawin);
    gsetcliprect(xd->gawin, getrect(xd->gawin));
    if(xd->fp) {
	if (xd->kind==PNG)
	    R_SaveAsPng(xd->gawin, xd->windowWidth, xd->windowHeight,
			privategetpixel, 0, xd->fp,
			R_OPAQUE(xd->bg) ? 0 : xd->pngtrans) ;
	else if (xd->kind==JPEG)
	    R_SaveAsJpeg(xd->gawin, xd->windowWidth, xd->windowHeight,
			 privategetpixel, 0, xd->quality, xd->fp) ;
	else
	    R_SaveAsBmp(xd->gawin, xd->windowWidth, xd->windowHeight,
			privategetpixel, 0, xd->fp);
	fclose(xd->fp);
    }
    gsetcliprect(xd->gawin, r);
    xd->fp = NULL;
}

/* These are the menu item versions */
static void SaveAsPng(NewDevDesc *dd,char *fn)
{
    FILE *fp;
    rect r;
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    if (!Load_Rbitmap_Dll()) {
	R_ShowMessage("Impossible to load Rbitmap.dll");
	return;
    }
    if ((fp=fopen(fn, "wb")) == NULL) {
	char msg[MAX_PATH+32];

	strcpy(msg, "Impossible to open ");
	strncat(msg, fn, MAX_PATH);
	R_ShowMessage(msg);
	return;
    }
    r = ggetcliprect(xd->bm);
    gsetcliprect(xd->bm, getrect(xd->bm));
    R_SaveAsPng(xd->bm, xd->windowWidth, xd->windowHeight,
		privategetpixel, 0, fp, 0) ;
    /* R_OPAQUE(xd->bg) ? 0 : xd->canvascolor) ; */
    gsetcliprect(xd->bm, r);
    fclose(fp);
}

static void SaveAsJpeg(NewDevDesc *dd,int quality,char *fn)
{
    FILE *fp;
    rect r;
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    if (!Load_Rbitmap_Dll()) {
	R_ShowMessage("Impossible to load Rbitmap.dll");
	return;
    }
    if ((fp=fopen(fn,"wb")) == NULL) {
	char msg[MAX_PATH+32];
	strcpy(msg, "Impossible to open ");
	strncat(msg, fn, MAX_PATH);
	R_ShowMessage(msg);
	return;
    }
    r = ggetcliprect(xd->bm);
    gsetcliprect(xd->bm, getrect(xd->bm));
    R_SaveAsJpeg(xd->bm,xd->windowWidth, xd->windowHeight,
		 privategetpixel, 0, quality, fp) ;
    gsetcliprect(xd->bm, r);
    fclose(fp);
}


static void SaveAsBmp(NewDevDesc *dd,char *fn)
{
    FILE *fp;
    rect r;
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    if (!Load_Rbitmap_Dll()) {
	R_ShowMessage("Impossible to load Rbitmap.dll");
	return;
    }
    if ((fp=fopen(fn, "wb")) == NULL) {
	char msg[MAX_PATH+32];

	strcpy(msg, "Impossible to open ");
	strncat(msg, fn, MAX_PATH);
	R_ShowMessage(msg);
	return;
    }
    r = ggetcliprect(xd->bm);
    gsetcliprect(xd->bm, getrect(xd->bm));
    R_SaveAsBmp(xd->bm, xd->windowWidth, xd->windowHeight,
		privategetpixel, 0, fp) ;
    gsetcliprect(xd->bm, r);
    fclose(fp);
}

#include "Startup.h"
extern UImode CharacterMode;

SEXP do_bringtotop(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int dev;
    GEDevDesc *gdd;
    gadesc *xd;

    checkArity(op, args);
    dev = asInteger(CAR(args));
    if(dev == -1) { /* console */
	if(CharacterMode == RGui) BringToTop(RConsole);
    } else {
	if(dev < 1 || dev > NumDevices() || dev == NA_INTEGER)
	    errorcall(call, "invalid value of `which'");
	gdd = (GEDevDesc *) GetDevice(dev - 1);
	if(!gdd) errorcall(call, "invalid device");
	xd = (gadesc *) gdd->dev->deviceSpecific;
	if(!xd) errorcall(call, "invalid device");
	BringToTop(xd->gawin);
    }
    return R_NilValue;
}
