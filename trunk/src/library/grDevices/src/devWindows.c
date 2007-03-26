/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2003  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004        The R Foundation
 *  Copyright (C) 2004-7      The R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/*--- Device Driver for Windows; this file started from
 *  src/unix/X11/devX11.c
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Graphics.h>
#include <Rdevices.h>
#include <Fileio.h>
#include <stdio.h>
#include "opt.h"
#include "graphapp/ga.h"
#include "graphapp/stdimg.h"
#include "console.h"
#include "rui.h"
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include "devWindows.h"
#include "grDevices.h"
#ifdef ENABLE_NLS
#define G_(String) libintl_dgettext("RGui", String)
#define GN_(String) gettext_noop (String)
#else /* not NLS */
#define G_(String) (String)
#define GN_(String) String
#endif

static
Rboolean GADeviceDriver(NewDevDesc *dd, char *display, double width,
			double height, double pointsize,
			Rboolean recording, int resize, int bg, int canvas,
			double gamma, int xpos, int ypos, Rboolean buffered,
			SEXP psenv, Rboolean restoreConsole);


/* a colour used to represent the background on png if transparent
   NB: used as RGB and BGR
*/
#define PNG_TRANS 0xd6d3d6

/* these really are globals: per machine, not per window */
static double user_xpinch = 0.0, user_ypinch = 0.0;

static void GAsetunits(double xpinch, double ypinch)
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
#define CLIP if (xd->clip.width>0) gsetcliprect(_d,xd->clip)

static drawing _d;

#define DRAW(a) {if(xd->kind != SCREEN) {_d=xd->gawin; CLIP; a;} else {_d=xd->bm; CLIP; a; if(!xd->buffered) {_d=xd->gawin; CLIP; a;} _d=xd->bm; CLIP; a;}}

#define SHOW  if(xd->kind==SCREEN) {gbitblt(xd->gawin,xd->bm,pt(0,0),getrect(xd->bm));GALastUpdate=GetTickCount();}
#define SH if(xd->kind==SCREEN && xd->buffered) GA_Timer(xd)


#define SF 20  /* scrollbar resolution */

        /********************************************************/
	/* Each driver can have its own device-specic graphical */
	/* parameters and resources.  these should be wrapped	*/
	/* in a structure (gadesc in devWindows.h)              */
	/* and attached to the overall device description via 	*/
	/* the dd->deviceSpecific pointer			*/
	/* NOTE that there are generic graphical parameters	*/
	/* which must be set by the device driver, but are	*/
	/* common to all device types (see Graphics.h)		*/
	/* so go in the GPar structure rather than this device- */
	/* specific structure					*/
	/********************************************************/

static rect getregion(gadesc *xd)
{
    rect r = getrect(xd->bm);
    r.x += max(0, xd->xshift);
    r.y += max(0, xd->yshift);
    r.width = min(r.width, xd->showWidth);
    r.height = min(r.height, xd->showHeight);
    return r;
}

/* Update the screen 100ms after last plotting call or 500ms after last
   update */

static UINT TimerNo = 0;
static gadesc *GA_xd;
static DWORD GALastUpdate = 0;

static VOID CALLBACK
GA_timer_proc(HWND hwnd, UINT message, UINT tid, DWORD time)
{
    if ((message != WM_TIMER) || tid != TimerNo || !GA_xd) return;
    gbitblt(GA_xd->gawin, GA_xd->bm, pt(0,0), getrect(GA_xd->bm));
    GALastUpdate = time;
}

static void GA_Timer(gadesc *xd)
{
    DWORD now = GetTickCount();
    if(TimerNo != 0) KillTimer(0, TimerNo);
    if(now > GALastUpdate + xd->timesince) {
	gbitblt(xd->gawin, xd->bm, pt(0,0), getrect(xd->bm));
	GALastUpdate = now;
    } else {
	GA_xd = xd;
	TimerNo = SetTimer(0, 0, (UINT) xd->timeafter, GA_timer_proc);
    }
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
		      R_GE_gcontext *gc,
		      NewDevDesc *dd);
static void GA_Clip(double x0, double x1, double y0, double y1,
		     NewDevDesc *dd);
static void GA_Close(NewDevDesc *dd);
static void GA_Deactivate(NewDevDesc *dd);
static SEXP GA_getEvent(SEXP eventRho, char* prompt);
static void GA_Hold(NewDevDesc *dd);
static Rboolean GA_Locator(double *x, double *y, NewDevDesc *dd);
static void GA_Line(double x1, double y1, double x2, double y2,
		    R_GE_gcontext *gc,
		    NewDevDesc *dd);
static void GA_MetricInfo(int c,
			  R_GE_gcontext *gc,
			  double* ascent, double* descent,
			  double* width, NewDevDesc *dd);
static void GA_Mode(int mode, NewDevDesc *dd);
static void GA_NewPage(R_GE_gcontext *gc,
		       NewDevDesc *dd);
static void GA_Polygon(int n, double *x, double *y,
		       R_GE_gcontext *gc,
		       NewDevDesc *dd);
static void GA_Polyline(int n, double *x, double *y,
			R_GE_gcontext *gc,
			NewDevDesc *dd);
static void GA_Rect(double x0, double y0, double x1, double y1,
		    R_GE_gcontext *gc,
		    NewDevDesc *dd);
static void GA_Size(double *left, double *right,
		    double *bottom, double *top,
		    NewDevDesc *dd);
static void GA_Resize(NewDevDesc *dd);
static double GA_StrWidth(char *str,
			  R_GE_gcontext *gc,
			  NewDevDesc *dd);
static void GA_Text(double x, double y, char *str,
		    double rot, double hadj,
		    R_GE_gcontext *gc,
		    NewDevDesc *dd);
static Rboolean GA_Open(NewDevDesc*, gadesc*, char*, double, double,
			Rboolean, int, int, double, int, int, int);
static Rboolean GA_NewFrameConfirm();


	/********************************************************/
	/* end of list of required device driver actions 	*/
	/********************************************************/

Rboolean winNewFrameConfirm();

	/* Support Routines */

static double pixelHeight(drawing  d);
static double pixelWidth(drawing d);
static void SetColor(int, double, NewDevDesc *);
static void SetFont(char*, int, int, double, NewDevDesc *);
static int Load_Rbitmap_Dll();
void UnLoad_Rbitmap_Dll();
static void SaveAsPng(NewDevDesc *dd, char *fn);
static void SaveAsJpeg(NewDevDesc *dd, int quality, char *fn);
static void SaveAsBmp(NewDevDesc *dd, char *fn);
static void SaveAsBitmap(NewDevDesc *dd, int res);

static void PrivateCopyDevice(NewDevDesc *dd, NewDevDesc *ndd, char *name)
{
    GEDevDesc* gdd;
    int saveDev = curDevice();
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    gsetcursor(xd->gawin, WatchCursor);
    gsetVar(install(".Device"),
	    mkString(name), R_BaseEnv);
    gdd = GEcreateDevDesc(ndd);
    addDevice((DevDesc*) gdd);
    GEcopyDisplayList(devNumber((DevDesc*) dd));
    KillDevice((DevDesc*) gdd);
    selectDevice(saveDev);
/*    KillDevice(GetDevice(devNumber((DevDesc*) ndd))); */
    gsetcursor(xd->gawin, ArrowCursor);
    show(xd->gawin);
}

static void SaveAsWin(NewDevDesc *dd, char *display, Rboolean restoreConsole)
{
    NewDevDesc *ndd = (NewDevDesc *) calloc(1, sizeof(NewDevDesc));
    GEDevDesc* gdd = (GEDevDesc*) GetDevice(devNumber((DevDesc*) dd));
    if (!ndd) {
	R_ShowMessage(_("Not enough memory to copy graphics window"));
	return;
    }
    if(!R_CheckDeviceAvailableBool()) {
	free(ndd);
	R_ShowMessage(_("No device available to copy graphics window"));
	return;
    }

    ndd->displayList = R_NilValue;
    if (GADeviceDriver(ndd, display,
		       fromDeviceWidth(toDeviceWidth(1.0, GE_NDC, gdd),
				       GE_INCHES, gdd),
		       fromDeviceHeight(toDeviceHeight(-1.0, GE_NDC, gdd),
					GE_INCHES, gdd),
		       ((gadesc*) dd->deviceSpecific)->basefontsize,
		       0, 1, White, White, 1, NA_INTEGER, NA_INTEGER, FALSE,
		       R_GlobalEnv, restoreConsole))
        PrivateCopyDevice(dd, ndd, display);
}


static void SaveAsPostscript(NewDevDesc *dd, char *fn)
{
    SEXP s;
    NewDevDesc *ndd = (NewDevDesc *) calloc(1, sizeof(NewDevDesc));
    GEDevDesc* gdd = (GEDevDesc*) GetDevice(devNumber((DevDesc*) dd));
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    char family[256], encoding[256], paper[256], bg[256], fg[256],
	**afmpaths = NULL;

    if (!ndd) {
	R_ShowMessage(_("Not enough memory to copy graphics window"));
	return;
    }
    if(!R_CheckDeviceAvailableBool()) {
	free(ndd);
	R_ShowMessage(_("No device available to copy graphics window"));
	return;
    }


    ndd->displayList = R_NilValue;

    /* Set default values and pad with zeroes ... */
    strncpy(family, "Helvetica", 256);
    strcpy(encoding, "ISOLatin1.enc");
    strncpy(paper, "default", 256);
    strncpy(bg, "transparent", 256);
    strncpy(fg, "black", 256);
    /* and then try to get it from .PostScript.Options */
    s = findVar(install(".PostScript.Options"), xd->psenv);
    if ((s != R_UnboundValue) && (s != R_NilValue)) {
	SEXP names = getAttrib(s, R_NamesSymbol);
	int i,done;
	for (i=0, done=0; (done<4) && (i<length(s)) ; i++) {
	    if(!strcmp("family", CHAR(STRING_ELT(names, i)))) {
		strncpy(family, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)), 255);
		done += 1;
	    }
	    if(!strcmp("paper", CHAR(STRING_ELT(names, i)))) {
		strncpy(paper, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)), 255);
		done += 1;
	    }
	    if(!strcmp("bg", CHAR(STRING_ELT(names, i)))) {
		strncpy(bg, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)), 255);
		done += 1;
	    }
	    if(!strcmp("fg", CHAR(STRING_ELT(names, i)))) {
		strncpy(fg, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)), 255);
		done += 1;
	    }
	}
    }
    if (PSDeviceDriver(ndd, fn, paper, family, afmpaths, encoding,
                       bg, fg,
		       fromDeviceWidth(toDeviceWidth(1.0, GE_NDC, gdd),
				       GE_INCHES, gdd),
		       fromDeviceHeight(toDeviceHeight(-1.0, GE_NDC, gdd),
					GE_INCHES, gdd),
		       (double)0, ((gadesc*) dd->deviceSpecific)->basefontsize,
		       0, 1, 0, "", "R Graphics Output", R_NilValue, "rgb"))
	/* horizontal=F, onefile=F, pagecentre=T, print.it=F */
	PrivateCopyDevice(dd, ndd, "postscript");
}


static void SaveAsPDF(NewDevDesc *dd, char *fn)
{
    SEXP s;
    NewDevDesc *ndd = (NewDevDesc *) calloc(1, sizeof(NewDevDesc));
    GEDevDesc* gdd = (GEDevDesc*) GetDevice(devNumber((DevDesc*) dd));
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    char family[256], encoding[256], bg[256], fg[256], **afmpaths = NULL;

    if (!ndd) {
	R_ShowMessage(_("Not enough memory to copy graphics window"));
	return;
    }
    if(!R_CheckDeviceAvailableBool()) {
	free(ndd);
	R_ShowMessage(_("No device available to copy graphics window"));
	return;
    }

    ndd->displayList = R_NilValue;

    /* Set default values... */
    s = findVar(install(".PostScript.Options"), xd->psenv);
    strncpy(family, "Helvetica", 256);
    strcpy(encoding, "ISOLatin1.enc");
    strncpy(bg, "transparent", 256);
    strncpy(fg, "black", 256);
    /* and then try to get it from .PostScript.Options */
    if ((s != R_UnboundValue) && (s != R_NilValue)) {
	SEXP names = getAttrib(s, R_NamesSymbol);
	int i,done;
	for (i=0, done=0; (done<3) && (i<length(s)) ; i++) {
	    if(!strcmp("family", CHAR(STRING_ELT(names, i)))) {
		strncpy(family, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)),255);
		done += 1;
	    }
	    if(!strcmp("bg", CHAR(STRING_ELT(names, i)))) {
		strncpy(bg, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)), 255);
		done += 1;
	    }
	    if(!strcmp("fg", CHAR(STRING_ELT(names, i)))) {
		strncpy(fg, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)), 255);
		done += 1;
	    }
	}
    }
    if (PDFDeviceDriver(ndd, fn, "special", family, afmpaths, encoding,
                        bg, fg,
			fromDeviceWidth(toDeviceWidth(1.0, GE_NDC, gdd),
					GE_INCHES, gdd),
			fromDeviceHeight(toDeviceHeight(-1.0, GE_NDC, gdd),
					 GE_INCHES, gdd),
			((gadesc*) dd->deviceSpecific)->basefontsize,
			1, 0, "R Graphics Output", R_NilValue, 1, 4))
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
	    for (i = 0; i < fontnum; i++) free(fontname[i]);
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
    xd->fontfamily[0] = '\0';
    xd->font = gnewfont(xd->gawin, fontname[0], fontstyle[0],
			xd->fontsize, 0.0);
    if (!xd->font) {
	xd->usefixed= TRUE;
	xd->font = xd->fixedfont = FixedFont;
	if (!xd->fixedfont)
	    return 0;
    }
    return 1;
}

/* Return a non-relocatable copy of a string */

static char *SaveFontSpec(SEXP sxp, int offset)
{
    char *s;
    if(!isString(sxp) || length(sxp) <= offset)
	error(_("Invalid font specification"));
    s = R_alloc(strlen(CHAR(STRING_ELT(sxp, offset)))+1, sizeof(char));
    strcpy(s, CHAR(STRING_ELT(sxp, offset)));
    return s;
}

/*
 * Take the fontfamily from a gcontext (which is device-independent)
 * and convert it into a Windows-specific font description using
 * the Windows font database (see src/library/grDevices/R/windows/windows.R)
 *
 * IF gcontext fontfamily is empty ("")
 * OR IF can't find gcontext fontfamily in font database
 * THEN return NULL
 */
static char* translateFontFamily(char* family) {
    SEXP graphicsNS, windowsenv, fontdb, fontnames;
    int i, nfonts;
    char* result = NULL;
    PROTECT_INDEX xpi;

    PROTECT(graphicsNS = R_FindNamespace(ScalarString(mkChar("grDevices"))));
    PROTECT_WITH_INDEX(windowsenv = findVar(install(".Windowsenv"),
					    graphicsNS), &xpi);
    if(TYPEOF(windowsenv) == PROMSXP)
	REPROTECT(windowsenv = eval(windowsenv, graphicsNS), xpi);
    PROTECT(fontdb = findVar(install(".Windows.Fonts"), windowsenv));
    PROTECT(fontnames = getAttrib(fontdb, R_NamesSymbol));
    nfonts = LENGTH(fontdb);
    if (strlen(family) > 0) {
	int found = 0;
	for (i = 0; i < nfonts && !found; i++) {
	    char* fontFamily = CHAR(STRING_ELT(fontnames, i));
	    if (strcmp(family, fontFamily) == 0) {
		found = 1;
		result = SaveFontSpec(VECTOR_ELT(fontdb, i), 0);
	    }
	}
	if (!found)
	    warning(_("Font family not found in Windows font database"));
    }
    UNPROTECT(4);
    return result;
}

/* Set the font size and face */
/* If the font of this size and at that the specified */
/* rotation is not present it is loaded. */
/* 0 = plain text, 1 = bold */
/* 2 = oblique, 3 = bold-oblique */

#define SMALLEST 1

static void SetFont(char *family, int face, int size, double rot,
		    NewDevDesc *dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    char* fontfamily;

    if (face < 1 || face > fontnum)
	face = 1;
    if (size < SMALLEST) size = SMALLEST;
    if (!xd->usefixed &&
	(size != xd->fontsize || face != xd->fontface ||
	 rot != xd->fontangle || strcmp(family, xd->fontfamily))) {
        del(xd->font); doevent();
	/*
	 * If specify family = "", get family from face via Rdevga
	 *
	 * If specify a family and a face in 1 to 4 then get
	 * that family (mapped through WindowsFonts()) and face.
	 *
	 * If specify face > 4 then get font from face via Rdevga
	 * (whether specifed family or not).
	 */
	fontfamily = translateFontFamily(family);
	if (fontfamily && face <= 4) {
	    xd->font = gnewfont(xd->gawin,
				fontfamily, fontstyle[face - 1],
				size, rot);
	    if (xd->font)
                strcpy(xd->fontfamily, family);
	} else {
            xd->font = gnewfont(xd->gawin,
				fontname[face - 1], fontstyle[face - 1],
				size, rot);
	}
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
 *      In this driver, done in graphapp/gdraw.c
 */

static void SetLineStyle(R_GE_gcontext *gc, NewDevDesc *dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    xd->lty = gc->lty;
    if(xd->lwdscale != 1.0)
	/* will round to nearest integer */
	xd->lwd = xd->lwdscale * gc->lwd + 0.5;
    else xd->lwd = gc->lwd;
    if(xd->lwd < 1) xd->lwd = 1;
    switch (gc->lend) {
    case GE_ROUND_CAP:
	xd->lend = PS_ENDCAP_ROUND;
	break;
    case GE_BUTT_CAP:
	xd->lend = PS_ENDCAP_FLAT;
	break;
    case GE_SQUARE_CAP:
	xd->lend = PS_ENDCAP_SQUARE;
	break;
    default:
	error(_("Invalid line end"));
    }
    switch (gc->ljoin) {
    case GE_ROUND_JOIN:
	xd->ljoin = PS_JOIN_ROUND;
	break;
    case GE_MITRE_JOIN:
	xd->ljoin = PS_JOIN_MITER;
	break;
    case GE_BEVEL_JOIN:
	xd->ljoin = PS_JOIN_BEVEL;
	break;
    default:
	error(_("Invalid line join"));
    }

    xd->lmitre = gc->lmitre;
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
	    /* avoid trying to replay list if there has been no drawing */
	    if(gdd->dirty) {
		xd->replaying = TRUE;
		GEplayDisplayList(gdd);
		xd->replaying = FALSE;
	    }
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

	if (!xd->locator && !xd->confirmation && !dd->gettingEvent)
	    return;
	if (button & LeftButton) {
	    int useBeep = xd->locator && asLogical(GetOption(install("locatorBell"),
					      R_BaseEnv));
	    if(useBeep) gabeep();
	    xd->clicked = 1;
	    xd->px = pt.x;
	    xd->py = pt.y;
	} else
	    xd->clicked = 2;
	if (dd->gettingEvent) { 
	    xd->eventResult = doMouseEvent(xd->eventRho, dd, meMouseDown, 
	    			button, pt.x, pt.y);
            if (xd->buffered) SHOW;	
        }
    }
}

static void HelpMouseMove(window w, int button, point pt)
{
    if (AllDevicesKilled) return;
    {
	NewDevDesc *dd = (NewDevDesc *) getdata(w);
	gadesc *xd = (gadesc *) dd->deviceSpecific;

	if (dd->gettingEvent) {
	    xd->eventResult = doMouseEvent(xd->eventRho, dd, meMouseMove, 
	    			button, pt.x, pt.y);
	    if (xd->buffered) SHOW;	    			
	}
    }
}

static void HelpMouseUp(window w, int button, point pt)
{
    if (AllDevicesKilled) return;
    {
	NewDevDesc *dd = (NewDevDesc *) getdata(w);
	gadesc *xd = (gadesc *) dd->deviceSpecific;

	if (dd->gettingEvent) { 
	    xd->eventResult = doMouseEvent(xd->eventRho, dd, meMouseUp,
	    			button, pt.x, pt.y);
	    if (xd->buffered) SHOW;	
	}
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

static void menunextplot(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    xd->clicked = 2;
}

static void menufilebitmap(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    char *fn;
    /* the following use a private hook to set the default extension */
    if (m == xd->mpng) {
      setuserfilter(G_("Png files (*.png)\0*.png\0\0"));
      fn = askfilesave(G_("Portable network graphics file"), "|.png");
    } else if (m == xd->mbmp) {
      setuserfilter(G_("Windows bitmap files (*.bmp)\0*.bmp\0\0"));
      fn = askfilesave(G_("Windows bitmap file"), "|.bmp");
    } else {
      setuserfilter(G_("Jpeg files (*.jpeg,*jpg)\0*.jpeg;*.jpg\0\0"));
      fn = askfilesave(G_("Jpeg file"), "|.jpg");
    }
    if (!fn) return;
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

    setuserfilter(G_("Postscript files (*.ps)\0*.ps\0All files (*.*)\0*.*\0\0"));
    fn = askfilesave(G_("Postscript file"), "|.ps");
    if (!fn) return;
    SaveAsPostscript(dd, fn);
}


static void menupdf(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    char  *fn;

    setuserfilter(G_("PDF files (*.pdf)\0*.pdf\0All files (*.*)\0*.*\0\0"));
    fn = askfilesave(G_("PDF file"), "|.pdf");
    if (!fn) return;
    SaveAsPDF(dd, fn);
}


static void menuwm(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    char  display[550], *fn;

    setuserfilter(G_("Enhanced metafiles (*.emf)\0*.emf\0All files (*.*)\0*.*\0\0"));
    fn = askfilesave(G_("Enhanced metafiles"), "|.emf");
    if (!fn) return;
    if(strlen(fn) > 512) {
	askok(G_("file path selected is too long: only 512 bytes are allowed"));
	return;
    }
    sprintf(display, "win.metafile:%s", fn);
    SaveAsWin(dd, display, TRUE);
}


static void menuclpwm(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    SaveAsWin(dd, "win.metafile", TRUE);
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

static void menustayontop(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    BringToTop(xd->gawin, 2);
}

static void menuprint(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    SaveAsWin(dd, "win.print:", TRUE);
}

static void menuclose(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    HelpClose(xd->gawin);
}

static void grpopupact(control m)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (ismdi())
    	disable(xd->grmenustayontop);
    else {
    	if (isTopmost(xd->gawin))
     	    check(xd->grmenustayontop);
    	else
    	    uncheck(xd->grmenustayontop);
    }
}

/* plot history */

/* NB: this puts .SavedPlots in .GlobalEnv */
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
                       R_ShowMessage(_("plot history seems corrupted"));\
                       return;}
#define pMOVE(a) {pCURRENTPOS+=a;\
                  if(pCURRENTPOS<0) pCURRENTPOS=0;\
                  if(pCURRENTPOS>pNUMPLOTS-1) pCURRENTPOS=pNUMPLOTS-1;\
                  Replay(dd,vDL);SETDL;}
#define pEXIST ((vDL!=R_UnboundValue) && (vDL!=R_NilValue))
#define pMUSTEXIST if(!pEXIST){R_ShowMessage(_("no plot history!"));return;}




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
    PROTECT(snapshot);
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
	R_ShowMessage(G_("No plot to replace!"));
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
	    vDL = findVar(install(".SavedPlots"), R_GlobalEnv);
	    /* may have changed vDL pointer */
	}
	pMOVE((xd->needsave) ? 0 : -1);
    }
}

static void menugrclear(control m)
{
    defineVar(install(".SavedPlots"), R_NilValue, R_GlobalEnv);
}

static void menugvar(control m)
{
    SEXP  vDL;
    char *v = askstring(G_("Variable name"), "");
    NewDevDesc *dd = (NewDevDesc *) getdata(m);

    if (!v)
	return;
    vDL = findVar(install(v), R_GlobalEnv);
    if (!pEXIST || !pNUMPLOTS) {
	R_ShowMessage(G_("Variable doesn't exist or doesn't contain any plots!"));
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
    v = askstring(G_("Name of variable to save to"), "");
    if (!v)
	return;
    defineVar(install(v), vDL, R_GlobalEnv);
}
/* end of plot history */

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

static R_KeyName getKeyName(int key) 
{
    if (F1 <= key && key <= F10) return knF1 + key - F1 ;
    else switch (key) {
	case LEFT: return knLEFT;
	case UP:   return knUP;
	case RIGHT:return knRIGHT;
	case DOWN: return knDOWN;
	case PGUP: return knPGUP;
	case PGDN: return knPGDN;
	case END:  return knEND;
	case HOME: return knHOME;
	case INS:  return knINS;
	case DEL:  return knDEL;	
	default:   return knUNKNOWN;
    }
}    
    			     
static void CHelpKeyIn(control w, int key)
{
    NewDevDesc *dd = (NewDevDesc *) getdata(w);
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    
    R_KeyName keyname;
    
    if (dd->gettingEvent) {
    	keyname = getKeyName(key);
    	if (keyname > knUNKNOWN) {
    	    xd->eventResult = doKeybd(xd->eventRho, dd, keyname, NULL);
	    if (xd->buffered) SHOW;	
	}
    } else {
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
	  case ENTER:
	    xd->enterkey = TRUE;
	    break;
	}
    }
}

__declspec(dllimport) extern int UserBreak;

static void NHelpKeyIn(control w, int key)
{
    char keyname[7];
    
    NewDevDesc *dd = (NewDevDesc *) getdata(w);
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    
    if (dd->gettingEvent) {
	if (0 < key && key < 32) {
	    strcpy(keyname, "ctrl- ");
	    keyname[5] = (char) (key + 'A' - 1);
    	} else {
    	    keyname[0] = (char) key;
	    keyname[1] = '\0';
	}
	xd->eventResult = doKeybd(xd->eventRho, dd, knUNKNOWN, keyname);
	if (xd->buffered) SHOW;	
    } else {
	if (xd->replaying) return;
	switch (key) {
	  case '\n':  /* ENTER has been translated to newline */
	    xd->enterkey = TRUE;
	    return;
	  case ESC:
	    UserBreak = TRUE;
	    return;
	}
	if (ggetkeystate() != CtrlKey)
	    return;
	key = 'A' + key - 1;
	if (key == 'C')
	    menuclpbm(xd->mclpbm);
	if (dd->displayList == R_NilValue)
	    return;
	if (key == 'W')
	    menuclpwm(xd->mclpwm);
	else if (key == 'P')
	    menuprint(xd->mprint);
    }
}

static void mbarf(control m)
{
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
		  Rboolean recording, int resize, int xpos, int ypos)
{
    menu  m;
    int   iw, ih;
    int   cw, ch;
    double dw, dw0, dh, d;
    char buf[100];

    xd->kind = SCREEN;
    if (R_FINITE(user_xpinch) && user_xpinch > 0.0)
	dw = dw0 = (int) (w * user_xpinch);
    else
	dw = dw0 = (int) (w / pixelWidth(NULL));
    if (R_FINITE(user_ypinch) && user_ypinch > 0.0)
	dh = (int) (w * user_ypinch);
    else
	dh = (int) (h / pixelHeight(NULL));

    if (ismdi() && !isiconic(RFrame)) {
	cw = RgetMDIwidth();
	ch = RgetMDIheight();
    } else {
	cw = devicewidth(NULL);
	ch = deviceheight(NULL);
    }

    if (resize != 3) {
	if ((dw / cw) > 0.85) {
	    d = dh / dw;
	    dw = 0.85 * cw;
	    dh = d * dw;
	}
	if ((dh / ch) > 0.85) {
	    d = dw / dh;
	    dh = 0.85 * ch;
	    dw = d * dh;
	}
    } else {
	dw = min(dw, 0.85*cw);
	dh = min(dh, 0.85*ch);
    }
    iw = dw + 0.5;
    ih = dh + 0.5;
    if (resize == 2) xd->rescale_factor = dw/dw0;
    {
	int grx, gry;
	grx = (xpos == NA_INTEGER) ? Rwin_graphicsx : xpos;
	gry = (ypos == NA_INTEGER) ? Rwin_graphicsy : ypos;
	if (grx < 0) grx = cw - iw + grx;
	if (gry < 0) gry = ch - ih + gry;
	if (!(xd->gawin = newwindow("R Graphics",
				    rect(grx, gry, iw, ih),
				    Document | StandardWindow | Menubar |
				    VScrollbar | HScrollbar)))
	    return 0;
    }
    gchangescrollbar(xd->gawin, VWINSB, 0, ih/SF-1, ih/SF, 0);
    gchangescrollbar(xd->gawin, HWINSB, 0, iw/SF-1, iw/SF, 0);

    addto(xd->gawin);
    gsetcursor(xd->gawin, ArrowCursor);
    if (ismdi()) {
	int btsize = 24;
	rect r = rect(2, 2, btsize, btsize);
	control bt, tb;

	MCHECK(tb = newtoolbar(btsize + 4));
	gsetcursor(tb, ArrowCursor);
	addto(tb);

	MCHECK(bt = newtoolbutton(cam_image, r, menuclpwm));
	MCHECK(addtooltip(bt, G_("Copy to the clipboard as a metafile")));
	gsetcursor(bt, ArrowCursor);
	setdata(bt, (void *) dd);
	r.x += (btsize + 6);

	MCHECK(bt = newtoolbutton(print_image, r, menuprint));
	MCHECK(addtooltip(bt, G_("Print")));
	gsetcursor(bt, ArrowCursor);
	setdata(bt, (void *) dd);
	r.x += (btsize + 6);

	MCHECK(bt = newtoolbutton(console_image, r, menuconsole));
	MCHECK(addtooltip(bt, G_("Return focus to Console")));
	gsetcursor(bt, ArrowCursor);
	setdata(bt, (void *) dd);
	r.x += (btsize + 6);

	MCHECK(xd->stoploc = newtoolbutton(stop_image, r, menustop));
	MCHECK(addtooltip(xd->stoploc, G_("Stop locator")));
	gsetcursor(bt, ArrowCursor);
	setdata(xd->stoploc,(void *) dd);
	hide(xd->stoploc);
    } else
	xd->stoploc = NULL;

    /* First we prepare 'locator' menubar and popup */
    addto(xd->gawin);
    MCHECK(xd->mbarloc = newmenubar(NULL));
    MCHECK(newmenu(G_("Stop")));
    MCHECK(m = newmenuitem(G_("Stop locator"), 0, menustop));
    setdata(m, (void *) dd);
    MCHECK(xd->locpopup = newpopup(NULL));
    MCHECK(m = newmenuitem(G_("Stop"), 0, menustop));
    setdata(m, (void *) dd);
    MCHECK(newmenuitem(G_("Continue"), 0, NULL));

    /* Next the 'Click for next plot' menubar */
    MCHECK(xd->mbarconfirm = newmenubar(NULL));
    MCHECK(newmenu(G_("Next")));
    MCHECK(m = newmenuitem(G_("Next plot"), 0, menunextplot));
    setdata(m, (void *) dd);

    /* Normal menubar */
    MCHECK(xd->mbar = newmenubar(mbarf));
    MCHECK(m = newmenu(G_("File")));
    MCHECK(xd->msubsave = newsubmenu(m, G_("Save as")));
    MCHECK(xd->mwm = newmenuitem(G_("Metafile..."), 0, menuwm));
    MCHECK(xd->mps = newmenuitem(G_("Postscript..."), 0, menups));
    MCHECK(xd->mpdf = newmenuitem(G_("PDF..."), 0, menupdf));
    MCHECK(xd->mpng = newmenuitem(G_("Png..."), 0, menufilebitmap));
    MCHECK(xd->mbmp = newmenuitem(G_("Bmp..."), 0, menufilebitmap));
    MCHECK(newsubmenu(xd->msubsave,G_("Jpeg")));
    /* avoid gettext confusion with % */ 
    snprintf(buf, 100, G_("%s quality..."), "50%");
    MCHECK(xd->mjpeg50 = newmenuitem(buf, 0, menufilebitmap));
    snprintf(buf, 100, G_("%s quality..."), "75%");
    MCHECK(xd->mjpeg75 = newmenuitem(buf, 0, menufilebitmap));
    snprintf(buf, 100, G_("%s quality..."), "100%");
    MCHECK(xd->mjpeg100 = newmenuitem(buf, 0, menufilebitmap));
    MCHECK(newsubmenu(m, G_("Copy to the clipboard")));
    MCHECK(xd->mclpbm = newmenuitem(G_("as a Bitmap\tCTRL+C"), 0, menuclpbm));
    MCHECK(xd->mclpwm = newmenuitem(G_("as a Metafile\tCTRL+W"), 0, menuclpwm));
    addto(m);
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(xd->mprint = newmenuitem(G_("Print...\tCTRL+P"), 0, menuprint));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(xd->mclose = newmenuitem(G_("close Device"), 0, menuclose));
    MCHECK(newmenu(G_("History")));
    MCHECK(xd->mrec = newmenuitem(G_("Recording"), 0, menurec));
    if(recording) check(xd->mrec);
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(xd->madd = newmenuitem(G_("Add\tINS"), 0, menuadd));
    MCHECK(xd->mreplace = newmenuitem(G_("Replace"), 0, menureplace));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(xd->mprev = newmenuitem(G_("Previous\tPgUp"), 0, menuprev));
    MCHECK(xd->mnext = newmenuitem(G_("Next\tPgDown"), 0, menunext));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(xd->msvar = newmenuitem(G_("Save to variable..."), 0, menusvar));
    MCHECK(xd->mgvar = newmenuitem(G_("Get from variable..."), 0, menugvar));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(xd->mclear = newmenuitem(G_("Clear history"), 0, menugrclear));
    MCHECK(newmenu(G_("Resize")));
    MCHECK(xd->mR = newmenuitem(G_("R mode"), 0, menuR));
    if(resize == 1) check(xd->mR);
    MCHECK(xd->mfit = newmenuitem(G_("Fit to window"), 0, menufit));
    if(resize == 2) check(xd->mfit);
    MCHECK(xd->mfix = newmenuitem(G_("Fixed size"), 0, menufix));
    if(resize == 3) check(xd->mfix);
    newmdimenu();

    /* Normal popup */
    MCHECK(xd->grpopup = newpopup(grpopupact));
    setdata(xd->grpopup, (void *) dd);
    MCHECK(m = newmenuitem(G_("Copy as metafile"), 0, menuclpwm));
    setdata(m, (void *) dd);
    MCHECK(m = newmenuitem(G_("Copy as bitmap"), 0, menuclpbm));
    setdata(m, (void *) dd);
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(m = newmenuitem(G_("Save as metafile..."), 0, menuwm));
    setdata(m, (void *) dd);
    MCHECK(m = newmenuitem(G_("Save as postscript..."), 0, menups));
    setdata(m, (void *) dd);
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(xd->grmenustayontop = newmenuitem(G_("Stay on top"), 0, menustayontop));
    setdata(xd->grmenustayontop, (void *) dd);
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(m = newmenuitem(G_("Print..."), 0, menuprint));
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
    if (ismdi() && !(RguiMDI & RW_TOOLBAR)) toolbar_hide();
    show(xd->gawin); /* twice, for a Windows bug */
    show(xd->gawin);
    BringToTop(xd->gawin, 0);
    sethit(xd->gawin, devga_sbf);
    setresize(xd->gawin, HelpResize);
    setredraw(xd->gawin, HelpExpose);
    setmousedown(xd->gawin, HelpMouseClick);
    setmousemove(xd->gawin, HelpMouseMove);
    setmouseup(xd->gawin, HelpMouseUp);
    setkeydown(xd->gawin, NHelpKeyIn);
    setkeyaction(xd->gawin, CHelpKeyIn);
    setclose(xd->gawin, HelpClose);
    xd->recording = recording;
    xd->replaying = FALSE;
    xd->resizing = resize;

    dd->getEvent = GA_getEvent;
    
    dd->canGenMouseDown = TRUE;
    dd->canGenMouseMove = TRUE;
    dd->canGenMouseUp = TRUE;
    dd->canGenKeybd = TRUE;
    dd->gettingEvent = FALSE;
    
    xd->eventRho = NULL;
    xd->eventResult = NULL;

    return 1;
}

static Rboolean GA_Open(NewDevDesc *dd, gadesc *xd, char *dsp,
			double w, double h, Rboolean recording,
			int resize, int canvascolor, double gamma,
			int xpos, int ypos, int bg)
{
    rect  rr;
    char buf[600]; /* allow for pageno formats */

    if (!fontinitdone)
	RFontInit();

    /* Foreground and Background Colors */
    xd->bg = dd->startfill = bg; /* 0xffffffff; transparent */
    xd->col = dd->startcol = R_RGB(0, 0, 0);

    xd->fgcolor = Black;
    xd->bgcolor = xd->canvascolor = GArgb(canvascolor, gamma);
    xd->outcolor = myGetSysColor(COLOR_APPWORKSPACE);
    xd->rescale_factor = 1.0;
    xd->fast = 1;  /* Use 'cosmetic pens' if available.
		      Overridden for printers and metafiles.
		    */
    xd->xshift = xd->yshift = 0;
    xd->npage = 0;
    if (!dsp[0]) {
	if (!setupScreenDevice(dd, xd, w, h, recording, resize, xpos, ypos))
	    return FALSE;
    } else if (!strncmp(dsp, "win.print:", 10)) {
	xd->kind = PRINTER;
	xd->fast = 0; /* use scalable line widths */
	xd->gawin = newprinter(MM_PER_INCH * w, MM_PER_INCH * h, &dsp[10]);
	if (!xd->gawin)
	    return FALSE;
    } else if (!strncmp(dsp, "png:", 4) || !strncmp(dsp,"bmp:", 4)) {
	xd->res_dpi = (xpos == NA_INTEGER) ? 0 : xpos;
	xd->bg = dd->startfill = canvascolor;
	/* was R_RGB(255, 255, 255); white */
        xd->kind = (dsp[0]=='p') ? PNG : BMP;
	if(strlen(dsp+4) >= 512) error(_("filename too long in %s() call"),
				       (dsp[0]=='p') ? "png" : "bmp");
	strcpy(xd->filename, dsp+4);
	if (!Load_Rbitmap_Dll()) {
	    warning(_("Unable to load Rbitmap.dll"));
	    return FALSE;
	}

	if (w < 20 && h < 20)
	    warning(_("'width=%d, height=%d' are unlikely values in pixels"),
		    (int)w, (int) h);
	/*
	  Observe that given actual graphapp implementation 256 is
	  irrelevant,i.e., depth of the bitmap is that of graphic card
	  if required depth > 1
	*/
	if ((xd->gawin = newbitmap(w, h, 256)) == NULL) {
	    warning(_("Unable to allocate bitmap"));
	    return FALSE;
	}
	snprintf(buf, 600, xd->filename, 1);
	if ((xd->fp = R_fopen(buf, "wb")) == NULL) {
	    del(xd->gawin);
	    warning(_("Unable to open file '%s' for writing"), buf);
	    return FALSE;
	}
    } else if (!strncmp(dsp, "jpeg:", 5)) {
        char *p = strchr(&dsp[5], ':');
	xd->res_dpi = (xpos == NA_INTEGER) ? 0 : xpos;
	xd->bg = dd->startfill = canvascolor;
        xd->kind = JPEG;
	if (!p) return FALSE;
	if (!Load_Rbitmap_Dll()) {
	    warning(_("Unable to load Rbitmap.dll"));
	    return FALSE;
	}
	*p = '\0';
	xd->quality = atoi(&dsp[5]);
	*p = ':' ;
	if(strlen(p+1) >= 512) error(_("filename too long in jpeg() call"));
	strcpy(xd->filename, p+1);
	if (w < 20 && h < 20)
	    warning(_("'width=%d, height=%d' are unlikely values in pixels"),
		    (int)w, (int) h);
	if((xd->gawin = newbitmap(w, h, 256)) == NULL) {
	    warning(_("Unable to allocate bitmap"));
	    return FALSE;
	}
	snprintf(buf, 600, xd->filename, 1);
	if ((xd->fp = R_fopen(buf, "wb")) == NULL) {
	    del(xd->gawin);
	    warning(_("Unable to open file '%s' for writing"), buf);
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
	if(ld > ls && strlen(&dsp[ls + 1]) >= 512)
	    error(_("filename too long in win.metafile() call"));
	strcpy(xd->filename, (ld > ls) ? &dsp[ls + 1] : "");
	snprintf(buf, 600, xd->filename, 1);
	xd->w = MM_PER_INCH * w;
	xd->h =  MM_PER_INCH * h;
	xd->gawin = newmetafile(buf, MM_PER_INCH * w, MM_PER_INCH * h);
	xd->kind = METAFILE;
	xd->fast = 0; /* use scalable line widths */
	if (!xd->gawin) {
	    if(ld > ls)
		warning(_("Unable to open metafile '%s' for writing"), buf);
	    else
		warning(_("Unable to open clipboard to write metafile"));
	    return FALSE;
	}
    }
    xd->truedpi = devicepixelsy(xd->gawin);
    if ((xd->kind == PNG) || (xd->kind == JPEG) || (xd->kind == BMP))
      xd->wanteddpi = 72 ;
    else
      xd->wanteddpi = xd->truedpi;
    if (!SetBaseFont(xd)) {
	warning(_("can't find any fonts"));
	del(xd->gawin);
	if (xd->kind == SCREEN) del(xd->bm);
	return FALSE;
    }
    xd->lwdscale = xd->truedpi/96.0; /* matches ps/pdf */
    if(xd->lwdscale < 1.0) xd->lwdscale = 1.0; /* at least one pixel */
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

static double GA_StrWidth(char *str,
			  R_GE_gcontext *gc,
			  NewDevDesc *dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    double a;
    int   size = gc->cex * gc->ps + 0.5;

    SetFont(gc->fontfamily, gc->fontface, size, 0.0, dd);
#ifdef SUPPORT_UTF8
    if(gc->fontface != 5)
	a = (double) gwstrwidth(xd->gawin, xd->font, str);
    else
#endif
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
	/* Passing c == 0 gets font information.
	   In a mbcslocale for a non-symbol font 
	   we pass a Unicode point, otherwise an 8-bit char, and
	   we don't care which for a 7-bit char.
	 */

static void GA_MetricInfo(int c,
			  R_GE_gcontext *gc,
			  double* ascent, double* descent,
			  double* width, NewDevDesc *dd)
{
    int   a, d, w;
    int   size = gc->cex * gc->ps + 0.5;
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    SetFont(gc->fontfamily, gc->fontface, size, 0.0, dd);
#ifdef SUPPORT_MBCS
    if(mbcslocale && gc->fontface != 5 && c > 127)
	gwcharmetric(xd->gawin, xd->font, c, &a, &d, &w);
    else 
#endif
	gcharmetric(xd->gawin, xd->font, c, &a, &d, &w);
    /* Some Windows systems report that space has height and depth,
       so we have a kludge.  Note that 32 is space in symbol font too */
    if(c == 32) {
	*ascent  = 0.0;
	*descent = 0.0;
    } else {
	*ascent  = (double) a;
	*descent = (double) d;
    }
    *width   = (double) w;
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
    *left = dd->left;
    *top = dd->top;
    *right = dd->right;
    *bottom = dd->bottom;
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
		R_ShowMessage(_("Insufficient memory for resize. Killing device"));
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

static void GA_NewPage(R_GE_gcontext *gc,
		       NewDevDesc *dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    xd->npage++;
    if ((xd->kind == PRINTER) && xd->needsave)
	nextpage(xd->gawin);
    if ((xd->kind == METAFILE) && xd->needsave) {
	char buf[600];
	if (strlen(xd->filename) == 0)
	    error(_("A clipboard metafile can store only one figure."));
	else {
	    del(xd->gawin);
	    snprintf(buf, 600, xd->filename, xd->npage);
	    xd->gawin = newmetafile(buf, xd->w, xd->h);
	    if(!xd->gawin)
		error(_("metafile '%s' could not be created"), buf);
	}
    }
    if ((xd->kind == PNG || xd->kind == JPEG || xd->kind == BMP)
	&& xd->needsave) {
	char buf[600];
	SaveAsBitmap(dd, xd->res_dpi);
	snprintf(buf, 600, xd->filename, xd->npage);
	if ((xd->fp = R_fopen(buf, "wb")) == NULL)
	    error(_("Unable to open file '%s' for writing"), buf);
    }
    if (xd->kind == SCREEN) {
        if(xd->buffered) SHOW;
	if (xd->recording && xd->needsave)
	    AddtoPlotHistory(dd->savedSnapshot, 0);
	if (xd->replaying)
	    xd->needsave = FALSE;
	else
	    xd->needsave = TRUE;
    }
    xd->bg = gc->fill;
    if (!R_OPAQUE(xd->bg))
	xd->bgcolor = xd->canvascolor;
    else
	xd->bgcolor = GArgb(xd->bg, gc->gamma);
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
    SH;
}

#ifdef UNUSED
static void deleteGraphMenus(int devnum)
{
    char prefix[15];

    sprintf(prefix, "$Graph%i", devnum);
    windelmenus(prefix);
}
#endif

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
    GEDevDesc *gdd = (GEDevDesc *) GetDevice(devNumber((DevDesc*) dd));
    SEXP vDL;

    if (dd->onExit) {
	dd->onExit(dd);
	UserBreak = TRUE;
    }
    
    if (xd->kind == SCREEN) {
	if(xd->recording) {
	    AddtoPlotHistory(GEcreateSnapshot(gdd), 0);
	    /* May have changed vDL, so can't use GETDL above */	
	    vDL = findVar(install(".SavedPlots"), R_GlobalEnv);
	    pCURRENTPOS++; /* so PgUp goes to the last saved plot
			      when a windows() device is opened */
	}
	hide(xd->gawin);
	
	del(xd->bm);
	/* If this is the active device and buffered, shut updates off */
	if (xd == GA_xd) GA_xd = NULL;
    } else if ((xd->kind == PNG) || (xd->kind == JPEG) || (xd->kind == BMP)) {
	SaveAsBitmap(dd, xd->res_dpi);
    } 
    del(xd->font);
    del(xd->gawin);
/*
 * this is needed since the GraphApp delayed clean-up
 * ,i.e, I want free all resources NOW
 */
    /* I think the concern is rather to run all pending events on the
       device (but also on the console and others) */
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
		    R_GE_gcontext *gc,
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
    if (R_OPAQUE(gc->fill)) {
	SetColor(gc->fill, gc->gamma, dd);
	DRAW(gfillrect(_d, xd->fgcolor, r));

    }
    if (R_OPAQUE(gc->col)) {
	SetColor(gc->col, gc->gamma, dd);
	SetLineStyle(gc, dd);
	DRAW(gdrawrect(_d, xd->lwd, xd->lty, xd->fgcolor, r, 0, xd->lend, 
		       xd->ljoin, xd->lmitre));
    }
    SH;
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
		      R_GE_gcontext *gc,
		      NewDevDesc *dd)
{
    int   ir, ix, iy;
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    rect  rr;

    TRACEDEVGA("circle");
    ir = floor(r + 0.5);
    if (ir < 1) ir = 1;
    /* In-place conversion ok */

    ix = (int) x;
    iy = (int) y;
    rr = rect(ix - ir, iy - ir, 2 * ir, 2 * ir);
    if (R_OPAQUE(gc->fill)) {
	SetColor(gc->fill, gc->gamma, dd);
	DRAW(gfillellipse(_d, xd->fgcolor, rr));
    }
    if (R_OPAQUE(gc->col)) {
	SetLineStyle(gc, dd);
	SetColor(gc->col, gc->gamma, dd);
	DRAW(gdrawellipse(_d, xd->lwd, xd->fgcolor, rr, 0, xd->lend, 
			  xd->ljoin, xd->lmitre));
    }
    SH;
}

	/********************************************************/
	/* device_Line should have the side-effect that a single*/
	/* line is drawn (from x1,y1 to x2,y2)			*/
	/* x1, y1, x2, and y2 are in arbitrary coordinates and	*/
	/* the function is responsible for converting them to	*/
	/* DEVICE coordinates using GConvert			*/
	/********************************************************/

static void GA_Line(double x1, double y1, double x2, double y2,
		    R_GE_gcontext *gc,
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

    if (R_OPAQUE(gc->col)) {
	SetColor(gc->col, gc->gamma, dd);
	SetLineStyle(gc, dd);
	DRAW(gdrawline(_d, xd->lwd, xd->lty, xd->fgcolor,
		       pt(xx1, yy1), pt(xx2, yy2), 0, xd->lend, 
			  xd->ljoin, xd->lmitre));
	SH;
    }
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
			R_GE_gcontext *gc,
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
    if (R_OPAQUE(gc->col)) {
	SetColor(gc->col, gc->gamma, dd);
	SetLineStyle(gc, dd);
	DRAW(gdrawpolyline(_d, xd->lwd, xd->lty, xd->fgcolor, p, n, 0, 0, 
			   xd->lend, xd->ljoin, xd->lmitre));
    }
    vmaxset(vmax);
    SH;
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
		       R_GE_gcontext *gc,
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
    if (R_OPAQUE(gc->fill)) {
	SetColor(gc->fill, gc->gamma, dd);
	DRAW(gfillpolygon(_d, xd->fgcolor, points, n));
    }
    if (R_OPAQUE(gc->col)) {
	SetColor(gc->col, gc->gamma, dd);
	SetLineStyle(gc, dd);
	DRAW(gdrawpolygon(_d, xd->lwd, xd->lty, xd->fgcolor, points, n, 0, 
			  xd->lend, xd->ljoin, xd->lmitre));
    }
    vmaxset(vmax);
    SH;
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
		    R_GE_gcontext *gc,
		    NewDevDesc *dd)
{
    int   size;
    double pixs, xl, yl, rot1;
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    size = gc->cex * gc->ps + 0.5;
    pixs = - 1;
    xl = 0.0;
    yl = -pixs;
    rot1 = rot * DEG2RAD;
    x += -xl * cos(rot1) + yl * sin(rot1);
    y -= -xl * sin(rot1) - yl * cos(rot1);
    SetFont(gc->fontfamily, gc->fontface, size, rot, dd);
    SetColor(gc->col, gc->gamma, dd);
    if (R_OPAQUE(gc->col)) {
	if(mbcslocale && gc->fontface != 5) {
	    /* These macros need to be wrapped in braces */
	    DRAW(gwdrawstr1(_d, xd->font, xd->fgcolor, pt(x, y), str, hadj));
	} else {
	    DRAW(gdrawstr1(_d, xd->font, xd->fgcolor, pt(x, y), str, hadj));
	}
    }
    SH;
}

	/********************************************************/
	/* device_Locator should return the location of the next*/
	/* mouse click (in DEVICE coordinates;  GLocator is	*/
	/* responsible for any conversions)			*/
	/* not all devices will do anything (e.g., postscript)	*/
	/********************************************************/

static void donelocator(void *data)
{
    gadesc *xd;
    xd = (gadesc *)data;
    addto(xd->gawin);
    gchangemenubar(xd->mbar);
    if (xd->stoploc) {
      hide(xd->stoploc);
      show(xd->gawin);
    }
    gsetcursor(xd->gawin, ArrowCursor);
    gchangepopup(xd->gawin, xd->grpopup);
    addto(xd->gawin);
    setstatus(_("R Graphics"));
    xd->locator = FALSE;
}

static void GA_onExit(NewDevDesc *dd);

static Rboolean GA_Locator(double *x, double *y, NewDevDesc *dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    RCNTXT cntxt;

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
    setstatus(G_("Locator is active"));

    /* set up a context which will clean up if there's an error */
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_NilValue, R_NilValue,
	         R_NilValue, R_NilValue);
    cntxt.cend = &donelocator;
    cntxt.cenddata = xd;
    xd->cntxt = &cntxt;
    
    /* and an exit handler in case the window gets closed */
    dd->onExit = GA_onExit;
    
    while (!xd->clicked) {
	SH;
        WaitMessage();
	R_ProcessEvents();
    }
    
    dd->onExit = NULL;
    xd->cntxt = NULL;
    
    endcontext(&cntxt);
    donelocator((void *)xd);
    
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


static
Rboolean GADeviceDriver(NewDevDesc *dd, char *display, double width,
			double height, double pointsize,
			Rboolean recording, int resize, int bg, int canvas,
			double gamma, int xpos, int ypos, Rboolean buffered,
			SEXP psenv, Rboolean restoreConsole)
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
    if (ps < 1) 
	ps = 12;
    xd->fontface = -1;
    xd->fontsize = -1;
    xd->basefontsize = ps ;
    dd->startfont = 1;
    dd->startps = ps;
    dd->startlty = LTY_SOLID;
    dd->startgamma = gamma;

    /* Start the Device Driver and Hardcopy.  */

    if (!GA_Open(dd, xd, display, width, height, recording, resize, canvas,
		 gamma, xpos, ypos, bg)) {
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
    xd->newFrameConfirm = GA_NewFrameConfirm;
    xd->cntxt = NULL;

    /* set graphics parameters that must be set by device driver */
    /* Window Dimensions in Pixels */
    rr = getrect(xd->gawin);
    dd->left = (xd->kind == PRINTER) ? rr.x : 0;	/* left */
    dd->right = dd->left + rr.width - 0.0001;	/* right */
    dd->top = (xd->kind == PRINTER) ? rr.y : 0;	/* top */
    dd->bottom = dd->top + rr.height - 0.0001;	/* bottom */

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

    if (R_FINITE(user_xpinch) && user_xpinch > 0.0)
	dd->ipr[0] = 1.0/user_xpinch;
    else
	dd->ipr[0] = pixelWidth(xd->gawin);
    if (R_FINITE(user_ypinch) && user_ypinch > 0.0)
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
    xd->buffered = buffered;
    xd->psenv = psenv;
    {
	SEXP timeouts = GetOption(install("windowsTimeouts"), R_BaseEnv);
	if(isInteger(timeouts)){
	    xd->timeafter = INTEGER(timeouts)[0];
	    xd->timesince = INTEGER(timeouts)[1];
	} else {
	    warning(_("option 'windowsTimeouts' should be integer"));
	    xd->timeafter = 100;
	    xd->timesince = 500;
	}
    }
    xd->newFrameConfirm = GA_NewFrameConfirm;
    dd->displayListOn = (xd->kind == SCREEN);
    if (RConsole && restoreConsole) show(RConsole);
    return TRUE;
}

SEXP savePlot(SEXP args)
{
    SEXP filename, type;
    char *fn, *tp, display[550];
    int device;
    NewDevDesc* dd;
    Rboolean restoreConsole;

    args = CDR(args); /* skip entry point name */
    device = asInteger(CAR(args));
    if(device < 1 || device > NumDevices())
	error(_("invalid device number in savePlot"));
    dd = ((GEDevDesc*) GetDevice(device - 1))->dev;
    if(!dd) error(_("invalid device in savePlot"));
    filename = CADR(args);
    if (!isString(filename) || LENGTH(filename) != 1)
	error(_("invalid filename argument in savePlot"));
    fn = CHAR(STRING_ELT(filename, 0));
    type = CADDR(args);
    if (!isString(type) || LENGTH(type) != 1)
	error(_("invalid type argument in savePlot"));
    tp = CHAR(STRING_ELT(type, 0));
    restoreConsole = asLogical(CADDDR(args));
    
    if(!strcmp(tp, "png")) {
	SaveAsPng(dd, fn);
    } else if (!strcmp(tp,"bmp")) {
        SaveAsBmp(dd,fn);
    } else if(!strcmp(tp, "jpeg") || !strcmp(tp,"jpg")) {
      /*Default quality suggested in libjpeg*/
        SaveAsJpeg(dd, 75, fn);
    } else if (!strcmp(tp, "wmf") || !strcmp(tp, "emf")) {
	if(strlen(fn) > 512) {
	    askok(G_("file path selected is too long: only 512 bytes are allowed"));
	    return R_NilValue;
	}
	sprintf(display, "win.metafile:%s", fn);
	SaveAsWin(dd, display, restoreConsole);
    } else if (!strcmp(tp, "ps") || !strcmp(tp, "eps")) {
	SaveAsPostscript(dd, fn);
    } else if (!strcmp(tp, "pdf")) {
	SaveAsPDF(dd, fn);
    } else
	error(_("unknown type in savePlot"));
    return R_NilValue;
}


/* Rbitmap  */
#define BITMAP_DLL_NAME "\\modules\\Rbitmap.dll\0"
typedef int (*R_SaveAsBitmap)();
static R_SaveAsBitmap R_SaveAsPng, R_SaveAsJpeg, R_SaveAsBmp;

/* next two are in system.c */
#include <R_ext/libextern.h>
LibExtern int RbitmapAlreadyLoaded;
LibExtern HINSTANCE hRbitmapDll;
#undef LibExtern

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

static int png_rows = 0;

static unsigned long privategetpixel2(void *d,int i, int j)
{
    rgb c;
    c = ((rgb *)d)[i*png_rows + j];
    return c;
}

/* This is the device version */
static void SaveAsBitmap(NewDevDesc *dd, int res)
{
    rect r, r2;
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    unsigned char *data;

    r = ggetcliprect(xd->gawin);
    gsetcliprect(xd->gawin, r2 = getrect(xd->gawin));
    if(xd->fp) {
	getbitmapdata2(xd->gawin, &data);
	if(data) {
	    png_rows = r2.width;
	    if (xd->kind == PNG)
		R_SaveAsPng(data, xd->windowWidth, xd->windowHeight,
			    privategetpixel2, 0, xd->fp,
			    R_OPAQUE(xd->bg) ? 0 : xd->pngtrans, res) ;
	    else if (xd->kind == JPEG)
		R_SaveAsJpeg(data, xd->windowWidth, xd->windowHeight,
			     privategetpixel2, 0, xd->quality, xd->fp, res) ;
	    else
		R_SaveAsBmp(data, xd->windowWidth, xd->windowHeight,
			    privategetpixel2, 0, xd->fp, res);
	    free(data);
	} else
	    warning(_("processing of the plot ran out of memory"));
	fclose(xd->fp);
    }
    gsetcliprect(xd->gawin, r);
    xd->fp = NULL;
}

/* These are the menu item versions */
static void SaveAsPng(NewDevDesc *dd,char *fn)
{
    FILE *fp;
    rect r, r2;
    unsigned char *data;
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (!Load_Rbitmap_Dll()) {
	R_ShowMessage(_("Impossible to load Rbitmap.dll"));
	return;
    }
    if ((fp = R_fopen(fn, "wb")) == NULL) {
	char msg[MAX_PATH+32];

	strcpy(msg, "Impossible to open ");
	strncat(msg, fn, MAX_PATH);
	R_ShowMessage(msg);
	return;
    }
    r = ggetcliprect(xd->bm);
    gsetcliprect(xd->bm, r2 = getrect(xd->bm));
    getbitmapdata2(xd->bm, &data);
    if(data) {
	png_rows = r2.width;
	R_SaveAsPng(data, xd->windowWidth, xd->windowHeight,
		    privategetpixel2, 0, fp, 0, 0) ;
	free(data);
    } else
	warning(_("processing of the plot ran out of memory"));
    gsetcliprect(xd->bm, r);
    fclose(fp);
}

static void SaveAsJpeg(NewDevDesc *dd,int quality,char *fn)
{
    FILE *fp;
    rect r, r2;
    unsigned char *data;
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (!Load_Rbitmap_Dll()) {
	R_ShowMessage(_("Impossible to load Rbitmap.dll"));
	return;
    }
    if ((fp = R_fopen(fn,"wb")) == NULL) {
	char msg[MAX_PATH+32];
	strcpy(msg, "Impossible to open ");
	strncat(msg, fn, MAX_PATH);
	R_ShowMessage(msg);
	return;
    }
    r = ggetcliprect(xd->bm);
    gsetcliprect(xd->bm, r2 = getrect(xd->bm));
    getbitmapdata2(xd->bm, &data);
    if(data) {
	png_rows = r2.width;
	R_SaveAsJpeg(data,xd->windowWidth, xd->windowHeight,
		     privategetpixel2, 0, quality, fp, 0) ;
	free(data);
    } else
	warning(_("processing of the plot ran out of memory"));
    gsetcliprect(xd->bm, r);
    fclose(fp);
}


static void SaveAsBmp(NewDevDesc *dd,char *fn)
{
    FILE *fp;
    rect r, r2;
    unsigned char *data;
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (!Load_Rbitmap_Dll()) {
	R_ShowMessage(_("Impossible to load Rbitmap.dll"));
	return;
    }
    if ((fp = R_fopen(fn, "wb")) == NULL) {
	char msg[MAX_PATH+32];

	strcpy(msg, _("Impossible to open "));
	strncat(msg, fn, MAX_PATH);
	R_ShowMessage(msg);
	return;
    }
    r = ggetcliprect(xd->bm);
    gsetcliprect(xd->bm, r2 = getrect(xd->bm));

    getbitmapdata2(xd->bm, &data);
    if(data) {
	png_rows = r2.width;
	R_SaveAsBmp(data, xd->windowWidth, xd->windowHeight,
		    privategetpixel2, 0, fp, 0) ;
	free(data);
    } else
	warning(_("processing of the plot ran out of memory"));
    gsetcliprect(xd->bm, r);
    fclose(fp);
}

/* This is Guido's devga device, ga for GraphApp. */

SEXP devga(SEXP args)
{
    NewDevDesc *dev;
    GEDevDesc* dd;
    char *display, *vmax;
    double height, width, ps, xpinch, ypinch, gamma;
    int recording = 0, resize = 1, bg, canvas, xpos, ypos, buffered;
    Rboolean restoreConsole;
    SEXP sc, psenv;

    vmax = vmaxget();
    args = CDR(args); /* skip entry point name */
    display = CHAR(STRING_ELT(CAR(args), 0)); /* no longer need SaveString */
    args = CDR(args);
    width = asReal(CAR(args));
    args = CDR(args);
    height = asReal(CAR(args));
    args = CDR(args);
    if (width <= 0 || height <= 0)
	error(_("invalid width or height in devWindows"));
    ps = asReal(CAR(args));
    args = CDR(args);
    recording = asLogical(CAR(args));
    if (recording == NA_LOGICAL)
	error(_("invalid value of 'recording' in devWindows"));
    args = CDR(args);
    resize = asInteger(CAR(args));
    if (resize == NA_INTEGER)
	error(_("invalid value of 'resize' in devWindows"));
    args = CDR(args);
    xpinch = asReal(CAR(args));
    args = CDR(args);
    ypinch = asReal(CAR(args));
    args = CDR(args);
    sc = CAR(args);
    if (!isString(sc) && !isInteger(sc) && !isLogical(sc) && !isReal(sc))
	error(_("invalid value of 'canvas' in devWindows"));
    canvas = RGBpar(sc, 0);
    args = CDR(args);
    gamma = asReal(CAR(args));
    args = CDR(args);
    xpos = asInteger(CAR(args));
    args = CDR(args);
    ypos = asInteger(CAR(args));
    args = CDR(args);
    buffered = asLogical(CAR(args));
    if (buffered == NA_LOGICAL)
	error(_("invalid value of 'buffered' in devWindows"));
    args = CDR(args);
    psenv = CAR(args);
    args = CDR(args);
    sc = CAR(args);
    if (!isString(sc) && !isInteger(sc) && !isLogical(sc) && !isReal(sc))
	error(_("invalid value of 'bg' in devWindows"));
    bg = RGBpar(sc, 0);
    args = CDR(args);
    restoreConsole = asLogical(CAR(args));
    
    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	/* Allocate and initialize the device driver data */
	if (!(dev = (NewDevDesc *) calloc(1, sizeof(NewDevDesc))))
	    return 0;
	/* Do this for early redraw attempts */
	dev->displayList = R_NilValue;
	/* Make sure that this is initialised before a GC can occur.
	 * This (and displayList) get protected during GC
	 */
	dev->savedSnapshot = R_NilValue;
	GAsetunits(xpinch, ypinch);
	if (!GADeviceDriver(dev, display, width, height, ps, 
			    (Rboolean)recording, resize, bg, canvas, gamma,
			    xpos, ypos, (Rboolean)buffered, psenv, 
			    restoreConsole)) {
	    free(dev);
	    error(_("unable to start device devWindows"));
	}
	gsetVar(install(".Device"),
		mkString(display[0] ? display : "windows"), R_BaseEnv);
	dd = GEcreateDevDesc(dev);
	addDevice((DevDesc*) dd);
	GEinitDisplayList(dd);
    } END_SUSPEND_INTERRUPTS;
    vmaxset(vmax);
    return R_NilValue;
}

static void GA_onExit(NewDevDesc *dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    
    dd->onExit = NULL;
    xd->confirmation = FALSE;
    dd->gettingEvent = FALSE;
    xd->eventRho = NULL;
    
    if (xd->cntxt) endcontext(xd->cntxt);
    if (xd->locator) donelocator((void *)xd);
    
    addto(xd->gawin);
    gchangemenubar(xd->mbar);
    gchangepopup(xd->gawin, xd->grpopup);
    addto(xd->gawin);
    setstatus(_("R Graphics"));
    GA_Activate(dd);
}

static Rboolean GA_NewFrameConfirm()
{
    char *msg;
    GEDevDesc *dd = GEcurrentDevice();
    gadesc *xd = dd->dev->deviceSpecific;

    if (!xd || xd->kind != SCREEN)
	return FALSE;

    msg = G_("Waiting to confirm page change...");
    xd->confirmation = TRUE;
    xd->clicked = 0;
    xd->enterkey = 0;
    show(xd->gawin);
    addto(xd->gawin);
    gchangemenubar(xd->mbarconfirm);
    gchangepopup(xd->gawin, NULL);
    setstatus(msg);
    R_WriteConsole(msg, strlen(msg));
    R_WriteConsole("\n", 1);
    R_FlushConsole();
    settext(xd->gawin, G_("Click or hit ENTER for next page"));
    dd->dev->onExit = GA_onExit;  /* install callback for cleanup */
    while (!xd->clicked && !xd->enterkey) {
	SH;
        WaitMessage();
	R_ProcessEvents(); /* May not return if user interrupts */
    }
    dd->dev->onExit(dd->dev);

    return TRUE;
}

static SEXP GA_getEvent(SEXP eventRho, char* prompt)
{
    gadesc *xd;
    GEDevDesc *dd = GEcurrentDevice();

    xd = dd->dev->deviceSpecific;
    
    if (xd->eventRho) 
	error(_("recursive use of getGraphicsEvent not supported"));
    xd->eventRho = eventRho;
    
    dd->dev->gettingEvent = TRUE;
    show(xd->gawin);
    addto(xd->gawin);
    gchangemenubar(xd->mbar);
    gchangepopup(xd->gawin, NULL);
    setstatus(prompt);
    Rprintf(prompt, strlen(prompt));
    Rprintf("\n", 1);
    R_FlushConsole();
    settext(xd->gawin, prompt);
    xd->eventResult = NULL;
    dd->dev->onExit = GA_onExit;  /* install callback for cleanup */
    while (!xd->eventResult || xd->eventResult == R_NilValue) {
	SH;
        WaitMessage();
	R_ProcessEvents(); /* May not return if user interrupts */
    }
    dd->dev->onExit(dd->dev);

    return xd->eventResult;
}
