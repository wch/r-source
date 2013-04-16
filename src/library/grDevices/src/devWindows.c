/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2003  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004        The R Foundation
 *  Copyright (C) 2004-13     The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/*--- Device Driver for Windows; this file started from
 *  src/unix/X11/devX11.c
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#define R_USE_PROTOTYPES 1
#include <R_ext/GraphicsEngine.h>

#include <Fileio.h>
#include <stdio.h>
#include "opt.h"
#include "graphapp/ga.h"
#include "graphapp/stdimg.h"
#include "console.h"
#include "rui.h"
#define WIN32_LEAN_AND_MEAN 1
/* Mingw-w64 defines this to be 0x0502 */
#ifndef _WIN32_WINNT
# define _WIN32_WINNT 0x0500
#endif
#include <windows.h>
#include "devWindows.h"
#define DEVWINDOWS 1
#include "grDevices.h"

/* there are conflicts with Rmath.h */
#define imax2		Rf_imax2
#define imin2		Rf_imin2
int	imax2(int, int);
int	imin2(int, int);

#ifdef ENABLE_NLS
#define G_(String) libintl_dgettext("RGui", String)
#define GN_(String) gettext_noop (String)
#else /* not NLS */
#define G_(String) (String)
#define GN_(String) String
#endif

/* from extra.c */
extern size_t Rf_utf8towcs(wchar_t *wc, const char *s, size_t n);

static
Rboolean GADeviceDriver(pDevDesc dd, const char *display, double width,
			double height, double pointsize,
			Rboolean recording, int resize, int bg, int canvas,
			double gamma, int xpos, int ypos, Rboolean buffered,
			SEXP psenv, Rboolean restoreConsole,
			const char *title, Rboolean clickToConfirm,
			Rboolean fillOddEven, const char *family, int quality);


/* a colour used to represent the background on png if transparent
   NB: used as RGB and BGR
*/

#define PNG_TRANS 0xfdfefd

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
	/* used as a template for new drivers			*/
	/********************************************************/

#define MM_PER_INCH	25.4	/* mm -> inch conversion */

#define TRACEDEVGA(a)
#define CLIP if (xd->clip.width>0) gsetcliprect(_d,xd->clip)

static drawing _d;

#define DRAW(a) {if(xd->kind != SCREEN) {_d=xd->gawin; CLIP; a;} else {_d=xd->bm; CLIP; a; if(!xd->buffered) {_d=xd->gawin; CLIP; a;} }}

#define SHOW  if(xd->kind==SCREEN) {drawbits(xd); GALastUpdate = GetTickCount();}
#define SH if(xd->kind==SCREEN && xd->buffered && GA_xd) GA_Timer(xd)


#define SF 20  /* scrollbar resolution */

	/********************************************************/
	/* Each driver can have its own device-specic graphical */
	/* parameters and resources.  these should be wrapped	*/
	/* in a structure (gadesc in devWindows.h)              */
	/* and attached to the overall device description via	*/
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

/* Update the screen 100ms after last plotting call or 500ms after
   last update (by default).

   This runs on (asynchronous) timers for each device.
   Macro SHOW does an immediate update, and records the update
   in GALastUpdate.
   SHOW is called for expose and mouse events, and newpage.

   Macro SH calls GA_Timer.  If it is more than 500ms since the last
   update it does an update; otherwise it sets a timer running for
   100ms. In either case cancels any existing timer.
   SH is called for the graphics primitives.  (This could probably be
   replace by calling from Mode(0)).

   There are two conditions:
   (i) xd->buffered is true, which is a per-device condition.
   (ii) GA_xd is non-null.  This is used to inhibit updates during shutdown
   of the device, and also (post 2.14.0) when the device is held.
*/

static UINT_PTR TimerNo = 0;
static gadesc *GA_xd;
static DWORD GALastUpdate = 0;

static void drawbits(gadesc *xd)
{
    if (xd)
	gbitblt(xd->gawin, xd->bm, pt(0,0), getrect(xd->bm));
}

static VOID CALLBACK
GA_timer_proc(HWND hwnd, UINT message, UINT_PTR tid, DWORD time)
{
    if ((message != WM_TIMER) || tid != TimerNo || !GA_xd) return;
    drawbits(GA_xd);
    GALastUpdate = time;
}

static void GA_Timer(gadesc *xd)
{
    DWORD now = GetTickCount();
    if(TimerNo != 0) KillTimer(0, TimerNo);
    if(now > GALastUpdate + xd->timesince) {
	drawbits(xd);
	GALastUpdate = now;
    } else {
	GA_xd = xd;
	TimerNo = SetTimer((HWND)0, (UINT_PTR)0, (UINT) xd->timeafter,
			   GA_timer_proc);
    }
}

static int GA_holdflush(pDevDesc dd, int level)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    if(!xd->buffered) return 0;
    int old = xd->holdlevel;
    xd->holdlevel += level;
    if(xd->holdlevel <= 0) xd->holdlevel = 0;
    if(xd->holdlevel == 0) {
	GA_xd = xd;
	gsetcursor(xd->gawin, ArrowCursor);
	drawbits(GA_xd);
	GALastUpdate = GetTickCount();
    }
    if (old == 0 && xd->holdlevel > 0) {
	if(TimerNo != 0) KillTimer(0, TimerNo);
	drawbits(xd);
	GA_xd = NULL;
	gsetcursor(xd->gawin, WatchCursor);
    }
    return xd->holdlevel;
}


	/********************************************************/
	/* There are a number of actions that every device	*/
	/* driver is expected to perform (even if, in some	*/
	/* cases it does nothing - just so long as it doesn't	*/
	/* crash !).  this is how the graphics engine interacts */
	/* with each device. Each action will be documented	*/
	/* individually.					*/
	/* hooks for these actions must be set up when the	*/
	/* device is first created				*/
	/********************************************************/

	/* Device Driver Actions */

static void GA_Activate(pDevDesc dd);
static void GA_Circle(double x, double y, double r,
		      const pGEcontext gc,
		      pDevDesc dd);
static void GA_Clip(double x0, double x1, double y0, double y1,
		     pDevDesc dd);
static void GA_Close(pDevDesc dd);
static void GA_Deactivate(pDevDesc dd);
static void GA_eventHelper(pDevDesc dd, int code);
static Rboolean GA_Locator(double *x, double *y, pDevDesc dd);
static void GA_Line(double x1, double y1, double x2, double y2,
		    const pGEcontext gc,
		    pDevDesc dd);
static void GA_MetricInfo(int c,
			  const pGEcontext gc,
			  double* ascent, double* descent,
			  double* width, pDevDesc dd);
static void GA_Mode(int mode, pDevDesc dd);
static void GA_NewPage(const pGEcontext gc,
		       pDevDesc dd);
static void GA_Path(double *x, double *y,
                    int npoly, int *nper,
                    Rboolean winding,
                    const pGEcontext gc,
                    pDevDesc dd);
static void GA_Polygon(int n, double *x, double *y,
		       const pGEcontext gc,
		       pDevDesc dd);
static void GA_Polyline(int n, double *x, double *y,
			const pGEcontext gc,
			pDevDesc dd);
static void GA_Rect(double x0, double y0, double x1, double y1,
		    const pGEcontext gc,
		    pDevDesc dd);
static void GA_Size(double *left, double *right,
		    double *bottom, double *top,
		    pDevDesc dd);
static void GA_Resize(pDevDesc dd);
static void GA_Raster(unsigned int *raster, int w, int h,
                      double x, double y,
                      double width, double height,
                      double rot,
                      Rboolean interpolate,
                      const pGEcontext gc, pDevDesc dd);
static SEXP GA_Cap(pDevDesc dd);
static double GA_StrWidth(const char *str,
			  const pGEcontext gc,
			  pDevDesc dd);
static void GA_Text(double x, double y, const char *str,
		    double rot, double hadj,
		    const pGEcontext gc,
		    pDevDesc dd);
static Rboolean GA_Open(pDevDesc, gadesc*, const char*, double, double,
			Rboolean, int, int, double, int, int, int);
static Rboolean GA_NewFrameConfirm(pDevDesc);


	/********************************************************/
	/* end of list of required device driver actions	*/
	/********************************************************/

#include "rbitmap.h"

	/* Support Routines */

static double pixelHeight(drawing  d);
static double pixelWidth(drawing d);
static void SetColor(int, double, gadesc*);
static void SetFont(pGEcontext, double, gadesc*);
//static int Load_Rbitmap_Dll();
static void SaveAsPng(pDevDesc dd, const char *fn);
static void SaveAsJpeg(pDevDesc dd, int quality, const char *fn);
static void SaveAsBmp(pDevDesc dd, const char *fn);
static void SaveAsTiff(pDevDesc dd, const char *fn);
static void SaveAsBitmap(pDevDesc dd, int res);

static void PrivateCopyDevice(pDevDesc dd, pDevDesc ndd, const char *name)
{
    pGEDevDesc gdd;
    int saveDev = curDevice();
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    gsetcursor(xd->gawin, WatchCursor);
    gsetVar(R_DeviceSymbol, mkString(name), R_BaseEnv);
    ndd->displayListOn = FALSE;
    gdd = GEcreateDevDesc(ndd);
    GEaddDevice(gdd);
    GEcopyDisplayList(ndevNumber(dd));
    GEkillDevice(gdd);
    selectDevice(saveDev);
    gsetcursor(xd->gawin, ArrowCursor);
    show(xd->gawin);
}

static void SaveAsWin(pDevDesc dd, const char *display,
		      Rboolean restoreConsole)
{
    pDevDesc ndd = (pDevDesc) calloc(1, sizeof(DevDesc));
    if (!ndd) {
	R_ShowMessage(_("Not enough memory to copy graphics window"));
	return;
    }
    if(!R_CheckDeviceAvailableBool()) {
	free(ndd);
	R_ShowMessage(_("No device available to copy graphics window"));
	return;
    }

    pGEDevDesc gdd = desc2GEDesc(dd);
    if (GADeviceDriver(ndd, display,
		       fromDeviceWidth(toDeviceWidth(1.0, GE_NDC, gdd),
				       GE_INCHES, gdd),
		       fromDeviceHeight(toDeviceHeight(-1.0, GE_NDC, gdd),
					GE_INCHES, gdd),
		       ((gadesc*) dd->deviceSpecific)->basefontsize,
		       0, 1, White, White, 1, NA_INTEGER, NA_INTEGER, FALSE,
		       R_GlobalEnv, restoreConsole, "", FALSE,
		       ((gadesc*) dd->deviceSpecific)->fillOddEven, "",
		       DEFAULT_QUALITY))
	PrivateCopyDevice(dd, ndd, display);
}

static void init_PS_PDF(void)
{
    SEXP call, initS, grNS=R_FindNamespace(mkString("grDevices"));

    initS = findVarInFrame3(grNS, install("initPSandPDFfonts"), TRUE);
    if(initS == R_UnboundValue)
	error("missing initPSandPDFfonts() in grDevices namespace: this should not happen");
    PROTECT(call = lang1(initS));
    eval(call, R_GlobalEnv);
    UNPROTECT(1);
}


static void SaveAsPostscript(pDevDesc dd, const char *fn)
{
    SEXP s;
    pDevDesc ndd = (pDevDesc) calloc(1, sizeof(DevDesc));
    pGEDevDesc gdd = desc2GEDesc(dd);
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    char family[256], encoding[256], paper[256], bg[256], fg[256];
    const char **afmpaths = NULL;

    if (!ndd) {
	R_ShowMessage(_("Not enough memory to copy graphics window"));
	return;
    }
    if(!R_CheckDeviceAvailableBool()) {
	free(ndd);
	R_ShowMessage(_("No device available to copy graphics window"));
	return;
    }

    if(strchr(fn, '%')) error(_("'%%' is not allowed in file name"));

    /* need to initialize PS/PDF font database:
       also sets .PostScript.Options */
    init_PS_PDF();
    /* Set default values and pad with zeroes ... */
    strncpy(family, "Helvetica", 256);
    strcpy(encoding, "ISOLatin1.enc");
    strncpy(paper, "special", 256);
    strncpy(bg, "transparent", 256);
    strncpy(fg, "black", 256);
    /* and then try to get it from .PostScript.Options */
    s = findVar(install(".PostScript.Options"), xd->psenv);
    if ((s != R_UnboundValue) && (s != R_NilValue)) {
	SEXP names = getAttrib(s, R_NamesSymbol);
	int i, done;
	for (i = 0, done = 0; (done<  4) && (i < length(s)) ; i++) {
	    if(!strcmp("family", CHAR(STRING_ELT(names, i)))) {
		strncpy(family, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)), 255);
		done++;
	    }
	    if(!strcmp("paper", CHAR(STRING_ELT(names, i)))) {
		strncpy(paper, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)), 255);
		done++;
		if(strcmp("paper", "default") == 0)
		    strncpy(paper, "special", 255);
	    }
	    if(!strcmp("bg", CHAR(STRING_ELT(names, i)))) {
		strncpy(bg, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)), 255);
		done++;
	    }
	    if(!strcmp("fg", CHAR(STRING_ELT(names, i)))) {
		strncpy(fg, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)), 255);
		done++;
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
		       0, 1, 0, "", "R Graphics Output", R_NilValue, "rgb",
		       TRUE, xd->fillOddEven))
	/* horizontal=F, onefile=F, pagecentre=T, print.it=F */
	PrivateCopyDevice(dd, ndd, "postscript");
}


static void SaveAsPDF(pDevDesc dd, const char *fn)
{
    SEXP s;
    pDevDesc ndd = (pDevDesc) calloc(1, sizeof(DevDesc));
    pGEDevDesc gdd = desc2GEDesc(dd);
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    char family[256], encoding[256], bg[256], fg[256];
    const char **afmpaths = NULL;
    Rboolean useCompression = FALSE;

    if (!ndd) {
	R_ShowMessage(_("Not enough memory to copy graphics window"));
	return;
    }
    if(!R_CheckDeviceAvailableBool()) {
	free(ndd);
	R_ShowMessage(_("No device available to copy graphics window"));
	return;
    }

    if(strchr(fn, '%')) error(_("'%%' is not allowed in file name"));

    /* Set default values... */
    init_PS_PDF();
    s = findVar(install(".PDF.Options"), xd->psenv);
    strncpy(family, "Helvetica", 256);
    strcpy(encoding, "ISOLatin1.enc");
    strncpy(bg, "transparent", 256);
    strncpy(fg, "black", 256);
    /* and then try to get it from .PDF.Options */
    if ((s != R_UnboundValue) && (s != R_NilValue)) {
	SEXP names = getAttrib(s, R_NamesSymbol);
	for (int i = 0; i < length(s) ; i++) {
	    if(!strcmp("family", CHAR(STRING_ELT(names, i))))
		strncpy(family, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)),255);
	    if(!strcmp("bg", CHAR(STRING_ELT(names, i))))
		strncpy(bg, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)), 255);
	    if(!strcmp("fg", CHAR(STRING_ELT(names, i))))
		strncpy(fg, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)), 255);
	    if(!strcmp("compress", CHAR(STRING_ELT(names, i))))
		useCompression = LOGICAL(VECTOR_ELT(s, i))[0] != 0;
	}
    }
    if (PDFDeviceDriver(ndd, fn, "special", family, afmpaths, encoding,
			bg, fg,
			fromDeviceWidth(toDeviceWidth(1.0, GE_NDC, gdd),
					GE_INCHES, gdd),
			fromDeviceHeight(toDeviceHeight(-1.0, GE_NDC, gdd),
					 GE_INCHES, gdd),
			((gadesc*) dd->deviceSpecific)->basefontsize,
			1, 0, "R Graphics Output", R_NilValue, 1, 4,
			"rgb", TRUE, TRUE, xd->fillOddEven, useCompression))
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

    for (i = 0; i < 4; i++) fontname[i] = "Arial";
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

    snprintf(oops, 256, "%s/Rdevga", getenv("R_USER"));
    notdone = 1;
    fontnum = 0;
    fontinitdone = 1;
    if (!optopenfile(oops)) {
	snprintf(oops, 256, "%s/etc/Rdevga", getenv("R_HOME"));
	if (!optopenfile(oops)) {
	    RStandardFonts();
	    notdone = 0;
	}
    }
    while (notdone) {
	oops[0] = '\0';
	notdone = optread(opt, ':');
	if (notdone == 1)
	    snprintf(oops, 256, "[%s] Error at line %d.", optfile(), optline());
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
		    snprintf(oops, 256, "Unknown style at line %d. ", optline());
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

/* Return a non-relocatable copy of a string */

static char *SaveFontSpec(SEXP sxp, int offset)
{
    char *s;
    if(!isString(sxp) || length(sxp) <= offset)
	error(_("invalid font specification"));
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
static char* translateFontFamily(const char* family) {
    SEXP graphicsNS, windowsenv, fontdb, fontnames;
    int i, nfonts;
    char* result = NULL;
    PROTECT_INDEX xpi;

    PROTECT(graphicsNS = R_FindNamespace(ScalarString(mkChar("grDevices"))));
    PROTECT_WITH_INDEX(windowsenv = findVar(install(".WindowsEnv"),
					    graphicsNS), &xpi);
    if(TYPEOF(windowsenv) == PROMSXP)
	REPROTECT(windowsenv = eval(windowsenv, graphicsNS), xpi);
    PROTECT(fontdb = findVar(install(".Windows.Fonts"), windowsenv));
    PROTECT(fontnames = getAttrib(fontdb, R_NamesSymbol));
    nfonts = LENGTH(fontdb);
    if (strlen(family) > 0) {
	int found = 0;
	for (i = 0; i < nfonts && !found; i++) {
	    const char* fontFamily = CHAR(STRING_ELT(fontnames, i));
	    if (strcmp(family, fontFamily) == 0) {
		found = 1;
		result = SaveFontSpec(VECTOR_ELT(fontdb, i), 0);
	    }
	}
	if (!found)
	    warning(_("font family not found in Windows font database"));
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

static void SetFont(pGEcontext gc, double rot, gadesc *xd)
{
    int size, face = gc->fontface, usePoints;
    char* fontfamily;
    double fs = gc->cex * gc->ps;
    int quality = xd->fontquality;

    usePoints = xd->kind <= METAFILE;
    if (!usePoints && xd->res_dpi > 0) fs *= xd->res_dpi/72.0;
    size = fs + 0.5;

    if (face < 1 || face > fontnum) face = 1;
    if (size < SMALLEST) size = SMALLEST;
    if (size != xd->fontsize || face != xd->fontface ||
	 rot != xd->fontangle || strcmp(gc->fontfamily, xd->fontfamily)) {
	if(xd->font) del(xd->font);
	doevent();
	/*
	 * If specify family = "", get family from face via Rdevga
	 *
	 * If specify a family and a face in 1 to 4 then get
	 * that family (mapped through WindowsFonts()) and face.
	 *
	 * If specify face > 4 then get font from face via Rdevga
	 * (whether specifed family or not).
	 */
	char * fm = gc->fontfamily;
	if (!fm[0]) fm = xd->basefontfamily;
	fontfamily = translateFontFamily(fm);
	if (fontfamily && face <= 4) {
	    xd->font = gnewfont2(xd->gawin,
				fontfamily, fontstyle[face - 1],
				size, rot, usePoints, quality);
	} else {
	    xd->font = gnewfont2(xd->gawin,
				fontname[face - 1], fontstyle[face - 1],
				size, rot, usePoints, quality);
	}
	if (xd->font) {
	    strcpy(xd->fontfamily, gc->fontfamily);
	    xd->fontface = face;
	    xd->fontsize = size;
	    xd->fontangle = rot;
	} else {
	    /* Fallback: set Arial */
	    if (face > 4) face = 1;
	    xd->font = gnewfont2(xd->gawin,
				"Arial", fontstyle[face - 1],
				size, rot, usePoints, quality);
	    if (!xd->font)
		error("unable to set or substitute a suitable font");
	    xd->fontface = face;
	    xd->fontsize = size;
	    xd->fontangle = rot;
	    strcpy(xd->fontfamily, "Arial");
	    warning("unable to set font: using Arial");
	}
    }
}


static void SetColor(int color, double gamma, gadesc *xd)
{
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

static void SetLineStyle(const pGEcontext gc, pDevDesc dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    xd->lty = gc->lty;
    if(xd->lwdscale != 1.0)
	xd->lwd = xd->lwdscale * gc->lwd;
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
	error(_("invalid line end"));
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
	error(_("invalid line join"));
    }

    xd->lmitre = gc->lmitre;
}

/* Callback functions */


static void HelpResize(window w, rect r)
{
    if (AllDevicesKilled) return;
    {
	pDevDesc dd = (pDevDesc) getdata(w);
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
	pDevDesc dd = (pDevDesc) getdata(w);
	killDevice(ndevNumber(dd));
    }
}

static void HelpExpose(window w, rect r)
{
    if (AllDevicesKilled) return;
    {
	pDevDesc dd = (pDevDesc) getdata(w);
	pGEDevDesc gdd = desc2GEDesc(dd);
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
	pDevDesc dd = (pDevDesc) getdata(w);
	gadesc *xd = (gadesc *) dd->deviceSpecific;

	if (!xd->locator && !xd->confirmation && !dd->gettingEvent)
	    return;
	if (button & LeftButton) {
	    int useBeep = xd->locator &&
		asLogical(GetOption1(install("locatorBell")));
	    if(useBeep) gabeep();
	    xd->clicked = 1;
	    xd->px = pt.x;
	    xd->py = pt.y;
	} else
	    xd->clicked = 2;
	if (dd->gettingEvent) {
	    doMouseEvent(dd, meMouseDown, button, pt.x, pt.y);
	    if (xd->buffered) SHOW;
	}
    }
}

static void HelpMouseMove(window w, int button, point pt)
{
    if (AllDevicesKilled) return;
    {
	pDevDesc dd = (pDevDesc) getdata(w);
	gadesc *xd = (gadesc *) dd->deviceSpecific;

	if (dd->gettingEvent) {
	    doMouseEvent(dd, meMouseMove, button, pt.x, pt.y);
	    if (xd->buffered) SHOW;
	}
    }
}

static void HelpMouseUp(window w, int button, point pt)
{
    if (AllDevicesKilled) return;
    {
	pDevDesc dd = (pDevDesc) getdata(w);
	gadesc *xd = (gadesc *) dd->deviceSpecific;

	if (dd->gettingEvent) {
	    doMouseEvent(dd, meMouseUp,button, pt.x, pt.y);
	    if (xd->buffered) SHOW;
	}
    }
}

static void menustop(control m)
{
    pDevDesc dd = (pDevDesc) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    if (!xd->locator)
	return;
    xd->clicked = 2;
}

static void menunextplot(control m)
{
    pDevDesc dd = (pDevDesc) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    xd->clicked = 2;
}

static void menufilebitmap(control m)
{
    pDevDesc dd = (pDevDesc) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    char *fn;
    /* the following use a private hook to set the default extension */
    if (m == xd->mpng) {
	setuserfilter("Png files (*.png)\0*.png\0\0");
	fn = askfilesave(G_("Portable network graphics file"), "|.png");
    } else if (m == xd->mbmp) {
	setuserfilter("Windows bitmap files (*.bmp)\0*.bmp\0\0");
	fn = askfilesave(G_("Windows bitmap file"), "|.bmp");
    } else if (m == xd->mtiff) {
	setuserfilter("TIFF files (*.tiff,*.tif)\0*.tiff;*.tif\0\0");
	fn = askfilesave(G_("TIFF file"), "|.tif");
    } else {
	setuserfilter("Jpeg files (*.jpeg,*.jpg)\0*.jpeg;*.jpg\0\0");
	fn = askfilesave(G_("Jpeg file"), "|.jpg");
    }
    if (!fn) return;
    gsetcursor(xd->gawin, WatchCursor);
    show(xd->gawin);
    if (m==xd->mpng) SaveAsPng(dd, fn);
    else if (m==xd->mbmp) SaveAsBmp(dd, fn);
    else if (m==xd->mtiff) SaveAsTiff(dd, fn);
    else if (m==xd->mjpeg50) SaveAsJpeg(dd, 50, fn);
    else if (m==xd->mjpeg75) SaveAsJpeg(dd, 75, fn);
    else SaveAsJpeg(dd, 100, fn);
    gsetcursor(xd->gawin, ArrowCursor);
    show(xd->gawin);
}


static void menups(control m)
{
    pDevDesc dd = (pDevDesc) getdata(m);
    char  *fn;

    setuserfilter("Encapsulated postscript files (*.eps)\0*.eps\0Postscript files (*.ps)\0*.ps\0All files (*.*)\0*.*\0\0");
    fn = askfilesave(G_("Postscript file"), "|.ps");
    if (!fn) return;
    SaveAsPostscript(dd, fn);
}


static void menupdf(control m)
{
    pDevDesc dd = (pDevDesc) getdata(m);
    char  *fn;

    setuserfilter("PDF files (*.pdf)\0*.pdf\0All files (*.*)\0*.*\0\0");
    fn = askfilesave(G_("PDF file"), "|.pdf");
    if (!fn) return;
    SaveAsPDF(dd, fn);
}


static void menuwm(control m)
{
    pDevDesc dd = (pDevDesc) getdata(m);
    char  display[550], *fn;

    setuserfilter("Enhanced metafiles (*.emf)\0*.emf\0All files (*.*)\0*.*\0\0");
    fn = askfilesave(G_("Enhanced metafiles"), "|.emf");
    if (!fn) return;
    if(strlen(fn) > 512) {
	askok(G_("file path selected is too long: only 512 bytes are allowed"));
	return;
    }
    snprintf(display, 550, "win.metafile:%s", fn);
    SaveAsWin(dd, display, TRUE);
}


static void menuclpwm(control m)
{
    pDevDesc dd = (pDevDesc) getdata(m);
    SaveAsWin(dd, "win.metafile", TRUE);
}

static void menuclpbm(control m)
{
    pDevDesc dd = (pDevDesc) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    show(xd->gawin);
    gsetcursor(xd->gawin, WatchCursor);
    copytoclipboard(xd->bm);
    gsetcursor(xd->gawin, ArrowCursor);
}

static void menustayontop(control m)
{
    pDevDesc dd = (pDevDesc) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    BringToTop(xd->gawin, 2);
}

static void menuprint(control m)
{
    pDevDesc dd = (pDevDesc) getdata(m);
    SaveAsWin(dd, "win.print:", TRUE);
}

static void menuclose(control m)
{
    pDevDesc dd = (pDevDesc) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    HelpClose(xd->gawin);
}

static void grpopupact(control m)
{
    pDevDesc dd = (pDevDesc) getdata(m);
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
    PROTECT(class = mkString("SavedPlots"));
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

    PROTECT(class = mkString("recordedplot"));
    classgets(snapshot, class);
    SET_VECTOR_ELT(pHISTORY, where, snapshot);
    pCURRENTPOS = where;
    if (!replace) pNUMPLOTS += 1;
    SETDL;
    UNPROTECT(3);
}


static void Replay(pDevDesc dd, SEXP vDL)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    xd->replaying = TRUE;
    gsetcursor(xd->gawin, WatchCursor);
    GEplaySnapshot(pCURRENT, desc2GEDesc(dd));
    xd->replaying = FALSE;
    gsetcursor(xd->gawin, ArrowCursor);
}

static void menurec(control m)
{
    pDevDesc dd = (pDevDesc) getdata(m);
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
    pDevDesc dd = (pDevDesc) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    AddtoPlotHistory(GEcreateSnapshot(desc2GEDesc(dd)), 0);
    xd->needsave = FALSE;
}

static void menureplace(control m)
{
    pDevDesc dd = (pDevDesc) getdata(m);

    GETDL;
    pMUSTEXIST;
    pCHECK;
    if (pCURRENTPOS < 0) {
	R_ShowMessage(G_("No plot to replace!"));
	return;
    }
    AddtoPlotHistory(GEcreateSnapshot(desc2GEDesc(dd)), 1);
}

static void menunext(control m)
{
    pDevDesc dd = (pDevDesc) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    GETDL;
    if (xd->needsave) return;
    pMUSTEXIST;
    pCHECK;
    if (pCURRENTPOS != (pNUMPLOTS - 1)) pMOVE(1);
    PrintWarnings();
}

static void menuprev(control m)
{
    pDevDesc dd = (pDevDesc) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    GETDL;
    pMUSTEXIST;
    pCHECK;
    if (pNUMPLOTS) {
	if (xd->recording && xd->needsave) {
	    pGEDevDesc gdd = desc2GEDesc(dd);
	    if (gdd->displayList != R_NilValue) {
		AddtoPlotHistory(GEcreateSnapshot(gdd), 0);
		xd->needsave = FALSE;
		vDL = findVar(install(".SavedPlots"), R_GlobalEnv);
		/* may have changed vDL pointer */
	    }
	}
	pMOVE((xd->needsave) ? 0 : -1);
    }
    PrintWarnings();
}

static void menugrclear(control m)
{
    defineVar(install(".SavedPlots"), R_NilValue, R_GlobalEnv);
}

static void menugvar(control m)
{
    SEXP  vDL;
    char *v = askstring(G_("Variable name"), "");
    pDevDesc dd = (pDevDesc) getdata(m);

    if (!v)
	return;
    vDL = findVar(install(v), R_GlobalEnv);
    pMUSTEXIST;
    pCHECK;
    if (!pNUMPLOTS) {
	R_ShowMessage(G_("Variable doesn't contain any plots!"));
	return;
    }
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
    pDevDesc dd = (pDevDesc) getdata(m);
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
    pDevDesc dd = (pDevDesc) getdata(m);
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
    pDevDesc dd = (pDevDesc) getdata(m);
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
    pDevDesc dd = (pDevDesc) getdata(w);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    R_KeyName keyname;

    if (dd->gettingEvent) {
	keyname = getKeyName(key);
	if (keyname > knUNKNOWN) {
	    doKeybd(dd, keyname, NULL);
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

    pDevDesc dd = (pDevDesc) getdata(w);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (dd->gettingEvent) {
	if (0 < key && key < 32) {
	    strcpy(keyname, "ctrl- ");
	    keyname[5] = (char) (key + 'A' - 1);
	} else {
	    keyname[0] = (char) key;
	    keyname[1] = '\0';
	}
	doKeybd(dd, knUNKNOWN, keyname);
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
	if (ggetkeystate() != CtrlKey) return;
	key = 'A' + key - 1;
	if (key == 'C') menuclpbm(xd->mclpbm);
	if (desc2GEDesc(dd)->displayList == R_NilValue) return;
	if (key == 'W') menuclpwm(xd->mclpwm);
	else if (key == 'P') menuprint(xd->mprint);
    }
}

static void mbarf(control m)
{
    pDevDesc dd = (pDevDesc) getdata(m);
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    GETDL;
    if (pEXIST && !xd->replaying) {
	enable(xd->mnext);
	enable(xd->mprev);
	if (pCURRENTPOS >= 0 && desc2GEDesc(dd)->displayList != R_NilValue)
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
    if (!xd->replaying && desc2GEDesc(dd)->displayList != R_NilValue) {
	enable(xd->madd);
	enable(xd->mprint);
	enable(xd->mpng);
	enable(xd->mbmp);
	enable(xd->mtiff);
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
	disable(xd->mtiff);
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
	/* device_Open is not usually called directly by the	*/
	/* graphics engine;  it is usually only called from	*/
	/* the device-driver entry point.			*/
	/* this function should set up all of the device-	*/
	/* specific resources for a new device			*/
	/* this function is given a new	structure for device-	*/
	/* specific information AND it must FREE the structure	*/
	/* if anything goes seriously wrong			*/
	/* NOTE that it is perfectly acceptable for this	*/
	/* function to set generic graphics parameters too	*/
	/* (i.e., override the generic parameter settings	*/
	/* which GInit sets up) all at the author's own risk	*/
	/* of course :)						*/
	/********************************************************/

#define MCHECK(m) {if(!(m)) {del(xd->gawin); return 0;}}

static void devga_sbf(control c, int pos)
{
    pDevDesc dd = (pDevDesc) getdata(c);
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


static Rboolean
setupScreenDevice(pDevDesc dd, gadesc *xd, double w, double h,
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
	dh = (int) (h * user_ypinch);
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
				    VScrollbar | HScrollbar | CanvasSize)
		)) {
	    warning("unable to open window");
	    return FALSE;
	}
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
    MCHECK(xd->mwm = newmenuitem("Metafile...", 0, menuwm));
    MCHECK(xd->mps = newmenuitem("Postscript...", 0, menups));
    MCHECK(xd->mpdf = newmenuitem("PDF...", 0, menupdf));
    MCHECK(xd->mpng = newmenuitem("Png...", 0, menufilebitmap));
    MCHECK(xd->mbmp = newmenuitem("Bmp...", 0, menufilebitmap));
    MCHECK(xd->mtiff = newmenuitem("TIFF...", 0, menufilebitmap));
    MCHECK(newsubmenu(xd->msubsave, "Jpeg"));
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
    MCHECK(xd->bm2 = newbitmap(getwidth(xd->gawin), getheight(xd->gawin),
			       getdepth(xd->gawin)));
    gfillrect(xd->gawin, xd->outcolor, getrect(xd->gawin));
    gfillrect(xd->bm, xd->outcolor, getrect(xd->bm));
    addto(xd->gawin);
    setdata(xd->mbar, (void *) dd);
    setdata(xd->mpng, (void *) dd);
    setdata(xd->mbmp, (void *) dd);
    setdata(xd->mtiff, (void *) dd);
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
    setmousedrag(xd->gawin, HelpMouseMove);
    setmouseup(xd->gawin, HelpMouseUp);
    setkeydown(xd->gawin, NHelpKeyIn);
    setkeyaction(xd->gawin, CHelpKeyIn);
    setclose(xd->gawin, HelpClose);
    xd->recording = recording;
    xd->replaying = FALSE;
    xd->resizing = resize;

    dd->eventHelper = GA_eventHelper;

    dd->canGenMouseDown = TRUE;
    dd->canGenMouseMove = TRUE;
    dd->canGenMouseUp = TRUE;
    dd->canGenKeybd = TRUE;
    dd->gettingEvent = FALSE;

    GA_xd = xd;
    return TRUE;
}

static Rboolean GA_Open(pDevDesc dd, gadesc *xd, const char *dsp,
			double w, double h, Rboolean recording,
			int resize, int canvascolor, double gamma,
			int xpos, int ypos, int bg)
{
    rect  rr;
    char buf[600]; /* allow for pageno formats */

    if (!fontinitdone)
	RFontInit();

    /* Foreground and Background Colors */
    xd->bg = dd->startfill = bg;
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
    xd->fp = NULL; /* not all devices (e.g. TIFF) use the file pointer, but SaveAsBitmap
		      looks at it */

    if (!dsp[0]) {
	if (!setupScreenDevice(dd, xd, w, h, recording, resize, xpos, ypos))
	    return FALSE;
	xd->have_alpha = TRUE;
    } else if (!strncmp(dsp, "win.print:", 10)) {
	xd->kind = PRINTER;
	xd->fast = 0; /* use scalable line widths */
	xd->gawin = newprinter(MM_PER_INCH * w, MM_PER_INCH * h, &dsp[10]);
	if (!xd->gawin) {
	    warning("unable to open printer");
	    return FALSE;
	}
    } else if (!strncmp(dsp, "png:", 4) || !strncmp(dsp,"bmp:", 4)) {
	xd->res_dpi = (xpos == NA_INTEGER) ? 0 : xpos;
	xd->bg = dd->startfill = canvascolor;
	xd->kind = (dsp[0]=='p') ? PNG : BMP;
	if(strlen(dsp+4) >= 512) error(_("filename too long in %s() call"),
				       (dsp[0]=='p') ? "png" : "bmp");
	strcpy(xd->filename, R_ExpandFileName(dsp+4));
	if (!Load_Rbitmap_Dll()) {
	    warning("unable to load Rbitmap.dll");
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
	    warning(_("unable to allocate bitmap"));
	    return FALSE;
	}
	xd->bm = xd->gawin;
	if ((xd->bm2 = newbitmap(w, h, 256)) == NULL) {
	    warning(_("unable to allocate bitmap"));
	    return FALSE;
	}
	snprintf(buf, 600, xd->filename, 1);
	if ((xd->fp = R_fopen(buf, "wb")) == NULL) {
	    del(xd->gawin);
	    warning(_("unable to open file '%s' for writing"), buf);
	    return FALSE;
	}
	xd->have_alpha = TRUE;
    } else if (!strncmp(dsp, "jpeg:", 5)) {
	char *p = strchr(&dsp[5], ':');
	xd->res_dpi = (xpos == NA_INTEGER) ? 0 : xpos;
	xd->bg = dd->startfill = canvascolor;
	xd->kind = JPEG;
	if (!p) return FALSE;
	if (!Load_Rbitmap_Dll()) {
	    warning("unable to load Rbitmap.dll");
	    return FALSE;
	}
	*p = '\0';
	xd->quality = atoi(&dsp[5]);
	*p = ':' ;
	if(strlen(p+1) >= 512) error(_("filename too long in jpeg() call"));
	strcpy(xd->filename, R_ExpandFileName(p+1));
	if (w < 20 && h < 20)
	    warning(_("'width=%d, height=%d' are unlikely values in pixels"),
		    (int)w, (int) h);
	if((xd->gawin = newbitmap(w, h, 256)) == NULL) {
	    warning(_("unable to allocate bitmap"));
	    return FALSE;
	}
	xd->bm = xd->gawin;
	if ((xd->bm2 = newbitmap(w, h, 256)) == NULL) {
	    warning(_("unable to allocate bitmap"));
	    return FALSE;
	}
	snprintf(buf, 600, xd->filename, 1);
	if ((xd->fp = R_fopen(buf, "wb")) == NULL) {
	    del(xd->gawin);
	    warning(_("unable to open file '%s' for writing"), buf);
	    return FALSE;
	}
	xd->have_alpha = TRUE;
    } else if (!strncmp(dsp, "tiff:", 5)) {
	char *p = strchr(&dsp[5], ':');
	xd->res_dpi = (xpos == NA_INTEGER) ? 0 : xpos;
	xd->bg = dd->startfill = canvascolor;
	xd->kind = TIFF;
	if (!p) return FALSE;
	if (!Load_Rbitmap_Dll()) {
	    warning("unable to load Rbitmap.dll");
	    return FALSE;
	}
	*p = '\0';
	xd->quality = atoi(&dsp[5]);
	*p = ':' ;
	if(strlen(p+1) >= 512) error(_("filename too long in tiff() call"));
	strcpy(xd->filename, R_ExpandFileName(p+1));
	if (w < 20 && h < 20)
	    warning(_("'width=%d, height=%d' are unlikely values in pixels"),
		    (int) w, (int) h);
	if((xd->gawin = newbitmap(w, h, 256)) == NULL) {
	    warning(_("unable to allocate bitmap"));
	    return FALSE;
	}
	xd->bm = xd->gawin;
	if ((xd->bm2 = newbitmap(w, h, 256)) == NULL) {
	    warning(_("unable to allocate bitmap"));
	    return FALSE;
	}
	xd->have_alpha = TRUE;
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
	if (strncmp(dsp, s, ls) || (dsp[ls] && (dsp[ls] != ':'))) {
	    warning("invalid specification for file name in win.metafile()");
	    return FALSE;
	}
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
		warning(_("unable to open metafile '%s' for writing"), buf);
	    else
		warning(_("unable to open clipboard to write metafile"));
	    return FALSE;
	}
    }

    if (xd->kind <= METAFILE)
	xd->lwdscale = devicepixelsy(xd->gawin)/96.0; /* matches ps/pdf */
    else if (xd->res_dpi > 0)
	xd->lwdscale = xd->res_dpi/96.0;
    else
	xd->lwdscale = 72.0/96.0;
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
	/* converting from DEVICE to whatever units the user	*/
	/* asked for						*/
	/********************************************************/

static double GA_StrWidth(const char *str,
			  const pGEcontext gc,
			  pDevDesc dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    SetFont(gc, 0.0, xd);
    return (double) gstrwidth1(xd->gawin, xd->font, str, CE_NATIVE);
}

static double GA_StrWidth_UTF8(const char *str,
			      const pGEcontext gc,
			      pDevDesc dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    double a;

    /* This should never be called for symbol fonts */
    SetFont(gc, 0.0, xd);
    if(gc->fontface != 5)
	a = (double) gstrwidth1(xd->gawin, xd->font, str, CE_UTF8);
    else
	a = (double) gstrwidth1(xd->gawin, xd->font, str, CE_SYMBOL);
    return a;
}

	/********************************************************/
	/* device_MetricInfo should return height, depth, and	*/
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
			  const pGEcontext gc,
			  double* ascent, double* descent,
			  double* width, pDevDesc dd)
{
    int   a, d, w;
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    Rboolean Unicode = mbcslocale;

    if (c < 0) { Unicode = TRUE; c = -c; }
    SetFont(gc, 0.0, xd);
    if(Unicode && gc->fontface != 5 && c > 127)
	gwcharmetric(xd->gawin, xd->font, c, &a, &d, &w);
    else
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
	/* device_Clip is given the left, right, bottom, and	*/
	/* top of a rectangle (in DEVICE coordinates).  it	*/
	/* should have the side-effect that subsequent output	*/
	/* is clipped to the given rectangle			*/
	/********************************************************/

static void GA_Clip(double x0, double x1, double y0, double y1, pDevDesc dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    rect r;

    r = rcanon(rpt(pt(x0, y0), pt(x1, y1)));
    r.width  += 1;
    r.height += 1;
    xd->clip = r;
}

	/********************************************************/
	/* device_Resize is called whenever the device is	*/
	/* resized.  the function must update the GPar		*/
	/* parameters (left, right, bottom, and top) for the	*/
	/* new device size					*/
	/* this is not usually called directly by the graphics	*/
	/* engine because the detection of device resizes	*/
	/* (e.g., a window resize) are usually detected by	*/
	/* device-specific code	(see R_ProcessEvents)           */
	/********************************************************/

static void GA_Size(double *left, double *right,
		    double *bottom, double *top,
		    pDevDesc dd)
{
    *left = dd->left;
    *top = dd->top;
    /* There's a mysterious -0.0001 in the setting */
    *right = ceil(dd->right);
    *bottom = ceil(dd->bottom);
}

static void GA_Resize(pDevDesc dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;

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
	    {
		SEXP scale;
		PROTECT(scale = ScalarReal(rf));
		GEhandleEvent(GE_ScalePS, dd, scale);
		UNPROTECT(1);
	    }
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
		killDevice(ndevNumber(dd));
		return; /* since the device is killed */
	    }
	    if(xd->have_alpha) {
		del(xd->bm2);
		xd->bm2 = newbitmap(iw, ih, getdepth(xd->gawin));
		if (!xd->bm2) {
		    R_ShowMessage(_("Insufficient memory for resize. Disabling alpha blending"));
		    xd->have_alpha = FALSE;
		}
	    }

	    gfillrect(xd->gawin, xd->outcolor, getrect(xd->gawin));
	    gfillrect(xd->bm, xd->outcolor, getrect(xd->bm));
	}
    }
}

	/********************************************************/
	/* device_NewPage is called whenever a new plot requires*/
	/* a new page.  a new page might mean just clearing the	*/
	/* device (as in this case) or moving to a new page	*/
	/* (e.g., postscript)					*/
	/********************************************************/

static void GA_NewPage(const pGEcontext gc,
		       pDevDesc dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    xd->npage++;
    if ((xd->kind == PRINTER) && xd->needsave)
	nextpage(xd->gawin);
    if ((xd->kind == METAFILE) && xd->needsave) {
	char buf[600];
	if (strlen(xd->filename) == 0)
	    error(_("a clipboard metafile can store only one figure."));
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
	    error(_("unable to open file '%s' for writing"), buf);
    }
    if (xd->kind == TIFF && xd->needsave) {
	SaveAsBitmap(dd, xd->res_dpi);
    }
    if (xd->kind == SCREEN) {
	if(xd->buffered && !xd->holdlevel) SHOW;
	if (xd->recording && xd->needsave)
	    AddtoPlotHistory(desc2GEDesc(dd)->savedSnapshot, 0);
	if (xd->replaying)
	    xd->needsave = FALSE;
	else
	    xd->needsave = TRUE;
    }
    xd->bg = gc->fill;
    xd->warn_trans = FALSE;
    {
	unsigned int alpha = R_ALPHA(xd->bg);
	if(alpha  == 0) xd->bgcolor = xd->canvascolor;
	else {
	    xd->bgcolor = GArgb(xd->bg, gc->gamma);
	    if(alpha < 255)
		xd->bgcolor = (alpha * xd->bgcolor +
			       (255-alpha) * xd->canvascolor)/255;
	}
    }
    if (xd->kind != SCREEN) {
	xd->needsave = TRUE;
	xd->clip = getrect(xd->gawin);
	if(R_OPAQUE(xd->bg) || xd->kind == BMP || xd->kind == JPEG
	   || xd->kind == TIFF) {
	    DRAW(gfillrect(_d, xd->bgcolor, xd->clip));
	} else if(xd->kind == PNG) {
	    DRAW(gfillrect(_d, PNG_TRANS, xd->clip));
	}
	if(xd->kind == PNG)
	    xd->pngtrans = ggetpixel(xd->gawin, pt(0,0)) | 0xff000000;
    } else {
	xd->clip = getregion(xd);
	DRAW(gfillrect(_d, xd->bgcolor, xd->clip));
    }
    SH;
}

static void deleteGraphMenus(int devnum)
{
    char prefix[15];

    snprintf(prefix, 15, "$Graph%i", devnum);
    windelmenus(prefix);
}

	/********************************************************/
	/* device_Close is called when the device is killed	*/
	/* this function is responsible for destroying any	*/
	/* device-specific resources that were created in	*/
	/* device_Open and for FREEing the device-specific	*/
	/* parameters structure					*/
	/********************************************************/

static void GA_Close(pDevDesc dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    SEXP vDL;

    if (dd->onExit) {
	dd->onExit(dd);
	UserBreak = TRUE;
    }

    if (xd->kind == SCREEN) {
	if(xd->recording) {
	    AddtoPlotHistory(GEcreateSnapshot(desc2GEDesc(dd)), 0);
	    /* May have changed vDL, so can't use GETDL above */
	    vDL = findVar(install(".SavedPlots"), R_GlobalEnv);
	    pCURRENTPOS++; /* so PgUp goes to the last saved plot
			      when a windows() device is opened */
	}
	hide(xd->gawin);

	del(xd->bm);
	/* If this is the active device and buffered, shut updates off */
	if (xd == GA_xd) GA_xd = NULL;
	deleteGraphMenus(ndevNumber(dd) + 1);

    } else if ((xd->kind == PNG) || (xd->kind == JPEG)
	       || (xd->kind == BMP) || (xd->kind == TIFF)) {
	if (xd->kind == TIFF) xd->npage++;
	SaveAsBitmap(dd, xd->res_dpi);
    }
    del(xd->font);
    if(xd->bm2) del(xd->bm2);
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
	/* device_Activate is called when a device becomes the	*/
	/* active device.  in this case it is used to change the*/
	/* title of a window to indicate the active status of	*/
	/* the device to the user.  not all device types will	*/
	/* do anything						*/
	/********************************************************/

static void GA_Activate(pDevDesc dd)
{
    char  t[150];
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (xd->replaying || (xd->kind!=SCREEN))
	return;
    if(strlen(xd->title)) {
	snprintf(t, 140, xd->title, ndevNumber(dd) + 1);
	t[139] = '\0';
    } else {
	snprintf(t, 150, "R Graphics: Device %d", ndevNumber(dd) + 1);
    }
    strcat(t, " (ACTIVE)");
    settext(xd->gawin, t);
    if (xd != GA_xd)
    	drawbits(GA_xd);
    GA_xd = xd;
}

	/********************************************************/
	/* device_Deactivate is called when a device becomes	*/
	/* inactive.  in this case it is used to change the	*/
	/* title of a window to indicate the inactive status of */
	/* the device to the user.  not all device types will	*/
	/* do anything						*/
	/********************************************************/

static void GA_Deactivate(pDevDesc dd)
{
    char  t[150];
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (xd->replaying || (xd->kind != SCREEN))
	return;
    if(strlen(xd->title)) {
	snprintf(t, 140, xd->title, ndevNumber(dd) + 1);
	t[139] = '\0';
    } else {
	snprintf(t, 150, "R Graphics: Device %d", ndevNumber(dd) + 1);
    }
    strcat(t, " (inactive)");
    settext(xd->gawin, t);
}

#define WARN_SEMI_TRANS { \
	    if(!xd->warn_trans) warning(_("semi-transparency is not supported on this device: reported only once per page")); \
	    xd->warn_trans = TRUE; \
	}

#define DRAW2(col) {if(xd->kind != SCREEN) gcopyalpha(xd->gawin,xd->bm2,r,R_ALPHA(col)); else {gcopyalpha(xd->bm,xd->bm2,r,R_ALPHA(col)); if(!xd->buffered) drawbits(xd);}}



	/********************************************************/
	/* device_Rect should have the side-effect that a	*/
	/* rectangle is drawn with the given locations for its	*/
	/* opposite corners.  the border of the rectangle	*/
	/* should be in the given "fg" colour and the rectangle	*/
	/* should be filled with the given "bg" colour		*/
	/* if "fg" is NA_INTEGER then no border should be drawn */
	/* if "bg" is NA_INTEGER then the rectangle should not	*/
	/* be filled						*/
	/* the locations are in an arbitrary coordinate system	*/
	/* and this function is responsible for converting the	*/
	/* locations to DEVICE coordinates using GConvert	*/
	/********************************************************/

static void GA_Rect(double x0, double y0, double x1, double y1,
		    const pGEcontext gc,
		    pDevDesc dd)
{
    int   tmp;
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    rect  r, rr;

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
    /* zero width or height disappears, so handle that case specially in case it's just rounding */
    if ((int)x0 == (int)x1 && x1-x0 >= 0.5) {
    	x1 = (int)x1;
    	x0 = x1 - 1.0;
    }
    if ((int)y0 == (int)y1 && y1-y0 >= 0.5) {
    	y1 = (int)y1;
    	y0 = y1 - 1.0;
    }
    r = rect((int) x0, (int) y0, (int)x1 - (int)x0, (int)y1 - (int)y0);

    SetColor(gc->fill, gc->gamma, xd);
    if (R_OPAQUE(gc->fill)) {
	DRAW(gfillrect(_d, xd->fgcolor, r));
    } else if(R_ALPHA(gc->fill) > 0) {
	if(xd->have_alpha) {
	    rect cp = xd->clip;
	    /* We are only working with the screen device here, so
	       we can assume that x->bm is the current state.
	       Copying from the screen window does not work. */
	    /* Clip to the device region */
	    rr = r;
	    if (r.x < 0) {r.x = 0; r.width = r.width + rr.x;}
	    if (r.y < 0) {r.y = 0; r.height = r.height + rr.y;}
	    if (r.x + r.width > cp.x + cp.width)
		r.width = cp.x + cp.width - r.x;
	    if (r.y + r.height > cp.y + cp.height)
		r.height = cp.y + cp.height - r.y;
	    gsetcliprect(xd->bm, xd->clip);
	    gcopy(xd->bm2, xd->bm, r);
	    gfillrect(xd->bm2, xd->fgcolor, rr);
	    DRAW2(gc->fill);
	    r = rr;
	} else WARN_SEMI_TRANS;
    }

    SetColor(gc->col, gc->gamma, xd);
    SetLineStyle(gc, dd);
    if (R_OPAQUE(gc->col)) {
	DRAW(gdrawrect(_d, xd->lwd, xd->lty, xd->fgcolor, r, 0, xd->lend,
		       xd->ljoin, xd->lmitre));
    } else if(R_ALPHA(gc->col) > 0) {
	if(xd->have_alpha) {
	    int adj, tol = xd->lwd; /* only half needed */
	    rect cp = xd->clip;
	    rr = r;
	    r.x -= tol; r.y -= tol; r.width += 2*tol; r.height += 2*tol;
	    if (r.x < 0) {adj = r.x; r.x = 0; r.width = r.width + adj;}
	    if (r.y < 0) {adj = r.y; r.y = 0; r.height = r.height + adj;}
	    if (r.x + r.width > cp.x + cp.width)
		r.width = cp.x + cp.width - r.x;
	    if (r.y + r.height > cp.y + cp.height)
		r.height = cp.y + cp.height - r.y;
	    gsetcliprect(xd->bm, xd->clip);
	    gcopy(xd->bm2, xd->bm, r);
	    gdrawrect(xd->bm2, xd->lwd, xd->lty, xd->fgcolor, rr, 0, xd->lend,
		      xd->ljoin, xd->lmitre);
	    DRAW2(gc->col);
	} else WARN_SEMI_TRANS;
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
	/* the location is in arbitrary coordinates and the	*/
	/* function is responsible for converting this to	*/
	/* DEVICE coordinates.  the radius is given in DEVICE	*/
	/* coordinates						*/
	/********************************************************/

static void GA_Circle(double x, double y, double radius,
		      const pGEcontext gc,
		      pDevDesc dd)
{
    int   id, ix, iy;
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    rect  r, rr;

    TRACEDEVGA("circle");
    id = 2*radius + 0.5;
    if (id < 2) id = 2; /* diameter 1 is near-invisible */

    ix = (int) x;
    iy = (int) y;
    r = rr = rect(ix - id/2, iy - id/2, id, id);

    SetColor(gc->fill, gc->gamma, xd);
    if (R_OPAQUE(gc->fill)) {
	DRAW(gfillellipse(_d, xd->fgcolor, rr));
    } else if(R_ALPHA(gc->fill) > 0) {
	if (xd->have_alpha) {
	    rect cp = xd->clip;
	    /* Clip to the device region */
	    if (r.x < 0) {r.x = 0; r.width = r.width + rr.x;}
	    if (r.y < 0) {r.y = 0; r.height = r.height + rr.y;}
	    if (r.x + r.width > cp.x + cp.width)
		r.width = cp.x + cp.width - r.x;
	    if (r.y + r.height > cp.y + cp.height)
		r.height = cp.y + cp.height - r.y;
	    gsetcliprect(xd->bm, xd->clip);
	    gcopy(xd->bm2, xd->bm, r);
	    gfillellipse(xd->bm2, xd->fgcolor, rr);
	    DRAW2(gc->fill);
	    r = rr;
	} else WARN_SEMI_TRANS;
    }

    SetColor(gc->col, gc->gamma, xd);
    SetLineStyle(gc, dd);
    if (R_OPAQUE(gc->col)) {
	DRAW(gdrawellipse(_d, xd->lwd, xd->fgcolor, rr, 0, xd->lend,
			  xd->ljoin, xd->lmitre));
    } else if(R_ALPHA(gc->col) > 0) {
	if(xd->have_alpha) {
	    int adj, tol = xd->lwd; /* only half needed */
	    rect cp = xd->clip;
	    r.x -= tol; r.y -= tol; r.width += 2*tol; r.height += 2*tol;
	    if (r.x < 0) {adj = r.x; r.x = 0; r.width = r.width + adj;}
	    if (r.y < 0) {adj = r.y; r.y = 0; r.height = r.height + adj;}
	    if (r.x + r.width > cp.x + cp.width)
		r.width = cp.x + cp.width - r.x;
	    if (r.y + r.height > cp.y + cp.height)
		r.height = cp.y + cp.height - r.y;
	    gsetcliprect(xd->bm, xd->clip);
	    gcopy(xd->bm2, xd->bm, r);
	    gdrawellipse(xd->bm2, xd->lwd, xd->fgcolor, rr, 0, xd->lend,
			 xd->ljoin, xd->lmitre);
	    DRAW2(gc->col);
	} else WARN_SEMI_TRANS;
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
		    const pGEcontext gc,
		    pDevDesc dd)
{
    int   xx1, yy1, xx2, yy2;
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    /* In-place conversion ok */
    TRACEDEVGA("line");
    xx1 = (int) x1;
    yy1 = (int) y1;
    xx2 = (int) x2;
    yy2 = (int) y2;

    SetColor(gc->col, gc->gamma, xd);
    SetLineStyle(gc, dd);
    if (R_OPAQUE(gc->col)) {
	DRAW(gdrawline(_d, xd->lwd, xd->lty, xd->fgcolor,
		       pt(xx1, yy1), pt(xx2, yy2), 0, xd->lend,
			  xd->ljoin, xd->lmitre));
	SH;
    } else if(R_ALPHA(gc->col) > 0) {
	if(xd->have_alpha) {
	    rect r = xd->clip;
	    gsetcliprect(xd->bm, xd->clip);
	    gcopy(xd->bm2, xd->bm, r);
	    gdrawline(xd->bm2, xd->lwd, xd->lty, xd->fgcolor,
		      pt(xx1, yy1), pt(xx2, yy2), 0, xd->lend,
		      xd->ljoin, xd->lmitre);
	    DRAW2(gc->col);
	    SH;
	} else WARN_SEMI_TRANS;
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
			const pGEcontext gc,
			pDevDesc dd)
{
    const void *vmax = vmaxget();
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

    SetColor(gc->col, gc->gamma, xd);
    SetLineStyle(gc, dd);
    if (R_OPAQUE(gc->col)) {
	DRAW(gdrawpolyline(_d, xd->lwd, xd->lty, xd->fgcolor, p, n, 0, 0,
			   xd->lend, xd->ljoin, xd->lmitre));
    } else if(R_ALPHA(gc->col) > 0) {
	if(xd->have_alpha) {
	    rect r = xd->clip; /* lines can go well outside bbox of points */
	    gsetcliprect(xd->bm, xd->clip);
	    gcopy(xd->bm2, xd->bm, r);
	    gdrawpolyline(xd->bm2, xd->lwd, xd->lty, xd->fgcolor, p, n, 0, 0,
			  xd->lend, xd->ljoin, xd->lmitre);
	    DRAW2(gc->col);
	} else WARN_SEMI_TRANS;
    }
    vmaxset(vmax);
    SH;
}

	/********************************************************/
	/* device_Polygon should have the side-effect that a	*/
	/* polygon is drawn using the given x and y values	*/
	/* the polygon border should be drawn in the "fg"	*/
	/* colour and filled with the "bg" colour		*/
	/* if "fg" is NA_INTEGER don't draw the border		*/
	/* if "bg" is NA_INTEGER don't fill the polygon		*/
	/* the x and y values are in arbitrary coordinates and	*/
	/* the function is responsible for converting them to	*/
	/* DEVICE coordinates using GConvert			*/
	/********************************************************/

static void GA_Polygon(int n, double *x, double *y,
		       const pGEcontext gc,
		       pDevDesc dd)
{
    const void *vmax = vmaxget();
    point *points;
    rect r;
    double devx, devy;
    int   i, mx0 = 0, mx1 = 0, my0 = 0, my1 = 0;
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
	mx0 = imin2(mx0, points[i].x);
	mx1 = imax2(mx1, points[i].x);
	my0 = imin2(my0, points[i].y);
	my1 = imax2(my1, points[i].y);
    }
    r.x = mx0; r.width = mx1 - mx0;
    r.y = my0; r.height = my1 - my0;

    if (xd->doSetPolyFill && xd->fillOddEven == FALSE) {
	DRAW(gsetpolyfillmode(_d, 0));
	xd->doSetPolyFill = FALSE;  /* Only set it once */
    }

    SetColor(gc->fill, gc->gamma, xd);
    if (R_OPAQUE(gc->fill)) {
	DRAW(gfillpolygon(_d, xd->fgcolor, points, n));
    } else if(R_ALPHA(gc->fill) > 0) {
	if(xd->have_alpha) {
	    gsetcliprect(xd->bm, xd->clip);
	    gcopy(xd->bm2, xd->bm, r);
	    gfillpolygon(xd->bm2, xd->fgcolor, points, n);
	    DRAW2(gc->fill);
	} else WARN_SEMI_TRANS;
    }

    SetColor(gc->col, gc->gamma, xd);
    SetLineStyle(gc, dd);
    if (R_OPAQUE(gc->col)) {
	DRAW(gdrawpolygon(_d, xd->lwd, xd->lty, xd->fgcolor, points, n, 0,
			  xd->lend, xd->ljoin, xd->lmitre));
    } else if(R_ALPHA(gc->col) > 0) {
	if(xd->have_alpha) {
	    r = xd->clip;
	    gsetcliprect(xd->bm, xd->clip);
	    gcopy(xd->bm2, xd->bm, r);
	    gdrawpolygon(xd->bm2, xd->lwd, xd->lty, xd->fgcolor, points, n, 0,
			 xd->lend, xd->ljoin, xd->lmitre);
	    DRAW2(gc->col);
	} else WARN_SEMI_TRANS;
    }
    vmaxset(vmax);
    SH;
}

static void GA_Path(double *x, double *y,
                    int npoly, int *nper,
                    Rboolean winding,
                    const pGEcontext gc,
                    pDevDesc dd)
{
    const void *vmax = vmaxget();
    point *points;
    point *pointIndex;
    rect r;
    double devx, devy;
    int   i, mx0 = 0, mx1 = 0, my0 = 0, my1 = 0;
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    int ntot = 0;
    for (i=0; i < npoly; i++) {
        ntot = ntot + nper[i];
    }

    TRACEDEVGA("path");
    points = (point *) R_alloc(ntot, sizeof(point));
    if (!points)
	return;
    for (i = 0; i < ntot; i++) {
	devx = x[i];
	devy = y[i];
	points[i].x = (int) (devx);
	points[i].y = (int) (devy);
	mx0 = imin2(mx0, points[i].x);
	mx1 = imax2(mx1, points[i].x);
	my0 = imin2(my0, points[i].y);
	my1 = imax2(my1, points[i].y);
    }
    r.x = mx0; r.width = mx1 - mx0;
    r.y = my0; r.height = my1 - my0;

    if (winding) {
        DRAW(gsetpolyfillmode(_d, 0));
    } else {
        DRAW(gsetpolyfillmode(_d, 1));
    }

    SetColor(gc->fill, gc->gamma, xd);
    if (R_OPAQUE(gc->fill)) {
	DRAW(gfillpolypolygon(_d, xd->fgcolor, points, npoly, nper));
    } else if(R_ALPHA(gc->fill) > 0) {
	if(xd->have_alpha) {
	    gsetcliprect(xd->bm, xd->clip);
	    gcopy(xd->bm2, xd->bm, r);
	    gfillpolypolygon(xd->bm2, xd->fgcolor, points, npoly, nper);
	    DRAW2(gc->fill);
	} else WARN_SEMI_TRANS;
    }

    SetColor(gc->col, gc->gamma, xd);
    SetLineStyle(gc, dd);
    if (R_OPAQUE(gc->col)) {
        pointIndex = points;
        for (i = 0; i < npoly; i++) {
            DRAW(gdrawpolygon(_d, xd->lwd, xd->lty, xd->fgcolor,
                              pointIndex, nper[i], 0,
                              xd->lend, xd->ljoin, xd->lmitre));
            pointIndex = pointIndex + nper[i];
        }
    } else if(R_ALPHA(gc->col) > 0) {
	if(xd->have_alpha) {
	    r = xd->clip;
	    gsetcliprect(xd->bm, xd->clip);
	    gcopy(xd->bm2, xd->bm, r);
            pointIndex = points;
            for (i = 0; i < npoly; i++) {
                gdrawpolygon(xd->bm2, xd->lwd, xd->lty, xd->fgcolor,
                             pointIndex, nper[i], 0,
                             xd->lend, xd->ljoin, xd->lmitre);
                pointIndex = pointIndex + nper[i];
            }
	    DRAW2(gc->col);
	} else WARN_SEMI_TRANS;
    }
    vmaxset(vmax);
    SH;
}

static void doRaster(unsigned int *raster, int x, int y, int w, int h,
                     double rot, pDevDesc dd)
{
    const void *vmax = vmaxget();
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    rect  dr = rect(x, y, w, h);
    image img;
    byte *imageData;

    TRACEDEVGA("raster");

    /* Create image object */
    img = newimage(w, h, 32);

    /* Set the image pixels from the raster.
       Windows uses 0xaarrggbb.
       AlphaBlend requires pre-multiplied alpha, that is it uses
       (src + (1-alpha)*dest) for each pixel colour.
       We could re-order the lines here (top to bottom) to avoid a copy
       in imagetobitmap.
     */
    imageData = (byte *) R_alloc(4*w*h, sizeof(byte));
    for (int i = 0; i < w*h; i++) {
        byte alpha = R_ALPHA(raster[i]);
	double fac = alpha/255.0;
	imageData[i*4 + 3] = alpha;
	imageData[i*4 + 2] = 0.49 + fac * R_RED(raster[i]);
	imageData[i*4 + 1] = 0.49 + fac * R_GREEN(raster[i]);
	imageData[i*4 + 0] = 0.49 + fac * R_BLUE(raster[i]);
    }
    setpixels(img, imageData);
    if(xd->kind != SCREEN) {
        gsetcliprect(xd->gawin, xd->clip);
	gcopyalpha2(xd->gawin, img, dr);
    } else {
        gsetcliprect(xd->bm, xd->clip);
	gcopyalpha2(xd->bm, img, dr);
        if(!xd->buffered)
	    drawbits(xd);
    }

    /* Tidy up */
    delimage(img);
    SH;
    vmaxset(vmax);
}

static void flipRaster(unsigned int *rasterImage,
                       int imageWidth, int imageHeight,
                       int invertX, int invertY,
                       unsigned int *flippedRaster) {
    int i, j;
    int rowInc, rowOff, colInc, colOff;

    if (invertX) {
        colInc = -1;
        colOff = imageWidth - 1;
    } else {
        colInc = 1;
        colOff = 0;
    }
    if (invertY) {
        rowInc = -1;
        rowOff = imageHeight - 1;
    } else {
        rowInc = 1;
        rowOff = 0;
    }

    for (i = 0; i < imageHeight ;i++) {
        for (j = 0; j < imageWidth; j++) {
            int row = (rowInc*i + rowOff);
            int col = (colInc*j + colOff);
            flippedRaster[i*imageWidth + j] =
                rasterImage[row*imageWidth + col];
        }
    }
}

static void GA_Raster(unsigned int *raster, int w, int h,
                      double x, double y,
                      double width, double height,
                      double rot,
                      Rboolean interpolate,
                      const pGEcontext gc, pDevDesc dd)
{
    const void *vmax = vmaxget();
    double angle = rot*M_PI/180;
    unsigned int *image = raster;
    int imageWidth = w, imageHeight = h;
    Rboolean invertX = FALSE;
    Rboolean invertY = TRUE;

    /* The alphablend code cannot handle negative width or height */
    if (height < 0) {
        height = -height;
        invertY = FALSE;
    }
    if (width < 0) {
        width = -width;
        invertX = TRUE;
    }

    if (interpolate) {
        int newW = (int) (width + .5), newH = (int) (height + .5);
        unsigned int *newRaster;

        newRaster = (unsigned int *) R_alloc(newW * newH,
                                             sizeof(unsigned int));
        R_GE_rasterInterpolate(image, w, h, newRaster, newW, newH);
        image = newRaster;
        imageWidth = newW;
        imageHeight = newH;

    } else {
        /* Even if not interpolating, have to explicitly scale here
         * before doing rotation, so that image to rotate
         * is the right size AND so that can adjust (x, y)
         * correctly
         */
        int newW = (int) (width + .5), newH = (int) (height + .5);
        unsigned int *newRaster;

        newRaster = (unsigned int *) R_alloc(newW * newH,
                                             sizeof(unsigned int));
        R_GE_rasterScale(image, w, h, newRaster, newW, newH);
        image = newRaster;
        imageWidth = newW;
        imageHeight = newH;
    }

    if (invertX) {
        /* convert (x, y) from bottom-left to top-left */
        x -= imageWidth*cos(angle);
        if (angle != 0) y -= imageWidth*sin(angle);
    }
    if (!invertY) {
        /* convert (x, y) from bottom-left to top-left */
        y -= imageHeight*cos(angle);
        if (angle != 0) x -= imageHeight*sin(angle);
    }

    if (angle != 0) {
        int newW, newH;
        double xoff, yoff;
        unsigned int *resizedRaster, *rotatedRaster;

        R_GE_rasterRotatedSize(imageWidth, imageHeight, angle, &newW, &newH);
        R_GE_rasterRotatedOffset(imageWidth, imageHeight, angle, 0,
                                 &xoff, &yoff);

        resizedRaster = (unsigned int *) R_alloc(newW * newH,
                                             sizeof(unsigned int));
        R_GE_rasterResizeForRotation(image, imageWidth, imageHeight,
                                     resizedRaster, newW, newH, gc);

        rotatedRaster = (unsigned int *) R_alloc(newW * newH,
                                                 sizeof(unsigned int));
        R_GE_rasterRotate(resizedRaster, newW, newH, angle, rotatedRaster, gc,
                          /* Threshold alpha to
                           * transparent/opaque only
                           */
                          FALSE);

        /*
         * Adjust (x, y) for resized and rotated image
         */
        x -= (newW - imageWidth)/2 + xoff;
        y -= (newH - imageHeight)/2 - yoff;

        image = rotatedRaster;
        imageWidth = newW;
        imageHeight = newH;
    }

    if (invertX || invertY) {
        unsigned int *flippedRaster;

        flippedRaster = (unsigned int *) R_alloc(imageWidth * imageHeight,
                                                 sizeof(unsigned int));
        flipRaster(image, imageWidth, imageHeight,
                   invertX, invertY, flippedRaster);
        image = flippedRaster;
    }

    doRaster(image, (int) (x + .5), (int) (y + .5),
             imageWidth, imageHeight, rot, dd);

    vmaxset(vmax);
}

static SEXP GA_Cap(pDevDesc dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    SEXP dim, raster = R_NilValue;
    image img = NULL;
    byte *screenData;

    /* These in-place conversions are ok */
    TRACEDEVGA("cap");

    /* Only make sense for on-screen device */
    if(xd->kind == SCREEN) {
        img = bitmaptoimage(xd->gawin);
        if (imagedepth(img) == 8) img = convert8to32(img);

	if (img) {
	    int width = imagewidth(img), height = imageheight(img),
		size = width*height;
	    unsigned int *rint;

	    screenData = getpixels(img);

	    PROTECT(raster = allocVector(INTSXP, size));

	    /* Copy each byte of screen to an R matrix.
	     * The ARGB32 needs to be converted to R's ABGR32 */
	    rint = (unsigned int *) INTEGER(raster);
	    for (int i = 0; i < size; i++)
		rint[i] = R_RGBA(screenData[i*4 + 2],
				 screenData[i*4 + 1],
				 screenData[i*4 + 0],
				 255);
	    PROTECT(dim = allocVector(INTSXP, 2));
	    INTEGER(dim)[0] = height;
	    INTEGER(dim)[1] = width;
	    setAttrib(raster, R_DimSymbol, dim);

	    UNPROTECT(2);
	}

	/* Tidy up */
	delimage(img);
    }


    return raster;
}

	/********************************************************/
	/* device_Text should have the side-effect that the	*/
	/* given text is drawn at the given location		*/
	/* the text should be rotated according to rot (degrees)*/
	/* the location is in an arbitrary coordinate system	*/
	/* and this function is responsible for converting the	*/
	/* location to DEVICE coordinates using GConvert	*/
	/********************************************************/

static void GA_Text0(double x, double y, const char *str, int enc,
		     double rot, double hadj,
		     const pGEcontext gc,
		     pDevDesc dd)
{
    double pixs, xl, yl, rot1;
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    pixs = - 1;
    xl = 0.0;
    yl = -pixs;
    rot1 = rot * DEG2RAD;
    x += -xl * cos(rot1) + yl * sin(rot1);
    y -= -xl * sin(rot1) - yl * cos(rot1);

    SetFont(gc, rot, xd);
    SetColor(gc->col, gc->gamma, xd);
    if (R_OPAQUE(gc->col)) {
	if(gc->fontface != 5) {
	    /* As from 2.7.0 can use Unicode always */
	    int n = strlen(str), cnt;
	    R_CheckStack2(sizeof(wchar_t)*(n+1));
	    wchar_t wc[n+1];/* only need terminator to debug */
	    cnt = (enc == CE_UTF8) ?
		Rf_utf8towcs(wc, str, n+1): mbstowcs(wc, str, n);
	    /* These macros need to be wrapped in braces */
	    DRAW(gwdrawstr1(_d, xd->font, xd->fgcolor, pt(x, y),
			    wc, cnt, hadj));
	} else {
	    DRAW(gdrawstr1(_d, xd->font, xd->fgcolor, pt(x, y), str, hadj));
	}
    } else if(R_ALPHA(gc->col) > 0) {
	/*  it is too hard to get a correct bounding box */
	if(xd->have_alpha) {
	    rect r = xd->clip;
	    r = getregion(xd);
	    gsetcliprect(xd->bm, xd->clip);
	    gcopy(xd->bm2, xd->bm, r);
	    if(gc->fontface != 5) {
		int n = strlen(str), cnt;
		R_CheckStack2(sizeof(wchar_t)*(n+1));
		wchar_t wc[n+1];
		cnt = (enc == CE_UTF8) ?
		    Rf_utf8towcs(wc, str, n+1): mbstowcs(wc, str, n);
		gwdrawstr1(xd->bm2, xd->font, xd->fgcolor, pt(x, y),
			   wc, cnt, hadj);
	    } else
		gdrawstr1(xd->bm2, xd->font, xd->fgcolor, pt(x, y), str, hadj);
	    DRAW2(gc->col);
	} else WARN_SEMI_TRANS;
    }
    SH;
}

static void GA_Text(double x, double y, const char *str,
		    double rot, double hadj,
		    const pGEcontext gc,
		    pDevDesc dd)
{
    GA_Text0(x, y, str, CE_NATIVE, rot, hadj, gc, dd);
}

static void GA_Text_UTF8(double x, double y, const char *str,
			double rot, double hadj,
			const pGEcontext gc,
			pDevDesc dd)
{
    GA_Text0(x, y, str, CE_UTF8, rot, hadj, gc, dd);
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

static void GA_onExit(pDevDesc dd);

static Rboolean GA_Locator(double *x, double *y, pDevDesc dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    RCNTXT cntxt;

    if (xd->kind != SCREEN)
	return FALSE;
    if (xd->holdlevel > 0)
	error(_("attempt to use the locator after dev.hold()"));
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
    xd->cntxt = (void *) &cntxt;

    /* and an exit handler in case the window gets closed */
    dd->onExit = GA_onExit;

    while (!xd->clicked) {
	SH;
	if (!peekevent()) WaitMessage();
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
	/* device_Mode is called whenever the graphics engine	*/
	/* starts drawing (mode=1) or stops drawing (mode=0)	*/
	/* the device is not required to do anything		*/
	/********************************************************/

/* Set Graphics mode - not needed for X11 */
static void GA_Mode(int mode, pDevDesc dd)
{
}


	/********************************************************/
	/* the device-driver entry point is given a device	*/
	/* description structure that it must set up.  this	*/
	/* involves several important jobs ...			*/
	/* (1) it must ALLOCATE a new device-specific parameters*/
	/* structure and FREE that structure if anything goes	*/
	/* wrong (i.e., it won't report a successful setup to	*/
	/* the graphics engine (the graphics engine is NOT	*/
	/* responsible for allocating or freeing device-specific*/
	/* resources or parameters)				*/
	/* (2) it must initialise the device-specific resources */
	/* and parameters (mostly done by calling device_Open)	*/
	/* (3) it must initialise the generic graphical	*/
	/* parameters that are not initialised by GInit (because*/
	/* only the device knows what values they should have)	*/
	/* see Graphics.h for the official list of these	*/
	/* (4) it may reset generic graphics parameters that	*/
	/* have already been initialised by GInit (although you	*/
	/* should know what you are doing if you do this)	*/
	/* (5) it must attach the device-specific parameters	*/
	/* structure to the device description structure	*/
	/* e.g., dd->deviceSpecific = (void *) xd;		*/
	/* (6) it must FREE the overall device description if	*/
	/* it wants to bail out to the top-level		*/
	/* the graphics engine is responsible for allocating	*/
	/* the device description and freeing it in most cases	*/
	/* but if the device driver freaks out it needs to do	*/
	/* the clean-up itself					*/
	/********************************************************/


static
Rboolean GADeviceDriver(pDevDesc dd, const char *display, double width,
			double height, double pointsize,
			Rboolean recording, int resize, int bg, int canvas,
			double gamma, int xpos, int ypos, Rboolean buffered,
			SEXP psenv, Rboolean restoreConsole,
			const char *title, Rboolean clickToConfirm,
			Rboolean fillOddEven, const char *family,
			int quality)
{
    /* if need to bail out with some sort of "error" then */
    /* must free(dd) */

    int   ps; /* This really is in (big) points */
    gadesc *xd;
    rect  rr;

    /* allocate new device description */
    if (!(xd = (gadesc *) malloc(sizeof(gadesc)))) {
	warning("allocation failed in GADeviceDriver");
	return FALSE;
    }

    /* from here on, if need to bail out with "error", must also */
    /* free(xd) */

    ps = pointsize;
    if (ps < 1) ps = 12;
    /* Ensures a font is selected at first use */
    xd->font = NULL;
    xd->fontface = -1;
    xd->fontsize = -1;
    xd->fontangle = 0.0;
    xd->fontfamily[0] = '\0';
    xd->basefontsize = ps ;
    dd->startfont = 1;
    dd->startps = ps;
    dd->startlty = LTY_SOLID;
    dd->startgamma = gamma;
    xd->bm = NULL;
    xd->bm2 = NULL;
    xd->have_alpha = FALSE; /* selectively overridden in GA_Open */
    xd->warn_trans = FALSE;
    strncpy(xd->title, title, 101);
    xd->title[100] = '\0';
    strncpy(xd->basefontfamily, family, 101);
    xd->basefontfamily[100] = '\0';
    xd->fontquality = quality;
    xd->doSetPolyFill = TRUE;     /* will only set it once */
    xd->fillOddEven = fillOddEven;

    /* Start the Device Driver and Hardcopy.  */

    if (!GA_Open(dd, xd, display, width, height, recording, resize, canvas,
		 gamma, xpos, ypos, bg)) {
	warning("opening device failed");
	free(xd);
	return FALSE;
    }
    dd->deviceSpecific = (void *) xd;
    /* Set up Data Structures  */

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
    dd->path = GA_Path;
    dd->raster = GA_Raster;
    dd->cap = GA_Cap;
    dd->locator = GA_Locator;
    dd->mode = GA_Mode;
    dd->metricInfo = GA_MetricInfo;
    dd->newFrameConfirm = clickToConfirm ? GA_NewFrameConfirm : NULL;
    dd->hasTextUTF8 = TRUE;
    dd->strWidthUTF8 = GA_StrWidth_UTF8;
    dd->textUTF8 = GA_Text_UTF8;
    dd->useRotatedTextInContour = TRUE;
    xd->cntxt = NULL;
    dd->holdflush = GA_holdflush;
    xd->holdlevel = 0;

    dd->haveRaster = 2;  /* full support */
    dd->haveCapture = dd->haveLocator = (xd->kind == SCREEN) ? 2 : 1;
    dd->haveTransparency = 2;
    switch(xd->kind) {
    case SCREEN:
	dd->haveTransparentBg = 3;
    case PRINTER:
    case METAFILE:
    case PNG:
	dd->haveTransparentBg = 2;
	break;
    default: /* JPEG, BMP, TIFF */
	dd->haveTransparentBg = 1;
	break;
    }
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

    dd->startps = ps * xd->rescale_factor;
    if (xd->kind > METAFILE && xd->res_dpi > 0) ps *= xd->res_dpi/72.0;

    if (xd->kind <= METAFILE) {
	/* it is 12 *point*, not 12 pixel */
	double ps0 = ps * xd->rescale_factor;
	dd->cra[0] = 0.9 * ps0 * devicepixelsx(xd->gawin) / 72.0;
	dd->cra[1] = 1.2 * ps0 * devicepixelsy(xd->gawin) / 72.0;
    } else {
	dd->cra[0] = 0.9 * ps;
	dd->cra[1] = 1.2 * ps;
    }

    /* Character Addressing Offsets */
    /* These are used to plot a single plotting character */
    /* so that it is exactly over the plotting point */

    dd->xCharOffset = 0.50;
    dd->yCharOffset = 0.40;
    dd->yLineBias = 0.2;

    /* Inches per raster unit */

    if (xd->kind <= METAFILE) { /* non-screen devices set NA_real_ */
	if (R_FINITE(user_xpinch) && user_xpinch > 0.0)
	    dd->ipr[0] = 1.0/user_xpinch;
	else
	    dd->ipr[0] = pixelWidth(xd->gawin);
	if (R_FINITE(user_ypinch) && user_ypinch > 0.0)
	    dd->ipr[1] = 1.0/user_ypinch;
	else
	    dd->ipr[1] = pixelHeight(xd->gawin);
    } else if (xd->res_dpi > 0) {
	dd->ipr[0] = dd->ipr[1] = 1.0/xd->res_dpi;
    } else {
	dd->ipr[0] = dd->ipr[1] = 1.0/72.0;
    }


    /* Device capabilities */
    dd->canClip= TRUE;
    dd->canHAdj = 1; /* 0, 0.5, 1 */
    dd->canChangeGamma = FALSE;

    /* initialise device description (most of the work */
    /* has been done in GA_Open) */

    xd->resize = (resize == 3) || ismdi();   // MDI windows may be zoomed automatically
    xd->locator = FALSE;
    xd->buffered = buffered;
    xd->psenv = psenv;
    {
	SEXP timeouts = GetOption1(install("windowsTimeouts"));
	if(isInteger(timeouts)){
	    xd->timeafter = INTEGER(timeouts)[0];
	    xd->timesince = INTEGER(timeouts)[1];
	} else {
	    warning(_("option 'windowsTimeouts' should be integer"));
	    xd->timeafter = 100;
	    xd->timesince = 500;
	}
    }
    dd->displayListOn = (xd->kind == SCREEN);
    if (RConsole && restoreConsole) show(RConsole);
    return TRUE;
}

SEXP savePlot(SEXP args)
{
    SEXP filename, type;
    const char *fn, *tp; char display[550];
    int device;
    pDevDesc dd;
    Rboolean restoreConsole;

    args = CDR(args); /* skip entry point name */
    device = asInteger(CAR(args));
    if(device < 1 || device > NumDevices())
	error(_("invalid device number in 'savePlot'"));
    dd = GEgetDevice(device - 1)->dev;
    if(!dd) error(_("invalid device in 'savePlot'"));
    filename = CADR(args);
    if (!isString(filename) || LENGTH(filename) != 1)
	error(_("invalid filename argument in 'savePlot'"));
    /* in 2.8.0 this will always be passed as native, but be conserative */
    fn = translateChar(STRING_ELT(filename, 0));
    type = CADDR(args);
    if (!isString(type) || LENGTH(type) != 1)
	error(_("invalid type argument in 'savePlot'"));
    tp = CHAR(STRING_ELT(type, 0));
    restoreConsole = asLogical(CADDDR(args));

    if(!strcmp(tp, "png")) {
	SaveAsPng(dd, fn);
    } else if (!strcmp(tp,"bmp")) {
	SaveAsBmp(dd,fn);
    } else if (!strcmp(tp,"tiff")) {
	SaveAsTiff(dd,fn);
    } else if(!strcmp(tp, "jpeg") || !strcmp(tp,"jpg")) {
      /*Default quality suggested in libjpeg*/
	SaveAsJpeg(dd, 75, fn);
    } else if(!strcmp(tp, "tiff") || !strcmp(tp,"tif")) {
	SaveAsTiff(dd, fn);
    } else if (!strcmp(tp, "wmf") || !strcmp(tp, "emf")) {
	if(strlen(fn) > 512) {
	    askok(G_("file path selected is too long: only 512 bytes are allowed"));
	    return R_NilValue;
	}
	snprintf(display, 550, "win.metafile:%s", fn);
	SaveAsWin(dd, display, restoreConsole);
    } else if (!strcmp(tp, "ps") || !strcmp(tp, "eps")) {
	SaveAsPostscript(dd, fn);
    } else if (!strcmp(tp, "pdf")) {
	SaveAsPDF(dd, fn);
    } else
	error(_("unknown type in savePlot"));
    return R_NilValue;
}


static int png_rows = 0;

static unsigned int privategetpixel2(void *d,int i, int j)
{
    rgb c;
    c = ((rgb *)d)[i*png_rows + j];
    return c | 0xff000000;
}

/* This is the device version */
/* Values of res > 0 are used to set the resolution in the file */
static void SaveAsBitmap(pDevDesc dd, int res)
{
    rect r, r2;
    gadesc *xd = (gadesc *) dd->deviceSpecific;
    unsigned char *data;

    r = ggetcliprect(xd->gawin);
    gsetcliprect(xd->gawin, r2 = getrect(xd->gawin));
    if(xd->fp || xd->kind == TIFF) {
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
	    else if (xd->kind == TIFF) {
		char buf[600];
		snprintf(buf, 600, xd->filename, xd->npage - 1);
		R_SaveAsTIFF(data, xd->windowWidth, xd->windowHeight,
			     privategetpixel2, 0, buf, res, xd->quality) ;
	    } else
		R_SaveAsBmp(data, xd->windowWidth, xd->windowHeight,
			    privategetpixel2, 0, xd->fp, res);
	    free(data);
	} else
	    warning(_("processing of the plot ran out of memory"));
	if(xd->fp) fclose(xd->fp);
    }
    gsetcliprect(xd->gawin, r);
    xd->fp = NULL;
}

/* These are the menu item versions */
static void SaveAsPng(pDevDesc dd, const char *fn)
{
    FILE *fp;
    rect r, r2;
    unsigned char *data;
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (!Load_Rbitmap_Dll()) return;
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

static void SaveAsJpeg(pDevDesc dd, int quality, const char *fn)
{
    FILE *fp;
    rect r, r2;
    unsigned char *data;
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (!Load_Rbitmap_Dll()) return;
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


static void SaveAsBmp(pDevDesc dd, const char *fn)
{
    FILE *fp;
    rect r, r2;
    unsigned char *data;
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (!Load_Rbitmap_Dll()) return;
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

static void SaveAsTiff(pDevDesc dd, const char *fn)
{
    rect r, r2;
    unsigned char *data;
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    if (!Load_Rbitmap_Dll()) {
	R_ShowMessage(_("Impossible to load Rbitmap.dll"));
	return;
    }
    r = ggetcliprect(xd->bm);
    gsetcliprect(xd->bm, r2 = getrect(xd->bm));

    getbitmapdata2(xd->bm, &data);
    if(data) {
	png_rows = r2.width;
	R_SaveAsTIFF(data, xd->windowWidth, xd->windowHeight,
		     privategetpixel2, 0, fn, 0, 1 /* no compression */) ;
	free(data);
    } else
	warning(_("processing of the plot ran out of memory"));
    gsetcliprect(xd->bm, r);
}

/* This is Guido's devga device, 'ga' for GraphApp. */

#ifndef CLEARTYPE_QUALITY
# define CLEARTYPE_QUALITY 5
#endif

SEXP devga(SEXP args)
{
    pGEDevDesc gdd;
    const char *display, *title, *family;
    const void *vmax;
    double height, width, ps, xpinch, ypinch, gamma;
    int recording = 0, resize = 1, bg, canvas, xpos, ypos, buffered, quality;
    Rboolean restoreConsole, clickToConfirm, fillOddEven;
    SEXP sc, psenv;

    vmax = vmaxget();
    args = CDR(args); /* skip entry point name */
    display = CHAR(STRING_ELT(CAR(args), 0));
    args = CDR(args);
    width = asReal(CAR(args));
    args = CDR(args);
    height = asReal(CAR(args));
    args = CDR(args);
    if (width <= 0 || height <= 0)
	error(_("invalid 'width' or 'height'"));
    ps = asReal(CAR(args));
    args = CDR(args);
    recording = asLogical(CAR(args));
    if (recording == NA_LOGICAL)
	error(_("invalid value of '%s'"), "record");
    args = CDR(args);
    resize = asInteger(CAR(args));
    if (resize == NA_INTEGER)
	error(_("invalid value of '%s'"), "rescale");
    args = CDR(args);
    xpinch = asReal(CAR(args));
    args = CDR(args);
    ypinch = asReal(CAR(args));
    args = CDR(args);
    sc = CAR(args);
    if (!isString(sc) && !isInteger(sc) && !isLogical(sc) && !isReal(sc))
	error(_("invalid value of '%s'"), "canvas");
    canvas = RGBpar(sc, 0);
    args = CDR(args);
    gamma = asReal(CAR(args));
    args = CDR(args);
    xpos = asInteger(CAR(args)); /* used for res in png/jpeg/bmp */
    args = CDR(args);
    ypos = asInteger(CAR(args));
    args = CDR(args);
    buffered = asLogical(CAR(args));
    if (buffered == NA_LOGICAL)
	error(_("invalid value of '%s'"), "buffered");
    args = CDR(args);
    psenv = CAR(args);
    args = CDR(args);
    sc = CAR(args);
    if (!isString(sc) && !isInteger(sc) && !isLogical(sc) && !isReal(sc))
	error(_("invalid value of '%s'"), "bg");
    bg = RGBpar(sc, 0);
    args = CDR(args);
    restoreConsole = asLogical(CAR(args));
    args = CDR(args);
    sc = CAR(args);
    if (!isString(sc) || LENGTH(sc) != 1)
	error(_("invalid value of '%s'"), "title");
    title = CHAR(STRING_ELT(sc, 0));
    args = CDR(args);
    clickToConfirm = asLogical(CAR(args));
    args = CDR(args);
    fillOddEven = asLogical(CAR(args));
    if (fillOddEven == NA_LOGICAL)
	error(_("invalid value of '%s'"), "fillOddEven");
    args = CDR(args);
    sc = CAR(args);
    if (!isString(sc) || LENGTH(sc) != 1)
	error(_("invalid value of '%s'"), "family");
    family = CHAR(STRING_ELT(sc, 0));
    quality = DEFAULT_QUALITY;
    args = CDR(args);
    quality = asInteger(CAR(args));
//    printf("fontquality=%d\n", quality);
    switch (quality) {
    case 1: quality = DEFAULT_QUALITY; break;
    case 2: quality = NONANTIALIASED_QUALITY; break;
    case 3: quality = CLEARTYPE_QUALITY; break;
    case 4: quality = ANTIALIASED_QUALITY; break;
    default: quality = DEFAULT_QUALITY;
    }

    R_GE_checkVersionOrDie(R_GE_version);
    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	pDevDesc dev;
	char type[100];
	strcpy(type, "windows");
	if (display[0]) {
	    strncpy(type, display, 100);
	    // Package tkrplot assumes the exact form here
	    if(strncmp(display, "win.metafile", 12)) {
		char *p = strchr(type, ':');
		if(p) *p = '\0';
	    }
	}
	/* Allocate and initialize the device driver data */
	if (!(dev = (pDevDesc) calloc(1, sizeof(DevDesc)))) return 0;
	GAsetunits(xpinch, ypinch);
	if (!GADeviceDriver(dev, display, width, height, ps,
			    (Rboolean)recording, resize, bg, canvas, gamma,
			    xpos, ypos, (Rboolean)buffered, psenv,
			    restoreConsole, title, clickToConfirm,
			    fillOddEven, family, quality)) {
	    free(dev);
	    error(_("unable to start %s() device"), type);
	}
	gdd = GEcreateDevDesc(dev);
	GEaddDevice2(gdd, type);
    } END_SUSPEND_INTERRUPTS;
    vmaxset(vmax);
    return R_NilValue;
}

static void GA_onExit(pDevDesc dd)
{
    gadesc *xd = (gadesc *) dd->deviceSpecific;

    dd->onExit = NULL;
    xd->confirmation = FALSE;
    dd->gettingEvent = FALSE;

    if (xd->cntxt) endcontext((RCNTXT *)xd->cntxt);
    if (xd->locator) donelocator((void *)xd);

    addto(xd->gawin);
    gchangemenubar(xd->mbar);
    gchangepopup(xd->gawin, xd->grpopup);
    addto(xd->gawin);
    setstatus(_("R Graphics"));
    GA_Activate(dd);
}

static Rboolean GA_NewFrameConfirm(pDevDesc dev)
{
    char *msg;
    gadesc *xd = dev->deviceSpecific;

    if (!xd || xd->kind != SCREEN) return FALSE;

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
    BringToTop(xd->gawin, 0);
    dev->onExit = GA_onExit;  /* install callback for cleanup */
    while (!xd->clicked && !xd->enterkey) {
	SH;
	if (!peekevent()) WaitMessage();
	R_ProcessEvents(); /* May not return if user interrupts */
    }
    dev->onExit(dev);

    return TRUE;
}

static void GA_eventHelper(pDevDesc dd, int code)
{
    gadesc *xd = dd->deviceSpecific;

    if (code == 1) {
    	show(xd->gawin);
    	addto(xd->gawin);
    	gchangemenubar(xd->mbar);
    	gchangepopup(xd->gawin, NULL);
    	if (isEnvironment(dd->eventEnv)) {
    	    SEXP prompt = findVar(install("prompt"), dd->eventEnv);
    	    if (length(prompt) == 1) {
    		setstatus(CHAR(asChar(prompt)));
    		settext(xd->gawin, CHAR(asChar(prompt)));
    	    }
    	}
    	dd->onExit = GA_onExit;  /* install callback for cleanup */
    } else if (code == 0)
    	dd->onExit(dd);

    return;
}


static R_SaveAsBitmap R_devCairo;
static int RcairoAlreadyLoaded = 0;
static HINSTANCE hRcairoDll;

static int Load_Rcairo_Dll()
{
    if (!RcairoAlreadyLoaded) {
	char szFullPath[PATH_MAX];
	strcpy(szFullPath, R_HomeDir());
	strcat(szFullPath, "/library/grDevices/libs/");
	strcat(szFullPath, R_ARCH);
	strcat(szFullPath, "/winCairo.dll");
	if (((hRcairoDll = LoadLibrary(szFullPath)) != NULL) &&
	    ((R_devCairo =
	      (R_SaveAsBitmap)GetProcAddress(hRcairoDll, "in_Cairo"))
	     != NULL)) {
	    RcairoAlreadyLoaded = 1;
	} else {
	    if (hRcairoDll != NULL) FreeLibrary(hRcairoDll);
	    RcairoAlreadyLoaded = -1;
	    char buf[1000];
	    snprintf(buf, 1000, "Unable to load '%s'", szFullPath);
	    R_ShowMessage(buf);
	}
    }
    return (RcairoAlreadyLoaded > 0);
}

/*
   cairo(filename, type, width, height, pointsize, bg, res, antialias, quality)
*/
SEXP devCairo(SEXP args)
{
    if (!Load_Rcairo_Dll())
	error("unable to load winCairo.dll: was it built?");
    else (R_devCairo)(args);
    return R_NilValue;
}
