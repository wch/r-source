/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007-8  The R Foundation
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
 *
 *  Modular Quartz device for Mac OS X
 *
 *  Partially based on code by Byron Ellis
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if HAVE_AQUA

#include <Defn.h>
#include <Rinternals.h>
#define R_USE_PROTOTYPES 1
#include <R_ext/GraphicsEngine.h>
#include <R_ext/QuartzDevice.h>

#include "grDevices.h"
#ifdef SUPPORT_MBCS
#include <wchar.h>
#endif

#include <CoreFoundation/CoreFoundation.h>
/* FIXME: unused at present
#include <Carbon/Carbon.h>
*/

#define QBE_NATIVE   1  /* either Cocoa or Carbon depending on the OS X version */
#define QBE_COCOA    2  /* internal Cocoa */
#define QBE_CARBON   3  /* internal Carbon */
#define QBE_BITMAP   4  /* bitmap file creating */
#define QBE_PDF      5  /* PDF file creating */

typedef struct moduleTypes_s {
    const char *type;
    const char *subst;
    int qbe; /* Quartz back-end */
} quartz_module_t;

/* list of internally supported output modules */
const quartz_module_t quartz_modules[] = {
    { "",        0,                           QBE_NATIVE  },
    { "native",  0,                           QBE_NATIVE  },
    { "cocoa",   0,                           QBE_COCOA   },
    { "carbon",  0,                           QBE_CARBON  },
    { "pdf",     0,                           QBE_PDF     },
    { "png",     "public.png",                QBE_BITMAP  },
    { "jpeg",    "public.jpeg",               QBE_BITMAP  },
    { "jpg",     "public.jpeg",               QBE_BITMAP  },
    { "jpeg2000","public.jpeg-2000",          QBE_BITMAP  },
    { "tiff",    "public.tiff",               QBE_BITMAP  },
    { "tif",     "public.tiff",               QBE_BITMAP  },
    { "gif",     "com.compuserve.gif",        QBE_BITMAP  },
    { "psd",     "com.adobe.photoshop-image", QBE_BITMAP  },
    { "bmp",     "com.microsoft.bmp",         QBE_BITMAP  },
    { "sgi",     "com.sgi.sgi-image",         QBE_BITMAP  },
    { "pict",    "com.apple.pict",            QBE_BITMAP  },
    { 0, 0, 0} };



/* for compatibility with OS X <10.5 */
#ifndef CGFLOAT_DEFINED
typedef float CGFloat;
#define CGFLOAT_MIN FLT_MIN
#define CGFLOAT_MAX FLT_MAX
#define CGFLOAT_IS_DOUBLE 0
#define CGFLOAT_DEFINED 1
#endif

typedef struct QuartzSpecific_s {
    double        ps;
    double        scalex, scaley;  /* resolution correction: px/pt ratio */
    double        width,height;    /* size (in inches) */
    double        tscale;          /* text scale (resolution independent,
                                      i.e. it constitutes a text zoom factor */
    int           dirty;           /* dirtly flag. Not acted upon by the Quartz
                                      core, but QC sets it whenever a drawing
                                      operation is performed (see detailed
				      description in R_ext/QuartzDevice.h) */
    int           gstate;          /* gstate counter */
    int           async;           /* asynchronous drawing (i.e. context was
                                      not ready for an operation) */
    int           bg;              /* background color */
    int           canvas;          /* background color */
    int           antialias,smooth;/* smoothing flags (only aa makes any sense) */
    int           flags;           /* additional QDFLAGs */
    int           redraw;          /* redraw flag is set when replaying
		                              and inhibits syncs on Mode */
    CGRect        clipRect;        /* clipping rectangle */
    pDevDesc      dev;             /* device structure holding this one */

    void*         userInfo;        /* pointer to a module-dependent space */

    /* callbacks - except for getCGContext all others are optional */
    CGContextRef (*getCGContext)(QuartzDesc_t dev, void *userInfo);
    int          (*locatePoint)(QuartzDesc_t dev, void *userInfo, double *x, double *y);
    void         (*close)(QuartzDesc_t dev, void *userInfo);
    void         (*newPage)(QuartzDesc_t dev, void *userInfo, int flags);
    void         (*state)(QuartzDesc_t dev,  void *userInfo,  int state);
    void*        (*par)(QuartzDesc_t dev, void *userInfo,  void *par);
    void         (*sync)(QuartzDesc_t dev, void *userInfo);
} QuartzDesc;

/* coordinates:
   - R graphics (positions etc., usually points)
   - real size (e.g. inches)
   - display view (usually pixels)

   bookkeeping:
   - QuartzDevice.width/height:  inches
   - R GE size (.._Size): points
   - physical (on-screen) coordinates : pixels

the current implementation uses points as plotting units (i.e. this is what
Quartz tells R), but the canvas is specified in pixels. The scalex/y factors
specify the conversion factor between pixels and points.
We are *not* using R's scaling facilities, because R doesn't work with
non-square pixels (e.g. circles become ellipses).

FIXME: yes it does -- ipr is a two-element array.
 -- not entirely, because it uses text (e.g. "o") as symbols which is rendered
 in 1:1 aspect ratio and thus is squished on displays with non-square pixels
(That being a bug in Quartz, then!)

Actually, dp not points are used.
*/

#pragma mark QuartzDevice API (for modules)

/* Update should be called when ps or tscale change.
   Conservatively, it should be called on scale change, too, in case
   we decide to abandon the CTM approach */
static void QuartzDevice_Update(QuartzDesc_t desc);

/* this function must be called after a new context is created.
   it primes the context for drawing */
void QuartzDevice_ResetContext(QuartzDesc_t desc) {
    QuartzDesc *qd = ((QuartzDesc*) desc);
    qd->gstate = 0;
    qd->dirty = 0;
    if (qd->getCGContext) {
        CGContextRef ctx = qd->getCGContext(qd, qd->userInfo);
        if (ctx) {
            CGContextSetAllowsAntialiasing(ctx, qd->antialias);
            CGContextSetShouldSmoothFonts(ctx, qd->smooth);
            CGContextScaleCTM(ctx, qd->scalex, qd->scaley);
            CGContextSaveGState(ctx);
            qd->gstate = 1;
        }
    }
}

/* Uses (e.g. in window title) seems to assume this is 1-based */
int QuartzDevice_DevNumber(QuartzDesc_t desc) {
    return 1 + ndevNumber((((QuartzDesc*) desc)->dev));
}

double QuartzDevice_GetWidth(QuartzDesc_t desc)	{ return ((QuartzDesc*) desc)->width;  }
double QuartzDevice_GetHeight(QuartzDesc_t desc) { return ((QuartzDesc*) desc)->height;   }
void   QuartzDevice_SetSize(QuartzDesc_t desc, double width, double height)
{
    QuartzDesc *qd = ((QuartzDesc*) desc);
    qd->width = width;
    qd->height = height;
    qd->dev->right = width*72.0;
    qd->dev->bottom = height*72.0;
}

double QuartzDevice_GetScaledWidth(QuartzDesc_t desc)   { QuartzDesc *qd=((QuartzDesc*) desc); return qd->scalex*qd->width*72.0; }
double QuartzDevice_GetScaledHeight(QuartzDesc_t desc)  { QuartzDesc *qd=((QuartzDesc*) desc); return qd->scaley*qd->height*72.0; }
void QuartzDevice_SetScaledSize(QuartzDesc_t desc, double width, double height) {
    QuartzDesc *qd=((QuartzDesc*) desc);
    QuartzDevice_SetSize(desc, width/qd->scalex/72.0, height/qd->scaley/72.0);
}

double QuartzDevice_GetXScale(QuartzDesc_t desc) { return ((QuartzDesc*) desc)->scalex;  }
double QuartzDevice_GetYScale(QuartzDesc_t desc) { return ((QuartzDesc*) desc)->scaley;  }
void   QuartzDevice_SetScale(QuartzDesc_t desc, double scalex, double scaley) {
    ((QuartzDesc*) desc)->scalex = scalex;
    ((QuartzDesc*) desc)->scaley = scaley;
    QuartzDevice_Update(desc);
}

double QuartzDevice_GetTextScale(QuartzDesc_t desc) {
    return ((QuartzDesc*) desc)->tscale;
}

void   QuartzDevice_SetTextScale(QuartzDesc_t desc, double scale) {
    ((QuartzDesc*) desc)->tscale = scale;
    QuartzDevice_Update(desc);
}

double QuartzDevice_GetPointSize(QuartzDesc_t desc) {
    return ((QuartzDesc*) desc)->ps;
}

void   QuartzDevice_SetPointSize(QuartzDesc_t desc, double ps) {
    ((QuartzDesc*) desc)->ps = ps;
    QuartzDevice_Update(desc);
}

int   QuartzDevice_GetDirty(QuartzDesc_t desc) { return ((QuartzDesc*) desc)->dirty; }
void  QuartzDevice_SetDirty(QuartzDesc_t desc,int dirty) { ((QuartzDesc*) desc)->dirty = dirty; }

int   QuartzDevice_GetAntialias(QuartzDesc_t desc) { return ((QuartzDesc*) desc)->antialias; }
void  QuartzDevice_SetAntialias(QuartzDesc_t desc,int aa) {
    QuartzDesc *qd = (QuartzDesc*) desc;
    qd->antialias  = aa;
    if(NULL != qd->getCGContext)
        CGContextSetAllowsAntialiasing( qd->getCGContext(qd, qd->userInfo), aa );
}

void QuartzDevice_Kill(QuartzDesc_t desc) {
    pGEDevDesc dd = GEgetDevice(ndevNumber(((QuartzDesc*) desc)->dev));
    if (dd) GEkillDevice(dd);
}

int   QuartzDesc_GetFontSmooth(QuartzDesc_t desc) { return ((QuartzDesc*) desc)->smooth; }
void  QuartzDesc_SetFontSmooth(QuartzDesc_t desc, int fs) {
    QuartzDesc *qd = (QuartzDesc*) desc;
    qd->smooth = fs;
    if(qd->getCGContext)
        CGContextSetShouldSmoothFonts( qd->getCGContext(qd, qd->userInfo), fs);
}

int   QuartzDevice_GetBackground(QuartzDesc_t desc) { return ((QuartzDesc*) desc)->bg; }

static void   QuartzDevice_Update(QuartzDesc_t desc)
{
    QuartzDesc *qd = (QuartzDesc*) desc;
    pDevDesc dev= qd->dev;

    /* pre-scaling happens in Quartz (using CTM), so scales should not be
       reflected in R measurements. We tell R to use 72dpi which corresponds
       to plotting in pt coordinates */
    dev->cra[0] = 0.9*qd->ps*qd->tscale;
    dev->cra[1] = 1.2*qd->ps*qd->tscale;
    dev->ipr[0] = 1.0/72.0;
    dev->ipr[1] = 1.0/72.0;
}

void QuartzDevice_ReplayDisplayList(QuartzDesc_t desc)
{
    QuartzDesc *qd = (QuartzDesc*) desc;
    int _dirty = qd->dirty;
    pGEDevDesc gdd = desc2GEDesc(qd->dev);
    qd->redraw = 1;
    /* CHECK this */
    if(gdd->displayList != R_NilValue) GEplayDisplayList(gdd);
    qd->redraw = 0;
    qd->dirty = _dirty; /* we do NOT change the dirty flag */
}

void* QuartzDevice_GetSnapshot(QuartzDesc_t desc, int last)
{
    QuartzDesc *qd = (QuartzDesc*) desc;
    pGEDevDesc gd  = GEgetDevice(ndevNumber(qd->dev));
    SEXP snap;
    if (last)
	snap = desc2GEDesc(qd->dev)->savedSnapshot;
    else
	snap = GEcreateSnapshot(gd);
    if (R_NilValue == VECTOR_ELT(snap, 0))
	snap = 0;
    return (snap == R_NilValue) ? 0 : snap;
}

void QuartzDevice_RestoreSnapshot(QuartzDesc_t desc, void* snap)
{
    QuartzDesc *qd = (QuartzDesc*) desc;
    pGEDevDesc gd  = GEgetDevice(ndevNumber(qd->dev));
    if(NULL == snap) return; /*Aw, hell no!*/
    PROTECT((SEXP)snap);
    if(R_NilValue == VECTOR_ELT(snap,0))
        warning("Tried to restore an empty snapshot?");
    qd->redraw = 1;
    GEplaySnapshot((SEXP)snap, gd);
    qd->redraw = 0;
    qd->dirty = 0; /* we reset the dirty flag */
    UNPROTECT(1);
}

#pragma mark RGD API Function Prototypes

static void     RQuartz_Close(pDevDesc);
static void     RQuartz_Activate(pDevDesc);
static void     RQuartz_Deactivate(pDevDesc);
static void     RQuartz_Size(double*, double*, double*, double*, pDevDesc);
static void     RQuartz_NewPage(const pGEcontext, pDevDesc);
static void     RQuartz_Clip(double, double, double, double, pDevDesc);
static double   RQuartz_StrWidth(const char*, const pGEcontext, pDevDesc);
static void     RQuartz_Text(double, double, const char*, double, double, const pGEcontext, pDevDesc);
static void     RQuartz_Rect(double, double, double, double, const pGEcontext, pDevDesc);
static void     RQuartz_Circle(double, double, double, const pGEcontext, pDevDesc);
static void     RQuartz_Line(double, double, double, double, const pGEcontext, pDevDesc);
static void     RQuartz_Polyline(int, double*, double*, const pGEcontext, pDevDesc);
static void     RQuartz_Polygon(int, double*, double*, const pGEcontext, pDevDesc);
static Rboolean RQuartz_Locator(double*, double*, pDevDesc);
static void     RQuartz_Mode(int mode, pDevDesc);
static void     RQuartz_MetricInfo(int, const pGEcontext , double*, double*, double*, pDevDesc);

#pragma mark Quartz device implementation

void* QuartzDevice_Create(void *_dev, QuartzBackend_t *def)
{
    pDevDesc dev = _dev;

    dev->startfill = def->bg;
    dev->startcol  = R_RGB(0, 0, 0);
    dev->startps   = def->pointsize;
    dev->startfont = 1;
    dev->startlty  = LTY_SOLID;
    dev->startgamma= 1;

    /* Set up some happy pointers */
    dev->close        = RQuartz_Close;
    dev->activate     = RQuartz_Activate;
    dev->deactivate   = RQuartz_Deactivate;
    dev->size         = RQuartz_Size;
    dev->newPage      = RQuartz_NewPage;
    dev->clip         = RQuartz_Clip;
    dev->strWidth     = RQuartz_StrWidth;
    dev->text         = RQuartz_Text;
    dev->rect         = RQuartz_Rect;
    dev->circle       = RQuartz_Circle;
    dev->line         = RQuartz_Line;
    dev->polyline     = RQuartz_Polyline;
    dev->polygon      = RQuartz_Polygon;
    dev->locator      = RQuartz_Locator;
    dev->mode         = RQuartz_Mode;
    dev->metricInfo   = RQuartz_MetricInfo;
    dev->hasTextUTF8  = TRUE;
    dev->textUTF8     = RQuartz_Text;
    dev->strWidthUTF8 = RQuartz_StrWidth;

    dev->left = 0;
    dev->top  = 0;


    /* Magic numbers from on high. */
    dev->xCharOffset = 0.4900;
    dev->yCharOffset = 0.3333;
    dev->yLineBias   = 0.20; /* This is .2 for PS/PDF devices... */

    dev->canClip       = TRUE;
    dev->canHAdj       = 2;
    dev->canChangeGamma= FALSE;
    dev->displayListOn = TRUE; /* reset later depending on type */

    QuartzDesc *qd = calloc(1, sizeof(QuartzDesc));
    qd->width      = def->width;
    qd->height     = def->height;
    qd->userInfo   = def->userInfo;
    qd->getCGContext=def->getCGContext;
    qd->locatePoint= def->locatePoint;
    qd->close      = def->close;
    qd->newPage    = def->newPage;
    qd->state      = def->state;
    qd->sync       = def->sync;
    qd->scalex     = def->scalex;
    qd->scaley     = def->scaley;
    qd->tscale     = 1.0;
    qd->ps         = def->pointsize;
    qd->bg         = def->bg;
    qd->canvas     = def->canvas;
    qd->antialias  = /* FIXME: aa as a flag */ 1;
    qd->flags      = def->flags;
    qd->gstate     = 0;

    dev->deviceSpecific = qd;
    qd->dev             = dev;

    QuartzDevice_Update(qd);

    /* Re-set for bitmap devices later */
     dev->right = def->width*72.0;
     dev->bottom= def->height*72.0;;

    qd->clipRect = CGRectMake(0, 0, dev->right, dev->bottom);

    qd->dirty = 0;
    qd->redraw= 0;
    qd->async = 0;
    return (QuartzDesc_t)qd;
}

static QuartzFunctions_t qfn = {
    QuartzDevice_Create,
    QuartzDevice_DevNumber,
    QuartzDevice_Kill,
    QuartzDevice_ResetContext,
    QuartzDevice_GetWidth,
    QuartzDevice_GetHeight,
    QuartzDevice_SetSize,
    QuartzDevice_GetScaledWidth,
    QuartzDevice_GetScaledHeight,
    QuartzDevice_SetScaledSize,
    QuartzDevice_GetXScale,
    QuartzDevice_GetYScale,
    QuartzDevice_SetScale,
    QuartzDevice_SetTextScale,
    QuartzDevice_GetTextScale,
    QuartzDevice_SetPointSize,
    QuartzDevice_GetPointSize,
    QuartzDevice_GetDirty,
    QuartzDevice_SetDirty,
    QuartzDevice_ReplayDisplayList,
    QuartzDevice_GetSnapshot,
    QuartzDevice_RestoreSnapshot,
    QuartzDevice_GetAntialias,
    QuartzDevice_SetAntialias,
    QuartzDevice_GetBackground
};

QuartzFunctions_t *getQuartzAPI() {
    return &qfn;
}

/* old OS X versions has different names for some of the CGFont stuff */
#if MAC_OS_X_VERSION_MAX_ALLOWED <= MAC_OS_X_VERSION_10_4
#define CGFontCreateWithFontName CGFontCreateWithName
#define CGFontGetGlyphBBoxes CGFontGetGlyphBoundingBoxes
#define CGFontGetGlyphsForUnichars CGFontGetGlyphsForUnicodes
/* and some missing declarations */
extern CGFontRef CGFontCreateWithName(CFStringRef);
extern bool CGFontGetGlyphAdvances(CGFontRef font, const CGGlyph glyphs[], size_t count, int advances[]);
extern int CGFontGetUnitsPerEm(CGFontRef font);
extern bool CGFontGetGlyphBBoxes(CGFontRef font, const CGGlyph glyphs[], size_t count, CGRect bboxes[]);
#endif

/* These are internal (GlyphsForUnichars didn't used to be... Anyway...) */
extern CGFontRef CGContextGetFont(CGContextRef);
extern void CGFontGetGlyphsForUnichars(CGFontRef, const UniChar[], const CGGlyph[], size_t);


#define DEVDESC pDevDesc dd
#define CTXDESC const pGEcontext gc, pDevDesc dd

#define DEVSPEC QuartzDesc *xd = (QuartzDesc*) dd->deviceSpecific; CGContextRef ctx = xd->getCGContext(xd, xd->userInfo)
#define DRAWSPEC QuartzDesc *xd = (QuartzDesc*) dd->deviceSpecific; CGContextRef ctx = xd->getCGContext(xd, xd->userInfo); xd->dirty = 1
#define XD QuartzDesc *xd = (QuartzDesc*) dd->deviceSpecific
#pragma mark Device Implementation

CFStringRef RQuartz_FindFont(int fontface, char *fontfamily)
{
    SEXP ns, env, db, names;
    PROTECT_INDEX index;
    CFStringRef fontName = CFSTR("");
    PROTECT(ns = R_FindNamespace(ScalarString(mkChar("grDevices"))));
    PROTECT_WITH_INDEX(env = findVar(install(".Quartzenv"), ns), &index);
    if(TYPEOF(env) == PROMSXP)
        REPROTECT(env = eval(env,ns) ,index);
    PROTECT(db    = findVar(install(".Quartz.Fonts"), env));
    PROTECT(names = getAttrib(db, R_NamesSymbol));
    if(strlen(fontfamily) > 0) {
        int i;
        for(i = 0; i < length(names); i++)
            if(0 == strcmp(fontfamily, CHAR(STRING_ELT(names, i)))) break;
        if(i < length(names))
            fontName = CFStringCreateWithCString(kCFAllocatorDefault,
						 CHAR(STRING_ELT(VECTOR_ELT(db, i), fontface - 1)),
						 kCFStringEncodingUTF8);
    }
    UNPROTECT(4);
    return fontName;
}

CGFontRef RQuartz_Font(CTXDESC)
{
    int fontface = gc->fontface;
    CFMutableStringRef fontName = CFStringCreateMutable(kCFAllocatorDefault, 0);
     if((gc->fontface == 5) || (strcmp(gc->fontfamily, "symbol") == 0))
        CFStringAppend(fontName,CFSTR("Symbol"));
    else {
        CFStringRef font = RQuartz_FindFont(gc->fontface, gc->fontfamily);
        if(CFStringGetLength(font) > 0) {
            fontface = 1; /* This is handled by the lookup process */
            CFStringAppend(fontName, font);
        }
        CFRelease(font);
    }
    /* the default is Arial, but family="sans" is Helvetica */
    if(CFStringGetLength(fontName) == 0)
        CFStringAppend(fontName, CFSTR("Arial"));
    if(fontface == 2)
        CFStringAppend(fontName, CFSTR(" Bold"));
    if(fontface == 3)
        CFStringAppend(fontName, CFSTR(" Italic"));
    if(fontface == 4)
        CFStringAppend(fontName, CFSTR(" Bold Italic"));
    CGFontRef  font = CGFontCreateWithFontName(fontName);
    if(font == NULL) {
        /* Fall back on ATS */
        ATSFontRef tmp = ATSFontFindFromName(fontName, kATSOptionFlagsDefault);
        font = CGFontCreateWithPlatformFont(&tmp);
    }
    if(font == NULL) CFShow(fontName);  /* FIXME needs warning message */
    CFRelease(fontName);
    return font;
}

#define RQUARTZ_FILL   (1)
#define RQUARTZ_STROKE (1<<1)
#define RQUARTZ_LINE   (1<<2)
#define RQUARTZ_FONT   (1<<3)

void RQuartz_Set(CGContextRef ctx,const pGEcontext gc,int flags) {
    if(flags & RQUARTZ_FILL) {
        int fill = gc->fill;
        CGContextSetRGBFillColor(ctx, R_RED(fill)/255.0, R_GREEN(fill)/255.0, R_BLUE(fill)/255.0, R_ALPHA(fill)/255.0);
    }
    if(flags & RQUARTZ_STROKE) {
        int stroke = gc->col;
        CGContextSetRGBStrokeColor(ctx, R_RED(stroke)/255.0, R_GREEN(stroke)/255.0, R_BLUE(stroke)/255.0, R_ALPHA(stroke)/255.0);
    }
    if(flags & RQUARTZ_LINE) {
        CGFloat dashlist[8];
        int   i, ndash = 0;
        int   lty = gc->lty;
	float lwd = gc->lwd * 0.75;
        CGContextSetLineWidth(ctx, lwd);

        for(i = 0; i < 8 && lty; i++) {
            dashlist[ndash++] = (lwd >= 1 ? lwd : 1) * (lty & 15);
            lty >>= 4;
        }
        CGContextSetLineDash(ctx, 0, dashlist, ndash);
        CGLineCap cap = kCGLineCapButt;
        switch(gc->lend) {
            case GE_ROUND_CAP:  cap = kCGLineCapRound;  break;
            case GE_BUTT_CAP:   cap = kCGLineCapButt;   break;
            case GE_SQUARE_CAP: cap = kCGLineCapSquare; break;
        }
        CGContextSetLineCap(ctx,cap);
        CGLineJoin join = kCGLineJoinRound;
        switch(gc->ljoin) {
            case GE_ROUND_JOIN: join = kCGLineJoinRound; break;
            case GE_MITRE_JOIN: join = kCGLineJoinMiter; break;
            case GE_BEVEL_JOIN: join = kCGLineJoinBevel; break;
        }
        CGContextSetLineJoin(ctx, join);
        CGContextSetMiterLimit(ctx, gc->lmitre);
    }
    if(flags & RQUARTZ_FONT) {
        CGFontRef font = RQuartz_Font(gc, NULL);
        CGContextSetFont(ctx, font);
        CGContextSetFontSize(ctx, gc->cex * gc->ps);
    }
}

#define SET(X) RQuartz_Set(ctx, gc, (X))
#define NOCTX { xd->async = 1; return; }
#define NOCTXR(V) { xd->async = 1; return(V); }


static void RQuartz_Close(DEVDESC)
{
    XD;
    if (xd->close) xd->close(xd, xd->userInfo);
}

static void RQuartz_Activate(DEVDESC)
{
    XD;
    if (xd->state) xd->state(xd, xd->userInfo, 1);
}

static void RQuartz_Deactivate(DEVDESC)
{
    XD;
    if (xd->state) xd->state(xd, xd->userInfo, 0);
}

static void RQuartz_Size(double *left, double *right, double *bottom, double *top, DEVDESC)
{
    XD;
    *left = *top = 0;
    *right  = QuartzDevice_GetWidth(xd) * 72.0;
    *bottom = QuartzDevice_GetHeight(xd) * 72.0;
}

static void RQuartz_NewPage(CTXDESC)
{
    {
        DRAWSPEC;
        ctx = NULL;
        if (xd->newPage) xd->newPage(xd, xd->userInfo, xd->redraw ? QNPF_REDRAW : 0);
    }
    { /* we have to re-fetch the status *after* newPage since it may have changed it */
        DRAWSPEC;
        if (!ctx) NOCTX;
        {
            CGRect bounds = CGRectMake(0, 0,
				       QuartzDevice_GetScaledWidth(xd) * 72.0,
				       QuartzDevice_GetScaledHeight(xd) * 72.0);
	    /* The logic is to paint the canvas then gc->fill.
	       (The canvas colour is set to 0 on non-screen devices.)
	     */
	    if (R_ALPHA(xd->canvas) >0 && !R_OPAQUE(gc->fill)) {
		/* Paint the canvas colour. */
		int savefill = gc->fill;
		CGContextClearRect(ctx, bounds);
		gc->fill = xd->canvas;
		SET(RQUARTZ_FILL);
		CGContextFillRect(ctx, bounds);
		gc->fill = savefill;
	    }
	    SET(RQUARTZ_FILL); /* this will fill with gc->fill */
            CGContextFillRect(ctx, bounds);
        }
    }
}

static void RQuartz_Clip(double x0, double x1, double y0, double y1, DEVDESC)
{
    DRAWSPEC;
    if (!ctx) NOCTX;
    if(xd->gstate > 0) {
        --xd->gstate;
        CGContextRestoreGState(ctx);
    }
    CGContextSaveGState(ctx);
    xd->gstate++;
    if(x1 > x0) { double t = x1; x1 = x0;x0 = t; }
    if(y1 > y0) { double t = y1; y1 = y0;y0 = t; }
    xd->clipRect = CGRectMake(x0, y0, x1 - x0, y1 - y0);
    CGContextClipToRect(ctx, xd->clipRect);
}

/* non-symbol text is sent in UTF-8 */
static CFStringRef text2unichar(CTXDESC, const char *text, UniChar **buffer, int *free)
{
    CFStringRef str;
    if(gc->fontface == 5 || strcmp(gc->fontfamily, "symbol") == 0)
        str = CFStringCreateWithCString(NULL, text, kCFStringEncodingMacSymbol);
    else {
        str = CFStringCreateWithCString(NULL, text, kCFStringEncodingUTF8);
        /* Try fallback Latin1 encoding if UTF8 doesn't work 
	   -- should no longer be needed. */
        if(!str)
            CFStringCreateWithCString(NULL, text, kCFStringEncodingISOLatin1);
    }
    if (!str) return NULL;
    *buffer = (UniChar*) CFStringGetCharactersPtr(str);
    if (*buffer == NULL) {
        CFIndex length = CFStringGetLength(str);
        *buffer = malloc(length * sizeof(UniChar));
        CFStringGetCharacters(str, CFRangeMake(0, length), *buffer);
        *free = 1;
    }
    return str;
}

static double RQuartz_StrWidth(const char *text, CTXDESC)
{
    DEVSPEC;
    if (!ctx) NOCTXR(strlen(text) * 10.0); /* for sanity reasons */
    SET(RQUARTZ_FONT);
    {
        CGFontRef font = CGContextGetFont(ctx);
        float aScale   = (gc->cex * gc->ps * xd->tscale) / CGFontGetUnitsPerEm(font);
        UniChar *buffer;
        CGGlyph *glyphs;
        int     *advances;
        int Free = 0, len,i;
        CFStringRef str = text2unichar(gc, dd, text, &buffer, &Free);
	if (!str) return 0.0; /* invalid text contents */
        len = CFStringGetLength(str);
        glyphs = malloc(sizeof(CGGlyph) * len);
        advances = malloc(sizeof(int) * len);
        CGFontGetGlyphsForUnichars(font, buffer, glyphs,len);
        CGFontGetGlyphAdvances(font, glyphs, len, advances);
        {
            float width = 0.0; /* aScale*CGFontGetLeading(CGContextGetFont(ctx)); */
            for(i = 0; i < len; i++) width += aScale * advances[i];
            free(advances);
            free(glyphs);
            if(Free) free(buffer);
            CFRelease(str);
            return width;
        }
    }
}

static void RQuartz_Text(double x, double y, const char *text, double rot, double hadj, CTXDESC)
{
    DRAWSPEC;
    if (!ctx) NOCTX;
    /* A stupid hack because R isn't consistent. */
    int fill = gc->fill;
    gc->fill = gc->col;
    SET(RQUARTZ_FILL | RQUARTZ_STROKE | RQUARTZ_FONT);
    gc->fill = fill;
    CGFontRef font = CGContextGetFont(ctx);
    float aScale   = (gc->cex * gc->ps * xd->tscale) / CGFontGetUnitsPerEm(font);
    UniChar *buffer;
    CGGlyph   *glyphs;

    int Free = 0, len, i;
    float width = 0.0;
    CFStringRef str = text2unichar(gc, dd, text, &buffer, &Free);
    if (!str) return; /* invalid text contents */
    len = CFStringGetLength(str);
    glyphs = malloc(sizeof(CGGlyph) * len);
    CGFontGetGlyphsForUnichars(font, buffer, glyphs, len);
    int      *advances = malloc(sizeof(int) * len);
    CGSize   *g_adv    = malloc(sizeof(CGSize) * len);

    CGFontGetGlyphAdvances(font, glyphs, len, advances);
    for(i =0 ; i < len; i++) {
	width += advances[i] * aScale;
	g_adv[i] = CGSizeMake(aScale * advances[i] * cos(-0.0174532925*rot), aScale*advances[i]*sin(-0.0174532925 * rot));
    }
    free(advances);
    CGContextSetTextMatrix(ctx,
			   CGAffineTransformConcat(CGAffineTransformMakeScale(1.0, -1.0),
						   CGAffineTransformMakeRotation(-0.0174532925 * rot)));
    double ax = (width * hadj) * cos(-0.0174532925 * rot);
    double ay = (width * hadj) * sin(-0.0174532925 * rot);
    /*      double h  = CGFontGetXHeight(CGContextGetFont(ctx))*aScale; */
    CGContextSetTextPosition(ctx, x - ax, y - ay);
    /*      Rprintf("%s,%.2f %.2f (%.2f,%.2f) (%d,%f)\n",text,hadj,width,ax,ay,CGFontGetUnitsPerEm(CGContextGetFont(ctx)),CGContextGetFontSize(ctx));       */
    CGContextShowGlyphsWithAdvances(ctx,glyphs, g_adv, len);
    free(glyphs);
    free(g_adv);
    if(Free) free(buffer);
    CFRelease(str);
}

static void RQuartz_Rect(double x0, double y0, double x1, double y1, CTXDESC)
{
    DRAWSPEC;
    if (!ctx) NOCTX;
    SET(RQUARTZ_FILL | RQUARTZ_STROKE | RQUARTZ_LINE);
    CGContextBeginPath(ctx);
    CGContextAddRect(ctx, CGRectMake(x0, y0, x1 - x0, y1 - y0));
    CGContextDrawPath(ctx, kCGPathFillStroke);
}

static void RQuartz_Circle(double x, double y, double r, CTXDESC)
{
    DRAWSPEC;
    if (!ctx) NOCTX;
    SET(RQUARTZ_FILL | RQUARTZ_STROKE | RQUARTZ_LINE);
    double r2 = 2.0*r;
    CGContextBeginPath(ctx);
    CGContextAddEllipseInRect(ctx,CGRectMake(x-r,y-r,r2,r2));
    CGContextDrawPath(ctx,kCGPathFillStroke);
}

static void RQuartz_Line(double x1, double y1, double x2, double y2, CTXDESC)
{
    DRAWSPEC;
    if (!ctx) NOCTX;
    SET(RQUARTZ_STROKE | RQUARTZ_LINE);
    CGContextBeginPath(ctx);
    CGContextMoveToPoint(ctx, x1, y1);
    CGContextAddLineToPoint(ctx, x2, y2);
    CGContextStrokePath(ctx);
}

static void RQuartz_Polyline(int n, double *x, double *y, CTXDESC)
{
    if (n < 2) return;
    int i;
    DRAWSPEC;
    if (!ctx) NOCTX;
    SET(RQUARTZ_STROKE | RQUARTZ_LINE);
    CGContextBeginPath(ctx);
    CGContextMoveToPoint(ctx, x[0], y[0]);
    for(i = 1 ; i < n; i++)
	CGContextAddLineToPoint(ctx, x[i], y[i]);
    CGContextStrokePath(ctx);
}

static void RQuartz_Polygon(int n, double *x, double *y, CTXDESC)
{
    if (n < 2) return;
    int i;
    DRAWSPEC;
    if (!ctx) NOCTX;
    SET(RQUARTZ_FILL | RQUARTZ_STROKE | RQUARTZ_LINE);
    CGContextBeginPath(ctx);
    CGContextMoveToPoint(ctx, x[0], y[0]);
    for(i = 1; i < n; i++)
	CGContextAddLineToPoint(ctx, x[i], y[i]);
    CGContextClosePath(ctx);
    CGContextDrawPath(ctx, kCGPathFillStroke);
}

static void RQuartz_Mode(int mode, DEVDESC)
{
    DEVSPEC;
    if (!ctx) NOCTX;
    /* don't do anything in redraw as we can signal the end */
    if (xd->redraw) return;
    /* mode=0 -> drawing complete, signal sync */
    if (mode == 0) {
        if (xd->sync)
            xd->sync(xd, xd->userInfo);
        else
            CGContextSynchronize(ctx);
    }
}

static void
RQuartz_MetricInfo(int c, const pGEcontext gc,
		   double *ascent, double *descent, double *width,
		   pDevDesc dd)
{
    DRAWSPEC;
    if (!ctx) { /* dummy data if we have no context, for sanity reasons */
        *ascent = 10.0;
        *descent= 2.0;
        *width  = 9.0;
        NOCTX;
    }
    SET(RQUARTZ_FONT);
    {
	CGFontRef font = CGContextGetFont(ctx);
        float aScale   = (gc->cex * gc->ps * xd->tscale) / CGFontGetUnitsPerEm(font);
	UniChar  *buffer, single;
        CGGlyph  glyphs[8];
	CFStringRef str = NULL;
        int free_buffer = 0, len;
	*width = *ascent = *descent = 0.0; /* data for bail-out cases */
	if (c >= 0 && c <= ((mbcslocale && gc->fontface != 5) ? 127 : 255)) {
	    char    text[2] = { c, 0 };
	    str = text2unichar(gc, dd, text, &buffer, &free_buffer);
	    if(!str) return;
	    len = CFStringGetLength(str);
	    if (len > 7) return; /* this is basically impossible,
				    but you never know */
	} else {
	    single = (UniChar) ((c < 0) ? -c : c);
	    buffer = &single;
	    len = 1;
	}
        *width = 0.0;
        CGFontGetGlyphsForUnichars(font, buffer, glyphs, len);
        {
	    int i;
            int    advances[8];
            CGRect bboxes[8];
            CGFontGetGlyphAdvances(font, glyphs, len, advances);
            CGFontGetGlyphBBoxes(font, glyphs, len, bboxes);
            for(i = 0; i < len; i++)
                *width += advances[i] * aScale;
            *ascent  = aScale * (bboxes[0].size.height + bboxes[0].origin.y);
            *descent = -aScale * bboxes[0].origin.y;
        }
        if (free_buffer) free(buffer);
        if (str) CFRelease(str);
    }
}

static Rboolean RQuartz_Locator(double *x, double *y, DEVDESC)
{
    Rboolean res;
    DEVSPEC;
    ctx = NULL;
    if (!xd->locatePoint)
        return FALSE;
    res = xd->locatePoint(xd, xd->userInfo, x, y);
    *x/=xd->scalex;
    *y/=xd->scaley;
    return res;
}

#pragma mark -
#pragma mark R Interface

#include "qdCocoa.h"
#include "qdBitmap.h"
#include "qdPDF.h"
/* disabled for now until we get to test in on 10.3 #include "qdCarbon.h" */

/* current fake */
Rboolean QuartzCarbon_DeviceCreate(pDevDesc dd, QuartzFunctions_t *fn, QuartzParameters_t *par)
{
    return FALSE;
}

#define ARG(HOW,WHAT) HOW(CAR(WHAT));WHAT = CDR(WHAT)

/* C version of the Quartz call (experimental)
   returns 0 on success, error code on failure */
int Quartz_C(QuartzParameters_t *par, quartz_create_fn_t q_create)
{
    if (!q_create || !par) return -3;
    {
        char    *vmax = vmaxget();
	R_GE_checkVersionOrDie(R_GE_version);
        R_CheckDeviceAvailable();
        {
	    const char *devname = "quartz_off_screen";
	    /* FIXME: check this allocation */
            pDevDesc dev    = calloc(1, sizeof(NewDevDesc));

            if (!dev)
                return -1;

            if (!q_create(dev, &qfn, par)) {
                vmaxset(vmax);
                free(dev);
                return -2;
            }
	    if(streql(par->type, "") || streql(par->type, "native")
	       || streql(par->type, "cocoa") || streql(par->type, "carbon"))
		devname = "quartz";
            gsetVar(install(".Device"), mkString(devname), R_BaseEnv);
            pGEDevDesc dd = GEcreateDevDesc(dev);
            GEaddDevice(dd);
            GEinitDisplayList(dd);
            vmaxset(vmax);
        }
    }
    return 0;
}

/* ARGS: type, file, width, height, ps, family, antialias, fontsm,
   title, bg, canvas, dpi */
SEXP Quartz(SEXP args)
{
    SEXP tmps, bgs, canvass;
    double   width, height, ps;
    Rboolean antialias, smooth, autorefresh = TRUE, succ = FALSE;
    int      quartzpos, bg, canvas, module = 0;
    double   mydpi[2], *dpi = 0;
    const char *type, *mtype, *file, *family, *title;

    char    *vmax = vmaxget();
    /* Get function arguments */
    args = CDR(args); /* Skip the call */
    if (TYPEOF(CAR(args)) != STRSXP || LENGTH(CAR(args)) < 1)
        type = "";
    else
        type  = CHAR(STRING_ELT(CAR(args), 0));
    args = CDR(args);
    /* we may want to support connections at some point, but not yet ... */
    tmps = CAR(args);    args = CDR(args);
    if (isNull(tmps)) 
	file = NULL;
    else if (isString(tmps) && LENGTH(tmps) >= 1)
        file  = CHAR(STRING_ELT(tmps, 0));
    else
        error(_("invalid 'file' argument"));
    width     = ARG(asReal,args);
    height    = ARG(asReal,args);
    ps        = ARG(asReal,args);
    family    = CHAR(STRING_ELT(CAR(args), 0)); args = CDR(args);
    antialias = ARG(asLogical,args);
    smooth    = ARG(asLogical,args);
    title     = CHAR(STRING_ELT(CAR(args), 0)); args = CDR(args);
    bgs       = CAR(args); args = CDR(args);
    bg        = RGBpar(bgs, 0);
    canvass   = CAR(args); args = CDR(args);
    canvas    = RGBpar(canvass, 0) | 0xff000000; /* force opaque */
    tmps      = CAR(args); args = CDR(args);
    if (!isNull(tmps)) {
        tmps = coerceVector(tmps, REALSXP);
        if (LENGTH(tmps) > 0) {
            dpi = mydpi;
            mydpi[0] = REAL(tmps)[0];
            if (LENGTH(tmps) > 1)
                mydpi[1] = REAL(tmps)[1];
            else
                mydpi[1] = mydpi[0];
        }
    }
    /* just in case someone passed NAs/NaNs */
    if (dpi && (ISNAN(dpi[0]) || ISNAN(dpi[1]))) dpi=0;

    if (ISNAN(width) || ISNAN(height) || width <= 0.0 || height <= 0.0)
        error(_("invalid Quartz device size"));

    if (type) {
        const quartz_module_t *m = quartz_modules;
	mtype = type;
        while (m->type) {
            if (!strcasecmp(type, m->type)) {
                module = m->qbe;
                if (m->subst) mtype = m->subst;
                break;
            }
            m++;
        }
    }
    if (!strncasecmp(type, "bitmap:", 7)) {
        module = QBE_BITMAP;
        mtype = mtype + 7;
    }

    quartzpos = 1;

    R_GE_checkVersionOrDie(R_GE_version);
    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	pDevDesc dev = calloc(1, sizeof(NewDevDesc));

	if (!dev)
	    error(_("Unable to create device description."));

	QuartzParameters_t qpar = {
	    sizeof(qpar),
	    mtype, file, title,
	    -1.0, -1.0, width, height, ps,
	    family,
	    antialias ? QPFLAG_ANTIALIAS: 0,
	    -1, /* connection */
	    bg, canvas,
	    dpi
	};

	/* re-routed code has the first shot */
	if (ptr_QuartzBackend)
	    succ = ptr_QuartzBackend(dev, &qfn, &qpar);

	if (!succ) { /* try internal modules next */
	    switch (module) {
            case QBE_COCOA:
                succ = QuartzCocoa_DeviceCreate(dev, &qfn, &qpar);
                break;
            case QBE_NATIVE:
                /* native is essentially cocoa with carbon fall-back */
                succ = QuartzCocoa_DeviceCreate(dev, &qfn, &qpar);
                if (succ) break;
            case QBE_CARBON:
                succ = QuartzCarbon_DeviceCreate(dev, &qfn, &qpar);
                break;
            case QBE_PDF:
		qpar.canvas = 0; /* so not used */
                succ = QuartzPDF_DeviceCreate(dev, &qfn, &qpar);
		dev->displayListOn = FALSE;
                break;
            case QBE_BITMAP:
		/* we need to set up the default file name here, where we
		   know the original type name. */
		if (file == NULL) {
		    static char deffile[30];
		    snprintf(deffile, 30, "%s.%s", "Rplot%03d", type);
		    qpar.file = deffile;
		}
		qpar.canvas = 0; /* so not used */
		succ = QuartzBitmap_DeviceCreate(dev, &qfn, &qpar);
		dev->displayListOn = FALSE;
		dev->right = width * dpi[0];
		dev->bottom = height * dpi[1];
		break;
	    }
	}

	if(!succ) {
	    vmaxset(vmax);
	    free(dev);
	    error(_("Unable to create Quartz device target, given type may not be supported."));
	}
	const char *devname = "quartz_off_screen";
	if(streql(type, "") || streql(type, "native") || streql(type, "cocoa") 
	   || streql(type, "carbon")) devname = "quartz";
 	gsetVar(install(".Device"), mkString(devname), R_BaseEnv);
	pGEDevDesc dd = GEcreateDevDesc(dev);
	GEaddDevice(dd);
	GEinitDisplayList(dd);
    } END_SUSPEND_INTERRUPTS;
    vmaxset(vmax);
    return R_NilValue;
}

#else
/* --- no AQUA support = no Quartz --- */

#include "grDevices.h"

SEXP Quartz(SEXP args)
{
    warning(_("Quartz device is not available on this platform"));
    return R_NilValue;
}

void *getQuartzAPI()
{
    return NULL;
}

#endif
