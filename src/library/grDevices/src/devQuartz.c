/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007-11  The R Foundation
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
/* This sets ptr_QuartzBackend as a symbol in this file */
#define IN_AQUA_C 1
#include <R_ext/QuartzDevice.h>

#include "grDevices.h"

#include <CoreFoundation/CoreFoundation.h>

#define DEVQUARTZ_VERSION 1 /* first public Quartz API version */

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
    int           holdlevel;       /* hold level */
    int           redraw;          /* redraw flag is set when replaying
		                              and inhibits syncs on Mode */
    CGRect        clipRect;        /* clipping rectangle */
    pDevDesc      dev;             /* device structure holding this one */
    CGFontRef     font;            /* currently used font */

    void*         userInfo;        /* pointer to a module-dependent space */

    /* callbacks - except for getCGContext all others are optional */
    CGContextRef (*getCGContext)(QuartzDesc_t dev, void *userInfo);
    int          (*locatePoint)(QuartzDesc_t dev, void *userInfo, double *x, double *y);
    void         (*close)(QuartzDesc_t dev, void *userInfo);
    void         (*newPage)(QuartzDesc_t dev, void *userInfo, int flags);
    void         (*state)(QuartzDesc_t dev,  void *userInfo,  int state);
    void*        (*par)(QuartzDesc_t dev, void *userInfo, int set, const char *key, void *value);
    void         (*sync)(QuartzDesc_t dev, void *userInfo);
    void*        (*cap)(QuartzDesc_t dev, void*userInfo);
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

void QuartzDevice_Activate(QuartzDesc_t desc)
{
    QuartzDesc *qd = (QuartzDesc*) desc;
    if (qd) {
	int n = ndevNumber(qd->dev);
	selectDevice(n);
    }
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

static int quartz_embedding = 0;

static void* QuartzDevice_SetParameter(QuartzDesc_t desc, const char *key, void *value)
{
    if (desc) { /* backend-specific? pass it on */
	QuartzDesc *qd = (QuartzDesc*) desc;
	return (qd->par) ? qd->par(qd, qd->userInfo, 1, key, value) : NULL;
    } else { /* global? try to handle it */
	if (key) {
	    if (!streql(key, QuartzParam_EmbeddingFlags)) {
		if (value) quartz_embedding = ((int*)value)[0];
		return &quartz_embedding;
	    }
	}
    }
    return NULL;
}

void setup_RdotApp(void)
{
    int eflags = QP_Flags_CFLoop | QP_Flags_Cocoa | QP_Flags_Front;
    QuartzDevice_SetParameter(NULL, QuartzParam_EmbeddingFlags, &eflags);
}

static void*  QuartzDevice_GetParameter(QuartzDesc_t desc, const char *key)
{
    if (desc) { /* backend-specific? pass it on */
	QuartzDesc *qd = (QuartzDesc*) desc;
	return (qd->par) ? qd->par(qd, qd->userInfo, 0, key, NULL) : NULL;
    } else { /* global? try to handle it */
	if (key) {
	    if (!streql(key, QuartzParam_EmbeddingFlags)) return &quartz_embedding;
	}
    }
    return NULL;
}

#pragma mark RGD API Function Prototypes

static void     RQuartz_Close(pDevDesc);
static void     RQuartz_Activate(pDevDesc);
static void     RQuartz_Deactivate(pDevDesc);
static void     RQuartz_Size(double*, double*, double*, double*, pDevDesc);
static void     RQuartz_NewPage(const pGEcontext, pDevDesc);
static int      RQuartz_HoldFlush(pDevDesc, int);
static void     RQuartz_Clip(double, double, double, double, pDevDesc);
static double   RQuartz_StrWidth(const char*, const pGEcontext, pDevDesc);
static void     RQuartz_Text(double, double, const char*, double, double, const pGEcontext, pDevDesc);
static void     RQuartz_Rect(double, double, double, double, const pGEcontext, pDevDesc);
static void     RQuartz_Raster(unsigned int *raster, int w, int h,
                       double x, double y, double width, double height,
                       double rot, Rboolean interpolate,
                       const pGEcontext gc, pDevDesc dd);
static SEXP     RQuartz_Cap(pDevDesc dd);
static void     RQuartz_Circle(double, double, double, const pGEcontext, pDevDesc);
static void     RQuartz_Line(double, double, double, double, const pGEcontext, pDevDesc);
static void     RQuartz_Polyline(int, double*, double*, const pGEcontext, pDevDesc);
static void     RQuartz_Polygon(int, double*, double*, const pGEcontext, pDevDesc);
static void     RQuartz_Path(double*, double*, int, int*, Rboolean, const pGEcontext, pDevDesc);
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
    dev->raster       = RQuartz_Raster;
    dev->cap          = RQuartz_Cap;
    dev->circle       = RQuartz_Circle;
    dev->line         = RQuartz_Line;
    dev->polyline     = RQuartz_Polyline;
    dev->polygon      = RQuartz_Polygon;
    dev->path         = RQuartz_Path;
    dev->locator      = RQuartz_Locator;
    dev->mode         = RQuartz_Mode;
    dev->metricInfo   = RQuartz_MetricInfo;
    dev->holdflush    = RQuartz_HoldFlush;
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
    dev->displayListOn = (def->flags & QDFLAG_DISPLAY_LIST) ? TRUE : FALSE;

    dev->haveTransparency = 2;
    dev->haveTransparentBg = 3; /* FIXME: depends on underlying device */
    dev->haveRaster = 2;
    dev->haveCapture = (def->cap) ? 2 : 1;
    dev->haveLocator = (def->locatePoint) ? 2 : 1;

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
    qd->cap        = def->cap;
    qd->scalex     = def->scalex;
    qd->scaley     = def->scaley;
    qd->tscale     = 1.0;
    qd->ps         = def->pointsize;
    qd->bg         = def->bg;
    qd->canvas     = def->canvas;
    qd->antialias  = (def->flags & QPFLAG_ANTIALIAS) ? 1 : 0;
    qd->flags      = def->flags;
    qd->gstate     = 0;
    qd->font       = NULL;

    dev->deviceSpecific = qd;
    qd->dev             = dev;

    QuartzDevice_Update(qd);

    /* Re-set for bitmap devices later */
     dev->right = def->width*72.0;
     dev->bottom= def->height*72.0;

    qd->clipRect = CGRectMake(0, 0, dev->right, dev->bottom);

    qd->dirty = 0;
    qd->redraw= 0;
    qd->async = 0;
    qd->holdlevel = 0;
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
    QuartzDevice_GetBackground,
    QuartzDevice_Activate,
    QuartzDevice_SetParameter,
    QuartzDevice_GetParameter
};

/* currrently unused: was used by R.app via aqua.c */
QuartzFunctions_t *getQuartzAPI() {
    return &qfn;
}

/* old OS X versions has different names for some of the CGFont stuff */
#if MAC_OS_X_VERSION_MAX_ALLOWED <= MAC_OS_X_VERSION_10_4
#define CGFontCreateWithFontName CGFontCreateWithName
#define CGFontGetGlyphBBoxes CGFontGetGlyphBoundingBoxes
/* The following is a real pain. We have to work around bugs in CoreGraphics
   and Apple's dyloader simultaneously so a 10.4 binary runs on 10.5 as well. */
typedef void (*RQFontGetGlyphsForUnichars_t)(CGFontRef a, const UniChar b[], CGGlyph c[], size_t d);
static RQFontGetGlyphsForUnichars_t RQFontGetGlyphsForUnichars;
#include <dlfcn.h> /* dynamically find the right entry point on initialization */
__attribute__((constructor)) static void RQ_init() {
    void *r;
    if ((r = dlsym(RTLD_NEXT, "CGFontGetGlyphsForUnichars")) || (r = dlsym(RTLD_NEXT, "CGFontGetGlyphsForUnicodes")) ||
	(r = dlsym(RTLD_DEFAULT, "CGFontGetGlyphsForUnichars")) || (r = dlsym(RTLD_DEFAULT, "CGFontGetGlyphsForUnicodes")))
	RQFontGetGlyphsForUnichars = (RQFontGetGlyphsForUnichars_t) r;
    else
	error("Cannot load CoreGraphics"); /* this should never be reached but I suppose it's better than a hidden segfault */
}
#define CGFontGetGlyphsForUnichars RQFontGetGlyphsForUnichars
/* and some missing declarations */
extern CGFontRef CGFontCreateWithName(CFStringRef);
extern bool CGFontGetGlyphAdvances(CGFontRef font, const CGGlyph glyphs[], size_t count, int advances[]);
extern int CGFontGetUnitsPerEm(CGFontRef font);
extern bool CGFontGetGlyphBBoxes(CGFontRef font, const CGGlyph glyphs[], size_t count, CGRect bboxes[]);
#else
extern void CGFontGetGlyphsForUnichars(CGFontRef, const UniChar[], CGGlyph[], size_t);
#endif

extern CGFontRef CGContextGetFont(CGContextRef);

#define DEVDESC pDevDesc dd
#define CTXDESC const pGEcontext gc, pDevDesc dd

#define DEVSPEC QuartzDesc *xd = (QuartzDesc*) dd->deviceSpecific; CGContextRef ctx = xd->getCGContext(xd, xd->userInfo)
#define DRAWSPEC QuartzDesc *xd = (QuartzDesc*) dd->deviceSpecific; CGContextRef ctx = xd->getCGContext(xd, xd->userInfo); xd->dirty = 1
#define XD QuartzDesc *xd = (QuartzDesc*) dd->deviceSpecific

#pragma mark Quartz Font Cache

/* Font lookup is expesive yet frequent. Therefore we cache all used ATS fonts (which are global to the app). */

typedef struct font_cache_entry_s {
    ATSFontRef font;
    char *family;
    int  face;
} font_cache_entry_t;

#define max_fonts_per_block 32

typedef struct font_cache_s {
    font_cache_entry_t e[max_fonts_per_block];
    int fonts;
    struct font_cache_s *next;
} font_cache_t;

font_cache_t font_cache, *font_cache_tail = &font_cache;

static ATSFontRef RQuartz_CacheGetFont(const char *family, int face) {
    font_cache_t *fc = &font_cache;
    while (fc) {
        int i = 0, j = fc->fonts;
        while (i < j) {
            if (face == fc->e[i].face && streql(family, fc->e[i].family))
                return fc->e[i].font;
            i++;
        }
        fc = fc->next;
    }
    return 0;
}

static void RQuartz_CacheAddFont(const char *family, int face, ATSFontRef font) {
    if (font_cache_tail->fonts >= max_fonts_per_block)
        font_cache_tail = font_cache_tail->next = (font_cache_t*) calloc(1, sizeof(font_cache_t));
    {
        int i = font_cache_tail->fonts;
        font_cache_tail->e[i].font = font;
        font_cache_tail->e[i].family = strdup(family);
        font_cache_tail->e[i].face = face;
        font_cache_tail->fonts++;
    }
}

#ifdef UNUSED
static void RQuartz_CacheRelease() {
    font_cache_t *fc = &font_cache;
    while (fc) {
        font_cache_t *next = fc->next;
        int i = 0, j = fc->fonts;
        while (i < j) free(fc->e[i++].family);
        if (fc != &font_cache) free(fc);
        fc = next;
    }
    font_cache.fonts = 0;
}
#endif

#pragma mark Device Implementation

/* mapping of virtual family names (e.g "serif") and face to real font names using .Quartzenv$.Quartz.Fonts list */
const char *RQuartz_LookUpFontName(int fontface, const char *fontfamily)
{
    const char *mappedFont = 0;
    SEXP ns, env, db, names;
    PROTECT_INDEX index;
    PROTECT(ns = R_FindNamespace(ScalarString(mkChar("grDevices"))));
    PROTECT_WITH_INDEX(env = findVar(install(".Quartzenv"), ns), &index);
    if(TYPEOF(env) == PROMSXP)
        REPROTECT(env = eval(env,ns) ,index);
    PROTECT(db    = findVar(install(".Quartz.Fonts"), env));
    PROTECT(names = getAttrib(db, R_NamesSymbol));
    if (*fontfamily) {
        int i;
        for(i = 0; i < length(names); i++)
            if(streql(fontfamily, CHAR(STRING_ELT(names, i)))) {
                mappedFont = CHAR(STRING_ELT(VECTOR_ELT(db, i), fontface - 1));
                break;
            }
    }
    UNPROTECT(4);
    return mappedFont;
}

/* get a font according to the current graphics context */
CGFontRef RQuartz_Font(CTXDESC)
{
    const char *fontName = NULL, *fontFamily = gc->fontfamily;
    ATSFontRef atsFont = 0;
    int fontFace = gc->fontface;
    if (fontFace < 1 || fontFace > 5) fontFace = 1; /* just being paranoid */
    if (fontFace == 5)
        fontName = "Symbol";
    else
        fontName = RQuartz_LookUpFontName(fontFace, fontFamily[0] ? fontFamily : "default");
    if (fontName) {
        atsFont = RQuartz_CacheGetFont(fontName, 0); /* face is 0 because we are passing a true font name */
        if (!atsFont) { /* not in the cache, get it */
            CFStringRef cfFontName = CFStringCreateWithCString(NULL, fontName, kCFStringEncodingUTF8);
            atsFont = ATSFontFindFromName(cfFontName, kATSOptionFlagsDefault);
            if (!atsFont)
                atsFont = ATSFontFindFromPostScriptName(cfFontName, kATSOptionFlagsDefault);
            CFRelease(cfFontName);
            if (!atsFont) {
                warning(_("font \"%s\" could not be found for family \"%s\""), fontName, fontFamily);
                return NULL;
            }
            RQuartz_CacheAddFont(fontName, 0, atsFont);
        }
    } else { /* the real font name could not be looked up. We must use cache and/or find the right font by family and face */
        if (!fontFamily[0]) fontFamily = "Arial"; 
	/* Arial is the default, because Helvetica doesn't have Oblique 
	   on 10.4 - maybe change later? */
        atsFont = RQuartz_CacheGetFont(fontFamily, fontFace);
        if (!atsFont) { /* not in the cache? Then we need to find the 
			   proper font name from the family name and face */
            /* as it turns out kATSFontFilterSelectorFontFamily is not 
	       implemented in OS X (!!) so there is no way to query for a 
	       font from a specific family. Therefore we have to use 
	       text-matching heuristics ... very nasty ... */
            char compositeFontName[256];
            /* CFStringRef cfFontName; */
            if (strlen(fontFamily) > 210) error(_("font family name is too long"));
            while (!atsFont) { /* try different faces until exhausted or successful */
                strcpy(compositeFontName, fontFamily);
                if (fontFace == 2 || fontFace == 4) strcat(compositeFontName, " Bold");
                if (fontFace == 3 || fontFace == 4) strcat(compositeFontName, " Italic");
                CFStringRef cfFontName = CFStringCreateWithCString(NULL, compositeFontName, kCFStringEncodingUTF8);
                atsFont = ATSFontFindFromName(cfFontName, kATSOptionFlagsDefault);
                if (!atsFont) atsFont = ATSFontFindFromPostScriptName(cfFontName, kATSOptionFlagsDefault);
                CFRelease(cfFontName);
                if (!atsFont) {
                    if (fontFace == 1) { /* more guessing - fontFace == 1 may need Regular or Roman */
                        strcat(compositeFontName," Regular");
                        cfFontName = CFStringCreateWithCString(NULL, compositeFontName, kCFStringEncodingUTF8);
                        atsFont = ATSFontFindFromName(cfFontName, kATSOptionFlagsDefault);
                        CFRelease(cfFontName);
                        if (!atsFont) {
                            strcpy(compositeFontName, fontFamily);
                            strcat(compositeFontName," Roman");
                            cfFontName = CFStringCreateWithCString(NULL, compositeFontName, kCFStringEncodingUTF8);
                            atsFont = ATSFontFindFromName(cfFontName, kATSOptionFlagsDefault);
                            CFRelease(cfFontName);
                        }
                    } else if (fontFace == 3 || fontFace == 4) { /* Oblique is sometimes used instead of Italic (e.g. in Helvetica) */
                        strcpy(compositeFontName, fontFamily);
                        if (fontFace == 4) strcat(compositeFontName, " Bold");
                        strcat(compositeFontName," Oblique");
                        cfFontName = CFStringCreateWithCString(NULL, compositeFontName, kCFStringEncodingUTF8);
                        atsFont = ATSFontFindFromName(cfFontName, kATSOptionFlagsDefault);
                        CFRelease(cfFontName);                    
                    }
                }
                if (!atsFont) { /* try to fall back to a more plain face */
                    if (fontFace == 4) fontFace = 2;
                    else if (fontFace != 1) fontFace = 1;
                    else break;
                    atsFont = RQuartz_CacheGetFont(fontFamily, fontFace);
                    if (atsFont) break;
                }
            }
            if (!atsFont)
                warning(_("no font could be found for family \"%s\""), fontFamily);
            else
                RQuartz_CacheAddFont(fontFamily, fontFace, atsFont);
        }
    }

    return CGFontCreateWithPlatformFont(&atsFont);
}

#define RQUARTZ_FILL   (1)
#define RQUARTZ_STROKE (1<<1)
#define RQUARTZ_LINE   (1<<2)

static void RQuartz_SetFont(CGContextRef ctx, const pGEcontext gc, QuartzDesc *xd) {
    CGFontRef font = RQuartz_Font(gc, NULL);
    if (font) {
        CGContextSetFont(ctx, font);
        if (font != xd->font) {
            if (xd->font) CGFontRelease(xd->font);
            xd->font = font;
        }
    }
    CGContextSetFontSize(ctx, gc->cex * gc->ps);
}

/* pre-10.5 doesn't have kCGColorSpaceGenericRGB so fall back to kCGColorSpaceGenericRGB */
#if MAC_OS_X_VERSION_10_4 >= MAC_OS_X_VERSION_MAX_ALLOWED
#define kCGColorSpaceSRGB kCGColorSpaceGenericRGB
#endif

void RQuartz_Set(CGContextRef ctx,const pGEcontext gc,int flags) {
    CGColorSpaceRef cs = CGColorSpaceCreateWithName(kCGColorSpaceSRGB);
    if(flags & RQUARTZ_FILL) {
        int fill = gc->fill;
        CGFloat fillColor[] = { R_RED(fill)/255.0, 
                                R_GREEN(fill)/255.0, 
                                R_BLUE(fill)/255.0, 
                                R_ALPHA(fill)/255.0 };
        CGColorRef fillColorRef = CGColorCreate(cs, fillColor);
        CGContextSetFillColorWithColor(ctx, fillColorRef);
        CGColorRelease(fillColorRef);
    }
    if(flags & RQUARTZ_STROKE) {
        int stroke = gc->col;
        CGFloat strokeColor[] = { R_RED(stroke)/255.0, 
                                  R_GREEN(stroke)/255.0, 
                                  R_BLUE(stroke)/255.0, 
                                  R_ALPHA(stroke)/255.0 };
        CGColorRef strokeColorRef = CGColorCreate(cs, strokeColor);
        CGContextSetStrokeColorWithColor(ctx, strokeColorRef);
        CGColorRelease(strokeColorRef);
    }
    if(flags & RQUARTZ_LINE) {
        CGFloat dashlist[8];
        int   i, ndash = 0;
        int   lty = gc->lty;
	float lwd = (float)(gc->lwd * 0.75);
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
    CGColorSpaceRelease(cs);
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
	    /* reset the clipping region by restoring the base GC.
	       If there is no GC on the stack then the clipping region was never set. */
	    if (xd->gstate > 0) {
		CGContextRestoreGState(ctx);
		CGContextSaveGState(ctx);
		/* no need to modify gstate since we don't modify the stack */
	    }
	    /* The logic is to paint the canvas then gc->fill.
	       (The canvas colour is set to 0 on non-screen devices.)
	     */
	    if (R_ALPHA(xd->canvas) > 0 && !R_OPAQUE(gc->fill)) {
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

static int RQuartz_HoldFlush(DEVDESC, int level)
{
    int ol;
    XD;
    /* FIXME: should we check for interactive? */
    ol = xd->holdlevel;
    xd->holdlevel += level;
    if (xd->holdlevel < 0) xd->holdlevel = 0;
    if (xd->holdlevel == 0) { /* flush */
	/* trigger flush */
        if (xd->sync)
            xd->sync(xd, xd->userInfo);
        else {
	    CGContextRef ctx = xd->getCGContext(xd, xd->userInfo);
	    if (ctx)
		CGContextSynchronize(ctx);
	}
    } else if (ol == 0) { /* first hold */
	/* could display a wait cursor or something ... */
    }
    return xd->holdlevel;
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
    if(gc->fontface == 5)
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
	/* FIXME: check allocation */
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
    RQuartz_SetFont(ctx, gc, xd);

    CGFontRef font = CGContextGetFont(ctx);
    float aScale   = (float)((gc->cex * gc->ps * xd->tscale) /
			     CGFontGetUnitsPerEm(font));
    UniChar *buffer;
    CGGlyph *glyphs;
    int     *advances;
    int Free = 0, len;
    CFStringRef str = text2unichar(gc, dd, text, &buffer, &Free);
    if (!str) return 0.0; /* invalid text contents */
    len = (int) CFStringGetLength(str);
    /* FIXME: check allocations */
    glyphs = malloc(sizeof(CGGlyph) * len);
    advances = malloc(sizeof(int) * len);
    CGFontGetGlyphsForUnichars(font, buffer, glyphs, len);
    CGFontGetGlyphAdvances(font, glyphs, len, advances);
    float width = 0.0; /* aScale*CGFontGetLeading(CGContextGetFont(ctx)); */
    for(int i = 0; i < len; i++) width += aScale * advances[i];
    free(advances);
    free(glyphs);
    if(Free) free(buffer);
    CFRelease(str);
    return width;
}

static void RQuartz_Text(double x, double y, const char *text, double rot, double hadj, CTXDESC)
{
    DRAWSPEC;
    if (!ctx) NOCTX;
    /* A stupid hack because R isn't consistent. */
    int fill = gc->fill;
    gc->fill = gc->col;
    SET(RQUARTZ_FILL | RQUARTZ_STROKE);
    RQuartz_SetFont(ctx, gc, xd);
    gc->fill = fill;
    CGFontRef font = CGContextGetFont(ctx);
    float aScale   = (float) ((gc->cex * gc->ps * xd->tscale) /
			      CGFontGetUnitsPerEm(font));
    UniChar *buffer;
    CGGlyph   *glyphs;

    int Free = 0, len, i;
    float width = 0.0;
    CFStringRef str = text2unichar(gc, dd, text, &buffer, &Free);
    if (!str) return; /* invalid text contents */
    len = (int) CFStringGetLength(str);
    /* FIXME: check allocations */
    glyphs = malloc(sizeof(CGGlyph) * len);
    CGFontGetGlyphsForUnichars(font, buffer, glyphs, len);
    int      *advances = malloc(sizeof(int) * len);
    CGSize   *g_adv    = malloc(sizeof(CGSize) * len);

    CGFontGetGlyphAdvances(font, glyphs, len, advances);
    for(i =0 ; i < len; i++) {
	width += advances[i] * aScale;
	g_adv[i] = CGSizeMake(aScale * advances[i] * cos(-DEG2RAD*rot), aScale*advances[i]*sin(-DEG2RAD * rot));
    }
    free(advances);
    CGContextSetTextMatrix(ctx,
			   CGAffineTransformConcat(CGAffineTransformMakeScale(1.0, -1.0),
						   CGAffineTransformMakeRotation(-DEG2RAD * rot)));
    double ax = (width * hadj) * cos(-DEG2RAD * rot);
    double ay = (width * hadj) * sin(-DEG2RAD * rot);
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
    if (xd->flags & QDFLAG_RASTERIZED) {
        /* in the case of borderless rectangles snap them to pixels.
           this solves issues with image() without introducing other artifacts.
           other approaches (disabling anti-aliasing, drawing background first,
           snapping rect with borders) don't work as well, because they have
           unwanted visual side-effects. */
        if (R_ALPHA(gc->fill) > 0 && R_ALPHA(gc->col) == 0) {
	    /* store original values in case we need to go back */
	    double ox0 = x0, ox1 = x1, oy0 = y0, oy1 = y1;
            x0 = (round(x0 * xd->scalex)) / xd->scalex;
            x1 = (round(x1 * xd->scalex)) / xd->scalex;
            y0 = (round(y0 * xd->scaley)) / xd->scaley;
            y1 = (round(y1 * xd->scaley)) / xd->scaley;
	    /* work-around for PR#13744 - make sure the width or height
	       does not drop to 0 because of aligning. */
	    if (x0 == x1 && (ox0 != ox1)) x1 += ox1 - ox0;
	    if (y0 == y1 && (oy0 != oy1)) y1 += oy1 - oy0;
        }
    }
    CGContextBeginPath(ctx);
    CGContextAddRect(ctx, CGRectMake(x0, y0, x1 - x0, y1 - y0));
    CGContextDrawPath(ctx, kCGPathFillStroke);
}

static void RQuartz_Raster(unsigned int *raster, int w, int h,
                           double x, double y, 
                           double width, double height,
                           double rot, 
                           Rboolean interpolate,
                           const pGEcontext gc, pDevDesc dd)
{
    DRAWSPEC;
    if (!ctx) NOCTX;
    CGDataProviderRef dp;
    CGColorSpaceRef cs;
    CGImageRef img;
    
    /* Create a "data provider" containing the raster data */
    dp = CGDataProviderCreateWithData(NULL, (void *) raster, 4*w*h, NULL);

    cs = CGColorSpaceCreateWithName(kCGColorSpaceSRGB);

    /* Create a quartz image from the data provider */
    img = CGImageCreate(w, h, 
                        8,   /* bits per channel */
                        32,  /* bits per pixel */
                        4*w, /* bytes per row */
                        cs,  /* color space */
			/* R uses AGBR which is so unusual (inverted RGBA) that it corresponds to endinness inverse(!) to the host with alpha last (=RGBA).  */
#ifdef __BIG_ENDIAN__
                        kCGImageAlphaLast | kCGBitmapByteOrder32Little,
#else
                        kCGImageAlphaLast | kCGBitmapByteOrder32Big,
#endif
                        dp,  /* data provider */
                        NULL,/* decode array */
                        1,   /* interpolate (interpolation type below) */
                        kCGRenderingIntentDefault);

    if (height < 0) {
        y = y + height;
        height = -height;
    }

    CGContextSaveGState(ctx);
    /* Translate by height of image */
    CGContextTranslateCTM(ctx, 0.0, height);
    /* Flip vertical */
    CGContextScaleCTM(ctx, 1.0, -1.0);
    /* Translate to position */
    CGContextTranslateCTM(ctx, x, -y);
    /* Rotate */
    CGContextRotateCTM(ctx, rot*M_PI/180.0);
    /* Determine interpolation method */
    if (interpolate)
        CGContextSetInterpolationQuality(ctx, kCGInterpolationDefault);
    else
        CGContextSetInterpolationQuality(ctx, kCGInterpolationNone);
    /* Draw the quartz image */
    CGContextDrawImage(ctx, CGRectMake(0, 0, width, height), img);
    CGContextRestoreGState(ctx);

    /* Tidy up */
    CGColorSpaceRelease(cs);
    CGDataProviderRelease(dp);
    CGImageRelease(img);
}

static SEXP RQuartz_Cap(pDevDesc dd)
{
    SEXP raster = R_NilValue;
    DRAWSPEC;
    if (!ctx) NOCTXR(raster);

    if (xd->cap) 
        raster = (SEXP) xd->cap(xd, xd->userInfo);

    return raster;
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

#define max_segments 100

static void RQuartz_Polyline(int n, double *x, double *y, CTXDESC)
{
    if (n < 2) return;
    int i = 0;
    DRAWSPEC;
    if (!ctx) NOCTX;
    SET(RQUARTZ_STROKE | RQUARTZ_LINE);

    /* CGContextStrokeLineSegments turned out to be a bad idea due to
       Leopard restarting dashes for each segment.
       CGContextAddLineToPoint is fast enough. 
       The only remaining problem is that Quartz seems to restart
       dashes at segment breakup points. We should make the segments
       break-up an optional feature and possibly fix the underlying
       problem (software rendering).  
    */

    while (i < n) {
        int j = i + max_segments;
        if (j > n) j = n;
        CGContextBeginPath(ctx);
	if (i) i--; /* start at the last point of the preceding chunk */
        CGContextMoveToPoint(ctx, x[i], y[i]);
        while(++i < j)
            CGContextAddLineToPoint(ctx, x[i], y[i]);
        CGContextStrokePath(ctx);
    }
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

static void RQuartz_Path(double *x, double *y, 
                         int npoly, int* nper,
                         Rboolean winding,
                         CTXDESC)
{
    int i, j, index;
    DRAWSPEC;
    if (!ctx) NOCTX;
    SET(RQUARTZ_FILL | RQUARTZ_STROKE | RQUARTZ_LINE);
    index = 0;
    CGContextBeginPath(ctx);
    for (i=0; i < npoly; i++) {
        CGContextMoveToPoint(ctx, x[index], y[index]);
        index++;
        for(j=1; j < nper[i]; j++) {
            CGContextAddLineToPoint(ctx, x[index], y[index]);
            index++;
        }
        CGContextClosePath(ctx);
    }
    if (winding) {
        CGContextDrawPath(ctx, kCGPathFillStroke);
    } else {
        CGContextDrawPath(ctx, kCGPathEOFillStroke);
    }
}

static void RQuartz_Mode(int mode, DEVDESC)
{
    DEVSPEC;
    if (!ctx) NOCTX;
    /* don't do anything in redraw as we can signal the end */
    if (xd->redraw) return;
    /* mode=0 -> drawing complete, signal sync */
    if (mode == 0 && xd->holdlevel == 0) {
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
    RQuartz_SetFont(ctx, gc, xd);
    {
	CGFontRef font = CGContextGetFont(ctx);
        float aScale   = (float)((gc->cex * gc->ps * xd->tscale) /
				 CGFontGetUnitsPerEm(font));
	UniChar  *buffer, single;
        CGGlyph  glyphs[8];
	CFStringRef str = NULL;
        int free_buffer = 0, len;
	*width = *ascent = *descent = 0.0; /* data for bail-out cases */
	if (c >= 0 && c <= ((mbcslocale && gc->fontface != 5) ? 127 : 255)) {
	    char    text[2] = { (char)c, 0 };
	    str = text2unichar(gc, dd, text, &buffer, &free_buffer);
	    if(!str) return;
	    len = (int) CFStringGetLength(str);
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
QuartzDesc_t 
QuartzCarbon_DeviceCreate(pDevDesc dd, QuartzFunctions_t *fn, QuartzParameters_t *par)
{
    return NULL;
}

#define ARG(HOW,WHAT) HOW(CAR(WHAT));WHAT = CDR(WHAT)

/* C version of the Quartz call (experimental)
   Quartz descriptor on success, NULL on failure. 
   If errorCode is not NULL, it will contain the error code on exit */
QuartzDesc_t 
Quartz_C(QuartzParameters_t *par, quartz_create_fn_t q_create, int *errorCode)
{
    if (!q_create || !par) {
	if (errorCode) errorCode[0] = -4;
	return NULL;
    }
    {
        const void *vmax = vmaxget();
	QuartzDesc_t qd = NULL;
	R_GE_checkVersionOrDie(R_GE_version);
        R_CheckDeviceAvailable();
        {
	    const char *devname = "quartz_off_screen";
	    /* FIXME: check this allocation */
            pDevDesc dev    = calloc(1, sizeof(DevDesc));

            if (!dev) {
		if (errorCode) errorCode[0] = -2;
		return NULL;
	    }
            if (!(qd = q_create(dev, &qfn, par))) {
                vmaxset(vmax);
                free(dev);
		if (errorCode) errorCode[0] = -3;
		return NULL;
            }
	    if(streql(par->type, "") || streql(par->type, "native")
	       || streql(par->type, "cocoa") || streql(par->type, "carbon"))
		devname = "quartz";
            gsetVar(R_DeviceSymbol, mkString(devname), R_BaseEnv);
            pGEDevDesc dd = GEcreateDevDesc(dev);
            GEaddDevice(dd);
            GEinitDisplayList(dd);
            vmaxset(vmax);
        }
	return qd;
    }
}

/* ARGS: type, file, width, height, ps, family, antialias,
   title, bg, canvas, dpi */
SEXP Quartz(SEXP args)
{
    SEXP tmps, bgs, canvass;
    double   width, height, ps;
    Rboolean antialias;
    int      quartzpos, bg, canvas, module = 0;
    double   mydpi[2], *dpi = 0;
    const char *type, *mtype = 0, *family, *title;
    char *file = NULL;
    QuartzDesc_t qd = NULL;

    const void *vmax = vmaxget();
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
    else if (isString(tmps) && LENGTH(tmps) >= 1) {
        const char *tmp = R_ExpandFileName(CHAR(STRING_ELT(tmps, 0)));
	file = R_alloc(strlen(tmp) + 1, sizeof(char));
	strcpy(file, tmp);
    } else
        error(_("invalid 'file' argument"));
    width     = ARG(asReal,args);
    height    = ARG(asReal,args);
    ps        = ARG(asReal,args);
    family    = CHAR(STRING_ELT(CAR(args), 0)); args = CDR(args);
    antialias = ARG(asLogical,args);
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
        error(_("invalid quartz() device size"));

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

	if (!strncasecmp(type, "bitmap:", 7)) {
	    module = QBE_BITMAP;
	    mtype = mtype + 7;
	}
    }

    quartzpos = 1;

    R_GE_checkVersionOrDie(R_GE_version);
    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	pDevDesc dev = calloc(1, sizeof(DevDesc));

	if (!dev)
	    error(_("unable to create device description"));

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
	    qd = ptr_QuartzBackend(dev, &qfn, &qpar);

	if (qd == NULL) { /* try internal modules next */
	    switch (module) {
            case QBE_COCOA:
                qd = QuartzCocoa_DeviceCreate(dev, &qfn, &qpar);
                break;
            case QBE_NATIVE:
                /* native is essentially cocoa with carbon fall-back */
                qd = QuartzCocoa_DeviceCreate(dev, &qfn, &qpar);
                if (qd) break;
            case QBE_CARBON:
                qd = QuartzCarbon_DeviceCreate(dev, &qfn, &qpar);
                break;
            case QBE_PDF:
		qpar.canvas = 0; /* so not used */
                qd = QuartzPDF_DeviceCreate(dev, &qfn, &qpar);
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
		qd = QuartzBitmap_DeviceCreate(dev, &qfn, &qpar);
		break;
	    }
	}

	if (qd == NULL) {
	    vmaxset(vmax);
	    free(dev);
	    error(_("unable to create quartz() device target, given type may not be supported"));
	}
	const char *devname = "quartz_off_screen";
	if(streql(type, "") || streql(type, "native") || streql(type, "cocoa") 
	   || streql(type, "carbon")) devname = "quartz";
 	gsetVar(R_DeviceSymbol, mkString(devname), R_BaseEnv);
	pGEDevDesc dd = GEcreateDevDesc(dev);
	GEaddDevice(dd);
	GEinitDisplayList(dd);
    } END_SUSPEND_INTERRUPTS;
    vmaxset(vmax);
    return R_NilValue;
}

#include <sys/sysctl.h>

static double cached_darwin_version = 0.0;

/* Darwin version X.Y maps to OS X version 10.(X - 4).Y */
static double darwin_version() {
    char ver[32];
    size_t len = sizeof(ver) - 1;
    int mib[2] = { CTL_KERN, KERN_OSRELEASE };
    if (cached_darwin_version > 0.0)
	return cached_darwin_version;
    sysctl(mib, 2, &ver, &len, 0, 0);
    return (cached_darwin_version = atof(ver));
}

#include <mach/mach.h>
#include <servers/bootstrap.h>

/* even as of Darwin 9 there is no entry for bootstrap_info in bootrap headers */
extern kern_return_t bootstrap_info(mach_port_t , /* bootstrap port */
                                    name_array_t*, mach_msg_type_number_t*,  /* service */
                                    name_array_t*, mach_msg_type_number_t*,  /* server */
                                    bool_array_t*, mach_msg_type_number_t*); /* active */

/* returns 1 if window server session service
   (com.apple.windowserver.session) is present in the boostrap
   namespace (pre-Lion) or when a current session is present, active
   and there is no SSH_CONNECTION (Lion and later).
   returns 0 if an error occurred or the service is not
   present. For all practical purposes this returns 1 only if run
   interactively via LS. Although ssh to a machine that has a running
   session for the same user will allow a WS connection, this function
   will still return 0 in that case.
   NOTE: on Mac OS X 10.5 we are currently NOT searching the parent
   namespaces. This is currently OK, because the session service will
   be registered in the session namespace which is the last in the
   chain. However, this could change in the future.
 */
static int has_wss() {
    int res = 0;

    if (darwin_version() < 11.0) { /* before Lion we get reliable information from the bootstrap info */
	kern_return_t kr;
	mach_port_t self = mach_task_self();
	mach_port_t bport = MACH_PORT_NULL;
	kr = task_get_bootstrap_port(self, &bport);
	if (kr == KERN_SUCCESS) {
	    kern_return_t           kr;
	    name_array_t            serviceNames;
	    mach_msg_type_number_t  serviceNameCount;
	    name_array_t            serverNames;
	    mach_msg_type_number_t  serverNameCount;
	    bool_array_t            active;
	    mach_msg_type_number_t  activeCount;
	    
	    serviceNames  = NULL;
	    serverNames   = NULL;
	    active        = NULL;
	    
	    kr = bootstrap_info(bport, 
				&serviceNames, &serviceNameCount, 
				&serverNames, &serverNameCount, 
				&active, &activeCount);
	    if (kr == KERN_SUCCESS) {
		unsigned int i = 0;
		while (i < serviceNameCount) {
		    if (!strcmp(serviceNames[i], "com.apple.windowserver.session")) {
			res = 1;
			break;
		    }
		    i++;
		}
	    }
	}
	if (bport != MACH_PORT_NULL)
	    mach_port_deallocate(mach_task_self(), bport);
    } else {
	/* On Mac OS X 10.7 (Lion) and higher two things changed:
	   a) there is no com.apple.windowserver.session anymore 
	   so the above will fail
	   b) every process has now the full bootstrap info, 
	   so in fact even remote connections will be able to 
	   run on-screen tasks if the user is logged in
	   So we need to add some heuristics to decide when the user 
	   actually wants Quartz ... */   
	/* check user's session */
	CFDictionaryRef dict = CGSessionCopyCurrentDictionary();
	if (dict) { /* allright, let's see if the session is current */
	    CFTypeRef obj = CFDictionaryGetValue(dict, CFSTR("kCGSSessionOnConsoleKey"));
	    if (obj && CFGetTypeID(obj) == CFBooleanGetTypeID()) {
		/* even if this session is active, we don't use Quartz for SSH connections */
		if (CFBooleanGetValue(obj) && (!getenv("SSH_CONNECTION") || getenv("SSH_CONNECTION")[0] == 0))
		    res = 1;
	    }
	    CFRelease(dict);
	}
    }

    return res;
}

SEXP makeQuartzDefault() {
    return ScalarLogical(has_wss());
}

#else
/* --- no AQUA support = no Quartz --- */

#include "grDevices.h"
#include <R_ext/QuartzDevice.h>

SEXP Quartz(SEXP args)
{
    warning(_("Quartz device is not available on this platform"));
    return R_NilValue;
}

SEXP makeQuartzDefault() {
    return ScalarLogical(FALSE);
}

QuartzDesc_t 
Quartz_C(QuartzParameters_t *par, quartz_create_fn_t q_create, int *errorCode)
{
    if (errorCode) errorCode[0] = -1;
    return NULL;
}

void *getQuartzAPI()
{
    return NULL;
}

#endif
