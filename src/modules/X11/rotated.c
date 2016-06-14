/* ********************************************************************** */

/* xvertext 5.0, Copyright (c) 1993 Alan Richardson (mppa3@uk.ac.sussex.syma)
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both the
 * copyright notice and this permission notice appear in supporting
 * documentation.  All work developed as a consequence of the use of
 * this program should duly acknowledge such use. No representations are
 * made about the suitability of this software for any purpose.	 It is
 * provided "as is" without express or implied warranty.
 */

/* ********************************************************************** */


/* BETTER: xvertext now does rotation at any angle!!
 *
 * BEWARE: function arguments have CHANGED since version 2.0!!
 *
 * Protoized (ANSI C, no longer old K&R C):  Martin Maechler, R core team.
 * float -> double
 */

/* The version for R 2.1.0 is based on patches by
   Ei-ji Nakama <nakama@ki.rim.or.jp> for use in Japanese. */

/* ********************************************************************** */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#if !defined(strdup) && defined(HAVE_DECL_STRDUP) && !HAVE_DECL_STRDUP
extern char *strdup(const char *s1);
#endif

extern int utf8locale;
/* In theory we should do this, but it works less well
# ifdef X_HAVE_UTF8_STRING
#  define HAVE_XUTF8TEXTESCAPEMENT 1
#  define HAVE_XUTF8TEXTEXTENTS 1
#  define HAVE_XUTF8DRAWSTRING 1
# endif */

#include "rotated.h"

/* ---------------------------------------------------------------------- */

/* text alignment -- only NONE is used in R */

#define NONE		 0
#define TLEFT		 1
#define TCENTRE		 2
#define TRIGHT		 3
#define MLEFT		 4
#define MCENTRE		 5
#define MRIGHT		 6
#define BLEFT		 7
#define BCENTRE		 8
#define BRIGHT		 9

/* Make sure cache size is set */

#ifndef CACHE_SIZE_LIMIT
#define CACHE_SIZE_LIMIT 0
#endif /*CACHE_SIZE_LIMIT */

/* Make sure a cache method is specified */

#ifndef CACHE_XIMAGES
#ifndef CACHE_BITMAPS
#define CACHE_BITMAPS
#endif /*CACHE_BITMAPS*/
#endif /*CACHE_XIMAGES*/

#ifndef DEG2RAD
#define DEG2RAD 0.01745329251994329576
#endif

/* ---------------------------------------------------------------------- */


/* Debugging macros */

#ifdef DEBUG
static int debug=1;
#else
static int debug=0;
#endif /*DEBUG*/

#define DEBUG_PRINT1(a) if (debug) printf (a)
#define DEBUG_PRINT2(a, b) if (debug) printf (a, b)
#define DEBUG_PRINT3(a, b, c) if (debug) printf (a, b, c)
#define DEBUG_PRINT4(a, b, c, d) if (debug) printf (a, b, c, d)
#define DEBUG_PRINT5(a, b, c, d, e) if (debug) printf (a, b, c, d, e)


/* ---------------------------------------------------------------------- */

static double myround(double x)
{
    return floor(x+0.5);
}


/* A structure holding everything needed for a rotated string */

typedef struct rotated_text_item_template {
    Pixmap bitmap;
    XImage *ximage;

    char *text;
    char *font_name;
    Font fid;
    double angle;
    int align;
    double magnify;

    int cols_in;
    int rows_in;
    int cols_out;
    int rows_out;

    int nl;
    int max_width;
    double *corners_x;
    double *corners_y;

    long int size;
    int cached;

    struct rotated_text_item_template *next;
} RotatedTextItem;

static RotatedTextItem *first_text_item=NULL;


/* ---------------------------------------------------------------------- */


/* A structure holding current magnification and bounding box padding */

static struct style_template {
    double magnify;
    int bbx_pad;
} style={
    1.,
    0
    };


/* ---------------------------------------------------------------------- */


double			 XRotVersion(char *str, int n);
void			XRotSetMagnification(double m);
void			XRotSetBoundingBoxPad(int p);
int			XRotDrawString(Display *dpy, XFontStruct *font, double angle, Drawable drawable, GC gc, int x, int y, const char *str);
int			XRotDrawImageString(Display *dpy, XFontStruct *font, double angle, Drawable drawable, GC gc, int x, int y, const char *str);
int			XRotDrawAlignedString(Display *dpy, XFontStruct *font, double angle, Drawable drawable, GC gc, int x, int y, const char *text, int align);
int			XRotDrawAlignedImageString(Display *dpy, XFontStruct *font, double angle, Drawable drawable, GC gc, int x, int y, const char *text, int align);
XPoint		       *XRotTextExtents(Display *dpy, XFontStruct *font, double angle, int x, int y, const char *text, int align);

static XImage	       *MakeXImage(Display *dpy, int w, int h);
static int		XRotPaintAlignedString(Display *dpy, XFontStruct *font, double angle, Drawable drawable, GC gc, int x, int y, const char *text, int align, int bg);
static int		XRotDrawHorizontalString(Display *dpy, XFontStruct *font, Drawable drawable, GC gc, int x, int y, const char *text, int align, int bg);
static RotatedTextItem *XRotRetrieveFromCache(Display *dpy, XFontStruct *font, double angle, const char *text, int align);
static RotatedTextItem *XRotCreateTextItem(Display *dpy, XFontStruct *font, double angle, const char *text, int align);
static void		XRotAddToLinkedList(Display *dpy, RotatedTextItem *item);
static void		XRotFreeTextItem(Display *dpy, RotatedTextItem *item);
static XImage	       *XRotMagnifyImage(Display *dpy, XImage *ximage);

static int XmbRotDrawString(Display *dpy, XFontSet fontset, double angle,
			    Drawable drawable, GC gc, int x, int y, const char *str);

/* ---------------------------------------------------------------------- */

int XRfRotDrawString(Display *dpy, R_XFont *rfont, double angle,
		     Drawable drawable, GC gc, int x, int y, const char *str)
{
    if(rfont->type == Font_Set)
	return XmbRotDrawString(dpy, rfont->fontset, angle, drawable, gc, x, y, str);
    else
	return XRotDrawString(dpy, rfont->font, angle, drawable, gc, x, y, str);
}




/**************************************************************************/
/* Return version/copyright information					  */
/**************************************************************************/

double XRotVersion(char *str, int n)
{
    if(str!=NULL)
	strncpy(str, XV_COPYRIGHT, n);
    return XV_VERSION;
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/* Set the font magnification factor for all subsequent operations	  */
/**************************************************************************/

void XRotSetMagnification(double m)
{
    if(m>0.)
	style.magnify=m;
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/* Set the padding used when calculating bounding boxes			  */
/**************************************************************************/

void XRotSetBoundingBoxPad(int p)
{
    if(p>=0)
	style.bbx_pad=p;
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/*  Create an XImage structure and allocate memory for it		  */
/**************************************************************************/

static XImage *MakeXImage(Display *dpy, int w, int h)
{
    XImage *I;
    char *data;

    /* reserve memory for image */
    data=(char *)calloc((unsigned)(((w-1)/8+1)*h), 1);
    if(data==NULL)
	return NULL;

    /* create the XImage */
    I=XCreateImage(dpy, DefaultVisual(dpy, DefaultScreen(dpy)), 1, XYBitmap,
		   0, data, w, h, 8, 0);
    if(I==NULL)
	return NULL;

    I->byte_order=I->bitmap_bit_order=MSBFirst;
    return I;
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/*  A front end to XRotPaintAlignedString:				  */
/*	-no alignment, no background					  */
/**************************************************************************/

int XRotDrawString(Display *dpy, XFontStruct *font, double angle,
		   Drawable drawable, GC gc, int x, int y, const char *str)
{
    return (XRotPaintAlignedString(dpy, font, angle, drawable, gc,
				   x, y, str, NONE, 0));
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/*  A front end to XRotPaintAlignedString:				  */
/*	-no alignment, paints background				  */
/**************************************************************************/

int XRotDrawImageString(Display *dpy, XFontStruct *font, double angle,
			Drawable drawable, GC gc, int x, int y, const char *str)
{
    return(XRotPaintAlignedString(dpy, font, angle, drawable, gc,
				  x, y, str, NONE, 1));
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/*  A front end to XRotPaintAlignedString:				  */
/*	-does alignment, no background					  */
/**************************************************************************/

int XRotDrawAlignedString(Display *dpy, XFontStruct *font, double angle,
			  Drawable drawable, GC gc, int x, int y,
			  const char *text, int align)
{
    return(XRotPaintAlignedString(dpy, font, angle, drawable, gc,
				  x, y, text, align, 0));
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/*  A front end to XRotPaintAlignedString:				  */
/*	-does alignment, paints background				  */
/**************************************************************************/

int XRotDrawAlignedImageString(Display *dpy, XFontStruct *font, double angle,
			       Drawable drawable, GC gc, int x, int y,
			       const char *text, int align)
{
    return(XRotPaintAlignedString(dpy, font, angle, drawable, gc,
				  x, y, text, align, 1));
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/*  Aligns and paints a rotated string					  */
/**************************************************************************/

static int XRotPaintAlignedString(Display *dpy, XFontStruct *font, double angle,
				  Drawable drawable, GC gc, int x, int y,
				  const char *text, int align, int bg)
{
    int i;
    GC my_gc;
    int xp, yp;
    double hot_x, hot_y;
    double hot_xp, hot_yp;
    double sin_angle, cos_angle;
    RotatedTextItem *item;
    Pixmap bitmap_to_paint;

    /* return early for NULL/empty strings */
    if(text==NULL || *text=='\0') // R change in original R version
	return 0;

    if(strlen(text)==0)
	return 0;

    /* manipulate angle to 0<=angle<360 degrees */
    while(angle<0)
	angle+=360;

    while(angle>=360)
	angle-=360;

    angle *= DEG2RAD;

    /* horizontal text made easy */
    if(angle==0. && style.magnify==1.)
	return(XRotDrawHorizontalString(dpy, font, drawable, gc, x, y,
					text, align, bg));

    /* get a rotated bitmap */
    item=XRotRetrieveFromCache(dpy, font, angle, text, align);
    if(item==NULL)
	return 0;

    /* this gc has similar properties to the user's gc */
    // GCClipMask added in r6259 for clipping
    my_gc=XCreateGC(dpy, drawable, (unsigned long)0, 0);
    XCopyGC(dpy, gc, GCForeground|GCBackground|GCFunction|GCPlaneMask
	    |GCClipMask,
	    my_gc);

    /* alignment : which point (hot_x, hot_y) relative to bitmap centre
       coincides with user's specified point? */

    /* y position */
    if(align==TLEFT || align==TCENTRE || align==TRIGHT)
	hot_y=item->rows_in/2.*style.magnify;
    else if(align==MLEFT || align==MCENTRE || align==MRIGHT)
	hot_y=0;
    else if(align==BLEFT || align==BCENTRE || align==BRIGHT)
	hot_y= -item->rows_in/2.*style.magnify;
    else
	hot_y= -(item->rows_in/2.-font->descent)*style.magnify;

    /* x position */
    if(align==TLEFT || align==MLEFT || align==BLEFT || align==NONE)
	hot_x= -item->max_width/2.*style.magnify;
    else if(align==TCENTRE || align==MCENTRE || align==BCENTRE)
	hot_x=0;
    else
	hot_x=item->max_width/2.*style.magnify;

    /* pre-calculate sin and cos */
    // rounding added in original R version
    sin_angle = myround(sin(angle)*1000.0) / 1000.0;
    cos_angle = myround(cos(angle)*1000.0) / 1000.0;

    /* rotate hot_x and hot_y around bitmap centre */
    hot_xp = hot_x*cos_angle - hot_y*sin_angle;
    hot_yp = hot_x*sin_angle + hot_y*cos_angle;

    /* text background will be drawn using XFillPolygon */
    if(bg) {
	GC depth_one_gc;
	XPoint *xpoints;
	Pixmap empty_stipple;

	/* reserve space for XPoints */
	xpoints=(XPoint *)malloc((unsigned)(4*item->nl*sizeof(XPoint)));
	if(!xpoints)
	    return 1;

	/* rotate corner positions */
	for(i=0; i<4*item->nl; i++) {
	    xpoints[i].x=(short)(x + ( (item->corners_x[i]-hot_x)*cos_angle +
					(item->corners_y[i]+hot_y)*sin_angle));
	    xpoints[i].y=(short)(y + (-(item->corners_x[i]-hot_x)*sin_angle +
				      (item->corners_y[i]+hot_y)*cos_angle));
	}

	/* we want to swap foreground and background colors here;
	   XGetGCValues() is only available in R4+ */

	empty_stipple=XCreatePixmap(dpy, drawable, 1, 1, 1);

	depth_one_gc=XCreateGC(dpy, empty_stipple, (unsigned long)0, 0);
	XSetForeground(dpy, depth_one_gc, 0);
	XFillRectangle(dpy, empty_stipple, depth_one_gc, 0, 0, 2, 2);

	XSetStipple(dpy, my_gc, empty_stipple);
	XSetFillStyle(dpy, my_gc, FillOpaqueStippled);

	XFillPolygon(dpy, drawable, my_gc, xpoints, 4*item->nl, Nonconvex,
		     CoordModeOrigin);

	/* free our resources */
	free((char *)xpoints);
	XFreeGC(dpy, depth_one_gc);
	XFreePixmap(dpy, empty_stipple);
    }

    /* where should top left corner of bitmap go ? */
    xp=(short)(x - (item->cols_out/2. + hot_xp));
    yp=(short)(y - (item->rows_out/2. - hot_yp));

    /* by default we draw the rotated bitmap, solid */
    bitmap_to_paint=item->bitmap;

    /* handle user stippling */
#ifndef X11R3
    {
	GC depth_one_gc;
	XGCValues values;
	Pixmap new_bitmap, inverse;

	/* try and get some GC properties */
	if(XGetGCValues(dpy, gc,
			GCStipple|GCFillStyle|GCForeground|GCBackground|
			GCTileStipXOrigin|GCTileStipYOrigin,
			&values)) {

	    /* only do this if stippling requested */
	    if((values.fill_style==FillStippled ||
		values.fill_style==FillOpaqueStippled) && !bg) {

		/* opaque stipple: draw rotated text in background colour */
		if(values.fill_style==FillOpaqueStippled) {
		    XSetForeground(dpy, my_gc, values.background);
		    XSetFillStyle(dpy, my_gc, FillStippled);
		    XSetStipple(dpy, my_gc, item->bitmap);
		    XSetTSOrigin(dpy, my_gc, xp, yp);
		    XFillRectangle(dpy, drawable, my_gc, xp, yp,
				   item->cols_out, item->rows_out);
		    XSetForeground(dpy, my_gc, values.foreground);
		}

		/* this will merge the rotated text and the user's stipple */
		new_bitmap=XCreatePixmap(dpy, drawable,
					 item->cols_out, item->rows_out, 1);

		/* create a GC */
		depth_one_gc=XCreateGC(dpy, new_bitmap, (unsigned long)0, 0);
		XSetForeground(dpy, depth_one_gc, 1);
		XSetBackground(dpy, depth_one_gc, 0);

		/* set the relative stipple origin */
		XSetTSOrigin(dpy, depth_one_gc,
			     values.ts_x_origin-xp, values.ts_y_origin-yp);

		/* fill the whole bitmap with the user's stipple */
		XSetStipple(dpy, depth_one_gc, values.stipple);
		XSetFillStyle(dpy, depth_one_gc, FillOpaqueStippled);
		XFillRectangle(dpy, new_bitmap, depth_one_gc,
			       0, 0, item->cols_out, item->rows_out);

		/* set stipple origin back to normal */
		XSetTSOrigin(dpy, depth_one_gc, 0, 0);

		/* this will contain an inverse copy of the rotated text */
		inverse=XCreatePixmap(dpy, drawable,
				      item->cols_out, item->rows_out, 1);

		/* invert text */
		XSetFillStyle(dpy, depth_one_gc, FillSolid);
		XSetFunction(dpy, depth_one_gc, GXcopyInverted);
		XCopyArea(dpy, item->bitmap, inverse, depth_one_gc,
			  0, 0, item->cols_out, item->rows_out, 0, 0);

		/* now delete user's stipple everywhere EXCEPT on text */
		XSetForeground(dpy, depth_one_gc, 0);
		XSetBackground(dpy, depth_one_gc, 1);
		XSetStipple(dpy, depth_one_gc, inverse);
		XSetFillStyle(dpy, depth_one_gc, FillStippled);
		XSetFunction(dpy, depth_one_gc, GXcopy);
		XFillRectangle(dpy, new_bitmap, depth_one_gc,
			       0, 0, item->cols_out, item->rows_out);

		/* free resources */
		XFreePixmap(dpy, inverse);
		XFreeGC(dpy, depth_one_gc);

		/* this is the new bitmap */
		bitmap_to_paint=new_bitmap;
	    }
	}
    }
#endif /*X11R3*/

    /* paint text using stipple technique */
    XSetFillStyle(dpy, my_gc, FillStippled);
    XSetStipple(dpy, my_gc, bitmap_to_paint);
    XSetTSOrigin(dpy, my_gc, xp, yp);
    XFillRectangle(dpy, drawable, my_gc, xp, yp,
		   item->cols_out, item->rows_out);

    /* free our resources */
    XFreeGC(dpy, my_gc);

    /* stippled bitmap no longer needed */
    if(bitmap_to_paint!=item->bitmap)
	XFreePixmap(dpy, bitmap_to_paint);

#ifdef CACHE_XIMAGES
    XFreePixmap(dpy, item->bitmap);
#endif /*CACHE_XIMAGES*/

    /* if item isn't cached, destroy it completely */
    if(!item->cached)
	XRotFreeTextItem(dpy,item);

    /* we got to the end OK! */
    return 0;
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/*  Draw a horizontal string in a quick fashion				  */
/**************************************************************************/

static int XRotDrawHorizontalString(Display *dpy, XFontStruct *font,
				    Drawable drawable, GC gc, int x, int y,
				    const char *text, int align, int bg)
{
    GC my_gc;
    int nl=1, i;
    int height;
    int xp, yp;
    char *str1, *str2, *str3;
    char *str2_a="\0", *str2_b="\n\0";
    int dir, asc, desc;
    XCharStruct overall;

    if (text == NULL || *text=='\0') {  // addition in original R version
      DEBUG_PRINT1("Empty string, ignoring\n");
      return 0;
    }

    /* this gc has similar properties to the user's gc (including stipple) */
    my_gc=XCreateGC(dpy, drawable, (unsigned long)0, 0);
    // GCClipMask added in r6259 for clipping
    XCopyGC(dpy, gc,
	    GCForeground|GCBackground|GCFunction|GCStipple|GCFillStyle|
	    GCTileStipXOrigin|GCTileStipYOrigin|GCPlaneMask|GCClipMask, my_gc);
    XSetFont(dpy, my_gc, font->fid);

    /* count number of sections in string */
    if(align!=NONE)
	for(i=(int)strlen(text)-2; i >= 0; i--)
	    if(text[i]=='\n')
		nl++;

    /* ignore newline characters if not doing alignment */
    if(align==NONE)
	str2=str2_a;
    else
	str2=str2_b;

    /* overall font height */
    height=font->ascent+font->descent;

    /* y position */
    if(align==TLEFT || align==TCENTRE || align==TRIGHT)
	yp=y+font->ascent;
    else if(align==MLEFT || align==MCENTRE || align==MRIGHT)
	yp=y-nl*height/2+font->ascent;
    else if(align==BLEFT || align==BCENTRE || align==BRIGHT)
	yp=y-nl*height+font->ascent;
    else
	yp=y;

    str1=strdup(text);
    if(str1==NULL)
	return 1;

    str3=strtok(str1, str2);

    /* loop through each section in the string */
    do {
	XTextExtents(font, str3, (int)strlen(str3), &dir, &asc, &desc,
		     &overall);

	/* where to draw section in x ? */
	if(align==TLEFT || align==MLEFT || align==BLEFT || align==NONE)
	    xp=x;
	else if(align==TCENTRE || align==MCENTRE || align==BCENTRE)
	    xp=x-overall.rbearing/2;
	else
	    xp=x-overall.rbearing;

	/* draw string onto bitmap */
	if(!bg)
	    XDrawString(dpy, drawable, my_gc, xp, yp, str3, (int)strlen(str3));
	else
	    XDrawImageString(dpy, drawable, my_gc, xp, yp, str3, (int)strlen(str3));

	/* move to next line */
	yp+=height;

	str3=strtok((char *)NULL, str2);
    }
    while(str3!=NULL);

    free(str1);
    XFreeGC(dpy, my_gc);

    return 0;
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/*   Query cache for a match with this font/text/angle/alignment	  */
/*	 request, otherwise arrange for its creation			  */
/**************************************************************************/

static RotatedTextItem *XRotRetrieveFromCache(Display *dpy, XFontStruct *font,
					      double angle, const char *text,
					      int align)
{
    Font fid;
    char *font_name=NULL;
    unsigned long name_value;
    RotatedTextItem *item=NULL;
    RotatedTextItem *i1=first_text_item;

    /* get font name, if it exists */
    if(XGetFontProperty(font, XA_FONT, &name_value)) {
	DEBUG_PRINT1("got font name OK\n");
	font_name=XGetAtomName(dpy, name_value);
	fid=0;
    }
#ifdef CACHE_FID
    /* otherwise rely (unreliably?) on font ID */
    else {
	DEBUG_PRINT1("can't get fontname, caching FID\n");
	font_name=NULL;
	fid=font->fid;
    }
#else
    /* not allowed to cache font ID's */
    else {
	DEBUG_PRINT1("can't get fontname, can't cache\n");
	font_name=NULL;
	fid=0;
    }
#endif /*CACHE_FID*/

    /* look for a match in cache */

    /* matching formula:
       identical text;
       identical fontname (if defined, font ID's if not);
       angles close enough (<0.0001 here, could be smaller);
       HORIZONTAL alignment matches, OR it's a one line string;
       magnifications the same */

    while(i1 && !item) {
	/* match everything EXCEPT fontname/ID */
	if(strcmp(text, i1->text)==0 &&
	   fabs(angle-i1->angle)<0.0001 &&
	   style.magnify==i1->magnify &&
	   (i1->nl==1 ||
	    ((align==0)?9:(align-1))%3==
	      ((i1->align==0)?9:(i1->align-1))%3)) {

	    /* now match fontname/ID */
	    if(font_name!=NULL && i1->font_name!=NULL) {
		if(strcmp(font_name, i1->font_name)==0) {
		    item=i1;
		    DEBUG_PRINT1("Matched against font names\n");
		}
		else
		    i1=i1->next;
	    }
#ifdef CACHE_FID
	    else if(font_name==NULL && i1->font_name==NULL) {
		if(fid==i1->fid) {
		    item=i1;
		    DEBUG_PRINT1("Matched against FID's\n");
		}
		else
		    i1=i1->next;
	    }
#endif /*CACHE_FID*/
	    else
		i1=i1->next;
	}
	else
	    i1=i1->next;
    }

    if(item)
	DEBUG_PRINT1("**Found target in cache.\n");
    if(!item)
	DEBUG_PRINT1("**No match in cache.\n");

    /* no match */
    if(!item) {
	/* create new item */
	item=XRotCreateTextItem(dpy, font, angle, text, align);
	if(!item)
	    return NULL;

	/* record what it shows */
	item->text=strdup(text);

	/* fontname or ID */
	if(font_name!=NULL) {
	    item->font_name=strdup(font_name);
	    item->fid=0;
	}
	else {
	    item->font_name=NULL;
	    item->fid=fid;
	}

	item->angle=angle;
	item->align=align;
	item->magnify=style.magnify;

	/* cache it */
	XRotAddToLinkedList(dpy, item);
    }

    if(font_name)
	XFree(font_name);

    /* if XImage is cached, need to recreate the bitmap */

#ifdef CACHE_XIMAGES
    {
	GC depth_one_gc;

	/* create bitmap to hold rotated text */
	item->bitmap=XCreatePixmap(dpy, DefaultRootWindow(dpy),
				   item->cols_out, item->rows_out, 1);

	/* depth one gc */
	depth_one_gc=XCreateGC(dpy, item->bitmap, (unsigned long)0, 0);
	XSetBackground(dpy, depth_one_gc, 0);
	XSetForeground(dpy, depth_one_gc, 1);

	/* make the text bitmap from XImage */
	XPutImage(dpy, item->bitmap, depth_one_gc, item->ximage, 0, 0, 0, 0,
		  item->cols_out, item->rows_out);

	XFreeGC(dpy, depth_one_gc);
    }
#endif /*CACHE_XIMAGES*/

    return item;
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/*  Create a rotated text item						  */
/**************************************************************************/

static RotatedTextItem *XRotCreateTextItem(Display *dpy, XFontStruct *font,
					   double angle, const char *text, int align)
{
    RotatedTextItem *item=NULL;
    Pixmap canvas;
    GC font_gc;
    XImage *I_in;
    register int i, j;
    char *str1, *str2, *str3;
    char *str2_a="\0", *str2_b="\n\0";
    int height;
    int byte_w_in, byte_w_out;
    int xp, yp;
    double sin_angle, cos_angle;
    int it, jt;
    double itd, jtd;
    double di, dj;
    int ic=0;
    double xl, xr, xinc;
    int byte_out;
    int dir, asc, desc;
    XCharStruct overall;
    int old_cols_in=0, old_rows_in=0;

    /* allocate memory */
    item=(RotatedTextItem *)malloc((unsigned)sizeof(RotatedTextItem));
    if(!item)
	return NULL;

    /* count number of sections in string */
    item->nl=1;
    if(align!=NONE)
	for(i=(int)strlen(text)-2; i >= 0; i--)
	    if(text[i]=='\n')
		item->nl++;

    /* ignore newline characters if not doing alignment */
    if(align==NONE)
	str2=str2_a;
    else
	str2=str2_b;

    /* find width of longest section */
    str1=strdup(text);
    if(str1==NULL) {
	free(item);
	return NULL;
    }

    str3=strtok(str1, str2);

    XTextExtents(font, str3, (int)strlen(str3), &dir, &asc, &desc,
		 &overall);

    item->max_width=overall.rbearing;

    /* loop through each section */
    do {
	str3=strtok((char *)NULL, str2);

	if(str3!=NULL) {
	    XTextExtents(font, str3, (int)strlen(str3), &dir, &asc, &desc,
			 &overall);

	    if(overall.rbearing>item->max_width)
		item->max_width=overall.rbearing;
	}
    }
    while(str3!=NULL);

    free(str1);

    /* overall font height */
    height=font->ascent+font->descent;

    /* dimensions horizontal text will have */
    item->cols_in=item->max_width;
    item->rows_in=item->nl*height;

    /* fudge in case one of the above is zero: 
       for " ", r8300
     */
    if (!item->cols_in) item->cols_in=1;
    if (!item->rows_in) item->rows_in=1;

    /* bitmap for drawing on */
    canvas=XCreatePixmap(dpy, DefaultRootWindow(dpy),
			 item->cols_in, item->rows_in, 1);

    /* create a GC for the bitmap */
    font_gc=XCreateGC(dpy, canvas, (unsigned long)0, 0);
    XSetBackground(dpy, font_gc, 0);
    XSetFont(dpy, font_gc, font->fid);

    /* make sure the bitmap is blank */
    XSetForeground(dpy, font_gc, 0);
    XFillRectangle(dpy, canvas, font_gc, 0, 0,
		   item->cols_in+1, item->rows_in+1);
    XSetForeground(dpy, font_gc, 1);

    /* pre-calculate sin and cos */
    sin_angle = myround(sin(angle)*1000.0) / 1000.0;
    cos_angle = myround(cos(angle)*1000.0) / 1000.0;

    /* text background will be drawn using XFillPolygon */
    item->corners_x=
	(double *)malloc((unsigned)(4*item->nl*sizeof(double)));
    if(!item->corners_x) {
	free(item);
	return NULL;
    }
    item->corners_y=
	(double *)malloc((unsigned)(4*item->nl*sizeof(double)));
    if(!item->corners_y) {
	free(item->corners_x);
	free(item);
	return NULL;
    }

    /* draw text horizontally */

    /* start at top of bitmap */
    yp=font->ascent;

    str1=strdup(text);
    if(str1==NULL) {
	free(item->corners_y);
	free(item->corners_x);
	free(item);
	return NULL;
    }

    str3=strtok(str1, str2);

    /* loop through each section in the string */
    do {
	XTextExtents(font, str3, (int)strlen(str3), &dir, &asc, &desc,
		     &overall);

	/* where to draw section in x ? */
	if(align==TLEFT || align==MLEFT || align==BLEFT || align==NONE)
	    xp=0;
	else if(align==TCENTRE || align==MCENTRE || align==BCENTRE)
	    xp=(item->max_width-overall.rbearing)/2;
	else
	    xp=item->max_width-overall.rbearing;

	/* draw string onto bitmap */
	XDrawString(dpy, canvas, font_gc, xp, yp, str3, (int)strlen(str3));

	/* keep a note of corner positions of this string */
	item->corners_x[ic]=(xp-item->cols_in/2.)*style.magnify;
	item->corners_y[ic]=(yp-font->ascent-item->rows_in/2.) *style.magnify;
	item->corners_x[ic+1]=item->corners_x[ic];
	item->corners_y[ic+1]=item->corners_y[ic]+height*style.magnify;
	item->corners_x[item->nl*4-1-ic]=item->corners_x[ic]+
	    overall.rbearing*style.magnify;
	item->corners_y[item->nl*4-1-ic]=item->corners_y[ic];
	item->corners_x[item->nl*4-2-ic]=
	    item->corners_x[item->nl*4-1-ic];
	item->corners_y[item->nl*4-2-ic]=item->corners_y[ic+1];

	ic+=2;

	/* move to next line */
	yp+=height;

	str3=strtok((char *)NULL, str2);
    }
    while(str3!=NULL);

    free(str1);

    /* create image to hold horizontal text */
    I_in=MakeXImage(dpy, item->cols_in, item->rows_in);
    if(I_in==NULL) {
	free(item->corners_y);
	free(item->corners_x);
	free(item);
	return NULL;
    }

    /* extract horizontal text */
    XGetSubImage(dpy, canvas, 0, 0, item->cols_in, item->rows_in,
		 1, XYPixmap, I_in, 0, 0);
    I_in->format=XYBitmap;

    /* magnify horizontal text */
    if(style.magnify!=1.) {
	I_in=XRotMagnifyImage(dpy, I_in);

	old_cols_in=item->cols_in;
	old_rows_in=item->rows_in;
	item->cols_in=(int)(item->cols_in*style.magnify);
	item->rows_in=(int)(item->rows_in*style.magnify);
    }

    /* how big will rotated text be ? */
    item->cols_out=(int)(fabs(item->rows_in*sin_angle) +
			 fabs(item->cols_in*cos_angle) +0.99999 +2);

    item->rows_out=(int)(fabs(item->rows_in*cos_angle) +
			 fabs(item->cols_in*sin_angle) +0.99999 +2);

    if(item->cols_out%2==0)
	item->cols_out++;

    if(item->rows_out%2==0)
	item->rows_out++;

    /* create image to hold rotated text */
    item->ximage=MakeXImage(dpy, item->cols_out, item->rows_out);
    if(item->ximage==NULL) {
	XDestroyImage(I_in);
	free(item->corners_y);
	free(item->corners_x);
	free(item);
	return NULL;
    }

    byte_w_in=(item->cols_in-1)/8+1;
    byte_w_out=(item->cols_out-1)/8+1;

    /* we try to make this bit as fast as possible - which is why it looks
       a bit over-the-top */

    /* vertical distance from centre */
    dj=0.5-item->rows_out/2.;

    /* where abouts does text actually lie in rotated image? */
    /* check angle within 0.5 degrees (0.008 radians) */
    // This tolerance is in first R version, but not original
    if(fabs(angle)<0.008 || fabs(angle-M_PI/2)<0.008 ||
       fabs(angle-M_PI)<0.008 || fabs(angle-3*M_PI/2)<0.008) {
	xl=0;
	xr=(double)item->cols_out;
	xinc=0;
    }
    else if(angle<M_PI) {
	xl=item->cols_out/2.+
	    (dj-item->rows_in/(2.*cos_angle))/
	    tan(angle)-2.;
	xr=item->cols_out/2.+
	    (dj+item->rows_in/(2.*cos_angle))/
	    tan(angle)+2;
	xinc=1./tan(angle);
    }
    else {
	xl=item->cols_out/2.+
	    (dj+item->rows_in/(2.*cos_angle))/
	    tan(angle)-2.;
	xr=item->cols_out/2.+
	    (dj-item->rows_in/(2.*cos_angle))/
	    tan(angle)+2.;

	xinc=1./tan(angle);
    }

    /* loop through all relevent bits in rotated image */
    for(j=0; j<item->rows_out; j++) {

	/* no point re-calculating these every pass */
	di=(double)((xl<0)?0:(int)xl)+0.5-item->cols_out/2.;
	byte_out=(item->rows_out-j-1)*byte_w_out;

	/* loop through meaningful columns */
	for(i=((xl<0)?0:(int)xl);
	    i<((xr>=item->cols_out)?item->cols_out:(int)xr); i++) {

	    /* rotate coordinates */
	    // Using [ij]td is r6635
	    itd=item->cols_in/2. + ( di*cos_angle + dj*sin_angle);
	    jtd=item->rows_in/2. - (-di*sin_angle + dj*cos_angle);
	    it = (int)(itd - (itd < 0)); /* (int) -0.5 == 0 */
	    jt = (int)(jtd - (jtd < 0));

	    /* set pixel if required */
	    if(it>=0 && it<item->cols_in && jt>=0 && jt<item->rows_in)
		if((I_in->data[jt*byte_w_in+it/8] & 128>>(it%8))>0)
		    item->ximage->data[byte_out+i/8]|=128>>i%8;

	    di+=1;
	}
	dj+=1;
	xl+=xinc;
	xr+=xinc;
    }
    XDestroyImage(I_in);

    if(style.magnify!=1.) {
	item->cols_in=old_cols_in;
	item->rows_in=old_rows_in;
    }


#ifdef CACHE_BITMAPS

    /* create a bitmap to hold rotated text */
    item->bitmap=XCreatePixmap(dpy, DefaultRootWindow(dpy),
			       item->cols_out, item->rows_out, 1);

    /* make the text bitmap from XImage */
    XPutImage(dpy, item->bitmap, font_gc, item->ximage, 0, 0, 0, 0,
	      item->cols_out, item->rows_out);

    XDestroyImage(item->ximage);

#endif /*CACHE_BITMAPS*/

    XFreeGC(dpy, font_gc);
    XFreePixmap(dpy, canvas);

    return item;
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/*  Adds a text item to the end of the cache, removing as many items	  */
/*	from the front as required to keep cache size below limit	  */
/**************************************************************************/

static void XRotAddToLinkedList(Display *dpy, RotatedTextItem *item)
{

    static long int current_size=0;
    static RotatedTextItem *last=NULL;
    RotatedTextItem *i1=first_text_item, *i2=NULL;

#ifdef CACHE_BITMAPS

    /* I don't know how much memory a pixmap takes in the server -
       probably this + a bit more we can't account for */

    item->size=((item->cols_out-1)/8+1)*item->rows_out;

#else

    /* this is pretty much the size of a RotatedTextItem */

    item->size=((item->cols_out-1)/8+1)*item->rows_out +
	sizeof(XImage) + (int)strlen(item->text) +
	item->nl*8*sizeof(double) + sizeof(RotatedTextItem);

    if(item->font_name!=NULL)
	item->size+=(int)strlen(item->font_name);
    else
	item->size+=sizeof(Font);

#endif /*CACHE_BITMAPS */

#ifdef DEBUG
    /* count number of items in cache, for debugging */
    {
	int i=0;

	while(i1) {
	    i++;
	    i1=i1->next;
	}
	DEBUG_PRINT2("Cache has %d items.\n", i);
	i1=first_text_item;
    }
#endif

    DEBUG_PRINT4("current cache size=%ld, new item=%ld, limit=%d\n",
		 current_size, item->size, CACHE_SIZE_LIMIT*1024);

    /* if this item is bigger than whole cache, forget it */
    if(item->size>CACHE_SIZE_LIMIT*1024) {
	DEBUG_PRINT1("Too big to cache\n\n");
	item->cached=0;
	return;
    }

    /* remove elements from cache as needed */
    while(i1 && current_size+item->size>CACHE_SIZE_LIMIT*1024) {

	DEBUG_PRINT2("Removed %ld bytes\n", i1->size);

	if(i1->font_name!=NULL)
	    DEBUG_PRINT5("  (`%s'\n   %s\n   angle=%f align=%d)\n",
			 i1->text, i1->font_name, i1->angle, i1->align);

#ifdef CACHE_FID
	if(i1->font_name==NULL)
	    DEBUG_PRINT5("  (`%s'\n  FID=%ld\n   angle=%f align=%d)\n",
			 i1->text, i1->fid, i1->angle, i1->align);
#endif /*CACHE_FID*/

	current_size-=i1->size;

	i2=i1->next;

	/* free resources used by the unlucky item */
	XRotFreeTextItem(dpy, i1);

	/* remove it from linked list */
	first_text_item=i2;
	i1=i2;
    }

    /* add new item to end of linked list */
    if(first_text_item==NULL) {
	item->next=NULL;
	first_text_item=item;
	last=item;
    }
    else {
	item->next=NULL;
	last->next=item;
	last=item;
    }

    /* new cache size */
    current_size+=item->size;

    item->cached=1;

    DEBUG_PRINT1("Added item to cache.\n");
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/*  Free the resources used by a text item				  */
/**************************************************************************/

static void XRotFreeTextItem(Display *dpy, RotatedTextItem *item)
{
    free(item->text);

    if(item->font_name!=NULL)
	free(item->font_name);

    free((char *)item->corners_x);
    free((char *)item->corners_y);

#ifdef CACHE_BITMAPS
    XFreePixmap(dpy, item->bitmap);
#else
    XDestroyImage(item->ximage);
#endif /* CACHE_BITMAPS */

    free((char *)item);
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/* Magnify an XImage using bilinear interpolation			  */
/**************************************************************************/

static XImage *XRotMagnifyImage(Display *dpy, XImage *ximage)
{
    int i, j;
    double x, y;
    double u,t;
    XImage *I_out;
    int cols_in, rows_in;
    int cols_out, rows_out;
    register int i2, j2;
    double z1, z2, z3, z4;
    int byte_width_in, byte_width_out;
    double mag_inv;

    /* size of input image */
    cols_in=ximage->width;
    rows_in=ximage->height;

    /* size of final image */
    cols_out=(int)(cols_in*style.magnify);
    rows_out=(int)(rows_in*style.magnify);

    /* this will hold final image */
    I_out=MakeXImage(dpy, cols_out, rows_out);
    if(I_out==NULL)
	return NULL;

    /* width in bytes of input, output images */
    byte_width_in=(cols_in-1)/8+1;
    byte_width_out=(cols_out-1)/8+1;

    /* for speed */
    mag_inv=1./style.magnify;

    y=0.;

    /* loop over magnified image */
    for(j2=0; j2<rows_out; j2++) {
	x=0;
	j=(int)y;

	for(i2=0; i2<cols_out; i2++) {
	    i=(int)x;

	    /* bilinear interpolation - where are we on bitmap ? */
	    /* right edge */
	    if(i==cols_in-1 && j!=rows_in-1) {
		t=0;
		u=y-(double)j;

		z1=(ximage->data[j*byte_width_in+i/8] & 128>>(i%8))>0;
		z2=z1;
		z3=(ximage->data[(j+1)*byte_width_in+i/8] & 128>>(i%8))>0;
		z4=z3;
	    }
	    /* top edge */
	    else if(i!=cols_in-1 && j==rows_in-1) {
		t=x-(double)i;
		u=0;

		z1=(ximage->data[j*byte_width_in+i/8] & 128>>(i%8))>0;
		z2=(ximage->data[j*byte_width_in+(i+1)/8] & 128>>((i+1)%8))>0;
		z3=z2;
		z4=z1;
	    }
	    /* top right corner */
	    else if(i==cols_in-1 && j==rows_in-1) {
		u=0;
		t=0;

		z1=(ximage->data[j*byte_width_in+i/8] & 128>>(i%8))>0;
		z2=z1;
		z3=z1;
		z4=z1;
	    }
	    /* somewhere `safe' */
	    else {
		t=x-(double)i;
		u=y-(double)j;

		z1=(ximage->data[j*byte_width_in+i/8] & 128>>(i%8))>0;
		z2=(ximage->data[j*byte_width_in+(i+1)/8] & 128>>((i+1)%8))>0;
		z3=(ximage->data[(j+1)*byte_width_in+(i+1)/8] &
		    128>>((i+1)%8))>0;
		z4=(ximage->data[(j+1)*byte_width_in+i/8] & 128>>(i%8))>0;
	    }

            /* if interpolated value is greater than 0.5, set bit */
            if(((1-t)*(1-u)*z1 + t*(1-u)*z2 + t*u*z3 + (1-t)*u*z4)>0.5)
                I_out->data[j2*byte_width_out+i2/8]|=128>>i2%8;
            
	    x+=mag_inv;
	}
	y+=mag_inv;
    }

    /* destroy original */
    XDestroyImage(ximage);

    /* return big image */
    return I_out;
}



/* ---------------------------------------------------------------------- */


/**************************************************************************/
/* Calculate the bounding box some text will have when painted		  */
/**************************************************************************/

XPoint *XRotTextExtents(Display *dpy, XFontStruct *font, double angle,
			int x, int y, const char *text, int align)
{
    register int i;
    char *str1, *str2, *str3;
    char *str2_a="\0", *str2_b="\n\0";
    int height;
    double sin_angle, cos_angle;
    int nl, max_width;
    int cols_in, rows_in;
    double hot_x, hot_y;
    XPoint *xp_in, *xp_out;
    int dir, asc, desc;
    XCharStruct overall;

    /* manipulate angle to 0<=angle<360 degrees */
    while(angle<0)
	angle+=360;

    while(angle>360)
	angle-=360;

    angle *= DEG2RAD;

    /* count number of sections in string */
    nl=1;
    if(align!=NONE)
	for(i=(int)strlen(text)-2; i >= 0; i--)
	    if(text[i]=='\n')
		nl++;

    /* ignore newline characters if not doing alignment */
    if(align==NONE)
	str2=str2_a;
    else
	str2=str2_b;

    /* find width of longest section */
    str1=strdup(text);
    if(str1==NULL)
	return NULL;

    str3=strtok(str1, str2);

    XTextExtents(font, str3, (int)strlen(str3), &dir, &asc, &desc,
		 &overall);

    max_width=overall.rbearing;

    /* loop through each section */
    do {
	str3=strtok((char *)NULL, str2);

	if(str3!=NULL) {
	    XTextExtents(font, str3, (int)strlen(str3), &dir, &asc, &desc,
			 &overall);

	    if(overall.rbearing>max_width)
		max_width=overall.rbearing;
	}
    }
    while(str3!=NULL);

    free(str1);

    /* overall font height */
    height=font->ascent+font->descent;

    /* dimensions horizontal text will have */
    cols_in=max_width;
    rows_in=nl*height;

    /* pre-calculate sin and cos */
    sin_angle = myround(sin(angle)*1000.0) / 1000.0;
    cos_angle = myround(cos(angle)*1000.0) / 1000.0;

    /* y position */
    if(align==TLEFT || align==TCENTRE || align==TRIGHT)
	hot_y=rows_in/2.*style.magnify;
    else if(align==MLEFT || align==MCENTRE || align==MRIGHT)
	hot_y=0;
    else if(align==BLEFT || align==BCENTRE || align==BRIGHT)
	hot_y= -rows_in/2.*style.magnify;
    else
	hot_y= -(rows_in/2.- font->descent)*style.magnify;

    /* x position */
    if(align==TLEFT || align==MLEFT || align==BLEFT || align==NONE)
	hot_x= -max_width/2.*style.magnify;
    else if(align==TCENTRE || align==MCENTRE || align==BCENTRE)
	hot_x=0;
    else
	hot_x=max_width/2.*style.magnify;

    /* reserve space for XPoints */
    xp_in=(XPoint *)malloc((unsigned)(5*sizeof(XPoint)));
    if(!xp_in)
	return NULL;

    xp_out=(XPoint *)malloc((unsigned)(5*sizeof(XPoint)));
    if(!xp_out) {
	free(xp_in);
	return NULL;
    }

    /* bounding box when horizontal, relative to bitmap centre */
    xp_in[0].x= -(short)(cols_in*style.magnify/2. - style.bbx_pad);
    xp_in[0].y= (short)(rows_in*style.magnify/2. + style.bbx_pad);
    xp_in[1].x= (short)(cols_in*style.magnify/2. + style.bbx_pad);
    xp_in[1].y= (short)(rows_in*style.magnify/2. + style.bbx_pad);
    xp_in[2].x= (short)(cols_in*style.magnify/2. + style.bbx_pad);
    xp_in[2].y= -(short)(rows_in*style.magnify/2. - style.bbx_pad);
    xp_in[3].x= -(short)(cols_in*style.magnify/2. - style.bbx_pad);
    xp_in[3].y=-(short)(rows_in*style.magnify/2. - style.bbx_pad);
    xp_in[4].x=xp_in[0].x;
    xp_in[4].y=xp_in[0].y;

    /* rotate and translate bounding box */
    for(i=0; i<5; i++) {
	xp_out[i].x=(short)(x + ( ((double)xp_in[i].x-hot_x)*cos_angle +
				  ((double)xp_in[i].y+hot_y)*sin_angle));
	xp_out[i].y=(short)(y + (-((double)xp_in[i].x-hot_x)*sin_angle +
				 ((double)xp_in[i].y+hot_y)*cos_angle));
    }

    free((char *)xp_in);

    return xp_out;
}


static XFontStruct * RXFontStructOfFontSet(XFontSet font)
{
    char **ml;
    XFontStruct **fs_list;
    XFontsOfFontSet(font, &fs_list, &ml);
    return fs_list[0];
}

static int
XmbRotPaintAlignedString(Display *dpy, XFontSet font,
			 double angle, Drawable drawable, GC gc, int x, int y,
			 const char *text, int align);
static int
XmbRotDrawHorizontalString(Display *dpy, XFontSet font, Drawable drawable,
			   GC gc, int x, int y, const char *text, int align);
static RotatedTextItem
*XmbRotRetrieveFromCache(Display *dpy, XFontSet font, double angle, const char *text,
			 int align);
static RotatedTextItem
*XmbRotCreateTextItem(Display *dpy, XFontSet font, double angle, const char *text,
		      int align);

static int
XRfTextExtents(XFontSet font_set, const char *string, int num_bytes,
	       XRectangle *overall_ink_return,
	       XRectangle *overall_logical_return)
{
#ifdef HAVE_XUTF8TEXTEXTENTS
  if (utf8locale)
    return Xutf8TextExtents(font_set,string,num_bytes,
			    overall_ink_return, overall_logical_return);
#endif
  return XmbTextExtents(font_set,string,num_bytes,
			overall_ink_return, overall_logical_return);
}

static void
XRfDrawString(Display *display, Drawable d, XFontSet font_set, GC gc,
	      int x, int y, const char *string, int num_bytes)
{
#ifdef HAVE_XUTF8DRAWSTRING
    if (utf8locale)
	Xutf8DrawString(display, d, font_set, gc, x, y, string, num_bytes);
    else
#endif
	XmbDrawString(display, d, font_set, gc, x, y, string, num_bytes);
}

/* ---------------------------------------------------------------------- */


/**************************************************************************/
/*  A front end to XmbRotPaintAlignedString:				  */
/*	-no alignment, no background					  */
/**************************************************************************/


static int XmbRotDrawString(Display *dpy, XFontSet fontset, double angle,
			    Drawable drawable, GC gc, int x, int y, const char *str)
{
    return (XmbRotPaintAlignedString(dpy, fontset, angle, drawable, gc,
				     x, y, str, NONE));
}



/* ---------------------------------------------------------------------- */


/**************************************************************************/
/*  Aligns and paints a rotated string					  */
/**************************************************************************/

static int
XmbRotPaintAlignedString(Display *dpy, XFontSet font, double angle,
			 Drawable drawable, GC gc, int x, int y,
			 const char *text, int align)
{
    GC my_gc;
    int xp, yp;
    double hot_x, hot_y;
    double hot_xp, hot_yp;
    double sin_angle, cos_angle;
    RotatedTextItem *item;
    Pixmap bitmap_to_paint;
    XFontStruct *fs;

    /* return early for NULL/empty strings */
    if(text==NULL || *text=='\0')
	return 0;

    if(strlen(text)==0)
	return 0;

    /* manipulate angle to 0<=angle<360 degrees */
    while(angle<0)
	angle+=360;

    while(angle>=360)
	angle-=360;

    angle *= DEG2RAD;

    /* horizontal text made easy */
    if(angle==0. && style.magnify==1.)
	return(XmbRotDrawHorizontalString(dpy, font, drawable, gc, x, y,
					  text, align));

    /* get a rotated bitmap */
    item=XmbRotRetrieveFromCache(dpy, font, angle, text, align);
    if(item==NULL)
	return 0;

    /* this gc has similar properties to the user's gc */
    my_gc=XCreateGC(dpy, drawable, (unsigned long)0, 0);
    XCopyGC(dpy, gc, GCForeground|GCBackground|GCFunction|GCPlaneMask
	    |GCClipMask,
	    my_gc);

    /* alignment : which point (hot_x, hot_y) relative to bitmap centre
       coincides with user's specified point? */

    /* y position */
    fs = RXFontStructOfFontSet(font);

    if(align==TLEFT || align==TCENTRE || align==TRIGHT)
	hot_y=(double)item->rows_in/2*style.magnify;
    else if(align==MLEFT || align==MCENTRE || align==MRIGHT)
	hot_y=0;
    else if(align==BLEFT || align==BCENTRE || align==BRIGHT)
	hot_y= -(double)item->rows_in/2*style.magnify;
    else
	hot_y= -((double)item->rows_in/2-(double)fs->descent)*style.magnify;

    /* x position */
    if(align==TLEFT || align==MLEFT || align==BLEFT || align==NONE)
	hot_x= -(double)item->max_width/2*style.magnify;
    else if(align==TCENTRE || align==MCENTRE || align==BCENTRE)
	hot_x=0;
    else
	hot_x=(double)item->max_width/2*style.magnify;

    /* pre-calculate sin and cos */
    sin_angle = myround(sin(angle)*1000.0) / 1000.0;
    cos_angle = myround(cos(angle)*1000.0) / 1000.0;

    /* rotate hot_x and hot_y around bitmap centre */
    hot_xp = hot_x*cos_angle - hot_y*sin_angle;
    hot_yp = hot_x*sin_angle + hot_y*cos_angle;

    /* where should top left corner of bitmap go ? */
    xp=(int)(x-((double)item->cols_out/2 +hot_xp));
    yp=(int)(y-((double)item->rows_out/2 -hot_yp));

    /* by default we draw the rotated bitmap, solid */
    bitmap_to_paint=item->bitmap;

    /* handle user stippling */
#ifndef X11R3
    {
	GC depth_one_gc;
	XGCValues values;
	Pixmap new_bitmap, inverse;

	/* try and get some GC properties */
	if(XGetGCValues(dpy, gc,
			GCStipple|GCFillStyle|GCForeground|GCBackground|
			GCTileStipXOrigin|GCTileStipYOrigin,
			&values)) {

	    /* only do this if stippling requested */
	    if((values.fill_style==FillStippled ||
		values.fill_style==FillOpaqueStippled)) {

		/* opaque stipple: draw rotated text in background colour */
		if(values.fill_style==FillOpaqueStippled) {
		    XSetForeground(dpy, my_gc, values.background);
		    XSetFillStyle(dpy, my_gc, FillStippled);
		    XSetStipple(dpy, my_gc, item->bitmap);
		    XSetTSOrigin(dpy, my_gc, xp, yp);
		    XFillRectangle(dpy, drawable, my_gc, xp, yp,
				   item->cols_out, item->rows_out);
		    XSetForeground(dpy, my_gc, values.foreground);
		}

		/* this will merge the rotated text and the user's stipple */
		new_bitmap=XCreatePixmap(dpy, drawable,
					 item->cols_out, item->rows_out, 1);

		/* create a GC */
		depth_one_gc=XCreateGC(dpy, new_bitmap, (unsigned long)0, 0);
		XSetForeground(dpy, depth_one_gc, 1);
		XSetBackground(dpy, depth_one_gc, 0);

		/* set the relative stipple origin */
		XSetTSOrigin(dpy, depth_one_gc,
			     values.ts_x_origin-xp, values.ts_y_origin-yp);

		/* fill the whole bitmap with the user's stipple */
		XSetStipple(dpy, depth_one_gc, values.stipple);
		XSetFillStyle(dpy, depth_one_gc, FillOpaqueStippled);
		XFillRectangle(dpy, new_bitmap, depth_one_gc,
			       0, 0, item->cols_out, item->rows_out);

		/* set stipple origin back to normal */
		XSetTSOrigin(dpy, depth_one_gc, 0, 0);

		/* this will contain an inverse copy of the rotated text */
		inverse=XCreatePixmap(dpy, drawable,
				      item->cols_out, item->rows_out, 1);

		/* invert text */
		XSetFillStyle(dpy, depth_one_gc, FillSolid);
		XSetFunction(dpy, depth_one_gc, GXcopyInverted);
		XCopyArea(dpy, item->bitmap, inverse, depth_one_gc,
			  0, 0, item->cols_out, item->rows_out, 0, 0);

		/* now delete user's stipple everywhere EXCEPT on text */
		XSetForeground(dpy, depth_one_gc, 0);
		XSetBackground(dpy, depth_one_gc, 1);
		XSetStipple(dpy, depth_one_gc, inverse);
		XSetFillStyle(dpy, depth_one_gc, FillStippled);
		XSetFunction(dpy, depth_one_gc, GXcopy);
		XFillRectangle(dpy, new_bitmap, depth_one_gc,
			       0, 0, item->cols_out, item->rows_out);

		/* free resources */
		XFreePixmap(dpy, inverse);
		XFreeGC(dpy, depth_one_gc);

		/* this is the new bitmap */
		bitmap_to_paint=new_bitmap;
	    }
	}
    }
#endif /*X11R3*/

    /* paint text using stipple technique */
    XSetFillStyle(dpy, my_gc, FillStippled);
    XSetStipple(dpy, my_gc, bitmap_to_paint);
    XSetTSOrigin(dpy, my_gc, xp, yp);
    XFillRectangle(dpy, drawable, my_gc, xp, yp,
		   item->cols_out, item->rows_out);

    /* free our resources */
    XFreeGC(dpy, my_gc);

    /* stippled bitmap no longer needed */
    if(bitmap_to_paint!=item->bitmap)
	XFreePixmap(dpy, bitmap_to_paint);

#ifdef CACHE_XIMAGES
    XFreePixmap(dpy, item->bitmap);
#endif /*CACHE_XIMAGES*/

    /* if item isn't cached, destroy it completely */
    if(!item->cached)
	XRotFreeTextItem(dpy,item);

    /* we got to the end OK! */
    return 0;
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/*  Draw a horizontal string in a quick fashion				  */
/**************************************************************************/

static int XmbRotDrawHorizontalString(Display *dpy, XFontSet font,
				      Drawable drawable, GC gc, int x, int y,
				      const char *text, int align)
{
    GC my_gc;
    int nl=1, i;
    int height;
    int xp, yp;
    char *str1, *str2, *str3;
    char *str2_a="\0", *str2_b="\n\0";

    if (text == NULL || *text=='\0') {
      DEBUG_PRINT1("Empty string, ignoring\n");
      return 0;
    }

    /* this gc has similar properties to the user's gc (including stipple) */
    my_gc=XCreateGC(dpy, drawable, (unsigned long)0, 0);
    XCopyGC(dpy, gc,
	    GCForeground|GCBackground|GCFunction|GCStipple|GCFillStyle|
	    GCTileStipXOrigin|GCTileStipYOrigin|GCPlaneMask|GCClipMask, my_gc);

    /* count number of sections in string */
    // for() loop changed in r42804 
    // (efficiency, PR#9902, avoid recalculating strlen)
    if(align!=NONE)
	for(i=(int)strlen(text)-2; i >= 0; i--)
	    if(text[i]=='\n')
		nl++;

    /* ignore newline characters if not doing alignment */
    if(align==NONE)
	str2=str2_a;
    else
	str2=str2_b;

    /* overall font height */
    height
      = RXFontStructOfFontSet(font)->ascent
      + RXFontStructOfFontSet(font)->descent;

    /* y position */
    if(align==TLEFT || align==TCENTRE || align==TRIGHT)
	yp=y+RXFontStructOfFontSet(font)->ascent;
    else if(align==MLEFT || align==MCENTRE || align==MRIGHT)
	yp=y-nl*height/2+RXFontStructOfFontSet(font)->ascent;
    else if(align==BLEFT || align==BCENTRE || align==BRIGHT)
	yp=y-nl*height+RXFontStructOfFontSet(font)->ascent;
    else
	yp=y;

    str1 = strdup(text);
    if(str1 == NULL) return 1;

    str3=strtok(str1, str2);

    /* loop through each section in the string */
    do {

	XRectangle    r_ink, r_log;
	XRfTextExtents(font, str3, (int)strlen(str3), &r_ink, &r_log);

	/* where to draw section in x ? */
	if(align==TLEFT || align==MLEFT || align==BLEFT || align==NONE)
	    xp=x;
	else if(align==TCENTRE || align==MCENTRE || align==BCENTRE)
	    xp=x-r_log.width/2;
	else
	    xp=x-r_log.width;

	/* draw string onto bitmap */
	XRfDrawString(dpy, drawable, font, my_gc, xp, yp, str3, (int)strlen(str3));

	/* move to next line */
	yp+=height;

	str3=strtok((char *)NULL, str2);
    }
    while(str3!=NULL);

    free(str1);
    XFreeGC(dpy, my_gc);

    return 0;
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/*   Query cache for a match with this font/text/angle/alignment	  */
/*	 request, otherwise arrange for its creation			  */
/**************************************************************************/

static RotatedTextItem
*XmbRotRetrieveFromCache(Display *dpy, XFontSet font,
			 double angle, const char *text,
			 int align)
{
    Font fid;
    char *font_name=NULL;
    unsigned long name_value;
    RotatedTextItem *item=NULL;
    RotatedTextItem *i1=first_text_item;

    /* get font name, if it exists */
    if(XGetFontProperty(RXFontStructOfFontSet(font), XA_FONT, &name_value)) {
	DEBUG_PRINT1("got font name OK\n");
	font_name=XGetAtomName(dpy, name_value);
	fid=0;
    }
#ifdef CACHE_FID
    /* otherwise rely (unreliably?) on font ID */
    else {
	DEBUG_PRINT1("can't get fontname, caching FID\n");
	font_name=NULL;
	fid=font->fid;
    }
#else
    /* not allowed to cache font ID's */
    else {
	DEBUG_PRINT1("can't get fontname, can't cache\n");
	font_name=NULL;
	fid=0;
    }
#endif /*CACHE_FID*/

    /* look for a match in cache */

    /* matching formula:
       identical text;
       identical fontname (if defined, font ID's if not);
       angles close enough (<0.0001 here, could be smaller);
       HORIZONTAL alignment matches, OR it's a one line string;
       magnifications the same */
    // original version had 0.00001, first R version 0.0001

    while(i1 && !item) {
	/* match everything EXCEPT fontname/ID */
	if(strcmp(text, i1->text)==0 &&
	   fabs(angle-i1->angle)<0.0001 &&
	   style.magnify==i1->magnify &&
	   (i1->nl==1 ||
	    ((align==0)?9:(align-1))%3==
	      ((i1->align==0)?9:(i1->align-1))%3)) {

	    /* now match fontname/ID */
	    if(font_name!=NULL && i1->font_name!=NULL) {
		if(strcmp(font_name, i1->font_name)==0) {
		    item=i1;
		    DEBUG_PRINT1("Matched against font names\n");
		}
		else
		    i1=i1->next;
	    }
#ifdef CACHE_FID
	    else if(font_name==NULL && i1->font_name==NULL) {
		if(fid==i1->fid) {
		    item=i1;
		    DEBUG_PRINT1("Matched against FID's\n");
		}
		else
		    i1=i1->next;
	    }
#endif /*CACHE_FID*/
	    else
		i1=i1->next;
	}
	else
	    i1=i1->next;
    }

    if(item)
	DEBUG_PRINT1("**Found target in cache.\n");
    if(!item)
	DEBUG_PRINT1("**No match in cache.\n");

    /* no match */
    if(!item) {
	/* create new item */
	item=XmbRotCreateTextItem(dpy, font, angle, text, align);
	if(!item)
	    return NULL;

	/* record what it shows */
	item->text=strdup(text);

	/* fontname or ID */
	if(font_name!=NULL) {
	    item->font_name=strdup(font_name);
	    item->fid=0;
	}
	else {
	    item->font_name=NULL;
	    item->fid=fid;
	}

	item->angle=angle;
	item->align=align;
	item->magnify=style.magnify;

	/* cache it */
	XRotAddToLinkedList(dpy, item);
    }

    if(font_name)
	XFree(font_name);

    /* if XImage is cached, need to recreate the bitmap */

#ifdef CACHE_XIMAGES
    {
	GC depth_one_gc;

	/* create bitmap to hold rotated text */
	item->bitmap=XCreatePixmap(dpy, DefaultRootWindow(dpy),
				   item->cols_out, item->rows_out, 1);

	/* depth one gc */
	depth_one_gc=XCreateGC(dpy, item->bitmap, (unsigned long)0, 0);
	XSetBackground(dpy, depth_one_gc, 0);
	XSetForeground(dpy, depth_one_gc, 1);

	/* make the text bitmap from XImage */
	XPutImage(dpy, item->bitmap, depth_one_gc, item->ximage, 0, 0, 0, 0,
		  item->cols_out, item->rows_out);

	XFreeGC(dpy, depth_one_gc);
    }
#endif /*CACHE_XIMAGES*/

    return item;
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/*  Create a rotated text item						  */
/**************************************************************************/

static RotatedTextItem
*XmbRotCreateTextItem(Display *dpy, XFontSet font,
		      double angle, const char *text, int align)
{
    RotatedTextItem *item=NULL;
    Pixmap canvas;
    GC font_gc;
    XImage *I_in;
    register int i, j;
    char *str1, *str2, *str3;
    char *str2_a="\0", *str2_b="\n\0";
    int height;
    int byte_w_in, byte_w_out;
    int xp, yp;
    double sin_angle, cos_angle;
    int it, jt;
    double itd, jtd;
    double di, dj;
    int ic=0;
    double xl, xr, xinc;
    int byte_out;
    XRectangle    r_ink, r_log;
    int old_cols_in=0, old_rows_in=0;

    /* allocate memory */
    item=(RotatedTextItem *)malloc((unsigned)sizeof(RotatedTextItem));
    if(!item)
	return NULL;

    /* count number of sections in string */
    item->nl=1;
    if(align!=NONE)
	for(i=(int)strlen(text)-2; i >= 0; i--)
	    if(text[i]=='\n')
		item->nl++;

    /* ignore newline characters if not doing alignment */
    if(align==NONE)
	str2=str2_a;
    else
	str2=str2_b;

    /* find width of longest section */
    str1=strdup(text);
    if(str1==NULL) {
	free(item);
	return NULL;
    }

    str3=strtok(str1, str2);

    XRfTextExtents(font, str3, (int)strlen(str3), &r_ink, &r_log);

    item->max_width=r_log.width;

    /* loop through each section */
    do {
	str3=strtok((char *)NULL, str2);

	if(str3!=NULL) {
	  XRfTextExtents(font, str3, (int)strlen(str3), &r_ink, &r_log);

	    if(r_log.width>item->max_width)
		item->max_width=r_log.width;
	}
    }
    while(str3!=NULL);

    free(str1);

    /* overall font height */
    height=RXFontStructOfFontSet(font)->ascent
      +RXFontStructOfFontSet(font)->descent;

    /* dimensions horizontal text will have */
    item->cols_in=item->max_width;
    item->rows_in=item->nl*height;

    /* fudge in case one of the above is zero: */
    if (!item->cols_in) item->cols_in=1;
    if (!item->rows_in) item->rows_in=1;

    /* bitmap for drawing on */
    canvas=XCreatePixmap(dpy, DefaultRootWindow(dpy),
			 item->cols_in, item->rows_in, 1);

    /* create a GC for the bitmap */
    font_gc=XCreateGC(dpy, canvas, (unsigned long)0, 0);
    XSetBackground(dpy, font_gc, 0);

    /* make sure the bitmap is blank */
    XSetForeground(dpy, font_gc, 0);
    XFillRectangle(dpy, canvas, font_gc, 0, 0,
		   item->cols_in+1, item->rows_in+1);
    XSetForeground(dpy, font_gc, 1);

    /* pre-calculate sin and cos */
    sin_angle = myround(sin(angle)*1000.0) / 1000.0;
    cos_angle = myround(cos(angle)*1000.0) / 1000.0;

    /* text background will be drawn using XFillPolygon */
    item->corners_x=
	(double *)malloc((unsigned)(4*item->nl*sizeof(double)));
    if(!item->corners_x) {
	free(item);
	XFreeGC(dpy, font_gc);
	XFreePixmap(dpy, canvas);
	return NULL;
    }

    item->corners_y=
	(double *)malloc((unsigned)(4*item->nl*sizeof(double)));
    if(!item->corners_y) {
	free(item->corners_x);
	free(item);
	XFreeGC(dpy, font_gc);
	XFreePixmap(dpy, canvas);
	return NULL;
    }

    /* draw text horizontally */

    /* start at top of bitmap */
    yp=RXFontStructOfFontSet(font)->ascent;

    str1=strdup(text);
    if(str1==NULL) {
	free(item->corners_y);
	free(item->corners_x);
	free(item);
	XFreeGC(dpy, font_gc);
	XFreePixmap(dpy, canvas);
	return NULL;
    }

    str3=strtok(str1, str2);

    /* loop through each section in the string */
    do {
	XRectangle    r_ink, r_log;
	XRfTextExtents(font, str3, (int)strlen(str3), &r_ink, &r_log);

	/* where to draw section in x ? */
	if(align==TLEFT || align==MLEFT || align==BLEFT || align==NONE)
	    xp=0;
	else if(align==TCENTRE || align==MCENTRE || align==BCENTRE)
	    xp=(item->max_width-r_log.width)/2;
	else
	    xp=item->max_width-r_log.width;

	/* draw string onto bitmap */
	XRfDrawString(dpy, canvas, font, font_gc, xp, yp, str3, (int)strlen(str3));

	/* keep a note of corner positions of this string */
	item->corners_x[ic]=((double)xp-(double)item->cols_in/2)*style.magnify;
	item->corners_y[ic]=((double)(yp-RXFontStructOfFontSet(font)->ascent)-
			     (double)item->rows_in/2) *style.magnify;
	item->corners_x[ic+1]=item->corners_x[ic];
	item->corners_y[ic+1]=item->corners_y[ic]+(double)height*style.magnify;
	item->corners_x[item->nl*4-1-ic]=item->corners_x[ic]+
	    (double)r_log.width*style.magnify;
	item->corners_y[item->nl*4-1-ic]=item->corners_y[ic];
	item->corners_x[item->nl*4-2-ic]=
	    item->corners_x[item->nl*4-1-ic];
	item->corners_y[item->nl*4-2-ic]=item->corners_y[ic+1];

	ic+=2;

	/* move to next line */
	yp+=height;

	str3=strtok((char *)NULL, str2);
    }
    while(str3!=NULL);

    free(str1);

    /* create image to hold horizontal text */
    I_in=MakeXImage(dpy, item->cols_in, item->rows_in);
    if(I_in==NULL) {
	free(item->corners_y);
	free(item->corners_x);
	free(item);
	XFreeGC(dpy, font_gc);
	XFreePixmap(dpy, canvas);
	return NULL;
    }

    /* extract horizontal text */
    XGetSubImage(dpy, canvas, 0, 0, item->cols_in, item->rows_in,
		 1, XYPixmap, I_in, 0, 0);
    I_in->format=XYBitmap;

    /* magnify horizontal text */
    if(style.magnify!=1.) {
	I_in=XRotMagnifyImage(dpy, I_in);

	old_cols_in=item->cols_in;
	old_rows_in=item->rows_in;
	item->cols_in=(int)(item->cols_in*style.magnify);
	item->rows_in=(int)(item->rows_in*style.magnify);
    }

    /* how big will rotated text be ? */
    item->cols_out=(int)(fabs(item->rows_in*sin_angle) +
			 fabs(item->cols_in*cos_angle) +0.99999 +2);

    item->rows_out=(int)(fabs(item->rows_in*cos_angle) +
			 fabs(item->cols_in*sin_angle) +0.99999 +2);

    if(item->cols_out%2==0)
	item->cols_out++;

    if(item->rows_out%2==0)
	item->rows_out++;

    /* create image to hold rotated text */
    item->ximage=MakeXImage(dpy, item->cols_out, item->rows_out);
    if(item->ximage==NULL) {
	XDestroyImage(I_in);
	free(item->corners_y);
	free(item->corners_x);
	free(item);
	XFreeGC(dpy, font_gc);
	XFreePixmap(dpy, canvas);
	return NULL;
    }

    byte_w_in=(item->cols_in-1)/8+1;
    byte_w_out=(item->cols_out-1)/8+1;

    /* we try to make this bit as fast as possible - which is why it looks
       a bit over-the-top */

    /* vertical distance from centre */
    dj=0.5-(double)item->rows_out/2;

    /* where abouts does text actually lie in rotated image? */
    /* check angle within 0.5 degrees (0.008 radians) */
    if(fabs(angle)<0.008 || fabs(angle-M_PI/2)<0.008 ||
       fabs(angle-M_PI)<0.008 || fabs(angle-3*M_PI/2)<0.008 ||
       fabs(angle-2*M_PI)<0.008) {
	xl=0;
	xr=(double)item->cols_out;
	xinc=0;
    }
    else if(angle<M_PI) {
	xl=(double)item->cols_out/2+
	    (dj-(double)item->rows_in/(2*cos_angle))/
	    tan(angle)-2;
	xr=(double)item->cols_out/2+
	    (dj+(double)item->rows_in/(2*cos_angle))/
	    tan(angle)+2;
	xinc=1./tan(angle);
    }
    else {
	xl=(double)item->cols_out/2+
	    (dj+(double)item->rows_in/(2*cos_angle))/
	    tan(angle)-2;
	xr=(double)item->cols_out/2+
	    (dj-(double)item->rows_in/(2*cos_angle))/
	    tan(angle)+2;

	xinc=1./tan(angle);
    }

    /* loop through all relevent bits in rotated image */
    for(j=0; j<item->rows_out; j++) {

	/* no point re-calculating these every pass */
	di=(double)((xl<0)?0:(int)xl)+0.5-(double)item->cols_out/2;
	byte_out=(item->rows_out-j-1)*byte_w_out;

	/* loop through meaningful columns */
	for(i=((xl<0)?0:(int)xl);
	    i<((xr>=item->cols_out)?item->cols_out:(int)xr); i++) {

	    /* rotate coordinates */
	    itd=(double)item->cols_in/2 + ( di*cos_angle + dj*sin_angle);
	    jtd=(double)item->rows_in/2 - (-di*sin_angle + dj*cos_angle);
	    it = (int)(itd - (itd < 0)); /* (int) -0.5 == 0 */
	    jt = (int)(jtd - (jtd < 0));

	    /* set pixel if required */
	    if(it>=0 && it<item->cols_in && jt>=0 && jt<item->rows_in)
		if((I_in->data[jt*byte_w_in+it/8] & 128>>(it%8))>0)
		    item->ximage->data[byte_out+i/8]|=128>>i%8;

	    di+=1;
	}
	dj+=1;
	xl+=xinc;
	xr+=xinc;
    }
    XDestroyImage(I_in);

    if(style.magnify!=1.) {
	item->cols_in=old_cols_in;
	item->rows_in=old_rows_in;
    }


#ifdef CACHE_BITMAPS

    /* create a bitmap to hold rotated text */
    item->bitmap=XCreatePixmap(dpy, DefaultRootWindow(dpy),
			       item->cols_out, item->rows_out, 1);

    /* make the text bitmap from XImage */
    XPutImage(dpy, item->bitmap, font_gc, item->ximage, 0, 0, 0, 0,
	      item->cols_out, item->rows_out);

    XDestroyImage(item->ximage);

#endif /*CACHE_BITMAPS*/

    XFreeGC(dpy, font_gc);
    XFreePixmap(dpy, canvas);

    return item;
}


/* ---------------------------------------------------------------------- */


/**************************************************************************/
/* Calculate the bounding box some text will have when painted		  */
/**************************************************************************/

XPoint *XmbRotTextExtents(Display *dpy, XFontSet font, double angle,
			  int x, int y, const char *text, int align)
{
    register int i;
    char *str1, *str2, *str3;
    char *str2_a="\0", *str2_b="\n\0";
    int height;
    double sin_angle, cos_angle;
    int nl, max_width;
    int cols_in, rows_in;
    double hot_x, hot_y;
    XPoint *xp_in, *xp_out;
    XRectangle    r_ink, r_log;

    /* manipulate angle to 0<=angle<360 degrees */
    while(angle<0)
	angle+=360;

    while(angle>360)
	angle-=360;

    angle *= DEG2RAD;

    /* count number of sections in string */
    nl=1;
    if(align!=NONE)
	for(i=(int)strlen(text)-2; i >= 0; i--)
	    if(text[i]=='\n')
		nl++;

    /* ignore newline characters if not doing alignment */
    if(align==NONE)
	str2=str2_a;
    else
	str2=str2_b;

    /* find width of longest section */
    str1=strdup(text);
    if(str1==NULL)
	return NULL;

    str3=strtok(str1, str2);

    XRfTextExtents(font, str3, (int)strlen(str3), &r_ink, &r_log);

    max_width=r_log.width;

    /* loop through each section */
    do {
	str3=strtok((char *)NULL, str2);

	if(str3!=NULL) {
	  XRfTextExtents(font, str3, (int)strlen(str3), &r_ink, &r_log);

	    if(r_log.width>max_width)
		max_width=r_log.width;
	}
    }
    while(str3!=NULL);

    free(str1);

    /* overall font height */
    height=RXFontStructOfFontSet(font)->ascent
      +RXFontStructOfFontSet(font)->descent;

    /* dimensions horizontal text will have */
    cols_in=max_width;
    rows_in=nl*height;

    /* pre-calculate sin and cos */
    sin_angle = myround(sin(angle)*1000.0) / 1000.0;
    cos_angle = myround(cos(angle)*1000.0) / 1000.0;

    /* y position */
    if(align==TLEFT || align==TCENTRE || align==TRIGHT)
	hot_y=(double)rows_in/2*style.magnify;
    else if(align==MLEFT || align==MCENTRE || align==MRIGHT)
	hot_y=0;
    else if(align==BLEFT || align==BCENTRE || align==BRIGHT)
	hot_y= -(double)rows_in/2*style.magnify;
    else
	hot_y= -((double)rows_in/2-
		 (double)RXFontStructOfFontSet(font)->descent)*style.magnify;

    /* x position */
    if(align==TLEFT || align==MLEFT || align==BLEFT || align==NONE)
	hot_x= -(double)max_width/2*style.magnify;
    else if(align==TCENTRE || align==MCENTRE || align==BCENTRE)
	hot_x=0;
    else
	hot_x=(double)max_width/2*style.magnify;

    /* reserve space for XPoints */
    xp_in=(XPoint *)malloc((unsigned)(5*sizeof(XPoint)));
    if(!xp_in)
	return NULL;

    xp_out=(XPoint *)malloc((unsigned)(5*sizeof(XPoint)));
    if(!xp_out) {
	free(xp_in);
	return NULL;
    }

    /* bounding box when horizontal, relative to bitmap centre */
    xp_in[0].x= -(short)(cols_in*style.magnify/2. - style.bbx_pad);
    xp_in[0].y= (short)(rows_in*style.magnify/2. + style.bbx_pad);
    xp_in[1].x= (short)(cols_in*style.magnify/2. + style.bbx_pad);
    xp_in[1].y= (short)(rows_in*style.magnify/2. + style.bbx_pad);
    xp_in[2].x= (short)(cols_in*style.magnify/2. + style.bbx_pad);
    xp_in[2].y= -(short)(rows_in*style.magnify/2. - style.bbx_pad);
    xp_in[3].x= -(short)(cols_in*style.magnify/2. - style.bbx_pad);
    xp_in[3].y=-(short)(rows_in*style.magnify/2. - style.bbx_pad);
    xp_in[4].x=xp_in[0].x;
    xp_in[4].y=xp_in[0].y;

    /* rotate and translate bounding box */
    for(i=0; i<5; i++) {
	xp_out[i].x=(short)(x + ( ((double)xp_in[i].x-hot_x)*cos_angle +
				((double)xp_in[i].y+hot_y)*sin_angle));
	xp_out[i].y=(short)(y + (-((double)xp_in[i].x-hot_x)*sin_angle +
				 ((double)xp_in[i].y+hot_y)*cos_angle));
    }

    free((char *)xp_in);

    return xp_out;
}
