/* ************************************************************************ */


/* Header file for the `xvertext 5.0' routines.

   Copyright (c) 1993 Alan Richardson (mppa3@uk.ac.sussex.syma) */


/* ************************************************************************ */

#ifndef _XVERTEXT_INCLUDED_
#define _XVERTEXT_INCLUDED_


#define XV_VERSION	5.0
#define XV_COPYRIGHT \
      "xvertext routines Copyright (c) 1993 Alan Richardson"


/* ---------------------------------------------------------------------- */


/* text alignment */

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


/* ---------------------------------------------------------------------- */

/* Protoized : C++ or ANSI C */
/* only XRotDrawString is used in R */
double	XRotVersion(char*, int);
void	XRotSetMagnification(double);
void	XRotSetBoundingBoxPad(int);
int	XRotDrawString(Display*, XFontStruct*, double,
		       Drawable, GC, int, int, char*);
int	XRotDrawImageString(Display*, XFontStruct*, double,
			    Drawable, GC, int, int, char*);
int	XRotDrawAlignedString(Display*, XFontStruct*, double,
			      Drawable, GC, int, int, char*, int);
int	XRotDrawAlignedImageString(Display*, XFontStruct*, double,
				   Drawable, GC, int, int, char*, int);
XPoint *XRotTextExtents(Display*, XFontStruct*, double,
			int, int, char*, int);

/* addition in 2.1.0 */
int	XmbRotDrawString(Display*, XFontSet, double,
			 Drawable, GC, int, int, char*);

/* ---------------------------------------------------------------------- */

void                    XSetFontSet(Display *display,GC gc,XFontSet font);
XFontStruct            *XFontStructOfFontSet(XFontSet font);

#endif /* _XVERTEXT_INCLUDED_ */



