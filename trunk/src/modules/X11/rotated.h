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


typedef enum {One_Font, Font_Set} R_FontType;

typedef struct R_XFont
{
    R_FontType type;
    XFontStruct *font;  
    XFontSet fontset; 
    int height;  
    int ascent;
    int descent;
} R_XFont;


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
int	XRfRotDrawString(Display*, R_XFont*, double,
			 Drawable, GC, int, int, char*);

/* ---------------------------------------------------------------------- */
#endif /* _XVERTEXT_INCLUDED_ */
