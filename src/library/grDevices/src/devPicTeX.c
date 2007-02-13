/*
 *  A PicTeX device, (C) 1996 Valerio Aimale, for
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2001-5  The R Development Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>

#ifdef SUPPORT_MBCS
# include <R_ext/rlocale.h>
# include <wchar.h>
#endif /* SUPPORT_MBCS */

#include "Graphics.h"
#include "Fileio.h"
#include <Rdevices.h>
#include "grDevices.h"

	/* device-specific information per picTeX device */

#define DOTSperIN	72.27
#define in2dots(x) 	(DOTSperIN * x)

typedef struct {
    FILE *texfp;
    char filename[128];
    int pageno;
    int landscape;
    double width;
    double height;
    double pagewidth;
    double pageheight;
    double xlast;
    double ylast;
    double clipleft, clipright, cliptop, clipbottom;
    double clippedx0, clippedy0, clippedx1, clippedy1;
    int lty;
    rcolor col;
    rcolor fill;
    int fontsize;
    int fontface;
    Rboolean debug;
} picTeXDesc;


	/* Global device information */

static const double charwidth[4][128] = {
{
  0.5416690, 0.8333360, 0.7777810, 0.6111145, 0.6666690, 0.7083380, 0.7222240,
  0.7777810, 0.7222240, 0.7777810, 0.7222240, 0.5833360, 0.5361130, 0.5361130,
  0.8138910, 0.8138910, 0.2388900, 0.2666680, 0.5000020, 0.5000020, 0.5000020,
  0.5000020, 0.5000020, 0.6666700, 0.4444460, 0.4805580, 0.7222240, 0.7777810,
  0.5000020, 0.8611145, 0.9722260, 0.7777810, 0.2388900, 0.3194460, 0.5000020,
  0.8333360, 0.5000020, 0.8333360, 0.7583360, 0.2777790, 0.3888900, 0.3888900,
  0.5000020, 0.7777810, 0.2777790, 0.3333340, 0.2777790, 0.5000020, 0.5000020,
  0.5000020, 0.5000020, 0.5000020, 0.5000020, 0.5000020, 0.5000020, 0.5000020,
  0.5000020, 0.5000020, 0.2777790, 0.2777790, 0.3194460, 0.7777810, 0.4722240,
  0.4722240, 0.6666690, 0.6666700, 0.6666700, 0.6388910, 0.7222260, 0.5972240,
  0.5694475, 0.6666690, 0.7083380, 0.2777810, 0.4722240, 0.6944480, 0.5416690,
  0.8750050, 0.7083380, 0.7361130, 0.6388910, 0.7361130, 0.6458360, 0.5555570,
  0.6805570, 0.6875050, 0.6666700, 0.9444480, 0.6666700, 0.6666700, 0.6111130,
  0.2888900, 0.5000020, 0.2888900, 0.5000020, 0.2777790, 0.2777790, 0.4805570,
  0.5166680, 0.4444460, 0.5166680, 0.4444460, 0.3055570, 0.5000020, 0.5166680,
  0.2388900, 0.2666680, 0.4888920, 0.2388900, 0.7944470, 0.5166680, 0.5000020,
  0.5166680, 0.5166680, 0.3416690, 0.3833340, 0.3611120, 0.5166680, 0.4611130,
  0.6833360, 0.4611130, 0.4611130, 0.4347230, 0.5000020, 1.0000030, 0.5000020,
  0.5000020, 0.5000020
},
{
  0.5805590, 0.9166720, 0.8555600, 0.6722260, 0.7333370, 0.7944490, 0.7944490,
  0.8555600, 0.7944490, 0.8555600, 0.7944490, 0.6416700, 0.5861150, 0.5861150,
  0.8916720, 0.8916720, 0.2555570, 0.2861130, 0.5500030, 0.5500030, 0.5500030,
  0.5500030, 0.5500030, 0.7333370, 0.4888920, 0.5652800, 0.7944490, 0.8555600,
  0.5500030, 0.9472275, 1.0694500, 0.8555600, 0.2555570, 0.3666690, 0.5583360,
  0.9166720, 0.5500030, 1.0291190, 0.8305610, 0.3055570, 0.4277800, 0.4277800,
  0.5500030, 0.8555600, 0.3055570, 0.3666690, 0.3055570, 0.5500030, 0.5500030,
  0.5500030, 0.5500030, 0.5500030, 0.5500030, 0.5500030, 0.5500030, 0.5500030,
  0.5500030, 0.5500030, 0.3055570, 0.3055570, 0.3666690, 0.8555600, 0.5194470,
  0.5194470, 0.7333370, 0.7333370, 0.7333370, 0.7027820, 0.7944490, 0.6416700,
  0.6111145, 0.7333370, 0.7944490, 0.3305570, 0.5194470, 0.7638930, 0.5805590,
  0.9777830, 0.7944490, 0.7944490, 0.7027820, 0.7944490, 0.7027820, 0.6111145,
  0.7333370, 0.7638930, 0.7333370, 1.0388950, 0.7333370, 0.7333370, 0.6722260,
  0.3430580, 0.5583360, 0.3430580, 0.5500030, 0.3055570, 0.3055570, 0.5250030,
  0.5611140, 0.4888920, 0.5611140, 0.5111140, 0.3361130, 0.5500030, 0.5611140,
  0.2555570, 0.2861130, 0.5305590, 0.2555570, 0.8666720, 0.5611140, 0.5500030,
  0.5611140, 0.5611140, 0.3722250, 0.4216690, 0.4041690, 0.5611140, 0.5000030,
  0.7444490, 0.5000030, 0.5000030, 0.4763920, 0.5500030, 1.1000060, 0.5500030,
  0.5500030, 0.550003 },
{
  0.5416690, 0.8333360, 0.7777810, 0.6111145, 0.6666690, 0.7083380, 0.7222240,
  0.7777810, 0.7222240, 0.7777810, 0.7222240, 0.5833360, 0.5361130, 0.5361130,
  0.8138910, 0.8138910, 0.2388900, 0.2666680, 0.5000020, 0.5000020, 0.5000020,
  0.5000020, 0.5000020, 0.7375210, 0.4444460, 0.4805580, 0.7222240, 0.7777810,
  0.5000020, 0.8611145, 0.9722260, 0.7777810, 0.2388900, 0.3194460, 0.5000020,
  0.8333360, 0.5000020, 0.8333360, 0.7583360, 0.2777790, 0.3888900, 0.3888900,
  0.5000020, 0.7777810, 0.2777790, 0.3333340, 0.2777790, 0.5000020, 0.5000020,
  0.5000020, 0.5000020, 0.5000020, 0.5000020, 0.5000020, 0.5000020, 0.5000020,
  0.5000020, 0.5000020, 0.2777790, 0.2777790, 0.3194460, 0.7777810, 0.4722240,
  0.4722240, 0.6666690, 0.6666700, 0.6666700, 0.6388910, 0.7222260, 0.5972240,
  0.5694475, 0.6666690, 0.7083380, 0.2777810, 0.4722240, 0.6944480, 0.5416690,
  0.8750050, 0.7083380, 0.7361130, 0.6388910, 0.7361130, 0.6458360, 0.5555570,
  0.6805570, 0.6875050, 0.6666700, 0.9444480, 0.6666700, 0.6666700, 0.6111130,
  0.2888900, 0.5000020, 0.2888900, 0.5000020, 0.2777790, 0.2777790, 0.4805570,
  0.5166680, 0.4444460, 0.5166680, 0.4444460, 0.3055570, 0.5000020, 0.5166680,
  0.2388900, 0.2666680, 0.4888920, 0.2388900, 0.7944470, 0.5166680, 0.5000020,
  0.5166680, 0.5166680, 0.3416690, 0.3833340, 0.3611120, 0.5166680, 0.4611130,
  0.6833360, 0.4611130, 0.4611130, 0.4347230, 0.5000020, 1.0000030, 0.5000020,
  0.5000020, 0.5000020 },
{
  0.5805590, 0.9166720, 0.8555600, 0.6722260, 0.7333370, 0.7944490, 0.7944490,
  0.8555600, 0.7944490, 0.8555600, 0.7944490, 0.6416700, 0.5861150, 0.5861150,
  0.8916720, 0.8916720, 0.2555570, 0.2861130, 0.5500030, 0.5500030, 0.5500030,
  0.5500030, 0.5500030, 0.8002530, 0.4888920, 0.5652800, 0.7944490, 0.8555600,
  0.5500030, 0.9472275, 1.0694500, 0.8555600, 0.2555570, 0.3666690, 0.5583360,
  0.9166720, 0.5500030, 1.0291190, 0.8305610, 0.3055570, 0.4277800, 0.4277800,
  0.5500030, 0.8555600, 0.3055570, 0.3666690, 0.3055570, 0.5500030, 0.5500030,
  0.5500030, 0.5500030, 0.5500030, 0.5500030, 0.5500030, 0.5500030, 0.5500030,
  0.5500030, 0.5500030, 0.3055570, 0.3055570, 0.3666690, 0.8555600, 0.5194470,
  0.5194470, 0.7333370, 0.7333370, 0.7333370, 0.7027820, 0.7944490, 0.6416700,
  0.6111145, 0.7333370, 0.7944490, 0.3305570, 0.5194470, 0.7638930, 0.5805590,
  0.9777830, 0.7944490, 0.7944490, 0.7027820, 0.7944490, 0.7027820, 0.6111145,
  0.7333370, 0.7638930, 0.7333370, 1.0388950, 0.7333370, 0.7333370, 0.6722260,
  0.3430580, 0.5583360, 0.3430580, 0.5500030, 0.3055570, 0.3055570, 0.5250030,
  0.5611140, 0.4888920, 0.5611140, 0.5111140, 0.3361130, 0.5500030, 0.5611140,
  0.2555570, 0.2861130, 0.5305590, 0.2555570, 0.8666720, 0.5611140, 0.5500030,
  0.5611140, 0.5611140, 0.3722250, 0.4216690, 0.4041690, 0.5611140, 0.5000030,
  0.7444490, 0.5000030, 0.5000030, 0.4763920, 0.5500030, 1.1000060, 0.5500030,
  0.5500030, 0.550003
}
};

static const char * const fontname[] = {
    "cmss10",
    "cmssbx10",
    "cmssi10",
    "cmssxi10"
};


	/* Device driver actions */

static void PicTeX_Activate(NewDevDesc *dd);
static void PicTeX_Circle(double x, double y, double r,
			  R_GE_gcontext *gc,
			  NewDevDesc *dd);
static void PicTeX_Clip(double x0, double x1, double y0, double y1, 
			NewDevDesc *dd);
static void PicTeX_Close(NewDevDesc *dd);
static void PicTeX_Deactivate(NewDevDesc *dd);
static void PicTeX_Hold(NewDevDesc *dd);
static Rboolean PicTeX_Locator(double *x, double *y, NewDevDesc *dd);
static void PicTeX_Line(double x1, double y1, double x2, double y2,
			R_GE_gcontext *gc,
			NewDevDesc *dd);
static void PicTeX_MetricInfo(int c,
			      R_GE_gcontext *gc,
			      double* ascent, double* descent,
			      double* width, NewDevDesc *dd);
static void PicTeX_Mode(int mode, NewDevDesc *dd);
static void PicTeX_NewPage(R_GE_gcontext *gc, NewDevDesc *dd);
static void PicTeX_Polygon(int n, double *x, double *y, 
			   R_GE_gcontext *gc,
			   NewDevDesc *dd);
static void PicTeX_Rect(double x0, double y0, double x1, double y1,
			R_GE_gcontext *gc,
			NewDevDesc *dd);
static void PicTeX_Size(double *left, double *right,
			double *bottom, double *top,
			NewDevDesc *dd);
static double PicTeX_StrWidth(char *str, 
			      R_GE_gcontext *gc,
			      NewDevDesc *dd);
static void PicTeX_Text(double x, double y, char *str, 
			double rot, double hadj, 
			R_GE_gcontext *gc,
			NewDevDesc *dd);
static Rboolean PicTeX_Open(NewDevDesc*, picTeXDesc*);

	/* Support routines */

static void SetLinetype(int newlty, int newlwd, NewDevDesc *dd)
{
    picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

    int i, templty;
    ptd->lty = newlty;
    if (ptd->lty) {
	fprintf(ptd->texfp,"\\setdashpattern <");
	for(i=0 ; i<8 && newlty&15 ; i++) {
	    fprintf(ptd->texfp,"%dpt", newlwd * newlty&15);
	    templty = newlty>>4;
	    if ((i+1)<8 && templty&15) fprintf(ptd->texfp,", ");
	    newlty = newlty>>4;
	}
	fprintf(ptd->texfp,">\n");
    } else fprintf(ptd->texfp,"\\setsolid\n");
}


static void SetFont(int face, int size, picTeXDesc *ptd)
{
    int lface=face, lsize= size;
    if(lface < 1 || lface > 4 ) lface = 1;
    if(lsize < 1 || lsize > 24) lsize = 10;
    if(lsize != ptd->fontsize || lface != ptd->fontface) {
	fprintf(ptd->texfp, "\\font\\picfont %s at %dpt\\picfont\n",
		fontname[lface-1], lsize);
	ptd->fontsize = lsize;
	ptd->fontface = lface;
    }
}

static void PicTeX_Activate(NewDevDesc *dd)
{
}

static void PicTeX_Deactivate(NewDevDesc *dd)
{
}

static void PicTeX_MetricInfo(int c, 
			      R_GE_gcontext *gc,
			      double* ascent, double* descent,
			      double* width, NewDevDesc *dd)
{
    /* metric information not available => return 0,0,0 */
    *ascent = 0.0;
    *descent = 0.0;
    *width = 0.0;
}

	/* Initialize the device */

static Rboolean PicTeX_Open(NewDevDesc *dd, picTeXDesc *ptd)
{
    ptd->fontsize = 0;
    ptd->fontface = 0;
    ptd->debug = FALSE;
    if (!(ptd->texfp = R_fopen(R_ExpandFileName(ptd->filename), "w")))
	return FALSE;
    fprintf(ptd->texfp, "\\hbox{\\beginpicture\n");
    fprintf(ptd->texfp, "\\setcoordinatesystem units <1pt,1pt>\n");
    fprintf(ptd->texfp,
	    "\\setplotarea x from 0 to %.2f, y from 0 to %.2f\n",
	    in2dots(ptd->width), in2dots(ptd->height));
    fprintf(ptd->texfp,"\\setlinear\n");
    fprintf(ptd->texfp, "\\font\\picfont cmss10\\picfont\n");
    SetFont(1, 10, ptd);
    ptd->pageno++;
    return TRUE;
}


	/* Interactive Resize */

static void PicTeX_Size(double *left, double *right,
		     double *bottom, double *top,
		     NewDevDesc *dd)
{
    *left = dd->left;		/* left */
    *right = dd->right;/* right */
    *bottom = dd->bottom;		/* bottom */
    *top = dd->top;/* top */
}

static void PicTeX_Clip(double x0, double x1, double y0, double y1,
			NewDevDesc *dd)
{
    picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

    if(ptd->debug)
	fprintf(ptd->texfp, "%% Setting Clip Region to %.2f %.2f %.2f %.2f\n",
		x0, y0, x1, y1);
    ptd->clipleft = x0;
    ptd->clipright = x1;
    ptd->clipbottom = y0;
    ptd->cliptop = y1;
}

	/* Start a new page */

static void PicTeX_NewPage(R_GE_gcontext *gc,
			   NewDevDesc *dd)
{
    picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

    int face, size;
    if (ptd->pageno) {
	fprintf(ptd->texfp, "\\endpicture\n}\n\n\n");
	fprintf(ptd->texfp, "\\hbox{\\beginpicture\n");
	fprintf(ptd->texfp, "\\setcoordinatesystem units <1pt,1pt>\n");
	fprintf(ptd->texfp,
		"\\setplotarea x from 0 to %.2f, y from 0 to %.2f\n",
		in2dots(ptd->width), in2dots(ptd->height));
	fprintf(ptd->texfp,"\\setlinear\n");
	fprintf(ptd->texfp, "\\font\\picfont cmss10\\picfont\n");
    }
    ptd->pageno++;
    face = ptd->fontface;
    size = ptd->fontsize;
    ptd->fontface = 0;
    ptd->fontsize = 0;
    SetFont(face, size, ptd);
}

	/* Close down the driver */

static void PicTeX_Close(NewDevDesc *dd)
{
    picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

    fprintf(ptd->texfp, "\\endpicture\n}\n");
    fclose(ptd->texfp);

    free(ptd);
}

	/* Draw To */

static void PicTeX_ClipLine(double x0, double y0, double x1, double y1,
			    picTeXDesc *ptd)
{
    ptd->clippedx0 = x0; ptd->clippedx1 = x1;
    ptd->clippedy0 = y0; ptd->clippedy1 = y1;

    if ((ptd->clippedx0 < ptd->clipleft &&
	 ptd->clippedx1 < ptd->clipleft) ||
	(ptd->clippedx0 > ptd->clipright &&
	 ptd->clippedx1 > ptd->clipright) ||
	(ptd->clippedy0 < ptd->clipbottom &&
	 ptd->clippedy1 < ptd->clipbottom) ||
	(ptd->clippedy0 > ptd->cliptop &&
	 ptd->clippedy1 > ptd->cliptop)) {
	ptd->clippedx0 = ptd->clippedx1;
	ptd->clippedy0 = ptd->clippedy1;
	return;
    }

    /*Clipping Left */
    if (ptd->clippedx1 >= ptd->clipleft && ptd->clippedx0 < ptd->clipleft) {
	ptd->clippedy0 = ((ptd->clippedy1-ptd->clippedy0) /
			  (ptd->clippedx1-ptd->clippedx0) *
			  (ptd->clipleft-ptd->clippedx0)) +
	    ptd->clippedy0;
	ptd->clippedx0 = ptd->clipleft;
    }
    if (ptd->clippedx1 <= ptd->clipleft && ptd->clippedx0 > ptd->clipleft) {
	ptd->clippedy1 = ((ptd->clippedy1-ptd->clippedy0) /
			  (ptd->clippedx1-ptd->clippedx0) *
			  (ptd->clipleft-ptd->clippedx0)) +
	    ptd->clippedy0;
	ptd->clippedx1 = ptd->clipleft;
    }
    /* Clipping Right */
    if (ptd->clippedx1 >= ptd->clipright &&
	ptd->clippedx0 < ptd->clipright) {
	ptd->clippedy1 = ((ptd->clippedy1-ptd->clippedy0) /
			  (ptd->clippedx1-ptd->clippedx0) *
			  (ptd->clipright-ptd->clippedx0)) +
	    ptd->clippedy0;
	ptd->clippedx1 = ptd->clipright;
    }
    if (ptd->clippedx1 <= ptd->clipright &&
	ptd->clippedx0 > ptd->clipright) {
	ptd->clippedy0 = ((ptd->clippedy1-ptd->clippedy0) /
			  (ptd->clippedx1-ptd->clippedx0) *
			  (ptd->clipright-ptd->clippedx0)) +
	    ptd->clippedy0;
	ptd->clippedx0 = ptd->clipright;
    }
    /*Clipping Bottom */
    if (ptd->clippedy1 >= ptd->clipbottom  &&
	ptd->clippedy0 < ptd->clipbottom ) {
	ptd->clippedx0 = ((ptd->clippedx1-ptd->clippedx0) /
			  (ptd->clippedy1-ptd->clippedy0) *
			  (ptd->clipbottom -ptd->clippedy0)) +
	    ptd->clippedx0;
	ptd->clippedy0 = ptd->clipbottom ;
    }
    if (ptd->clippedy1 <= ptd->clipbottom &&
	ptd->clippedy0 > ptd->clipbottom ) {
	ptd->clippedx1 = ((ptd->clippedx1-ptd->clippedx0) /
			  (ptd->clippedy1-ptd->clippedy0) *
			  (ptd->clipbottom -ptd->clippedy0)) +
	    ptd->clippedx0;
	ptd->clippedy1 = ptd->clipbottom ;
    }
    /*Clipping Top */
    if (ptd->clippedy1 >= ptd->cliptop  && ptd->clippedy0 < ptd->cliptop ) {
	ptd->clippedx1 = ((ptd->clippedx1-ptd->clippedx0) /
			  (ptd->clippedy1-ptd->clippedy0) *
			  (ptd->cliptop -ptd->clippedy0)) +
	    ptd->clippedx0;
	ptd->clippedy1 = ptd->cliptop ;
    }
    if (ptd->clippedy1 <= ptd->cliptop && ptd->clippedy0 > ptd->cliptop ) {
	ptd->clippedx0 = ((ptd->clippedx1-ptd->clippedx0) /
			  (ptd->clippedy1-ptd->clippedy0) *
			  (ptd->cliptop -ptd->clippedy0)) +
	    ptd->clippedx0;
	ptd->clippedy0 = ptd->cliptop ;
    }
}

static void PicTeX_Line(double x1, double y1, double x2, double y2,
			R_GE_gcontext *gc,
			NewDevDesc *dd)
{
    picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

    if (x1 != x2 || y1 != y2) {
	SetLinetype(gc->lty, gc->lwd, dd);
	if(ptd->debug)
	    fprintf(ptd->texfp,
		    "%% Drawing line from %.2f, %.2f to %.2f, %.2f\n",
		    x1, y1, x2, y2);
	PicTeX_ClipLine(x1, y1, x2, y2, ptd);
	if (ptd->debug)
	    fprintf(ptd->texfp,
		    "%% Drawing clipped line from %.2f, %.2f to %.2f, %.2f\n",
		    ptd->clippedx0, ptd->clippedy0,
		    ptd->clippedx1, ptd->clippedy1);
	fprintf(ptd->texfp, "\\plot %.2f %.2f %.2f %.2f /\n",
		ptd->clippedx0, ptd->clippedy0,
		ptd->clippedx1, ptd->clippedy1);
    }
}

static void PicTeX_Polyline(int n, double *x, double *y, 
			    R_GE_gcontext *gc,
			    NewDevDesc *dd)
{
    double x1, y1, x2, y2;
    int i;
    picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

    SetLinetype(gc->lty, gc->lwd, dd);
    x1 = x[0];
    y1 = y[0];
    for (i = 1; i < n; i++) {
	x2 = x[i];
	y2 = y[i];
	PicTeX_ClipLine(x1, y1, x2, y2, ptd);
	fprintf(ptd->texfp, "\\plot %.2f %.2f %.2f %.2f /\n",
		ptd->clippedx0, ptd->clippedy0,
		ptd->clippedx1, ptd->clippedy1);
	x1 = x2;
	y1 = y2;
    }
}

	/* String Width in Rasters */
	/* For the current font in pointsize fontsize */

static double PicTeX_StrWidth(char *str, 
			      R_GE_gcontext *gc,
			      NewDevDesc *dd)
{
    picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

    char *p;
    int size;
    double sum;

    size = gc->cex * gc->ps + 0.5;
    SetFont(gc->fontface, size, ptd);
    sum = 0;
#if defined(SUPPORT_MBCS)
    if(mbcslocale && ptd->fontface != 5) {
	/* This version at least uses the state of the MBCS */
	int i, status, ucslen = mbcsToUcs2(str, NULL, 0);
	if (ucslen != (size_t)-1) {
	    ucs2_t *ucs;
	    ucs = (ucs2_t *) alloca(ucslen*sizeof(ucs2_t));
	    status = (int) mbcsToUcs2(str, ucs, ucslen);
	    if (status >= 0) 
		for (i = 0; i < ucslen; i++)
		    if(ucs[i] < 128) sum += charwidth[ptd->fontface-1][ucs[i]];
		    else sum += (double) Ri18n_wcwidth(ucs[i]) * 0.5; /* A guess */
	    else
		warning(_("invalid string in '%s'"), "PicTeX_StrWidth");
	} else
	    warning(_("invalid string in '%s'"), "PicTeX_StrWidth");
    } else
#endif
	for(p = str; *p; p++)
	    sum += charwidth[ptd->fontface-1][(int)*p];
    return sum * ptd->fontsize;
}


/* Possibly Filled Rectangle */
static void PicTeX_Rect(double x0, double y0, double x1, double y1,
			R_GE_gcontext *gc,
			NewDevDesc *dd)
{
    double x[4], y[4];

    x[0] = x0; y[0] = y0;
    x[1] = x0; y[1] = y1;
    x[2] = x1; y[2] = y1;
    x[3] = x1; y[3] = y0;
    PicTeX_Polygon(4, x, y, gc, dd);
}

static void PicTeX_Circle(double x, double y, double r,
			  R_GE_gcontext *gc,
			  NewDevDesc *dd)
{
    picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

    fprintf(ptd->texfp,
	    "\\circulararc 360 degrees from %.2f %.2f center at %.2f %.2f\n",
	    x, (y + r), x, y);
}

static void PicTeX_Polygon(int n, double *x, double *y, 
			   R_GE_gcontext *gc,
			   NewDevDesc *dd)
{
    double x1, y1, x2, y2;
    int i;
    picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

    SetLinetype(gc->lty, gc->lwd, dd);
    x1 = x[0];
    y1 = y[0];
    for (i=1; i<n; i++) {
	x2 = x[i];
	y2 = y[i];
	PicTeX_ClipLine(x1, y1, x2, y2, ptd);
	fprintf(ptd->texfp, "\\plot %.2f %.2f %.2f %.2f /\n",
		ptd->clippedx0, ptd->clippedy0,
		ptd->clippedx1, ptd->clippedy1);
	x1 = x2;
	y1 = y2;
    }
    x2 = x[0];
    y2 = y[0];
    PicTeX_ClipLine(x1, y1, x2, y2, ptd);
    fprintf(ptd->texfp, "\\plot %.2f %.2f %.2f %.2f /\n",
	    ptd->clippedx0, ptd->clippedy0,
	    ptd->clippedx1, ptd->clippedy1);
}

/* TeX Text Translations */
static void textext(char *str, picTeXDesc *ptd)
{
    fputc('{', ptd->texfp);
    for( ; *str ; str++)
	switch(*str) {
	case '$':
	    fprintf(ptd->texfp, "\\$");
	    break;

	case '%':
	    fprintf(ptd->texfp, "\\%%");
	    break;

	case '{':
	    fprintf(ptd->texfp, "\\{");
	    break;

	case '}':
	    fprintf(ptd->texfp, "\\}");
	    break;

	case '^':
	    fprintf(ptd->texfp, "\\^{}");
	    break;

	default:
	    fputc(*str, ptd->texfp);
	    break;
	}
    fprintf(ptd->texfp,"} ");
}

/* Rotated Text */

static void PicTeX_Text(double x, double y, char *str, 
			double rot, double hadj, 
			R_GE_gcontext *gc,
			NewDevDesc *dd)
{
    int size;
    double xoff = 0.0, yoff = 0.0;
    picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

    size = gc->cex * gc->ps + 0.5;
    SetFont(gc->fontface, size, ptd);
    if(ptd->debug) 
	fprintf(ptd->texfp,
		"%% Writing string of length %.2f, at %.2f %.2f, xc = %.2f yc = %.2f\n",
		(double)PicTeX_StrWidth(str, gc, dd), 
		x, y, 0.0, 0.0);
#if 0 /* Original */
    fprintf(ptd->texfp,"\\put ");
    textext(str, ptd);
    if (rot == 90 )
	fprintf(ptd->texfp," [rB] <%.2fpt,%.2fpt>", xoff, yoff);
    else fprintf(ptd->texfp," [lB] <%.2fpt,%.2fpt>", xoff, yoff);
#else /* use rotatebox */
    if (rot == 90 ){
	fprintf(ptd->texfp,"\\put {\\rotatebox{%d}",(int)rot);
	textext(str, ptd);
	fprintf(ptd->texfp,"} [rB] <%.2fpt,%.2fpt>", xoff, yoff);
    } else {
	fprintf(ptd->texfp,"\\put ");
	textext(str, ptd);
	fprintf(ptd->texfp," [lB] <%.2fpt,%.2fpt>", xoff, yoff);
    }
#endif
    fprintf(ptd->texfp," at %.2f %.2f\n", x, y);
}

/* Pick */
static Rboolean PicTeX_Locator(double *x, double *y, NewDevDesc *dd)
{
    return FALSE;
}


static void PicTeX_Mode(int mode, NewDevDesc* dd)
{
}

static void PicTeX_Hold(NewDevDesc *dd)
{
}

static
Rboolean PicTeXDeviceDriver(NewDevDesc *dd, char *filename, 
			    char *bg, char *fg,
			    double width, double height, 
			    Rboolean debug)
{
    picTeXDesc *ptd;

    if (!(ptd = (picTeXDesc *) malloc(sizeof(picTeXDesc))))
	return FALSE;

    strcpy(ptd->filename, filename);

    dd->startfill = str2col(bg);
    dd->startcol = str2col(fg);
    dd->startps = 10;
    dd->startlty = 0;
    dd->startfont = 1;
    dd->startgamma = 1;

    dd->newDevStruct = 1;
    
    dd->activate = PicTeX_Activate;
    dd->deactivate = PicTeX_Deactivate;
    dd->open = PicTeX_Open;
    dd->close = PicTeX_Close;
    dd->clip = PicTeX_Clip;
    dd->size = PicTeX_Size;
    dd->newPage = PicTeX_NewPage;
    dd->line = PicTeX_Line;
    dd->text = PicTeX_Text;
    dd->strWidth = PicTeX_StrWidth;
    dd->rect = PicTeX_Rect;
    dd->circle = PicTeX_Circle;
    dd->polygon = PicTeX_Polygon;
    dd->polyline = PicTeX_Polyline;
    dd->locator = PicTeX_Locator;
    dd->mode = PicTeX_Mode;
    dd->hold = PicTeX_Hold;
    dd->metricInfo = PicTeX_MetricInfo;

    /* Screen Dimensions in Pixels */

    dd->left = 0;		/* left */
    dd->right = in2dots(width);/* right */
    dd->bottom = 0;		/* bottom */
    dd->top = in2dots(height);/* top */
    ptd->width = width;
    ptd->height = height;

    if( ! PicTeX_Open(dd, ptd) ) 
	return FALSE;

    /* Base Pointsize */
    /* Nominal Character Sizes in Pixels */

    dd->cra[0] =	 (6.0/12.0) * 10.0;
    dd->cra[1] =	(10.0/12.0) * 10.0;

    /* Character Addressing Offsets */
    /* These offsets should center a single */
    /* plotting character over the plotting point. */
    /* Pure guesswork and eyeballing ... */

    dd->xCharOffset =  0; /*0.4900;*/
    dd->yCharOffset =  0; /*0.3333;*/
    dd->yLineBias = 0; /*0.1;*/

    /* Inches per Raster Unit */
    /* We use printer points, i.e. 72.27 dots per inch : */
    dd->ipr[0] = dd->ipr[1] = 1./DOTSperIN;

    dd->canResizePlot = FALSE;
    dd->canChangeFont = TRUE;
    dd->canRotateText = FALSE;
    dd->canResizeText = TRUE;
    dd->canClip = TRUE;
    dd->canHAdj = 0;
    dd->canChangeGamma = FALSE;

    ptd->lty = 1;
    ptd->pageno = 0;
    ptd->debug = debug;

    dd->deviceSpecific = (void *) ptd;
    dd->displayListOn = FALSE;
    return TRUE;
}

/*  PicTeX Device Driver Parameters
 *  --------------------
 *  file    = output filename
 *  bg	    = background color
 *  fg	    = foreground color
 *  width   = width in inches
 *  height  = height in inches
 *  debug   = Rboolean; if TRUE, write TeX-Comments into output.
 */

SEXP PicTeX(SEXP args)
{
    NewDevDesc *dev;
    GEDevDesc *dd;
    char *vmax;
    char *file, *bg, *fg;
    double height, width;
    Rboolean debug;

    vmax = vmaxget();
    args = CDR(args); /* skip entry point name */
    file = translateChar(asChar(CAR(args))); args = CDR(args);
    bg = CHAR(asChar(CAR(args)));   args = CDR(args);
    fg = CHAR(asChar(CAR(args)));   args = CDR(args);
    width = asReal(CAR(args));	     args = CDR(args);
    height = asReal(CAR(args));	     args = CDR(args);
    debug = asLogical(CAR(args));    args = CDR(args);
    if(debug == NA_LOGICAL) debug = FALSE;

    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	if (!(dev = (NewDevDesc *) calloc(1,sizeof(NewDevDesc))))
	    return 0;
	/* Do this for early redraw attempts */
	dev->displayList = R_NilValue;
	/* Make sure that this is initialised before a GC can occur.
	 * This (and displayList) get protected during GC
	 */
	dev->savedSnapshot = R_NilValue;
	if(!PicTeXDeviceDriver(dev, file, bg, fg, width, height, debug)) {
	    free(dev);
	    error(_("unable to start device PicTeX"));
	}
	gsetVar(install(".Device"), mkString("pictex"), R_BaseEnv);
	dd = GEcreateDevDesc(dev);
	addDevice((DevDesc*) dd);
	GEinitDisplayList(dd);
    } END_SUSPEND_INTERRUPTS;
    vmaxset(vmax);
    return R_NilValue;
}
