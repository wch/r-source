/*
 *  A PicTeX device, (C) 1996 Valerio Aimale, for
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Graphics.h"
#include "Fileio.h"

	/* device-specific information per picTeX device */

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
	rcolor fg;
	rcolor bg;
	int fontsize;
	int fontface;
	int debug;
} picTeXDesc;


	/* Global device information */

static double charwidth[4][128] = {
{
  0.5416690, 0.8333360, 0.7777810, 0.6111145, 0.6666690, 0.7083380,
0.7222240,
  0.7777810, 0.7222240, 0.7777810, 0.7222240, 0.5833360, 0.5361130,
0.5361130,
  0.8138910, 0.8138910, 0.2388900, 0.2666680, 0.5000020, 0.5000020,
0.5000020,
  0.5000020, 0.5000020, 0.6666700, 0.4444460, 0.4805580, 0.7222240,
0.7777810,
  0.5000020, 0.8611145, 0.9722260, 0.7777810, 0.2388900, 0.3194460,
0.5000020,
  0.8333360, 0.5000020, 0.8333360, 0.7583360, 0.2777790, 0.3888900,
0.3888900,
  0.5000020, 0.7777810, 0.2777790, 0.3333340, 0.2777790, 0.5000020,
0.5000020,
  0.5000020, 0.5000020, 0.5000020, 0.5000020, 0.5000020, 0.5000020,
0.5000020,
  0.5000020, 0.5000020, 0.2777790, 0.2777790, 0.3194460, 0.7777810,
0.4722240,
  0.4722240, 0.6666690, 0.6666700, 0.6666700, 0.6388910, 0.7222260,
0.5972240,
  0.5694475, 0.6666690, 0.7083380, 0.2777810, 0.4722240, 0.6944480,
0.5416690,
  0.8750050, 0.7083380, 0.7361130, 0.6388910, 0.7361130, 0.6458360,
0.5555570,
  0.6805570, 0.6875050, 0.6666700, 0.9444480, 0.6666700, 0.6666700,
0.6111130,
  0.2888900, 0.5000020, 0.2888900, 0.5000020, 0.2777790, 0.2777790,
0.4805570,
  0.5166680, 0.4444460, 0.5166680, 0.4444460, 0.3055570, 0.5000020,
0.5166680,
  0.2388900, 0.2666680, 0.4888920, 0.2388900, 0.7944470, 0.5166680,
0.5000020,
  0.5166680, 0.5166680, 0.3416690, 0.3833340, 0.3611120, 0.5166680,
0.4611130,
  0.6833360, 0.4611130, 0.4611130, 0.4347230, 0.5000020, 1.0000030,
0.5000020,
  0.5000020, 0.5000020
},
{
  0.5805590, 0.9166720, 0.8555600, 0.6722260, 0.7333370, 0.7944490,
0.7944490,
  0.8555600, 0.7944490, 0.8555600, 0.7944490, 0.6416700, 0.5861150,
0.5861150,
  0.8916720, 0.8916720, 0.2555570, 0.2861130, 0.5500030, 0.5500030,
0.5500030,
  0.5500030, 0.5500030, 0.7333370, 0.4888920, 0.5652800, 0.7944490,
0.8555600,
  0.5500030, 0.9472275, 1.0694500, 0.8555600, 0.2555570, 0.3666690,
0.5583360,
  0.9166720, 0.5500030, 1.0291190, 0.8305610, 0.3055570, 0.4277800,
0.4277800,
  0.5500030, 0.8555600, 0.3055570, 0.3666690, 0.3055570, 0.5500030,
0.5500030,
  0.5500030, 0.5500030, 0.5500030, 0.5500030, 0.5500030, 0.5500030,
0.5500030,
  0.5500030, 0.5500030, 0.3055570, 0.3055570, 0.3666690, 0.8555600,
0.5194470,
  0.5194470, 0.7333370, 0.7333370, 0.7333370, 0.7027820, 0.7944490,
0.6416700,
  0.6111145, 0.7333370, 0.7944490, 0.3305570, 0.5194470, 0.7638930,
0.5805590,
  0.9777830, 0.7944490, 0.7944490, 0.7027820, 0.7944490, 0.7027820,
0.6111145,
  0.7333370, 0.7638930, 0.7333370, 1.0388950, 0.7333370, 0.7333370,
0.6722260,
  0.3430580, 0.5583360, 0.3430580, 0.5500030, 0.3055570, 0.3055570,
0.5250030,
  0.5611140, 0.4888920, 0.5611140, 0.5111140, 0.3361130, 0.5500030,
0.5611140,
  0.2555570, 0.2861130, 0.5305590, 0.2555570, 0.8666720, 0.5611140,
0.5500030,
  0.5611140, 0.5611140, 0.3722250, 0.4216690, 0.4041690, 0.5611140,
0.5000030,
  0.7444490, 0.5000030, 0.5000030, 0.4763920, 0.5500030, 1.1000060,
0.5500030,
  0.5500030, 0.550003
},
{
  0.5416690, 0.8333360, 0.7777810, 0.6111145, 0.6666690, 0.7083380,
0.7222240,
  0.7777810, 0.7222240, 0.7777810, 0.7222240, 0.5833360, 0.5361130,
0.5361130,
  0.8138910, 0.8138910, 0.2388900, 0.2666680, 0.5000020, 0.5000020,
0.5000020,
  0.5000020, 0.5000020, 0.7375210, 0.4444460, 0.4805580, 0.7222240,
0.7777810,
  0.5000020, 0.8611145, 0.9722260, 0.7777810, 0.2388900, 0.3194460,
0.5000020,
  0.8333360, 0.5000020, 0.8333360, 0.7583360, 0.2777790, 0.3888900,
0.3888900,
  0.5000020, 0.7777810, 0.2777790, 0.3333340, 0.2777790, 0.5000020,
0.5000020,
  0.5000020, 0.5000020, 0.5000020, 0.5000020, 0.5000020, 0.5000020,
0.5000020,
  0.5000020, 0.5000020, 0.2777790, 0.2777790, 0.3194460, 0.7777810,
0.4722240,
  0.4722240, 0.6666690, 0.6666700, 0.6666700, 0.6388910, 0.7222260,
0.5972240,
  0.5694475, 0.6666690, 0.7083380, 0.2777810, 0.4722240, 0.6944480,
0.5416690,
  0.8750050, 0.7083380, 0.7361130, 0.6388910, 0.7361130, 0.6458360,
0.5555570,
  0.6805570, 0.6875050, 0.6666700, 0.9444480, 0.6666700, 0.6666700,
0.6111130,
  0.2888900, 0.5000020, 0.2888900, 0.5000020, 0.2777790, 0.2777790,
0.4805570,
  0.5166680, 0.4444460, 0.5166680, 0.4444460, 0.3055570, 0.5000020,
0.5166680,
  0.2388900, 0.2666680, 0.4888920, 0.2388900, 0.7944470, 0.5166680,
0.5000020,
  0.5166680, 0.5166680, 0.3416690, 0.3833340, 0.3611120, 0.5166680,
0.4611130,
  0.6833360, 0.4611130, 0.4611130, 0.4347230, 0.5000020, 1.0000030,
0.5000020,
  0.5000020, 0.5000020
},
{
  0.5805590, 0.9166720, 0.8555600, 0.6722260, 0.7333370, 0.7944490,
0.7944490,
  0.8555600, 0.7944490, 0.8555600, 0.7944490, 0.6416700, 0.5861150,
0.5861150,
  0.8916720, 0.8916720, 0.2555570, 0.2861130, 0.5500030, 0.5500030,
0.5500030,
  0.5500030, 0.5500030, 0.8002530, 0.4888920, 0.5652800, 0.7944490,
0.8555600,
  0.5500030, 0.9472275, 1.0694500, 0.8555600, 0.2555570, 0.3666690,
0.5583360,
  0.9166720, 0.5500030, 1.0291190, 0.8305610, 0.3055570, 0.4277800,
0.4277800,
  0.5500030, 0.8555600, 0.3055570, 0.3666690, 0.3055570, 0.5500030,
0.5500030,
  0.5500030, 0.5500030, 0.5500030, 0.5500030, 0.5500030, 0.5500030,
0.5500030,
  0.5500030, 0.5500030, 0.3055570, 0.3055570, 0.3666690, 0.8555600,
0.5194470,
  0.5194470, 0.7333370, 0.7333370, 0.7333370, 0.7027820, 0.7944490,
0.6416700,
  0.6111145, 0.7333370, 0.7944490, 0.3305570, 0.5194470, 0.7638930,
0.5805590,
  0.9777830, 0.7944490, 0.7944490, 0.7027820, 0.7944490, 0.7027820,
0.6111145,
  0.7333370, 0.7638930, 0.7333370, 1.0388950, 0.7333370, 0.7333370,
0.6722260,
  0.3430580, 0.5583360, 0.3430580, 0.5500030, 0.3055570, 0.3055570,
0.5250030,
  0.5611140, 0.4888920, 0.5611140, 0.5111140, 0.3361130, 0.5500030,
0.5611140,
  0.2555570, 0.2861130, 0.5305590, 0.2555570, 0.8666720, 0.5611140,
0.5500030,
  0.5611140, 0.5611140, 0.3722250, 0.4216690, 0.4041690, 0.5611140,
0.5000030,
  0.7444490, 0.5000030, 0.5000030, 0.4763920, 0.5500030, 1.1000060,
0.5500030,
  0.5500030, 0.550003
}
};

static char *fontname[] = {
	"cmss10",
	"cmssbx10",
	"cmssi10",
	"cmssxi10"
};


	/* Device driver actions */

static void   PicTeX_Activate(DevDesc *);
static void   PicTeX_Circle(double, double, int, double, int, int, DevDesc*);
static void   PicTeX_Clip(double, double, double, double, DevDesc*);
static void   PicTeX_Close(DevDesc*);
static void   PicTeX_Deactivate(DevDesc *);
static void   PicTeX_Hold(DevDesc*);
static void   PicTeX_Line(double, double, double, double, int, DevDesc*);
static int    PicTeX_Locator(double*, double*, DevDesc*);
static void   PicTeX_Mode(int, DevDesc*);
static void   PicTeX_NewPage(DevDesc*);
static int    PicTeX_Open(DevDesc*, picTeXDesc*);
static void   PicTeX_Polygon(int, double*, double*, int, int, int, DevDesc*);
static void   PicTeX_Polyline(int, double*, double*, int, DevDesc*);
static void   PicTeX_Rect(double, double, double, double, int, int, int,
			  DevDesc*);
static void   PicTeX_Resize(DevDesc*);
static double PicTeX_StrWidth(char*, DevDesc*);
static void   PicTeX_Text(double, double, int, char*, double, double, double,
			  DevDesc*);
static void   PicTeX_MetricInfo(int, double*, double*, double*, DevDesc*);


	/* Support routines */

static void SetLinetype(int newlty, int newlwd, DevDesc *dd)
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
	if(lface < 1 || lface > 4) lface = 1;
	if(lsize < 1 || lsize > 24) lsize = 10;
	if(lsize != ptd->fontsize || lface != ptd->fontface) {
		fprintf(ptd->texfp, "\\font\\picfont %s at %dpt\\picfont\n",
			fontname[lface-1], lsize);
		ptd->fontsize = lsize;
		ptd->fontface = lface;
	}
}

static void PicTeX_Activate(DevDesc *dd)
{
}

static void PicTeX_Deactivate(DevDesc *dd)
{
}

static void PicTeX_MetricInfo(int c, double *accent, double *descent,
			      double *width, DevDesc *dd)
{
	error("Metric information not yet available for this device");
}

	/* Initialize the device */

static int PicTeX_Open(DevDesc *dd, picTeXDesc *ptd)
{
    ptd->fontsize = 0;
    ptd->fontface = 0;
    ptd->debug = 0;
    if (!(ptd->texfp = R_fopen(R_ExpandFileName(ptd->filename), "w")))
	return 0;
    fprintf(ptd->texfp, "\\hbox{\\beginpicture\n");
    fprintf(ptd->texfp, "\\setcoordinatesystem units <1pt,1pt>\n");
    fprintf(ptd->texfp,
	    "\\setplotarea x from 0 to %.2f, y from 0 to %.2f\n",
	    ptd->width * 72.27, ptd->height * 72.27);
    fprintf(ptd->texfp,"\\setlinear\n");
    fprintf(ptd->texfp, "\\font\\picfont cmss10\\picfont\n");
    SetFont(1, 10, ptd);
    ptd->pageno += 1;
    return 1;
}


	/* Interactive Resize */

static void PicTeX_Resize(DevDesc *dd)
{
}

static void PicTeX_Clip(double x0, double x1, double y0, double y1,
			DevDesc *dd)
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

static void PicTeX_NewPage(DevDesc *dd)
{
	picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

	int face, size;
	if (ptd->pageno) {
		fprintf(ptd->texfp, "\\endpicture\n}\n\n\n");
		fprintf(ptd->texfp, "\\hbox{\\beginpicture\n");
		fprintf(ptd->texfp, "\\setcoordinatesystem units <1pt,1pt>\n");
		fprintf(ptd->texfp,
			"\\setplotarea x from 0 to %.2f, y from 0 to %.2f\n",
			ptd->width * 72.27, ptd->height * 72.27);
		fprintf(ptd->texfp,"\\setlinear\n");
		fprintf(ptd->texfp, "\\font\\picfont cmss10\\picfont\n");
	}
	ptd->pageno +=1;
	face = ptd->fontface;
	size = ptd->fontsize;
	ptd->fontface = 0;
	ptd->fontsize = 0;
	SetFont(face, size, ptd);
}

	/* Close down the driver */

static void PicTeX_Close(DevDesc *dd)
{
	picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

	fprintf(ptd->texfp, "\\endpicture\n}\n");
	fclose(ptd->texfp);

	free(ptd);
}


	/* Seek */
/* NO LONGER USED
static void PicTeX_MoveTo(double x, double y)
{
	xlast = x;
	ylast = y;
}
*/

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
			int coords, DevDesc *dd)
{
	picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

	if (x1 != x2 || y1 != y2) {
	SetLinetype(dd->gp.lty, dd->gp.lwd, dd);
	GConvert(&x1, &y1, coords, DEVICE, dd);
	GConvert(&x2, &y2, coords, DEVICE, dd);
	if(ptd->debug)
		fprintf(ptd->texfp,
			"%% Drawing line from %.2f, %.2f to %.2f, %.2f\n",
			x1, y1, x2, y2);
	PicTeX_ClipLine(x1, y1, x2, y2, ptd);
	if (ptd->debug)
		fprintf(ptd->texfp,
			"%% Drawing cliped ine from %.2f, %.2f to %.2f, %.2f\n",
			ptd->clippedx0, ptd->clippedy0,
			ptd->clippedx1, ptd->clippedy1);
	fprintf(ptd->texfp, "\\plot %.2f %.2f %.2f %.2f /\n",
		ptd->clippedx0, ptd->clippedy0,
		ptd->clippedx1, ptd->clippedy1);
	}
}

static void PicTeX_Polyline(int n, double *x, double *y, int coords,
			    DevDesc* dd)
{
	double x1, y1, x2, y2;
	int i;
	picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

	SetLinetype(dd->gp.lty, dd->gp.lwd, dd);
	x1 = x[0];
	y1 = y[0];
	GConvert(&x1, &y1, coords, DEVICE, dd);
	for (i=1; i<n; i++) {
		x2 = x[i];
		y2 = y[i];
		GConvert(&x2, &y2, coords, DEVICE, dd);
		PicTeX_ClipLine(x1, y1, x2, y2, ptd);
		fprintf(ptd->texfp, "\\plot %.2f %.2f %.2f %.2f /\n",
			ptd->clippedx0, ptd->clippedy0,
			ptd->clippedx1, ptd->clippedy1);
	}
}

	/* String Width in Rasters */
	/* For the current font in pointsize fontsize */

static double PicTeX_StrWidth(char *str, DevDesc *dd)
{
	picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

	char *p;
	int size;
	double sum;
	size = dd->gp.cex * dd->gp.ps + 0.5;
	SetFont(dd->gp.font, size, ptd);
	sum = 0;
	for(p=str ; *p ; p++)
		sum += charwidth[ptd->fontface-1][(int)*p];
	return sum * ptd->fontsize;
}




	/* Start a Path */
/* NO LONGER USED
	static void PicTeX_StartPath()
	{
		SetLinetype(GP->lty, dd->gp.lwd, dd);
	}
*/

	/* End a Path */
/* NO LONGER USED
	static void PicTeX_EndPath()
	{
	}
*/

/* Possibly Filled Rectangle */
static void PicTeX_Rect(double x0, double y0, double x1, double y1,
			int coords, int bg, int fg, DevDesc *dd)
{
	double x[4], y[4];
	x[0] = x0; y[0] = y0;
	x[1] = x0; y[1] = y1;
	x[2] = x1; y[2] = y1;
	x[3] = x1; y[3] = y0;
	PicTeX_Polygon(4, x, y, coords, bg, fg, dd);
}

static void PicTeX_Circle(double x, double y, int coords, double r,
			  int col, int border, DevDesc *dd)
{
	picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

	GConvert(&x, &y, coords, DEVICE, dd);
	fprintf(ptd->texfp,
	"\\circulararc 360 degrees from %.2f %.2f center at %.2f %.2f\n",
			x, (y + r), x, y);

}

static void PicTeX_Polygon(int n, double *x, double *y, int coords,
			   int bg, int fg, DevDesc* dd)
{
	double x1, y1, x2, y2;
	int i;
	picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

	SetLinetype(dd->gp.lty, dd->gp.lwd, dd);
	x1 = x[0];
	y1 = y[0];
	GConvert(&x1, &y1, coords, DEVICE, dd);
	for (i=1; i<n; i++) {
		x2 = x[i];
		y2 = y[i];
		GConvert(&x2, &y2, coords, DEVICE, dd);
		PicTeX_ClipLine(x1, y1, x2, y2, ptd);
		fprintf(ptd->texfp, "\\plot %.2f %.2f %.2f %.2f /\n",
			ptd->clippedx0, ptd->clippedy0,
			ptd->clippedx1, ptd->clippedy1);
		x1 = x2;
		y1 = y2;
	}
	x2 = x[0];
	y2 = y[0];
	GConvert(&x2, &y2, coords, DEVICE, dd);
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

static void PicTeX_Text(double x, double y, int coords,
			char *str, double xc, double yc, double rot,
			DevDesc *dd)
{
	int size;
	double xoff = 0.0, yoff = 0.0, xl, yl, xctemp;
	picTeXDesc *ptd = (picTeXDesc *) dd->deviceSpecific;

	size = dd->gp.cex * dd->gp.ps + 0.5;
	SetFont(dd->gp.font, size, ptd);
	GConvert(&x, &y, coords, DEVICE, dd);
	if(ptd->debug) fprintf(ptd->texfp,
	"%% Writing string of length %.2f, at %.2f %.2f, xc = %.2f yc = %.2f\n",
	(double)PicTeX_StrWidth(str, dd), x, y, xc, yc);
	if (ptd->debug) fprintf(ptd->texfp,
	"%% Writing string of length %.2f, at %.2f %.2f, xc = %.2f yc = %.2f\n",
	(double)PicTeX_StrWidth(str, dd), x, y, xc, yc);
	if(xc != 0.0 || yc != 0.0) {
		if (rot == 90 || rot == 270) {
			xctemp = xc; xc = yc;
			yc = xctemp;
			yc = 1.0 - yc;
		}
		xl = PicTeX_StrWidth(str, dd);
		yl = GConvertXUnits(1, CHARS, DEVICE, dd);
			/* yl = GP->cex * GP->cra[0]; */
		xoff = -xc * xl;
		yoff = -yc * yl;

	}

	fprintf(ptd->texfp,"\\put ");
	textext(str, ptd);
	if (rot == 90 )
	fprintf(ptd->texfp," [rB] <%.2fpt,%.2fpt>", xoff, yoff);
	else fprintf(ptd->texfp," [lB] <%.2fpt,%.2fpt>", xoff, yoff);
	fprintf(ptd->texfp," at %.2f %.2f\n", x, y);
}

/* Pick */
static int PicTeX_Locator(double *x, double *y, DevDesc *dd)
{
	return 0;
}


/* Set Graphics mode - not needed for PS */
static void PicTeX_Mode(int mode, DevDesc* dd)
{
}

/* GraphicsInteraction() for the Mac */
static void PicTeX_Hold(DevDesc *dd)
{
}

int PicTeXDeviceDriver(DevDesc *dd, char *filename, char *bg, char *fg,
		       double width, double height, int debug)
{
	picTeXDesc *ptd;

	if (!(ptd = (picTeXDesc *) malloc(sizeof(picTeXDesc))))
		return 0;

	strcpy(ptd->filename, filename);

	dd->dp.bg = dd->gp.bg = str2col(bg);
	dd->dp.fg = dd->gp.fg = str2col(fg);

	dd->dp.activate = PicTeX_Activate;
	dd->dp.deactivate = PicTeX_Deactivate;
	dd->dp.open = PicTeX_Open;
	dd->dp.close = PicTeX_Close;
	dd->dp.clip = PicTeX_Clip;
	dd->dp.resize = PicTeX_Resize;
	dd->dp.newPage = PicTeX_NewPage;
	dd->dp.line = PicTeX_Line;
	dd->dp.text = PicTeX_Text;
	dd->dp.strWidth = PicTeX_StrWidth;
	dd->dp.rect = PicTeX_Rect;
	dd->dp.circle = PicTeX_Circle;
	dd->dp.polygon = PicTeX_Polygon;
	dd->dp.polyline = PicTeX_Polyline;
	dd->dp.locator = PicTeX_Locator;
	dd->dp.mode = PicTeX_Mode;
	dd->dp.hold = PicTeX_Hold;

/*	dd->dp.metricInfo = PicTeX_MetricInfo;
 */
	dd->dp.metricInfo = NULL;

	/* Screen Dimensions in Pixels */

	dd->dp.left = 0;		/* left */
	dd->dp.right = 72.27 * width;	/* right */
	dd->dp.bottom = 0;		/* bottom */
	dd->dp.top = 72.27 * height;	/* top */

	if( ! PicTeX_Open(dd, ptd) ) return 0;

		/* Base Pointsize */
		/* Nominal Character Sizes in Pixels */

	dd->dp.ps = 10;
	dd->dp.cra[0] =	 (6.0/12.0) * 10.0;
	dd->dp.cra[1] =	 (10.0/12.0) * 10.0;

		/* Character Addressing Offsets */
		/* These offsets should center a single */
		/* plotting character over the plotting point. */
		/* Pure guesswork and eyeballing ... */

	dd->dp.xCharOffset =  0; /*0.4900;*/
	dd->dp.yCharOffset =  0; /*0.3333;*/
	dd->dp.yLineBias = 0; /*0.1;*/

		/* Inches per Raster Unit */
		/* We use printer points */
		/* I.e. 72.27 dots per inch */

	dd->dp.ipr[0] = 1.0/72.27;
	dd->dp.ipr[1] = 1.0/72.27;

	dd->dp.canResizePlot = 0;
	dd->dp.canChangeFont = 1;
	dd->dp.canRotateText = 0;
	dd->dp.canResizeText = 1;
	dd->dp.canClip = 1;

	ptd->lty = 1;
	ptd->pageno = 0;
	ptd->debug = debug;

	dd->deviceSpecific = (void *) ptd;

	return 1;
}
