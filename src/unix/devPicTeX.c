/* 
 *  A PicTeX device, (C) 1996 Valerio Aimale, for 
 *  R : A Computer Langage for Statistical Data Analysis
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 *
 */

#include "Graphics.h"
#include "Fileio.h"

/*void error(char*);*/
int ValidColor(unsigned int);

static char *filename;
static int pageno;
static int landscape;
static double width;
static double height;
static double pagewidth;
static double pageheight;
static double xlast;
static double ylast;
static double clipleft, clipright, cliptop, clipbottom;
static double clippedx0, clippedy0, clippedx1, clippedy1;
static int lty;
static rcolor col;
static rcolor fg;
static rcolor bg;
static int fontsize = 0;
static int fontface = 0;
static int debug = 0;

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


static FILE *texfp;


static void SetLinetype(int newlty)
{
	int i, templty;
	lty = newlty;
	if (lty) {
		fprintf(texfp,"\\setdashpattern <");
		for(i=0 ; i<8 && newlty&15 ; i++) {
			fprintf(texfp,"%dpt", newlty&15);
			templty = newlty>>4;
			if ((i+1)<8 && templty&15) fprintf(texfp,", ");
			newlty = newlty>>4;
		}
		fprintf(texfp,">\n");
	} else fprintf(texfp,"\\setsolid\n");
}



static char *fontname[] = {
	"cmss10",
	"cmssbx10",
	"cmssi10",
	"cmssxi10"
};


static void SetFont(int face, int size)
{
	int lface=face, lsize= size;
	if(lface < 1 || lface > 4) lface = 1;
	if(lsize < 1 || lsize > 24) lsize = 10;
	if(lsize != fontsize || lface != fontface) {
		fprintf(texfp, "\\font\\picfont %s at %dpt\\picfont\n",
			fontname[lface-1], lsize);
		fontsize = lsize;
		fontface = lface;
	}
}

	/* Initialize the device */

static int PicTeX_Open(void)
{
	int i;
	fontsize = 0;
	fontface = 0;
	if(!(texfp = R_fopen(filename, "w"))) return 0;
	fprintf(texfp, "\\hbox{\\beginpicture\n");
	fprintf(texfp, "\\setcoordinatesystem units <1pt,1pt>\n");
	fprintf(texfp, "\\setplotarea x from 0 to %.2f, y from 0 to %.2f\n",
		width * 72.27, height * 72.27);
	fprintf(texfp,"\\setlinear\n");
	fprintf(texfp, "\\font\\picfont cmss10\\picfont\n");
	SetFont(1, 10);
	pageno += 1;
	return 1;
}



	/* Interactive Resize */

static void PicTeX_Resize()
{
}

static void PicTeX_Clip(double x0, double x1, double y0, double y1)
{
	if(debug)
	fprintf(texfp, "%% Setting Clip Region to %.2f %.2f %.2f %.2f\n", 
		x0, y0, x1, y1);
	clipleft = x0;
	clipright = x1;
	clipbottom = y0;
	cliptop = y1;
}

	/* Start a new page */

static void PicTeX_NewPlot()
{
	int face, size;
	if (pageno) {
		fprintf(texfp, "\\endpicture\n}\n\n\n"); 
		fprintf(texfp, "\\hbox{\\beginpicture\n");
		fprintf(texfp, "\\setcoordinatesystem units <1pt,1pt>\n");
		fprintf(texfp, 
			"\\setplotarea x from 0 to %.2f, y from 0 to %.2f\n",
			width * 72.27, height * 72.27);
		fprintf(texfp,"\\setlinear\n");
		fprintf(texfp, "\\font\\picfont cmss10\\picfont\n");
	}
	pageno +=1;
	face = fontface;
	size = fontsize;
	fontface = 0;
	fontsize = 0;
	SetFont(face, size);
}

	/* Close down the driver */

static void PicTeX_Close(void)
{
	fprintf(texfp, "\\endpicture\n}\n");
	fclose(texfp);
}


	/* Seek */

static void PicTeX_MoveTo(double x, double y)
{
	xlast = x;
	ylast = y;
}

	/* Draw To */

static void PicTeX_ClipLine(double x0, double y0, double x1, double y1)
{
	clippedx0 = x0; clippedx1 = x1;
	clippedy0 = y0; clippedy1 = y1;

	if ((clippedx0 < clipleft && clippedx1 < clipleft) || 
		(clippedx0 > clipright && clippedx1 > clipright) || 
		(clippedy0 < clipbottom && clippedy1 < clipbottom) || 
		(clippedy0 > cliptop && clippedy1 > cliptop)) {
	clippedx0 = clippedx1;
	clippedy0 = clippedy1;
	return;
	}


	/*Clipping Left */
	if (clippedx1 >= clipleft && clippedx0 < clipleft) {
		clippedy0 = ((clippedy1-clippedy0) / (clippedx1-clippedx0)
			* (clipleft-clippedx0)) + clippedy0; 
		clippedx0 = clipleft;
	}
	if (clippedx1 <= clipleft && clippedx0 > clipleft) {
		clippedy1 = ((clippedy1-clippedy0) / (clippedx1-clippedx0)
			* (clipleft-clippedx0)) + clippedy0; 
		clippedx1 = clipleft;
	}	
	/* Clipping Right */
	if (clippedx1 >= clipright && clippedx0 < clipright) {
		clippedy1 = ((clippedy1-clippedy0) / (clippedx1-clippedx0)
			* (clipright-clippedx0)) + clippedy0; 
		clippedx1 = clipright;
	}
	if (clippedx1 <= clipright && clippedx0 > clipright) {
		clippedy0 = ((clippedy1-clippedy0) / (clippedx1-clippedx0)
			* (clipright-clippedx0)) + clippedy0; 
		clippedx0 = clipright;	
	}
	/*Clipping Bottom */
	if (clippedy1 >= clipbottom  && clippedy0 < clipbottom ) {
		clippedx0 = ((clippedx1-clippedx0) / (clippedy1-clippedy0)
			* (clipbottom -clippedy0)) + clippedx0; 
		clippedy0 = clipbottom ;
	}
	if (clippedy1 <= clipbottom && clippedy0 > clipbottom ) {
		clippedx1 = ((clippedx1-clippedx0) / (clippedy1-clippedy0)
			* (clipbottom -clippedy0)) + clippedx0; 
		clippedy1 = clipbottom ;
	}
	/*Clipping Top */
	if (clippedy1 >= cliptop  && clippedy0 < cliptop ) {
		clippedx1 = ((clippedx1-clippedx0) / (clippedy1-clippedy0)
			* (cliptop -clippedy0)) + clippedx0; 
		clippedy1 = cliptop ;
	}
	if (clippedy1 <= cliptop && clippedy0 > cliptop ) {
		clippedx0 = ((clippedx1-clippedx0) / (clippedy1-clippedy0)
			* (cliptop -clippedy0)) + clippedx0; 
		clippedy0 = cliptop ;
	}
}

static void PicTeX_LineTo(double x, double y)
{
	if (xlast != x || ylast != y) {
	if(debug) 
	fprintf(texfp,"%% Drawing line from %.2f, %.2f to %.2f, %.2f\n",
		xlast, ylast, x, y);
	PicTeX_ClipLine(xlast, ylast, x, y);
	if (debug)
	fprintf(texfp,"%% Drawing clipped ine from %.2f, %.2f to %.2f, %.2f\n",
		clippedx0, clippedy0,
		clippedx1, clippedy1);
	fprintf(texfp, "\\plot %.2f %.2f %.2f %.2f /\n",
		clippedx0, clippedy0,
		clippedx1, clippedy1);
	xlast = x;
	ylast = y;
	}
}


	/* String Width in Rasters */
	/* For the current font in pointsize fontsize */

static double PicTeX_StrWidth(char *str)
{
	char *p;
	int size; 
	double sum;
	size = GP->cex * GP->ps + 0.5;
	SetFont(GP->font, size);
	sum = 0;
	for(p=str ; *p ; *p++)
		sum += charwidth[fontface-1][*p];
	return sum * fontsize; 
}




	/* Start a Path */

static void PicTeX_StartPath()
{
	SetLinetype(GP->lty);
}


	/* End a Path */

static void PicTeX_EndPath()
{
}


/* Possibly Filled Rectangle */
static void PicTeX_Rect(double x0, double y0, double x1, double y1, int bg, int fg)
{
        SetLinetype(GP->lty);
	PicTeX_MoveTo(x0, y0);
	PicTeX_LineTo(x1, y0);
	PicTeX_LineTo(x1, y1);
	PicTeX_LineTo(x0, y1);
	PicTeX_LineTo(x0, y0);
}

static void PicTeX_Circle(double x, double y, double r, int col, int
border)
{
	fprintf(texfp,
	"\\circulararc 360 degrees from %.2f %.2f center at %.2f %.2f\n",
			x, (y + r), x, y);

}


static void PicTeX_Polygon(int n, double *x, double *y, int bg, int fg)
{
	int i;
	if (debug) fprintf(texfp,"%% Drawing polygon with %d vertices.\n",n);
	PicTeX_MoveTo(x[0], y[0]);
	for(i=1 ; i<n ; i++) {
		PicTeX_LineTo(x[i],y[i]);
	}
	PicTeX_LineTo(x[0], y[0]);
}

/* TeX Text Translations */
static void textext(char *str)
{
	fputc('{', texfp);
	for( ; *str ; str++)
		switch(*str) {
			case '$':
				fprintf(texfp, "\\$");
				break;

			case '%':
				fprintf(texfp, "\\%%");
				break;

			case '{':
				fprintf(texfp, "\\{");
				break;

			case '}':
				fprintf(texfp, "\\}");
				break;

			default:
				fputc(*str, texfp);
				break;
		}
	fprintf(texfp,"} ");
}

/* Rotated Text */

static double deg2rad = 0.01745329251994329576;

static void PicTeX_Text(double x, double y, char *str, double xc, double
yc, double rot)
{
	int size;
	double xoff = 0.0, yoff = 0.0, xl, yl, xltemp, xctemp, xlast, ylast;

	size = GP->cex * GP->ps + 0.5;
	SetFont(GP->font, size);
	if(debug) fprintf(texfp,
	"%% Writing string of length %.2f, at %.2f %.2f, xc = %.2f yc = %.2f\n",
	(double)PicTeX_StrWidth(str), x, y, xc, yc); 
	if (debug) fprintf(texfp,
	"%% Writing string of length %.2f, at %.2f %.2f, xc = %.2f yc = %.2f\n",
	(double)PicTeX_StrWidth(str), x, y,xc,yc);
 	if(xc != 0.0 || yc != 0.0) {
		if (rot == 90 || rot == 270) {
			xctemp = xc; xc = yc;
			yc = xctemp;
 			yc = 1.0 - yc;
		}
		xl = PicTeX_StrWidth(str);
		yl = GP->cex * GP->cra[0];
		xoff = -xc * xl;
		yoff = -yc * yl;

	}

	fprintf(texfp,"\\put ");
	textext(str);
	if (rot == 90 )
	fprintf(texfp," [rB] <%.2fpt,%.2fpt>", xoff, yoff);
	else fprintf(texfp," [lB] <%.2fpt,%.2fpt>", xoff, yoff);
	fprintf(texfp," at %.2f %.2f\n", x, y);
}

/* Pick */
static int PicTeX_Locator(int *x, int *y)
{
	return 0;
}


/* Set Graphics mode - not needed for PS */
static void PicTeX_Mode(int mode)
{
}

/* GraphicsInteraction() for the Mac */
static void PicTeX_Hold()
{
}


int PicTeXDeviceDriver(char **cpars, int ncpars, double *npars, int
nnpars)
{
	double xoff, yoff;
	char *pagetype;
	DevInit = 0;

	if(ncpars != 3 || nnpars != 3)
		error("invalid device parameters (pictex)\n");

	filename = cpars[0];
	width = npars[0];
	height = npars[1];
	debug = npars[2];
#ifdef OLD
	DP->bg = GP->bg = npars[3];
	DP->fg = GP->fg = npars[4];
#else
        DP->bg = GP->bg = str2col(cpars[1]);
        DP->fg = GP->fg = str2col(cpars[2]);
#endif

	DevOpen = PicTeX_Open;
	DevClose = PicTeX_Close;
	DevClip = PicTeX_Clip;
	DevResize = PicTeX_Resize;
	DevNewPlot = PicTeX_NewPlot;
	DevStartPath = PicTeX_StartPath;
	DevEndPath = PicTeX_EndPath;
	DevMoveTo = PicTeX_MoveTo;
	DevLineTo = PicTeX_LineTo;
	DevText = PicTeX_Text;
	DevStrWidth = PicTeX_StrWidth;
	DevRect = PicTeX_Rect;
	DevCircle = PicTeX_Circle;
	DevPolygon = PicTeX_Polygon;
	DevLocator = PicTeX_Locator;
	DevMode = PicTeX_Mode;
	DevHold = PicTeX_Hold;

		/* Screen Dimensions in Pixels */

	GP->left = 0;			/* left */
	GP->right = 72.27 * width; 	/* right */
	GP->bottom = 0;			/* bottom */
	GP->top = 72.27 * height;	/* top */

	if( ! PicTeX_Open() ) return 0;

		/* Base Pointsize */
		/* Nominal Character Sizes in Pixels */

	GP->ps = 10;
	GP->cra[0] =  (6.0/12.0) * 10.0;
	GP->cra[1] =  (10.0/12.0) * 10.0;

		/* Character Addressing Offsets */
		/* These offsets should center a single */
		/* plotting character over the plotting point. */
		/* Pure guesswork and eyeballing ... */

	GP->xCharOffset =  0; /*0.4900;*/
	GP->yCharOffset =  0; /*0.3333;*/
	GP->yLineBias = 0; /*0.1;*/

		/* Inches per Raster Unit */
		/* We use printer points */
		/* I.e. 72.27 dots per inch */

	GP->ipr[0] = 1.0/72.27;
	GP->ipr[1] = 1.0/72.27;

	GP->canResizePlot = 0;
	GP->canChangeFont = 1;
	GP->canRotateText = 0;
	GP->canResizeText = 1;
	GP->canClip = 1;

	lty = 1;
	pageno = 0;

	DevInit = 1;
	xlast = 0; ylast = 0;
	return 1;
}
