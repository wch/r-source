/*
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


#include "Defn.h"
#include "Graphics.h"
#include "PS.h"
#include "Fileio.h"
#include "psx11.h"
#include <math.h>
#include <stdio.h>

#define PORTRAIT	1
#define LANDSCAPE	2
#define	FLEXIBLE	3

static FILE	*psfp;				/* temporary file */
static char	filename[128];			/* temporary file name */
static double	truepagewidth;			/* unrotated page width */
static double	truepageheight;			/* unrotated page height */
static char	*papername;			/* page name */
static int	pageorient;			/* page orientation */
static int	canrotate;			/* free to turn page */
static double	pagewidth;			/* page width */
static double	pageheight;			/* page height */
static int	pageno;				/* page number */
static double	plotwidth;			/* plot width */
static double	plotheight;			/* plot width */
static double	xscale;				/* pixel -> points scaling */
static double	yscale;				/* pixel -> points scaling */
static double	xoffset;			/* page offset */
static double	yoffset;			/* page offset */
static double	fontsize;			/* current font size */
static int	fontface;			/* current font face */
static rcolor	col;				/* current color */
static int	lty;				/* current lty */

void psx11_SetColor(int newcol)
{
	if(newcol != col) {
		col = newcol;
		PostScriptSetColor(psfp,
                        R_RED(newcol)/255.0,
                        R_GREEN(newcol)/255.0,
                        R_BLUE(newcol)/255.0); 
	}
}

void psx11_SetLinetype(int newlty)
{
        int i, ltyarray[8];
	if(newlty != lty) {
		lty = newlty;
		for(i=0 ; i<8 && newlty&15 ; i++) {
			ltyarray[i] = newlty&15;
			newlty = newlty>>4;
		}
		PostScriptSetLineTexture(psfp, ltyarray, i);
	}
}

void psx11_SetFont(int face, int size)
{
	if(face != fontface || size != fontsize) {
                PostScriptSetFont(psfp, face-1, size);
		fontface = face;
		fontsize = size;
	}
}

static char *fontname[][6][2] = {
	{ { "Helvetica",				"Helvetica",},
	  { "Helvetica",				"Helv",},
	  { "Helvetica-Bold",				"HelvB",},
	  { "Helvetica-Oblique",			"HelvO",},
	  { "Helvetica-BoldOblique",			"HelvBO",},
	  { "Symbol",					"Symbol",}, },
};
 
static char* a4paper = "A4";
static char* letterpaper = "letter";

int psx11_Open(char *pagetype, int orientation)
{
	if(!strcmp(pagetype, "a4") || !strcmp(pagetype, "A4")) {
		papername = a4paper;
		truepagewidth  = 72.0 * 21.0 / 2.54;
		truepageheight = 72.0 * 29.7 / 2.54;
	}
	else if(!strcmp(pagetype, "letter")) {
		papername = letterpaper;
		truepagewidth  = 72.0 *  8.5;
		truepageheight = 72.0 * 11.0;
	}
	else return 0;

	switch(orientation) {
		case PORTRAIT:
			pageorient = PORTRAIT;
			pagewidth = truepagewidth; 
			pageheight = truepageheight;
			canrotate = 0;
			break;
		case LANDSCAPE:
			pageorient = LANDSCAPE;
			pagewidth = truepageheight;
			pageheight = truepagewidth;
			canrotate = 0;
			break;
		case FLEXIBLE:
			pageorient = FLEXIBLE;
			canrotate = 1;
			break;
		default:
			return 0;
	}
	psfp = NULL;
	sprintf(filename, "/tmp/Rps%d.ps", getpid());
	if((psfp = R_fopen(filename, "w")) == NULL)
		return 0;
	return 1;
}


void psx11_Close()
{
	if(psfp) {
		fclose(psfp);
	}
	unlink(filename);
}

void psx11_NewPlot(int xsize, int ysize, double pw, double ph,
	int face, int size, rcolor startcol, int startlty, rcolor bg)
{
		/* compute plot dimensions in pixels */
		/* compute window -> page mappings */

	if(psfp) fclose(psfp);
	psfp = R_fopen(filename, "w");
	xscale = 72.0 * pw;
	yscale = 72.0 * ph;
	plotwidth  = xscale * xsize;
	plotheight = yscale * ysize;
	if(canrotate) {
		if(plotwidth < plotheight) {
			pageorient = PORTRAIT;
			pagewidth = truepagewidth;
			pageheight = truepageheight;
		}
		else {
			pageorient = LANDSCAPE;
			pagewidth = truepageheight;
			pageheight = truepagewidth;
		}
	}

	if(plotwidth > pagewidth || plotheight > pageheight)
		REprintf("warning: plot size exceeds physical page size\n");

	xoffset = 0.5 * (pagewidth - plotwidth);
	yoffset = pageheight - 0.5 * (pageheight - plotheight);
	yscale = - yscale;

	if(pageorient == LANDSCAPE)
		PostScriptFileHeader(psfp,
			&(fontname[0][0][0]),
			papername,
			truepagewidth,
			truepageheight,
			1,
			yoffset-plotheight,
			xoffset,
			yoffset,
			xoffset+plotwidth);
	else
		PostScriptFileHeader(psfp,
			&(fontname[0][0][0]),
			papername,
			pagewidth,
			pageheight,
			0,
			xoffset,
			yoffset-plotheight,
			xoffset+plotwidth,
			yoffset);

        PostScriptStartPage(psfp, 1);
        if(DP->bg != R_RGB(255,255,255)) {
                psx11_SetColor(DP->bg);
                PostScriptFilledRectangle(psfp,
                        xoffset,
			yoffset,
			xoffset+plotwidth,
			yoffset-plotheight);
                psx11_SetColor(0);
                PostScriptOpenRectangle(psfp,
                        xoffset,
			yoffset,
			xoffset+plotwidth,
			yoffset-plotheight);
        }
	fontface = -1;
	fontsize = -1;
	lty = 0;
	col = 0;
        psx11_SetFont(face, size);
        psx11_SetColor(startcol);
        psx11_SetLinetype(startlty);
}

void psx11_Clip(double x0, double x1, double y0, double y1)
{
	PostScriptSetClipRect(psfp,
		xoffset + xscale * x0,
		xoffset + xscale * x1,
		yoffset + yscale * y0,
		yoffset + yscale * y1);
}

void psx11_StartPath()
{
	psx11_SetColor(GP->col);   
	psx11_SetLinetype(GP->lty);
	PostScriptStartPath(psfp);
}

void psx11_EndPath()
{
	PostScriptEndPath(psfp);
}

void psx11_MoveTo(double x, double y)
{
	PostScriptMoveTo(psfp,
		xoffset + xscale * x, yoffset + yscale * y);
}

void psx11_LineTo(double x, double y)
{
	PostScriptLineTo(psfp,
		xoffset + xscale * x, yoffset + yscale * y);
}

void psx11_Text(double x, double y, char *str, double xc, double yc, double rot)
{
	PostScriptText(psfp,
		xoffset + xscale * x, yoffset + yscale * y,
		str, xc, yc, rot);
}

void psx11_Circle(double x, double y, double r, int col, int border)
{
	if(col != NA_INTEGER) { 
		psx11_SetColor(col);
		PostScriptFilledCircle(psfp,
			xoffset + xscale * x,
			yoffset + yscale * y,
			xscale * r);
	}
	if(border != NA_INTEGER) {  
		psx11_SetColor(border);
		PostScriptOpenCircle(psfp,
			xoffset + xscale * x,
			yoffset + yscale * y,
			xscale * r);
	}
}

void psx11_Rect(double x0, double y0, double x1, double y1, int bg, int fg)
{
	if(bg != NA_INTEGER) {
		psx11_SetColor(bg);
		PostScriptFilledRectangle(psfp,
			xoffset + xscale * x0,
			yoffset + yscale * y0,
			xoffset + xscale * x1,
			yoffset + yscale * y1);
	}
	if(fg != NA_INTEGER) {
		psx11_SetColor(fg);
		psx11_SetLinetype(GP->lty);
		PostScriptOpenRectangle(psfp,
			xoffset + xscale * x0,
			yoffset + yscale * y0,
			xoffset + xscale * x1,
			yoffset + yscale * y1);
	}
}


	/*  This routine is special.   */
	/*  Make sure this is in sync  */
	/*  with PostScript.c          */

void psx11_Polygon(int n, double *x, double *y, int bg, int fg)
{
	int i;
	if(bg != NA_INTEGER) {
		psx11_SetColor(bg);
		fprintf(psfp, "np\n");
		fprintf(psfp, "%.2f %.2f m\n",
			xoffset + xscale * x[0],
			yoffset + yscale * y[0]);
		for(i=1 ; i<n ; i++) {
			fprintf(psfp, "%.2f %.2f l\n",
				xoffset + xscale * x[i],
				yoffset + yscale * y[i]);
		}	
		fprintf(psfp, "cp f\n");
	}
	if(fg != NA_INTEGER) {
		psx11_SetColor(fg);
		fprintf(psfp, "np\n");
		fprintf(psfp, "%.2f %.2f m\n",
			xoffset + xscale * x[0],
			yoffset + yscale * y[0]);
		for(i=1 ; i<n ; i++) {
			fprintf(psfp, "%.2f %.2f l\n",
				xoffset + xscale * x[i],
				yoffset + yscale * y[i]);
		}	
		fprintf(psfp, "cp o\n");
	}
}

void psx11_PrintPlot()
{
	FILE *ifp, *ofp;
	char *lprcmd;
	int c;

	if(!psfp) error("no plot file present (yet)\n");
	fflush(psfp);

	if((lprcmd = getenv("R_PRINTCMD")) == NULL)
		error("don't know how to print\n");
	
	if((ofp=popen(lprcmd, "w")) == NULL)
		error("unable print plot\n");

	if((ifp=R_fopen(filename, "r")) == NULL) {
		fclose(ofp);
		error("unable to open plot file \"%s\"\n", filename);
	}
	while((c=fgetc(ifp)) != EOF)
		fputc(c, ofp);
	PostScriptFileTrailer(ofp, 1);
	fclose(ifp);
	pclose(ofp);
}

void psx11_SavePlot(char *name)
{
	FILE *ifp, *ofp;
	int c;

	if(!psfp) error("no plot file present (yet)\n");
	fflush(psfp);

	if((ofp=R_fopen(name, "w")) == NULL)
		error("unable to open output file \"%s\" for plot\n", name);
	if((ifp=R_fopen(filename, "r")) == NULL) {
		fclose(ofp);
		error("unable to open plot file \"%s\"\n", filename);
	}
	while((c=fgetc(ifp)) != EOF)
		fputc(c, ofp);
	PostScriptFileTrailer(ofp, 1);
	fclose(ofp);
	fclose(ifp);
}
