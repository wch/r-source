/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997 Robert Gentleman, Ross Ihaka and the R Core Team
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

#include "Graphics.h"
#include "Errormsg.h"
#include "Arith.h"
#include <string.h>
#include <stdlib.h>

#define	DEG2RAD		0.01745329251994329576
#define XINVFMAP(x)	((x - GP->fig2dev.ax)/GP->fig2dev.bx)
#define YINVFMAP(y)	((y - GP->fig2dev.ay)/GP->fig2dev.by)

/* Conversion Functions Between Various Metrics */

double xNDCtoChar(double x)
{
	return x / GP->xNDCPerChar;
}

double yNDCtoChar(double y)
{
	return y / GP->yNDCPerChar;
}

double xChartoNDC(double x)
{
	return x * GP->xNDCPerChar;
}

double yChartoNDC(double y)
{
	return y * GP->yNDCPerChar;
}

double xNDCtoInch(double x)
{
	return x / GP->xNDCPerInch;
}

double yNDCtoInch(double y)
{
	return y / GP->yNDCPerInch;
}

double xInchtoNDC(double x)
{
	return x * GP->xNDCPerInch;
}

double yInchtoNDC(double y)
{
	return y * GP->yNDCPerInch;
}

double xInchtoChar(double x)
{
	return x * (GP->xNDCPerInch/GP->xNDCPerChar);
}

double yInchtoChar(double y)
{
	return y * (GP->yNDCPerInch/GP->yNDCPerChar);
}

double xChartoInch(double x)
{
	return x * (GP->xNDCPerChar/GP->xNDCPerInch);
}

double yChartoInch(double y)
{
	return y * (GP->yNDCPerChar/GP->yNDCPerInch);
}

double xFigtoInch(double x)
{
	return xNDCtoInch(x * GP->fig2dev.bx/GP->ndc2dev.bx);
}

double yFigtoInch(double y)
{
	return yNDCtoInch(y * GP->fig2dev.by/GP->ndc2dev.by);
}

double xInchtoFig(double x)
{
	return xInchtoNDC(x * GP->ndc2dev.bx/GP->fig2dev.bx);
}

double yInchtoFig(double y)
{
	return yInchtoNDC(y * GP->ndc2dev.by/GP->fig2dev.by);
}

double xUsrtoInch(double x)
{
	return xFigtoInch(x * GP->win2fig.bx);
}

double yUsrtoInch(double y)
{
	return yFigtoInch(y * GP->win2fig.by);
}


#define NR	(GP->mfg[2])
#define NC	(GP->mfg[3])
#define BYROW	(GP->mfind)

void NewFrameConfirm();				/* bring this into this file. */

/*  DevNull -- Do nothing - place holder  */
void DevNull(void)
{
}

/*  GInit --  Set default graphics parameter values  */
/*  This routine is as part of the process of starting a device driver  */

void GInit()
{
	GP->state = 0;

	GP->ann = 1;
	GP->err = 0;
	GP->bty = 'o';

	GP->cex = 1.0;
	GP->cexbase = 1.0;
	GP->cexmain = 1.2;
	GP->cexlab = 1.0;
	GP->cexsub = 1.0;
	GP->cexaxis = 1.0;

	GP->col = 0;
	GP->colmain = 0;
	GP->collab = 0;
	GP->colsub = 0;
	GP->colaxis = 0;
	GP->gamma = 1;

	/* GP->ps = 10; */
	GP->font = 1;
	GP->fontmain = 2;
	GP->fontlab = 1;
	GP->fontsub = 1;
	GP->fontaxis = 1;

	GP->pch = 1;
	GP->lty = LTY_SOLID;
	GP->smo = 1;

	/* String Adjustment and rotation */
	GP->adj = 0.5;
	GP->crt = 0.0;
	GP->srt = 0.0;

	/* Character extents are based on the raster size */
	GP->asp = GP->ipr[1] / GP->ipr[0];
	/* GP->cin[0] = GP->cra[0] * GP->ipr[0]; */
	/* GP->cin[1] = GP->cra[1] * GP->ipr[1]; */
	/* GP->csi = GP->cin[0]; */
	GP->mkh = GP->cra[0] * GP->ipr[0];

	/* Positioning of margin text */
	GP->mgp[0] = 3;
	GP->mgp[1] = 1;
	GP->mgp[2] = 0;

	/* Axis annotation parameters */
	GP->lab[0] = 5;
	GP->lab[1] = 5;
	GP->lab[2] = 7;
	GP->las = 0;
	GP->tck = -0.2;
	GP->tmag = 1.2;
	GP->type = 'p';
	GP->xaxp[0] = 0.0;
	GP->xaxp[1] = 1.0;
	GP->xaxp[2] = 5.0;
	GP->xaxs = 'r';
	GP->xaxt = 's';
	GP->xlog = 0;
	GP->xpd = 0;
	GP->yaxp[0] = 0.0;
	GP->yaxp[1] = 1.0;
	GP->yaxp[2] = 5.0;
	GP->yaxs = 'r';
	GP->yaxt = 's';
	GP->ylog = 0;

	/* Outer Margins */
	GP->mex = 1.0;
	GP->oma[0] = 0.0;
	GP->oma[1] = 0.0;
	GP->oma[2] = 0.0;
	GP->oma[3] = 0.0;
	GP->fig[0] = 0.0;
	GP->fig[1] = 1.0;
	GP->fig[2] = 0.0;
	GP->fig[3] = 1.0;

	/* Inner Margins */
	GP->mar[0] = 5.1;
	GP->mar[1] = 4.1;
	GP->mar[2] = 4.1;
	GP->mar[3] = 2.1;

	/* Multi-figure parameters */
	GP->mfind  = 0;
	GP->mfg[0] = 1;
	GP->mfg[1] = 1;
	GP->mfg[2] = 1;
	GP->mfg[3] = 1;

	/* Misc plotting parameters */
	GP->new = 0;
	GP->pty = 'm';
	GP->lwd = 1;

	/* Data window */
	GP->usr[0] = 0.0;
	GP->usr[1] = 1.0;
	GP->usr[2] = 0.0;
	GP->usr[3] = 1.0;

	GReset();
	memcpy(DP, GP, sizeof(GPar));
}

#define XNMAP1(x) (GP->ndc2dev.ax+GP->ndc2dev.bx*(x))
#define YNMAP1(y) (GP->ndc2dev.ay+GP->ndc2dev.by*(y))
#define XFMAP1(x) (GP->fig2dev.ax+GP->fig2dev.bx*(x))
#define YFMAP1(y) (GP->fig2dev.ay+GP->fig2dev.by*(y))
#define XIMAP1(x) (GP->inner2dev.ax+GP->inner2dev.bx*(x))
#define YIMAP1(y) (GP->inner2dev.ay+GP->inner2dev.by*(y))



/*  GMapNDC2Dev -- Hide the device behind an NDC layer  */
/*  Use this coordinate system for outer margin coordinates  */

static void GMapNDC2Dev(void)
{
	GP->ndc2dev.bx = DP->ndc2dev.bx = (GP->right - GP->left);
	GP->ndc2dev.ax = DP->ndc2dev.ax = GP->left;
	GP->ndc2dev.by = DP->ndc2dev.by = (GP->top - GP->bottom);
	GP->ndc2dev.ay = DP->ndc2dev.ay = GP->bottom;
}


/*  GMapInner2Dev --  Normalised coordinates for the inner region. */
/*  Use this coordinate system for setting up multiple figures  */
/*  This is also used when specifying the figure region directly  */
/*  Note that this is incompatible with S which uses then entire  */
/*  device surface for such a plot  */

static void GMapInner2Dev(void)
{
	double x0, x1, y0, y1;
	x0 = XNMAP1(xChartoNDC(GP->cexbase * GP->mex * GP->oma[1]));
	y0 = YNMAP1(yChartoNDC(GP->cexbase * GP->mex * GP->oma[0]));
	x1 = XNMAP1(1.0-xChartoNDC(GP->cexbase * GP->mex * GP->oma[3]));
	y1 = YNMAP1(1.0-yChartoNDC(GP->cexbase * GP->mex * GP->oma[2]));
	GP->inner2dev.bx = DP->inner2dev.bx = x1 - x0;
	GP->inner2dev.ax = DP->inner2dev.ax = x0;
	GP->inner2dev.by = DP->inner2dev.by = y1 - y0;
	GP->inner2dev.ay = DP->inner2dev.ay = y0;
}


/*  GMapFig2Dev -- Normalised coordinates for the figure region  */

static void GMapFig2Dev(void)
{
	double nr, nc, x0, x1, y0, y1;
	nr = GP->mfg[2];
	nc = GP->mfg[3];
	y0 = (nr - GP->mfg[0]) / nr;
	y1 = (nr-GP->mfg[0]+1) / nr;
	x0 = (GP->mfg[1]-1) / nc;
	x1 = (GP->mfg[1]) / nc;
	GP->fig[0] = DP->fig[0] = x0;
	GP->fig[1] = DP->fig[1] = x1;
	GP->fig[2] = DP->fig[2] = y0;
	GP->fig[3] = DP->fig[3] = y1;
	y0 = YIMAP1(y0);
	y1 = YIMAP1(y1);
	x0 = XIMAP1(x0);
	x1 = XIMAP1(x1);
	GP->fig2dev.bx = DP->fig2dev.bx = x1 - x0;
	GP->fig2dev.ax = DP->fig2dev.ax = x0;
	GP->fig2dev.by = DP->fig2dev.by = y1 - y0;
	GP->fig2dev.ay = DP->fig2dev.ay = y0;
}


/*  GMapSpecialFig2Dev -- Normalised coordinates for the figure region  */
/*  This is called into play when par(fig=) is used */

static void GMapSpecialFig2Dev(void)
{
	double x0, x1, y0, y1;
	x0 = GP->fig[0] = DP->fig[0];
	x1 = GP->fig[1] = DP->fig[1];
	y0 = GP->fig[2] = DP->fig[2];
	y1 = GP->fig[3] = DP->fig[3];
	y0 = YIMAP1(y0);
	y1 = YIMAP1(y1);
	x0 = XIMAP1(x0);
	x1 = XIMAP1(x1);
	GP->fig2dev.bx = DP->fig2dev.bx = x1 - x0;
	GP->fig2dev.ax = DP->fig2dev.ax = x0;
	GP->fig2dev.by = DP->fig2dev.by = y1 - y0;
	GP->fig2dev.ay = DP->fig2dev.ay = y0;
}


/*  GSetViewPort -- Normalized coordinates for the plot region */

static void GSetViewPort(void)
{
	double x0, x1, y0, y1, xline, yline;
	double center, width;

	xline = fabs(GP->cra[1] * GP->asp / GP->fig2dev.bx);
	yline = fabs(GP->cra[1] / GP->fig2dev.by);
	x0 = XFMAP1(GP->cexbase * GP->mex * GP->mar[1] * xline);
	y0 = YFMAP1(GP->cexbase * GP->mex * GP->mar[0] * yline);
	x1 = XFMAP1(1.0 - GP->cexbase * GP->mex * GP->mar[3] * xline);
	y1 = YFMAP1(1.0 - GP->cexbase * GP->mex * GP->mar[2] * yline);
	if(GP->pty == 's') {
		/* maximal plot size in inches */
		/* there should be a better way to do this */
		xline = xNDCtoInch((x1-x0)/GP->ndc2dev.bx);
		yline = yNDCtoInch((y1-y0)/GP->ndc2dev.by);
		/* shrink the longer side */
		if(xline > yline) {
			width = 0.5*(x1 - x0);
			center = 0.5*(x1 + x0);
			x0 = center-width*(yline/xline);
			x1 = center+width*(yline/xline);
		}
		else {
			width = 0.5*(y1 - y0);
			center = 0.5*(y1 + y0);
			y0 = center-width*(xline/yline);
			y1 = center+width*(xline/yline);
		}
	}
	GP->plt[0] = DP->plt[0] = (x0 - GP->fig2dev.ax)/GP->fig2dev.bx;
	GP->plt[1] = DP->plt[1] = (x1 - GP->fig2dev.ax)/GP->fig2dev.bx;
	GP->plt[2] = DP->plt[2] = (y0 - GP->fig2dev.ay)/GP->fig2dev.by;
	GP->plt[3] = DP->plt[3] = (y1 - GP->fig2dev.ay)/GP->fig2dev.by;
}


/*  GMapWin2Fig -- world to figure Mapping  */

void GMapWin2Fig(void)
{
	GP->win2fig.bx = DP->win2fig.bx =
		(GP->plt[1] - GP->plt[0])/(GP->usr[1] - GP->usr[0]);
	GP->win2fig.ax = DP->win2fig.ax =
		GP->plt[0] - GP->win2fig.bx * GP->usr[0];
	GP->win2fig.by = DP->win2fig.by =
		(GP->plt[3] - GP->plt[2])/(GP->usr[3] - GP->usr[2]);
	GP->win2fig.ay = DP->win2fig.ay =
		GP->plt[2] - GP->win2fig.by * GP->usr[2];
}


/*  GMapping -- Set up mappings between coordinate systems  */
/*  This is the user's interface to the mapping routines above */

void GMapping(int which)
{
	switch(which) {
		case 0:
			if(GP->mfg[1] > 0) GMapNDC2Dev();
		case 1:
			if(GP->mfg[1] > 0) GMapInner2Dev();
		case 2:
			if(GP->mfg[1] > 0) GMapFig2Dev();
			else GMapSpecialFig2Dev();
		case 3:
			GSetViewPort();
	}
}


/*  GReset -- Reset coordinate systems mappings and unit yardsticks */

void GReset()
{
	/* Recompute Mappings */
	GMapping(0);
	/* GForceClip(); */

	/* Units Conversion */
	GP->xNDCPerInch = DP->xNDCPerInch =
		1.0/fabs(GP->ndc2dev.bx * GP->ipr[0]);
	GP->yNDCPerInch = DP->yNDCPerInch =
		1.0/fabs(GP->ndc2dev.by * GP->ipr[1]);
	GP->xNDCPerChar = DP->xNDCPerChar =
		fabs(GP->cra[1] / GP->ndc2dev.bx);
	GP->yNDCPerChar = DP->yNDCPerChar =
		fabs(GP->cra[1] / GP->ndc2dev.by);
}


/*  GSetState -- This records whether GNewPlot has been called  */

void GSetState(int newstate)
{
	DP->state = newstate;
}


/*  GCheckState -- Enquire whether GNewPlot has been called  */

void GCheckState()
{
	if(DP->state == 0)
		error("plot.new has not been called yet\n");
}


/*  GNewPlot -- Begin a new plot (advance to new frame if needed)  */

void GNewPlot()
{
	int i, j, n;

	if(!DevInit) error("No graphics device is active\n");

	/* Restore Default Parameters */
	DevResize();
	memcpy(GP, DP, sizeof(GPar));

	n = NR * NC;
	if(n > 1) {
		i = GP->mfg[0]-1;
		j = GP->mfg[1]-1;
		if(!GP->new) {
			if(!BYROW)  {
				i = i+1;
				if(i >= NR) j=j+1;
			} else {
				j = j+1;
				if(j >= NC) i=i+1;
			}
			i = i%NR;
			j = j%NC;
			if(i==0 && j==0) {
				if(GP->ask) NewFrameConfirm();
				DevNewPlot();
				GP->new =  DP->new = 1;
			}
			GP->mfg[0] = DP->mfg[0] = i+1;
			GP->mfg[1] = DP->mfg[1] = j+1;
		}
	}
	else {
		if(!GP->new) {
			if(GP->ask) NewFrameConfirm();
			DevNewPlot();
		}
		GP->new = DP->new = 1;
	}
	GReset();
	GForceClip();
}


static int oldstate = -99;		/*  Interpretation  */
					/*  state = 0, graphics off */
					/*  state = 1, graphics on */
					/*  state = 2, graphical input on */
					/*  (Ignored by most drivers)  */

/*  GMode - Check that everything is initialized  */

void GMode(int state)
{
	if(!DevInit)
		error("No graphics device is active\n");
	if(state != oldstate)
		DevMode(state);
	DP->new = GP->new = 0;
	oldstate = state;
}


/*  GRestore -- Restore the graphics parameters from the device copy  */

void GRestore()
{
	if( !DevInit ) error("No graphics device is active\n");
	memcpy(GP, DP, sizeof(GPar));
}


/* Saving and restoring of "inline" graphical parameters. */
/* These are the ones which can be specified as a arguments */
/* to high-level graphics functions. */

static double	adjsave;	/* adj */
static int	annsave;	/* ann */
static int	btysave;	/* bty */
static double	cexsave;	/* cex */
static double	cexmainsave;	/* cex.main */
static double	cexlabsave;	/* cex.lab */
static double	cexsubsave;	/* cex.sub */
static double	cexaxissave;	/* cex.axis */
static int	colsave;	/* col */
static int	fgsave;		/* fg */
static int	bgsave;		/* bg */
static int	colmainsave;	/* col.main */
static int	collabsave;	/* col.lab */
static int	colsubsave;	/* col.sub */
static int	colaxissave;	/* col.axis */
static double	crtsave;	/* character rotation */
static int	fontsave;	/* font */
static int	fontmainsave;	/* font.main */
static int	fontlabsave;	/* font.lab */
static int	fontsubsave;	/* font.sub */
static int	fontaxissave;	/* font.axis */
#ifdef NO
   static int	csisave;	/* line spacing in inches */
#endif
static int	errsave;	/* error mode */
static int	labsave[3];	/* axis labelling parameters */
static int	lassave;	/* label style */
static int	ltysave;	/* line type */
static double	lwdsave;	/* line width */
static double	mgpsave[3];	/* margin position for annotation */
static double	mkhsave;	/* mark height */
static int	pchsave;	/* plotting character */
static double	srtsave;	/* string rotation */
static double	tcksave;	/* tick mark length */
static double	xaxpsave[3];	/* x axis parameters */
static int	xaxssave;	/* x axis calculation style */
static int	xaxtsave;	/* x axis type */
static int	xpdsave;	/* clipping control */
static double	yaxpsave[3];	/* y axis parameters */
static int	yaxssave;	/* y axis calculation style */
static int	yaxtsave;	/* y axis type */


/*  GSavePars -- Make a temporary copy of the inline parameter values  */

void GSavePars(void)
{
	adjsave = GP->adj;
	annsave = GP->ann;
	btysave = GP->bty;
	cexsave = GP->cex;
	cexlabsave = GP->cexlab;
	cexmainsave = GP->cexmain;
	cexsubsave = GP->cexsub;
	cexaxissave = GP->cexaxis;
	colsave = GP->col;
	fgsave = GP->fg;
	bgsave = GP->bg;
	collabsave = GP->collab;
	colmainsave = GP->colmain;
	colsubsave = GP->colsub;
	colaxissave = GP->colaxis;
	crtsave = GP->crt;
	errsave = GP->err;
	fontsave = GP->font;
	fontmainsave = GP->fontmain;
	fontlabsave = GP->fontlab;
	fontsubsave = GP->fontsub;
	fontaxissave = GP->fontaxis;
	/* csisave = GP->csi; */
	labsave[0] = GP->lab[0];
	labsave[1] = GP->lab[1];
	labsave[2] = GP->lab[2];
	lassave = GP->las;
	ltysave = GP->lty;
	lwdsave = GP->lwd;
	mgpsave[0] = GP->mgp[0];
	mgpsave[1] = GP->mgp[1];
	mgpsave[2] = GP->mgp[2];
	mkhsave = GP->mkh;
	pchsave = GP->pch;
	srtsave = GP->srt;
	tcksave = GP->tck;
	xaxpsave[0] = GP->xaxp[0];
	xaxpsave[1] = GP->xaxp[1];
	xaxpsave[2] = GP->xaxp[2];
	xaxssave = GP->xaxs;
	xaxtsave = GP->xaxt;
	xpdsave = GP->xpd;
	yaxpsave[0] = GP->yaxp[0];
	yaxpsave[1] = GP->yaxp[1];
	yaxpsave[2] = GP->yaxp[2];
	yaxssave = GP->yaxs;
	yaxtsave = GP->yaxt;
}


/*  GSavePars -- Restore temorarily saved inline parameter values  */

void GRestorePars(void)
{
	GP->adj = adjsave;
	GP->ann = annsave;
	GP->bty = btysave;
	GP->cex = cexsave;
	GP->cexlab = cexlabsave;
	GP->cexmain = cexmainsave;
	GP->cexsub = cexsubsave;
	GP->cexaxis = cexaxissave;
	GP->col = colsave;
	GP->fg = fgsave;
	GP->bg = bgsave;
	GP->collab = collabsave;
	GP->colmain = colmainsave;
	GP->colsub = colsubsave;
	GP->colaxis = colaxissave;
	GP->crt = crtsave;
	GP->err = errsave;
	GP->font = fontsave;
	GP->fontmain = fontmainsave;
	GP->fontlab = fontlabsave;
	GP->fontsub = fontsubsave;
	GP->fontaxis = fontaxissave;
	/* GP->csi = csisave; */
	GP->lab[0] = labsave[0];
	GP->lab[1] = labsave[1];
	GP->lab[2] = labsave[2];
	GP->las = lassave;
	GP->lty = ltysave;
	GP->lwd = lwdsave;
	GP->mgp[0] = mgpsave[0];
	GP->mgp[1] = mgpsave[1];
	GP->mgp[2] = mgpsave[2];
	GP->mkh = mkhsave;
	GP->pch = pchsave;
	GP->srt = srtsave;
	GP->tck = tcksave;
	GP->xaxp[0] = xaxpsave[0];
	GP->xaxp[1] = xaxpsave[1];
	GP->xaxp[2] = xaxpsave[2];
	GP->xaxs = xaxssave;
	GP->xaxt = xaxtsave;
	GP->xpd = xpdsave;
	GP->yaxp[0] = yaxpsave[0];
	GP->yaxp[1] = yaxpsave[1];
	GP->yaxp[2] = yaxpsave[2];
	GP->yaxs = yaxssave;
	GP->yaxt = yaxtsave;
}


/*  GClip -- Update the device clipping region (depends on GP->xpd)  */

static int clipstate = -99;

void GClip()
{
	if(GP->xpd != clipstate) {
		if(GP->xpd)
			DevClip(GP->left, GP->right, GP->bottom, GP->top);
		else
			DevClip(XFMAP(GP->plt[0]), XFMAP(GP->plt[1]),
				YFMAP(GP->plt[2]), YFMAP(GP->plt[3]));
		clipstate = GP->xpd;
	}
}


/*  GForceClip -- Forced update of the device clipping region  */

void GForceClip()
{
	if(GP->xpd)
		DevClip(GP->left, GP->right, GP->bottom, GP->top);
	else
		DevClip(XFMAP(GP->plt[0]), XFMAP(GP->plt[1]),
			YFMAP(GP->plt[2]), YFMAP(GP->plt[3]));
	clipstate = GP->xpd;
}

#ifdef OUT
void GLPretty(double*, double*, int*);
void GPretty(double*, double*, int*);
void GLScale(double, double, int);
void GScale(double, double, int);
#endif

/*  GLPretty -- Set scale and ticks for logarithmic scales  */
/*  Note: 1 2 5 10 looks good on logarithmic scales  */

void GLPretty(double *xmin, double *xmax, int *n)
{
	double u1, u2, v1, v2, p1, p2;
	
	if((*xmax) <= 7.8651*(*xmin))
		GPretty(xmin, xmax, n);
	else {
		p1 = ceil(*xmin-0.0001);
		p2 = floor(*xmax+0.0001);
		u1 = pow(10.0, p1);
		v1 = pow(10.0, p2);
		*n = p2 - p1;

		if(*n <= 1) {
			*n = 2*(*n);
			u2 = 0.5*u1;
			if(u2 <= *xmin) {
				u2 = u1;
			}
			else (*n)++;
	
			v2 = 5.0*v1;
			if(v2 >= *xmax) {
				v2 = v1;
			}
			else (*n)++;

			*xmin = u2;
			*xmax = v2;
		}
		else {
			*xmin = u1;
			*xmax = v1;
		}
	}
}

void GPretty(double *s, double *u, int *ndiv)
{
	double base, cell, unit, tmp;
	int ns, nu;

	if( *s == R_PosInf || *u == R_PosInf
	 || *s == R_NegInf || *u == R_NegInf || *ndiv == 0 )
		error("infinite axis extents\n");

	cell = FLT_EPSILON + (*u-*s) / *ndiv;
	base = pow(10.0, floor(log10(cell)));
	unit = base;
	if(fabs((tmp = 2.0*base)-cell) < fabs(unit-cell)) unit = tmp;
	if(fabs((tmp = 5.0*base)-cell) < fabs(unit-cell)) unit = tmp;
	if(fabs((tmp = 10.0*base)-cell) < fabs(unit-cell)) unit = tmp;
	
	ns = floor(*s/unit);
	while(ns*unit > *s - FLT_EPSILON) ns--;
	ns++;
	*s = unit*ns;
	
	nu = ceil(*u/unit);
	while(nu*unit < *u + FLT_EPSILON) nu++;
	nu--;
	*u = unit*nu;
	
	*ndiv = (*u-*s)/unit+0.5;
}

void GScale(double xmin, double xmax, int axis)
{
	int log, n, style, swap;
	double temp;

	log = 0;

	if(axis == 1 || axis == 3) {
		n = GP->lab[0];
		style = GP->xaxs;
		if(GP->xlog) {
			log = 1;
			xmin = log10(xmin);
			xmax = log10(xmax);
		}
	}
	else {
		n = GP->lab[1];
		style = GP->yaxs;
		if(GP->ylog) {
			log = 1;
			xmin = log10(xmin);
			xmax = log10(xmax);
		}
	}

	if(xmin == xmax) {
		if(xmin == 0) {
			xmin = -1;
			xmax =  1;
		}
		else {
			xmin = 0.6 * xmin;
			xmax = 1.4 * xmax;
		}
	}

	switch(style) {
		case 'r':
			temp = 0.04 * (xmax-xmin);
			xmin = xmin - temp;
			xmax = xmax + temp;
		case 'i':
			break;
		case 's':
		case 'e':
		default:
			error("axis style \"%c\" unimplemented\n", style);
	}

	if(axis == 1 || axis == 3) {
		GP->usr[0] = DP->usr[0] = xmin;
		GP->usr[1] = DP->usr[1] = xmax;
	}
	else {
		GP->usr[2] = DP->usr[2] = xmin;
		GP->usr[3] = DP->usr[3] = xmax;
	}

	if(xmin > xmax) {
		swap = 1;
		temp = xmin;
		xmin = xmax;
		xmax = temp;
	}
	else swap = 0;

	if(log) {
		xmin = pow(10.0,xmin);
		xmax = pow(10.0,xmax);
	}
	GPretty(&xmin, &xmax, &n);

	if(swap) {
		temp = xmin;
		xmin = xmax;
		xmax = temp;
	}

	if(axis == 1 || axis == 3) {
		GP->xaxp[0] = DP->xaxp[0] = xmin;
		GP->xaxp[1] = DP->xaxp[1] = xmax;
		GP->xaxp[2] = DP->xaxp[2] = n;
	}
	else {
		GP->yaxp[0] = DP->yaxp[0] = xmin;
		GP->yaxp[1] = DP->yaxp[1] = xmax;
		GP->yaxp[2] = DP->yaxp[2] = n;
	}
}


/*  GSetupAxis -- Set up the default axis information  */

void GSetupAxis(int axis)
{
	double xmin, xmax;
	int n;

	if(axis == 1 || axis == 3) {
		n = GP->lab[0];
		xmin = GP->usr[0];
		xmax = GP->usr[1];
	}
	else {
		n = GP->lab[1];
		xmin = GP->usr[2];
		xmax = GP->usr[3];
	}

	GPretty(&xmin, &xmax, &n);

	if(axis == 1 || axis == 3) {
		GP->xaxp[0] = DP->xaxp[0] = xmin;
		GP->xaxp[1] = DP->xaxp[1] = xmax;
		GP->xaxp[2] = DP->xaxp[2] = n;
	}
	else {
		GP->yaxp[0] = DP->yaxp[0] = xmin;
		GP->yaxp[1] = DP->yaxp[1] = xmax;
		GP->yaxp[2] = DP->yaxp[2] = n;
	}
}


/*  GMoveTo -- Update the current "pen position"  */

void GMoveTo(double x, double y)
{
	if(DP->canClip) GClip();
	DevMoveTo(XFMAP(x),YFMAP(y));
	DP->xlast = x;
	DP->ylast = y;
}


/*  GArrow -- Draw an arrow  */

void GArrow(double xfrom, double yfrom, double xto, double yto, 
	double length, double angle, int code)
{
	double rot, x1, x2, xc,  y1, y2, yc;

	GStartPath();
	GMoveTo(xfrom, yfrom);
	GLineTo(xto, yto);
	GEndPath();

	if(code & 1) {
		xc = xFigtoInch(xto - xfrom);
		yc = yFigtoInch(yto - yfrom);
		rot= atan2(yc, xc);
		x1 = xfrom + xInchtoFig(length * cos(rot+angle*DEG2RAD));
		y1 = yfrom + xInchtoFig(length * sin(rot+angle*DEG2RAD));
		x2 = xfrom + xInchtoFig(length * cos(rot-angle*DEG2RAD));
		y2 = yfrom + xInchtoFig(length * sin(rot-angle*DEG2RAD));
		GStartPath();
		GMoveTo(x1, y1);
		GLineTo(xfrom, yfrom);
		GLineTo(x2, y2);
		GEndPath();
	}
	if(code & 2) {
		xc = xFigtoInch(xfrom - xto);
		yc = yFigtoInch(yfrom - yto);
		rot= atan2(yc, xc);
		x1 = xto + xInchtoFig(length * cos(rot+angle*DEG2RAD));
		y1 = yto + xInchtoFig(length * sin(rot+angle*DEG2RAD));
		x2 = xto + xInchtoFig(length * cos(rot-angle*DEG2RAD));
		y2 = yto + xInchtoFig(length * sin(rot-angle*DEG2RAD));
		GStartPath();
		GMoveTo(x1, y1);
		GLineTo(xto, yto);
		GLineTo(x2, y2);
		GEndPath();
	}
}


/*  GBox -- Draw a box about one of several regions  */

void GBox(int which)
{
	double x0, x1, y0, y1;
	GClip();
	switch(which) {
	case 1: /* Plot */
		GStartPath();
		switch(GP->bty) {
		case 'o':
		case 'O':
			GMoveTo(GP->plt[0], GP->plt[2]);
			GLineTo(GP->plt[1], GP->plt[2]);
			GLineTo(GP->plt[1], GP->plt[3]);
			GLineTo(GP->plt[0], GP->plt[3]);
			GLineTo(GP->plt[0], GP->plt[2]);
			break;
		case 'l':
		case 'L':
			GMoveTo(GP->plt[0], GP->plt[3]);
			GLineTo(GP->plt[0], GP->plt[2]);
			GLineTo(GP->plt[1], GP->plt[2]);
			break;
		case '7':
			GMoveTo(GP->plt[0], GP->plt[3]);
			GLineTo(GP->plt[1], GP->plt[3]);
			GLineTo(GP->plt[1], GP->plt[2]);
			break;
		case 'c':
		case 'C':
			GMoveTo(GP->plt[1], GP->plt[2]);
			GLineTo(GP->plt[0], GP->plt[2]);
			GLineTo(GP->plt[0], GP->plt[3]);
			GLineTo(GP->plt[1], GP->plt[3]);
			break;
		default:
			break;
		}
		GEndPath();
		break;
	case 2: /* Figure */
		GStartPath();
		GMoveTo(0.0, 0.0);
		GLineTo(1.0, 0.0);
		GLineTo(1.0, 1.0);
		GLineTo(0.0, 1.0);
		GLineTo(0.0, 0.0);
		GEndPath();
		break;
	case 4: /* Device border */
		x0 = XINVFMAP(DP->ndc2dev.ax);
		y0 = YINVFMAP(DP->ndc2dev.ay);
		x1 = XINVFMAP(DP->ndc2dev.ax + DP->ndc2dev.bx);
		y1 = YINVFMAP(DP->ndc2dev.ay + DP->ndc2dev.by);
		GStartPath();
		GMoveTo(x0, y0);
		GLineTo(x1, y0);
		GLineTo(x1, y1);
		GLineTo(x0, y1);
		GLineTo(x0, y0);
		GEndPath();
		break;
	default:
		error("invalid GBox argument\n");
	}
}


/*  GCircle -- Draw a circle (radius is given in inches)  */

void GCircle(double x, double y, double radius, int col, int border)
{
	double ix, iy, ir;
	ix = XFMAP(x);
	iy = YFMAP(y);
#ifdef POINTS
	ir = radius/(72.0 * GP->ipr[0]);
#endif
	ir = radius/GP->ipr[0];
	DevCircle(ix, iy, ir, col, border);
}


/*  GStartPath -- Begin a connected path */

void GStartPath()
{
	if(GP->lty != NA_INTEGER)
		DevStartPath();
}


/*  GEndPath -- End a connected path */

void GEndPath()
{
	if(GP->lty != NA_INTEGER)
		DevEndPath();
}


/* Draw Line Segments, Clipping to the Viewport */
/* Cohen-Sutherland Algorithm -- Unneeded if the device can do the clipping */

#define	BOTTOM	001
#define	LEFT	002
#define	TOP	004
#define	RIGHT	010

/* Parameters for clipping to the plot region in figure coordinates */

#define Clipxl (GP->plt[0])
#define Clipxr (GP->plt[1])
#define Clipyb (GP->plt[2])
#define Clipyt (GP->plt[3])

static int clipcode(double x, double y)
{
	int c;

	c = 0;
	if(x < Clipxl)
		c |= LEFT;
	else if(x > Clipxr)
		c |= RIGHT;
	if(y < Clipyb)
		c |= BOTTOM;
	else if(y > Clipyt)
		c |= TOP;
	return c;
}

static void CSclip(double x1, double y1, double x2, double y2)
{
	int c, c1, c2;
	double x, y, xstart, ystart;

	xstart = x1;
	ystart = y1;
	
	c1 = clipcode(x1, y1);
	c2 = clipcode(x2, y2);
	while( c1 || c2 ) {
		if(c1 & c2) goto skip;
		if( c1 ) c = c1;
		else c = c2;
		if( c & LEFT ) {
			y = y1+(y2-y1)*(Clipxl-x1)/(x2-x1);
			x = Clipxl;
		}
		else if( c & RIGHT ) {
			y = y1+(y2-y1)*(Clipxr-x1)/(x2-x1);
			x = Clipxr;
		}
		else if( c & BOTTOM ) {
			x = x1+(x2-x1)*(Clipyb-y1)/(y2-y1);
			y = Clipyb;
		}
		else if( c & TOP ) {
			x = x1+(x2-x1)*(Clipyt-y1)/(y2-y1);
			y = Clipyt;
		}

		if( c==c1 ) {
			x1 = x;
			y1 = y;
			c1 = clipcode(x,y);
		}
		else {
			x2 = x;
			y2 = y;
			c2 = clipcode(x,y);
		}
	}
	if(x1 != xstart || y1 != ystart)
		DevMoveTo(XFMAP(x1),YFMAP(y1));
	DevLineTo(XFMAP(x2),YFMAP(y2));
skip:
	;
}


/*  GLineTo -- Draw a clipped line segment */

void GLineTo(double x, double y)
{
	if(GP->lty != NA_INTEGER) {
		if(DP->canClip) {
			GClip();
			DevLineTo(XFMAP(x),YFMAP(y));
		}
		else {
			if(GP->xpd) DevLineTo(XFMAP(x),YFMAP(y));
			else CSclip(DP->xlast, DP->ylast, x, y);
		}
	}
	DP->xlast = x;
	DP->ylast = y;
}


/*  GPolygon -- Draw a polygon  */
/*  Filled with color bg and outlined with color fg  */
/*  These may both be NA_INTEGER  */

void GPolygon(int n, double *x, double *y, int bg, int fg, int usrcoords,
	double *work)
{
	int i;
	double *dx, *dy;
	double xx, yy;
	dx = &work[0];
	dy = &work[n];

	for(i=0 ; i<n ; i++) {
		xx = x[i];
		yy = y[i];
		if(!FINITE(xx) || !FINITE(yy))
			error("NA encountered in polygon\n");
		if(usrcoords) {
			xx = XMAP(xx);
			yy = YMAP(yy);
		}
		dx[i] = XFMAP(xx);
		dy[i] = YFMAP(yy);
	}

	/* If device can't clip we should use something */
	/* like Sutherland-Hodgman here */

	if(DP->canClip) GClip();
	DevPolygon(n, dx, dy, bg, fg);
}


/*  GRect -- Draw a rectangle  */
/*  Filled with color bg and outlined with color fg  */
/*  These may both be NA_INTEGER  */

void GRect(double x0, double y0, double x1, double y1, int bg, int fg)
{
	DevRect(XFMAP(x0), YFMAP(y0), XFMAP(x1), YFMAP(y1), bg, fg);
}


/*  GSymbol -- Draw one of the R special symbols  */

static void Inch2Fig(double v, double *x, double *y)
{
	*x = v / xNDCtoInch(GP->fig2dev.bx / DP->ndc2dev.bx);
	*y = v / yNDCtoInch(GP->fig2dev.by / DP->ndc2dev.by);
}

#define SMALL	0.25
#ifdef OLD
#define RADIUS	0.425
#else
#define RADIUS	0.375
#endif
#define SQRC	0.88622692545275801364		/* sqrt(pi / 4) */
#define DMDC	1.25331413731550025119		/* sqrt(pi / 4) * sqrt(2) */
#define TRC0	1.55512030155621416073		/* sqrt(4 * pi/(3 * sqrt(3))) */
#define TRC1	1.34677368708859836060		/* TRC0 * sqrt(3) / 2 */
#define TRC2	0.77756015077810708036		/* TRC0 / 2 */
#define CMAG	1.1				/* Circle magnifier */

void GSymbol(double x, double y, int pch)
{
	double r, xc, yc, unit;
	double xx[4], yy[4], zz[8];
	char str[2];
	int ltysave;

	if(' ' <= pch && pch <= 255) {
		str[0] = pch;
		str[1] = '\0';
		GText(x, y, str, GP->xCharOffset, GP->yCharOffset, 0.0);
	}
	else {
		ltysave = GP->lty;
		GP->lty = LTY_SOLID;

		switch(pch) {

		case 0: /* S square */
			unit = RADIUS * GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			GRect(x-xc, y-yc, x+xc, y+yc, NA_INTEGER, GP->col);
			break;

		case 1: /* S octahedron ( circle) */
			xc = CMAG * RADIUS * GStrWidth("0", 3);
			GCircle(x, y, xc, NA_INTEGER, GP->col);
			break;

		case 2:	/* S triangle - point up */
			unit = RADIUS * GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			r = TRC0 * yc;
			xc = TRC1 * xc;
			yc = TRC2 * yc;
			GStartPath();
			GMoveTo(x, y+r);
			GLineTo(x+xc, y-yc);
			GLineTo(x-xc, y-yc);
			GLineTo(x, y+r);
			GEndPath();
			break;

		case 3: /* S plus */
			unit = sqrt(2.0)*RADIUS*GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			GStartPath();
			GMoveTo(x-xc, y);
			GLineTo(x+xc, y);
			GMoveTo(x, y-yc);
			GLineTo(x, y+yc);
			GEndPath();
			break;

		case 4: /* S times */
			unit = RADIUS * GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			GStartPath();
			GMoveTo(x-xc, y-yc);
			GLineTo(x+xc, y+yc);
			GMoveTo(x-xc, y+yc);
			GLineTo(x+xc, y-yc);
			GEndPath();
			break;

		case 5: /* S diamond */
			unit = sqrt(2.0) * RADIUS * GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			GStartPath();
			GMoveTo(x-xc, y);
			GLineTo(x, y+yc);
			GLineTo(x+xc, y);
			GLineTo(x, y-yc);
			GLineTo(x-xc, y);
			GEndPath();
			break;

		case 6: /* S triangle - point down */
			unit = RADIUS * GStrWidth("0",3);
			Inch2Fig(unit, &xc, &yc);
			xc = TRC1 * xc;
			r = TRC0 * yc;
			yc = TRC2 * yc;
			GStartPath();
			GMoveTo(x, y-r);
			GLineTo(x+xc, y+yc);
			GLineTo(x-xc, y+yc);
			GLineTo(x, y-r);
			GEndPath();
			break;

		case 7:	/* S square and times superimposed */
			unit =  RADIUS * GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			GStartPath();
			GMoveTo(x-xc, y-yc);
			GLineTo(x+xc, y+yc);
			GMoveTo(x-xc, y+yc);
			GLineTo(x+xc, y-yc);
			GEndPath();
			GRect(x-xc, y-yc, x+xc, y+yc, NA_INTEGER, GP->col);
			break;

		case 8: /* S plus and times superimposed */
			unit =  RADIUS * GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			GStartPath();
			GMoveTo(x-xc, y-yc);
			GLineTo(x+xc, y+yc);
			GMoveTo(x-xc, y+yc);
			GLineTo(x+xc, y-yc);
			xc = sqrt(2.0) * xc;
			yc = sqrt(2.0) * yc;
			GMoveTo(x-xc, y);
			GLineTo(x+xc, y);
			GMoveTo(x, y-yc);
			GLineTo(x, y+yc);
			GEndPath();
			break;

		case 9: /* S diamond and plus superimposed */
			unit = sqrt(2.0) * RADIUS * GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			GStartPath();
			GMoveTo(x-xc, y);
			GLineTo(x, y+yc);
			GLineTo(x+xc, y);
			GLineTo(x, y-yc);
			GLineTo(x-xc, y);
			GMoveTo(x-xc, y);
			GLineTo(x+xc, y);
			GMoveTo(x, y-yc);
			GLineTo(x, y+yc);
			GEndPath();
			break;

		case 10: /* S hexagon (circle) and plus superimposed */
			unit = CMAG * RADIUS * GStrWidth("0", 3);
			GCircle(x, y, unit, NA_INTEGER, GP->col);
			Inch2Fig(unit, &xc, &yc);
			GStartPath();
			GMoveTo(x-xc, y);
			GLineTo(x+xc, y);
			GMoveTo(x, y-yc);
			GLineTo(x, y+yc);
			GEndPath();
			break;

		case 11: /* S superimposed triangles */
			unit = RADIUS * GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			xc = TRC1 * xc;
			r = TRC0 * yc;
			yc = TRC2 * yc;
			yc = 0.5 * (yc + r);
			GStartPath();
			GMoveTo(x, y+yc);
			GLineTo(x+xc, y-yc);
			GLineTo(x-xc, y-yc);
			GLineTo(x, y+yc);
			GMoveTo(x, y-yc);
			GLineTo(x+xc, y+yc);
			GLineTo(x-xc, y+yc);
			GLineTo(x, y-yc);
			GEndPath();
			break;

		case 12: /* S square and plus superimposed */
			unit = RADIUS * GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			GStartPath();
			GMoveTo(x, y-yc);
			GLineTo(x, y+yc);
			GMoveTo(x-xc, y);
			GLineTo(x+xc, y);
			GEndPath();
			GRect(x-xc, y-yc, x+xc, y+yc, NA_INTEGER, GP->col);
			break;

		case 13: /* S octagon (circle) and times superimposed */
			unit = CMAG * RADIUS * GStrWidth("0", 3);
			GCircle(x, y, unit, NA_INTEGER, GP->col);
			Inch2Fig(unit, &xc, &yc);
			GStartPath();
			GMoveTo(x-xc, y-yc);
			GLineTo(x+xc, y+yc);
			GMoveTo(x+xc, y-yc);
			GLineTo(x-xc, y+yc);
			GEndPath();
			break;

		case 14: /* S square and point-up triangle superimposed */
			unit = RADIUS * GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			GStartPath();
			GMoveTo(x-xc, y-yc);
			GLineTo(x, y+yc);
			GLineTo(x+xc, y-yc);
			GLineTo(x-xc, y-yc);
			GEndPath();
			GRect(x-xc, y-yc, x+xc, y+yc, NA_INTEGER, GP->col);
			break;

		case 15: /* S filled square */
			unit = RADIUS * GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			xx[0] = x-xc; yy[0] = y-yc;
			xx[1] = x+xc; yy[1] = y-yc;
			xx[2] = x+xc; yy[2] = y+yc;
			xx[3] = x-xc; yy[3] = y+yc;
			GPolygon(4, xx, yy, GP->col, NA_INTEGER, 0, zz);
			break;

		case 16: /* S filled octagon (circle) */
			unit = RADIUS * GStrWidth("0", 3);
			GCircle(x, y, unit, GP->col, GP->col);
			break;

		case 17: /* S filled point-up triangle */
			unit = RADIUS * GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			r = TRC0 * yc;
			xc = TRC1 * xc;
			yc = TRC2 * yc;
			xx[0] = x;    yy[0] = y+r;
			xx[1] = x+xc; yy[1] = y-yc;
			xx[2] = x-xc; yy[2] = y-yc;
			GPolygon(3, xx, yy, GP->col, NA_INTEGER, 0, zz);
			break;
			
		case 18:
			unit = RADIUS * GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			xx[0] = x;    yy[0] = y-yc;
			xx[1] = x+xc; yy[1] = y;
			xx[2] = x;    yy[2] = y+yc;
			xx[3] = x-xc; yy[3] = y;
			GPolygon(4, xx, yy, GP->col, NA_INTEGER, 0, zz);
			break;

		case 19: /* R filled circle */
			unit = RADIUS * GStrWidth("0", 3);
			GCircle(x, y, unit, GP->col, GP->col);
			break;


		case 20: /* R Dot */
			unit = SMALL * GStrWidth("0", 3);
			GCircle(x, y, unit, GP->col, GP->col);
			break;


		case 21: /* circles */
			unit = RADIUS * GStrWidth("0", 3);
			GCircle(x, y, unit, GP->bg, GP->col);
			break;

		case  22: /* squares */
			unit = RADIUS * SQRC * GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			GRect(x-xc, y-yc, x+xc, y+yc, GP->bg, GP->col);
			break;

		case 23: /* diamonds */
			unit = RADIUS * DMDC * GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			xx[0] = x   ; yy[0] = y-yc;
			xx[1] = x+xc; yy[1] = y;
			xx[2] = x   ; yy[2] = y+yc;
			xx[3] = x-xc; yy[3] = y;
			GPolygon(4, xx, yy, GP->bg, GP->col, 0, zz);
			break;

		case 24: /* triangle (point up) */
			unit = RADIUS * GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			r = TRC0 * yc;
			xc = TRC1 * xc;
			yc = TRC2 * yc;
			xx[0] = x;    yy[0] = y+r;
			xx[1] = x+xc; yy[1] = y-yc;
			xx[2] = x-xc; yy[2] = y-yc;
			GPolygon(3, xx, yy, GP->bg, GP->col, 0, zz);
			break;

		case 25: /* triangle (point down) */
			unit = RADIUS * GStrWidth("0", 3);
			Inch2Fig(unit, &xc, &yc);
			r = TRC0 * yc;
			xc = TRC1 * xc;
			yc = TRC2 * yc;
			xx[0] = x;    yy[0] = y-r;
			xx[1] = x+xc; yy[1] = y+yc;
			xx[2] = x-xc; yy[2] = y+yc;
			GPolygon(3, xx, yy, GP->bg, GP->col, 0, zz);
			break;
		}
		GP->lty = ltysave;
	}
}


/*  GLocator -- Read the current pen position */

int GLocator(double *x, double *y, int usercoords)
{
	double ix, iy;
	if(!DevLocator)
		error("no locator capability in device driver\n");
	if(DevLocator(&ix, &iy)) {
		*x = (ix - GP->fig2dev.ax) / GP->fig2dev.bx;
		*y = (iy - GP->fig2dev.ay) / GP->fig2dev.by;
		if(usercoords) {
			*x = (*x - GP->win2fig.ax) / GP->win2fig.bx;
			*y = (*y - GP->win2fig.ay) / GP->win2fig.by;
		}
		return 1;
	}
	else
		return 0;
}


/* GMetricInfo -- Access character fontr metric information  */

void GMetricInfo(int c, double *ascent, double *descent, double *width, int units)
{
	double a, b;

	if(DevMetricInfo)
		DevMetricInfo(c, ascent, descent, width);
	else
		error("detailed character metric information unavailable\n");

	switch(units) {
		case 1: /* user == world */
			a = 1.0 / (GP->fig2dev.bx * GP->win2fig.bx);
			b = 1.0 / (GP->fig2dev.by * GP->win2fig.by);
			break;
		case 2:	/* figure */
			a = 1.0 / GP->fig2dev.bx;
			b = 1.0 / GP->fig2dev.by;
			break;
		case 3: /* inches */
			a = 1.0 * GP->ipr[0];
			b = 1.0 * GP->ipr[1];
			break;
		case 4: /* rasters */
			a = 1.0;
			b = 1.0;
			break;
	}
	*ascent = b * (*ascent);
	*descent = b * (*descent);
	*width = a * (*width);
}


/*  GStrWidth -- Compute string width  */

double GStrWidth(char *str, int units)
{
	double w = DevStrWidth(str) / GP->fig2dev.bx;
	switch(units) {
		case 1: /* user == world */
			w = (DevStrWidth(str) / GP->fig2dev.bx) / GP->win2fig.bx;
			break;
		case 2:	/* figure */
			w = DevStrWidth(str) / GP->fig2dev.bx;
			break;
		case 3: /* inches */
			w = DevStrWidth(str) * GP->ipr[0];
			break;
		case 4: /* rasters */
			w = DevStrWidth(str);
			break;
	}
	return w;
}


double GStrHeight(char *str, int units)
{
	double h = GP->cex * GP->cra[1];
	switch(units) {
		case 1: /* user == world */
			h = (h / GP->fig2dev.bx) / GP->win2fig.bx;
			break;
		case 2: /* figure */
			h = h / GP->fig2dev.bx;
			break;
		case 3: /* inches */
			h = h * GP->ipr[0];
			break;
		case 4: /* rasters */
			break;
	}
	return h;
}


/*  GMtext -- Draw text in plot margins  */

void GMtext(char *str, int side, double line, int outer, double at, int las)
{
	double a, x, y, xadj, yadj;

	if(outer) {
		switch(side) {
		case 1:
			x = at;
			y = yChartoNDC(GP->cexbase*GP->mex*(GP->oma[0]-line-1));
			x = DP->inner2dev.ax + DP->inner2dev.bx * x;
			y = DP->ndc2dev.ay + DP->ndc2dev.by * y;
			a = 0.0;
			xadj = GP->adj;
			yadj = 0.0;
			break;
		case 2:
			x = xChartoNDC(GP->cexbase*GP->mex*(GP->oma[1]-line));
			y = at;
			x = DP->ndc2dev.ax + DP->ndc2dev.bx * x;
			y = DP->inner2dev.ay + DP->inner2dev.by * y;
			a = 90.0;
			xadj = GP->adj;
			yadj = 0.0;
			break;
		case 3:
			x = at;
			y = 1.0-yChartoNDC(GP->cexbase*GP->mex*(GP->oma[2]-line));
			x = DP->inner2dev.ax + DP->inner2dev.bx * x;
			y = DP->ndc2dev.ay + DP->ndc2dev.by * y;
			a = 0.0;
			xadj = GP->adj;
			yadj = 0.0;
			break;
		case 4:
			x = 1.0-xChartoNDC(GP->cexbase*GP->mex*(GP->oma[3]-line-1));
			y = at;
			x = DP->ndc2dev.ax + DP->ndc2dev.bx * x;
			y = DP->inner2dev.ay + DP->inner2dev.by * y;
			a = 90.0;
			xadj = GP->adj;
			yadj = 0.0;
			break;
		}
		x = XINVFMAP(x);
		y = YINVFMAP(y);
		GText(x, y, str, xadj, yadj, a);
	}
	else {
		switch(side) {
		case 1:
			if(las == 2) {
				y = GP->plt[2] - yInchtoFig(yChartoInch(GP->cexbase*GP->mex*(line+GP->yLineBias)));
				x = XMAP(at) - xInchtoFig(xChartoInch(GP->cexbase*GP->mex*GP->yLineBias));
				a = 90.0;
				xadj = 1.0;
				yadj = 0.5;
			}
			else {
				y = GP->plt[2] - yInchtoFig(yChartoInch(GP->cexbase*GP->mex*(line+1-GP->yLineBias)));
				x = XMAP(at);
				a = 0.0;
				xadj = GP->adj;
				yadj = 0.0;
			}
			break;
		case 2:
			if(las == 1 || las == 2) {
				x = GP->plt[0] - xInchtoFig(xChartoInch(GP->cexbase*GP->mex*(line+GP->yLineBias)));
				y = YMAP(at) + yInchtoFig(yChartoInch(GP->cexbase*GP->mex*GP->yLineBias));
				a = 0.0;
				xadj = 1.0;
				yadj = 0.5;
			}
			else {
				x = GP->plt[0] - xInchtoFig(xChartoInch(GP->cexbase*GP->mex*(line+GP->yLineBias)));
				y = YMAP(at);
				a = 90.0;
				xadj = GP->adj;
				yadj = 0.0;
			}
			break;
		case 3:
			if(las == 2) {
				y = GP->plt[3] + yInchtoFig(yChartoInch(GP->cexbase*GP->mex*(line+GP->yLineBias)));
				x = XMAP(at) - xInchtoFig(xChartoInch(GP->cexbase*GP->mex*GP->yLineBias));
				a = 90.0;
				xadj = 0.0;
				yadj = 0.5;
			}
			else {
				y = GP->plt[3] + yInchtoFig(yChartoInch(GP->cexbase*GP->mex*(line+GP->yLineBias)));
				x = XMAP(at);
				a = 0.0;
				xadj = GP->adj;
				yadj = 0.0;
			}
			break;
		case 4:
			if(las == 1 || las == 2) {
				x = GP->plt[1] + xInchtoFig(xChartoInch(GP->cexbase*GP->mex*(line+GP->yLineBias)));
				y = YMAP(at) + yInchtoFig(yChartoInch(GP->cexbase*GP->mex*GP->yLineBias));
				a = 0.0;
				xadj = 0.0;
				yadj = 0.5;
			}
			else {
				x = GP->plt[1] + xInchtoFig(xChartoInch(GP->cexbase*GP->mex*(line+1-GP->yLineBias)));
				y = YMAP(at);
				a = 90.0;
				xadj = GP->adj;
				yadj = 0.0;
			}
			break;
		}
		GText(x, y, str, xadj, yadj, a);
	}
}


/*  GTitles -- Plot margin annotation  */

void GTitles(char *main, char *sub, char *xlab, char *ylab)
{
	double xl, yl;
	double cexsave, fontsave, xpdsave;

	if(!GP->ann) return;

	/* Conversion of ``lines'' to ``figure location'' */
	xl = fabs((GP->cra[1]*GP->mex)/GP->fig2dev.bx);
	yl = fabs((GP->cra[1]*GP->mex)/GP->fig2dev.by);

	xpdsave = GP->xpd;
	fontsave = GP->font;
	cexsave = GP->cex;
	GP->xpd = 1;

	if(main) {
		GP->font = GP->fontmain;
		GP->cex = GP->cexmain;
		GText(0.5*(GP->plt[0]+GP->plt[1]), 0.5*(GP->plt[3]+1.0),
			main, 0.5, 0.5, 0.0);
	}
	if(sub) {
		GP->font = GP->fontsub;
		GP->cex = GP->cexsub;
		GText(0.5*(GP->plt[0]+GP->plt[1]),
			GP->plt[2] - yl * (GP->mgp[0]+2.0),
			xlab, 0.5, 0.0, 0.0);
	}
	if(xlab) {
		GP->font = GP->fontlab;
		GP->cex = GP->cexlab;
		GText(0.5*(GP->plt[0]+GP->plt[1]),
			GP->plt[2] - yl * (GP->mgp[0]+1.0),
			xlab, 0.5, 0.0, 0.0);
	}
	if(ylab) {
		GP->font = GP->fontlab;
		GP->cex = GP->cexlab;
		GText(GP->plt[0] - xl * GP->mgp[0], 0.5*(GP->plt[2]+GP->plt[3]),
			ylab, 0.5, 0.0, 90.0);
	}
	GP->font = fontsave;
	GP->cex = GP->cex;
	GP->xpd = xpdsave;
}


/*  GText -- Draw text in a plot  */

void GText(double x, double y, char *str, double xc, double yc, double rot)
{
	double ix, iy, xtest, ytest;
	
	ix = XFMAP(x);
	iy = YFMAP(y);
	if(str && *str)
		if(DP->canClip) {
			GClip();
			DevText(ix, iy, str, xc, yc, rot);
		}
		else {
			if(!DP->xpd) {
				xtest = (ix-XFMAP(GP->plt[0]))*(XFMAP(GP->plt[1])-ix);
				ytest = (iy-YFMAP(GP->plt[2]))*(YFMAP(GP->plt[3])-iy);
				if(xtest < 0 || ytest < 0) return;
			}
			DevText(ix, iy, str, xc, yc, rot);
		}
}


/*  GSavePlot -- Save the current plot (only X11 at present)  */

void GSavePlot(char *name)
{
	if(DevSavePlot == DevNull)
		error("no hardcopy facilities in this graphics driver\n");
	DevSavePlot(name);
}


/*  GPrintPlot -- Print the current plot (only X11 at present)  */

void GPrintPlot(void)
{
	if(DevPrintPlot == DevNull)
		error("no hardcopy facilities in this graphics driver\n");
	DevPrintPlot();
}


/*  hsv2rgb -- HSV to RGB conversion  */
/*  Based on HSV_TO_RGB from Foley and Van Dam First Ed. Page 616 */
/*  See Alvy Ray Smith, Color Gamut Transform Pairs, SIGGRAPH '78 */

void hsv2rgb(double h, double s, double v, double *r, double *g, double *b)
{
	double	f, p, q, t;
	double	modf();
	int i;

	h = 6 * modf(h, &f);
	i = floor(h);
	f = h - i;
	p = v * (1 - s);
	q = v * (1 - s * f);
	t = v * (1 - (s * (1 - f)));
	switch (i) {
	case 0: 
		*r = v; 
		*g = t; 
		*b = p; 
		break;
	case 1: 
		*r = q; 
		*g = v; 
		*b = p; 
		break;
	case 2: 
		*r = p; 
		*g = v; 
		*b = t; 
		break;
	case 3: 
		*r = p; 
		*g = q; 
		*b = v; 
		break;
	case 4: 
		*r = t; 
		*g = p; 
		*b = v; 
		break;
	case 5: 
		*r = v; 
		*g = p; 
		*b = q; 
		break;
	default: 
		error("bad hsv to rgb color conversion\n");
	}
}
