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

#include "Graphics.h"

static void GMapSpecialFig2Dev(void);

#define XNMAP1(x) (GP->ndc2dev.ax+GP->ndc2dev.bx*(x))
#define YNMAP1(y) (GP->ndc2dev.ay+GP->ndc2dev.by*(y))
#define XFMAP1(x) (GP->fig2dev.ax+GP->fig2dev.bx*(x))
#define YFMAP1(y) (GP->fig2dev.ay+GP->fig2dev.by*(y))
#define XIMAP1(x) (GP->inner2dev.ax+GP->inner2dev.bx*(x))
#define YIMAP1(y) (GP->inner2dev.ay+GP->inner2dev.by*(y))


void GMapping(int which)
{
	switch(which) {
		case 0:
			if(GP->mfg[1] > 0) GMapNDC2Dev();
		case 1:
			if(GP->mfg[1] > 0) GMapInner2Dev();
		case 2:
			if(GP->mfg[1] > 0)
				GMapFig2Dev();
			else
				GMapSpecialFig2Dev();
		case 3:
			GSetViewPort();
	}
}

/* Hide the device behind an NDC layer */
/* Use this coordinate system for outer margin coordinates */
void GMapNDC2Dev(void)
{
	GP->ndc2dev.bx = DP->ndc2dev.bx = (GP->right - GP->left);
	GP->ndc2dev.ax = DP->ndc2dev.ax = GP->left;
	GP->ndc2dev.by = DP->ndc2dev.by = (GP->top - GP->bottom);
	GP->ndc2dev.ay = DP->ndc2dev.ay = GP->bottom;
}

/* Normalised coordinates for the inner (non-margin region) */
/* Use this coordinate system for setting up multiple figures */
/* This is also used when specifying the figure region directly */
/* Note that this is incompatible with S which uses then entire */
/* device surface for such a plot */
void GMapInner2Dev(void)
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

/* Normalised coordinates for the figure region */
void GMapFig2Dev(void)
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

/* Normalised coordinates for the figure region */
/* This is called into play when par(fig=) is used */
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

/* Normalized coordinates for the plot region */
void GSetViewPort(void)
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

/* World->Figure Mapping */
/* Checks on ranges ? */
void GMapWin2Fig(void)
{
	GP->win2fig.bx = DP->win2fig.bx = (GP->plt[1] - GP->plt[0])/(GP->usr[1] - GP->usr[0]);
	GP->win2fig.ax = DP->win2fig.ax = GP->plt[0] - GP->win2fig.bx * GP->usr[0];
	GP->win2fig.by = DP->win2fig.by = (GP->plt[3] - GP->plt[2])/(GP->usr[3] - GP->usr[2]);
	GP->win2fig.ay = DP->win2fig.ay = GP->plt[2] - GP->win2fig.by * GP->usr[2];
}
