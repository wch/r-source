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
