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
#include "string.h"

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
