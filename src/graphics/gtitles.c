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
