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

#define XINVFMAP(x) ((x - GP->fig2dev.ax)/GP->fig2dev.bx)
#define YINVFMAP(y) ((y - GP->fig2dev.ay)/GP->fig2dev.by)

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
