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
