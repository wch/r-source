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

double xNDCtoChar(double x) { return x / GP->xNDCPerChar; }
double yNDCtoChar(double y) { return y / GP->yNDCPerChar; }
double xChartoNDC(double x) { return x * GP->xNDCPerChar; }
double yChartoNDC(double y) { return y * GP->yNDCPerChar; }

double xNDCtoInch(double x) { return x / GP->xNDCPerInch; }
double yNDCtoInch(double y) { return y / GP->yNDCPerInch; }
double xInchtoNDC(double x) { return x * GP->xNDCPerInch; }
double yInchtoNDC(double y) { return y * GP->yNDCPerInch; }

double xInchtoChar(double x) { return x * (GP->xNDCPerInch/GP->xNDCPerChar); }
double yInchtoChar(double y) { return y * (GP->yNDCPerInch/GP->yNDCPerChar); }
double xChartoInch(double x) { return x * (GP->xNDCPerChar/GP->xNDCPerInch); }
double yChartoInch(double y) { return y * (GP->yNDCPerChar/GP->yNDCPerInch); }

double xFigtoInch(double x) { return xNDCtoInch(x * GP->fig2dev.bx/GP->ndc2dev.bx); }
double yFigtoInch(double y) { return yNDCtoInch(y * GP->fig2dev.by/GP->ndc2dev.by); }
double xInchtoFig(double x) { return xInchtoNDC(x * GP->ndc2dev.bx/GP->fig2dev.bx); }
double yInchtoFig(double y) { return yInchtoNDC(y * GP->ndc2dev.by/GP->fig2dev.by); }

double xUsrtoInch(double x) { return xFigtoInch(x * GP->win2fig.bx); }
double yUsrtoInch(double y) { return yFigtoInch(y * GP->win2fig.by); }
