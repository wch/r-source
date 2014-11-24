/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2004-12   The R Core Team.
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#include <Rinternals.h>
#include <R_ext/Boolean.h>
#include <R_ext/GraphicsEngine.h> /* for DevDesc */

#ifdef ENABLE_NLS
#include <libintl.h>
#undef _
#define _(String) dgettext ("grDevices", String)
#else
#define _(String) (String)
#endif

SEXP R_CreateAtVector(SEXP axp, SEXP usr, SEXP nint, SEXP is_log);
SEXP R_GAxisPars(SEXP usr, SEXP is_log, SEXP nintLog);

SEXP PicTeX(SEXP);

SEXP PostScript(SEXP);
SEXP XFig(SEXP);
SEXP PDF(SEXP);
SEXP Type1FontInUse(SEXP, SEXP);
SEXP CIDFontInUse(SEXP, SEXP);

#ifndef _WIN32
SEXP Quartz(SEXP);
SEXP makeQuartzDefault();

SEXP X11(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP savePlot(SEXP call, SEXP op, SEXP args, SEXP rho);
#endif

SEXP devCairo(SEXP);

Rboolean
PSDeviceDriver(pDevDesc, const char*, const char*, const char*,
	       const char **, const char*, const char*, const char*,
	       double, double, Rboolean, double, Rboolean, Rboolean,
	       Rboolean, const char*, const char*, SEXP, const char*, int,
	       Rboolean);

Rboolean
PDFDeviceDriver(pDevDesc, const char *, const char *, const char *,
		const char **, const char *, const char *, const char *,
		double, double, double, int, int, const char*, SEXP,
		int, int, const char *, int, int, Rboolean, Rboolean);

#ifdef _WIN32
SEXP devga(SEXP);
SEXP savePlot(SEXP);
SEXP bringToTop(SEXP, SEXP);
SEXP msgWindow(SEXP, SEXP);
#endif

SEXP devcap(SEXP args);
SEXP devcapture(SEXP args);
SEXP devcontrol(SEXP args);
SEXP devcopy(SEXP args);
SEXP devcur(SEXP args);
SEXP devdisplaylist(SEXP args);
SEXP devholdflush(SEXP args);
SEXP devnext(SEXP args);
SEXP devoff(SEXP args);
SEXP devprev(SEXP args);
SEXP devset(SEXP args);
SEXP devsize(SEXP args);

SEXP chull(SEXP x);

SEXP contourLines(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP getSnapshot(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP playSnapshot(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP getGraphicsEvent(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP getGraphicsEventEnv(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP setGraphicsEventEnv(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP devAskNewPage(SEXP call, SEXP op, SEXP args, SEXP env);

#ifndef DEVWINDOWS
SEXP rgb(SEXP r, SEXP g, SEXP b, SEXP a, SEXP MCV, SEXP nam);
SEXP hsv(SEXP h, SEXP s, SEXP v, SEXP a);
SEXP hcl(SEXP h, SEXP c, SEXP l, SEXP a, SEXP sfixup);
SEXP gray(SEXP lev, SEXP a);
SEXP colors(void);
SEXP col2rgb(SEXP colors, SEXP alpha);
SEXP palette(SEXP value);
SEXP palette2(SEXP value);
SEXP RGB2hsv(SEXP rgb);
#endif

unsigned int inRGBpar3(SEXP, int, unsigned int);
const char *incol2name(unsigned int col);
unsigned int inR_GE_str2col(const char *s);
void initPalette(void);

SEXP cairoVersion(void);
