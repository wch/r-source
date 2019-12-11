/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2004-2017   The R Core Team.
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
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R.h>
#include <Rinternals.h>
#include <R_ext/GraphicsEngine.h>

#include "grDevices.h"
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

#ifndef _WIN32
/* This really belongs with the X11 module, but it is about devices */
static SEXP cairoProps(SEXP in)
{
    int which = asInteger(in);
    if(which == 1)
	return ScalarLogical(
#ifdef HAVE_WORKING_CAIRO
	    1
#else
	    0
#endif
	    );
    else if(which == 2)
	return ScalarLogical(
#ifdef HAVE_PANGOCAIRO
	    1
#else
	    0
#endif
	    );
    return R_NilValue;
}
#endif

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef CallEntries[] = {
    CALLDEF(Type1FontInUse, 2),
    CALLDEF(CIDFontInUse, 2),
    CALLDEF(R_CreateAtVector, 4),
    CALLDEF(R_GAxisPars, 3),
    CALLDEF(chull, 1),
    CALLDEF(gray, 2),
    CALLDEF(RGB2hsv, 1),
    CALLDEF(rgb, 6),
    CALLDEF(hsv, 4),
    CALLDEF(hcl, 5),
    CALLDEF(col2rgb, 2),
    CALLDEF(colors, 0),
    CALLDEF(palette, 1),
    CALLDEF(palette2, 1),
    CALLDEF(cairoVersion, 0),
    CALLDEF(bmVersion, 0),

#ifndef _WIN32
    CALLDEF(makeQuartzDefault, 0),
    CALLDEF(cairoProps, 1),
#else
    CALLDEF(bringToTop, 2),
    CALLDEF(msgWindow, 2),
#endif
    {NULL, NULL, 0}
};

#define EXTDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_ExternalMethodDef ExtEntries[] = {
    EXTDEF(PicTeX, 6),
    EXTDEF(PostScript, 19),
    EXTDEF(XFig, 14),
    EXTDEF(PDF, 20),
    EXTDEF(devCairo, 11),
    EXTDEF(devcap, 0),
    EXTDEF(devcapture, 1),
    EXTDEF(devcontrol, 1),
    EXTDEF(devcopy, 1),
    EXTDEF(devcur, 0),
    EXTDEF(devdisplaylist, 0),
    EXTDEF(devholdflush, 1),
    EXTDEF(devnext, 1),
    EXTDEF(devoff, 1),
    EXTDEF(devprev, 1),
    EXTDEF(devset, 1),
    EXTDEF(devsize, 0),
    EXTDEF(contourLines, 4),
    EXTDEF(getSnapshot, 0),
    EXTDEF(playSnapshot, 1),
    EXTDEF(getGraphicsEvent, 1),
    EXTDEF(getGraphicsEventEnv, 1),
    EXTDEF(setGraphicsEventEnv, 2),
    EXTDEF(setPattern, 1),
    EXTDEF(devAskNewPage, 1),

#ifdef _WIN32
    EXTDEF(savePlot, 4),
    EXTDEF(devga, 21),
#else
    EXTDEF(savePlot, 3),
    EXTDEF(Quartz, 11),
    EXTDEF(X11, 17),
#endif
    {NULL, NULL, 0}
};

#ifdef HAVE_AQUA
extern void setup_RdotApp(void);
extern Rboolean useaqua;
#endif

void attribute_visible R_init_grDevices(DllInfo *dll)
{
    initPalette();
    R_registerRoutines(dll, NULL, CallEntries, NULL, ExtEntries);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);

#ifdef HAVE_AQUA
/* R.app will run event loop, so if we are running under that we don't
   need to run one here */
    if(useaqua) setup_RdotApp();
#endif
}
