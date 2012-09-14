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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R.h>
#include <Rinternals.h>
#include <R_ext/GraphicsEngine.h>

#include "grDevices.h"
#include <R_ext/Rdynload.h>

#ifndef WIN32
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


#ifndef WIN32
void *getQuartzAPI();
#endif

static R_CMethodDef CEntries [] = {
#ifndef WIN32
    // This is used by src/unix/aqua.c, as a symbol to be looked up
    {"getQuartzAPI", (DL_FUNC) getQuartzAPI, 0},
#endif
    {NULL, NULL, 0}
};

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef CallEntries[] = {
    // NB: each *also* needs an entry in useDynLib() in ../NAMESPACE !
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

#ifndef WIN32
    CALLDEF(makeQuartzDefault, 0),
    CALLDEF(cairoProps, 1),
#endif
    {NULL, NULL, 0}
};

#define EXTDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_ExternalMethodDef ExtEntries[] = {
    EXTDEF(PicTeX, 6),
    EXTDEF(PostScript, 16),
    EXTDEF(XFig, 11),
    EXTDEF(PDF, 16),
    EXTDEF(devCairo, 10),
    EXTDEF(devcap, 1),
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
    EXTDEF(savePlot, 3),
    EXTDEF(contourLines, 5),
    EXTDEF(getSnapshot, 0),
    EXTDEF(playSnapshot, 1),
    EXTDEF(getGraphicsEvent, 1),
    EXTDEF(getGraphicsEventEnv, 1),
    EXTDEF(setGraphicsEventEnv, 2),
    EXTDEF(colors, 0),
    EXTDEF(col2rgb, 1),
    EXTDEF(palette, 1),
    EXTDEF(devAskNewPage, 1),

#ifdef WIN32
    EXTDEF(devga, 19),
#else
    EXTDEF(Quartz, 12),
    EXTDEF(X11, 17),
#endif
    {NULL, NULL, 0}
};

void R_init_grDevices(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, ExtEntries);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
