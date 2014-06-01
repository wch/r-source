/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2012     the R Core Team
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

#include <config.h>
#include <Defn.h>
#include <Internal.h>
#include "grDevices.h"

#ifndef _WIN32
SEXP do_X11(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_saveplot(SEXP call, SEXP op, SEXP args, SEXP env);

SEXP X11(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return do_X11(call, op, CDR(args), env);
}

SEXP savePlot(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return do_saveplot(call, op, CDR(args), env);
}
#endif

SEXP contourLines(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return do_contourLines(call, op, CDR(args), env);
}

SEXP getSnapshot(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return do_getSnapshot(call, op, CDR(args), env);
}

SEXP playSnapshot(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return do_playSnapshot(call, op, CDR(args), env);
}

SEXP getGraphicsEvent(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return do_getGraphicsEvent(call, op, CDR(args), env);
}

SEXP getGraphicsEventEnv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return do_getGraphicsEventEnv(call, op, CDR(args), env);
}

SEXP setGraphicsEventEnv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return do_setGraphicsEventEnv(call, op, CDR(args), env);
}

#ifdef _WIN32
SEXP bringtotop(SEXP sdev, SEXP sstay);
SEXP msgwindow(SEXP sdev, SEXP stype);


SEXP bringToTop(SEXP sdev, SEXP sstay)
{
    return bringtotop(sdev, sstay);
}

SEXP msgWindow(SEXP sdev, SEXP stype)
{
    return msgwindow(sdev, stype);
}

#endif


#include <R_ext/GraphicsEngine.h>

SEXP devAskNewPage(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int ask;
    pGEDevDesc gdd = GEcurrentDevice();
    Rboolean oldask = gdd->ask;

    args = CDR(args);
    if (!isNull(CAR(args))) {
	ask = asLogical(CAR(args));
	if (ask == NA_LOGICAL) error(_("invalid '%s' argument"), "ask");
	gdd->ask = ask;
	R_Visible = FALSE;
    } else R_Visible = TRUE;

    return ScalarLogical(oldask);
}


