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

#include <Rinternals.h>
#include <Internal.h>

#ifndef WIN32
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



SEXP rgb(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return do_rgb(call, op, CDR(args), env);
}

SEXP hsv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return do_hsv(call, op, CDR(args), env);
}

SEXP hcl(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return do_hcl(call, op, CDR(args), env);
}

SEXP gray(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return do_gray(call, op, CDR(args), env);
}

SEXP colors(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return do_colors(call, op, CDR(args), env);
}

SEXP col2rgb(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return do_col2RGB(call, op, CDR(args), env);
}

SEXP palette(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return do_palette(call, op, CDR(args), env);
}

SEXP RGB2hsv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return do_RGB2hsv(call, op, CDR(args), env);
}


