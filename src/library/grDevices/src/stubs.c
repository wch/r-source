/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-12     the R Core Team
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
