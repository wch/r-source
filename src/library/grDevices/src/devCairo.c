/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 2011-2014     The R Core Team
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

#include <Defn.h>


#include <R_ext/Rdynload.h>
int R_cairoCdynload(int local, int now);

typedef SEXP (*R_cairo)(SEXP args);
typedef SEXP (*R_cairoVersion_t)(void);

static R_cairo R_devCairo;
static R_cairoVersion_t R_cairoVersion;

static int Load_Rcairo_Dll(void)
{
    static int initialized = 0;
 
    if (initialized) return initialized;
    initialized = -1;

    int res = R_cairoCdynload(1, 1);
    if(!res) return initialized;
    R_devCairo = (R_cairo) R_FindSymbol("in_Cairo", "cairo", NULL);
    if (!R_devCairo) error("failed to load cairo DLL");
    R_cairoVersion = (R_cairoVersion_t) R_FindSymbol("in_CairoVersion", "cairo", NULL);
    initialized = 1;
    return initialized;
}


SEXP devCairo(SEXP args)
{
    if (Load_Rcairo_Dll() < 0) warning("failed to load cairo DLL");
    else (R_devCairo)(args);
    return R_NilValue;
}

SEXP cairoVersion(void)
{
#ifdef HAVE_WORKING_CAIRO
    if (Load_Rcairo_Dll() < 0) return mkString("");
    else return (R_cairoVersion)();
#else
    return mkString("");
#endif
}
