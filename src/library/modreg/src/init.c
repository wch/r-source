/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001   The R Development Core Team.
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <R.h>
#include "modreg.h"
#include <R_ext/Rdynload.h>

R_CMethodDef CEntries[] = {
    {"BDRksmooth", (DL_FUNC) &BDRksmooth, 8},  
    {"loess_raw", (DL_FUNC) &loess_raw, 24},  
    {"loess_dfit", (DL_FUNC) &loess_dfit, 13},  
    {"loess_dfitse", (DL_FUNC) &loess_dfitse, 16},  
    {"loess_ifit", (DL_FUNC) &loess_ifit, 8},  
    {"loess_ise", (DL_FUNC) &loess_ise, 15},  
    {NULL, NULL, 0}
};

R_FortranMethodDef FortEntries[] = {
    {"lowesw", (DL_FUNC) &F77_SUB(lowesw), 4},  
    {"lowesp", (DL_FUNC) &F77_SUB(lowesp), 7},  
    {"setppr", (DL_FUNC) &F77_SUB(setppr), 6},  
    {"smart", (DL_FUNC) &F77_SUB(smart), 16},  
    {"pppred", (DL_FUNC) &F77_SUB(pppred), 5},  
    {"qsbart", (DL_FUNC) &F77_SUB(qsbart), 21},  
    {"bvalus", (DL_FUNC) &F77_SUB(bvalus), 7},  
    {"supsmu", (DL_FUNC) &F77_SUB(supsmu), 10},  
    {NULL, NULL, 0}
};

void R_init_modreg(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, FortEntries, NULL);
}
