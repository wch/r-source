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

#include "R.h"
#include "ts.h"
#include "R_ext/Rdynload.h"

R_CMethodDef CEntries[] = {
    {"acf", (DL_FUNC) &acf, 6},  
    {"uni_pacf", (DL_FUNC) &uni_pacf, 3},  
    {"artoma", (DL_FUNC) &artoma, 4},  
    {"burg", (DL_FUNC) &burg, 6},  
    {"multi_burg", (DL_FUNC) &multi_burg, 11},  
    {"multi_yw", (DL_FUNC) &multi_yw, 10},  
    {"setup_starma", (DL_FUNC) &setup_starma, 7},  
    {"Dotrans", (DL_FUNC) &Dotrans, 2},  
    {"get_s2", (DL_FUNC) &get_s2, 1},  
    {"get_resid", (DL_FUNC) &get_resid, 1},  
    {"arma0fa", (DL_FUNC) &arma0fa, 2},  
    {"arma0_kfore", (DL_FUNC) &arma0_kfore, 5},  
    {"free_starma", (DL_FUNC) &free_starma, 0},  
    {"R_intgrt_vec", (DL_FUNC) &R_intgrt_vec, 4},  
/*
 Called with NAOK in filter()
    {"filter1", (DL_FUNC) &filter1, 7},  
    {"filter2", (DL_FUNC) &filter2, 5},  
*/
    {"R_pp_sum", (DL_FUNC) &R_pp_sum, 4},  
    {NULL, NULL, 0}
};

R_FortranMethodDef FortEntries[] = {
    {"eureka", (DL_FUNC) &F77_SUB(eureka), 6},  
    {"stl", (DL_FUNC) &F77_SUB(stl), 18},  
    {NULL, NULL, 0}
};

void R_init_ts(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, FortEntries, NULL);
}
