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
#include "eda.h"
#include "R_ext/Rdynload.h"

R_CMethodDef CEntries[] = {
    {"Rsm_3RSR", (DL_FUNC) &Rsm_3RSR, 5},  
    {"Rsm_3RSS", (DL_FUNC) &Rsm_3RSS, 5},
    {"Rsm_3RS3R", (DL_FUNC) &Rsm_3RS3R, 5},  
    {"Rsm_3R", (DL_FUNC) &Rsm_3R, 5},  
    {"Rsm_3", (DL_FUNC) &Rsm_3, 5},  
    {"Rsm_S", (DL_FUNC) &Rsm_S, 5},  
    {NULL, NULL, 0}
};


void R_init_eda(DllInfo *dll)
{
/*
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
*/
}

