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
#include "mva.h"
#include "R_ext/Rdynload.h"

static const R_CMethodDef CEntries[] = {
    {"dblcen", (DL_FUNC) &dblcen, 2},  
    {"R_cutree", (DL_FUNC) &R_cutree, 2},  
/*
  Called with NAOK and DUP which are passed down to give 8 args.
  Fix naokfind()
    {"R_distance", (DL_FUNC) &R_distance, 6},  
*/
    {NULL, NULL, 0}
};

static const R_FortranMethodDef FortEntries[] = {
    {"hclust", (DL_FUNC) &F77_SUB(hclust), 11}, 
    {"hcass2", (DL_FUNC) &F77_SUB(hcass2), 6},  
    {"kmns", (DL_FUNC) &F77_SUB(kmns), 17},  
    {NULL, NULL, 0}
};

void R_init_mva(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, FortEntries, NULL);
}
