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

#include "ctest.h"
#include "R_ext/Rdynload.h"

static const R_CMethodDef cmethods[]  = {
  {"chisqsim", (DL_FUNC) &chisqsim, 11},  
  {"d2x2xk", (DL_FUNC) &d2x2xk, 5},
  {"dansari", (DL_FUNC) &dansari, 4},
  {"fexact",   (DL_FUNC) &fexact, 10},
  {"kendall_tau", (DL_FUNC) &kendall_tau, 4},
  {"pansari",  (DL_FUNC)&pansari, 4},
  {"pkendall", (DL_FUNC)  &pkendall, 3},
  {"pkstwo", (DL_FUNC) &pkstwo, 3},
  {"prho", (DL_FUNC) &prho, 5},
  {"psmirnov2x", (DL_FUNC) &psmirnov2x, 3},
  {"qansari",  (DL_FUNC) &qansari, 4},
  {"swilk", (DL_FUNC) &swilk, 9},
  {NULL, NULL, 0}
};

void R_init_ctest(DllInfo *dll)
{
    R_registerRoutines(dll, cmethods, NULL, NULL, NULL);
}
