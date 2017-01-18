/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-2017   The R Core Team.
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
 *  https://www.R-project.org/Licenses/
 */

#include <R.h>
#include <Rinternals.h>
#include "tcltk.h"
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

#define C_DEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CMethodDef CEntries[] = {
#ifdef Win32
    C_DEF(tcltk_start, 0),
    C_DEF(tcltk_end, 0),
#else
    C_DEF(tcltk_init, 1),
    C_DEF(RTcl_ActivateConsole, 0),
#endif
    {NULL, NULL, 0}
};

#define EXTDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_ExternalMethodDef ExternEntries[] = {
    EXTDEF(dotTcl, -1),
    EXTDEF(dotTclObjv, 1),
    EXTDEF(dotTclcallback, -1),
    EXTDEF(RTcl_ObjFromVar, 1),
    EXTDEF(RTcl_AssignObjToVar, 2),
    EXTDEF(RTcl_StringFromObj, 1),
    EXTDEF(RTcl_ObjAsCharVector, 1),
    EXTDEF(RTcl_ObjAsDoubleVector, 1),
    EXTDEF(RTcl_ObjAsIntVector, 1),
    EXTDEF(RTcl_ObjAsRawVector, 1),
    EXTDEF(RTcl_ObjFromCharVector, 2),
    EXTDEF(RTcl_ObjFromDoubleVector, 2),
    EXTDEF(RTcl_ObjFromIntVector, 2),
    EXTDEF(RTcl_ObjFromRawVector, 1),
    /* (..FromRaw... has only 1 arg, no drop=) */
    EXTDEF(RTcl_ServiceMode, 1),
    EXTDEF(RTcl_GetArrayElem, 2),
    EXTDEF(RTcl_RemoveArrayElem, 2),
    EXTDEF(RTcl_SetArrayElem, 3),
    {NULL, NULL, 0}
};


void attribute_visible R_init_tcltk(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, ExternEntries);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, FALSE);
}

