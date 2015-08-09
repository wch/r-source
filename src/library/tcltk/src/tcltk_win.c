/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000--2013  The R Core Team
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

#include "tcltk.h"

#include <R_ext/Boolean.h>
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>

Rboolean R_ToplevelExec(void (*fun)(void *), void *data);

/* We don't need a re-entrancy guard on Windows as Tcl_ServiceAll
   has one -- it uses serviceMode, setting it to TCL_SERVICE_NONE
   whilst it is running.  Unlike TclDoOneEvent, it checks on entry.
 */
static void TclSpinLoop(void *data)
{
    Tcl_ServiceAll();
}

static void _R_tcldo(void)
{
    (void) R_ToplevelExec(TclSpinLoop, NULL);
}

/* import from src/gnuwin32/system.c -- private, so in no header */
typedef void (*DO_FUNC)();
extern void set_R_Tcldo(DO_FUNC ptr);
extern void unset_R_Tcldo(DO_FUNC ptr);
static int TkUp = 0, Tcl_polled = 0;


void tcltk_start(void)
{
    HWND active = GetForegroundWindow(); /* ActiveTCL steals the focus */

    if(!TkUp) tcltk_init(&TkUp); /* won't return on error */
    if (!Tcl_polled) {
	set_R_Tcldo(&_R_tcldo);
	Tcl_polled = 1;
    }
    _R_tcldo();  /* one call to trigger the focus stealing bug */
    SetForegroundWindow(active); /* and fix it */
}

void tcltk_end(void)
{
//    Tcl_DeleteInterp(RTcl_interp);
//    Tcl_Finalize();
    unset_R_Tcldo(&_R_tcldo);
    Tcl_polled = 0;
//    TkUp = 0;
}
