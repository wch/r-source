/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000--2007  The R Development Core Team
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

#include <tcl.h>
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>

void tcltk_init();

extern __declspec(dllimport) void (* R_tcldo)();

static void _R_tcldo()
{
    Tcl_ServiceAll();
}

static void (* old_R_tcldo)();

void tcltk_start()
{
    HWND active = GetForegroundWindow(); /* ActiveTCL steals the focus */
    tcltk_init(); /* won't return on error */
    old_R_tcldo = R_tcldo;
    R_tcldo = &_R_tcldo;
    _R_tcldo();  /* one call to trigger the focus stealing bug */
    SetForegroundWindow(active); /* and fix it */
}

void tcltk_end()
{
    R_tcldo = old_R_tcldo;
}
