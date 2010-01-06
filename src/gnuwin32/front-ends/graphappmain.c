/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: graphappmain.c (original routines in init.c)
 * Platform: Windows  Version: 2.40
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 * Version: 1.60  Changes: drawarc/fillarc(r,0,360) now encloses.
 *    New fillellipse() function replaces Windows Ellipse().
 * Version: 2.00  Changes: New class system implemented.
 * Version: 2.02  Changes: Added support for functions like MoveToEx.
 * Version: 2.15  Changes: Fixed brush origins problem.
 * Version: 2.20  Changes: Moved some arrays from context.c to here.
 * Version: 2.40  Changes: Moved drawimage to this file.
 */

/* Copyright (C) 1993-1998 Lachlan Patrick

   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.
*/

#include <windows.h>

/* The mingw-runtime startup code has _argc and _argv as visible symbols,
   as do the MS compilers.  But the mingw-w64-crt is different */

extern void 
GA_startgraphapp(HINSTANCE Instance, HINSTANCE PrevInstance, int CmdShow);


int PASCAL
WinMain (HINSTANCE Instance, HINSTANCE PrevInstance, LPSTR CmdLine,
	 int CmdShow)
{
#ifdef WIN64
    extern int     __argc;
    extern char ** __argv;
    extern int main(int, char**);

    main(__argc, __argv);
#else
    extern int _argc;
    extern char **_argv;
    extern void AppMain(int argc, char **argv);

    GA_startgraphapp(Instance, PrevInstance, CmdShow);
    AppMain(_argc, _argv);
#endif

    return 0;
}
