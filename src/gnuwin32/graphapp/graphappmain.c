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

#define NONAMELESSUNION
#include "internal.h"

extern void startgraphapp(HINSTANCE Instance, HINSTANCE PrevInstance, int CmdShow);


__declspec(dllimport) extern unsigned int R_reserved_size;
#include <limits.h>

static void check_max_mem(int argc, char **argv)
{
    int ac = argc;
    char *p = NULL, **av = argv;
    long v;

    while (--ac) {
	++av;
	if(strncmp(*av, "--max-mem-size", 14) == 0) {
	    if(strlen(*av) < 16) {
		ac--; av++; p = *av;
	    } else p = &(*av)[15];
	    v = strtol(p, &p, 10);
	    if(p[0] == 'M') {
		if((1024*1024 * (double)v) > LONG_MAX) return;
		v = 1024*1024*v;
	    } else if(p[0] == 'K') {
		if((1024 * (double)v) > LONG_MAX) return;
		v = 1024*v;
	    } else if(p[0] == 'k') {
		if((1000 * (double)v) > LONG_MAX) return;
		v = 1000*v;
	    }
#ifdef LEA_MALLOC
	    if (v > R_reserved_size) R_reserved_size = v;
#endif
	    return;
	}
    }
}

/*
 *  If PASS_ARGS is zero, the main function will be passed zero
 *  and NULL instead of argc and argv.
 *  If it is one, the argc and argv parameters will be passed.
 *  If it is two, the environ parameter will also be passed.
 *  We define the main function as returning void, so this
 *  method ignores any value returned from main.
 */

int PASCAL
WinMain (HINSTANCE Instance, HINSTANCE PrevInstance, LPSTR CmdLine,
	 int CmdShow)
{
#if (PASS_ARGS > 1) /* define argc, argv, environ */
	extern int _argc;
	extern char **_argv;
	extern char **environ;
	extern void AppMain(int argc, char **argv, char **envp);
#elif (PASS_ARGS > 0) /* only define argc and argv */
	extern int _argc;
	extern char **_argv;
	extern void AppMain(int argc, char **argv);
#else /* else pass zero and NULL to main */
	extern void AppMain(int argc, char **argv);
#endif /* end arg declarations */

	/* do this here, before ANY used of malloc + friends */
	check_max_mem(_argc, _argv);
        startgraphapp(Instance, PrevInstance, CmdShow);
	/*
	 *  Call the main function now.
	*/
#if (PASS_ARGS > 1)		/* pass argc, argv, environ */
	AppMain(_argc, _argv, environ);
#elif (PASS_ARGS > 0)	/* only pass argc and argv */
	AppMain(_argc, _argv);
#else			/* pass zero and NULL */
	AppMain(0, NULL);
#endif

	/*
	 *  Call the mainloop function to handle events.
	 */


	return 0;
}
