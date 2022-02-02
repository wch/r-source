/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: init.c -- library initialisation code.
 * Platform: Windows  Version: 2.23  Date: 1997/09/09
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 * Version: 2.00  Changes: New object class system.
 * Version: 2.20  Changes: Added EasyWin support in Borland C.
 * Version: 2.22  Changes: Now main doesn't use environ pointer.
 */

/* Copyright (C) 1993-1998 Lachlan Patrick

   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.
*/

#include "internal.h"

/*
 *  Windows implementation globals.
 */

PROTECTED char * app_name = "GraphApp";
PROTECTED int    app_initialised = 0;
PROTECTED HANDLE this_instance = NULL;
PROTECTED HANDLE prev_instance = NULL;

/*
 *  Functions defined in this file.
 */
static void setappname(char *title);
static void getappname(HANDLE Instance);

int initapp(int argc, char **argv);
void exitapp(void);
void gabeep(void);

/*
 *  Initialise application. Returns zero on failure.
 */
int initapp(int argc, char **argv)
{
    if (! app_initialised)
    {
	app_initialised = 1;
	init_objects();
	init_events();
	init_fonts();
	init_cursors();
	init_contexts();
	init_menus();
    }
    return (argc < 1) ? 1 : argc;
}

/*
 *  This function releases all GDI objects.
 */
PROTECTED
void app_cleanup(void)
{
    if (app_initialised) {
	app_initialised = 0;
	finish_contexts();
	finish_objects();
	finish_events();
    }
}

/*
 *  Standard quit application function.
 *  This function releases all GDI objects and terminates the program.
 *  It can be called at any time from within a program, or at the end,
 *  or else it will be called if all windows are closed.
 */
void exitapp(void)
{
    app_cleanup();
    active_windows = 0;
    exit(0);
}

/*
 *  Run an application program:
 */
int execapp(char *app)
{
#ifdef __MSDOS__
    if (WinExec(app, SW_SHOW) < 32)
	return 0;
#else
    if (system(app) != 0)
	return 0;
#endif
    return 1;
}



/*
 *  Play error sound
 */
void gabeep(void)
{
    MessageBeep(MB_ICONASTERISK);
}

/*
 *  Take a name like "PROG.EXE" and set app_name to be "Prog".
 */
static void setappname(char *title)
{
    int len;
    char c;

    if (title[0] == '\0')
	return;
    for (len=1; (c=title[len]) != '\0'; len++)
	title[len] = tolower(c);
    if ((len-=4)<0)
	len = 0;
    if (! string_diff(title+len, ".exe"))
	title[len] = '\0';

    app_name = new_string(title);
}

/*
 *  Find out what the application is called.
 */
static void getappname(HANDLE Instance)
{
    char exename[MAX_PATH];
    char title[MAX_PATH];

    /* find out executable name */
    GetModuleFileName(Instance, exename, sizeof(exename));
    GetFileTitle(exename, title, sizeof(title));
    setappname(title);
}


/*
 *  The main Windows entry point is the WinMain function.
 */



void startgraphapp(HINSTANCE Instance, HINSTANCE PrevInstance, int CmdShow)
{
    /*
     *  Save some variables for later.
     */
    this_instance = Instance;
    /* prev_instance = PrevInstance; is always NULL */

    /*
     *  Initialise the graphical interface.
     */
    initapp(0, NULL);
    getappname(Instance);
}

