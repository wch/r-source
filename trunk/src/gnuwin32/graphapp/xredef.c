/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: xredef.c -- redefinitions for cross-platform portability.
 * Platform: Neutral  Version: 2.20  Date: 1997/05/05
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 */

/* Copyright (C) 1993-1997 Lachlan Patrick

   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.
*/

#include "internal.h"

/*
 *  This file only exists to allow GraphApp code to link to the
 *  real function names defined in the graphapp.h header file.
 *
 *  Certain functions are redefined in the header file because
 *  the Macintosh  platform rejects them.
 *  Rather than redesign the library to avoid these name-clashes,
 *  the function names are defined differently using macros.
 *
 *  This file defines the real function names. Usually this will
 *  be unnecessary, but it's included here for completeness.
 */

#ifdef REDEFINE_FUNC_NAMES
#undef addpt
#undef subpt
#undef equalpt
#undef newmenu
#undef newcontrol
#undef newwindow

point addpt(point p1, point p2);
point subpt(point p1, point p2);
int equalpt(point p1, point p2);
menu newmenu(char *name);
control newcontrol(char *text, rect r);
window newwindow(char *name, rect r, long flags);

point addpt(point p1, point p2)
{
	p1.x += p2.x;
	p1.y += p2.y;
	return p1;
}

point subpt(point p1, point p2)
{
	p1.x -= p2.x;
	p1.y -= p2.y;
	return p1;
}

int equalpt(point p1, point p2)
{
	if ((p1.x==p2.x) && (p1.y==p2.y))
		return 1;
	else
		return 0;
}

menu newmenu(char *name)
{
	return GA_newmenu(name);
}

control newcontrol(char *text, rect r)
{
	return GA_newcontrol(text, r);
}

window newwindow(char *name, rect r, long flags)
{
	return GA_newwindow(name, r, flags);
}

#endif

