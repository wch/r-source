/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2003 The R Development Core Team
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#if defined(HAVE_X11)

#include <R_ext/RX11.h>	     /* typedefs for the module routine types */

static R_X11Routines routines, *ptr = &routines;

static int initialized = 0;

R_X11Routines *
R_setX11Routines(R_X11Routines *routines)
{
    R_X11Routines *tmp;
    tmp = ptr;
    ptr = routines;
    return tmp;
}

static void X11_Init(void)
{
    int res;

    initialized = -1;
    if(strcmp(R_GUIType, "X11") && strcmp(R_GUIType, "GNOME") &&
	    strcmp(R_GUIType, "Tk")) {
	warning("X11 module is not available under this GUI");
	return;
    }
    res = moduleCdynload("R_X11", 1, 1);
    if(!res) return;
    initialized = 1;    
    return;
}


SEXP do_X11(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    if(!initialized) X11_Init();
    if(initialized > 0)
	return (*ptr->X11)(call, op, args, rho);
    else {
	error("R_X11 module cannot be loaded");
	return R_NilValue;
    }
}

SEXP do_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    if(!initialized) X11_Init();
    if(initialized > 0)
	return (*ptr->de)(call, op, args, rho);
    else {
	error("R_X11 module cannot be loaded");
	return R_NilValue;
    }
}

Rboolean R_GetX11Image(int d, void *pximage, int *pwidth, int *pheight)
{
    if(!initialized) X11_Init();
    if(initialized > 0)
	return (*ptr->image)(d, pximage, pwidth, pheight);
    else {
	error("R_X11 module cannot be loaded");
	return FALSE;
    }
}

#else

SEXP do_X11(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("X11 is not available");
    return R_NilValue;
}

SEXP do_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("X11 is not available");
    return R_NilValue;
}

Rboolean R_GetX11Image(int d, void *pximage, int *pwidth, int *pheight)
{
    error("X11 is not available");
    return FALSE;
}
#endif
