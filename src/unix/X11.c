/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2012 The R Core Team
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>

#include <Rconnections.h>
#include <Rdynpriv.h>

#ifdef HAVE_X11

#include <Rmodules/RX11.h>   /* typedefs for the module routine types */

static R_X11Routines routines, *ptr = &routines;

static int initialized = 0;

R_X11Routines * R_setX11Routines(R_X11Routines *routines)
{
    R_X11Routines *tmp;
    tmp = ptr;
    ptr = routines;
    return tmp;
}

int attribute_hidden R_X11_Init(void)
{
    int res;

    if(initialized) return initialized;

    initialized = -1;
    if(strcmp(R_GUIType, "none") == 0) {
	warning(_("X11 module is not available under this GUI"));
	return initialized;
    }
    res = R_moduleCdynload("R_X11", 1, 1);
    if(!res) return initialized;
    if(!ptr->access)
	error(_("X11 routines cannot be accessed in module"));
    initialized = 1;
    return initialized;
}

Rboolean attribute_hidden R_access_X11(void)
{
    R_X11_Init();
    return (initialized > 0) ? (*ptr->access)() > 0 : FALSE;
}

// called from src/library/grDevices/src/stubs.c
SEXP do_X11(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    R_X11_Init();
    if(initialized > 0)
	return (*ptr->X11)(call, op, args, rho);
    else {
	error(_("X11 module cannot be loaded"));
	return R_NilValue;
    }
}

// called from src/library/grDevices/src/stubs.c
SEXP do_saveplot(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    R_X11_Init();
    if(initialized > 0)
	return (*ptr->saveplot)(call, op, args, rho);
    else {
	error(_("X11 module cannot be loaded"));
	return R_NilValue;
    }
}

// exported for src/include/R_ext/GetX11Image.h (and package tkrplot)
Rboolean R_GetX11Image(int d, void *pximage, int *pwidth, int *pheight)
{
    R_X11_Init();
    if(initialized > 0)
	return (*ptr->image)(d, pximage, pwidth, pheight);
    else {
	error(_("X11 module cannot be loaded"));
	return FALSE;
    }
}

Rboolean attribute_hidden R_ReadClipboard(Rclpconn clpcon, char *type)
{
    R_X11_Init();
    if(initialized > 0)
	return (*ptr->readclp)(clpcon, type);
    else {
	error(_("X11 module cannot be loaded"));
	return FALSE;
    }
}

SEXP do_bmVersion(void)
{
   SEXP ans = PROTECT(allocVector(STRSXP, 3)),
	nms = PROTECT(allocVector(STRSXP, 3));
    setAttrib(ans, R_NamesSymbol, nms);
    SET_STRING_ELT(nms, 0, mkChar("libpng"));
    SET_STRING_ELT(nms, 1, mkChar("jpeg"));
    SET_STRING_ELT(nms, 2, mkChar("libtiff"));
    R_X11_Init();
    if(initialized > 0) {
	SET_STRING_ELT(ans, 0, mkChar((*ptr->R_pngVersion)()));
	SET_STRING_ELT(ans, 1, mkChar((*ptr->R_jpegVersion)()));
	SET_STRING_ELT(ans, 2, mkChar((*ptr->R_tiffVersion)()));
    }
    UNPROTECT(2);
    return ans;
}
#else /* No HAVE_X11 */

Rboolean attribute_hidden R_access_X11(void)
{
    return FALSE;
}

SEXP do_X11(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error(_("X11 is not available"));
    return R_NilValue;
}

SEXP do_saveplot(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error(_("X11 is not available"));
    return R_NilValue;
}

Rboolean R_GetX11Image(int d, void *pximage, int *pwidth, int *pheight)
{
    error(_("X11 is not available"));
    return FALSE;
}

Rboolean attribute_hidden R_ReadClipboard(Rclpconn con, char *type)
{
    error(_("X11 is not available"));
    return FALSE;
}

SEXP do_bmVersion(void)
{
    SEXP ans = PROTECT(allocVector(STRSXP, 3)),
	nms = PROTECT(allocVector(STRSXP, 3));
    setAttrib(ans, R_NamesSymbol, nms);
    SET_STRING_ELT(nms, 0, mkChar("libpng"));
    SET_STRING_ELT(nms, 1, mkChar("jpeg"));
    SET_STRING_ELT(nms, 2, mkChar("libtiff"));
    UNPROTECT(2);
    return ans;
}
#endif
