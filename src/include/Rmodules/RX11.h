/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-5 The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* Definitions for the X11 module.  Not intended for end-user use */

#ifndef R_X11_MODULE_H
#define R_X11_MODULE_H

#include <Rinternals.h>
#include <Rconnections.h>

typedef SEXP (*R_do_X11)(SEXP call, SEXP op, SEXP args, SEXP rho);
typedef SEXP (*R_X11DataEntryRoutine)(SEXP call, SEXP op, SEXP args, SEXP rho);
typedef Rboolean (*R_GetX11ImageRoutine)(int d, void *pximage, 
					 int *pwidth, int *pheight);
typedef int (*R_X11_access)();

typedef Rboolean (*R_X11clp)(Rclpconn);


typedef struct {
    R_do_X11 X11;
    R_X11DataEntryRoutine de;
    R_GetX11ImageRoutine  image;
    R_X11_access access;
    R_X11clp readclp;
} R_X11Routines;

R_X11Routines *R_setX11Routines(R_X11Routines *routines);


#endif /* R_X11_MODULE_H */
