/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-3     the R Development Core Team
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
#include <config.h>
#endif

#include "Defn.h"
#include "Rdevices.h"
#include "devUI.h"

Rboolean stub_GnomeDeviceDriver(DevDesc *dd, char *display, 
			   double width, double height, double pointsize)
{
    error("the gnome device is not loaded in this version of R");
    return FALSE;
}

Rboolean stub_GTKDeviceDriver(DevDesc *dd, char *display, double width, 
			  double height, double pointsize)
{
    error("the gtk device is only available with --gui=GNOME");
    return FALSE;
}

SEXP stub_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("the data entry editor has not been loaded");
    return R_NilValue;
}

SEXP do_loadhistory(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    ptr_R_loadhistory(call, op, args, rho);
    return R_NilValue;
}

SEXP do_savehistory(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    ptr_R_savehistory(call, op, args, rho);
    return R_NilValue;
}
