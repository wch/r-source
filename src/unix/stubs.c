/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000     the R Development Core Team
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
#include "Graphics.h"
#include "devUI.h"

int stub_X11DeviceDriver(DevDesc *dd,
		    char *display,
		    double width,
		    double height,
		    double pointsize,
		    double gamma,
		    int colormodel,
                    int maxcube)
{
    error("the x11 device has not been loaded");
    return 0;
}

int stub_GnomeDeviceDriver(DevDesc *dd, char *display, 
			   double width, double height, double pointsize)
{
    error("the gnome device is not loaded in this version of R");
    return 0;
}

int stub_GTKDeviceDriver(DevDesc *dd, char *display, double width, 
			  double height, double pointsize)
{
    error("the gtk device is only available with --gui=GNOME");
    return 0;
}

SEXP do_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return ptr_dataentry(call, op, args, rho);
}

SEXP stub_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("the data entry editor has not been loaded");
    return R_NilValue;
}
