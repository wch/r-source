/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000  R Development Core Team
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

#ifndef DEVICES_H_
#define DEVICES_H_

#include "R_ext/Graphics.h"

#define InitGraphics		Rf_InitGraphics
#define KillAllDevices		Rf_KillAllDevices

/* Initialise internal device structures. */
void InitGraphics(void);
/* Kill all active devices (used at shutdown). */
void KillAllDevices(void);

		/* Drivers from ../main/dev....c , description there: */

Rboolean 
PicTeXDeviceDriver(DevDesc*, char*, char*, char*, double, double, Rboolean);

Rboolean 
PSDeviceDriver(DevDesc*, char*, char*, char*, char**,
	       char*, char*, double, double, Rboolean, double, 
	       Rboolean, Rboolean, Rboolean, char*);
Rboolean 
XFigDeviceDriver(DevDesc*, char*, char*, char*,
		 char*, char*, double, double, 
		 Rboolean, double, Rboolean, Rboolean);


#ifdef OLD_Macintosh
Rboolean MacDeviceDriver(char**, int, double*, int);
#endif

#endif
