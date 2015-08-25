/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-8 The R Core Team.
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/* Definitions for the base graphics system.
   So should be private.
 */

#ifndef R_GRAPHICSBASE_H_
#define R_GRAPHICSBASE_H_

typedef struct {
    GPar dp;		  /* current device default parameters: 
			     those which will be used at the next GNewPage */
    GPar gp;		  /* current device current parameters */
    GPar dpSaved;	  /* saved device default parameters:
			     graphics state at the time that the currently
			     displayed plot was started, so we can replay
			     the display list.
			  */
    Rboolean baseDevice;  /* Has the device received base output? */
} baseSystemState;

void registerBase(void); /* used in devices.c */
void unregisterBase(void); /* used in devices.c */

void Rf_setBaseDevice(Rboolean val, pGEDevDesc dd); /* used in graphics.c */

int baseRegisterIndex;

#endif /* R_GRAPHICSBASE_ */
