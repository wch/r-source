/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-8 The R Development Core Team.
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
 *  http://www.r-project.org/Licenses/
 */

/* Definitions for the base graphics system. Only used in <Graphics.h> */

#ifndef R_GRAPHICSBASE_H_
#define R_GRAPHICSBASE_H_

typedef struct {
    GPar dp;		/* current device default parameters */
    GPar gp;		/* current device current parameters */
    GPar dpSaved;		/* saved device default parameters */
    /*
     * Has the device received base output?
     */
    Rboolean baseDevice;  
} baseSystemState;

void registerBase(); /* used in devices.c */

Rboolean Rf_baseDevice(DevDesc *dd); /* unused */
void Rf_setBaseDevice(Rboolean val, DevDesc *dd); /* used in graphics.c */
int baseRegisterIndex; /* used in devices.c */

#endif /* R_GRAPHICSBASE_ */
