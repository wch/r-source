/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-4 The R Development Core Team.
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

/* Definitions for the base graphics system. Only used in <Graphics.h> */

typedef struct {
    GPar dp;		/* current device default parameters */
    GPar gp;		/* current device current parameters */
    GPar dpSaved;		/* saved device default parameters */
    /*
     * Has the device received base output?
     */
    Rboolean baseDevice;  
} baseSystemState;

void registerBase();

Rboolean Rf_baseDevice(DevDesc *dd);
void Rf_setBaseDevice(Rboolean val, DevDesc *dd);
