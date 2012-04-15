/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-8  R Core Team
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

#ifndef DEVICES_H_
#define DEVICES_H_

/* A public header.
   This is almost entirely of historical interest: it was the 
   interface for graphics device in R < 2.7.0
 */

#ifdef EVENTUALLY
#error Rdevices.h is obsolete
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include <Rgraphics.h>
#include <R_ext/GraphicsEngine.h> /* modern version */
#include <R_ext/Boolean.h>

#define addDevice		Rf_addDevice
#define devNumber		Rf_devNumber
#define GetDevice		Rf_GetDevice
#define KillDevice		Rf_KillDevice

/*-------------------------------------------------------------------
 *
 *  DEVICE FUNCTIONS are concerned with the creation and destruction
 *  of devices.
 *
 */

/* Return a pointer to a device which is identified by number */
DevDesc* GetDevice(int);

/* Kill device which is identified by number.
 * Here DevDesc * is being used as an opaque pointer to GEDevDesc.
 */
void KillDevice(DevDesc*);

/* Get the index of the specified device. 
 * This is used by a device to map from a *NewDevDesc to a device number.
 * Here DevDesc * is being used as an opaque pointer to NewDevDesc.
 */
int devNumber(DevDesc *);
/* New, properly declared version in GraphicsDevices.h */

/* ...NO DOC... */
/* Here DevDesc * is being used as an opaque pointer to GEDevDesc. */
/* Replaced by GEaddDevice */
void addDevice(DevDesc *);


#ifdef __cplusplus
}
#endif

#endif
