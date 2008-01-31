/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-8  R Development Core Team
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

#ifdef __cplusplus
extern "C" {
#endif

#include <Rgraphics.h>
#include <R_ext/Boolean.h>

#define addDevice		Rf_addDevice
#define deviceNumber		Rf_deviceNumber
#define devNumber		Rf_devNumber
#define GetDevice		Rf_GetDevice
#define KillDevice		Rf_KillDevice
#define killDevice		Rf_killDevice
#define nextDevice		Rf_nextDevice
#define NumDevices		Rf_NumDevices
#define prevDevice		Rf_prevDevice

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


/* --- rest also in GraphicsDevice.h ---- */
/* How many devices exist ? (>= 1) */
int NumDevices(void);

/* Check for an available device slot */
void R_CheckDeviceAvailable(void);
Rboolean R_CheckDeviceAvailableBool(void);

/*-------------------------------------------------------------------
 *
 *  DEVICE UTILITIES are concerned with providing information
 *  for R interpreted functions.
 *
 */

/* Return the number of the next device. */
int nextDevice(int);

/* Return the number of the previous device. */
int prevDevice(int);

/* Make the specified device (specified by number) the current device */
int selectDevice(int);

/* Kill device which is identified by number. */
void killDevice(int);

#ifdef __cplusplus
}
#endif

#endif
