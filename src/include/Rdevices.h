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

#ifdef __cplusplus
extern "C" {
#endif

#include <Rgraphics.h>
#include <R_ext/Boolean.h>

#define addDevice		Rf_addDevice
#define copyDisplayList		Rf_copyDisplayList
#define deviceNumber		Rf_deviceNumber
#define devNumber		Rf_devNumber
#define DevNull			Rf_DevNull
#define enableDisplayList	Rf_enableDisplayList
#define inhibitDisplayList	Rf_inhibitDisplayList
#define InitGraphics		Rf_InitGraphics
#define GetDevice		Rf_GetDevice
#define KillAllDevices		Rf_KillAllDevices
#define KillDevice		Rf_KillDevice
#define killDevice		Rf_killDevice
#define nextDevice		Rf_nextDevice
#define NumDevices		Rf_NumDevices
#define StartDevice		Rf_StartDevice
#define playDisplayList		Rf_playDisplayList
#define prevDevice		Rf_prevDevice
#define recordGraphicOperation	Rf_recordGraphicOperation

/* Initialize internal device structures. */
void InitGraphics(void);
/* Kill all active devices (used at shutdown). */
void KillAllDevices(void);
/*
 * Free the font and encoding structures used by
 * PostScript, Xfig, and PDF devices
 */
void freeType1Fonts(void);

/* Drivers from ../main/devices.c , 
 * description there;   platform-specific drivers in platform directories! */

Rboolean 
PSDeviceDriver(DevDesc*, char*, char*, char*, char**,
	       char*, char*, char*, double, double, Rboolean, double, 
	       Rboolean, Rboolean, Rboolean, char*, char*, SEXP);
Rboolean 
XFigDeviceDriver(DevDesc*, char*, char*, char*,
		 char*, char*, double, double, 
		 Rboolean, double, Rboolean, Rboolean);
Rboolean
PDFDeviceDriver(DevDesc* dd, char *, char *, char *, 
		char *, char *, double, double, double,
		int, char*, SEXP, int, int);



/*-------------------------------------------------------------------
 *
 *  DEVICE FUNCTIONS are concerned with the creation and destruction
 *  of devices.
 *
 */

/* Return a pointer to a device which is identified by number */
DevDesc* GetDevice(int);
/* Kill device which is identified by number. */
void KillDevice(DevDesc*);
/* How many devices exist ? (>= 1) */
int NumDevices(void);
/* Get the index of the specified device. 
 * This is used by the graphics engine to map from a *GEDevDesc to
 * a device number.
 */
int deviceNumber(DevDesc*);
/* Get the index of the specified device. 
 * This is used by a device to map from a *NewDevDesc to
 * a device number.
 */
int devNumber(DevDesc *dd);
/* Create a new device. */
int StartDevice(SEXP, SEXP, int, SEXP, int);
/* Check for an available device slot */
void R_CheckDeviceAvailable(void);
Rboolean R_CheckDeviceAvailableBool(void);

void DevNull(void);

/* Miscellaneous */
void recordGraphicOperation(SEXP, SEXP, DevDesc*);
void copyDisplayList(int);
void playDisplayList(DevDesc*);
void enableDisplayList(DevDesc*);
void inhibitDisplayList(DevDesc*);

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
/* ...NO DOC... */
void addDevice(DevDesc *);

#ifdef __cplusplus
}
#endif

#endif
