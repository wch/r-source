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

#include <Rgraphics.h>

#define InitGraphics		Rf_InitGraphics
#define KillAllDevices		Rf_KillAllDevices

/* Initialize internal device structures. */
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

#define addDevice		Rf_addDevice
#define copyDisplayList		Rf_copyDisplayList
#define curDevice		Rf_curDevice
#define CurrentDevice		Rf_CurrentDevice
#define deviceNumber		Rf_deviceNumber
#define DevNull			Rf_DevNull
#define inhibitDisplayList	Rf_inhibitDisplayList
#define initDisplayList		Rf_initDisplayList
#define GetDevice		Rf_GetDevice
#define KillDevice		Rf_KillDevice
#define killDevice		Rf_killDevice
#define NewFrameConfirm		Rf_NewFrameConfirm
#define nextDevice		Rf_nextDevice
#define NoDevices		Rf_NoDevices
#define NumDevices		Rf_NumDevices
#define StartDevice		Rf_StartDevice
#define playDisplayList		Rf_playDisplayList
#define prevDevice		Rf_prevDevice
#define recordGraphicOperation	Rf_recordGraphicOperation

/*-------------------------------------------------------------------
 *
 *  DEVICE FUNCTIONS are concerned with the creation and destruction
 *  of devices.
 *
 */

/* Return a pointer to the current device. */
DevDesc* CurrentDevice(void);
/* Return a pointer to a device which is identified by number */
DevDesc* GetDevice(int);
/* Kill device which is identified by number. */
void KillDevice(DevDesc*);
/* Is the null device the current device? */
int NoDevices(void);
/* How many devices exist ? (>= 1) */
int NumDevices(void);
/* Get the index of the specified device. */
int deviceNumber(DevDesc*);
/* Create a new device. */
int StartDevice(SEXP, SEXP, int, SEXP, int);

void DevNull(void);

/* Miscellaneous */
void NewFrameConfirm(void);
void recordGraphicOperation(SEXP, SEXP, DevDesc*);
void initDisplayList(DevDesc *dd);
void copyDisplayList(int);
void playDisplayList(DevDesc*);
void inhibitDisplayList(DevDesc*);

/*-------------------------------------------------------------------
 *
 *  DEVICE UTILITIES are concerned with providing information
 *  for R interpreted functions.
 *
 */

/* Return the number of the current device. */
int curDevice(void);
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



#endif
