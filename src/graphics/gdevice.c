/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "Defn.h"
#include "Graphics.h"
#include "Platform.h"
#include <string.h>
#include <stdio.h>

int  DevInit;
int  (*DevOpen)();
void (*DevClose)();
void (*DevResize)();
void (*DevNewPlot)();
void (*DevClip)();
void (*DevStartPath)();
void (*DevEndPath)();
void (*DevMoveTo)();
void (*DevLineTo)();
double (*DevStrWidth)();
void (*DevText)();
void (*DevRText)();
void (*DevDot)();
void (*DevRect)();
void (*DevCircle)();
void (*DevPolygon)();
int  (*DevLocator)();
void (*DevMode)();
void (*DevHold)();
#ifdef OLD
void (*DevColor)();
void (*DevFont)();
void (*DevLinetype)();
#endif
void (*DevSavePlot)();
void (*DevPrintPlot)();
void (*DevMetricInfo)();



extern void DevNull();

GPar GParams, DParams;
GPar *GP = &GParams;
GPar *DP = &DParams;

#ifdef Unix
int PSDeviceDriver(char**, int, double*, int);
int X11DeviceDriver(char**, int, double*, int);
int PicTeXDeviceDriver(char**, int, double*, int);
#ifdef SOON
int XFigDeviceDriver(char**, int, double*, int);
#endif
#endif

#ifdef Win32
int PSDeviceDriver(char**, int, double*, int);
int WinDeviceDriver(char**, int, double*, int);
#endif

#ifdef Macintosh
int MacDeviceDriver(char**, int, double*, int);
#endif

int SetDevice(char *name, char **cpars, int ncpars, double *npars, int nnpars)
{
	if(DevInit) KillDevice();

	/* Only some devices need to provide these */
	/* Notable examples are QuickDraw and PostScript */

	DevStartPath = DevNull;
	DevEndPath = DevNull;

	/* Hardcopy facilities */
	/* Currently only X11 provides this */

	DevSavePlot = DevNull;
	DevPrintPlot = DevNull;
	DevMetricInfo = 0;

	
#ifdef Macintosh
	if(!strcmp(name, "Macintosh"))
		if(MacDeviceDriver(cpars, ncpars, npars, nnpars)) goto have_device;
#endif

#ifndef Macintosh
	if(!strcmp(name, "postscript"))
		if(PSDeviceDriver(cpars, ncpars, npars, nnpars)) goto have_device;
#endif

#ifdef Unix
	if(!strcmp(name, "X11"))
		if(X11DeviceDriver(cpars, ncpars, npars, nnpars)) goto have_device;
#endif

#ifdef Unix
	if(!strcmp(name, "pictex"))
		if(PicTeXDeviceDriver(cpars, ncpars, npars, nnpars)) goto have_device;
#endif 

#ifdef Win32
	if( !strcmp(name, "Win32") )
		if(WinDeviceDriver(cpars, ncpars, npars, nnpars)) goto have_device;
#endif

#ifdef Unix
#ifdef SOON
	if( !strcmp(name, "XFig") )
		if(XFigDeviceDriver(cpars, ncpars, npars, nnpars)) goto have_device;
#endif
#endif

	/* Other Device Drivers Go Here */

	/* Device not found */
	return 0;
	
have_device:
	GInit();
	return 1;
}

void KillDevice()
{
	if(DevInit) DevClose();
	DevInit = 0;
}
