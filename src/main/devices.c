/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2008  The R Core Team
 *  Copyright (C) 2002--2005  The R Foundation
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


 *  This is an extensive reworking by Paul Murrell of an original
 *  quick hack by Ross Ihaka designed to give a superset of the
 *  functionality in the AT&T Bell Laboratories GRZ library.
 *
 */

/* This should be regarded as part of the graphics engine */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <Graphics.h>
#include <GraphicsBase.h> 
#include <R_ext/GraphicsEngine.h>

int baseRegisterIndex = -1;

GPar* dpptr(pGEDevDesc dd) {
    if (baseRegisterIndex == -1)
	error(_("the base graphics system is not registered"));
    baseSystemState *bss = dd->gesd[baseRegisterIndex]->systemSpecific;
    return &(bss->dp);
}

static SEXP R_INLINE getSymbolValue(SEXP symbol)
{
    if (TYPEOF(symbol) != SYMSXP)
	error("argument to 'getSymbolValue' is not a symbol");
    return findVar(symbol, R_BaseEnv);
}

/*
 *  DEVICE FUNCTIONS
 *
 *  R allows there to be (up to 64) multiple devices in
 *  existence at the same time.	 Only one device is the
 *  active device and all drawing occurs in this device
 *
 *  Each device has its own set of graphics parameters
 *  so that switching between devices, switches between
 *  their graphical contexts (e.g., if you set the line
 *  width on one device then switch to another device,
 *  don't expect to be using the line width you just set!)
 *
 *  Each device has additional device-specific graphics
 *  parameters which the device driver (i.e., NOT this
 *  generic graphics code) is wholly responsible for
 *  maintaining (including creating and destroying special
 *  resources such as X11 windows).
 *
 *  Each device has a display list which records every
 *  graphical operation since the last dpptr(dd)->newPage;
 *  this is used to redraw the output on the device
 *  when it is resized and to copy output from one device
 *  to another (this can be disabled, which is the default
 *  for postscript).
 *
 *  NOTE: that graphical operations should only be
 *  recorded in the displayList if they are "guaranteed"
 *  to succeed (to avoid heaps of error messages on a
 *  redraw) which means that the recording should be the
 *  last thing done in a graphical operation (see do_*
 *  in plot.c).
 *
 */

static int R_CurrentDevice = 0;
static int R_NumDevices = 1;
/*
   R_MaxDevices is defined in Rgraphics.h to be 64.  Slots are
   initiialized to be NULL, and returned to NULL when a device is
   removed.

   Slot 0 is the null device, and slot 63 is keep empty as a sentinel
   for over-allocation: if a driver fails to call
   R_CheckDeviceAvailable and uses this slot the device it allocated
   will be killed.

   'active' means has been successfully opened and is not in the
   process of being closed and destroyed.  We do this to allow for GUI
   callbacks starting to kill a device whilst another is being killed.
 */
static pGEDevDesc R_Devices[R_MaxDevices];
static Rboolean active[R_MaxDevices];

/* a dummy description to point to when there are no active devices */

static GEDevDesc nullDevice;

/* In many cases this is used to mean that the current device is
   the null device, and in others to mean that there is no open device.
   The two condiions are currently the same, as no way is provided to
   select the null device (selectDevice(0) immediately opens a device).

   But watch out if you intend to change the logic of any of this.
*/

/* Used in grid */
int NoDevices(void)
{
    return (R_NumDevices == 1 || R_CurrentDevice == 0);
}

int NumDevices(void)
{
    return R_NumDevices;
}

pGEDevDesc GEcurrentDevice(void)
{
    /* If there are no active devices
     * check the options for a "default device".
     * If there is one, start it up. */
    if (NoDevices()) {
	SEXP defdev = GetOption1(install("device"));
	if (isString(defdev) && length(defdev) > 0) {
	    SEXP devName = installChar(STRING_ELT(defdev, 0));
	    /*  Not clear where this should be evaluated, since
		grDevices need not be in the search path.
		So we look for it first on the global search path.
	    */
	    defdev = findVar(devName, R_GlobalEnv);
	    if(defdev != R_UnboundValue) {
		PROTECT(defdev = lang1(devName));
		eval(defdev, R_GlobalEnv);
		UNPROTECT(1);
	    } else {
		/* Not globally visible:
		   try grDevices namespace if loaded.
		   The option is unlikely to be set if it is not loaded,
		   as the default setting is in grDevices:::.onLoad.
		*/
		SEXP ns = findVarInFrame(R_NamespaceRegistry,
					 install("grDevices"));
		if(ns != R_UnboundValue &&
		   findVar(devName, ns) != R_UnboundValue) {
		    PROTECT(defdev = lang1(devName));
		    eval(defdev, ns);
		    UNPROTECT(1);
		} else
		    error(_("no active or default device"));
	    }
	} else if(TYPEOF(defdev) == CLOSXP) {
	    PROTECT(defdev = lang1(defdev));
	    eval(defdev, R_GlobalEnv);
	    UNPROTECT(1);
	} else
	    error(_("no active or default device"));
    }
    return R_Devices[R_CurrentDevice];
}

pGEDevDesc GEgetDevice(int i)
{
    return R_Devices[i];
}

int curDevice(void)
{
    return R_CurrentDevice;
}


int nextDevice(int from)
{
    if (R_NumDevices == 1)
	return 0;
    else {
	int i = from;
	int nextDev = 0;
	while ((i < (R_MaxDevices-1)) && (nextDev == 0))
	    if (active[++i]) nextDev = i;
	if (nextDev == 0) {
	    /* start again from 1 */
	    i = 0;
	    while ((i < (R_MaxDevices-1)) && (nextDev == 0))
		if (active[++i]) nextDev = i;
	}
	return nextDev;
    }
}

int prevDevice(int from)
{
    if (R_NumDevices == 1)
	return 0;
    else {
	int i = from;
	int prevDev = 0;
	while ((i > 1) && (prevDev == 0))
	    if (active[--i]) prevDev = i;
	if (prevDev == 0) {
	    /* start again from R_MaxDevices */
	    i = R_MaxDevices;
	    while ((i > 1) && (prevDev == 0))
		if (active[--i]) prevDev = i;
	}
	return prevDev;
    }
}

/* This should be called if you have a pointer to a GEDevDesc
 * and you want to find the corresponding device number
 */

int GEdeviceNumber(pGEDevDesc dd)
{
    int i;
    for (i = 1; i < R_MaxDevices; i++)
	if (R_Devices[i] == dd) return i;
    return 0;
}

/* This should be called if you have a pointer to a DevDesc
 * and you want to find the corresponding device number
 */
int ndevNumber(pDevDesc dd)
{
    int i;
    for (i = 1; i < R_MaxDevices; i++)
	if (R_Devices[i] != NULL && R_Devices[i]->dev == dd)
	    return i;
    return 0;
}

int selectDevice(int devNum)
{
    /* Valid to select nullDevice, but that will open a new device.
       See ?dev.set.
     */
    if((devNum >= 0) && (devNum < R_MaxDevices) &&
       (R_Devices[devNum] != NULL) && active[devNum])
    {
	pGEDevDesc gdd;

	if (!NoDevices()) {
	    pGEDevDesc oldd = GEcurrentDevice();
	    if (oldd->dev->deactivate) oldd->dev->deactivate(oldd->dev);
	}

	R_CurrentDevice = devNum;

	/* maintain .Device */
	gsetVar(R_DeviceSymbol,
		elt(getSymbolValue(R_DevicesSymbol), devNum),
		R_BaseEnv);

	gdd = GEcurrentDevice(); /* will start a device if current is null */
	if (!NoDevices()) /* which it always will be */
	    if (gdd->dev->activate) gdd->dev->activate(gdd->dev);
	return devNum;
    }
    else
	return selectDevice(nextDevice(devNum));
}

/* historically the close was in the [kK]illDevices.
   only use findNext = FALSE when shutting R dowm, and .Device[s] are not
   updated.
*/
static
void removeDevice(int devNum, Rboolean findNext)
{
    /* Not vaild to remove nullDevice */
    if((devNum > 0) && (devNum < R_MaxDevices) &&
       (R_Devices[devNum] != NULL) && active[devNum])
    {
	int i;
	SEXP s;
	pGEDevDesc g = R_Devices[devNum];

	active[devNum] = FALSE; /* stops it being selected again */
	R_NumDevices--;

	if(findNext) {
	    /* maintain .Devices */
	    PROTECT(s = getSymbolValue(R_DevicesSymbol));
	    for (i = 0; i < devNum; i++) s = CDR(s);
	    SETCAR(s, mkString(""));
	    UNPROTECT(1);

	    /* determine new current device */
	    if (devNum == R_CurrentDevice) {
		R_CurrentDevice = nextDevice(R_CurrentDevice);
		/* maintain .Device */
		gsetVar(R_DeviceSymbol,
			elt(getSymbolValue(R_DevicesSymbol), R_CurrentDevice),
			R_BaseEnv);

		/* activate new current device */
		if (R_CurrentDevice) {
		    pGEDevDesc gdd = GEcurrentDevice();
		    if(gdd->dev->activate) gdd->dev->activate(gdd->dev);
		}
	    }
	}
	g->dev->close(g->dev);
	GEdestroyDevDesc(g);
	R_Devices[devNum] = NULL;
    }
}

void GEkillDevice(pGEDevDesc gdd)
{
    removeDevice(GEdeviceNumber(gdd), TRUE);
}

void killDevice(int devNum)
{
    removeDevice(devNum, TRUE);
}


/* Used by front-ends via R_CleanUp to shutdown all graphics devices
   at the end of a session. Not the same as graphics.off(), and leaves
   .Devices and .Device in an invalid state. */
void KillAllDevices(void)
{
    /* Avoid lots of activation followed by removal of devices
       while (R_NumDevices > 1) killDevice(R_CurrentDevice);
    */
    int i;
    for(i = R_MaxDevices-1; i > 0; i--) removeDevice(i, FALSE);
    R_CurrentDevice = 0;  /* the null device, for tidyness */

    /* <FIXME> Disable this for now */
    /*
     * Free the font and encoding structures used by
     * PostScript, Xfig, and PDF devices
     */
    /* freeType1Fonts();
       </FIXME>*/

    /* FIXME: There should really be a formal graphics finaliser
     * but this is a good proxy for now.
     */
    // unregisterBase();
    if (baseRegisterIndex != -1) {
	GEunregisterSystem(baseRegisterIndex);
	baseRegisterIndex = -1; 
    }
}

/* A common construction in some graphics devices */
pGEDevDesc desc2GEDesc(pDevDesc dd)
{
    int i;
    for (i = 1; i < R_MaxDevices; i++)
	if (R_Devices[i] != NULL && R_Devices[i]->dev == dd)
	    return R_Devices[i];
    /* shouldn't happen ...
       but might if device is not yet registered or being killed */
    return R_Devices[0]; /* safe as will not replay a displayList */
}

/* ------- interface for creating devices ---------- */

void R_CheckDeviceAvailable(void)
{
    if (R_NumDevices >= R_MaxDevices - 1)
	error(_("too many open devices"));
}

Rboolean R_CheckDeviceAvailableBool(void)
{
    if (R_NumDevices >= R_MaxDevices - 1) return FALSE;
    else return TRUE;
}

void GEaddDevice(pGEDevDesc gdd)
{
    int i;
    Rboolean appnd;
    SEXP s, t;
    pGEDevDesc oldd;

    PROTECT(s = getSymbolValue(R_DevicesSymbol));

    if (!NoDevices())  {
	oldd = GEcurrentDevice();
	if(oldd->dev->deactivate) oldd->dev->deactivate(oldd->dev);
    }

    /* find empty slot for new descriptor */
    i = 1;
    if (CDR(s) == R_NilValue)
	appnd = TRUE;
    else {
	s = CDR(s);
	appnd = FALSE;
    }
    while (R_Devices[i] != NULL) {
	i++;
	if (CDR(s) == R_NilValue)
	    appnd = TRUE;
	else
	    s = CDR(s);
    }
    R_CurrentDevice = i;
    R_NumDevices++;
    R_Devices[i] = gdd;
    active[i] = TRUE;

    GEregisterWithDevice(gdd);
    if(gdd->dev->activate) gdd->dev->activate(gdd->dev);

    /* maintain .Devices (.Device has already been set) */
    t = PROTECT(duplicate(getSymbolValue(R_DeviceSymbol)));
    if (appnd)
	SETCDR(s, CONS(t, R_NilValue));
    else
	SETCAR(s, t);

    UNPROTECT(2);

    /* In case a device driver did not call R_CheckDeviceAvailable
       before starting its allocation, we complete the allocation and
       then call killDevice here.  This ensures that the device gets a
       chance to deallocate its resources and the current active
       device is restored to a sane value. */
    if (i == R_MaxDevices - 1) {
	killDevice(i);
	error(_("too many open devices"));
    }
}

/* convenience wrappers */
void GEaddDevice2(pGEDevDesc gdd, const char *name)
{
    gsetVar(R_DeviceSymbol, mkString(name), R_BaseEnv);
    GEaddDevice(gdd);
    GEinitDisplayList(gdd);
}

void GEaddDevice2f(pGEDevDesc gdd, const char *name, const char *file)
{
    SEXP f = PROTECT(mkString(name));
    if(file) {
      SEXP s_filepath = install("filepath");
      setAttrib(f, s_filepath, mkString(file));
    }
    gsetVar(R_DeviceSymbol, f, R_BaseEnv);
    UNPROTECT(1);
    GEaddDevice(gdd);
    GEinitDisplayList(gdd);
}


Rboolean Rf_GetOptionDeviceAsk(void); /* from options.c */

/* Create a GEDevDesc, given a pDevDesc
 */
pGEDevDesc GEcreateDevDesc(pDevDesc dev)
{
    /* Wrap the device description within a graphics engine
     * device description (add graphics engine information
     * to the device description).
     */
    pGEDevDesc gdd = (GEDevDesc*) calloc(1, sizeof(GEDevDesc));
    /* NULL the gesd array
     */
    int i;
    if (!gdd)
	error(_("not enough memory to allocate device (in GEcreateDevDesc)"));
    for (i = 0; i < MAX_GRAPHICS_SYSTEMS; i++) gdd->gesd[i] = NULL;
    gdd->dev = dev;
    gdd->displayListOn = dev->displayListOn;
    gdd->displayList = R_NilValue; /* gc needs this */
    gdd->savedSnapshot = R_NilValue; /* gc needs this */
    gdd->dirty = FALSE;
    gdd->recordGraphics = TRUE;
    gdd->ask = Rf_GetOptionDeviceAsk();
    gdd->dev->eventEnv = R_NilValue;  /* gc needs this */
    return gdd;
}


void attribute_hidden InitGraphics(void)
{
    R_Devices[0] = &nullDevice;
    active[0] = TRUE;
    // these are static arrays, not really needed
    for (int i = 1; i < R_MaxDevices; i++) {
	R_Devices[i] = NULL;
	active[i] = FALSE;
    }

    /* init .Device and .Devices */
    SEXP s = PROTECT(mkString("null device"));
    gsetVar(R_DeviceSymbol, s, R_BaseEnv);
    s = PROTECT(mkString("null device"));
    gsetVar(R_DevicesSymbol, CONS(s, R_NilValue), R_BaseEnv);
    UNPROTECT(2);
}


void NewFrameConfirm(pDevDesc dd)
{
    if(!R_Interactive) return;
    /* dd->newFrameConfirm(dd) will either handle this, or return
       FALSE to ask the engine to do so. */
    if(dd->newFrameConfirm && dd->newFrameConfirm(dd)) ;
    else {
	unsigned char buf[1024];
	R_ReadConsole(_("Hit <Return> to see next plot: "), buf, 1024, 0);
    }
}
