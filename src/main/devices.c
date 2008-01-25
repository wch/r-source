/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2008  Robert Gentleman, Ross Ihaka and the
 *			      R Development Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Graphics.h>
#include <Rdevices.h>		/* KillAllDevices */
#include <R_ext/GraphicsBase.h> /* registerBase */

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
 *  graphical operation since the last Rf_dpptr(dd)->newPage;
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

void DevNull(void) {}

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
static DevDesc* R_Devices[R_MaxDevices];
static Rboolean active[R_MaxDevices];

/* a dummy description to point to when there are no active devices */

static DevDesc nullDevice;

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


DevDesc* CurrentDevice(void)
{
    /* If there are no active devices
     * check the options for a "default device".
     * If there is one, start it up. */
    if (NoDevices()) {
	SEXP defdev = GetOption(install("device"), R_BaseEnv);
	if (isString(defdev) && length(defdev) > 0) {
	    SEXP devName = install(CHAR(STRING_ELT(defdev, 0)));
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


DevDesc* GetDevice(int i)
{
    return R_Devices[i];
}


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

void attribute_hidden InitGraphics(void)
{
    int i;
    SEXP s, t;

    R_Devices[0] = &nullDevice;
    active[0] = TRUE;
    for (i = 1; i < R_MaxDevices; i++) {
	R_Devices[i] = NULL;
	active[i] = FALSE;
    }

    /* init .Device and .Devices */
    PROTECT(s = mkString("null device"));
    gsetVar(install(".Device"), s, R_BaseEnv);
    PROTECT(t = mkString("null device"));
    gsetVar(install(".Devices"), CONS(t, R_NilValue), R_BaseEnv);
    UNPROTECT(2);

    /* Register the base graphics system with the graphics engine
     */
    registerBase();
}


static SEXP getSymbolValue(char *symbolName)
{
    SEXP t;
    t = findVar(install(symbolName), R_BaseEnv);
    return t;
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


void addDevice(DevDesc *dd)
{
    int i;
    Rboolean appnd;
    SEXP s, t;
    DevDesc *oldd;
    PROTECT(s = getSymbolValue(".Devices"));

    if (!NoDevices())  {
	oldd = CurrentDevice();
	((GEDevDesc*) oldd)->dev->deactivate(((GEDevDesc*) oldd)->dev);
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
    R_Devices[i] = dd;
    active[i] = TRUE;

    GEregisterWithDevice((GEDevDesc*) dd);
    ((GEDevDesc*) dd)->dev->activate(((GEDevDesc*) dd)->dev);

    /* maintain .Devices (.Device has already been set) */
    PROTECT(t = ScalarString(STRING_ELT(getSymbolValue(".Device"), 0)));
    if (appnd)
	SETCDR(s, CONS(t, R_NilValue));
    else
	SETCAR(s, t);

    UNPROTECT(2);

    copyGPar(Rf_dpptr(dd), Rf_gpptr(dd));
    GReset(dd);

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

/* This should be called if you have a DevDesc or a GEDevDesc
 * and you want to find the corresponding device number
 */
int deviceNumber(DevDesc *dd)
{
    int i;
    for (i = 1; i < R_MaxDevices; i++)
	if (R_Devices[i] == dd)
	    return i;
    return 0;
}

/* This should be called if you have a NewDevDesc
 * and you want to find the corresponding device number
 */
int devNumber(DevDesc *dd)
{
    int i;
    for (i = 1; i < R_MaxDevices; i++)
	if (R_Devices[i] != NULL &&
	    ((GEDevDesc*) R_Devices[i])->dev == (NewDevDesc*) dd)
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
	DevDesc *dd;

	if (!NoDevices()) {
	    GEDevDesc *oldd = (GEDevDesc*) CurrentDevice();
	    oldd->dev->deactivate(oldd->dev);
	}

	R_CurrentDevice = devNum;

	/* maintain .Device */
	gsetVar(install(".Device"),
		elt(getSymbolValue(".Devices"), devNum),
		R_BaseEnv);

	dd = CurrentDevice(); /* will start a device if current is null */
	if (!NoDevices()) /* which it always will be */
	    ((GEDevDesc*) dd)->dev->activate(((GEDevDesc*) dd)->dev);
	return devNum;
    }
    else
	return selectDevice(nextDevice(devNum));
}

/* historically the close was in the [kK]illDevices.
   only use findNext= TRUE when shutting R dowm, and .Device[s] are not
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
	GEDevDesc *g = (GEDevDesc*) R_Devices[devNum];

	active[devNum] = FALSE; /* stops it being selected again */
	R_NumDevices--;

	if(findNext) {
	    /* maintain .Devices */
	    PROTECT(s = getSymbolValue(".Devices"));
	    for (i = 0; i < devNum; i++) s = CDR(s);
	    SETCAR(s, mkString(""));
	    UNPROTECT(1);

	    /* determine new current device */
	    if (devNum == R_CurrentDevice) {
		R_CurrentDevice = nextDevice(R_CurrentDevice);
		/* maintain .Device */
		gsetVar(install(".Device"),
			elt(getSymbolValue(".Devices"), R_CurrentDevice),
			R_BaseEnv);

		/* activate new current device */
		if (R_CurrentDevice) {
		    DevDesc *dd = CurrentDevice();
		    ((GEDevDesc*) dd)->dev->activate(((GEDevDesc*) dd)->dev);
		    copyGPar(Rf_dpptr(dd), Rf_gpptr(dd));
		    GReset(dd);
		}
	    }
	}
	g->dev->close(g->dev);
	GEdestroyDevDesc(g);
	R_Devices[devNum] = NULL;
    }
}

void KillDevice(DevDesc *dd)
{
    removeDevice(deviceNumber(dd), TRUE);
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
    GEunregisterSystem(baseRegisterIndex);
}


/* Code for maintaining DISPLAY LISTS  (Generic list code from ./list.c) */

void initDisplayList(DevDesc *dd)
{
    /* init saveParams */
    copyGPar(Rf_dpptr(dd), Rf_dpSavedptr(dd));
    ((GEDevDesc*) dd)->dev->displayList = R_NilValue;
}


void recordGraphicOperation(SEXP op, SEXP args, DevDesc *dd)
{
    GErecordGraphicOperation(op, args, ((GEDevDesc*) dd));
}

/* NOTE this is not declared static because it is also used in
 * base.c
 * Once graphics.c gets hacked to pieces and split into engine.c and base.c
 * then this can be made static again.
 */
attribute_hidden
void restoredpSaved(DevDesc *dd)
{
    /* NOTE that not all params should be restored before playing */
    /* the display list (e.g., don't restore the device size) */

    int i, j, nr, nc;

    /* do NOT restore basic device driver properties;  they are */
    /* either meant to be different (e.g., left, right, bottom, top */
    /* changed because of window resize) or never change (e.g., ipr) */

    Rf_dpptr(dd)->state = Rf_dpSavedptr(dd)->state;
    Rf_dpptr(dd)->adj = Rf_dpSavedptr(dd)->adj;
    Rf_dpptr(dd)->ann = Rf_dpSavedptr(dd)->ann;
    Rf_dpptr(dd)->bg = Rf_dpSavedptr(dd)->bg;
    Rf_dpptr(dd)->bty = Rf_dpSavedptr(dd)->bty;
    Rf_dpptr(dd)->cex = Rf_dpSavedptr(dd)->cex;
    Rf_gpptr(dd)->lheight = Rf_dpSavedptr(dd)->lheight;
    Rf_dpptr(dd)->col = Rf_dpSavedptr(dd)->col;
    Rf_dpptr(dd)->crt = Rf_dpSavedptr(dd)->crt;
    Rf_dpptr(dd)->err = Rf_dpSavedptr(dd)->err;
    Rf_dpptr(dd)->fg = Rf_dpSavedptr(dd)->fg;
    Rf_dpptr(dd)->font = Rf_dpSavedptr(dd)->font;
    strncpy(Rf_dpptr(dd)->family, Rf_dpSavedptr(dd)->family, 201);
    Rf_dpptr(dd)->gamma = Rf_dpSavedptr(dd)->gamma;
    Rf_dpptr(dd)->lab[0] = Rf_dpSavedptr(dd)->lab[0];
    Rf_dpptr(dd)->lab[1] = Rf_dpSavedptr(dd)->lab[1];
    Rf_dpptr(dd)->lab[2] = Rf_dpSavedptr(dd)->lab[2];
    Rf_dpptr(dd)->las = Rf_dpSavedptr(dd)->las;
    Rf_dpptr(dd)->lty = Rf_dpSavedptr(dd)->lty;
    Rf_dpptr(dd)->lwd = Rf_dpSavedptr(dd)->lwd;
    Rf_dpptr(dd)->lend = Rf_dpSavedptr(dd)->lend;
    Rf_dpptr(dd)->ljoin = Rf_dpSavedptr(dd)->ljoin;
    Rf_dpptr(dd)->lmitre = Rf_dpSavedptr(dd)->lmitre;
    Rf_dpptr(dd)->mgp[0] = Rf_dpSavedptr(dd)->mgp[0];
    Rf_dpptr(dd)->mgp[1] = Rf_dpSavedptr(dd)->mgp[1];
    Rf_dpptr(dd)->mgp[2] = Rf_dpSavedptr(dd)->mgp[2];
    Rf_dpptr(dd)->mkh = Rf_dpSavedptr(dd)->mkh;
    Rf_dpptr(dd)->pch = Rf_dpSavedptr(dd)->pch;
    Rf_dpptr(dd)->ps = Rf_dpSavedptr(dd)->ps; /*was commented out --why?*/
    Rf_dpptr(dd)->smo = Rf_dpSavedptr(dd)->smo;
    Rf_dpptr(dd)->srt = Rf_dpSavedptr(dd)->srt;
    Rf_dpptr(dd)->tck = Rf_dpSavedptr(dd)->tck;
    Rf_dpptr(dd)->tcl = Rf_dpSavedptr(dd)->tcl;
    Rf_dpptr(dd)->xaxp[0] = Rf_dpSavedptr(dd)->xaxp[0];
    Rf_dpptr(dd)->xaxp[1] = Rf_dpSavedptr(dd)->xaxp[1];
    Rf_dpptr(dd)->xaxp[2] = Rf_dpSavedptr(dd)->xaxp[2];
    Rf_dpptr(dd)->xaxs = Rf_dpSavedptr(dd)->xaxs;
    Rf_dpptr(dd)->xaxt = Rf_dpSavedptr(dd)->xaxt;
    Rf_dpptr(dd)->xpd = Rf_dpSavedptr(dd)->xpd;
    Rf_dpptr(dd)->xlog = Rf_dpSavedptr(dd)->xlog;
    Rf_dpptr(dd)->yaxp[0] = Rf_dpSavedptr(dd)->yaxp[0];
    Rf_dpptr(dd)->yaxp[1] = Rf_dpSavedptr(dd)->yaxp[1];
    Rf_dpptr(dd)->yaxp[2] = Rf_dpSavedptr(dd)->yaxp[2];
    Rf_dpptr(dd)->yaxs = Rf_dpSavedptr(dd)->yaxs;
    Rf_dpptr(dd)->yaxt = Rf_dpSavedptr(dd)->yaxt;
    Rf_dpptr(dd)->ylog = Rf_dpSavedptr(dd)->ylog;
    Rf_dpptr(dd)->cexbase = Rf_dpSavedptr(dd)->cexbase;
    Rf_dpptr(dd)->cexmain = Rf_dpSavedptr(dd)->cexmain;
    Rf_dpptr(dd)->cexlab = Rf_dpSavedptr(dd)->cexlab;
    Rf_dpptr(dd)->cexsub = Rf_dpSavedptr(dd)->cexsub;
    Rf_dpptr(dd)->cexaxis = Rf_dpSavedptr(dd)->cexaxis;
    Rf_dpptr(dd)->fontmain = Rf_dpSavedptr(dd)->fontmain;
    Rf_dpptr(dd)->fontlab = Rf_dpSavedptr(dd)->fontlab;
    Rf_dpptr(dd)->fontsub = Rf_dpSavedptr(dd)->fontsub;
    Rf_dpptr(dd)->fontaxis = Rf_dpSavedptr(dd)->fontaxis;
    Rf_dpptr(dd)->colmain = Rf_dpSavedptr(dd)->colmain;
    Rf_dpptr(dd)->collab = Rf_dpSavedptr(dd)->collab;
    Rf_dpptr(dd)->colsub = Rf_dpSavedptr(dd)->colsub;
    Rf_dpptr(dd)->colaxis = Rf_dpSavedptr(dd)->colaxis;

    /* must restore layout parameters;	the different graphics */
    /* regions and coordinate transformations will be recalculated */
    /* but they need all of the layout information restored for this */
    /* to happen correctly */

    Rf_dpptr(dd)->devmode = Rf_dpSavedptr(dd)->devmode;
    Rf_dpptr(dd)->fig[0] = Rf_dpSavedptr(dd)->fig[0];
    Rf_dpptr(dd)->fig[1] = Rf_dpSavedptr(dd)->fig[1];
    Rf_dpptr(dd)->fig[2] = Rf_dpSavedptr(dd)->fig[2];
    Rf_dpptr(dd)->fig[3] = Rf_dpSavedptr(dd)->fig[3];
    Rf_dpptr(dd)->fin[0] = Rf_dpSavedptr(dd)->fin[0];
    Rf_dpptr(dd)->fin[1] = Rf_dpSavedptr(dd)->fin[1];
    Rf_dpptr(dd)->fUnits = Rf_dpSavedptr(dd)->fUnits;
    Rf_dpptr(dd)->defaultFigure = Rf_dpSavedptr(dd)->defaultFigure;
    Rf_dpptr(dd)->mar[0] = Rf_dpSavedptr(dd)->mar[0];
    Rf_dpptr(dd)->mar[1] = Rf_dpSavedptr(dd)->mar[1];
    Rf_dpptr(dd)->mar[2] = Rf_dpSavedptr(dd)->mar[2];
    Rf_dpptr(dd)->mar[3] = Rf_dpSavedptr(dd)->mar[3];
    Rf_dpptr(dd)->mai[0] = Rf_dpSavedptr(dd)->mai[0];
    Rf_dpptr(dd)->mai[1] = Rf_dpSavedptr(dd)->mai[1];
    Rf_dpptr(dd)->mai[2] = Rf_dpSavedptr(dd)->mai[2];
    Rf_dpptr(dd)->mai[3] = Rf_dpSavedptr(dd)->mai[3];
    Rf_dpptr(dd)->mUnits = Rf_dpSavedptr(dd)->mUnits;
    Rf_dpptr(dd)->mex = Rf_dpSavedptr(dd)->mex;
    nr = Rf_dpptr(dd)->numrows = Rf_dpSavedptr(dd)->numrows;
    nc = Rf_dpptr(dd)->numcols = Rf_dpSavedptr(dd)->numcols;
    Rf_dpptr(dd)->currentFigure = Rf_dpSavedptr(dd)->currentFigure;
    Rf_dpptr(dd)->lastFigure = Rf_dpSavedptr(dd)->lastFigure;
    for (i = 0; i < nr && i < MAX_LAYOUT_ROWS; i++) {
	Rf_dpptr(dd)->heights[i] = Rf_dpSavedptr(dd)->heights[i];
	Rf_dpptr(dd)->cmHeights[i] = Rf_dpSavedptr(dd)->cmHeights[i];
    }
    for (j = 0; j < nc && j < MAX_LAYOUT_COLS; j++) {
	Rf_dpptr(dd)->widths[j] = Rf_dpSavedptr(dd)->widths[j];
	Rf_dpptr(dd)->cmWidths[j] = Rf_dpSavedptr(dd)->cmWidths[j];
    }
    for (i = 0; i < nr*nc && i < MAX_LAYOUT_CELLS; i++) {
	Rf_dpptr(dd)->order[i] = Rf_dpSavedptr(dd)->order[i];
	Rf_dpptr(dd)->respect[i] = Rf_dpSavedptr(dd)->respect[i];
    }
    Rf_dpptr(dd)->rspct = Rf_dpSavedptr(dd)->rspct;
    Rf_dpptr(dd)->layout = Rf_dpSavedptr(dd)->layout;
    Rf_dpptr(dd)->mfind = Rf_dpSavedptr(dd)->mfind;
    Rf_dpptr(dd)->new = Rf_dpSavedptr(dd)->new;
    Rf_dpptr(dd)->oma[0] = Rf_dpSavedptr(dd)->oma[0];
    Rf_dpptr(dd)->oma[1] = Rf_dpSavedptr(dd)->oma[1];
    Rf_dpptr(dd)->oma[2] = Rf_dpSavedptr(dd)->oma[2];
    Rf_dpptr(dd)->oma[3] = Rf_dpSavedptr(dd)->oma[3];
    Rf_dpptr(dd)->omi[0] = Rf_dpSavedptr(dd)->omi[0];
    Rf_dpptr(dd)->omi[1] = Rf_dpSavedptr(dd)->omi[1];
    Rf_dpptr(dd)->omi[2] = Rf_dpSavedptr(dd)->omi[2];
    Rf_dpptr(dd)->omi[3] = Rf_dpSavedptr(dd)->omi[3];
    Rf_dpptr(dd)->omd[0] = Rf_dpSavedptr(dd)->omd[0];
    Rf_dpptr(dd)->omd[1] = Rf_dpSavedptr(dd)->omd[1];
    Rf_dpptr(dd)->omd[2] = Rf_dpSavedptr(dd)->omd[2];
    Rf_dpptr(dd)->omd[3] = Rf_dpSavedptr(dd)->omd[3];
    Rf_dpptr(dd)->oUnits = Rf_dpSavedptr(dd)->oUnits;
    Rf_dpptr(dd)->plt[0] = Rf_dpSavedptr(dd)->plt[0];
    Rf_dpptr(dd)->plt[1] = Rf_dpSavedptr(dd)->plt[1];
    Rf_dpptr(dd)->plt[2] = Rf_dpSavedptr(dd)->plt[2];
    Rf_dpptr(dd)->plt[3] = Rf_dpSavedptr(dd)->plt[3];
    Rf_dpptr(dd)->pin[0] = Rf_dpSavedptr(dd)->pin[0];
    Rf_dpptr(dd)->pin[1] = Rf_dpSavedptr(dd)->pin[1];
    Rf_dpptr(dd)->pUnits = Rf_dpSavedptr(dd)->pUnits;
    Rf_dpptr(dd)->defaultPlot = Rf_dpSavedptr(dd)->defaultPlot;
    Rf_dpptr(dd)->pty = Rf_dpSavedptr(dd)->pty;
    Rf_dpptr(dd)->usr[0] = Rf_dpSavedptr(dd)->usr[0];
    Rf_dpptr(dd)->usr[1] = Rf_dpSavedptr(dd)->usr[1];
    Rf_dpptr(dd)->usr[2] = Rf_dpSavedptr(dd)->usr[2];
    Rf_dpptr(dd)->usr[3] = Rf_dpSavedptr(dd)->usr[3];
    Rf_dpptr(dd)->logusr[0] = Rf_dpSavedptr(dd)->logusr[0];
    Rf_dpptr(dd)->logusr[1] = Rf_dpSavedptr(dd)->logusr[1];
    Rf_dpptr(dd)->logusr[2] = Rf_dpSavedptr(dd)->logusr[2];
    Rf_dpptr(dd)->logusr[3] = Rf_dpSavedptr(dd)->logusr[3];
}


/* FIXME : If a non-active window is resized to an invalid size */
/* that window is left active.  */

void playDisplayList(DevDesc *dd)
{
    int savedDevice;
    Rboolean asksave;
    SEXP theList;
    theList = Rf_displayList(dd);
    if (theList != R_NilValue) {
	asksave = Rf_gpptr(dd)->ask;
	Rf_gpptr(dd)->ask = TRUE;
	restoredpSaved(dd);
	copyGPar(Rf_dpptr(dd), Rf_gpptr(dd));
	GReset(dd);
	savedDevice = curDevice();
	selectDevice(deviceNumber(dd));
	while (theList != R_NilValue) {
	    SEXP theOperation = CAR(theList);
	    SEXP op = CAR(theOperation);
	    SEXP args = CDR(theOperation);
	    PRIMFUN(op) (R_NilValue, op, args, R_NilValue);
	    if (!Rf_gpptr(dd)->valid) break;
	    theList = CDR(theList);
	}
	Rf_gpptr(dd)->ask = asksave;
	selectDevice(savedDevice);
    }
}


/* FIXME:  This assumes that the only drawing is base graphics drawing.
 * For example, copying a display list containing grid drawing will
 * not work properly (grid drawing is not based on a Rf_dpSavedptr;  grid
 * drawing IS based on its own separate graphics state)
 * Once the conversion of device drivers is complete, this should just
 * be able to call GEcopyDisplayList
 */
void copyDisplayList(int fromDevice)
{
    DevDesc *dd = CurrentDevice();
    ((GEDevDesc*) dd)->dev->displayList =
	Rf_displayList(R_Devices[fromDevice]);
    copyGPar(Rf_dpSavedptr(R_Devices[fromDevice]),
	     Rf_dpSavedptr(dd));
    playDisplayList(dd);
    if (!((GEDevDesc*) dd)->dev->displayListOn)
	initDisplayList(dd);
}


void inhibitDisplayList(DevDesc *dd)
{
    GEinitDisplayList((GEDevDesc*) dd);
    ((GEDevDesc*) dd)->dev->displayListOn = FALSE;
}

void enableDisplayList(DevDesc *dd)
{
    GEinitDisplayList((GEDevDesc*) dd);
    ((GEDevDesc*) dd)->dev->displayListOn = TRUE;
}

/* FIXME:  NewFrameConfirm should be a standard device function */
#ifdef Win32
Rboolean winNewFrameConfirm(void);
#endif

void NewFrameConfirm(void)
{
    unsigned char buf[16];
#ifdef Win32
	int i;
	Rboolean haveWindowsDevice;
	SEXP dotDevices = findVar(install(".Devices"), R_BaseEnv); /* This is a pairlist! */
#endif

    if(!R_Interactive) return;
#ifdef Win32
    for(i = 0; i < curDevice(); i++)  /* 0-based */
	dotDevices = CDR(dotDevices);
    haveWindowsDevice =
	strcmp(CHAR(STRING_ELT(CAR(dotDevices), 0)), "windows") == 0;
    
    if (!haveWindowsDevice || !winNewFrameConfirm())
#endif
	R_ReadConsole(_("Hit <Return> to see next plot: "), buf, 16, 0);
}

#define checkArity_length					\
    checkArity(op, args);					\
    if(!LENGTH(CAR(args)))					\
	error(_("argument must have positive length"))

SEXP attribute_hidden do_devcontrol(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int listFlag;

    checkArity(op, args);
    if(PRIMVAL(op) == 0) { /* dev.control */
	listFlag = asLogical(CAR(args));
	if(listFlag == NA_LOGICAL) error(_("invalid argument"));
	if(listFlag)
	    enableDisplayList(CurrentDevice());
	else
	    inhibitDisplayList(CurrentDevice());
    } else { /* dev.displaylist */
	GEDevDesc *dd = (GEDevDesc*)CurrentDevice();
	listFlag = dd->dev->displayListOn;
    }
    return ScalarLogical(listFlag);
}

SEXP attribute_hidden do_devcopy(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity_length;
    GEcopyDisplayList(INTEGER(CAR(args))[0] - 1);
    return R_NilValue;
}

SEXP attribute_hidden do_devcur(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return ScalarInteger(curDevice() + 1);
}

SEXP attribute_hidden do_devnext(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity_length;
    return ScalarInteger( nextDevice(INTEGER(CAR(args))[0] - 1) + 1 );
}

SEXP attribute_hidden do_devprev(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity_length;
    return ScalarInteger( prevDevice(INTEGER(CAR(args))[0] - 1) + 1 );
}

SEXP attribute_hidden do_devset(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int devNum = INTEGER(CAR(args))[0] - 1;
    checkArity(op, args);
    return ScalarInteger( selectDevice(devNum) + 1 );
}

SEXP attribute_hidden do_devoff(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity_length;
    killDevice(INTEGER(CAR(args))[0] - 1);
    return R_NilValue;
}

