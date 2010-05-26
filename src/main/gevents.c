/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2004-7  The R Foundation
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


 *  This is an implementation of modal event handling in R graphics
 *  by Duncan Murdoch
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rmath.h>
#include <R_ext/GraphicsEngine.h>

SEXP attribute_hidden
do_setGraphicsEventEnv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP eventEnv;
    int devnum;
    pGEDevDesc gdd;
    pDevDesc dd;

    checkArity(op, args);
    
    devnum = INTEGER(CAR(args))[0] - 1;
    if(devnum < 1 || devnum > R_MaxDevices)
	error(_("invalid graphical device number"));

    gdd = GEgetDevice(devnum);
    if(!gdd) errorcall(call, _("invalid device"));
    dd = gdd->dev;
    args=CDR(args);
    
    eventEnv = CAR(args);
    if (TYPEOF(eventEnv) != ENVSXP) 
        error(_("internal error"));
    
    dd->eventEnv = eventEnv;
	
    return(R_NilValue);
}

SEXP attribute_hidden
do_getGraphicsEventEnv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int devnum;
    pGEDevDesc gdd;

    checkArity(op, args);
    
    devnum = INTEGER(CAR(args))[0] - 1;
    if(devnum < 1 || devnum > R_MaxDevices)
	error(_("invalid graphical device number"));

    gdd = GEgetDevice(devnum);
    if(!gdd) errorcall(call, _("invalid device"));
    return gdd->dev->eventEnv;
}

SEXP attribute_hidden
do_getGraphicsEvent(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP result = R_NilValue, prompt;
    pDevDesc dd;
    pGEDevDesc gd;
    int i, count=0, devNum;

    checkArity(op, args);
    
    prompt = CAR(args);
    if (!isString(prompt) || !length(prompt)) error(_("invalid prompt"));

    /* NB:  cleanup of event handlers must be done by driver in onExit handler */
    
    if (!NoDevices()) {
        /* Initialize all devices */
        i = 1;
	devNum = curDevice();
	while (i++ < NumDevices()) {
	    gd = GEgetDevice(devNum);
	    dd = gd->dev;
	    if (dd->gettingEvent)
	    	error(_("recursive use of getGraphicsEvent not supported"));
	    if (dd->eventEnv != R_NilValue) {
	        if (dd->initEvent) dd->initEvent(dd, TRUE);
	        dd->gettingEvent = TRUE;
	        defineVar(install("result"), R_NilValue, dd->eventEnv);
	        count++;
	    }
	    devNum = nextDevice(devNum);
	}
	if (!count)
	    error(_("no graphics event handlers set"));
	    
	Rprintf("%s\n", CHAR(asChar(prompt)));
	R_FlushConsole();

	/* Poll them */
	while (result == R_NilValue) {
	    R_ProcessEvents();
	    i = 1;
	    devNum = curDevice();
	    while (i++ < NumDevices()) {
		gd = GEgetDevice(devNum);
		dd = gd->dev;
		if (dd->eventEnv != R_NilValue) {
		    result = findVar(install("result"), dd->eventEnv);
		    if (result != R_NilValue && result != R_UnboundValue) {
		        break;
		    }
		}
		devNum = nextDevice(devNum);
	    }
	}
	/* clean up */
        i = 1;
	devNum = curDevice();
	while (i++ < NumDevices()) {
	    gd = GEgetDevice(devNum);
	    dd = gd->dev;
	    if (dd->eventEnv != R_NilValue) {
	        if (dd->initEvent) dd->initEvent(dd, FALSE);
	        dd->gettingEvent = FALSE;
	    }
	    devNum = nextDevice(devNum);
	}
	
    }
    return(result);
}

static const char * mouseHandlers[] =
{"onMouseDown", "onMouseUp", "onMouseMove"};

/* used in devWindows.c and cairoDevice */
void doMouseEvent(pDevDesc dd, R_MouseEvent event,
		  int buttons, double x, double y)
{
    int i;
    SEXP handler, bvec, sx, sy, temp, result;

    dd->gettingEvent = FALSE; /* avoid recursive calls */

    handler = findVar(install(mouseHandlers[event]), dd->eventEnv);
    if (TYPEOF(handler) == PROMSXP)
	handler = eval(handler, dd->eventEnv);

    if (TYPEOF(handler) == CLOSXP) {
        defineVar(install("which"), ScalarInteger(ndevNumber(dd)+1), dd->eventEnv);
	PROTECT(bvec = allocVector(INTSXP, 3));
	i = 0;
	if (buttons & leftButton) INTEGER(bvec)[i++] = 0;
	if (buttons & middleButton) INTEGER(bvec)[i++] = 1;
	if (buttons & rightButton) INTEGER(bvec)[i++] = 2;
	SETLENGTH(bvec, i);

	PROTECT(sx = ScalarReal( (x - dd->left) / (dd->right - dd->left) ));
	PROTECT(sy = ScalarReal((y - dd->bottom) / (dd->top - dd->bottom) ));
	PROTECT(temp = lang4(handler, bvec, sx, sy));
	PROTECT(result = eval(temp, dd->eventEnv));
	defineVar(install("result"), result, dd->eventEnv);
	UNPROTECT(5);	
	R_FlushConsole();
    }
    dd->gettingEvent = TRUE;
    return;
}

static const char * keynames[] =
{"Left", "Up", "Right", "Down",
 "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11","F12",
 "PgUp", "PgDn", "End", "Home", "Ins", "Del"};

/* used in devWindows.c and cairoDevice */
void doKeybd(pDevDesc dd, R_KeyName rkey,
	     const char *keyname)
{
    SEXP handler, skey, temp, result;

    dd->gettingEvent = FALSE; /* avoid recursive calls */

    handler = findVar(install("onKeybd"), dd->eventEnv);
    if (TYPEOF(handler) == PROMSXP)
	handler = eval(handler, dd->eventEnv);

    if (TYPEOF(handler) == CLOSXP) {
        defineVar(install("which"), ScalarInteger(ndevNumber(dd)+1), dd->eventEnv);
	PROTECT(skey = mkString(keyname ? keyname : keynames[rkey]));
	PROTECT(temp = lang2(handler, skey));
	PROTECT(result = eval(temp, dd->eventEnv));
        defineVar(install("result"), result, dd->eventEnv);
	UNPROTECT(3);	
	R_FlushConsole();
    }
    dd->gettingEvent = TRUE;
    return;
}
