/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2004  The R Foundation
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
 *  A copy of the GNU General Public License is available via WWW at
 *  http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 *  writing to the Free Software Foundation, Inc., 59 Temple Place,
 *  Suite 330, Boston, MA  02111-1307  USA.


 *  This is an implementation of modal event handling in R graphics
 *  by Duncan Murdoch
 */
 
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rmath.h>
#include <Graphics.h>
#include <Rdevices.h>

SEXP do_getGraphicsEvent(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP prompt, onMouseDown, onMouseMove, onMouseUp, onKeybd;
    GEDevDesc *dd;
    NewDevDesc *nd;
    
    checkArity(op, args);
    
    dd = GEcurrentDevice();
    nd = dd->dev;
    
    if (!nd->newDevStruct || !nd->getEvent) 
    	errorcall(call, "Graphics device does not support graphics events");
    
    if (nd->mouseDownHandler || nd->mouseMoveHandler 
     || nd->mouseUpHandler   || nd->keybdHandler) 
	errorcall(call, "graphics event handler already in use");
     
    prompt = CAR(args);
    if (!isString(prompt)) errorcall(call, "invalid prompt");
    args = CDR(args);
    
    onMouseDown = CAR(args);
    if (TYPEOF(onMouseDown) == NILSXP) onMouseDown = NULL;
    else if (!nd->canGenMouseDown) errorcall(call, "onMouseDown not supported");
    else if (TYPEOF(onMouseDown) != CLOSXP) errorcall(call, "invalid onMouseDown callback");
    nd->mouseDownHandler = onMouseDown;
    args = CDR(args);
    
    onMouseMove = CAR(args);
    if (TYPEOF(onMouseMove) == NILSXP) onMouseMove = NULL;
    else if (!nd->canGenMouseMove) errorcall(call, "onMouseMove not supported");
    else if (TYPEOF(onMouseMove) != CLOSXP) errorcall(call, "invalid onMouseMove callback");
    nd->mouseMoveHandler = onMouseMove;
    args = CDR(args);
    
    onMouseUp = CAR(args);
    if (TYPEOF(onMouseUp) == NILSXP) onMouseUp = NULL;
    else if (!nd->canGenMouseUp) errorcall(call, "onMouseUp not supported");
    else if (TYPEOF(onMouseUp) != CLOSXP) errorcall(call, "invalid onMouseUp callback");
    nd->mouseUpHandler = onMouseUp;
    args = CDR(args);
    
    onKeybd = CAR(args);
    if (TYPEOF(onKeybd) == NILSXP) onKeybd = NULL;
    else if (!nd->canGenKeybd) errorcall(call, "onKeybd not supported");
    else if (TYPEOF(onKeybd) != CLOSXP) errorcall(call, "invalid onKeybd callback");
    nd->keybdHandler = onKeybd;
    
    if (!nd->mouseDownHandler && !nd->mouseMoveHandler 
     && !nd->mouseUpHandler   && !nd->keybdHandler) 
	errorcall(call, "must install at least one handler");
     
    /* NB:  cleanup of event handlers must be done by driver in onExit handler */
    
    return(nd->getEvent(CHAR(STRING_ELT(prompt,0))));
}
    
#define leftButton   1
#define middleButton 2
#define rightButton  4

static void doMouseEvent(SEXP handler, NewDevDesc *dd, int buttons, double x, double y)
{
    int i;
    SEXP bvec, sx, sy, temp;
    
    dd->gettingEvent = FALSE; /* avoid recursive calls */
    
    PROTECT(bvec = allocVector(INTSXP, 3));
    i = 0;
    if (buttons & leftButton) INTEGER(bvec)[i++] = 0;
    if (buttons & middleButton) INTEGER(bvec)[i++] = 1;
    if (buttons & rightButton) INTEGER(bvec)[i++] = 2;
    SETLENGTH(bvec, i);
    
    PROTECT(sx = allocVector(REALSXP, 1));
    REAL(sx)[0] = (x - dd->left) / (dd->right - dd->left);
    PROTECT(sy = allocVector(REALSXP, 1));
    REAL(sy)[0] = (y - dd->bottom) / (dd->top - dd->bottom);
    
    PROTECT(temp = lang4(handler, bvec, sx, sy));
    dd->eventResult = eval(temp, dd->eventRho);
    UNPROTECT(4);
    R_FlushConsole();
    
    dd->gettingEvent = TRUE;
}

void doMouseDown(NewDevDesc *dd, int buttons, double x, double y)
{
    if (dd->mouseDownHandler) doMouseEvent(dd->mouseDownHandler, dd, buttons, x, y);
}

void doMouseUp(NewDevDesc *dd, int buttons, double x, double y)
{
    if (dd->mouseUpHandler) doMouseEvent(dd->mouseUpHandler, dd, buttons, x, y);
}

void doMouseMove(NewDevDesc *dd, int buttons, double x, double y)
{
    if (dd->mouseMoveHandler) doMouseEvent(dd->mouseMoveHandler, dd, buttons, x, y);
}

static char * keynames[] = {"Left", "Up", "Right", "Down",
    			 "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10",
    			 "F11","F12",
    			 "PgUp", "PgDn", "End", "Home", "Ins", "Del"};

void doKeybd(NewDevDesc *dd, R_KeyName rkey)
{
    doKeybd2(dd, keynames[rkey]);
}

void doKeybd2(NewDevDesc *dd, char *keyname)
{
    SEXP skey, temp;
    
    if (dd->keybdHandler) {
    
    	dd->gettingEvent = FALSE; /* avoid recursive calls */
    
    	PROTECT(skey = allocVector(STRSXP, 1));
    	SET_STRING_ELT(skey, 0, mkChar(keyname));
    
    	PROTECT(temp = lang2(dd->keybdHandler, skey));
    	dd->eventResult = eval(temp, dd->eventRho);
    	UNPROTECT(2);
    	R_FlushConsole();
    
    	dd->gettingEvent = TRUE;
    }
}
