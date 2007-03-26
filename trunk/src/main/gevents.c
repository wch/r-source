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
 *  A copy of the GNU General Public License is available via WWW at
 *  http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 *  writing to the Free Software Foundation, Inc., 51 Franklin Street
 *  Fifth Floor, Boston, MA 02110-1301  USA.


 *  This is an implementation of modal event handling in R graphics
 *  by Duncan Murdoch
 */
 
/* <UTF8> char here is either ASCII or handled as a whole */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rmath.h>
#include <Rinterface.h>
#include <Graphics.h>
#include <Rdevices.h>


SEXP attribute_hidden do_getGraphicsEvent(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP prompt, onMouseDown, onMouseMove, onMouseUp, onKeybd;
    GEDevDesc *dd;
    NewDevDesc *nd;
    
    checkArity(op, args);
    
    dd = GEcurrentDevice();
    nd = dd->dev;
    
    if (!nd->newDevStruct || !nd->getEvent) 
    	errorcall(call, _("graphics device does not support graphics events"));
    
    prompt = CAR(args);
    if (!isString(prompt) || !length(prompt)) errorcall(call, _("invalid prompt"));
    args = CDR(args);
    
    onMouseDown = CAR(args);
    if (TYPEOF(onMouseDown) == NILSXP) onMouseDown = NULL;
    else if (!nd->canGenMouseDown)
	errorcall(call, _("'onMouseDown' not supported"));
    else if (TYPEOF(onMouseDown) != CLOSXP) 
	errorcall(call, _("invalid 'onMouseDown' callback"));
    args = CDR(args);
    
    onMouseMove = CAR(args);
    if (TYPEOF(onMouseMove) == NILSXP) onMouseMove = NULL;
    else if (!nd->canGenMouseMove) 
	errorcall(call, _("'onMouseMove' not supported"));
    else if (TYPEOF(onMouseMove) != CLOSXP)
	errorcall(call, _("invalid 'onMouseMove' callback"));
    args = CDR(args);
    
    onMouseUp = CAR(args);
    if (TYPEOF(onMouseUp) == NILSXP) onMouseUp = NULL;
    else if (!nd->canGenMouseUp) 
	errorcall(call, _("'onMouseUp' not supported"));
    else if (TYPEOF(onMouseUp) != CLOSXP) 
	errorcall(call, _("invalid 'onMouseUp' callback"));
    args = CDR(args);
    
    onKeybd = CAR(args);
    if (TYPEOF(onKeybd) == NILSXP) onKeybd = NULL;
    else if (!nd->canGenKeybd) 
	errorcall(call, _("'onKeybd' not supported"));
    else if (TYPEOF(onKeybd) != CLOSXP)
	errorcall(call, _("invalid 'onKeybd' callback"));
    
    /* NB:  cleanup of event handlers must be done by driver in onExit handler */
    
    return(nd->getEvent(env, translateChar(STRING_ELT(prompt,0))));
}
    
#define leftButton   1
#define middleButton 2
#define rightButton  4

static char * mouseHandlers[] = {"onMouseDown", "onMouseUp", "onMouseMove"};

SEXP doMouseEvent(SEXP eventRho, NewDevDesc *dd, R_MouseEvent event,
			 int buttons, double x, double y)
{
    int i;
    SEXP handler, bvec, sx, sy, temp, result;
    
    dd->gettingEvent = FALSE; /* avoid recursive calls */
    
    handler = findVar(install(mouseHandlers[event]), eventRho);
    if (TYPEOF(handler) == PROMSXP)
    	handler = eval(handler, eventRho);
    
    result = NULL;
    
    if (handler != R_UnboundValue && handler != R_NilValue) {
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
	PROTECT(result = eval(temp, eventRho));

	R_FlushConsole();
	UNPROTECT(5);    
    }
    dd->gettingEvent = TRUE;
    return result;
}

static char * keynames[] = {"Left", "Up", "Right", "Down",
    			 "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10",
    			 "F11","F12",
    			 "PgUp", "PgDn", "End", "Home", "Ins", "Del"};

SEXP doKeybd(SEXP eventRho, NewDevDesc *dd, R_KeyName rkey, char *keyname)
{
    SEXP handler, skey, temp, result;
    
    dd->gettingEvent = FALSE; /* avoid recursive calls */

    handler = findVar(install("onKeybd"), eventRho);
    if (TYPEOF(handler) == PROMSXP)
    	handler = eval(handler, eventRho);
    	
    result = NULL;
    
    if (handler != R_UnboundValue && handler != R_NilValue) {   
    
    	PROTECT(skey = allocVector(STRSXP, 1));
    	if (keyname) SET_STRING_ELT(skey, 0, mkChar(keyname));
    	else SET_STRING_ELT(skey, 0, mkChar(keynames[rkey]));
    
    	PROTECT(temp = lang2(handler, skey));
    	PROTECT(result = eval(temp, eventRho));
    	R_FlushConsole();
    	UNPROTECT(3);
    	
    }
    dd->gettingEvent = TRUE;
    return result;
}
