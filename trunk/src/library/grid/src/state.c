/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-3 Paul Murrell
 *                2003-5 The R Development Core Team
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
 */

#include "grid.h"

int gridRegisterIndex;

/* The gridSystemState (per device) consists of 
 * GSS_DEVSIZE 0 = current size of device
 * GSS_CURRLOC 1 = current location of grid "pen" 
 * GSS_DL 2 = grid display list
 * GSS_DLINDEX 3 = display list index
 * GSS_DLON 4 = is the display list on?
 * GSS_GPAR 5 = gpar settings
 * GSS_GPSAVED 6 = previous gpar settings
 * GSS_VP 7 = viewport
 * GSS_GLOBALINDEX 8 = index of this system state in the global list of states
 * GSS_GRIDDEVICE 9 = does this device contain grid output?
 * GSS_PREVLOC 10 = previous location of grid "pen" 
 * GSS_ENGINEDLON 11 = are we using the graphics engine's display list?
 * GSS_CURRGROB 12 = current grob being drawn (for determining 
 *   the list of grobs to search when evaluating a grobwidth/height
 *   unit via gPath)
 * GSS_ENGINERECORDING 13 = are we already inside a .Call.graphics call?
 *   Used by grid.Call.graphics to avoid unnecessary recording on 
 *   engine display list
 * GSS_ASK 14 = should we prompt the user before starting a new page?
 * GSS_SCALE 15 = a scale or "zoom" factor for all output 
 *   (to support "fit to window" resizing on windows device)
 * 
 * NOTE: if you add to this list you MUST change the size of the vector
 * allocated in createGridSystemState() below.
*/

SEXP createGridSystemState()
{
    return allocVector(VECSXP, 16);
}

void initDL(GEDevDesc *dd)
{
    SEXP dl, dlindex;
    SEXP vp = gridStateElement(dd, GSS_VP);
    SEXP gsd = (SEXP) dd->gesd[gridRegisterIndex]->systemSpecific;
    /* The top-level viewport goes at the start of the display list
     */
    PROTECT(dl = allocVector(VECSXP, 100));
    SET_VECTOR_ELT(dl, 0, vp);
    SET_VECTOR_ELT(gsd, GSS_DL, dl);
    PROTECT(dlindex = allocVector(INTSXP, 1));
    INTEGER(dlindex)[0] = 1;
    SET_VECTOR_ELT(gsd, GSS_DLINDEX, dlindex);
    UNPROTECT(2);
}

/*
 * This is used to init some bits of the system state
 * Called when a grahpics engine redraw is about to occur
 * NOTE that it does not init all of the state, in particular,
 * the display list is not initialised here (see initDL), 
 * nor is the ROOT viewport (see initVP),
 * nor is the current gpar (see initGP)
 */
void initOtherState(GEDevDesc* dd)
{
    SEXP currloc, prevloc, recording;
    SEXP state = (SEXP) dd->gesd[gridRegisterIndex]->systemSpecific;
    currloc = VECTOR_ELT(state, GSS_CURRLOC);
    REAL(currloc)[0] = NA_REAL;
    REAL(currloc)[1] = NA_REAL;    
    prevloc = VECTOR_ELT(state, GSS_PREVLOC);
    REAL(prevloc)[0] = NA_REAL;
    REAL(prevloc)[1] = NA_REAL;    
    SET_VECTOR_ELT(state, GSS_CURRGROB, R_NilValue);
    recording = VECTOR_ELT(state, GSS_ENGINERECORDING);
    LOGICAL(recording)[0] = FALSE;
    SET_VECTOR_ELT(state, GSS_ENGINERECORDING, recording);
}

void fillGridSystemState(SEXP state, GEDevDesc* dd) 
{
    SEXP devsize, currloc, prevloc, dlon, enginedlon, recording;
    SEXP griddev, gridask, gridscale;
    PROTECT(devsize = allocVector(REALSXP, 2));
    REAL(devsize)[0] = 0;
    REAL(devsize)[1] = 0;
    SET_VECTOR_ELT(state, GSS_DEVSIZE, devsize);
    /* "current location"
     * Initial setting relies on the fact that all values sent to devices
     * are in INCHES;  so (0, 0) is the bottom-left corner of the device.
     */
    PROTECT(currloc = allocVector(REALSXP, 2));
    REAL(currloc)[0] = NA_REAL;
    REAL(currloc)[1] = NA_REAL;    
    SET_VECTOR_ELT(state, GSS_CURRLOC, currloc);
    PROTECT(prevloc = allocVector(REALSXP, 2));
    REAL(prevloc)[0] = NA_REAL;
    REAL(prevloc)[1] = NA_REAL;    
    SET_VECTOR_ELT(state, GSS_PREVLOC, prevloc);
    PROTECT(dlon = allocVector(LGLSXP, 1));
    LOGICAL(dlon)[0] = TRUE;
    SET_VECTOR_ELT(state, GSS_DLON, dlon);
    PROTECT(enginedlon = allocVector(LGLSXP, 1));
    LOGICAL(enginedlon)[0] = TRUE;
    SET_VECTOR_ELT(state, GSS_ENGINEDLON, enginedlon);
    SET_VECTOR_ELT(state, GSS_CURRGROB, R_NilValue);
    PROTECT(recording = allocVector(LGLSXP, 1));    
    LOGICAL(recording)[0] = FALSE;
    SET_VECTOR_ELT(state, GSS_ENGINERECORDING, recording);
    initGPar(dd);
    SET_VECTOR_ELT(state, GSS_GPSAVED, R_NilValue);
    /* Do NOT initialise top-level viewport or grid display list for
     * this device until there is some grid output 
     */
    SET_VECTOR_ELT(state, GSS_GLOBALINDEX, R_NilValue);
    /* Note that no grid output has occurred on the device yet.
     */
    PROTECT(griddev = allocVector(LGLSXP, 1));
    LOGICAL(griddev)[0] = FALSE;
    SET_VECTOR_ELT(state, GSS_GRIDDEVICE, griddev);
    PROTECT(gridask = allocVector(LGLSXP, 1));
    LOGICAL(gridask)[0] = FALSE;
    SET_VECTOR_ELT(state, GSS_ASK, gridask);
    PROTECT(gridscale = allocVector(REALSXP, 1));
    REAL(gridscale)[0] = 1.0;
    SET_VECTOR_ELT(state, GSS_SCALE, gridscale);
    UNPROTECT(9);
}

SEXP gridStateElement(GEDevDesc *dd, int elementIndex)
{
    return VECTOR_ELT((SEXP) dd->gesd[gridRegisterIndex]->systemSpecific, 
		      elementIndex);
}

void setGridStateElement(GEDevDesc *dd, int elementIndex, SEXP value)
{
    SET_VECTOR_ELT((SEXP) dd->gesd[gridRegisterIndex]->systemSpecific, 
		   elementIndex, value);
}

static void deglobaliseState(SEXP state)
{
    int index = INTEGER(VECTOR_ELT(state, GSS_GLOBALINDEX))[0];
    SET_VECTOR_ELT(findVar(install(".GRID.STATE"), R_gridEvalEnv), 
		   index, R_NilValue);
}

static int findStateSlot()
{
    int i;
    int result = -1;
    SEXP globalstate = findVar(install(".GRID.STATE"), R_gridEvalEnv);
    for (i = 0; i < length(globalstate); i++)
	if (VECTOR_ELT(globalstate, i) == R_NilValue) {
	    result = i;
	    break;
	}
    if (result < 0)
	error(_("Unable to store grid state.  Too many devices open?"));
    return result;
}

static void globaliseState(SEXP state)
{
    int index = findStateSlot();
    SEXP globalstate, indexsxp;
    PROTECT(globalstate = findVar(install(".GRID.STATE"), R_gridEvalEnv));
    /* Record the index for deglobalisation
     */
    PROTECT(indexsxp = allocVector(INTSXP, 1));
    INTEGER(indexsxp)[0] = index;
    SET_VECTOR_ELT(state, GSS_GLOBALINDEX, indexsxp);
    SET_VECTOR_ELT(globalstate, index, state);
    UNPROTECT(2);
}

SEXP gridCallback(GEevent task, GEDevDesc *dd, SEXP data) {
    SEXP result = R_NilValue;
    SEXP valid, scale;
    SEXP gridState;
    GESystemDesc *sd;
    SEXP currentgp;
    SEXP gsd;
    SEXP devsize;
    R_GE_gcontext gc;
    switch (task) {
    case GE_InitState:
	/* Create the initial grid state for a device
	 */
	PROTECT(gridState = createGridSystemState());
	/* Store that state with the device for easy retrieval
	 */
	sd = dd->gesd[gridRegisterIndex];
	sd->systemSpecific = (void*) gridState;
	/* Initialise the grid state for a device
	 */
	fillGridSystemState(gridState, dd);
	/* Also store the state beneath a top-level variable so
	 * that it does not get garbage-collected
	 */
	globaliseState(gridState);
	UNPROTECT(1);
	break;
    case GE_FinaliseState:
	sd = dd->gesd[gridRegisterIndex];
	/* Simply detach the system state from the global variable
	 * and it will be garbage-collected
	 */
	deglobaliseState((SEXP) sd->systemSpecific);
	/* Also set the device pointer to NULL
	 */
	sd->systemSpecific = NULL;	
	break;
    case GE_SaveState:
	break;
    case GE_RestoreState:
	gsd = (SEXP) dd->gesd[gridRegisterIndex]->systemSpecific;
	PROTECT(devsize = allocVector(REALSXP, 2));
	getDeviceSize(dd, &(REAL(devsize)[0]), &(REAL(devsize)[1]));
	SET_VECTOR_ELT(gsd, GSS_DEVSIZE, devsize);
	UNPROTECT(1);
	/* Only bother to do any grid drawing setup 
	 * if there has been grid output
	 * on this device.
	 */
	if (LOGICAL(gridStateElement(dd, GSS_GRIDDEVICE))[0]) {
	    if (LOGICAL(gridStateElement(dd, GSS_ENGINEDLON))[0]) {
		/* The graphics engine is about to replay the display list
		 * So we "clear" the device and reset the grid graphics state
		 */
		/* There are two main situations in which this occurs:
		 * (i) a screen is resized
		 *     In this case, it is ok-ish to do a GENewPage
		 *     because that has the desired effect and no 
		 *     undesirable effects because it only happens on
		 *     a screen device -- a new page is the same as
		 *     clearing the screen
		 * (ii) output on one device is copied to another device
		 *     In this case, a GENewPage is NOT a good thing, however,
		 *     here we will start with a new device and it will not
		 *     have any grid output so this section will not get called
		 *     SO we will not get any unwanted blank pages.
		 *
		 * All this is a bit fragile;  ultimately, what would be ideal
		 * is a dev->clearPage primitive for all devices in addition
		 * to the dev->newPage primitive
		 */ 
		currentgp = gridStateElement(dd, GSS_GPAR);
		gcontextFromgpar(currentgp, 0, &gc, dd);
		GENewPage(&gc, dd);
		initGPar(dd);
		initVP(dd);
		initOtherState(dd);
	    } else {
		/*
		 * If we have turned off the graphics engine's display list
		 * then we have to redraw the scene ourselves
		 */
		SEXP fcall;
		PROTECT(fcall = lang1(install("draw.all")));
		eval(fcall, R_gridEvalEnv); 
		UNPROTECT(1);
	    }
	}
	break;
    case GE_CopyState:
	break;
    case GE_CheckPlot:
	PROTECT(valid = allocVector(LGLSXP, 1));
	LOGICAL(valid)[0] = TRUE;
	UNPROTECT(1);
	result = valid;
    case GE_SaveSnapshotState:
	break;
    case GE_RestoreSnapshotState:
	break;
    case GE_ScalePS:
	/*
	 * data is a numeric scale factor
	 */
	PROTECT(scale = allocVector(REALSXP, 1));
	REAL(scale)[0] = REAL(gridStateElement(dd, GSS_SCALE))[0]*
	    REAL(data)[0];
	setGridStateElement(dd, GSS_SCALE, scale);
	UNPROTECT(1);
	break;
    }
    return result;
}

