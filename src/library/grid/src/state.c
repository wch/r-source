/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-3 Paul Murrell
 *                2003-5 The R Core Team
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
 *  https://www.R-project.org/Licenses/
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
 * [GSS_ASK 14 = should we prompt the user before starting a new page?
 *  Replaced by per-device setting as from R 2.7.0.]
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

void initDL(pGEDevDesc dd)
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
void initOtherState(pGEDevDesc dd)
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

void fillGridSystemState(SEXP state, pGEDevDesc dd) 
{
    SEXP devsize, currloc, prevloc;

    PROTECT(state);
    devsize = allocVector(REALSXP, 2);
    REAL(devsize)[0] = 0;
    REAL(devsize)[1] = 0;
    SET_VECTOR_ELT(state, GSS_DEVSIZE, devsize);
    /* "current location"
     * Initial setting relies on the fact that all values sent to devices
     * are in INCHES;  so (0, 0) is the bottom-left corner of the device.
     */
    currloc = allocVector(REALSXP, 2);
    REAL(currloc)[0] = NA_REAL;
    REAL(currloc)[1] = NA_REAL;    
    SET_VECTOR_ELT(state, GSS_CURRLOC, currloc);
    prevloc = allocVector(REALSXP, 2);
    REAL(prevloc)[0] = NA_REAL;
    REAL(prevloc)[1] = NA_REAL;    
    SET_VECTOR_ELT(state, GSS_PREVLOC, prevloc);
    SET_VECTOR_ELT(state, GSS_DLON, ScalarLogical(TRUE));
    SET_VECTOR_ELT(state, GSS_ENGINEDLON, ScalarLogical(TRUE));
    SET_VECTOR_ELT(state, GSS_CURRGROB, R_NilValue);
    SET_VECTOR_ELT(state, GSS_ENGINERECORDING, ScalarLogical(FALSE));
    initGPar(dd);
    SET_VECTOR_ELT(state, GSS_GPSAVED, R_NilValue);
    /* Do NOT initialise top-level viewport or grid display list for
     * this device until there is some grid output 
     */
    SET_VECTOR_ELT(state, GSS_GLOBALINDEX, R_NilValue);
    /* Note that no grid output has occurred on the device yet.
     */
    SET_VECTOR_ELT(state, GSS_GRIDDEVICE, ScalarLogical(FALSE));
#if 0
    SET_VECTOR_ELT(state, GSS_ASK, ScalarLogical(dd->ask));
#endif
    SET_VECTOR_ELT(state, GSS_SCALE, ScalarReal(1.0));
    UNPROTECT(1);
}

SEXP gridStateElement(pGEDevDesc dd, int elementIndex)
{
    return VECTOR_ELT((SEXP) dd->gesd[gridRegisterIndex]->systemSpecific, 
		      elementIndex);
}

void setGridStateElement(pGEDevDesc dd, int elementIndex, SEXP value)
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
	error(_("unable to store 'grid' state.  Too many devices open?"));
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

SEXP gridCallback(GEevent task, pGEDevDesc dd, SEXP data) {
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
        /* Indicate success */
        result = R_BlankString;
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
		/* 
                 * ONLY start a new page if 'grid' drawing is first entry
                 * on the DL.
                 * Determine this by checking for "C_par" or "C_plot_new"
                 * at head of DL (assumes that 'graphics' is only other
                 * possible graphics system).
                 * This is a NASTY solution to a RARE problem.
                 * RARE because it will only occur when resizing a 
                 * window, copying between devices, or replaying a
                 * recorded plot when the engine DL contains a mix
                 * of 'graphics' and 'grid'.
                 * NASTY because it requires 'grid' to know about
                 * 'graphics' internals and the engine DL internals.
		 */ 
                /* 'data' is engine DL */
                if (data != R_NilValue) {
                    SEXP firstDLentry = CAR(data);
                    SEXP args = CADR(firstDLentry);
                    int newpage = 1;
                    if (isVector(CAR(args))) {
                        SEXP name = VECTOR_ELT(CAR(args), 0);
                        if (isString(name) &&
                            (!strcmp(CHAR(STRING_ELT(name, 0)), 
                                     "C_par") ||
                             !strcmp(CHAR(STRING_ELT(name, 0)), 
                                     "C_plot_new"))) {
                            newpage = 0;
                        }
                    }
                    if (newpage) {
                        currentgp = gridStateElement(dd, GSS_GPAR);
                        gcontextFromgpar(currentgp, 0, &gc, dd);
                        GENewPage(&gc, dd);
                    }
                }
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
        { 
            if (!isNull(gridStateElement(dd, GSS_DL))) {
                int dlIndex = INTEGER(gridStateElement(dd, GSS_DLINDEX))[0];
                if (dlIndex > 0) {
                    /* called from GEcopyDisplayList */
                    pGEDevDesc curdd = GEcurrentDevice();
                    /* See GE_RestoreSnapshotState for explanation of this 
                     * dirtying 
                     */
                    GEdirtyDevice(curdd);
                    dirtyGridDevice(curdd);
                    setGridStateElement(curdd, GSS_DL, 
                                        gridStateElement(dd, GSS_DL));
                    setGridStateElement(curdd, GSS_DLINDEX, 
                                        gridStateElement(dd, GSS_DLINDEX));
                }
            }
        }
	break;
    case GE_CheckPlot:
	PROTECT(valid = allocVector(LGLSXP, 1));
	LOGICAL(valid)[0] = TRUE;
	UNPROTECT(1);
	result = valid;
    case GE_SaveSnapshotState:
        {
            SEXP pkgName;
            /*
             * Save the current 'grid' DL.
             */
            PROTECT(result = allocVector(VECSXP, 2));
            SET_VECTOR_ELT(result, 0, gridStateElement(dd, GSS_DL));
            SET_VECTOR_ELT(result, 1, gridStateElement(dd, GSS_DLINDEX));
            PROTECT(pkgName = mkString("grid"));
            setAttrib(result, install("pkgName"), pkgName);
            UNPROTECT(2);
        }
	break;
    case GE_RestoreSnapshotState:
        { 
            int i, nState = LENGTH(data) - 1;
            SEXP gridState, snapshotEngineVersion;
            PROTECT(gridState = R_NilValue);
            /* Prior to engine version 11, "pkgName" was not stored.
             * (can tell because "engineVersion" was not stored either.)
             * Assume 'grid' is second state in snapshot
             * (or first, if only one state)
             * (though this could be fatal).
             */
            PROTECT(snapshotEngineVersion = 
                    getAttrib(data, install("engineVersion")));
            if (isNull(snapshotEngineVersion)) {
                gridState = VECTOR_ELT(data, imin2(nState, 2));
            } else {
                for (i=0; i<nState; i++) {
                    SEXP state = VECTOR_ELT(data, i + 1);
                    if (!strcmp(CHAR(STRING_ELT(getAttrib(state, 
                                                          install("pkgName")), 
                                                0)), 
                                "grid")) {
                        gridState = state;
                    }
                }
            }
            /* The recorded plot may have been recorded WITHOUT 'grid' loaded
             * OR it might have been recorded with 'grid' loaded, but WITHOUT
             * any grid drawing
             */
            if (!isNull(gridState) && !isNull(VECTOR_ELT(gridState, 0))) {
                int dlIndex = INTEGER(VECTOR_ELT(gridState, 1))[0];
                if (dlIndex > 0) {
                /* 
                 * Dirty the device (in case this is first drawing on device)
                 * to stop dirtyGridDevice() from starting new page
                 * (because this GE_RestoreSnapshotState will be followed by 
                 *  GE_RestoreState, which will start a new page).
                 * Dirty the device, in a 'grid' sense, (in case this is first
                 * 'grid' drawing on device) to stop first element on 'grid' DL
                 * (which will be a call to L_gridDirty()) from resetting the
                 * 'grid' DL.
                 */
                    GEdirtyDevice(dd);
                    dirtyGridDevice(dd);
                /*
                 * Restore the saved 'grid' DL.
                 * (the 'grid' vpTree will be recreated by replay of 'grid' DL)
                 */
                    setGridStateElement(dd, GSS_DL, 
                                        VECTOR_ELT(gridState, 0));
                    setGridStateElement(dd, GSS_DLINDEX, 
                                        VECTOR_ELT(gridState, 1));
                }
            }
            UNPROTECT(2);
        }
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

