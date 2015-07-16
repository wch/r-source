/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-12   The R Core Team.
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
 */

/* The beginning of code which represents an R base graphics system
 * separate from an R graphics engine (separate from R devices)
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Graphics.h>
#include <GraphicsBase.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#undef _
#define _(String) dgettext ("graphics", String)
#else
#define _(String) (String)
#endif

/* From src/main/devices.c */
extern int baseRegisterIndex;

static R_INLINE GPar* dpSavedptr(pGEDevDesc dd) {
    if (baseRegisterIndex == -1)
	error(_("no base graphics system is registered"));
    baseSystemState *bss = dd->gesd[baseRegisterIndex]->systemSpecific;
    return &(bss->dpSaved);
}

static void restoredpSaved(pGEDevDesc dd)
{
    /* NOTE that not all params should be restored before playing */
    /* the display list (e.g., don't restore the device size) */

    /* This could probably now just do a memcpy */
    int i, j, nr, nc;

    dpptr(dd)->state = dpSavedptr(dd)->state;
    /* does not restore 'valid' */
    dpptr(dd)->adj = dpSavedptr(dd)->adj;
    dpptr(dd)->ann = dpSavedptr(dd)->ann;
    dpptr(dd)->bg = dpSavedptr(dd)->bg;
    dpptr(dd)->bty = dpSavedptr(dd)->bty;
    dpptr(dd)->cex = dpSavedptr(dd)->cex;
    gpptr(dd)->lheight = dpSavedptr(dd)->lheight;
    dpptr(dd)->col = dpSavedptr(dd)->col;
    dpptr(dd)->crt = dpSavedptr(dd)->crt;
    dpptr(dd)->err = dpSavedptr(dd)->err;
    dpptr(dd)->fg = dpSavedptr(dd)->fg;
    strncpy(dpptr(dd)->family, dpSavedptr(dd)->family, 201);
    dpptr(dd)->font = dpSavedptr(dd)->font;
    dpptr(dd)->gamma = dpSavedptr(dd)->gamma;
    dpptr(dd)->lab[0] = dpSavedptr(dd)->lab[0];
    dpptr(dd)->lab[1] = dpSavedptr(dd)->lab[1];
    dpptr(dd)->lab[2] = dpSavedptr(dd)->lab[2];
    dpptr(dd)->las = dpSavedptr(dd)->las;
    dpptr(dd)->lty = dpSavedptr(dd)->lty;
    dpptr(dd)->lwd = dpSavedptr(dd)->lwd;
    dpptr(dd)->lend = dpSavedptr(dd)->lend;
    dpptr(dd)->ljoin = dpSavedptr(dd)->ljoin;
    dpptr(dd)->lmitre = dpSavedptr(dd)->lmitre;
    dpptr(dd)->mgp[0] = dpSavedptr(dd)->mgp[0];
    dpptr(dd)->mgp[1] = dpSavedptr(dd)->mgp[1];
    dpptr(dd)->mgp[2] = dpSavedptr(dd)->mgp[2];
    dpptr(dd)->mkh = dpSavedptr(dd)->mkh;
    dpptr(dd)->pch = dpSavedptr(dd)->pch;
    dpptr(dd)->ps = dpSavedptr(dd)->ps; /*was commented out --why? Well, it never changes */
    dpptr(dd)->smo = dpSavedptr(dd)->smo;
    dpptr(dd)->srt = dpSavedptr(dd)->srt;
    dpptr(dd)->tck = dpSavedptr(dd)->tck;
    dpptr(dd)->tcl = dpSavedptr(dd)->tcl;
    dpptr(dd)->xaxp[0] = dpSavedptr(dd)->xaxp[0];
    dpptr(dd)->xaxp[1] = dpSavedptr(dd)->xaxp[1];
    dpptr(dd)->xaxp[2] = dpSavedptr(dd)->xaxp[2];
    dpptr(dd)->xaxs = dpSavedptr(dd)->xaxs;
    dpptr(dd)->xaxt = dpSavedptr(dd)->xaxt;
    dpptr(dd)->xpd = dpSavedptr(dd)->xpd;
    /* not oldxpd, which is a gpptr concept */
    dpptr(dd)->xlog = dpSavedptr(dd)->xlog;
    dpptr(dd)->yaxp[0] = dpSavedptr(dd)->yaxp[0];
    dpptr(dd)->yaxp[1] = dpSavedptr(dd)->yaxp[1];
    dpptr(dd)->yaxp[2] = dpSavedptr(dd)->yaxp[2];
    dpptr(dd)->yaxs = dpSavedptr(dd)->yaxs;
    dpptr(dd)->yaxt = dpSavedptr(dd)->yaxt;
    dpptr(dd)->ylog = dpSavedptr(dd)->ylog;
    dpptr(dd)->cexbase = dpSavedptr(dd)->cexbase;
    dpptr(dd)->cexmain = dpSavedptr(dd)->cexmain;
    dpptr(dd)->cexlab = dpSavedptr(dd)->cexlab;
    dpptr(dd)->cexsub = dpSavedptr(dd)->cexsub;
    dpptr(dd)->cexaxis = dpSavedptr(dd)->cexaxis;
    dpptr(dd)->fontmain = dpSavedptr(dd)->fontmain;
    dpptr(dd)->fontlab = dpSavedptr(dd)->fontlab;
    dpptr(dd)->fontsub = dpSavedptr(dd)->fontsub;
    dpptr(dd)->fontaxis = dpSavedptr(dd)->fontaxis;
    dpptr(dd)->colmain = dpSavedptr(dd)->colmain;
    dpptr(dd)->collab = dpSavedptr(dd)->collab;
    dpptr(dd)->colsub = dpSavedptr(dd)->colsub;
    dpptr(dd)->colaxis = dpSavedptr(dd)->colaxis;

    /* must restore layout parameters;	the different graphics */
    /* regions and coordinate transformations will be recalculated */
    /* but they need all of the layout information restored for this */
    /* to happen correctly */

    dpptr(dd)->devmode = dpSavedptr(dd)->devmode;
    dpptr(dd)->fig[0] = dpSavedptr(dd)->fig[0];
    dpptr(dd)->fig[1] = dpSavedptr(dd)->fig[1];
    dpptr(dd)->fig[2] = dpSavedptr(dd)->fig[2];
    dpptr(dd)->fig[3] = dpSavedptr(dd)->fig[3];
    dpptr(dd)->fin[0] = dpSavedptr(dd)->fin[0];
    dpptr(dd)->fin[1] = dpSavedptr(dd)->fin[1];
    dpptr(dd)->fUnits = dpSavedptr(dd)->fUnits;
    dpptr(dd)->defaultFigure = dpSavedptr(dd)->defaultFigure;
    dpptr(dd)->mar[0] = dpSavedptr(dd)->mar[0];
    dpptr(dd)->mar[1] = dpSavedptr(dd)->mar[1];
    dpptr(dd)->mar[2] = dpSavedptr(dd)->mar[2];
    dpptr(dd)->mar[3] = dpSavedptr(dd)->mar[3];
    dpptr(dd)->mai[0] = dpSavedptr(dd)->mai[0];
    dpptr(dd)->mai[1] = dpSavedptr(dd)->mai[1];
    dpptr(dd)->mai[2] = dpSavedptr(dd)->mai[2];
    dpptr(dd)->mai[3] = dpSavedptr(dd)->mai[3];
    dpptr(dd)->mUnits = dpSavedptr(dd)->mUnits;
    dpptr(dd)->mex = dpSavedptr(dd)->mex;
    nr = dpptr(dd)->numrows = dpSavedptr(dd)->numrows;
    nc = dpptr(dd)->numcols = dpSavedptr(dd)->numcols;
    dpptr(dd)->currentFigure = dpSavedptr(dd)->currentFigure;
    dpptr(dd)->lastFigure = dpSavedptr(dd)->lastFigure;
    for (i = 0; i < nr && i < MAX_LAYOUT_ROWS; i++) {
	dpptr(dd)->heights[i] = dpSavedptr(dd)->heights[i];
	dpptr(dd)->cmHeights[i] = dpSavedptr(dd)->cmHeights[i];
    }
    for (j = 0; j < nc && j < MAX_LAYOUT_COLS; j++) {
	dpptr(dd)->widths[j] = dpSavedptr(dd)->widths[j];
	dpptr(dd)->cmWidths[j] = dpSavedptr(dd)->cmWidths[j];
    }
    for (i = 0; i < nr*nc && i < MAX_LAYOUT_CELLS; i++) {
	dpptr(dd)->order[i] = dpSavedptr(dd)->order[i];
	dpptr(dd)->respect[i] = dpSavedptr(dd)->respect[i];
    }
    dpptr(dd)->rspct = dpSavedptr(dd)->rspct;
    dpptr(dd)->layout = dpSavedptr(dd)->layout;
    dpptr(dd)->mfind = dpSavedptr(dd)->mfind;
    dpptr(dd)->new = dpSavedptr(dd)->new;
    dpptr(dd)->oma[0] = dpSavedptr(dd)->oma[0];
    dpptr(dd)->oma[1] = dpSavedptr(dd)->oma[1];
    dpptr(dd)->oma[2] = dpSavedptr(dd)->oma[2];
    dpptr(dd)->oma[3] = dpSavedptr(dd)->oma[3];
    dpptr(dd)->omi[0] = dpSavedptr(dd)->omi[0];
    dpptr(dd)->omi[1] = dpSavedptr(dd)->omi[1];
    dpptr(dd)->omi[2] = dpSavedptr(dd)->omi[2];
    dpptr(dd)->omi[3] = dpSavedptr(dd)->omi[3];
    dpptr(dd)->omd[0] = dpSavedptr(dd)->omd[0];
    dpptr(dd)->omd[1] = dpSavedptr(dd)->omd[1];
    dpptr(dd)->omd[2] = dpSavedptr(dd)->omd[2];
    dpptr(dd)->omd[3] = dpSavedptr(dd)->omd[3];
    dpptr(dd)->oUnits = dpSavedptr(dd)->oUnits;
    dpptr(dd)->plt[0] = dpSavedptr(dd)->plt[0];
    dpptr(dd)->plt[1] = dpSavedptr(dd)->plt[1];
    dpptr(dd)->plt[2] = dpSavedptr(dd)->plt[2];
    dpptr(dd)->plt[3] = dpSavedptr(dd)->plt[3];
    dpptr(dd)->pin[0] = dpSavedptr(dd)->pin[0];
    dpptr(dd)->pin[1] = dpSavedptr(dd)->pin[1];
    dpptr(dd)->pUnits = dpSavedptr(dd)->pUnits;
    dpptr(dd)->defaultPlot = dpSavedptr(dd)->defaultPlot;
    dpptr(dd)->pty = dpSavedptr(dd)->pty;
    dpptr(dd)->usr[0] = dpSavedptr(dd)->usr[0];
    dpptr(dd)->usr[1] = dpSavedptr(dd)->usr[1];
    dpptr(dd)->usr[2] = dpSavedptr(dd)->usr[2];
    dpptr(dd)->usr[3] = dpSavedptr(dd)->usr[3];
    dpptr(dd)->logusr[0] = dpSavedptr(dd)->logusr[0];
    dpptr(dd)->logusr[1] = dpSavedptr(dd)->logusr[1];
    dpptr(dd)->logusr[2] = dpSavedptr(dd)->logusr[2];
    dpptr(dd)->logusr[3] = dpSavedptr(dd)->logusr[3];
}

static SEXP baseCallback(GEevent task, pGEDevDesc dd, SEXP data)
{
    GESystemDesc *sd;
    baseSystemState *bss, *bss2;
    SEXP result = R_NilValue;

    switch (task) {
    case GE_FinaliseState:
	/* called from unregisterOne */
	sd = dd->gesd[baseRegisterIndex];
	free(sd->systemSpecific);
	sd->systemSpecific = NULL;
	break;
    case GE_InitState:
    {
	/* called from registerOne */
	pDevDesc dev;
	GPar *ddp;
	sd = dd->gesd[baseRegisterIndex];
	dev = dd->dev;
	bss = sd->systemSpecific = malloc(sizeof(baseSystemState));
        /* Bail out if necessary */
        if (!bss) return result;
	/* Make sure initialized, or valgrind may complain. */
        memset(bss, 0, sizeof(baseSystemState));
	ddp = &(bss->dp);
	GInit(ddp);
	/* For some things, the device sets the starting value at least. */
	ddp->ps = dev->startps;
	ddp->col = ddp->fg = dev->startcol;
	ddp->bg = dev->startfill;
	ddp->font = dev->startfont;
	ddp->lty = dev->startlty;
	ddp->gamma = dev->startgamma;
	/* Initialise the gp settings too: formerly in addDevice. */
	copyGPar(ddp, &(bss->gp));
	GReset(dd);
	/*
	 * The device has not yet received any base output
	 */
	bss->baseDevice = FALSE;
        /* Indicate success */
        result = R_BlankString;
	break;
    }
    case GE_CopyState:
    {
	/* called from GEcopyDisplayList */
	pGEDevDesc curdd = GEcurrentDevice();
	bss = dd->gesd[baseRegisterIndex]->systemSpecific;
	bss2 = curdd->gesd[baseRegisterIndex]->systemSpecific;
	copyGPar(&(bss->dpSaved), &(bss2->dpSaved));
	restoredpSaved(curdd);
	copyGPar(&(bss2->dp), &(bss2->gp));
	GReset(curdd);
	break;
    }
    case GE_SaveState:
	/* called from GEinitDisplayList */
	bss = dd->gesd[baseRegisterIndex]->systemSpecific;
	copyGPar(&(bss->dp), &(bss->dpSaved));
	break;
    case GE_RestoreState:
	/* called from GEplayDisplayList */
	bss = dd->gesd[baseRegisterIndex]->systemSpecific;
	restoredpSaved(dd);
	copyGPar(&(bss->dp), &(bss->gp));
	GReset(dd);
	break;
    case GE_SaveSnapshotState:
	/* called from GEcreateSnapshot */
	bss = dd->gesd[baseRegisterIndex]->systemSpecific;
	/* Changed from INTSXP in 2.7.0: but saved graphics lists
	   are protected by an R version number */
	PROTECT(result = allocVector(RAWSXP, sizeof(GPar)));
	copyGPar(&(bss->dpSaved), (GPar*) RAW(result));
	UNPROTECT(1);
	break;
    case GE_RestoreSnapshotState:
	/* called from GEplaySnapshot */
	bss = dd->gesd[baseRegisterIndex]->systemSpecific;
	copyGPar((GPar*) RAW(data), &(bss->dpSaved));
	restoredpSaved(dd);
	copyGPar(&(bss->dp), &(bss->gp));
	GReset(dd);
	break;
    case GE_CheckPlot:
	/* called from GEcheckState:
	   Check that the current plotting state is "valid"
	 */
	bss = dd->gesd[baseRegisterIndex]->systemSpecific;
	result = ScalarLogical(bss->baseDevice ?
			       (bss->gp.state == 1) && bss->gp.valid :
			       TRUE);
	break;
    case GE_ScalePS:
    {
	/* called from GEhandleEvent in devWindows.c */
	GPar *ddp, *ddpSaved;
	bss = dd->gesd[baseRegisterIndex]->systemSpecific;
	ddp = &(bss->dp);
	ddpSaved = &(bss->dpSaved);
	if (isReal(data) && LENGTH(data) == 1) {
	    double rf = REAL(data)[0];
	    ddp->scale *= rf;
	    /* Modify the saved settings so this effects display list too */
	    ddpSaved->scale *= rf;
	} else
	  error("event 'GE_ScalePS' requires a single numeric value");
	break;
    }
    }
    return result;
}

/* (un)Register the base graphics system with the graphics engine
 */
void
registerBase(void) {
    GEregisterSystem(baseCallback, &baseRegisterIndex);
}

void
unregisterBase(void) {
    GEunregisterSystem(baseRegisterIndex);
    baseRegisterIndex = -1;   
}

SEXP RunregisterBase(void)
{
    unregisterBase();
    return R_NilValue;
}

/* FIXME: Make this a macro to avoid function call overhead?
   Inline it if you really think it matters.
 */
GPar* gpptr(pGEDevDesc dd) {
    if (baseRegisterIndex == -1)
	error(_("the base graphics system is not registered"));
    baseSystemState *bss = dd->gesd[baseRegisterIndex]->systemSpecific;
    return &(bss->gp);
}

GPar* dpptr(pGEDevDesc dd) {
    if (baseRegisterIndex == -1)
	error(_("the base graphics system is not registered"));
    baseSystemState *bss = dd->gesd[baseRegisterIndex]->systemSpecific;
    return &(bss->dp);
}

/* called in GNewPlot to mark device as 'dirty' */
void Rf_setBaseDevice(Rboolean val, pGEDevDesc dd) {
    if (baseRegisterIndex == -1)
	error(_("the base graphics system is not registered"));
    baseSystemState *bss = dd->gesd[baseRegisterIndex]->systemSpecific;
    bss->baseDevice = val;
}
