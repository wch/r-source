/* The beginning of code which represents an R base graphics system
 * separate from an R graphics engine (separate from R devices)
 */

#include <Defn.h>
#include <Graphics.h>
#include <Rdevices.h>

int baseRegisterIndex;

void baseCallback(GEevent task, GEDevDesc *dd) {
    GESystemDesc *sd;
    NewDevDesc *dev;
    GPar *ddp; 
    switch (task) {
    case GE_FinaliseState:
	sd = dd->gesd[baseRegisterIndex];
	free(sd->systemSpecific);
	sd->systemSpecific = NULL;
	break;
    case GE_InitState:
	sd = dd->gesd[baseRegisterIndex];
	dev = dd->dev;
	sd->systemSpecific = malloc(sizeof(baseSystemState));
	ddp = &(((baseSystemState*) sd->systemSpecific)->dp);
	GInit(ddp);
	/* Some things are set by the device, so copy them across now.
	 */
	ddp->ipr[0] = dev->ipr[0];
	ddp->ipr[1] = dev->ipr[1];
	ddp->cra[0] = dev->cra[0];
	ddp->cra[1] = dev->cra[1];
	ddp->asp = dev->asp;
	ddp->left = dev->left;
	ddp->right = dev->right;
	ddp->top = dev->top;
	ddp->bottom = dev->bottom;
	ddp->xCharOffset = dev->xCharOffset;
	ddp->yCharOffset = dev->yCharOffset;
	ddp->yLineBias = dev->yLineBias;
	ddp->canResizePlot = dev->canResizePlot;
	ddp->canChangeFont = dev->canChangeFont;
	ddp->canRotateText = dev->canRotateText;
	ddp->canResizeText = dev->canResizeText;
	ddp->canClip = dev->canClip;
	ddp->canHAdj = dev->canHAdj;
	/* For some things, the device sets the starting value at least.
	 */
	ddp->ps = dev->startps;
	ddp->col = ddp->fg = dev->startcol;
	ddp->bg = dev->startfill;
	/* ddp->font = dev->startfont; */
	/* ddp->lty = dev->startlty; */
	/* Initialise the gp settings too.
	 */
	copyGPar(ddp, &(((baseSystemState*) sd->systemSpecific)->gp));
	break;
    case GE_Redraw:
	playDisplayList((DevDesc*) dd);
	break;
    }
}

/* Register the base graphics system with the graphics engine
 */
void registerBase() {
    baseRegisterIndex = GEregisterSystem(baseCallback);
}

/* FIXME: Make this a macro to avoid function call overhead?
 */
GPar* gpptr(DevDesc *dd) {
    if (dd->newDevStruct) 
	return &(((baseSystemState*) GEsystemState((GEDevDesc*) dd, 
						   baseRegisterIndex))->gp);
    else
	return &(dd->gp);
}

GPar* dpptr(DevDesc *dd) {
    if (dd->newDevStruct) 
	return &(((baseSystemState*) GEsystemState((GEDevDesc*) dd, 
						   baseRegisterIndex))->dp);
    else
	return &(dd->dp);
}

GPar* dpSavedptr(DevDesc *dd) {
    if (dd->newDevStruct) 
	return &(((baseSystemState*) GEsystemState((GEDevDesc*) dd, 
						   baseRegisterIndex))->dpSaved);
    else
	return &(dd->dpSaved);
}

SEXP displayList(DevDesc *dd) {
    if (dd->newDevStruct) 
	return ((GEDevDesc*) dd)->dev->displayList;
    else
	return dd->displayList;
}
