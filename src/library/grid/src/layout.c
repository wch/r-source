/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-3 Paul Murrell
 *                2003-2013 The R Core Team
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

#include "grid.h"

/* This stuff always returns an LViewportLocation in "npc" units
 */

int layoutNRow(SEXP l) {
    return INTEGER(VECTOR_ELT(l, LAYOUT_NROW))[0];
}

int layoutNCol(SEXP l) {
    return INTEGER(VECTOR_ELT(l, LAYOUT_NCOL))[0];
}

SEXP layoutWidths(SEXP l) {
    return VECTOR_ELT(l, LAYOUT_WIDTHS);
}

SEXP layoutHeights(SEXP l) {
    return VECTOR_ELT(l, LAYOUT_HEIGHTS);
}

int layoutRespect(SEXP l) {
    return INTEGER(VECTOR_ELT(l, LAYOUT_VRESPECT))[0];
}

int* layoutRespectMat(SEXP l) {
    return INTEGER(VECTOR_ELT(l, LAYOUT_MRESPECT));
}

double layoutHJust(SEXP l) {
    return REAL(VECTOR_ELT(l, LAYOUT_VJUST))[0];
}

double layoutVJust(SEXP l) {
    return REAL(VECTOR_ELT(l, LAYOUT_VJUST))[1];
}

Rboolean relativeUnit(SEXP unit, int index,
		   pGEDevDesc dd) {
    return pureNullUnit(unit, index, dd);
}

void findRelWidths(SEXP layout, int *relativeWidths,
		   pGEDevDesc dd) 
{
    int i;
    SEXP widths = layoutWidths(layout);
    for (i=0; i<layoutNCol(layout); i++) 
	relativeWidths[i] = relativeUnit(widths, i, dd);
}

void findRelHeights(SEXP layout, int *relativeHeights,
		   pGEDevDesc dd) 
{
    int i;
    SEXP heights = layoutHeights(layout);
    for (i=0; i<layoutNRow(layout); i++) 
	relativeHeights[i] = relativeUnit(heights, i, dd);
}

void allocateKnownWidths(SEXP layout, 
			 int *relativeWidths, 
			 double parentWidthCM, double parentHeightCM,
			 LViewportContext parentContext,
			 const pGEcontext parentgc,
			 pGEDevDesc dd,
			 double *npcWidths, double *widthLeftCM) 
{
    int i;
    SEXP widths = layoutWidths(layout);
    for (i=0; i<layoutNCol(layout); i++) 
	if (!relativeWidths[i]) {
	    npcWidths[i] = transformWidth(widths, i, parentContext,
					  parentgc,
					  parentWidthCM, parentHeightCM, 
					  0, 0, dd)*2.54;
	    *widthLeftCM -= npcWidths[i];
	}
}

void allocateKnownHeights(SEXP layout, 
			  int *relativeHeights,
			  double parentWidthCM, double parentHeightCM,
			  LViewportContext parentContext,
			  const pGEcontext parentgc,
			  pGEDevDesc dd,
			  double *npcHeights, double *heightLeftCM) 
{
    int i;
    SEXP heights = layoutHeights(layout);
    for (i=0; i<layoutNRow(layout); i++) 
	if (!relativeHeights[i]) {
	    npcHeights[i] = transformHeight(heights, i, parentContext,
					    parentgc,
					    parentWidthCM, parentHeightCM, 
					    0, 0, dd)*2.54;
	    *heightLeftCM -= npcHeights[i];
	}
}

int colRespected(int col, SEXP layout) {
    int i;
    int result = 0;
    int respect = layoutRespect(layout);
    int *respectMat = layoutRespectMat(layout);
    if (respect == 1)
	result = 1;
    else
	for (i=0; i<layoutNRow(layout); i++)
	    if (respectMat[col*layoutNRow(layout) + i] != 0)
		result = 1;
    return result;
}

int rowRespected(int row, SEXP layout) {
    int i;
    int result = 0;
    int respect = layoutRespect(layout);
    int *respectMat = layoutRespectMat(layout);
    if (respect == 1)
	result = 1;
    else
	for (i=0; i<layoutNCol(layout); i++)
	    if (respectMat[i*layoutNRow(layout) + row] != 0)
		result = 1;
    return result;
}

/* 
 * These sum up ALL relative widths and heights (unit = "null")
 * Some effort is made to find all truly null units
 * (e.g., including a grobwidth unit where the grob's width is null)
 */
double totalWidth(SEXP layout, int *relativeWidths,
		  LViewportContext parentContext,
		  const pGEcontext parentgc,
		  pGEDevDesc dd)
{
    int i;
    SEXP widths = layoutWidths(layout);
    double totalWidth = 0;
    for (i=0; i<layoutNCol(layout); i++) 
	if (relativeWidths[i])
	    totalWidth += transformWidth(widths, i, parentContext, 
					 parentgc,
					 /* 
					  * NOTE: 0, 0, here is ok
					  * because we are only 
					  * obtaining "null" units
					  */
					 0, 0, 1, 0, dd);
    return totalWidth;
}

double totalHeight(SEXP layout, int *relativeHeights,
		   LViewportContext parentContext,
		   const pGEcontext parentgc,
		   pGEDevDesc dd)
{
    int i;
    SEXP heights = layoutHeights(layout);
    double totalHeight = 0;
    for (i=0; i<layoutNRow(layout); i++) 
	if (relativeHeights[i])
	    totalHeight += transformHeight(heights, i, parentContext, 
					   parentgc,
					   /* 
					    * NOTE: 0, 0, here is ok
					    * because we are only 
					    * obtaining "null" units
					    */
					   0, 0, 1, 0, dd);
    return totalHeight;
}

void allocateRespected(SEXP layout, 
		       int *relativeWidths, int *relativeHeights,
		       double *reducedWidthCM, double *reducedHeightCM,
		       LViewportContext parentContext,
		       const pGEcontext parentgc,
		       pGEDevDesc dd,
		       double *npcWidths, double *npcHeights)
{
    int i;
    SEXP widths = layoutWidths(layout);
    SEXP heights = layoutHeights(layout);
    int respect = layoutRespect(layout);
    double sumWidth = totalWidth(layout, relativeWidths, parentContext,
				 parentgc, dd);
    double sumHeight = totalHeight(layout, relativeHeights, parentContext,
				   parentgc, dd);
    double denom, mult;
    double tempWidthCM = *reducedWidthCM;
    double tempHeightCM = *reducedHeightCM;
    if (respect > 0) {
	/* Determine whether aspect ratio of available space is
	 * bigger or smaller than aspect ratio of layout
	 */
	// NB: widths could be zero
	// if ((tempHeightCM / tempWidthCM) > (sumHeight / sumWidth)) {
	if ( tempHeightCM * sumWidth > sumHeight * tempWidthCM) {
	    denom = sumWidth;
	    mult = tempWidthCM;
	}
	else {
	    denom = sumHeight;
	    mult = tempHeightCM;
	}
	/* Allocate respected widths 
	 */
	for (i=0; i<layoutNCol(layout); i++)
	    if (relativeWidths[i])
		if (colRespected(i, layout)) {
		    /* 
		     * Special case of respect, but sumHeight = 0.
		     * Action is to allocate widths as if unrespected.
		     * Ok to test == 0 because will only be 0 if 
		     * all relative heights are actually exactly 0.
		     */
		    if (sumHeight == 0) {
			denom = sumWidth;
			mult = tempWidthCM;
		    }
		    /* Build a unit SEXP with a single value and no data
		     */
		    npcWidths[i] = pureNullUnitValue(widths, i) / 
                        denom*mult;
		    *reducedWidthCM -= npcWidths[i];
		}
	/* Allocate respected heights
	 */
	for (i=0; i<layoutNRow(layout); i++)
	    if (relativeHeights[i])
		if (rowRespected(i, layout)) {
		    /* 
		     * Special case of respect, but sumWidth = 0.
		     * Action is to allocate widths as if unrespected.
		     * Ok to test == 0 because will only be 0 if 
		     * all relative heights are actually exactly 0.
		     */
		    if (sumWidth == 0) {
			denom = sumHeight;
			mult = tempHeightCM;
		    }
		    npcHeights[i] = pureNullUnitValue(heights, i) / 
                        denom*mult;
		    *reducedHeightCM -= npcHeights[i];
		}
    }
}

void setRespectedZero(SEXP layout, 
                      int *relativeWidths, int *relativeHeights,
                      double *npcWidths, double *npcHeights)
{
    int i;
    for (i=0; i<layoutNCol(layout); i++)
        if (relativeWidths[i])
            if (colRespected(i, layout)) 
                npcWidths[i] = 0;
    for (i=0; i<layoutNRow(layout); i++)
        if (relativeHeights[i])
            if (rowRespected(i, layout)) 
                npcHeights[i] = 0;
}

/* These sum up unrespected relative widths and heights (unit = "null")
 */
double totalUnrespectedWidth(SEXP layout, int *relativeWidths,
			     LViewportContext parentContext,
			     const pGEcontext parentgc,
			     pGEDevDesc dd)
{
    int i;
    SEXP widths = layoutWidths(layout);
    double totalWidth = 0;
    for (i=0; i<layoutNCol(layout); i++) 
	if (relativeWidths[i])
	    if (!colRespected(i, layout))
		totalWidth += transformWidth(widths, i, parentContext, 
					     parentgc,
					     /* 
					      * NOTE: 0, 0, here is ok
					      * because we are only 
					      * obtaining "null" units
					      */
					     0, 0, 1, 0, dd);
    return totalWidth;
}

double totalUnrespectedHeight(SEXP layout, int *relativeHeights,
			      LViewportContext parentContext,
			      const pGEcontext parentgc,
			      pGEDevDesc dd)
{
    int i;
    SEXP heights = layoutHeights(layout);
    double totalHeight = 0;
    for (i=0; i<layoutNRow(layout); i++) 
	if (relativeHeights[i])
	    if (!rowRespected(i, layout))
		totalHeight += transformHeight(heights, i, parentContext, 
					       parentgc,
					       /* 
						* NOTE: 0, 0, here is ok
						* because we are only 
						* obtaining "null" units
						*/
					       0, 0, 1, 0, dd);
    return totalHeight;
}


void setRemainingWidthZero(SEXP layout, 
                           int *relativeWidths, 
                           double *npcWidths)
{
    int i;
    for (i=0; i<layoutNCol(layout); i++)
        if (relativeWidths[i])
            if (!colRespected(i, layout)) 
                npcWidths[i] = 0;
}

void allocateRemainingWidth(SEXP layout, int *relativeWidths,
			    double remainingWidthCM, 
			    LViewportContext parentContext, 
			    const pGEcontext parentgc,
			    pGEDevDesc dd,
			    double *npcWidths)
{
    int i;
    SEXP widths = layoutWidths(layout);
    double sumWidth;
    sumWidth = totalUnrespectedWidth(layout, relativeWidths,
				     parentContext, parentgc, dd);
    if (sumWidth > 0) {
        for (i=0; i<layoutNCol(layout); i++) 
            if (relativeWidths[i])
                if (!colRespected(i, layout))
                    npcWidths[i] = remainingWidthCM*
                        transformWidth(widths, i, parentContext, parentgc,
				   /* 
				    * NOTE: 0, 0, here is ok
				    * because we are only 
				    * obtaining "null" units
				    */
                                       0, 0, 1, 0, dd)/
                        sumWidth;
    } else {
        /* 
         * If ALL relative widths are zero then they all get 
         * allocated zero width
         */
        setRemainingWidthZero(layout, relativeWidths, npcWidths);
    }
}

void setRemainingHeightZero(SEXP layout, 
                            int *relativeHeights, 
                            double *npcHeights)
{
    int i;
    for (i=0; i<layoutNRow(layout); i++)
        if (relativeHeights[i])
            if (!rowRespected(i, layout)) 
                npcHeights[i] = 0;
}

void allocateRemainingHeight(SEXP layout, int *relativeHeights,
			     double remainingHeightCM, 
			     LViewportContext parentContext,
			     const pGEcontext parentgc,
			     pGEDevDesc dd,
			     double *npcHeights)
{
    int i;
    SEXP heights = layoutHeights(layout);
    double sumHeight;
    sumHeight = totalUnrespectedHeight(layout, relativeHeights,
				       parentContext, parentgc, dd);
    if (sumHeight > 0) {
        for (i=0; i<layoutNRow(layout); i++) 
            if (relativeHeights[i])
                if (!rowRespected(i, layout))
                    npcHeights[i] = remainingHeightCM*
                        transformHeight(heights, i, parentContext, parentgc,
				    /* 
				     * NOTE: 0, 0, here is ok
				     * because we are only 
				     * obtaining "null" units
				     */
                                        0, 0, 1, 0, dd)/
                        sumHeight;
    } else {
        /* 
         * If ALL relative heights are zero then they all get 
         * allocated zero height
         */
        setRemainingHeightZero(layout, relativeHeights, npcHeights);
    }
}

static double sumDims(double dims[], int from, int to)
{
    int i;
    double s = 0;
    for (i = from; i < to + 1; i++)
	s = s + dims[i];
    return s;
}

static void subRegion(SEXP layout,
		      int minrow, int maxrow, int mincol, int maxcol,
		      double widths[], double heights[], 
                      double parentWidthCM, double parentHeightCM,
		      double *left, double *bottom, 
		      double *width, double *height) 
{
    double hjust = layoutHJust(layout);
    double vjust = layoutVJust(layout);
    double totalWidth = sumDims(widths, 0, layoutNCol(layout) - 1);
    double totalHeight = sumDims(heights, 0, layoutNRow(layout) - 1);
    *width = sumDims(widths, mincol, maxcol);
    *height = sumDims(heights, minrow, maxrow);    
    /* widths and heights are in CM */
    *left = parentWidthCM*hjust - totalWidth*hjust + 
        sumDims(widths, 0, mincol - 1);
    *bottom =  parentHeightCM*vjust + (1 - vjust)*totalHeight - 
	sumDims(heights, 0, maxrow);
    /*
     * From when hjust and vjust were enums
     *
    switch (layoutHJust(layout)) {
    case L_LEFT:
	*left = sumDims(widths, 0, mincol - 1);
	break;
    case L_RIGHT: 
	*left = 1 - sumDims(widths, mincol, layoutNCol(layout) - 1);
	break;
    case L_CENTRE: 
    case L_CENTER:
	*left = (0.5 - totalWidth/2) + sumDims(widths, 0, mincol - 1);
	break;
    }
    switch (layoutVJust(layout)) {
    case L_BOTTOM:
	*bottom = totalHeight - sumDims(heights, 0, maxrow);
	break;
    case L_TOP:
	*bottom = 1 - sumDims(heights, 0, maxrow);
	break;
    case L_CENTRE: 
    case L_CENTER:
	*bottom = (0.5 - totalHeight/2) + totalHeight
	    - sumDims(heights, 0, maxrow);
    }
    */
}

void calcViewportLayout(SEXP viewport,
			double parentWidthCM,
			double parentHeightCM,
			LViewportContext parentContext,
			const pGEcontext parentgc,
			pGEDevDesc dd)
{
    int i;
    SEXP currentWidths, currentHeights;
    SEXP layout = viewportLayout(viewport);
    double *npcWidths = (double *) R_alloc(layoutNCol(layout), sizeof(double));
    double *npcHeights = (double *) R_alloc(layoutNRow(layout), 
					    sizeof(double));
    int *relativeWidths = (int *) R_alloc(layoutNCol(layout), sizeof(int));
    int *relativeHeights = (int *) R_alloc(layoutNRow(layout), sizeof(int));    
    double reducedWidthCM = parentWidthCM;
    double reducedHeightCM = parentHeightCM;
    /* Figure out which rows and cols have relative heights and widths 
     */
    findRelWidths(layout, relativeWidths, dd);
    findRelHeights(layout, relativeHeights, dd);
    /* For any width or height which has a unit other than "null"
     * we can immediately figure out its physical size.
     * We do this and return the widthCM and heightCM 
     * remaining after these widths and heights have been allocated
     */
    allocateKnownWidths(layout, relativeWidths,
			parentWidthCM, parentHeightCM, 
			parentContext, parentgc,
			dd, npcWidths,
			&reducedWidthCM);
    allocateKnownHeights(layout, relativeHeights,
			 parentWidthCM, parentHeightCM, 
			 parentContext, parentgc,
			 dd, npcHeights, 
			 &reducedHeightCM);

    /* Now allocate respected widths and heights and return
     * widthCM and heightCM remaining 
     */
    if (reducedWidthCM > 0 ||
        reducedHeightCM > 0) {
        allocateRespected(layout, relativeWidths, relativeHeights, 
                          &reducedWidthCM, &reducedHeightCM,
                          parentContext, parentgc, dd,
                          npcWidths, npcHeights);
    } else {
        /* 
         * IF EITHER we started with ZERO widthCM and heightCM
         *    OR we've used up all the widthCM and heightCM
         * THEN all respected widths/heights get ZERO
         */ 
        setRespectedZero(layout, relativeWidths, relativeHeights, 
                         npcWidths, npcHeights);
    }
    /* Now allocate relative widths and heights (unit = "null")
     * in the remaining space
     */
    if (reducedWidthCM > 0) {
        allocateRemainingWidth(layout, relativeWidths,
                               reducedWidthCM, 
                               parentContext, parentgc, dd, npcWidths);
    } else {
        /* 
         * IF EITHER we started with ZERO width
         *    OR we've used up all the width
         * THEN any relative widths get ZERO
         */ 
        setRemainingWidthZero(layout, relativeWidths, npcWidths);
    }
    if (reducedHeightCM > 0) {
        allocateRemainingHeight(layout, relativeHeights,
                                reducedHeightCM, 
                                parentContext, parentgc, dd, npcHeights);
    } else {
        /* 
         * IF EITHER we started with ZERO height
         *    OR we've used up all the height
         * THEN any relative heights get ZERO
         */ 
        setRemainingHeightZero(layout, relativeHeights, npcHeights);
    }
    /* Record the widths and heights in the viewport
     */
    PROTECT(currentWidths = allocVector(REALSXP, layoutNCol(layout)));
    PROTECT(currentHeights = allocVector(REALSXP, layoutNRow(layout)));
    for (i=0; i<layoutNCol(layout); i++) {
        /* Layout widths are stored in CM
         */
	REAL(currentWidths)[i] = npcWidths[i];
    }
    for (i=0; i<layoutNRow(layout); i++) {
        /* Layout heights are stored in CM
         */
	REAL(currentHeights)[i] = npcHeights[i];
    }
    SET_VECTOR_ELT(viewport, PVP_WIDTHS, currentWidths);
    SET_VECTOR_ELT(viewport, PVP_HEIGHTS, currentHeights);
    UNPROTECT(2);
}

Rboolean checkPosRowPosCol(SEXP vp, SEXP parent) 
{
    int ncol = layoutNCol(viewportLayout(parent));
    int nrow = layoutNRow(viewportLayout(parent));
    if (!isNull(viewportLayoutPosRow(vp)) &&
	(INTEGER(viewportLayoutPosRow(vp))[0] < 1 ||
	 INTEGER(viewportLayoutPosRow(vp))[1] > nrow))
        error(_("invalid 'layout.pos.row'"));
    if (!isNull(viewportLayoutPosCol(vp)) &&
	(INTEGER(viewportLayoutPosCol(vp))[0] < 1 ||
	 INTEGER(viewportLayoutPosCol(vp))[1] > ncol))
        error(_("invalid 'layout.pos.col'"));
    return TRUE;
}

void calcViewportLocationFromLayout(SEXP layoutPosRow,
				    SEXP layoutPosCol,
				    SEXP parent,
				    LViewportLocation *vpl)
{
    int minrow, maxrow, mincol, maxcol;
    double x, y, width, height;
    SEXP vpx, vpy, vpwidth, vpheight;
    SEXP layout = viewportLayout(parent);
    /* It is possible for ONE of layoutPosRow and layoutPosCol to
     * be NULL;  this is interpreted as "occupy all rows/cols"
     * NOTE: The " - 1" is there because R is 1-based and C is zero-based 
     */
    if (isNull(layoutPosRow)) {
	minrow = 0;
	maxrow = layoutNRow(layout) - 1;
    } else {
	minrow = INTEGER(layoutPosRow)[0] - 1;
	maxrow = INTEGER(layoutPosRow)[1] - 1;
    }
    if (isNull(layoutPosCol)) {
	mincol = 0;
	maxcol = layoutNCol(layout) - 1;
    } else {
	mincol = INTEGER(layoutPosCol)[0] - 1;
	maxcol = INTEGER(layoutPosCol)[1] - 1;
    }
    /* Put the relevant values into vpl */
    subRegion(viewportLayout(parent), minrow, maxrow, mincol, maxcol,
	      REAL(viewportLayoutWidths(parent)), 
	      REAL(viewportLayoutHeights(parent)),
              REAL(viewportWidthCM(parent))[0],
              REAL(viewportHeightCM(parent))[0],
	      &x, &y, &width, &height);
    /* Layout widths and heights are stored in CM
     */
    PROTECT(vpx = unit(x, L_CM));
    vpl->x = vpx;
    PROTECT(vpy = unit(y, L_CM));
    vpl->y = vpy;
    PROTECT(vpwidth = unit(width, L_CM));
    vpl->width = vpwidth;
    PROTECT(vpheight = unit(height, L_CM));
    vpl->height = vpheight;
    vpl->hjust = 0;
    vpl->vjust = 0;
    /* Question:  Is there any chance that these newly-allocated 
     * unit SEXPs will get corrupted after this unprotect ??
     */
    UNPROTECT(4);
}

