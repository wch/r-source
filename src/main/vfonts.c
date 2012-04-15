/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-8 The R Core Team
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

/* This should be regarded as part of the graphics engine */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rdynpriv.h>
#include <R_ext/GraphicsEngine.h>
#include <Rmodules/Rvfonts.h>

static VfontRoutines routines;
static int initialized = 0;

void
R_GE_setVFontRoutines(R_GE_VStrWidthRoutine vwidth,
		      R_GE_VStrHeightRoutine vheight,
		      R_GE_VTextRoutine vtext)
{
    routines.GEVStrWidth = vwidth;
    routines.GEVStrHeight = vheight;
    routines.GEVText = vtext;
}

static void vfonts_Init(void)
{
    int res = R_moduleCdynload("vfonts", 1, 1);
    initialized = -1;
    if(!res) return;
    if(!routines.GEVStrWidth)
	error(_("vfont routines cannot be accessed in module"));
    initialized = 1;
    return;
}

attribute_hidden
double R_GE_VStrWidth(const char *s, cetype_t enc, const pGEcontext gc, pGEDevDesc dd)
{
    double res;
    if(!initialized) vfonts_Init();
    if(initialized > 0) {
	const void *vmax = vmaxget();
	const char *str = reEnc(s, enc, CE_LATIN1, 2 /* '.' */);
	res = (*routines.GEVStrWidth)(str, gc, dd);
	vmaxset(vmax);
	return res;
    } else {
	error(_("Hershey fonts cannot be loaded"));
	return 0.0; /* -Wall */
    }
}

attribute_hidden
double R_GE_VStrHeight(const char *s, cetype_t enc, const pGEcontext gc, pGEDevDesc dd)
{
    if(!initialized) vfonts_Init();
    if(initialized > 0) {
	/* The strheight does not depend on the encoding. */
	return (*routines.GEVStrHeight)(s, gc, dd);
    } else {
	error(_("Hershey fonts cannot be loaded"));
	return 0.0;
    }
}


attribute_hidden
void R_GE_VText(double x, double y, const char * const s, cetype_t enc,
		double x_justify, double y_justify, double rotation,
		const pGEcontext gc, pGEDevDesc dd)
{
    if(!initialized) vfonts_Init();
    if(initialized > 0) {
	const void *vmax = vmaxget();
	const char *str = reEnc(s, enc, CE_LATIN1, 2 /* '.' */);
	(*routines.GEVText)(x, y, str, x_justify, y_justify, rotation, gc, dd);
	vmaxset(vmax);
    } else
	error(_("Hershey fonts cannot be loaded"));
}
