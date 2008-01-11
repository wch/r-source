/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-8 The R Development Core Team
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

/* <UTF8> char here is either ASCII or handled as a whole.
   However, the interpretation is as ASCII or in some cases Latin-1
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rdynpriv.h>
#include <Graphics.h>
#include <Rmodules/Rvfonts.h>

static VfontRoutines routines, *ptr = &routines;


static int initialized = 0;

void
R_GE_setVFontRoutines(R_GE_VStrWidthRoutine vwidth,
		      R_GE_VStrHeightRoutine vheight,
		      R_GE_VTextRoutine vtext)
{
    ptr->GEVStrWidth = vwidth;
    ptr->GEVStrHeight = vheight;
    ptr->GEVText = vtext;
}

static void vfonts_Init(void)
{
    int res = R_moduleCdynload("vfonts", 1, 1);
    initialized = -1;
    if(!res) return;
    if(!ptr->GEVStrWidth)
	error(_("vfont routines cannot be accessed in module"));
    initialized = 1;
    return;
}

attribute_hidden
double R_GE_VStrWidth(const char *s, int enc, R_GE_gcontext *gc, GEDevDesc *dd)
{
    if(!initialized) vfonts_Init();
    if(initialized > 0) {
	const char *str = reEnc(s, enc, CE_LATIN1, 2 /* '.' */);
	return (*ptr->GEVStrWidth)(str, gc, dd);
    } else {
	error(_("Hershey fonts cannot be loaded"));
	return 0.0;
    }
}

attribute_hidden
double R_GE_VStrHeight(const char *s, int enc, R_GE_gcontext *gc, GEDevDesc *dd)
{
    if(!initialized) vfonts_Init();
    if(initialized > 0) {
	/* The strheight does not depend on the encoding. */
	return (*ptr->GEVStrHeight)(s, gc, dd);
    } else {
	error(_("Hershey fonts cannot be loaded"));
	return 0.0;
    }
}


attribute_hidden
void R_GE_VText(double x, double y, const char * const s, int enc,
		double x_justify, double y_justify, double rotation,
		R_GE_gcontext *gc,
		GEDevDesc *dd)
{
    if(!initialized) vfonts_Init();
    if(initialized > 0) {
	const char *str = reEnc(s, enc, CE_LATIN1, 2 /* '.' */);
	(*ptr->GEVText)(x, y, str, x_justify, y_justify, rotation, gc, dd);
    } else
	error(_("Hershey fonts cannot be loaded"));
}
