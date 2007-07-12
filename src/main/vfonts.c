/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-7 The R Development Core Team
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
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
double GVStrWidth (const char *s, int typeface, int fontindex,
		   int unit, DevDesc *dd)
{
    R_GE_gcontext gc;
    char *str = (char *)s;
#ifdef SUPPORT_MBCS
    char *buff;
    Rboolean conv = mbcslocale;
#endif
    gcontextFromGP(&gc, dd);
    gc.fontface = typeface;
    gc.fontfamily[0] = fontindex;
#ifdef SUPPORT_MBCS
    if(typeface == 0  && (fontindex == 5 || fontindex == 6)) conv = FALSE;
    if(conv && !utf8strIsASCII(str)) {
	buff = alloca(strlen(str)+1); /* Output string cannot be longer */
        R_CheckStack();
	if(!buff) error(_("allocation failure in GVStrWidth"));
	mbcsToLatin1((char*) s, buff);
	str = buff;
    }
#endif
    return GConvertXUnits(R_GE_VStrWidth(str, &gc, (GEDevDesc *) dd),
			  DEVICE, unit, dd);
}

attribute_hidden
double R_GE_VStrWidth(const char *s, R_GE_gcontext *gc, GEDevDesc *dd)
{
    if(!initialized) vfonts_Init();
    if(initialized > 0)
	return (*ptr->GEVStrWidth)(s, gc, dd);
    else {
	error(_("Hershey fonts cannot be loaded"));
	return 0.0;
    }
}

attribute_hidden
double GVStrHeight (const char *s, int typeface, int fontindex,
		    int unit, DevDesc *dd)
{
    R_GE_gcontext gc;
    char *str = (char *)s;
#ifdef SUPPORT_MBCS
    char *buff;
    Rboolean conv = mbcslocale;
#endif
    gcontextFromGP(&gc, dd);
    gc.fontface = typeface;
    gc.fontfamily[0] = fontindex;
#ifdef SUPPORT_MBCS
    if(typeface == 0  && (fontindex == 5 || fontindex == 6)) conv = FALSE;
    if(conv && !utf8strIsASCII(str)) {
	buff = alloca(strlen(str)+1); /* Output string cannot be longer */
        R_CheckStack();
	if(!buff) error(_("allocation failure in GVStrHeight"));
	mbcsToLatin1((char *) s, buff);
	str = buff;
    }
#endif
    return GConvertYUnits(R_GE_VStrHeight(str, &gc, (GEDevDesc *) dd),
			  DEVICE, unit, dd);
}

attribute_hidden
double R_GE_VStrHeight(const char *s, R_GE_gcontext *gc, GEDevDesc *dd)
{
    if(!initialized) vfonts_Init();
    if(initialized > 0)
	return (*ptr->GEVStrHeight)(s, gc, dd);
    else {
	error(_("Hershey fonts cannot be loaded"));
	return 0.0;
    }
}

attribute_hidden
void GVText (double x, double y, int unit, const char *s,
	     int typeface, int fontindex,
	     double x_justify, double y_justify, double rotation,
	     DevDesc *dd)
{
    R_GE_gcontext gc;
    const char *str = s;
#ifdef SUPPORT_MBCS
    char *buff;
    Rboolean conv = mbcslocale;
#endif
    gcontextFromGP(&gc, dd);
    /*
     * Ensure that the current par(xpd) settings are enforced.
     */
    GClip(dd);
    GConvert(&x, &y, unit, DEVICE, dd);
    gc.fontface = fontindex;
    gc.fontfamily[0] = typeface;
#ifdef SUPPORT_MBCS
    if(typeface == 0  && (fontindex == 5 || fontindex == 6)) conv = FALSE;
    if(conv && !utf8strIsASCII(str)) {
	buff = alloca(strlen(str)+1); /* Output string cannot be longer */
	R_CheckStack();
	if(!buff) error(_("allocation failure in GVText"));
	mbcsToLatin1(s, buff);
	str = buff;
    }
#endif
    R_GE_VText(x, y, str, x_justify, y_justify, rotation,
	       &gc, (GEDevDesc *) dd);
}

attribute_hidden
void R_GE_VText(double x, double y, const char * const s,
		double x_justify, double y_justify, double rotation,
		R_GE_gcontext *gc,
		GEDevDesc *dd)
{
    if(!initialized) vfonts_Init();
    if(initialized > 0)
	(*ptr->GEVText)(x, y, s, x_justify, y_justify, rotation, gc, dd);
    else
	error(_("Hershey fonts cannot be loaded"));
}
