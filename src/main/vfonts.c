/*
 *  R : A Computer Language for Statistical Data Analysis
  *  Copyright (C) 2001 The R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rdynpriv.h>
#include <Graphics.h>

typedef struct {
    R_GE_VTextRoutine GVText;
    R_GE_VStrWidthRoutine GVStrWidth;
    R_GE_VStrHeightRoutine GVStrHeight;
} VfontRoutines;

static VfontRoutines routines, *ptr = &routines;

/*
static double (*ptr_GVStrWidth)(const unsigned char *s, int typeface, 
				int fontindex,
				int unit, DevDesc *dd);
static double (*ptr_GVStrHeight)(const unsigned char *s, int typeface, 
				 int fontindex,
				 int unit, DevDesc *dd);
static void (*ptr_GVText)(double x, double y, int unit, char *s, 
			  int typeface, int fontindex,
			  double x_justify, double y_justify, double rotation,
			  DevDesc *dd);
*/

static int initialized = 0;

void
R_GE_setVFontRoutines(R_GE_VStrWidthRoutine vwidth, 
		      R_GE_VStrHeightRoutine vheight, 
		      R_GE_VTextRoutine vtext)
{
    ptr->GVStrWidth = vwidth;
    ptr->GVStrHeight = vheight;
    ptr->GVText = vtext;
}

static void vfonts_Init(void)
{
    int res = moduleCdynload("vfonts", 1, 1);
    initialized = -1;
    if(!res) return;
    initialized = 1;    
    return;
}

double GVStrWidth (const unsigned char *s, int typeface, int fontindex,
		   int unit, DevDesc *dd)
{
    return GConvertXUnits(R_GE_VStrWidth(s, typeface, fontindex, 1, 
					 Rf_gpptr(dd)->cex, 
					 Rf_gpptr(dd)->ps, 
					 (GEDevDesc *) dd),
			  DEVICE, unit, dd);
}

double R_GE_VStrWidth(const unsigned char *s, int typeface, int fontindex,
		      double lineheight, double cex, double ps, GEDevDesc *dd)
{
    if(!initialized) vfonts_Init();
    if(initialized > 0)
	return (*ptr->GVStrWidth)(s, typeface, fontindex, lineheight,
				  cex, ps, dd);
    else {
	error("Hershey fonts cannot be loaded");
	return 0.0;
    }
}

double GVStrHeight (const unsigned char *s, int typeface, int fontindex,
		    int unit, DevDesc *dd)
{
    return GConvertYUnits(R_GE_VStrHeight(s, typeface, fontindex, 1, 
					  Rf_gpptr(dd)->cex, 
					  Rf_gpptr(dd)->ps, 
					  (GEDevDesc *) dd),
			  DEVICE, unit, dd);
}

double R_GE_VStrHeight(const unsigned char *s, int typeface, int fontindex,
		       double lineheight, double cex, double ps, GEDevDesc *dd)
{
    if(!initialized) vfonts_Init();
    if(initialized > 0)
	return (*ptr->GVStrHeight)(s, typeface, fontindex, lineheight,
				   cex, ps, dd);
    else {
	error("Hershey fonts cannot be loaded");
	return 0.0;
    }
}

void GVText (double x, double y, int unit, char *s, 
	     int typeface, int fontindex,
	     double x_justify, double y_justify, double rotation,
	     DevDesc *dd)
{
    /* 
     * Ensure that the current par(xpd) settings are enforced.
     */
    GClip(dd);
    GConvert(&x, &y, unit, DEVICE, dd);
    R_GE_VText(x, y, s, typeface, fontindex, x_justify, y_justify,
	       rotation, Rf_gpptr(dd)->col, Rf_gpptr(dd)->gamma,
	       1 /* lineheight */, Rf_gpptr(dd)->cex, Rf_gpptr(dd)->ps,
	       (GEDevDesc *) dd);
}

void R_GE_VText(double x, double y, char *s, 
		int typeface, int fontindex,
		double x_justify, double y_justify, double rotation,
		int col, double gamma, double lineheight,
		double cex, double ps,
		GEDevDesc *dd)
{
    if(!initialized) vfonts_Init();
    if(initialized > 0)
	(*ptr->GVText)(x, y, s, typeface, fontindex, 
		       x_justify, y_justify, rotation, 
		       col, gamma, lineheight, cex, ps, dd);
    else
	error("Hershey fonts cannot be loaded");
}
