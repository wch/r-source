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
#include <Rgraphics.h>
#include "R_ext/Rdynpriv.h"

typedef double  (*dblDL_FUNC)();
static dblDL_FUNC ptr_GVStrWidth, ptr_GVStrHeight;
static DL_FUNC ptr_GVText;

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

static void vfonts_Init(void)
{
    int res = moduleCdynload("vfonts", 1, 1);
    initialized = -1;
    if(!res) return;
    
    if(!(ptr_GVStrWidth =  (dblDL_FUNC)R_FindSymbol("Rvf_GVStrWidth", 
						    "vfonts", NULL))) return;
    if(!(ptr_GVStrHeight = (dblDL_FUNC)R_FindSymbol("Rvf_GVStrHeight", 
						    "vfonts", NULL))) return;
    if(!(ptr_GVText = R_FindSymbol("Rvf_GVText", "vfonts", NULL))) return;
    initialized = 1;    
    return;
}

double GVStrWidth (const unsigned char *s, int typeface, int fontindex,
		   int unit, DevDesc *dd)
{
    if(!initialized) vfonts_Init();
    if(initialized > 0)
	return (*ptr_GVStrWidth)(s, typeface, fontindex, unit, dd);
    else {
	error("Hershey fonts cannot be loaded");
	return 0.0;
    }
}

double GVStrHeight (const unsigned char *s, int typeface, int fontindex,
		    int unit, DevDesc *dd)
{
    if(!initialized) vfonts_Init();
    if(initialized > 0)
	return (*ptr_GVStrHeight)(s, typeface, fontindex, unit, dd);
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
    if(!initialized) vfonts_Init();
    if(initialized > 0)
	(*ptr_GVText)(x, y, unit, s, typeface, fontindex, 
		      x_justify, y_justify, rotation, dd);
    else
	error("Hershey fonts cannot be loaded");
}
