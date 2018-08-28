/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2018  The R Core Team
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2002--2011  The R Foundation
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


 *  This is an extensive reworking by Paul Murrell of an original
 *  quick hack by Ross Ihaka designed to give a superset of the
 *  functionality in the AT&T Bell Laboratories GRZ library.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <float.h> /* for DBL_EPSILON etc */
#include <Graphics.h>
// --> R_ext/GraphicsEngine.h + Rgraphics.h
#include <GraphicsBase.h>       /* setBaseDevice */
#include <Rmath.h>		/* eg. fmax2() */

#ifdef ENABLE_NLS
#include <libintl.h>
#undef _
#define _(String) dgettext ("grDevices", String)
#else
#define _(String) (String)
#endif

/*--->> Documentation now in  ../include/Rgraphics.h  "API" ----- */

double R_Log10(double x)
{
    return (R_FINITE(x) && x > 0.0) ? log10(x) : NA_REAL;
}

/*-------------------------------------------------------------------
 *
 *  TRANSFORMATIONS
 *
 *    There are five major regions on a device, for any
 *    particular figure:  the outer margins, which "stick"
 *    to the edges of the device;  the inner region, which
 *    is defined as the total device less the outer margins;
 *    the figure region, which defaults from the current
 *    layout (mfrow, mfcol, layout) unless the user specifies
 *    it directly (fig, fin);  the figure margins, which
 *    "stick" to the edges of the plot region;	and thed
 *    plot region, which is the figure region less the figure
 *    margins by default unless the user specifies it directly
 *    (plt, pin)
 *
 *  COORDINATE SYSTEMS
 *
 *    DEVICE  = devices natural coordinate system
 *		(e.g., pixels, 1/72", ...)
 *    NDC     = normalised device coordinates (0..1 on device)
 *    INCHES  = inches
 *    OMA1..4 = outer margin coordinates
 *    NIC     = normalised inner region coordinates
 *		(0..1 on inner region)
 *    NFC     = normalised figure coordinates
 *		(0..1 on figure region)
 *    MAR1..4 = figure margin coordinates
 *    NPC     = normalised plot coordinates
 *		(0..1 on plot region)
 *    USER    = world or data coordinates
 *
 *
 *  UNITS
 *
 *    All of the above, except OMA1..4 and MAR1..4, plus ...
 *
 *	LINES = line coordinates (lines of margin;  based on mex)
 *	CHARS = char coordinates (lines of text;  based on cex)
 *
 *    The function Convert(value, from, to) is provided
 *    to transform between any pair of coordinate systems
 *    (for transforming locations)
 *
 *    The functions ConvertXUnits(value, from, to) and
 *    ConvertYUnits(value, from, to) are provided to transform
 *    between any pair of units (for transforming dimensions)
 *
 *    IMPORTANT:  if user coordinates are logged, then the
 *    conversion to/from USER units will not work.  in this
 *    case it is necessary to use convert(x1) - convert(x2)
 *    rather than convert(x1 - x2)
 *
 */


/* In interpreted R, units are as follows:
 *	1 = "user"
 *	2 = "figure"
 *	3 = "inches"
 * the function GMapUnits provides a mapping
 * between interpreted units and internal units.
 */
GUnit GMapUnits(int Runits)
{
    switch (Runits) {
    case 1:	return USER;
    case 2:	return NFC;
    case 3:	return INCHES;
    default:	return 0;
    }
}

	/* Conversions Between Units*/

/* Used to be global (non-static) -- but are nowhere declared.
 * The public interface is through G[XY]ConvertUnits() */

static double xNDCtoDevUnits(double x, pGEDevDesc dd)
{
    return x*fabs(gpptr(dd)->ndc2dev.bx);
}

static double yNDCtoDevUnits(double y, pGEDevDesc dd)
{
    return y*fabs(gpptr(dd)->ndc2dev.by);
}

static double xNICtoDevUnits(double x, pGEDevDesc dd)
{
    return x*fabs(gpptr(dd)->inner2dev.bx);
}

static double yNICtoDevUnits(double y, pGEDevDesc dd)
{
    return y*fabs(gpptr(dd)->inner2dev.by);
}

static double xNFCtoDevUnits(double x, pGEDevDesc dd)
{
    return x*fabs(gpptr(dd)->fig2dev.bx);
}

static double yNFCtoDevUnits(double y, pGEDevDesc dd)
{
    return y*fabs(gpptr(dd)->fig2dev.by);
}

static double xNPCtoDevUnits(double x, pGEDevDesc dd)
{
    return xNFCtoDevUnits(x*(gpptr(dd)->plt[1] - gpptr(dd)->plt[0]), dd);
}

static double yNPCtoDevUnits(double y, pGEDevDesc dd)
{
    return yNFCtoDevUnits(y*(gpptr(dd)->plt[3] - gpptr(dd)->plt[2]), dd);
}

static double xUsrtoDevUnits(double x, pGEDevDesc dd)
{
    return xNFCtoDevUnits(x*gpptr(dd)->win2fig.bx, dd);
}

static double yUsrtoDevUnits(double y, pGEDevDesc dd)
{
    return yNFCtoDevUnits(y*gpptr(dd)->win2fig.by, dd);
}

static double xInchtoDevUnits(double x, pGEDevDesc dd)
{
    return xNDCtoDevUnits(x*gpptr(dd)->xNDCPerInch, dd);
}

static double yInchtoDevUnits(double y, pGEDevDesc dd)
{
    return yNDCtoDevUnits(y*gpptr(dd)->yNDCPerInch, dd);
}

static double xLinetoDevUnits(double x, pGEDevDesc dd)
{
    return xNDCtoDevUnits(x*gpptr(dd)->xNDCPerLine, dd);
}

static double yLinetoDevUnits(double y, pGEDevDesc dd)
{
    return yNDCtoDevUnits(y*gpptr(dd)->yNDCPerLine, dd);
}

static double xChartoDevUnits(double x, pGEDevDesc dd)
{
    return xNDCtoDevUnits(x*gpptr(dd)->cex*gpptr(dd)->xNDCPerChar, dd);
}

static double yChartoDevUnits(double y, pGEDevDesc dd)
{
    return yNDCtoDevUnits(y*gpptr(dd)->cex*gpptr(dd)->yNDCPerChar, dd);
}

static double xDevtoNDCUnits(double x, pGEDevDesc dd)
{
    return x/fabs(gpptr(dd)->ndc2dev.bx);
}

static double yDevtoNDCUnits(double y, pGEDevDesc dd)
{
    return y/fabs(gpptr(dd)->ndc2dev.by);
}

static double xDevtoNICUnits(double x, pGEDevDesc dd)
{
    return x/fabs(gpptr(dd)->inner2dev.bx);
}

static double yDevtoNICUnits(double y, pGEDevDesc dd)
{
    return y/fabs(gpptr(dd)->inner2dev.by);
}

static double xDevtoNFCUnits(double x, pGEDevDesc dd)
{
    return x/fabs(gpptr(dd)->fig2dev.bx);
}

static double yDevtoNFCUnits(double y, pGEDevDesc dd)
{
    return y/fabs(gpptr(dd)->fig2dev.by);
}

static double xDevtoNPCUnits(double x, pGEDevDesc dd)
{
    return xDevtoNFCUnits(x, dd)/(gpptr(dd)->plt[1] - gpptr(dd)->plt[0]);
}

static double yDevtoNPCUnits(double y, pGEDevDesc dd)
{
    return yDevtoNFCUnits(y, dd)/(gpptr(dd)->plt[3] - gpptr(dd)->plt[2]);
}

static double xDevtoUsrUnits(double x, pGEDevDesc dd)
{
    return xDevtoNFCUnits(x, dd)/gpptr(dd)->win2fig.bx;
}

static double yDevtoUsrUnits(double y, pGEDevDesc dd)
{
    return yDevtoNFCUnits(y, dd)/gpptr(dd)->win2fig.by;
}

static double xDevtoInchUnits(double x, pGEDevDesc dd)
{
    return xDevtoNDCUnits(x, dd)/gpptr(dd)->xNDCPerInch;
}

static double yDevtoInchUnits(double y, pGEDevDesc dd)
{
    return yDevtoNDCUnits(y, dd)/gpptr(dd)->yNDCPerInch;
}

static double xDevtoLineUnits(double x, pGEDevDesc dd)
{
    return xDevtoNDCUnits(x, dd)/gpptr(dd)->xNDCPerLine;
}

static double yDevtoLineUnits(double y, pGEDevDesc dd)
{
    return yDevtoNDCUnits(y, dd)/gpptr(dd)->yNDCPerLine;
}

/* NOTE that use the _current_ gpptr(dd)->cex here */
/* the conversion for lines doesn't have to worry about */
/* this because gpptr(dd)->mex can only be set once per plot */

static double xDevtoCharUnits(double x, pGEDevDesc dd)
{
    return xDevtoNDCUnits(x, dd)/(gpptr(dd)->cex * gpptr(dd)->xNDCPerChar);
}

static double yDevtoCharUnits(double y, pGEDevDesc dd)
{
    return yDevtoNDCUnits(y, dd)/(gpptr(dd)->cex * gpptr(dd)->yNDCPerChar);
}

static void NORET BadUnitsError(const char *where)
{
    error(_("bad units specified in '%s'"), where);
}

/* GConvertXUnits() and GConvertYUnits() convert
   a single value fromUnits toUnits : */

double GConvertXUnits(double x, GUnit fromUnits, GUnit toUnits, pGEDevDesc dd)
{
    double dev, final;
    switch (fromUnits) {
    case DEVICE: dev = x; break;
    case NDC:	 dev = xNDCtoDevUnits(x, dd); break;
    case NIC:	 dev = xNICtoDevUnits(x, dd); break;
    case NFC:	 dev = xNFCtoDevUnits(x, dd); break;
    case NPC:	 dev = xNPCtoDevUnits(x, dd); break;
    case USER:	 dev = xUsrtoDevUnits(x, dd); break;
    case INCHES: dev = xInchtoDevUnits(x, dd); break;
    case LINES:	 dev = xLinetoDevUnits(x, dd); break;
    case CHARS:	 dev = xChartoDevUnits(x, dd); break;
    default:	 dev = 0; BadUnitsError("GConvertXUnits");

    }
    switch (toUnits) {
    case DEVICE: final = dev; break;
    case NDC:	 final = xDevtoNDCUnits(dev, dd); break;
    case NIC:	 final = xDevtoNICUnits(dev, dd); break;
    case NFC:	 final = xDevtoNFCUnits(dev, dd); break;
    case NPC:	 final = xDevtoNPCUnits(dev, dd); break;
    case USER:	 final = xDevtoUsrUnits(dev, dd); break;
    case INCHES: final = xDevtoInchUnits(dev, dd); break;
    case LINES:	 final = xDevtoLineUnits(dev, dd); break;
    case CHARS:	 final = xDevtoCharUnits(dev, dd); break;
    default:	 final = 0; BadUnitsError("GConvertXUnits");
    }
    return final;
}

double GConvertYUnits(double y, GUnit fromUnits, GUnit toUnits, pGEDevDesc dd)
{
    double dev, final;
    switch (fromUnits) {
    case DEVICE: dev = y; break;
    case NDC:	 dev = yNDCtoDevUnits(y, dd); break;
    case NIC:	 dev = yNICtoDevUnits(y, dd); break;
    case NFC:	 dev = yNFCtoDevUnits(y, dd); break;
    case NPC:	 dev = yNPCtoDevUnits(y, dd); break;
    case USER:	 dev = yUsrtoDevUnits(y, dd); break;
    case INCHES: dev = yInchtoDevUnits(y, dd); break;
    case LINES:	 dev = yLinetoDevUnits(y, dd); break;
    case CHARS:	 dev = yChartoDevUnits(y, dd); break;
    default:	 dev = 0; BadUnitsError("GConvertYUnits");
    }
    switch (toUnits) {
    case DEVICE: final = dev; break;
    case NDC:	 final = yDevtoNDCUnits(dev, dd); break;
    case NIC:	 final = yDevtoNICUnits(dev, dd); break;
    case NFC:	 final = yDevtoNFCUnits(dev, dd); break;
    case NPC:	 final = yDevtoNPCUnits(dev, dd); break;
    case USER:	 final = yDevtoUsrUnits(dev, dd); break;
    case INCHES: final = yDevtoInchUnits(dev, dd); break;
    case LINES:	 final = yDevtoLineUnits(dev, dd); break;
    case CHARS:	 final = yDevtoCharUnits(dev, dd); break;
    default:	 final = 0; BadUnitsError("GConvertYUnits");
    }
    return final;
}

/* Functions to convert locations from one coordinate system to another */

/* OTHER coordinate systems to DEVICE */

/* Used to be global (non-static) -- but are nowhere declared.
 * The public interface is  GConvert(), GConvertX(), GConvertY() */
static double xNDCtoDev(double x, pGEDevDesc dd)
{
    return gpptr(dd)->ndc2dev.ax + x*gpptr(dd)->ndc2dev.bx;
}

static double yNDCtoDev(double y, pGEDevDesc dd)
{
    return gpptr(dd)->ndc2dev.ay + y*gpptr(dd)->ndc2dev.by;
}

static double xInchtoDev(double x, pGEDevDesc dd)
{
    return xNDCtoDev(x*gpptr(dd)->xNDCPerInch, dd);
}

static double yInchtoDev(double y, pGEDevDesc dd)
{
    return yNDCtoDev(y*gpptr(dd)->yNDCPerInch, dd);
}

static double xLinetoDev(double x, pGEDevDesc dd)
{
    return xNDCtoDev(x*gpptr(dd)->xNDCPerLine, dd);
}

static double yLinetoDev(double y, pGEDevDesc dd)
{
    return yNDCtoDev(y*gpptr(dd)->yNDCPerLine, dd);
}

static double xNICtoDev(double x, pGEDevDesc dd)
{
    return gpptr(dd)->inner2dev.ax + x*gpptr(dd)->inner2dev.bx;
}

static double yNICtoDev(double y, pGEDevDesc dd)
{
    return gpptr(dd)->inner2dev.ay + y*gpptr(dd)->inner2dev.by;
}
/* NOTE that an x-coordinate in OMA2 or OMA4 converts to a */
/* y-coordinate in Dev and a y-coordinate in OMA2 or OMA4 */
/* converts to an x-coordinate in Dev */

static double xOMA1toDev(double x, pGEDevDesc dd)
{
    return xNICtoDev(x, dd);
}

static double yOMA1toDev(double y, pGEDevDesc dd)
{
    return yLinetoDev((gpptr(dd)->oma[0] - y), dd);
}

static double xOMA2toyDev(double x, pGEDevDesc dd)
{
    return yNICtoDev(x, dd);
}

static double yOMA2toxDev(double y, pGEDevDesc dd)
{
    return xLinetoDev((gpptr(dd)->oma[1] - y), dd);
}

static double xOMA3toDev(double x, pGEDevDesc dd)
{
    return xNICtoDev(x, dd);
}

static double yOMA3toDev(double y, pGEDevDesc dd)
{
    double ndc = 1.0-yDevtoNDC(yLinetoDev((gpptr(dd)->oma[2] - y), dd), dd);
    return yNDCtoDev(ndc, dd);
}

static double xOMA4toyDev(double x, pGEDevDesc dd)
{
    return yNICtoDev(x, dd);
}

static double yOMA4toxDev(double y, pGEDevDesc dd)
{
    double ndc = 1.0-xDevtoNDC(xLinetoDev(gpptr(dd)->oma[3]-y, dd), dd);
    return xNDCtoDev(ndc, dd);
}

static double xNFCtoDev(double x, pGEDevDesc dd)
{
    return gpptr(dd)->fig2dev.ax + x*gpptr(dd)->fig2dev.bx;
}

static double yNFCtoDev(double y, pGEDevDesc dd)
{
    return gpptr(dd)->fig2dev.ay + y*gpptr(dd)->fig2dev.by;
}

static double xNPCtoDev(double x, pGEDevDesc dd)
{
    return xNFCtoDev(gpptr(dd)->plt[0] +
		     x*(gpptr(dd)->plt[1] - gpptr(dd)->plt[0]), dd);
}

static double yNPCtoDev(double y, pGEDevDesc dd)
{
    return yNFCtoDev(gpptr(dd)->plt[2] +
		     y*(gpptr(dd)->plt[3] - gpptr(dd)->plt[2]), dd);
}

static double xUsrtoDev(double x, pGEDevDesc dd)
{
    if (gpptr(dd)->xlog)
	x = R_Log10(x);
    return xNFCtoDev(gpptr(dd)->win2fig.ax + x*gpptr(dd)->win2fig.bx, dd);
}

static double yUsrtoDev(double y, pGEDevDesc dd)
{
    if (gpptr(dd)->ylog)
	y = R_Log10(y);
    return yNFCtoDev(gpptr(dd)->win2fig.ay + y*gpptr(dd)->win2fig.by, dd);
}

/* NOTE that an x-coordinate in MAR2 or MAR4 converts to a */
/* y-coordinate in Dev and a y-coordinate in MAR2 or MAR4 */
/* converts to an x-coordinate in Dev */

static double xMAR1toDev(double x, pGEDevDesc dd)
{
    return xUsrtoDev(x, dd);
}

static double yMAR1toDev(double y, pGEDevDesc dd)
{
    double nfc = GConvertYUnits(y, LINES, NFC, dd);
    return yNFCtoDev(gpptr(dd)->plt[2] - nfc, dd);
}

static double xMAR2toyDev(double x, pGEDevDesc dd)
{
    return yUsrtoDev(x, dd);
}

static double yMAR2toxDev(double y, pGEDevDesc dd)
{
    double nfc = GConvertXUnits(y, LINES, NFC, dd);
    return xNFCtoDev(gpptr(dd)->plt[0] - nfc, dd);
}

static double xMAR3toDev(double x, pGEDevDesc dd)
{
    return xUsrtoDev(x, dd);
}

static double yMAR3toDev(double y, pGEDevDesc dd)
{
    double nfc = GConvertYUnits(y, LINES, NFC, dd);
    return yNFCtoDev(gpptr(dd)->plt[3] + nfc, dd);
}

static double xMAR4toyDev(double x, pGEDevDesc dd)
{
    return yUsrtoDev(x, dd);
}

static double yMAR4toxDev(double y, pGEDevDesc dd)
{
    double nfc = GConvertXUnits(y, LINES, NFC, dd);
    return xNFCtoDev(gpptr(dd)->plt[1] + nfc, dd);
}

/* DEVICE coordinates to OTHER */

double xDevtoNDC(double x, pGEDevDesc dd)
{
    return (x - gpptr(dd)->ndc2dev.ax)/gpptr(dd)->ndc2dev.bx;
}

double yDevtoNDC(double y, pGEDevDesc dd)
{
    return (y - gpptr(dd)->ndc2dev.ay)/gpptr(dd)->ndc2dev.by;
}

static double xDevtoInch(double x, pGEDevDesc dd)
{
    return xDevtoNDC(x, dd)/gpptr(dd)->xNDCPerInch;
}

static double yDevtoInch(double y, pGEDevDesc dd)
{
    return yDevtoNDC(y, dd)/gpptr(dd)->yNDCPerInch;
}

static double xDevtoLine(double x, pGEDevDesc dd)
{
  return xDevtoNDC(x, dd)/gpptr(dd)->xNDCPerLine;
}

static double yDevtoLine(double y, pGEDevDesc dd)
{
    return yDevtoNDC(y, dd)/gpptr(dd)->yNDCPerLine;
}

static double xDevtoNIC(double x, pGEDevDesc dd)
{
    return (x - gpptr(dd)->inner2dev.ax)/gpptr(dd)->inner2dev.bx;
}

static double yDevtoNIC(double y, pGEDevDesc dd)
{
    return (y - gpptr(dd)->inner2dev.ay)/gpptr(dd)->inner2dev.by;
}

static double xDevtoOMA1(double x, pGEDevDesc dd)
{
    return xDevtoNIC(x, dd);
}

static double yDevtoOMA1(double y, pGEDevDesc dd)
{
    return gpptr(dd)->oma[0] - yDevtoLine(y, dd);
}

static double xDevtoyOMA2(double x, pGEDevDesc dd)
{
    return gpptr(dd)->oma[1] - xDevtoLine(x, dd);
}

static double yDevtoxOMA2(double y, pGEDevDesc dd)
{
    return yDevtoNIC(y, dd);
}

static double xDevtoOMA3(double x, pGEDevDesc dd)
{
    return xDevtoNIC(x, dd);
}

static double yDevtoOMA3(double y, pGEDevDesc dd)
{
    double line = (1.0 - yDevtoNDC(y, dd))/gpptr(dd)->yNDCPerLine;
    return gpptr(dd)->oma[2] - line;
}

static double xDevtoyOMA4(double x, pGEDevDesc dd)
{
    double line = (1.0 - xDevtoNDC(x, dd))/gpptr(dd)->xNDCPerLine;
    return gpptr(dd)->oma[3] - line;
}

static double yDevtoxOMA4(double y, pGEDevDesc dd)
{
    return yDevtoNIC(y, dd);
}

double xDevtoNFC(double x, pGEDevDesc dd)
{
    return (x - gpptr(dd)->fig2dev.ax)/gpptr(dd)->fig2dev.bx;
}

double yDevtoNFC(double y, pGEDevDesc dd)
{
    return (y - gpptr(dd)->fig2dev.ay)/gpptr(dd)->fig2dev.by;
}

double xDevtoNPC(double x, pGEDevDesc dd)
{
    return (xDevtoNFC(x, dd) - gpptr(dd)->plt[0])/
	(gpptr(dd)->plt[1] - gpptr(dd)->plt[0]);
}

double yDevtoNPC(double y, pGEDevDesc dd)
{
    return (yDevtoNFC(y, dd) - gpptr(dd)->plt[2])/
	(gpptr(dd)->plt[3] - gpptr(dd)->plt[2]);
}

/* a special case (NPC = normalised plot region coordinates) */

double xNPCtoUsr(double x, pGEDevDesc dd)
{
    if (gpptr(dd)->xlog)
	return Rexp10(gpptr(dd)->logusr[0] +
		   x*(gpptr(dd)->logusr[1] - gpptr(dd)->logusr[0]));
    else
	return gpptr(dd)->usr[0] + x*(gpptr(dd)->usr[1] - gpptr(dd)->usr[0]);
}

double yNPCtoUsr(double y, pGEDevDesc dd)
{
    if (gpptr(dd)->ylog)
	return Rexp10(gpptr(dd)->logusr[2] +
		   y*(gpptr(dd)->logusr[3]-gpptr(dd)->logusr[2]));
    else
	return gpptr(dd)->usr[2] + y*(gpptr(dd)->usr[3] - gpptr(dd)->usr[2]);
}

double xDevtoUsr(double x, pGEDevDesc dd)
{
    double nfc = xDevtoNFC(x, dd);
    if (gpptr(dd)->xlog)
	return Rexp10((nfc - gpptr(dd)->win2fig.ax)/gpptr(dd)->win2fig.bx);
    else
	return (nfc - gpptr(dd)->win2fig.ax)/gpptr(dd)->win2fig.bx;
}

double yDevtoUsr(double y, pGEDevDesc dd)
{
  double nfc = yDevtoNFC(y, dd);
  if (gpptr(dd)->ylog)
    return Rexp10((nfc - gpptr(dd)->win2fig.ay)/gpptr(dd)->win2fig.by);
  else
    return (nfc - gpptr(dd)->win2fig.ay)/gpptr(dd)->win2fig.by;
}

static double xDevtoMAR1(double x, pGEDevDesc dd)
{
    return xDevtoUsr(x, dd);
}

static double yDevtoMAR1(double y, pGEDevDesc dd)
{
    return gpptr(dd)->oma[0] + gpptr(dd)->mar[0] - yDevtoLine(y, dd);
}

static double xDevtoyMAR2(double x, pGEDevDesc dd)
{
    return gpptr(dd)->oma[1] + gpptr(dd)->mar[1] - xDevtoLine(x, dd);
}

static double yDevtoxMAR2(double y, pGEDevDesc dd)
{
    return yDevtoUsr(y, dd);
}

static double xDevtoMAR3(double x, pGEDevDesc dd)
{
    return xDevtoUsr(x, dd);
}

static double yDevtoMAR3(double y, pGEDevDesc dd)
{
    double line = GConvertYUnits(1.0 - yDevtoNFC(y, dd), NFC, LINES, dd);
    return gpptr(dd)->mar[2] - line;
}

static double xDevtoyMAR4(double x, pGEDevDesc dd)
{
    double line = GConvertXUnits(1.0 - xDevtoNFC(x, dd), NFC, LINES, dd);
    return gpptr(dd)->mar[3] - line;
}

static double yDevtoxMAR4(double y, pGEDevDesc dd)
{
    return yDevtoUsr(y, dd);
}

/* the Convert function converts a LOCATION in the FROM coordinate */
/* system to a LOCATION in the TO coordinate system */

void GConvert(double *x, double *y, GUnit from, GUnit to, pGEDevDesc dd)
{
    double devx, devy;

    switch (from) {
    case DEVICE:
	devx = *x;
	devy = *y;
	break;
    case NDC:
	devx = xNDCtoDev(*x, dd);
	devy = yNDCtoDev(*y, dd);
	break;
    case INCHES:
	devx = xInchtoDev(*x, dd);
	devy = yInchtoDev(*y, dd);
	break;
    case OMA1:
	devx = xOMA1toDev(*x, dd);
	devy = yOMA1toDev(*y, dd);
	break;
    case OMA2:
	devx = yOMA2toxDev(*y, dd);
	devy = xOMA2toyDev(*x, dd);
	break;
    case OMA3:
	devx = xOMA3toDev(*x, dd);
	devy = yOMA3toDev(*y, dd);
	break;
    case OMA4:
	devx = yOMA4toxDev(*y, dd);
	devy = xOMA4toyDev(*x, dd);
	break;
    case NIC:
	devx = xNICtoDev(*x, dd);
	devy = yNICtoDev(*y, dd);
	break;
    case NFC:
	devx = xNFCtoDev(*x, dd);
	devy = yNFCtoDev(*y, dd);
	break;
    case MAR1:
	devx = xMAR1toDev(*x, dd);
	devy = yMAR1toDev(*y, dd);
	break;
    case MAR2:
	devx = yMAR2toxDev(*y, dd);
	devy = xMAR2toyDev(*x, dd);
	break;
    case MAR3:
	devx = xMAR3toDev(*x, dd);
	devy = yMAR3toDev(*y, dd);
	break;
    case MAR4:
	devx = yMAR4toxDev(*y, dd);
	devy = xMAR4toyDev(*x, dd);
	break;
    case NPC:
	devx = xNPCtoDev(*x, dd);
	devy = yNPCtoDev(*y, dd);
	break;
    case USER:
	devx = xUsrtoDev(*x, dd);
	devy = yUsrtoDev(*y, dd);
	break;
    default:
	devx = 0;	/* for -Wall */
	devy = 0;
	BadUnitsError("GConvert");
    }

    switch (to) {
    case DEVICE:
	*x = devx;
	*y = devy;
	break;
    case NDC:
	*x = xDevtoNDC(devx, dd);
	*y = yDevtoNDC(devy, dd);
	break;
    case INCHES:
	*x = xDevtoInch(devx, dd);
	*y = yDevtoInch(devy, dd);
	break;
    case LINES:
	*x = xDevtoLine(devx, dd);
	*y = yDevtoLine(devy, dd);
	break;
    case NIC:
	*x = xDevtoNIC(devx, dd);
	*y = yDevtoNIC(devy, dd);
	break;
    case OMA1:
	*x = xDevtoOMA1(devx, dd);
	*y = yDevtoOMA1(devy, dd);
	break;
    case OMA2:
	*x = yDevtoxOMA2(devy, dd);
	*y = xDevtoyOMA2(devx, dd);
	break;
    case OMA3:
	*x = xDevtoOMA3(devx, dd);
	*y = yDevtoOMA3(devy, dd);
	break;
    case OMA4:
	*x = yDevtoxOMA4(devy, dd);
	*y = xDevtoyOMA4(devx, dd);
	break;
    case NFC:
	*x = xDevtoNFC(devx, dd);
	*y = yDevtoNFC(devy, dd);
	break;
    case NPC:
	*x = xDevtoNPC(devx, dd);
	*y = yDevtoNPC(devy, dd);
	break;
    case USER:
	*x = xDevtoUsr(devx, dd);
	*y = yDevtoUsr(devy, dd);
	break;
    case MAR1:
	*x = xDevtoMAR1(devx, dd);
	*y = yDevtoMAR1(devy, dd);
	break;
    case MAR2:
	*x = yDevtoxMAR2(devy, dd);
	*y = xDevtoyMAR2(devx, dd);
	break;
    case MAR3:
	*x = xDevtoMAR3(devx, dd);
	*y = yDevtoMAR3(devy, dd);
	break;
    case MAR4:
	*x = yDevtoxMAR4(devy, dd);
	*y = xDevtoyMAR4(devx, dd);
	break;
    default:
	BadUnitsError("GConvert");
    }
}

double GConvertX(double x, GUnit from, GUnit to, pGEDevDesc dd)
{
    double devx;
    switch (from) {
    case DEVICE:devx = x;	break;
    case NDC:	devx = xNDCtoDev(x, dd);	break;
    case INCHES:devx = xInchtoDev(x, dd);	break;
    case LINES: devx = xLinetoDev(x, dd);       break;
    case OMA1:	devx = xOMA1toDev(x, dd);	break;
    /*case OMA2:	x <--> y */
    case OMA3:	devx = xOMA3toDev(x, dd);	break;
    /*case OMA4:	x <--> y */
    case NIC:	devx = xNICtoDev(x, dd);	break;
    case NFC:	devx = xNFCtoDev(x, dd);	break;
    case MAR1:	devx = xMAR1toDev(x, dd);	break;
    /*case MAR2:	x <--> y */
    case MAR3:	devx = xMAR3toDev(x, dd);	break;
    /*case MAR4:	x <--> y */
    case NPC:	devx = xNPCtoDev(x, dd);	break;
    case USER:	devx = xUsrtoDev(x, dd);	break;
    default:	devx = 0;/* for -Wall */ BadUnitsError("GConvertX");
    }

    switch (to) {
    case DEVICE:x = devx;	break;
    case NDC:	x = xDevtoNDC(devx, dd);	break;
    case INCHES:x = xDevtoInch(devx, dd);	break;
    case LINES:	x = xDevtoLine(devx, dd);	break;
    case NIC:	x = xDevtoNIC(devx, dd);	break;
    case OMA1:	x = xDevtoOMA1(devx, dd);	break;
    /*case OMA2:	x <--> y */
    case OMA3:	x = xDevtoOMA3(devx, dd);	break;
    /*case OMA4:	x <--> y */
    case NFC:	x = xDevtoNFC(devx, dd);	break;
    case USER:	x = xDevtoUsr(devx, dd);	break;
    case MAR1:	x = xDevtoMAR1(devx, dd);	break;
    /*case MAR2:	x <--> y */
    case MAR3:	x = xDevtoMAR3(devx, dd);	break;
    /*case MAR4:	x <--> y */
    case NPC:	x = xDevtoNPC(devx, dd);	break;
    default:	BadUnitsError("GConvertX");
    }
    return x;
}

double GConvertY(double y, GUnit from, GUnit to, pGEDevDesc dd)
{
    double devy;
    switch (from) {
    case DEVICE:devy = y;	break;
    case NDC:	devy = yNDCtoDev(y, dd);	break;
    case INCHES:devy = yInchtoDev(y, dd);	break;
    case LINES: devy = yLinetoDev(y, dd);       break;
    case OMA1:	devy = yOMA1toDev(y, dd);	break;
    /*case OMA2:	x <--> y */
    case OMA3:	devy = yOMA3toDev(y, dd);	break;
    /*case OMA4:	x <--> y */
    case NIC:	devy = yNICtoDev(y, dd);	break;
    case NFC:	devy = yNFCtoDev(y, dd);	break;
    case MAR1:	devy = yMAR1toDev(y, dd);	break;
    /*case MAR2:	x <--> y */
    case MAR3:	devy = yMAR3toDev(y, dd);	break;
    /*case MAR4:	x <--> y */
    case NPC:	devy = yNPCtoDev(y, dd);	break;
    case USER:	devy = yUsrtoDev(y, dd);	break;
    default:	devy = 0;/* for -Wall */ BadUnitsError("GConvertY");
    }

    switch (to) {
    case DEVICE:y = devy;	break;
    case NDC:	y = yDevtoNDC(devy, dd);	break;
    case INCHES:y = yDevtoInch(devy, dd);	break;
    case LINES:	y = yDevtoLine(devy, dd);	break;
    case NIC:	y = yDevtoNIC(devy, dd);	break;
    case OMA1:	y = yDevtoOMA1(devy, dd);	break;
    /*case OMA2:	x <--> y */
    case OMA3:	y = yDevtoOMA3(devy, dd);	break;
    /*case OMA4:	x <--> y */
    case NFC:	y = yDevtoNFC(devy, dd);	break;
    case USER:	y = yDevtoUsr(devy, dd);	break;
    case MAR1:	y = yDevtoMAR1(devy, dd);	break;
    /*case MAR2:	x <--> y */
    case MAR3:	y = yDevtoMAR3(devy, dd);	break;
    /*case MAR4:	x <--> y */
    case NPC:   y = yDevtoNPC(devy, dd);	break;
    default:	BadUnitsError("GConvertY");
    }
    return y;
}

/* Code for layouts */

static double sum(double values[], int n, int cmValues[], int cmSum)
{
    int i;
    double s = 0;
    for (i = 0; i < n; i++)
	if ((cmSum && cmValues[i]) || (!cmSum && !cmValues[i]))
	    s = s + values[i];
    return s;
}

static double sumWidths(pGEDevDesc dd)
{
    return sum(gpptr(dd)->widths, gpptr(dd)->numcols, gpptr(dd)->cmWidths, 0);
}

static double sumCmWidths(pGEDevDesc dd)
{
    return sum(gpptr(dd)->widths, gpptr(dd)->numcols,  gpptr(dd)->cmWidths, 1);
}

static double sumHeights(pGEDevDesc dd)
{
    return sum(gpptr(dd)->heights, gpptr(dd)->numrows, gpptr(dd)->cmHeights, 0);
}

static double sumCmHeights(pGEDevDesc dd)
{
    return sum(gpptr(dd)->heights, gpptr(dd)->numrows, gpptr(dd)->cmHeights, 1);
}

static int tallLayout(double cmWidth, double cmHeight, pGEDevDesc dd)
{
    return (cmHeight/sumHeights(dd)) > (cmWidth/sumWidths(dd));
}

static void figureExtent(int *minCol, int *maxCol, int *minRow, int *maxRow,
			 int figureNum, pGEDevDesc dd)
{
    int minc = -1;
    int maxc = -1;
    int minr = -1;
    int maxr = -1;
    int i, j;
    int nr = gpptr(dd)->numrows;
    for (i = 0; i < nr; i++)
	for (j = 0; j < gpptr(dd)->numcols; j++)
	    if (gpptr(dd)->order[i + j*nr] == figureNum) {
		if ((minc == -1) || (j < minc))
		    minc = j;
		if ((maxc == -1) || (j > maxc))
		    maxc = j;
		if ((minr == -1) || (i < minr))
		    minr = i;
		if ((maxr == -1) || (i > maxr))
		    maxr = i;
	    }
    *minCol = minc;
    *maxCol = maxc;
    *minRow = minr;
    *maxRow = maxr;
}

static double sumRegions(double regions[], int from, int to)
{
    int i;
    double s = 0;
    for (i = from; i < to + 1; i++)
	s = s + regions[i];
    return s;
}

static void largestRegion(double *width, double *height,
			  double layoutAspectRatio, double innerAspectRatio)
{
    if (layoutAspectRatio < innerAspectRatio) {
	*width = 1.0;
	*height = layoutAspectRatio/innerAspectRatio;
    }
    else {
	*width = innerAspectRatio/layoutAspectRatio;
	*height = 1.0;
    }
}

static void layoutRegion(double *width, double *height,
			 double widths[], double heights[],
			 double cmWidth, double cmHeight, pGEDevDesc dd)
{
  largestRegion(width, height,
		sum(heights, gpptr(dd)->numrows, gpptr(dd)->cmHeights, 0)/
		sum(widths, gpptr(dd)->numcols, gpptr(dd)->cmWidths, 0),
		cmHeight/cmWidth);
}


	/* allocate one dimension (width or height) for either */
	/* relative or cm units */

static void allocDimension(double dimensions[], double sumDimensions, int n,
			   int cmDimensions[], int cmDimension)
{
    int i;
    for (i = 0; i < n; i++)
	if ((cmDimension && cmDimensions[i]) ||
	    (!cmDimension && !cmDimensions[i]))
	    dimensions[i] = dimensions[i]/sumDimensions;
}

static void allCmRegions(double widths[], double heights[],
			 double cmWidth, double cmHeight, pGEDevDesc dd)
{
    allocDimension(widths, cmWidth, gpptr(dd)->numcols, gpptr(dd)->cmWidths, 1);
    allocDimension(heights, cmHeight, gpptr(dd)->numrows, gpptr(dd)->cmHeights, 1);
}

static void modifyDimension(double dimension[], double multiplier, double n,
			    int cmDimensions[])
{
    int i;
    for (i = 0; i < n; i++)
	if (!cmDimensions[i])
	    dimension[i] = dimension[i] * multiplier;
}

static void modifyRegions(double widths[], double heights[],
		   double colMultiplier, double rowMultiplier,
		   pGEDevDesc dd)
{
    modifyDimension(widths, colMultiplier, gpptr(dd)->numcols, gpptr(dd)->cmWidths);
    modifyDimension(heights, rowMultiplier, gpptr(dd)->numrows, gpptr(dd)->cmHeights);
}

static void regionsWithoutRespect(double widths[], double heights[], pGEDevDesc dd)
{
    allocDimension(widths,
		   sum(widths, gpptr(dd)->numcols, gpptr(dd)->cmWidths, 0),
		   gpptr(dd)->numcols, gpptr(dd)->cmWidths, 0);
    allocDimension(heights,
		   sum(heights, gpptr(dd)->numrows, gpptr(dd)->cmHeights, 0),
		   gpptr(dd)->numrows, gpptr(dd)->cmHeights, 0);
}

static void regionsWithRespect(double widths[], double heights[],
			       double cmWidth, double cmHeight, pGEDevDesc dd)
{
    double cm, rm;
    layoutRegion(&cm, &rm, widths, heights, cmWidth, cmHeight, dd);
    regionsWithoutRespect(widths, heights, dd);
    modifyRegions(widths, heights, cm, rm, dd);
}

static void widthsRespectingHeights(double widths[],
				    double cmWidth, double cmHeight,
				    pGEDevDesc dd)
{
    int i, j;
    int respectedCols[MAX_LAYOUT_COLS];
    double widthLeft;
    double disrespectedWidth = 0;
    int nr = gpptr(dd)->numrows;
    for (j = 0; j < gpptr(dd)->numcols; j++) {
	respectedCols[j] = 0;
	widths[j] = gpptr(dd)->widths[j];
    }
    for (i = 0; i < nr; i++)
	for (j = 0; j < gpptr(dd)->numcols; j++)
	    if (gpptr(dd)->respect[i + j * nr] &&
		!gpptr(dd)->cmWidths[j]) respectedCols[j] = 1;
    for (j = 0; j < gpptr(dd)->numcols; j++)
	if (!respectedCols[j])
	    disrespectedWidth += gpptr(dd)->widths[j];
    widthLeft = sumHeights(dd) * cmWidth/cmHeight -
	sumWidths(dd) + disrespectedWidth;
    for (j = 0; j < gpptr(dd)->numcols; j++)
	if (!respectedCols[j])
	    widths[j] = widthLeft * widths[j]/disrespectedWidth;
}

static void regionsRespectingHeight(double widths[], double heights[],
			     double cmWidth, double cmHeight,
			     pGEDevDesc dd)
{
    widthsRespectingHeights(widths, cmWidth, cmHeight, dd);
    regionsWithRespect(widths, heights, cmWidth, cmHeight, dd);
}

static void heightsRespectingWidths(double heights[],
			     double cmWidth, double cmHeight,
			     pGEDevDesc dd)
{
    int i, j;
    int respectedRows[MAX_LAYOUT_ROWS];
    double heightLeft;
    double disrespectedHeight = 0;
    int nr = gpptr(dd)->numrows;
    for (i = 0; i < nr; i++) {
	respectedRows[i] = 0;
	heights[i] = gpptr(dd)->heights[i];
    }
    for (i = 0; i < nr; i++)
	for (j = 0; j < gpptr(dd)->numcols; j++)
	    if (gpptr(dd)->respect[i + j*nr] &&
		!gpptr(dd)->cmHeights[i]) respectedRows[i] = 1;
    for (i = 0; i < gpptr(dd)->numrows; i++)
	if (!respectedRows[i])
	    disrespectedHeight += gpptr(dd)->heights[i];
    heightLeft = sumWidths(dd) * cmHeight/cmWidth -
	sumHeights(dd) + disrespectedHeight;
    for (i = 0; i < gpptr(dd)->numrows; i++)
	if (!respectedRows[i])
	    heights[i] = heightLeft * heights[i]/disrespectedHeight;
}

static void regionsRespectingWidth(double widths[], double heights[],
			    double cmWidth, double cmHeight,
			    pGEDevDesc dd)
{
    heightsRespectingWidths(heights, cmWidth, cmHeight, dd);
    regionsWithRespect(widths, heights, cmWidth, cmHeight, dd);
}

static void noCmRegions(double widths[], double heights[],
		 double cmWidth, double cmHeight, pGEDevDesc dd)
{
    switch (gpptr(dd)->rspct) {
    case 0:
	regionsWithoutRespect(widths, heights, dd);
	break;
    case 1:
	regionsWithRespect(widths, heights, cmWidth, cmHeight, dd);
	break;
    case 2:
	if (tallLayout(cmWidth, cmHeight, dd))
	    regionsRespectingWidth(widths, heights, cmWidth, cmHeight, dd);
	else
	    regionsRespectingHeight(widths, heights, cmWidth, cmHeight, dd);
    }
}

static void notAllCmRegions(double widths[], double heights[],
			    double cmWidth, double cmHeight, pGEDevDesc dd)
{
    double newCmWidth, newCmHeight;
    newCmWidth = cmWidth - sumCmWidths(dd);
    newCmHeight = cmHeight - sumCmHeights(dd);
    noCmRegions(widths, heights, newCmWidth, newCmHeight, dd);
    allocDimension(widths, cmWidth, gpptr(dd)->numcols, gpptr(dd)->cmWidths, 1);
    allocDimension(heights, cmHeight, gpptr(dd)->numrows, gpptr(dd)->cmHeights, 1);
    modifyDimension(widths, newCmWidth/cmWidth, gpptr(dd)->numcols,
		    gpptr(dd)->cmWidths);
    modifyDimension(heights, newCmHeight/cmHeight, gpptr(dd)->numrows,
		    gpptr(dd)->cmHeights);
}

static void widthCmRegions(double widths[], double heights[],
			   double cmWidth, double cmHeight, pGEDevDesc dd)
{
    allocDimension(widths, cmWidth, gpptr(dd)->numcols, gpptr(dd)->cmWidths, 1);
    allocDimension(heights, sumHeights(dd), gpptr(dd)->numrows,
		   gpptr(dd)->cmHeights, 0);
    modifyDimension(heights, (cmHeight - sumCmHeights(dd))/cmHeight,
		    gpptr(dd)->numrows, gpptr(dd)->cmHeights);
    allocDimension(heights, cmHeight, gpptr(dd)->numrows,
		   gpptr(dd)->cmHeights, 1);
}

static void heightCmRegions(double widths[], double heights[],
			    double cmWidth, double cmHeight, pGEDevDesc dd)
{
    allocDimension(heights, cmHeight, gpptr(dd)->numrows, gpptr(dd)->cmHeights, 1);
    allocDimension(widths, sumWidths(dd), gpptr(dd)->numcols,
		   gpptr(dd)->cmWidths, 0);
    modifyDimension(widths, (cmWidth - sumCmWidths(dd))/cmWidth,
		    gpptr(dd)->numcols, gpptr(dd)->cmWidths);
    allocDimension(widths, cmWidth, gpptr(dd)->numcols,
		   gpptr(dd)->cmWidths, 1);
}

static Rboolean allCmWidths(pGEDevDesc dd)
{
    int j;
    for (j = 0; j < gpptr(dd)->numcols; j++)
	if (!gpptr(dd)->cmWidths[j])
	    return FALSE;
    return TRUE;
}

static Rboolean allCmHeights(pGEDevDesc dd)
{
    int i;
    for (i = 0; i < gpptr(dd)->numrows; i++)
	if (!gpptr(dd)->cmHeights[i])
	    return FALSE;
    return TRUE;
}

static Rboolean noCmWidths(pGEDevDesc dd)
{
    int j;
    for (j = 0; j < gpptr(dd)->numcols; j++)
	if (gpptr(dd)->cmWidths[j])
	    return FALSE;
    return TRUE;
}

static Rboolean noCmHeights(pGEDevDesc dd)
{
    int i;
    for (i = 0; i < gpptr(dd)->numrows; i++)
	if (gpptr(dd)->cmHeights[i])
	    return FALSE;
    return TRUE;
}

static void someCmRegions(double widths[], double heights[],
		   double cmWidth, double cmHeight, pGEDevDesc dd)
{
    if (allCmWidths(dd))
	widthCmRegions(widths, heights, cmWidth, cmHeight, dd);
    else if (allCmHeights(dd))
	heightCmRegions(widths, heights, cmWidth, cmHeight, dd);
    else
	notAllCmRegions(widths, heights, cmWidth, cmHeight, dd);
}

static Rboolean allCm(pGEDevDesc dd)
{
    return allCmWidths(dd) && allCmHeights(dd);
}

static Rboolean noCm(pGEDevDesc dd)
{
    return noCmWidths(dd) && noCmHeights(dd);
}

static void layoutRegions(double widths[], double heights[],
		   double cmWidth, double cmHeight, pGEDevDesc dd)
{
    int i, j;
    for (j = 0; j < gpptr(dd)->numcols; j++)
	widths[j] = gpptr(dd)->widths[j];
    for (i = 0; i < gpptr(dd)->numrows; i++)
	heights[i] = gpptr(dd)->heights[i];

    if (allCm(dd))
	allCmRegions(widths, heights, cmWidth, cmHeight, dd);
    else if (noCm(dd))
	noCmRegions(widths, heights, cmWidth, cmHeight, dd);
    else
	someCmRegions(widths, heights, cmWidth, cmHeight, dd);
}

static void subRegion(double *left, double *right, double *bottom, double *top,
		      int mincol, int maxcol,
		      int minrow, int maxrow,
		      double widths[], double heights[], pGEDevDesc dd)
{
    double totalWidth = sumRegions(widths, 0, gpptr(dd)->numcols-1);
    double totalHeight = sumRegions(heights, 0, gpptr(dd)->numrows-1);
    *left = (0.5 - totalWidth/2) + sumRegions(widths, 0, mincol-1);
    *right = (0.5 - totalWidth/2) + sumRegions(widths, 0, maxcol);
    *bottom = (0.5 - totalHeight/2) + totalHeight
	- sumRegions(heights, 0, maxrow);
    *top = (0.5 - totalHeight/2) + totalHeight
	- sumRegions(heights, 0, minrow-1);
}

/* a fudge for backwards compatibility (of sorts) with par(mfg) */
/* return the top-left-most row/col that the current figure */
/* occupies in the current layout */

void currentFigureLocation(int *row, int *col, pGEDevDesc dd)
{
    int maxcol, maxrow;
    if (gpptr(dd)->layout)
	figureExtent(col, &maxcol, row, &maxrow, gpptr(dd)->currentFigure, dd);
    else if (gpptr(dd)->mfind) { /* mfcol */
	*row = (gpptr(dd)->currentFigure - 1)%gpptr(dd)->numrows;
	*col = (gpptr(dd)->currentFigure - 1)/gpptr(dd)->numrows;
    }
    else { /* mfrow */
	*row = (gpptr(dd)->currentFigure - 1)/gpptr(dd)->numcols;
	*col = (gpptr(dd)->currentFigure - 1)%gpptr(dd)->numcols;
    }
}

/*  mapNDC2Dev -- transformation from NDC to Dev  */
/*  Use this coordinate system for outer margin coordinates  */
/*  This must be called if the device is resized */

static void mapNDC2Dev(pGEDevDesc dd)
{
    /* For new devices, have to check the device's idea of its size
     * in case there has been a resize.
     */
    double asp = dd->dev->ipr[1] / dd->dev->ipr[0];

    gpptr(dd)->ndc2dev.bx = dpptr(dd)->ndc2dev.bx =
	dd->dev->right - dd->dev->left;
    gpptr(dd)->ndc2dev.ax = dpptr(dd)->ndc2dev.ax = dd->dev->left;
    gpptr(dd)->ndc2dev.by = dpptr(dd)->ndc2dev.by =
	dd->dev->top - dd->dev->bottom;
    gpptr(dd)->ndc2dev.ay = dpptr(dd)->ndc2dev.ay = dd->dev->bottom;
    /* Units Conversion */

    gpptr(dd)->xNDCPerInch = dpptr(dd)->xNDCPerInch =
	1.0/fabs(gpptr(dd)->ndc2dev.bx * dd->dev->ipr[0]);
    gpptr(dd)->yNDCPerInch = dpptr(dd)->yNDCPerInch =
	1.0/fabs(gpptr(dd)->ndc2dev.by * dd->dev->ipr[1]);
    gpptr(dd)->xNDCPerChar = dpptr(dd)->xNDCPerChar =
	fabs(gpptr(dd)->cexbase * gpptr(dd)->scale *
	     dd->dev->cra[1] * asp / gpptr(dd)->ndc2dev.bx);
    gpptr(dd)->yNDCPerChar = dpptr(dd)->yNDCPerChar =
	fabs(gpptr(dd)->cexbase * gpptr(dd)->scale *
	     dd->dev->cra[1] / gpptr(dd)->ndc2dev.by);
    gpptr(dd)->xNDCPerLine = dpptr(dd)->xNDCPerLine =
	fabs(gpptr(dd)->mex * gpptr(dd)->cexbase * gpptr(dd)->scale *
	     dd->dev->cra[1] * asp / gpptr(dd)->ndc2dev.bx);
    gpptr(dd)->yNDCPerLine = dpptr(dd)->yNDCPerLine =
	fabs(gpptr(dd)->mex * gpptr(dd)->cexbase * gpptr(dd)->scale *
	     dd->dev->cra[1] / gpptr(dd)->ndc2dev.by);
}

static void updateOuterMargins(pGEDevDesc dd)
{
    switch (gpptr(dd)->oUnits) {
    case LINES:
	gpptr(dd)->omi[0] = dpptr(dd)->omi[0] =
	    GConvertYUnits(gpptr(dd)->oma[0], LINES, INCHES, dd);
	gpptr(dd)->omi[1] = dpptr(dd)->omi[1] =
	    GConvertXUnits(gpptr(dd)->oma[1], LINES, INCHES, dd);
	gpptr(dd)->omi[2] = dpptr(dd)->omi[2] =
	    GConvertYUnits(gpptr(dd)->oma[2], LINES, INCHES, dd);
	gpptr(dd)->omi[3] = dpptr(dd)->omi[3] =
	    GConvertXUnits(gpptr(dd)->oma[3], LINES, INCHES, dd);
	gpptr(dd)->omd[0] = dpptr(dd)->omd[0] =
	    GConvertXUnits(gpptr(dd)->oma[1], LINES, NDC, dd);
	gpptr(dd)->omd[1] = dpptr(dd)->omd[1] =
	    1 - GConvertXUnits(gpptr(dd)->oma[3], LINES, NDC, dd);
	gpptr(dd)->omd[2] = dpptr(dd)->omd[2] =
	    GConvertYUnits(gpptr(dd)->oma[0], LINES, NDC, dd);
	gpptr(dd)->omd[3] = dpptr(dd)->omd[3] =
	    1 - GConvertYUnits(gpptr(dd)->oma[2], LINES, NDC, dd);
	break;
    case INCHES:
	gpptr(dd)->oma[0] = dpptr(dd)->oma[0] =
	    GConvertYUnits(gpptr(dd)->omi[0], INCHES, LINES, dd);
	gpptr(dd)->oma[1] = dpptr(dd)->oma[1] =
	    GConvertXUnits(gpptr(dd)->omi[1], INCHES, LINES, dd);
	gpptr(dd)->oma[2] = dpptr(dd)->oma[2] =
	    GConvertYUnits(gpptr(dd)->omi[2], INCHES, LINES, dd);
	gpptr(dd)->oma[3] = dpptr(dd)->oma[3] =
	    GConvertXUnits(gpptr(dd)->omi[3], INCHES, LINES, dd);
	gpptr(dd)->omd[0] = dpptr(dd)->omd[0] =
	    GConvertXUnits(gpptr(dd)->omi[1], INCHES, NDC, dd);
	gpptr(dd)->omd[1] = dpptr(dd)->omd[1] =
	    1 - GConvertXUnits(gpptr(dd)->omi[3], INCHES, NDC, dd);
	gpptr(dd)->omd[2] = dpptr(dd)->omd[2] =
	    GConvertYUnits(gpptr(dd)->omi[0], INCHES, NDC, dd);
	gpptr(dd)->omd[3] = dpptr(dd)->omd[3] =
	    1 - GConvertYUnits(gpptr(dd)->omi[2], INCHES, NDC, dd);
	break;
    case NDC:
	gpptr(dd)->oma[0] = dpptr(dd)->oma[0] =
	    GConvertYUnits(gpptr(dd)->omd[2], NDC, LINES, dd);
	gpptr(dd)->oma[1] = dpptr(dd)->oma[1] =
	    GConvertXUnits(gpptr(dd)->omd[0], NDC, LINES, dd);
	gpptr(dd)->oma[2] = dpptr(dd)->oma[2] =
	    GConvertYUnits(1 - gpptr(dd)->omd[3], NDC, LINES, dd);
	gpptr(dd)->oma[3] = dpptr(dd)->oma[3] =
	    GConvertXUnits(1 - gpptr(dd)->omd[1], NDC, LINES, dd);
	gpptr(dd)->omi[0] = dpptr(dd)->omi[0] =
	    GConvertYUnits(gpptr(dd)->omd[2], NDC, INCHES, dd);
	gpptr(dd)->omi[1] = dpptr(dd)->omi[1] =
	    GConvertXUnits(gpptr(dd)->omd[0], NDC, INCHES, dd);
	gpptr(dd)->omi[2] = dpptr(dd)->omi[2] =
	    GConvertYUnits(1 - gpptr(dd)->omd[3], NDC, INCHES, dd);
	gpptr(dd)->omi[3] = dpptr(dd)->omi[3] =
	    GConvertXUnits(1 - gpptr(dd)->omd[1], NDC, INCHES, dd);
	break;
    default: break; /*nothing (-Wall) */
    }
}

/*  mapInner2Dev -- transformation from NIC to Dev  */
/*  Use this coordinate system for setting up multiple figures	*/
/*  This is also used when specifying the figure region directly  */
/*  Note that this is incompatible with S which uses then entire  */
/*  device surface for such a plot  */
/*  This must be called per DevNewPlot, if the NDCtoDev transformation */
/*  changes, and if oma changes */

static void mapInner2Dev(pGEDevDesc dd)
{
    double x0, x1, y0, y1;
    x0 = xLinetoDev(gpptr(dd)->oma[1], dd);
    y0 = yLinetoDev(gpptr(dd)->oma[0], dd);
    x1 = GConvertXUnits(gpptr(dd)->oma[3], LINES, NDC, dd);
    x1 = xNDCtoDev(1.0 - x1, dd);
    y1 = GConvertYUnits(gpptr(dd)->oma[2], LINES, NDC, dd);
    y1 = yNDCtoDev(1.0 - y1, dd);
    gpptr(dd)->inner2dev.bx = dpptr(dd)->inner2dev.bx = x1 - x0;
    gpptr(dd)->inner2dev.ax = dpptr(dd)->inner2dev.ax = x0;
    gpptr(dd)->inner2dev.by = dpptr(dd)->inner2dev.by = y1 - y0;
    gpptr(dd)->inner2dev.ay = dpptr(dd)->inner2dev.ay = y0;
}

/* mapFigureRegion -- calculate figure region in NIC  */

static void mapFigureRegion(pGEDevDesc dd)
{
    int mincol, maxcol, minrow, maxrow;
    double x0, x1, y0, y1;
    double widths[MAX_LAYOUT_COLS], heights[MAX_LAYOUT_ROWS];
    if (gpptr(dd)->layout) {
	layoutRegions(widths, heights,
		      GConvertXUnits(1.0, NIC, INCHES, dd)*2.54,
		      GConvertYUnits(1.0, NIC, INCHES, dd)*2.54, dd);
	figureExtent(&mincol, &maxcol, &minrow, &maxrow,
		     gpptr(dd)->currentFigure, dd);
	subRegion(&x0, &x1, &y0, &y1,
		  mincol, maxcol, minrow, maxrow,
		  widths, heights, dd);
    }
    else {
	int row, col;
	if (gpptr(dd)->mfind) {
	    col = (gpptr(dd)->currentFigure-1) / gpptr(dd)->numrows + 1;
	    row = gpptr(dd)->currentFigure - (col-1)*gpptr(dd)->numrows;
	}
	else {
	    row = (gpptr(dd)->currentFigure-1) / gpptr(dd)->numcols + 1;
	    col = gpptr(dd)->currentFigure - (row-1)*gpptr(dd)->numcols;
	}
	x0 = (double) (col-1) / gpptr(dd)->numcols;
	x1 = (double) col / gpptr(dd)->numcols;
	y0 = (double) (gpptr(dd)->numrows - row) / gpptr(dd)->numrows;
	y1 = (double) (gpptr(dd)->numrows - row + 1) / gpptr(dd)->numrows;
    }
    gpptr(dd)->fig[0] = dpptr(dd)->fig[0] = x0;
    gpptr(dd)->fig[1] = dpptr(dd)->fig[1] = x1;
    gpptr(dd)->fig[2] = dpptr(dd)->fig[2] = y0;
    gpptr(dd)->fig[3] = dpptr(dd)->fig[3] = y1;
    gpptr(dd)->fUnits = dpptr(dd)->fUnits = NIC;
}

static void updateFigureRegion(pGEDevDesc dd)
{
    double nicWidth, nicHeight;
    switch (gpptr(dd)->fUnits) {
    case NIC:
	gpptr(dd)->fin[0] = dpptr(dd)->fin[0] =
	    GConvertXUnits(gpptr(dd)->fig[1] - gpptr(dd)->fig[0], NIC, INCHES, dd);
	gpptr(dd)->fin[1] = dpptr(dd)->fin[1] =
	    GConvertYUnits(gpptr(dd)->fig[3] - gpptr(dd)->fig[2], NIC, INCHES, dd);
	break;
    case INCHES:
	nicWidth = GConvertXUnits(gpptr(dd)->fin[0], INCHES, NIC, dd);
	nicHeight = GConvertYUnits(gpptr(dd)->fin[1], INCHES, NIC, dd);
	gpptr(dd)->fig[0] = dpptr(dd)->fig[0] = 0.5 - nicWidth/2;
	gpptr(dd)->fig[1] = dpptr(dd)->fig[1] = gpptr(dd)->fig[0] + nicWidth;
	gpptr(dd)->fig[2] = dpptr(dd)->fig[2] = 0.5 - nicHeight/2;
	gpptr(dd)->fig[3] = dpptr(dd)->fig[3] = gpptr(dd)->fig[2] + nicHeight;
	break;
    default: /*nothing*/ break;
    }
}

/*  mapFig2Dev -- Transformation from NFC to Dev  */
/* This must be called per plot.new and if the NICtoDev transformation */
/* changes */

static void mapFig2Dev(pGEDevDesc dd)
{
    double x0, x1, y0, y1;
    y0 = yNICtoDev(gpptr(dd)->fig[2], dd);
    y1 = yNICtoDev(gpptr(dd)->fig[3], dd);
    x0 = xNICtoDev(gpptr(dd)->fig[0], dd);
    x1 = xNICtoDev(gpptr(dd)->fig[1], dd);
    gpptr(dd)->fig2dev.bx = dpptr(dd)->fig2dev.bx = x1 - x0;
    gpptr(dd)->fig2dev.ax = dpptr(dd)->fig2dev.ax = x0;
    gpptr(dd)->fig2dev.by = dpptr(dd)->fig2dev.by = y1 - y0;
    gpptr(dd)->fig2dev.ay = dpptr(dd)->fig2dev.ay = y0;
}

static void updateFigureMargins(pGEDevDesc dd)
{
    switch (gpptr(dd)->mUnits) {
    case LINES:
	gpptr(dd)->mai[0] = dpptr(dd)->mai[0] =
	    GConvertYUnits(gpptr(dd)->mar[0], LINES, INCHES, dd);
	gpptr(dd)->mai[1] = dpptr(dd)->mai[1] =
	    GConvertXUnits(gpptr(dd)->mar[1], LINES, INCHES, dd);
	gpptr(dd)->mai[2] = dpptr(dd)->mai[2] =
	    GConvertYUnits(gpptr(dd)->mar[2], LINES, INCHES, dd);
	gpptr(dd)->mai[3] = dpptr(dd)->mai[3] =
	    GConvertXUnits(gpptr(dd)->mar[3], LINES, INCHES, dd);
	break;
    case INCHES:
	gpptr(dd)->mar[0] = dpptr(dd)->mar[0] =
	    GConvertYUnits(gpptr(dd)->mai[0], INCHES, LINES, dd);
	gpptr(dd)->mar[1] = dpptr(dd)->mar[1] =
	    GConvertXUnits(gpptr(dd)->mai[1], INCHES, LINES, dd);
	gpptr(dd)->mar[2] = dpptr(dd)->mar[2] =
	    GConvertYUnits(gpptr(dd)->mai[2], INCHES, LINES, dd);
	gpptr(dd)->mar[3] = dpptr(dd)->mar[3] =
	    GConvertXUnits(gpptr(dd)->mai[3], INCHES, LINES, dd);
	break;
    default: /*nothing*/ break;
    }
}

/* mapPlotRegion -- plot region in NFC */

static void mapPlotRegion(pGEDevDesc dd)
{
    double x0, x1, y0, y1;
    x0 = GConvertXUnits(gpptr(dd)->mar[1], LINES, NFC, dd);
    y0 = GConvertYUnits(gpptr(dd)->mar[0], LINES, NFC, dd);
    x1 = 1.0 - GConvertXUnits(gpptr(dd)->mar[3], LINES, NFC, dd);
    y1 = 1.0 - GConvertYUnits(gpptr(dd)->mar[2], LINES, NFC, dd);
    if(gpptr(dd)->pty == 's') {
	/* maximal plot size in inches */
	double center, width, height;
	double inchWidth = GConvertXUnits(x1 - x0, NFC, INCHES, dd);
	double inchHeight = GConvertYUnits(y1 - y0, NFC, INCHES, dd);
	/* shrink the longer side */
	if (inchWidth > inchHeight) {
	    width = 0.5*GConvertXUnits(inchHeight, INCHES, NFC, dd);
	    center = 0.5*(x1 + x0);
	    x0 = center-width;
	    x1 = center+width;
	}
	else {
	    height = 0.5*GConvertYUnits(inchWidth, INCHES, NFC, dd);
	    center = 0.5*(y1 + y0);
	    y0 = center-height;
	    y1 = center+height;
	}
    }
    gpptr(dd)->plt[0] = dpptr(dd)->plt[0] = x0;
    gpptr(dd)->plt[1] = dpptr(dd)->plt[1] = x1;
    gpptr(dd)->plt[2] = dpptr(dd)->plt[2] = y0;
    gpptr(dd)->plt[3] = dpptr(dd)->plt[3] = y1;
    gpptr(dd)->pUnits = dpptr(dd)->pUnits = NFC;
}

static void updatePlotRegion(pGEDevDesc dd)
{
    double nfcWidth, nfcHeight;
    switch (gpptr(dd)->pUnits) {
    case NFC:
	gpptr(dd)->pin[0] = dpptr(dd)->pin[0] =
	    GConvertXUnits(gpptr(dd)->plt[1] - gpptr(dd)->plt[0], NFC, INCHES, dd);
	gpptr(dd)->pin[1] = dpptr(dd)->pin[1] =
	    GConvertYUnits(gpptr(dd)->plt[3] - gpptr(dd)->plt[2], NFC, INCHES, dd);
	break;
    case INCHES:
	nfcWidth = GConvertXUnits(gpptr(dd)->pin[0], INCHES, NFC, dd);
	nfcHeight = GConvertYUnits(gpptr(dd)->pin[1], INCHES, NFC, dd);
	gpptr(dd)->plt[0] = dpptr(dd)->plt[0] = 0.5 - nfcWidth/2;
	gpptr(dd)->plt[1] = dpptr(dd)->plt[1] = gpptr(dd)->plt[0] + nfcWidth;
	gpptr(dd)->plt[2] = dpptr(dd)->plt[2] = 0.5 - nfcHeight/2;
	gpptr(dd)->plt[3] = dpptr(dd)->plt[3] = gpptr(dd)->plt[2] + nfcHeight;
	break;
    default: /*nothing*/ break;
    }
}

/*  GMapWin2Fig -- transformation from Usr to NFC */

void GMapWin2Fig(pGEDevDesc dd)
{
    if (gpptr(dd)->xlog) {
	gpptr(dd)->win2fig.bx = dpptr(dd)->win2fig.bx =
	    (gpptr(dd)->plt[1] - gpptr(dd)->plt[0])/
	    (gpptr(dd)->logusr[1] - gpptr(dd)->logusr[0]);
	gpptr(dd)->win2fig.ax = dpptr(dd)->win2fig.ax =
	    gpptr(dd)->plt[0] - gpptr(dd)->win2fig.bx * gpptr(dd)->logusr[0];
    }
    else {
	gpptr(dd)->win2fig.bx = dpptr(dd)->win2fig.bx =
	    (gpptr(dd)->plt[1] - gpptr(dd)->plt[0])/
	    (gpptr(dd)->usr[1] - gpptr(dd)->usr[0]);
	gpptr(dd)->win2fig.ax = dpptr(dd)->win2fig.ax =
	    gpptr(dd)->plt[0] - gpptr(dd)->win2fig.bx * gpptr(dd)->usr[0];
    }
    if (gpptr(dd)->ylog) {
	gpptr(dd)->win2fig.by = dpptr(dd)->win2fig.by =
	    (gpptr(dd)->plt[3] - gpptr(dd)->plt[2])/
	    (gpptr(dd)->logusr[3] - gpptr(dd)->logusr[2]);
	gpptr(dd)->win2fig.ay = dpptr(dd)->win2fig.ay =
	    gpptr(dd)->plt[2] - gpptr(dd)->win2fig.by * gpptr(dd)->logusr[2];
    }
    else {
	gpptr(dd)->win2fig.by = dpptr(dd)->win2fig.by =
	    (gpptr(dd)->plt[3] - gpptr(dd)->plt[2])/
	    (gpptr(dd)->usr[3] - gpptr(dd)->usr[2]);
	gpptr(dd)->win2fig.ay = dpptr(dd)->win2fig.ay =
	    gpptr(dd)->plt[2] - gpptr(dd)->win2fig.by * gpptr(dd)->usr[2];
    }
}

/*  mapping -- Set up mappings between coordinate systems  */
/*  This is the user's interface to the mapping routines above */

static
void mapping(pGEDevDesc dd, int which)
{
    switch(which) {
    case 0:
	mapNDC2Dev(dd);
    case 1:
	updateOuterMargins(dd);
	mapInner2Dev(dd);
    case 2:
	if (gpptr(dd)->defaultFigure)
	    mapFigureRegion(dd);
	updateFigureRegion(dd);
	mapFig2Dev(dd);
    case 3:
	updateFigureMargins(dd);
	if (gpptr(dd)->defaultPlot)
	    mapPlotRegion(dd);
	updatePlotRegion(dd);
    }
}

/*  GReset -- Reset coordinate systems mappings and unit yardsticks */

void GReset(pGEDevDesc dd)
{
    /* Character extents are based on the raster size */
    gpptr(dd)->mkh = gpptr(dd)->scale * dd->dev->cra[0]
	* dd->dev->ipr[0];

    /* Recompute Mappings */
    mapping(dd, 0);
}

/*  Is the figure region too big ? */

/* Why is this FLT_EPSILON? */
static Rboolean validFigureRegion(pGEDevDesc dd)
{
    return ((gpptr(dd)->fig[0] > 0-FLT_EPSILON) &&
	    (gpptr(dd)->fig[1] < 1+FLT_EPSILON) &&
	    (gpptr(dd)->fig[2] > 0-FLT_EPSILON) &&
	    (gpptr(dd)->fig[3] < 1+FLT_EPSILON));
}

/*  Is the figure region too small ? */

static Rboolean validOuterMargins(pGEDevDesc dd)
{
    return ((gpptr(dd)->fig[0] < gpptr(dd)->fig[1]) &&
	    (gpptr(dd)->fig[2] < gpptr(dd)->fig[3]));
}

/* Is the plot region too big ? */

static Rboolean validPlotRegion(pGEDevDesc dd)
{
    return ((gpptr(dd)->plt[0] > 0-FLT_EPSILON) &&
	    (gpptr(dd)->plt[1] < 1+FLT_EPSILON) &&
	    (gpptr(dd)->plt[2] > 0-FLT_EPSILON) &&
	    (gpptr(dd)->plt[3] < 1+FLT_EPSILON));
}

/* Is the plot region too small ? */

static Rboolean validFigureMargins(pGEDevDesc dd)
{
    return ((gpptr(dd)->plt[0] < gpptr(dd)->plt[1]) &&
	    (gpptr(dd)->plt[2] < gpptr(dd)->plt[3]));
}

static void NORET invalidError(const char *message, pGEDevDesc dd)
{
    dpptr(dd)->currentFigure -= 1;
    if (dpptr(dd)->currentFigure < 1)
	dpptr(dd)->currentFigure = dpptr(dd)->lastFigure;
    gpptr(dd)->currentFigure = dpptr(dd)->currentFigure;
    error(message);
}

Rboolean GRecording(SEXP call, pGEDevDesc dd)
{
    return GErecording(call, dd);
}

/*  GNewPlot -- Begin a new plot (advance to new frame if needed)  */
pGEDevDesc GNewPlot(Rboolean recording)
{
    pGEDevDesc dd;

    /* Restore Default Parameters */

    dd = GEcurrentDevice();
    GRestore(dd);

    /* GNewPlot always starts a new plot UNLESS the user has set
     * gpptr(dd)->new to TRUE by par(new=TRUE)
     * If gpptr(dd)->new is FALSE, we leave it that way (further GNewPlot's
     * will move on to subsequent plots)
     * If gpptr(dd)->new is TRUE, any subsequent drawing will dirty the plot
     * and reset gpptr(dd)->new to FALSE
     */

    /* we can call par(mfg) before any plotting.
       That sets new = TRUE and also sets currentFigure <= lastFigure
       so treat separately. */

    /* The logic for when to start a new page is mimiced in the
     * read-only par("page") in par.c, SO if you make changes
     * to the logic here, you will need to change that as well
     */
    if (!gpptr(dd)->new) {
	R_GE_gcontext gc;
	gcontextFromGP(&gc, dd);
	dpptr(dd)->currentFigure += 1;
	gpptr(dd)->currentFigure = dpptr(dd)->currentFigure;
	if (gpptr(dd)->currentFigure > gpptr(dd)->lastFigure) {
	    if (recording) {
		if (dd->ask) {
		    NewFrameConfirm(dd->dev);
		    /*
		     * User may have killed device during pause for prompt
		     */
		    if (NoDevices())
			error(_("attempt to plot on null device"));
		    else
			dd = GEcurrentDevice();
		}
		GEinitDisplayList(dd);
	    }
	    GENewPage(&gc, dd);
	    dpptr(dd)->currentFigure = gpptr(dd)->currentFigure = 1;
	}

	GReset(dd);
	GForceClip(dd);
    } else if(!gpptr(dd)->state) { /* device is unused */
	R_GE_gcontext gc;
	gcontextFromGP(&gc, dd);
	if (recording) {
	    if (dd->ask) {
		NewFrameConfirm(dd->dev);
		/*
		 * User may have killed device during pause for prompt
		 */
		if (NoDevices())
		    error(_("attempt to plot on null device"));
		else
		    dd = GEcurrentDevice();
	    }
	    GEinitDisplayList(dd);
	}
	GENewPage(&gc, dd);
	dpptr(dd)->currentFigure = gpptr(dd)->currentFigure = 1;
	GReset(dd);
	GForceClip(dd);
    }

    /* IF the division of the device into separate regions */
    /* has resulted in any invalid regions ... */
    /* IF this was a user command (i.e., we are recording) */
    /* send an error message to the command line */
    /* IF we are replaying then draw a message in the output */

#define G_ERR_MSG(msg)			\
	if (recording)			\
	    invalidError(msg, dd);	\
	else {				\
	    int xpdsaved = gpptr(dd)->xpd; \
	    gpptr(dd)->xpd = 2; \
	    GText(0.5,0.5, NFC, msg, -1, 0.5,0.5,  0, dd);  \
	    gpptr(dd)->xpd = xpdsaved; \
	}

    dpptr(dd)->valid = gpptr(dd)->valid = FALSE;
    if (!validOuterMargins(dd)) {
	G_ERR_MSG(_("outer margins too large (figure region too small)"));
    } else if (!validFigureRegion(dd)) {
	G_ERR_MSG(_("figure region too large"));
    } else if (!validFigureMargins(dd)) {
	G_ERR_MSG(_("figure margins too large"));
    } else if (!validPlotRegion(dd)) {
	G_ERR_MSG(_("plot region too large"));
    } else {
	dpptr(dd)->valid = gpptr(dd)->valid = TRUE;
	/*
	 * At this point, base output has been successfully
	 * produced on the device, so mark the device "dirty"
	 * with respect to base graphics.
	 * This is used when checking whether the device is
	 * "valid" with respect to base graphics
	 */
	Rf_setBaseDevice(TRUE, dd);
	GEdirtyDevice(dd);
    }

    return dd;
}
#undef G_ERR_MSG

/*
// (usr, log, n_inp) |--> (axp = (min, max), n_out) :

void GAxisPars(double *min, double *max, int *n, Rboolean log, int axis)

* ----> in src/main/graphics.c (as used in grDevices too)
*          ------------------- */

void GScale(double min, double max, int axis, pGEDevDesc dd)
{
/* GScale: used to default axis information
 *	   i.e., if user has NOT specified par(usr=...)
 * NB: can have min > max !
 */
#define EPS_FAC_1  16

    Rboolean is_xaxis = (axis == 1 || axis == 3);
    int log, n, style;
    double temp, min_o = 0., max_o = 0., tmp2 = 0.;/*-Wall*/

    if(is_xaxis) {
	n = gpptr(dd)->lab[0];
	style = gpptr(dd)->xaxs;
	log = gpptr(dd)->xlog;
    }
    else {
	n = gpptr(dd)->lab[1];
	style = gpptr(dd)->yaxs;
	log = gpptr(dd)->ylog;
    }

    if (log) {
	/*  keep original  min, max - to use in extremis */
	min_o = min; max_o = max;
	min = log10(min);
	max = log10(max);
    }
    if(!R_FINITE(min) || !R_FINITE(max)) {
	warning(_("nonfinite axis limits [GScale(%g,%g,%d, .); log=%d]"),
		min, max, axis, log);
	if(!R_FINITE(min)) min = - .45 * DBL_MAX;
	if(!R_FINITE(max)) max = + .45 * DBL_MAX;
	/* max - min is now finite */
    }
    /* Version <= 1.2.0 had
       if (min == max)	 -- exact equality for real numbers */
    temp = fmax2(fabs(max), fabs(min));
    if(temp == 0) {/* min = max = 0 */
	min = -1;
	max =  1;
    }
    else if(fabs(max - min) < temp * EPS_FAC_1 * DBL_EPSILON) {
	temp *= (min == max) ? .4 : 1e-2;
	min -= temp;
	max += temp;
    }

    switch(style) {
    case 'r':
	temp = 0.04 * (max-min);
	min -= temp;
	max += temp;
	break;
    case 'i':
	break;
    case 's':/* FIXME --- implement  's' and 'e' axis styles ! */
    case 'e':
    default:
	error(_("axis style \"%c\" unimplemented"), style);
    }

    if (log) { /* 10^max may have gotten +Inf ; or  10^min has become 0 */
	if((temp = Rexp10(min)) == 0.) {/* or < 1.01*DBL_MIN */
	    temp = fmin2(min_o, 1.01* DBL_MIN); /* allow smaller non 0 */
	    min = log10(temp);
	}
	if(max >= 308.25) { /* overflows */
	    tmp2 = fmax2(max_o, .99 * DBL_MAX);
	    max = log10(tmp2);
	} else tmp2 = Rexp10(max);
    }
    if(is_xaxis) {
	if (log) {
	    gpptr(dd)->usr[0] = dpptr(dd)->usr[0] = temp;
	    gpptr(dd)->usr[1] = dpptr(dd)->usr[1] = tmp2;
	    gpptr(dd)->logusr[0] = dpptr(dd)->logusr[0] = min;
	    gpptr(dd)->logusr[1] = dpptr(dd)->logusr[1] = max;
	} else {
	    gpptr(dd)->usr[0] = dpptr(dd)->usr[0] = min;
	    gpptr(dd)->usr[1] = dpptr(dd)->usr[1] = max;
	}
    } else {
	if (log) {
	    gpptr(dd)->usr[2] = dpptr(dd)->usr[2] = temp;
	    gpptr(dd)->usr[3] = dpptr(dd)->usr[3] = tmp2;
	    gpptr(dd)->logusr[2] = dpptr(dd)->logusr[2] = min;
	    gpptr(dd)->logusr[3] = dpptr(dd)->logusr[3] = max;
	} else {
	    gpptr(dd)->usr[2] = dpptr(dd)->usr[2] = min;
	    gpptr(dd)->usr[3] = dpptr(dd)->usr[3] = max;
	}
    }

    /* This is not directly needed when [xy]axt = "n",
     * but may later be different in another call to axis(), e.g.:
      > plot(1, xaxt = "n");  axis(1)
     * In that case, do_axis() should do the following:
     */

    // Computation of [xy]axp[0:2] == (min,max,n) :
    GAxisPars(&min, &max, &n, log, axis);

#define G_Store_AXP(is_X)			\
    if(is_X) {					\
	gpptr(dd)->xaxp[0] = dpptr(dd)->xaxp[0] = min;	\
	gpptr(dd)->xaxp[1] = dpptr(dd)->xaxp[1] = max;	\
	gpptr(dd)->xaxp[2] = dpptr(dd)->xaxp[2] = n;	\
    }						\
    else {					\
	gpptr(dd)->yaxp[0] = dpptr(dd)->yaxp[0] = min;	\
	gpptr(dd)->yaxp[1] = dpptr(dd)->yaxp[1] = max;	\
	gpptr(dd)->yaxp[2] = dpptr(dd)->yaxp[2] = n;	\
    }

    G_Store_AXP(is_xaxis);
}
#undef EPS_FAC_1
#undef EPS_FAC_2

void GSetupAxis(int axis, pGEDevDesc dd)
{
/*  GSetupAxis -- Set up the default axis information
 *		  called when user specifies	par(usr =...) */
/*  What should happen if			------------
 *   xlog or ylog = TRUE ? */
    double min, max;
    int n;
    Rboolean is_xaxis = (axis == 1 || axis == 3);

    if(is_xaxis) {
	n = gpptr(dd)->lab[0];
	min = gpptr(dd)->usr[0];
	max = gpptr(dd)->usr[1];
    }
    else {
	n = gpptr(dd)->lab[1];
	min = gpptr(dd)->usr[2];
	max = gpptr(dd)->usr[3];
    }

    GPretty(&min, &max, &n);

    G_Store_AXP(is_xaxis);
}
#undef G_Store_AXP

/*-------------------------------------------------------------------
 *
 *  GPAR FUNCTIONS
 *
 */


/* Set default graphics parameter values in a GPar.
 * This initialises the plot state, plus the graphical
 * parameters that are not the responsibility of the device initialisation.

 * Called from baseCallback.
 */

void GInit(GPar *dp)
{
    dp->state = 0;
    dp->valid = FALSE;

    dp->ann = TRUE;
    dp->err = 0;
    dp->bty = 'o';

    dp->mkh = .001;/* dummy value > 0  --- set in GReset : unused in R */
    dp->cex = 1.0;
    dp->lheight = 1.0;
    dp->cexbase = 1.0;
    dp->cexmain = 1.2;
    dp->cexlab = 1.0;
    dp->cexsub = 1.0;
    dp->cexaxis = 1.0;

    dp->col = R_RGB(0, 0, 0);
    dp->colmain = R_RGB(0, 0, 0);
    dp->collab = R_RGB(0, 0, 0);
    dp->colsub = R_RGB(0, 0, 0);
    dp->colaxis = R_RGB(0, 0, 0);
    dp->gamma = 1;

    dp->scale = 1.0;
    strcpy(dp->family, "");
    dp->font = 1;
    dp->fontmain = 2;
    dp->fontlab = 1;
    dp->fontsub = 1;
    dp->fontaxis = 1;

    dp->pch = 1;
    dp->lty = LTY_SOLID;
    dp->lend = GE_ROUND_CAP;
    dp->ljoin = GE_ROUND_JOIN;
    dp->lmitre = 10.0;
    dp->smo = 1;

    /* String Adjustment and rotation */
    dp->adj = 0.5;
    dp->crt = 0.0;
    dp->srt = 0.0;

    /* Positioning of margin text */
    dp->mgp[0] = 3;
    dp->mgp[1] = 1;
    dp->mgp[2] = 0;

    /* Axis annotation parameters */
    dp->lab[0] = 5;
    dp->lab[1] = 5;
    dp->lab[2] = 7;
    dp->las = 0;
    dp->tck = NA_REAL;
    dp->tcl = -0.5;
    dp->xaxp[0] = 0.0;
    dp->xaxp[1] = 1.0;
    dp->xaxp[2] = 5.0;
    dp->xaxs = 'r';
    dp->xaxt = 's';
    dp->xlog = FALSE;
    dp->xpd = 0;
    dp->oldxpd = -99;
    dp->yaxp[0] = 0.0;
    dp->yaxp[1] = 1.0;
    dp->yaxp[2] = 5.0;
    dp->yaxs = 'r';
    dp->yaxt = 's';
    dp->ylog = FALSE;

    /* Outer Margins */
    dp->mex = 1.0;
    dp->oma[0] = 0.0;
    dp->oma[1] = 0.0;
    dp->oma[2] = 0.0;
    dp->oma[3] = 0.0;
    dp->oUnits = LINES;
    dp->fig[0] = 0.0;
    dp->fig[1] = 1.0;
    dp->fig[2] = 0.0;
    dp->fig[3] = 1.0;
    dp->fUnits = NIC;
    dp->defaultFigure = TRUE;	/* the figure region is calculated from */
				/* the layout by default */
    dp->pUnits = NFC;
    dp->defaultPlot = TRUE;	/* the plot region is calculated as */
				/* figure-margin by default */

    /* Inner Margins */
    dp->mar[0] = 5.1;
    dp->mar[1] = 4.1;
    dp->mar[2] = 4.1;
    dp->mar[3] = 2.1;
    dp->mUnits = LINES;

    /* Multi-figure parameters */
    dp->layout = FALSE;
    dp->mfind  = 0;

    dp->numrows = 1;
    dp->numcols = 1;
    dp->currentFigure = 1;
    dp->lastFigure = 1;
    dp->heights[0] = 1;
    dp->widths[0] = 1;
    dp->cmHeights[0] = 0;
    dp->cmWidths[0] = 0;
    dp->order[0] = 1;
    dp->rspct = 0;
    dp->respect[0] = 0;

    /* Misc plotting parameters */
    dp->new = FALSE;
    dp->devmode = -99;
    dp->pty = 'm';
    dp->lwd = 1;

    /* Data window */
    dp->usr[0] = 0.0;
    dp->usr[1] = 1.0;
    dp->usr[2] = 0.0;
    dp->usr[3] = 1.0;
}

/* Copy a GPar structure from source to dest. */
void copyGPar(GPar *source, GPar *dest)
{
    memcpy(dest, source, sizeof(GPar));
}


/* Restore the graphics parameters from the device copy. */
void GRestore(pGEDevDesc dd)
{
    if (NoDevices()) error(_("no graphics device is active"));
    copyGPar(dpptr(dd), gpptr(dd));
}


/* FIXME: reorganize this as a memcpy */

/*  Saving and restoring of "inline" graphical	*/
/*  parameters.	 These are the ones which can be  */
/*  specified as a arguments to high-level  */
/*  graphics functions.	 */

static double	adjsave;	/* adj */
static int	annsave;	/* ann */
static char	btysave;	/* bty */
static double	cexsave;	/* cex */
static double   lheightsave;
static double	cexbasesave;	/* cexbase */
static double	cexmainsave;	/* cex.main */
static double	cexlabsave;	/* cex.lab */
static double	cexsubsave;	/* cex.sub */
static double	cexaxissave;	/* cex.axis */
static int	colsave;	/* col */
static int	fgsave;		/* fg */
static int	bgsave;		/* bg */
static int	colmainsave;	/* col.main */
static int	collabsave;	/* col.lab */
static int	colsubsave;	/* col.sub */
static int	colaxissave;	/* col.axis */
static double	crtsave;	/* character rotation */
static char     familysave[201];
static int	fontsave;	/* font */
static int	fontmainsave;	/* font.main */
static int	fontlabsave;	/* font.lab */
static int	fontsubsave;	/* font.sub */
static int	fontaxissave;	/* font.axis */
static int	errsave;	/* error mode */
static int	labsave[3];	/* axis labelling parameters */
static int	lassave;	/* label style */
static int	ltysave;	/* line type */
static double	lwdsave;	/* line width */
static R_GE_lineend lendsave;
static R_GE_linejoin ljoinsave;
static double   lmitresave;
static double	mgpsave[3];	/* margin position for annotation */
static double	mkhsave;	/* mark height */
static int	pchsave;	/* plotting character */
static double	srtsave;	/* string rotation */
static double	tcksave;	/* tick mark length */
static double	tclsave;	/* tick mark length in LINES */
static double	xaxpsave[3];	/* x axis parameters */
static char	xaxssave;	/* x axis calculation style */
static char	xaxtsave;	/* x axis type */
static int	xpdsave;	/* clipping control */
static double	yaxpsave[3];	/* y axis parameters */
static char	yaxssave;	/* y axis calculation style */
static char	yaxtsave;	/* y axis type */


/* Make a temporary copy of the inline parameter values. */
void GSavePars(pGEDevDesc dd)
{
    adjsave = gpptr(dd)->adj;
    annsave = gpptr(dd)->ann;
    btysave = gpptr(dd)->bty;
    cexsave = gpptr(dd)->cex;
    lheightsave = gpptr(dd)->lheight;
    cexbasesave = gpptr(dd)->cexbase;
    cexlabsave = gpptr(dd)->cexlab;
    cexmainsave = gpptr(dd)->cexmain;
    cexsubsave = gpptr(dd)->cexsub;
    cexaxissave = gpptr(dd)->cexaxis;
    colsave = gpptr(dd)->col;
    fgsave = gpptr(dd)->fg;
    bgsave = gpptr(dd)->bg;
    collabsave = gpptr(dd)->collab;
    colmainsave = gpptr(dd)->colmain;
    colsubsave = gpptr(dd)->colsub;
    colaxissave = gpptr(dd)->colaxis;
    crtsave = gpptr(dd)->crt;
    errsave = gpptr(dd)->err;
    strncpy(familysave, gpptr(dd)->family, 201);
    fontsave = gpptr(dd)->font;
    fontmainsave = gpptr(dd)->fontmain;
    fontlabsave = gpptr(dd)->fontlab;
    fontsubsave = gpptr(dd)->fontsub;
    fontaxissave = gpptr(dd)->fontaxis;
    labsave[0] = gpptr(dd)->lab[0];
    labsave[1] = gpptr(dd)->lab[1];
    labsave[2] = gpptr(dd)->lab[2];
    lassave = gpptr(dd)->las;
    ltysave = gpptr(dd)->lty;
    lwdsave = gpptr(dd)->lwd;
    lendsave = gpptr(dd)->lend;
    ljoinsave = gpptr(dd)->ljoin;
    lmitresave = gpptr(dd)->lmitre;
    mgpsave[0] = gpptr(dd)->mgp[0];
    mgpsave[1] = gpptr(dd)->mgp[1];
    mgpsave[2] = gpptr(dd)->mgp[2];
    mkhsave = gpptr(dd)->mkh;
    pchsave = gpptr(dd)->pch;
    srtsave = gpptr(dd)->srt;
    tcksave = gpptr(dd)->tck;
    tclsave = gpptr(dd)->tcl;
    xaxpsave[0] = gpptr(dd)->xaxp[0];
    xaxpsave[1] = gpptr(dd)->xaxp[1];
    xaxpsave[2] = gpptr(dd)->xaxp[2];
    xaxssave = gpptr(dd)->xaxs;
    xaxtsave = gpptr(dd)->xaxt;
    xpdsave = gpptr(dd)->xpd;
    yaxpsave[0] = gpptr(dd)->yaxp[0];
    yaxpsave[1] = gpptr(dd)->yaxp[1];
    yaxpsave[2] = gpptr(dd)->yaxp[2];
    yaxssave = gpptr(dd)->yaxs;
    yaxtsave = gpptr(dd)->yaxt;
}


/*  Restore temporarily saved inline parameter values	*/
void GRestorePars(pGEDevDesc dd)
{
    gpptr(dd)->adj = adjsave;
    gpptr(dd)->ann = annsave;
    gpptr(dd)->bty = btysave;
    gpptr(dd)->cex = cexsave;
    gpptr(dd)->lheight = lheightsave;
    gpptr(dd)->cexbase = cexbasesave;
    gpptr(dd)->cexlab = cexlabsave;
    gpptr(dd)->cexmain = cexmainsave;
    gpptr(dd)->cexsub = cexsubsave;
    gpptr(dd)->cexaxis = cexaxissave;
    gpptr(dd)->col = colsave;
    gpptr(dd)->fg = fgsave;
    gpptr(dd)->bg = bgsave;
    gpptr(dd)->collab = collabsave;
    gpptr(dd)->colmain = colmainsave;
    gpptr(dd)->colsub = colsubsave;
    gpptr(dd)->colaxis = colaxissave;
    gpptr(dd)->crt = crtsave;
    gpptr(dd)->err = errsave;
    strncpy(gpptr(dd)->family, familysave, 201);
    gpptr(dd)->font = fontsave;
    gpptr(dd)->fontmain = fontmainsave;
    gpptr(dd)->fontlab = fontlabsave;
    gpptr(dd)->fontsub = fontsubsave;
    gpptr(dd)->fontaxis = fontaxissave;
    gpptr(dd)->lab[0] = labsave[0];
    gpptr(dd)->lab[1] = labsave[1];
    gpptr(dd)->lab[2] = labsave[2];
    gpptr(dd)->las = lassave;
    gpptr(dd)->lty = ltysave;
    gpptr(dd)->lwd = lwdsave;
    gpptr(dd)->lend = lendsave;
    gpptr(dd)->ljoin = ljoinsave;
    gpptr(dd)->lmitre = lmitresave;
    gpptr(dd)->mgp[0] = mgpsave[0];
    gpptr(dd)->mgp[1] = mgpsave[1];
    gpptr(dd)->mgp[2] = mgpsave[2];
    gpptr(dd)->mkh = mkhsave;
    gpptr(dd)->pch = pchsave;
    gpptr(dd)->srt = srtsave;
    gpptr(dd)->tck = tcksave;
    gpptr(dd)->tcl = tclsave;
    gpptr(dd)->xaxp[0] = xaxpsave[0];
    gpptr(dd)->xaxp[1] = xaxpsave[1];
    gpptr(dd)->xaxp[2] = xaxpsave[2];
    gpptr(dd)->xaxs = xaxssave;
    gpptr(dd)->xaxt = xaxtsave;
    gpptr(dd)->xpd = xpdsave;
    gpptr(dd)->yaxp[0] = yaxpsave[0];
    gpptr(dd)->yaxp[1] = yaxpsave[1];
    gpptr(dd)->yaxp[2] = yaxpsave[2];
    gpptr(dd)->yaxs = yaxssave;
    gpptr(dd)->yaxt = yaxtsave;
}

/*-------------------------------------------------------------------
 *
 *  DEVICE STATE FUNCTIONS
 *
 */


/* This records whether GNewPlot has been called. */
void GSetState(int newstate, pGEDevDesc dd)
{
    dpptr(dd)->state = gpptr(dd)->state = newstate;
}



/* Enquire whether GNewPlot has been called. */
void GCheckState(pGEDevDesc dd)
{
    if(gpptr(dd)->state == 0)
	error(_("plot.new has not been called yet"));
    if (!gpptr(dd)->valid)
	error(_("invalid graphics state"));
}

/*-------------------------------------------------------------------
 * GRAPHICAL PRIMITIVES
 *
 */

/* CLIPPING paradigm:

   R uses both the clipping capabilities of the device (if present)
   and its own internal clipping algorithms.
   If the device has no clipping capabilities (canClip = FALSE) then R
   does all of the clipping internally.
   If the device has clipping capabilities, R still does some internal
   clipping (to the device extent).  This is to avoid "silly" values
   being sent to the device (e.g., X11 and Ghostview will barf if you
   send a ridiculously large number to them).  Call this silly-clipping.

       The problem with getting R to do some of the clipping is that it is
       not necessarily as good as the device at clipping (e.g., R's text
       clipping is very crude).  This is the motivation for leaving as much
       of the clipping as possible to the device.
       R does different amounts of silly-clipping for different primitives.
       See the individual routines for more info.
*/


static void setClipRect(double *x1, double *y1, double *x2, double *y2,
			int coords, pGEDevDesc dd)
{
    /*
     * xpd = 0 means clip to current plot region
     * xpd = 1 means clip to current figure region
     * xpd = 2 means clip to device region
     */
    *x1 = 0.0;
    *y1 = 0.0;
    *x2 = 1.0;
    *y2 = 1.0;
    switch (gpptr(dd)->xpd) {
    case 0:
	GConvert(x1, y1, NPC, coords, dd);
	GConvert(x2, y2, NPC, coords, dd);
	break;
    case 1:
	GConvert(x1, y1, NFC, coords, dd);
	GConvert(x2, y2, NFC, coords, dd);
	break;
    case 2:
	GConvert(x1, y1, NDC, coords, dd);
	GConvert(x2, y2, NDC, coords, dd);
	break;
    }
}

/* Update the device clipping region (depends on GP->xpd). */
void GClip(pGEDevDesc dd)
{
    if (gpptr(dd)->xpd != gpptr(dd)->oldxpd) {
	double x1, y1, x2, y2;
	setClipRect(&x1, &y1, &x2, &y2, DEVICE, dd);
	GESetClip(x1, y1, x2, y2, dd);
	gpptr(dd)->oldxpd = gpptr(dd)->xpd;
    }
}


/*  Forced update of the device clipping region. */
void GForceClip(pGEDevDesc dd)
{
    double x1, y1, x2, y2;
    if (gpptr(dd)->state == 0) return;
    setClipRect(&x1, &y1, &x2, &y2, DEVICE, dd);
    GESetClip(x1, y1, x2, y2, dd);
    gpptr(dd)->oldxpd = gpptr(dd)->xpd;
}

/*
 * Function to generate an R_GE_gcontext from gpptr info
 *
 * In some cases, the settings made here will need to be overridden
 * (eps. the fill setting)
 */
/* Used here and in do_xspline */
void gcontextFromGP(pGEcontext gc, pGEDevDesc dd)
{
    gc->col = gpptr(dd)->col;
    gc->fill = gpptr(dd)->bg;  /* This may need manual adjusting */
    gc->gamma = gpptr(dd)->gamma;
    /*
     * Scale by "zoom" factor to allow for fit-to-window resizing in Windows
     */
    gc->lwd = gpptr(dd)->lwd * gpptr(dd)->scale;
    gc->lty = gpptr(dd)->lty;
    gc->lend = gpptr(dd)->lend;
    gc->ljoin = gpptr(dd)->ljoin;
    gc->lmitre = gpptr(dd)->lmitre;
    gc->cex = gpptr(dd)->cex;
    /*
     * Scale by "zoom" factor to allow for fit-to-window resizing in Windows
     */
    gc->ps = (double) gpptr(dd)->ps * gpptr(dd)->scale;
    gc->lineheight = gpptr(dd)->lheight;
    gc->fontface = gpptr(dd)->font;
    strncpy(gc->fontfamily, gpptr(dd)->family, 201);
}

/* Draw a line. */
/* If the device canClip, R clips line to device extent and
   device does all other clipping. */
void GLine(double x1, double y1, double x2, double y2, int coords, pGEDevDesc dd)
{
    R_GE_gcontext gc; gcontextFromGP(&gc, dd);
    if (gpptr(dd)->lty == LTY_BLANK) return;
    /*
     * Work in device coordinates because that is what the
     * graphics engine needs.
     */
    GConvert(&x1, &y1, coords, DEVICE, dd);
    GConvert(&x2, &y2, coords, DEVICE, dd);
    /*
     * Ensure that the base clipping region is set on the device
     */
    GClip(dd);
    if(R_FINITE(x1) && R_FINITE(y1) && R_FINITE(x2) && R_FINITE(y2))
	GELine(x1, y1, x2, y2, &gc, dd);
}

/* We need extra graphics device closure handling
   when inside a call to locator (it should raise
   an error and return).  PR#15253

   This assume that locator is running on only one device at a time,
   which is currently safe.
*/
static void (*old_close)(pDevDesc) = NULL;

static void
#ifndef WIN32
NORET
#endif
locator_close(pDevDesc dd)
{
    if(old_close) old_close(dd);
    dd->close = old_close;
    old_close = NULL;
    /* It's not safe to call error() in a Windows event handler, so
       the GA_Close method records the close event separately.
    */
#ifndef WIN32
    error(_("graphics device closed during call to locator or identify"));
#endif
}


/* Read the current "pen" position. */
Rboolean GLocator(double *x, double *y, int coords, pGEDevDesc dd)
{
  Rboolean ret;
  /* store original close handler (it will still be called on
     closure) and assign new handler that throws an error
  */
  old_close = (dd->dev)->close;
  dd->dev->close = &locator_close;

  if(dd->dev->locator && dd->dev->locator(x, y, dd->dev)) {
      GConvert(x, y, DEVICE, coords, dd);
      ret =  TRUE;
  } else ret =  FALSE;
  /* restore original close handler */
  dd->dev->close = old_close;
  old_close = NULL;
  return ret;

}

/* Access character font metric information.  */
void GMetricInfo(int c, double *ascent, double *descent, double *width,
		 GUnit units, pGEDevDesc dd)
{
    R_GE_gcontext gc;
    gcontextFromGP(&gc, dd);
    dd->dev->metricInfo(c & 0xFF, &gc, ascent, descent, width, dd->dev);
    if (units != DEVICE) {
	*ascent = GConvertYUnits(*ascent, DEVICE, units, dd);
	*descent = GConvertYUnits(*descent, DEVICE, units, dd);
	*width = GConvertXUnits(*width, DEVICE, units, dd);
    }
}


/* Check that everything is initialized :
	Interpretation :
	mode = 0, graphics off
	mode = 1, graphics on
	mode = 2, graphical input on (ignored by most drivers)
*/
void GMode(int mode, pGEDevDesc dd)
{
    if (NoDevices())
	error(_("No graphics device is active"));
    if(mode != gpptr(dd)->devmode) GEMode(mode, dd); /* dd->dev->mode(mode, dd->dev); */
    gpptr(dd)->new = dpptr(dd)->new = FALSE;
    gpptr(dd)->devmode = dpptr(dd)->devmode = mode;
}


/*
***********************************
* START GClipPolygon code
*
* Everything up to END GClipPolygon code
* is just here to support GClipPolygon
* which only exists to satisfy the
* Rgraphics.h API (which should be
* superceded by the API provided by
* GraphicsDevice.h and GraphicsEngine.h)
***********************************
*/
/*
 * If device can't clip we should use something like Sutherland-Hodgman here
 *
 * NOTE:  most of this code (up to GPolygon) is only now used by
 * GClipPolygon -- GPolygon runs the new GEPolygon in engine.c
 */
typedef enum {
    Left = 0,
    Right = 1,
    Bottom = 2,
    Top = 3
} Edge;

/* Clipper State Variables */
typedef struct {
    int first;    /* true if we have seen the first point */
    double fx;    /* x coord of the first point */
    double fy;    /* y coord of the first point */
    double sx;    /* x coord of the most recent point */
    double sy;    /* y coord of the most recent point */
}
GClipState;

/* The Clipping Rectangle */
typedef struct {
    double xmin;
    double xmax;
    double ymin;
    double ymax;
}
GClipRect;

static
int inside (Edge b, double px, double py, GClipRect *clip)
{
    switch (b) {
    case Left:   if (px < clip->xmin) return 0; break;
    case Right:  if (px > clip->xmax) return 0; break;
    case Bottom: if (py < clip->ymin) return 0; break;
    case Top:    if (py > clip->ymax) return 0; break;
    }
    return 1;
}

static
int cross (Edge b, double x1, double y1, double x2, double y2,
	   GClipRect *clip)
{
    if (inside (b, x1, y1, clip) == inside (b, x2, y2, clip))
	return 0;
    else return 1;
}

static
void intersect (Edge b, double x1, double y1, double x2, double y2,
		double *ix, double *iy, GClipRect *clip)
{
    double m = 0;

    if (x1 != x2) m = (y1 - y2) / (x1 - x2);
    switch (b) {
    case Left:
	*ix = clip->xmin;
	*iy = y2 + (clip->xmin - x2) * m;
	break;
    case Right:
	*ix = clip->xmax;
	*iy = y2 + (clip->xmax - x2) * m;
	break;
    case Bottom:
	*iy = clip->ymin;
	if (x1 != x2) *ix = x2 + (clip->ymin - y2) / m;
	else *ix = x2;
	break;
    case Top:
	*iy = clip->ymax;
	if (x1 != x2) *ix = x2 + (clip->ymax - y2) / m;
	else *ix = x2;
	break;
    }
}

static
void clipPoint (Edge b, double x, double y,
		double *xout, double *yout, int *cnt, int store,
		GClipRect *clip, GClipState *cs)
{
    double ix = 0.0, iy = 0.0 /* -Wall */;

    if (!cs[b].first) {
	/* No previous point exists for this edge. */
	/* Save this point. */
	cs[b].first = 1;
	cs[b].fx = x;
	cs[b].fy = y;
    }
    else
	/* A previous point exists.  */
	/* If 'p' and previous point cross edge, find intersection.  */
	/* Clip against next boundary, if any.  */
	/* If no more edges, add intersection to output list. */
	if (cross (b, x, y, cs[b].sx, cs[b].sy, clip)) {
	    intersect (b, x, y, cs[b].sx, cs[b].sy, &ix, &iy, clip);
	    if (b < Top)
		clipPoint (b + 1, ix, iy, xout, yout, cnt, store,
			   clip, cs);
	    else {
		if (store) {
		    xout[*cnt] = ix;
		    yout[*cnt] = iy;
		}
		(*cnt)++;
	    }
	}

    /* Save as most recent point for this edge */
    cs[b].sx = x;
    cs[b].sy = y;

    /* For all, if point is 'inside' */
    /* proceed to next clip edge, if any */
    if (inside (b, x, y, clip)) {
	if (b < Top)
	    clipPoint (b + 1, x, y, xout, yout, cnt, store, clip, cs);
	else {
	    if (store) {
		xout[*cnt] = x;
		yout[*cnt] = y;
	    }
	    (*cnt)++;
	}
    }
}

static
void closeClip (double *xout, double *yout, int *cnt, int store,
		GClipRect *clip, GClipState *cs)
{
    double ix = 0.0, iy = 0.0 /* -Wall */;
    Edge b;

    for (b = Left; b <= Top; b++) {
	if (cross (b, cs[b].sx, cs[b].sy, cs[b].fx, cs[b].fy, clip)) {
	    intersect (b, cs[b].sx, cs[b].sy,
		       cs[b].fx, cs[b].fy, &ix, &iy, clip);
	    if (b < Top)
		clipPoint (b + 1, ix, iy, xout, yout, cnt, store, clip, cs);
	    else {
		if (store) {
		    xout[*cnt] = ix;
		    yout[*cnt] = iy;
		}
		(*cnt)++;
	    }
	}
    }
}

int GClipPolygon(double *x, double *y, int n, int coords, int store,
		 double *xout, double *yout, pGEDevDesc dd)
{
    int i, cnt = 0;
    GClipState cs[4];
    GClipRect clip;
    for (i = 0; i < 4; i++)
	cs[i].first = 0;
    /* Set up the cliprect here for R. */
    setClipRect(&clip.xmin, &clip.ymin, &clip.xmax, &clip.ymax, coords, dd);
    /* If necessary, swap the clip region extremes */
    if (clip.xmax < clip.xmin) {
	double swap = clip.xmax;
	clip.xmax = clip.xmin;
	clip.xmin = swap;
    }
    if (clip.ymax < clip.ymin) {
	double swap = clip.ymax;
	clip.ymax = clip.ymin;
	clip.ymin = swap;
    }
    for (i = 0; i < n; i++)
	clipPoint (Left, x[i], y[i], xout, yout, &cnt, store, &clip, cs);
    closeClip (xout, yout, &cnt, store, &clip, cs);
    return (cnt);
}
/*
***********************************
* END GClipPolygon code
***********************************
*/

/*
 * This is just here to satisfy the Rgraphics.h API.
 * This allows new graphics API (GraphicsDevice.h, GraphicsEngine.h)
 * to be developed alongside.
 * Could be removed if Rgraphics.h ever gets REPLACED by new API
 * NOTE that base graphics code (in plot.c) still calls this.
 */
/* GPolygon -- Draw a polygon
 *	Filled with color bg and outlined with color fg
 *	These may both be NA_INTEGER
 */
void GPolygon(int n, double *x, double *y, int coords,
	      int bg, int fg, pGEDevDesc dd)
{
    int i;
    double *xx;
    double *yy;
    const void *vmaxsave = vmaxget();
    R_GE_gcontext gc; gcontextFromGP(&gc, dd);

    if (gpptr(dd)->lty == LTY_BLANK)
	fg = R_TRANWHITE; /* transparent for the border */

    /*
     * Work in device coordinates because that is what the
     * graphics engine needs.
     */
    xx = (double*) R_alloc(n, sizeof(double));
    yy = (double*) R_alloc(n, sizeof(double));
    if (!xx || !yy)
	error("unable to allocate memory (in GPolygon)");
    for (i=0; i<n; i++) {
	xx[i] = x[i];
	yy[i] = y[i];
	GConvert(&(xx[i]), &(yy[i]), coords, DEVICE, dd);
    }
    /*
     * Ensure that the base clipping region is set on the device
     */
    GClip(dd);
    gc.col = fg;
    gc.fill = bg;
    GEPolygon(n, xx, yy, &gc, dd);
    vmaxset(vmaxsave);
}

#include <stdio.h>

/* Draw a series of line segments. */
/* If the device canClip, R clips to the device extent and the device
   does all other clipping */
void GPolyline(int n, double *x, double *y, int coords, pGEDevDesc dd)
{
    int i;
    double *xx;
    double *yy;
    const void *vmaxsave = vmaxget();
    R_GE_gcontext gc; gcontextFromGP(&gc, dd);

    /*
     * Work in device coordinates because that is what the
     * graphics engine needs.
     */
    xx = (double*) R_alloc(n, sizeof(double));
    yy = (double*) R_alloc(n, sizeof(double));
    if (!xx || !yy)
	error("unable to allocate memory (in GPolyline)");
    for (i=0; i<n; i++) {
	xx[i] = x[i];
	yy[i] = y[i];
	GConvert(&(xx[i]), &(yy[i]), coords, DEVICE, dd);
    }
    /*
     * Ensure that the base clipping region is set on the device
     */
    GClip(dd);
    GEPolyline(n, xx, yy, &gc, dd);
    vmaxset(vmaxsave);
}


/*
 * This is just here to satisfy the Rgraphics.h API.
 * This allows new graphics API (GraphicsDevice.h, GraphicsEngine.h)
 * to be developed alongside.
 * Could be removed if Rgraphics.h ever gets REPLACED by new API
 * NOTE that base graphics code (do_symbol in plot.c) still calls this.
 *
 * NB: this fiddles with radius = 0.
 */
void GCircle(double x, double y, int coords,
	     double radius, int bg, int fg, pGEDevDesc dd)
{
    double ir;
    R_GE_gcontext gc; gcontextFromGP(&gc, dd);

    ir = radius/dd->dev->ipr[0];
    ir = (ir > 0) ? ir : 1;

    if (gpptr(dd)->lty == LTY_BLANK)
	fg = R_TRANWHITE; /* transparent for the border */

    /*
     * Work in device coordinates because that is what the
     * graphics engine needs.
     */
    GConvert(&x, &y, coords, DEVICE, dd);
    /*
     * Ensure that the base clipping region is set on the device
     */
    GClip(dd);
    gc.col = fg;
    gc.fill = bg;
    GECircle(x, y, ir, &gc, dd);
}

/* Draw a rectangle	*/
/* Filled with color bg and outlined with color fg  */
/* These may both be NA_INTEGER	 */
void GRect(double x0, double y0, double x1, double y1, int coords,
	   int bg, int fg, pGEDevDesc dd)
{
    R_GE_gcontext gc; gcontextFromGP(&gc, dd);

    if (gpptr(dd)->lty == LTY_BLANK)
	fg = R_TRANWHITE; /* transparent for the border */

    /*
     * Work in device coordinates because that is what the
     * graphics engine needs.
     */
    GConvert(&x0, &y0, coords, DEVICE, dd);
    GConvert(&x1, &y1, coords, DEVICE, dd);
    /*
     * Ensure that the base clipping region is set on the device
     */
    GClip(dd);
    gc.col = fg;
    gc.fill = bg;
    GERect(x0, y0, x1, y1, &gc, dd);
}

void GPath(double *x, double *y,
           int npoly, int *nper,
           Rboolean winding,
           int bg, int fg, pGEDevDesc dd)
{
    R_GE_gcontext gc; gcontextFromGP(&gc, dd);

    if (gpptr(dd)->lty == LTY_BLANK)
	fg = R_TRANWHITE; /* transparent for the border */

    /*
     * Ensure that the base clipping region is set on the device
     */
    GClip(dd);
    gc.col = fg;
    gc.fill = bg;
    GEPath(x, y, npoly, nper, winding, &gc, dd);
}

void GRaster(unsigned int* image, int w, int h,
             double x0, double y0, double x1, double y1,
             double angle, Rboolean interpolate,
             pGEDevDesc dd)
{
    R_GE_gcontext gc; gcontextFromGP(&gc, dd);

    /*
     * Ensure that the base clipping region is set on the device
     */
    GClip(dd);

    GERaster(image, w, h, x0, y0, x1, y1, angle, interpolate,
             &gc, dd);
}

/* Compute string width. */
double GStrWidth(const char *str, cetype_t enc, GUnit units, pGEDevDesc dd)
{
    double w;
    R_GE_gcontext gc; gcontextFromGP(&gc, dd);
    w = GEStrWidth(str, (gc.fontface == 5) ? CE_SYMBOL:enc, &gc, dd);
    if (units != DEVICE)
	w = GConvertXUnits(w, DEVICE, units, dd);
    return w;
}


/* Compute string height. */
double GStrHeight(const char *str, cetype_t enc, GUnit units, pGEDevDesc dd)
{
    double h;
    R_GE_gcontext gc; gcontextFromGP(&gc, dd);
    h = GEStrHeight(str, (gc.fontface == 5) ? CE_SYMBOL:enc, &gc, dd);
    if (units != DEVICE)
	h = GConvertYUnits(h, DEVICE, units, dd);
    return h;
}

/* Draw text in a plot. */
/* If you want EXACT centering of text (e.g., like in GSymbol) */
/* then pass NA_REAL for xc and yc */
void GText(double x, double y, int coords, const char *str, cetype_t enc,
	   double xc, double yc, double rot, pGEDevDesc dd)
{
    R_GE_gcontext gc; gcontextFromGP(&gc, dd);
    /*
     * Work in device coordinates because that is what the
     * graphics engine needs.
     */
    GConvert(&x, &y, coords, DEVICE, dd);
    /*
     * Ensure that the base clipping region is set on the device
     */
    GClip(dd);
    GEText(x, y, str, (gc.fontface == 5) ? CE_SYMBOL:enc, xc, yc, rot, &gc, dd);
}

/*-------------------------------------------------------------------
 *
 *  GRAPHICAL UTILITIES
 *
 */


/* GArrow -- Draw an arrow. */
/* NOTE that the length parameter is in inches. */
void GArrow(double xfrom, double yfrom, double xto, double yto, int coords,
	    double length, double angle, int code, pGEDevDesc dd)
{

    double xfromInch = xfrom;
    double yfromInch = yfrom;
    double xtoInch = xto;
    double ytoInch = yto;
    double rot, xc, yc;
    double x[3], y[3];
    double eps = 1.e-3;

    GLine(xfrom, yfrom, xto, yto, coords, dd);

    GConvert(&xfromInch, &yfromInch, coords, INCHES, dd);
    GConvert(&xtoInch, &ytoInch, coords, INCHES, dd);
    if((code & 3) == 0) return; /* no arrows specified */
    if(length == 0) return; /* zero-length arrow heads */

    if(hypot(xfromInch - xtoInch, yfromInch - ytoInch) < eps) {
	/* effectively 0-length arrow */
	warning(_("zero-length arrow is of indeterminate angle and so skipped"));
	return;
    }
    angle *= DEG2RAD;
    if(code & 1) {
	xc = xtoInch - xfromInch;
	yc = ytoInch - yfromInch;
	rot= atan2(yc, xc);
	x[0] = xfromInch + length * cos(rot+angle);
	y[0] = yfromInch + length * sin(rot+angle);
	x[1] = xfromInch;
	y[1] = yfromInch;
	x[2] = xfromInch + length * cos(rot-angle);
	y[2] = yfromInch + length * sin(rot-angle);
	GPolyline(3, x, y, INCHES, dd);
    }
    if(code & 2) {
	xc = xfromInch - xtoInch;
	yc = yfromInch - ytoInch;
	rot= atan2(yc, xc);
	x[0] = xtoInch + length * cos(rot+angle);
	y[0] = ytoInch + length * sin(rot+angle);
	x[1] = xtoInch;
	y[1] = ytoInch;
	x[2] = xtoInch + length * cos(rot-angle);
	y[2] = ytoInch + length * sin(rot-angle);
	GPolyline(3, x, y, INCHES, dd);
    }
}


/* Draw a box about one of several regions:  box(which) */
void GBox(int which, pGEDevDesc dd)
{
    double x[7], y[7];
    if (which == 1) {/* plot */
	x[0] = gpptr(dd)->plt[0]; y[0] = gpptr(dd)->plt[2];/* <- , __ */
	x[1] = gpptr(dd)->plt[1]; y[1] = gpptr(dd)->plt[2];/* -> , __ */
	x[2] = gpptr(dd)->plt[1]; y[2] = gpptr(dd)->plt[3];/* -> , ^  */
	x[3] = gpptr(dd)->plt[0]; y[3] = gpptr(dd)->plt[3];/* <- , ^  */
	x[4] = x[0];	      y[4] = y[0];	   /* <- , __ */
	x[5] = x[1];	      y[5] = y[1];	   /* -> , __ */
	x[6] = x[2];	      y[6] = y[2];	   /* -> , __ */
    }
    else {/* "figure", "inner", or "outer" */
	x[0] = 0.; y[0] = 0.;
	x[1] = 1.; y[1] = 0.;
	x[2] = 1.; y[2] = 1.;
	x[3] = 0.; y[3] = 1.;
    }
    switch(which) {
    case 1: /* Plot */
	switch(gpptr(dd)->bty) {
	case 'o':
	case 'O':
	    GPolygon(4, x, y, NFC,
		     R_TRANWHITE, gpptr(dd)->col, dd);
	    break;
	case 'l':
	case 'L':
	    GPolyline(3, x+3, y+3, NFC, dd);
	    break;
	case '7':
	    GPolyline(3, x+1, y+1, NFC, dd);
	    break;
	case 'c':
	case 'C':
	case '[':
	    GPolyline(4, x+2, y+2, NFC, dd);
	    break;
	case ']':/* new */
	    GPolyline(4, x, y, NFC, dd);
	    break;
	case 'u':
	case 'U':
	    GPolyline(4, x+3, y+3, NFC, dd);
	    break;
	case 'n':
	case 'N': /* nothing */
	    break;
	default:
	    warning(_("invalid par(\"bty\") = '%c'; no box() drawn"),
		    gpptr(dd)->bty);
	}
	break;
    case 2: /* Figure */
	GPolygon(4, x, y, NFC,
		 R_TRANWHITE, gpptr(dd)->col, dd);
	break;
    case 3: /* Inner Region */
	GPolygon(4, x, y, NIC,
		 R_TRANWHITE, gpptr(dd)->col, dd);
	break;
    case 4: /* "outer": Device border */
	GPolygon(4, x, y, NDC,
		 R_TRANWHITE, gpptr(dd)->col, dd);
	break;
    default:
	error(_("invalid argument to GBox"));
    }
}


/*
void GLPretty(double *ul, double *uh, int *n)

void GPretty(double *lo, double *up, int *ndiv)

* ----> in src/main/graphics.c (as used in grDevices too)
*          ------------------- */


#define SMALL	0.25
#define RADIUS	0.375
#define SQRC	0.88622692545275801364		/* sqrt(pi / 4) */
#define DMDC	1.25331413731550025119		/* sqrt(pi / 4) * sqrt(2) */
#define TRC0	1.55512030155621416073		/* sqrt(4 * pi/(3 * sqrt(3))) */
#define TRC1	1.34677368708859836060		/* TRC0 * sqrt(3) / 2 */
#define TRC2	0.77756015077810708036		/* TRC0 / 2 */
#define CMAG	1.0				/* Circle magnifier, now defunct */
#define GSTR_0  dpptr(dd)->scale * dd->dev->cra[1] * 0.5 * dd->dev->ipr[1] * gpptr(dd)->cex
/* NOTE: This cex is already multiplied with cexbase */

/* Draw one of the R special symbols. */
void GSymbol(double x, double y, int coords, int pch, pGEDevDesc dd)
{
    double size = GConvertYUnits(GSTR_0, INCHES, DEVICE, dd);
    R_GE_gcontext gc; gcontextFromGP(&gc, dd);
    /*
     * Work in device coordinates because that is what the
     * graphics engine needs.
     */
    GConvert(&x, &y, coords, DEVICE, dd);
    /*
     * Ensure that the base clipping region is set on the device
     */
    GClip(dd);
    /*
     * Force line type LTY_SOLID
     * i.e., current par(lty) is ignored when drawing symbols
     */
    gc.lty = LTY_SOLID;
    /*
     * special case for pch = "."
     */
    if(pch == 46) size = gpptr(dd)->cex;
    GESymbol(x, y, pch, size, &gc, dd);
}


/* Draw text in plot margins. */
void GMtext(const char *str, cetype_t enc, int side, double line, int outer,
	    double at, int las, double yadj, pGEDevDesc dd)
{
/* "las" gives the style of axis labels:
	 0 = always parallel to the axis [= default],
	 1 = always horizontal,
	 2 = always perpendicular to the axis.
	 3 = always vertical.
*/
    double angle, xadj;
    int coords;

    /* Init to keep -Wall happy: */
    angle = 0.;
    coords = 0;

    xadj = gpptr(dd)->adj;	/* ALL cases */
    if(outer) {
	switch(side) {
	case 1:	    coords = OMA1;	break;
	case 2:	    coords = OMA2;	break;
	case 3:	    coords = OMA3;	break;
	case 4:	    coords = OMA4;	break;
	}
    }
    else {
	switch(side) {
	case 1:	    coords = MAR1;	break;
	case 2:	    coords = MAR2;	break;
	case 3:	    coords = MAR3;	break;
	case 4:	    coords = MAR4;	break;
	}
    }
    /* Note: I changed gpptr(dd)->yLineBias to 0.3 here. */
    /* Purely visual tuning. RI */
    /* This has been replaced by a new argument padj (=yadj here) to axis()
       and mtext() and that can either be set manually or is determined in
       a somehow fuzzy manner with respect to current side and las settings.
       Uwe L.
    */
    /* Note from PR#14532:
       yLineBias is the proportion of line height that is white
       space. The manipulation of "line" below is pure visual tuning
       such that when we plot horizontal text on side 1 (or vertical
       text on side 4) with padj=0 (i.e. text written *above* the
       specified y-value), it is symmetric w.r.t text written on sides
       1 and 2 with padj=0.
    */
    switch(side) {
    case 1:
	if(las == 2 || las == 3) {
	    angle = 90;
	}
	else {
	    line += (1/gpptr(dd)->mex)*(1 - dd->dev->yLineBias);
	    angle = 0;
	}
	break;
    case 2:
	if(las == 1 || las == 2) {
	    angle = 0;
	}
	else {
	    line += (1/gpptr(dd)->mex)*dd->dev->yLineBias;
	    angle = 90;
	}
	break;
    case 3:
	if(las == 2 || las == 3) {
	    angle = 90;
	}
	else {
	    line += (1/gpptr(dd)->mex)*dd->dev->yLineBias;
	    angle = 0;
	}
	break;
    case 4:
	if(las == 1 || las == 2) {
	    angle = 0;
	}
	else {
	    line += (1/gpptr(dd)->mex)*(1 - dd->dev->yLineBias);
	    angle = 90;
	}
	break;
    }
    GText(at, line, coords, str, enc, xadj, yadj, angle, dd);
}/* GMtext */

/* ------------------------------------------------------------
   code below here moved from plotmath.c, which said

 *  This source code module:
 *  Copyright (C) 1997, 1998 Paul Murrell and Ross Ihaka
 *  Copyright (C) 1998-2008  The R Core Team

 */

double GExpressionWidth(SEXP expr, GUnit units, pGEDevDesc dd)
{
    R_GE_gcontext gc;
    double width;
    gcontextFromGP(&gc, dd);
    width = GEExpressionWidth(expr, &gc, dd);
    if (units == DEVICE)
	return width;
    else
	return GConvertXUnits(width, DEVICE, units, dd);
}

double GExpressionHeight(SEXP expr, GUnit units, pGEDevDesc dd)
{
    R_GE_gcontext gc;
    double height;
    gcontextFromGP(&gc, dd);
    height = GEExpressionHeight(expr, &gc, dd);
    if (units == DEVICE)
	return height;
    else
	return GConvertYUnits(height, DEVICE, units, dd);
}

/* Comment is NOT true: used in plot.c for strwidth and strheight.
 *
 * This is just here to satisfy the Rgraphics.h API.
 * This allows new graphics API (GraphicsDevice.h, GraphicsEngine.h)
 * to be developed alongside.
 * Could be removed if Rgraphics.h ever gets REPLACED by new API
 * NOTE that base graphics code no longer calls this -- the base
 * graphics system directly calls the graphics engine for mathematical
 * annotation (GEMathText in ../../../main/plotmath.c )
 */
void GMathText(double x, double y, int coords, SEXP expr,
	       double xc, double yc, double rot,
	       pGEDevDesc dd)
{
    R_GE_gcontext gc;
    gcontextFromGP(&gc, dd);
    GConvert(&x, &y, coords, DEVICE, dd);
    GClip(dd);
    GEMathText(x, y, expr, xc, yc, rot, &gc, dd);
}

void GMMathText(SEXP str, int side, double line, int outer,
		double at, int las, double yadj, pGEDevDesc dd)
{
    int coords = 0;
    double xadj, angle = 0;

    /* IF font metric information is not available for device */
    /* then bail out */
    double ascent, descent, width;
    GMetricInfo('M', &ascent, &descent, &width, DEVICE, dd);
    if ((ascent == 0) && (descent == 0) && (width == 0))
	error(_("metric information not available for this device"));

    xadj = gpptr(dd)->adj;

    /* This is MOSTLY the same as the same section of GMtext
     * BUT it differs because it sets different values for yadj for
     * different situations.
     * Paul
     */
     /* changed to unify behaviour with changes in GMText. Uwe */
    if(outer) {
	switch(side) {
	case 1:	    coords = OMA1;	break;
	case 2:	    coords = OMA2;	break;
	case 3:	    coords = OMA3;	break;
	case 4:	    coords = OMA4;	break;
	}
    }
    else {
	switch(side) {
	case 1:	    coords = MAR1;	break;
	case 2:	    coords = MAR2;	break;
	case 3:	    coords = MAR3;	break;
	case 4:	    coords = MAR4;	break;
	}
    }
    switch(side) {
    case 1:
	if(las == 2 || las == 3) {
	    angle = 90;
	}
	else {
	    /*	    line = line + 1 - gpptr(dd)->yLineBias;
		    angle = 0;
		    yadj = NA_REAL; */
	    line += (1/gpptr(dd)->mex)*(1 - dd->dev->yLineBias);
	    angle = 0;
	}
	break;
    case 2:
	if(las == 1 || las == 2) {
	    angle = 0;
	}
	else {
	    /*	    line = line + gpptr(dd)->yLineBias;
		    angle = 90;
		    yadj = NA_REAL; */
	    /* The following line is needed for symmetry with plain text
	       but changes existing output */
	    line += (1/gpptr(dd)->mex)*dd->dev->yLineBias;
	    angle = 90;
	}
	break;
    case 3:
	if(las == 2 || las == 3) {
	    angle = 90;
	}
	else {
	    /*   line = line + gpptr(dd)->yLineBias;
		 angle = 0;
		 yadj = NA_REAL; */
	    /* The following line is needed for symmetry with plain text
	       but changes existing output */
	    line += (1/gpptr(dd)->mex)*dd->dev->yLineBias;
	    angle = 0;
	}
	break;
    case 4:
	if(las == 1 || las == 2) {
	    angle = 0;
	}
	else {
	    /*   line = line + 1 - gpptr(dd)->yLineBias;
		 angle = 90;
		 yadj = NA_REAL; */
	    line += (1/gpptr(dd)->mex)*(1 - dd->dev->yLineBias);
	    angle = 90;
	}
	break;
    }
    GMathText(at, line, coords, str, xadj, yadj, angle, dd);
}/* GMMathText */

/* -------------------- end of code from plotmath ------------- */
