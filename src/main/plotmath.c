/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997 Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2014 The R Core Team
 *
 *  This source code module:
 *  Copyright (C) 1997, 1998 Paul Murrell and Ross Ihaka
 *  Copyright (C) 1998-2014 The R Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <Defn.h>

#include <ctype.h>
#include <rlocale.h>


#include <Rmath.h> // provides M_2PI
#include <R_ext/GraphicsEngine.h>


/*
 *  TeX Math Styles
 *
 *  The TeXBook, Appendix G, Page 441.
 *
 */

typedef enum {
    STYLE_SS1 = 1,
    STYLE_SS  = 2,
    STYLE_S1  = 3,
    STYLE_S   = 4,
    STYLE_T1  = 5,
    STYLE_T   = 6,
    STYLE_D1  = 7,
    STYLE_D   = 8
} STYLE;

typedef struct {
    unsigned int BoxColor;
    double BaseCex;
    double ReferenceX;
    double ReferenceY;
    double CurrentX;
    double CurrentY;
    double CurrentAngle;
    double CosAngle;
    double SinAngle;
    STYLE CurrentStyle;
} mathContext;

static GEUnit MetricUnit = GE_INCHES;

/* Font Definitions */

typedef enum {
    PlainFont	   = 1,
    BoldFont	   = 2,
    ItalicFont	   = 3,
    BoldItalicFont = 4,
    SymbolFont	   = 5
} FontType;

/*
 *  Italic Correction Factor
 *
 *  The correction for a character is computed as ItalicFactor
 *  times the height (above the baseline) of the character's
 *  bounding box.
 *
 */

static double ItalicFactor = 0.15;

/* Drawing basics */


/* Convert CurrentX and CurrentY from */
/* 0 angle to and CurrentAngle */

static double ConvertedX(mathContext *mc, pGEDevDesc dd)
{
    double rotatedX = mc->ReferenceX +
	(mc->CurrentX - mc->ReferenceX) * mc->CosAngle -
	(mc->CurrentY - mc->ReferenceY) * mc->SinAngle;
    return toDeviceX(rotatedX, MetricUnit, dd);
}

static double ConvertedY(mathContext *mc, pGEDevDesc dd)
{
    double rotatedY = mc->ReferenceY +
	(mc->CurrentY - mc->ReferenceY) * mc->CosAngle +
	(mc->CurrentX - mc->ReferenceX) * mc->SinAngle;
    return toDeviceY(rotatedY, MetricUnit, dd);
}

static void PMoveAcross(double xamount, mathContext *mc)
{
    mc->CurrentX += xamount;
}

static void PMoveUp(double yamount, mathContext *mc)
{
    mc->CurrentY += yamount;
}

static void PMoveTo(double x, double y, mathContext *mc)
{
    mc->CurrentX = x;
    mc->CurrentY = y;
}

/* Basic Font Properties */

static double xHeight(pGEcontext gc, pGEDevDesc dd)
{
    double height, depth, width;
    GEMetricInfo('x', gc, &height, &depth, &width, dd);
    return fromDeviceHeight(height, MetricUnit, dd);
}

static double XHeight(pGEcontext gc, pGEDevDesc dd)
{
    double height, depth, width;
    GEMetricInfo('X', gc, &height, &depth, &width, dd);
    return fromDeviceHeight(height, MetricUnit, dd);
}

static double AxisHeight(pGEcontext gc, pGEDevDesc dd)
{
    double height, depth, width;
    GEMetricInfo('+', gc, &height, &depth, &width, dd);
    return fromDeviceHeight(0.5 * height, MetricUnit, dd);
}

static double Quad(pGEcontext gc, pGEDevDesc dd)
{
    double height, depth, width;
    GEMetricInfo('M', gc, &height, &depth, &width, dd);
    return fromDeviceHeight(width, MetricUnit, dd);
}

/* The height of digits */
static double FigHeight(pGEcontext gc, pGEDevDesc dd)
{
    double height, depth, width;
    GEMetricInfo('0', gc, &height, &depth, &width, dd);
    return fromDeviceHeight(height, MetricUnit, dd);
}

/* Depth of lower case descenders */
static double DescDepth(pGEcontext gc, pGEDevDesc dd)
{
    double height, depth, width;
    GEMetricInfo('g', gc, &height, &depth, &width, dd);
    return fromDeviceHeight(depth, MetricUnit, dd);
}

/* Thickness of rules */
static double RuleThickness(void)
{
    return 0.015;
}

static double ThinSpace(pGEcontext gc, pGEDevDesc dd)
{
    double height, depth, width;
    static double OneSixth = 0.16666666666666666666;
    GEMetricInfo('M', gc, &height, &depth, &width, dd);
    return fromDeviceHeight(OneSixth * width, MetricUnit, dd);
}

static double MediumSpace(pGEcontext gc, pGEDevDesc dd)
{
    double height, depth, width;
    static double TwoNinths = 0.22222222222222222222;
    GEMetricInfo('M', gc, &height, &depth, &width, dd);
    return fromDeviceHeight(TwoNinths * width, MetricUnit, dd);
}

static double ThickSpace(pGEcontext gc, pGEDevDesc dd)
{
    double height, depth, width;
    static double FiveEighteenths = 0.27777777777777777777;
    GEMetricInfo('M', gc, &height, &depth, &width, dd);
    return fromDeviceHeight(FiveEighteenths * width, MetricUnit, dd);
}

static double MuSpace(pGEcontext gc, pGEDevDesc dd)
{
    double height, depth, width;
    static double OneEighteenth = 0.05555555555555555555;
    GEMetricInfo('M', gc, &height, &depth, &width, dd);
    return fromDeviceHeight(OneEighteenth * width, MetricUnit, dd);
}


/*
 *  Mathematics Layout Parameters
 *
 *  The TeXBook, Appendix G, Page 447.
 *
 *  These values are based on an inspection of TeX metafont files
 *  together with some visual simplification.
 *
 *  Note : The values are ``optimised'' for PostScript.
 *
 */

typedef enum {
    sigma2,  sigma5,  sigma6,  sigma8,	sigma9,	 sigma10, sigma11,
    sigma12, sigma13, sigma14, sigma15, sigma16, sigma17, sigma18,
    sigma19, sigma20, sigma21, sigma22, xi8, xi9, xi10, xi11, xi12, xi13
}
TEXPAR;

#define SUBS	       0.7

static double TeX(TEXPAR which, pGEcontext gc, pGEDevDesc dd)
{
    switch(which) {
    case sigma2:  /* space */
    case sigma5:  /* x_height */
	return xHeight(gc, dd);

    case sigma6:  /* quad */
	return Quad(gc, dd);

    case sigma8:  /* num1 */
	return AxisHeight(gc, dd)
	    + 3.51 * RuleThickness()
	    + 0.15 * XHeight(gc, dd)		/* 54/36 * 0.1 */
	    + SUBS * DescDepth(gc, dd);
    case sigma9:  /* num2 */
	return AxisHeight(gc, dd)
	    + 1.51 * RuleThickness()
	    + 0.08333333 * XHeight(gc, dd);	/* 30/36 * 0.1 */
    case sigma10: /* num3 */
	return AxisHeight(gc, dd)
	    + 1.51 * RuleThickness()
	    + 0.1333333 * XHeight(gc, dd);	/* 48/36 * 0.1 */
    case sigma11: /* denom1 */
	return	- AxisHeight(gc, dd)
	    + 3.51 * RuleThickness()
	    + SUBS * FigHeight(gc, dd)
	    + 0.344444 * XHeight(gc, dd);	/* 124/36 * 0.1 */
    case sigma12: /* denom2 */
	return	- AxisHeight(gc, dd)
	    + 1.51 * RuleThickness()
	    + SUBS * FigHeight(gc, dd)
	    + 0.08333333 * XHeight(gc, dd);	/* 30/36 * 0.1 */

    case sigma13: /* sup1 */
	return 0.95 * xHeight(gc, dd);
    case sigma14: /* sup2 */
	return 0.825 * xHeight(gc, dd);
    case sigma15: /* sup3 */
	return 0.7 * xHeight(gc, dd);

    case sigma16: /* sub1 */
	return 0.35 * xHeight(gc, dd);
    case sigma17: /* sub2 */
	return 0.45 * XHeight(gc, dd);

    case sigma18: /* sup_drop */
	return 0.3861111 * XHeight(gc, dd);

    case sigma19: /* sub_drop */
	return 0.05 * XHeight(gc, dd);

    case sigma20: /* delim1 */
	return 2.39 * XHeight(gc, dd);
    case sigma21: /* delim2 */
	return 1.01 *XHeight(gc, dd);

    case sigma22: /* axis_height */
	return AxisHeight(gc, dd);

    case xi8:	  /* default_rule_thickness */
	return RuleThickness();

    case xi9:	  /* big_op_spacing1 */
    case xi10:	  /* big_op_spacing2 */
    case xi11:	  /* big_op_spacing3 */
    case xi12:	  /* big_op_spacing4 */
    case xi13:	  /* big_op_spacing5 */
	return 0.15 * XHeight(gc, dd);
    default:/* never happens (enum type) */
	error("invalid `which' in C function TeX"); return 0;/*-Wall*/
    }
}

static STYLE GetStyle(mathContext *mc)
{
    return mc->CurrentStyle;
}

static void SetStyle(STYLE newstyle, mathContext *mc, pGEcontext gc)
{
    switch (newstyle) {
    case STYLE_D:
    case STYLE_T:
    case STYLE_D1:
    case STYLE_T1:
	gc->cex = 1.0 * mc->BaseCex;
	break;
    case STYLE_S:
    case STYLE_S1:
	gc->cex = 0.7 * mc->BaseCex;
	break;
    case STYLE_SS:
    case STYLE_SS1:
	gc->cex = 0.5 * mc->BaseCex;
	break;
    default:
	error(_("invalid math style encountered"));
    }
    mc->CurrentStyle = newstyle;
}

static void SetPrimeStyle(STYLE style, mathContext *mc, pGEcontext gc)
{
    switch (style) {
    case STYLE_D:
    case STYLE_D1:
	SetStyle(STYLE_D1, mc, gc);
	break;
    case STYLE_T:
    case STYLE_T1:
	SetStyle(STYLE_T1, mc, gc);
	break;
    case STYLE_S:
    case STYLE_S1:
	SetStyle(STYLE_S1, mc, gc);
	break;
    case STYLE_SS:
    case STYLE_SS1:
	SetStyle(STYLE_SS1, mc, gc);
	break;
    }
}

static void SetSupStyle(STYLE style, mathContext *mc, pGEcontext gc)
{
    switch (style) {
    case STYLE_D:
    case STYLE_T:
	SetStyle(STYLE_S, mc, gc);
	break;
    case STYLE_D1:
    case STYLE_T1:
	SetStyle(STYLE_S1, mc, gc);
	break;
    case STYLE_S:
    case STYLE_SS:
	SetStyle(STYLE_SS, mc, gc);
	break;
    case STYLE_S1:
    case STYLE_SS1:
	SetStyle(STYLE_SS1, mc, gc);
	break;
    }
}

static void SetSubStyle(STYLE style, mathContext *mc, pGEcontext gc)
{
    switch (style) {
    case STYLE_D:
    case STYLE_T:
    case STYLE_D1:
    case STYLE_T1:
	SetStyle(STYLE_S1, mc, gc);
	break;
    case STYLE_S:
    case STYLE_SS:
    case STYLE_S1:
    case STYLE_SS1:
	SetStyle(STYLE_SS1, mc, gc);
	break;
    }
}

static void SetNumStyle(STYLE style, mathContext *mc, pGEcontext gc)
{
    switch (style) {
    case STYLE_D:
	SetStyle(STYLE_T, mc, gc);
	break;
    case STYLE_D1:
	SetStyle(STYLE_T1, mc, gc);
	break;
    default:
	SetSupStyle(style, mc, gc);
    }
}

static void SetDenomStyle(STYLE style, mathContext *mc, pGEcontext gc)
{
    if (style > STYLE_T)
	SetStyle(STYLE_T1, mc, gc);
    else
	SetSubStyle(style, mc, gc);
}

static int IsCompactStyle(STYLE style, mathContext *mc, pGEcontext gc)
{
    switch (style) {
    case STYLE_D1:
    case STYLE_T1:
    case STYLE_S1:
    case STYLE_SS1:
	return 1;
    default:
	return 0;
    }
}


#ifdef max
#undef max
#endif
/* Return maximum of two doubles. */
static double max(double x, double y)
{
    if (x > y) return x;
    else return y;
}


/* Bounding Boxes */
/* These including italic corrections and an */
/* indication of whether the nucleus was simple. */

typedef struct {
    double height;
    double depth;
    double width;
    double italic;
    int simple;
} BBOX;


#define bboxHeight(bbox) bbox.height
#define bboxDepth(bbox) bbox.depth
#define bboxWidth(bbox) bbox.width
#define bboxItalic(bbox) bbox.italic
#define bboxSimple(bbox) bbox.simple


static BBOX MakeBBox(double height, double depth, double width)
{
    BBOX bbox;
    bboxHeight(bbox) = height;
    bboxDepth(bbox)  = depth;
    bboxWidth(bbox)  = width;
    bboxItalic(bbox) = 0;
    bboxSimple(bbox) = 0;
    return bbox;
}

static BBOX NullBBox(void)
{
    BBOX bbox;
    bboxHeight(bbox) = 0;
    bboxDepth(bbox)  = 0;
    bboxWidth(bbox)  = 0;
    bboxItalic(bbox) = 0;
    bboxSimple(bbox) = 0;
    return bbox;
}

static BBOX ShiftBBox(BBOX bbox1, double shiftV)
{
    bboxHeight(bbox1) = bboxHeight(bbox1) + shiftV;
    bboxDepth(bbox1)  = bboxDepth(bbox1) - shiftV;
    bboxWidth(bbox1)  = bboxWidth(bbox1);
    bboxItalic(bbox1) = bboxItalic(bbox1);
    bboxSimple(bbox1) = bboxSimple(bbox1);
    return bbox1;
}

static BBOX EnlargeBBox(BBOX bbox, double deltaHeight, double deltaDepth,
		      double deltaWidth)
{
    bboxHeight(bbox) += deltaHeight;
    bboxDepth(bbox)  += deltaDepth;
    bboxWidth(bbox)  += deltaWidth;
    return bbox;
}

static BBOX CombineBBoxes(BBOX bbox1, BBOX bbox2)
{
    bboxHeight(bbox1) = max(bboxHeight(bbox1), bboxHeight(bbox2));
    bboxDepth(bbox1)  = max(bboxDepth(bbox1), bboxDepth(bbox2));
    bboxWidth(bbox1)  = bboxWidth(bbox1) + bboxWidth(bbox2);
    bboxItalic(bbox1) = bboxItalic(bbox2);
    bboxSimple(bbox1) = bboxSimple(bbox2);
    return bbox1;
}

static BBOX CombineAlignedBBoxes(BBOX bbox1, BBOX bbox2)
{
    bboxHeight(bbox1) = max(bboxHeight(bbox1), bboxHeight(bbox2));
    bboxDepth(bbox1)  = max(bboxDepth(bbox1), bboxDepth(bbox2));
    bboxWidth(bbox1)  = max(bboxWidth(bbox1), bboxWidth(bbox2));
    bboxItalic(bbox1) = 0;
    bboxSimple(bbox1) = 0;
    return bbox1;
}

static BBOX CombineOffsetBBoxes(BBOX bbox1, int italic1,
				BBOX bbox2, int italic2,
				double xoffset,
				double yoffset)
{
    double width1 = bboxWidth(bbox1) + (italic1 ? bboxItalic(bbox1) : 0);
    double width2 = bboxWidth(bbox2) + (italic2 ? bboxItalic(bbox2) : 0);
    bboxWidth(bbox1) = max(width1, width2 + xoffset);
    bboxHeight(bbox1) = max(bboxHeight(bbox1), bboxHeight(bbox2) + yoffset);
    bboxDepth(bbox1) = max(bboxDepth(bbox1), bboxDepth(bbox2) - yoffset);
    bboxItalic(bbox1) = 0;
    bboxSimple(bbox1) = 0;
    return bbox1;
}

static double CenterShift(BBOX bbox)
{
    return 0.5 * (bboxHeight(bbox) - bboxDepth(bbox));
}


typedef struct {
    char *name;
    int code;
} SymTab;

/* Determine a match between symbol name and string. */

static int NameMatch(SEXP expr, const char *aString)
{
    if (!isSymbol(expr)) return 0;
    return !strcmp(CHAR(PRINTNAME(expr)), aString);
}

static int StringMatch(SEXP expr, const char *aString)
{
    return !strcmp(translateChar(STRING_ELT(expr, 0)), aString);
}
/* Code to determine the ascii code corresponding */
/* to an element of a mathematical expression. */

#define A_HAT		  94
#define A_TILDE		 126

#define S_SPACE		  32
#define S_PARENLEFT	  40
#define S_PARENRIGHT	  41
#define S_ASTERISKMATH	  42
#define S_COMMA		  44
#define S_SLASH		  47
#define S_RADICALEX	  96
#define S_FRACTION	 164
#define S_ELLIPSIS	 188
#define S_INTERSECTION	 199
#define S_UNION		 200
#define S_PRODUCT	 213
#define S_RADICAL	 214
#define S_SUM		 229
#define S_INTEGRAL	 242
#define S_BRACKETLEFTTP	 233
#define S_BRACKETLEFTBT	 235
#define S_BRACKETRIGHTTP 249
#define S_BRACKETRIGHTBT 251

#define N_LIM		1001
#define N_LIMINF	1002
#define N_LIMSUP	1003
#define N_INF		1004
#define N_SUP		1005
#define N_MIN		1006
#define N_MAX		1007


/* The Full Adobe Symbol Font */

static SymTab
SymbolTable[] = {
    { "space",		 32 },
    { "exclam",		 33 },
    { "universal",	 34 },
    { "numbersign",	 35 },
    { "existential",	 36 },
    { "percent",	 37 },
    { "ampersand",	 38 },
    { "suchthat",	 39 },
    { "parenleft",	 40 },
    { "parenright",	 41 },
    { "asteriskmath",	 42 },
    { "plus",		 43 },
    { "comma",		 44 },
    { "minus",		 45 },
    { "period",		 46 },
    { "slash",		 47 },
    { "0",		 48 },
    { "1",		 49 },
    { "2",		 50 },
    { "3",		 51 },
    { "4",		 52 },
    { "5",		 53 },
    { "6",		 54 },
    { "7",		 55 },
    { "8",		 56 },
    { "9",		 57 },
    { "colon",		 58 },
    { "semicolon",	 59 },
    { "less",		 60 },
    { "equal",		 61 },
    { "greater",	 62 },
    { "question",	 63 },
    { "congruent",	 64 },

    { "Alpha",/* 0101= */65 }, /* Upper Case Greek Characters */
    { "Beta",		 66 },
    { "Chi",		 67 },
    { "Delta",		 68 },
    { "Epsilon",	 69 },
    { "Phi",		 70 },
    { "Gamma",		 71 },
    { "Eta",		 72 },
    { "Iota",		 73 },
    { "theta1",		 74 },
    { "vartheta",	 74 },
    { "Kappa",		 75 },
    { "Lambda",		 76 },
    { "Mu",		 77 },
    { "Nu",		 78 },
    { "Omicron",	 79 },
    { "Pi",		 80 },
    { "Theta",		 81 },
    { "Rho",		 82 },
    { "Sigma",		 83 },
    { "Tau",		 84 },
    { "Upsilon",	 85 },
    { "sigma1",		 86 },
    { "varsigma",	 86 },
    { "stigma",		 86 },
    { "Omega",		 87 },
    { "Xi",		 88 },
    { "Psi",		 89 },
    { "Zeta",/* 0132 = */90 },

    { "bracketleft",	 91 },	/* Miscellaneous Special Characters */
    { "therefore",	 92 },
    { "bracketright",	 93 },
    { "perpendicular",	 94 },
    { "underscore",	 95 },
    { "radicalex",	 96 },

    { "alpha",/* 0141= */97 },	/* Lower Case Greek Characters */
    { "beta",		 98 },
    { "chi",		 99 },
    { "delta",		100 },
    { "epsilon",	101 },
    { "phi",		102 },
    { "gamma",		103 },
    { "eta",		104 },
    { "iota",		105 },
    { "phi1",		106 },
    { "varphi",		106 },
    { "kappa",		107 },
    { "lambda",		108 },
    { "mu",		109 },
    { "nu",		110 },
    { "omicron",	111 },
    { "pi",		112 },
    { "theta",		113 },
    { "rho",		114 },
    { "sigma",		115 },
    { "tau",		116 },
    { "upsilon",	117 },
    { "omega1",		118 },
    { "omega",		119 },
    { "xi",		120 },
    { "psi",		121 },
    { "zeta",/* 0172= */122 },

    { "braceleft",	123 },	/* Miscellaneous Special Characters */
    { "bar",		124 },
    { "braceright",	125 },
    { "similar",	126 },

    { "Upsilon1",	161 },	/* Lone Greek */
    { "minute",		162 },
    { "lessequal",	163 },
    { "fraction",	164 },
    { "infinity",	165 },
    { "florin",		166 },
    { "club",		167 },
    { "diamond",	168 },
    { "heart",		169 },
    { "spade",		170 },
    { "arrowboth",	171 },
    { "arrowleft",	172 },
    { "arrowup",	173 },
    { "arrowright",	174 },
    { "arrowdown",	175 },
    { "degree",		176 },
    { "plusminus",	177 },
    { "second",		178 },
    { "greaterequal",	179 },
    { "multiply",	180 },
    { "proportional",	181 },
    { "partialdiff",	182 },
    { "bullet",		183 },
    { "divide",		184 },
    { "notequal",	185 },
    { "equivalence",	186 },
    { "approxequal",	187 },
    { "ellipsis",	188 },
    { "arrowvertex",	189 },
    { "arrowhorizex",	190 },
    { "carriagereturn", 191 },
    { "aleph",		192 },
    { "Ifraktur",	193 },
    { "Rfraktur",	194 },
    { "weierstrass",	195 },
    { "circlemultiply", 196 },
    { "circleplus",	197 },
    { "emptyset",	198 },
    { "intersection",	199 },/* = 0307 */
    { "union",		200 },/* = 0310 */
    { "propersuperset", 201 },
    { "reflexsuperset", 202 },
    { "notsubset",	203 },
    { "propersubset",	204 },
    { "reflexsubset",	205 },
    { "element",	206 },
    { "notelement",	207 },
    { "angle",		208 },
    { "nabla",		209 },/* = 0321, Adobe name 'gradient' */
    { "registerserif",	210 },
    { "copyrightserif", 211 },
    { "trademarkserif", 212 },
    { "product",	213 },
    { "radical",	214 },
    { "dotmath",	215 },
    { "logicaland",	217 },
    { "logicalor",	218 },
    { "arrowdblboth",	219 },
    { "arrowdblleft",	220 },
    { "arrowdblup",	221 },
    { "arrowdblright",	222 },
    { "arrowdbldown",	223 },
    { "lozenge",	224 },
    { "angleleft",	225 },
    { "registersans",	226 },
    { "copyrightsans",	227 },
    { "trademarksans",	228 },
    { "summation",	229 },
    { "parenlefttp",	230 },
    { "parenleftex",	231 },
    { "parenleftbt",	232 },
    { "bracketlefttp",	233 },
    { "bracketleftex",	234 },
    { "bracketleftbt",	235 },
    { "bracelefttp",	236 },
    { "braceleftmid",	237 },
    { "braceleftbt",	238 },
    { "braceex",	239 },
    { "angleright",	241 },
    { "integral",	242 },
    { "integraltp",	243 },
    { "integralex",	244 },
    { "integralbt",	245 },
    { "parenrighttp",	246 },
    { "parenrightex",	247 },
    { "parenrightbt",	248 },
    { "bracketrighttp", 249 },
    { "bracketrightex", 250 },
    { "bracketrightbt", 251 },
    { "bracerighttp",	252 },
    { "bracerightmid",	253 },
    { "bracerightbt",	254 },

    { NULL,		  0 },
};

static int SymbolCode(SEXP expr)
{
    int i;
    for (i = 0; SymbolTable[i].code; i++)
	if (NameMatch(expr, SymbolTable[i].name))
	    return SymbolTable[i].code;
    return 0;
}

/* this is the one really used: */
static int TranslatedSymbol(SEXP expr)
{
    int code = SymbolCode(expr);
    if ((0101 <= code && code <= 0132)	||   /* l/c Greek */
	(0141 <= code && code <= 0172)	||   /* u/c Greek */
	code == 0300			||   /* aleph */
	code == 0241			||   /* Upsilon1 */
	code == 0242			||   /* minute */
	code == 0245			||   /* infinity */
	code == 0260			||   /* degree */
	code == 0262			||   /* second */
	code == 0266                    ||   /* partialdiff */
	code == 0321                    ||   /* nabla */
	0)
	return code;
    else
	return 0;
}

/* Code to determine the nature of an expression. */

static int FormulaExpression(SEXP expr)
{
    return (TYPEOF(expr) == LANGSXP);
}

static int NameAtom(SEXP expr)
{
    return (TYPEOF(expr) == SYMSXP);
}

static int NumberAtom(SEXP expr)
{
    return ((TYPEOF(expr) == REALSXP) ||
	    (TYPEOF(expr) == INTSXP)  ||
	    (TYPEOF(expr) == CPLXSXP));
}

static int StringAtom(SEXP expr)
{
    return (TYPEOF(expr) == STRSXP);
}

/* Code to determine a font from the */
/* nature of the expression */

static FontType GetFont(pGEcontext gc)
{
    return gc->fontface;
}

static FontType SetFont(FontType font, pGEcontext gc)
{
    FontType prevfont = gc->fontface;
    gc->fontface = font;
    return prevfont;
}

static int UsingItalics(pGEcontext gc)
{
    return (gc->fontface == ItalicFont ||
	    gc->fontface == BoldItalicFont);
}

static BBOX GlyphBBox(int chr, pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox;
    double height, depth, width;
    int chr1 = chr;
    if(dd->dev->wantSymbolUTF8 && gc->fontface == 5)
	chr1 = -Rf_AdobeSymbol2ucs2(chr);
    GEMetricInfo(chr1, gc, &height, &depth, &width, dd);
    bboxHeight(bbox) = fromDeviceHeight(height, MetricUnit, dd);
    bboxDepth(bbox)  = fromDeviceHeight(depth, MetricUnit, dd);
    bboxWidth(bbox)  = fromDeviceHeight(width, MetricUnit, dd);
    bboxItalic(bbox) = 0;
    bboxSimple(bbox) = 1;
    return bbox;
}

static BBOX RenderElement(SEXP, int, mathContext*, pGEcontext , pGEDevDesc);
static BBOX RenderOffsetElement(SEXP, double, double, int,
				mathContext*, pGEcontext , pGEDevDesc);
static BBOX RenderExpression(SEXP, int, mathContext*, pGEcontext , pGEDevDesc);
static BBOX RenderSymbolChar(int, int, mathContext*, pGEcontext , pGEDevDesc);


/*  Code to Generate Bounding Boxes and Draw Formulae.	*/

static BBOX RenderItalicCorr(BBOX bbox, int draw, mathContext *mc,
			     pGEcontext gc, pGEDevDesc dd)
{
    if (bboxItalic(bbox) > 0) {
	if (draw)
	    PMoveAcross(bboxItalic(bbox), mc);
	bboxWidth(bbox) += bboxItalic(bbox);
	bboxItalic(bbox) = 0;
    }
    return bbox;
}

static BBOX RenderGap(double gap, int draw, mathContext *mc,
		      pGEcontext gc, pGEDevDesc dd)
{
    if (draw)
	PMoveAcross(gap, mc);
    return MakeBBox(0, 0, gap);
}

/* Draw a Symbol from the Special Font:
   this is assumed to be 8-bit encoded in Adobe Symbol.
 */

static BBOX RenderSymbolChar(int ascii, int draw, mathContext *mc,
			     pGEcontext gc, pGEDevDesc dd)
{
    FontType prev;
    BBOX bbox;
    char asciiStr[2];
    if (ascii == A_HAT || ascii == A_TILDE)
	prev = SetFont(PlainFont, gc);
    else
	prev = SetFont(SymbolFont, gc);
    bbox = GlyphBBox(ascii, gc, dd);
    if (draw) {
	asciiStr[0] = (char) ascii;
	asciiStr[1] = '\0';
	GEText(ConvertedX(mc ,dd), ConvertedY(mc, dd), asciiStr,
	       CE_SYMBOL,
	       0.0, 0.0, mc->CurrentAngle, gc,
	       dd);
	PMoveAcross(bboxWidth(bbox), mc);
    }
    SetFont(prev, gc);
    return bbox;
}

/* Draw a Symbol String in "Math Mode" */
/* This code inserts italic corrections after */
/* every character. */

static BBOX RenderSymbolStr(const char *str, int draw, mathContext *mc,
			    pGEcontext gc, pGEDevDesc dd)
{
    char chr[7] = "";
    const char *s = str;
    BBOX glyphBBox;
    BBOX resultBBox = NullBBox();
    double lastItalicCorr = 0;
    FontType prevfont = GetFont(gc);
    FontType font = prevfont;

    if (str) {
	/* Need to advance by character, not byte, except in the symbol font.
	   The latter would be hard to achieve, but perhaps not impossible.
	 */
	if(mbcslocale && gc->fontface != 5) {
	    wchar_t wc;
	    mbstate_t mb_st;
	    size_t res;

	    mbs_init(&mb_st);
	    while (*s) {
		wc = 0;
		res = mbrtowc(&wc, s, MB_LEN_MAX, &mb_st);
		if(res == -1) error("invalid multibyte string '%s'", s);
		if (iswdigit(wc) && font != PlainFont) {
		    font = PlainFont;
		    SetFont(PlainFont, gc);
		}
		else if (font != prevfont) {
		    font = prevfont;
		    SetFont(prevfont, gc);
		}
		glyphBBox = GlyphBBox((unsigned int) wc, gc, dd);
		if (UsingItalics(gc))
		    bboxItalic(glyphBBox) =
			ItalicFactor * bboxHeight(glyphBBox);
		else
		    bboxItalic(glyphBBox) = 0;
		if (draw) {
		    memset(chr, 0, sizeof(chr));
		    /* should not be possible, as we just converted to wc */
		    if(wcrtomb(chr, wc, &mb_st) == -1)
			error("invalid multibyte string");
		    PMoveAcross(lastItalicCorr, mc);
		    GEText(ConvertedX(mc ,dd), ConvertedY(mc, dd), chr,
			   CE_NATIVE,
			   0.0, 0.0, mc->CurrentAngle, gc, dd);
		    PMoveAcross(bboxWidth(glyphBBox), mc);
		}
		bboxWidth(resultBBox) += lastItalicCorr;
		resultBBox = CombineBBoxes(resultBBox, glyphBBox);
		lastItalicCorr = bboxItalic(glyphBBox);
		s += res;
	    }
	} else {
	    while (*s) {
		if (isdigit((int)*s) && font != PlainFont) {
		    font = PlainFont;
		    SetFont(PlainFont, gc);
		}
		else if (font != prevfont) {
		    font = prevfont;
		    SetFont(prevfont, gc);
		}
		glyphBBox = GlyphBBox((unsigned char) *s, gc, dd);
		if (UsingItalics(gc))
		    bboxItalic(glyphBBox) =
			ItalicFactor * bboxHeight(glyphBBox);
		else
		    bboxItalic(glyphBBox) = 0;
		if (draw) {
		    chr[0] = *s;
		    PMoveAcross(lastItalicCorr, mc);
		    GEText(ConvertedX(mc ,dd), ConvertedY(mc, dd), chr,
			   CE_NATIVE,
			   0.0, 0.0, mc->CurrentAngle, gc, dd);
		    PMoveAcross(bboxWidth(glyphBBox), mc);
		}
		bboxWidth(resultBBox) += lastItalicCorr;
		resultBBox = CombineBBoxes(resultBBox, glyphBBox);
		lastItalicCorr = bboxItalic(glyphBBox);
		s++;
	    }
	}
	if (font != prevfont)
	    SetFont(prevfont, gc);
    }
    bboxSimple(resultBBox) = 1;
    return resultBBox;
}

/* Code for Character String Atoms. */

/* This only gets called from RenderAccent */
static BBOX RenderChar(int ascii, int draw, mathContext *mc,
		       pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox;
    char asciiStr[7];

    bbox = GlyphBBox(ascii, gc, dd);
    if (draw) {
	memset(asciiStr, 0, sizeof(asciiStr));
	if(mbcslocale) {
	    size_t res = wcrtomb(asciiStr, ascii, NULL);
	    if(res == -1)
		error("invalid character in current multibyte locale");
	} else
	    asciiStr[0] = (char) ascii;
	GEText(ConvertedX(mc ,dd), ConvertedY(mc, dd), asciiStr, CE_NATIVE,
	       0.0, 0.0, mc->CurrentAngle, gc,
	       dd);
	PMoveAcross(bboxWidth(bbox), mc);
    }
    return bbox;
}

/* This gets called on strings and PRINTNAMES */
static BBOX RenderStr(const char *str, int draw, mathContext *mc,
		      pGEcontext gc, pGEDevDesc dd)
{
    BBOX glyphBBox = NullBBox(); /* might be use do italic corr on str="" */
    BBOX resultBBox = NullBBox();
    int nc = 0;
    cetype_t enc = (gc->fontface == 5) ? CE_SYMBOL : CE_NATIVE;

    if (str) {
	/* need to advance by character, not byte, except in the symbol font */
	if(mbcslocale && gc->fontface != 5) {
	    size_t n = strlen(str), used;
	    wchar_t wc;
	    const char *p = str;
	    mbstate_t mb_st;
	    mbs_init(&mb_st);
	    while ((used = Mbrtowc(&wc, p, n, &mb_st)) > 0) {
		/* On Windows could have sign extension here */
		glyphBBox = GlyphBBox((unsigned int) wc, gc, dd);
		resultBBox = CombineBBoxes(resultBBox, glyphBBox);
		p += used; n -= used; nc++;
	    }
	} else {
	    const char *s = str;
	    while (*s) {
		/* Watch for sign extension here - fixed > 2.7.1 */
		glyphBBox = GlyphBBox((unsigned char) *s, gc, dd);
		resultBBox = CombineBBoxes(resultBBox, glyphBBox);
		s++; nc++;
	    }
	}
	if(nc > 1) {
	    /* Finding the width by adding up boxes is incorrect (kerning) */
	    double wd = GEStrWidth(str, enc, gc, dd);
	    bboxWidth(resultBBox) = fromDeviceHeight(wd, MetricUnit, dd);
	}
	if (draw) {
	    GEText(ConvertedX(mc ,dd), ConvertedY(mc, dd), str, enc,
		   0.0, 0.0, mc->CurrentAngle, gc, dd);
	    PMoveAcross(bboxWidth(resultBBox), mc);
	}
	if (UsingItalics(gc))
	    bboxItalic(resultBBox) = ItalicFactor * bboxHeight(glyphBBox);
	else
	    bboxItalic(resultBBox) = 0;
    }
    bboxSimple(resultBBox) = 1;
    return resultBBox;
}


/* Code for Symbol Font Atoms */

static BBOX RenderSymbol(SEXP expr, int draw, mathContext *mc,
			 pGEcontext gc, pGEDevDesc dd)
{
    int code;
    if ((code = TranslatedSymbol(expr)))
	return RenderSymbolChar(code, draw, mc, gc, dd);
    else
	return RenderSymbolStr(CHAR(PRINTNAME(expr)), draw, mc, gc, dd);
}

static BBOX RenderSymbolString(SEXP expr, int draw, mathContext *mc,
			       pGEcontext gc, pGEDevDesc dd)
{
    int code;
    if ((code = TranslatedSymbol(expr)))
	return RenderSymbolChar(code, draw, mc, gc, dd);
    else
	return RenderStr(CHAR(PRINTNAME(expr)), draw, mc, gc, dd);
}


/* Code for Numeric Atoms */

static BBOX RenderNumber(SEXP expr, int draw, mathContext *mc,
			 pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox;
    FontType prevfont = SetFont(PlainFont, gc);
    PrintDefaults();
    bbox = RenderStr(CHAR(asChar(expr)), draw, mc, gc, dd);
    SetFont(prevfont, gc);
    return bbox;
}

/* Code for String Atoms */

static BBOX RenderString(SEXP expr, int draw, mathContext *mc,
			 pGEcontext gc, pGEDevDesc dd)
{
    return RenderStr(translateChar(STRING_ELT(expr, 0)), draw, mc, gc, dd);
}

/* Code for Ellipsis (ldots, cdots, ...) */

static int DotsAtom(SEXP expr)
{
    if (NameMatch(expr, "cdots") ||
	NameMatch(expr, "...")	 ||
	NameMatch(expr, "ldots"))
	    return 1;
    return 0;
}

static BBOX RenderDots(SEXP expr, int draw, mathContext *mc,
		       pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox = RenderSymbolChar(S_ELLIPSIS, 0, mc, gc, dd);
    if (NameMatch(expr, "cdots") || NameMatch(expr, "...")) {
	double shift = AxisHeight(gc, dd) - 0.5 * bboxHeight(bbox);
	if (draw) {
	    PMoveUp(shift, mc);
	    RenderSymbolChar(S_ELLIPSIS, 1, mc, gc, dd);
	    PMoveUp(-shift, mc);
	}
	return ShiftBBox(bbox, shift);
    }
    else {
	if (draw)
	    RenderSymbolChar(S_ELLIPSIS, 1, mc, gc, dd);
	return bbox;
    }
}

/*----------------------------------------------------------------------
 *
 *  Code for Atoms
 *
 */

static BBOX RenderAtom(SEXP expr, int draw, mathContext *mc,
		       pGEcontext gc, pGEDevDesc dd)
{
    if (NameAtom(expr)) {
	if (DotsAtom(expr))
	    return RenderDots(expr, draw, mc, gc, dd);
	else
	    return RenderSymbol(expr, draw, mc, gc, dd);
    }
    else if (NumberAtom(expr))
	return RenderNumber(expr, draw, mc, gc, dd);
    else if (StringAtom(expr))
	return RenderString(expr, draw, mc, gc, dd);

    return NullBBox();		/* -Wall */
}


/*----------------------------------------------------------------------
 *
 *  Code for Binary / Unary Operators  (~, +, -, ... )
 *
 *  Note that there are unary and binary ~ s.
 *
 */

static int SpaceAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "~");
}


static BBOX RenderSpace(SEXP expr, int draw, mathContext *mc,
			pGEcontext gc, pGEDevDesc dd)
{

    BBOX opBBox, arg1BBox, arg2BBox;
    int nexpr = length(expr);

    if (nexpr == 2) {
	opBBox = RenderSymbolChar(' ', draw, mc, gc, dd);
	arg1BBox = RenderElement(CADR(expr), draw, mc, gc, dd);
	return CombineBBoxes(opBBox, arg1BBox);
    }
    else if (nexpr == 3) {
	arg1BBox = RenderElement(CADR(expr), draw, mc, gc, dd);
	opBBox = RenderSymbolChar(' ', draw, mc, gc, dd);
	arg2BBox = RenderElement(CADDR(expr), draw, mc, gc, dd);
	opBBox = CombineBBoxes(arg1BBox, opBBox);
	opBBox = CombineBBoxes(opBBox, arg2BBox);
	return opBBox;
    }
    else
	error(_("invalid mathematical annotation"));

    return NullBBox();		/* -Wall */
}

static SymTab BinTable[] = {
    { "*",		 052 },	/* Binary Operators */
    { "+",		 053 },
    { "-",		 055 },
    { "/",		 057 },
    { ":",		 072 },
    { "%+-%",		0261 },
    { "%*%",		0264 },
    { "%/%",		0270 },
    { "%intersection%", 0307 },
    { "%union%",	0310 },
    { "%.%",            0327 }, /* cdot or dotmath */
    { NULL,		   0 }
};

static int BinAtom(SEXP expr)
{
    int i;

    for (i = 0; BinTable[i].code; i++)
	if (NameMatch(expr, BinTable[i].name))
	    return BinTable[i].code;
    return 0;
}

static BBOX RenderSlash(int draw, mathContext *mc, pGEcontext gc,
			pGEDevDesc dd)
{
    /* Line Drawing Version */
    double x[2], y[2];
    double depth = 0.5 * TeX(sigma22, gc, dd);
    double height = XHeight(gc, dd) + 0.5 * TeX(sigma22, gc, dd);
    double width = 0.5 * xHeight(gc, dd);
    if (draw) {
	int savedlty = gc->lty;
	double savedlwd = gc->lwd;
	PMoveAcross(0.5 * width, mc);
	PMoveUp(-depth, mc);
	x[0] = ConvertedX(mc, dd);
	y[0] = ConvertedY(mc, dd);
	PMoveAcross(width, mc);
	PMoveUp(depth + height, mc);
	x[1] = ConvertedX(mc, dd);
	y[1] = ConvertedY(mc, dd);
	PMoveUp(-height, mc);
	gc->lty = LTY_SOLID;
	if (gc->lwd > 1)
	    gc->lwd = 1;
	GEPolyline(2, x, y, gc, dd);
	PMoveAcross(0.5 * width, mc);
	gc->lty = savedlty;
	gc->lwd = savedlwd;
    }
    return MakeBBox(height, depth, 2 * width);
}

static BBOX RenderBin(SEXP expr, int draw, mathContext *mc,
		      pGEcontext gc, pGEDevDesc dd)
{
    int op = BinAtom(CAR(expr));
    int nexpr = length(expr);
    BBOX bbox;
    double gap;

    if(nexpr == 3) {
	if (op == S_ASTERISKMATH) {
	    bbox = RenderElement(CADR(expr), draw, mc, gc, dd);
	    bbox = RenderItalicCorr(bbox, draw, mc, gc, dd);
	    return CombineBBoxes(bbox, RenderElement(CADDR(expr), draw,
						     mc, gc, dd));
	}
	else if (op == S_SLASH) {
	    gap = 0;
	    bbox = RenderElement(CADR(expr), draw, mc, gc, dd);
	    bbox = RenderItalicCorr(bbox, draw, mc, gc, dd);
	    bbox = CombineBBoxes(bbox, RenderGap(gap, draw, mc, gc, dd));
	    bbox = CombineBBoxes(bbox, RenderSlash(draw, mc, gc, dd));
	    bbox = CombineBBoxes(bbox, RenderGap(gap, draw, mc, gc, dd));
	    return CombineBBoxes(bbox, RenderElement(CADDR(expr), draw,
						     mc, gc, dd));
	}
	else {
	    gap = (mc->CurrentStyle > STYLE_S) ? MediumSpace(gc, dd) : 0;
	    bbox = RenderElement(CADR(expr), draw, mc, gc, dd);
	    bbox = RenderItalicCorr(bbox, draw, mc, gc, dd);
	    bbox = CombineBBoxes(bbox, RenderGap(gap, draw, mc, gc, dd));
	    bbox = CombineBBoxes(bbox, RenderSymbolChar(op, draw, mc, gc, dd));
	    bbox = CombineBBoxes(bbox, RenderGap(gap, draw, mc, gc, dd));
	    return CombineBBoxes(bbox, RenderElement(CADDR(expr), draw,
						     mc, gc, dd));
	}
    }
    else if(nexpr == 2) {
	gap = (mc->CurrentStyle > STYLE_S) ? ThinSpace(gc, dd) : 0;
	bbox = RenderSymbolChar(op, draw, mc, gc, dd);
	bbox = CombineBBoxes(bbox, RenderGap(gap, draw, mc, gc, dd));
	return CombineBBoxes(bbox, RenderElement(CADR(expr), draw, mc,
						 gc, dd));
    }
    else
	error(_("invalid mathematical annotation"));

    return NullBBox();		/* -Wall */

}


/*----------------------------------------------------------------------
 *
 *  Code for Subscript and Superscipt Expressions
 *
 *  Rules 18, 18a, ..., 18f of the TeXBook.
 *
 */

static int SuperAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "^");
}

static int SubAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "[");
}

/* Note : If all computations are correct */
/* We do not need to save and restore the */
/* current location here.  This is paranoia. */
static BBOX RenderSub(SEXP expr, int draw, mathContext *mc,
		      pGEcontext gc, pGEDevDesc dd)
{
    BBOX bodyBBox, subBBox;
    SEXP body = CADR(expr);
    SEXP sub = CADDR(expr);
    STYLE style = GetStyle(mc);
    double savedX = mc->CurrentX;
    double savedY = mc->CurrentY;
    double v, s16;
    bodyBBox = RenderElement(body, draw, mc, gc, dd);
    bodyBBox = RenderItalicCorr(bodyBBox, draw, mc, gc, dd);
    v = bboxSimple(bodyBBox) ? 0 : bboxDepth(bodyBBox) + TeX(sigma19, gc, dd);
    s16 = TeX(sigma16, gc, dd);
    SetSubStyle(style, mc, gc);
    subBBox = RenderElement(sub, 0, mc, gc, dd);
    v = max(max(v, s16), bboxHeight(subBBox) - 0.8 * sigma5);
    subBBox = RenderOffsetElement(sub, 0, -v, draw, mc, gc, dd);
    bodyBBox = CombineBBoxes(bodyBBox, subBBox);
    SetStyle(style, mc, gc);
    if (draw)
	PMoveTo(savedX + bboxWidth(bodyBBox), savedY, mc);
    return bodyBBox;
}

static BBOX RenderSup(SEXP expr, int draw, mathContext *mc,
		      pGEcontext gc, pGEDevDesc dd)
{
    BBOX bodyBBox, subBBox, supBBox;
    SEXP body = CADR(expr);
    SEXP sup = CADDR(expr);
    SEXP sub = R_NilValue;	/* -Wall */
    STYLE style = GetStyle(mc);
    double savedX = mc->CurrentX;
    double savedY = mc->CurrentY;
    double theta, delta, width;
    double u, p;
    double v, s5, s17;
    int haveSub;
    if (FormulaExpression(body) && SubAtom(CAR(body))) {
	sub = CADDR(body);
	body = CADR(body);
	haveSub = 1;
    }
    else haveSub = 0;
    bodyBBox = RenderElement(body, draw, mc, gc, dd);
    delta = bboxItalic(bodyBBox);
    bodyBBox = RenderItalicCorr(bodyBBox, draw, mc, gc, dd);
    width = bboxWidth(bodyBBox);
    if (bboxSimple(bodyBBox)) {
	u = 0;
	v = 0;
    }
    else {
	u = bboxHeight(bodyBBox) - TeX(sigma18, gc, dd);
	v = bboxDepth(bodyBBox) + TeX(sigma19, gc, dd);
    }
    theta = TeX(xi8, gc, dd);
    s5 = TeX(sigma5, gc, dd);
    s17 = TeX(sigma17, gc, dd);
    if (style == STYLE_D)
	p = TeX(sigma13, gc, dd);
    else if (IsCompactStyle(style, mc, gc))
	p = TeX(sigma15, gc, dd);
    else
	p = TeX(sigma14, gc, dd);
    SetSupStyle(style, mc, gc);
    supBBox = RenderElement(sup, 0, mc, gc, dd);
    u = max(max(u, p), bboxDepth(supBBox) + 0.25 * s5);

    if (haveSub) {
	SetSubStyle(style, mc, gc);
	subBBox = RenderElement(sub, 0, mc, gc, dd);
	v = max(v, s17);
	if ((u - bboxDepth(supBBox)) - (bboxHeight(subBBox) - v) < 4 * theta) {
	    double psi = 0.8 * s5 - (u - bboxDepth(supBBox));
	    if (psi > 0) {
		u += psi;
		v -= psi;
	    }
	}
	if (draw)
	    PMoveTo(savedX, savedY, mc);
	subBBox = RenderOffsetElement(sub, width, -v, draw, mc, gc, dd);
	if (draw)
	    PMoveTo(savedX, savedY, mc);
	SetSupStyle(style, mc, gc);
	supBBox = RenderOffsetElement(sup, width + delta, u, draw, mc, gc, dd);
	bodyBBox = CombineAlignedBBoxes(bodyBBox, subBBox);
	bodyBBox = CombineAlignedBBoxes(bodyBBox, supBBox);
    }
    else {
	supBBox = RenderOffsetElement(sup, 0, u, draw, mc, gc, dd);
	bodyBBox = CombineBBoxes(bodyBBox, supBBox);
    }
    if (draw)
	PMoveTo(savedX + bboxWidth(bodyBBox), savedY, mc);
    SetStyle(style, mc, gc);
    return bodyBBox;
}


/*----------------------------------------------------------------------
 *
 *  Code for Accented Expressions (widehat, bar, widetilde, ...)
 *
 */

#define ACCENT_GAP  0.2
#define HAT_HEIGHT  0.3

#define NTILDE	    8
#define DELTA	    0.05

static int WideTildeAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "widetilde");
}

static BBOX RenderWideTilde(SEXP expr, int draw, mathContext *mc,
			    pGEcontext gc, pGEDevDesc dd)
{
    double savedX = mc->CurrentX;
    double savedY = mc->CurrentY;
    BBOX bbox = RenderElement(CADR(expr), draw, mc, gc, dd);
    double height = bboxHeight(bbox);
    /*double width = bboxWidth(bbox);*/
    double totalwidth = bboxWidth(bbox) + bboxItalic(bbox);
    double delta = totalwidth * (1 - 2 * DELTA) / NTILDE;
    double start = DELTA * totalwidth;
    double accentGap = ACCENT_GAP * XHeight(gc, dd);
    double hatHeight = 0.5 * HAT_HEIGHT * XHeight(gc, dd);
    double c = M_2PI / NTILDE;
    double x[NTILDE + 3], y[NTILDE + 3];
    double baseX, baseY, xval, yval;
    int i;

    if (draw) {
	int savedlty = gc->lty;
	double savedlwd = gc->lwd;
	baseX = savedX;
	baseY = savedY + height + accentGap;
	PMoveTo(baseX, baseY, mc);
	x[0] = ConvertedX(mc, dd);
	y[0] = ConvertedY(mc, dd);
	for (i = 0; i <= NTILDE; i++) {
	    xval = start + i * delta;
	    yval = 0.5 * hatHeight * (sin(c * i) + 1);
	    PMoveTo(baseX + xval, baseY + yval, mc);
	    x[i + 1] = ConvertedX(mc, dd);
	    y[i + 1] = ConvertedY(mc, dd);
	}
	PMoveTo(baseX + totalwidth, baseY + hatHeight, mc);
	x[NTILDE + 2] = ConvertedX(mc, dd);
	y[NTILDE + 2] = ConvertedY(mc, dd);
	gc->lty = LTY_SOLID;
	if (gc->lwd > 1)
	    gc->lwd = 1;
	GEPolyline(NTILDE + 3, x, y, gc, dd);
	PMoveTo(savedX + totalwidth, savedY, mc);
	gc->lty = savedlty;
	gc->lwd = savedlwd;
    }
    return MakeBBox(height + accentGap + hatHeight,
		    bboxDepth(bbox), totalwidth);
}

static int WideHatAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "widehat");
}

static BBOX RenderWideHat(SEXP expr, int draw, mathContext *mc,
			  pGEcontext gc, pGEDevDesc dd)
{
    double savedX = mc->CurrentX;
    double savedY = mc->CurrentY;
    BBOX bbox = RenderElement(CADR(expr), draw, mc, gc, dd);
    double accentGap = ACCENT_GAP * XHeight(gc, dd);
    double hatHeight = HAT_HEIGHT * XHeight(gc, dd);
    double totalwidth = bboxWidth(bbox) + bboxItalic(bbox);
    double height = bboxHeight(bbox);
    double width = bboxWidth(bbox);
    double x[3], y[3];

    if (draw) {
	int savedlty = gc->lty;
	double savedlwd = gc->lwd;
	PMoveTo(savedX, savedY + height + accentGap, mc);
	x[0] = ConvertedX(mc, dd);
	y[0] = ConvertedY(mc, dd);
	PMoveAcross(0.5 * totalwidth, mc);
	PMoveUp(hatHeight, mc);
	x[1] = ConvertedX(mc, dd);
	y[1] = ConvertedY(mc, dd);
	PMoveAcross(0.5 * totalwidth, mc);
	PMoveUp(-hatHeight, mc);
	x[2] = ConvertedX(mc, dd);
	y[2] = ConvertedY(mc, dd);
	gc->lty = LTY_SOLID;
	if (gc->lwd > 1)
	    gc->lwd = 1;
	GEPolyline(3, x, y, gc, dd);
	PMoveTo(savedX + width, savedY, mc);
	gc->lty = savedlty;
	gc->lwd = savedlwd;
    }
    return EnlargeBBox(bbox, accentGap + hatHeight, 0, 0);
}

static int BarAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "bar");
}

static BBOX RenderBar(SEXP expr, int draw, mathContext *mc,
		      pGEcontext gc, pGEDevDesc dd)
{
    double savedX = mc->CurrentX;
    double savedY = mc->CurrentY;
    BBOX bbox = RenderElement(CADR(expr), draw, mc, gc, dd);
    double accentGap = ACCENT_GAP * XHeight(gc, dd);
    /*double hatHeight = HAT_HEIGHT * XHeight(gc, dd);*/
    double height = bboxHeight(bbox);
    double width = bboxWidth(bbox);
    double offset = bboxItalic(bbox);
    double x[2], y[2];

    if (draw) {
	int savedlty = gc->lty;
	double savedlwd = gc->lwd;
	PMoveTo(savedX + offset, savedY + height + accentGap, mc);
	x[0] = ConvertedX(mc, dd);
	y[0] = ConvertedY(mc, dd);
	PMoveAcross(width, mc);
	x[1] = ConvertedX(mc, dd);
	y[1] = ConvertedY(mc, dd);
	gc->lty = LTY_SOLID;
	if (gc->lwd > 1)
	    gc->lwd = 1;
	GEPolyline(2, x, y, gc, dd);
	PMoveTo(savedX + width, savedY, mc);
	gc->lty = savedlty;
	gc->lwd = savedlwd;
    }
    return EnlargeBBox(bbox, accentGap, 0, 0);
}

static struct {
    char *name;
    int code;
}
AccentTable[] = {
    { "hat",		 94 },
    { "ring",		176 },
    { "tilde",		126 },
    { "dot",            215 },
    { NULL,		  0 },
};

static int AccentCode(SEXP expr)
{
    int i;
    for (i = 0; AccentTable[i].code; i++)
	if (NameMatch(expr, AccentTable[i].name))
	    return AccentTable[i].code;
    return 0;
}

static int AccentAtom(SEXP expr)
{
    return NameAtom(expr) && (AccentCode(expr) != 0);
}

static void InvalidAccent(SEXP expr)
{
    errorcall(expr, _("invalid accent"));
}

static BBOX RenderAccent(SEXP expr, int draw, mathContext *mc,
			 pGEcontext gc, pGEDevDesc dd)
{
    SEXP body, accent;
    double savedX = mc->CurrentX;
    double savedY = mc->CurrentY;
    BBOX bodyBBox, accentBBox;
    double xoffset, yoffset, width, italic;
    int code;
    if (length(expr) != 2)
	InvalidAccent(expr);
    accent = CAR(expr);
    body = CADR(expr);
    code = AccentCode(accent);
    if (code == 0)
	InvalidAccent(expr);
    bodyBBox = RenderElement(body, 0, mc, gc, dd);
    italic = bboxItalic(bodyBBox);
    if (code == 176 || /* ring (as degree) */
	code == 215)   /* dotmath */
	accentBBox = RenderSymbolChar(code, 0, mc, gc, dd);
    else
	accentBBox = RenderChar(code, 0, mc, gc, dd);
    width = max(bboxWidth(bodyBBox) + bboxItalic(bodyBBox),
		bboxWidth(accentBBox));
    xoffset = 0.5 *(width - bboxWidth(bodyBBox));
    bodyBBox = RenderGap(xoffset, draw, mc, gc, dd);
    bodyBBox = CombineBBoxes(bodyBBox, RenderElement(body, draw, mc, gc, dd));
    bodyBBox = CombineBBoxes(bodyBBox, RenderGap(xoffset, draw, mc, gc, dd));
    PMoveTo(savedX, savedY, mc);
    xoffset = 0.5 *(width - bboxWidth(accentBBox))
	+ 0.9 * italic;
    yoffset = bboxHeight(bodyBBox) + bboxDepth(accentBBox) +
	0.1 * XHeight(gc, dd);
    if (draw) {
	PMoveTo(savedX + xoffset, savedY + yoffset, mc);
	if (code == 176 || /* ring (as degree) */
	    code == 215) /* dotmath */
	    RenderSymbolChar(code, draw, mc, gc, dd);
	else
	    RenderChar(code, draw, mc, gc, dd);
    }
    bodyBBox = CombineOffsetBBoxes(bodyBBox, 0, accentBBox, 0,
				   xoffset, yoffset);
    if (draw)
	PMoveTo(savedX + width, savedY, mc);
    return bodyBBox;
}


/*----------------------------------------------------------------------
 *
 *  Code for Fraction Expressions  (over, atop)
 *
 *  Rules 15, 15a, ..., 15e of the TeXBook
 *
 */

static void NumDenomVShift(BBOX numBBox, BBOX denomBBox,
			   double *u, double *v,
			   mathContext *mc, pGEcontext gc, pGEDevDesc dd)
{
    double a, delta, phi, theta;
    a = TeX(sigma22, gc, dd);
    theta = TeX(xi8, gc, dd);
    if(mc->CurrentStyle > STYLE_T) {
	*u = TeX(sigma8, gc, dd);
	*v = TeX(sigma11, gc, dd);
	phi = 3 * theta;
    }
    else {
	*u = TeX(sigma9, gc, dd);
	*v = TeX(sigma12, gc, dd);
	phi = theta;
    }
    delta = (*u - bboxDepth(numBBox)) - (a + 0.5 * theta);
    /*
     * Numerators and denominators on fractions appear too far from
     * horizontal bar.
     * Reread of Knuth suggests removing "+ theta" components below.
     */
    if (delta < phi)
	*u += (phi - delta); /* + theta; */
    delta = (a + 0.5 * theta) - (bboxHeight(denomBBox) - *v);
    if (delta < phi)
	*v += (phi - delta); /* + theta; */
}

static void NumDenomHShift(BBOX numBBox, BBOX denomBBox,
			   double *numShift, double *denomShift)
{
    double numWidth = bboxWidth(numBBox);
    double denomWidth = bboxWidth(denomBBox);
    if (numWidth > denomWidth) {
	*numShift = 0;
	*denomShift = (numWidth - denomWidth) / 2;
    }
    else {
	*numShift = (denomWidth - numWidth) / 2;
	*denomShift = 0;
    }
}

static BBOX RenderFraction(SEXP expr, int rule, int draw,
			   mathContext *mc, pGEcontext gc, pGEDevDesc dd)
{
    SEXP numerator = CADR(expr);
    SEXP denominator = CADDR(expr);
    BBOX numBBox, denomBBox;
    double nHShift, dHShift;
    double nVShift, dVShift;
    double width, x[2], y[2];
    double savedX = mc->CurrentX;
    double savedY = mc->CurrentY;
    STYLE style;

    style = GetStyle(mc);
    SetNumStyle(style, mc, gc);
    numBBox = RenderItalicCorr(RenderElement(numerator, 0, mc, gc, dd), 0,
			       mc, gc, dd);
    SetDenomStyle(style, mc, gc);
    denomBBox = RenderItalicCorr(RenderElement(denominator, 0, mc, gc, dd), 0,
				 mc, gc, dd);
    SetStyle(style, mc, gc);

    width = max(bboxWidth(numBBox), bboxWidth(denomBBox));
    NumDenomHShift(numBBox, denomBBox, &nHShift, &dHShift);
    NumDenomVShift(numBBox, denomBBox, &nVShift, &dVShift, mc, gc, dd);

    mc->CurrentX = savedX;
    mc->CurrentY = savedY;
    SetNumStyle(style, mc, gc);
    numBBox = RenderOffsetElement(numerator, nHShift, nVShift, draw, mc,
				  gc, dd);

    mc->CurrentX = savedX;
    mc->CurrentY = savedY;
    SetDenomStyle(style, mc, gc);
    denomBBox = RenderOffsetElement(denominator, dHShift, -dVShift, draw,
				    mc, gc, dd);

    SetStyle(style, mc, gc);

    if (draw) {
	if (rule) {
	    int savedlty = gc->lty;
	    double savedlwd = gc->lwd;
	    mc->CurrentX = savedX;
	    mc->CurrentY = savedY;
	    PMoveUp(AxisHeight(gc, dd), mc);
	    x[0] = ConvertedX(mc, dd);
	    y[0] = ConvertedY(mc, dd);
	    PMoveAcross(width, mc);
	    x[1] = ConvertedX(mc, dd);
	    y[1] = ConvertedY(mc, dd);
	    gc->lty = LTY_SOLID;
	    if (gc->lwd > 1)
		gc->lwd = 1;
	    GEPolyline(2, x, y, gc, dd);
	    PMoveUp(-AxisHeight(gc, dd), mc);
	    gc->lty = savedlty;
	    gc->lwd = savedlwd;
	}
	PMoveTo(savedX + width, savedY, mc);
    }
    return CombineAlignedBBoxes(numBBox, denomBBox);
}

static BBOX RenderUnderline(SEXP expr, int draw, mathContext *mc,
			    pGEcontext gc, pGEDevDesc dd)
{
    SEXP body = CADR(expr);
    BBOX BBox;
    double width, adepth, depth, x[2], y[2];
    double savedX = mc->CurrentX;
    double savedY = mc->CurrentY;

    BBox = RenderItalicCorr(RenderElement(body, 0, mc, gc, dd), 0, mc, gc, dd);
    width = bboxWidth(BBox);

    mc->CurrentX = savedX;
    mc->CurrentY = savedY;
    BBox = RenderElement(body, draw, mc, gc, dd);
    adepth = 0.1 * XHeight(gc, dd);
    depth = bboxDepth(BBox) + adepth;

    if (draw) {
	int savedlty = gc->lty;
	double savedlwd = gc->lwd;
	mc->CurrentX = savedX;
	mc->CurrentY = savedY;
	PMoveUp(-depth, mc);
	x[0] = ConvertedX(mc, dd);
	y[0] = ConvertedY(mc, dd);
	PMoveAcross(width, mc);
	x[1] = ConvertedX(mc, dd);
	y[1] = ConvertedY(mc, dd);
	gc->lty = LTY_SOLID;
	if (gc->lwd > 1)
	    gc->lwd = 1;
	GEPolyline(2, x, y, gc, dd);
	PMoveUp(depth, mc);
	gc->lty = savedlty;
	gc->lwd = savedlwd;
	PMoveTo(savedX + width, savedY, mc);
    }
    return EnlargeBBox(BBox, 0.0, adepth, 0.0);
}


static int OverAtom(SEXP expr)
{
    return NameAtom(expr) &&
	(NameMatch(expr, "over") || NameMatch(expr, "frac"));
}

static BBOX RenderOver(SEXP expr, int draw, mathContext *mc,
		       pGEcontext gc, pGEDevDesc dd)
{
    return RenderFraction(expr, 1, draw, mc, gc, dd);
}

static int UnderlAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "underline");
}

static BBOX RenderUnderl(SEXP expr, int draw, mathContext *mc,
			 pGEcontext gc, pGEDevDesc dd)
{
    return RenderUnderline(expr, draw, mc, gc, dd);
}


static int AtopAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "atop");
}

static BBOX RenderAtop(SEXP expr, int draw, mathContext *mc,
		       pGEcontext gc, pGEDevDesc dd)
{
    return RenderFraction(expr, 0, draw, mc, gc, dd);
}

/*----------------------------------------------------------------------
 *
 *  Code for Grouped Expressions  (e.g. ( ... ))
 *
 *    group(ldelim, body, rdelim)
 *
 *    bgroup(ldelim, body, rdelim)
 *
 */

#define DelimSymbolMag 1.25

static int DelimCode(SEXP expr, SEXP head)
{
    int code = 0;
    if (NameAtom(head)) {
	if (NameMatch(head, "lfloor"))
	    code = S_BRACKETLEFTBT;
	else if (NameMatch(head, "rfloor"))
	    code = S_BRACKETRIGHTBT;
	if (NameMatch(head, "lceil"))
	    code = S_BRACKETLEFTTP;
	else if (NameMatch(head, "rceil"))
	    code = S_BRACKETRIGHTTP;
    }
    else if (StringAtom(head) && length(head) > 0) {
	if (StringMatch(head, "|"))
	    code = '|';
	else if (StringMatch(head, "||"))
	    code = 2;
	else if (StringMatch(head, "("))
	    code = '(';
	else if (StringMatch(head, ")"))
	    code = ')';
	else if (StringMatch(head, "["))
	    code = '[';
	else if (StringMatch(head, "]"))
	    code = ']';
	else if (StringMatch(head, "{"))
	    code = '{';
	else if (StringMatch(head, "}"))
	    code = '}';
	else if (StringMatch(head, "") || StringMatch(head, "."))
	    code = '.';
    }
    if (code == 0)
	errorcall(expr, _("invalid group delimiter"));
    return code;
}

static BBOX RenderDelimiter(int delim, int draw, mathContext *mc,
			    pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox;
    double savecex = gc->cex;
    gc->cex = DelimSymbolMag * gc->cex;
    bbox = RenderSymbolChar(delim, draw, mc, gc, dd);
    gc->cex = savecex;
    return bbox;
}

static int GroupAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "group");
}

static BBOX RenderGroup(SEXP expr, int draw, mathContext *mc,
			pGEcontext gc, pGEDevDesc dd)
{
    double cexSaved = gc->cex;
    BBOX bbox;
    int code;
    if (length(expr) != 4)
	errorcall(expr, _("invalid group specification"));
    bbox = NullBBox();
    code = DelimCode(expr, CADR(expr));
    gc->cex = DelimSymbolMag * gc->cex;
    if (code == 2) {
	bbox = RenderSymbolChar('|', draw, mc, gc, dd);
	bbox = RenderSymbolChar('|', draw, mc, gc, dd);
    }
    else if (code != '.')
	bbox = RenderSymbolChar(code, draw, mc, gc, dd);
    gc->cex = cexSaved;
    bbox = CombineBBoxes(bbox, RenderElement(CADDR(expr), draw, mc, gc, dd));
    bbox = RenderItalicCorr(bbox, draw, mc, gc, dd);
    code = DelimCode(expr, CADDDR(expr));
    gc->cex = DelimSymbolMag * gc->cex;
    if (code == 2) {
	bbox = CombineBBoxes(bbox, RenderSymbolChar('|', draw, mc, gc, dd));
	bbox = CombineBBoxes(bbox, RenderSymbolChar('|', draw, mc, gc, dd));
    }
    else if (code != '.')
	bbox = CombineBBoxes(bbox, RenderSymbolChar(code, draw, mc, gc, dd));
    gc->cex = cexSaved;
    return bbox;
}

static int BGroupAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "bgroup");
}

static BBOX RenderDelim(int which, double dist, int draw, mathContext *mc,
			pGEcontext gc, pGEDevDesc dd)
{
    double savedX = mc->CurrentX;
    double savedY = mc->CurrentY;
    FontType prev = SetFont(SymbolFont, gc);
    BBOX ansBBox, topBBox, botBBox, extBBox, midBBox;
    int top, bot, ext, mid;
    int i, n;
    double topShift, botShift, extShift, midShift;
    double ytop, ybot, extHeight, delta;
    double axisHeight = TeX(sigma22, gc, dd);

    switch(which) {
    case '.':
	SetFont(prev, gc);
	return NullBBox();
	break;
    case '|':
    case 2:
	top = 239; ext = 239; bot = 239; mid = 0;
	break;
    case '(':
	top = 230; ext = 231; bot = 232; mid = 0;
	break;
    case ')':
	top = 246; ext = 247; bot = 248; mid = 0;
	break;
    case '[':
	top = 233; ext = 234; bot = 235; mid = 0;
	break;
    case ']':
	top = 249; ext = 250; bot = 251; mid = 0;
	break;
    case '{':
	top = 236; ext = 239; bot = 238; mid = 237;
	break;
    case '}':
	top = 252; ext = 239; bot = 254; mid = 253;
	break;
    default:
	error(_("group is incomplete"));
	return NullBBox();/*never reached*/
    }
    topBBox = GlyphBBox(top, gc, dd);
    extBBox = GlyphBBox(ext, gc, dd);
    botBBox = GlyphBBox(bot, gc, dd);
    if (which == '{' || which == '}') {
	if (1.2 * (bboxHeight(topBBox) + bboxDepth(topBBox)) > dist)
	    dist = 1.2 * (bboxHeight(topBBox) + bboxDepth(botBBox));
    }
    else {
	if (0.8 * (bboxHeight(topBBox) + bboxDepth(topBBox)) > dist)
	    dist = 0.8 * (bboxHeight(topBBox) + bboxDepth(topBBox));
    }
    extHeight = bboxHeight(extBBox) + bboxDepth(extBBox);
    topShift = dist - bboxHeight(topBBox) + axisHeight;
    botShift = dist - bboxDepth(botBBox) - axisHeight;
    extShift = 0.5 * (bboxHeight(extBBox) - bboxDepth(extBBox));
    topBBox = ShiftBBox(topBBox, topShift);
    botBBox = ShiftBBox(botBBox, -botShift);
    ansBBox = CombineAlignedBBoxes(topBBox, botBBox);
    if (which == '{' || which == '}') {
	midBBox = GlyphBBox(mid, gc, dd);
	midShift = axisHeight
	    - 0.5 * (bboxHeight(midBBox) - bboxDepth(midBBox));
	midBBox = ShiftBBox(midBBox, midShift);
	ansBBox = CombineAlignedBBoxes(ansBBox, midBBox);
	if (draw) {
	    PMoveTo(savedX, savedY + topShift, mc);
	    RenderSymbolChar(top, draw, mc, gc, dd);
	    PMoveTo(savedX, savedY + midShift, mc);
	    RenderSymbolChar(mid, draw, mc, gc, dd);
	    PMoveTo(savedX, savedY - botShift, mc);
	    RenderSymbolChar(bot, draw, mc, gc, dd);
	    PMoveTo(savedX + bboxWidth(ansBBox), savedY, mc);
	}
    }
    else {
	if (draw) {
	    /* draw the top and bottom elements */
	    PMoveTo(savedX, savedY + topShift, mc);
	    RenderSymbolChar(top, draw, mc, gc, dd);
	    PMoveTo(savedX, savedY - botShift, mc);
	    RenderSymbolChar(bot, draw, mc, gc, dd);
	    /* now join with extenders */
	    ytop = axisHeight + dist
		- (bboxHeight(topBBox) + bboxDepth(topBBox));
	    ybot = axisHeight - dist
		+ (bboxHeight(botBBox) + bboxDepth(botBBox));
	    n = (int) ceil((ytop - ybot) / (0.99 * extHeight));
	    if (n > 0) {
		delta = (ytop - ybot) / n;
		for (i = 0; i < n; i++) {
		    PMoveTo(savedX, savedY + ybot +
			    (i + 0.5) * delta - extShift, mc);
		    RenderSymbolChar(ext, draw, mc, gc, dd);
		}
	    }
	    PMoveTo(savedX + bboxWidth(ansBBox), savedY, mc);

	}
    }
    SetFont(prev, gc);
    return ansBBox;
}

static BBOX RenderBGroup(SEXP expr, int draw, mathContext *mc,
			 pGEcontext gc, pGEDevDesc dd)
{
    double dist;
    BBOX bbox;
    double axisHeight = TeX(sigma22, gc, dd);
    double extra = 0.2 * xHeight(gc, dd);
    int delim1, delim2;
    if (length(expr) != 4)
	errorcall(expr, _("invalid group specification"));
    bbox = NullBBox();
    delim1 = DelimCode(expr, CADR(expr));
    delim2 = DelimCode(expr, CADDDR(expr));
    bbox = RenderElement(CADDR(expr), 0, mc, gc, dd);
    dist = max(bboxHeight(bbox) - axisHeight, bboxDepth(bbox) + axisHeight);
    bbox = RenderDelim(delim1, dist + extra, draw, mc, gc, dd);
    bbox = CombineBBoxes(bbox,	RenderElement(CADDR(expr), draw, mc, gc, dd));
    bbox = RenderItalicCorr(bbox, draw, mc, gc, dd);
    bbox = CombineBBoxes(bbox,	RenderDelim(delim2, dist + extra, draw, mc,
					    gc, dd));
    return bbox;
}

/*----------------------------------------------------------------------
 *
 *  Code for Parenthetic Expressions  (i.e. ( ... ))
 *
 */

static int ParenAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "(");
}

static BBOX RenderParen(SEXP expr, int draw, mathContext *mc,
			pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox;
    bbox = RenderDelimiter(S_PARENLEFT, draw, mc, gc, dd);
    bbox = CombineBBoxes(bbox, RenderElement(CADR(expr), draw, mc, gc, dd));
    bbox = RenderItalicCorr(bbox, draw, mc, gc, dd);
    return CombineBBoxes(bbox, RenderDelimiter(S_PARENRIGHT, draw, mc, gc, dd));
}

/*----------------------------------------------------------------------
 *
 *  Code for Integral Operators.
 *
 */

static int IntAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "integral");
}


static BBOX RenderIntSymbol(int draw, mathContext *mc, pGEcontext gc,
			    pGEDevDesc dd)
{
    double savedX = mc->CurrentX;
    double savedY = mc->CurrentY;
    if (GetStyle(mc) > STYLE_T) {
	BBOX bbox1 = RenderSymbolChar(243, 0, mc, gc, dd);
	BBOX bbox2 = RenderSymbolChar(245, 0, mc, gc, dd);
	double shift;
	shift = TeX(sigma22, gc, dd) + 0.99 * bboxDepth(bbox1);
	PMoveUp(shift, mc);
	bbox1 = ShiftBBox(RenderSymbolChar(243, draw, mc, gc, dd), shift);
	mc->CurrentX = savedX;
	mc->CurrentY = savedY;
	shift = TeX(sigma22, gc, dd) - 0.99 * bboxHeight(bbox2);
	PMoveUp(shift, mc);
	bbox2 = ShiftBBox(RenderSymbolChar(245, draw, mc, gc, dd), shift);
	if (draw)
	    PMoveTo(savedX + max(bboxWidth(bbox1), bboxWidth(bbox2)),
		    savedY, mc);
	else
	    PMoveTo(savedX, savedY, mc);
	return CombineAlignedBBoxes(bbox1, bbox2);
    }
    else {
	return RenderSymbolChar(0362, draw, mc, gc, dd);
    }
}

static BBOX RenderInt(SEXP expr, int draw, mathContext *mc,
		      pGEcontext gc, pGEDevDesc dd)
{
    BBOX opBBox, lowerBBox, upperBBox, bodyBBox;
    int nexpr = length(expr);
    STYLE style = GetStyle(mc);
    double savedX = mc->CurrentX;
    double savedY = mc->CurrentY;
    double hshift, vshift, width;

    opBBox = RenderIntSymbol(draw, mc, gc, dd);
    width = bboxWidth(opBBox);
    mc->CurrentX = savedX;
    mc->CurrentY = savedY;
    if (nexpr > 2) {
	hshift = 0.5 * width + ThinSpace(gc, dd);
	SetSubStyle(style, mc, gc);
	lowerBBox = RenderElement(CADDR(expr), 0, mc, gc, dd);
	vshift = bboxDepth(opBBox) + CenterShift(lowerBBox);
	lowerBBox = RenderOffsetElement(CADDR(expr), hshift, -vshift, draw,
					mc, gc, dd);
	opBBox = CombineAlignedBBoxes(opBBox, lowerBBox);
	SetStyle(style, mc, gc);
	mc->CurrentX = savedX;
	mc->CurrentY = savedY;
    }
    if (nexpr > 3) {
	hshift = width + ThinSpace(gc, dd);
	SetSupStyle(style, mc, gc);
	upperBBox = RenderElement(CADDDR(expr), 0, mc, gc, dd);
	vshift = bboxHeight(opBBox) - CenterShift(upperBBox);
	upperBBox = RenderOffsetElement(CADDDR(expr), hshift, vshift, draw,
					mc, gc, dd);
	opBBox = CombineAlignedBBoxes(opBBox, upperBBox);
	SetStyle(style, mc, gc);
	mc->CurrentX = savedX;
	mc->CurrentY = savedY;
    }
    PMoveAcross(bboxWidth(opBBox), mc);
    if (nexpr > 1) {
	bodyBBox = RenderElement(CADR(expr), draw, mc, gc, dd);
	opBBox = CombineBBoxes(opBBox, bodyBBox);
    }
    return opBBox;
}


/*----------------------------------------------------------------------
 *
 *  Code for Operator Expressions (sum, product, lim, inf, sup, ...)
 *
 */

#define OperatorSymbolMag  1.25

static SymTab OpTable[] = {
    { "prod",		S_PRODUCT },
    { "sum",		S_SUM },
    { "union",		S_UNION },
    { "intersect",	S_INTERSECTION },
    { "lim",		N_LIM },
    { "liminf",		N_LIMINF },
    { "limsup",		N_LIMINF },
    { "inf",		N_INF },
    { "sup",		N_SUP },
    { "min",		N_MIN },
    { "max",		N_MAX },
    { NULL,		0 }
};

static int OpAtom(SEXP expr)
{
    int i;
    for (i = 0; OpTable[i].code; i++)
	if (NameMatch(expr, OpTable[i].name))
	    return OpTable[i].code;
    return 0;
}

static BBOX RenderOpSymbol(SEXP op, int draw, mathContext *mc,
			   pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox;
    double cexSaved = gc->cex;
    /*double savedX = mc->CurrentX;*/
    /*double savedY = mc->CurrentY;*/
    double shift;
    int display = (GetStyle(mc) > STYLE_T);
    int opId = OpAtom(op);

    if (opId == S_SUM || opId == S_PRODUCT ||
	opId == S_UNION || opId == S_INTERSECTION) {
	if (display) {
	    gc->cex = OperatorSymbolMag * gc->cex;
	    bbox = RenderSymbolChar(OpAtom(op), 0, mc, gc, dd);
	    shift = 0.5 * (bboxHeight(bbox) - bboxDepth(bbox)) -
		TeX(sigma22, gc, dd);
	    if (draw) {
		PMoveUp(-shift, mc);
		bbox = RenderSymbolChar(opId, 1, mc, gc, dd);
		PMoveUp(shift, mc);
	    }
	    gc->cex = cexSaved;
	    return ShiftBBox(bbox, -shift);
	}
	else return RenderSymbolChar(opId, draw, mc, gc, dd);
    }
    else {
	FontType prevfont = SetFont(PlainFont, gc);
	bbox = RenderStr(CHAR(PRINTNAME(op)), draw, mc, gc, dd);
	SetFont(prevfont, gc);
	return bbox;
    }
}

static BBOX RenderOp(SEXP expr, int draw, mathContext *mc,
		     pGEcontext gc, pGEDevDesc dd)
{
    BBOX lowerBBox = NullBBox() /* -Wall */, upperBBox = NullBBox(), bodyBBox;
    double savedX = mc->CurrentX;
    double savedY = mc->CurrentY;
    int nexpr = length(expr);
    STYLE style = GetStyle(mc);
    BBOX opBBox = RenderOpSymbol(CAR(expr), 0, mc, gc, dd);
    double width = bboxWidth(opBBox);
    double hshift, lvshift, uvshift;
    lvshift = uvshift = 0;	/* -Wall */
    if (nexpr > 2) {
	SetSubStyle(style, mc, gc);
	lowerBBox = RenderElement(CADDR(expr), 0, mc, gc, dd);
	SetStyle(style, mc, gc);
	width = max(width, bboxWidth(lowerBBox));
	lvshift = max(TeX(xi10, gc, dd), TeX(xi12, gc, dd) -
		      bboxHeight(lowerBBox));
	lvshift = bboxDepth(opBBox) + bboxHeight(lowerBBox) + lvshift;
    }
    if (nexpr > 3) {
	SetSupStyle(style, mc, gc);
	upperBBox = RenderElement(CADDDR(expr), 0, mc, gc, dd);
	SetStyle(style, mc, gc);
	width = max(width, bboxWidth(upperBBox));
	uvshift = max(TeX(xi9, gc, dd), TeX(xi11, gc, dd) -
		      bboxDepth(upperBBox));
	uvshift = bboxHeight(opBBox) + bboxDepth(upperBBox) + uvshift;
    }
    hshift = 0.5 * (width - bboxWidth(opBBox));
    opBBox = RenderGap(hshift, draw, mc, gc, dd);
    opBBox = CombineBBoxes(opBBox,
			   RenderOpSymbol(CAR(expr), draw, mc, gc, dd));
    mc->CurrentX = savedX;
    mc->CurrentY = savedY;
    if (nexpr > 2) {
	SetSubStyle(style, mc, gc);
	hshift = 0.5 * (width - bboxWidth(lowerBBox));
	lowerBBox = RenderOffsetElement(CADDR(expr), hshift, -lvshift, draw,
					mc, gc, dd);
	SetStyle(style, mc, gc);
	opBBox = CombineAlignedBBoxes(opBBox, lowerBBox);
	mc->CurrentX = savedX;
	mc->CurrentY = savedY;
    }
    if (nexpr > 3) {
	SetSupStyle(style, mc, gc);
	hshift = 0.5 * (width - bboxWidth(upperBBox));
	upperBBox = RenderOffsetElement(CADDDR(expr), hshift, uvshift, draw,
					mc, gc, dd);
	SetStyle(style, mc, gc);
	opBBox = CombineAlignedBBoxes(opBBox, upperBBox);
	mc->CurrentX = savedX;
	mc->CurrentY = savedY;
    }
    opBBox = EnlargeBBox(opBBox, TeX(xi13, gc, dd), TeX(xi13, gc, dd), 0);
    if (draw)
	PMoveAcross(width, mc);
    opBBox = CombineBBoxes(opBBox,
			   RenderGap(ThinSpace(gc, dd), draw, mc, gc, dd));
    bodyBBox = RenderElement(CADR(expr), draw, mc, gc, dd);
    return CombineBBoxes(opBBox, bodyBBox);
}


/*----------------------------------------------------------------------
 *
 *  Code for radical expressions (root, sqrt)
 *
 *  Tunable parameteters :
 *
 *  RADICAL_GAP	   The gap between the nucleus and the radical extension.
 *  RADICAL_SPACE  Extra space to the left and right of the nucleus.
 *
 */

#define RADICAL_GAP    0.4
#define RADICAL_SPACE  0.2

static int RadicalAtom(SEXP expr)
{
    return NameAtom(expr) &&
	(NameMatch(expr, "root") ||
	 NameMatch(expr, "sqrt"));
}

static BBOX RenderScript(SEXP expr, int draw, mathContext *mc,
			 pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox;
    STYLE style = GetStyle(mc);
    SetSupStyle(style, mc, gc);
    bbox = RenderElement(expr, draw, mc, gc, dd);
    SetStyle(style, mc, gc);
    return bbox;
}

static BBOX RenderRadical(SEXP expr, int draw, mathContext *mc,
			  pGEcontext gc, pGEDevDesc dd)
{
    SEXP body = CADR(expr);
    SEXP order = CADDR(expr);
    BBOX bodyBBox, orderBBox;
    double radWidth, radHeight;
    double leadWidth, leadHeight, twiddleHeight;
    double hshift, vshift;
    double radGap, radSpace, radTrail;
    STYLE style = GetStyle(mc);
    double savedX = mc->CurrentX;
    double savedY = mc->CurrentY;
    double x[5], y[5];

    radGap = RADICAL_GAP * xHeight(gc, dd);
    radSpace = RADICAL_SPACE * xHeight(gc, dd);
    radTrail = MuSpace(gc, dd);
    SetPrimeStyle(style, mc, gc);
    bodyBBox = RenderElement(body, 0, mc, gc, dd);
    bodyBBox = RenderItalicCorr(bodyBBox, 0, mc, gc, dd);

    radWidth = 0.6 *XHeight(gc, dd);
    radHeight = bboxHeight(bodyBBox) + radGap;
    twiddleHeight = CenterShift(bodyBBox);

    leadWidth = radWidth;
    leadHeight = radHeight;
    if (order != R_NilValue) {
	SetSupStyle(style, mc, gc);
	orderBBox = RenderScript(order, 0, mc, gc, dd);
	leadWidth = max(leadWidth, bboxWidth(orderBBox) + 0.4 * radWidth);
	hshift = leadWidth - bboxWidth(orderBBox) - 0.4 * radWidth;
	vshift = leadHeight - bboxHeight(orderBBox);
	if (vshift - bboxDepth(orderBBox) < twiddleHeight + radGap)
	    vshift = twiddleHeight + bboxDepth(orderBBox) + radGap;
	if (draw) {
	    PMoveTo(savedX + hshift, savedY + vshift, mc);
	    orderBBox = RenderScript(order, draw, mc, gc, dd);
	}
	orderBBox = EnlargeBBox(orderBBox, vshift, 0, hshift);
    }
    else
	orderBBox = NullBBox();
    if (draw) {
	int savedlty = gc->lty;
	double savedlwd = gc->lwd;
	PMoveTo(savedX + leadWidth - radWidth, savedY, mc);
	PMoveUp(0.8 * twiddleHeight, mc);
	x[0] = ConvertedX(mc, dd);
	y[0] = ConvertedY(mc, dd);
	PMoveUp(0.2 * twiddleHeight, mc);
	PMoveAcross(0.3 * radWidth, mc);
	x[1] = ConvertedX(mc, dd);
	y[1] = ConvertedY(mc, dd);
	PMoveUp(-(twiddleHeight + bboxDepth(bodyBBox)), mc);
	PMoveAcross(0.3 * radWidth, mc);
	x[2] = ConvertedX(mc, dd);
	y[2] = ConvertedY(mc, dd);
	PMoveUp(bboxDepth(bodyBBox) + bboxHeight(bodyBBox) + radGap, mc);
	PMoveAcross(0.4 * radWidth, mc);
	x[3] = ConvertedX(mc, dd);
	y[3] = ConvertedY(mc, dd);
	PMoveAcross(radSpace + bboxWidth(bodyBBox) + radTrail, mc);
	x[4] = ConvertedX(mc, dd);
	y[4] = ConvertedY(mc, dd);
	gc->lty = LTY_SOLID;
	if (gc->lwd > 1)
	    gc->lwd = 1;
	GEPolyline(5, x, y, gc, dd);
	PMoveTo(savedX, savedY, mc);
	gc->lty = savedlty;
	gc->lwd = savedlwd;
    }
    orderBBox =
	CombineAlignedBBoxes(orderBBox,
			     RenderGap(leadWidth + radSpace, draw, mc, gc, dd));
    SetPrimeStyle(style, mc, gc);
    orderBBox = CombineBBoxes(orderBBox,
			      RenderElement(body, draw, mc, gc, dd));
    orderBBox = CombineBBoxes(orderBBox,
			      RenderGap(2 * radTrail, draw, mc, gc, dd));
    orderBBox = EnlargeBBox(orderBBox, radGap, 0, 0);/* << fixes PR#1101 */
    SetStyle(style, mc, gc);
    return orderBBox;
}

/*----------------------------------------------------------------------
 *
 *  Code for Absolute Value Expressions (abs)
 *
 */

static int AbsAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "abs");
}

static BBOX RenderAbs(SEXP expr, int draw, mathContext *mc,
		      pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox = RenderElement(CADR(expr), 0, mc, gc, dd);
    double height = bboxHeight(bbox);
    double depth = bboxDepth(bbox);
    double x[2], y[2];

    bbox= RenderGap(MuSpace(gc, dd), draw, mc, gc, dd);
    if (draw) {
	int savedlty = gc->lty;
	double savedlwd = gc->lwd;
	PMoveUp(-depth, mc);
	x[0] = ConvertedX(mc, dd);
	y[0] = ConvertedY(mc, dd);
	PMoveUp(depth + height, mc);
	x[1] = ConvertedX(mc, dd);
	y[1] = ConvertedY(mc, dd);
	gc->lty = LTY_SOLID;
	if (gc->lwd > 1)
	    gc->lwd = 1;
	GEPolyline(2, x, y, gc, dd);
	PMoveUp(-height, mc);
	gc->lty = savedlty;
	gc->lwd = savedlwd;
    }
    bbox = CombineBBoxes(bbox, RenderGap(MuSpace(gc, dd), draw, mc, gc, dd));
    bbox = CombineBBoxes(bbox, RenderElement(CADR(expr), draw, mc, gc, dd));
    bbox = RenderItalicCorr(bbox, draw, mc, gc, dd);
    bbox = CombineBBoxes(bbox, RenderGap(MuSpace(gc, dd), draw, mc, gc, dd));
    if (draw) {
	int savedlty = gc->lty;
	double savedlwd = gc->lwd;
	PMoveUp(-depth, mc);
	x[0] = ConvertedX(mc, dd);
	y[0] = ConvertedY(mc, dd);
	PMoveUp(depth + height, mc);
	x[1] = ConvertedX(mc, dd);
	y[1] = ConvertedY(mc, dd);
	gc->lty = LTY_SOLID;
	if (gc->lwd > 1)
	    gc->lwd = 1;
	GEPolyline(2, x, y, gc, dd);
	PMoveUp(-height, mc);
	gc->lty = savedlty;
	gc->lwd = savedlwd;
    }
    bbox = CombineBBoxes(bbox, RenderGap(MuSpace(gc, dd), draw, mc, gc, dd));
    return bbox;
}

/*----------------------------------------------------------------------
 *
 *  Code for Grouped Expressions (i.e. { ... } )
 *
 */

static int CurlyAtom(SEXP expr)
{
    return NameAtom(expr) &&
	NameMatch(expr, "{");
}

static BBOX RenderCurly(SEXP expr, int draw, mathContext *mc,
			pGEcontext gc, pGEDevDesc dd)
{
    return RenderElement(CADR(expr), draw, mc, gc, dd);
}


/*----------------------------------------------------------------------
 *
 *  Code for Relation Expressions (i.e. ... ==, !=, ...)
 *
 */

				/* Binary Relationships */
static
SymTab RelTable[] = {
    { "<",		 60 },	/* less */
    { "==",		 61 },	/* equal */
    { ">",		 62 },	/* greater */
    { "%=~%",		 64 },	/* congruent */
    { "!=",		185 },	/* not equal */
    { "<=",		163 },	/* less or equal */
    { ">=",		179 },	/* greater or equal */
    { "%==%",		186 },	/* equivalence */
    { "%~~%",		187 },	/* approxequal */
    { "%prop%",         181 },  /* proportional to */
    { "%~%",            126 },  /* distributed as */

    { "%<->%",		171 },	/* Arrows */
    { "%<-%",		172 },
    { "%up%",		173 },
    { "%->%",		174 },
    { "%down%",		175 },
    { "%<=>%",		219 },
    { "%<=%",		220 },
    { "%dblup%",	221 },
    { "%=>%",		222 },
    { "%dbldown%",	223 },

    { "%supset%",	201 },	/* Sets (TeX Names) */
    { "%supseteq%",	202 },
    { "%notsubset%",	203 },
    { "%subset%",	204 },
    { "%subseteq%",	205 },
    { "%in%",		206 },
    { "%notin%",	207 },

    { NULL,		  0 },
};

static int RelAtom(SEXP expr)
{
    int i;
    for (i = 0; RelTable[i].code; i++)
	if (NameMatch(expr, RelTable[i].name))
	    return RelTable[i].code;
    return 0;
}

static BBOX RenderRel(SEXP expr, int draw, mathContext *mc,
		      pGEcontext gc, pGEDevDesc dd)
{
    int op = RelAtom(CAR(expr));
    int nexpr = length(expr);
    BBOX bbox;
    double gap;

    if(nexpr == 3) {
	gap = (mc->CurrentStyle > STYLE_S) ? ThickSpace(gc, dd) : 0;
	bbox = RenderElement(CADR(expr), draw, mc, gc, dd);
	bbox = RenderItalicCorr(bbox, draw, mc, gc, dd);
	bbox = CombineBBoxes(bbox, RenderGap(gap, draw, mc, gc, dd));
	bbox = CombineBBoxes(bbox, RenderSymbolChar(op, draw, mc, gc, dd));
	bbox = CombineBBoxes(bbox, RenderGap(gap, draw, mc, gc, dd));
	return
	    CombineBBoxes(bbox, RenderElement(CADDR(expr), draw, mc, gc, dd));
    }
    else error(_("invalid mathematical annotation"));

    return NullBBox();		/* -Wall */
}


/*----------------------------------------------------------------------
 *
 *  Code for Boldface Expressions
 *
 */

static int BoldAtom(SEXP expr)
{
    return NameAtom(expr) &&
	NameMatch(expr, "bold");
}

static BBOX RenderBold(SEXP expr, int draw, mathContext *mc,
		       pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox;
    FontType prevfont = SetFont(BoldFont, gc);
    bbox = RenderElement(CADR(expr), draw, mc, gc, dd);
    SetFont(prevfont, gc);
    return bbox;
}

/*----------------------------------------------------------------------
 *
 *  Code for Italic Expressions
 *
 */

static int ItalicAtom(SEXP expr)
{
    return NameAtom(expr) &&
	(NameMatch(expr, "italic") || NameMatch(expr, "math"));
}

static BBOX RenderItalic(SEXP expr, int draw, mathContext *mc,
			 pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox;
    FontType prevfont = SetFont(ItalicFont, gc);
    bbox = RenderElement(CADR(expr), draw, mc, gc, dd);
    SetFont(prevfont, gc);
    return bbox;
}

/*----------------------------------------------------------------------
 *
 *  Code for Plain (i.e. Roman) Expressions
 *
 */

static int PlainAtom(SEXP expr)
{
    return NameAtom(expr) &&
	NameMatch(expr, "plain");
}

static BBOX RenderPlain(SEXP expr, int draw, mathContext *mc,
			pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox;
    int prevfont = SetFont(PlainFont, gc);
    bbox = RenderElement(CADR(expr), draw, mc, gc, dd);
    SetFont(prevfont, gc);
    return bbox;
}

/*----------------------------------------------------------------------
 *
 *  Code for SymbolFace (i.e. font = 5) Expressions
 *
 *  This makes the default font an Adobe Symbol Encoded font
 *  (provides access to any character in the Adobe Symbol Font
 *   encoding via strings like "\042" for the universal ["for all"]
 *   symbol, without the need for separate special names for each
 *   of these symbols).
 *
 */

static int SymbolFaceAtom(SEXP expr)
{
    return NameAtom(expr) &&
	NameMatch(expr, "symbol");
}

static BBOX RenderSymbolFace(SEXP expr, int draw, mathContext *mc,
			     pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox;
    int prevfont = SetFont(SymbolFont, gc);
    bbox = RenderElement(CADR(expr), draw, mc, gc, dd);
    SetFont(prevfont, gc);
    return bbox;
}

/*----------------------------------------------------------------------
 *
 *  Code for Bold Italic Expressions
 *
 */

static int BoldItalicAtom(SEXP expr)
{
    return NameAtom(expr) &&
	(NameMatch(expr, "bolditalic") || NameMatch(expr, "boldmath"));
}

static BBOX RenderBoldItalic(SEXP expr, int draw, mathContext *mc,
			     pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox;
    int prevfont = SetFont(BoldItalicFont, gc);
    bbox = RenderElement(CADR(expr), draw, mc, gc, dd);
    SetFont(prevfont, gc);
    return bbox;
}

/*----------------------------------------------------------------------
 *
 *  Code for Styles
 *
 */

static int StyleAtom(SEXP expr)
{
    return (NameAtom(expr) &&
	    (NameMatch(expr, "displaystyle") ||
	     NameMatch(expr, "textstyle")    ||
	     NameMatch(expr, "scriptstyle")   ||
	     NameMatch(expr, "scriptscriptstyle")));
}

static BBOX RenderStyle(SEXP expr, int draw, mathContext *mc,
			pGEcontext gc, pGEDevDesc dd)
{
    STYLE prevstyle = GetStyle(mc);
    BBOX bbox;
    if (NameMatch(CAR(expr), "displaystyle"))
	SetStyle(STYLE_D, mc, gc);
    else if (NameMatch(CAR(expr), "textstyle"))
	SetStyle(STYLE_T, mc, gc);
    else if (NameMatch(CAR(expr), "scriptstyle"))
	SetStyle(STYLE_S, mc, gc);
    else if (NameMatch(CAR(expr), "scriptscriptstyle"))
	SetStyle(STYLE_SS, mc, gc);
    bbox = RenderElement(CADR(expr), draw, mc, gc, dd);
    SetStyle(prevstyle, mc, gc);
    return bbox;
}

/*----------------------------------------------------------------------
 *
 *  Code for Phantom Expressions
 *
 */

static int PhantomAtom(SEXP expr)
{
    return (NameAtom(expr) &&
	    (NameMatch(expr, "phantom") ||
	     NameMatch(expr, "vphantom")));
}

static BBOX RenderPhantom(SEXP expr, int draw, mathContext *mc,
			  pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox = RenderElement(CADR(expr), 0, mc, gc, dd);
    if (NameMatch(CAR(expr), "vphantom")) {
	bboxWidth(bbox) = 0;
	bboxItalic(bbox) = 0;
    }
    else RenderGap(bboxWidth(bbox), draw, mc, gc, dd);
    return bbox;
}

/*----------------------------------------------------------------------
 *
 *  Code for Concatenate Expressions
 *
 */

static int ConcatenateAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "paste");
}

static BBOX RenderConcatenate(SEXP expr, int draw, mathContext *mc,
			      pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox = NullBBox();
    int i, n;

    expr = CDR(expr);
    n = length(expr);

    for (i = 0; i < n; i++) {
	bbox = CombineBBoxes(bbox, RenderElement(CAR(expr), draw, mc, gc, dd));
	if (i != n - 1)
	    bbox = RenderItalicCorr(bbox, draw, mc, gc, dd);
	expr = CDR(expr);
    }
    return bbox;
}

/*----------------------------------------------------------------------
 *
 *  Code for Comma-Separated Lists
 *
 */

static BBOX RenderCommaList(SEXP expr, int draw, mathContext *mc,
			    pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox = NullBBox();
    double small = 0.4 * ThinSpace(gc, dd);
    int i, n;
    n = length(expr);
    for (i = 0; i < n; i++) {
	if (NameAtom(CAR(expr)) && NameMatch(CAR(expr), "...")) {
	    if (i > 0) {
		bbox = CombineBBoxes(bbox, RenderSymbolChar(S_COMMA, draw,
							    mc, gc, dd));
		bbox = CombineBBoxes(bbox, RenderSymbolChar(S_SPACE, draw,
							    mc, gc, dd));
	    }
	    bbox = CombineBBoxes(bbox, RenderSymbolChar(S_ELLIPSIS, draw,
							mc, gc, dd));
	    bbox = CombineBBoxes(bbox, RenderGap(small, draw, mc, gc, dd));
	}
	else {
	    if (i > 0) {
		bbox = CombineBBoxes(bbox, RenderSymbolChar(S_COMMA, draw,
							    mc, gc, dd));
		bbox = CombineBBoxes(bbox, RenderSymbolChar(S_SPACE, draw,
							    mc, gc, dd));
	    }
	    bbox = CombineBBoxes(bbox, RenderElement(CAR(expr), draw, mc,
						     gc, dd));
	}
	expr = CDR(expr);
    }
    return bbox;
}

/*----------------------------------------------------------------------
 *
 *  Code for General Expressions
 *
 */

static BBOX RenderExpression(SEXP expr, int draw, mathContext *mc,
			     pGEcontext gc, pGEDevDesc dd)
{
    BBOX bbox;
    if (NameAtom(CAR(expr)))
	bbox = RenderSymbolString(CAR(expr), draw, mc, gc, dd);
    else
	bbox = RenderElement(CAR(expr), draw, mc, gc, dd);
    bbox = RenderItalicCorr(bbox, draw, mc, gc, dd);
    bbox = CombineBBoxes(bbox, RenderDelimiter(S_PARENLEFT, draw, mc, gc, dd));
    bbox = CombineBBoxes(bbox, RenderCommaList(CDR(expr), draw, mc, gc, dd));
    bbox = RenderItalicCorr(bbox, draw, mc, gc, dd);
    bbox = CombineBBoxes(bbox, RenderDelimiter(S_PARENRIGHT, draw, mc, gc, dd));
    return bbox;
}

/*----------------------------------------------------------------------
 *
 *  Code for Comma Separated List Expressions
 *
 */

static int ListAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "list");
}

static BBOX RenderList(SEXP expr, int draw, mathContext *mc,
		       pGEcontext gc, pGEDevDesc dd)
{
    return RenderCommaList(CDR(expr), draw, mc, gc, dd);
}

/* Dispatching procedure which determines nature of expression. */


static BBOX RenderFormula(SEXP expr, int draw, mathContext *mc,
			  pGEcontext gc, pGEDevDesc dd)
{
    SEXP head = CAR(expr);

    if (SpaceAtom(head))
	return RenderSpace(expr, draw, mc, gc, dd);
    else if (BinAtom(head))
	return RenderBin(expr, draw, mc, gc, dd);
    else if (SuperAtom(head))
	return RenderSup(expr, draw, mc, gc, dd);
    else if (SubAtom(head))
	return RenderSub(expr, draw, mc, gc, dd);
    else if (WideTildeAtom(head))
	return RenderWideTilde(expr, draw, mc, gc, dd);
    else if (WideHatAtom(head))
	return RenderWideHat(expr, draw, mc, gc, dd);
    else if (BarAtom(head))
	return RenderBar(expr, draw, mc, gc, dd);
    else if (AccentAtom(head))
	return RenderAccent(expr, draw, mc, gc, dd);
    else if (OverAtom(head))
	return RenderOver(expr, draw, mc, gc, dd);
    else if (UnderlAtom(head))
	return RenderUnderl(expr, draw, mc, gc, dd);
    else if (AtopAtom(head))
	return RenderAtop(expr, draw, mc, gc, dd);
    else if (ParenAtom(head))
	return RenderParen(expr, draw, mc, gc, dd);
    else if (BGroupAtom(head))
	return RenderBGroup(expr, draw, mc, gc, dd);
    else if (GroupAtom(head))
	return RenderGroup(expr, draw, mc, gc, dd);
    else if (IntAtom(head))
	return RenderInt(expr, draw, mc, gc, dd);
    else if (OpAtom(head))
	return RenderOp(expr, draw, mc, gc, dd);
    else if (RadicalAtom(head))
	return RenderRadical(expr, draw, mc, gc, dd);
    else if (AbsAtom(head))
	return RenderAbs(expr, draw, mc, gc, dd);
    else if (CurlyAtom(head))
	return RenderCurly(expr, draw, mc, gc, dd);
    else if (RelAtom(head))
	return RenderRel(expr, draw, mc, gc, dd);
    else if (BoldAtom(head))
	return RenderBold(expr, draw, mc, gc, dd);
    else if (ItalicAtom(head))
	return RenderItalic(expr, draw, mc, gc, dd);
    else if (PlainAtom(head))
	return RenderPlain(expr, draw, mc, gc, dd);
    else if (SymbolFaceAtom(head))
	return RenderSymbolFace(expr, draw, mc, gc, dd);
    else if (BoldItalicAtom(head))
	return RenderBoldItalic(expr, draw, mc, gc, dd);
    else if (StyleAtom(head))
	return RenderStyle(expr, draw, mc, gc, dd);
    else if (PhantomAtom(head))
	return RenderPhantom(expr, draw, mc, gc, dd);
    else if (ConcatenateAtom(head))
	return RenderConcatenate(expr, draw, mc, gc, dd);
    else if (ListAtom(head))
	return RenderList(expr, draw, mc, gc, dd);
    else
	return RenderExpression(expr, draw, mc, gc, dd);
}


/* Dispatch on whether atom (symbol, string, number, ...) */
/* or formula (some sort of expression) */

static BBOX RenderElement(SEXP expr, int draw, mathContext *mc,
			  pGEcontext gc, pGEDevDesc dd)
{
    if (FormulaExpression(expr))
	return RenderFormula(expr, draw, mc, gc, dd);
    else
	return RenderAtom(expr, draw, mc, gc, dd);
}

static BBOX RenderOffsetElement(SEXP expr, double x, double y, int draw,
				mathContext *mc, pGEcontext gc,
				pGEDevDesc dd)
{
    BBOX bbox;
    double savedX = mc->CurrentX;
    double savedY = mc->CurrentY;
    if (draw) {
	mc->CurrentX += x;
	mc->CurrentY += y;
    }
    bbox = RenderElement(expr, draw, mc, gc, dd);
    bboxWidth(bbox) += x;
    bboxHeight(bbox) += y;
    bboxDepth(bbox) -= y;
    mc->CurrentX = savedX;
    mc->CurrentY = savedY;
    return bbox;

}

/* Functions forming the R API */

/* Calculate width of expression */
/* BBOXes are in INCHES (see MetricUnit) */

double GEExpressionWidth(SEXP expr,
			 pGEcontext gc,
			 pGEDevDesc dd)
{
    BBOX bbox;
    double width;

    /*
     * Build a "drawing context" for the current expression
     */
    mathContext mc;
    mc.BaseCex = gc->cex;
    mc.BoxColor = 4291543295U;  // name2col("pink");
    mc.CurrentStyle = STYLE_D;
    /*
     * Some "empty" values.  Will be filled in after BBox is calc'ed
     */
    mc.ReferenceX = 0;
    mc.ReferenceY = 0;
    mc.CurrentX = 0;
    mc.CurrentY = 0;
    mc.CurrentAngle = 0;
    mc.CosAngle = 0;
    mc.SinAngle = 0;

    SetFont(PlainFont, gc);
    bbox = RenderElement(expr, 0, &mc, gc, dd);
    width  = bboxWidth(bbox);
    /*
     * NOTE that we do fabs() here in case the device
     * runs right-to-left.
     * This is so that these calculations match those
     * for string widths and heights, where the width
     * and height of text is positive no matter how
     * the device drawing is oriented.
     */
    return fabs(toDeviceWidth(width, GE_INCHES, dd));
}

double GEExpressionHeight(SEXP expr,
			  pGEcontext gc,
			  pGEDevDesc dd)
{
    BBOX bbox;
    double height;

    /*
     * Build a "drawing context" for the current expression
     */
    mathContext mc;
    mc.BaseCex = gc->cex;
    mc.BoxColor = 4291543295U;  // name2col("pink");
    mc.CurrentStyle = STYLE_D;
    /*
     * Some "empty" values.  Will be filled in after BBox is calc'ed
     */
    mc.ReferenceX = 0;
    mc.ReferenceY = 0;
    mc.CurrentX = 0;
    mc.CurrentY = 0;
    mc.CurrentAngle = 0;
    mc.CosAngle = 0;
    mc.SinAngle = 0;

    SetFont(PlainFont, gc);
    bbox = RenderElement(expr, 0, &mc, gc, dd);
    height = bboxHeight(bbox) + bboxDepth(bbox);
    /* NOTE that we do fabs() here in case the device
     * draws top-to-bottom (like an X11 window).
     * This is so that these calculations match those
     * for string widths and heights, where the width
     * and height of text is positive no matter how
     * the device drawing is oriented.
     */
    return fabs(toDeviceHeight(height, GE_INCHES, dd));
}

void GEExpressionMetric(SEXP expr,
                        const pGEcontext gc,
                        double *ascent, double *descent, double *width,
                        pGEDevDesc dd)
{
    BBOX bbox;

    /*
     * Build a "drawing context" for the current expression
     */
    mathContext mc;
    mc.BaseCex = gc->cex;
    mc.BoxColor = 4291543295U;  // name2col("pink");
    mc.CurrentStyle = STYLE_D;
    /*
     * Some "empty" values.  Will be filled in after BBox is calc'ed
     */
    mc.ReferenceX = 0;
    mc.ReferenceY = 0;
    mc.CurrentX = 0;
    mc.CurrentY = 0;
    mc.CurrentAngle = 0;
    mc.CosAngle = 0;
    mc.SinAngle = 0;

    SetFont(PlainFont, gc);
    bbox = RenderElement(expr, 0, &mc, gc, dd);
    /* NOTE that we do fabs() here in case the device
     * draws top-to-bottom (like an X11 window).
     * This is so that these calculations match those
     * for string widths and heights, where the width
     * and height of text is positive no matter how
     * the device drawing is oriented.
     */
    *width = fabs(toDeviceWidth(bboxWidth(bbox), GE_INCHES, dd));
    *ascent = fabs(toDeviceHeight(bboxHeight(bbox), GE_INCHES, dd));
    *descent = fabs(toDeviceHeight(bboxDepth(bbox), GE_INCHES, dd));
}

void GEMathText(double x, double y, SEXP expr,
		double xc, double yc, double rot,
		pGEcontext gc,
		pGEDevDesc dd)
{
    BBOX bbox;
    mathContext mc;

    /* If font metric information is not available for device
       then bail out */
    double ascent, descent, width;
    GEMetricInfo('M', gc, &ascent, &descent, &width, dd);
    if ((ascent == 0.0) && (descent == 0.0) && (width == 0.0))
	error(_("Metric information not available for this family/device"));

    /*
     * Build a "drawing context" for the current expression
     */
    mc.BaseCex = gc->cex;
    mc.BoxColor = 4291543295U;  // name2col("pink");
    mc.CurrentStyle = STYLE_D;

    /*
     * Some "empty" values.  Will be filled in after BBox is calc'ed
     */
    mc.ReferenceX = 0;
    mc.ReferenceY = 0;
    mc.CurrentX = 0;
    mc.CurrentY = 0;
    mc.CurrentAngle = 0;
    mc.CosAngle = 0;
    mc.SinAngle = 0;

    SetFont(PlainFont, gc);
    bbox = RenderElement(expr, 0, &mc, gc, dd);
    mc.ReferenceX = fromDeviceX(x, GE_INCHES, dd);
    mc.ReferenceY = fromDeviceY(y, GE_INCHES, dd);
    if (R_FINITE(xc))
	mc.CurrentX = mc.ReferenceX - xc * bboxWidth(bbox);
    else
	/* Paul 2002-02-11
	 * If xc == NA then should centre horizontally.
	 * Used to left-adjust.
	 */
	mc.CurrentX = mc.ReferenceX - 0.5 * bboxWidth(bbox);
    if (R_FINITE(yc))
	mc.CurrentY = mc.ReferenceY + bboxDepth(bbox)
	    - yc * (bboxHeight(bbox) + bboxDepth(bbox));
    else
	/* Paul 11/2/02
	 * If xc == NA then should centre vertically.
	 * Used to bottom-adjust.
	 */
	mc.CurrentY = mc.ReferenceY + bboxDepth(bbox)
	    - 0.5 * (bboxHeight(bbox) + bboxDepth(bbox));
    mc.CurrentAngle = rot;
    rot *= M_PI_2 / 90 ;/* radians */
    mc.CosAngle = cos(rot);
    mc.SinAngle = sin(rot);
    RenderElement(expr, 1, &mc, gc, dd);
}/* GEMathText */
