/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997 Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2000	The R Development Core Team
 *
 *  This source code module:
 *  Copyright (C) 1997, 1998 Paul Murrell and Ross Ihaka
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

#include <ctype.h>

#include <Defn.h>
#include <Rmath.h>
#include <Graphics.h>


/* The R graphics device */

static DevDesc *MathDevice;
static unsigned int BoxColor;
static unsigned int TextColor;
static double BaseCex = 1;
static GUnit MetricUnit = INCHES;

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

static double ReferenceX;
static double ReferenceY;
static double CurrentX;
static double CurrentY;
static double CurrentAngle;
static double CosAngle;
static double SinAngle;


/* Convert CurrentX and CurrentY from */
/* 0 angle to and CurrentAngle */

static double ConvertedX()
{
    double rotatedX = ReferenceX +
	(CurrentX - ReferenceX) * CosAngle -
	(CurrentY - ReferenceY) * SinAngle;
    return rotatedX;
}

static double ConvertedY()
{
    double rotatedY = ReferenceY +
	(CurrentY - ReferenceY) * CosAngle +
	(CurrentX - ReferenceX) * SinAngle;
    return rotatedY;
}

static void PMoveAcross(double xamount)
{
    CurrentX += xamount;
}

static void PMoveUp(double yamount)
{
    CurrentY += yamount;
}

static void PMoveTo(double x, double y)
{
    CurrentX = x;
    CurrentY = y;
}

/* Basic Font Properties */

#ifdef OLD
static double FontHeight()
{
    double height, depth, width;
    GMetricInfo(0, &height, &depth, &width, MetricUnit, MathDevice);
    return height + depth;
}
#endif

static double xHeight()
{
    double height, depth, width;
    GMetricInfo('x', &height, &depth, &width, MetricUnit, MathDevice);
    return height;
}

static double XHeight()
{
    double height, depth, width;
    GMetricInfo('X', &height, &depth, &width, MetricUnit, MathDevice);
    return height;
}

static double AxisHeight()
{
    double height, depth, width;
    GMetricInfo('+', &height, &depth, &width, MetricUnit, MathDevice);
    return 0.5 * height;
}

static double Quad()
{
    double height, depth, width;
    GMetricInfo('M', &height, &depth, &width, MetricUnit, MathDevice);
    return width;
}

/* The height of digits */
static double FigHeight()
{
    double height, depth, width;
    GMetricInfo('0', &height, &depth, &width, MetricUnit, MathDevice);
    return height;
}

/* Depth of lower case descenders */
static double DescDepth()
{
    double height, depth, width;
    GMetricInfo('g', &height, &depth, &width, MetricUnit, MathDevice);
    return depth;
}

#ifdef NOT_used_currently/*-- out 'def'	 (-Wall) --*/
static double AscHeight()
{
    double height, depth, width, save;
    GMetricInfo('d', &height, &depth, &width, MetricUnit, MathDevice);
    save = height;
    GMetricInfo('a', &height, &depth, &width, MetricUnit, MathDevice);
    return save - height;
}
#endif
/* Thickness of rules */
static double RuleThickness()
{
    return 0.015;
}

static double ThinSpace()
{
    double height, depth, width;
    static double OneSixth = 0.16666666666666666666;
    GMetricInfo('M', &height, &depth, &width, MetricUnit, MathDevice);
    return OneSixth * width;
}

static double MediumSpace()
{
    double height, depth, width;
    static double TwoNinths = 0.22222222222222222222;
    GMetricInfo('M', &height, &depth, &width, MetricUnit, MathDevice);
    return TwoNinths * width;
}

static double ThickSpace()
{
    double height, depth, width;
    static double FiveEighteenths = 0.27777777777777777777;
    GMetricInfo('M', &height, &depth, &width, MetricUnit, MathDevice);
    return FiveEighteenths * width;
}

static double MuSpace()
{
    double height, depth, width;
    static double OneEighteenth = 0.05555555555555555555;
    GMetricInfo('M', &height, &depth, &width, MetricUnit, MathDevice);
    return OneEighteenth * width;
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

static double TeX(TEXPAR which)
{
    switch(which) {
    case sigma2:  /* space */
    case sigma5:  /* x_height */
	return xHeight();

    case sigma6:  /* quad */
	return Quad();

    case sigma8:  /* num1 */
	return AxisHeight()
	    + 3.51 * RuleThickness()
	    + 0.15 * XHeight()		/* 54/36 * 0.1 */
	    + SUBS * DescDepth();
    case sigma9:  /* num2 */
	return AxisHeight()
	    + 1.51 * RuleThickness()
	    + 0.08333333 * XHeight();	/* 30/36 * 0.1 */
    case sigma10: /* num3 */
	return AxisHeight()
	    + 1.51 * RuleThickness()
	    + 0.1333333 * XHeight();	/* 48/36 * 0.1 */
    case sigma11: /* denom1 */
	return	- AxisHeight()
	    + 3.51 * RuleThickness()
	    + SUBS * FigHeight()
	    + 0.344444 * XHeight();	/* 124/36 * 0.1 */
    case sigma12: /* denom2 */
	return	- AxisHeight()
	    + 1.51 * RuleThickness()
	    + SUBS * FigHeight()
	    + 0.08333333 * XHeight();	/* 30/36 * 0.1 */

    case sigma13: /* sup1 */
	return 0.95 * xHeight();
    case sigma14: /* sup2 */
	return 0.825 * xHeight();
    case sigma15: /* sup3 */
	return 0.7 * xHeight();

    case sigma16: /* sub1 */
	return 0.35 * xHeight();
    case sigma17: /* sub2 */
	return 0.45 * XHeight();

    case sigma18: /* sup_drop */
	return 0.3861111 * XHeight();

    case sigma19: /* sub_drop */
	return 0.05 * XHeight();

    case sigma20: /* delim1 */
	return 2.39 * XHeight();
    case sigma21: /* delim2 */
	return 1.01 *XHeight();

    case sigma22: /* axis_height */
	return AxisHeight();

    case xi8:	  /* default_rule_thickness */
	return RuleThickness();

    case xi9:	  /* big_op_spacing1 */
    case xi10:	  /* big_op_spacing2 */
    case xi11:	  /* big_op_spacing3 */
    case xi12:	  /* big_op_spacing4 */
    case xi13:	  /* big_op_spacing5 */
	return 0.15 * XHeight();
    default:/* never happens (enum type) */
	error("invalid `which' in TeX()!"); return 0;/*-Wall*/
    }
}

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

static STYLE CurrentStyle;

static STYLE GetStyle()
{
    return CurrentStyle;
}

static void SetStyle(STYLE newstyle)
{
    switch (newstyle) {
    case STYLE_D:
    case STYLE_T:
    case STYLE_D1:
    case STYLE_T1:
	MathDevice->gp.cex = 1.0 * BaseCex;
	break;
    case STYLE_S:
    case STYLE_S1:
	MathDevice->gp.cex = 0.7 * BaseCex;
	break;
    case STYLE_SS:
    case STYLE_SS1:
	MathDevice->gp.cex = 0.5 * BaseCex;
	break;
    default:
	error("invalid math style encountered");
    }
    CurrentStyle = newstyle;
}

static void SetPrimeStyle(STYLE style)
{
    switch (style) {
    case STYLE_D:
    case STYLE_D1:
	SetStyle(STYLE_D1);
	break;
    case STYLE_T:
    case STYLE_T1:
	SetStyle(STYLE_T1);
	break;
    case STYLE_S:
    case STYLE_S1:
	SetStyle(STYLE_S1);
	break;
    case STYLE_SS:
    case STYLE_SS1:
	SetStyle(STYLE_SS1);
	break;
    }
}

static void SetSupStyle(STYLE style)
{
    switch (style) {
    case STYLE_D:
    case STYLE_T:
	SetStyle(STYLE_S);
	break;
    case STYLE_D1:
    case STYLE_T1:
	SetStyle(STYLE_S1);
	break;
    case STYLE_S:
    case STYLE_SS:
	SetStyle(STYLE_SS);
	break;
    case STYLE_S1:
    case STYLE_SS1:
	SetStyle(STYLE_SS1);
	break;
    }
}

static void SetSubStyle(STYLE style)
{
    switch (style) {
    case STYLE_D:
    case STYLE_T:
    case STYLE_D1:
    case STYLE_T1:
	SetStyle(STYLE_S1);
	break;
    case STYLE_S:
    case STYLE_SS:
    case STYLE_S1:
    case STYLE_SS1:
	SetStyle(STYLE_SS1);
	break;
    }
}

static void SetNumStyle(STYLE style)
{
    switch (style) {
    case STYLE_D:
	SetStyle(STYLE_T);
	break;
    case STYLE_D1:
	SetStyle(STYLE_T1);
	break;
    default:
	SetSupStyle(style);
    }
}

static void SetDenomStyle(STYLE style)
{
    if (style > STYLE_T)
	SetStyle(STYLE_T1);
    else
	SetSubStyle(style);
}

static int IsCompactStyle(STYLE style)
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

static BBOX NullBBox()
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

#ifdef NOT_used_currently/*-- out 'def'	 (-Wall) --*/
static BBOX DrawBBox(BBOX bbox, double xoffset, double yoffset)
{
    double xsaved = CurrentX;
    double ysaved = CurrentY;
    double x[5], y[5];
    CurrentX += xoffset;
    CurrentY += yoffset;
    MathDevice->gp.col = BoxColor;
    PMoveUp(-bboxDepth(bbox));
    x[4] = x[0] = ConvertedX();
    y[4] = y[0] = ConvertedY();
    PMoveAcross(bboxWidth(bbox));
    x[1] = ConvertedX();
    y[1] = ConvertedY();
    PMoveUp(bboxHeight(bbox) + bboxDepth(bbox));
    x[2] = ConvertedX();
    y[2] = ConvertedY();
    PMoveAcross(-bboxWidth(bbox));
    x[3] = ConvertedX();
    y[3] = ConvertedY();
    GPolyline(5, x, y, INCHES, MathDevice);
    PMoveTo(xsaved, ysaved);
    MathDevice->gp.col = TextColor;
    return bbox;
}
#endif

typedef struct {
    char *name;
    int code;
} SymTab;

/* Determine a match between symbol name and string. */

static int NameMatch(SEXP expr, char *aString)
{
    if (!isSymbol(expr)) return 0;
    return !strcmp(CHAR(PRINTNAME(expr)), aString);
}

static int StringMatch(SEXP expr, char *aString)
{
    return !strcmp(CHAR(STRING_ELT(expr, 0)), aString);
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
    { "gradient",	209 },
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

static int TranslatedSymbol(SEXP expr)
{
    int code = SymbolCode(expr);
    if ((0101 <= code && code <= 0132)	||   /* Greek */
	(0141 <= code && code <= 0172)	||   /* Greek */
	code == 0241			||   /* Upsilon1 */
	code == 0242			||   /* minute */
	code == 0245			||   /* infinity */
	code == 0260			||   /* degree */
	code == 0262			||   /* second */
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

#ifdef NOT_used_currently/*-- out 'def'	 (-Wall) --*/
static int symbolAtom(SEXP expr)
{
    int i;
    if (NameAtom(expr)) {
	for (i = 0; SymbolTable[i].code; i++)
	    if (NameMatch(expr, SymbolTable[i].name))
		return 1;
    }
    return 0;
}
#endif
/* Code to determine a font from the */
/* nature of the expression */

#ifdef NOT_used_currently/*-- out 'def'	 (-Wall) --*/
static FontType CurrentFont = 3;
#endif
static FontType GetFont()
{
    return MathDevice->gp.font;
}

static FontType SetFont(FontType font)
{
    FontType prevfont = MathDevice->gp.font;
    MathDevice->gp.font = font;
    return prevfont;
}

static int UsingItalics()
{
    return (MathDevice->gp.font == ItalicFont ||
	    MathDevice->gp.font == BoldItalicFont);
}

static BBOX GlyphBBox(int chr)
{
    BBOX bbox;
    double height, depth, width;
    GMetricInfo(chr, &height, &depth, &width, MetricUnit, MathDevice);
    bboxHeight(bbox) = height;
    bboxDepth(bbox)  = depth;
    bboxWidth(bbox)  = width;
    bboxItalic(bbox) = 0;
    bboxSimple(bbox) = 1;
    return bbox;
}

static BBOX RenderElement(SEXP, int);
static BBOX RenderOffsetElement(SEXP, double, double, int);
static BBOX RenderExpression(SEXP, int);
static BBOX RenderSymbolChar(int, int);


/*  Code to Generate Bounding Boxes and Draw Formulae.	*/

static BBOX RenderItalicCorr(BBOX bbox, int draw)
{
    if (bboxItalic(bbox) > 0) {
	if (draw)
	    PMoveAcross(bboxItalic(bbox));
	bboxWidth(bbox) += bboxItalic(bbox);
	bboxItalic(bbox) = 0;
    }
    return bbox;
}

static BBOX RenderGap(double gap, int draw)
{
    if (draw)
	PMoveAcross(gap);
    return MakeBBox(0, 0, gap);
}

/* Draw a Symbol from the Special Font */

static BBOX RenderSymbolChar(int ascii, int draw)
{
    FontType prev;
    BBOX bbox;
    char asciiStr[2];
    if (ascii == A_HAT || ascii == A_TILDE)
	prev = SetFont(PlainFont);
    else
	prev = SetFont(SymbolFont);
    bbox = GlyphBBox(ascii);
    if (draw) {
	asciiStr[0] = ascii;
	asciiStr[1] = '\0';
	GText(ConvertedX(), ConvertedY(), INCHES, asciiStr,
	      0.0, 0.0, CurrentAngle, MathDevice);
	PMoveAcross(bboxWidth(bbox));
    }
    SetFont(prev);
    return bbox;
}

/* Draw a Symbol String in "Math Mode */
/* This code inserts italic corrections after */
/* every character. */

static BBOX RenderSymbolStr(char *str, int draw)
{
    char chr[2];
    BBOX glyphBBox;
    BBOX resultBBox = NullBBox();
    double lastItalicCorr = 0;
    FontType prevfont = GetFont();
    FontType font = prevfont;
    chr[1] = '\0';
    if (str) {
	char *s = str;
	while (*s) {
	    if (isdigit((int)*s) && font != PlainFont) {
		font = PlainFont;
		SetFont(PlainFont);
	    }
	    else if (font != prevfont) {
		font = prevfont;
		SetFont(prevfont);
	    }
	    glyphBBox = GlyphBBox(*s);
	    if (UsingItalics())
		bboxItalic(glyphBBox) = ItalicFactor * bboxHeight(glyphBBox);
	    else
		bboxItalic(glyphBBox) = 0;
	    if (draw) {
		chr[0] = *s;
		PMoveAcross(lastItalicCorr);
		GText(ConvertedX(), ConvertedY(), INCHES, chr,
		      0.0, 0.0, CurrentAngle, MathDevice);
		PMoveAcross(bboxWidth(glyphBBox));
	    }
	    bboxWidth(resultBBox) += lastItalicCorr;
	    resultBBox = CombineBBoxes(resultBBox, glyphBBox);
	    lastItalicCorr = bboxItalic(glyphBBox);
	    s++;
	}
	if (font != prevfont)
	    SetFont(prevfont);
    }
    bboxSimple(resultBBox) = 1;
    return resultBBox;
}

/* Code for Character String Atoms. */

static BBOX RenderChar(int ascii, int draw)
{
    BBOX bbox;
    char asciiStr[2];
    bbox = GlyphBBox(ascii);
    if (draw) {
	asciiStr[0] = ascii;
	asciiStr[1] = '\0';
	GText(ConvertedX(), ConvertedY(), INCHES, asciiStr,
	      0.0, 0.0, CurrentAngle, MathDevice);
	PMoveAcross(bboxWidth(bbox));
    }
    return bbox;
}

static BBOX RenderStr(char *str, int draw)
{
    BBOX glyphBBox;
    BBOX resultBBox = NullBBox();
    if (str) {
	char *s = str;
	while (*s) {
	    glyphBBox = GlyphBBox(*s);
	    resultBBox = CombineBBoxes(resultBBox, glyphBBox);
	    s++;
	}
	if (draw) {
	    GText(ConvertedX(), ConvertedY(), INCHES, str,
		  0.0, 0.0, CurrentAngle, MathDevice);
	    PMoveAcross(bboxWidth(resultBBox));
	}
	if (UsingItalics())
	    bboxItalic(resultBBox) = ItalicFactor * bboxHeight(glyphBBox);
	else
	    bboxItalic(resultBBox) = 0;
    }
    bboxSimple(resultBBox) = 1;
    return resultBBox;
}


/* Code for Symbol Font Atoms */

static BBOX RenderSymbol(SEXP expr, int draw)
{
    int code;
    if ((code = TranslatedSymbol(expr)))
	return RenderSymbolChar(code, draw);
    else
	return RenderSymbolStr(CHAR(PRINTNAME(expr)), draw);
}

static BBOX RenderSymbolString(SEXP expr, int draw)
{
    int code;
    if ((code = TranslatedSymbol(expr)))
	return RenderSymbolChar(code, draw);
    else
	return RenderStr(CHAR(PRINTNAME(expr)), draw);
}


/* Code for Numeric Atoms */

static BBOX RenderNumber(SEXP expr, int draw)
{
    BBOX bbox;
    FontType prevfont = SetFont(PlainFont);
    bbox = RenderStr(CHAR(asChar(expr)), draw);
    SetFont(prevfont);
    return bbox;
}

/* Code for String Atoms */

static BBOX RenderString(SEXP expr, int draw)
{
    return RenderStr(CHAR(STRING_ELT(expr, 0)), draw);
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

static BBOX RenderDots(SEXP expr, int draw)
{
    BBOX bbox = RenderSymbolChar(S_ELLIPSIS, 0);
    if (NameMatch(expr, "cdots") || NameMatch(expr, "...")) {
	double shift = AxisHeight() - 0.5 * bboxHeight(bbox);
	if (draw) {
	    PMoveUp(shift);
	    RenderSymbolChar(S_ELLIPSIS, 1);
	    PMoveUp(-shift);
	}
	return ShiftBBox(bbox, shift);
    }
    else {
	if (draw)
	    RenderSymbolChar(S_ELLIPSIS, 1);
	return bbox;
    }
}

/*----------------------------------------------------------------------
 *
 *  Code for Atoms
 *
 */

static BBOX RenderAtom(SEXP expr, int draw)
{
    if (NameAtom(expr)) {
	if (DotsAtom(expr))
	    return RenderDots(expr, draw);
	else
	    return RenderSymbol(expr, draw);
    }
    else if (NumberAtom(expr))
	return RenderNumber(expr, draw);
    else if (StringAtom(expr))
	return RenderString(expr, draw);

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


static BBOX RenderSpace(SEXP expr, int draw)
{

    BBOX opBBox, arg1BBox, arg2BBox;
    int nexpr = length(expr);

    if (nexpr == 2) {
	opBBox = RenderSymbolChar(' ', draw);
	arg1BBox = RenderElement(CADR(expr), draw);
	return CombineBBoxes(opBBox, arg1BBox);
    }
    else if (nexpr == 3) {
	arg1BBox = RenderElement(CADR(expr), draw);
	opBBox = RenderSymbolChar(' ', draw);
	arg2BBox = RenderElement(CADDR(expr), draw);
	opBBox = CombineBBoxes(arg1BBox, opBBox);
	opBBox = CombineBBoxes(opBBox, arg2BBox);
	return opBBox;
    }
    else
	error("invalid mathematical annotation");

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

#define SLASH2

static BBOX RenderSlash(int draw)
{
#ifdef SLASH0
    /* The Default Font Character */
    return RenderSymbolChar(S_SLASH, draw);
#endif
#ifdef SLASH1
    /* Symbol Magnify Version */
    double savecex = MathDevice->gp.cex;
    BBOX bbox;
    double height1, height2;
    height1 = bboxHeight(RenderSymbolChar(S_SLASH, 0));
    MathDevice->gp.cex = 1.2 * MathDevice->gp.cex;
    height2 = bboxHeight(RenderSymbolChar(S_SLASH, 0));
    if (draw)
	PMoveUp(- 0.5 * (height2 - height1));
    bbox = RenderSymbolChar(S_SLASH, draw);
    if (draw)
	PMoveUp(0.5 * (height2 - height1));
    MathDevice->gp.cex = savecex;
    return bbox;
#endif
#ifdef SLASH2
    /* Line Drawing Version */
    double x[2], y[2];
    double depth = 0.5 * TeX(sigma22);
    double height = XHeight() + 0.5 * TeX(sigma22);
    double width = 0.5 * xHeight();
    if (draw) {
	PMoveAcross(0.5 * width);
	PMoveUp(-depth);
	x[0] = ConvertedX();
	y[0] = ConvertedY();
	PMoveAcross(width);
	PMoveUp(depth + height);
	x[1] = ConvertedX();
	y[1] = ConvertedY();
	PMoveUp(-height);
	GPolyline(2, x, y, INCHES, MathDevice);
	PMoveAcross(0.5 * width);
    }
    return MakeBBox(height, depth, 2 * width);
#endif
#ifdef SLASH3
    /* Offset Overprinting - A Failure! */
    BBOX slashBBox = RenderSymbolChar(S_SLASH, 0);
    BBOX ansBBox;
    double height = bboxHeight(slashBBox);
    double depth = bboxDepth(slashBBox);
    double width = bboxWidth(slashBBox);
    double slope = (height + depth) / slope;
    double delta = TeX(sigma22);
    if (draw)
	PMoveUp(-delta);
    ansBBox = ShiftBBox(RenderSymbolChar(S_SLASH, draw), -delta);
    PMoveUp(2 * delta);
    ansBBox = CombineBBoxes(ansBBox, RenderGap(2 * delta / slope, draw));
    ansBBox = ShiftBBox(RenderSymbolChar(S_SLASH, draw), 2 * delta);
    PMoveUp(-delta);
    return ansBBox;
#endif
}

static BBOX RenderBin(SEXP expr, int draw)
{
    int op = BinAtom(CAR(expr));
    int nexpr = length(expr);
    BBOX bbox;
    double gap;

    if(nexpr == 3) {
	if (op == S_ASTERISKMATH) {
	    bbox = RenderElement(CADR(expr), draw);
	    bbox = RenderItalicCorr(bbox, draw);
	    return CombineBBoxes(bbox, RenderElement(CADDR(expr), draw));
	}
	else if (op == S_SLASH) {
	    gap = 0;
	    bbox = RenderElement(CADR(expr), draw);
	    bbox = RenderItalicCorr(bbox, draw);
	    bbox = CombineBBoxes(bbox, RenderGap(gap, draw));
	    bbox = CombineBBoxes(bbox, RenderSlash(draw));
	    bbox = CombineBBoxes(bbox, RenderGap(gap, draw));
	    return CombineBBoxes(bbox, RenderElement(CADDR(expr), draw));
	}
	else {
	    gap = (CurrentStyle > STYLE_S) ? MediumSpace() : 0;
	    bbox = RenderElement(CADR(expr), draw);
	    bbox = RenderItalicCorr(bbox, draw);
	    bbox = CombineBBoxes(bbox, RenderGap(gap, draw));
	    bbox = CombineBBoxes(bbox, RenderSymbolChar(op, draw));
	    bbox = CombineBBoxes(bbox, RenderGap(gap, draw));
	    return CombineBBoxes(bbox, RenderElement(CADDR(expr), draw));
	}
    }
    else if(nexpr == 2) {
	gap = (CurrentStyle > STYLE_S) ? ThinSpace() : 0;
	bbox = RenderSymbolChar(op, draw);
	bbox = CombineBBoxes(bbox, RenderGap(gap, draw));
	return CombineBBoxes(bbox, RenderElement(CADR(expr), draw));
    }
    else
	error("invalid mathematical annotation");

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
static BBOX RenderSub(SEXP expr, int draw)
{
    BBOX bodyBBox, subBBox;
    SEXP body = CADR(expr);
    SEXP sub = CADDR(expr);
    STYLE style = GetStyle();
    double savedX = CurrentX;
    double savedY = CurrentY;
    double v, s5, s16;
    bodyBBox = RenderElement(body, draw);
    bodyBBox = RenderItalicCorr(bodyBBox, draw);
    v = bboxSimple(bodyBBox) ? 0 : bboxDepth(bodyBBox) + TeX(sigma19);
    s5 = TeX(sigma5);
    s16 = TeX(sigma16);
    SetSubStyle(style);
    subBBox = RenderElement(sub, 0);
    v = max(max(v, s16), bboxHeight(subBBox) - 0.8 * sigma5);
    subBBox = RenderOffsetElement(sub, 0, -v, draw);
    bodyBBox = CombineBBoxes(bodyBBox, subBBox);
    SetStyle(style);
    if (draw)
	PMoveTo(savedX + bboxWidth(bodyBBox), savedY);
    return bodyBBox;
}

static BBOX RenderSup(SEXP expr, int draw)
{
    BBOX bodyBBox, subBBox, supBBox;
    SEXP body = CADR(expr);
    SEXP sup = CADDR(expr);
    SEXP sub = R_NilValue;	/* -Wall */
    STYLE style = GetStyle();
    double savedX = CurrentX;
    double savedY = CurrentY;
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
    bodyBBox = RenderElement(body, draw);
    delta = bboxItalic(bodyBBox);
    bodyBBox = RenderItalicCorr(bodyBBox, draw);
    width = bboxWidth(bodyBBox);
    if (bboxSimple(bodyBBox)) {
	u = 0;
	v = 0;
    }
    else {
	u = bboxHeight(bodyBBox) - TeX(sigma18);
	v = bboxDepth(bodyBBox) + TeX(sigma19);
    }
    theta = TeX(xi8);
    s5 = TeX(sigma5);
    s17 = TeX(sigma17);
    if (style == STYLE_D)
	p = TeX(sigma13);
    else if (IsCompactStyle(style))
	p = TeX(sigma15);
    else
	p = TeX(sigma14);
    SetSupStyle(style);
    supBBox = RenderElement(sup, 0);
    u = max(max(u, p), bboxDepth(supBBox) + 0.25 * s5);

    if (haveSub) {
	SetSubStyle(style);
	subBBox = RenderElement(sub, 0);
	v = max(v, s17);
	if ((u - bboxDepth(supBBox)) - (bboxHeight(subBBox) - v) < 4 * theta) {
	    double psi = 0.8 * s5 - (u - bboxDepth(supBBox));
	    if (psi > 0) {
		u += psi;
		v -= psi;
	    }
	}
	if (draw)
	    PMoveTo(savedX, savedY);
	subBBox = RenderOffsetElement(sub, width, -v, draw);
	if (draw)
	    PMoveTo(savedX, savedY);
	SetSupStyle(style);
	supBBox = RenderOffsetElement(sup, width + delta, u, draw);
	bodyBBox = CombineAlignedBBoxes(bodyBBox, subBBox);
	bodyBBox = CombineAlignedBBoxes(bodyBBox, supBBox);
    }
    else {
	supBBox = RenderOffsetElement(sup, 0, u, draw);
	bodyBBox = CombineBBoxes(bodyBBox, supBBox);
    }
    if (draw)
	PMoveTo(savedX + bboxWidth(bodyBBox), savedY);
    SetStyle(style);
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

static BBOX RenderWideTilde(SEXP expr, int draw)
{
    double savedX = CurrentX;
    double savedY = CurrentY;
    BBOX bbox = RenderElement(CADR(expr), draw);
    double height = bboxHeight(bbox);
    /*double width = bboxWidth(bbox);*/
    double totalwidth = bboxWidth(bbox) + bboxItalic(bbox);
    double delta = totalwidth * (1 - 2 * DELTA) / NTILDE;
    double start = DELTA * totalwidth;
    double accentGap = ACCENT_GAP * XHeight();
    double hatHeight = 0.5 * HAT_HEIGHT * XHeight();
    double c = 8 * atan(1.0) / NTILDE;
    double x[NTILDE + 3], y[NTILDE + 3];
    double baseX, baseY, xval, yval;
    int i;

    if (draw) {
	baseX = savedX;
	baseY = savedY + height + accentGap;
	PMoveTo(baseX, baseY);
	x[0] = ConvertedX();
	y[0] = ConvertedY();
	for (i = 0; i <= NTILDE; i++) {
	    xval = start + i * delta;
	    yval = 0.5 * hatHeight * (sin(c * i) + 1);
	    PMoveTo(baseX + xval, baseY + yval);
	    x[i + 1] = ConvertedX();
	    y[i + 1] = ConvertedY();
	}
	PMoveTo(baseX + totalwidth, baseY + hatHeight);
	x[NTILDE + 2] = ConvertedX();
	y[NTILDE + 2] = ConvertedY();
	GPolyline(NTILDE + 3, x, y, INCHES, MathDevice);
	PMoveTo(savedX + totalwidth, savedY);
    }
    return MakeBBox(height + accentGap + hatHeight,
		    bboxDepth(bbox), totalwidth);
}

static int WideHatAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "widehat");
}

static BBOX RenderWideHat(SEXP expr, int draw)
{
    double savedX = CurrentX;
    double savedY = CurrentY;
    BBOX bbox = RenderElement(CADR(expr), draw);
    double accentGap = ACCENT_GAP * XHeight();
    double hatHeight = HAT_HEIGHT * XHeight();
    double totalwidth = bboxWidth(bbox) + bboxItalic(bbox);
    double height = bboxHeight(bbox);
    double width = bboxWidth(bbox);
    double x[3], y[3];

    if (draw) {
	PMoveTo(savedX, savedY + height + accentGap);
	x[0] = ConvertedX();
	y[0] = ConvertedY();
	PMoveAcross(0.5 * totalwidth);
	PMoveUp(hatHeight);
	x[1] = ConvertedX();
	y[1] = ConvertedY();
	PMoveAcross(0.5 * totalwidth);
	PMoveUp(-hatHeight);
	x[2] = ConvertedX();
	y[2] = ConvertedY();
	GPolyline(3, x, y, INCHES, MathDevice);
	PMoveTo(savedX + width, savedY);
    }
    return EnlargeBBox(bbox, accentGap + hatHeight, 0, 0);
}

static int BarAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "bar");
}

static BBOX RenderBar(SEXP expr, int draw)
{
    double savedX = CurrentX;
    double savedY = CurrentY;
    BBOX bbox = RenderElement(CADR(expr), draw);
    double accentGap = ACCENT_GAP * XHeight();
    /*double hatHeight = HAT_HEIGHT * XHeight();*/
    double height = bboxHeight(bbox);
    double width = bboxWidth(bbox);
    double offset = bboxItalic(bbox);
    double x[2], y[2];

    if (draw) {
	PMoveTo(savedX + offset, savedY + height + accentGap);
	x[0] = ConvertedX();
	y[0] = ConvertedY();
	PMoveAcross(width);
	x[1] = ConvertedX();
	y[1] = ConvertedY();
	GPolyline(2, x, y, INCHES, MathDevice);
	PMoveTo(savedX + width, savedY);
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
    errorcall(expr, "invalid accent");
}

static BBOX RenderAccent(SEXP expr, int draw)
{
    SEXP body, accent;
    double savedX = CurrentX;
    double savedY = CurrentY;
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
    bodyBBox = RenderElement(body, 0);
    italic = bboxItalic(bodyBBox);
    accentBBox = RenderChar(code, 0);
    width = max(bboxWidth(bodyBBox) + bboxItalic(bodyBBox),
		bboxWidth(accentBBox));
    xoffset = 0.5 *(width - bboxWidth(bodyBBox));
    bodyBBox = RenderGap(xoffset, draw);
    bodyBBox = CombineBBoxes(bodyBBox, RenderElement(body, draw));
    bodyBBox = CombineBBoxes(bodyBBox, RenderGap(xoffset, draw));
    PMoveTo(savedX, savedY);
    xoffset = 0.5 *(width - bboxWidth(accentBBox))
	+ 0.9 * italic;
    yoffset = bboxHeight(bodyBBox) + bboxDepth(accentBBox) + 0.1 * XHeight();
    if (draw) {
	PMoveTo(savedX + xoffset, savedY + yoffset);
	RenderChar(code, draw);
    }
    bodyBBox = CombineOffsetBBoxes(bodyBBox, 0, accentBBox, 0,
				   xoffset, yoffset);
    PMoveTo(savedX + width, savedY);
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
			     double *u, double *v)
{
    double a, delta, phi, theta;
    a = TeX(sigma22);
    theta = TeX(xi8);
    if(CurrentStyle > STYLE_T) {
	*u = TeX(sigma8);
	*v = TeX(sigma11);
	phi = 3 * theta;
    }
    else {
	*u = TeX(sigma9);
	*v = TeX(sigma12);
	phi = theta;
    }
    delta = (*u - bboxDepth(numBBox)) - (a + 0.5 * theta);
    if (delta < phi)
	*u += (phi - delta) + theta;
    delta = (a + 0.5 * theta) - (bboxHeight(denomBBox) - *v);
    if (delta < phi)
	*v += (phi - delta) + theta;
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

static BBOX RenderFraction(SEXP expr, int rule, int draw)
{
    SEXP numerator = CADR(expr);
    SEXP denominator = CADDR(expr);
    BBOX numBBox, denomBBox;
    double nHShift, dHShift;
    double nVShift, dVShift;
    double width, x[2], y[2];
    double savedX = CurrentX;
    double savedY = CurrentY;
    STYLE style;

    style = GetStyle();
    SetNumStyle(style);
    numBBox = RenderItalicCorr(RenderElement(numerator, 0), 0);
    SetDenomStyle(style);
    denomBBox = RenderItalicCorr(RenderElement(denominator, 0), 0);
    SetStyle(style);

    width = max(bboxWidth(numBBox), bboxWidth(denomBBox));
    NumDenomHShift(numBBox, denomBBox, &nHShift, &dHShift);
    NumDenomVShift(numBBox, denomBBox, &nVShift, &dVShift);

    CurrentX = savedX;
    CurrentY = savedY;
    SetNumStyle(style);
    numBBox = RenderOffsetElement(numerator, nHShift, nVShift, draw);

    CurrentX = savedX;
    CurrentY = savedY;
    SetDenomStyle(style);
    denomBBox = RenderOffsetElement(denominator, dHShift, -dVShift, draw);

    SetStyle(style);

    if (draw) {
	if (rule) {
	    CurrentX = savedX;
	    CurrentY = savedY;
	    PMoveUp(AxisHeight());
	    x[0] = ConvertedX();
	    y[0] = ConvertedY();
	    PMoveAcross(width);
	    x[1] = ConvertedX();
	    y[1] = ConvertedY();
	    GPolyline(2, x, y, INCHES, MathDevice);
	    PMoveUp(-AxisHeight());
	}
	PMoveTo(savedX + width, savedY);
    }
    return CombineAlignedBBoxes(numBBox, denomBBox);
}

static int OverAtom(SEXP expr)
{
    return NameAtom(expr) &&
	(NameMatch(expr, "over") || NameMatch(expr, "frac"));
}

static BBOX RenderOver(SEXP expr, int draw)
{
    return RenderFraction(expr, 1, draw);
}

static int AtopAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "atop");
}

static BBOX RenderAtop(SEXP expr, int draw)
{
    return RenderFraction(expr, 0, draw);
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
	errorcall(expr, "invalid group delimiter");
    return code;
}

static BBOX RenderDelimiter(int delim, int draw)
{
    BBOX bbox;
    double savecex = MathDevice->gp.cex;
    MathDevice->gp.cex = DelimSymbolMag * MathDevice->gp.cex;
    bbox = RenderSymbolChar(delim, draw);
    MathDevice->gp.cex = savecex;
    return bbox;
}

static int GroupAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "group");
}

static BBOX RenderGroup(SEXP expr, int draw)
{
    double cexSaved = MathDevice->gp.cex;
    BBOX bbox;
    int code;
    if (length(expr) != 4)
	errorcall(expr, "invalid group specification");
    bbox = NullBBox();
    code = DelimCode(expr, CADR(expr));
    MathDevice->gp.cex = DelimSymbolMag * MathDevice->gp.cex;
    if (code == 2) {
	bbox = RenderSymbolChar('|', draw);
	bbox = RenderSymbolChar('|', draw);
    }
    else if (code != '.')
	bbox = RenderSymbolChar(code, draw);
    MathDevice->gp.cex = cexSaved;
    bbox = CombineBBoxes(bbox, RenderElement(CADDR(expr), draw));
    bbox = RenderItalicCorr(bbox, draw);
    code = DelimCode(expr, CADDDR(expr));
    MathDevice->gp.cex = DelimSymbolMag * MathDevice->gp.cex;
    if (code == 2) {
	bbox = CombineBBoxes(bbox, RenderSymbolChar('|', draw));
	bbox = CombineBBoxes(bbox, RenderSymbolChar('|', draw));
    }
    else if (code != '.')
	bbox = CombineBBoxes(bbox, RenderSymbolChar(code, draw));
    MathDevice->gp.cex = cexSaved;
    return bbox;
}

static int BGroupAtom(SEXP expr)
{
    return NameAtom(expr) && NameMatch(expr, "bgroup");
}

static BBOX RenderDelim(int which, double dist, int draw)
{
    double savedX = CurrentX;
    double savedY = CurrentY;
    FontType prev = SetFont(SymbolFont);
    BBOX ansBBox, topBBox, botBBox, extBBox, midBBox;
    int top, bot, ext, mid;
    int i, n;
    double topShift, botShift, extShift, midShift;
    double ytop, ybot, extHeight, delta;
    double axisHeight = TeX(sigma22);

    switch(which) {
    case '.':
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
	error("group is incomplete");
	return ansBBox;/*never reached*/
    }
    topBBox = GlyphBBox(top);
    extBBox = GlyphBBox(ext);
    botBBox = GlyphBBox(bot);
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
	midBBox = GlyphBBox(mid);
	midShift = axisHeight
	    - 0.5 * (bboxHeight(midBBox) - bboxDepth(midBBox));
	midBBox = ShiftBBox(midBBox, midShift);
	ansBBox = CombineAlignedBBoxes(ansBBox, midBBox);
	if (draw) {
	    PMoveTo(savedX, savedY + topShift);
	    RenderSymbolChar(top, draw);
	    PMoveTo(savedX, savedY + midShift);
	    RenderSymbolChar(mid, draw);
	    PMoveTo(savedX, savedY - botShift);
	    RenderSymbolChar(bot, draw);
	    PMoveTo(savedX + bboxWidth(ansBBox), savedY);
	}
    }
    else {
	if (draw) {
	    /* draw the top and bottom elements */
	    PMoveTo(savedX, savedY + topShift);
	    RenderSymbolChar(top, draw);
	    PMoveTo(savedX, savedY - botShift);
	    RenderSymbolChar(bot, draw);
	    /* now join with extenders */
	    ytop = axisHeight + dist
		- (bboxHeight(topBBox) + bboxDepth(topBBox));
	    ybot = axisHeight - dist
		+ (bboxHeight(botBBox) + bboxDepth(botBBox));
	    n = ceil((ytop - ybot) / (0.99 * extHeight));
	    if (n > 0) {
		delta = (ytop - ybot) / n;
		for (i = 0; i < n; i++) {
		    PMoveTo(savedX, savedY + ybot + (i + 0.5) * delta - extShift);
		    RenderSymbolChar(ext, draw);
		}
	    }
	    PMoveTo(savedX + bboxWidth(ansBBox), savedY);

	}
    }
    SetFont(prev);
    return ansBBox;
}

static BBOX RenderBGroup(SEXP expr, int draw)
{
    double dist;
    BBOX bbox;
    double axisHeight = TeX(sigma22);
    double extra = 0.2 * xHeight();
    int delim1, delim2;
    if (length(expr) != 4)
	errorcall(expr, "invalid group specification");
    bbox = NullBBox();
    delim1 = DelimCode(expr, CADR(expr));
    delim2 = DelimCode(expr, CADDDR(expr));
    bbox = RenderElement(CADDR(expr), 0);
    dist = max(bboxHeight(bbox) - axisHeight, bboxDepth(bbox) + axisHeight);
    bbox = RenderDelim(delim1, dist + extra, draw);
    bbox = CombineBBoxes(bbox,	RenderElement(CADDR(expr), draw));
    bbox = RenderItalicCorr(bbox, draw);
    bbox = CombineBBoxes(bbox,	RenderDelim(delim2, dist + extra, draw));
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

static BBOX RenderParen(SEXP expr, int draw)
{
    BBOX bbox;
    bbox = RenderDelimiter(S_PARENLEFT, draw);
    bbox = CombineBBoxes(bbox, RenderElement(CADR(expr), draw));
    bbox = RenderItalicCorr(bbox, draw);
    return CombineBBoxes(bbox, RenderDelimiter(S_PARENRIGHT, draw));
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


static BBOX RenderIntSymbol(int draw)
{
    double savedX = CurrentX;
    double savedY = CurrentY;
    if (GetStyle() > STYLE_T) {
	BBOX bbox1 = RenderSymbolChar(243, 0);
	BBOX bbox2 = RenderSymbolChar(245, 0);
	double shift;
	shift = TeX(sigma22) + 0.99 * bboxDepth(bbox1);
	PMoveUp(shift);
	bbox1 = ShiftBBox(RenderSymbolChar(243, draw), shift);
	CurrentX = savedX;
	CurrentY = savedY;
	shift = TeX(sigma22) - 0.99 * bboxHeight(bbox2);
	PMoveUp(shift);
	bbox2 = ShiftBBox(RenderSymbolChar(245, draw), shift);
	if (draw)
	    PMoveTo(savedX + max(bboxWidth(bbox1), bboxWidth(bbox2)), savedY);
	else
	    PMoveTo(savedX, savedY);
	return CombineAlignedBBoxes(bbox1, bbox2);
    }
    else {
	return RenderSymbolChar(0362, draw);
    }
}

static BBOX RenderInt(SEXP expr, int draw)
{
    BBOX opBBox, lowerBBox, upperBBox, bodyBBox;
    int nexpr = length(expr);
    STYLE style = GetStyle();
    double savedX = CurrentX;
    double savedY = CurrentY;
    double hshift, vshift, width;

    opBBox = RenderIntSymbol(draw);
    width = bboxWidth(opBBox);
    CurrentX = savedX;
    CurrentY = savedY;
    if (nexpr > 2) {
	hshift = 0.5 * width + ThinSpace();
	SetSubStyle(style);
	lowerBBox = RenderElement(CADDR(expr), 0);
	vshift = bboxDepth(opBBox) + CenterShift(lowerBBox);
	lowerBBox = RenderOffsetElement(CADDR(expr), hshift, -vshift, draw);
	opBBox = CombineAlignedBBoxes(opBBox, lowerBBox);
	SetStyle(style);
	CurrentX = savedX;
	CurrentY = savedY;
    }
    if (nexpr > 3) {
	hshift = width + ThinSpace();
	SetSupStyle(style);
	upperBBox = RenderElement(CADDDR(expr), 0);
	vshift = bboxHeight(opBBox) - CenterShift(upperBBox);
	upperBBox = RenderOffsetElement(CADDDR(expr), hshift, vshift, draw);
	opBBox = CombineAlignedBBoxes(opBBox, upperBBox);
	SetStyle(style);
	CurrentX = savedX;
	CurrentY = savedY;
    }
    PMoveAcross(bboxWidth(opBBox));
    if (nexpr > 1) {
	bodyBBox = RenderElement(CADR(expr), draw);
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

static BBOX RenderOpSymbol(SEXP op, int draw)
{
    BBOX bbox;
    double cexSaved = MathDevice->gp.cex;
    /*double savedX = CurrentX;*/
    /*double savedY = CurrentY;*/
    double shift;
    int display = (GetStyle() > STYLE_T);
    int opId = OpAtom(op);

    if (opId == S_SUM || opId == S_PRODUCT ||
	opId == S_UNION || opId == S_INTERSECTION) {
	if (display) {
	    MathDevice->gp.cex = OperatorSymbolMag * MathDevice->gp.cex;
	    bbox = RenderSymbolChar(OpAtom(op), 0);
	    shift = 0.5 * (bboxHeight(bbox) - bboxDepth(bbox)) - TeX(sigma22);
	    if (draw) {
		PMoveUp(-shift);
		bbox = RenderSymbolChar(opId, 1);
		PMoveUp(shift);
	    }
	    MathDevice->gp.cex = cexSaved;
	    return ShiftBBox(bbox, -shift);
	}
	else return RenderSymbolChar(opId, draw);
    }
    else {
	FontType prevfont = SetFont(PlainFont);
	bbox = RenderStr(CHAR(PRINTNAME(op)), draw);
	SetFont(prevfont);
	return bbox;
    }
}

static BBOX RenderOp(SEXP expr, int draw)
{
    BBOX lowerBBox, upperBBox, bodyBBox;
    double savedX = CurrentX;
    double savedY = CurrentY;
    int nexpr = length(expr);
    STYLE style = GetStyle();
    BBOX opBBox = RenderOpSymbol(CAR(expr), 0);
    double width = bboxWidth(opBBox);
    double hshift, lvshift, uvshift;
    lvshift = uvshift = 0;	/* -Wall */
    if (nexpr > 2) {
	SetSubStyle(style);
	lowerBBox = RenderElement(CADDR(expr), 0);
	SetStyle(style);
	width = max(width, bboxWidth(lowerBBox));
	lvshift = max(TeX(xi10), TeX(xi12) - bboxHeight(lowerBBox));
	lvshift = bboxDepth(opBBox) + bboxHeight(lowerBBox) + lvshift;
    }
    if (nexpr > 3) {
	SetSupStyle(style);
	upperBBox = RenderElement(CADDDR(expr), 0);
	SetStyle(style);
	width = max(width, bboxWidth(upperBBox));
	uvshift = max(TeX(xi9), TeX(xi11) - bboxDepth(upperBBox));
	uvshift = bboxHeight(opBBox) + bboxDepth(upperBBox) + uvshift;
    }
    hshift = 0.5 * (width - bboxWidth(opBBox));
    opBBox = RenderGap(hshift, draw);
    opBBox = CombineBBoxes(opBBox, RenderOpSymbol(CAR(expr), draw));
    CurrentX = savedX;
    CurrentY = savedY;
    if (nexpr > 2) {
	SetSubStyle(style);
	hshift = 0.5 * (width - bboxWidth(lowerBBox));
	lowerBBox = RenderOffsetElement(CADDR(expr), hshift, -lvshift, draw);
	SetStyle(style);
	opBBox = CombineAlignedBBoxes(opBBox, lowerBBox);
	CurrentX = savedX;
	CurrentY = savedY;
    }
    if (nexpr > 3) {
	SetSupStyle(style);
	hshift = 0.5 * (width - bboxWidth(upperBBox));
	upperBBox = RenderOffsetElement(CADDDR(expr), hshift, uvshift, draw);
	SetStyle(style);
	opBBox = CombineAlignedBBoxes(opBBox, upperBBox);
	CurrentX = savedX;
	CurrentY = savedY;
    }
    opBBox = EnlargeBBox(opBBox, TeX(xi13), TeX(xi13), 0);
    if (draw)
	PMoveAcross(width);
    opBBox = CombineBBoxes(opBBox, RenderGap(ThinSpace(), draw));
    bodyBBox = RenderElement(CADR(expr), draw);
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

static BBOX RenderScript(SEXP expr, int draw)
{
    BBOX bbox;
    STYLE style = GetStyle();
    SetSupStyle(style);
    bbox = RenderElement(expr, draw);
    SetStyle(style);
    return bbox;
}

static BBOX RenderRadical(SEXP expr, int draw)
{
    SEXP body = CADR(expr);
    SEXP order = CADDR(expr);
    BBOX bodyBBox, orderBBox;
    double radWidth, radHeight, radDepth;
    double leadWidth, leadHeight, twiddleHeight;
    double hshift, vshift;
    double radGap, radSpace, radTrail;
    STYLE style = GetStyle();
    double savedX = CurrentX;
    double savedY = CurrentY;
    double x[5], y[5];

    radGap = RADICAL_GAP * xHeight();
    radSpace = RADICAL_SPACE * xHeight();
    radTrail = MuSpace();
    SetPrimeStyle(style);
    bodyBBox = RenderElement(body, 0);
    bodyBBox = RenderItalicCorr(bodyBBox, 0);

    radWidth = 0.6 *XHeight();
    radHeight = bboxHeight(bodyBBox) + radGap;
    radDepth = bboxDepth(bodyBBox);
    twiddleHeight = CenterShift(bodyBBox);

    leadWidth = radWidth;
    leadHeight = radHeight;
    if (order != R_NilValue) {
	SetSupStyle(style);
	orderBBox = RenderScript(order, 0);
	leadWidth = max(leadWidth, bboxWidth(orderBBox) + 0.4 * radWidth);
	hshift = leadWidth - bboxWidth(orderBBox) - 0.4 * radWidth;
	vshift = leadHeight - bboxHeight(orderBBox);
	if (vshift - bboxDepth(orderBBox) < twiddleHeight + radGap)
	    vshift = twiddleHeight + bboxDepth(orderBBox) + radGap;
	if (draw) {
	    PMoveTo(savedX + hshift, savedY + vshift);
	    orderBBox = RenderScript(order, draw);
	}
	orderBBox = EnlargeBBox(orderBBox, vshift, 0, hshift);
    }
    else
	orderBBox = NullBBox();
    if (draw) {
	PMoveTo(savedX + leadWidth - radWidth, savedY);
	PMoveUp(0.8 * twiddleHeight);
	x[0] = ConvertedX();
	y[0] = ConvertedY();
	PMoveUp(0.2 * twiddleHeight);
	PMoveAcross(0.3 * radWidth);
	x[1] = ConvertedX();
	y[1] = ConvertedY();
	PMoveUp(-(twiddleHeight + bboxDepth(bodyBBox)));
	PMoveAcross(0.3 * radWidth);
	x[2] = ConvertedX();
	y[2] = ConvertedY();
	PMoveUp(bboxDepth(bodyBBox) + bboxHeight(bodyBBox) + radGap);
	PMoveAcross(0.4 * radWidth);
	x[3] = ConvertedX();
	y[3] = ConvertedY();
	PMoveAcross(radSpace + bboxWidth(bodyBBox) + radTrail);
	x[4] = ConvertedX();
	y[4] = ConvertedY();
	GPolyline(5, x, y, INCHES, MathDevice);
	PMoveTo(savedX, savedY);
    }
    orderBBox = CombineAlignedBBoxes(orderBBox,
				     RenderGap(leadWidth + radSpace, draw));
    SetPrimeStyle(style);
    orderBBox = CombineBBoxes(orderBBox, RenderElement(body, draw));
    orderBBox = CombineBBoxes(orderBBox, RenderGap(2 * radTrail, draw));
    SetStyle(style);
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

static BBOX RenderAbs(SEXP expr, int draw)
{
    BBOX bbox = RenderElement(CADR(expr), 0);
    double height = bboxHeight(bbox);
    double depth = bboxDepth(bbox);
    double x[2], y[2];

    bbox= RenderGap(MuSpace(), draw);
    if (draw) {
	PMoveUp(-depth);
	x[0] = ConvertedX();
	y[0] = ConvertedY();
	PMoveUp(depth + height);
	x[1] = ConvertedX();
	y[1] = ConvertedY();
	GPolyline(2, x, y, INCHES, MathDevice);
	PMoveUp(-height);
    }
    bbox = CombineBBoxes(bbox, RenderGap(MuSpace(), draw));
    bbox = CombineBBoxes(bbox, RenderElement(CADR(expr), draw));
    bbox = RenderItalicCorr(bbox, draw);
    bbox = CombineBBoxes(bbox, RenderGap(MuSpace(), draw));
    if (draw) {
	PMoveUp(-depth);
	x[0] = ConvertedX();
	y[0] = ConvertedY();
	PMoveUp(depth + height);
	x[1] = ConvertedX();
	y[1] = ConvertedY();
	GPolyline(2, x, y, INCHES, MathDevice);
	PMoveUp(-height);
    }
    bbox = CombineBBoxes(bbox, RenderGap(MuSpace(), draw));
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

static BBOX RenderCurly(SEXP expr, int draw)
{
    return RenderElement(CADR(expr), draw);
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

static BBOX RenderRel(SEXP expr, int draw)
{
    int op = RelAtom(CAR(expr));
    int nexpr = length(expr);
    BBOX bbox;
    double gap;

    if(nexpr == 3) {
	gap = (CurrentStyle > STYLE_S) ? ThickSpace() : 0;
	bbox = RenderElement(CADR(expr), draw);
	bbox = RenderItalicCorr(bbox, draw);
	bbox = CombineBBoxes(bbox, RenderGap(gap, draw));
	bbox = CombineBBoxes(bbox, RenderSymbolChar(op, draw));
	bbox = CombineBBoxes(bbox, RenderGap(gap, draw));
	return CombineBBoxes(bbox, RenderElement(CADDR(expr), draw));
    }
    else error("invalid mathematical annotation");

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

static BBOX RenderBold(SEXP expr, int draw)
{
    BBOX bbox;
    FontType prevfont = SetFont(BoldFont);
    bbox = RenderElement(CADR(expr), draw);
    SetFont(prevfont);
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

static BBOX RenderItalic(SEXP expr, int draw)
{
    BBOX bbox;
    FontType prevfont = SetFont(ItalicFont);
    bbox = RenderElement(CADR(expr), draw);
    SetFont(prevfont);
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

static BBOX RenderPlain(SEXP expr, int draw)
{
    BBOX bbox;
    int prevfont = SetFont(PlainFont);
    bbox = RenderElement(CADR(expr), draw);
    SetFont(prevfont);
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

static BBOX RenderBoldItalic(SEXP expr, int draw)
{
    BBOX bbox;
    int prevfont = SetFont(BoldItalicFont);
    bbox = RenderElement(CADR(expr), draw);
    SetFont(prevfont);
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

static BBOX RenderStyle(SEXP expr, int draw)
{
    STYLE prevstyle = GetStyle();
    BBOX bbox;
    if (NameMatch(CAR(expr), "displaystyle"))
	SetStyle(STYLE_D);
    else if (NameMatch(CAR(expr), "textstyle"))
	SetStyle(STYLE_T);
    else if (NameMatch(CAR(expr), "scriptstyle"))
	SetStyle(STYLE_S);
    else if (NameMatch(CAR(expr), "scriptscriptstyle"))
	SetStyle(STYLE_SS);
    bbox = RenderElement(CADR(expr), draw);
    SetStyle(prevstyle);
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

static BBOX RenderPhantom(SEXP expr, int draw)
{
    BBOX bbox = RenderElement(CADR(expr), 0);
    if (NameMatch(CAR(expr), "vphantom")) {
	bboxWidth(bbox) = 0;
	bboxItalic(bbox) = 0;
    }
    else RenderGap(bboxWidth(bbox), draw);
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

static BBOX RenderConcatenate(SEXP expr, int draw)
{
    BBOX bbox = NullBBox();
    int i, n;

    expr = CDR(expr);
    n = length(expr);

    for (i = 0; i < n; i++) {
	bbox = CombineBBoxes(bbox, RenderElement(CAR(expr), draw));
	if (i != n - 1)
	    bbox = RenderItalicCorr(bbox, draw);
	expr = CDR(expr);
    }
    return bbox;
}

/*----------------------------------------------------------------------
 *
 *  Code for Comma-Separated Lists
 *
 */

static BBOX RenderCommaList(SEXP expr, int draw)
{
    BBOX bbox = NullBBox();
    double small = 0.4 * ThinSpace();
    int i, n;
    n = length(expr);
    for (i = 0; i < n; i++) {
	if (NameAtom(CAR(expr)) && NameMatch(CAR(expr), "...")) {
	    if (i > 0) {
		bbox = CombineBBoxes(bbox, RenderSymbolChar(S_COMMA, draw));
#ifdef OLD
		bbox = CombineBBoxes(bbox, RenderGap(small, draw));
#else
		bbox = CombineBBoxes(bbox, RenderSymbolChar(S_SPACE, draw));
#endif
	    }
	    bbox = CombineBBoxes(bbox, RenderSymbolChar(S_ELLIPSIS, draw));
	    bbox = CombineBBoxes(bbox, RenderGap(small, draw));
	}
	else {
	    if (i > 0) {
		bbox = CombineBBoxes(bbox, RenderSymbolChar(S_COMMA, draw));
		bbox = CombineBBoxes(bbox, RenderSymbolChar(S_SPACE, draw));
	    }
	    bbox = CombineBBoxes(bbox, RenderElement(CAR(expr), draw));
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

static BBOX RenderExpression(SEXP expr, int draw)
{
    BBOX bbox;
    if (NameAtom(CAR(expr)))
	bbox = RenderSymbolString(CAR(expr), draw);
    else
	bbox = RenderElement(CAR(expr), draw);
    bbox = RenderItalicCorr(bbox, draw);
    bbox = CombineBBoxes(bbox, RenderDelimiter(S_PARENLEFT, draw));
    bbox = CombineBBoxes(bbox, RenderCommaList(CDR(expr), draw));
    bbox = RenderItalicCorr(bbox, draw);
    bbox = CombineBBoxes(bbox, RenderDelimiter(S_PARENRIGHT, draw));
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

static BBOX RenderList(SEXP expr, int draw)
{
    return RenderCommaList(CDR(expr), draw);
}

/* Dispatching procedure which determines nature of expression. */


static BBOX RenderFormula(SEXP expr, int draw)
{
    SEXP head = CAR(expr);

    if (SpaceAtom(head))
	return RenderSpace(expr, draw);
    else if (BinAtom(head))
	return RenderBin(expr, draw);
    else if (SuperAtom(head))
	return RenderSup(expr, draw);
    else if (SubAtom(head))
	return RenderSub(expr, draw);
    else if (WideTildeAtom(head))
	return RenderWideTilde(expr, draw);
    else if (WideHatAtom(head))
	return RenderWideHat(expr, draw);
    else if (BarAtom(head))
	return RenderBar(expr, draw);
    else if (AccentAtom(head))
	return RenderAccent(expr, draw);
    else if (OverAtom(head))
	return RenderOver(expr, draw);
    else if (AtopAtom(head))
	return RenderAtop(expr, draw);
    else if (ParenAtom(head))
	return RenderParen(expr, draw);
    else if (BGroupAtom(head))
	return RenderBGroup(expr, draw);
    else if (GroupAtom(head))
	return RenderGroup(expr, draw);
    else if (IntAtom(head))
	return RenderInt(expr, draw);
    else if (OpAtom(head))
	return RenderOp(expr, draw);
    else if (RadicalAtom(head))
	return RenderRadical(expr, draw);
    else if (AbsAtom(head))
	return RenderAbs(expr, draw);
    else if (CurlyAtom(head))
	return RenderCurly(expr, draw);
    else if (RelAtom(head))
	return RenderRel(expr, draw);
    else if (BoldAtom(head))
	return RenderBold(expr, draw);
    else if (ItalicAtom(head))
	return RenderItalic(expr, draw);
    else if (PlainAtom(head))
	return RenderPlain(expr, draw);
    else if (BoldItalicAtom(head))
	return RenderBoldItalic(expr, draw);
    else if (StyleAtom(head))
	return RenderStyle(expr, draw);
    else if (PhantomAtom(head))
	return RenderPhantom(expr, draw);
    else if (ConcatenateAtom(head))
	return RenderConcatenate(expr, draw);
    else if (ListAtom(head))
	return RenderList(expr, draw);
    else
	return RenderExpression(expr, draw);
}


/* Dispatch on whether atom (symbol, string, number, ...) */
/* or formula (some sort of expression) */

static BBOX RenderElement(SEXP expr, int draw)
{
    if (FormulaExpression(expr))
	return RenderFormula(expr, draw);
    else
	return RenderAtom(expr, draw);
}

static BBOX RenderOffsetElement(SEXP expr, double x, double y, int draw)
{
    BBOX bbox;
    double savedX = CurrentX;
    double savedY = CurrentY;
    if (draw) {
	CurrentX += x;
	CurrentY += y;
    }
    bbox = RenderElement(expr, draw);
    bboxWidth(bbox) += x;
    bboxHeight(bbox) += y;
    bboxDepth(bbox) -= y;
    CurrentX = savedX;
    CurrentY = savedY;
    return bbox;

}

/* Calculate width of expression */
/* BBOXes are in INCHES (see MetricUnit) */

/* #ifdef'ed this function out to shut -Wall up */
#ifdef OLD
static
void GExpressionBBox(SEXP expr, GUnit units, double *width,
		     double *height, double *depth, DevDesc *dd)
{
    BBOX bbox;
    MathDevice = dd;
/* The following two lines don't look right to me, but I inserted them
   because otherwise you get trouble if you calculate BBoxes without
   plotting any math first... Similar problem in the next two functions
	 --pd */
    CurrentStyle = STYLE_D;
    SetFont(PlainFont);
    bbox = RenderElement(expr, 0);
    *width  = bboxWidth(bbox);
    *height  = bboxHeight(bbox);
    *depth  = bboxDepth(bbox);
    if (units != INCHES) {
	*width = GConvertXUnits(*width, INCHES, units, dd);
	*height = GConvertYUnits(*height, INCHES, units, dd);
	*depth = GConvertYUnits(*depth, INCHES, units, dd);
    }
}
#endif

double GExpressionWidth(SEXP expr, GUnit units, DevDesc *dd)
{
    BBOX bbox;
    double width;
    MathDevice = dd;
    CurrentStyle = STYLE_D;
    SetFont(PlainFont);
    bbox = RenderElement(expr, 0);
    width  = bboxWidth(bbox);
    if (units == INCHES)
	return width;
    else
	return GConvertXUnits(width, INCHES, units, dd);
}

double GExpressionHeight(SEXP expr, GUnit units, DevDesc *dd)
{
    BBOX bbox;
    double height;
    MathDevice = dd;
    CurrentStyle = STYLE_D;
    SetFont(PlainFont);
    bbox = RenderElement(expr, 0);
    height = bboxHeight(bbox) + bboxDepth(bbox);
    if (units == INCHES)
	return height;
    else
	return GConvertYUnits(height, INCHES, units, dd);
}

/* Functions forming the R API */

void GMathText(double x, double y, int coords, SEXP expr,
	       double xc, double yc, double rot, DevDesc *dd)
{
    BBOX bbox;

#ifdef BUG61
#else
    /* IF font metric information is not available for device */
    /* then bail out */
    double ascent, descent, width;
    GMetricInfo(0, &ascent, &descent, &width, DEVICE, dd);
    if ((ascent==0) && (descent==0) && (width==0))
	error("Metric information not yet available for this device");
#endif

    MathDevice = dd;
    BaseCex = MathDevice->gp.cex;
    BoxColor = name2col("pink");
    TextColor = MathDevice->gp.col;
    CurrentStyle = STYLE_D;
    SetFont(PlainFont);
    bbox = RenderElement(expr, 0);
    ReferenceX = x;
    ReferenceY = y;
    GConvert(&ReferenceX, &ReferenceY, coords, INCHES, dd);
    if (R_FINITE(xc))
	CurrentX = ReferenceX - xc * bboxWidth(bbox);
    else
	CurrentX = ReferenceX;
    if (R_FINITE(yc))
	CurrentY = ReferenceY + bboxDepth(bbox)
	    - yc * (bboxHeight(bbox) + bboxDepth(bbox));
    else
	CurrentY = ReferenceY;
    CurrentAngle = rot;
    rot *= M_PI_2 / 90 ;/* radians */
    CosAngle = cos(rot);
    SinAngle = sin(rot);
    RenderElement(expr, 1);
}/* GMathText */


void GMMathText(SEXP str, int side, double line, int outer,
		double at, int las, DevDesc *dd)
{
    int coords = 0, subcoords;
    double xadj, yadj = 0, angle = 0;

#ifdef BUG61
#else
    /* IF font metric information is not available for device */
    /* then bail out */
    double ascent, descent, width;
    GMetricInfo(0, &ascent, &descent, &width, DEVICE, dd);
    if ((ascent==0) && (descent==0) && (width==0))
	error("Metric information not yet available for this device");
#endif

    MathDevice = dd;

    xadj = MathDevice->gp.adj;

    /* This is MOSTLY the same as the same section of GMtext
     * BUT it differs because it sets different values for yadj for
     * different situations.
     * Paul
     */
    if(outer) {
	switch(side) {
	case 1:	    coords = OMA1;	break;
	case 2:	    coords = OMA2;	break;
	case 3:	    coords = OMA3;	break;
	case 4:	    coords = OMA4;	break;
	}
	subcoords = NIC;
    }
    else {
	switch(side) {
	case 1:	    coords = MAR1;	break;
	case 2:	    coords = MAR2;	break;
	case 3:	    coords = MAR3;	break;
	case 4:	    coords = MAR4;	break;
	}
	subcoords = USER;
    }
    /* Note: I changed dd->gp.yLineBias to 0.3 here. */
    /* Purely visual tuning. RI */
    switch(side) {
    case 1:
	if(las == 2 || las == 3) {
	    at = at + GConvertXUnits(0.3, LINES, subcoords, dd);
	    angle = 90;
	    yadj = 0.5;
	}
	else {
	    line = line + 1 - dd->gp.yLineBias;
	    angle = 0;
	    yadj = NA_REAL;
	}
	break;
    case 2:
	if(las == 1 || las == 2) {
	    at = at - GConvertYUnits(0.3, LINES, subcoords, dd);
	    angle = 0;
	    yadj = 0.5;
	}
	else {
	    line = line + dd->gp.yLineBias;
	    angle = 90;
	    yadj = NA_REAL;
	}
	break;
    case 3:
	if(las == 2 || las == 3) {
	    at = at + GConvertXUnits(0.3, LINES, subcoords, dd);
	    angle = 90;
	    yadj = 0.5;
	}
	else {
	    line = line + dd->gp.yLineBias;
	    angle = 0;
	    yadj = NA_REAL;
	}
	break;
    case 4:
	if(las == 1 || las == 2) {
	    at = at - GConvertYUnits(0.3, LINES, subcoords, dd);
	    angle = 0;
	    yadj = 0.5;
	}
	else {
	    line = line + 1 - dd->gp.yLineBias;
	    angle = 90;
	    yadj = NA_REAL;
	}
	break;
    }
    GMathText(at, line, coords, str, xadj, yadj, angle, dd);
}/* GMMathText */
