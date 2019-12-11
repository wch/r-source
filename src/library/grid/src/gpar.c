/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-3 Paul Murrell
 *                2003-2014 The R Core Team
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
 */

#include "grid.h"
#include <string.h>

/* Some access methods for gpars */
SEXP gpFontSizeSXP(SEXP gp) {
    return VECTOR_ELT(gp, GP_FONTSIZE);
}
double gpFontSize(SEXP gp, int i) {
    SEXP fontsize = gpFontSizeSXP(gp);
    return REAL(fontsize)[i % LENGTH(fontsize)];
}
double gpFontSize2(SEXP gp, int i, int* gpIsScalar) {
    SEXP fontsize = gpFontSizeSXP(gp);
    gpIsScalar[GP_FONTSIZE] = LENGTH(fontsize) == 1;
    return REAL(fontsize)[i % LENGTH(fontsize)];
}

SEXP gpLineHeightSXP(SEXP gp) {
    return VECTOR_ELT(gp, GP_LINEHEIGHT);
}

double gpLineHeight(SEXP gp, int i) {
    SEXP lineheight = gpLineHeightSXP(gp);
    return REAL(lineheight)[i % LENGTH(lineheight)];
}
double gpLineHeight2(SEXP gp, int i, int* gpIsScalar) {
    SEXP lineheight = gpLineHeightSXP(gp);
    gpIsScalar[GP_LINEHEIGHT] = LENGTH(lineheight) == 1;
    return REAL(lineheight)[i % LENGTH(lineheight)];
}

SEXP gpColSXP(SEXP gp) {
    return VECTOR_ELT(gp, GP_COL);
}
/* grid has no concept of 'colour 0' (bg in base) */
int gpCol(SEXP gp, int i) {
    SEXP col = gpColSXP(gp);
    int result;
    if (isNull(col))
	result = R_TRANWHITE;
    else
	result = RGBpar3(col, i % LENGTH(col), R_TRANWHITE);
    return result;
}
int gpCol2(SEXP gp, int i, int* gpIsScalar) {
    SEXP col = gpColSXP(gp);
    gpIsScalar[GP_COL] = LENGTH(col) == 1;
    int result;
    if (isNull(col))
        result = R_TRANWHITE;
    else
        result = RGBpar3(col, i % LENGTH(col), R_TRANWHITE);
    return result;
}

SEXP gpFillSXP(SEXP gp) {
    return VECTOR_ELT(gp, GP_FILL);
}

int gpFill(SEXP gp, int i) {
    SEXP fill = gpFillSXP(gp);
    int result;
    if (isNull(fill))
	result = R_TRANWHITE;
    else
	result = RGBpar3(fill, i % LENGTH(fill), R_TRANWHITE);
    return result;
}
int gpFill2(SEXP gp, int i, int* gpIsScalar) {
    SEXP fill = gpFillSXP(gp);
    gpIsScalar[GP_FILL] = LENGTH(fill) == 1;
    int result;
    if (isNull(fill))
        result = R_TRANWHITE;
    else
        result = RGBpar3(fill, i % LENGTH(fill), R_TRANWHITE);
    return result;
}

SEXP gpGammaSXP(SEXP gp) {
    return VECTOR_ELT(gp, GP_GAMMA);
}

double gpGamma(SEXP gp, int i) {
    SEXP gamma = gpGammaSXP(gp);
    return REAL(gamma)[i % LENGTH(gamma)];
}
double gpGamma2(SEXP gp, int i, int* gpIsScalar) {
    SEXP gamma = gpGammaSXP(gp);
    gpIsScalar[GP_GAMMA] = LENGTH(gamma) == 1;
    return REAL(gamma)[i % LENGTH(gamma)];
}

SEXP gpLineTypeSXP(SEXP gp) {
    return VECTOR_ELT(gp, GP_LTY);
}

int gpLineType(SEXP gp, int i) {
    SEXP linetype = gpLineTypeSXP(gp);
    return GE_LTYpar(linetype, i % LENGTH(linetype));
}
int gpLineType2(SEXP gp, int i, int* gpIsScalar) {
    SEXP linetype = gpLineTypeSXP(gp);
    gpIsScalar[GP_LTY] = LENGTH(linetype) == 1;
    return GE_LTYpar(linetype, i % LENGTH(linetype));
}

SEXP gpLineWidthSXP(SEXP gp) {
    return VECTOR_ELT(gp, GP_LWD);
}

double gpLineWidth(SEXP gp, int i) {
    SEXP linewidth = gpLineWidthSXP(gp);
    return REAL(linewidth)[i % LENGTH(linewidth)];
}
double gpLineWidth2(SEXP gp, int i, int* gpIsScalar) {
    SEXP linewidth = gpLineWidthSXP(gp);
    gpIsScalar[GP_LWD] = LENGTH(linewidth) == 1;
    return REAL(linewidth)[i % LENGTH(linewidth)];
}

SEXP gpCexSXP(SEXP gp) {
    return VECTOR_ELT(gp, GP_CEX);
}

double gpCex(SEXP gp, int i) {
    SEXP cex = gpCexSXP(gp);
    return REAL(cex)[i % LENGTH(cex)];
}
double gpCex2(SEXP gp, int i, int* gpIsScalar) {
    SEXP cex = gpCexSXP(gp);
    gpIsScalar[GP_CEX] = LENGTH(cex) == 1;
    return REAL(cex)[i % LENGTH(cex)];
}

SEXP gpFontSXP(SEXP gp) {
    return VECTOR_ELT(gp, GP_FONT);
}

int gpFont(SEXP gp, int i) {
    SEXP font = gpFontSXP(gp);
    return INTEGER(font)[i % LENGTH(font)];
}
int gpFont2(SEXP gp, int i, int* gpIsScalar) {
    SEXP font = gpFontSXP(gp);
    gpIsScalar[GP_FONT] = LENGTH(font) == 1;
    return INTEGER(font)[i % LENGTH(font)];
}

SEXP gpFontFamilySXP(SEXP gp) {
    return VECTOR_ELT(gp, GP_FONTFAMILY);
}

const char* gpFontFamily(SEXP gp, int i) {
    SEXP fontfamily = gpFontFamilySXP(gp);
    return CHAR(STRING_ELT(fontfamily, i % LENGTH(fontfamily)));
}
const char* gpFontFamily2(SEXP gp, int i, int* gpIsScalar) {
    SEXP fontfamily = gpFontFamilySXP(gp);
    gpIsScalar[GP_FONTFAMILY] = LENGTH(fontfamily) == 1;
    return CHAR(STRING_ELT(fontfamily, i % LENGTH(fontfamily)));
}

SEXP gpAlphaSXP(SEXP gp) {
    return VECTOR_ELT(gp, GP_ALPHA);
}

double gpAlpha(SEXP gp, int i) {
    SEXP alpha = gpAlphaSXP(gp);
    return REAL(alpha)[i % LENGTH(alpha)];
}
double gpAlpha2(SEXP gp, int i, int* gpIsScalar) {
    SEXP alpha = gpAlphaSXP(gp);
    gpIsScalar[GP_ALPHA] = LENGTH(alpha) == 1;
    return REAL(alpha)[i % LENGTH(alpha)];
}

SEXP gpLineEndSXP(SEXP gp) {
    return VECTOR_ELT(gp, GP_LINEEND);
}

R_GE_lineend gpLineEnd(SEXP gp, int i) {
    SEXP lineend = gpLineEndSXP(gp);
    return GE_LENDpar(lineend, i % LENGTH(lineend));
}
R_GE_lineend gpLineEnd2(SEXP gp, int i, int* gpIsScalar) {
    SEXP lineend = gpLineEndSXP(gp);
    gpIsScalar[GP_LINEEND] = LENGTH(lineend) == 1;
    return GE_LENDpar(lineend, i % LENGTH(lineend));
}

SEXP gpLineJoinSXP(SEXP gp) {
    return VECTOR_ELT(gp, GP_LINEJOIN);
}

R_GE_linejoin gpLineJoin(SEXP gp, int i) {
    SEXP linejoin = gpLineJoinSXP(gp);
    return GE_LJOINpar(linejoin, i % LENGTH(linejoin));
}
R_GE_linejoin gpLineJoin2(SEXP gp, int i, int* gpIsScalar) {
    SEXP linejoin = gpLineJoinSXP(gp);
    gpIsScalar[GP_LINEJOIN] = LENGTH(linejoin) == 1;
    return GE_LJOINpar(linejoin, i % LENGTH(linejoin));
}

SEXP gpLineMitreSXP(SEXP gp) {
    return VECTOR_ELT(gp, GP_LINEMITRE);
}

double gpLineMitre(SEXP gp, int i) {
    SEXP linemitre = gpLineMitreSXP(gp);
    return REAL(linemitre)[i % LENGTH(linemitre)];
}
double gpLineMitre2(SEXP gp, int i, int* gpIsScalar) {
    SEXP linemitre = gpLineMitreSXP(gp);
    gpIsScalar[GP_LINEMITRE] = LENGTH(linemitre) == 1;
    return REAL(linemitre)[i % LENGTH(linemitre)];
}

SEXP gpLexSXP(SEXP gp) {
    return VECTOR_ELT(gp, GP_LEX);
}

double gpLex(SEXP gp, int i) {
    SEXP lex = gpLexSXP(gp);
    return REAL(lex)[i % LENGTH(lex)];
}
double gpLex2(SEXP gp, int i, int* gpIsScalar) {
    SEXP lex = gpLexSXP(gp);
    gpIsScalar[GP_LEX] = LENGTH(lex) == 1;
    return REAL(lex)[i % LENGTH(lex)];
}

/*
 * Never access fontface because fontface values are stored in font
 * Historical reasons ...
 */

/*
 * Combine gpar alpha with alpha level stored in colour 
 *
 * finalAlpha = gpAlpha*(R_ALPHA(col)/255)
 *
 * Based on my reading of how group alpha and individual
 * object alphas are combined in the SVG 1.0 docs
 *
 * Also has nice properties:
 *  (i)   range of finalAlpha is 0 to 1.
 *  (ii)  if either of gpAlpha or R_ALPHA(col) are 0 then finalAlpha = 0
 *        (i.e., can never make fully transparent colour less transparent).
 *  (iii) in order to get finalAlpha = 1, both gpAlpha and R_ALPHA(col)
 *        must be 1 (i.e., only way to get fully opaque is if both
 *        alpha levels are fully opaque).
 */
static unsigned int combineAlpha(double alpha, int col) 
{
    unsigned int newAlpha = (unsigned int)((alpha*(R_ALPHA(col)/255.0))*255);
    return R_RGBA(R_RED(col), R_GREEN(col), R_BLUE(col), newAlpha);
}

static SEXP resolveFill(SEXP pattern) 
{
    SEXP resolveFn, R_fcall, result;
    PROTECT(resolveFn = findFun(install("resolveFill"), R_gridEvalEnv));
    PROTECT(R_fcall = lang2(resolveFn, pattern));
    result = eval(R_fcall, R_gridEvalEnv);
    UNPROTECT(2);
    return result;
}

SEXP resolveGPar(SEXP gp) 
{
    SEXP result = R_NilValue;
    if (Rf_inherits(gpFillSXP(gp), "GridPattern")) {
        SEXP resolvedFill = PROTECT(resolveFill(gpFillSXP(gp)));
        setListElement(gp, "fill", resolvedFill);
        result = resolvedFill;
        UNPROTECT(1);
    }
    return result;
}

/* 
 * Generate an R_GE_gcontext from a gpar
 */
void gcontextFromgpar(SEXP gp, int i, const pGEcontext gc, pGEDevDesc dd) 
{
    /* 
     * Combine gpAlpha with col and fill
     */
    gc->col = combineAlpha(gpAlpha(gp, i), gpCol(gp, i));
    /*
     * Fill could be colour OR pattern
     */
    if (Rf_inherits(gpFillSXP(gp), "GridPattern")) {
        if (Rf_inherits(gpFillSXP(gp), "GridResolvedPattern")) {
            int fillIndex = INTEGER(getListElement(gpFillSXP(gp), "index"))[0];
            gc->fill = R_TRANWHITE;
            gc->patternFill = fillIndex;
        } else {
            gc->fill = R_TRANWHITE;
            gc->patternFill = -1;
        }
    } else {
        gc->fill = combineAlpha(gpAlpha(gp, i), gpFill(gp, i));
        gc->patternFill = -1;
    }
    gc->gamma = gpGamma(gp, i);
    /*
     * Combine gpLex with lwd
     * Also scale by GSS_SCALE (a "zoom" factor)
     */
    gc->lwd = gpLineWidth(gp, i) * gpLex(gp, i) * 
	REAL(gridStateElement(dd, GSS_SCALE))[0];
    gc->lty = gpLineType(gp, i);
    gc->lend = gpLineEnd(gp, i);
    gc->ljoin = gpLineJoin(gp, i);
    gc->lmitre = gpLineMitre(gp, i);
    gc->cex = gpCex(gp, i);
    /*
     * Scale by GSS_SCALE (a "zoom" factor)
     */
    gc->ps = gpFontSize(gp, i) * REAL(gridStateElement(dd, GSS_SCALE))[0];
    gc->lineheight = gpLineHeight(gp, i);
    gc->fontface = gpFont(gp, i);
    strcpy(gc->fontfamily, gpFontFamily(gp, i));
}

SEXP L_setGPar(SEXP gpars) 
{
    /* Set the value of the current gpars on the current device
     * Need to do this in here so that redrawing via R BASE display
     * list works 
     */
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    setGridStateElement(dd, GSS_GPAR, gpars);
    return R_NilValue;
}

SEXP L_getGPar(void) 
{
    /* Get the value of the current gpars on the current device
     * Need to do this in here so that redrawing via R BASE display
     * list works 
     */
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    return gridStateElement(dd, GSS_GPAR);
}

SEXP L_getGPsaved() 
{
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    return gridStateElement(dd, GSS_GPSAVED);
}

SEXP L_setGPsaved(SEXP gpars) 
{
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    setGridStateElement(dd, GSS_GPSAVED, gpars);
    return R_NilValue;
}

void initGPar(pGEDevDesc dd)
{
    pDevDesc dev = dd->dev;
    SEXP gpar, gparnames, class;
    SEXP gpfill, gpcol, gpgamma, gplty, gplwd, gpcex, gpfs, gplh, gpfont;
    SEXP gpfontfamily, gpalpha, gplineend, gplinejoin, gplinemitre, gplex;
    SEXP gsd = (SEXP) dd->gesd[gridRegisterIndex]->systemSpecific;
    PROTECT(gpar = allocVector(VECSXP, 15));
    PROTECT(gparnames = allocVector(STRSXP, 15));
    SET_STRING_ELT(gparnames, GP_FILL, mkChar("fill"));
    SET_STRING_ELT(gparnames, GP_COL, mkChar("col"));
    SET_STRING_ELT(gparnames, GP_GAMMA, mkChar("gamma"));
    SET_STRING_ELT(gparnames, GP_LTY, mkChar("lty"));
    SET_STRING_ELT(gparnames, GP_LWD, mkChar("lwd"));
    SET_STRING_ELT(gparnames, GP_CEX, mkChar("cex"));
    SET_STRING_ELT(gparnames, GP_FONTSIZE, mkChar("fontsize"));
    SET_STRING_ELT(gparnames, GP_LINEHEIGHT, mkChar("lineheight"));
    SET_STRING_ELT(gparnames, GP_FONT, mkChar("font"));
    SET_STRING_ELT(gparnames, GP_FONTFAMILY, mkChar("fontfamily"));
    SET_STRING_ELT(gparnames, GP_ALPHA, mkChar("alpha"));
    SET_STRING_ELT(gparnames, GP_LINEEND, mkChar("lineend"));
    SET_STRING_ELT(gparnames, GP_LINEJOIN, mkChar("linejoin"));
    SET_STRING_ELT(gparnames, GP_LINEMITRE, mkChar("linemitre"));
    SET_STRING_ELT(gparnames, GP_LEX, mkChar("lex"));
    setAttrib(gpar, R_NamesSymbol, gparnames);
    PROTECT(gpfill = allocVector(STRSXP, 1));
    SET_STRING_ELT(gpfill, 0, mkChar(col2name(dev->startfill)));
    SET_VECTOR_ELT(gpar, GP_FILL, gpfill);
    PROTECT(gpcol = allocVector(STRSXP, 1));
    SET_STRING_ELT(gpcol, 0, mkChar(col2name(dev->startcol)));
    SET_VECTOR_ELT(gpar, GP_COL, gpcol);
    PROTECT(gpgamma = allocVector(REALSXP, 1));
    REAL(gpgamma)[0] = dev->startgamma;
    SET_VECTOR_ELT(gpar, GP_GAMMA, gpgamma);
    PROTECT(gplty = GE_LTYget(dev->startlty));
    SET_VECTOR_ELT(gpar, GP_LTY, gplty);
    PROTECT(gplwd = allocVector(REALSXP, 1));
    REAL(gplwd)[0] = 1;
    SET_VECTOR_ELT(gpar, GP_LWD, gplwd);
    PROTECT(gpcex = allocVector(REALSXP, 1));
    REAL(gpcex)[0] = 1;
    SET_VECTOR_ELT(gpar, GP_CEX, gpcex);
    PROTECT(gpfs = allocVector(REALSXP, 1));
    REAL(gpfs)[0] = dev->startps;
    SET_VECTOR_ELT(gpar, GP_FONTSIZE, gpfs);
    PROTECT(gplh = allocVector(REALSXP, 1));
    REAL(gplh)[0] = 1.2;
    SET_VECTOR_ELT(gpar, GP_LINEHEIGHT, gplh);
    PROTECT(gpfont = allocVector(INTSXP, 1));
    INTEGER(gpfont)[0] = dev->startfont;
    SET_VECTOR_ELT(gpar, GP_FONT, gpfont);
    PROTECT(gpfontfamily = allocVector(STRSXP, 1));
    /* 
     * A font family of "" means that the default font
     * set up by the device will be used.
     */
    SET_STRING_ELT(gpfontfamily, 0, mkChar(""));
    SET_VECTOR_ELT(gpar, GP_FONTFAMILY, gpfontfamily);
    PROTECT(gpalpha = allocVector(REALSXP, 1));
    REAL(gpalpha)[0] = 1;
    SET_VECTOR_ELT(gpar, GP_ALPHA, gpalpha);
    PROTECT(gplineend = allocVector(STRSXP, 1));
    SET_STRING_ELT(gplineend, 0, mkChar("round"));
    SET_VECTOR_ELT(gpar, GP_LINEEND, gplineend);
    PROTECT(gplinejoin = allocVector(STRSXP, 1));
    SET_STRING_ELT(gplinejoin, 0, mkChar("round"));
    SET_VECTOR_ELT(gpar, GP_LINEJOIN, gplinejoin);
    PROTECT(gplinemitre = allocVector(REALSXP, 1));
    REAL(gplinemitre)[0] = 10;
    SET_VECTOR_ELT(gpar, GP_LINEMITRE, gplinemitre);
    PROTECT(gplex = allocVector(REALSXP, 1));
    REAL(gplex)[0] = 1;
    SET_VECTOR_ELT(gpar, GP_LEX, gplex);
    PROTECT(class = allocVector(STRSXP, 1));
    SET_STRING_ELT(class, 0, mkChar("gpar"));
    classgets(gpar, class);
    SET_VECTOR_ELT(gsd, GSS_GPAR, gpar);
    UNPROTECT(18);
}

// Minimal primitive gc setters
// These should only be called after gcontextFromgpar has been called once
void initGContext(SEXP gp, const pGEcontext gc, pGEDevDesc dd, int* gpIsScalar, 
                  const pGEcontext gcCache) 
{
    int i = 0;
    /* 
     * Combine gpAlpha with col and fill
     */
    gcCache->col = gc->col = 
        combineAlpha(gpAlpha2(gp, i, gpIsScalar), gpCol2(gp, i, gpIsScalar));
    if (Rf_inherits(gpFillSXP(gp), "GridPattern")) {
        if (Rf_inherits(gpFillSXP(gp), "GridResolvedPattern")) {
            int fillIndex = INTEGER(getListElement(gpFillSXP(gp), "index"))[0];
            gcCache->fill = gc->fill = R_TRANWHITE;
            gcCache->patternFill = gc->patternFill = fillIndex;
        } else {
            gcCache->fill = gc->fill = R_TRANWHITE;
            gcCache->patternFill = gc->patternFill = -1;
        }
        gpIsScalar[GP_FILL] = 1;
    } else {
        gcCache->fill = gc->fill = 
            combineAlpha(gpAlpha(gp, i), gpFill2(gp, i, gpIsScalar));
        gcCache->patternFill = gc->patternFill = -1;
    }
    gcCache->gamma = gc->gamma = gpGamma2(gp, i, gpIsScalar);
    /*
     * Combine gpLex with lwd
     * Also scale by GSS_SCALE (a "zoom" factor)
     */
    gcCache->lwd = gc->lwd = gpLineWidth2(gp, i, gpIsScalar) * 
        gpLex2(gp, i, gpIsScalar) * REAL(gridStateElement(dd, GSS_SCALE))[0];
    gcCache->lty = gc->lty = gpLineType2(gp, i, gpIsScalar);
    gcCache->lend = gc->lend = gpLineEnd2(gp, i, gpIsScalar);
    gcCache->ljoin = gc->ljoin = gpLineJoin2(gp, i, gpIsScalar);
    gcCache->lmitre = gc->lmitre = gpLineMitre2(gp, i, gpIsScalar);
    gcCache->cex = gc->cex = gpCex2(gp, i, gpIsScalar);
    /*
     * Scale by GSS_SCALE (a "zoom" factor)
     */
    gcCache->ps = gc->ps = gpFontSize2(gp, i, gpIsScalar) * 
        REAL(gridStateElement(dd, GSS_SCALE))[0];
    gcCache->lineheight = gc->lineheight = gpLineHeight2(gp, i, gpIsScalar);
    gcCache->fontface = gc->fontface = gpFont2(gp, i, gpIsScalar);
    strcpy(gc->fontfamily, gpFontFamily2(gp, i, gpIsScalar));
    strcpy(gcCache->fontfamily, gc->fontfamily);
}
void updateGContext(SEXP gp, int i, const pGEcontext gc, pGEDevDesc dd, 
                    int* gpIsScalar, const pGEcontext gcCache)
{
    if (gpIsScalar[0] == -1) {
        error(_("updateGContext must only be called after initGContext"));
    }
    if (!(gpIsScalar[GP_ALPHA] && gpIsScalar[GP_COL])) {
        double alpha = gpAlpha(gp, i);
        if (alpha == 1.0) gc->col = gpCol(gp, i);
        else gc->col = combineAlpha(alpha, gpCol(gp, i));
    } else {
        gc->col = gcCache->col;
    }
    if (Rf_inherits(gpFillSXP(gp), "GridPattern")) {
        gc->fill = gcCache->fill;
        gc->patternFill = gcCache->patternFill;
    } else {
        if (!(gpIsScalar[GP_ALPHA] && gpIsScalar[GP_FILL])) {
            double alpha = gpAlpha(gp, i);
            if (alpha == 1.0) gc->fill = gpFill(gp, i);
            else gc->fill = combineAlpha(alpha, gpFill(gp, i));
        } else {
            gc->fill = gcCache->fill;
        }
        gc->patternFill = gcCache->patternFill;
    }
    gc->gamma = gpIsScalar[GP_GAMMA] ? gcCache->gamma : gpGamma(gp, i);
    gc->lwd = (gpIsScalar[GP_LWD] && gpIsScalar[GP_LEX]) ? gcCache->lwd :
        gpLineWidth(gp, i) * gpLex(gp, i) * 
            REAL(gridStateElement(dd, GSS_SCALE))[0];
    gc->lty = gpIsScalar[GP_LTY] ? gcCache->lty : gpLineType(gp, i);
    gc->lend = gpIsScalar[GP_LINEEND] ? gcCache->lend : gpLineEnd(gp, i);
    gc->ljoin = gpIsScalar[GP_LINEJOIN] ? gcCache->ljoin : gpLineJoin(gp, i);
    gc->lmitre = gpIsScalar[GP_LINEMITRE] ? gcCache->lmitre : gpLineMitre(gp, i);
    gc->cex = gpIsScalar[GP_CEX] ? gcCache->cex : gpCex(gp, i);
    gc->ps = gpIsScalar[GP_FONTSIZE] ? gcCache->ps : gpFontSize(gp, i) * 
        REAL(gridStateElement(dd, GSS_SCALE))[0];
    gc->lineheight = gpIsScalar[GP_LINEHEIGHT] ? gcCache->lineheight :
        gpLineHeight(gp, i);
    gc->fontface = gpIsScalar[GP_FONT] ? gcCache->fontface : gpFont(gp, i);
    if (gpIsScalar[GP_FONTFAMILY]) {
        strcpy(gc->fontfamily, gcCache->fontfamily);
    } else {
        strcpy(gc->fontfamily, gpFontFamily(gp, i));
    }
}
