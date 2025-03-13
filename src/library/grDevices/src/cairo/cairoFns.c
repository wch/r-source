/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008--2025  R Core Team
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

/* Included by

  cairoBM.c (with NO_X11 defined)
  src/modules/X11/devX11.c if HAVE_WORKING_X11_CAIRO is defined

  pX11Desc is a pointer to one of two similar structures defined in
  cairoBM.h and devX11.h
*/

/* Entry points used

    cairo_arc
    cairo_clip
    cairo_close_path
    cairo_create
    cairo_destroy
    cairo_fill_preserve
    cairo_get_source
    cairo_get_target
    cairo_image_surface_create
    cairo_image_surface_create_for_data
    cairo_image_surface_get_data (1.2)
    cairo_image_surface_get_stride
    cairo_line_to
    cairo_move_to
    cairo_new_path
    cairo_paint
    cairo_pattern_set_extend
    cairo_pattern_set_filter
    cairo_rectangle
    cairo_rel_move_to
    cairo_reset_clip
    cairo_restore
    cairo_rotate
    cairo_save
    cairo_scale
    cairo_set_antialias
    cairo_set_dash
    cairo_set_fill_rule
    cairo_set_line_cap
    cairo_set_line_join
    cairo_set_line_width
    cairo_set_miter_limit
    cairo_set_operator
    cairo_set_source_rgb
    cairo_set_source_rgba
    cairo_set_source_surface
    cairo_status
    cairo_status_to_string
    cairo_stroke
    cairo_surface_destroy
    cairo_surface_status

    cairo_xlib_surface_create
    cairo_xlib_surface_set_size

    cairo_show_text
    cairo_text_extents

    cairo_ft_font_face_create_for_ft_face [macOS]

    g_object_unref  (glib)

    pango_cairo_create_layout (1.10)
    pango_cairo_show_layout (1.10)
    pango_font_description_free
    pango_font_description_new
    pango_font_description_set_family
    pango_font_description_set_size
    pango_font_description_set_style
    pango_font_description_set_weight
    pango_layout_get_line
    pango_layout_line_get_pixel_extents
    pango_layout_set_font_description
    pango_layout_set_text

*/

static void CairoCol(unsigned int col, double* R, double* G, double* B)
{
    *R = R_RED(col)/255.0;
    *G = R_GREEN(col)/255.0;
    *B = R_BLUE(col)/255.0;
    *R = pow(*R, RedGamma);
    *G = pow(*G, GreenGamma);
    *B = pow(*B, BlueGamma);
}

static void CairoColor(unsigned int col, pX11Desc xd)
{
    unsigned int alpha = R_ALPHA(col);
    double red, blue, green;

    CairoCol(col, &red, &green, &blue);

    /* This optimization should not be necessary, but alpha = 1 seems
       to cause image fallback in some backends */
    if (alpha == 255)
	cairo_set_source_rgb(xd->cc, red, green, blue);
    else
	cairo_set_source_rgba(xd->cc, red, green, blue, alpha/255.0);
}

/*
 ***************************
 * Patterns
 ***************************
 */

/* Just a starting value */
#define maxPatterns 64

static void CairoInitPatterns(pX11Desc xd)
{
    int i;
    xd->numPatterns = maxPatterns;
    xd->patterns = malloc(sizeof(cairo_pattern_t*) * xd->numPatterns);
    for (i = 0; i < xd->numPatterns; i++) {
        xd->patterns[i] = NULL;
    }
}

static int CairoGrowPatterns(pX11Desc xd)
{
    int i, newMax = 2*xd->numPatterns;
    void *tmp;
    tmp = realloc(xd->patterns, sizeof(cairo_pattern_t*) * newMax);
    if (!tmp) { 
        warning(_("Cairo patterns exhausted (failed to increase maxPatterns)"));
        return 0;
    }
    xd->patterns = tmp;
    for (i = xd->numPatterns; i < newMax; i++) {
        xd->patterns[i] = NULL;
    }
    xd->numPatterns = newMax;
    return 1;
}

static void CairoCleanPatterns(pX11Desc xd)
{
    int i;
    for (i = 0; i < xd->numPatterns; i++) {
        if (xd->patterns[i] != NULL) {
            cairo_pattern_destroy(xd->patterns[i]);
            xd->patterns[i] = NULL;
        }
    }    
}

static void CairoDestroyPatterns(pX11Desc xd)
{
    int i;
    for (i = 0; i < xd->numPatterns; i++) {
        if (xd->patterns[i] != NULL) {
            cairo_pattern_destroy(xd->patterns[i]);
        }
    }    
    free(xd->patterns);
}

static int CairoNewPatternIndex(pX11Desc xd)
{
    int i;
    for (i = 0; i < xd->numPatterns; i++) {
        if (xd->patterns[i] == NULL) {
            return i;
        } else {
            if (i == (xd->numPatterns - 1) &&
                !CairoGrowPatterns(xd)) {
                return -1;
            }
        }
    }    
    /* Should never get here, but just in case */
    warning(_("Cairo patterns exhausted"));
    return -1;
}

static cairo_pattern_t* CairoLinearGradient(SEXP gradient, pX11Desc xd)
{
    unsigned int col;
    unsigned int alpha;
    double red, blue, green;
    int i, nStops = R_GE_linearGradientNumStops(gradient);
    double stop;
    cairo_extend_t extend = CAIRO_EXTEND_NONE;
    cairo_pattern_t *cairo_gradient;  
    cairo_gradient = 
        cairo_pattern_create_linear(R_GE_linearGradientX1(gradient),
                                    R_GE_linearGradientY1(gradient),
                                    R_GE_linearGradientX2(gradient),
                                    R_GE_linearGradientY2(gradient));
    for (i = 0; i < nStops; i++) {
        col = R_GE_linearGradientColour(gradient, i);
        stop = R_GE_linearGradientStop(gradient, i);
        CairoCol(col, &red, &green, &blue);
        alpha = R_ALPHA(col);
        if (alpha == 255)
            cairo_pattern_add_color_stop_rgb(cairo_gradient, stop, 
                                             red, green, blue);
        else
            cairo_pattern_add_color_stop_rgba(cairo_gradient, stop, 
                                              red, green, blue, alpha/255.0);
    }
    switch(R_GE_linearGradientExtend(gradient)) {
    case R_GE_patternExtendNone: extend = CAIRO_EXTEND_NONE; break;
    case R_GE_patternExtendPad: extend = CAIRO_EXTEND_PAD; break;
    case R_GE_patternExtendReflect: extend = CAIRO_EXTEND_REFLECT; break;
    case R_GE_patternExtendRepeat: extend = CAIRO_EXTEND_REPEAT; break;
    }
    cairo_pattern_set_extend(cairo_gradient, extend);    
    return cairo_gradient;
}

static cairo_pattern_t* CairoRadialGradient(SEXP gradient, pX11Desc xd)
{
    unsigned int col;
    unsigned int alpha;
    double red, blue, green;
    int i, nStops = R_GE_radialGradientNumStops(gradient);
    double stop;
    cairo_extend_t extend = CAIRO_EXTEND_NONE;
    cairo_pattern_t *cairo_gradient;  
    cairo_gradient = 
        cairo_pattern_create_radial(R_GE_radialGradientCX1(gradient),
                                    R_GE_radialGradientCY1(gradient),
                                    R_GE_radialGradientR1(gradient),
                                    R_GE_radialGradientCX2(gradient),
                                    R_GE_radialGradientCY2(gradient),
                                    R_GE_radialGradientR2(gradient));
    for (i = 0; i < nStops; i++) {
        col = R_GE_radialGradientColour(gradient, i);
        stop = R_GE_radialGradientStop(gradient, i);
        CairoCol(col, &red, &green, &blue);
        alpha = R_ALPHA(col);
        if (alpha == 255)
            cairo_pattern_add_color_stop_rgb(cairo_gradient, stop, 
                                             red, green, blue);
        else
            cairo_pattern_add_color_stop_rgba(cairo_gradient, stop, 
                                              red, green, blue, alpha/255.0);
    }
    switch(R_GE_radialGradientExtend(gradient)) {
    case R_GE_patternExtendNone: extend = CAIRO_EXTEND_NONE; break;
    case R_GE_patternExtendPad: extend = CAIRO_EXTEND_PAD; break;
    case R_GE_patternExtendReflect: extend = CAIRO_EXTEND_REFLECT; break;
    case R_GE_patternExtendRepeat: extend = CAIRO_EXTEND_REPEAT; break;
    }
    cairo_pattern_set_extend(cairo_gradient, extend);    
    return cairo_gradient;
}

static cairo_pattern_t *CairoTilingPattern(SEXP pattern, pX11Desc xd)
{
    cairo_t *cc = xd->cc;
    SEXP R_fcall;
    cairo_pattern_t *cairo_tiling;
    cairo_extend_t extend = CAIRO_EXTEND_NONE;
    /* Start new group - drawing is redirected to this group */
    cairo_push_group(cc);
    /* Scale the drawing to fill the temporary group surface */
    cairo_matrix_t tm;
    cairo_matrix_init_identity(&tm);
    cairo_matrix_scale(&tm, 
                       xd->windowWidth/R_GE_tilingPatternWidth(pattern),
                       xd->windowHeight/R_GE_tilingPatternHeight(pattern));
    cairo_matrix_translate(&tm, 
                           -R_GE_tilingPatternX(pattern),
                           -R_GE_tilingPatternY(pattern));
    cairo_set_matrix(cc, &tm);
    /* Play the pattern function to build the pattern */
    R_fcall = PROTECT(lang1(R_GE_tilingPatternFunction(pattern)));
    eval(R_fcall, R_GlobalEnv);
    UNPROTECT(1);
    /* Close group and return resulting pattern */
    cairo_tiling = cairo_pop_group(cc);
    /* Scale the pattern to its proper size */
    cairo_matrix_init_identity(&tm);
    cairo_matrix_scale(&tm, 
                       xd->windowWidth/R_GE_tilingPatternWidth(pattern),
                       xd->windowHeight/R_GE_tilingPatternHeight(pattern));
    cairo_matrix_translate(&tm, 
                           -R_GE_tilingPatternX(pattern), 
                           -R_GE_tilingPatternY(pattern));
    cairo_pattern_set_matrix(cairo_tiling, &tm);
    switch(R_GE_tilingPatternExtend(pattern)) {
    case R_GE_patternExtendNone: extend = CAIRO_EXTEND_NONE; break;
    case R_GE_patternExtendPad: extend = CAIRO_EXTEND_PAD; break;
    case R_GE_patternExtendReflect: extend = CAIRO_EXTEND_REFLECT; break;
    case R_GE_patternExtendRepeat: extend = CAIRO_EXTEND_REPEAT; break;
    }
    cairo_pattern_set_extend(cairo_tiling, extend);
    return cairo_tiling;
}

static cairo_pattern_t *CairoCreatePattern(SEXP pattern, pX11Desc xd)
{
    cairo_pattern_t *cairo_pattern = NULL;
    switch(R_GE_patternType(pattern)) {
    case R_GE_linearGradientPattern: 
        cairo_pattern = CairoLinearGradient(pattern, xd);
        break;
    case R_GE_radialGradientPattern:
        cairo_pattern = CairoRadialGradient(pattern, xd);            
        break;
    case R_GE_tilingPattern:
        cairo_pattern = CairoTilingPattern(pattern, xd);
        break;
    }
    return cairo_pattern;
}

static int CairoSetPattern(SEXP pattern, pX11Desc xd)
{
    int index = CairoNewPatternIndex(xd);
    if (index >= 0) {
        cairo_pattern_t *cairo_pattern = CairoCreatePattern(pattern, xd);

        xd->patterns[index] = cairo_pattern;
    }
    return index;
}

static void CairoReleasePattern(int index, pX11Desc xd)
{
    if (xd->patterns[index]) {
        cairo_pattern_destroy(xd->patterns[index]);
        xd->patterns[index] = NULL;
    } else {
        warning(_("Attempt to release non-existent pattern"));
    }
}

static void CairoPatternFill(SEXP ref, pX11Desc xd)
{
    int index = INTEGER(ref)[0];
    if (index >= 0) {
        cairo_set_source(xd->cc, xd->patterns[index]);
    } else {
        /* Patterns may have been exhausted */
	cairo_set_source_rgba(xd->cc, 0.0, 0.0, 0.0, 0.0);
    }
    cairo_fill_preserve(xd->cc);
}

/*
 ***************************
 * Clipping paths
 ***************************
 */

/* Just a starting value */
#define maxClipPaths 64

static void CairoInitClipPaths(pX11Desc xd)
{
    int i;
    /* Zero clip paths */
    xd->numClipPaths = maxClipPaths;
    xd->clippaths = malloc(sizeof(cairo_path_t*) * xd->numClipPaths);
    for (i = 0; i < xd->numClipPaths; i++) {
        xd->clippaths[i] = NULL;
    }
}

static int CairoGrowClipPaths(pX11Desc xd)
{
    int i, newMax = 2*xd->numClipPaths;
    void *tmp;
    tmp = realloc(xd->clippaths, sizeof(cairo_path_t*) * newMax);
    if (!tmp) { 
        warning(_("Cairo clipping paths exhausted (failed to increase maxClipPaths)"));
        return 0;
    }
    xd->clippaths = tmp;
    for (i = xd->numClipPaths; i < newMax; i++) {
        xd->clippaths[i] = NULL;
    }
    xd->numClipPaths = newMax;
    return 1;
}

static void CairoCleanClipPaths(pX11Desc xd)
{
    int i;
    for (i = 0; i < xd->numClipPaths; i++) {
        if (xd->clippaths[i] != NULL) {
            cairo_path_destroy(xd->clippaths[i]);
            xd->clippaths[i] = NULL;
        }
    }    
}

static void CairoDestroyClipPaths(pX11Desc xd)
{
    int i;
    for (i = 0; i < xd->numClipPaths; i++) {
        if (xd->clippaths[i] != NULL) {
            cairo_path_destroy(xd->clippaths[i]);
            xd->clippaths[i] = NULL;
        }
    }    
    free(xd->clippaths);
}

static int CairoNewClipPathIndex(pX11Desc xd)
{
    int i;
    for (i = 0; i < xd->numClipPaths; i++) {
        if (xd->clippaths[i] == NULL) {
            return i;
        } else {
            if (i == (xd->numClipPaths - 1) &&
                !CairoGrowClipPaths(xd)) {
                return -1;
            }
        }
    }    
    warning(_("Cairo clipping paths exhausted"));
    return -1;
}

static cairo_path_t* CairoCreateClipPath(SEXP clipPath, int index, pX11Desc xd)
{
    cairo_t *cc = xd->cc;
    SEXP R_fcall;
    cairo_path_t *cairo_clippath;
    /* Save the current path */
    cairo_path_t *cairo_saved_path = cairo_copy_path(cc);
    /* Increment the "appending" count */
    xd->appending++;
    /* Clear the current path */
    cairo_new_path(cc);
    /* Play the clipPath function to build the clipping path */
    R_fcall = PROTECT(lang1(clipPath));
    eval(R_fcall, R_GlobalEnv);
    UNPROTECT(1);
    /* Apply path fill rule */
    switch (R_GE_clipPathFillRule(clipPath)) {
    case R_GE_nonZeroWindingRule: 
        cairo_set_fill_rule(xd->cc, CAIRO_FILL_RULE_WINDING); break;
    case R_GE_evenOddRule:
        cairo_set_fill_rule(xd->cc, CAIRO_FILL_RULE_EVEN_ODD); break;
    }
    /* Set the clipping region from the path */
    cairo_reset_clip(cc);
    cairo_clip_preserve(cc);
    /* Save the clipping path (for reuse) */
    cairo_clippath = cairo_copy_path(cc);
    /* Clear the path again */
    cairo_new_path(cc);
    /* Decrement the "appending" count */
    xd->appending--;
    /* Restore the saved path */
    cairo_append_path(cc, cairo_saved_path);
    /* Destroy the saved path */
    cairo_path_destroy(cairo_saved_path);
    /* Return the clipping path */
    return cairo_clippath;
}

static void CairoReuseClipPath(cairo_path_t *cairo_clippath, pX11Desc xd)
{
    cairo_t *cc = xd->cc;
    /* Save the current path */
    cairo_path_t *cairo_saved_path = cairo_copy_path(cc);
    /* Clear the current path */
    cairo_new_path(cc);
    /* Append the clipping path */
    cairo_append_path(cc, cairo_clippath);
    /* Set the clipping region from the path (which clears the path) */
    cairo_reset_clip(cc);
    cairo_clip(cc);
    /* Restore the saved path */
    cairo_append_path(cc, cairo_saved_path);
    /* Destroy the saved path */
    cairo_path_destroy(cairo_saved_path);
}

static SEXP CairoSetClipPath(SEXP path, SEXP ref, pX11Desc xd)
{
    cairo_path_t *cairo_clippath;
    SEXP newref = R_NilValue;
    int index;

    if (isNull(ref)) {
        /* Must generate new ref */
        index = CairoNewClipPathIndex(xd);
        if (index < 0) {
            /* Unless we have run out of space */
        } else {
            /* Create this clipping path */
            cairo_clippath = CairoCreateClipPath(path, index, xd);
            xd->clippaths[index] = cairo_clippath;
            PROTECT(newref = allocVector(INTSXP, 1));
            INTEGER(newref)[0] = index;
            UNPROTECT(1);
        }
    } else {
        /* Reuse indexed clip path */
        int index = INTEGER(ref)[0];
        if (xd->clippaths[index]) {
            CairoReuseClipPath(xd->clippaths[index], xd);
        } else {
            /* BUT if index clip path does not exist, create a new one */
            cairo_clippath = CairoCreateClipPath(path, index, xd);
            xd->clippaths[index] = cairo_clippath;
            warning(_("Attempt to reuse non-existent clipping path"));
        }
    }

    return newref;
}

static void CairoReleaseClipPath(int index, pX11Desc xd)
{
    if (xd->clippaths[index]) {
        cairo_path_destroy(xd->clippaths[index]);
        xd->clippaths[index] = NULL;
    } else {
        warning(_("Attempt to release non-existent clipping path"));
    }
}

/*
 ***************************
 * Masks
 ***************************
 */

/* Just a starting value */
#define maxMasks 64

static void CairoInitMasks(pX11Desc xd)
{
    int i;
    xd->numMasks = 20;
    xd->masks = malloc(sizeof(cairo_pattern_t*) * xd->numMasks);
    for (i = 0; i < xd->numMasks; i++) {
        xd->masks[i] = NULL;
    }
    xd->currentMask = -1;
}

static int CairoGrowMasks(pX11Desc xd)
{
    int i, newMax = 2*xd->numMasks;
    void *tmp;
    tmp = realloc(xd->masks, sizeof(cairo_pattern_t*) * newMax);
    if (!tmp) { 
        warning(_("Cairo masks exhausted (failed to increase maxMasks)"));
        return 0;
    }
    xd->masks = tmp;
    for (i = xd->numMasks; i < newMax; i++) {
        xd->masks[i] = NULL;
    }
    xd->numMasks = newMax;
    return 1;
}

static void CairoCleanMasks(pX11Desc xd)
{
    int i;
    for (i = 0; i < xd->numMasks; i++) {
        if (xd->masks[i] != NULL) {
            cairo_pattern_destroy(xd->masks[i]);
            xd->masks[i] = NULL;
        }
    }    
    xd->currentMask = -1;
}

static void CairoDestroyMasks(pX11Desc xd)
{
    int i;
    for (i = 0; i < xd->numMasks; i++) {
        if (xd->masks[i] != NULL) {
            cairo_pattern_destroy(xd->masks[i]);
            xd->masks[i] = NULL;
        }
    }    
    free(xd->masks);
}

static int CairoNewMaskIndex(pX11Desc xd)
{
    int i;
    for (i = 0; i < xd->numMasks; i++) {
        if (xd->masks[i] == NULL) {
            return i;
        } else {
            if (i == (xd->numMasks - 1) &&
                !CairoGrowMasks(xd)) {
                return -1;
            }
        }
    }    
    warning(_("Cairo masks exhausted"));
    return -1;
}

static cairo_pattern_t *CairoCreateMask(SEXP mask, pX11Desc xd)
{
    cairo_t *cc = xd->cc;
    SEXP R_fcall;
    /* Start new group - drawing is redirected to this group */
    cairo_push_group(cc);
    /* Start with "over" operator */
    cairo_set_operator(cc, CAIRO_OPERATOR_OVER);
    /* Play the mask function to build the mask */
    R_fcall = PROTECT(lang1(mask));
    eval(R_fcall, R_GlobalEnv);
    UNPROTECT(1);
    /* Close group and return resulting mask */
    return cairo_pop_group(cc);
}

static SEXP CairoSetMask(SEXP mask, SEXP ref, pX11Desc xd)
{
    int index;
    cairo_pattern_t *cairo_mask;
    SEXP newref = R_NilValue;

    if (isNull(mask)) {
        /* Set NO mask */
        index = -1;
    } else if (R_GE_maskType(mask) == R_GE_luminanceMask) {
        warning(_("Ignored luminance mask (not supported on this device)"));
        /* Set NO mask */
        index = -1;        
    } else {
        if (isNull(ref)) {
            /* Create a new mask */
            index = CairoNewMaskIndex(xd);
            if (index >= 0) {
                cairo_mask = CairoCreateMask(mask, xd);
                xd->masks[index] = cairo_mask;
            }
        } else {
            /* Reuse existing mask */
            index = INTEGER(ref)[0];
            if (index >= 0 && !xd->masks[index]) {
                /* But if it does not exist, make a new one */
                index = CairoNewMaskIndex(xd);
                if (index >= 0) {
                    cairo_mask = CairoCreateMask(mask, xd);
                    xd->masks[index] = cairo_mask;
                }
            }
        }
        newref = PROTECT(allocVector(INTSXP, 1));
        INTEGER(newref)[0] = index;
        UNPROTECT(1);
    }

    xd->currentMask = index;

    return newref;
}

static void CairoReleaseMask(int index, pX11Desc xd)
{
    if (xd->masks[index]) {
        cairo_pattern_destroy(xd->masks[index]);
        xd->masks[index] = NULL;
    } else {
        warning(_("Attempt to release non-existent mask"));
    }
}

/*
 ***************************
 * Groups
 ***************************
 */

/* Just a starting value */
#define maxGroups 64

static void CairoInitGroups(pX11Desc xd)
{
    int i;
    xd->numGroups = 20;
    xd->groups = malloc(sizeof(cairo_pattern_t*) * xd->numGroups);
    for (i = 0; i < xd->numGroups; i++) {
        xd->groups[i] = NULL;
    }
    xd->nullGroup = cairo_pattern_create_rgb(0, 0, 0);
    xd->currentGroup = -1;
}

static int CairoGrowGroups(pX11Desc xd)
{
    int i, newMax = 2*xd->numGroups;
    void *tmp;
    tmp = realloc(xd->groups, sizeof(cairo_pattern_t*) * newMax);
    if (!tmp) { 
        warning(_("Cairo groups exhausted (failed to increase maxGroups)"));
        return 0;
    }
    xd->groups = tmp;
    for (i = xd->numGroups; i < newMax; i++) {
        xd->groups[i] = NULL;
    }
    xd->numGroups = newMax;
    return 1;
}

static void CairoCleanGroups(pX11Desc xd)
{
    int i;
    for (i = 0; i < xd->numGroups; i++) {
        if (xd->groups[i] != NULL &&
            xd->groups[i] != xd->nullGroup) {
            cairo_pattern_destroy(xd->groups[i]);
            xd->groups[i] = NULL;
        }
    }    
    xd->currentGroup = -1;
}

static void CairoDestroyGroups(pX11Desc xd)
{
    int i;
    for (i = 0; i < xd->numGroups; i++) {
        if (xd->groups[i] != NULL &&
            xd->groups[i] != xd->nullGroup) {
            cairo_pattern_destroy(xd->groups[i]);
            xd->groups[i] = NULL;
        }
    }    
    free(xd->groups);
    cairo_pattern_destroy(xd->nullGroup);
}

static int CairoNewGroupIndex(pX11Desc xd)
{
    int i;
    for (i = 0; i < xd->numGroups; i++) {
        if (xd->groups[i] == NULL) {
            /* Place temporary hold on this slot in case of 
             * group within group */
            xd->groups[i] = xd->nullGroup;
            return i;
        } else {
            if (i == (xd->numGroups - 1) &&
                !CairoGrowGroups(xd)) {
                return -1;
            }
        }
    }    
    warning(_("Cairo groups exhausted"));
    return -1;
}


static cairo_operator_t CairoOperator(int op) 
{
    cairo_operator_t cairo_op = CAIRO_OPERATOR_OVER;
    switch(op) {
    case R_GE_compositeClear: cairo_op = CAIRO_OPERATOR_CLEAR; break;
    case R_GE_compositeSource: cairo_op = CAIRO_OPERATOR_SOURCE; break;
    case R_GE_compositeOver: cairo_op = CAIRO_OPERATOR_OVER; break;
    case R_GE_compositeIn: cairo_op = CAIRO_OPERATOR_IN; break;
    case R_GE_compositeOut: cairo_op = CAIRO_OPERATOR_OUT; break;
    case R_GE_compositeAtop: cairo_op = CAIRO_OPERATOR_ATOP; break;
    case R_GE_compositeDest: cairo_op = CAIRO_OPERATOR_DEST; break;
    case R_GE_compositeDestOver: cairo_op = CAIRO_OPERATOR_DEST_OVER; break;
    case R_GE_compositeDestIn: cairo_op = CAIRO_OPERATOR_DEST_IN; break;
    case R_GE_compositeDestOut: cairo_op = CAIRO_OPERATOR_DEST_OUT; break;
    case R_GE_compositeDestAtop: cairo_op = CAIRO_OPERATOR_DEST_ATOP; break;
    case R_GE_compositeXor: cairo_op = CAIRO_OPERATOR_XOR; break;
    case R_GE_compositeAdd: cairo_op = CAIRO_OPERATOR_ADD; break;
    case R_GE_compositeSaturate: cairo_op = CAIRO_OPERATOR_SATURATE; break;
    case R_GE_compositeMultiply: cairo_op = CAIRO_OPERATOR_MULTIPLY; break;
    case R_GE_compositeScreen: cairo_op = CAIRO_OPERATOR_SCREEN; break;
    case R_GE_compositeOverlay: cairo_op = CAIRO_OPERATOR_OVERLAY; break;
    case R_GE_compositeDarken: cairo_op = CAIRO_OPERATOR_DARKEN; break;
    case R_GE_compositeLighten: cairo_op = CAIRO_OPERATOR_LIGHTEN; break;
    case R_GE_compositeColorDodge: cairo_op = CAIRO_OPERATOR_COLOR_DODGE; break;
    case R_GE_compositeColorBurn: cairo_op = CAIRO_OPERATOR_COLOR_BURN; break;
    case R_GE_compositeHardLight: cairo_op = CAIRO_OPERATOR_HARD_LIGHT; break;
    case R_GE_compositeSoftLight: cairo_op = CAIRO_OPERATOR_SOFT_LIGHT; break;
    case R_GE_compositeDifference: cairo_op = CAIRO_OPERATOR_DIFFERENCE; break;
    case R_GE_compositeExclusion: cairo_op = CAIRO_OPERATOR_EXCLUSION; break;
    }
    return cairo_op;
}

static cairo_pattern_t *CairoCreateGroup(SEXP src, int op, SEXP dst, 
                                         pX11Desc xd)
{
    cairo_t *cc = xd->cc;
    cairo_pattern_t *cairo_group;
    SEXP R_fcall;

    /* Start new group - drawing is redirected to this group */
    cairo_push_group(cc);
    /* Start with "over" operator */
    cairo_set_operator(cc, CAIRO_OPERATOR_OVER);

    if (dst != R_NilValue) {
        /* Play the destination function to draw the destination */
        R_fcall = PROTECT(lang1(dst));
        eval(R_fcall, R_GlobalEnv);
        UNPROTECT(1);
    }
    /* Set the group operator */
    cairo_set_operator(cc, CairoOperator(op));
    /* Play the source function to draw the source */
    R_fcall = PROTECT(lang1(src));
    eval(R_fcall, R_GlobalEnv);
    UNPROTECT(1);

    /* Close group and return the resulting pattern */
    cairo_group = cairo_pop_group(cc);

    return cairo_group;
}

static SEXP CairoDefineGroup(SEXP src, int op, SEXP dst, pX11Desc xd)
{
    cairo_pattern_t *cairo_group;
    SEXP ref;
    int index;

    /* Create a new group */
    index = CairoNewGroupIndex(xd);
    if (index >= 0) {
        int savedGroup = xd->currentGroup;
        xd->currentGroup = index;
        cairo_group = CairoCreateGroup(src, op, dst, xd);
        xd->currentGroup = savedGroup;
        xd->groups[index] = cairo_group;
    }

    ref = PROTECT(allocVector(INTSXP, 1));
    INTEGER(ref)[0] = index;
    UNPROTECT(1);

    return ref;
}

static bool cairoBegin(pX11Desc xd);
static void cairoEnd(bool grouping, pX11Desc xd); 

static void CairoUseGroup(SEXP ref, SEXP trans, pX11Desc xd)
{
    cairo_t *cc = xd->cc;
    int index;
    cairo_matrix_t transform;
    bool grouping = false;

    index = INTEGER(ref)[0];
    if (index < 0) {
        warning(_("Groups exhausted"));
        return;
    }

    if (index >= 0 && !xd->groups[index]) {
        warning("Unknown group ");
        return;
    } 

    if (!xd->appending) {
        grouping = cairoBegin(xd);
    }

    /* Draw the group */
    cairo_save(cc);
    
    if (trans != R_NilValue) {
        transform.xx = REAL(trans)[0];
        transform.yx = REAL(trans)[3];
        transform.xy = REAL(trans)[1];
        transform.yy = REAL(trans)[4];
        transform.x0 = REAL(trans)[2];
        transform.y0 = REAL(trans)[5];
        
        cairo_transform(cc, &transform);
    } 

    cairo_set_source(cc, xd->groups[index]);
    cairo_paint(cc);

    cairo_restore(cc);

    if (!xd->appending) {
        cairoEnd(grouping, xd);
    }
}

static void CairoReleaseGroup(int index, pX11Desc xd)
{
    if (xd->groups[index]) {
        cairo_pattern_destroy(xd->groups[index]);
        xd->groups[index] = NULL;
    } else {
        warning(_("Attempt to release non-existent group"));
    }
}


/*
 ***************************
 * Rendering
 ***************************
 */
static void CairoLineType(const pGEcontext gc, pX11Desc xd)
{
    cairo_t *cc = xd->cc;
    double lwd = gc->lwd;
    cairo_line_cap_t lcap = CAIRO_LINE_CAP_SQUARE;
    cairo_line_join_t ljoin = CAIRO_LINE_JOIN_ROUND;
    switch(gc->lend){
    case GE_ROUND_CAP: lcap = CAIRO_LINE_CAP_ROUND; break;
    case GE_BUTT_CAP: lcap = CAIRO_LINE_CAP_BUTT; break;
    case GE_SQUARE_CAP: lcap = CAIRO_LINE_CAP_SQUARE; break;
    }
    switch(gc->ljoin){
    case GE_ROUND_JOIN: ljoin = CAIRO_LINE_JOIN_ROUND; break;
    case GE_MITRE_JOIN: ljoin = CAIRO_LINE_JOIN_MITER; break;
    case GE_BEVEL_JOIN: ljoin = CAIRO_LINE_JOIN_BEVEL; break;
    }
    cairo_set_line_width(cc, (lwd > 0.01 ? lwd : 0.01) * xd->lwdscale);
    cairo_set_line_cap(cc, lcap);
    cairo_set_line_join(cc, ljoin);
    cairo_set_miter_limit(cc, gc->lmitre);

    if (gc->lty == 0 || gc->lty == -1 || gc->lty == NA_INTEGER)
	cairo_set_dash(cc, 0, 0, 0);
    else {
	double ls[16], lwd = (gc->lwd > 1) ? gc->lwd : 1;
	int l;
        /* Use unsigned int otherwise right shift of 'dt'
           may not terminate for loop */
        unsigned int dt = gc->lty;
	for (l = 0; dt != 0; dt >>= 4, l++)
	    ls[l] = (dt & 0xF) * lwd * xd->lwdscale;
	cairo_set_dash(cc, ls, l, 0);
    }
}

static void Cairo_Clip(double x0, double x1, double y0, double y1,
		       pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    if (x1 < x0) { double h = x1; x1 = x0; x0 = h; };
    if (y1 < y0) { double h = y1; y1 = y0; y0 = h; };

    cairo_reset_clip(xd->cc);
    cairo_new_path(xd->cc);
    cairo_rectangle(xd->cc, x0, y0, x1 - x0, y1 - y0);
    cairo_clip(xd->cc);
}

static bool implicitGroup(pX11Desc xd) {
    return xd->currentGroup >= 0 && 
        (cairo_get_operator(xd->cc) == CAIRO_OPERATOR_CLEAR ||
         cairo_get_operator(xd->cc) == CAIRO_OPERATOR_SOURCE);
}

/* Set up for drawing a shape */
static bool cairoBegin(pX11Desc xd) 
{
    bool grouping = implicitGroup(xd);
    if (xd->currentMask >= 0) {
        /* If masking, draw temporary pattern */
        cairo_push_group(xd->cc);
    }
    if (grouping) {
        cairo_push_group(xd->cc);
    }
    return grouping;
}

static void cairoEnd(bool grouping, pX11Desc xd) 
{
    if (grouping) {
        cairo_pattern_t *source = cairo_pop_group(xd->cc);
        cairo_set_source(xd->cc, source);
        cairo_paint(xd->cc);
        cairo_pattern_destroy(source);            
    }
    if (xd->currentMask >= 0) {
        /* If masking, use temporary pattern as source and mask that */
        cairo_pattern_t *source = cairo_pop_group(xd->cc);
        cairo_pattern_t *mask = xd->masks[xd->currentMask];
        cairo_set_source(xd->cc, source);
        cairo_mask(xd->cc, mask);
        /* Release temporary pattern */
        cairo_pattern_destroy(source);
    }
}

static void cairoFill(const pGEcontext gc, pX11Desc xd) 
{
    /* patternFill overrides fill */
    if (gc->patternFill != R_NilValue) { 
        CairoPatternFill(gc->patternFill, xd);
    } else if (R_ALPHA(gc->fill) > 0) {
        cairo_set_antialias(xd->cc, CAIRO_ANTIALIAS_NONE);
        CairoColor(gc->fill, xd);
        cairo_fill_preserve(xd->cc);
        cairo_set_antialias(xd->cc, xd->antialias);
    }
}

static void cairoStroke(const pGEcontext gc, pX11Desc xd) 
{
    if (R_ALPHA(gc->col) > 0 && gc->lty != -1) {
        CairoColor(gc->col, xd);
        CairoLineType(gc, xd);
        cairo_stroke(xd->cc);
    }
}

static void cairoRectPath(double x0, double y0, double x1, double y1, 
                          pX11Desc xd) 
{
    cairo_rectangle(xd->cc, x0, y0, x1 - x0, y1 - y0);
}

static void cairoRect(double x0, double y0, double x1, double y1,
                      const pGEcontext gc, pDevDesc dd, int op) 
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    bool grouping = cairoBegin(xd);
    cairo_new_path(xd->cc);
    cairoRectPath(x0, y0, x1, y1, xd);
    if (op) { /* fill */
        cairoFill(gc, xd);
    } else {
        cairoStroke(gc, xd);
    }
    cairoEnd(grouping, xd);
}

static void Cairo_Rect(double x0, double y0, double x1, double y1,
		       const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    if (xd->appending) {
        cairoRectPath(x0, y0, x1, y1, xd);
    } else {
        bool fill = (gc->patternFill != R_NilValue) || 
            (R_ALPHA(gc->fill) > 0);
        bool stroke = (R_ALPHA(gc->col) > 0 && gc->lty != -1);
        if (fill && stroke) {
            cairoRect(x0, y0, x1, y1, gc, dd, 1); /* fill */
            cairoRect(x0, y0, x1, y1, gc, dd, 0); /* stroke */
        } else if (fill) {
            cairoRect(x0, y0, x1, y1, gc, dd, 1);
        } else if (stroke) {
            cairoRect(x0, y0, x1, y1, gc, dd, 0);
        }        
    }
}

static void cairoCirclePath(double x, double y, double r,
                            pX11Desc xd) 
{
    /* move to start of circle arc */
    cairo_new_sub_path(xd->cc);
    /* radius 0.5 seems to be visible */
    cairo_arc(xd->cc, x, y, (r > 0.5 ? r : 0.5), 0.0, 2 * M_PI);
}

static void cairoCircle(double x, double y, double r,
			 const pGEcontext gc, pDevDesc dd, int op)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    bool grouping = cairoBegin(xd);
    cairo_new_path(xd->cc);
    cairoCirclePath(x, y, r, xd);
    if (op) { /* fill */
        cairoFill(gc, xd);
    } else {
        cairoStroke(gc, xd);
    }
    cairoEnd(grouping, xd);    
}

static void Cairo_Circle(double x, double y, double r,
			 const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    if (xd->appending) {
        cairoCirclePath(x, y, r, xd);
    } else {
        bool fill = (gc->patternFill != R_NilValue) || 
            (R_ALPHA(gc->fill) > 0);
        bool stroke = (R_ALPHA(gc->col) > 0 && gc->lty != -1);
        if (fill && stroke) {
            cairoCircle(x, y, r, gc, dd, 1); /* fill */
            cairoCircle(x, y, r, gc, dd, 0); /* stroke */
        } else if (fill) {
            cairoCircle(x, y, r, gc, dd, 1);
        } else if (stroke) {
            cairoCircle(x, y, r, gc, dd, 0);
        }        
    }
}

static void cairoLinePath(double x1, double y1, double x2, double y2,
                          pX11Desc xd) 
{
    cairo_move_to(xd->cc, x1, y1);
    cairo_line_to(xd->cc, x2, y2);
}

static void cairoLine(double x1, double y1, double x2, double y2,
		       const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    bool grouping = cairoBegin(xd);
    cairo_new_path(xd->cc);
    cairoLinePath(x1, y1, x2, y2, xd);
    cairoStroke(gc, xd);
    cairoEnd(grouping, xd);
}

static void Cairo_Line(double x1, double y1, double x2, double y2,
		       const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    if (xd->appending) {
        cairoLinePath(x1, y1, x2, y2, xd);
    } else {
        bool stroke = (R_ALPHA(gc->col) > 0 && gc->lty != -1);
        if (stroke) {
            cairoLine(x1, y1, x2, y2, gc, dd);
        }        
    }
}

static void cairoPolylinePath(int n, double *x, double *y,
                          pX11Desc xd) 
{
    int i;
    cairo_move_to(xd->cc, x[0], y[0]);
    for(i = 0; i < n; i++) cairo_line_to(xd->cc, x[i], y[i]);
}

static void cairoPolyline(int n, double *x, double *y,
			   const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    bool grouping = cairoBegin(xd);
    cairo_new_path(xd->cc);
    cairoPolylinePath(n, x, y, xd);
    cairoStroke(gc, xd);
    cairoEnd(grouping, xd);
}

static void Cairo_Polyline(int n, double *x, double *y,
			   const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    if (xd->appending) {
        cairoPolylinePath(n, x, y, xd);
    } else {
        bool stroke = (R_ALPHA(gc->col) > 0 && gc->lty != -1);
        if (stroke) {
            cairoPolyline(n, x, y, gc, dd);
        }        
    }
}

static void cairoPolygonPath(int n, double *x, double *y,
                             pX11Desc xd) 
{
    int i;
    cairo_move_to(xd->cc, x[0], y[0]);
    for(i = 0; i < n; i++) cairo_line_to(xd->cc, x[i], y[i]);
    cairo_close_path(xd->cc);
}

static void cairoPolygon(int n, double *x, double *y,
                         const pGEcontext gc, pDevDesc dd, int op)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    bool grouping = cairoBegin(xd);
    cairo_new_path(xd->cc);
    cairoPolygonPath(n, x, y, xd);
    if (op) { /* fill */
        cairoFill(gc, xd);
    } else {
        cairoStroke(gc, xd);
    }
    cairoEnd(grouping, xd);    
}

static void Cairo_Polygon(int n, double *x, double *y,
			  const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    if (xd->appending) {
        cairoPolygonPath(n, x, y, xd);
    } else {
        bool fill = (gc->patternFill != R_NilValue) || 
            (R_ALPHA(gc->fill) > 0);
        bool stroke = (R_ALPHA(gc->col) > 0 && gc->lty != -1);
        if (fill && stroke) {
            cairoPolygon(n, x, y, gc, dd, 1); /* fill */
            cairoPolygon(n, x, y, gc, dd, 0); /* stroke */
        } else if (fill) {
            cairoPolygon(n, x, y, gc, dd, 1);
        } else if (stroke) {
            cairoPolygon(n, x, y, gc, dd, 0);
        }        
    }
}

static void cairoPathPath(double *x, double *y, int npoly, int *nper,
                          Rboolean winding,
                          pX11Desc xd) 
{
    int i, j, n;
    n = 0;
    for (i=0; i < npoly; i++) {
        cairo_move_to(xd->cc, x[n], y[n]);
        n++;
        for(j=1; j < nper[i]; j++) {
            cairo_line_to(xd->cc, x[n], y[n]);
            n++;
        }
        cairo_close_path(xd->cc);
    }
}

static void cairoPath(double *x, double *y, int npoly, int *nper,
                      Rboolean winding,
                      const pGEcontext gc, pDevDesc dd, int op)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    bool grouping = cairoBegin(xd);
    cairo_new_path(xd->cc);
    cairoPathPath(x, y, npoly, nper, winding, xd);
    if (op) { /* fill */
        if (winding) 
            cairo_set_fill_rule(xd->cc, CAIRO_FILL_RULE_WINDING);
        else 
            cairo_set_fill_rule(xd->cc, CAIRO_FILL_RULE_EVEN_ODD);
        cairoFill(gc, xd);
    } else {
        cairoStroke(gc, xd);
    }
    cairoEnd(grouping, xd);    
}

static void Cairo_Path(double *x, double *y,
                       int npoly, int *nper,
                       Rboolean winding,
                       const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    if (xd->appending) {
        cairoPathPath(x, y, npoly, nper, winding, xd);
    } else {
        bool fill = (gc->patternFill != R_NilValue) || 
            (R_ALPHA(gc->fill) > 0);
        bool stroke = (R_ALPHA(gc->col) > 0 && gc->lty != -1);
        if (fill && stroke) {
            cairoPath(x, y, npoly, nper, winding, gc, dd, 1); /* fill */
            cairoPath(x, y, npoly, nper, winding, gc, dd, 0); /* stroke */
        } else if (fill) {
            cairoPath(x, y, npoly, nper, winding, gc, dd, 1);
        } else if (stroke) {
            cairoPath(x, y, npoly, nper, winding, gc, dd, 0);
        }        
    }
}

static cairo_surface_t* createImageSurface(unsigned int *raster, int w, int h)
{
    int i;
    cairo_surface_t *image;
    unsigned char *imageData;

    imageData = (unsigned char *) R_alloc(4*w*h, sizeof(unsigned char));
    /* The R ABGR needs to be converted to a Cairo ARGB 
     * AND values need to by premultiplied by alpha 
     */
    for (i=0; i<w*h; i++) {
        int alpha = R_ALPHA(raster[i]);
        imageData[i*4 + 3] = (unsigned char) alpha;
        if (alpha < 255) {
            imageData[i*4 + 2] = (unsigned char)(R_RED(raster[i]) * alpha / 255);
	    imageData[i*4 + 1] = (unsigned char)(R_GREEN(raster[i]) * alpha / 255);
	    imageData[i*4 + 0] = (unsigned char)(R_BLUE(raster[i]) * alpha / 255);
        } else {
            imageData[i*4 + 2] = R_RED(raster[i]);
            imageData[i*4 + 1] = R_GREEN(raster[i]);
            imageData[i*4 + 0] = R_BLUE(raster[i]);
        }
    }
    image = cairo_image_surface_create_for_data(imageData, 
                                                CAIRO_FORMAT_ARGB32,
                                                w, h, 
                                                4*w);
    return(image);
}

static void Cairo_Raster(unsigned int *raster, int w, int h,
                         double x, double y, 
                         double width, double height,
                         double rot, 
                         Rboolean interpolate,
                         const pGEcontext gc, pDevDesc dd)
{
    int imageWidth, imageHeight;
    const void *vmax = vmaxget();
    cairo_surface_t *image;
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    /* 
     * A raster image adds nothing to a clipping path (?)
     */
    if (xd->appending) 
        return;
    
    cairo_save(xd->cc);

    bool grouping = cairoBegin(xd);

    /* If we are going to use the graphics engine for interpolation
     * the image used for the Cairo surface is going to be a
     * different size
     */
    // clang 15 prefers & here for constant expressions.
    if (interpolate & (CAIRO_VERSION_MAJOR < 2) & (CAIRO_VERSION_MINOR < 6)) {
        imageWidth = (int) (width + .5);
        imageHeight = abs((int) (height + .5));
    } else {    
        imageWidth = w;
        imageHeight = h;
    }

    cairo_translate(xd->cc, x, y);
    cairo_rotate(xd->cc, -rot*M_PI/180);
    cairo_scale(xd->cc, width/imageWidth, height/imageHeight);
    /* Flip vertical first */
    cairo_translate(xd->cc, 0, imageHeight/2.0);
    cairo_scale(xd->cc, 1, -1);
    cairo_translate(xd->cc, 0, -imageHeight/2.0);

    if (interpolate) {
	// clang 15 prefers & here for constant expressions.
        if ((CAIRO_VERSION_MAJOR < 2) & (CAIRO_VERSION_MINOR < 6)) {
            /* CAIRO_EXTEND_PAD not supported for image sources 
             * so use graphics engine for interpolation 
             */
            unsigned int *rasterImage;
            rasterImage = (unsigned int *) R_alloc(imageWidth * imageHeight,
                                                   sizeof(unsigned int));
            R_GE_rasterInterpolate(raster, w, h, 
                                   rasterImage, imageWidth, imageHeight);
            image = createImageSurface(rasterImage, imageWidth, imageHeight);
            cairo_set_source_surface(xd->cc, image, 0, 0);
        } else {
            image = createImageSurface(raster, w, h);
            cairo_set_source_surface(xd->cc, image, 0, 0);
            cairo_pattern_set_filter(cairo_get_source(xd->cc), 
                                     CAIRO_FILTER_BILINEAR);
            cairo_pattern_set_extend(cairo_get_source(xd->cc), 
                                     CAIRO_EXTEND_PAD);
        }
    } else {
        image = createImageSurface(raster, w, h);
        cairo_set_source_surface(xd->cc, image, 0, 0);
        cairo_pattern_set_filter(cairo_get_source(xd->cc), 
                                 CAIRO_FILTER_NEAREST);
    }

    cairo_new_path(xd->cc);
    cairo_rectangle(xd->cc, 0, 0, imageWidth, imageHeight);
    cairo_clip(xd->cc);
    cairo_paint(xd->cc); 

    cairoEnd(grouping, xd);

    cairo_restore(xd->cc);
    cairo_surface_destroy(image);

    vmaxset(vmax);
}

#ifndef NO_X11
static SEXP Cairo_Cap(pDevDesc dd)
{
    int i, width, height, size;
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    cairo_surface_t* screen;
    cairo_format_t format;
    unsigned int *screenData;
    SEXP dim, raster = R_NilValue;
    unsigned int *rint;

    screen = cairo_surface_reference(cairo_get_target(xd->cc));
    width = cairo_image_surface_get_width(screen);
    height = cairo_image_surface_get_height(screen);
    screenData = (unsigned int*) cairo_image_surface_get_data(screen);

    /* The type of image surface will depend on what sort
     * of X11 color model has been used */
    format = cairo_image_surface_get_format(screen);
    /* For now, if format is not RGB24 just bail out */
    if (format != CAIRO_FORMAT_RGB24) {
	cairo_surface_destroy(screen);
        return raster;
    }
        
    size = width*height;

    /* FIXME: the screen surface reference will leak if allocVector() fails */
    PROTECT(raster = allocVector(INTSXP, size));

    /* Copy each byte of screen to an R matrix. 
     * The Cairo RGB24 needs to be converted to an R ABGR32.
     * Cairo uses native endiannes (A=msb,R,G,B=lsb) so use int* instead of char* */
    rint = (unsigned int *) INTEGER(raster);
    for (i = 0; i < size; i++)
        rint[i] = R_RGB((screenData[i] >> 16) & 255, (screenData[i] >> 8) & 255, screenData[i] & 255);
    
    /* Release MY reference to the screen surface (do it here in case anything fails below) */
    cairo_surface_destroy(screen);    

    PROTECT(dim = allocVector(INTSXP, 2));
    INTEGER(dim)[0] = height;
    INTEGER(dim)[1] = width;
    setAttrib(raster, R_DimSymbol, dim);

    UNPROTECT(2);
    return raster;
}
#endif

#ifdef HAVE_PANGOCAIRO
/* ------------- pangocairo section --------------- */

SEXP in_CairoFT(void) 
{
    return mkString("");
}

static PangoFontDescription 
*PG_getFont(const pGEcontext gc, double fs, const char *family,
            const char *symbolfamily)
{
    PangoFontDescription *fontdesc;
    gint face = gc->fontface;
    double size = gc->cex * gc->ps * fs, ssize = PANGO_SCALE * size;
#ifdef Win32
    const char *times = "Times New Roman", *hv = "Arial";
#else
    const char *times = "times", *hv = "Helvetica";
#endif
    if (face < 1 || face > 5) face = 1;

    fontdesc = pango_font_description_new();
    if (face == 5) {
	pango_font_description_set_family(fontdesc, symbolfamily);
    } else {
	const char *fm = gc->fontfamily;
	if (!fm[0]) fm = family;
	if (streql(fm, "mono")) fm = "courier";
	else if (streql(fm, "serif")) fm = times;
	else if (streql(fm, "sans")) fm = hv;
	pango_font_description_set_family(fontdesc, fm);
	if (face == 2 || face == 4)
	    pango_font_description_set_weight(fontdesc, PANGO_WEIGHT_BOLD);
	if (face == 3 || face == 4)
	    pango_font_description_set_style(fontdesc, PANGO_STYLE_OBLIQUE);
    }
    /* seems a ssize < 1 gums up pango, PR#14369 */
    if (ssize < 1) ssize = 1.0;
    pango_font_description_set_size(fontdesc, (gint) ssize);

    return fontdesc;
}

static PangoLayout
*PG_layout(PangoFontDescription *desc, cairo_t *cc, const char *str)
{
    PangoLayout *layout;

    layout = pango_cairo_create_layout(cc);
    pango_layout_set_font_description(layout, desc);
    pango_layout_set_text(layout, str, -1);
    return layout;
}

static void
PG_text_extents(cairo_t *cc, PangoLayout *layout,
		gint *lbearing, gint *rbearing,
		gint *width, gint *ascent, gint *descent, int ink)
{
    PangoRectangle rect, lrect;

    // This could be pango_layout_get_line_readonly since 1.16
    // Something like #if PANGO_VERSION_CHECK(1,16,0)
    pango_layout_line_get_pixel_extents(pango_layout_get_line(layout, 0),
					&rect, &lrect);

    if (width) *width = lrect.width;
    if (ink) {
	if (ascent) *ascent = PANGO_ASCENT(rect);
	if (descent) *descent = PANGO_DESCENT(rect);
	if (lbearing) *lbearing = PANGO_LBEARING(rect);
	if (rbearing) *rbearing = PANGO_RBEARING(rect);
    } else {
	if (ascent) *ascent = PANGO_ASCENT(lrect);
	if (descent) *descent = PANGO_DESCENT(lrect);
	if (lbearing) *lbearing = PANGO_LBEARING(lrect);
	if (rbearing) *rbearing = PANGO_RBEARING(lrect);
    }
}

static void
PangoCairo_MetricInfo(int c, const pGEcontext gc,
		      double* ascent, double* descent,
		      double* width, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    char str[16];
    int Unicode = mbcslocale;
    PangoFontDescription *desc = 
	PG_getFont(gc, xd->fontscale, xd->basefontfamily, xd->symbolfamily);
    PangoLayout *layout;
    gint iascent, idescent, iwidth;

    if (c == 0) c = 77;
    if (c < 0) {c = -c; Unicode = 2;}

    if (Unicode) {
        const char *textstr; 
	Rf_ucstoutf8(str, (unsigned int) c);
        /* Unicode == 2 means we have a Unicode point */
        if (Unicode > 1 && gc->fontface == 5 && !xd->usePUA) {
            textstr = utf8Toutf8NoPUA(str);
            /* At most 3 bytes (plus null) in textstr */
            for (int i = 0; i < 4; i++) str[i] = textstr[i]; 
        } 
    } else {
	/* Here we assume that c < 256 */
	str[0] = (char) c; str[1] = (char) 0;
    }
    layout = PG_layout(desc, xd->cc, str);
    PG_text_extents(xd->cc, layout, NULL, NULL, &iwidth,
		    &iascent, &idescent, 1);
    g_object_unref(layout);
    pango_font_description_free(desc);
    *ascent = iascent;
    *descent = idescent;
    *width = iwidth;
#if 0
    printf("c = %d, '%s', face %d %f %f %f\n",
	   c, str, gc->fontface, *width, *ascent, *descent);
#endif
}


static double
PangoCairo_StrWidth(const char *str, const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    gint width;

    const char *textstr;
    if (!utf8Valid(str)) error("invalid string in PangoCairo_Text");
    if (gc->fontface == 5 && !xd->usePUA) {
        textstr = utf8Toutf8NoPUA(str);
    } else {
        textstr = str;
    }
    PangoFontDescription *desc = 
	PG_getFont(gc, xd->fontscale, xd->basefontfamily, xd->symbolfamily);
    PangoLayout *layout = PG_layout(desc, xd->cc, textstr);

    PG_text_extents(xd->cc, layout, NULL, NULL, &width, NULL, NULL, 0);
    g_object_unref(layout);
    pango_font_description_free(desc);
    return (double) width;
}

static void
PangoCairo_Text(double x, double y,
		const char *str, double rot, double hadj,
		const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    
    const char *textstr;
    if (!utf8Valid(str)) error("invalid string in PangoCairo_Text");
    if (gc->fontface == 5 && !xd->usePUA) {
        textstr = utf8Toutf8NoPUA(str);
    } else {
        textstr = str;
    }

    if (R_ALPHA(gc->col) > 0) {
	gint ascent, lbearing, width;
	PangoLayout *layout;
	PangoFontDescription *desc = 
	    PG_getFont(gc, xd->fontscale, xd->basefontfamily, xd->symbolfamily);
	cairo_save(xd->cc);

        bool grouping = cairoBegin(xd);

	layout = PG_layout(desc, xd->cc, textstr);
	PG_text_extents(xd->cc, layout, &lbearing, NULL, &width,
			&ascent, NULL, 0);
	cairo_move_to(xd->cc, x, y);
	if (rot != 0.0) cairo_rotate(xd->cc, -rot/180.*M_PI);
	/* pango has a coord system at top left */
	cairo_rel_move_to(xd->cc, -lbearing - width*hadj, -ascent);

        if (!xd->appending) {
            CairoColor(gc->col, xd);
            pango_cairo_show_layout(xd->cc, layout);
        } else {
            pango_cairo_layout_path(xd->cc, layout);
        }

        cairoEnd(grouping, xd);

	cairo_restore(xd->cc);
	g_object_unref(layout);
	pango_font_description_free(desc);
    }
}

#else
/* ------------- cairo-ft section --------------- */

/* This uses what cairo refers to as its 'toy' interface:
   http://cairographics.org/manual/cairo-text.html

   No diagnostics that glyphs are present, no kerning. 

   CAIRO_HAS_FT_FONT is defined (or not) in cairo-features.h included
   by cairo.h.  Windows builds of cairo often have it but the include
   paths in Windows R are not set up to include freetype2, needed by
   cairo-ft.h and ft2build.h.
*/

#if CAIRO_HAS_FT_FONT && !defined(_WIN32)
/* 
   The branch used on macOS and other-Unix alikes without pango but
   with freetype2/fontconfig (FT implies FC in Cairo).
*/

#include <cairo-ft.h>
#include <ft2build.h> // currently included by cairo-ft.h
#include FT_FREETYPE_H
#include <fontconfig/fontconfig.h>

SEXP in_CairoFT(void) 
{
//    return mkString("yes");

    FT_Library ft;
    FT_Error err = FT_Init_FreeType(&ft);
    if (err) return mkString("unknown");
    FT_Int major, minor, patch;
    FT_Library_Version(ft, &major, &minor, &patch);
    char buf[100];
    snprintf(buf, 100, "%d.%d.%d/%d.%d.%d",
	     major, minor, patch,
	     FC_MAJOR, FC_MINOR, FC_REVISION);
    return mkString(buf);
}

/* cairo font cache - to prevent unnecessary font lookups */
typedef struct Rc_font_cache_s {
    const char *family;
    int face;
    cairo_font_face_t *font;
    struct Rc_font_cache_s *next;
} Rc_font_cache_t;

static Rc_font_cache_t *cache, *cache_tail;

static cairo_font_face_t *Rc_findFont(const char *family, int face)
{
    Rc_font_cache_t *here = cache;
    while (here) {
	if (here->face == face && streql(here->family, family))
	    return here->font;
	here = here->next;
    }
    return NULL;
}

static void Rc_addFont(const char *family, int face, cairo_font_face_t* font)
{
    Rc_font_cache_t *fc = (Rc_font_cache_t*) malloc(sizeof(Rc_font_cache_t));
    if (!fc) return;
    fc->family = Rstrdup(family);
    fc->face = face;
    fc->font = font;
    fc->next = NULL;
    if (cache)
	cache_tail = cache_tail->next = fc;
    else
	cache = cache_tail = fc;
}

/* FC patterns to append to font family names */
static const char *face_styles[4] = {
    ":style=Regular",
    ":style=Bold",
    ":style=Italic",
    ":style=Bold Italic,BoldItalic"
};

static int fc_loaded;
static FT_Library ft_library;

/* use FC to find a font, load it in FT and return the Cairo FT font face */
static cairo_font_face_t *FC_getFont(const char *family, int style)
{
    FcFontSet *fs;
    FcPattern *pat, *match;
    FcResult  result;
    FcChar8   *file;
    char      fcname[250]; /* 200 for family + 50 for style */

    /* find candidate fonts via FontConfig */
    if (!fc_loaded) {
	if (!FcInit()) {
	    warning("unable to initialize FontConfig in cairo-ft font selection");
	    return NULL;
	}
	fc_loaded = 1;
    }
    style &= 3;
    strcpy(fcname, family);
    strcat(fcname, face_styles[style]);
    pat = FcNameParse((FcChar8 *)fcname);
    if (!pat) return NULL;
    FcConfigSubstitute (0, pat, FcMatchPattern);
    FcDefaultSubstitute (pat);
    fs = FcFontSetCreate ();
    match = FcFontMatch (0, pat, &result);
    FcPatternDestroy (pat);
    if (!match) {
	FcFontSetDestroy (fs);
	return NULL;
    }
    FcFontSetAdd (fs, match);

    /* then try to load the font into FT */
    if (fs) {
	for (int j = 0; j < fs->nfont; j++) {
	    /* find the font file + face index and use it with FreeType */
	    int index = 0;
	    if (FcPatternGetString (fs->fonts[j], FC_FILE, 0, &file)
		== FcResultMatch &&
		FcPatternGetInteger(fs->fonts[j], FC_INDEX, 0, &index)
		== FcResultMatch) {
		FT_Face face;
		if (!ft_library && FT_Init_FreeType(&ft_library)) {
		    FcFontSetDestroy (fs);
		    return NULL;
		}
		/* some FreeType versions have broken index support,
		   fall back to index 0 */
		if (!FT_New_Face(ft_library, 
				 (const char *) file, index, &face) ||
		    (index && !FT_New_Face(ft_library, 
					   (const char *) file, 0, &face))) {

#ifdef __APPLE__
		    /* FreeType is broken on macOS in that face index
		       is often wrong (unfortunately even for Helvetica!)
		       - we try to find the best match through enumeration.
		       And italic and bold are swapped */
		    if (style == 2) style = 1; else if (style == 1) style = 2;
		    if (face->num_faces > 1 && 
			(face->style_flags & 3) != style) {
			for (int i = 0; i < face->num_faces; i++) {
			    FT_Face alt_face;
			    if (!FT_New_Face(ft_library, 
					     (const char *) file, 
					     i, &alt_face)) {
				if ((alt_face->style_flags & 3) == style) {
				    FT_Done_Face(face);
				    face = alt_face;
				    break;
				} else FT_Done_Face(alt_face);
			    }
			}
		    }
#endif

		    return cairo_ft_font_face_create_for_ft_face(face, FT_LOAD_DEFAULT);
		}
	    }
	} // end of for loop
	FcFontSetDestroy (fs);
    } // if fs
    return NULL;
}

static void FT_getFont(pGEcontext gc, pDevDesc dd, double fs)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    int face = gc->fontface;
    double size = gc->cex * gc->ps *fs;
    cairo_font_face_t *cairo_face = NULL;
    const char *family;
#ifdef Win32
    char *times = "Times New Roman", *hv = "Arial";
#else
    char *times = "times", *hv = "Helvetica";
#endif

    if (face < 1 || face > 5) face = 1;
    family = gc->fontfamily;
    if (face == 5) {
	if (!*family) family = xd->symbolfamily;
    } else {
	if (!*family) family = xd->basefontfamily;
	if (streql(family, "sans")) family = hv;
	else if (streql(family, "serif")) family = times;
	else if (streql(family, "mono")) family = "Courier";
    }
    /* check the cache first */
    cairo_face = Rc_findFont(family, face);
    if (!cairo_face) {
	cairo_face = FC_getFont(family, face - 1);
	if (!cairo_face) return; /* No message? */
	Rc_addFont(family, face, cairo_face);
    }
    cairo_set_font_face (xd->cc, cairo_face);
    /* FIXME: this should really use cairo_set_font_matrix
       if pixels are non-square on a screen device. */
    cairo_set_font_size (xd->cc, size);
}

#else
// Branch without using FreeType/FontConfig, including on Windows

SEXP in_CairoFT(void) 
{
    return mkString("");
}

static void FT_getFont(pGEcontext gc, pDevDesc dd, double fs)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    int face = gc->fontface;
    double size = gc->cex * gc->ps *fs;
    char *family;
    int slant = CAIRO_FONT_SLANT_NORMAL, wt = CAIRO_FONT_WEIGHT_NORMAL;
#ifdef Win32
    char *times = "Times New Roman", *hv = "Arial";
#else
    char *times = "times", *hv = "Helvetica";
#endif

    if (face < 1 || face > 5) face = 1;
    if (face == 5) family = xd->symbolfamily;
    if (face == 2 || face == 4) wt = CAIRO_FONT_WEIGHT_BOLD;
    if (face == 3 || face == 4) slant = CAIRO_FONT_SLANT_ITALIC;
    if (face != 5) {
	/* This is a 'toy', remember?
	   The manual recommnends the CSS2 names "serif", "sans-serif",
	   "monospace" */
	char *fm = gc->fontfamily;
	if (!fm[0]) fm = xd->basefontfamily;
	if (streql(fm, "mono")) family = "courier";
	else if (streql(fm, "serif")) family = times;
	else if (streql(fm, "sans")) family = hv;
	else if (fm[0]) family = fm;
	// none of the above, so ultimate fallback.
	else family = hv;
    }

    cairo_select_font_face (xd->cc, family, slant, wt);
    /* FIXME: this should really use cairo_set_font_matrix
       if pixels are non-square on a screen device. */
    cairo_set_font_size (xd->cc, size);
}
#endif

static void Cairo_MetricInfo(int c, pGEcontext gc,
			     double* ascent, double* descent,
			     double* width, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    cairo_text_extents_t exts;
    char str[16];
    int Unicode = mbcslocale;

    if (c == 0) c = 77;
    if (c < 0) {c = -c; Unicode = 2;}

    if (Unicode) {
        const char *textstr;
	Rf_ucstoutf8(str, (unsigned int) c);
	if (Unicode > 1 && gc->fontface == 5 &&
	    dd->wantSymbolUTF8 == NA_LOGICAL &&
	    strcmp(xd->symbolfamily, "Symbol") != 0) {
            /* Single-byte Windows */
            textstr = utf8ToLatin1AdobeSymbol2utf8(str, xd->usePUA);
            /* At most 3 bytes (plus null) in textstr */
            for (int i = 0; i < 4; i++) str[i] = textstr[i]; 
	} else if (Unicode > 1 && gc->fontface == 5 && !xd->usePUA) {
            textstr = utf8Toutf8NoPUA(str);
            /* At most 3 bytes (plus null) in textstr */
            for (int i = 0; i < 4; i++) str[i] = textstr[i]; 
        } 
    } else {
	/* Here, we assume that c < 256 */
	str[0] = (char)c; str[1] = 0;
    }

    FT_getFont(gc, dd, xd->fontscale);
    cairo_text_extents(xd->cc, str, &exts);
    *ascent  = -exts.y_bearing;
    *descent = exts.height + exts.y_bearing;
    *width = exts.x_advance;
}

static double Cairo_StrWidth(const char *str, pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    cairo_text_extents_t exts;

    const char *textstr;
    if (!utf8Valid(str)) error("invalid string in Cairo_StrWidth");
    if (gc->fontface == 5 && dd->wantSymbolUTF8 == NA_LOGICAL &&
	strcmp(xd->symbolfamily, "Symbol") != 0) {
        /* Single-byte Windows */
        textstr = utf8ToLatin1AdobeSymbol2utf8(str, xd->usePUA);
    } else if (gc->fontface == 5 && !xd->usePUA) {
        textstr = utf8Toutf8NoPUA(str);
    } else {
        textstr = str;
    }
    FT_getFont(gc, dd, xd->fontscale);
    cairo_text_extents(xd->cc, textstr, &exts);
    return exts.x_advance;
}

static void Cairo_Text(double x, double y,
		       const char *str, double rot, double hadj,
		       pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    const char *textstr;

    if (!utf8Valid(str)) error("invalid string in Cairo_Text");

    if (gc->fontface == 5 && dd->wantSymbolUTF8 == NA_LOGICAL &&
	strcmp(xd->symbolfamily, "Symbol") != 0) {
        /* Single-byte Windows */
        textstr = utf8ToLatin1AdobeSymbol2utf8(str, xd->usePUA);
    } else if (gc->fontface == 5 && !xd->usePUA) {
        textstr = utf8Toutf8NoPUA(str);
    } else {
        textstr = str;
    }
    if (R_ALPHA(gc->col) > 0) {
	cairo_save(xd->cc);

        bool grouping = false;
        if (!xd->appending) {
            grouping = cairoBegin(xd);
        }

	FT_getFont(gc, dd, xd->fontscale);
	cairo_move_to(xd->cc, x, y);
	if (hadj != 0.0 || rot != 0.0) {
	    cairo_text_extents_t te;
	    cairo_text_extents(xd->cc, textstr, &te);
	    if (rot != 0.0) cairo_rotate(xd->cc, -rot/180.*M_PI);
	    if (hadj != 0.0)
		cairo_rel_move_to(xd->cc, -te.x_advance * hadj, 0);
	}
        if (!xd->appending) {
            CairoColor(gc->col, xd);
            cairo_show_text(xd->cc, textstr);
        } else {
            cairo_text_path(xd->cc, textstr);
        }

        if (!xd->appending) {
            cairoEnd(grouping, xd);
        }

	cairo_restore(xd->cc);
    }
}

#endif

static SEXP Cairo_SetPattern(SEXP pattern, pDevDesc dd) 
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    SEXP ref;
    PROTECT(ref = allocVector(INTSXP, 1));
    INTEGER(ref)[0] = CairoSetPattern(pattern, xd);
    UNPROTECT(1);
    return ref;
}

static void Cairo_ReleasePattern(SEXP ref, pDevDesc dd) 
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    /* NULL means release all patterns */
    if (ref == R_NilValue) {
        CairoCleanPatterns(xd);
    } else {
        CairoReleasePattern(INTEGER(ref)[0], xd);
    }
}

static SEXP Cairo_SetClipPath(SEXP path, SEXP ref, pDevDesc dd) 
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    return CairoSetClipPath(path, ref, xd);
}

static void Cairo_ReleaseClipPath(SEXP ref, pDevDesc dd) 
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    /* NULL means release all patterns */
    if (isNull(ref)) {
        CairoCleanClipPaths(xd);
    } else {
        int i;
        for (i = 0; i < LENGTH(ref); i++) {
            CairoReleaseClipPath(INTEGER(ref)[i], xd);
        }
    }
}

static SEXP Cairo_SetMask(SEXP mask, SEXP ref, pDevDesc dd) 
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    return CairoSetMask(mask, ref, xd);
}

static void Cairo_ReleaseMask(SEXP ref, pDevDesc dd) 
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    /* NULL means release all patterns */
    if (isNull(ref)) {
        CairoCleanMasks(xd);
    } else {
        int i;
        for (i = 0; i < LENGTH(ref); i++) {
            CairoReleaseMask(INTEGER(ref)[i], xd);
        }
    }
}

static SEXP Cairo_DefineGroup(SEXP source, int op, SEXP destination, 
                              pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    return CairoDefineGroup(source, op, destination, xd);
}

static void Cairo_UseGroup(SEXP ref, SEXP trans, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    CairoUseGroup(ref, trans, xd);
}

static void Cairo_ReleaseGroup(SEXP ref, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    /* NULL means release all groups */
    if (isNull(ref)) {
        CairoCleanGroups(xd);
    } else {
        int i;
        for (i = 0; i < LENGTH(ref); i++) {
            CairoReleaseGroup(INTEGER(ref)[i], xd);
        }
    }
}

/*
 ***************************
 * Stroking and filling paths
 ***************************
 */
static void CairoStrokePath(SEXP path,
                            const pGEcontext gc, 
                            pX11Desc xd)
{
    cairo_t *cc = xd->cc;
    SEXP R_fcall;
    bool grouping = false;

    if (!xd->appending) {
        grouping = cairoBegin(xd);
    }

    /* Increment the "appending" count */
    xd->appending++;
    /* Clear the current path */
    cairo_new_path(cc);
    /* Play the path function to build the path */
    R_fcall = PROTECT(lang1(path));
    eval(R_fcall, R_GlobalEnv);
    UNPROTECT(1);
    /* Decrement the "appending" count */
    xd->appending--;
    /* Stroke the path */

    if (!xd->appending) {
        if (R_ALPHA(gc->col) > 0 && gc->lty != -1) {
            cairoStroke(gc, xd);
        }
        cairoEnd(grouping, xd);
    }
}

static void Cairo_Stroke(SEXP path, const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    CairoStrokePath(path, gc, xd);    
}

static void CairoFillPath(SEXP path,
                          int rule,
                          const pGEcontext gc, 
                          pX11Desc xd)
{
    cairo_t *cc = xd->cc;
    SEXP R_fcall;
    bool grouping = false;

    if (!xd->appending) {
        grouping = cairoBegin(xd);
    }

    /* Increment the "appending" count */
    xd->appending++;
    /* Clear the current path */
    cairo_new_path(cc);
    /* Play the path function to build the path */
    R_fcall = PROTECT(lang1(path));
    eval(R_fcall, R_GlobalEnv);
    UNPROTECT(1);
    /* Decrement the "appending" count */
    xd->appending--;
    /* Stroke the path */

    if (!xd->appending) {
        /* patternFill overrides fill */
        if (gc->patternFill != R_NilValue || 
            R_ALPHA(gc->fill) > 0) { 
            switch (rule) {
            case R_GE_nonZeroWindingRule: 
                cairo_set_fill_rule(xd->cc, CAIRO_FILL_RULE_WINDING); break;
            case R_GE_evenOddRule:
                cairo_set_fill_rule(xd->cc, CAIRO_FILL_RULE_EVEN_ODD); break;
            }
            cairoFill(gc, xd);
        }
        cairoEnd(grouping, xd);
    }
}

static void Cairo_Fill(SEXP path, int rule, const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    CairoFillPath(path, rule, gc, xd);    
}

static void CairoFillStrokePath(SEXP path,
                                int rule,
                                const pGEcontext gc, 
                                pX11Desc xd)
{
    cairo_t *cc = xd->cc;
    SEXP R_fcall;
    /* Increment the "appending" count */
    xd->appending++;
    /* Clear the current path */
    cairo_new_path(cc);
    /* Play the path function to build the path */
    R_fcall = PROTECT(lang1(path));
    eval(R_fcall, R_GlobalEnv);
    UNPROTECT(1);
    /* Decrement the "appending" count */
    xd->appending--;
}

static void CairoFillStroke(SEXP path, int rule, 
                            const pGEcontext gc, pDevDesc dd, int op)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    bool grouping = cairoBegin(xd);
    CairoFillStrokePath(path, rule, gc, xd);
    if (op) { /* fill */
        cairoFill(gc, xd);
    } else {
        cairoStroke(gc, xd);
    }
    cairoEnd(grouping, xd);
}

static void Cairo_FillStroke(SEXP path, int rule, 
                             const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    if (xd->appending) {
        CairoFillStrokePath(path, rule, gc, xd);
    } else {
        bool fill = (gc->patternFill != R_NilValue) || 
            (R_ALPHA(gc->fill) > 0);
        bool stroke = (R_ALPHA(gc->col) > 0 && gc->lty != -1);
        if (fill) {
            switch (rule) {
            case R_GE_nonZeroWindingRule: 
                cairo_set_fill_rule(xd->cc, CAIRO_FILL_RULE_WINDING); break;
            case R_GE_evenOddRule:
                cairo_set_fill_rule(xd->cc, CAIRO_FILL_RULE_EVEN_ODD); break;
            }
        }
        if (fill && stroke) {
            CairoFillStroke(path, rule, gc, dd, 1);
            CairoFillStroke(path, rule, gc, dd, 0);
        } else if (fill) {
            CairoFillStroke(path, rule, gc, dd, 1);
        } else if (stroke) {
            CairoFillStroke(path, rule, gc, dd, 0);
        }
    }
}

/*
 ***************************
 * Device capabilities
 ***************************
 */

static SEXP Cairo_Capabilities(SEXP capabilities) {
    SEXP patterns, clippingPaths, masks, compositing, transforms, paths, glyphs;

    PROTECT(patterns = allocVector(INTSXP, 3));
    INTEGER(patterns)[0] = R_GE_linearGradientPattern;
    INTEGER(patterns)[1] = R_GE_radialGradientPattern;
    INTEGER(patterns)[2] = R_GE_tilingPattern;
    SET_VECTOR_ELT(capabilities, R_GE_capability_patterns, patterns);
    UNPROTECT(1);

    PROTECT(clippingPaths = allocVector(INTSXP, 1));
    INTEGER(clippingPaths)[0] = 1;
    SET_VECTOR_ELT(capabilities, R_GE_capability_clippingPaths, clippingPaths);
    UNPROTECT(1);

    PROTECT(masks = allocVector(INTSXP, 1));
    INTEGER(masks)[0] = R_GE_alphaMask;
    SET_VECTOR_ELT(capabilities, R_GE_capability_masks, masks);
    UNPROTECT(1);

    PROTECT(compositing = allocVector(INTSXP, 25));
    INTEGER(compositing)[0] = R_GE_compositeMultiply;
    INTEGER(compositing)[1] = R_GE_compositeScreen;
    INTEGER(compositing)[2] = R_GE_compositeOverlay;
    INTEGER(compositing)[3] = R_GE_compositeDarken;
    INTEGER(compositing)[4] = R_GE_compositeLighten;
    INTEGER(compositing)[5] = R_GE_compositeColorDodge;
    INTEGER(compositing)[6] = R_GE_compositeColorBurn;
    INTEGER(compositing)[7] = R_GE_compositeHardLight;
    INTEGER(compositing)[8] = R_GE_compositeSoftLight;
    INTEGER(compositing)[9] = R_GE_compositeDifference;
    INTEGER(compositing)[10] = R_GE_compositeExclusion;
    INTEGER(compositing)[11] = R_GE_compositeClear;
    INTEGER(compositing)[12] = R_GE_compositeSource;
    INTEGER(compositing)[13] = R_GE_compositeOver;
    INTEGER(compositing)[14] = R_GE_compositeIn;
    INTEGER(compositing)[15] = R_GE_compositeOut;
    INTEGER(compositing)[16] = R_GE_compositeAtop;
    INTEGER(compositing)[17] = R_GE_compositeDest;
    INTEGER(compositing)[18] = R_GE_compositeDestOver;
    INTEGER(compositing)[19] = R_GE_compositeDestIn;
    INTEGER(compositing)[20] = R_GE_compositeDestOut;
    INTEGER(compositing)[21] = R_GE_compositeDestAtop;
    INTEGER(compositing)[22] = R_GE_compositeXor;
    INTEGER(compositing)[23] = R_GE_compositeAdd;
    INTEGER(compositing)[24] = R_GE_compositeSaturate;
    SET_VECTOR_ELT(capabilities, R_GE_capability_compositing, compositing);
    UNPROTECT(1);

    PROTECT(transforms = allocVector(INTSXP, 1));
    INTEGER(transforms)[0] = 1;
    SET_VECTOR_ELT(capabilities, R_GE_capability_transformations, transforms);
    UNPROTECT(1);

    PROTECT(paths = allocVector(INTSXP, 1));
    INTEGER(paths)[0] = 1;
    SET_VECTOR_ELT(capabilities, R_GE_capability_paths, paths);
    UNPROTECT(1);

    PROTECT(glyphs = allocVector(INTSXP, 1));
    INTEGER(glyphs)[0] = 1;
    SET_VECTOR_ELT(capabilities, R_GE_capability_glyphs, glyphs);
    UNPROTECT(1);

    return capabilities;
}

/*
 ***************************
 * Typesetting
 ***************************
 */

#if CAIRO_HAS_FT_FONT
#include <cairo-ft.h>
#endif

static void Cairo_Glyph(int n, int *glyphs, double *x, double *y, 
                        SEXP font, double size, 
                        int colour, double rot, pDevDesc dd) 
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    int i;
    bool grouping = false;

    if (!xd->appending) {
        grouping = cairoBegin(xd);
    }

    double weight = R_GE_glyphFontWeight(font);
    int style = R_GE_glyphFontStyle(font);
    int wt, sl;
    if (weight > 400) {
        wt = CAIRO_FONT_WEIGHT_BOLD;
    } else {
        wt = CAIRO_FONT_WEIGHT_NORMAL;
    }
    if (style == R_GE_text_style_normal) {
        sl = CAIRO_FONT_SLANT_NORMAL;
    } else {
        sl = CAIRO_FONT_SLANT_ITALIC;
    }

    cairo_font_face_t *cairo_face = NULL;
#if CAIRO_HAS_FT_FONT
    FcPattern *pattern = FcPatternBuild(NULL,
                                        FC_FILE, FcTypeString, 
                                        R_GE_glyphFontFile(font),
                                        FC_INDEX, FcTypeInteger, 
                                        R_GE_glyphFontIndex(font),
                                        NULL);
    cairo_face = cairo_ft_font_face_create_for_pattern(pattern);
    FcPatternDestroy(pattern);
#endif
    if (cairo_face && 
        cairo_font_face_status(cairo_face) == CAIRO_STATUS_SUCCESS) {
        cairo_set_font_face(xd->cc, cairo_face);
    } else {
        warning(_("Font file not found; matching font family and face"));
        cairo_select_font_face(xd->cc, 
                               R_GE_glyphFontFamily(font), sl, wt);
    }
    /* Text size (in "points") MUST match the scale of the glyph 
     * location (in "bigpts").  The latter depends on device dpi.
     */
    cairo_set_font_size(xd->cc, size / (72*dd->ipr[0]));

    for (i=0; i<n; i++) {
        
        if (rot != 0.0) {
            cairo_save(xd->cc);
            cairo_translate(xd->cc, x[i], y[i]);
            cairo_rotate(xd->cc, -rot/180.*M_PI);
            cairo_translate(xd->cc, -x[i], -y[i]);
        }

        cairo_glyph_t cairoGlyph;
        cairoGlyph.index = glyphs[i];
        cairoGlyph.x = x[i];
        cairoGlyph.y = y[i];
        if (!xd->appending) {
            CairoColor(colour, xd);
            cairo_show_glyphs(xd->cc, &cairoGlyph, 1);
        } else {
            cairo_glyph_path(xd->cc, &cairoGlyph, 1);
        }

        if (!xd->appending) {
            cairoEnd(grouping, xd);
        }

        if (rot != 0.0) {
            cairo_restore(xd->cc);
        }
    }
    
}

