/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2012  The R Core Team
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

/* TODO
   - spreadsheet copy and paste?
 */

/* Use of strchr here is MBCS-safe */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "win-nls.h"

#include <wchar.h>
#include <rlocale.h>

#define R_USE_SIGNALS 1
#include "Defn.h"
#include <Internal.h>
#include "Print.h"
#include <Rinternals.h>
#include <R_ext/Parse.h>  /* parsing is used in handling escape codes */

#include "graphapp/ga.h"
#include "console.h"
#include "consolestructs.h"
#include "rui.h"

typedef enum {UNKNOWNN, NUMERIC, CHARACTER} CellType;

/* Used to check if eventloop needs to be run */
static Rboolean R_de_up;

#ifndef max
#define max(a, b) (((a)>(b))?(a):(b))
#endif
#ifndef min
#define min(a, b) (((a)<(b))?(a):(b))
#endif
#define BOXW(x) (min(((x<100 && DE->nboxchars == 0) ? DE->boxw[x] : DE->box_w), DE->p->w - DE->boxw[0] - 2*DE->bwidth - 2))

#define FIELDWIDTH 10
#define BUFSIZE 200

typedef struct {
    dataeditor de;
    ConsoleData p;
    int box_w;                       /* width of a box */
    int boxw[100];                   /* widths of cells */
    int box_h;                       /* height of a box */
    int windowWidth;                 /* current width of the window */
    int windowHeight;                /* current height of the window */
    int currentexp;                  /* boolean: whether an cell is active */
    int crow;                        /* current row */
    int ccol;                        /* current column */
    int nwide, nhigh;
    int colmax, colmin, rowmax, rowmin;
    int ndecimal;                    /* count decimal points */
    int ne;                          /* count exponents */
    int nneg;			     /* indicate whether its a negative */
    int clength;                     /* number of characters currently entered */
    int inSpecial;
    char buf[BUFSIZE];
    char *bufp;
    int bwidth;			/* width of the border */
    int hwidth;			/* width of header  */
    int text_xoffset, text_yoffset;
    field celledit;
    Rboolean CellModified, CellEditable;
    int xmaxused, ymaxused;
    int oldWIDTH, oldHEIGHT;
    int nboxchars;
    char labform[6];
    int xScrollbarScale, yScrollbarScale;
    SEXP work, names, lens;
    PROTECT_INDEX wpi, npi, lpi;
    menuitem de_mvw;
    SEXP ssNA_STRING;
    Rboolean isEditor;
} destruct, *DEstruct;

/* Local Function Definitions */

static void advancerect(DEstruct, int);
static void bell(void);
static void cleararea(DEstruct, int, int, int, int, rgb);
static void clearrect(DEstruct);
static void closerect(DEstruct);
static void clearwindow(DEstruct);
static void de_closewin(DEstruct);
static void copyarea(DEstruct, int, int, int, int);
static void copyH(DEstruct, int, int, int);
static void deredraw(DEstruct);
static void downlightrect(DEstruct);
static void drawwindow(DEstruct);
static void drawcol(DEstruct, int);
/* static void de_drawline(int, int, int, int);*/
static void de_drawtext(DEstruct, int, int, const char *);
static void drawrectangle(DEstruct, int, int, int, int, int, int);
static void drawrow(DEstruct, int);
static void find_coords(DEstruct, int, int, int*, int*);
static void handlechar(DEstruct, const char *);
static void highlightrect(DEstruct);
static Rboolean initwin(DEstruct, const char *);
static void jumppage(DEstruct, int);
static void jumpwin(DEstruct, int, int);
static void de_popupmenu(DEstruct, int, int, int);
static void printlabs(DEstruct);
static void printrect(DEstruct, int, int);
static void printstring(DEstruct, const char *, int, int, int, int);
static void printelt(DEstruct, SEXP, int, int, int);
static void setcellwidths(DEstruct);

static dataeditor newdataeditor(DEstruct, const char *);
static void de_copy(control c);
static void de_paste(control c);
static void de_delete(control c);


#define WIN32_LEAN_AND_MEAN 1
#include <windows.h> /* for Sleep */

int mb_char_len(const char *buf, int clength)
{
    int i, mb_len = 0;

    for(i = 0; i <= clength; i += mb_len)
	mb_len = mbrtowc(NULL, buf+i, MB_CUR_MAX, NULL);
    return mb_len;
}

static void moveback(DEstruct DE)
{
    int mb_len;

    if (DE->clength > 0) {
	mb_len = mb_char_len(DE->buf, DE->clength-1);
	DE->clength -= mb_len;
	DE->bufp -= mb_len;
	printstring(DE, DE->buf, DE->clength, DE->crow, DE->ccol, 1);
    } else bell();
}


 /*
  Underlying assumptions (for this version R >= 1.8.0)

  The data are stored in a list `work', with unused columns having
  NULL entries.  The names for the list are in `names', which should
  have a name for all displayable columns (up to xmaxused).
  The *used* lengths of the columns are in `lens': this needs only be
  set for non-NULL columns.

  If the list was originally length(0), that should work with
  0 pre-defined cols.  (It used to have 1 pre-defined numeric column.)

  All row and col numbers are 1-based.

  BDR May 2003
 */

/*
   ssNewVector is just an interface to allocVector but it lets us set
   the fields to NA. We need to have a special NA for strings so that
   we can differentiate between uninitialized elements in the vectors
   and user supplied NA's; hence ssNA_STRING
 */

static SEXP ssNewVector(DEstruct DE, SEXPTYPE type, int vlen)
{
    SEXP tvec;
    int j;

    tvec = allocVector(type, vlen);
    for (j = 0; j < vlen; j++)
	if (type == REALSXP)
	    REAL(tvec)[j] = NA_REAL;
	else if (type == STRSXP)
	    SET_STRING_ELT(tvec, j, DE->ssNA_STRING);
    return (tvec);
}

static void de_closewin_cend(void *DE)
{
    de_closewin((DEstruct) DE);
}

SEXP Win_dataentry(SEXP args)
{
    SEXP colmodes, tnames, tvec, tvec2, work2;
    SEXPTYPE type;
    int i, j, cnt, len, nprotect;
    RCNTXT cntxt;
    char clab[25];
    destruct DE1;
    DEstruct DE = &DE1;

    DE->isEditor = TRUE;
    nprotect = 0;/* count the PROTECT()s */
    PROTECT_WITH_INDEX(DE->work = duplicate(CAR(args)), &DE->wpi); nprotect++;
    colmodes = CADR(args);
    tnames = getAttrib(DE->work, R_NamesSymbol);

    if (TYPEOF(DE->work) != VECSXP || TYPEOF(colmodes) != VECSXP)
	error(G_("invalid argument"));

    /* initialize the constants */

    DE->bufp = DE->buf;
    DE->ne = 0;
    DE->currentexp = 0;
    DE->nneg = 0;
    DE->ndecimal = 0;
    DE->clength = 0;
    DE->inSpecial = 0;
    DE->ccol = 1;
    DE->crow = 1;
    DE->colmin = 1;
    DE->rowmin = 1;
    PROTECT(DE->ssNA_STRING = duplicate(NA_STRING));
    nprotect++;
    DE->bwidth = 0;
    DE->hwidth = 5;

    /* setup work, names, lens  */
    DE->xmaxused = length(DE->work); DE->ymaxused = 0;
    PROTECT_WITH_INDEX(DE->lens = allocVector(INTSXP, DE->xmaxused), &DE->lpi);
    nprotect++;

    if (isNull(tnames)) {
	PROTECT_WITH_INDEX(DE->names = allocVector(STRSXP, DE->xmaxused),
			   &DE->npi);
	for(i = 0; i < DE->xmaxused; i++) {
	    snprintf(clab, 25, "var%d", i);
	    SET_STRING_ELT(DE->names, i, mkChar(clab));
	}
    } else
	PROTECT_WITH_INDEX(DE->names = duplicate(tnames), &DE->npi);
    nprotect++;
    for (i = 0; i < DE->xmaxused; i++) {
	int len = LENGTH(VECTOR_ELT(DE->work, i));
	INTEGER(DE->lens)[i] = len;
	DE->ymaxused = max(len, DE->ymaxused);
	type = TYPEOF(VECTOR_ELT(DE->work, i));
	if (LENGTH(colmodes) > 0 && !isNull(VECTOR_ELT(colmodes, i)))
	    type = str2type(CHAR(STRING_ELT(VECTOR_ELT(colmodes, i), 0)));
	if (type != STRSXP) type = REALSXP;
	if (isNull(VECTOR_ELT(DE->work, i))) {
	    if (type == NILSXP) type = REALSXP;
	    SET_VECTOR_ELT(DE->work, i, ssNewVector(DE, type, 100));
	} else if (!isVector(VECTOR_ELT(DE->work, i)))
	    error(G_("invalid type for value"));
	else {
	    if (TYPEOF(VECTOR_ELT(DE->work, i)) != type)
		SET_VECTOR_ELT(DE->work, i,
			       coerceVector(VECTOR_ELT(DE->work, i), type));
	}
    }

    DE->xScrollbarScale = DE->yScrollbarScale = 1;

    /* start up the window, more initializing in here */
    if (initwin(DE, G_("Data Editor")))
	error(G_("invalid device"));
    R_de_up = TRUE;

    /* set up a context which will close the window if there is an error */
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &de_closewin_cend;
    cntxt.cenddata = (void *)DE;

    highlightrect(DE);

    while (R_de_up) {
	/* avoid consuming 100% CPU time here */
	Sleep(10);
	R_ProcessEvents();
    }

    endcontext(&cntxt);

    /* drop out unused columns */
    for(i = 0, cnt = 0; i < DE->xmaxused; i++)
	if(!isNull(VECTOR_ELT(DE->work, i))) cnt++;
    if (cnt < DE->xmaxused) {
	PROTECT(work2 = allocVector(VECSXP, cnt)); nprotect++;
	for(i = 0, j = 0; i < DE->xmaxused; i++) {
	    if(!isNull(VECTOR_ELT(DE->work, i))) {
		SET_VECTOR_ELT(work2, j, VECTOR_ELT(DE->work, i));
		INTEGER(DE->lens)[j] = INTEGER(DE->lens)[i];
		SET_STRING_ELT(DE->names, j, STRING_ELT(DE->names, i));
		j++;
	    }
	}
	REPROTECT(DE->names = lengthgets(DE->names, cnt), DE->npi);
    } else work2 = DE->work;

    for (i = 0; i < LENGTH(work2); i++) {
	len = INTEGER(DE->lens)[i];
	tvec = VECTOR_ELT(work2, i);
	if (LENGTH(tvec) != len) {
	    tvec2 = ssNewVector(DE, TYPEOF(tvec), len);
	    for (j = 0; j < len; j++) {
		if (TYPEOF(tvec) == REALSXP) {
			REAL(tvec2)[j] = REAL(tvec)[j];
		} else if (TYPEOF(tvec) == STRSXP) {
		    if (STRING_ELT(tvec, j) != DE->ssNA_STRING)
			SET_STRING_ELT(tvec2, j, STRING_ELT(tvec, j));
		    else
			SET_STRING_ELT(tvec2, j, NA_STRING);
		} else
		    error(G_("dataentry: internal memory problem"));
	    }
	    SET_VECTOR_ELT(work2, i, tvec2);
	}
    }

    setAttrib(work2, R_NamesSymbol, DE->names);
    UNPROTECT(nprotect);
    return work2;
}

/* Window Drawing Routines */

static rgb bbg;

static void setcellwidths(DEstruct DE)
{
    int i, w, dw;

    DE->windowWidth = w = 2*DE->bwidth + DE->boxw[0] + BOXW(DE->colmin);
    DE->nwide = 2;
    for (i = 2; i < 100; i++) { /* 100 on-screen columns cannot occur */
	dw = BOXW(i + DE->colmin - 1);
	if((w += dw) > DE->p->w ||
	   (!DE->isEditor && i > DE->xmaxused - DE->colmin + 1)
	    ) {
	    DE->nwide = i;
	    DE->windowWidth = w - dw;
	    break;
	}
    }
}


static void drawwindow(DEstruct DE)
{
    /* might have resized */
    setcellwidths(DE);
    DE->nhigh = (DE->p->h - 2 * DE->bwidth - DE->hwidth - 3) / DE->box_h;
    if(!DE->isEditor && DE->nhigh > DE->ymaxused+1) DE->nhigh = DE->ymaxused+1;
    DE->windowHeight = DE->nhigh * DE->box_h + 2 * DE->bwidth + DE->hwidth;
    DE->oldWIDTH = DE->p->w;
    DE->oldHEIGHT = DE->p->h;

    clearwindow(DE);
    deredraw(DE);
    /* row/col 1 = pos 0 */
    gchangescrollbar(DE->de, VWINSB, (DE->rowmin - 1)/DE->yScrollbarScale,
		     DE->ymaxused/DE->yScrollbarScale,
		     max(DE->nhigh/DE->yScrollbarScale, 1), 0);
    gchangescrollbar(DE->de, HWINSB, (DE->colmin - 1)/DE->xScrollbarScale,
		     DE->xmaxused/DE->xScrollbarScale,
		     max(DE->nwide/DE->xScrollbarScale, 1), 0);
}

static void doHscroll(DEstruct DE, int oldcol)
{
    int i, dw;
    int oldnwide = DE->nwide, oldwindowWidth = DE->windowWidth;

    /* horizontal re-position */
    setcellwidths(DE);
    DE->colmax = DE->colmin + (DE->nwide - 2);
    if (oldcol < DE->colmin) { /* drop oldcol...colmin - 1 */
	dw = DE->boxw[0];
	for (i = oldcol; i < DE->colmin; i++) dw += BOXW(i);
	copyH(DE, dw, DE->boxw[0], oldwindowWidth - dw + 1);
	dw = oldwindowWidth - BOXW(oldcol) + 1;
	cleararea(DE, dw, DE->hwidth, DE->p->w-dw, DE->p->h, DE->p->guiColors[dataeditbg]);
	/* oldnwide includes the row labels */
	for (i = oldcol+oldnwide-1; i <= DE->colmax; i++) drawcol(DE, i);
    } else {
	/* move one or more cols left */
	dw = BOXW(DE->colmin);
	copyH(DE, DE->boxw[0], DE->boxw[0] + dw, DE->windowWidth - dw + 1);
	dw = DE->windowWidth + 1;
	cleararea(DE, dw, DE->hwidth, DE->p->w-dw, DE->p->h, DE->p->guiColors[dataeditbg]);
	drawcol(DE, DE->colmin);
    }
    gchangescrollbar(DE->de, HWINSB, (DE->colmin - 1)/DE->xScrollbarScale,
		     DE->xmaxused/DE->xScrollbarScale,
		     max(DE->nwide/DE->xScrollbarScale, 1), 0);
    highlightrect(DE);
}

/* find_coords finds the coordinates of the upper left corner of the
   given cell on the screen: row and col are on-screen coords */

static void find_coords(DEstruct DE,
			int row, int col, int *xcoord, int *ycoord)
{
    int i, w;
    w = DE->bwidth;
    if (col > 0) w += DE->boxw[0];
    for(i = 1; i < col; i ++) w += BOXW(i + DE->colmin - 1);
    *xcoord = w;
    *ycoord = DE->bwidth + DE->hwidth + DE->box_h * row;
}

/* draw the window with the top left box at column wcol and row wrow */

static void jumpwin(DEstruct DE, int wcol, int wrow)
{
    if (wcol < 0 || wrow < 0) {
	bell();
	return;
    }
    closerect(DE);
    if (DE->colmin != wcol || DE->rowmin != wrow) {
	DE->colmin = wcol;
	DE->rowmin = wrow;
	deredraw(DE);
	gchangescrollbar(DE->de, VWINSB, (DE->rowmin - 1)/DE->yScrollbarScale,
			 DE->ymaxused/DE->yScrollbarScale,
			 max(DE->nhigh/DE->yScrollbarScale, 1), 0);
	gchangescrollbar(DE->de, HWINSB, (DE->colmin - 1)/DE->xScrollbarScale,
			 DE->xmaxused/DE->xScrollbarScale,
			 max(DE->nwide/DE->xScrollbarScale, 1), 0);
    } else highlightrect(DE);
}

static void advancerect(DEstruct DE, int which)
{

    /* if we are in the header, changing a name then only down is
       allowed */
    if (DE->crow < 1 && which != DOWN) {
	bell();
	return;
    }

    closerect(DE);

    switch (which) {
    case UP:
	if (DE->crow == 1) {
	    if (DE->rowmin == 1)
		bell();
	    else
		jumppage(DE, UP);
	} else
	    DE->crow--;
	break;
    case DOWN:
	if (DE->crow == (DE->nhigh - 1))
	    jumppage(DE, DOWN);
	else
	    DE->crow++;
	break;
    case RIGHT:
	if (DE->ccol == (DE->nwide - 1))
	    jumppage(DE, RIGHT);
	else
	    DE->ccol++;
	break;
    case LEFT:
	if (DE->ccol == 1) {
	    if (DE->colmin == 1)
		bell();
	    else
		jumppage(DE, LEFT);
	} else
	    DE->ccol--;
	break;
    default:
	UNIMPLEMENTED("advancerect");
    }

    highlightrect(DE);
}

static const char *get_col_name(DEstruct DE, int col)
{
    static char clab[25];
    if (col <= DE->xmaxused) {
	/* don't use NA labels */
	SEXP tmp = STRING_ELT(DE->names, col - 1);
	if(tmp != NA_STRING) return(CHAR(tmp));
    }
    snprintf(clab, 25, "var%d", col);
    return clab;
}

static int get_col_width(DEstruct DE, int col)
{
    int i, w = 0, w1, fw = FIELDWIDTH;
    const char *strp;
    SEXP tmp, lab;

    if (DE->nboxchars > 0) return DE->nboxchars;
    if (col <= DE->xmaxused) {
	tmp = VECTOR_ELT(DE->work, col - 1);
	if (isNull(tmp)) return fw;
	/* don't use NA labels */
	lab = STRING_ELT(DE->names, col - 1);
	if(lab != NA_STRING) w = strlen(CHAR(lab)); else w = fw;
	PrintDefaults();
	for (i = 0; i < INTEGER(DE->lens)[col - 1]; i++) {
	    strp = EncodeElement(tmp, i, 0, '.');
	    w1 = strlen(strp);
	    if (w1 > w) w = w1;
	}
	if(w < 5) w = 5;
	if(w < 8) w++;
	if(w > 50) w = 50;
	return w;
    }
    return fw;
}

static CellType get_col_type(DEstruct DE, int col)
{
    SEXP tmp;
    CellType res = UNKNOWNN;

    if (col <= DE->xmaxused) {
	tmp = VECTOR_ELT(DE->work, col - 1);
	if(TYPEOF(tmp) == REALSXP) res = NUMERIC;
	if(TYPEOF(tmp) == STRSXP) res = CHARACTER;
    }
    return res;
}


/* whichcol is absolute col no, col is position on screen */
static void drawcol(DEstruct DE, int whichcol)
{
    int i, src_x, src_y, len, col = whichcol - DE->colmin + 1,
	bw = BOXW(whichcol);
    const char *clab;
    SEXP tmp;

    find_coords(DE, 0, col, &src_x, &src_y);
    cleararea(DE, src_x, src_y, bw, DE->windowHeight, DE->p->guiColors[dataeditbg]);
    cleararea(DE, src_x, src_y, bw, DE->box_h, bbg);
    for (i = 0; i < DE->nhigh; i++)
	drawrectangle(DE, src_x, DE->hwidth + i * DE->box_h, bw, DE->box_h, 1, 1);

    /* now fill it in if it is active */
    clab = get_col_name(DE, whichcol);
    printstring(DE, clab, strlen(clab), 0, col, 0);

    if (DE->xmaxused >= whichcol) {
	tmp = VECTOR_ELT(DE->work, whichcol - 1);
	if (!isNull(tmp)) {
	    len = min(DE->rowmax, INTEGER(DE->lens)[whichcol - 1]);
	    for (i = (DE->rowmin - 1); i < len; i++)
		printelt(DE, tmp, i, i - DE->rowmin + 2, col);
	}
    }
}


/* whichrow is absolute row no */
static void drawrow(DEstruct DE, int whichrow)
{
    int i, src_x, src_y, row = whichrow - DE->rowmin + 1, w;
    char rlab[15];
    SEXP tvec;

    find_coords(DE, row, 0, &src_x, &src_y);
    cleararea(DE, src_x, src_y, DE->windowWidth, DE->box_h,
	      (whichrow > 0) ? DE->p->guiColors[dataeditbg] : bbg);
    drawrectangle(DE, src_x, src_y, DE->boxw[0], DE->box_h, 1, 1);

    snprintf(rlab, 15, DE->labform, whichrow);
    printstring(DE, rlab, strlen(rlab), row, 0, 0);

    w = DE->bwidth + DE->boxw[0];
    for (i = DE->colmin; i <= DE->colmax; i++) {
	drawrectangle(DE, w, src_y, BOXW(i), DE->box_h, 1, 1);
	w += BOXW(i);
    }

    for (i = DE->colmin; i <= DE->colmax; i++) {
	if (i > DE->xmaxused) break;
	if (!isNull(tvec = VECTOR_ELT(DE->work, i - 1)))
	    if (whichrow <= INTEGER(DE->lens)[i - 1])
		printelt(DE, tvec, whichrow - 1, row, i - DE->colmin + 1);
    }
}

/* printelt: print the correct value from vector[vrow] into the
   spreadsheet in row ssrow and col sscol */

/* WARNING: This has no check that you're not beyond the end of the
   vector. Caller must check. */

static void printelt(DEstruct DE, SEXP invec, int vrow, int ssrow, int sscol)
{
    const char *strp;
    PrintDefaults();
    if (TYPEOF(invec) == REALSXP) {
	strp = EncodeElement(invec, vrow, 0, '.');
	printstring(DE, strp, strlen(strp), ssrow, sscol, 0);
    }
    else if (TYPEOF(invec) == STRSXP) {
	if (STRING_ELT(invec, vrow) != DE->ssNA_STRING) {
	    strp = EncodeElement(invec, vrow, 0, '.');
	    printstring(DE, strp, strlen(strp), ssrow, sscol, 0);
	}
    }
    else
	error(G_("dataentry: internal memory error"));
}


static void drawelt(DEstruct DE, int whichrow, int whichcol)
{
    int i;
    const char *clab;
    SEXP tmp;

    if (whichrow == 0) {
	clab = get_col_name(DE, whichcol + DE->colmin - 1);
	printstring(DE, clab, strlen(clab), 0, whichcol, 0);
    } else {
	if (DE->xmaxused >= whichcol + DE->colmin - 1) {
	    tmp = VECTOR_ELT(DE->work, whichcol + DE->colmin - 2);
	    if (!isNull(tmp) && (i = DE->rowmin + whichrow - 2) <
		INTEGER(DE->lens)[whichcol + DE->colmin - 2] )
		printelt(DE, tmp, i, whichrow, whichcol);
	} else
	    printstring(DE, "", 0, whichrow,  whichcol, 0);
    }
}

static void jumppage(DEstruct DE, int dir)
{
    int i, w, oldcol, wcol;

    switch (dir) {
    case UP:
	DE->rowmin--;
	DE->rowmax--;
	copyarea(DE, 0, DE->hwidth + DE->box_h, 0, DE->hwidth + 2 * DE->box_h);
	drawrow(DE, DE->rowmin);
	gchangescrollbar(DE->de, VWINSB, (DE->rowmin - 1)/DE->yScrollbarScale,
			 DE->ymaxused/DE->yScrollbarScale,
			 max(DE->nhigh/DE->yScrollbarScale, 1), 0);
	break;
    case DOWN:
	// if (DE->rowmax >= 65535) return;
	DE->rowmin++;
	DE->rowmax++;
	copyarea(DE, 0, DE->hwidth + 2 * DE->box_h, 0, DE->hwidth + DE->box_h);
	drawrow(DE, DE->rowmax);
	gchangescrollbar(DE->de, VWINSB, (DE->rowmin - 1)/DE->yScrollbarScale,
			 DE->ymaxused/DE->yScrollbarScale,
			 max(DE->nhigh/DE->yScrollbarScale, 1), 0);
	break;
    case LEFT:
	DE->colmin--;
	doHscroll(DE, DE->colmin + 1);
	break;
    case RIGHT:
	oldcol = DE->colmin;
	wcol = DE->colmin + DE->ccol + 1; /* column to be selected */
	/* There may not be room to fit the next column in */
	w = DE->p->w - DE->boxw[0] - BOXW(DE->colmax + 1);
	for (i = DE->colmax; i >= oldcol; i--) {
	    w -= BOXW(i);
	    if(w < 0) {
		DE->colmin = i + 1;
		break;
	    }
	}
	DE->ccol = wcol - DE->colmin;
	doHscroll(DE, oldcol);
	break;
    }
}
/* draw a rectangle, used to highlight/downlight the current box */

static void printrect(DEstruct DE, int lwd, int fore)
{
    int x, y;
    find_coords(DE, DE->crow, DE->ccol, &x, &y);
    drawrectangle(DE, x + lwd - 1, y + lwd - 1,
		  BOXW(DE->ccol+DE->colmin-1) - lwd + 1,
		  DE->box_h - lwd + 1, lwd, fore);
}

static void downlightrect(DEstruct DE)
{
    printrect(DE, 2, 0);
    printrect(DE, 1, 1);
}

static void highlightrect(DEstruct DE)
{
    if(DE->isEditor)
	printrect(DE, 2, 1);
    else
	printrect(DE, 1, 1);
}


static Rboolean getccol(DEstruct DE)
{
    SEXP tmp, tmp2;
    int i, len, newlen, wcol, wrow;
    SEXPTYPE type;
    char clab[25];
    Rboolean newcol = FALSE;

    wcol = DE->ccol + DE->colmin - 1;
    wrow = DE->crow + DE->rowmin - 1;
    if (wcol > DE->xmaxused) {
	/* extend work, names and lens */
	REPROTECT(DE->work = lengthgets(DE->work, wcol), DE->wpi);
	REPROTECT(DE->names = lengthgets(DE->names, wcol), DE->npi);
	for (i = DE->xmaxused; i < wcol; i++) {
	    snprintf(clab, 25, "var%d", i + 1);
	    SET_STRING_ELT(DE->names, i, mkChar(clab));
	}
	REPROTECT(DE->lens = lengthgets(DE->lens, wcol), DE->lpi);
	DE->xmaxused = wcol;
    }
    if (isNull(VECTOR_ELT(DE->work, wcol - 1))) {
	newcol = TRUE;
	SET_VECTOR_ELT(DE->work, wcol - 1,
		       ssNewVector(DE, REALSXP, max(100, wrow)));
	INTEGER(DE->lens)[wcol - 1] = 0;
    }
    if (!isVector(tmp = VECTOR_ELT(DE->work, wcol - 1)))
	error(G_("internal type error in dataentry"));
    len = INTEGER(DE->lens)[wcol - 1];
    type = TYPEOF(tmp);
    if (len < wrow) {
	for (newlen = max(len * 2, 10) ; newlen < wrow ; newlen *= 2)
	    ;
	tmp2 = ssNewVector(DE, type, newlen);
	for (i = 0; i < len; i++)
	    if (type == REALSXP)
		REAL(tmp2)[i] = REAL(tmp)[i];
	    else if (type == STRSXP)
		SET_STRING_ELT(tmp2, i, STRING_ELT(tmp, i));
	    else
		error(G_("internal type error in dataentry"));
	SET_VECTOR_ELT(DE->work, wcol - 1, tmp2);
    }
    return newcol;
}

static SEXP processEscapes(SEXP x)
{
    SEXP newval, pattern, replacement, expr;
    ParseStatus status;
    
    /* We process escape sequences in a scalar string by escaping
       unescaped quotes, then quoting the whole thing and parsing it.  This
       is supposed to be equivalent to the R code

       newval <- gsub(perl=TRUE, "(?<!\\\\)((\\\\\\\\)*)\"", "\\1\\\\\"", x)
       newval <- sub('(^.*$)', '"\1"', newval)
       newval <- eval(parse(text=newval))

       We do it this way to avoid extracting the escape handling
       code from the parser.  We need it in C code because this may be executed
       numerous times from C in dataentry.c */
    	
    PROTECT( pattern = mkString("(?<!\\\\)((\\\\\\\\)*)\"") );
    PROTECT( replacement = mkString("\\1\\\\\"") );
    PROTECT( expr = lang5(install("gsub"), ScalarLogical(1), pattern, replacement, x) );
    SET_TAG( CDR(expr), install("perl") );

    PROTECT( newval = eval(expr, R_BaseEnv) );
    PROTECT( pattern = mkString("(^.*$)") );
    PROTECT( replacement = mkString("\"\\1\"") );
    PROTECT( expr = lang4(install("sub"), pattern, replacement, newval) );
    PROTECT( newval = eval(expr, R_BaseEnv) );
    PROTECT( expr = R_ParseVector( newval, 1, &status, R_NilValue) );
    
    /* We only handle the first entry. If this were available more generally,
       we'd probably want to loop over all of expr */
       
    if (status == PARSE_OK && length(expr))
	PROTECT( newval = eval(VECTOR_ELT(expr, 0), R_BaseEnv) );
    else
	PROTECT( newval = R_NilValue );  /* protect just so the count doesn't change */
    UNPROTECT(10);
    return newval;
}

/* close up the entry to a cell, put the value that has been entered
   into the correct place and as the correct type */

static void closerect(DEstruct DE)
{
    SEXP cvec;
    int wcol = DE->ccol + DE->colmin - 1, wrow = DE->rowmin + DE->crow - 1,
	wrow0;
    Rboolean newcol;

    *(DE->bufp) = '\0';

    if (DE->CellModified || DE->CellEditable) {
	if (DE->CellEditable) {
	    strncpy(DE->buf, GA_gettext(DE->celledit), BUFSIZE-1);
	    DE->clength = strlen(DE->buf);
	    hide(DE->celledit);
	    del(DE->celledit);
	}
	newcol = getccol(DE);
	cvec = VECTOR_ELT(DE->work, wcol - 1);
	wrow0 = INTEGER(DE->lens)[wcol - 1];
	if (wrow > wrow0) INTEGER(DE->lens)[wcol - 1] = wrow;
	DE->ymaxused = max(DE->ymaxused, wrow);
	if (DE->clength != 0) {
	    /* do it this way to ensure NA, Inf, ...  can get set */
	    char *endp;
	    double new = R_strtod(DE->buf, &endp);
	    int warn = !isBlankString(endp);
	    if (TYPEOF(cvec) == STRSXP) {
	    	SEXP newval;
	    	PROTECT( newval = mkString(DE->buf) );
	    	PROTECT( newval = processEscapes(newval) );
	    	if (TYPEOF(newval) == STRSXP && length(newval) == 1)
		    SET_STRING_ELT(cvec, wrow - 1, STRING_ELT(newval, 0));
		else
		    warning(G_("dataentry: parse error on string"));
		UNPROTECT(2);
	    } else
		REAL(cvec)[wrow - 1] = new;
	    if (newcol && warn) {
		/* change mode to character */
		SEXP tmp = coerceVector(cvec, STRSXP);
		SET_STRING_ELT(tmp, wrow - 1, mkChar(DE->buf));
		SET_VECTOR_ELT(DE->work, wcol - 1, tmp);
	    }
	} else {
	    if (TYPEOF(cvec) == STRSXP)
		SET_STRING_ELT(cvec, wrow - 1, NA_STRING);
	    else
		REAL(cvec)[wrow - 1] = NA_REAL;
	}
	drawelt(DE, DE->crow, DE->ccol);  /* to get the cell scrolling right */
	if(wrow > wrow0) drawcol(DE, wcol); /* to fill in NAs */
    }
    DE->CellEditable = DE->CellModified = FALSE;

    downlightrect(DE);
    gsetcursor(DE->de, ArrowCursor);

    DE->ndecimal = 0;
    DE->nneg = 0;
    DE->ne = 0;
    DE->currentexp = 0;
    DE->clength = 0;
    DE->inSpecial = 0;
    DE->bufp = DE->buf;
}

/* print a null terminated string, check to see if it is longer than
   the print area and print it, left adjusted if necessary; clear the
   area of previous text; */

/* This version will only display BUFSIZE chars, but the maximum col width
   will not allow that many */
static void printstring(DEstruct DE, const char *ibuf, int buflen,
			int row, int col, int left)
{
    int x_pos, y_pos, bw, fw, bufw;
    char buf[BUFSIZE+1];

    find_coords(DE, row, col, &x_pos, &y_pos);
    if (col == 0) bw = DE->boxw[0]; else bw = BOXW(col+DE->colmin-1);
    cleararea(DE, x_pos + 1, y_pos + 1, bw - 1, DE->box_h - 1,
	      (row==0 || col==0) ? bbg:DE->p->guiColors[dataeditbg]);
    fw = min(BUFSIZE, (bw - 8)/(DE->p->fw));
    bufw = min(fw, buflen);
    strncpy(buf, ibuf, bufw);
    buf[bufw] = '\0';
    if (buflen > fw) {
	if(left) {
	    strncpy(buf, ibuf + buflen - fw, fw);
	    buf[fw] = '\0';
	    *buf = '<';
	} else {
	    *(buf + fw - 1) = '>';
	    *(buf + fw) = '\0';
	}
    }
    de_drawtext(DE, x_pos + DE->text_xoffset, y_pos - DE->text_yoffset, buf);
}

static void clearrect(DEstruct DE)
{
    int x_pos, y_pos;

    find_coords(DE, DE->crow, DE->ccol, &x_pos, &y_pos);
    cleararea(DE, x_pos, y_pos, BOXW(DE->ccol+DE->colmin-1),
	      DE->box_h, DE->p->guiColors[dataeditbg]);
}

/* handlechar has to be able to parse decimal numbers and strings,
   depending on the current column type, only printing characters
   should get this far */

/* --- Not true! E.g. ESC ends up in here... */

static void handlechar(DEstruct DE, const char *text)
{
    int c = text[0];

    if ( c == '\033' ) {
	DE->CellModified = FALSE;
	DE->clength = 0;
	drawelt(DE, DE->crow, DE->ccol);
	gsetcursor(DE->de, ArrowCursor);
	return;
    } else {
	DE->CellModified = TRUE;
	gsetcursor(DE->de, TextCursor);
    }

    if (DE->clength == 0) {
	switch(get_col_type(DE, DE->ccol + DE->colmin - 1)) {
	case NUMERIC:
	    DE->currentexp = 1;
	    break;
	default:
	    DE->currentexp = 2;
	}
	clearrect(DE);
	highlightrect(DE);
    }

    if (DE->currentexp == 1)	/* we are parsing a number */
	switch (c) {
	case '-':
	    if (DE->nneg == 0)
		DE->nneg++;
	    else
		goto donehc;
	    break;
	case '.':
	    if (DE->ndecimal == 0)
		DE->ndecimal++;
	    else
		goto donehc;
	    break;
	case 'e':
	case 'E':
	    if (DE->ne == 0) {
		DE->nneg = DE->ndecimal = 0;	/* might have decimal in exponent */
		DE->ne++;
	    }
	    else
		goto donehc;
	    break;
	case 'N':
	    if(DE->nneg) goto donehc;
	case 'I':
	    DE->inSpecial++;
	    break;
	default:
	    if (!DE->inSpecial && !isdigit((int)text[0]))
		goto donehc;
	    break;
	}

    if (DE->clength++ > 199) {
	warning(G_("dataentry: expression too long"));
	DE->clength--;
	goto donehc;
    }

    *(DE->bufp)++ = text[0];
    printstring(DE, DE->buf, DE->clength, DE->crow, DE->ccol, 1);
    return;

donehc:
    bell();
}

static void printlabs(DEstruct DE)
{
    char clab[15];
    const char *p;
    int i;

    for (i = DE->colmin; i <= DE->colmax; i++) {
	p = get_col_name(DE, i);
	printstring(DE, p, strlen(p), 0, i - DE->colmin + 1, 0);
    }
    for (i = DE->rowmin; i <= DE->rowmax; i++) {
	snprintf(clab, 15, DE->labform, i);
	printstring(DE, clab, strlen(clab), i - DE->rowmin + 1, 0, 0);
    }
}

	      /* ================ GraphApp-specific ================ */

static void bell(void)
{
    gabeep();
}

static void cleararea(DEstruct DE,
		      int xpos, int ypos, int width, int height, rgb col)
{
    gfillrect(DE->de, col, rect(xpos, ypos, width, height));
}

static void clearwindow(DEstruct DE)
{
    gfillrect(DE->de, DE->p->guiColors[dataeditbg], rect(0, 0, DE->p->w, DE->p->h));
}


static void drawrectangle(DEstruct DE,
			  int xpos, int ypos, int width, int height,
			  int lwd, int fore)
{
    /* only used on screen, so always fast */
    gdrawrect(DE->de, lwd, 0, (fore==1)? DE->p->guiColors[dataedituser]: DE->p->guiColors[dataeditbg],
	      rect(xpos, ypos, width, height), 1, PS_ENDCAP_SQUARE,
	      PS_JOIN_BEVEL, 10);
}

static void de_drawtext(DEstruct DE, int xpos, int ypos, const char *text)
{
    gdrawstr(DE->de, DE->p->f, DE->p->guiColors[dataeditfg], pt(xpos, ypos), text);
}

/* Keypress callbacks */

static void de_normalkeyin(control c, int k)
{
    int i, st;
    char text[1];
    DEstruct DE = getdata(c);

    st = ggetkeystate();
    if ((DE->p->chbrk) && (k == DE->p->chbrk) &&
	((!DE->p->modbrk) || ((DE->p->modbrk) && (st == DE->p->modbrk)))) {
	DE->p->fbrk(c);
	return;
    }
    if (st & CtrlKey) {
	switch (k + 'A' - 1) {
	case 'B':
	    i = DE->rowmin - DE->nhigh + 2;
	    jumpwin(DE, DE->colmin, max(i, 1));
	    break;
	case 'F':
	    jumpwin(DE, DE->colmin, DE->rowmax);
	    break;
	case 'H':
	    moveback(DE);
	    break;
	case 'I':
	    if (st & ShiftKey) advancerect(DE, LEFT);
	    else advancerect(DE, RIGHT);
	    break;
	case 'N':
	case 'J':
	    advancerect(DE, DOWN);
	    break;
	case 'C':
	    de_copy(DE->de);
	    break;
	case 'V':
	    de_paste(DE->de);
	    break;
	case 'L':
	    for (i = DE->colmin; i < DE->colmax; i++)
		if (i < 100)
		    DE->boxw[i] = get_col_width(DE, i)*(DE->p->fw) + 8;
	    drawwindow(DE);
	    break;
	default:
	    bell();
	}
    } else if(k == '\b') {
	moveback(DE);
    } else if(k == '\n' || k == '\r') {
	advancerect(DE, DOWN);
    } else if(k == '\t') {
	if (st & ShiftKey) advancerect(DE, LEFT);
	else advancerect(DE, RIGHT);
    } else {
	text[0] = k;
	handlechar(DE, text);
    }

}

static void de_im(control c, font *f, point *pt)
{
    DEstruct DE = getdata(c);
    int x, y;

    drawelt(DE, DE->crow, DE->ccol);
    gsetcursor(DE->de, ArrowCursor);
    find_coords(DE, DE->crow, DE->ccol, &x, &y);
    pt->x = x + DE->text_xoffset + DE->clength * fontwidth(consolefn);
    pt->y = y - DE->text_yoffset - 1;
    *f = consolefn;
}

static void de_ctrlkeyin(control c, int key)
{
    int st, i;
    DEstruct DE = getdata(c);

    st = ggetkeystate();
    if ((DE->p->chbrk) && (key == DE->p->chbrk) &&
	((!DE->p->modbrk) || ((DE->p->modbrk) && (st == DE->p->modbrk)))) {
	DE->p->fbrk(c);
	return;
    }
    switch (key) {
    case HOME:
	jumpwin(DE, 1, 1);
	downlightrect(DE);
	DE->crow = DE->ccol = 1;
	highlightrect(DE);
	break;
    case END:
	i = DE->ymaxused - DE->nhigh + 2;
	if(DE->isEditor)
	    jumpwin(DE, DE->xmaxused, max(i, 1));
	else {
	    /* Try to work out which cols we can fit in */
	    int j, w = 0;
	    for(j = DE->xmaxused;j >= 0; j--) {
		w += BOXW(j);
		if(w > DE->p->w) break;
	    }
	    jumpwin(DE, min(j+2, DE->xmaxused), max(i, 1));
	}
	downlightrect(DE);
	DE->crow = DE->ymaxused - DE->rowmin + 1;
	DE->ccol = 1;
	highlightrect(DE);
	break;
    case PGUP:
	i = DE->rowmin - DE->nhigh + 2;
	jumpwin(DE, DE->colmin, max(i, 1));
	break;
    case PGDN:
	i = DE->ymaxused - DE->nhigh + 2;
	if(DE->isEditor)
	    jumpwin(DE, DE->colmin, DE->rowmax);
	else
	    jumpwin(DE, DE->colmin, min(i, DE->rowmax));
	break;
    case LEFT:
	advancerect(DE, LEFT);
	break;
    case RIGHT:
	advancerect(DE, RIGHT);
	break;
    case UP:
	advancerect(DE, UP);
	break;
    case DOWN:
	advancerect(DE, DOWN);
	break;
    case DEL:
	moveback(DE);
	break;
     case ENTER:
	 advancerect(DE, DOWN);
	 break;
    default:
	;
    }
}

/* mouse callbacks */

static const char *get_cell_text(DEstruct DE)
{
    int  wrow = DE->rowmin + DE->crow - 2, wcol = DE->colmin + DE->ccol - 1;
    const char *prev = "";
    SEXP tvec;

    if (wcol <= DE->xmaxused) {
	tvec = VECTOR_ELT(DE->work, wcol - 1);
	if (!isNull(tvec) && wrow < INTEGER(DE->lens)[wcol - 1]) {
	    PrintDefaults();
	    if (TYPEOF(tvec) == REALSXP) {
		prev = EncodeElement(tvec, wrow, 0, '.');
	    } else if (TYPEOF(tvec) == STRSXP) {
		if (STRING_ELT(tvec, wrow) != DE->ssNA_STRING)
		    prev = EncodeElement(tvec, wrow, 0, '.');
	    } else error(G_("dataentry: internal memory error"));
	}
    }
    return prev;
}

static int online, clickline;

static void de_mousedown(control c, int buttons, point xy)
{
    int xw, yw, wcol=0, wrow, i, w;
    DEstruct DE;

    if (buttons & LeftButton) {
	DE = getdata(c);
	xw = xy.x;
	yw = xy.y;

	closerect(DE);

	/* check to see if the click was in the header */

	if (yw < DE->hwidth + DE->bwidth) {
	    /* too high */
	    return;
	}
	/* translate to box coordinates */

	wrow = (yw - DE->bwidth - DE->hwidth) / DE->box_h;

	/* see if it is in the row labels */
	if (xw < DE->bwidth + DE->boxw[0]) {
	    bell();
	    highlightrect(DE);
	    return;
	}
	w = DE->bwidth + DE->boxw[0];
	for (i = 1; i <= DE->nwide; i++)
	    if((w += BOXW(i+DE->colmin-1)) > xw) {
		wcol = i;
		break;
	    }

	/* see if we selected a line */
	w = DE->bwidth;
	online = 0;
	for (i = 0; i <= DE->nwide; i++) {
	    if(i == 0) w += DE->boxw[0]; else w += BOXW(i+DE->colmin-1);
	    if (abs(w - xw) <= 2) {
		online = 1;
		clickline = i; /* between cols i and i+1 */
		highlightrect(DE);
		gsetcursor(DE->de, HandCursor);
		return;
	    }
	}

	/* next check to see if it is in the column labels */

	if (yw < DE->hwidth + DE->bwidth + DE->box_h) {
	    if (xw > DE->bwidth + DE->boxw[0]) {
		highlightrect(DE);
		de_popupmenu(DE, xw, yw, wcol);
		return;
	    } else {
		/* in 0th column */
		highlightrect(DE);
		bell();
	    }
	} else if (wrow > DE->nhigh - 1 || wcol > DE->nwide - 1) {
	    /* off the grid */
	    highlightrect(DE);
	    bell();
	    return;
	} else if (buttons & DblClick) {
	    int x, y, bw;
	    const char *prev;
	    rect rr;

	    DE->ccol = wcol;
	    DE->crow = wrow;
	    highlightrect(DE);
	    find_coords(DE, DE->crow, DE->ccol, &x, &y);
	    bw = BOXW(DE->ccol + DE->colmin - 1);
	    rr = rect(x + DE->text_xoffset, y - DE->text_yoffset - 1,
		      bw - DE->text_xoffset - 2,
		      DE->box_h - DE->text_yoffset - 2);
	    prev = get_cell_text(DE);
	    if (strlen(prev) * (DE->p->fw) > bw)
		rr.width = (strlen(prev) + 2) * (DE->p->fw);
	    addto(DE->de);
	    DE->celledit = newfield_no_border(prev, rr);
	    settextfont(DE->celledit, DE->p->f);
	    setbackground(DE->celledit, DE->p->guiColors[dataeditbg]);
	    setforeground(DE->celledit, DE->p->guiColors[dataedituser]);

	    show(DE->celledit);
	    DE->CellEditable = TRUE;
	} else if (buttons & LeftButton) {
	    DE->ccol = wcol;
	    DE->crow = wrow;
	}
	highlightrect(DE);
	return;
    }
}

static void de_mouseup(control c, int buttons, point xy)
{
    int xw, bw, i, w;
    DEstruct DE;

    if (online) {
	DE = getdata(c);
	xw = xy.x;
	w = DE->bwidth + DE->boxw[0];
	for(i = 1; i < clickline; i++) w+= BOXW(i+DE->colmin-1);
	bw = xw - w;
	if (bw < (DE->p->fw)*4 + 8) bw = (DE->p->fw)*4 + 8;
	if (bw > (DE->p->fw)*50) bw = (DE->p->fw)*50;
	if(clickline < 100) DE->boxw[clickline] = bw;
	gsetcursor(DE->de, ArrowCursor);
	deredraw(DE);
    }
}

static void de_redraw(control c, rect r)
{
    DEstruct DE = getdata(c);
    if (DE->p->w != DE->oldWIDTH || DE->p->h != DE->oldHEIGHT) drawwindow(DE);
    else deredraw(DE);
}

static void deredraw(DEstruct DE)
{
    int i;

    setcellwidths(DE);

    if(DE->hwidth > 0)
	gfillrect(DE->de, bbg, rect(0, 0, DE->p->w, DE->hwidth));
    gfillrect(DE->de, bbg, rect(0, 0, DE->boxw[0], DE->windowHeight));

    for (i = 1; i < DE->nhigh; i++)
	drawrectangle(DE, 0, DE->hwidth + i * DE->box_h, DE->boxw[0],
		      DE->box_h, 1, 1);
    if(DE->isEditor) {
	DE->colmax = DE->colmin + (DE->nwide - 2);
	DE->rowmax = DE->rowmin + (DE->nhigh - 2);
    } else {
	DE->colmax = min(DE->xmaxused, DE->colmin + (DE->nwide - 2));
	DE->rowmax = min(DE->ymaxused, DE->rowmin + (DE->nhigh - 2));
    }
    printlabs(DE);
    for (i = DE->colmin; i <= DE->colmax; i++) drawcol(DE,i);
    gfillrect(DE->de, DE->p->guiColors[dataeditbg], rect(DE->windowWidth+1, DE->hwidth,
				      DE->p->w - DE->windowWidth-1,
				      DE->p->h - DE->hwidth));
    highlightrect(DE);
}

static void de_closewin(DEstruct DE)
{
    closerect(DE);
    hide(DE->de);
    del(DE->de);
}

static void copyarea(DEstruct DE, int src_x, int src_y, int dest_x, int dest_y)
{
    int mx = max(src_x, dest_x), my = max(src_y, dest_y);
    copyrect(DE->de, pt(dest_x, dest_y),
	     rect(src_x, src_y, DE->windowWidth - mx, DE->windowHeight - my));
}

static void copyH(DEstruct DE, int src_x, int dest_x, int width)
{
    copyrect(DE->de, pt(dest_x, DE->hwidth),
	     rect(src_x, DE->hwidth, width, DE->windowHeight - DE->hwidth));
}

static Rboolean initwin(DEstruct DE, const char *title)
{
    int i, labdigs;
    rect r;

    DE->de = newdataeditor(DE, title);
    if(!DE->de) return TRUE;
    DE->oldWIDTH = DE->oldHEIGHT = 0;
    DE->nboxchars = 5;

    DE->nboxchars = asInteger(GetOption1(install("de.cellwidth")));
    if (DE->nboxchars == NA_INTEGER || DE->nboxchars < 0) DE->nboxchars = 0;
    if (DE->nboxchars > 0) check(DE->de_mvw);
    DE->box_w = ((DE->nboxchars >0)?DE->nboxchars:FIELDWIDTH)*(DE->p->fw) + 8;
    /* this used to presume 4 chars sufficed for row numbering */
    labdigs = max(3, 1+floor(log10((double)DE->ymaxused)));
    DE->boxw[0] = (1+labdigs)*(DE->p->fw) + 8;
    snprintf(DE->labform, 6, "%%%dd", labdigs);
    for(i = 1; i < 100; i++)
	DE->boxw[i] = get_col_width(DE, i) * (DE->p->fw) + 8;
    DE->box_h = (DE->p->fh) + 4;
    DE->text_xoffset = 5;
    DE->text_yoffset = -3;
    setcellwidths(DE);
    DE->nhigh = (DE->p->h - 2 * DE->bwidth - DE->hwidth - 3) / DE->box_h;
    if(!DE->isEditor && DE->nhigh > DE->ymaxused+1) DE->nhigh = DE->ymaxused+1;
    DE->windowHeight = DE->nhigh * DE->box_h + 2 * DE->bwidth + DE->hwidth;
    r = getrect(DE->de);
    r.width = DE->windowWidth + 3;
    r.height = DE->windowHeight + 3;
    resize(DE->de, r);

    DE->CellModified = DE->CellEditable = FALSE;
    bbg = dialog_bg();
    /* set the active cell to be the upper left one */
    DE->crow = 1;
    DE->ccol = 1;
    /* drawwindow(); done as repaint but
       decide if we need scrollbars here to avoid flashing*/
    DE->nhigh = (DE->p->h - 2 * DE->bwidth - DE->hwidth) / DE->box_h;
    gchangescrollbar(DE->de, VWINSB, 0, DE->ymaxused/DE->yScrollbarScale,
		     max(DE->nhigh/DE->yScrollbarScale, 1), 0);
    setcellwidths(DE);
    gchangescrollbar(DE->de, HWINSB, 0, DE->xmaxused/DE->xScrollbarScale,
		     max(DE->nwide/DE->xScrollbarScale, 1), 0);
    show(DE->de);
    show(DE->de); /* a precaution, as PD reports transparent windows */
    BringToTop(DE->de, 0);
    DE->buf[BUFSIZE-1] = '\0';
    return FALSE;
}

/* Menus */

static window wconf, devw;
static radiobutton rb_num, rb_char;
static label lwhat, lrb;
static field varname;
static int isnumeric, popupcol;

static void popupclose(control c)
{
    SEXP tvec;
    char buf[BUFSIZE], clab[25];
    int i;
    DEstruct DE = getdata(c);

    buf[BUFSIZE-1] = '\0';
    strncpy(buf, GA_gettext(varname), BUFSIZE-1);
    if(!strlen(buf)) {
	askok(G_("column names cannot be blank"));
	return;
    }
    if (popupcol > DE->xmaxused) {
	/* extend work, names and lens */
	REPROTECT(DE->work = lengthgets(DE->work, popupcol), DE->wpi);
	REPROTECT(DE->names = lengthgets(DE->names, popupcol), DE->npi);
	/* Last col name is set later */
	for (i = DE->xmaxused+1; i < popupcol - 1; i++) {
	    snprintf(clab, 25, "var%d", i + 1);
	    SET_STRING_ELT(DE->names, i, mkChar(clab));
	}
	REPROTECT(DE->lens = lengthgets(DE->lens, popupcol), DE->lpi);
	DE->xmaxused = popupcol;
    }
    tvec = VECTOR_ELT(DE->work, popupcol - 1);
    if(ischecked(rb_num) && !isnumeric) {
	if (isNull(tvec))
	    SET_VECTOR_ELT(DE->work, popupcol - 1,
			   ssNewVector(DE, REALSXP, 100));
	else
	    SET_VECTOR_ELT(DE->work, popupcol - 1, coerceVector(tvec, REALSXP));
    } else if(ischecked(rb_char) && isnumeric) {
	if (isNull(tvec))
	    SET_VECTOR_ELT(DE->work, popupcol - 1,
			   ssNewVector(DE, STRSXP, 100));
	else
	    SET_VECTOR_ELT(DE->work, popupcol - 1, coerceVector(tvec, STRSXP));
    }
    SET_STRING_ELT(DE->names, popupcol - 1, mkChar(buf));
    hide(wconf);
    del(wconf);
}

static void nm_hit_key(window w, int key)
{
    if(key == '\n') popupclose(wconf);
}

static void de_popupmenu(DEstruct DE, int x_pos, int y_pos, int col)
{
    const char *blah;
    rect r = screen_coords(DE->de);

    popupcol = DE->colmin + col - 1;
    blah = get_col_name(DE, popupcol);
    wconf = newwindow(G_("Variable editor"),
		      rect(x_pos + r.x-150, y_pos + r.y-50, 300, 100),
		      Titlebar | Closebox | Modal);
    setclose(wconf, popupclose);
    setbackground(wconf, bbg);
    lwhat = newlabel(G_("variable name"), rect(10, 22, 90, 20), AlignLeft);
    varname = newfield(blah, rect(100, 20, 120, 20));
    lrb = newlabel(G_("type"), rect(50, 62, 50, 20), AlignLeft);
    rb_num = newradiobutton("numeric", rect(100, 60 , 80, 20), NULL);
    rb_char = newradiobutton("character", rect(180, 60 , 80, 20), NULL);
    isnumeric = (get_col_type(DE, popupcol) == NUMERIC);
    if (isnumeric) check(rb_num); else check(rb_char);
    setkeydown(wconf, nm_hit_key);
    setdata(wconf, DE); /* for popupclose, nm_hit_key */
    show(wconf);
}

static void de_copy(control c)
{
    DEstruct DE = getdata(c);
    copystringtoclipboard(get_cell_text(DE));
}

static void de_paste(control c)
{
    char *p;
    DEstruct DE = getdata(c);

    closerect(DE);
    if ( clipboardhastext() &&
	 !getstringfromclipboard(DE->buf, BUFSIZE-1) ) {
	/* set current cell to first line of clipboard */
	DE->CellModified = TRUE;
	if ((p = strchr(DE->buf, '\n'))) *p = '\0';
	DE->clength = strlen(DE->buf);
	DE->bufp = DE->buf + DE->clength;
	closerect(DE);
    }
    highlightrect(DE);
}

static void de_delete(control c)
{
    DEstruct DE = getdata(c);
    DE->CellModified = TRUE;
    DE->buf[0] = '\0';
    DE->clength = 0;
    DE->bufp = DE->buf + DE->clength;
    closerect(DE);
    highlightrect(DE);
}

static void de_autosize(control c)
{
    DEstruct DE = getdata(c);
    int col = DE->ccol + DE->colmin - 1;

    closerect(DE);
    if(col < 100) {
	DE->boxw[col] = get_col_width(DE,col)*(DE->p->fw) + 8;
	deredraw(DE);
    }
}

static void de_stayontop(control c)
{
    DEstruct DE = getdata(c);
    BringToTop(DE->de, 2);
}

static void de_sbf(control c, int pos)
{
    DEstruct DE = getdata(c);
    if (pos < 0) { /* horizontal */
	DE->colmin = min(DE->xmaxused, -pos*DE->xScrollbarScale);
    } else {
	DE->rowmin = 1 + pos*DE->yScrollbarScale;
	if(DE->rowmin > DE->ymaxused - DE->nhigh + 2)
	    DE->rowmin = max(1,DE->ymaxused - DE->nhigh + 2);
/*	printf("pos %d, rowmin %d, scale %d\n", pos, DE->rowmin, DE->yScrollbarScale); */
    }
    drawwindow(DE);
}

static checkbox varwidths;

static void vw_close(control c)
{
    int x;
    DEstruct DE = getdata(c);
    if (ischecked(varwidths)) x = 0;
    else x = atoi(GA_gettext(varname)); /* 0 if error */
    x = min(x, 50);
    if (x != DE->nboxchars) {
	DE->nboxchars = x;
	DE->box_w = ((DE->nboxchars >0)?DE->nboxchars:FIELDWIDTH)*(DE->p->fw) + 8;
	deredraw(DE);
    }
    hide(devw);
    del(devw);
    if (DE->nboxchars > 0) check(DE->de_mvw);
    else uncheck(DE->de_mvw);
    addto(DE->de);
}

static void vw_hit_key(window w, int key)
{
    if(key == '\n') vw_close(w);
}

static void vw_callback(control c)
{
    if (ischecked(varwidths)) disable(varname);
    else enable(varname);
}


static void de_popup_vw(DEstruct DE)
{
    char blah[25];

    devw = newwindow(G_("Cell width(s)"),
		     rect(0, 0, 250, 60),
		     Titlebar | Centered | Closebox | Modal);
    setdata(devw, DE);
    setclose(devw, vw_close);
    setbackground(devw, bbg);
    lwhat = newlabel(G_("Cell width"), rect(10, 20, 70, 20), AlignLeft);
    snprintf(blah, 25, "%d", DE->nboxchars);
    varname = newfield(blah, rect(80, 20, 40, 20));
    varwidths = newcheckbox(G_("variable"), rect(150, 20, 80, 20), vw_callback);
    if (DE->nboxchars == 0) {
	check(varwidths);
	disable(varname);
    }
    setkeydown(devw, vw_hit_key);
    show(devw);
}

static void menudecellwidth(control m)
{
    de_popup_vw(getdata(m));
}

static void deldataeditor(control m)
{
    DEstruct DE = getdata(m);
    freeConsoleData(DE->p);
}

static void declose(control m)
{
    DEstruct DE = getdata(m);

    de_closewin(DE);
    show(RConsole);
    R_de_up = FALSE;
}

static void deresize(console c, rect r)
{
    DEstruct DE = getdata(c);
    if (((DE->p->w  == r.width) &&
	 (DE->p->h == r.height)) ||
	(r.width == 0) || (r.height == 0) ) /* minimize */
	return;;
    DE->p->w = r.width;
    DE->p->h = r.height;
}


static void menudehelp(control m)
{
    char s[] = GN_("Navigation.\n  Keyboard: cursor keys move selection\n\tTab move right, Shift+Tab moves left\n\tPgDn or Ctrl+F: move down one screenful\n\tPgUp or Ctrl+B: move up one screenful\n\tHome: move to (1,1) cell\n\tEnd: show last rows of last column.\n   Mouse: left-click in a cell, use the scrollbar(s).\n\nEditing.\n  Type in the currently hightlighted cell\n  Double-click in a cell for an editable field\n\nMisc.\n  Ctrl-L redraws the screen, auto-resizing the columns\n  Ctrl-C copies selected cell\n  Ctrl-V pastes to selected cell\n  Right-click menu for copy, paste, autosize currently selected column\n\n");
    askok(G_(s));
}


static MenuItem DePopup[28] = {
    {GN_("Help"), menudehelp, 0},
    {"-", 0, 0},
    {GN_("Copy selected cell"), de_copy, 0},
    {GN_("Paste to selected cell"), de_paste, 0},
    {GN_("Autosize column"), de_autosize, 0},
    {"-", 0, 0},
    {GN_("Stay on top"), de_stayontop, 0},
    {"-", 0, 0},
    {GN_("Close"), declose, 0},
    LASTMENUITEM
};

static void demenuact(control m)
{
    /* use this to customize the menu */
}

static void depopupact(control m)
{
    DEstruct DE = getdata(m);
    /* use this to customize the menu */

    if (ismdi())
	disable(DePopup[6].m);
    else {
	if (isTopmost(DE->de))
	    check(DePopup[6].m);
	else
	    uncheck(DePopup[6].m);
    }
}


#define MCHECK(a) if (!(a)) {del(c);return NULL;}

RECT *RgetMDIsize(void); /* in rui.c */

static dataeditor newdataeditor(DEstruct DE, const char *title)
{
    int w, h, x, y;
    dataeditor c;
    menuitem m;

    DE->p = newconsoledata((consolefn) ? consolefn : FixedFont,
			   pagerrow, pagercol, 0, 0,
			   guiColors,
			   DATAEDITOR, 0, 0);
    if (!DE->p) return NULL;

    w = DE->p->w ;
    h = DE->p->h;
    if (ismdi()) {
	RECT *pR = RgetMDIsize();
	x = (pR->right - w) / 3; x = x > 20 ? x:20;
	y = (pR->bottom - h) / 3; y = y > 20 ? y:20;
    } else {
	x = (devicewidth(NULL) - w) / 3;
	y = (deviceheight(NULL) - h) / 3 ;
    }
    c = (dataeditor) newwindow(title, rect(x, y, w, h),
			       Document | StandardWindow | Menubar |
			       VScrollbar | HScrollbar | TrackMouse);
    if (!c) {
	freeConsoleData(DE->p);
	return NULL;
    }
    setdata(c, DE);
    if(h == 0) DE->p->h = getheight(c);
    if(w == 0) DE->p->w  = getwidth(c);
    (DE->p->cols) = DE->p->w / (DE->p->fw) - 1;
    (DE->p->rows) = DE->p->h / (DE->p->fh) - 1;
    DE->p->right = (DE->p->w - (DE->p->cols)*(DE->p->fw)) / 2;
    DE->p->top = (DE->p->h - (DE->p->rows)*(DE->p->fh)) / 2;
    gsetcursor(c, ArrowCursor);
    setbackground(c, guiColors[dataeditbg]);
    if (ismdi() && (RguiMDI & RW_TOOLBAR)) {
	/* blank toolbar to stop windows jumping around */
	int btsize = 24;
	control tb;
	addto(c);
	MCHECK(tb = newtoolbar(btsize + 4));
	gsetcursor(tb, ArrowCursor);
    }
    if(DE->isEditor) {
	MCHECK(m = gpopup(depopupact, DePopup));
	setdata(m, DE);
	setdata(DePopup[2].m, DE);
	setdata(DePopup[3].m, DE);
	setdata(DePopup[4].m, DE);
	setdata(DePopup[6].m, DE);
	setdata(DePopup[8].m, DE);
    }
    MCHECK(m = newmenubar(demenuact));
    MCHECK(newmenu(G_("File")));
/*    MCHECK(m = newmenuitem("-", 0, NULL));*/
    MCHECK(m = newmenuitem(G_("Close"), 0, declose));
    setdata(m, DE);
    if(DE->isEditor) {
	newmdimenu();
	MCHECK(newmenu(G_("Edit")));
	MCHECK(m = newmenuitem(G_("Copy  \tCTRL+C"), 0, de_copy));
	setdata(m, DE);
	MCHECK(m = newmenuitem(G_("Paste \tCTRL+V"), 0, de_paste));
	setdata(m, DE);
	MCHECK(m = newmenuitem(G_("Delete\tDEL"), 0, de_delete));
	setdata(m, DE);
	MCHECK(m = newmenuitem("-", 0, NULL));
	MCHECK(m = DE->de_mvw = newmenuitem(G_("Cell widths ..."), 0,
					    menudecellwidth));
	setdata(m, DE);
	MCHECK(m = newmenu(G_("Help")));
	MCHECK(newmenuitem(G_("Data editor"), 0, menudehelp));
    }

    setdata(c, DE); /* Why the repeat? */
    setresize(c, deresize);
    setredraw(c, de_redraw);
    setdel(c, deldataeditor);
    setclose(c, declose);
    sethit(c, de_sbf);
    setkeyaction(c, de_ctrlkeyin);
    if(DE->isEditor) {
	setkeydown(c, de_normalkeyin);
	setim(c, de_im);
	setmousedown(c, de_mousedown);
	setmouseup(c, de_mouseup);
    }
    return(c);
}

static void dv_closewin_cend(void *data)
{
    DEstruct DE = (DEstruct) data;
    R_ReleaseObject(DE->lens);
    R_ReleaseObject(DE->work);
    de_closewin(DE);
    free(DE);
}

SEXP Win_dataviewer(SEXP args)
{
    SEXP stitle;
    SEXPTYPE type;
    int i, nprotect;
    RCNTXT cntxt;
    DEstruct DE = (DEstruct) malloc(sizeof(destruct));

    DE->isEditor = FALSE;
    nprotect = 0;/* count the PROTECT()s */
    DE->work = CAR(args);
    DE->names = getAttrib(DE->work, R_NamesSymbol);

    if (TYPEOF(DE->work) != VECSXP)
	error(G_("invalid argument"));
    stitle = CADR(args);
    if (!isString(stitle) || LENGTH(stitle) != 1)
	error(G_("invalid argument"));

    /* initialize the constants */

    DE->bufp = DE->buf;
    DE->ne = 0;
    DE->currentexp = 0;
    DE->nneg = 0;
    DE->ndecimal = 0;
    DE->clength = 0;
    DE->inSpecial = 0;
    DE->ccol = 1;
    DE->crow = 1;
    DE->colmin = 1;
    DE->rowmin = 1;
    PROTECT(DE->ssNA_STRING = duplicate(NA_STRING));
    nprotect++;
    DE->bwidth = 0;
    DE->hwidth = 5;

    /* setup lens  */
    DE->xmaxused = length(DE->work); DE->ymaxused = 0;
    PROTECT_WITH_INDEX(DE->lens = allocVector(INTSXP, DE->xmaxused), &DE->lpi);
    nprotect++;

    for (i = 0; i < DE->xmaxused; i++) {
	int len = LENGTH(VECTOR_ELT(DE->work, i));
	INTEGER(DE->lens)[i] = len;
	DE->ymaxused = max(len, DE->ymaxused);
	type = TYPEOF(VECTOR_ELT(DE->work, i));
	if (type != STRSXP && type != REALSXP)
	    error(G_("invalid argument"));
    }

    DE->xScrollbarScale = DE->yScrollbarScale = 1;

    /* start up the window, more initializing in here */
    if (initwin(DE, CHAR(STRING_ELT(stitle, 0))))
	error(G_("invalid device"));

    /* set up a context which will close the window if there is an error */
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &dv_closewin_cend;
    cntxt.cenddata = (void *)DE;

    R_PreserveObject(DE->work); /* also preserves names */
    R_PreserveObject(DE->lens);
    UNPROTECT(nprotect);
    return R_NilValue;
}
