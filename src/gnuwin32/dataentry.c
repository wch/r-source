/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2006  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
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
#include <R_ext/rlocale.h>

#include "Defn.h"
#include "Print.h"

#include "graphapp/ga.h"
#include "console.h"
#include "consolestructs.h"
#include "rui.h"

static dataeditor de;
static ConsoleData p;

typedef enum {UNKNOWNN, NUMERIC, CHARACTER} CellType;

static Rboolean R_de_up;

#ifndef max
#define max(a, b) (((a)>(b))?(a):(b))
#endif
#ifndef min
#define min(a, b) (((a)<(b))?(a):(b))
#endif
#define BOXW(x) (min(((x<100 && nboxchars==0)?boxw[x]:box_w),WIDTH-boxw[0]-2*bwidth-2))

#define FIELDWIDTH 10
#define BUFSIZE 200

/* Local Function Definitions */

static void advancerect(int);
static void bell();
static void cleararea(int, int, int, int, rgb);
static void clearrect(void);
static void closerect(void);
static void clearwindow(void);
static void de_closewin(void);
static void copyarea(int, int, int, int);
static void copyH(int, int, int);
static void deredraw(void);
static void eventloop(void);
static void downlightrect(void);
static void drawwindow(void);
static void drawcol(int);
/* static void de_drawline(int, int, int, int);*/
static void de_drawtext(int, int, char *);
static void drawrectangle(int, int, int, int, int, int);
static void drawrow(int);
static void find_coords(int, int, int*, int*);
static void handlechar(char*);
static void highlightrect(void);
static Rboolean initwin(void);
static void jumppage(int);
static void jumpwin(int, int);
static void de_popupmenu(int, int, int);
static void printlabs(void);
static void printrect(int, int);
static void printstring(char*, int, int, int, int);
static void printelt(SEXP, int, int, int);
static void setcellwidths(void);

static dataeditor newdataeditor();
static void de_copy(control c);
static void de_paste(control c);
static void de_delete(control c);
static menuitem de_mvw;

static SEXP work, names, lens;
static PROTECT_INDEX wpi, npi, lpi;
static SEXP ssNA_STRING;
static double ssNA_REAL;

/* Global variables needed for the graphics */

static int box_w;                       /* width of a box */
static int boxw[100];                   /* widths of cells */
static int box_h;                       /* height of a box */
static int windowWidth;                 /* current width of the window */
static int windowHeight;                /* current height of the window */
static int currentexp;                  /* boolean: whether an cell is active */
static int crow;                        /* current row */
static int ccol;                        /* current column */
static int nwide, nhigh;
static int colmax, colmin, rowmax, rowmin;
static int ndecimal;                    /* count decimal points */
static int ne;                          /* count exponents */
static int nneg;			/* indicate whether its a negative */
static int clength;                     /* number of characters currently entered */
static char buf[BUFSIZE];
static char *bufp;
static int bwidth;			/* width of the border */
static int hwidth;			/* width of header  */
static int text_xoffset, text_yoffset;
static field celledit;
static Rboolean newcol, CellModified, CellEditable;
static int xmaxused, ymaxused;
static int oldWIDTH=0, oldHEIGHT=0;
static int nboxchars=0;
static int labdigs=4;
static char labform[6];
static int xScrollbarScale=1, yScrollbarScale=1;

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h> /* for Sleep */

int mb_char_len(char *buf, int clength, wchar_t *wc); /* from console.c */

static void moveback()
{
    int mb_len;
    wchar_t wc;

    if (clength > 0) {
	mb_len = mb_char_len(buf, clength-1, &wc);
	clength -= mb_len;
	bufp -= mb_len;
	printstring(buf, clength, crow, ccol, 1);
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
   ssNewVector is just an interface to allocVector but it lets us
   set the fields to NA. We need to have a special NA for reals and
   strings so that we can differentiate between uninitialized elements
   in the vectors and user supplied NA's; hence ssNA_REAL and ssNA_STRING
 */

static SEXP ssNewVector(SEXPTYPE type, int vlen)
{
    SEXP tvec;
    int j;

    tvec = allocVector(type, vlen);
    for (j = 0; j < vlen; j++)
	if (type == REALSXP)
	    REAL(tvec)[j] = ssNA_REAL;
	else if (type == STRSXP)
	    SET_STRING_ELT(tvec, j, STRING_ELT(ssNA_STRING, 0));
    return (tvec);
}
static void eventloop()
{
    while (R_de_up) {
	/* avoid consuming 100% CPU time here */
	Sleep(10);
	R_ProcessEvents();
    }
}

static void de_closewin_cend(void *data)
{
    de_closewin();
}

SEXP do_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP colmodes, tnames, tvec, tvec2, work2;
    SEXPTYPE type;
    int i, j, cnt, len, nprotect;
    RCNTXT cntxt;
    char clab[25];

    nprotect = 0;/* count the PROTECT()s */
    PROTECT_WITH_INDEX(work = duplicate(CAR(args)), &wpi); nprotect++;
    colmodes = CADR(args);
    tnames = getAttrib(work, R_NamesSymbol);

    if (TYPEOF(work) != VECSXP || TYPEOF(colmodes) != VECSXP)
	errorcall(call, G_("invalid argument"));

    /* initialize the constants */

    bufp = buf;
    ne = 0;
    currentexp = 0;
    nneg = 0;
    ndecimal = 0;
    clength = 0;
    ccol = 1;
    crow = 1;
    colmin = 1;
    rowmin = 1;
    ssNA_REAL = -NA_REAL;
    tvec = allocVector(REALSXP, 1);
    REAL(tvec)[0] = ssNA_REAL;
    PROTECT(ssNA_STRING = coerceVector(tvec, STRSXP)); nprotect++;
    bwidth = 0;
    hwidth = 5;

    /* setup work, names, lens  */
    xmaxused = length(work); ymaxused = 0;
    PROTECT_WITH_INDEX(lens = allocVector(INTSXP, xmaxused), &lpi);
    nprotect++;

    if (isNull(tnames)) {
	PROTECT_WITH_INDEX(names = allocVector(STRSXP, xmaxused), &npi);
	for(i = 0; i < xmaxused; i++) {
	    sprintf(clab, "var%d", i);
	    SET_STRING_ELT(names, i, mkChar(clab));
	}
    } else
	PROTECT_WITH_INDEX(names = duplicate(tnames), &npi);
    nprotect++;
    for (i = 0; i < xmaxused; i++) {
	int len = LENGTH(VECTOR_ELT(work, i));
	INTEGER(lens)[i] = len;
	ymaxused = max(len, ymaxused);
	type = TYPEOF(VECTOR_ELT(work, i));
	if (LENGTH(colmodes) > 0 && !isNull(VECTOR_ELT(colmodes, i)))
	    type = str2type(CHAR(STRING_ELT(VECTOR_ELT(colmodes, i), 0)));
	if (type != STRSXP) type = REALSXP;
	if (isNull(VECTOR_ELT(work, i))) {
	    if (type == NILSXP) type = REALSXP;
	    SET_VECTOR_ELT(work, i, ssNewVector(type, 100));
	} else if (!isVector(VECTOR_ELT(work, i)))
	    errorcall(call, G_("invalid type for value"));
	else {
	    if (TYPEOF(VECTOR_ELT(work, i)) != type)
		SET_VECTOR_ELT(work, i,
			       coerceVector(VECTOR_ELT(work, i), type));
	}
    }

    /* scale scrollbars as needed */
    if (xmaxused > 10000) xScrollbarScale = xmaxused/1000;
    if (ymaxused > 10000) yScrollbarScale = ymaxused/1000;

    /* start up the window, more initializing in here */
    if (initwin())
	errorcall(call, G_("invalid device"));

    /* set up a context which will close the window if there is an error */
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &de_closewin_cend;
    cntxt.cenddata = NULL;

    highlightrect();

    eventloop();

    endcontext(&cntxt);

    /* drop out unused columns */
    for(i = 0, cnt = 0; i < xmaxused; i++)
	if(!isNull(VECTOR_ELT(work, i))) cnt++;
    if (cnt < xmaxused) {
	PROTECT(work2 = allocVector(VECSXP, cnt)); nprotect++;
	for(i = 0, j = 0; i < xmaxused; i++) {
	    if(!isNull(VECTOR_ELT(work, i))) {
		SET_VECTOR_ELT(work2, j, VECTOR_ELT(work, i));
		INTEGER(lens)[j] = INTEGER(lens)[i];
		SET_STRING_ELT(names, j, STRING_ELT(names, i));
		j++;
	    }
	}
	REPROTECT(names = lengthgets(names, cnt), npi);
    } else work2 = work;

    for (i = 0; i < LENGTH(work2); i++) {
	len = INTEGER(lens)[i];
	tvec = VECTOR_ELT(work2, i);
	if (LENGTH(tvec) != len) {
	    tvec2 = ssNewVector(TYPEOF(tvec), len);
	    for (j = 0; j < len; j++) {
		if (TYPEOF(tvec) == REALSXP) {
		    if (REAL(tvec)[j] != ssNA_REAL)
			REAL(tvec2)[j] = REAL(tvec)[j];
		    else
			REAL(tvec2)[j] = NA_REAL;
		} else if (TYPEOF(tvec) == STRSXP) {
		    if (!streql(CHAR(STRING_ELT(tvec, j)),
				CHAR(STRING_ELT(ssNA_STRING, 0))))
			SET_STRING_ELT(tvec2, j, STRING_ELT(tvec, j));
		    else
			SET_STRING_ELT(tvec2, j, NA_STRING);
		} else
		    error(G_("dataentry: internal memory problem"));
	    }
	    SET_VECTOR_ELT(work2, i, tvec2);
	}
    }

    setAttrib(work2, R_NamesSymbol, names);
    UNPROTECT(nprotect);
    return work2;
}

/* Window Drawing Routines */

static rgb bbg;

static void setcellwidths(void)
{
    int i, w, dw;

    windowWidth = w = 2*bwidth + boxw[0] + BOXW(colmin);
    nwide = 2;
    for (i = 2; i < 100; i++) { /* 100 on-screen columns cannot occur */
	dw = BOXW(i + colmin - 1);
	if((w += dw) > WIDTH) {
	    nwide = i;
	    windowWidth = w - dw;
	    break;
	}
    }
}


static void drawwindow(void)
{
    /* might have resized */
    setcellwidths();
    nhigh = (HEIGHT - 2 * bwidth - hwidth - 3) / box_h;
    windowHeight = nhigh * box_h + 2 * bwidth + hwidth;
    oldWIDTH = WIDTH;
    oldHEIGHT = HEIGHT;

    clearwindow();
    deredraw();
    /* row/col 1 = pos 0 */
    gchangescrollbar(de, VWINSB, (rowmin - 1)/yScrollbarScale,
		     ymaxused/yScrollbarScale,
		     max(nhigh/yScrollbarScale, 1), 0);
    gchangescrollbar(de, HWINSB, (colmin - 1)/xScrollbarScale,
		     xmaxused/xScrollbarScale,
		     max(nwide/xScrollbarScale, 1), 0);
}

static void doHscroll(int oldcol)
{
    int i, dw;
    int oldnwide = nwide, oldwindowWidth = windowWidth;

    /* horizontal re-position */
    setcellwidths();
    colmax = colmin + (nwide - 2);
    if (oldcol < colmin) { /* drop oldcol...colmin - 1 */
	dw = boxw[0];
	for (i = oldcol; i < colmin; i++) dw += BOXW(i);
	copyH(dw, boxw[0], oldwindowWidth - dw + 1);
	dw = oldwindowWidth - BOXW(oldcol) + 1;
	cleararea(dw, hwidth, WIDTH-dw, HEIGHT, p->bg);
	/* oldnwide includes the row labels */
	for (i = oldcol+oldnwide-1; i <= colmax; i++) drawcol(i);
    } else {
	/* move one or more cols left */
	dw = BOXW(colmin);
	copyH(boxw[0], boxw[0] + dw, windowWidth - dw + 1);
	dw = windowWidth + 1;
	cleararea(dw, hwidth, WIDTH-dw, HEIGHT, p->bg);
	drawcol(colmin);
    }
    gchangescrollbar(de, HWINSB, (colmin - 1)/xScrollbarScale,
		     xmaxused/xScrollbarScale,
		     max(nwide/xScrollbarScale, 1), 0);
    highlightrect();
}

/* find_coords finds the coordinates of the upper left corner of the
   given cell on the screen: row and col are on-screen coords */

static void find_coords(int row, int col, int *xcoord, int *ycoord)
{
    int i, w;
    w = bwidth;
    if (col > 0) w += boxw[0];
    for(i = 1; i < col; i ++) w += BOXW(i + colmin - 1);
    *xcoord = w;
    *ycoord = bwidth + hwidth + box_h * row;
}

/* draw the window with the top left box at column wcol and row wrow */

static void jumpwin(int wcol, int wrow)
{
    if (wcol < 0 || wrow < 0) {
	bell();
	return;
    }
    closerect();
    if (colmin != wcol || rowmin != wrow) {
	colmin = wcol;
	rowmin = wrow;
	deredraw();
    } else highlightrect();
}

static void advancerect(int which)
{

    /* if we are in the header, changing a name then only down is
       allowed */
    if (crow < 1 && which != DOWN) {
	bell();
	return;
    }

    closerect();

    switch (which) {
    case UP:
	if (crow == 1) {
	    if (rowmin == 1)
		bell();
	    else
		jumppage(UP);
	} else
	    crow--;
	break;
    case DOWN:
	if (crow == (nhigh - 1))
	    jumppage(DOWN);
	else
	    crow++;
	break;
    case RIGHT:
	if (ccol == (nwide - 1))
	    jumppage(RIGHT);
	else
	    ccol++;
	break;
    case LEFT:
	if (ccol == 1) {
	    if (colmin == 1)
		bell();
	    else
		jumppage(LEFT);
	} else
	    ccol--;
	break;
    default:
	UNIMPLEMENTED("advancerect");
    }

    highlightrect();
}

static char *get_col_name(int col)
{
    static char clab[25];
    if (col <= xmaxused) {
	/* don't use NA labels */
	SEXP tmp = STRING_ELT(names, col - 1);
	if(tmp != NA_STRING) return(CHAR(tmp));
    }
    sprintf(clab, "var%d", col);
    return clab;
}

static int get_col_width(int col)
{
    int i, w = 0, w1, fw = FIELDWIDTH;
    char *strp;
    SEXP tmp, lab;

    if (nboxchars > 0) return nboxchars;
    if (col <= xmaxused) {
	tmp = VECTOR_ELT(work, col - 1);
	if (isNull(tmp)) return fw;
	/* don't use NA labels */
	lab = STRING_ELT(names, col - 1);
	if(lab != NA_STRING) w = strlen(CHAR(lab)); else w = fw;
	PrintDefaults(R_NilValue);
	for (i = 0; i < INTEGER(lens)[col - 1]; i++) {
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

static CellType get_col_type(int col)
{
    SEXP tmp;
    CellType res = UNKNOWNN;

    if (col <= xmaxused) {
	tmp = VECTOR_ELT(work, col - 1);
	if(TYPEOF(tmp) == REALSXP) res = NUMERIC;
	if(TYPEOF(tmp) == STRSXP) res = CHARACTER;
    }
    return res;
}


/* whichcol is absolute col no, col is position on screen */
static void drawcol(int whichcol)
{
    int i, src_x, src_y, len, col = whichcol - colmin + 1, bw = BOXW(whichcol);
    char *clab;
    SEXP tmp;

    find_coords(0, col, &src_x, &src_y);
    cleararea(src_x, src_y, bw, windowHeight, p->bg);
    cleararea(src_x, src_y, bw, box_h, bbg);
    for (i = 0; i < nhigh; i++)
	drawrectangle(src_x, hwidth + i * box_h, bw, box_h, 1, 1);

    /* now fill it in if it is active */
    clab = get_col_name(whichcol);
    printstring(clab, strlen(clab), 0, col, 0);

   if (xmaxused >= whichcol) {
	tmp = VECTOR_ELT(work, whichcol - 1);
	if (!isNull(tmp)) {
	    len = min(rowmax, INTEGER(lens)[whichcol - 1]);
	    for (i = (rowmin - 1); i < len; i++)
		printelt(tmp, i, i - rowmin + 2, col);
	}
    }
}


/* whichrow is absolute row no */
static void drawrow(int whichrow)
{
    int i, src_x, src_y, row = whichrow - rowmin + 1, w;
    char rlab[15];
    SEXP tvec;

    find_coords(row, 0, &src_x, &src_y);
    cleararea(src_x, src_y, windowWidth, box_h, (whichrow > 0)?p->bg:bbg);
    drawrectangle(src_x, src_y, boxw[0], box_h, 1, 1);

    sprintf(rlab, labform, whichrow);
    printstring(rlab, strlen(rlab), row, 0, 0);

    w = bwidth + boxw[0];
    for (i = colmin; i <= colmax; i++) {
	drawrectangle(w, src_y, BOXW(i), box_h, 1, 1);
	w += BOXW(i);
    }

    for (i = colmin; i <= colmax; i++) {
	if (i > xmaxused) break;
	if (!isNull(tvec = VECTOR_ELT(work, i - 1)))
	    if (whichrow <= INTEGER(lens)[i - 1])
		printelt(tvec, whichrow - 1, row, i - colmin + 1);
    }
}

/* printelt: print the correct value from vector[vrow] into the
   spreadsheet in row ssrow and col sscol */

/* WARNING: This has no check that you're not beyond the end of the
   vector. Caller must check. */

static void printelt(SEXP invec, int vrow, int ssrow, int sscol)
{
    char *strp;
    PrintDefaults(R_NilValue);
    if (TYPEOF(invec) == REALSXP) {
	if (REAL(invec)[vrow] != ssNA_REAL) {
	    strp = EncodeElement(invec, vrow, 0, '.');
	    printstring(strp, strlen(strp), ssrow, sscol, 0);
	}
    }
    else if (TYPEOF(invec) == STRSXP) {
	if (!streql(CHAR(STRING_ELT(invec, vrow)),
		    CHAR(STRING_ELT(ssNA_STRING, 0)))) {
	    strp = EncodeElement(invec, vrow, 0, '.');
	    printstring(strp, strlen(strp), ssrow, sscol, 0);
	}
    }
    else
	error(G_("dataentry: internal memory error"));
}


static void drawelt(int whichrow, int whichcol)
{
    int i;
    char *clab;
    SEXP tmp;

    if (whichrow == 0) {
	clab = get_col_name(whichcol + colmin - 1);
	printstring(clab, strlen(clab), 0, whichcol, 0);
    } else {
	if (xmaxused >= whichcol + colmin - 1) {
	    tmp = VECTOR_ELT(work, whichcol + colmin - 2);
	    if (!isNull(tmp) && (i = rowmin + whichrow - 2) <
		INTEGER(lens)[whichcol + colmin - 2] )
		printelt(tmp, i, whichrow, whichcol);
	} else
	    printstring("", 0, whichrow,  whichcol, 0);
    }
}

static void jumppage(int dir)
{
    int i, w, oldcol, wcol;

    switch (dir) {
    case UP:
	rowmin--;
	rowmax--;
	copyarea(0, hwidth + box_h, 0, hwidth + 2 * box_h);
	drawrow(rowmin);
	gchangescrollbar(de, VWINSB, (rowmin - 1)/yScrollbarScale,
			 ymaxused/yScrollbarScale,
			 max(nhigh/yScrollbarScale, 1), 0);
	break;
    case DOWN:
	if (rowmax >= 65535) return;
	rowmin++;
	rowmax++;
	copyarea(0, hwidth + 2 * box_h, 0, hwidth + box_h);
	drawrow(rowmax);
	gchangescrollbar(de, VWINSB, (rowmin - 1)/yScrollbarScale,
			 ymaxused/yScrollbarScale,
			 max(nhigh/yScrollbarScale, 1), 0);
	break;
    case LEFT:
	colmin--;
	doHscroll(colmin+1);
	break;
    case RIGHT:
	oldcol = colmin;
	wcol = colmin + ccol + 1; /* column to be selected */
        /* There may not be room to fit the next column in */
	w = WIDTH - boxw[0] - BOXW(colmax + 1);
	for (i = colmax; i >= oldcol; i--) {
	    w -= BOXW(i);
	    if(w < 0) {
		colmin = i + 1;
		break;
	    }
	}
	ccol = wcol - colmin;
	doHscroll(oldcol);
	break;
    }
}
/* draw a rectangle, used to highlight/downlight the current box */

static void printrect(int lwd, int fore)
{
    int x, y;
    find_coords(crow, ccol, &x, &y);
    drawrectangle(x + lwd - 1, y + lwd - 1,
		  BOXW(ccol+colmin-1) - lwd + 1,
		  box_h - lwd + 1, lwd, fore);
}

static void downlightrect(void)
{
    printrect(2, 0);
    printrect(1, 1);
}

static void highlightrect(void)
{
    printrect(2, 1);
}


static void getccol()
{
    SEXP tmp, tmp2;
    int i, len, newlen, wcol, wrow;
    SEXPTYPE type;
    char clab[25];

    wcol = ccol + colmin - 1;
    wrow = crow + rowmin - 1;
    if (wcol > xmaxused) {
	/* extend work, names and lens */
	REPROTECT(work = lengthgets(work, wcol), wpi);
	REPROTECT(names = lengthgets(names, wcol), npi);
	for (i = xmaxused; i < wcol; i++) {
	    sprintf(clab, "var%d", i + 1);
	    SET_STRING_ELT(names, i, mkChar(clab));
	}
	REPROTECT(lens = lengthgets(lens, wcol), lpi);
	xmaxused = wcol;
    }
    newcol = FALSE;
    if (isNull(VECTOR_ELT(work, wcol - 1))) {
	newcol = TRUE;
	SET_VECTOR_ELT(work, wcol - 1, ssNewVector(REALSXP, max(100, wrow)));
	INTEGER(lens)[wcol - 1] = 0;
    }
    if (!isVector(tmp = VECTOR_ELT(work, wcol - 1)))
	error(G_("internal type error in dataentry"));
    len = INTEGER(lens)[wcol - 1];
    type = TYPEOF(tmp);
    if (len < wrow) {
	for (newlen = max(len * 2, 10) ; newlen < wrow ; newlen *= 2)
	    ;
	tmp2 = ssNewVector(type, newlen);
	for (i = 0; i < len; i++)
	    if (type == REALSXP)
		REAL(tmp2)[i] = REAL(tmp)[i];
	    else if (type == STRSXP)
		SET_STRING_ELT(tmp2, i, STRING_ELT(tmp, i));
	    else
		error(G_("internal type error in dataentry"));
	SET_VECTOR_ELT(work, wcol - 1, tmp2);
    }
}

/* close up the entry to a cell, put the value that has been entered
   into the correct place and as the correct type */

static void closerect(void)
{
    SEXP cvec;
    int wcol = ccol + colmin - 1, wrow = rowmin + crow - 1, wrow0;

    *bufp = '\0';

    if (CellModified || CellEditable) {
	if (CellEditable) {
	    strncpy(buf, GA_gettext(celledit), BUFSIZE-1);
	    clength = strlen(buf);
	    hide(celledit);
	    del(celledit);
	}
	getccol();
	cvec = VECTOR_ELT(work, wcol - 1);
	wrow0 = INTEGER(lens)[wcol - 1];
	if (wrow > wrow0) INTEGER(lens)[wcol - 1] = wrow;
	ymaxused = max(ymaxused, wrow);
	if (clength != 0) {
	    /* do it this way to ensure NA, Inf, ...  can get set */
	    char *endp;
	    double new = R_strtod(buf, &endp);
	    int warn = !isBlankString(endp);
	    if (TYPEOF(cvec) == STRSXP)
		SET_STRING_ELT(cvec, wrow - 1, mkChar(buf));
	    else
		REAL(cvec)[wrow - 1] = new;
	    if (newcol & warn) {
		/* change mode to character */
		SET_VECTOR_ELT(work, wcol - 1, coerceVector(cvec, STRSXP));
		SET_STRING_ELT(VECTOR_ELT(work, wcol - 1), wrow - 1,
			       mkChar(buf));
	    }
	} else {
	    if (TYPEOF(cvec) == STRSXP)
		SET_STRING_ELT(cvec, wrow - 1, NA_STRING);
	    else
		REAL(cvec)[wrow - 1] = NA_REAL;
	}
	drawelt(crow, ccol);  /* to get the cell scrolling right */
	if(wrow > wrow0) drawcol(wcol); /* to fill in NAs */
    }
    CellEditable = CellModified = FALSE;

    downlightrect();
    gsetcursor(de, ArrowCursor);

    ndecimal = 0;
    nneg = 0;
    ne = 0;
    currentexp = 0;
    clength = 0;
    bufp = buf;
}

/* print a null terminated string, check to see if it is longer than
   the print area and print it, left adjusted if necessary; clear the
   area of previous text; */

/* This version will only display BUFSIZE chars, but the maximum col width
   will not allow that many */
static void printstring(char *ibuf, int buflen, int row, int col, int left)
{
    int x_pos, y_pos, bw, fw, bufw;
    char buf[BUFSIZE+1];

    find_coords(row, col, &x_pos, &y_pos);
    if (col == 0) bw = boxw[0]; else bw = BOXW(col+colmin-1);
    cleararea(x_pos + 1, y_pos + 1, bw - 1, box_h - 1,
	      (row==0 || col==0) ? bbg:p->bg);
    fw = min(BUFSIZE, (bw - 8)/FW);
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
    de_drawtext(x_pos + text_xoffset, y_pos - text_yoffset, buf);
}

static void clearrect(void)
{
    int x_pos, y_pos;

    find_coords(crow, ccol, &x_pos, &y_pos);
    cleararea(x_pos, y_pos, BOXW(ccol+colmin-1), box_h, p->bg);
}

/* handlechar has to be able to parse decimal numbers and strings,
   depending on the current column type, only printing characters
   should get this far */

/* --- Not true! E.g. ESC ends up in here... */

static void handlechar(char *text)
{
    int c = text[0];

    if ( c == '\033' ) {
	CellModified = FALSE;
	clength = 0;
	drawelt(crow, ccol);
	gsetcursor(de, ArrowCursor);
	return;
    } else {
	CellModified = TRUE;
	gsetcursor(de, TextCursor);
    }

    if (clength == 0) {
	switch(get_col_type(ccol + colmin - 1)) {
	case NUMERIC:
	    currentexp = 1;
	    break;
	default:
	    currentexp = 2;
	}
	clearrect();
	highlightrect();
    }

    if (currentexp == 1)	/* we are parsing a number */
	switch (c) {
	case '-':
	    if (nneg == 0)
		nneg++;
	    else
		goto donehc;
	    break;
	case '.':
	    if (ndecimal == 0)
		ndecimal++;
	    else
		goto donehc;
	    break;
	case 'e':
	case 'E':
	    if (ne == 0) {
		nneg = ndecimal = 0;	/* might have decimal in exponent */
		ne++;
	    }
	    else
		goto donehc;
	    break;
	default:
	    if (!isdigit((int)text[0]))
		goto donehc;
	    break;
	}

    if (clength++ > 199) {
	warning(G_("dataentry: expression too long"));
	clength--;
	goto donehc;
    }

    *bufp++ = text[0];
    printstring(buf, clength, crow, ccol, 1);
    return;

 donehc:
    bell();
}

static void printlabs(void)
{
    char clab[15], *p;
    int i;

    for (i = colmin; i <= colmax; i++) {
	p = get_col_name(i);
	printstring(p, strlen(p), 0, i - colmin + 1, 0);
    }
    for (i = rowmin; i <= rowmax; i++) {
	sprintf(clab, labform, i);
	printstring(clab, strlen(clab), i - rowmin + 1, 0, 0);
    }
}

              /* ================ GraphApp-specific ================ */

static void bell(void)
{
    gabeep();
}

static void cleararea(int xpos, int ypos, int width, int height, rgb col)
{
    gfillrect(de, col, rect(xpos, ypos, width, height));
}

static void clearwindow(void)
{
    gfillrect(de, p->bg, rect(0, 0, WIDTH, HEIGHT));
}


static void drawrectangle(int xpos, int ypos, int width, int height,
			  int lwd, int fore)
{
    /* only used on screen, so always fast */
    gdrawrect(de, lwd, 0, (fore==1)? p->ufg: p->bg,
	      rect(xpos, ypos, width, height), 1, PS_ENDCAP_SQUARE, 
	      PS_JOIN_BEVEL, 10);
}

static void de_drawtext(int xpos, int ypos, char *text)
{
    gdrawstr(de, p->f, p->fg, pt(xpos, ypos), text);
}

/* Keypress callbacks */

static void de_normalkeyin(control c, int k)
{
    int i, st;
    char text[1];

    st = ggetkeystate();
    if ((p->chbrk) && (k == p->chbrk) &&
	((!p->modbrk) || ((p->modbrk) && (st == p->modbrk)))) {
	p->fbrk(c);
	return;
    }
    if (st & CtrlKey) {
	switch (k + 'A' - 1) {
	case 'B':
	    i = rowmin - nhigh + 2;
	    jumpwin(colmin, max(i, 1));
	    break;
	case 'F':
	    jumpwin(colmin, rowmax);
	    break;
	case 'H':
	    moveback();
	    break;
	case 'I':
	    if (st & ShiftKey) advancerect(LEFT);
	    else advancerect(RIGHT);
	    break;
	case 'N':
	case 'J':
	    advancerect(DOWN);
	    break;
	case 'C':
	    de_copy(de);
	    break;
	case 'V':
	    de_paste(de);
	    break;
	case 'L':
	    for (i = colmin; i < colmax; i++)
		if (i < 100) boxw[i] = get_col_width(i)*FW + 8;
	    drawwindow();
	    break;
	default:
	    bell();
	}
    } else if(k == '\b') {
	moveback();
    } else if(k == '\n' || k == '\r') {
	    advancerect(DOWN);
    } else if(k == '\t') {
	if (st & ShiftKey) advancerect(LEFT);
	else advancerect(RIGHT);
    } else {
	text[0] = k;
	handlechar(text);
    }

}

static void de_ctrlkeyin(control c, int key)
{
    int st, i;

    st = ggetkeystate();
    if ((p->chbrk) && (key == p->chbrk) &&
	((!p->modbrk) || ((p->modbrk) && (st == p->modbrk)))) {
	p->fbrk(c);
	return;
    }
    switch (key) {
    case HOME:
	jumpwin(1, 1);
	downlightrect();
	crow = ccol = 1;
	highlightrect();
	break;
    case END:
	i = ymaxused - nhigh + 2;
	jumpwin(xmaxused, max(i, 1));
	downlightrect();
	crow = ymaxused - rowmin + 1;
	ccol = 1;
	highlightrect();
	break;
    case PGUP:
	i = rowmin - nhigh + 2;
	jumpwin(colmin, max(i, 1));
	break;
    case PGDN:
	jumpwin(colmin, rowmax);
	break;
    case LEFT:
	advancerect(LEFT);
	break;
    case RIGHT:
	advancerect(RIGHT);
	break;
    case UP:
	advancerect(UP);
	break;
    case DOWN:
	advancerect(DOWN);
	break;
    case DEL:
	moveback();
	break;
     case ENTER:
	 advancerect(DOWN);
	 break;
    default:
	;
    }
}

/* mouse callbacks */

static char *get_cell_text(void)
{
    int  wrow = rowmin + crow - 2, wcol = colmin + ccol - 1;
    char *prev = "";
    SEXP tvec;

    if (wcol <= xmaxused) {
	tvec = VECTOR_ELT(work, wcol - 1);
	if (!isNull(tvec) && wrow < INTEGER(lens)[wcol - 1]) {
	    PrintDefaults(R_NilValue);
	    if (TYPEOF(tvec) == REALSXP) {
		if (REAL(tvec)[wrow] != ssNA_REAL)
		    prev = EncodeElement(tvec, wrow, 0, '.');
	    } else if (TYPEOF(tvec) == STRSXP) {
		if (!streql(CHAR(STRING_ELT(tvec, wrow)),
			    CHAR(STRING_ELT(ssNA_STRING, 0))))
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

    if (buttons & LeftButton) {
	xw = xy.x;
	yw = xy.y;

	closerect();

	/* check to see if the click was in the header */

	if (yw < hwidth + bwidth) {
	    /* too high */
	    return;
	}
	/* translate to box coordinates */

	wrow = (yw - bwidth - hwidth) / box_h;

	/* see if it is in the row labels */
	if (xw < bwidth + boxw[0]) {
	    bell();
	    highlightrect();
	    return;
	}
	w = bwidth + boxw[0];
	for (i = 1; i <= nwide; i++)
	    if((w += BOXW(i+colmin-1)) > xw) {
		wcol = i;
		break;
	}

	/* see if we selected a line */
	w = bwidth;
	online = 0;
	for (i = 0; i <= nwide; i++) {
	    if(i == 0) w += boxw[0]; else w += BOXW(i+colmin-1);
	    if (abs(w - xw) <= 2) {
		online = 1;
		clickline = i; /* between cols i and i+1 */
		highlightrect();
		gsetcursor(de, HandCursor);
		return;
	    }
	}

	/* next check to see if it is in the column labels */

	if (yw < hwidth + bwidth + box_h) {
	    if (xw > bwidth + boxw[0]) {
		highlightrect();
		de_popupmenu(xw, yw, wcol);
		return;
	    } else {
		/* in 0th column */
		highlightrect();
		bell();
	    }
	} else if (wrow > nhigh - 1 || wcol > nwide - 1) {
		/* off the grid */
		highlightrect();
		bell();
		return;
	} else if (buttons & DblClick) {
	    int x, y, bw;
	    char *prev;
	    rect rr;

	    ccol = wcol;
	    crow = wrow;
	    highlightrect();
	    find_coords(crow, ccol, &x, &y);
	    bw = BOXW(ccol + colmin - 1);
	    rr = rect(x + text_xoffset, y - text_yoffset - 1,
		      bw - text_xoffset - 2,
		      box_h - text_yoffset - 2);
	    prev = get_cell_text();
	    if (strlen(prev) * FW > bw)
		rr.width = (strlen(prev) + 2) * FW;
	    addto(de);
	    celledit = newfield_no_border(prev, rr);
	    setbackground(celledit, p->bg);
	    setforeground(celledit, p->ufg);
	    settextfont(celledit, p->f);
	    show(celledit);
	    CellEditable = TRUE;
	} else if (buttons & LeftButton) {
	    ccol = wcol;
	    crow = wrow;
	}
	highlightrect();
	return;
    }
}

static void de_mouseup(control c, int buttons, point xy)
{
    int xw, bw, i, w;

    if (online) {
	xw = xy.x;
	w = bwidth + boxw[0];
	for(i = 1; i < clickline; i++) w+= BOXW(i+colmin-1);
	bw = xw - w;
	if (bw < FW*4 + 8) bw = FW*4 + 8;
	if (bw > FW*50) bw = FW*50;
	if(clickline < 100) boxw[clickline] = bw;
	gsetcursor(de, ArrowCursor);
	deredraw();
    }
}

static void de_redraw(control c, rect r)
{
    if (WIDTH != oldWIDTH || HEIGHT != oldHEIGHT) drawwindow();
    else deredraw();
}

static void deredraw(void)
{
    int i;

    setcellwidths();

    if(hwidth > 0) gfillrect(de, bbg, rect(0, 0, WIDTH, hwidth));
    gfillrect(de, bbg, rect(0, 0, boxw[0], windowHeight));

    for (i = 1; i < nhigh; i++)
	drawrectangle(0, hwidth + i * box_h, boxw[0], box_h, 1, 1);
    colmax = colmin + (nwide - 2);
    rowmax = rowmin + (nhigh - 2);
    printlabs();
    /* if (!isNull(work) I don't think it can be null */
    for (i = colmin; i <= colmax; i++) drawcol(i);
    gfillrect(de, p->bg, rect(windowWidth+1, hwidth, WIDTH-windowWidth-1,
			      HEIGHT - hwidth));
    highlightrect();
}

static void de_closewin(void)
{
    closerect();
    hide(de);
    del(de);
}

static void copyarea(int src_x, int src_y, int dest_x, int dest_y)
{
    int mx = max(src_x, dest_x), my = max(src_y, dest_y);
    copyrect(de, pt(dest_x, dest_y),
	     rect(src_x, src_y, windowWidth - mx, windowHeight - my));
}

static void copyH(int src_x, int dest_x, int width)
{
    copyrect(de, pt(dest_x, hwidth),
	     rect(src_x, hwidth, width, windowHeight - hwidth));
}

static Rboolean initwin(void)
{
    int i;
    rect r;

    de = newdataeditor();
    if(!de) return TRUE;
    p = getdata(de);

    nboxchars = asInteger(GetOption(install("de.cellwidth"), R_GlobalEnv));
    if (nboxchars == NA_INTEGER || nboxchars < 0) nboxchars = 0;
    if (nboxchars > 0) check(de_mvw);
    box_w = ((nboxchars >0)?nboxchars:FIELDWIDTH)*FW + 8;
    /* this used to presume 4 chars sufficed for row numbering */
    labdigs = max(3, 1+floor(log10((double)ymaxused)));
    boxw[0] = (1+labdigs)*FW + 8;
    sprintf(labform, "%%%dd", labdigs);
    for(i = 1; i < 100; i++) boxw[i] = get_col_width(i)*FW + 8;
    box_h = FH + 4;
    text_xoffset = 5;
    text_yoffset = -3;
    setcellwidths();
    nhigh = (HEIGHT - 2 * bwidth - hwidth - 3) / box_h;
    windowHeight = nhigh * box_h + 2 * bwidth + hwidth;
    r = getrect(de);
    r.width = windowWidth + 3;
    r.height = windowHeight + 3;
    resize(de, r);

    CellModified = CellEditable = FALSE;
    bbg = dialog_bg();
    /* set the active cell to be the upper left one */
    crow = 1;
    ccol = 1;
    /* drawwindow(); done as repaint but
       decide if we need scrollbars here to avoid flashing*/
    nhigh = (HEIGHT - 2 * bwidth - hwidth) / box_h;
    gchangescrollbar(de, VWINSB, 0, ymaxused/yScrollbarScale,
		     max(nhigh/yScrollbarScale, 1), 0);
    setcellwidths();
    gchangescrollbar(de, HWINSB, 0, xmaxused/xScrollbarScale,
		     max(nwide/xScrollbarScale, 1), 0);
    show(de);
    show(de); /* a precaution, as PD reports transparent windows */
    BringToTop(de, 0);
    R_de_up = TRUE;
    buf[BUFSIZE-1] = '\0';
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
    buf[BUFSIZE-1] = '\0';
    strncpy(buf, GA_gettext(varname), BUFSIZE-1);
    if(!strlen(buf)) {
	askok(G_("column names cannot be blank"));
	return;
    }
    if (popupcol > xmaxused) {
	/* extend work, names and lens */
	REPROTECT(work = lengthgets(work, popupcol), wpi);
	REPROTECT(names = lengthgets(names, popupcol), npi);
	/* Last col name is set later */
	for (i = xmaxused+1; i < popupcol - 1; i++) {
	    sprintf(clab, "var%d", i + 1);
	    SET_STRING_ELT(names, i, mkChar(clab));
	}
	REPROTECT(lens = lengthgets(lens, popupcol), lpi);
	xmaxused = popupcol;
    }
    tvec = VECTOR_ELT(work, popupcol - 1);
    if(ischecked(rb_num) && !isnumeric) {
	if (isNull(tvec))
	    SET_VECTOR_ELT(work, popupcol - 1, ssNewVector(REALSXP, 100));
	else
	    SET_VECTOR_ELT(work, popupcol - 1, coerceVector(tvec, REALSXP));
    } else if(ischecked(rb_char) && isnumeric) {
	if (isNull(tvec))
	    SET_VECTOR_ELT(work, popupcol - 1, ssNewVector(STRSXP, 100));
	else
	    SET_VECTOR_ELT(work, popupcol - 1, coerceVector(tvec, STRSXP));
    }
    SET_STRING_ELT(names, popupcol - 1, mkChar(buf));
    hide(wconf);
    del(wconf);
}

static void nm_hit_key(window w, int key)
{
    if(key == '\n') popupclose(wconf);
}

static void de_popupmenu(int x_pos, int y_pos, int col)
{
    char *blah;
    rect r = screen_coords(de);

    popupcol = colmin + col - 1;
    blah = get_col_name(popupcol);
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
    isnumeric = (get_col_type(popupcol) == NUMERIC);
    if (isnumeric) check(rb_num); else check(rb_char);
    setkeydown(wconf, nm_hit_key);
    show(wconf);
}

static void de_copy(control c)
{
    copystringtoclipboard(get_cell_text());
}

static void de_paste(control c)
{
    char *p;

    closerect();
    if ( clipboardhastext() &&
	 !getstringfromclipboard(buf, BUFSIZE-1) ) {
	/* set current cell to first line of clipboard */
	CellModified = TRUE;
	if ((p = strchr(buf, '\n'))) *p = '\0';
	clength = strlen(buf);
	bufp = buf + clength;
	closerect();
    }
    highlightrect();
}

static void de_delete(control c)
{
    CellModified = TRUE;
    buf[0] = '\0';
    clength = -1;
    bufp = buf + clength;
    closerect();
    highlightrect();
}

static void de_autosize(control c)
{
    int col = ccol + colmin - 1;

    closerect();
    if(col < 100) {
	boxw[col] = get_col_width(col)*FW + 8;
	deredraw();
    }
}

static void de_stayontop(control c)
{
    BringToTop(de, 2);
}

static void de_sbf(control c, int pos)
{
    if (pos < 0) { /* horizontal */
	colmin = 1 + (-pos*xScrollbarScale - 1);
    } else {
	rowmin = 1 + pos*yScrollbarScale;
	if(rowmin > ymaxused - nhigh + 2) rowmin = max(1,ymaxused - nhigh + 2);
    }
    drawwindow();
}

static checkbox varwidths;

static void vw_close(control c)
{
    int x;
    if (ischecked(varwidths)) x = 0;
    else x = atoi(GA_gettext(varname)); /* 0 if error */
    x = min(x, 50);
    if (x != nboxchars) {
	nboxchars = x;
	box_w = ((nboxchars >0)?nboxchars:FIELDWIDTH)*FW + 8;
	deredraw();
    }
    hide(devw);
    del(devw);
    if (nboxchars > 0) check(de_mvw);
    else uncheck(de_mvw);
    addto(de);
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


static void de_popup_vw(void)
{
    char blah[25];

    devw = newwindow(G_("Cell width(s)"),
		      rect(0, 0, 250, 60),
		      Titlebar | Centered | Closebox | Modal);
    setclose(devw, vw_close);
    setbackground(devw, bbg);
    lwhat = newlabel(G_("Cell width"), rect(10, 20, 70, 20), AlignLeft);
    sprintf(blah, "%d", nboxchars);
    varname = newfield(blah, rect(80, 20, 40, 20));
    varwidths = newcheckbox(G_("variable"), rect(150, 20, 80, 20), vw_callback);
    if (nboxchars == 0) {
	check(varwidths);
	disable(varname);
    }
    setkeydown(devw, vw_hit_key);
    show(devw);
}

static void menudecellwidth(control m)
{
    de_popup_vw();
}

static void deldataeditor(control m)
{
    freeConsoleData(getdata(m));
}

static void declose(control m)
{
    de_closewin();
    show(RConsole);
    R_de_up = FALSE;
}

static void deresize(console c, rect r)
{
    if (((WIDTH  == r.width) &&
	 (HEIGHT == r.height)) ||
	(r.width == 0) || (r.height == 0) ) /* minimize */
        return;;
    WIDTH = r.width;
    HEIGHT = r.height;
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
    /* use this to customize the menu */
    if (ismdi())
    	disable(DePopup[6].m);
    else {
    	if (isTopmost(de))
    	    check(DePopup[6].m);
    	else
    	    uncheck(DePopup[6].m);
    }
}


#define MCHECK(a) if (!(a)) {del(c);return NULL;}

RECT *RgetMDIsize(); /* in rui.c */

static dataeditor newdataeditor(void)
{
    ConsoleData p;
    int w, h, x, y;
    dataeditor c;
    menuitem m;

    p = newconsoledata((consolefn) ? consolefn : FixedFont,
		       pagerrow, pagercol, 0, 0,
		       consolefg, consoleuser, consolebg,
		       DATAEDITOR, 0);
    if (!p) return NULL;

    w = WIDTH ;
    h = HEIGHT;
    if (ismdi()) {
	RECT *pR = RgetMDIsize();
	x = (pR->right - w) / 3; x = x > 20 ? x:20;
	y = (pR->bottom - h) / 3; y = y > 20 ? y:20;
    } else {
	x = (devicewidth(NULL) - w) / 3;
	y = (deviceheight(NULL) - h) / 3 ;
    }
    c = (dataeditor) newwindow(G_("Data Editor"), rect(x, y, w, h),
			       Document | StandardWindow | Menubar |
			       VScrollbar | HScrollbar | TrackMouse);
    if (!c) {
         freeConsoleData(p);
         return NULL;
    }
    setdata(c, p);
    if(h == 0) HEIGHT = getheight(c);
    if(w == 0) WIDTH  = getwidth(c);
    COLS = WIDTH / FW - 1;
    ROWS = HEIGHT / FH - 1;
    BORDERX = (WIDTH - COLS*FW) / 2;
    BORDERY = (HEIGHT - ROWS*FH) / 2;
    gsetcursor(c, ArrowCursor);
    setbackground(c, consolebg);
    if (ismdi() && (RguiMDI & RW_TOOLBAR)) {
	/* blank toolbar to stop windows jumping around */
        int btsize = 24;
        control tb;
        addto(c);
        MCHECK(tb = newtoolbar(btsize + 4));
	gsetcursor(tb, ArrowCursor);
    }
    MCHECK(gpopup(depopupact, DePopup));
    MCHECK(m = newmenubar(demenuact));
    MCHECK(newmenu(G_("File")));
/*    MCHECK(m = newmenuitem("-", 0, NULL));*/
    MCHECK(m = newmenuitem(G_("Close"), 0, declose));
    newmdimenu();
    MCHECK(newmenu(G_("Edit")));
    MCHECK(m = newmenuitem(G_("Copy  \tCTRL+C"), 0, de_copy));
    MCHECK(m = newmenuitem(G_("Paste \tCTRL+V"), 0, de_paste));
    MCHECK(m = newmenuitem(G_("Delete\tDEL"), 0, de_delete));
    MCHECK(m = newmenuitem("-", 0, NULL));
    MCHECK(de_mvw = newmenuitem(G_("Cell widths ..."), 0, menudecellwidth));
    MCHECK(m = newmenu(G_("Help")));
    MCHECK(newmenuitem(G_("Data editor"), 0, menudehelp));

    setdata(c, p);
    setresize(c, deresize);
    setredraw(c, de_redraw);
    setdel(c, deldataeditor);
    setclose(c, declose);
    sethit(c, de_sbf);
    setkeyaction(c, de_ctrlkeyin);
    setkeydown(c, de_normalkeyin);
    setmousedown(c, de_mousedown);
    setmouseup(c, de_mouseup);
    return(c);
}
