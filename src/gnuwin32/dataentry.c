/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2000  Robert Gentleman, Ross Ihaka and the
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* TODO
   - spreadsheet copy and paste?
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "Print.h"

#include "graphapp/ga.h"
#include "console.h"
#include "consolestructs.h"
#include "rui.h"

static dataeditor de;
static ConsoleData p;

static int R_de_up;

#ifndef max
#define max(a, b) (((a)>(b))?(a):(b))
#endif
#ifndef min
#define min(a, b) (((a)<(b))?(a):(b))
#endif
#define BOXW(x) (min(((x<100 && nboxchars==0)?boxw[x]:box_w),WIDTH-boxw[0]-2*bwidth-2))

#define FIELDWIDTH 10

/* Local Function Definitions */

static void advancerect(int);
static void bell();
static void cleararea(int, int, int, int, rgb);
static void clearrect();
static void closerect();
static void clearwindow();
static void de_closewin();
static void copyarea(int, int, int, int);
static void copyH(int, int, int);
static void deredraw();
static void eventloop();
static void downlightrect();
static void drawwindow();
static void drawcol(int);
/* static void de_drawline(int, int, int, int);*/
static void de_drawtext(int, int, char *);
static void drawrectangle(int, int, int, int, int, int);
static void drawrow(int);
static void find_coords(int, int, int*, int*);
static void handlechar(char*);
static void highlightrect();
static int  initwin();
static void jumppage(int);
static void jumpwin(int, int);
static void de_popupmenu(int, int, int);
static void printlabs();
static void printrect(int, int);
static void printstring(char*, int, int, int, int);
static void printelt(SEXP, int, int, int);
static void setcellwidths();

static dataeditor newdataeditor();
static void de_copy(control c);
static void de_paste(control c);
static void de_delete(control c);
static menuitem de_mvw;

static SEXP inputlist;  /* each element is a vector for that row */
static SEXP ssNA_STRING;
static double ssNA_REAL;


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
    SETLEVELS(tvec, 0);
    return (tvec);
}
/* Global variables needed for the graphics */

static int box_w;                       /* width of a box */
static int boxw[100];                   /* widthes of cells */
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
static char buf[30];
static char *bufp;
static int bwidth;			/* width of the border */
static int hwidth;			/* width of header  */
static int text_xoffset, text_yoffset;
static int CellModified;
static int CellEditable;
static field celledit;
static int newcol;
static int xmaxused, ymaxused;
static int oldWIDTH=0, oldHEIGHT=0;
static int nboxchars=0;


void R_ProcessEvents(); /* in system.c */

static void eventloop()
{
    while (R_de_up) R_ProcessEvents();
}

SEXP do_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP tvec2, tvec, colmodes, indata;
    SEXPTYPE type;
    int i, j,len, nprotect, tmp;
    RCNTXT cntxt;

    nprotect = 0;/* count the PROTECT()s */
    PROTECT(indata = VectorToPairList(CAR(args))); nprotect++;
    PROTECT(colmodes = VectorToPairList(CADR(args))); nprotect++;

    if (!isList(indata) || !isList(colmodes))
	errorcall(call, "invalid argument");

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

    /* setup inputlist  */

    if (indata != R_NilValue) {
	xmaxused = 0; ymaxused = 0;
	PROTECT(inputlist = duplicate(indata)); nprotect++;
	for (tvec = inputlist, tvec2 = colmodes;
	     tvec != R_NilValue;
	     tvec = CDR(tvec), tvec2 = CDR(tvec2)) {
	    type = TYPEOF(CAR(tvec)); xmaxused++;
	    if (CAR(tvec2) != R_NilValue)
		type = str2type(CHAR(STRING_ELT(CAR(tvec2), 0)));
	    if (type != STRSXP)
		type = REALSXP;
	    if (CAR(tvec) == R_NilValue) {
		if (type == NILSXP)
		    type = REALSXP;
		SETCAR(tvec, ssNewVector(type, 100));
		SET_TAG(tvec, install("var1"));
		SETLEVELS(CAR(tvec), 0);
	    }
	    else if (!isVector(CAR(tvec)))
		errorcall(call, "invalid type for value");
	    else {
		if (TYPEOF(CAR(tvec)) != type)
		    SETCAR(tvec, coerceVector(CAR(tvec), type));
		tmp = SETLEVELS(CAR(tvec), LENGTH(CAR(tvec)));
		ymaxused = max(tmp, ymaxused);
	    }
	}
    }
    else if (colmodes == R_NilValue ) {
	PROTECT(inputlist = allocList(1)); nprotect++;
	SETCAR(inputlist, ssNewVector(REALSXP, 100));
	SET_TAG(inputlist, install("var1"));
	SETLEVELS(CAR(inputlist), 0);
    }
    else {
	errorcall(call, "invalid parameter(s) ");
    }


    /* start up the window, more initializing in here */
    if (initwin())
	errorcall(call, "invalid device");

    /* set up a context which will close the window if there is an error */
    begincontext(&cntxt, 8, R_NilValue, R_NilValue, R_NilValue, R_NilValue);
    cntxt.cend = &de_closewin;

    highlightrect();

    eventloop();

    endcontext(&cntxt);

    /* drop out unused columns */
    i = 0;
    for (tvec = inputlist; tvec != R_NilValue; tvec = CDR(tvec))
	if (CAR(tvec) == R_NilValue) {
	    if (i == 0)
		inputlist = CDR(inputlist);
	    else {
		tvec2 = nthcdr(inputlist, (i - 1));
		SETCDR(tvec2, CDR(tvec));
	    }
	}
	else
	    i++;

    for (tvec = inputlist; tvec != R_NilValue; tvec = CDR(tvec)) {
	len = LEVELS(CAR(tvec));
	if (LENGTH(CAR(tvec)) != len) {
	    tvec2 = ssNewVector(TYPEOF(CAR(tvec)), len);
	    PROTECT(tvec);
	    for (j = 0; j < len; j++)
		if (TYPEOF(CAR(tvec)) == REALSXP) {
		    if (REAL(CAR(tvec))[j] != ssNA_REAL)
			REAL(tvec2)[j] = REAL(CAR(tvec))[j];
		    else
			REAL(tvec2)[j] = NA_REAL;
		} else if (TYPEOF(CAR(tvec)) == STRSXP) {
		    if (!streql(CHAR(STRING_ELT(CAR(tvec), j)),
				CHAR(STRING_ELT(ssNA_STRING, 0))))
			SET_STRING_ELT(tvec2, j, STRING_ELT(CAR(tvec), j));
		    else
			SET_STRING_ELT(tvec2, j, NA_STRING);
		} else
		    error("dataentry: internal memory problem");
	    SETCAR(tvec, tvec2);
	    UNPROTECT(1);
	}
    }

    UNPROTECT(nprotect);
    return PairToVectorList(inputlist);
}

/* Window Drawing Routines */

static rgb bbg;

static void setcellwidths()
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


static void drawwindow()
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
    gchangescrollbar(de, VWINSB, rowmin-1, ymaxused, nhigh, 0);
    gchangescrollbar(de, HWINSB, colmin-1, xmaxused, nwide, 0);
}

static void doHscroll(int oldcol)
{
    int i, dw;
    int oldnwide = nwide, oldwindowWidth = windowWidth;

    /* horizontal re-position */
    setcellwidths();
    colmax = colmin + (nwide - 2);
    if (oldcol < colmin) { /* drop oldcol...colmin-1 */
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
    gchangescrollbar(de, HWINSB, colmin-1, xmaxused, nwide, 0);
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
    SEXP tmp;
    static char clab[15];
    if (col <= length(inputlist)) {
	tmp = nthcdr(inputlist, col - 1);
	if (TAG(tmp) != R_NilValue)
	    return CHAR(PRINTNAME(TAG(tmp)));
    }
    sprintf(clab, "var%d", col);
    return clab;
}

static int get_col_width(int col)
{
    int i, w = 0, w1, fw = FIELDWIDTH;
    char *strp;
    SEXP tmp;

    if (nboxchars > 0) return nboxchars;
    if (col <= length(inputlist)) {
	tmp = nthcdr(inputlist, col - 1);
	if (tmp == R_NilValue) return fw;
	PrintDefaults(R_NilValue);
	if (TAG(tmp) != R_NilValue)
	    w = strlen(CHAR(PRINTNAME(TAG(tmp))));
	else w = fw;
	tmp = CAR(tmp);
	PrintDefaults(R_NilValue);
	for (i = 0; i < (int)LEVELS(tmp); i++) {
	    strp = EncodeElement(tmp, i, 0);
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

typedef enum {UNKNOWNN, NUMERIC, CHARACTER} CellType;

static CellType get_col_type(int col)
{
    SEXP tmp;
    CellType res = UNKNOWNN;

    if (col <= length(inputlist)) {
	tmp = CAR(nthcdr(inputlist, col - 1));
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

   if (length(inputlist) >= whichcol) {
	tmp = nthcdr(inputlist, whichcol - 1);
	if (CAR(tmp) != R_NilValue) {
	    len = min(rowmax, LEVELS(CAR(tmp)) );
	    for (i = (rowmin - 1); i < len; i++)
		printelt(CAR(tmp), i, i - rowmin + 2, col);
	}
    }
}


/* whichrow is absolute row no */
static void drawrow(int whichrow)
{
    int i, src_x, src_y, lenip, row = whichrow - rowmin + 1, w;
    char rlab[15];
    SEXP tvec;

    find_coords(row, 0, &src_x, &src_y);
    cleararea(src_x, src_y, windowWidth, box_h, (whichrow > 0)?p->bg:bbg);
    drawrectangle(src_x, src_y, boxw[0], box_h, 1, 1);

    sprintf(rlab, "%4d", whichrow);
    printstring(rlab, strlen(rlab), row, 0, 0);

    w = bwidth + boxw[0];
    for (i = colmin; i <= colmax; i++) {
	drawrectangle(w, src_y, BOXW(i), box_h, 1, 1);
	w += BOXW(i);
    }

    lenip = length(inputlist);
    for (i = colmin; i <= colmax; i++) {
	if (i > lenip) break;
	tvec = CAR(nthcdr(inputlist, i - 1));
	if (tvec != R_NilValue)
	    if (whichrow <= (int)LEVELS(tvec))
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
	    strp = EncodeElement(invec, vrow, 0);
	    printstring(strp, strlen(strp), ssrow, sscol, 0);
	}
    }
    else if (TYPEOF(invec) == STRSXP) {
	if (!streql(CHAR(STRING_ELT(invec, vrow)),
		    CHAR(STRING_ELT(ssNA_STRING, 0)))) {
	    strp = EncodeElement(invec, vrow, 0);
	    printstring(strp, strlen(strp), ssrow, sscol, 0);
	}
    }
    else
	error("dataentry: internal memory error");
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
	if (length(inputlist) >= whichcol + colmin - 1) {
	    tmp = nthcdr(inputlist, whichcol + colmin - 2);
	    if (CAR(tmp) != R_NilValue &&
		(i = rowmin + whichrow - 2) < (int)LEVELS(CAR(tmp)) )
		printelt(CAR(tmp), i, whichrow, whichcol);
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
	gchangescrollbar(de, VWINSB, rowmin-1, ymaxused, nhigh, 0);
	break;
    case DOWN:
	rowmin++;
	rowmax++;
	copyarea(0, hwidth + 2 * box_h, 0, hwidth + box_h);
	drawrow(rowmax);
	gchangescrollbar(de, VWINSB, rowmin-1, ymaxused, nhigh, 0);
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
    drawrectangle(x + lwd - 1, y + lwd -1,
		  BOXW(ccol+colmin-1) - lwd + 1,
		  box_h - lwd + 1, lwd, fore);
}

static void downlightrect()
{
    printrect(2, 0);
    printrect(1, 1);
}

static void highlightrect()
{
    printrect(2, 1);
}


static SEXP getccol()
{
    SEXP tmp, tmp2;
    int i, len, newlen, wcol, wrow;
    SEXPTYPE type;
    char cname[10];

    wcol = ccol + colmin - 1;
    wrow = crow + rowmin - 1;
    if (length(inputlist) < wcol)
	inputlist = listAppend(inputlist,
			       allocList(wcol - length(inputlist)));
    tmp = nthcdr(inputlist, wcol - 1);
    newcol = 0;
    if (CAR(tmp) == R_NilValue) {
	newcol = 1;
	xmaxused = wcol;
	len = max(100, wrow);
	SETCAR(tmp, ssNewVector(REALSXP, len));
	if (TAG(tmp) == R_NilValue) {
	    sprintf(cname, "var%d", wcol);
	    SET_TAG(tmp, install(cname));
	}
    }
    if (!isVector(CAR(tmp)))
	error("internal type error in dataentry");
    len = LENGTH(CAR(tmp));
    type = TYPEOF(CAR(tmp));
    if (len < wrow) {
	for (newlen = max(len * 2, 10) ; newlen < wrow ; newlen *= 2)
	    ;
	tmp2 = ssNewVector(type, newlen);
	for (i = 0; i < len; i++)
	    if (type == REALSXP)
		REAL(tmp2)[i] = REAL(CAR(tmp))[i];
	    else if (type == STRSXP)
		SET_STRING_ELT(tmp2, i, STRING_ELT(CAR(tmp), i));
	    else
		error("internal type error in dataentry");
	SETLEVELS(tmp2, LEVELS(CAR(tmp)));
	SETCAR(tmp, tmp2);
    }
    return (tmp);
}

/* close up the entry to a cell, put the value that has been entered
   into the correct place and as the correct type */

extern double R_strtod(char *c, char **end); /* in coerce.c */

static void closerect()
{
    SEXP cvec, c0vec, tvec;
    int wcol = ccol + colmin - 1, wrow = rowmin + crow - 1, wrow0;

    *bufp = '\0';

    if (CellModified || CellEditable) {
	if (CellEditable) {
	    strcpy(buf, gettext(celledit));
	    clength = strlen(buf);
	    hide(celledit);
	    del(celledit);
	}
	c0vec = getccol();
	cvec = CAR(c0vec);
	wrow0 = (int)LEVELS(cvec);
	if (wrow > wrow0) SETLEVELS(cvec, wrow);
	ymaxused = max(ymaxused, wrow);
	if (clength != 0) {
	    /* do it this way to ensure NA, Inf, ...  can get set */
	    char *endp;
	    double new = R_strtod(buf, &endp);
	    int warn = !isBlankString(endp);
	    if (TYPEOF(cvec) == STRSXP) {
		tvec = allocString(strlen(buf));
		strcpy(CHAR(tvec), buf);
		SET_STRING_ELT(cvec, wrow - 1, tvec);
	    } else
		REAL(cvec)[wrow - 1] = new;
	    if (newcol & warn) {
		/* change mode to character */
		int levs = LEVELS(cvec);
		cvec = SETCAR(c0vec, coerceVector(cvec, STRSXP));
		SETLEVELS(cvec, levs);
		tvec = allocString(strlen(buf));
		strcpy(CHAR(tvec), buf);
		SET_STRING_ELT(cvec, wrow - 1, tvec);
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
    CellEditable = CellModified = 0;

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

static void printstring(char *ibuf, int buflen, int row, int col, int left)
{
    int x_pos, y_pos, bw, fw, bufw;
    char buf[201];

    find_coords(row, col, &x_pos, &y_pos);
    if (col == 0) bw = boxw[0]; else bw = BOXW(col+colmin-1);
    cleararea(x_pos + 1, y_pos + 1, bw - 1, box_h - 1,
	      (row==0 || col==0) ? bbg:p->bg);
    fw = min(200, (bw - 8)/FW);
    bufw = min(fw, buflen);
    strncpy(buf, ibuf, bufw);
    buf[bufw] = '\0';
    if (buflen > fw) {
	if(left) {
	    strncpy(buf, ibuf + buflen - fw, fw);
	    buf[fw + 1] = '\0';
	    *buf = '<';
	} else {
	    *(buf + fw - 1) = '>';
	    *(buf + fw) = '\0';
	}
    }
    de_drawtext(x_pos + text_xoffset, y_pos - text_yoffset, buf);
}

static void clearrect()
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
	CellModified = 0;
	clength = 0;
	drawelt(crow, ccol);
	gsetcursor(de, ArrowCursor);
	return;
    } else {
	CellModified = 1;
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

    if (clength++ > 29) {
	warning("dataentry: expression too long");
	clength--;
	goto donehc;
    }

    *bufp++ = text[0];
    printstring(buf, clength, crow, ccol, 1);
    return;

 donehc:
    bell();
}

static void printlabs()
{
    char clab[10], *p;
    int i;

    for (i = colmin; i <= colmax; i++) {
	p = get_col_name(i);
	printstring(p, strlen(p), 0, i - colmin + 1, 0);
    }
    for (i = rowmin; i <= rowmax; i++) {
	sprintf(clab, "%4d", i);
	printstring(clab, strlen(clab), i - rowmin + 1, 0, 0);
    }
}

              /* ================ GraphApp-specific ================ */

static void bell()
{
    gabeep();
}

static void cleararea(int xpos, int ypos, int width, int height, rgb col)
{
    gfillrect(de, col, rect(xpos, ypos, width, height));
}

static void clearwindow()
{
    gfillrect(de, p->bg, rect(0, 0, WIDTH, HEIGHT));
}

#if 0
static void de_drawline(int fromx, int fromy, int tox, int toy)
{
    gdrawline(de, 1, 0, p->ufg, pt(fromx, fromy), pt(tox, toy));
}
#endif

static void drawrectangle(int xpos, int ypos, int width, int height,
			  int lwd, int fore)
{
    gdrawrect(de, lwd, 0, (fore==1)? p->ufg: p->bg,
	      rect(xpos, ypos, width, height));
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
	    if (clength > 0) {
		clength--;
		bufp--;
		printstring(buf, clength, crow, ccol, 1);
	    } else bell();
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
	    if (clength > 0) {
		clength--;
		bufp--;
		printstring(buf, clength, crow, ccol, 1);
	    } else bell();
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
	if (clength > 0) {
	    clength--;
	    bufp--;
	    printstring(buf, clength, crow, ccol, 1);
	} else de_delete(de);
	break;
     case ENTER:
	 advancerect(DOWN);
	 break;
    default:
	;
    }
}

/* mouse callbacks */

static char *get_cell_text()
{
    int  wrow = rowmin + crow - 2, wcol = colmin + ccol - 1;
    char *prev = "";
    SEXP tvec;

    if (wcol <= length(inputlist)) {
	tvec = CAR(nthcdr(inputlist, wcol - 1));
	if (tvec != R_NilValue && wrow < (int)LEVELS(tvec)) {
	    PrintDefaults(R_NilValue);
	    if (TYPEOF(tvec) == REALSXP) {
		if (REAL(tvec)[wrow] != ssNA_REAL)
		    prev = EncodeElement(tvec, wrow, 0);
	    } else if (TYPEOF(tvec) == STRSXP) {
		if (!streql(CHAR(STRING_ELT(tvec, wrow)),
			    CHAR(STRING_ELT(ssNA_STRING, 0))))
		    prev = EncodeElement(tvec, wrow, 0);
	    } else error("dataentry: internal memory error");
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
	} else if (wrow > nhigh - 1 || wcol > nwide -1) {
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
	    CellEditable = 1;
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

static void deredraw()
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
    if (inputlist != R_NilValue)
	for (i = colmin; i <= colmax; i++) drawcol(i);
    gfillrect(de, p->bg, rect(windowWidth+1, hwidth, WIDTH-windowWidth-1, 
			      HEIGHT - hwidth));
    highlightrect();
}

static void de_closewin()
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

static int  initwin()
{
    int i;
    rect r;

    de = newdataeditor();
    if(!de) return 1;
    p = getdata(de);

    nboxchars = asInteger(GetOption(install("de.cellwidth"), R_GlobalEnv));
    if (nboxchars == NA_INTEGER || nboxchars < 0) nboxchars = 0;
    if (nboxchars > 0) check(de_mvw);
    box_w = ((nboxchars >0)?nboxchars:FIELDWIDTH)*FW + 8;
    boxw[0] = 5*FW + 8;
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

    CellModified = CellEditable = 0;
    bbg = dialog_bg();
    /* set the active cell to be the upper left one */
    crow = 1;
    ccol = 1;
    /* drawwindow(); done as repaint but
       decide if we need scrollbars here to avoid flashing*/
    nhigh = (HEIGHT - 2 * bwidth - hwidth) / box_h;
    gchangescrollbar(de, VWINSB, 0, ymaxused, nhigh, 0);
    setcellwidths();
    gchangescrollbar(de, HWINSB, 0, xmaxused, nwide, 0);    
    show(de);
    show(de); /* a precaution, as PD reports transparent windows */
    BringToTop(de);
    R_de_up = 1;
    return 0;
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
    int levs;
    char buf[30];

    strcpy(buf, gettext(varname));
    if(!strlen(buf)) {
	askok("column names cannot be blank");
	return;
    }
    if (length(inputlist) < popupcol) {
	inputlist =
	    listAppend(inputlist,
		       allocList((popupcol - length(inputlist))));
    }
    tvec = nthcdr(inputlist, popupcol - 1);
    if(ischecked(rb_num) && !isnumeric) {
	if (CAR(tvec) == R_NilValue) SETCAR(tvec, ssNewVector(REALSXP, 100));
	levs = LEVELS(CAR(tvec));
	SETCAR(tvec, coerceVector(CAR(tvec), REALSXP));
	SETLEVELS(CAR(tvec), levs);

    } else if(ischecked(rb_char) && isnumeric) {
	if (CAR(tvec) == R_NilValue) SETCAR(tvec, ssNewVector(STRSXP, 100));
	levs = LEVELS(CAR(tvec));
	SETCAR(tvec, coerceVector(CAR(tvec), STRSXP));
	SETLEVELS(CAR(tvec), levs);
    }
    SET_TAG(tvec, install(buf));
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
    wconf = newwindow("Variable editor",
		      rect(x_pos + r.x-150, y_pos + r.y-50, 300, 100),
		      Titlebar | Closebox | Modal);
    setclose(wconf, popupclose);
    setbackground(wconf, bbg);
    lwhat = newlabel("variable name", rect(10, 22, 90, 20), AlignLeft);
    varname = newfield(blah, rect(100, 20, 120, 20));
    lrb = newlabel("type", rect(50, 62, 50, 20), AlignLeft);
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
	 !getstringfromclipboard(buf, 29) ) {
	/* set current cell to first line of clipboard */
	CellModified = 1;
	if ((p = strchr(buf, '\n'))) *p = '\0';
	clength = strlen(buf);
	bufp = buf + clength;
	closerect();
    }
    highlightrect();
}

static void de_delete(control c)
{
    CellModified = 1;
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

static void de_sbf(control c, int pos)
{
    if (pos < 0) { /* horizontal */
	colmin = 1 + (-pos - 1);
    } else {
	rowmin = 1 + pos;
	if(rowmin > ymaxused - nhigh + 2) rowmin = ymaxused - nhigh + 2;
    }
    drawwindow();
}

static checkbox varwidths;

static void vw_close(control c)
{
    int x;
    if (ischecked(varwidths)) x = 0;
    else x = atoi(gettext(varname)); /* 0 if error */
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


static void de_popup_vw()
{
    char blah[25];

    devw = newwindow("Cell width(s)",
		      rect(0, 0, 250, 60),
		      Titlebar | Centered | Closebox | Modal);
    setclose(devw, vw_close);
    setbackground(devw, bbg);
    lwhat = newlabel("Cell width", rect(10, 20, 70, 20), AlignLeft);
    sprintf(blah, "%d", nboxchars);
    varname = newfield(blah, rect(80, 20, 40, 20));
    varwidths = newcheckbox("variable", rect(150, 20, 80, 20), vw_callback);
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
    R_de_up =0;
}

static void deresize(console c, rect r)
FBEGIN
    if (((WIDTH  == r.width) &&
	 (HEIGHT == r.height)) ||
	(r.width == 0) || (r.height == 0) ) /* minimize */
        FVOIDRETURN;
    WIDTH = r.width;
    HEIGHT = r.height;
FVOIDEND

static void menudehelp(control m)
{
    char s[] = "Navigation.\n  Keyboard: cursor keys move selection\n\tTab move right, Shift+Tab moves left\n\tPgDn or Ctrl+F: move down one screenful\n\tPgUp or Ctrl+B: move up one screenful\n\tHome: move to (1,1) cell\n\tEnd: show last rows of last column.\n   Mouse: left-click in a cell, use the scrollbar(s).\n\nEditing.\n  Type in the currently hightlighted cell\n  Double-click in a cell for an editable field\n\nMisc.\n  Ctrl-L redraws the screen, auto-resizing the columns\n  Ctrl-C copies selected cell\n  Ctrl-V pastes to selected cell\n  Right-click menu for copy, paste, autosize currently selected column\n\n";
    askok(s);
}


static MenuItem DePopup[28] = {
    {"Help", menudehelp, 0},
    {"-", 0, 0},
    {"Copy selected cell", de_copy, 0},
    {"Paste to selected cell", de_paste, 0},
    {"Autosize column", de_autosize, 0},
    {"-", 0, 0},
    {"Close", declose, 0},
    LASTMENUITEM
};

static void demenuact(control m)
{
    /* use this to customize the menu */
}

static void depopupact(control m)
{
    /* use this to customize the menu */
}


#define MCHECK(a) if (!(a)) {del(c);return NULL;}

static dataeditor newdataeditor()
{
    ConsoleData p;
    int w, h, x, y;
    dataeditor c;
    menuitem m;
    
    p = newconsoledata((consolefn) ? consolefn : FixedFont,
		       pagerrow, pagercol, 0, 0,
		       consolefg, consoleuser, consolebg,
		       DATAEDITOR);
    if (!p) return NULL;

    w = WIDTH ;
    h = HEIGHT;
#ifdef USE_MDI
    if (ismdi()) {
	RECT *pR = RgetMDIsize();
	x = (pR->right - w) / 3; x = x > 20 ? x:20;
	y = (pR->bottom - h) / 3; y = y > 20 ? y:20;
    } else {
#endif
	x = (devicewidth(NULL) - w) / 3;
	y = (deviceheight(NULL) - h) / 3 ;
#ifdef USE_MDI
    }
#endif
    c = (dataeditor) newwindow(" Data Editor", rect(x, y, w, h),
			       Document | StandardWindow | TrackMouse |
			       VScrollbar | HScrollbar);
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
#ifdef USE_MDI
    if (ismdi() && (RguiMDI & RW_TOOLBAR)) {
	/* blank toolbar to stop windows jumping around */
        int btsize = 24;
        control tb;
        addto(c);
        MCHECK(tb = newtoolbar(btsize + 4));
	gsetcursor(tb, ArrowCursor);
    }
#endif
    MCHECK(gpopup(depopupact, DePopup));
    MCHECK(m = newmenubar(demenuact));
    MCHECK(newmenu("File"));
/*    MCHECK(m = newmenuitem("-", 0, NULL));*/
    MCHECK(m = newmenuitem("Close", 0, declose));
#ifdef USE_MDI
    newmdimenu();
#endif
    MCHECK(newmenu("Edit"));
    MCHECK(m = newmenuitem("Copy  \tCTRL+C", 0, de_copy));
    MCHECK(m = newmenuitem("Paste \tCTRL+V", 0, de_paste));
    MCHECK(m = newmenuitem("Delete\tDEL", 0, de_delete));
    MCHECK(m = newmenuitem("-", 0, NULL));
    MCHECK(de_mvw = newmenuitem("Cell widths ...", 0, menudecellwidth));
    MCHECK(m = newmenu("Help"));
    MCHECK(newmenuitem("Data editor", 0, menudehelp));
 
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
