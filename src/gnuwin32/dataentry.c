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
   - is END useful here?
   - ctrl-home ctrl-end etc?
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "Print.h"

SEXP old_do_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("no data entry editor in this version of R");
    return R_NilValue;
}

#include "graphapp/ga.h"
#include "console.h"
#include "consolestructs.h"

static dataeditor de;
static ConsoleData p;

extern int R_de_up;

#define FIELDWIDTH 10

/* Local Function Definitions */
 
static void advancerect(int);
static void bell();
static void cleararea(int, int, int, int, rgb);
static void clearrect();
static void closerect();
static void clearwindow();
void de_closewin();
static void copyarea(int src_x, int src_y, int dest_x, int dest_y);
static void eventloop();
static void downlightrect();
static void drawwindow();
static void drawcol(int);
static void de_drawline(int, int, int, int);
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


static SEXP inputlist;  /* each element is a vector for that row */
static SEXP ssNA_STRING;
static double ssNA_REAL;


SEXP ssNewVector(SEXPTYPE type, int vlen)
{
    SEXP tvec;
    int j;

    tvec = allocVector(type, vlen);
    for (j = 0; j < vlen; j++)
	if (type == REALSXP)
	    REAL(tvec)[j] = ssNA_REAL;
	else if (type == STRSXP)
	    STRING(tvec)[j] = STRING(ssNA_STRING)[0];
    LEVELS(tvec) = 0;
    return (tvec);
}
/* Global variables needed for the graphics */
 
static int box_w;                       /* width of a box */
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



void R_ProcessEvents(); /* in system.c */

static void eventloop()
{
    while (R_de_up) R_ProcessEvents();
}

SEXP do_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP tvec2, tvec, colmodes, indata;
    SEXPTYPE type;
    int i, j,len, nprotect;
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
    hwidth = 0;

    /* setup inputlist  */

    if (indata != R_NilValue) {
	PROTECT(inputlist = duplicate(indata)); nprotect++;
	for (tvec = inputlist, tvec2 = colmodes; 
	     tvec != R_NilValue; 
	     tvec = CDR(tvec), tvec2 = CDR(tvec2)) {
	    type = TYPEOF(CAR(tvec));
	    if (CAR(tvec2) != R_NilValue)
		type = str2type(CHAR(STRING(CAR(tvec2))[0]));
	    if (type != STRSXP)
		type = REALSXP;
	    if (CAR(tvec) == R_NilValue) {
		if (type == NILSXP)
		    type = REALSXP;
		CAR(tvec) = ssNewVector(type, 100);
		TAG(tvec) = install("var1");
		LEVELS(CAR(tvec)) = 0;
	    }
	    else if (!isVector(CAR(tvec)))
		errorcall(call, "invalid type for value");
	    else {
		if (TYPEOF(CAR(tvec)) != type)
		    CAR(tvec) = coerceVector(CAR(tvec), type);
		LEVELS(CAR(tvec)) = LENGTH(CAR(tvec));
	    }
	}
    }
    else if (colmodes == R_NilValue ) {
	PROTECT(inputlist = allocList(1)); nprotect++;
	CAR(inputlist) = ssNewVector(REALSXP, 100);
	TAG(inputlist) = install("var1");
	LEVELS(CAR(inputlist)) = 0;
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
		    if (!streql(CHAR(STRING(CAR(tvec))[j]), 
				CHAR(STRING(ssNA_STRING)[0])))
			STRING(tvec2)[j] = STRING(CAR(tvec))[j];
		    else
			STRING(tvec2)[j] = NA_STRING;
		} else
		    error("spreadsheet: internal memory problem");
	    CAR(tvec) = tvec2;
	    UNPROTECT(1);
	}
    }

    UNPROTECT(nprotect);
    return PairToVectorList(inputlist);
}

/* Window Drawing Routines */

static rgb bbg=rgb(255,255,255);


void drawwindow()
{
    int i;
    
    /* might have resized */
    windowWidth = WIDTH;
    windowHeight = HEIGHT;
    nwide = (windowWidth - 2 * bwidth) / box_w;
    nhigh = (windowHeight - 2 * bwidth - hwidth) / box_h;
    windowWidth = nwide * box_w + 2 * bwidth;
    windowHeight = nhigh * box_h + 2 * bwidth - hwidth;

    clearwindow();

    gfillrect(de, bbg, rect(0, 0, windowWidth, box_h));
    gfillrect(de, bbg, rect(0, 0, box_w, windowHeight));

    for (i = 1; i <= nwide; i++)
	de_drawline(i * box_w, hwidth, i * box_w, windowHeight);
    for (i = 1; i <= nhigh; i++)
	de_drawline(0, hwidth + i * box_h, windowWidth, hwidth + i * box_h);
    colmax = colmin + (nwide - 2);
     /* so row 0 and col 0 are reserved for labels */
    rowmax = rowmin + (nhigh - 2);
    printlabs();
    if (inputlist != R_NilValue)
	for (i = colmin; i <= colmax; i++) drawcol(i);
    highlightrect();
    show(de);
}

/* find_coords finds the coordinates of the upper left corner of the
   given cell on the screen */

void find_coords(int row, int col, int *xcoord, int *ycoord)
{
    *xcoord = bwidth + box_w * col;
    *ycoord = bwidth + hwidth + box_h * row;
}

/* draw the window with the top left box at column wcol and row wrow */

void jumpwin(int wcol, int wrow)
{
    if (wcol < 0 || wrow < 0) {
	bell();
	return;
    }
    closerect();
    colmin = wcol;
    rowmin = wrow;
    drawwindow();
}

void advancerect(int which)
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

/* whichcol is absolute col no, col is position on screen */
void drawcol(int whichcol)
{
    int i, src_x, src_y, len, col = whichcol - colmin + 1;
    char clab[15];
    SEXP tmp;

    find_coords(0, col, &src_x, &src_y);
    cleararea(src_x, src_y, box_w, windowHeight, p->bg);
    cleararea(src_x, src_y, box_w, box_h, bbg);
    for (i = 0; i < nhigh; i++)
	drawrectangle(src_x, hwidth + i * box_h, box_w, box_h, 1, 1);

    /* now fill it in if it is active */

    if (length(inputlist) >= whichcol) {
	tmp = nthcdr(inputlist, whichcol - 1);
	if (TAG(tmp) != R_NilValue)
	    printstring(CHAR(PRINTNAME(TAG(tmp))),
			strlen(CHAR(PRINTNAME(TAG(tmp)))), 0, col, 0);
	else {
	    sprintf(clab, "var%d", whichcol);
	    printstring(clab, strlen(clab), 0, col, 0);
	}
	if (CAR(tmp) != R_NilValue) {
	    len = ((int)LEVELS(CAR(tmp)) > rowmax) ? rowmax : LEVELS(CAR(tmp));
	    for (i = (rowmin - 1); i < len; i++)
		printelt(CAR(tmp), i, i - rowmin + 2, col);
	}
    }
    else {
	sprintf(clab, "var%d", whichcol);
	printstring(clab, strlen(clab), 0, col, 0);
    }
    show(de);
}


/* whichrow is absolute row no */
void drawrow(int whichrow)
{
    int i, src_x, src_y, lenip, row = whichrow - rowmin + 1;
    char rlab[15];
    SEXP tvec;

    find_coords(row, 0, &src_x, &src_y);
    cleararea(src_x, src_y, windowWidth, box_h, (whichrow > 0)?p->bg:bbg);
    for (i = 0; i <= nwide; i++)
	drawrectangle(i * box_w, src_y, box_w, box_h, 1, 1);

    sprintf(rlab, "row %4d", whichrow);
    printstring(rlab, strlen(rlab), row, 0, 0);

    lenip = length(inputlist);
    for (i = colmin; i <= colmax; i++) {
	if (i > lenip) break;
	tvec = CAR(nthcdr(inputlist, i - 1));
	if (tvec != R_NilValue)
	    if (whichrow <= (int)LEVELS(tvec))
	    printelt(tvec, whichrow - 1, row, i - colmin + 1);
    }
    show(de);
}


/* printelt: print the correct value from vector[vrow] into the
   spreadsheet in row ssrow and col sscol */

/* WARNING: This has no check that you're not beyond the end of the
   vector. Caller must check. */

void printelt(SEXP invec, int vrow, int ssrow, int sscol)
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
	if (!streql(CHAR(STRING(invec)[vrow]), CHAR(STRING(ssNA_STRING)[0]))) {
	    strp = EncodeElement(invec, vrow, 0);
	    printstring(strp, strlen(strp), ssrow, sscol, 0);
	}
    }
    else
	error("spreadsheet: internal memory error");
}


static void drawelt(int whichrow, int whichcol)
{
    int i;
    char clab[15];
    SEXP tmp;

    if (length(inputlist) >= whichcol + colmin - 1) {
	tmp = nthcdr(inputlist, whichcol + colmin - 2);
	if (whichrow == 0) {
	    if (TAG(tmp) != R_NilValue) {
		printstring(
		    CHAR(PRINTNAME(TAG(tmp))),
		    strlen(CHAR(PRINTNAME(TAG(tmp)))), 0, whichcol, 0);
	    } else {
		sprintf(clab, "var%d", whichcol + colmin - 1);
		printstring(clab, strlen(clab), 0, whichcol, 0);
	    }
	} else
	    if (CAR(tmp) != R_NilValue && 
		(i = rowmin + whichrow - 2) < (int)LEVELS(CAR(tmp)) )
		printelt(CAR(tmp), i, whichrow, whichcol);
    }
    else if (whichrow == 0){
	sprintf(clab, "var%d", whichcol + colmin - 1);
	printstring(clab, strlen(clab), 0, whichcol, 0);
    }
    else
	printstring("", 0, whichrow,  whichcol, 0);

    show(de);
}

void jumppage(int dir)
{
    switch (dir) {
    case UP:
	rowmin--;
	rowmax--;
	copyarea(0, hwidth + box_h, 0, hwidth + 2 * box_h);
	drawrow(rowmin);
	break;
    case DOWN:
	rowmin++;
	rowmax++;
	copyarea(0, hwidth + 2 * box_h, 0, hwidth + box_h);
	drawrow(rowmax);
	break;
    case LEFT:
	colmin--;
	colmax--;
	copyarea(box_w, hwidth, 2 * box_w, hwidth);
	drawcol(colmin);
	break;
    case RIGHT:
	colmin++;
	colmax++;
	copyarea(2 * box_w, hwidth, box_w, hwidth);
	drawcol(colmax);
	break;
    }
}
/* draw a rectangle, used to highlight/downlight the current box */

void printrect(int lwd, int fore)
{
    drawrectangle(ccol * box_w + lwd - 1, 
		  hwidth + crow * box_h + lwd -1, 
		  box_w - lwd + 1, box_h - lwd + 1, lwd, fore);
    show(de);
}

void downlightrect()
{
    printrect(2, 0);
    printrect(1, 1);
}

void highlightrect()
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
	len = (wrow < 100) ? 100 : wrow;
	CAR(tmp) = ssNewVector(REALSXP, len);
	if (TAG(tmp) == R_NilValue) {
	    sprintf(cname, "var%d", wcol);
	    TAG(tmp) = install(cname);
	}
    }
    if (!isVector(CAR(tmp)))
	error("internal type error in spreadsheet");
    len = LENGTH(CAR(tmp));
    type = TYPEOF(CAR(tmp));
    if (len < wrow) {
	for (newlen = len * 2 ; newlen < wrow ; newlen *= 2)
	    ;
	tmp2 = ssNewVector(type, newlen);
	for (i = 0; i < len; i++)
	    if (type == REALSXP)
		REAL(tmp2)[i] = REAL(CAR(tmp))[i];
	    else if (type == STRSXP)
		STRING(tmp2)[i] = STRING(CAR(tmp))[i];
	    else
		error("internal type error in spreadsheet");
	LEVELS(tmp2) = LEVELS(CAR(tmp));
	CAR(tmp) = tmp2;
    }
    return (tmp);
}

/* close up the entry to a cell, put the value that has been entered
   into the correct place and as the correct type */

extern double R_strtod(char *c, char **end); /* in coerce.c */

void closerect()
{
    SEXP cvec, c0vec, tvec;
    int wcol = ccol + colmin - 1, wrow = rowmin + crow - 1;
    
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
	if (wrow > (int)LEVELS(cvec)) LEVELS(cvec) = wrow;
	if (clength != 0) {
	    int warn = 0;
	    double new;
	    char *endp;
	    /* do it this way to ensure NA, Inf, ...  can get set */
	    new = R_strtod(buf, &endp);
	    warn = !isBlankString(endp);
	    if (TYPEOF(cvec) == STRSXP) {
		tvec = allocString(strlen(buf));
		strcpy(CHAR(tvec), buf);
		STRING(cvec)[wrow - 1] = tvec;
	    } else
		REAL(cvec)[wrow - 1] = new;
	    if (newcol & warn) {
		/* change mode to character */
		int levs = LEVELS(cvec);
		cvec = CAR(c0vec) = coerceVector(cvec, STRSXP);
		LEVELS(cvec) = levs;
		tvec = allocString(strlen(buf));
		strcpy(CHAR(tvec), buf);
		STRING(cvec)[wrow - 1] = tvec;
	    }
	} else {
	    if (TYPEOF(cvec) == STRSXP) 
		STRING(cvec)[wrow - 1] = NA_STRING;
	    else 
		REAL(cvec)[wrow - 1] = NA_REAL;
	}
	drawelt(crow, ccol); /* to get the cell scrolling right */
    }
    CellEditable = CellModified = 0;
    if (newcol) drawcol(wcol); /* to fill in NAs */
    

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

void printstring(char *ibuf, int buflen, int row, int col, int left)
{
    int x_pos, y_pos;
    char buf[45], *pc=buf;

    find_coords(row, col, &x_pos, &y_pos);
    cleararea(col * box_w + 2, 
	      hwidth + row * box_h + 2,
	      box_w - 3, 
	      box_h - 3, (row==0||col==0) ? bbg:p->bg);
    strncpy(buf, ibuf, buflen);
    buf[buflen] = '\0';
    if (buflen > FIELDWIDTH) {
	if(left) {
	    pc += buflen - FIELDWIDTH;
	    *pc = '<';
	} else {
	    *(pc + FIELDWIDTH - 1) = '>';
	    *(pc + FIELDWIDTH) = '\0';	    
	}
    }
    de_drawtext(x_pos + text_xoffset, y_pos - text_yoffset, pc);
    show(de);
}

void clearrect()
{
    cleararea(ccol * box_w, hwidth + crow * box_h, box_w, box_h, p->bg);
    show(de);
}

/* handlechar has to be able to parse decimal numbers and strings,
   depending on the current column type, only printing characters
   should get this far */

/* --- Not true! E.g. ESC ends up in here... */

void handlechar(char *text)
{
    int c;
    SEXP tvec;

    c = text[0];

    if ( c == '\033' ) {
	CellModified = 0;
	clength = 0;
	drawelt(crow, ccol);
	gsetcursor(de, ArrowCursor);
	return;
    }
    else {
	CellModified = 1;
	gsetcursor(de, TextCursor);
    }
    
    if (clength == 0) {
	if (length(inputlist) >= ccol + colmin - 1)
	    tvec = nthcdr(inputlist, ccol + colmin - 2);
	else
	    tvec = R_NilValue;
	if (TYPEOF(CAR(tvec)) == REALSXP)	/* numeric data */
	    currentexp = 1;
	else                                    /* character data */
	    currentexp = 2;
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
	warning("spreadsheet: expression too long");
	clength--;
	goto donehc;
    }

    *bufp++ = text[0];
    printstring(buf, clength, crow, ccol, 1);
    return;

 donehc:	
    bell();
}

void printlabs()
{
    char clab[10];
    int i;
    SEXP tppoint;

    if (length(inputlist) > colmin)
	tppoint = nthcdr(inputlist, colmin - 1);
    else
	tppoint = R_NilValue;

    for (i = colmin; i <= colmax; i++)
	if (TAG(tppoint) != R_NilValue) {
	    printstring(CHAR(PRINTNAME(TAG(tppoint))),
			strlen(CHAR(PRINTNAME(TAG(tppoint)))), 
			0, i - colmin + 1, 0);
	    tppoint = CDR(tppoint);
	}
	else {
	    sprintf(clab, "var%d", i);
	    printstring(clab, strlen(clab), 0, i - colmin + 1, 0);
	}
    for (i = rowmin; i <= rowmax; i++) {
	sprintf(clab, "row %4d", i);
	printstring(clab, strlen(clab), i - rowmin + 1, 0, 0);
    }
}

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
}

static void de_drawline(int fromx, int fromy, int tox, int toy)
{
    gdrawline(de, 1, 0, p->ufg, pt(fromx, fromy), pt(tox, toy));
}

static void drawrectangle(int xpos, int ypos, int width, int height, 
			  int lwd, int fore)
{
    gdrawrect(de, lwd, 0, (fore==1)? p->ufg: p->bg, 
	      rect(xpos, ypos, width, height));
}

static void de_drawtext(int xpos, int ypos, char *text)
{
    gdrawstr(de, p->f, p->fg, pt(xpos, ypos), text);
    show(de);
}

/* Keypress callbacks */

void de_normalkeyin(control c, int k)
{
    int i, st;
    char text[1];

    st = ggetkeystate();
    if ((p->chbrk) && (k == p->chbrk) &&
	((!p->modbrk) || ((p->modbrk) && (st == p->modbrk)))) {
	p->fbrk(c);
	return;
    }
    if (st == CtrlKey) {
	switch (k + 'A' - 1) {
	case 'B':
	    i = rowmin - nhigh + 2;
	    jumpwin(colmin, (i < 1) ? 1:i);
	    jumpwin(colmin, i);
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
	    advancerect(RIGHT);
	    break;
	case 'N':
	case 'J':
	    advancerect(DOWN);
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
	    advancerect(RIGHT);	    
    } else {
	text[0] = k;
	handlechar(text);
    }

}

void de_ctrlkeyin(control c, int key)
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
	break;
    case END:
	jumpwin(colmax, rowmax);
	break;
    case PGUP:
	i = rowmin - nhigh + 2;
	jumpwin(colmin, (i < 1) ? 1:i);
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
	} else bell();
	break;
     case ENTER:
	 advancerect(DOWN);
	 break;
    default:
	;
    }
}

/* mouse callbacks */

void de_mousedown(control c, int buttons, point xy)
{
    int xw, yw, wcol, wrow;
    
    if (buttons) {
	xw = xy.x;
	yw = xy.y;
	
	closerect();
	
	/* check to see if the click was in the header */

	if (yw < hwidth + bwidth) {
	    /* too high */
	    return;
	}
	/* translate to box coordinates */

	wcol = (xw - bwidth) / box_w;
	wrow = (yw - bwidth - hwidth) / box_h;

	/* see if it is in the row labels */
	if (wcol == 0) {
	    bell();
	    highlightrect();
	    return;
	}

	/* next check to see if it is in the column labels */

	if (yw < hwidth + bwidth + box_h) {
	    if (xw > bwidth + box_w)
		de_popupmenu(xw, yw, wcol);
	    else {
		/* in 0th column */
		highlightrect();
		bell();
	    }
	} else if (buttons & LeftButton) {
	    ccol = wcol;
	    crow = wrow;
	} else if (buttons & RightButton) {
	    int x, y;
	    char *prev = "";
	    SEXP tvec;
	    rect rr;
	    
	    ccol = wcol;
	    crow = wrow--;
	    highlightrect();
	    find_coords(crow, ccol, &x, &y);
	    if (ccol <= length(inputlist)) {
		tvec = CAR(nthcdr(inputlist, ccol - 1));
		if (tvec != R_NilValue && wrow <= (int)LEVELS(tvec)) {
		    PrintDefaults(R_NilValue);
		    if (TYPEOF(tvec) == REALSXP) {
			if (REAL(tvec)[wrow] != ssNA_REAL)
			    prev = EncodeElement(tvec, wrow, 0);
		    } else if (TYPEOF(tvec) == STRSXP) {
			if (!streql(CHAR(STRING(tvec)[wrow]), 
				    CHAR(STRING(ssNA_STRING)[0])))
			    prev = EncodeElement(tvec, wrow, 0);
		    } else error("spreadsheet: internal memory error");
		}
	    }
	    rr = rect(x+text_xoffset, y-text_yoffset-1, 
		      box_w-text_xoffset-2, box_h-text_yoffset-2);
	    if (strlen(prev) > FIELDWIDTH)
		rr.width = (strlen(prev) + 2) * FW;
	    celledit = newfield_no_border(prev, rr);
	    setbackground(celledit, p->bg);
	    setforeground(celledit, p->ufg);
	    settextfont(celledit, p->f);
	    show(celledit);
	    CellEditable = 1;
	}
	highlightrect();
	return;
    }
}

void de_redraw(control c, rect r)
{
    drawwindow();
}

void de_closewin()
{
    closerect();
    hide(de);
    del(de);
}

#include <windows.h>
extern HDC get_context(dataeditor);

static void copyarea(int src_x, int src_y, int dest_x, int dest_y)
{
    HDC dc = get_context(de);
    BitBlt(dc, dest_x, dest_y,
	   windowWidth - src_x, windowHeight - src_y, 
	   dc, src_x, src_y, SRCCOPY);
}

static int  initwin()
{    
    rect r;
    de = newdataeditor();
    if(!de) return 1;
    p = getdata(de);
    box_w = FIELDWIDTH*FW + 8;
    box_h = FH + 4;
    text_xoffset = 5;
    text_yoffset = -3;
    windowWidth = WIDTH;
    windowHeight = HEIGHT;
    nwide = (windowWidth - 2 * bwidth) / box_w;
    nhigh = (windowHeight - 2 * bwidth - hwidth) / box_h;
    windowWidth = nwide * box_w + 2 * bwidth;
    windowHeight = nhigh * box_h + 2 * bwidth - hwidth;
    bbg = myGetSysColor(COLOR_BTNFACE);
    r = getrect(de);
    r.width = windowWidth + 3;
    r.height = windowHeight + 3;
    resize(de, r);    
    CellModified = CellEditable = 0;
    drawwindow();
    /* set the active cell to be the upper left one */
    crow = 1;
    ccol = 1;
    show(de);
    R_de_up = 1;
    return 0;
}

/* Menus */

static window wconf;
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
	if (CAR(tvec) == R_NilValue) CAR(tvec) = ssNewVector(REALSXP, 100);
	levs = LEVELS(CAR(tvec));
	CAR(tvec) = coerceVector(CAR(tvec), REALSXP);
	LEVELS(CAR(tvec)) = levs;
	
    } else if(ischecked(rb_char) && isnumeric) {
	if (CAR(tvec) == R_NilValue) CAR(tvec) = ssNewVector(STRSXP, 100);
	levs = LEVELS(CAR(tvec));
	CAR(tvec) = coerceVector(CAR(tvec), STRSXP);
	LEVELS(CAR(tvec)) = levs;
    }
    TAG(tvec) = install(buf);
    hide(wconf);
    del(wconf);
    addto(de);
    drawwindow();
}

void getscreenrect(control, rect *);

static void de_popupmenu(int x_pos, int y_pos, int col)
{
    char blah[25];
    rect r;

    popupcol = colmin + col - 1;
    if (popupcol <= length(inputlist)) {
	SEXP tvec = nthcdr(inputlist, popupcol - 1);
	strcpy(blah, CHAR(PRINTNAME(TAG(tvec))));
	isnumeric = TYPEOF(CAR(tvec)) == REALSXP;
    } else {
	sprintf(blah, "var%d", col);
	isnumeric = 1;
    }
    getscreenrect(de, &r);
    wconf = newwindow("Variable editor", 
		      rect(x_pos + r.x-150, y_pos + r.y-50, 300, 100),
		      Titlebar | Modal | Closebox);
    setclose(wconf, popupclose);
    setbackground(wconf, LightGrey);
    lwhat = newlabel("variable name", rect(10, 20, 90, 20), AlignLeft);
    varname = newfield(blah, rect(100, 20, 120, 20));
    lrb = newlabel("type", rect(50, 60, 50, 20), AlignLeft);
    rb_num = newradiobutton("numeric", rect(100, 60 , 80, 20), NULL);
    rb_char = newradiobutton("character", rect(180, 60 , 80, 20), NULL);
    if (isnumeric) check(rb_num); else check(rb_char);
    show(wconf);
}
