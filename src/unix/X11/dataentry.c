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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "Print.h"

#include "dataentry.h"
#include <stdlib.h>


static void clearwindow(void);

/*
   The spreadsheet function returns a list of vectors. The types of
   these vectors can be specified by the user as can their names. It
   the names are specified they are set during initialization. The
   user can change these via a menu interface, they can also change
   the type.

   The vectors are created too long and if they need to be increased
   this is done by using the next higher power of 2. They start 100
   long. To cut them to the correct length for return you need to know
   the largest row number that was assigned to. LEVELS (sxpinfo.gp) is
   used to keep track of this, separately for each vector. Vectors are
   initialized to NA when they are created so that NA is returned for
   any cell that was not set by the user.  So that coercion back and
   forth maintains values of ssNA_REAL and ssNA_STRING I have set
   ssNA_STRING to be coerceVector(ssNA_REAL), very weird but easy.

   In Macintosh we need to call the main event loop to get
   events. This ensures that the spreadsheet interacts well with the
   other windows. Under X windows we let the window manager handle
   those sorts of details.

 */

static char *menu_label[] =
{
    "Real",
    "Character",
    "Change Name",
};

/*
   ssNewVector is just an interface to allocVector but it lets us
   set the fields to NA. We need to have a special NA for reals and
   strings so that we can differentiate between uninitialized elements
   in the vectors and user supplied NA's; hence ssNA_REAL and ssNA_STRING
 */

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

SEXP RX11_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho)
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
    bwidth = 5;
    hwidth = 30;

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
    cntxt.cend = &closewin;

    highlightrect();

    eventloop();

    endcontext(&cntxt);
    closewin();

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

/* Event Loop Functions */


static void eventloop()
{
    int done;
    DEEvent ioevent;


    done = 0;
    while (done == 0) {
	if (NextEvent(&ioevent)) {
	    switch (WhichEvent(ioevent)) {
	    case activateEvt:
		drawwindow();
		break;
	    case mouseDown:
		done = doMouseDown(&ioevent);
		break;
	    case keyDown:
		doSpreadKey(0, &ioevent);
		break;
	    case MappingNotify:
		RefreshKeyboardMapping(&ioevent);
		break;
	    case ConfigureNotify:
		doConfigure(&ioevent);
		break;
	    }
	}
    }
}

int doMouseDown(DEEvent * event)
{
    return findsquare();
}

static int CellModified;

static void doSpreadKey(int key, DEEvent * event)
{
    KeySym iokey;
    char text[1];

    iokey = GetKey(event);
    text[0] = GetCharP(event);

    if (CheckControl(event))
	doControl(event);
    else if ((iokey == XK_Return) || (iokey == XK_KP_Enter)
	     || (iokey == XK_Linefeed) || (iokey == XK_Down))
	advancerect(DOWN);
    else if (iokey == XK_Left)
	advancerect(LEFT);
    else if ((iokey == XK_Right) || (iokey == XK_Tab))
	advancerect(RIGHT);
    else if (iokey == XK_Up)
	advancerect(UP);
    else if ((iokey == XK_BackSpace) || (iokey == XK_Delete)) {
	if (clength > 0) {
	    clength--;
	    bufp--;
	    printstring(buf, clength, crow, ccol, 1);
	}
	else
	    bell();
    }
    else if (iokey == XK_Home)
	jumpwin(1, 1);
    else if (IsModifierKey(iokey)) {
    }
    else 
	handlechar(text);
}


/* Window Drawing Routines */

void drawwindow()
{
    int i;

    /* if there is an active cell enter the data in it */
    closerect();


    /* now set up the window with the new dimensions */

    clearwindow();

    setattribsfromwindow();

    nwide = (windowWidth - 2 * bwidth) / box_w;

    setlineattribs(1);

    for (i = 1; i <= nwide; i++)
	drawline(i * box_w, hwidth, i * box_w, windowHeight);
    nhigh = (windowHeight - 2 * bwidth - hwidth) / box_h;
    for (i = 1; i <= nhigh; i++)
	drawline(0, hwidth + i * box_h, windowWidth, hwidth + i * box_h);
    colmax = colmin + (nwide - 2);	/* so row 0 and col 0 are reserved for labels */
    rowmax = rowmin + (nhigh - 2);
    printlabs();
    if (inputlist != R_NilValue)
	for (i = colmin; i <= colmax; i++)
	    drawcol(i);

    /* draw the quit box */

    i = textwidth("Quit", 4);
    drawrectangle(windowWidth - 6 - bwidth - i, 3, i + 4, hwidth - 6);
    drawtext(windowWidth - 4 - bwidth - i, hwidth - 7, "Quit", 4);

    /* set the active rectangle to be the upper left one */
    crow = 1;
    ccol = 1;
    highlightrect();

    Rsync();

}

static void clearwindow()
{
    XClearWindow(iodisplay, iowindow);
}

/* find_coords finds the coordinates of the upper left corner of the
   given square on the screen */

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

void drawrow(int whichrow)
{
    int i, src_x, src_y, lenip;
    char rlab[15];
    SEXP tvec;

    find_coords(whichrow, 0, &src_x, &src_y);
    cleararea(src_x, src_y, windowWidth, box_h);
    setlineattribs(1);
    for (i = 0; i <= nwide; i++)
	drawrectangle(i * box_w, src_y, box_w, box_h);

    sprintf(rlab, "R %d", rowmin + whichrow - 1);
    printstring(rlab, strlen(rlab), whichrow, 0, 0);

    lenip = length(inputlist);
    for (i = colmin; i <= colmax; i++) {
	if (i > lenip)
	    break;
	tvec = CAR(nthcdr(inputlist, i - 1));
	if (tvec != R_NilValue)
	    if (whichrow + rowmin - 1 <= (int)LEVELS(tvec))
		printelt(tvec, whichrow + rowmin - 2, 
			 whichrow, i - colmin + 1);
    }

    Rsync();
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

void drawcol(int whichcol)
{
    int i, src_x, src_y, len;
    char clab[15];
    SEXP tmp;

    find_coords(0, whichcol, &src_x, &src_y);
    cleararea(src_x, src_y, box_w, windowHeight);
    setlineattribs(1);
    for (i = 0; i <= nhigh; i++)
	drawrectangle(src_x, hwidth + i * box_h, box_w, box_h);

    /* now fill it in if it is active */

    if (length(inputlist) >= whichcol + colmin - 1) {
	tmp = nthcdr(inputlist, whichcol + colmin - 2);
	if (TAG(tmp) != R_NilValue)
	    printstring(CHAR(PRINTNAME(TAG(tmp))),
			strlen(CHAR(PRINTNAME(TAG(tmp)))), 0, whichcol, 0);
	else {
	    sprintf(clab, "var%d", whichcol + colmin - 1);
	    printstring(clab, strlen(clab), 0, whichcol, 0);
	}
	if (CAR(tmp) != R_NilValue) {
	    len = ((int)LEVELS(CAR(tmp)) > rowmax) ? rowmax : LEVELS(CAR(tmp));
	    for (i = (rowmin - 1); i < len; i++)
		printelt(CAR(tmp), i, i - rowmin + 2, whichcol);
	}
    }
    else {
	sprintf(clab, "var%d", whichcol + colmin - 1);
	printstring(clab, strlen(clab), 0, whichcol, 0);
    }
    Rsync();
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

    Rsync();
}

void jumppage(int dir)
{
    switch (dir) {
    case UP:
	rowmin--;
	rowmax--;
	copyarea(0, hwidth + box_h, 0, hwidth + 2 * box_h);
	drawrow(1);
	break;
    case DOWN:
	rowmin++;
	rowmax++;
	copyarea(0, hwidth + 2 * box_h, 0, hwidth + box_h);
	drawrow((nhigh - 1));
	if (2 * bwidth + box_h * nhigh + hwidth != windowHeight)
	    drawrow(nhigh);
	break;
    case LEFT:
	colmin--;
	colmax--;
	copyarea(box_w, hwidth, 2 * box_w, hwidth);
	drawcol(1);
	break;
    case RIGHT:
	colmin++;
	colmax++;
	copyarea(2 * box_w, hwidth, box_w, hwidth);
	drawcol((nwide - 1));
	if (2 * bwidth + nwide * box_w != windowWidth)
	    drawcol(nwide);
	break;
    }
}
/* draw a rectangle, used to highlight/downlight the current box */

void printrect(int lwd)
{
    setlineattribs(lwd);
    drawrectangle(ccol * box_w + lwd - 1, 
		  hwidth + crow * box_h + lwd -1, 
		  box_w - lwd + 1, box_h - lwd + 1);
    Rsync();
}

void downlightrect()
{
    setforeground(0);
    printrect(2);
    setforeground(1);
    printrect(1);
}

void highlightrect()
{
    setforeground(1);
    printrect(2);
}

/* find out whether the button click was in the quit box */
static int checkquit(int xw)
{
    int wi;

    wi = textwidth("Quit", 4);
    if ((xw < windowWidth - bwidth - 2) 
	&& (xw > windowWidth - bwidth - wi - 6))
	return 1;
    else
	return 0;
}

/* when a buttonpress event happens find the square that is being
   pointed to if the pointer is in the header we need to see if the
   quit button was pressed and if so quit. This is done by having
   findsquare return an int which is zero if we should quit and one
   otherwise */

int findsquare()
{

    int xw, yw, xr, yr, wcol, wrow;

    closerect();
    querypointer(&xr, &yr, &xw, &yw);

    /* check to see if the click was in the header */

    if (yw < hwidth + bwidth) {
	if (checkquit(xw))
	    return 1;
	else
	    return 0;
    }
    /* translate to box coordinates */

    wcol = (xw - bwidth) / box_w;
    wrow = (yw - bwidth - hwidth) / box_h;

    /* see if it is in the row labels */
    if (wcol == 0) {
	bell();
	highlightrect();
	return 0;
    }

    /* next check to see if it is in the column labels */

    if (yw < hwidth + bwidth + box_h) {
	if (xw > bwidth + box_w)
	    popupmenu(xr, yr, wcol, wrow);
	else {
	    highlightrect();
	    bell();
	}
    } else if (wcol != ccol || wrow != crow) {
	ccol = wcol;
	crow = wrow;
    }
    highlightrect();
    return 0;
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
    if (CAR(tmp) == R_NilValue) {
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
    return (CAR(tmp));
}

/* close up the entry to a cell, put the value that has been entered
   into the correct place and as the correct type */

void closerect()
{
    SEXP cvec, tvec;

    *bufp = '\0';

    /* first check to see if anything has been entered */
    if (CellModified) {
	if (clength != 0) {
	    if (crow == 0) {  
		/* then we are entering a new column name */
		if (length(inputlist) < ccol + colmin - 1)
		    inputlist = 
			listAppend(inputlist, 
				   allocList((ccol - colmin - 1 
					      - length(inputlist))));
		tvec = nthcdr(inputlist, ccol + colmin - 2);
		TAG(tvec) = install(buf);
		printstring(buf, strlen(buf), 0, ccol - colmin + 1, 0);
	    } else {
		cvec = getccol();
		if ((crow + rowmin - 1) > (int)LEVELS(cvec))
		    LEVELS(cvec) = (crow + rowmin - 1);
		if (TYPEOF(cvec) == STRSXP) {
		    tvec = allocString(strlen(buf));
		    strcpy(CHAR(tvec), buf);
		    STRING(cvec)[(rowmin + crow - 2)] = tvec;
		} else 
		    REAL(cvec)[(rowmin + crow - 2)] = atof(buf);
		drawelt(crow, ccol);
	    }
	}
	else 
	    if (crow == 0) {
		sprintf(buf, "var%d", ccol);
		printstring(buf, strlen(buf), 0, ccol - colmin + 1, 0);
	    } else {
		cvec = getccol();
		if ((crow + rowmin - 1) > (int)LEVELS(cvec))
		    LEVELS(cvec) = (crow + rowmin - 1);
		if (TYPEOF(cvec) == STRSXP) 
		    STRING(cvec)[(rowmin + crow - 2)] = NA_STRING;
		else 
		    REAL(cvec)[(rowmin + crow - 2)] = NA_REAL;
		drawelt(crow, ccol);
	    }
    }
    CellModified = 0;

    downlightrect();

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
    int i, x_pos, y_pos;
    char buf[45], *pc = buf;

    find_coords(row, col, &x_pos, &y_pos);
    cleararea(col * box_w + 2, 
	      hwidth + row * box_h + 2,
	      box_w - 3, 
	      box_h - 3);
    strncpy(buf, ibuf, buflen);
    if(left) {
	for (i = buflen; i > 1; i--) {
	    if (textwidth(pc, i) < (box_w - text_offset)) break;
	    *(++pc) = '<';
	}
    } else {
	for (i = buflen; i > 1; i--) {
	    if (textwidth(buf, i) < (box_w - text_offset)) break;
	    *(pc + i - 2) = '>';
	}
    }
    drawtext(x_pos + text_offset, y_pos + box_h - text_offset, pc, i);
    Rsync();
}

void clearrect()
{
    cleararea(ccol * box_w, hwidth + crow * box_h, box_w, box_h);
    Rsync();
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
	return;
    }
    else
	CellModified = 1;

    if (clength == 0) {
	if (length(inputlist) >= ccol + colmin - 1)
	    tvec = nthcdr(inputlist, ccol + colmin - 2);
	else
	    tvec = R_NilValue;
	if (crow == 0)	                        /* variable name */
	    currentexp = 3;
	else if (TYPEOF(CAR(tvec)) == STRSXP)	/* character data */
	    currentexp = 2;
	else                                    /* numeric data */
	    currentexp = 1;
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
    if (currentexp == 3) {
	if (isspace(c))
	    goto donehc;
	if (clength == 0) {
	    if (c != '.' && !isalpha(c))
		goto donehc;
	    else if (c != '.' && !isalnum(c))
		goto donehc;
	}
    }

    if (clength++ > 29) {
	warning("spreadsheet: expression too long");
	clength--;
	goto donehc;
    }

    *bufp++ = text[0];
    printstring(buf, clength, crow, ccol, 1);
    return;

 donehc:	bell();
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
	sprintf(clab, "R %d", i);
	printstring(clab, strlen(clab), i - rowmin + 1, 0, 0);
    }
}


/* Event Loop Functions */

static int NextEvent(DEEvent * ioevent)
{
    XNextEvent(iodisplay, ioevent);
    return 1;
}

static int WhichEvent(DEEvent ioevent)
{
    return ioevent.type;
}

static KeySym GetKey(DEEvent * event)
{
    int i;
    char text[1];
    KeySym iokey;

    i = XLookupString(event, text, 10, &iokey, 0);
    return iokey;
}

static char GetCharP(DEEvent * event)
{
    int i;
    char text[1];
    KeySym iokey;

    i = XLookupString(event, text, 1, &iokey, 0);
    return text[0];
}

static int CheckControl(DEEvent * event)
{
    return (*event).xkey.state & ControlMask;
}

static void doControl(DEEvent * event)
{
    int i;
    char text[1];
    KeySym iokey;

    (*event).xkey.state = 0;
    i = XLookupString(event, text, 1, &iokey, 0);
    /* one row overlap when scrolling: top line <--> bottom line */
    if (text[0] == 'f')
	jumpwin(colmin, rowmax);
    else if (text[0] == 'b') {
	i = rowmin - nhigh + 2;
	jumpwin(colmin, (i < 1) ? 1: i);
    }
}


static void doConfigure(DEEvent * event)
{
    if ((windowWidth != (*event).xconfigure.width) ||
	(windowHeight != (*event).xconfigure.height))
	drawwindow();
}

static void RefreshKeyboardMapping(DEEvent * event)
{
    XRefreshKeyboardMapping(event);
}

/* Initialize/Close Windows */

void closewin()
{
    XFreeGC(iodisplay, iogc);
    XDestroyWindow(iodisplay, iowindow);
    XCloseDisplay(iodisplay);
}

/* set up the window, print the grid and column/row labels */

int initwin()
{
    int i, twidth;
    int ioscreen;
    unsigned long iowhite, ioblack;
    char ioname[] = "R DataEntryWindow";
    char digits[] = "123456789.0";
    Window root;
    XEvent ioevent;
    XSetWindowAttributes winattr;


    if ((iodisplay = XOpenDisplay(NULL)) == NULL)
	return (1);

    /* Get Font Loaded if we can */

    font_info = XLoadQueryFont(iodisplay, font_name);
    if (font_info == NULL)
	return 1;		/* ERROR */

    /* find out how wide the input boxes should be and set up the
       window size defaults */

    twidth = textwidth(digits, strlen(digits));
    box_w = twidth + 4;
    box_h = font_info->max_bounds.ascent 
	+ font_info->max_bounds.descent + 4;
    text_offset = 2 + font_info->max_bounds.descent;
    windowWidth = 6 * box_w;
    windowHeight = 26 * box_h + hwidth;

    ioscreen = DefaultScreen(iodisplay);
    iowhite = WhitePixel(iodisplay, ioscreen);
    ioblack = BlackPixel(iodisplay, ioscreen);
    hand_cursor = XCreateFontCursor(iodisplay, XC_hand2);


    iohint.x = 0;
    iohint.y = 0;
    iohint.width = windowWidth;
    iohint.height = windowHeight;
    iohint.flags = PPosition | PSize;
    root = DefaultRootWindow(iodisplay);

    if ((iowindow = XCreateSimpleWindow(
	iodisplay,
	root,
	iohint.x,
	iohint.y,
	iohint.width,
	iohint.height,
	bwidth,
	ioblack,
	iowhite)) == 0)
	return 1;

    XSetStandardProperties(iodisplay, iowindow, ioname, ioname, None,
			   ioname, 0, &iohint);

    winattr.backing_store = Always;
    XChangeWindowAttributes(iodisplay, iowindow, CWBackingStore, &winattr);

    iogc = XCreateGC(iodisplay, iowindow, 0, 0);
    XSetFont(iodisplay, iogc, font_info->fid);
    XSetBackground(iodisplay, iogc, iowhite);
    setforeground(1);

    XSelectInput(iodisplay, iowindow,
		 ButtonPressMask | KeyPressMask 
		 | ExposureMask | StructureNotifyMask);
    XMapRaised(iodisplay, iowindow);


    /* now set up the menu-window, for now use the same text
       dimensions as above */

    menuwindow = XCreateSimpleWindow(iodisplay, root, 0, 0, twidth,
				     4 * box_h, 2, ioblack, iowhite);
    for (i = 0; i < 4; i++) {
	menupanes[i] = XCreateSimpleWindow(iodisplay, menuwindow, 0, 
					   box_h * i, twidth, box_h, 
					   1, ioblack, iowhite);
	XSelectInput(iodisplay, menupanes[i],
		     ButtonPressMask | ButtonReleaseMask | ExposureMask);
    }

    /* XMapSubwindows(iodisplay, menuwindow); */


    winattr.override_redirect = True;
    XChangeWindowAttributes(iodisplay, menuwindow, 
			    CWBackingStore | CWOverrideRedirect, &winattr);
    Rsync();

    /* this next sequence makes sure the window is up and ready before
       you start drawing in it */

    XNextEvent(iodisplay, &ioevent);
    if (ioevent.xany.type == Expose) {
	while (ioevent.xexpose.count)
	    XNextEvent(iodisplay, &ioevent);
    }

    drawwindow();
    CellModified = 0;
    return 0;
}

/* MAC/X11 BASICS */

static void bell()
{
    XBell(iodisplay, 20);
}

static void cleararea(int xpos, int ypos, int width, int height)
{
    XClearArea(iodisplay, iowindow, xpos, ypos, width, height, 0);
}

static void copyarea(int src_x, int src_y, int dest_x, int dest_y)
{
    XCopyArea(iodisplay, iowindow, iowindow, iogc, src_x, src_y,
	      windowWidth - src_x, windowHeight - src_y, dest_x, dest_y);
    Rsync();
}

static void drawline(int fromx, int fromy, int tox, int toy)
{
    XDrawLine(iodisplay, iowindow, iogc, fromx, fromy, tox, toy);
}

static void drawrectangle(int xpos, int ypos, int width, int height)
{
    XDrawRectangle(iodisplay, iowindow, iogc, xpos, ypos, width, height);
}

static void drawtext(int xpos, int ypos, char *text, int len)
{
    XDrawImageString(iodisplay, iowindow, iogc, xpos,
		     ypos, text, len);
    Rsync();
}

static void querypointer(int *xr, int *yr, int *xw, int *yw)
{
    unsigned int keys;
    Window root, child;

    XQueryPointer(iodisplay, iowindow, &root, &child, xr, yr, xw, yw, &keys);
}

static void setattribsfromwindow()
{
    XWindowAttributes attribs;

    XGetWindowAttributes(iodisplay, iowindow, &attribs);
    windowWidth = attribs.width;
    windowHeight = attribs.height;
    bwidth = attribs.border_width;
}

static void setforeground(int which)
{
    if (which == 0)
	XSetForeground(iodisplay, iogc, WhitePixel(iodisplay,
						   DefaultScreen(iodisplay)));
    else
	XSetForeground(iodisplay, iogc, BlackPixel(iodisplay,
						   DefaultScreen(iodisplay)));
}

static void setlineattribs(int width)
{
    XSetLineAttributes(iodisplay, iogc, width, LineSolid, CapRound, JoinRound);
}

static void Rsync()
{
    XSync(iodisplay, 0);
}

static int textwidth(char *text, int nchar)
{
    int t1;

    t1 = XTextWidth(font_info, text, nchar);
    return t1;
}

/* Menus */

void popupmenu(int x_pos, int y_pos, int col, int row)
{
    int i, button, levs;
    char name[20];
    XEvent event;
    Window selected_pane;
    SEXP tvec;

    XMapSubwindows(iodisplay, menuwindow);
    XMapRaised(iodisplay, menuwindow);
    XMoveWindow(iodisplay, menuwindow, x_pos, y_pos);

    /* now fill in the menu panes with the correct information */

    if (length(inputlist) < col + colmin - 1)
	inputlist = listAppend(inputlist, 
			       allocList(col + colmin - 1 
					 - length(inputlist)));
    tvec = nthcdr(inputlist, col + colmin - 2);
    if (TAG(tvec) != R_NilValue)
	sprintf(name, "  %s", CHAR(PRINTNAME(TAG(tvec))));
    else
	sprintf(name, " COLUMN %d", col + colmin - 1);
    XDrawString(iodisplay, 
		menupanes[0], iogc, 3, box_h - 3, name, strlen(name));
    for (i = 1; i < 4; i++)
	XDrawString(iodisplay, 
		    menupanes[i], iogc, 3, box_h - 3,
		    menu_label[i - 1], strlen(menu_label[i - 1]));
    if (CAR(tvec) == R_NilValue || TYPEOF(CAR(tvec)) == REALSXP)
	XDrawString(iodisplay, menupanes[1], iogc, box_w - 20, box_h - 3,
		    "X", 1);
    else
	XDrawString(iodisplay, menupanes[2], iogc, box_w - 20, box_h - 3,
		    "X", 1);

/*
  start an event loop; we're looking for a button press and a button
  release in the same window
*/

    while (1) {
	XNextEvent(iodisplay, &event);
	if (event.type == ButtonPress) {
	    button = event.xbutton.button;
	    selected_pane = event.xbutton.window;
	    for (i = 0; selected_pane != menupanes[i]; i++)
		if (i >= 4) goto done;
	    while (1) {
		while (XCheckTypedEvent(iodisplay, ButtonPress, &event));
		XMaskEvent(iodisplay, ButtonReleaseMask, &event);
		if (event.xbutton.button == button)
		    break;
	    }
	    if (selected_pane == event.xbutton.window) {
		for (i = 0; selected_pane != menupanes[i]; i++);
		switch (i) {
		case 0:
		    bell();
		    break;
		case 1:
		    if (CAR(tvec) == R_NilValue)
			CAR(tvec) = ssNewVector(REALSXP, 100);
		    levs = LEVELS(CAR(tvec));
		    CAR(tvec) = coerceVector(CAR(tvec), REALSXP);
		    LEVELS(CAR(tvec)) = levs;
		    goto done;
		case 2:
		    if (CAR(tvec) == R_NilValue)
			CAR(tvec) = ssNewVector(STRSXP, 100);
		    levs = LEVELS(CAR(tvec));
		    CAR(tvec) = coerceVector(CAR(tvec), STRSXP);
		    LEVELS(CAR(tvec)) = levs;
		    goto done;
		case 3:
		    closerect();
		    ccol = col;
		    crow = 0;
		    clearrect();
		    goto done;
		}
	    }
	}		
        /* this doesn't work and perhaps I should move it up to the
           main control loop */
	else if (event.type == Expose) {
	    if (event.xexpose.window == menuwindow) {
		XDrawString(iodisplay, menupanes[0], iogc, 3, box_h - 3,
			    name, strlen(name));
		for (i = 1; i < 4; i++)
		    XDrawString(iodisplay, menupanes[i], iogc, 3, box_h - 3,
				menu_label[i - 1], strlen(menu_label[i - 1]));
	    }
	}
    }
 done:	popdownmenu();
    highlightrect();
}

void popdownmenu()
{
    XUnmapWindow(iodisplay, menuwindow);
    XUnmapSubwindows(iodisplay, menuwindow);
}
