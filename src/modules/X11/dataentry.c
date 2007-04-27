/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2007  Robert Gentleman, Ross Ihaka and the
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

/* <UTF8>
   Used XTextWidth and XDrawText, so need to use fontsets

   Also needed input context.
*/

/* The version for R 2.1.0 is partly based on patches by
   Ei-ji Nakama <nakama@ki.rim.or.jp> for use with Japanese fonts. */

#define DPRINTS(x) printf(#x "=[%s]\n", x)
#define DPRINTX(x) printf(#x "=%x\n", x)
#define DPRINTD(x) printf(#x "=%d\n", x)

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <stdlib.h>

#ifndef _Xconst
#define _Xconst const
#endif
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>
#include <X11/Intrinsic.h>

#include <Print.h>
/* For the input handlers of the event loop mechanism: */
#include <R_ext/eventloop.h>

#ifdef SUPPORT_MBCS
/* This only uses a FontSet in a MBCS */
# define USE_FONTSET 1
/* In theory we should do this, but it works less well
# ifdef X_HAVE_UTF8_STRING
#  define HAVE_XUTF8TEXTEXTENTS 1
#  define HAVE_XUTF8DRAWSTRING 1
#  define HAVE_XUTF8DRAWIMAGESTRING 1
# endif */
#endif

#ifndef HAVE_KEYSYM
#define KeySym int
#endif

#define DEEvent XEvent

typedef enum { UP, DOWN, LEFT, RIGHT } DE_DIRECTION;

typedef enum {UNKNOWNN, NUMERIC, CHARACTER} CellType;

/* EXPORTS : */
SEXP RX11_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho);

/* Global variables needed for the graphics */
static Display *iodisplay = NULL;
static XContext deContext;
static int nView = 0; /* number of open data windows */
static int fdView = -1;

typedef struct {
    Window iowindow;
    GC iogc;
    XFontStruct *font_info;
    SEXP work, names, lens;
    PROTECT_INDEX wpi, npi, lpi;
    int box_w;                       /* width of a box */
    int boxw[100];                   /* widths of cells */
    int box_h;                       /* height of a box */
    int windowWidth;                 /* current width of the window */
    int fullwindowWidth;
    int windowHeight;                /* current height of the window */
    int fullwindowHeight;
    int crow;                        /* current row */
    int ccol;                        /* current column */
    int nwide, nhigh;
    int colmax, colmin, rowmax, rowmin;
    int bwidth;			/* width of the border */
    int hht;			/* height of header  */
    int text_offset;
    int nboxchars;
    int xmaxused, ymaxused;
    char labform[6];
    Rboolean isEditor;
    Atom prot;
} destruct, *DEstruct;

/* Local Function Definitions */

static void advancerect(DEstruct, DE_DIRECTION);
static int  CheckControl(DEEvent *);
static int  CheckShift(DEEvent *);
static int  checkquit(int);
static void clearrect(DEstruct);
static void closerect(DEstruct);
static void clearwindow(DEstruct);
static void closewin(DEstruct);
static void copycell(DEstruct);
static void doControl(DEstruct, DEEvent*);
static int  doMouseDown(DEstruct, DEEvent*);
static void doSpreadKey(DEstruct, int, DEEvent*);
static void downlightrect(DEstruct);
static void drawwindow(DEstruct);
static void drawcol(DEstruct, int);
static void drawrow(DEstruct, int);
static void eventloop(DEstruct);
static void find_coords(DEstruct, int, int, int*, int*);
static int  findcell(DEstruct);
static char *GetCharP(DEEvent*);
static KeySym GetKey(DEEvent*);
static void handlechar(DEstruct, char*);
static void highlightrect(DEstruct);
static Rboolean initwin(DEstruct, char *);
static void jumppage(DEstruct, DE_DIRECTION);
static void jumpwin(DEstruct, int, int);
static void pastecell(DEstruct, int, int);
static void popdownmenu(DEstruct);
static void popupmenu(DEstruct, int, int, int, int);
static void printlabs(DEstruct);
static void printrect(DEstruct, int, int);
static void printstring(DEstruct, char*, int, int, int, int);
static void printelt(DEstruct, SEXP, int, int, int);
static void RefreshKeyboardMapping(DEEvent*);
static void cell_cursor_init(DEstruct);

/* Functions to hide Xlib calls */
static void bell(void);
static void cleararea(DEstruct, int, int, int, int);
static void copyH(DEstruct, int, int, int);
static void copyarea(DEstruct, int, int, int, int);
static void doConfigure(DEstruct, DEEvent *ioevent);
static void drawrectangle(DEstruct, int, int, int, int, int, int);
static void drawtext(DEstruct, int, int, char*, int);
static void RefreshKeyboardMapping(DEEvent *ioevent);
static void Rsync(DEstruct);
static int textwidth(DEstruct, char*, int);
static int WhichEvent(DEEvent ioevent);

static void R_ProcessX11Events(void *data);


static char *get_col_name(DEstruct, int col);
static int  get_col_width(DEstruct, int col);
static CellType get_col_type(DEstruct, int col);
#ifdef USE_FONTSET
static void calc_pre_edit_pos(DEstruct DE);
#endif
static int last_wchar_bytes(char *);
static SEXP ssNewVector(SEXPTYPE, int);
static SEXP ssNA_STRING;


/* only used in the editor */
static Atom _XA_WM_PROTOCOLS = 0;
static Window menuwindow, menupanes[4];
static Rboolean CellModified;
static int box_coords[6];
static int currentexp;                  /* whether an cell is active */
static int ndecimal;                    /* count decimal points */
static int ne;                          /* count exponents */
static int nneg;			/* indicate whether its a negative */
static int clength;                     /* number of characters currently entered */
static int inSpecial;

#define BOOSTED_BUF_SIZE    201
static char buf[BOOSTED_BUF_SIZE];	/* boosted to allow for MBCS */
static char *bufp;
static char copycontents[sizeof(buf)+1] ;

/* The next few and used only for the editor in MBCS locales */
#ifdef USE_FONTSET
static Status           status;
static XFontSet         font_set;
static XFontStruct	**fs_list;
static int		font_set_cnt;
static char             *fontset_name="-*-fixed-medium-r-normal--13-*-*-*-*-*-*-*";
static XIM		ioim;
static XIMStyle         ioim_style;
static XIMStyles        *ioim_styles;

/*
 * XIM:
 * OverTheSpot XIMPreeditPosition | XIMStatusArea;
 * OffTheSpot  XIMPreeditArea     | XIMStatusArea;
 * Root        XIMPreeditNothing  | XIMStatusNothing;
 */
static XIMStyle preedit_styles[] = {
    XIMPreeditPosition,
    XIMPreeditArea,
    XIMPreeditNothing,
    XIMPreeditNone,
    (XIMStyle)NULL,
};
static XIMStyle status_styles[] = {
    XIMStatusArea,
    XIMStatusNothing,
    XIMStatusNone,
    (XIMStyle)NULL,
};
static XIC ioic;
#endif

#ifndef max
#define max(a, b) (((a)>(b))?(a):(b))
#endif
#ifndef min
#define min(a, b) (((a)<(b))?(a):(b))
#endif
#define BOXW(x) (min(((x<100 && DE->nboxchars==0)?DE->boxw[x]:DE->box_w), DE->fullwindowWidth-DE->boxw[0]-2*DE->bwidth-2))

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
   The spreadsheet function returns a list of vectors. The types of
   these vectors can be specified by the user as can their names. It
   the names are specified they are set during initialization. The
   user can change these via a menu interface, they can also change
   the type.

   The vectors are created too long and if they need to be increased
   this is done by using the next higher power of 2. They start 100
   long.  Vectors are initialized to NA when they are created so that
   NA is returned for any cell that was not set by the user.  We use
   a special type of NA to distinguish this from user-supplied NAs.

   In Macintosh we needed to call the main event loop to get
   events. This ensures that the spreadsheet interacts well with the
   other windows. Under X windows we let the window manager handle
   those sorts of details.

 */

static char *menu_label[] =
{
    " Real",
    " Character",
    "Change Name ",
};

/*
   ssNewVector is just an interface to allocVector but it lets us
   set the fields to NA. We need to have a special NA for reals and
   strings so that we can differentiate between uninitialized elements
   in the vectors and user supplied NA's; hence ssNA_STRING
 */

static SEXP ssNewVector(SEXPTYPE type, int vlen)
{
    SEXP tvec;
    int j;

    tvec = allocVector(type, vlen);
    for (j = 0; j < vlen; j++)
	if (type == REALSXP)
	    REAL(tvec)[j] = NA_REAL;
	else if (type == STRSXP)
	    SET_STRING_ELT(tvec, j, ssNA_STRING);
    return (tvec);
}

static void closewin_cend(void *data)
{
    DEstruct DE = (DEstruct) data;
    closewin(DE);
}

SEXP RX11_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP colmodes, tnames, tvec, tvec2, work2;
    SEXPTYPE type;
    int i, j, cnt, len, nprotect;
    RCNTXT cntxt;
    char clab[25];
    char *title = "R Data Editor";
    destruct DE1;
    DEstruct DE = &DE1;

    nprotect = 0;/* count the PROTECT()s */
    PROTECT_WITH_INDEX(DE->work = duplicate(CAR(args)), &DE->wpi); nprotect++;
    colmodes = CADR(args);
    tnames = getAttrib(DE->work, R_NamesSymbol);

    if (TYPEOF(DE->work) != VECSXP || TYPEOF(colmodes) != VECSXP)
	errorcall(call, "invalid argument");

    /* initialize the constants */

    bufp = buf;
    ne = 0;
    currentexp = 0;
    nneg = 0;
    ndecimal = 0;
    clength = 0;
    inSpecial = 0;
    DE->ccol = 1;
    DE->crow = 1;
    DE->colmin = 1;
    DE->rowmin = 1;
    PROTECT(ssNA_STRING = duplicate(NA_STRING));
    nprotect++;
    DE->bwidth = 5;
    DE->hht = 30;
    DE->isEditor = TRUE;

    /* setup work, names, lens  */
    DE->xmaxused = length(DE->work); DE->ymaxused = 0;
    PROTECT_WITH_INDEX(DE->lens = allocVector(INTSXP, DE->xmaxused), &DE->lpi);
    nprotect++;

    if (isNull(tnames)) {
	PROTECT_WITH_INDEX(DE->names = allocVector(STRSXP, DE->xmaxused),
			   &DE->npi);
	for(i = 0; i < DE->xmaxused; i++) {
	    sprintf(clab, "var%d", i);
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
	    SET_VECTOR_ELT(DE->work, i, ssNewVector(type, 100));
	} else if (!isVector(VECTOR_ELT(DE->work, i)))
	    errorcall(call, "invalid type for value");
	else {
	    if (TYPEOF(VECTOR_ELT(DE->work, i)) != type)
		SET_VECTOR_ELT(DE->work, i,
			       coerceVector(VECTOR_ELT(DE->work, i), type));
	}
    }


    /* start up the window, more initializing in here */
    if (initwin(DE, title))
	errorcall(call, "invalid device");

    /* set up a context which will close the window if there is an error */
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &closewin_cend;
    cntxt.cenddata = (void *) DE;

    highlightrect(DE);

    cell_cursor_init(DE);

    eventloop(DE);

    endcontext(&cntxt);
    closewin(DE);
    if(nView == 0) {
	XCloseDisplay(iodisplay);
	iodisplay = NULL;
    }

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
	    tvec2 = ssNewVector(TYPEOF(tvec), len);
	    for (j = 0; j < len; j++) {
		if (TYPEOF(tvec) == REALSXP) {
			REAL(tvec2)[j] = REAL(tvec)[j];
		} else if (TYPEOF(tvec) == STRSXP) {
		    if (STRING_ELT(tvec, j) != ssNA_STRING)
			SET_STRING_ELT(tvec2, j, STRING_ELT(tvec, j));
		    else
			SET_STRING_ELT(tvec2, j, NA_STRING);
		} else
		    error("dataentry: internal memory problem");
	    }
	    SET_VECTOR_ELT(work2, i, tvec2);
	}
    }

    setAttrib(work2, R_NamesSymbol, DE->names);
    UNPROTECT(nprotect);
    return work2;
}

static void dv_closewin_cend(void *data)
{
    DEstruct DE = (DEstruct) data;
    R_ReleaseObject(DE->lens);
    R_ReleaseObject(DE->work);
    closewin(DE);
    free(DE);
    nView--;
}

SEXP in_R_X11_dataviewer(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP stitle;
    SEXPTYPE type;
    int i, nprotect;
    RCNTXT cntxt;
    DEstruct DE = (DEstruct) malloc(sizeof(destruct));

    nView++;

    nprotect = 0;/* count the PROTECT()s */
    DE->work = CAR(args);
    DE->names = getAttrib(DE->work, R_NamesSymbol);

    if (TYPEOF(DE->work) != VECSXP)
	errorcall(call, "invalid argument");
    stitle = CADR(args);
    if (!isString(stitle) || LENGTH(stitle) != 1)
	errorcall(call, "invalid argument");

    /* initialize the constants */

    bufp = buf;
    ne = 0;
    currentexp = 0;
    nneg = 0;
    ndecimal = 0;
    clength = 0;
    inSpecial = 0;
    DE->ccol = 1;
    DE->crow = 1;
    DE->colmin = 1;
    DE->rowmin = 1;
    DE->bwidth = 5;
    DE->hht = 10;
    DE->isEditor = FALSE;

    /* setup work, names, lens  */
    DE->xmaxused = length(DE->work); DE->ymaxused = 0;
    PROTECT_WITH_INDEX(DE->lens = allocVector(INTSXP, DE->xmaxused), &DE->lpi);
    nprotect++;

    for (i = 0; i < DE->xmaxused; i++) {
	int len = LENGTH(VECTOR_ELT(DE->work, i));
	INTEGER(DE->lens)[i] = len;
	DE->ymaxused = max(len, DE->ymaxused);
	type = TYPEOF(VECTOR_ELT(DE->work, i));
	if (type != STRSXP && type != REALSXP)
	    errorcall(call, "invalid argument");
    }


    /* start up the window, more initializing in here */
    if (initwin(DE, CHAR(STRING_ELT(stitle, 0))))
	errorcall(call, "invalid device");

    /* set up a context which will close the window if there is an error */
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &dv_closewin_cend;
    cntxt.cenddata = (void *) DE;

    highlightrect(DE);

    cell_cursor_init(DE);

    if(fdView < 0) {
	fdView = ConnectionNumber(iodisplay);
	addInputHandler(R_InputHandlers, fdView,
			R_ProcessX11Events, XActivity);
    }

    drawwindow(DE);

    R_PreserveObject(DE->work); /* also preserves names */
    R_PreserveObject(DE->lens);
    UNPROTECT(nprotect);
    return R_NilValue;
}

/* Window Drawing Routines */

static void setcellwidths(DEstruct DE)
{
    int i, w, dw;

    DE->windowWidth = w = 2*DE->bwidth + DE->boxw[0] + BOXW(DE->colmin);
    DE->nwide = 2;
    for (i = 2; i < 100; i++) { /* 100 on-screen columns cannot occur */
	dw = BOXW(i + DE->colmin - 1);
	if((w += dw) > DE->fullwindowWidth ||
	   (!DE->isEditor && i > DE->xmaxused - DE->colmin + 1)) {
	    DE->nwide = i;
	    DE->windowWidth = w - dw;
	    break;
	}
    }
}

static void drawwindow(DEstruct DE)
{
    int i, st;
    XWindowAttributes attribs;

    /* if there is an active cell enter the data in it */
    /*
     * case colname input Expose not use.
     * closerect();
     */

    /* now set up the window with the new dimensions */
    XGetWindowAttributes(iodisplay, DE->iowindow, &attribs);
    DE->bwidth = attribs.border_width;
    DE->fullwindowWidth = attribs.width;
    DE->fullwindowHeight = attribs.height;
    setcellwidths(DE);
    DE->nhigh = (DE->fullwindowHeight - 2 * DE->bwidth - DE->hht) / DE->box_h;
    DE->windowHeight = DE->nhigh * DE->box_h + 2 * DE->bwidth;

    clearwindow(DE);


    for (i = 1; i < DE->nhigh; i++)
	drawrectangle(DE, 0, DE->hht + i * DE->box_h, DE->boxw[0], DE->box_h,
		      1, 1);
     /* so row 0 and col 0 are reserved for labels */
    DE->colmax = DE->colmin + (DE->nwide - 2);
    DE->rowmax = DE->rowmin + (DE->nhigh - 2);
    printlabs(DE);
    for (i = DE->colmin; i <= DE->colmax; i++) drawcol(DE, i);

    if(DE->isEditor) {
	/* draw the quit etc boxes */

	i = textwidth(DE, "Quit", 4);
	box_coords[0] = st = DE->fullwindowWidth - 6 - DE->bwidth;
	box_coords[1] = st - i;
	drawrectangle(DE, st - i, 3, i + 4, DE->hht - 6, 1, 1);
	drawtext(DE, st + 2 - i, DE->hht - 7, "Quit", 4);

	box_coords[4] = st = st - 5*i;
	i = textwidth(DE, "Paste", 5);
	box_coords[5] = st - i;
	drawrectangle(DE, st - i, 3, i + 4, DE->hht - 6, 1, 1);
	drawtext(DE, st + 2 - i, DE->hht - 7, "Paste", 5);

	box_coords[2] = st = st - 2*i;
	i = textwidth(DE, "Copy", 4);
	box_coords[3] = st - i;
	drawrectangle(DE, st - i, 3, i + 4, DE->hht - 6, 1, 1);
	drawtext(DE, st + 2 - i, DE->hht - 7, "Copy", 4);
    }

    highlightrect(DE);

    Rsync(DE);

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
	cleararea(DE, dw, DE->hht, DE->fullwindowWidth-dw,
		  DE->fullwindowHeight);
	/* oldnwide includes the row labels */
	for (i = oldcol+oldnwide-1; i <= DE->colmax; i++) drawcol(DE, i);
    } else {
	/* move one or more cols left */
	dw = BOXW(DE->colmin);
	copyH(DE, DE->boxw[0], DE->boxw[0] + dw, DE->windowWidth - dw + 1);
	dw = DE->windowWidth + 1;
	cleararea(DE, dw, DE->hht, DE->fullwindowWidth-dw,
		  DE->fullwindowHeight);
	drawcol(DE, DE->colmin);
    }

    highlightrect(DE);
    cell_cursor_init(DE);

    Rsync(DE);
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
    *ycoord = DE->bwidth + DE->hht + DE->box_h * row;
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
	closerect(DE);
	drawwindow(DE);
    } else highlightrect(DE);
}

static void advancerect(DEstruct DE, DE_DIRECTION which)
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
	if (!DE->isEditor && DE->crow+DE->rowmin > DE->ymaxused) {
	    bell();
	    break;
	}
	if (DE->crow == (DE->nhigh - 1))
	    jumppage(DE, DOWN);
	else
	    DE->crow++;
	break;
    case RIGHT:
	if (!DE->isEditor && DE->ccol+DE->colmin > DE->xmaxused) {
	    bell();
	    break;
	}
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

    cell_cursor_init(DE);
}

static void cell_cursor_init(DEstruct DE)
{
    int i, whichrow = DE->crow + DE->rowmin - 1,
	whichcol = DE->ccol + DE->colmin -1;
    SEXP tmp;

    memset(buf,0,sizeof(buf));

    if (DE->crow == 0 ){
	strncpy(buf,
		get_col_name(DE, whichcol),
		BOOSTED_BUF_SIZE-1);
    } else {
	if (length(DE->work) >= whichcol) {
	    tmp = VECTOR_ELT(DE->work, whichcol - 1);
	    if (tmp != R_NilValue &&
		(i = whichrow - 1) < LENGTH(tmp) ) {
		PrintDefaults(R_NilValue);
		if (TYPEOF(tmp) == REALSXP) {
		    strncpy(buf, EncodeElement(tmp, i, 0, '.'),
			    BOOSTED_BUF_SIZE-1);
		} else if (TYPEOF(tmp) == STRSXP) {
		    if (STRING_ELT(tmp, i) != ssNA_STRING)
			strncpy(buf, EncodeElement(tmp, i, 0, '.'),
				BOOSTED_BUF_SIZE-1);
		}
	    }
	}
    }
    buf[BOOSTED_BUF_SIZE-1] = '\0';
    clength = strlen(buf);
    bufp = buf + clength;
}

static char *get_col_name(DEstruct DE, int col)
{
    static char clab[25];
    if (col <= DE->xmaxused) {
	/* don't use NA labels */
	SEXP tmp = STRING_ELT(DE->names, col - 1);
	if(tmp != NA_STRING) return(CHAR(tmp));
    }
    sprintf(clab, "var%d", col);
    return clab;
}

static int get_col_width(DEstruct DE, int col)
{
    int i, w = 0, w1;
    char *strp;
    SEXP tmp, lab;

    if (DE->nboxchars > 0) return DE->box_w;
    if (col <= DE->xmaxused) {
	tmp = VECTOR_ELT(DE->work, col - 1);
	if (isNull(tmp)) return DE->box_w;
	/* don't use NA labels */
	lab = STRING_ELT(DE->names, col - 1);
	if(lab != NA_STRING) strp = CHAR(lab); else strp = "var12";
	PrintDefaults(R_NilValue);

	w = textwidth(DE, strp, strlen(strp));
	for (i = 0; i < INTEGER(DE->lens)[col - 1]; i++) {
	    strp = EncodeElement(tmp, i, 0, '.');
	    w1 = textwidth(DE, strp, strlen(strp));
	    if (w1 > w) w = w1;
	}
	if(w < 0.5*DE->box_w) w = 0.5*DE->box_w;
	if(w < 0.8*DE->box_w) w+= 0.1*DE->box_w;
	if(w > 600) w = 600;
	return w+8;
    }
    return DE->box_w;
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
    char *clab;
    SEXP tmp;

    find_coords(DE, 0, col, &src_x, &src_y);
    cleararea(DE, src_x, src_y, bw, DE->windowHeight);
    for (i = 0; i < DE->nhigh; i++)
	drawrectangle(DE, src_x, DE->hht + i * DE->box_h, bw, DE->box_h, 1, 1);

    /* now fill it in if it is active */
    clab = get_col_name(DE, whichcol);
    printstring(DE ,clab, strlen(clab), 0, col, 0);

   if (DE->xmaxused >= whichcol) {
	tmp = VECTOR_ELT(DE->work, whichcol - 1);
	if (!isNull(tmp)) {
	    len = min(DE->rowmax, INTEGER(DE->lens)[whichcol - 1]);
	    for (i = (DE->rowmin - 1); i < len; i++)
		printelt(DE, tmp, i, i - DE->rowmin + 2, col);
	}
    }
    Rsync(DE);
}


/* whichrow is absolute row no */
static void drawrow(DEstruct DE, int whichrow)
{
    int i, src_x, src_y, row = whichrow - DE->rowmin + 1, w;
    char rlab[15];
    SEXP tvec;

    find_coords(DE, row, 0, &src_x, &src_y);
    cleararea(DE, src_x, src_y, DE->windowWidth, DE->box_h);
    drawrectangle(DE, src_x, src_y, DE->boxw[0], DE->box_h, 1, 1);

    sprintf(rlab, DE->labform, whichrow);
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

    Rsync(DE);
}

/* printelt: print the correct value from vector[vrow] into the
   spreadsheet in row ssrow and col sscol */

/* WARNING: This has no check that you're not beyond the end of the
   vector. Caller must check. */

static void printelt(DEstruct DE, SEXP invec, int vrow, int ssrow, int sscol)
{
    char *strp;
    PrintDefaults(R_NilValue);
    if (TYPEOF(invec) == REALSXP) {
	strp = EncodeElement(invec, vrow, 0, '.');
	printstring(DE ,strp, strlen(strp), ssrow, sscol, 0);
    }
    else if (TYPEOF(invec) == STRSXP) {
	if (STRING_ELT(invec, vrow) != ssNA_STRING) {
	    strp = EncodeElement(invec, vrow, 0, '.');
	    printstring(DE ,strp, strlen(strp), ssrow, sscol, 0);
	}
    }
    else
	error("dataentry: internal memory error");
}


static void drawelt(DEstruct DE, int whichrow, int whichcol)
{
    int i;
    char *clab;
    SEXP tmp;

    if (whichrow == 0) {
	clab = get_col_name(DE, whichcol + DE->colmin - 1);
	printstring(DE ,clab, strlen(clab), 0, whichcol, 0);
    } else {
	if (DE->xmaxused >= whichcol + DE->colmin - 1) {
	    tmp = VECTOR_ELT(DE->work, whichcol + DE->colmin - 2);
	    if (!isNull(tmp) && (i = DE->rowmin + whichrow - 2) <
		INTEGER(DE->lens)[whichcol + DE->colmin - 2] )
		printelt(DE, tmp, i, whichrow, whichcol);
	} else
	    printstring(DE, "", 0, whichrow,  whichcol, 0);
    }

    Rsync(DE);
}

static void jumppage(DEstruct DE, DE_DIRECTION dir)
{
    int i, w, oldcol, wcol;

    switch (dir) {
    case UP:
	DE->rowmin--;
	DE->rowmax--;
	copyarea(DE, 0, DE->hht + DE->box_h, 0, DE->hht + 2 * DE->box_h);
	drawrow(DE, DE->rowmin);
	break;
    case DOWN:
	if (DE->rowmax >= 65535) return;
	DE->rowmin++;
	DE->rowmax++;
	copyarea(DE, 0, DE->hht + 2 * DE->box_h, 0, DE->hht + DE->box_h);
	drawrow(DE, DE->rowmax);
	break;
    case LEFT:
	DE->colmin--;
	doHscroll(DE, DE->colmin+1);
	break;
    case RIGHT:
	oldcol = DE->colmin;
	wcol = DE->colmin + DE->ccol + 1; /* column to be selected */
        /* There may not be room to fit the next column in */
	w = DE->fullwindowWidth - DE->boxw[0] - BOXW(DE->colmax + 1);
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
    Rsync(DE);
}

static void downlightrect(DEstruct DE)
{
    printrect(DE, 2, 0);
    printrect(DE, 1, 1);
}

static void highlightrect(DEstruct DE)
{
    printrect(DE, 2, 1);
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
	    sprintf(clab, "var%d", i + 1);
	    SET_STRING_ELT(DE->names, i, mkChar(clab));
	}
	REPROTECT(DE->lens = lengthgets(DE->lens, wcol), DE->lpi);
	DE->xmaxused = wcol;
    }
    if (isNull(VECTOR_ELT(DE->work, wcol - 1))) {
	newcol = TRUE;
	SET_VECTOR_ELT(DE->work, wcol - 1,
		       ssNewVector(REALSXP, max(100, wrow)));
	INTEGER(DE->lens)[wcol - 1] = 0;
    }
    if (!isVector(tmp = VECTOR_ELT(DE->work, wcol - 1)))
	error("internal type error in dataentry");
    len = INTEGER(DE->lens)[wcol - 1];
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
		error("internal type error in dataentry");
	SET_VECTOR_ELT(DE->work, wcol - 1, tmp2);
    }
    return newcol;
}

/* close up the entry to a cell, put the value that has been entered
   into the correct place and as the correct type */

static void closerect(DEstruct DE)
{
    SEXP cvec;
    int i, wcol = DE->ccol + DE->colmin - 1,
	wrow = DE->rowmin + DE->crow - 1, wrow0;
    char clab[25];
    Rboolean newcol;

    *bufp = '\0';

    /* first check to see if anything has been entered */
    if (CellModified) {
	if (DE->crow == 0) {
	    if (clength != 0) {
		/* then we are entering a new column name */
		if (DE->xmaxused < wcol) {
		    /* extend work, names and lens */
		    REPROTECT(DE->work = lengthgets(DE->work, wcol), DE->wpi);
		    REPROTECT(DE->names = lengthgets(DE->names, wcol),
			      DE->npi);
		    for (i = DE->xmaxused; i < wcol - 1; i++) {
			sprintf(clab, "var%d", i + 1);
			SET_STRING_ELT(DE->names, i, mkChar(clab));
		    }
		    REPROTECT(DE->lens = lengthgets(DE->lens, wcol), DE->lpi);
		    DE->xmaxused = wcol;
		}
		SET_STRING_ELT(DE->names, wcol - 1, mkChar(buf));
		printstring(DE ,buf, strlen(buf), 0, wcol, 0);
	    } else {
		sprintf(buf, "var%d", DE->ccol);
		printstring(DE ,buf, strlen(buf), 0, wcol, 0);
	    }
	} else {
	    newcol = getccol(DE);
	    cvec = VECTOR_ELT(DE->work, wcol - 1);
	    wrow0 = INTEGER(DE->lens)[wcol - 1];
	    if (wrow > wrow0) INTEGER(DE->lens)[wcol - 1] = wrow;
	    DE->ymaxused = max(DE->ymaxused, wrow);
	    if (clength != 0) {
		/* do it this way to ensure NA, Inf, ...  can get set */
		char *endp;
		double new = R_strtod(buf, &endp);
		Rboolean warn = !isBlankString(endp);
		if (TYPEOF(cvec) == STRSXP)
		    SET_STRING_ELT(cvec, wrow - 1, mkChar(buf));
		else
		    REAL(cvec)[wrow - 1] = new;
		if (newcol && warn) {
		    /* change mode to character */
		    SEXP tmp = coerceVector(cvec, STRSXP);
		    SET_STRING_ELT(tmp, wrow - 1, mkChar(buf));
		    SET_VECTOR_ELT(DE->work, wcol - 1, tmp);
		}
	    } else {
		if (TYPEOF(cvec) == STRSXP)
		    SET_STRING_ELT(cvec, wrow - 1, NA_STRING);
		else
		    REAL(cvec)[wrow - 1] = NA_REAL;
	    }
	    drawelt(DE, DE->crow, DE->ccol); /* to get the cell scrolling right */
	    if(wrow > wrow0) drawcol(DE, wcol); /* to fill in NAs */
	}
    }
    CellModified = FALSE;

    downlightrect(DE);

    ndecimal = 0;
    nneg = 0;
    ne = 0;
    currentexp = 0;
    clength = 0;
    inSpecial = 0;
    bufp = buf;
}

/* print a null terminated string, check to see if it is longer than
   the print area and print it, left adjusted if necessary; clear the
   area of previous text; */

/* This version will only display 200 chars, but the maximum col width
   will not allow that many */
static void printstring(DEstruct DE,
			char *ibuf, int buflen, int row, int col, int left)
{
    int i, x_pos, y_pos, bw, bufw;
    char pbuf[BOOSTED_BUF_SIZE];
#ifdef USE_FONTSET
    int wcsbufw,j;
    wchar_t wcspbuf[BOOSTED_BUF_SIZE], *wcspc = wcspbuf;
    wchar_t wcs[BOOSTED_BUF_SIZE];
    char    s[BOOSTED_BUF_SIZE];
    wchar_t *w_p;
    char    *p;
    int cnt;

    find_coords(DE, row, col, &x_pos, &y_pos);
    if (col == 0) bw = DE->boxw[0]; else bw = BOXW(col+DE->colmin-1);
    cleararea(DE, x_pos + 2, y_pos + 2, bw - 3, DE->box_h - 3);
    bufw = (buflen > BOOSTED_BUF_SIZE-1) ? BOOSTED_BUF_SIZE-1 : buflen;
    strncpy(pbuf, ibuf, bufw);
    pbuf[bufw] = '\0';

    p = pbuf;
    wcsbufw = mbsrtowcs(wcspbuf, (const char **)&p, bufw, NULL);
    wcspbuf[wcsbufw]=L'\0';
    if(left) {
        for (i = wcsbufw; i > 1; i--) {
	    for(j=0;*(wcspc+j)!=L'\0';j++)wcs[j]=*(wcspc+j);
	    wcs[j]=L'\0';
	    w_p=wcs;
	    cnt=wcsrtombs(s,(const wchar_t **)&w_p,sizeof(s)-1,NULL);
	    s[cnt]='\0';
            if (textwidth(DE, s, strlen(s)) < (bw - DE->text_offset)) break;
            *(++wcspc) = L'<';
        }
    } else {
        for (i = wcsbufw; i > 1; i--) {
	    for(j=0;*(wcspc+j)!=L'\0';j++)wcs[j]=*(wcspc+j);
	    wcs[j]=L'\0';
	    w_p=wcs;
	    cnt=wcsrtombs(s,(const wchar_t **)&w_p,sizeof(s)-1,NULL);
	    s[cnt]='\0';
            if (textwidth(DE, s, strlen(s)) < (bw - DE->text_offset)) break;
            *(wcspbuf + i - 2) = L'>';
            *(wcspbuf + i - 1) = L'\0';
        }
    }
    for(j=0;*(wcspc+j)!=L'\0';j++) wcs[j]=*(wcspc+j);
    wcs[j]=L'\0';
    w_p=wcs;
    cnt=wcsrtombs(s,(const wchar_t **)&w_p,sizeof(s)-1,NULL);

    drawtext(DE, x_pos + DE->text_offset, y_pos + DE->box_h - DE->text_offset,
	     s, cnt);

#else  /* USE_FONTSET */
    char *pc = pbuf;
    find_coords(DE, row, col, &x_pos, &y_pos);
    if (col == 0) bw = DE->boxw[0]; else bw = BOXW(col+DE->colmin-1);
    cleararea(DE, x_pos + 2, y_pos + 2, bw - 3, DE->box_h - 3);
    bufw = (buflen > 200) ? 200 : buflen;
    strncpy(pbuf, ibuf, bufw);
    if(left) {
        for (i = bufw; i > 1; i--) {
            if (textwidth(DE, pc, i) < (bw - DE->text_offset)) break;
            *(++pc) = '<';
        }
    } else {
        for (i = bufw; i > 1; i--) {
            if (textwidth(DE, pbuf, i) < (bw - DE->text_offset)) break;
            *(pbuf + i - 2) = '>';
        }
    }
    drawtext(DE, x_pos + DE->text_offset, y_pos + DE->box_h - DE->text_offset,
	     pc, i);
#endif /* USE_FONTSET */

    Rsync(DE);
}

static void clearrect(DEstruct DE)
{
    int x_pos, y_pos;

    find_coords(DE, DE->crow, DE->ccol, &x_pos, &y_pos);
    cleararea(DE, x_pos, y_pos, BOXW(DE->ccol+DE->colmin-1), DE->box_h);
    Rsync(DE);
}

/* handlechar has to be able to parse decimal numbers and strings,
   depending on the current column type, only printing characters
   should get this far */

/* --- Not true! E.g. ESC ends up in here... */
#ifdef USE_FONTSET
#include <R_ext/rlocale.h>
#include <wchar.h>
#include <wctype.h>
#endif

/* <FIXME> This is not correct for stateful MBCSs, but that's hard to
   do as we get a char at a time */
static void handlechar(DEstruct DE, char *text)
{
    int i, c = text[0];
#ifdef USE_FONTSET
    wchar_t wcs[BOOSTED_BUF_SIZE];

    memset(wcs,0,sizeof(wcs));
#endif

    if ( c == '\033' ) { /* ESC */
	CellModified = FALSE;
	clength = 0;
        bufp = buf;
	drawelt(DE, DE->crow, DE->ccol);
	cell_cursor_init(DE);
	return;
    } else
	CellModified = TRUE;

    if (clength == 0) {

	if (DE->crow == 0)	                        /* variable name */
	    currentexp = 3;
	else
	    switch(get_col_type(DE, DE->ccol + DE->colmin - 1)) {
	    case NUMERIC:
		currentexp = 1;
		break;
	    default:
		currentexp = 2;
	    }
	clearrect(DE);
	highlightrect(DE);
    }

    /* NA number? */
    if (get_col_type(DE, DE->ccol + DE->colmin - 1) == NUMERIC) {
	/* input numeric for NA of buffer , suppress NA etc.*/
	if(strcmp(buf, "NA") == 0 || strcmp(buf, "NaN") == 0 ||
	   strcmp(buf, "Inf") == 0 || strcmp(buf, "-Inf") == 0) {
	    buf[0] = '\0';
	    clength = 0;
	    bufp = buf;
	}
    }

    if (currentexp == 1) {	/* we are parsing a number */

#ifdef USE_FONTSET
      char *mbs = text;
      int cnt = mbsrtowcs(wcs, (const char **)&mbs, strlen(text)+1, NULL);

      for(i = 0; i < cnt; i++) {
	  switch (wcs[i]) {
	  case L'-':
	      if (nneg == 0) nneg++; else goto donehc;
	      break;
	  case L'.':
	      if (ndecimal == 0) ndecimal++; else goto donehc;
	      break;
	  case L'e':
	  case L'E':
	      if (ne == 0) {
		  nneg = ndecimal = 0;	/* might have decimal in exponent */
		  ne++;
	      } else goto donehc;
	      break;
	case L'N':
	    if(nneg) goto donehc;
	case L'I':
	    inSpecial++;
	    break;
	  default:
	      if (!inSpecial && !iswdigit(wcs[i])) goto donehc;
	      break;
	  }
      }
#else
        switch (c) {
	case '-':
	    if (nneg == 0) nneg++; else goto donehc;
	    break;
	case '.':
	    if (ndecimal == 0) ndecimal++; else goto donehc;
	    break;
	case 'e':
	case 'E':
	    if (ne == 0) {
		nneg = ndecimal = 0;	/* might have decimal in exponent */
		ne++;
	    } else goto donehc;
	    break;
	case 'N':
	    if(nneg) goto donehc;
	case 'I':
	    inSpecial++;
	    break;
	default:
	    if (!inSpecial && !isdigit(c)) goto donehc;
	    break;
	}
#endif
    }
    if (currentexp == 3) {
#ifdef USE_FONTSET
	char *mbs = text;
	int cnt = mbsrtowcs(wcs, (const char **)&mbs, strlen(text)+1, NULL);
	for(i = 0; i < cnt; i++) {
	    if (iswspace(wcs[i])) goto donehc;
	    if (clength == 0 && wcs[i] != L'.' && !iswalpha(wcs[i]))
		goto donehc;
	    else if (wcs[i] != L'.' && !iswalnum(wcs[i])) goto donehc;
	}
#else
	if (isspace(c)) goto donehc;
	if (clength == 0  && c != '.' && !isalpha(c)) goto donehc;
	else if (c != '.' && !isalnum(c)) goto donehc;
#endif
    }

    if (clength+strlen(text) > BOOSTED_BUF_SIZE - MB_CUR_MAX - 1) {
	warning("dataentry: expression too long");
	goto donehc;
    }

    for(i = 0; i < strlen(text); i++) *bufp++ = text[i];
    *(bufp+1) = '\0';
    clength += strlen(text);
    printstring(DE, buf, clength, DE->crow, DE->ccol, 1);
    return;

 donehc:
    bell();
}

static void printlabs(DEstruct DE)
{
    char clab[15], *p;
    int i;

    for (i = DE->colmin; i <= DE->colmax; i++) {
	p = get_col_name(DE, i);
	printstring(DE, p, strlen(p), 0, i - DE->colmin + 1, 0);
    }
    for (i = DE->rowmin; i <= DE->rowmax; i++) {
	sprintf(clab, DE->labform, i);
	printstring(DE, clab, strlen(clab), i - DE->rowmin + 1, 0, 0);
    }
}

               /* ================ X11-specific ================ */

/* find out whether the button click was in the quit box */
static int checkquit(int xw)
{
    if (xw > box_coords[1] && xw < box_coords[0]) return 1;
    if (xw > box_coords[3] && xw < box_coords[2]) return 2;
    if (xw > box_coords[5] && xw < box_coords[4]) return 3;
    return 0;
}

/* when a buttonpress event happens find the square that is being
   pointed to if the pointer is in the header we need to see if the
   quit button was pressed and if so quit. This is done by having
   findcell return an int which is one if we should quit and zero
   otherwise */

static int findcell(DEstruct DE)
{

    int xw, yw, xr, yr, wcol=0, wrow, i, w;
    unsigned int keys;
    Window root, child;

    closerect(DE);
    XQueryPointer(iodisplay, DE->iowindow, &root, &child,
		  &xr, &yr, &xw, &yw, &keys);

    if (keys & Button1Mask) { /* left click */

	/* check to see if the click was in the header */

	if (yw < DE->hht + DE->bwidth) {
	    i =  checkquit(xw);
	    if (i == 1) return 1;
	    else if (i == 2) copycell(DE);
	    else if (i == 3) pastecell(DE, DE->crow, DE->ccol);
	    return 0;
	}


	/* see if it is in the row labels */
	if (xw < DE->bwidth + DE->boxw[0]) {
	    bell();
	    highlightrect(DE);
	    return 0;
	}
	/* translate to box coordinates */
	wrow = (yw - DE->bwidth - DE->hht) / DE->box_h;
	w = DE->bwidth + DE->boxw[0];
	for (i = 1; i <= DE->nwide; i++)
	    if((w += BOXW(i+DE->colmin-1)) > xw) {
		wcol = i;
		break;
	    }

	/* next check to see if it is in the column labels */

	if (yw < DE->hht + DE->bwidth + DE->box_h) {
	    if (xw > DE->bwidth + DE->boxw[0])
		popupmenu(DE, xr, yr, wcol, wrow);
	    else {
		highlightrect(DE);
		bell();
	    }
	} else if (wrow > DE->nhigh - 1 || wcol > DE->nwide -1) {
		/* off the grid */
		highlightrect(DE);
		bell();
	} else if (wcol != DE->ccol || wrow != DE->crow) {
	    DE->ccol = wcol;
	    DE->crow = wrow;
	}
    }
    if (keys & Button2Mask) { /* Paste */
	int row, col = 0;

	if (yw < DE->hht + DE->bwidth || xw < DE->bwidth + DE->boxw[0])
	    return 0;

	/* translate to box coordinates */
	row = (yw - DE->bwidth - DE->hht) / DE->box_h;
	w = DE->bwidth + DE->boxw[0];
	for (i = 1; i <= DE->nwide; i++)
	    if ((w += BOXW(i+DE->colmin-1)) > xw) {
		col = i;
		break;
	    }
	pastecell(DE, row, col);
    }
    highlightrect(DE);
    return 0;
}


/* Event Loop Functions */
#define mouseDown 	ButtonPress
#define keyDown		KeyPress
#define activateEvt	MapNotify
#define updateEvt	Expose

static void eventloop(DEstruct DE)
{
    int done;
    DEEvent ioevent;

    done = 0;
    while (done == 0) {
        XNextEvent(iodisplay, &ioevent);
        {
#ifdef USE_FONTSET
            if (XFilterEvent(&ioevent, None)){
		if(ioic){
		    XSetICFocus(ioic);
		    if (ioim_style & XIMPreeditPosition)
			calc_pre_edit_pos(DE);
		}
		continue;
	    }
#endif

	    switch (WhichEvent(ioevent)) {
	    case keyDown:/* KeyPress */
		doSpreadKey(DE, 0, &ioevent);
		break;
            case Expose:
		while(XCheckTypedEvent(iodisplay, Expose, &ioevent))
		    ;
		/*
		 * XIM on  - KeyPress - Expose
		 * XIM off - KeyPress - KeyRelease
		 * colname change XIM on mode. type Backspace.
		 */
	        if(DE->crow == 0){
		    drawwindow(DE);
		    printstring(DE, buf, clength, DE->crow, DE->ccol, 1);
		} else {
		    closerect(DE);
		    drawwindow(DE);
		    cell_cursor_init(DE);
		}
		break;
	    case activateEvt:/* MapNotify */
	      	closerect(DE);
		drawwindow(DE);
		cell_cursor_init(DE);
 		break;
	    case mouseDown:/* ButtonPress */
		if(DE->isEditor) {
		    done  = doMouseDown(DE, &ioevent);
		    cell_cursor_init(DE);
		}
		break;
	    case MappingNotify:
		RefreshKeyboardMapping(&ioevent);
		break;
	    case ConfigureNotify:
		while(XCheckTypedEvent(iodisplay, ConfigureNotify, &ioevent))
		    ;
		doConfigure(DE, &ioevent);
		cell_cursor_init(DE);
		break;
	    case ClientMessage:
		if(ioevent.xclient.message_type == _XA_WM_PROTOCOLS
		   && ioevent.xclient.data.l[0] == DE->prot) {
		    /* user clicked on `close' aka `destroy' */
		    done = 1;
		}
		break;
	    }
	}
    }
}

static void R_ProcessX11Events(void *data)
{
    caddr_t temp;
    DEstruct DE = NULL;
    DEEvent ioevent;
    int done = 0;

    while (XPending(iodisplay)) {
        XNextEvent(iodisplay, &ioevent);
	XFindContext(iodisplay, ioevent.xany.window, deContext, &temp);
	DE = (DEstruct) temp;
	switch (WhichEvent(ioevent)) {
	case keyDown:/* KeyPress */
	    doSpreadKey(DE, 0, &ioevent);
	    break;
	case Expose:
	    while(XCheckTypedEvent(iodisplay, Expose, &ioevent))
		;
	    drawwindow(DE);
	    break;
	case MappingNotify:
	    RefreshKeyboardMapping(&ioevent);
	    break;
	case ConfigureNotify:
	    while(XCheckTypedEvent(iodisplay, ConfigureNotify, &ioevent))
		;
	    doConfigure(DE, &ioevent);
	    cell_cursor_init(DE);
	    break;
	case activateEvt:/* MapNotify */
	    break;
	case ClientMessage:
	    if(ioevent.xclient.message_type == _XA_WM_PROTOCOLS
	       && ioevent.xclient.data.l[0] == DE->prot) {
		/* user clicked on `close' aka `destroy' */
		done = 1;
	    }
	    break;
	}
    }
    if(done) {
	R_ReleaseObject(DE->lens);
	R_ReleaseObject(DE->work);
	closewin(DE);
	free(DE);
	nView--;
	if(nView == 0) {
	    removeInputHandler(&R_InputHandlers,
			       getInputHandler(R_InputHandlers,fdView));
	    fdView = -1;
	    XCloseDisplay(iodisplay);
	    iodisplay = NULL;
	}
	
    }
}

static int doMouseDown(DEstruct DE, DEEvent * event)
{
    return findcell(DE);
}

static void doSpreadKey(DEstruct DE, int key, DEEvent * event)
{
    KeySym iokey;
    char *text;

    iokey = GetKey(event);
    text = GetCharP(event);

    if (CheckControl(event))
	doControl(DE, event);
    else if ((iokey == XK_Return)  || (iokey == XK_KP_Enter) ||
	     (iokey == XK_Linefeed)|| (iokey == XK_Down))
	advancerect(DE, DOWN);
    else if (iokey == XK_Left)
	advancerect(DE, LEFT);
    else if (iokey == XK_Right)
	advancerect(DE, RIGHT);
    else if (iokey == XK_Up)
	advancerect(DE, UP);
#ifdef XK_Page_Up
    else if (iokey == XK_Page_Up) {
	int i = DE->rowmin - DE->nhigh + 2;
	jumpwin(DE, DE->colmin, max(1, i));
	cell_cursor_init(DE);
    }
#elif defined(XK_Prior)
    else if (iokey == XK_Prior) {
	int i = DE->rowmin - DE->nhigh + 2;
	jumpwin(DE, DE->colmin, max(1, i));
	cell_cursor_init(DE);
    }
#endif
#ifdef XK_Page_Down
    else if (iokey == XK_Page_Down) {
	if(DE->isEditor)
	   jumpwin(DE, DE->colmin, DE->rowmax);
	else {
	    int i = DE->ymaxused - DE->nhigh + 2;
	    jumpwin(DE, DE->colmin, max(i, 1));
	}
	cell_cursor_init(DE);
    }
#elif defined(XK_Next)
    else if (iokey == XK_Next) {
	jumpwin(DE, DE->colmin, DE->rowmax);
	cell_cursor_init(DE);
    }
#endif
    else if (DE->isEditor && (iokey == XK_BackSpace || iokey == XK_Delete)) {
	if (clength > 0) {
	    int last_w ;
	    last_w = last_wchar_bytes(NULL);
	    clength -= last_w;
	    bufp -= last_w;
	    *bufp = '\0';
	    CellModified = TRUE;
	    printstring(DE, buf, clength, DE->crow, DE->ccol, 1);
	} else bell();
    }
    else if (iokey == XK_Tab) {
	if(CheckShift(event)) advancerect(DE, LEFT);
	else advancerect(DE, RIGHT);
    }
    else if (iokey == XK_Home) {
	jumpwin(DE, 1, 1);
	downlightrect(DE);
	DE->crow = DE->ccol = 1;
	highlightrect(DE);
	cell_cursor_init(DE);
    }
    else if (iokey == XK_End) {
	int i = DE->ymaxused - DE->nhigh + 2, j, w = 0 ;
	/* Try to work out which cols we can fit in */
	for(j = DE->xmaxused;j >= 0; j--) {
	    w += BOXW(j);
	    if(w > DE->fullwindowWidth) break;
	}
	jumpwin(DE, min(j+1, DE->xmaxused), max(i, 1));
	downlightrect(DE);
	DE->crow = DE->ymaxused - DE->rowmin + 1;
	DE->ccol = DE->xmaxused - DE->colmin + 1;
	highlightrect(DE);
	cell_cursor_init(DE);
    }
    else if (IsModifierKey(iokey)) {
    }
    else if(DE->isEditor) {
	handlechar(DE, text);
    }
}


static int WhichEvent(DEEvent ioevent)
{
    return ioevent.type;
}

static KeySym GetKey(DEEvent * event)
{
    char text[1];
    KeySym iokey;

    XLookupString((XKeyEvent *)event, text, 1, &iokey, NULL);
    return iokey;
}

static char *GetCharP(DEEvent * event)
{
    static char text[BOOSTED_BUF_SIZE];
    KeySym iokey;

    memset(text,0,sizeof(text));

#ifdef USE_FONTSET
    if(mbcslocale) {
#ifdef HAVE_XUTF8LOOKUPSTRING
        if(utf8locale)
	    Xutf8LookupString(ioic, (XKeyEvent *)event,
			      text, sizeof(text) - clength,
			      &iokey, &status);
	else
#endif
	    XmbLookupString(ioic, (XKeyEvent *)event,
			    text, sizeof(text) - clength,
			    &iokey, &status);
	/* FIXME check the return code */
	if(status == XBufferOverflow)
	    warning("dataentry: expression too long");
    } else
#endif
	XLookupString((XKeyEvent *)event,
		      text, sizeof(text) - clength,
		      &iokey, NULL);
    return text;
}

static int CheckControl(DEEvent * event)
{
    return (*event).xkey.state & ControlMask;
}

static int CheckShift(DEEvent * event)
{
    return (*event).xkey.state & ShiftMask;
}

static void doControl(DEstruct DE, DEEvent * event)
{
    int i;
    char text[1];
    KeySym iokey;

    (*event).xkey.state = 0;
    XLookupString((XKeyEvent *)event, text, 1, &iokey, NULL);
    /* one row overlap when scrolling: top line <--> bottom line */
    switch (text[0]) {
	case 'b':
	    i = DE->rowmin - DE->nhigh + 2;
	    jumpwin(DE, DE->colmin, max(1, i));
	    break;
	case 'f':
	    jumpwin(DE, DE->colmin, DE->rowmax);
	    break;
	case 'l':
	    closerect(DE);
	    for (i = 1 ; i <= min(100, DE->xmaxused); i++)
		DE->boxw[i] = get_col_width(DE, i);
	    closerect(DE);
	    drawwindow(DE);
	    break;
    }
    cell_cursor_init(DE);
}


static void doConfigure(DEstruct DE, DEEvent * event)
{
    if ((DE->fullwindowWidth != (*event).xconfigure.width) ||
	(DE->fullwindowHeight != (*event).xconfigure.height)) {
	closerect(DE);
	drawwindow(DE);
    }
}

static void RefreshKeyboardMapping(DEEvent * event)
{
    XRefreshKeyboardMapping((XMappingEvent *)event);
}

/* Initialize/Close Windows */

void closewin(DEstruct DE)
{
    XFreeGC(iodisplay, DE->iogc);
#ifdef USE_FONTSET
    if(mbcslocale  && DE->isEditor) {
	XDestroyIC(ioic);
	XCloseIM(ioim);
    }
#endif
    XDestroyWindow(iodisplay, DE->iowindow);
    /* XCloseDisplay(iodisplay); */
}

#define USE_Xt 1

#ifdef USE_Xt
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
typedef struct gx_device_X_s {
    Pixel background, foreground, borderColor;
    Dimension borderWidth;
    String geometry;
} gx_device_X;

/* (String) casts are here to suppress warnings about discarding `const' */
#define RINIT(a,b,t,s,o,it,n)\
  {(String)(a), (String)(b), (String)t, sizeof(s),\
   XtOffsetOf(gx_device_X, o), (String)it, (n)}
#define rpix(a,b,o,n)\
  RINIT(a,b,XtRPixel,Pixel,o,XtRString,(XtPointer)(n))
#define rdim(a,b,o,n)\
  RINIT(a,b,XtRDimension,Dimension,o,XtRImmediate,(XtPointer)(n))
#define rstr(a,b,o,n)\
  RINIT(a,b,XtRString,String,o,XtRString,(char*)(n))

static XtResource x_resources[] = {
    rpix(XtNforeground, XtCForeground, foreground, "XtDefaultForeground"),
    rpix(XtNbackground, XtCBackground, background, "XtDefaultBackground"),
    rstr(XtNgeometry, XtCGeometry, geometry, NULL),
};

static const int x_resource_count = XtNumber(x_resources);
static gx_device_X xdev;
#endif

static int R_X11Err(Display *dsp, XErrorEvent *event)
{
    char buff[1000];
    XGetErrorText(dsp, event->error_code, buff, 1000);
    warning("X11 protocol error: %s", buff);
    return 0;
}

static int R_X11IOErr(Display *dsp)
{
    error("X11 fatal IO error: please save work and shut down R");
    return 0; /* but should never get here */
}

/* set up the window, print the grid and column/row labels */

static Rboolean initwin(DEstruct DE, char *title) /* TRUE = Error */
{
    int i, twidth, w, minwidth, labdigs;
    int ioscreen;
    unsigned long iowhite, ioblack;
    char digits[] = "123456789.0";
    char             *font_name="9x15";
    Window root;
    XEvent ioevent;
    XSetWindowAttributes winattr;
    XWindowAttributes attribs;
    XSizeHints *hint;
    unsigned long fevent=0UL;
#ifdef USE_FONTSET
    int j,k;
    XVaNestedList   xva_nlist;
    XPoint xpoint;
#endif

    strcpy(copycontents, "");

#ifdef USE_FONTSET
    if (!XSupportsLocale ())
	warning("locale not supported by Xlib: some X ops will operate in C locale");
    if (!XSetLocaleModifiers ("")) warning("X cannot set locale modifiers");
#endif

    if(!iodisplay) {
	if ((iodisplay = XOpenDisplay(NULL)) == NULL) {
	    warning("unable to open display");
	    return TRUE;
	}
	deContext = XUniqueContext();
	XSetErrorHandler(R_X11Err);
	XSetIOErrorHandler(R_X11IOErr);
    }
    

    /* Get Font Loaded if we can */

#ifdef USE_FONTSET
    if(mbcslocale) {
	int  missing_charset_count;
	char **missing_charset_list;
	char *def_string;

	char opt_fontset_name[512];

	/*
	  options("X11fonts")[1] read font name
	*/
	char *s = CHAR(STRING_ELT(GetOption(install("X11fonts"),
					    R_NilValue), 0));

	if (s == NULL) {
	    strcpy(opt_fontset_name,fontset_name);
	} else {
	    sprintf(opt_fontset_name,s,"medium","r",12);
	}

	font_set = XCreateFontSet(iodisplay, opt_fontset_name,
				  &missing_charset_list,
				  &missing_charset_count, &def_string);
	if (missing_charset_count) XFreeStringList(missing_charset_list);
	if (font_set == NULL) {
	    warning("unable to create fontset %s", fontset_name);
	    return TRUE; /* ERROR */
	}
    } else
#endif
    {
	DE->font_info = XLoadQueryFont(iodisplay, font_name);
	if (DE->font_info == NULL) {
	    warning("unable to losd font %s", font_name);
	    return TRUE; /* ERROR */
	}
    }

    /* find out how wide the input boxes should be and set up the
       window size defaults */

    DE->nboxchars = asInteger(GetOption(install("de.cellwidth"), R_GlobalEnv));
    if (DE->nboxchars == NA_INTEGER || DE->nboxchars < 0) DE->nboxchars = 0;

    twidth = textwidth(DE, digits, strlen(digits));

    if (DE->nboxchars > 0) twidth = (twidth * DE->nboxchars)/10;
    DE->box_w = twidth + 4;
#ifdef USE_FONTSET
    if(mbcslocale) {
	XFontSetExtents *extent = XExtentsOfFontSet(font_set);
	char **ml;
	DE->box_h = (extent->max_logical_extent.height)
	    + (extent->max_logical_extent.height / 5) + 4;
	font_set_cnt = XFontsOfFontSet(font_set, &fs_list, &ml);
	DE->text_offset = 2 + fs_list[0]->max_bounds.descent;
    } else
#endif
    {
	DE->box_h = DE->font_info->max_bounds.ascent
	    + DE->font_info->max_bounds.descent + 4;
	DE->text_offset = 2 + DE->font_info->max_bounds.descent;
    }
    DE->windowHeight = 26 * DE->box_h + DE->hht + 2;
    /* this used to presume 4 chars sufficed for row numbering */
    labdigs = max(3, 1+floor(log10((double)DE->ymaxused)));
    sprintf(DE->labform, "%%%dd", labdigs);
    DE->boxw[0] = 0.1*labdigs*textwidth(DE, "0123456789", 10) +
	textwidth(DE, " ", 1) + 8;
    for(i = 1; i < 100; i++) DE->boxw[i] = get_col_width(DE, i);

    /* try for a window width that covers all the columns, or is around
       800 pixels */
    w = DE->windowWidth = 0;
    for(i = 0; i <= DE->xmaxused; i++) {
	w += DE->boxw[i];
	if(w > 800) {
	    DE->windowWidth = w - DE->boxw[i];
	    break;
	}
    }
    if(DE->windowWidth == 0) DE->windowWidth = w;
    DE->windowWidth += 2;
    /* allow enough width for buttons */
    minwidth = 7.5 * textwidth(DE, "Paste", 5);
    if(DE->windowWidth < minwidth) DE->windowWidth = minwidth;

    ioscreen = DefaultScreen(iodisplay);
    iowhite = WhitePixel(iodisplay, ioscreen);
    ioblack = BlackPixel(iodisplay, ioscreen);


    hint = XAllocSizeHints();

    hint->x = 0;
    hint->y = 0;
    hint->width = DE->windowWidth;
    hint->height = DE->windowHeight;
    hint->flags = PPosition | PSize;
    /*
     * not necessary?
    hints.flags = InputHint;
    hints.input = True;
    */
    root = DefaultRootWindow(iodisplay);

#ifdef USE_Xt
    {
	XtAppContext app_con;
	Widget toplevel;
	Display *xtdpy;
        int zero = 0;

	XtToolkitInitialize();
	app_con = XtCreateApplicationContext();
	/* XtAppSetFallbackResources(app_con, x_fallback_resources);*/
	xtdpy = XtOpenDisplay(app_con, NULL, "r_dataentry", "R_dataentry",
			      NULL, 0, &zero, NULL);
	toplevel = XtAppCreateShell(NULL, "R_dataentry",
				    applicationShellWidgetClass,
				    xtdpy, NULL, 0);
	XtGetApplicationResources(toplevel, (XtPointer) &xdev,
				  x_resources,
				  x_resource_count,
				  NULL, 0);
	XtDestroyWidget(toplevel);
	XtCloseDisplay(xtdpy);
	XtDestroyApplicationContext(app_con);
	if (xdev.geometry != NULL) {
	    char gstr[40];
	    int bitmask;

	    sprintf(gstr, "%dx%d+%d+%d", hint->width,
		    hint->height, hint->x, hint->y);
	    bitmask = XWMGeometry(iodisplay, DefaultScreen(iodisplay),
				  xdev.geometry, gstr,
				  1,
				  hint,
				  &hint->x, &hint->y,
				  &hint->width, &hint->height,
				  &hint->win_gravity);

	    if (bitmask & (XValue | YValue))
		hint->flags |= USPosition;
	    if (bitmask & (WidthValue | HeightValue))
		hint->flags |= USSize;
	}
	ioblack = xdev.foreground;
	iowhite = xdev.background;
    }
#endif
    if ((DE->iowindow = XCreateSimpleWindow(
	     iodisplay,
	     root,
	     hint->x,
	     hint->y,
	     hint->width,
	     hint->height,
	     DE->bwidth,
	     ioblack,
	     iowhite)) == 0) {
	warning("unable to open window for data editor");
	return TRUE;
    }

    /*
    XSetStandardProperties(iodisplay, DE->iowindow, ioname, ioname, None,
			   (char **)NULL, 0, iohint);
    */
    XSetWMNormalHints(iodisplay, DE->iowindow, hint);
    XFree(hint);


    winattr.backing_store = WhenMapped;
    XChangeWindowAttributes(iodisplay, DE->iowindow, CWBackingStore,
			    &winattr);

    /* set up protocols so that window manager sends */
    /* me an event when user "destroys" window */
    if(!_XA_WM_PROTOCOLS)
	_XA_WM_PROTOCOLS = XInternAtom(iodisplay, "WM_PROTOCOLS", 0);
    DE->prot = XInternAtom(iodisplay, "WM_DELETE_WINDOW", 0);
    XSetWMProtocols(iodisplay, DE->iowindow, &DE->prot, 1);
    /*
     * not necessary
    XSetWMHints(iodisplay, DE->iowindow, &hints);
     */

    DE->iogc = XCreateGC(iodisplay, DE->iowindow, 0, 0);

#ifdef USE_FONTSET
    if(mbcslocale) {
	ioim = XOpenIM(iodisplay, NULL, NULL, NULL);
	if(!ioim) {
	    XDestroyWindow(iodisplay, DE->iowindow);
	    XCloseDisplay(iodisplay);
	    warning("unable to open X Input Method");
	    return TRUE;
	}

	/* search supported input style */
	XGetIMValues(ioim, XNQueryInputStyle, &ioim_styles,NULL);
	for(i = 0; i < ioim_styles->count_styles; i++) {
	    for(j = 0; preedit_styles[j]; j++){
		for(k = 0; status_styles[k]; k++){
		    ioim_style = (preedit_styles[j] | status_styles[k]);
		    if( ioim_styles->supported_styles[i] == ioim_style) {
			goto loop_out;
		    }
		}
	    }
	}
    loop_out:

	/* create input context */
	xpoint.x = 0; xpoint.y=0;
	xva_nlist = XVaCreateNestedList(0, XNFontSet, font_set,
					XNSpotLocation, &xpoint, NULL);

	ioic = XCreateIC(ioim,
 			 XNInputStyle, ioim_style,
			 XNClientWindow,DE->iowindow,
			 XNFocusWindow,DE->iowindow,
			 XNPreeditAttributes, xva_nlist,
			 XNStatusAttributes, xva_nlist,
			 NULL);
	XFree(xva_nlist);
	if(!ioic) {
	    XCloseIM(ioim);
	    XDestroyWindow(iodisplay, DE->iowindow);
	    XCloseDisplay(iodisplay);
	    warning("unable to open X Input Context");
	    return TRUE;
	}

	/* get XIM processes event. */
	XGetICValues(ioic, XNFilterEvents, &fevent, NULL);
    }
#endif

#ifdef USE_FONTSET
    if(!mbcslocale)
#endif
	XSetFont(iodisplay, DE->iogc, DE->font_info->fid);
    XSetBackground(iodisplay, DE->iogc, iowhite);
    XSetForeground(iodisplay, DE->iogc, ioblack);
    XSetLineAttributes(iodisplay, DE->iogc, 1, LineSolid, CapRound,
		       JoinRound);

    /*
    XSelectInput(iodisplay, DE->iowindow,
		 ButtonPressMask | KeyPressMask
		 | ExposureMask | StructureNotifyMask | fevent);
    */

    XSelectInput(iodisplay, DE->iowindow,
		 ButtonPressMask
		 | KeyPressMask
		 | StructureNotifyMask
		 | ExposureMask
		 | EnterWindowMask
		 | LeaveWindowMask
		 | fevent);
    XMapRaised(iodisplay, DE->iowindow);

    /* now set up the menu-window, for now use the same text
       dimensions as above */

    /* font size consideration */
    for(i = 0; i < (sizeof(menu_label)/sizeof(char *)); i++)
	twidth = (twidth<textwidth(DE, menu_label[i],strlen(menu_label[i]))) ?
	    textwidth(DE, menu_label[i],strlen(menu_label[i])) : twidth;

    menuwindow = XCreateSimpleWindow(iodisplay, root, 0, 0, twidth,
				     4 * DE->box_h, 2, ioblack, iowhite);
    for (i = 0; i < 4; i++) {
	menupanes[i] = XCreateSimpleWindow(iodisplay, menuwindow, 0,
					   DE->box_h * i, twidth, DE->box_h,
					   1, ioblack, iowhite);
	XSelectInput(iodisplay, menupanes[i],
		     ButtonPressMask | ButtonReleaseMask | ExposureMask
		     );
    }

    /* XMapSubwindows(iodisplay, menuwindow); */


    XStoreName(iodisplay, DE->iowindow, title);
    winattr.override_redirect = True;
    XChangeWindowAttributes(iodisplay, menuwindow,
			    CWBackingStore | CWOverrideRedirect, &winattr);
    Rsync(DE);

    /* this next sequence makes sure the window is up and ready before
       you start drawing in it */

    XNextEvent(iodisplay, &ioevent);
    if (ioevent.xany.type == Expose) {
	while (ioevent.xexpose.count)
	    XNextEvent(iodisplay, &ioevent);
    }
    XGetWindowAttributes(iodisplay, DE->iowindow, &attribs);
    DE->bwidth = attribs.border_width;
    DE->fullwindowWidth = attribs.width;
    DE->fullwindowHeight = attribs.height;


    /* set the active rectangle to be the upper left one */
    DE->crow = 1;
    DE->ccol = 1;
    CellModified = FALSE;
    XSaveContext(iodisplay, DE->iowindow, deContext, (caddr_t) DE);
    return FALSE;/* success */
}

/* MAC/X11 BASICS */

static void bell()
{
    XBell(iodisplay, 20);
}

static void cleararea(DEstruct DE, int xpos, int ypos, int width, int height)
{
    XClearArea(iodisplay, DE->iowindow, xpos, ypos, width, height, 0);
}

static void clearwindow(DEstruct DE)
{
    XClearWindow(iodisplay, DE->iowindow);
}

static void copyarea(DEstruct DE, int src_x, int src_y, int dest_x, int dest_y)
{
    int mx = max(src_x, dest_x), my = max(src_y, dest_y);
    XCopyArea(iodisplay, DE->iowindow, DE->iowindow, DE->iogc,
	      src_x, src_y,
	      DE->fullwindowWidth - mx, DE->fullwindowHeight - my,
	      dest_x, dest_y);
    Rsync(DE);
}

static void copyH(DEstruct DE, int src_x, int dest_x, int width)
{
    XCopyArea(iodisplay, DE->iowindow, DE->iowindow, DE->iogc,
	      src_x+DE->bwidth, DE->hht,
	      width, DE->windowHeight+1, dest_x+DE->bwidth, DE->hht);
}

static void drawrectangle(DEstruct DE,
			  int xpos, int ypos, int width, int height,
			  int lwd, int fore)
{
#ifdef USE_Xt
    if (fore == 0)
	XSetForeground(iodisplay, DE->iogc, xdev.background);
    else
	XSetForeground(iodisplay, DE->iogc, xdev.foreground);
#else
    if (fore == 0)
	XSetForeground(iodisplay, DE->iogc,
		       WhitePixel(iodisplay, DefaultScreen(iodisplay)));
    else
	XSetForeground(iodisplay, DE->iogc,
		       BlackPixel(iodisplay, DefaultScreen(iodisplay)));
#endif
    XSetLineAttributes(iodisplay, DE->iogc, lwd, LineSolid,
		       CapRound, JoinRound);
    XDrawRectangle(iodisplay, DE->iowindow, DE->iogc, xpos, ypos,
		   width, height);
}

static void drawtext(DEstruct DE, int xpos, int ypos, char *text, int len)
{
#ifdef USE_FONTSET
    if(mbcslocale)
#ifdef HAVE_XUTF8DRAWIMAGESTRING
        if(utf8locale)
	    Xutf8DrawImageString(iodisplay, DE->iowindow, font_set,
				 DE->iogc, xpos, ypos,text, len);
        else
#endif
	    XmbDrawImageString(iodisplay, DE->iowindow, font_set,
			       DE->iogc, xpos, ypos,text, len);
    else
#endif
	XDrawImageString(iodisplay, DE->iowindow, DE->iogc,
			 xpos, ypos, text, len);
    Rsync(DE);
}

static void Rsync(DEstruct DE)
{
    XSync(iodisplay, 0);
}

static int textwidth(DEstruct DE, char *text, int nchar)
{

#ifdef USE_FONTSET
    if(mbcslocale) {
#ifdef HAVE_XUTF8TEXTESCAPEMENT
        if (utf8locale)
	    return Xutf8TextEscapement(font_set, text, nchar);
        else
#endif
	    return XmbTextEscapement(font_set, text, nchar);
    }
#endif
    return XTextWidth(DE->font_info, text, nchar);
}

/* Menus */

void popupmenu(DEstruct DE, int x_pos, int y_pos, int col, int row)
{
    int i, button, popupcol = col + DE->colmin - 1;
    char *name, clab[20];
    XEvent event;
    Window selected_pane;
    SEXP tvec;

    XMoveWindow(iodisplay, menuwindow, x_pos, y_pos);
    XMapSubwindows(iodisplay, menuwindow);
    XMapRaised(iodisplay, menuwindow);

    /* now fill in the menu panes with the correct information */

    if (popupcol > DE->xmaxused) {
	/* extend work, names and lens */
	REPROTECT(DE->work = lengthgets(DE->work, popupcol), DE->wpi);
	REPROTECT(DE->names = lengthgets(DE->names, popupcol), DE->npi);
	for (i = DE->xmaxused+1; i < popupcol; i++) {
	    sprintf(clab, "var%d", i + 1);
	    SET_STRING_ELT(DE->names, i, mkChar(clab));
	}
	REPROTECT(DE->lens = lengthgets(DE->lens, popupcol), DE->lpi);
	DE->xmaxused = popupcol;
    }
    tvec = VECTOR_ELT(DE->work, popupcol - 1);
    name = CHAR(STRING_ELT(DE->names, popupcol - 1));
#ifdef USE_FONTSET
    if(mbcslocale)
#ifdef HAVE_XUTF8DRAWSTRING
        if(utf8locale)
	    Xutf8DrawString(iodisplay,
			    menupanes[0],
			    font_set, DE->iogc, 3, DE->box_h - 3, name,
			    strlen(name));
	else
#endif
	    XmbDrawString(iodisplay,
			  menupanes[0],
			  font_set, DE->iogc, 3, DE->box_h - 3, name,
			  strlen(name));
    else
#endif
        XDrawString(iodisplay,
		    menupanes[0], DE->iogc, 3, DE->box_h - 3, name,
		    strlen(name));
    for (i = 1; i < 4; i++)
#ifdef USE_FONTSET
      if(mbcslocale)
#ifdef HAVE_XUTF8DRAWSTRING
        if(utf8locale)
	  Xutf8DrawString(iodisplay,
			  menupanes[i],
			  font_set, DE->iogc, 3, DE->box_h - 3,
			  menu_label[i - 1], strlen(menu_label[i - 1]));
	else
#endif
	  XmbDrawString(iodisplay,
			menupanes[i],
			font_set, DE->iogc, 3, DE->box_h - 3,
			menu_label[i - 1], strlen(menu_label[i - 1]));
      else
#endif
	XDrawString(iodisplay,
		    menupanes[i], DE->iogc, 3, DE->box_h - 3,
		    menu_label[i - 1], strlen(menu_label[i - 1]));

    if (isNull(tvec) || TYPEOF(tvec) == REALSXP)
#ifdef USE_FONTSET
      if(mbcslocale)
#ifdef HAVE_XUTF8DRAWSTRING
        if(utf8locale)
	  Xutf8DrawString(iodisplay,
			  menupanes[1],
			  font_set, DE->iogc, 0, DE->box_h - 3,
			  "*", 1);
	else
#endif
	  XmbDrawString(iodisplay,
			menupanes[1],
			font_set, DE->iogc, 0, DE->box_h - 3,
			"*", 1);
      else
#endif
	XDrawString(iodisplay, menupanes[1], DE->iogc, 0, DE->box_h - 3,
		    "*", 1);
    else
#ifdef USE_FONTSET
      if(mbcslocale)
#ifdef HAVE_XUTF8DRAWSTRING
        if(utf8locale)
	  Xutf8DrawString(iodisplay,
			  menupanes[2],
			  font_set, DE->iogc, 0, DE->box_h - 3,
			  "*", 1);
	else
#endif
	  XmbDrawString(iodisplay,
			menupanes[2],
			font_set, DE->iogc, 0, DE->box_h - 3,
			"*", 1);
      else
#endif
	XDrawString(iodisplay, menupanes[2], DE->iogc, 0, DE->box_h - 3,
		    "*", 1);

/*
  start an event loop; we're looking for a button press and a button
  release in the same window
*/

    while (1) {
	XNextEvent(iodisplay, &event);

	/* event is processed with input method */

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
		    if (isNull(tvec))
			SET_VECTOR_ELT(DE->work, popupcol - 1,
				       ssNewVector(REALSXP, 100));
		    else
			SET_VECTOR_ELT(DE->work, popupcol - 1,
				       coerceVector(tvec, REALSXP));
		    goto done;
		case 2:
		    if (isNull(tvec))
			SET_VECTOR_ELT(DE->work, popupcol - 1,
				       ssNewVector(STRSXP, 100));
		    else {
			SET_VECTOR_ELT(DE->work, popupcol - 1,
				       coerceVector(tvec, STRSXP));
		    }

		    goto done;
		case 3:
		    closerect(DE);
		    DE->ccol = col;
		    DE->crow = 0;
		    clearrect(DE);
		    goto done;
		}
	    }
	}
        /* this doesn't work and perhaps I should move it up to the
           main control loop */
	else if (event.type == Expose) {
	    if (event.xexpose.window == menuwindow) {
		XDrawString(iodisplay, menupanes[0], DE->iogc, 3,
			    DE->box_h - 3, name, strlen(name));
		for (i = 1; i < 4; i++)
		    XDrawString(iodisplay, menupanes[i], DE->iogc, 3,
				DE->box_h - 3,
				menu_label[i - 1], strlen(menu_label[i - 1]));
	    }
	}
    }
 done:
    popdownmenu(DE);
    highlightrect(DE);
}

void popdownmenu(DEstruct DE)
{
    XUnmapWindow(iodisplay, menuwindow);
    XUnmapSubwindows(iodisplay, menuwindow);
}

static void copycell(DEstruct DE)
{
  /*
   * whichrow = crow + colmin - 1 => whichrow = crow + rowmin - 1
   *                   ^^^                             ^^^
   */
    int i, whichrow = DE->crow + DE->rowmin - 1,
	whichcol = DE->ccol + DE->colmin -1;
    SEXP tmp;

    if (whichrow == 0) {
	/* won't have  cell here */
    } else {
	strcpy(copycontents, "");
	if (length(DE->work) >= whichcol) {
	    tmp = VECTOR_ELT(DE->work, whichcol - 1);
	    if (tmp != R_NilValue &&
		(i = whichrow - 1) < LENGTH(tmp) ) {
		PrintDefaults(R_NilValue);
                if (TYPEOF(tmp) == REALSXP) {
			strncpy(copycontents, EncodeElement(tmp, i, 0, '.'),
				BOOSTED_BUF_SIZE-1);
			copycontents[BOOSTED_BUF_SIZE-1]='\0';
                } else if (TYPEOF(tmp) == STRSXP) {
                    if (STRING_ELT(tmp, i) != ssNA_STRING) {
			strncpy(copycontents, EncodeElement(tmp, i, 0, '.'),
				BOOSTED_BUF_SIZE-1);
			copycontents[BOOSTED_BUF_SIZE-1]='\0';
		    }
		}
	    }
	}
    }
    highlightrect(DE);
}

static void pastecell(DEstruct DE, int row, int col)
{
    downlightrect(DE);
    DE->crow = row; DE->ccol = col;
    if (strlen(copycontents)) {
	strcpy(buf, copycontents);
	clength = strlen(copycontents);
	bufp = buf + clength;
	CellModified = TRUE;
    }
    closerect(DE);
    highlightrect(DE);
}

#ifdef USE_FONTSET
static void calc_pre_edit_pos(DEstruct DE)
{
    XVaNestedList   xva_nlist;
    XPoint          xpoint;
    int i;
    int w;

    xpoint.x = DE->boxw[0];
    for (i = 1; i < DE->ccol; i++)
	xpoint.x += BOXW(DE->colmin + i - 1);
#ifdef HAVE_XUTF8TEXTESCAPEMENT
    if(utf8locale)
	w = Xutf8TextEscapement(font_set, buf, clength);
    else
#endif
	w = XmbTextEscapement(font_set, buf, clength);
    xpoint.x += (w > BOXW(DE->colmin + DE->ccol - 1)) ?
	BOXW(DE->colmin + DE->ccol - 1) : w;
    xpoint.x += DE->text_offset;
    xpoint.y = DE->hht + (DE->crow+1) * DE->box_h - DE->text_offset;

    /*
      <FIXME>
      I may depend on implementation of XIM, but I do not obey,
      setting value, and investigation in various implementation
      system is need.
      It is only a problem in an appearance.
    */
    xva_nlist = XVaCreateNestedList(0,
				    XNSpotLocation, &xpoint,
				    XNFontSet, font_set,
				    NULL);
    XSetICValues(ioic, XNPreeditAttributes, xva_nlist, NULL);

    XFree(xva_nlist);
    return;
}
#endif

/* last character bytes */
static int last_wchar_bytes(char *str)
{
#ifdef USE_FONTSET
    wchar_t   wcs[BOOSTED_BUF_SIZE];
    mbstate_t mb_st;
    int cnt;
    char last_mbs[8];
    char *mbs;
    size_t bytes;

    mbs = (str == NULL) ? buf : str;

    memset(wcs, 0 ,sizeof(wcs));
    memset(&mb_st,0, sizeof(mbstate_t));

    if((size_t)-1 == (cnt = mbsrtowcs(wcs, (const char **)&mbs,
				      strlen(mbs), &mb_st))) {
        return 0;
    }
    if(wcs[0] == L'\0') return 0;

    memset(last_mbs, 0, sizeof(last_mbs));
    bytes = wcrtomb(last_mbs, wcs[cnt-1], &mb_st); /* -Wall */
    return(bytes);
#else
    return(1);
#endif
}
