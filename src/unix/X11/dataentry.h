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

#define NeedFunctionPrototypes 0

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>

#define KeySym int
#define DEEvent XEvent

enum { UP, DOWN, LEFT, RIGHT };

/* Local Function Definitions */
 
static void advancerect(int);
static int  CheckControl(DEEvent*);
static int  CheckShift(DEEvent*);
static int  checkquit(int);
static void clearrect();
static void clearwindow(void);
static void closerect();
static void closewin();
static void copycell();
static void doControl(DEEvent*);
static int  doMouseDown(DEEvent*);
static void eventloop();
static void doSpreadKey(int, DEEvent*);
static void downlightrect();
static void drawwindow();
static void drawcol(int);
static void drawrow(int);
static void find_coords(int, int, int*, int*);
static int  findcell();
static char GetCharP(DEEvent*);
static KeySym GetKey(DEEvent*);
static void handlechar(char*);
static void highlightrect();
static int  initwin();
static void jumppage(int);
static void jumpwin(int, int);
static void pastecell(int, int);
static void popdownmenu();
static void popupmenu(int, int, int, int);
static void printlabs();
static void printrect(int, int);
static void printstring(char*, int, int, int, int);
static void printelt(SEXP, int, int, int);
static void RefreshKeyboardMapping(DEEvent*);
 
/* Global variables needed for the graphics */
 
static int box_w;                       /* width of a box */
static int boxw[100];
static int box_h;                       /* height of a box */
static int windowWidth;                 /* current width of the window */
static int fullwindowWidth;
static int windowHeight;                /* current height of the window */
static int fullwindowHeight;
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
static int text_offset;
 
/* Xwindows Globals */
 
static Display          *iodisplay;
static Window           iowindow, menuwindow, menupanes[4];
static GC               iogc;
static XSizeHints       iohint;
static Cursor           hand_cursor;
static char             *font_name="9x15";
static XFontStruct      *font_info;

#define mouseDown 	ButtonPress
#define keyDown		KeyPress
#define activateEvt	MapNotify
#define updateEvt	Expose
 
/* Functions to hide Xlib calls */
 
static void bell();
static void cleararea(int, int, int, int);
static void copyH(int, int, int);
static void copyarea(int, int, int, int);
static void doConfigure(DEEvent *ioevent);
/* static void drawline(int, int, int, int); */
static void drawrectangle(int, int, int, int, int, int);
static void drawtext(int, int, char*, int);
static int  NextEvent(DEEvent *ioevent);
static void RefreshKeyboardMapping(DEEvent *ioevent);
static void Rsync();
static int textwidth(char*, int);
static int WhichEvent(DEEvent ioevent);

 
/* R Expression Global variables */
 
static SEXP inputlist;  /* each element is a vector for that row */
SEXP listAppend(SEXP, SEXP);
static SEXP ssNewVector(SEXPTYPE, int);
static SEXP ssNA_STRING;
static double ssNA_REAL;
