/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995  Robert Gentleman and Ross Ihaka
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


#include "wincons.h"
#include "Graphics.h"
#include "Print.h"

#define RDD_NUM 21
#define RDD_CHAR        22
#define RDD_NAME        23
 
enum { UP, DOWN, LEFT, RIGHT };

/* Local Function Definitions */
 
static void advancerect(int);
static void clearrect();
static void clearwindow();
static void closerect();
static void CloseDE();

static void doDEKey(WPARAM);
static void downlightrect();
static void drawwindow();
static void drawcol(int);
static void find_coords(int, int, int*, int*);
static int  findsquare();
static void handlechar(WPARAM);
static void highlightrect();
static int  initwin();
static void jumppage(int);
static void jumpwin(int, int);
static int  nchars(char*, int);
static void popdownmenu();
static void popupmenu(int);
static void printlabs();
static void printrect(int);
static void printstring(char*, int, int, int);
static void printelt(SEXP, int, int, int);
static void querypointer(int*, int*, int*, int*);
 
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
static int nneg;                        /* indicate whether its a negative */
static int clength;                     /* number of characters currently entered */
static char buf[30];
static char *bufp;
static int text_offset;
static char digits[] = "123456789.0";
static int doneSpread=1;
 
/* Last Point Coordinates */
static int xlast = 0;
static int ylast = 0;
 
/* Functions to hide Xlib calls */
 
static void cleararea(int, int, int, int);
static void drawline(int, int, int, int);
static void drawrectangle(int, int, int, int);
static void drawtext(int, int, char*, int);
static void setforeground(int);
static void setattribsfromwindow();
static void setlineattribs( int);
static int textwidth(char*, int);
 
/* R Expression Global variables */
 
static SEXP inputlist;  /* each element is a vector for that row */
SEXP listAppend(SEXP, SEXP);
SEXP ssNewVector(SEXPTYPE, int);
static SEXP ssNA_STRING;
static double ssNA_REAL;
 
/*double atof();*/
