/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file consolestructs.h
 *  Copyright (C) 2004	      The R Foundation
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

/* xbuf */

typedef unsigned int xint;
typedef unsigned long xlong;

struct structXBUF {
    xint  ns, ms, shift;
    xlong dim, av;
    char *b, **s, *free;
    int  *user;
};

typedef struct structXBUF *xbuf;

/* console */

struct structConsoleData {
    int   kind;			/* console or pager */
    int   rows, cols;		/* dimension in char */
    int   w, h;			/* dimensions in pixel */

    font  f;			/* font */
    int   fw, fh;
    int   top, right;           /* borders */
    rgb   bg, fg, ufg;		/* colours */
    int   fv, fc;		/* first line and first char visible */
    int   r, c;			/* cursor position */
    int   overwrite;		/* overwrite mode */
    int   sel, mx0, my0, mx1, my1;	/* selection */
    xbuf  lbuf;			/* lines buffer */
    int   firstkey, numkeys;	/* keys buffer */
    char *kbuf;
    int   already;              /* number of keys in buffer to be processed
				   before clipb. */
    char *clp;                  /* data from the clipboard */
    int  pclp;

    int   lazyupdate, needredraw, newfv, newfc;	/* updating and redrawing */
    bitmap bm;

    int   input, cur_pos, cur_byte, max_pos, max_byte, 
	prompt_len, prompt_wid; /* editing */

    char  chbrk, modbrk;	/* hook for user's break */
    void  (*fbrk) ();

    menuitem mcopy, mpaste, mpastecmds, mpopcopy, mpoppaste, mpoppastecmds;
};

typedef struct structConsoleData *ConsoleData;
#define CONSOLE 1
#define PAGER 2
#define DATAEDITOR 3

#define BM  (p->bm)
#define ROWS (p->rows)
#define COLS (p->cols)
#define WIDTH (p->w)
#define HEIGHT (p->h)
#define BORDERX (p->right)
#define BORDERY (p->top)
#define FW (p->fw)
#define FH (p->fh)
#define FV (p->fv)
#define FC (p->fc)
#define NEWFV (p->newfv)
#define NEWFC (p->newfc)
#define NUMLINES (p->lbuf->ns)
#define LINE(i)  (p->lbuf->s[i])
#define USER(i)  (p->lbuf->user[i])
#define VLINE(i) ((strlen(LINE(i))>FC) ? &LINE(i)[FC] : "")
#define RLINE(i) (rect(0, BORDERY + (i)*FH, WIDTH, FH))
#define RMLINES(i,j) (rect(0, BORDERY + (i)*FH, WIDTH, (j-i+1)*FH))
#define cur_byte (p->cur_byte)
#define max_byte (p->max_byte)
#define cur_pos (p->cur_pos)
#define prompt_len (p->prompt_len)
#define prompt_wid (p->prompt_wid)
#define CURCOL  (p->c)  /* column of cursor on whole line */

#define WRITELINE(i, j) writeline(p, i, j)

#define REDRAW drawconsole(c, getrect(c))


#define RSHOW(r) {gbitblt(c, p->bm, topleft(r), r);}

ConsoleData newconsoledata(font f, int rows, int cols,
    int bufbytes, int buflines,
    rgb fg, rgb ufg, rgb bg, int kind);

void freeConsoleData(ConsoleData p);
void setfirstvisible(control c, int fv);
void setfirstcol(control c, int newcol);
void console_sbf(control c, int pos);
void console_mousedrag(control c, int button, point pt);
void console_mouserep(control c, int button, point pt);
void console_mousedown(control c, int button, point pt);
void consoleresize(console c, rect r);
void console_ctrlkeyin(control c, int key);
void console_normalkeyin(control c, int k);

font consolefn;
int fontsty, pointsize;
int consoler, consolec;
int pagerrow, pagercol;
rgb consolebg, consolefg, consoleuser, pagerhighlight;

#define DIMLBUF 64*1024         /* console buffer size in chars */
#define MLBUF   8*1024          /* console buffer size in lines */
#define SLBUF   512             /* console buffer shift in lines */
#define NKEYS   512		/* 8Kb paste buffer */
#define TABSIZE 8

xbuf newxbuf(xlong dim, xint ms, xint shift);
void xbufgrow(xbuf p, xlong dim, xint ms);
void xbufdel(xbuf p);
void xbufaddc(xbuf p, char c);



