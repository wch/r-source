/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file console.c
 *  Copyright (C) 1998--2000  Guido Masarotto and Brian Ripley
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

#ifdef Win32
#define USE_MDI 1
#endif

#include <windows.h>
#include <string.h>
#include <ctype.h>
#include "graphapp/ga.h"
#ifdef USE_MDI
#include "graphapp/stdimg.h"
#endif
#include "console.h"
#include "consolestructs.h"
#include "rui.h"


/* xbuf */

xbuf newxbuf(xlong dim, xint ms, xint shift)
{
    xbuf  p;

    p = (xbuf) winmalloc(sizeof(struct structXBUF));
    if (!p)
	return NULL;
    p->b = (char *) winmalloc(dim + 1);
    if (!p->b) {
	winfree(p);
	return NULL;
    }
    p->user = (int *) winmalloc(ms * sizeof(int));
    if (!p->user) {
	winfree(p->b);
	winfree(p);
	return NULL;
    }
    p->s = (char **) winmalloc(ms * sizeof(char *));
    if (!p->s) {
	winfree(p->b);
	winfree(p->user);
	winfree(p);
	return NULL;
    }
    p->ns = 1;
    p->ms = ms;
    p->shift = shift;
    p->dim = dim;
    p->av = dim;
    p->free = p->b;
    p->s[0] = p->b;
    p->user[0] = -1;
    *p->b = '\0';
    return p;
}

void xbufdel(xbuf p) 
{
   if (!p) return;
   winfree(p->s);
   winfree(p->b);
   winfree(p->user);
   winfree(p);
}

static void xbufshift(xbuf p)
{
    xint  i;
    xlong mshift;
    char *new0;

    if (p->shift >= p->ns) {
	p->ns = 1;
	p->av = p->dim;
	p->free = p->b;
	p->s[0] = p->b;
	*p->b = '\0';
	p->user[0] = -1;
	return;
    }
    new0 = p->s[p->shift];
    mshift = new0 - p->s[0];
    memmove(p->b, p->s[p->shift], p->dim - mshift);
    memmove(p->user, &p->user[p->shift], (p->ms - p->shift) * sizeof(int));
    for (i = p->shift; i < p->ns; i++)
	p->s[i - p->shift] = p->s[i] - mshift;
    p->ns = p->ns - p->shift;
    p->free -= mshift;
    p->av += mshift;
}

static int xbufmakeroom(xbuf p, xlong size)
{
    if (size > p->dim) return 0;
    while ((p->av < size) || (p->ns == p->ms)) {
	xbufshift(p);
    }
    p->av -= size;
    return 1;
}

#define XPUTC(c) {xbufmakeroom(p,1); *p->free++=c;}

void xbufaddc(xbuf p, char c)
{
    int   i;

    switch (c) {
      case '\a':
	gabeep();
	break;
      case '\b':
	if (strlen(p->s[p->ns - 1])) {
	    p->free--;
	    p->av++;
	}
	break;
      case '\r':
	break;
      case '\t':
	XPUTC(' ');
	*p->free = '\0';
	for (i = strlen(p->s[p->ns - 1]); (i % TABSIZE); i++)
	    XPUTC(' ');
	break;
      case '\n':
	XPUTC('\0');
	p->s[p->ns] = p->free;
	p->user[p->ns++] = -1;
	break;
      default:
	XPUTC(c);
    }
    *p->free = '\0';
}

static void xbufadds(xbuf p, char *s, int user)
{
    char *ps;
    int   l;

    l = user ? strlen(p->s[p->ns - 1]) : -1;
    for (ps = s; *ps; ps++)
	xbufaddc(p, *ps);
    p->user[p->ns - 1] = l;
}

static void xbuffixl(xbuf p)
{
    char *ps, *old;

    if (!p->ns)
	return;
    ps = p->s[p->ns - 1];
    old = p->free;
    p->free = ps + strlen(ps);
    p->av = p->dim - (p->free - p->b);
}


/* console */

rgb consolebg = White, consolefg = Black, consoleuser = Red,
    pagerhighlight = Red;

ConsoleData
newconsoledata(font f, int rows, int cols,
	       rgb fg, rgb ufg, rgb bg, int kind)
{
    ConsoleData p;

    initapp(0, 0);
    p = (ConsoleData) winmalloc(sizeof(struct structConsoleData));
    if (!p)
	return NULL;
    p->kind = kind;
    if (kind == CONSOLE) {
	p->lbuf = newxbuf(DIMLBUF, MLBUF, SLBUF);
	if (!p->lbuf) {
	    winfree(p);
	    return NULL;
	}
	p->history = newxbuf(DIMHIST, MHIST, SHIST);
	if (!p->history) {
	    xbufdel(p->lbuf);
	    winfree(p);
	    return NULL;
	}
	p->kbuf = winmalloc(NKEYS * sizeof(char));
	if (!p->kbuf) {
	    xbufdel(p->lbuf);
	    xbufdel(p->history);
	    winfree(p);
	    return NULL;
	}
    } else {
	p->lbuf = NULL;
	p->history = NULL;
	p->kbuf = NULL;
    }
    p->bm = NULL;
    p->rows = rows;
    p->cols = cols;
    p->fg = fg;
    p->bg = bg;
    p->ufg = ufg;
    p->f = f;
    FH = fontheight(f);
    FW = fontwidth(f);
    WIDTH = (COLS + 1) * FW;
    HEIGHT = (ROWS + 1) * FH + 1; /* +1 avoids size problems in MDI */
    FV = FC = 0;
    p->newfv = p->newfc = 0;
    p->firstkey = p->numkeys = 0;
    p->clp = NULL;
    p->r = -1;
    p->lazyupdate = 1;
    p->needredraw = 0;
    p->my0 = p->my1 = -1;
    p->mx0 = 5;
    p->mx1 = 14;
    p->sel = 0;
    return (p);
}

static void writelineHelper(ConsoleData p, int fch, int lch,
			    rgb fgr, rgb bgr, int j, int len, char *s)
{
    rect  r;
    char  chf, chl, ch;
    int   last;

    r = rect(BORDERX + fch * FW, BORDERY + j * FH, (lch - fch + 1) * FW, FH);
    gfillrect(p->bm, bgr, r);

    if (len > fch) {
	if (FC && (fch == 0)) {
	    chf = s[0];
	    s[0] = '$';
	} else
	    chf = '\0';
	if ((len > COLS) && (lch == COLS - 1)) {
	    chl = s[lch];
	    s[lch] = '$';
	} else
	    chl = '\0';
	last = lch + 1;
	if (len > last) {
	    ch = s[last];
	    s[last] = '\0';
	} else
	    ch = '\0';
	gdrawstr(p->bm, p->f, fgr, pt(r.x, r.y), &s[fch]);
	if (ch)
	    s[last] = ch;
	if (chl)
	    s[lch] = chl;
	if (chf)
	    s[fch] = chf;
    }
}

#define WLHELPER(a, b, c, d) writelineHelper(p, a, b, c, d, j, len, s)

static int writeline(ConsoleData p, int i, int j)
{
    char *s;
    int   insel, len, col1, d;
    int   c1, c2, c3, x0, y0, x1, y1;

    if ((i < 0) || (i >= NUMLINES))
	FRETURN(0);
    s = VLINE(i);
    len = strlen(s);
    col1 = COLS - 1;
    insel = p->sel ? ((i - p->my0) * (i - p->my1)) : 1;
    if (insel < 0) {
	WLHELPER(0, col1, White, DarkBlue);
	FRETURN(len);
    }
    if ((USER(i) >= 0) && (USER(i) < FC + COLS)) {
	if (USER(i) <= FC)
	    WLHELPER(0, col1, p->ufg, p->bg);
	else {
	    d = USER(i) - FC;
	    WLHELPER(0, d - 1, p->fg, p->bg);
	    WLHELPER(d, col1, p->ufg, p->bg);
	}
    } else if (USER(i) == -2) {
	WLHELPER(0, col1, pagerhighlight, p->bg);
    } else
	WLHELPER(0, col1, p->fg, p->bg);
    if ((p->r >= 0) && (p->c >= FC) && (p->c < FC + COLS) &&
	(i == NUMLINES - 1))
	WLHELPER(p->c - FC, p->c - FC, p->bg, p->ufg);
    if (insel != 0) FRETURN(len);
    c1 = (p->my0 < p->my1);
    c2 = (p->my0 == p->my1);
    c3 = (p->mx0 < p->mx1);
    if (c1 || (c2 && c3)) {
	x0 = p->mx0; y0 = p->my0;
	x1 = p->mx1; y1 = p->my1;
    } else {
	x0 = p->mx1; y0 = p->my1;
	x1 = p->mx0; y1 = p->my0;
    }
    if (i == y0) {
	if (FC + COLS < x0) FRETURN(len);
	c1 = (x0 > FC) ? (x0 - FC) : 0;
    } else
	c1 = 0;
    if (i == y1) {
	if (FC > x1) FRETURN(len);
	c2 = (x1 > FC + COLS) ? (COLS - 1) : (x1 - FC);
    } else
	c2 = COLS - 1;
    WLHELPER(c1, c2, White, DarkBlue);
    return len;
}

void drawconsole(control c, rect r)
FBEGIN
PBEGIN
    int i, ll, wd, maxwd = 0;

    ll = min(NUMLINES, ROWS);
    gfillrect(BM, p->bg, getrect(BM));
    if(!ll) FVOIDRETURN;
    for (i = 0; i < ll; i++) {
	wd = WRITELINE(NEWFV + i, i);
	if(wd > maxwd) maxwd = wd;
    }
    RSHOW(getrect(c));
    FV = NEWFV;
    p->needredraw = 0;
/* always display scrollbar if FC > 0 */
    if(maxwd < COLS - 1) maxwd = COLS -1;
    maxwd += FC;
    gchangescrollbar(c, HWINSB, FC, maxwd, COLS,
                     p->kind == CONSOLE || NUMLINES > ROWS);
    gchangescrollbar(c, VWINSB, FV, NUMLINES - 1 , ROWS, p->kind == CONSOLE);
PEND
FVOIDEND

void setfirstvisible(control c, int fv)
FBEGIN
    int  ds, rw, ww;

    if (NUMLINES <= ROWS) FVOIDRETURN;
    if (fv < 0) fv = 0;
    else if (fv > NUMLINES - ROWS) fv = NUMLINES - ROWS;
    if (fv < 0) fv = 0;
    ds = fv - FV;
    if ((ds == 0) && !p->needredraw) FVOIDRETURN;
    if (abs(ds) > 1) {
        NEWFV = fv;
        REDRAW;
        FVOIDRETURN;
    }
    if (p->needredraw) {
        ww = min(NUMLINES, ROWS) - 1;
        rw = FV + ww;
        writeline(p, rw, ww);
        if (ds == 0) {
	    PBEGIN;
	    RSHOW(RLINE(ww));
	    PEND;
 	    FVOIDRETURN;
        }
    }
    PBEGIN
    if (ds == 1) {
        gscroll(BM, pt(0, -FH), RMLINES(0, ROWS - 1));
        gfillrect(BM, p->bg, RLINE(ROWS - 1));
	WRITELINE(fv + ROWS - 1, ROWS - 1);
    }
    else if (ds == -1) {
        gscroll(BM, pt(0, FH), RMLINES(0, ROWS - 1));
        gfillrect(BM, p->bg, RLINE(0));
	WRITELINE(fv, 0);
    }
    RSHOW(getrect(c));
    PEND
    FV = fv;
    NEWFV = fv;
    p->needredraw = 0;
    gchangescrollbar(c, VWINSB, fv, NUMLINES - 1 , ROWS, p->kind == CONSOLE);
FVOIDEND

void setfirstcol(control c, int newcol)
FBEGIN
    int i, ml, li, ll;

    ll = (NUMLINES < ROWS) ? NUMLINES : ROWS;
    if (newcol > 0) {
	for (i = 0, ml = 0; i < ll; i++) {
 	    li = strlen(LINE(NEWFV + i));
	    ml = (ml < li) ? li : ml;
	}
	ml = ml - COLS;
	ml = 5*(ml/5 + 1);
	if (newcol > ml) newcol = ml;
    }
    if (newcol < 0) newcol = 0;
    FC = newcol;
    REDRAW;
FVOIDEND

void console_mousedrag(control c, int button, point pt)
FBEGIN
    pt.x -= BORDERX;
    pt.y -= BORDERY;
    if (button & LeftButton) {
	int r, s;
	r=((pt.y > 32000) ? 0 : ((pt.y > HEIGHT) ? HEIGHT : pt.y))/FH;
	s=((pt.x > 32000) ? 0 : ((pt.x > WIDTH) ? WIDTH : pt.x))/FW;
	if ((r < 0) || (r > ROWS) || (s < 0) || (s > COLS))
 	    FVOIDRETURN;
	p->my1 = FV + r;
	p->mx1 = FC + s;
	p->needredraw = 1;
	if ((p->mx1 != p->mx0) || (p->my1 != p->my0))
	   p->sel = 1;
	if (pt.y <= 0) setfirstvisible(c, FV - 3);
	else if (pt.y >= ROWS*FH) setfirstvisible(c, FV+3);
	if (pt.x <= 0) setfirstcol(c, FC - 3);
	else if (pt.x >= COLS*FW) setfirstcol(c, FC+3);
	else REDRAW;
    }
FVOIDEND

void console_mouserep(control c, int button, point pt)
FBEGIN
    if ((button & LeftButton) && (p->sel)) console_mousedrag(c, button,pt);
FVOIDEND

void console_mousedown(control c, int button, point pt)
FBEGIN
    pt.x -= BORDERX;
    pt.y -= BORDERY;
    if (p->sel) {
        p->sel = 0;
        p->needredraw = 1;  /* FIXME */
        REDRAW;
    }
    if (button & LeftButton) {
	p->my0 = FV + pt.y/FH;
	p->mx0 = FC + pt.x/FW;
    }
FVOIDEND

void consoletogglelazy(control c)
FBEGIN
    if (p->kind == PAGER) return;
    p->lazyupdate = (p->lazyupdate + 1) % 2;
FVOIDEND

int consolegetlazy(control c)
FBEGIN
FEND(p->lazyupdate)

void consoleflush(control c)
FBEGIN
  REDRAW;
FVOIDEND

#define BEGINLINE 1
#define ENDLINE   2
#define CHARLEFT 3
#define CHARRIGHT 4
#define NEXTHISTORY 5
#define PREVHISTORY 6
#define KILLRESTOFLINE 7
#define BACKCHAR  8
#define DELETECHAR 22 /* ^I is printable in some systems */
#define KILLLINE 21

static void storekey(control c,int k)
FBEGIN
    if (p->kind == PAGER) return;
    if (k == BKSP) k = BACKCHAR;
    if (p->numkeys >= NKEYS) {
	gabeep();
	FVOIDRETURN;
     }
     p->kbuf[(p->firstkey + p->numkeys) % NKEYS] = k;
     p->numkeys++;
FVOIDEND

void consolecmd(control c, char *cmd)
FBEGIN
    char *ch;
    int i;
    if (p->sel) {
	p->sel = 0;
	p->needredraw = 1;
	REDRAW;
    }
    storekey(c, BEGINLINE);
    storekey(c, KILLRESTOFLINE);
    for (ch = cmd; *ch; ch++) storekey(c, *ch);
    storekey(c, '\n');
/* if we are editing we save the actual line */
    if (p->r > -1) {
      ch = &(p->lbuf->s[p->lbuf->ns - 1][prompt_len]);
      for (; *ch; ch++) storekey(c, *ch);
      for (i = max_pos; i > cur_pos; i--) storekey(c, CHARLEFT);
    }
FVOIDEND

/* the following three routines are  system dependent */
void consolepaste(control c)
FBEGIN
    HGLOBAL hglb;
    char *pc, *new = NULL;
    if (p->sel) {
	p->sel = 0;
	p->needredraw = 1;
	REDRAW;
     }
    if (p->kind == PAGER) FVOIDRETURN;
    if ( OpenClipboard(NULL) &&
         (hglb = GetClipboardData(CF_TEXT)) &&
         (pc = (char *)GlobalLock(hglb)))
    {
        if (p->clp) {
           new = winrealloc((void *)p->clp, strlen(p->clp) + strlen(pc) + 1);
        }
        else {
           new = winmalloc(strlen(pc) + 1) ;
           if (new) new[0] = '\0';
           p->already = p->numkeys;
           p->pclp = 0;
        }
        if (new) {
           p->clp = new;
           strcat(p->clp, pc);
        }
        else {
           R_ShowMessage("Not enough memory");
        }
        GlobalUnlock(hglb);
    }
    CloseClipboard();
FVOIDEND

static void consoletoclipboardHelper(control c, int x0, int y0, int x1, int y1)
FBEGIN
    HGLOBAL hglb;
    int ll, i, j;
    char ch, *s;

    i = y0; j = x0; ll = 1;
    while ((i < y1) || ((i == y1) && (j <= x1))) {
	if (LINE(i)[j]) {
	    ll += 1;
	    j += 1;
	}
	else {
	    ll += 2;
	    i += 1;
	    j = 0;
	}
    }
    if (!(hglb = GlobalAlloc(GHND, ll))){
        R_ShowMessage("Insufficient memory: text not moved to the clipboard");
        FVOIDRETURN;
    }
    if (!(s = (char *)GlobalLock(hglb))){
        R_ShowMessage("Insufficient memory: text not moved to the clipboard");
        FVOIDRETURN;
    }
    i = y0; j = x0;
    while ((i < y1) || ((i == y1) && (j <= x1))) {
	ch = LINE(i)[j];
	if (ch) {
 	    *s++ = ch;
	    j += 1;
	} else {
	    *s++ = '\r'; *s++ = '\n';
	    i += 1;
	    j = 0;
	}
    }
    *s = '\0';
    GlobalUnlock(hglb);
    if (!OpenClipboard(NULL) || !EmptyClipboard()) {
        R_ShowMessage("Unable to open the clipboard");
        GlobalFree(hglb);
        FVOIDRETURN;
    }
    SetClipboardData(CF_TEXT, hglb);
    CloseClipboard();
FVOIDEND

/* end of system dependent part */

int consolecanpaste(control c)
{
    return clipboardhastext();
}


int consolecancopy(control c)
FBEGIN
FEND(p->sel)

void consolecopy(control c)
FBEGIN
    if (p->sel) {
	int len, c1, c2, c3;
	int x0, y0, x1, y1;
	if (p->my0 >= NUMLINES) p->my0 = NUMLINES - 1;
	if (p->my0 < 0) p->my0 = 0;
	len = strlen(LINE(p->my0));
	if (p->mx0 >= len) p->mx0 = len - 1;
	if (p->mx0 < 0) p->mx0 = 0;
	if (p->my1 >= NUMLINES) p->my1 = NUMLINES - 1;
	if (p->my1 < 0) p->my1 = 0;
	len = strlen(LINE(p->my1));
	if (p->mx1 >= len) p->mx1 = len - 1;
	if (p->mx1 < 0) p->mx1 = 0;
	c1 = (p->my0 < p->my1);
	c2 = (p->my0 == p->my1);
	c3 = (p->mx0 < p->mx1);
	if (c1 || (c2 && c3)) {
	   x0 = p->mx0; y0 = p->my0;
	   x1 = p->mx1; y1 = p->my1;
	}
	else {
	   x0 = p->mx1; y0 = p->my1;
	   x1 = p->mx0; y1 = p->my0;
	}
	consoletoclipboardHelper(c, x0, y0, x1, y1);
	p->sel = 0;
	REDRAW;
    }
FVOIDEND

void consoleselectall(control c)
FBEGIN
   if (NUMLINES) {
       p->sel = 1;
       p->my0 = p->mx0 = 0;
       p->my1 = NUMLINES - 1;
       p->mx1 = strlen(LINE(p->my1));
       REDRAW;
    }
FVOIDEND

void console_normalkeyin(control c, int k)
FBEGIN
    int st;

    st = ggetkeystate();
    if ((p->chbrk) && (k == p->chbrk) &&
	((!p->modbrk) || ((p->modbrk) && (st == p->modbrk)))) {
	p->fbrk(c);
	return;
    }
    if (st == CtrlKey)
	switch (k + 'A' - 1) {
	case 'A':
	    k = BEGINLINE;
	    break;
	case 'B':
	    k = CHARLEFT;
	    break;
	case 'C':
	    consolecopy(c);
	    st = -1;
	    break;
	case 'D':
	    k = DELETECHAR;
	    break;
	case 'E':
	    k = ENDLINE;
	    break;
	case 'F':
	    k = CHARRIGHT;
	    break;
	case 'K':
	    k = KILLRESTOFLINE;
	    break;
	case 'N':
	    k = NEXTHISTORY;
	    break;
	case 'P':
	    k = PREVHISTORY;
	    break;
	case 'U':
	    k = KILLLINE;
	    break;
	case 'V':
	case 'Y':
	    if(p->kind == PAGER) {
		consolecopy(c); 
		consolepaste(RConsole);
	    }
	    else consolepaste(c);
	    st = -1;
	    break;
	case 'X':
	    consolecopy(c);
	    consolepaste(c);
	    st = -1;
	    break;
	case 'W':
	    consoletogglelazy(c);
	    st = -1;
	    break;
	}
    if (p->sel) {
	p->sel = 0;
	p->needredraw = 1;
	REDRAW;
    }
    if (st == -1) return;
    storekey(c,k);
FVOIDEND

void console_ctrlkeyin(control c, int key)
FBEGIN
    int st;

    st = ggetkeystate();
    if ((p->chbrk) && (key == p->chbrk) &&
	((!p->modbrk) || ((p->modbrk) && (st == p->modbrk)))) {
	p->fbrk(c);
	return;
    }
    switch (key) {
     case PGUP: setfirstvisible(c, NEWFV - ROWS); break;
     case PGDN: setfirstvisible(c, NEWFV + ROWS); break;
     case HOME:
	 if (st == CtrlKey)
	     setfirstvisible(c, 0);
	 else
	     if (p->kind == PAGER)
		 setfirstcol(c, 0);
	     else
		 storekey(c, BEGINLINE);
	 break;
     case END:
	 if (st == CtrlKey)
	     setfirstvisible(c, NUMLINES);
	 else
	     storekey(c, ENDLINE);
	 break;
     case UP:
	 if ((st == CtrlKey) || (p->kind == PAGER))
	     setfirstvisible(c, NEWFV - 1);
	 else
	     storekey(c, PREVHISTORY);
	 break;
     case DOWN:
	 if ((st == CtrlKey) || (p->kind == PAGER))
	     setfirstvisible(c, NEWFV + 1);
	 else
	     storekey(c, NEXTHISTORY);
	 break;
     case LEFT:
	 if ((st == CtrlKey) || (p->kind == PAGER))
	     setfirstcol(c, FC - 5);
	 else
	     storekey(c, CHARLEFT);
	 break;
     case RIGHT:
	 if ((st == CtrlKey) || (p->kind == PAGER))
	     setfirstcol(c, FC + 5);
	 else
	     storekey(c, CHARRIGHT);
	 break;
     case DEL:
	 if (st == CtrlKey)
	     storekey(c, KILLRESTOFLINE);
	 else if (st  ==  ShiftKey)
	     consolecopy(c);
	 else
	     storekey(c, DELETECHAR);
	 break;
     case ENTER:
	 storekey(c, '\n');
	 break;
     case INS:
	 if (st == ShiftKey)
	     consolepaste(c);
	 break;
    }
    if (p->sel) {
	p->sel = 0;
	p->needredraw = 1;
	REDRAW;
    }
FVOIDEND

int consolewrites(control c, char *s)
FBEGIN
    xbufadds(p->lbuf, s, 0);
    FC = 0;
    if (strchr(s, '\n')) p->needredraw = 1;
    if (!p->lazyupdate || (p->r >= 0))
        setfirstvisible(c, NUMLINES - ROWS);
    else {
        p->newfv = NUMLINES - ROWS;
       if (p->newfv < 0) p->newfv = 0;
    }
FEND(0)

void freeConsoleData(ConsoleData p)
{
    if (!p) return;
    if (p->bm) del(p->bm);
    if (p->kind == CONSOLE) {
        if (p->lbuf) xbufdel(p->lbuf);
	if (p->history) xbufdel(p->history);
	if (p->kbuf) winfree(p->kbuf);
    }
    winfree(p);
}

static void delconsole(control c)
{
    freeConsoleData(getdata(c));
}

/* console readline (coded looking to the GNUPLOT 3.5 readline)*/
void R_ProcessEvents();
static char consolegetc(control c)
{
    ConsoleData p;
    char ch;

    p = getdata(c);
    p->c = cur_pos + prompt_len;
    while((p->numkeys == 0) && (!p->clp))
    {
	if (!peekevent()) WaitMessage();
	R_ProcessEvents();
    }
    if (p->sel) {
	p->sel = 0;
	p->needredraw = 1;
	REDRAW;
    }
    if (!p->already && p->clp)
    {
	ch = p->clp[p->pclp++];
	if (!(p->clp[p->pclp])) {
	    winfree(p->clp);
	    p->clp = NULL;
	}
    }
    else
    {
          ch = p->kbuf[p->firstkey];
          p->firstkey = (p->firstkey + 1) % NKEYS;
          p->numkeys--;
          if (p->already) p->already--;
    }
    return ch;
}

static void consoleunputc(control c)
FBEGIN
    p->numkeys += 1;
    if (p->firstkey > 0) p->firstkey -= 1;
    else p->firstkey = NKEYS - 1 ;
FVOIDEND

static void checkvisible(control c)
FBEGIN
    int newfc;

    p->c = cur_pos+prompt_len;
    setfirstvisible(c, NUMLINES-ROWS);
    newfc = 0;
    while ((p->c <= newfc) || (p->c > newfc+COLS-2)) newfc += 5;
    if (newfc != FC) setfirstcol(c, newfc);
FVOIDEND

static void draweditline(control c)
FBEGIN
    checkvisible(c);
    if (p->needredraw) {
        REDRAW;
    }
    else {
        PBEGIN
          WRITELINE(NUMLINES - 1, p->r);
          RSHOW(RLINE(p->r));
        PEND
    }
FVOIDEND

int consolereads(control c, char *prompt, char *buf, int len, int addtohistory)
FBEGIN
    char cur_char;
    char *cur_line;
    int  hentry;
    char *aLine;

    /* print the prompt */
    xbufadds(p->lbuf, prompt, 1);
    if (!xbufmakeroom(p->lbuf, len + 1)) FRETURN(1);
    aLine = p->lbuf->s[p->lbuf->ns - 1];
    prompt_len = strlen(aLine);
    if (NUMLINES > ROWS) {
	p->r = ROWS - 1;
	p->newfv = NUMLINES - ROWS;
    }
    else {
	p->r = NUMLINES - 1;
	p->newfv = 0;
    }
    p->c = prompt_len;
    p->fc = 0;
    cur_pos = 0;
    max_pos = 0;
    cur_line = &aLine[prompt_len];
    cur_line[0] = '\0';
    hentry=-1;
    REDRAW;
    for(;;) {
	char chtype;
	cur_char = consolegetc(c);
	chtype = isprint(cur_char) ||
	    ((unsigned char)cur_char > 0x7f);
	if(chtype && (max_pos < len - 2)) {
	    int i;
	    for(i = max_pos; i > cur_pos; i--) {
		cur_line[i] = cur_line[i - 1];
	    }
	    cur_line[cur_pos] = cur_char;
	    cur_pos += 1;
	    max_pos += 1;
	    cur_line[max_pos] = '\0';
	    draweditline(c);
	} else {
	    /* do normal editing commands */
	    int i;
	    switch(cur_char) {
	    case BEGINLINE:
		cur_pos = 0;
		break;
	    case CHARLEFT:
		if(cur_pos > 0) {
		    cur_pos -= 1;
		}
		break;
	    case ENDLINE:
		cur_pos = max_pos;
		break;
	    case CHARRIGHT:
		if(cur_pos < max_pos) {
		    cur_pos += 1;
		}
		break;
	    case KILLRESTOFLINE:
		max_pos = cur_pos;
		cur_line[max_pos]='\0';
		break;
	    case KILLLINE:
		max_pos = cur_pos = 0;
		cur_line[max_pos]='\0';
		break;
	    case PREVHISTORY:
		hentry += 1;
		if(hentry < NHISTORY) {
		    if (hentry == 0) strcpy(buf, cur_line);
		    strcpy(cur_line, HISTORY(hentry));
		    cur_pos = max_pos = strlen(cur_line);
		}
		else
		    hentry -= 1;
		break;
	    case NEXTHISTORY:
		if(hentry > 0) {
		    hentry -= 1;
		    strcpy(cur_line, HISTORY(hentry));
		}
		else if (hentry == 0) {
		    hentry =- 1;
		    strcpy(cur_line, buf);
		}
		cur_pos = max_pos = strlen(cur_line);
		break;
	    case BACKCHAR:
		if(cur_pos > 0) {
		    cur_pos -= 1;
		    for(i = cur_pos; i < max_pos; i++)
			cur_line[i] = cur_line[i + 1];
		    max_pos -= 1;
		}
		break;
	    case DELETECHAR:
		if(max_pos == 0) break;
		if(cur_pos < max_pos) {
		    for(i = cur_pos; i < max_pos; i++)
			cur_line[i] = cur_line[i + 1];
		    max_pos -= 1;
		}
		break;
	    default:
		if (chtype || (cur_char=='\n')) {
		    if (chtype) {
			if (cur_pos == max_pos) {
			    consoleunputc(c);
			} else {
			    gabeep();
			    break;
			}
		    }
		    cur_line[max_pos] = '\n';
		    cur_line[max_pos + 1] = '\0';
		    strcpy(buf, cur_line);
		    p->r = -1;
		    cur_line[max_pos] = '\0';
		    if (max_pos && addtohistory) {
			xbufadds(p->history, "\n", 0);
			xbufadds(p->history, cur_line, 0);
		    }
		    xbuffixl(p->lbuf);
		    consolewrites(c, "\n");
		    REDRAW;
		    FRETURN(0);
		}
		break;
	    }
	    draweditline(c);
	}
    }
FVOIDEND

void savehistory(control c, char *s)
FBEGIN
    FILE *fp;
    int i;

    if (!s || !NHISTORY) return;
    fp = fopen(s, "w");
    if (!fp) {
       char msg[256];
       sprintf(msg, "Unable to open `%s'", s);
       R_ShowMessage(msg);
       FVOIDRETURN;
    }
    for (i = NHISTORY - 1; i >= 0; i--) {
       fprintf(fp, "%s\n", HISTORY(i));
    }
    fclose(fp);
FVOIDEND

void readhistory(control c, char *s)
FBEGIN
    FILE *fp;
    int c;

    if (!s || !(fp = fopen(s, "r"))) FVOIDRETURN;
    while ((c = getc(fp)) != EOF) xbufaddc(p->history, c);
    /* remove blank line started by final \n */
    p->history->ns--;
    fclose(fp);
FVOIDEND

void console_sbf(control c, int pos)
FBEGIN
    if (pos < 0) {
	pos = -pos - 1 ;
	if (FC != pos) setfirstcol(c, pos);
    } else 
        if (FV != pos) setfirstvisible(c, pos);
FVOIDEND

void Rconsolesetwidth(int);
int setWidthOnResize = 0;

int consolecols(console c)
{
    ConsoleData p = getdata(c);

    return p->cols;
}

void consoleresize(console c, rect r)
FBEGIN
    int rr, pcols = COLS;

    if (((WIDTH  == r.width) &&
	 (HEIGHT == r.height)) ||
	(r.width == 0) || (r.height == 0) ) /* minimize */
        FVOIDRETURN;
/*
 *  set first visible to keep the bottom line on a console,
 *  the middle line on a pager
 */
    if (p->kind == CONSOLE) rr = FV + ROWS;
    else rr = FV + ROWS/2;
    ROWS = r.height/FH - 1;
    if (p->kind == CONSOLE) rr -= ROWS;
    else rr -= ROWS/2;
    COLS = r.width/FW - 1;
    WIDTH = r.width;
    HEIGHT = r.height;
    BORDERX = (WIDTH - COLS*FW) / 2;
    BORDERY = (HEIGHT - ROWS*FH) / 2;
    del(BM);
    BM = newbitmap(r.width, r.height, 2);
    if (!BM) {
       R_ShowMessage("Insufficient memory. Please close the console");
       return ;
    }
    if(!p->lbuf) FVOIDRETURN;    /* don't implement resize if no content
				   yet in pager */
    if (p->r >= 0) {
        if (NUMLINES > ROWS) {
	    p->r = ROWS - 1;
        } else
	    p->r = NUMLINES - 1;
    }
    clear(c);
    p->needredraw = 1;
    setfirstvisible(c, rr);
    if (setWidthOnResize && p->kind == CONSOLE && COLS != pcols)
        Rconsolesetwidth(COLS);
FVOIDEND

void consolesetbrk(console c, actionfn fn, char ch, char mod)
FBEGIN
    p->chbrk = ch;
    p->modbrk = mod;
    p->fbrk = fn;
FVOIDEND

font consolefn = NULL;
char fontname[LF_FACESIZE+1];
int fontsty, pointsize;
int consoler = 25, consolec = 80;
int pagerrow = 25, pagercol = 80;
int pagerMultiple = 1, haveusedapager = 0;

void
setconsoleoptions(char *fnname,int fnsty, int fnpoints,
                  int rows, int cols, rgb nfg, rgb nufg, rgb nbg, rgb high,
		  int pgr, int pgc, int multiplewindows, int widthonresize)
{
    char msg[LF_FACESIZE + 128];
    strncpy(fontname, fnname, LF_FACESIZE);
    fontname[LF_FACESIZE] = '\0';
    fontsty =   fnsty;
    pointsize = fnpoints;
    if (consolefn) del(consolefn);
    consolefn = NULL;
    if (strcmp(fontname, "FixedFont"))
       consolefn = gnewfont(NULL, fnname, fnsty, fnpoints, 0.0);
    if (!consolefn) {
       sprintf(msg,
	       "Font %s-%d-%d  not found.\nUsing system fixed font.",
               fontname, fontsty | FixedWidth, pointsize);
       R_ShowMessage(msg);
       consolefn = FixedFont;
    }
    if (!ghasfixedwidth(consolefn)) {
       sprintf(msg,
	       "Font %s-%d-%d has variable width.\nUsing system fixed font.",
               fontname, fontsty, pointsize);
       R_ShowMessage(msg);
       consolefn = FixedFont;
    }
    consoler = rows;
    consolec = cols;
    consolefg = nfg;
    consoleuser = nufg;
    consolebg = nbg;
    pagerhighlight = high;
    pagerrow = pgr;
    pagercol = pgc;
    pagerMultiple = multiplewindows;
    setWidthOnResize = widthonresize;
}

void consoleprint(console c)
FBEGIN
   printer lpr;
   int cc, rr, fh, cl, cp, clinp, i;
   int top, left;
   font f;
   char *s = "", lc = '\0', msg[LF_FACESIZE + 128], title[60];
   cursor cur;
   if (!(lpr = newprinter(0.0, 0.0))) FVOIDRETURN;
   show(c);
/*
 * If possible, we avoid to use FixedFont for printer since it hasn't the
 * right size
*/
   f = gnewfont(lpr, strcmp(fontname, "FixedFont") ? fontname : "Courier New",
         fontsty, pointsize, 0.0);
   if (!f) {
     /* Should not happen but....*/
       sprintf(msg, "Font %s-%d-%d  not found.\nUsing system fixed font.",
	       strcmp(fontname, "FixedFont") ? fontname : "Courier New",
	       fontsty, pointsize);
       R_ShowMessage(msg);
       f = FixedFont;
   }
   top = devicepixelsy(lpr) / 5;
   left = devicepixelsx(lpr) / 5;
   fh = fontheight(f);
   rr = getheight(lpr) - top;
   cc = getwidth(lpr) - 2*left;
   cl = 0;
   clinp = rr;
   cp = 1;
   strncpy(title,gettext(c), 59);
   if (strlen(gettext(c)) > 59) strcpy(&title[57], "...");
   cur = currentcursor();
   setcursor(WatchCursor);
   while ((cl < NUMLINES) || (*s)) {
     if (clinp + fh >= rr) {
       if (cp > 1) nextpage(lpr);
       gdrawstr(lpr, f, Black, pt(left, top), title);
       sprintf(msg, "Pag.%d", cp++);
       gdrawstr(lpr, f, Black, pt(cc - gstrwidth(lpr, f, msg) - 1, top), msg);
       clinp = top + 2 * fh;
     }
     if (!*s) {
        if (cl < NUMLINES)
           s = LINE(cl++);
        else
           break;
     }
     if (!*s) {
       clinp += fh;
     }
     else {
       for (i = strlen(s); i > 0; i--) {
         lc = s[i];
         s[i] = '\0';
         if (gstrwidth(lpr, f, s) < cc) break;
         s[i] = lc;
       }
       gdrawstr(lpr, f, Black, pt(left, clinp), s);
       clinp += fh;
       s[i] = lc;
       s = &s[i];
     }
   }
   if (f != FixedFont) del(f);
   del(lpr);
   setcursor(cur);
FVOIDEND

console newconsole(char *name, int flags)
{
    console c;
    ConsoleData p;

    p = newconsoledata((consolefn) ? consolefn : FixedFont,
                     consoler, consolec,
                     consolefg, consoleuser, consolebg,
                     CONSOLE);
    if (!p) return NULL;
    c = (console) newwindow(name, rect(0, 0, WIDTH, HEIGHT),
			    flags | TrackMouse | VScrollbar | HScrollbar);
    HEIGHT = getheight(c);
    WIDTH  = getwidth(c);
    COLS = WIDTH / FW - 1;
    ROWS = HEIGHT / FH - 1;
    gsetcursor(c, ArrowCursor);
    gchangescrollbar(c, VWINSB, 0, 0, ROWS, 1);
    gchangescrollbar(c, HWINSB, 0, COLS-1, COLS, 1);
    BORDERX = (WIDTH - COLS*FW) / 2;
    BORDERY = (HEIGHT - ROWS*FH) / 2;
    setbackground(c, consolebg);
    BM = newbitmap(WIDTH, HEIGHT, 2);
    if (!c || !BM ) {
	freeConsoleData(p);
	del(c);
	return NULL;
    }
    setdata(c, p);
    sethit(c, console_sbf);
    setresize(c, consoleresize);
    setredraw(c, drawconsole);
    setdel(c, delconsole);
    setkeyaction(c, console_ctrlkeyin);
    setkeydown(c, console_normalkeyin);
    setmousedrag(c, console_mousedrag);
    setmouserepeat(c, console_mouserep);
    setmousedown(c, console_mousedown);
    return(c);
}

void  consolehelp()
{
    char s[4096];

    strcpy(s,"Scrolling.\n");
    strcat(s,"  Keyboard: PgUp, PgDown, Ctrl+Arrows, Ctrl+Home, Ctrl+End,\n");
    strcat(s,"  Mouse: use the scrollbar(s).\n\n");
    strcat(s,"Editing.\n");
    strcat(s,"  Moving the cursor: \n");
    strcat(s,"     Left arrow or Ctrl+B: move backward one character;\n");
    strcat(s,"     Right arrow or Ctrl+F: move forward one character;\n");
    strcat(s,"     Home or Ctrl+A: go to beginning of line;\n");
    strcat(s,"     End or Ctrl+E: go to end of line;\n");
    strcat(s,"  History: Up and Down Arrows, Ctrl+P, Ctrl+N\n");
    strcat(s,"  Deleting:\n");
    strcat(s,"     Del or Ctrl+D: delete current character;\n");
    strcat(s,"     Backspace: delete preceding character;\n");
    strcat(s,"     Ctrl+Del or Ctrl+K: delete text from current character to end of line.\n");
    strcat(s,"     Ctrl+U: delete all text from current line.\n");
    strcat(s,"  Copy and paste.\n");
    strcat(s,"     Use the mouse (with the left button held down) to mark (select) text.\n");
    strcat(s,"     Use Shift+Del (or Ctrl+C) to copy the marked text to the clipboard and\n");
    strcat(s,"     Shift+Ins (or Ctrl+V or Ctrl+Y) to paste the content of the clipboard (if any)\n");
    strcat(s,"     to the console, Ctrl+X first copy then paste\n\n");
    strcat(s,"Note: Console is updated only when some input is required.\n");
    strcat(s,"  Use Ctrl+W to toggle this feature off/on.\n\n");
    strcat(s,"Use ESC to stop the interpreter.\n\n");
    strcat(s,"Standard Windows hotkeys can be used to switch to the\n");
    strcat(s,"graphics device (Ctrl+Tab or Ctrl+F6 in MDI, Alt+Tab in SDI)");
    askok(s);
}

