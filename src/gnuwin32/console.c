/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file console.c
 *  Copyright (C) 1998--2003  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004-7      The R Foundation
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "win-nls.h"
#include <R_ext/Boolean.h>
extern Rboolean mbcslocale;

/* Use of strchr here is MBCS-safe */

#define USE_MDI 1
extern void R_ProcessEvents(void);

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <string.h>
#include <ctype.h>
#include <wchar.h>
#include <limits.h>
#include <R_ext/rlocale.h>
#include "graphapp/ga.h"
#ifdef USE_MDI
#include "graphapp/stdimg.h"
#endif
#include "console.h"
#include "consolestructs.h"
#include "rui.h"
#include "getline/getline.h"
#include "Startup.h" /* for UImode */
#include <Fileio.h>

extern char *alloca(size_t);

extern UImode  CharacterMode;

static void performCompletion(control c);

#define mbs_init(x) memset(x, 0, sizeof(mbstate_t))

static mbstate_t mb_st; /* use for char transpose as well */

/* This is input from the keyboard, so we do not do validity checks.
   OTOH, we do allow stateful encodings, assuming the state is reset
   at the beginning of each line */

int mb_char_len(char *buf, int clength)
{
    int i, mb_len = 0;

    mbs_init(&mb_st);
    for(i = 0; i <= clength; i += mb_len)
	mb_len = mbrtowc(NULL, buf+i, MB_CUR_MAX, &mb_st);
    return mb_len;
}

/* <FIXME> replace by Ri18n_wcswidth */
int mbswidth(char *buf)
{
    char *p =buf;
    int res = 0, used;
    wchar_t wc;

    mbs_init(&mb_st);
    while(*p) {
	used = mbrtowc(&wc, p, MB_CUR_MAX, &mb_st);
	if(used < 0) return -1;
	p += used;
	res += Ri18n_wcwidth(wc);
    }
    return res;
}

void setCURCOL(ConsoleData p)
{
    char *P = LINE(NUMLINES - 1);
    int w0 = 0;

    if(mbcslocale) {
	int used;
	wchar_t wc;
	mbs_init(&mb_st);
	while (P < LINE(NUMLINES - 1) + prompt_len + cur_byte) {
	    used = mbrtowc(&wc, P, MB_CUR_MAX, &mb_st);
	    if(used <= 0) break;
	    if(wc == L'\r') w0 = 0; else w0 += Ri18n_wcwidth(wc);
	    P += used;
	}
    } else
	for (; P < LINE(NUMLINES - 1) + prompt_len + cur_byte; P++)
	    if(*P == '\r') w0 = 0; else w0++;

    CURCOL = w0;
}


/* xbuf */

xbuf newxbuf(xlong dim, xint ms, xint shift)
{
    xbuf  p;

    p = (xbuf) malloc(sizeof(struct structXBUF));
    if (!p)
	return NULL;
    p->b = (char *) malloc(dim + 1);
    if (!p->b) {
	free(p);
	return NULL;
    }
    p->user = (int *) malloc(ms * sizeof(int));
    if (!p->user) {
	free(p->b);
	free(p);
	return NULL;
    }
    p->s = (char **) malloc(ms * sizeof(char *));
    if (!p->s) {
	free(p->b);
	free(p->user);
	free(p);
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

/* reallocate increased buffer sizes and update pointers */
void xbufgrow(xbuf p, xlong dim, xint ms)
{
    void *ret, *ret2;
    int i, change;

    if(dim > p->dim) {
	ret = realloc(p->b, dim + 1);
	if(ret) {
	    change = (unsigned long) ret - (unsigned long) p->b;
	    p->b = (char *) ret;
	    p->av += change;
	    p->free += change;
	    for (i = 0; i < p->ns; i++)  p->s[i] += change;
	    p->dim = dim;
	}
    }
    if(ms > p->ms) {
	ret = realloc(p->s, ms * sizeof(char *));
	if(ret) {
	    ret2 = realloc(p->user, ms * sizeof(int));
	    if(ret2) {
		p->s = (char **) ret;
		p->user = (int *) ret2;
		p->ms = ms;
	    }
	}
    }
}

void xbufdel(xbuf p)
{
   if (!p) return;
   free(p->s);
   free(p->b);
   free(p->user);
   free(p);
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
	    /* delete the last character, not the last byte */
	    if(mbcslocale) {
		char *buf = p->s[p->ns - 1];
		int l = mb_char_len(buf, strlen(buf)-1);
		p->free -= l;
		p->av += l;
		
	    } else {
		p->free--;
		p->av++;
	    }
	}
	break;
	/* The following implemented a destructive CR
    case '\r':
        {
	    int l = strlen(p->s[p->ns - 1]);
	    p->free -= l;
	    p->av += l;
	}
	break;
	*/
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

rgb consolebg = White, consolefg = Black, consoleuser = gaRed,
    pagerhighlight = gaRed;

extern int R_HistorySize;  /* from Defn.h */

ConsoleData
newconsoledata(font f, int rows, int cols, int bufbytes, int buflines,
	       rgb fg, rgb ufg, rgb bg, int kind, int buffered)
{
    ConsoleData p;

    initapp(0, 0);
    p = (ConsoleData) malloc(sizeof(struct structConsoleData));
    if (!p)
	return NULL;
    p->kind = kind;
    if (kind == CONSOLE) {
	p->lbuf = newxbuf(bufbytes, buflines, SLBUF);
	if (!p->lbuf) {
	    free(p);
	    return NULL;
	}
	p->kbuf = malloc(NKEYS * sizeof(char));
	if (!p->kbuf) {
	    xbufdel(p->lbuf);
	    free(p);
	    return NULL;
	}
    } else {
	p->lbuf = NULL;
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
    p->overwrite = 0;
    p->lazyupdate = 1;
    p->needredraw = 0;
    p->wipe_completion = 0;
    p->my0 = p->my1 = -1;
    p->mx0 = 5;
    p->mx1 = 14;
    p->sel = 0;
    p->input = 0;
    p->lazyupdate = buffered;
    return (p);
}

/* Here fch and lch are columns, and we have to cope with both MBCS
   and double-width chars. */

static void writelineHelper(ConsoleData p, int fch, int lch,
			    rgb fgr, rgb bgr, int j, int len, char *s)
{
    rect  r;
    int last;
    char ch, chf, chl;
    int i, used, w0;
    char *buff, *P = s, *q;
    wchar_t wc;
    Rboolean leftedge;

    /* This is right, since columns are of fixed size */
    r = rect(BORDERX + fch * FW, BORDERY + j * FH, (lch - fch + 1) * FW, FH);
    gfillrect(p->bm, bgr, r);

    if (len > FC+fch) {
	/* Some of the string is visible: */
	if(mbcslocale) {
	    q = buff = alloca(strlen(s) + 1); /* overkill */

	    leftedge = FC && (fch == 0);
	    if(leftedge) fch++;
	    mbs_init(&mb_st);
	    for (w0 = -FC; w0 < fch && *P; ) { /* should have enough ... */
		P += mbrtowc(&wc, P, MB_CUR_MAX, &mb_st);
		w0 += Ri18n_wcwidth(wc);
	    }
	    /* Now we have got to on or just after the left edge.
	       Possibly have a widechar hanging over.
	       If so, fill with blanks.
	    */
	    if(w0 > fch) for(i = 0; i < w0 - fch; i++) *q++ = ' ';

	    if (leftedge) *q++ = '$';

	    while (w0 < lch) {
		used = mbrtowc(&wc, P, MB_CUR_MAX, &mb_st);
		if(used <= 0) break;
		w0 += Ri18n_wcwidth(wc);
		if(w0 > lch) break; /* char straddling the right edge
				       is not displayed */
		for(j = 0; j < used; j++) *q++ = *P++;
	    }
	    *q = 0;
	    if((len > FC+COLS) && (lch == COLS - 1)) *q++ = '$';
	    else {
		used = mbrtowc(NULL, P, MB_CUR_MAX, &mb_st);
		for(j = 0; j < used; j++) *q++ = *P++;
	    }
	    *q = '\0';
	    gdrawstr(p->bm, p->f, fgr, pt(r.x, r.y), buff);
	} else {
	    /* we don't know the string length, so modify it in place */
	    if (FC && (fch == 0)) {chf = s[FC]; s[FC] = '$';} else chf = '\0';
	    if ((len > FC+COLS) && (lch == COLS - 1)) {
		chl = s[FC+lch]; s[FC+lch] = '$';
	    }
	    else chl = '\0';
	    last = FC + lch + 1;
	    if (len > last) {ch = s[last]; s[last] = '\0';} else ch = '\0';
	    gdrawstr(p->bm, p->f, fgr, pt(r.x, r.y), &s[FC+fch]);
	    /* restore the string */
	    if (ch) s[last] = ch;
	    if (chl) s[FC+lch] = chl;
	    if (chf) s[FC] = chf;
	}
    }
}

#define WLHELPER(a, b, c, d) writelineHelper(p, a, b, c, d, j, len, s)

/* write line i of the buffer at row j on bitmap */
static int writeline(ConsoleData p, int i, int j)
{
    char *s, *stmp, *p0;
    int   insel, len, col1, d;
    int   c1, c2, c3, x0, y0, x1, y1;
    rect r;

    if ((i < 0) || (i >= NUMLINES)) return 0;
    stmp = s = LINE(i);
    if(mbcslocale)
	len = mbswidth(stmp);
    else
	len = strlen(stmp);
    /* If there is a \r in the line, we need to preprocess it */
    if((p0 = strchr(s, '\r'))) {
	int l, l1;
	stmp = LINE(i);
	s = alloca(strlen(stmp) + 1);
	l = p0 - stmp;
	strncpy(s, stmp, l);
	stmp = p0 + 1;
	while((p0 = strchr(stmp, '\r'))) {
	    l1 = p0 - stmp;
	    strncpy(s, stmp, l1);
	    if(l1 > l) l = l1;
	    stmp = p0 + 1;	    
	}
	l1 = strlen(stmp);
	strncpy(s, stmp, l1);
	if(l1 > l) l = l1;	
	s[l] = '\0';
	len = l; /* for redraw that uses len */
    }
    col1 = COLS - 1;
    insel = p->sel ? ((i - p->my0) * (i - p->my1)) : 1;
    if (insel < 0) {
	WLHELPER(0, col1, White, DarkBlue);
	return len;
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
    /* This is the cursor, and it may need to be variable-width */
    if ((p->r >= 0) && (CURCOL >= FC) && (CURCOL < FC + COLS) &&
	(i == NUMLINES - 1)) {
	if(mbcslocale) { /* determine the width of the current char */
	    int w0, used = 0, ii;
	    wchar_t wc;
	    char *P = s, nn[10];
	    mbs_init(&mb_st);
	    for (w0 = 0; w0 <= CURCOL; ) {
		used = mbrtowc(&wc, P, MB_CUR_MAX, &mb_st);
		if(used == 0) break;
		w0 += Ri18n_wcwidth(wc);
		P += used;
	    }
	    /* term string '\0' box width = 1 fix */
	    w0 = (wc == L'\0') ? 1 : Ri18n_wcwidth(wc); 
	    P -= used;
	    r = rect(BORDERX + (CURCOL - FC) * FW, BORDERY + j * FH,
		     w0 * FW, FH);
	    gfillrect(p->bm, p->ufg, r);
	    for(ii = 0; ii < used; ii++) nn[ii] = P[ii];
	    nn[used] = '\0';
	    gdrawstr(p->bm, p->f, p->bg, pt(r.x, r.y), nn);
	} else
	    WLHELPER(CURCOL - FC, CURCOL - FC, p->bg, p->ufg);
    }
    if (insel != 0) return len;
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
	if (FC + COLS < x0) return len;
	if(mbcslocale) {
	    int w0, used = 0, w1=1;
	    wchar_t wc;
	    char *P = s;
	    mbs_init(&mb_st);
	    for (w0 = 0; w0 < x0; ) {
		used = mbrtowc(&wc, P, MB_CUR_MAX, &mb_st);
		if(used == 0) break;
		w1 = Ri18n_wcwidth(wc);
		w0 += w1;
		P += used;
	    }
	    if(w0 > x0) x0 = w0 - w1;
	}
	c1 = (x0 > FC) ? (x0 - FC) : 0;
    } else
	c1 = 0;
    if (i == y1) {
	if (FC > x1) return len;
	if(mbcslocale) {
	    int w0, used = 0, wl = 1;
	    wchar_t wc;
	    char *P = s;
	    mbs_init(&mb_st);
	    for (w0 = 0; w0 <= x1; ) {
		used = mbrtowc(&wc, P, MB_CUR_MAX, &mb_st);
		if(used == 0) break;
		wl = Ri18n_wcwidth(wc);
		w0 += wl;
		P += used;
	    }
	    x1 = w0-1;
	}
	c2 = (x1 > FC + COLS) ? (COLS - 1) : (x1 - FC);
    } else
	c2 = COLS - 1;
    WLHELPER(c1, c2, White, DarkBlue);
    return len;
}

void drawconsole(control c, rect r) /* r is unused here */
{
    ConsoleData p = getdata(c);

    int i, ll, wd, maxwd = 0;

    ll = min(NUMLINES, ROWS);
    if(!BM) return;;     /* This is a workaround for PR#1711.
			    BM should never be null here */
    gfillrect(BM, p->bg, getrect(BM));
    if(!ll) return;;
    for (i = 0; i < ll; i++) {
	wd = WRITELINE(NEWFV + i, i);
	if(wd > maxwd) maxwd = wd;
    }
    RSHOW(getrect(c));
    FV = NEWFV;
    p->needredraw = 0;
/* always display scrollbar if FC > 0 */
    if(maxwd < COLS - 1) maxwd = COLS - 1;
    maxwd += FC;
    gchangescrollbar(c, HWINSB, FC, maxwd-FC, COLS,
                     p->kind == CONSOLE || NUMLINES > ROWS);
    gchangescrollbar(c, VWINSB, FV, NUMLINES - 1 , ROWS, p->kind == CONSOLE);
}

void setfirstvisible(control c, int fv)
{
    ConsoleData p = getdata(c);

    int  ds, rw, ww;

    if (NUMLINES <= ROWS) return;;
    if (fv < 0) fv = 0;
    else if (fv > NUMLINES - ROWS) fv = NUMLINES - ROWS;
    if (fv < 0) fv = 0;
    ds = fv - FV;
    if ((ds == 0) && !p->needredraw) return;;
    if (abs(ds) > 1) {
        NEWFV = fv;
        REDRAW;
        return;;
    }
    if (p->needredraw) {
        ww = min(NUMLINES, ROWS) - 1;
        rw = FV + ww;
        writeline(p, rw, ww);
        if (ds == 0) {
	    RSHOW(RLINE(ww));
 	    return;;
        }
    }
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
    FV = fv;
    NEWFV = fv;
    p->needredraw = 0;
    gchangescrollbar(c, VWINSB, fv, NUMLINES - 1 , ROWS, p->kind == CONSOLE);
}

void setfirstcol(control c, int newcol)
{
    ConsoleData p = getdata(c);

    int i, ml, li, ll;

    ll = (NUMLINES < ROWS) ? NUMLINES : ROWS;
    if (newcol > 0) {
	for (i = 0, ml = 0; i < ll; i++) {
	    /* <FIXME> this should really take \r into account */
	    if(mbcslocale)
		li = mbswidth(LINE(NEWFV + i));
	    else
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
}

void console_mousedrag(control c, int button, point pt)
{
    ConsoleData p = getdata(c);

    pt.x -= BORDERX;
    pt.y -= BORDERY;
    if (button & LeftButton) {
	int r, s;
	r=((pt.y > 32000) ? 0 : ((pt.y > HEIGHT) ? HEIGHT : pt.y))/FH;
	s=((pt.x > 32000) ? 0 : ((pt.x > WIDTH) ? WIDTH : pt.x))/FW;
	if ((r < 0) || (r > ROWS) || (s < 0) || (s > COLS))
 	    return;;
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
}

void console_mouserep(control c, int button, point pt)
{
    ConsoleData p = getdata(c);

    if ((button & LeftButton) && (p->sel)) console_mousedrag(c, button,pt);
}

void console_mousedown(control c, int button, point pt)
{
    ConsoleData p = getdata(c);

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
}

void consoletogglelazy(control c)
{
    ConsoleData p = getdata(c);

    if (p->kind == PAGER) return;
    p->lazyupdate = (p->lazyupdate + 1) % 2;
}

int consolegetlazy(control c)
{
    ConsoleData p = getdata(c);
    return p->lazyupdate;
}


void consoleflush(control c)
{
    REDRAW;
}


/* These are the getline keys ^A ^E ^B ^F ^N ^P ^K ^H ^D ^U ^T ^O,
   plus ^Z for EOF.

   We also use ^C ^V/^Y ^X (copy/paste/both) ^W ^L
*/
#define BEGINLINE 1
#define ENDLINE   5
#define CHARLEFT 2
#define CHARRIGHT 6
#define NEXTHISTORY 14
#define PREVHISTORY 16
#define KILLRESTOFLINE 11
#define BACKCHAR  8
#define DELETECHAR 4
#define KILLLINE 21
#define CHARTRANS 20
#define OVERWRITE 15
#define EOFKEY 26

/* ^I for completion */

#define TABKEY 9

/* free ^G ^Q ^R ^S, perhaps ^J */

static void checkpointpos(xbuf p, int save)
{
    static int ns, av;
    static char *free;
    if(save) {
	ns = p->ns;
	av = p->av;
	free = p->free;
    } else {
	p->ns = ns;
	p->av = av;
	p->free = free;
    }
}

static void storekey(control c,int k)
{
    ConsoleData p = getdata(c);

    if (p->wipe_completion) {
	p->wipe_completion = 0;
	checkpointpos(p->lbuf, 0);
	p->needredraw = 1;
	REDRAW;
    }
    if (p->kind == PAGER) return;
    if (k == BKSP) k = BACKCHAR;
    if (k == TABKEY) {
        performCompletion(c); 
	return;
    }
    if (p->numkeys >= NKEYS) {
	gabeep();
	return;;
     }
     p->kbuf[(p->firstkey + p->numkeys) % NKEYS] = k;
     p->numkeys++;
}


#include <Rinternals.h>
#include <R_ext/Parse.h>

static int rcompgen_available = -1;

void set_rcompgen_available(int x)
{
    rcompgen_available = x;
}
    

static void performCompletion(control c)
{
    ConsoleData p = getdata(c);
    int i, alen, alen2, max_show = 10, cursor_position = p->c - prompt_wid;
    char *partial_line = LINE(NUMLINES - 1) + prompt_wid;
    char *additional_text;
    char *pline, *cmd;
    SEXP cmdSexp, cmdexpr, ans = R_NilValue;
    ParseStatus status;

    if(!rcompgen_available) return;
    
    if(rcompgen_available < 0) {
	char *p = getenv("R_COMPLETION");
	if(p && strcmp(p, "FALSE") == 0) {
	    rcompgen_available = 0;
	    return;	    
	}
	/* First check if namespace is loaded */
	if(findVarInFrame(R_NamespaceRegistry, install("rcompgen"))
	   != R_UnboundValue) rcompgen_available = 1;
	else { /* Then try to load it */
	    char *p = "try(loadNamespace('rcompgen'), silent=TRUE)";
	    PROTECT(cmdSexp = mkString(p));
	    cmdexpr = PROTECT(R_ParseVector(cmdSexp, -1, &status, R_NilValue));
	    if(status == PARSE_OK) {
		for(i = 0; i < length(cmdexpr); i++)
		    eval(VECTOR_ELT(cmdexpr, i), R_GlobalEnv);
	    }
	    UNPROTECT(2);
	    if(findVarInFrame(R_NamespaceRegistry, install("rcompgen"))
	       != R_UnboundValue) rcompgen_available = 1;
	    else {
		rcompgen_available = 0;
		return;
	    }
	}
    }

    /* FIXME: need to escape quotes properly */
    pline = alloca(strlen(partial_line) + 1);
    strcpy(pline, partial_line);
    /* poor attempt at escaping quotes that sort of works */
    alen = strlen(pline);
    for (i = 0; i < alen; i++)
        if (pline[i] == '"') pline[i] = '\'';

    cmd = alloca(strlen(pline) + 100);
    sprintf(cmd, "rcompgen:::.win32consoleCompletion(\"%s\", %d)",
	    pline, cursor_position);
    PROTECT(cmdSexp = mkString(cmd));
    cmdexpr = PROTECT(R_ParseVector(cmdSexp, -1, &status, R_NilValue));
    if (status != PARSE_OK) {
	UNPROTECT(2);
	/* Uncomment next line to debug */
	/* Rprintf("failed: %s \n", cmd); */
	/* otherwise pretend that nothing happened and return */
	return;
    }
    /* Loop is needed here as EXPSEXP will be of length > 1 */
    for(i = 0; i < length(cmdexpr); i++)
	ans = eval(VECTOR_ELT(cmdexpr, i), R_GlobalEnv);
    UNPROTECT(2);

    /* ans has the form list(addition, possible), where 'addition' is
       unique additional text if any, and 'possible' is a character
       vector holding possible completions if any (already formatted
       for linewise printing in the current implementation).  If
       'possible' has any content, we want to print those (or show in
       status bar or whatever).  Otherwise add the 'additional' text
       at the cursor */

#define ADDITION 0
#define POSSIBLE 1

    alen = length(VECTOR_ELT(ans, POSSIBLE));
    additional_text = CHAR(STRING_ELT( VECTOR_ELT(ans, ADDITION), 0 ));
    alen2 = strlen(additional_text);
    if (alen) {
        /* make a copy of the current string first */
	char *buf1, *p1 = LINE(NUMLINES - 1);
	checkpointpos(p->lbuf, 1);
	buf1 = alloca(strlen(p1) + 1);
	sprintf(buf1,"%s\n", p1); 
	consolewrites(c, buf1);

	for (i = 0; i < min(alen, max_show); i++) {
	    consolewrites(c, CHAR(STRING_ELT(VECTOR_ELT(ans, POSSIBLE), i)));
	    consolewrites(c, "\n");
	}
	if (alen > max_show)
	    consolewrites(c, "\n[...truncated]\n");
	p->wipe_completion = 1;
    }

    if (alen2) 
	for (i = 0; i < alen2; i++) storekey(c, additional_text[i]);
    return;
}

void consolecmd(control c, char *cmd)
{
    ConsoleData p = getdata(c);

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
      /* <FIXME> This needs to go back in bytes, not chars */
      for (i = max_byte; i > cur_byte; i--) storekey(c, CHARLEFT);
    }
}

static int CleanTranscript(char *tscpt, char *cmds)
{
    /*
     * Filter R commands out of a string that contains
     * prompts, commands, and output.
     * Uses a simple algorithm that just looks for '>'
     * prompts and '+' continuation following a simple prompt prefix.
     * Always return the length of the string required
     * to hold the filtered commands.
     * If cmds is a non-null pointer, write the commands
     * to cmds & terminate with null.
     */
    int incommand = 0, startofline = 1, len = 0;
    char nonprefix[] = ">+ \t\n\r";
    while (*tscpt) {
	if (startofline) {
	    /* skip initial whitespace */
	    while (*tscpt==' ' || *tscpt=='\t')
		tscpt++;
	    /* skip over the prompt prefix */
	    while (*tscpt && !strchr(nonprefix, *tscpt))
	    	tscpt++;
	    if (*tscpt=='>' || (incommand && *tscpt=='+')) {
		tscpt++;
		if (*tscpt==' ' || *tscpt=='\t') tscpt++;
		incommand = 1;
	    } else {
		incommand = 0;
	    }
	    startofline = 0;
	} else {
	    if (incommand) {
		if (cmds)
		    *(cmds++) = *tscpt;
		len++;
	    }
	    if (*tscpt=='\n')
		startofline = 1;
	    tscpt++;
	}
    }
    if (cmds) {
	/* seem to have to terminate with two nulls, otherwise
	   pasting empty commands doesn't work correctly (e.g.,
	   when clipboard contains 'XXX') */
	*cmds = '\0';
	*(cmds+1) = '\0';
    }
    return(len+2);
}

/* Send a single newline to the console */
void consolenewline(control c)
{
    storekey(c, '\n');
}

/* the following four routines are system dependent */
void consolepaste(control c)
{
    ConsoleData p = getdata(c);

    HGLOBAL hglb;
    char *pc, *new = NULL;
    if (p->sel) {
	p->sel = 0;
	p->needredraw = 1;
	REDRAW;
     }
    if (p->kind == PAGER) return;;
    if ( OpenClipboard(NULL) &&
         (hglb = GetClipboardData(CF_TEXT)) &&
         (pc = (char *)GlobalLock(hglb)))
    {
        if (p->clp) {
           new = realloc((void *)p->clp, strlen(p->clp) + strlen(pc) + 1);
        }
        else {
           new = malloc(strlen(pc) + 1) ;
           if (new) new[0] = '\0';
           p->already = p->numkeys;
           p->pclp = 0;
        }
        if (new) {
           p->clp = new;
           strcat(p->clp, pc);
        }
        else {
           R_ShowMessage(G_("Not enough memory"));
        }
        GlobalUnlock(hglb);
    }
    CloseClipboard();
}

void consolepastecmds(control c)
{
    ConsoleData p = getdata(c);

    HGLOBAL hglb;
    char *pc, *new = NULL;
    if (p->sel) {
	p->sel = 0;
	p->needredraw = 1;
	REDRAW;
     }
    if (p->kind == PAGER) return;;
    if ( OpenClipboard(NULL) &&
         (hglb = GetClipboardData(CF_TEXT)) &&
         (pc = (char *)GlobalLock(hglb)))
    {
        if (p->clp) {
	    new = realloc((void *)p->clp, strlen(p->clp) +
			  CleanTranscript(pc, 0));
        }
        else {
	    new = malloc(CleanTranscript(pc, 0)) ;
	    if (new) new[0] = '\0';
	    p->already = p->numkeys;
	    p->pclp = 0;
        }
        if (new) {
            p->clp = new;
	    /* copy just the commands from the clipboard */
	    for (; *new; ++new); /* append to the end of 'new' */
	    CleanTranscript(pc, new);
        }
        else {
	    R_ShowMessage(G_("Not enough memory"));
        }
        GlobalUnlock(hglb);
    }
    CloseClipboard();
}

/* This works with columns, not chars or bytes */
static void consoletoclipboardHelper(control c, int x0, int y0, int x1, int y1)
{
    ConsoleData p = getdata(c);

    HGLOBAL hglb;
    int ll, i, j;
    char *s;

    int w0 = 0 /* -Wall */, used=0, x00, x11=100000;
    wchar_t wc;
    char *P;
    if(mbcslocale) {

	i = y0; x00 = x0; ll = 1; /* terminator */
	while (i <= y1) {
	    P = LINE(i);
	    mbs_init(&mb_st);
	    for (w0 = 0; w0 < x00 && *P; ) {
		P += mbrtowc(&wc, P, MB_CUR_MAX, &mb_st);
		w0 += Ri18n_wcwidth(wc);
	    }
	    x00 = 0;
	    if(i == y1) x11 = x1+1; /* cols are 0-based */
	    while (w0 < x11 && *P) {
		used = mbrtowc(&wc, P, MB_CUR_MAX, &mb_st);
		ll += used;
		P += used;
		w0 += Ri18n_wcwidth(wc);
	    }
	    if(w0 < x11) ll += 2;  /* \r\n */
	    i++;
	}
    } else {
	i = y0; j = x0; ll = 1; /* terminator */
	while ((i < y1) || ((i == y1) && (j <= x1))) {
	    if (LINE(i)[j]) {
		ll++;
		j++;
	    }
	    else {
		ll += 2;
		i++;
		j = 0;
	    }
	}
    }

    if (!(hglb = GlobalAlloc(GHND, ll))){
        R_ShowMessage(G_("Insufficient memory: text not copied to the clipboard"));
        return;
    }
    if (!(s = (char *)GlobalLock(hglb))){
        R_ShowMessage(G_("Insufficient memory: text not copied to the clipboard"));
        return;
    }
    if(mbcslocale) {
	i = y0; x00 = x0; x11=100000;
	while (i <= y1) {
	    P = LINE(i);
	    mbs_init(&mb_st);
	    for (w0 = 0; w0 < x00 && *P; ) {
		P += mbrtowc(&wc, P, MB_CUR_MAX, &mb_st);
		w0 += Ri18n_wcwidth(wc);
	    }
	    x00 = 0;
	    if(i == y1) x11 = x1+1;
	    while (w0 < x11 && *P) {
		used = mbrtowc(&wc, P, MB_CUR_MAX, &mb_st);
		w0 += Ri18n_wcwidth(wc);
		for(j = 0; j < used; j++) *s++ = *P++;
	    }
	    if(w0 < x11) *s++ = '\r'; *s++ = '\n';
	    i++;
	}
    } else {
	i = y0; j = x0;
	while ((i < y1) || ((i == y1) && (j <= x1))) {
	    char ch = LINE(i)[j];
	    if (ch) {
		*s++ = ch;
		j++;
	    } else {
		*s++ = '\r'; *s++ = '\n';
		i++;
		j = 0;
	    }
	}
    }
    *s = '\0';
    GlobalUnlock(hglb);
    if (!OpenClipboard(NULL) || !EmptyClipboard()) {
        R_ShowMessage(G_("Unable to open the clipboard"));
        GlobalFree(hglb);
        return;;
    }
    SetClipboardData(CF_TEXT, hglb);
    CloseClipboard();
}

/* end of system dependent part */

int consolecanpaste(control c)
{
    return clipboardhastext();
}


int consolecancopy(control c)
{
    ConsoleData p = getdata(c);
    return p->sel;
}


void consolecopy(control c)
{
    ConsoleData p = getdata(c);

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
	if (p->mx1 >= len) p->mx1 = len/* - 1*/;
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
}

void consoleselectall(control c)
{
    ConsoleData p = getdata(c);

   if (NUMLINES) {
       p->sel = 1;
       p->my0 = p->mx0 = 0;
       p->my1 = NUMLINES - 1;
       p->mx1 = strlen(LINE(p->my1));
       REDRAW;
    }
}

void console_normalkeyin(control c, int k)
{
    ConsoleData p = getdata(c);

    int st;

    st = ggetkeystate();
    if ((p->chbrk) && (k == p->chbrk) &&
	((!p->modbrk) || ((p->modbrk) && (st == p->modbrk)))) {
	p->fbrk(c);
	return;
    }
    if (st == CtrlKey)
	switch (k + 'A' - 1) {
	    /* most are stored as themselves */
	case 'C':
	    consolecopy(c);
	    st = -1;
	    break;
	case 'V':
	case 'Y':
	    if(p->kind == PAGER) {
		consolecopy(c);
		if (CharacterMode == RGui) consolepaste(RConsole);
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
	case 'L':
	    consoleclear(c);
	    st = -1;
	    break;
	case 'O':
	    p->overwrite = !p->overwrite;
	    st = -1;
	    break;
	}
    if (p->sel) {
	p->sel = 0;
	p->needredraw = 1;
	REDRAW;
    }
    if (st == -1) return;
    if (p->kind == PAGER) {
	if(k == 'q' || k == 'Q') pagerbclose(c);
	if(k == ' ') setfirstvisible(c, NEWFV + ROWS);
	if(k == '-') setfirstvisible(c, NEWFV - ROWS);
	if(k == 'F' - 'A' + 1) setfirstvisible(c, NEWFV + ROWS);
	if(k == 'B' - 'A' + 1) setfirstvisible(c, NEWFV - ROWS);
	if(k == 1) consoleselectall(c);
	return;
    }
    storekey(c, k);
}

void console_ctrlkeyin(control c, int key)
{
    ConsoleData p = getdata(c);

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
}

static Rboolean incomplete = FALSE;
int consolewrites(control c, char *s)
{
    ConsoleData p = getdata(c);

    char buf[1001];
    if(p->input) {
        int i, len = strlen(LINE(NUMLINES - 1));
	/* save the input line */
	strncpy(buf, LINE(NUMLINES - 1), 1000);
	buf[1000] = '\0';
	/* now zap it */
	for(i = 0; i < len; i++) xbufaddc(p->lbuf, '\b');
	if (incomplete) {
	    p->lbuf->ns--;
	    p->lbuf->free--;
	    p->lbuf->av++;
	}
	USER(NUMLINES - 1) = -1;
    }
    xbufadds(p->lbuf, s, 0);
    FC = 0;
    if(p->input) {
	incomplete = (s[strlen(s) - 1] != '\n');
        if (incomplete) xbufaddc(p->lbuf, '\n');
	xbufadds(p->lbuf, buf, 1);
    }
    if (strchr(s, '\n')) p->needredraw = 1;
    if (!p->lazyupdate || (p->r >= 0))
        setfirstvisible(c, NUMLINES - ROWS);
    else {
        p->newfv = NUMLINES - ROWS;
        if (p->newfv < 0) p->newfv = 0;
    }
    if(p->input) REDRAW;
    return 0;
}

void freeConsoleData(ConsoleData p)
{
    if (!p) return;
    if (p->bm) del(p->bm);
    if (p->kind == CONSOLE) {
        if (p->lbuf) xbufdel(p->lbuf);
	if (p->kbuf) free(p->kbuf);
    }
    free(p);
}

static void delconsole(control c)
{
    freeConsoleData(getdata(c));
}

/* console readline (coded looking to the GNUPLOT 3.5 readline)*/
static char consolegetc(control c)
{
    ConsoleData p;
    char ch;

    p = getdata(c);
    while((p->numkeys == 0) && (!p->clp))
    {
	if (!peekevent()) WaitMessage();
	R_ProcessEvents();
    }
    if (p->sel) {
	p->sel = 0;
	p->needredraw = 1;
	setCURCOL(p); /* Needed? */
	REDRAW;
    }
    if (!p->already && p->clp)
    {
	ch = p->clp[p->pclp++];
	if (!(p->clp[p->pclp])) {
	    free(p->clp);
	    p->clp = NULL;
	}
    } else {
          ch = p->kbuf[p->firstkey];
          p->firstkey = (p->firstkey + 1) % NKEYS;
          p->numkeys--;
          if (p->already) p->already--;
    }
    return ch;
}

static void consoleunputc(control c)
{
    ConsoleData p = getdata(c);

    if(p->clp) p->pclp--;
    else {
	p->numkeys += 1;
	if (p->firstkey > 0) p->firstkey -= 1;
	else p->firstkey = NKEYS - 1;
    }
}

/* This scrolls as far left as possible */
static void checkvisible(control c)
{
    ConsoleData p = getdata(c);

    int newfc;

    setfirstvisible(c, NUMLINES-ROWS);
    newfc = 0;
    while ((CURCOL <= newfc) || (CURCOL > newfc+COLS-2)) newfc += 5;
    if (newfc != FC) setfirstcol(c, newfc);
}

static void draweditline(control c)
{
    ConsoleData p = getdata(c);
    setCURCOL(p);
    checkvisible(c);
    if (p->needredraw) {
        REDRAW;
    } else {
	WRITELINE(NUMLINES - 1, p->r);
	RSHOW(RLINE(p->r));
    }
}

/* Here multibyte characters will be entered as individual bytes */
int consolereads(control c, char *prompt, char *buf, int len, int addtohistory)
{
    ConsoleData p = getdata(c);

    char cur_char;
    char *cur_line, *P;
    char *aLine;
    int ns0 = NUMLINES, w0 = 0, pre_prompt_len;
    int mb_len;

    pre_prompt_len = strlen(LINE(NUMLINES - 1));
    /* print the prompt */
    xbufadds(p->lbuf, prompt, 1);
    if (!xbufmakeroom(p->lbuf, len + 1)) return 1;
    P = aLine = LINE(NUMLINES - 1);
    prompt_len = strlen(aLine);
    if(mbcslocale) {
	int used;
	wchar_t wc;
	mbs_init(&mb_st);
	while (P < aLine + pre_prompt_len) {
	    used = mbrtowc(&wc, P, MB_CUR_MAX, &mb_st);
	    if(used <= 0) break;
	    if(wc == L'\r') w0 = 0; else w0 += Ri18n_wcwidth(wc);
	    P += used;
	}
	USER(NUMLINES - 1) = w0;
	prompt_wid = w0 + mbswidth(prompt);
    } else {
	for (; P < aLine + pre_prompt_len; P++)
	    if(*P == '\r') w0 = 0; else w0++;
	USER(NUMLINES - 1) = w0;
	prompt_wid = w0 + strlen(prompt);
    }
    if (NUMLINES > ROWS) {
	p->r = ROWS - 1;
	p->newfv = NUMLINES - ROWS;
    } else {
	p->r = NUMLINES - 1;
	p->newfv = 0;
    }
    CURCOL = prompt_wid;
    p->fc = 0;
    cur_byte = 0;
    max_byte = 0;
    cur_line = &aLine[prompt_len];
    cur_line[0] = '\0';
    REDRAW;
    for(;;) {
	char chtype;
	p->input = 1;
	cur_char = consolegetc(c);
	p->input = 0;
	chtype = ((unsigned char)cur_char > 0x1f);
	if(NUMLINES != ns0) { /* we scrolled, e.g. cleared screen */
            cur_line = LINE(NUMLINES - 1) + prompt_len;
	    ns0 = NUMLINES;
	    if (NUMLINES > ROWS) {
		p->r = ROWS - 1;
		p->newfv = NUMLINES - ROWS;
	    } else {
		p->r = NUMLINES - 1;
		p->newfv = 0;
	    }
	    USER(NUMLINES - 1) = prompt_wid;
	    p->needredraw = 1;
	}
        if(chtype && (max_byte <= len - 2 - MB_CUR_MAX)) { 
	    /* not a control char: we need to fit in the char\n\0 */
	    int i;
	    if(mbcslocale) {
		char s[9];
		int clen;
		int res;
		wchar_t wc;

		memset(s, 0, sizeof(s));
		for(clen = 0; clen <= MB_CUR_MAX;) {
		    s[clen++] = cur_char;
		    mbs_init(&mb_st);
		    res = mbrtowc(&wc, s, clen ,&mb_st);
		    if(res >= 0) break;
		    cur_char = consolegetc(c);
		}
		if( p->overwrite ==1 && cur_byte != max_byte ) {
		    mb_len = mb_char_len(cur_line, cur_byte);
		    for(i = cur_byte; i <= max_byte-mb_len ; i++)
			cur_line[i] = cur_line[i + mb_len];
		    max_byte -= mb_len;
		}
		for(i = max_byte; i >= cur_byte; i--) 
		    cur_line[i+clen] = cur_line[i]; 
		for(i = 0;  i< clen; i++) 
		    cur_line[cur_byte + i] = s[i];
		max_byte += clen;
		cur_byte += clen; 
	    } else {
		if(!p->overwrite) {
		    for(i = max_byte; i > cur_byte; i--) {
			cur_line[i] = cur_line[i - 1];
		    }
		}
		cur_line[cur_byte] = cur_char;
		if(!p->overwrite || cur_byte == max_byte) {
		    max_byte += 1;
		    cur_line[max_byte] = '\0';
		}
		cur_byte++;
	    }
	} else { /* a control char */
	    /* do normal editing commands */
	    int i;
	    switch(cur_char) {
	    case BEGINLINE:
		cur_byte = 0;
		break;
	    case CHARLEFT:
		if(cur_byte > 0) {
		    cur_byte -= mb_char_len(cur_line, cur_byte-1);
		}
		break;
	    case ENDLINE:
		cur_byte = max_byte;
		break;
	    case CHARRIGHT:
		if(cur_byte < max_byte) {
		    cur_byte += mb_char_len(cur_line, cur_byte);
		}
		break;
	    case KILLRESTOFLINE:
		max_byte = cur_byte;
		cur_line[max_byte]='\0';
		break;
	    case KILLLINE:
		max_byte = cur_byte = 0;
		cur_line[max_byte]='\0';
		break;
	    case PREVHISTORY:
		strcpy(cur_line, gl_hist_prev());
		cur_byte = max_byte = strlen(cur_line);
		break;
	    case NEXTHISTORY:
		strcpy(cur_line, gl_hist_next());
		cur_byte = max_byte = strlen(cur_line);
		break;
	    case BACKCHAR:
		if(cur_byte > 0) {
		    mb_len = mb_char_len(cur_line, cur_byte-1);
		    cur_byte -= mb_len;
		    for(i = cur_byte; i <= max_byte - mb_len; i++)
		        cur_line[i] = cur_line[i + mb_len];
		    max_byte -= mb_len;
		}
		break;
	    case DELETECHAR:
		if(max_byte == 0) break;
		if(cur_byte < max_byte) {
		    mb_len = mb_char_len(cur_line, cur_byte);
		    for(i = cur_byte; i <= max_byte - mb_len; i++)
			cur_line[i] = cur_line[i + mb_len];
		    max_byte -= mb_len;
		}
		break;
	    case CHARTRANS:
		if(cur_byte < 1) break;
		if(cur_byte >= max_byte) break;
		{
		    int j, l_len = mb_char_len(cur_line, cur_byte-1), r_len;
		    /* we should not reset the state here */
		    if(mbcslocale)
			r_len = mbrtowc(NULL, cur_line+cur_byte, MB_CUR_MAX,
					&mb_st);
		    else
			r_len = 1;
		    for (i = 0; i < r_len; i++)
			for(j = 0; j < l_len; j++) {
			    cur_char = cur_line[cur_byte+i-j];
			    cur_line[cur_byte+i-j] = cur_line[cur_byte+i-j-1];
			    cur_line[cur_byte+i-j-1] = cur_char;
			}
		    cur_byte += r_len - l_len;
		}
		break;
	    default:   /* Another control char, or overflow */
		if (chtype || (cur_char == '\n') || (cur_char == EOFKEY)) {
		    if (chtype) {
			if (cur_byte == max_byte) {
			    consoleunputc(c);
			} else {
			    gabeep();
			    break;
			}
		    }
		    if((cur_char == '\n') || (cur_char == EOFKEY)) {
			cur_line[max_byte] = '\n';
			cur_line[max_byte + 1] = '\0';
		    } else {
			cur_line[max_byte] = '\0';
		    }
		    /* just to be safe */
		    strncpy(buf, cur_line, len);
		    p->r = -1;
		    cur_line[max_byte] = '\0';
		    if (max_byte && addtohistory) gl_histadd(cur_line);
		    xbuffixl(p->lbuf);
		    consolewrites(c, "\n");
		    REDRAW;
		    return cur_char == EOFKEY;
		}
		break;
	    }
	}
	draweditline(c);
    }
}

void console_sbf(control c, int pos)
{
    ConsoleData p = getdata(c);

    if (pos < 0) {
	pos = -pos - 1 ;
	if (FC != pos) setfirstcol(c, pos);
    } else
        if (FV != pos) setfirstvisible(c, pos);
}

void Rconsolesetwidth(int);
int setWidthOnResize = 0;

int consolecols(console c)
{
    ConsoleData p = getdata(c);

    return p->cols;
}

void consoleresize(console c, rect r)
{
    ConsoleData p = getdata(c);

    int rr, pcols = COLS;

    if (((WIDTH  == r.width) &&
	 (HEIGHT == r.height)) ||
	(r.width == 0) || (r.height == 0) ) /* minimize */
        return;;
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
       R_ShowMessage(G_("Insufficient memory. Please close the console"));
       return ;
    }
    if(!p->lbuf) return;;    /* don't implement resize if no content
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
}

void consolesetbrk(console c, actionfn fn, char ch, char mod)
{
    ConsoleData p = getdata(c);

    p->chbrk = ch;
    p->modbrk = mod;
    p->fbrk = fn;
}

font consolefn = NULL;
char fontname[LF_FACESIZE+1];
int fontsty, pointsize;
int consoler = 25, consolec = 80, consolex = 0, consoley = 0;
int pagerrow = 25, pagercol = 80;
int pagerMultiple = 1, haveusedapager = 0;
int consolebufb = DIMLBUF, consolebufl = MLBUF, consolebuffered = 1;

void
setconsoleoptions(char *fnname,int fnsty, int fnpoints,
                  int rows, int cols, int consx, int consy,
		  rgb nfg, rgb nufg, rgb nbg, rgb high,
		  int pgr, int pgc, int multiplewindows, int widthonresize,
		  int bufbytes, int buflines, int buffered)
{
    char msg[LF_FACESIZE + 128];
    strncpy(fontname, fnname, LF_FACESIZE);
    fontname[LF_FACESIZE] = '\0';
    fontsty =   fnsty;
    pointsize = fnpoints;
    if (consolefn) del(consolefn);
    consolefn = NULL;
    if (strcmp(fontname, "FixedFont")) {
	consolefn = gnewfont(NULL, fnname, fnsty | FixedWidth, fnpoints, 0.0);
	if (!consolefn) {
	    /* This is unlikely to happen: it will find some match */
	    sprintf(msg,
		    G_("Font %s-%d-%d  not found.\nUsing system fixed font"),
		    fontname, fontsty | FixedWidth, pointsize);
	    R_ShowMessage(msg);
	    consolefn = FixedFont;
	}
    }
/*    if (!ghasfixedwidth(consolefn)) {
       sprintf(msg,
	       "Font %s-%d-%d has variable width.\nUsing system fixed font.",
               fontname, fontsty, pointsize);
       R_ShowMessage(msg);
       consolefn = FixedFont;
       } */
    consoler = rows;
    consolec = cols;
    consolex = consx;
    consoley = consy;
    consolefg = nfg;
    consoleuser = nufg;
    consolebg = nbg;
    pagerhighlight = high;
    pagerrow = pgr;
    pagercol = pgc;
    pagerMultiple = multiplewindows;
    setWidthOnResize = widthonresize;
    consolebufb = bufbytes;
    consolebufl = buflines;
    consolebuffered = buffered;
}

void consoleprint(console c)
{
    ConsoleData p = getdata(c);


    printer lpr;
    int cc, rr, fh, cl, cp, clinp, i;
    int top, left;
    int x0, y0, x1, y1;
    font f;
    char *s = "", lc = '\0', msg[LF_FACESIZE + 128], title[60];
    char buf[1024];
    cursor cur;
    if (!(lpr = newprinter(0.0, 0.0, ""))) return;;
    show(c);
/*
 * If possible, we avoid to use FixedFont for printer since it hasn't the
 * right size
 */
    f = gnewfont(lpr, strcmp(fontname, "FixedFont") ? fontname : "Courier New",
		 fontsty, pointsize, 0.0);
    if (!f) {
	/* Should not happen but....*/
	sprintf(msg, G_("Font %s-%d-%d  not found.\nUsing system fixed font"),
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
    strncpy(title, GA_gettext(c), 59);
    if (strlen(GA_gettext(c)) > 59) strcpy(&title[56], "...");
    cur = currentcursor();
    setcursor(WatchCursor);

    /* Look for a selection */
    if (p->sel) {
	int len, c1, c2, c3;
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
    } else {
	x0 = y0 = 0;
	y1 = NUMLINES - 1;
	x1 = strlen(LINE(y1));
    }

    cl = y0; /* current line */
    clinp = rr;
    cp = 1; /* current page */

    /* s is possible continuation line */
    while ((cl <= y1) || (*s)) {
	if (clinp + fh >= rr) {
	    if (cp > 1) nextpage(lpr);
	    gdrawstr(lpr, f, Black, pt(left, top), title);
	    sprintf(msg, "Page %d", cp++);
	    gdrawstr(lpr, f, Black,
                     pt(cc - gstrwidth(lpr, f, msg) - 1, top),
		     msg);
	    clinp = top + 2 * fh;
	}
	if (!*s) {
            if (cl == y0) s = LINE(cl++) + x0;
	    else if (cl < y1) s = LINE(cl++);
	    else if (cl == y1) {
		s = strncpy(buf, LINE(cl++), 1023);
		s[min(x1, 1023) + 1] = '\0';
	    } else break;
	}
	if (!*s) {
	    clinp += fh;
	} else {
	    for (i = strlen(s); i > 0; i--) {
		lc = s[i];
		s[i] = '\0';
		if (gstrwidth(lpr, f, s) < cc) break;
		s[i] = lc;
	    }
	    gdrawstr(lpr, f, Black, pt(left, clinp), s);
	    clinp += fh;
	    s[i] = lc;
	    s = s + i;
	}
    }

    if (f != FixedFont) del(f);
    del(lpr);
    setcursor(cur);
}

void consolesavefile(console c, int pager)
{
    ConsoleData p = getdata(c);

    char *fn;
    cursor cur;
    FILE *fp;
    int x0, y0, x1, y1, cl;
    char *s, buf[1024];

    setuserfilter("Text files (*.txt)\0*.txt\0All files (*.*)\0*.*\0\0");
    if(p->sel)
        fn = askfilesave(G_("Save selection to"), "lastsave.txt");
    else
        fn = askfilesave(G_("Save console contents to"), "lastsave.txt");
    show(c);
    if (fn) {
	fp = R_fopen(fn, "wt");
	if (!fp) return;
	cur = currentcursor();
	setcursor(WatchCursor);

	/* Look for a selection */
	if (p->sel) {
	    int len, c1, c2, c3;
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
	} else {
	    x0 = y0 = 0;
	    y1 = NUMLINES - 1;
	    x1 = strlen(LINE(y1));
	}

	for (cl = y0; cl <= y1; cl++) {
	    if (cl == y0) s = LINE(cl) + x0;
	    else if (cl < y1) s = LINE(cl);
	    else if (cl == y1) {
		s = strncpy(buf, LINE(cl), 1023);
		s[min(x1, 1023) + 1] = '\0';
	    } else break;
	    fputs(s, fp); fputc('\n', fp);
	}
	fclose(fp);
	setcursor(cur);
    }
}


console newconsole(char *name, int flags)
{
    console c;
    ConsoleData p;

    p = newconsoledata((consolefn) ? consolefn : FixedFont,
		       consoler, consolec, consolebufb, consolebufl,
		       consolefg, consoleuser, consolebg,
		       CONSOLE, consolebuffered);
    if (!p) return NULL;
    c = (console) newwindow(name, rect(consolex, consoley, WIDTH, HEIGHT),
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

    strcpy(s,G_("Scrolling.\n"));
    strcat(s,G_("  Keyboard: PgUp, PgDown, Ctrl+Arrows, Ctrl+Home, Ctrl+End,\n"));
    strcat(s,G_("  Mouse: use the scrollbar(s).\n\n"));
    strcat(s,G_("Editing.\n"));
    strcat(s,G_("  Moving the cursor: \n"));
    strcat(s,G_("     Left arrow or Ctrl+B: move backward one character;\n"));
    strcat(s,G_("     Right arrow or Ctrl+F: move forward one character;\n"));
    strcat(s,G_("     Home or Ctrl+A: go to beginning of line;\n"));
    strcat(s,G_("     End or Ctrl+E: go to end of line;\n"));
    strcat(s,G_("  History: Up and Down Arrows, Ctrl+P, Ctrl+N\n"));
    strcat(s,G_("  Deleting:\n"));
    strcat(s,G_("     Del or Ctrl+D: delete current character;\n"));
    strcat(s,G_("     Backspace: delete preceding character;\n"));
    strcat(s,G_("     Ctrl+Del or Ctrl+K: delete text from current character to end of line.\n"));
    strcat(s,G_("     Ctrl+U: delete all text from current line.\n"));
    strcat(s,G_("  Copy and paste.\n"));
    strcat(s,G_("     Use the mouse (with the left button held down) to mark (select) text.\n"));
    strcat(s,G_("     Use Shift+Del (or Ctrl+C) to copy the marked text to the clipboard and\n"));
    strcat(s,G_("     Shift+Ins (or Ctrl+V or Ctrl+Y) to paste the content of the clipboard (if any)  \n"));
    strcat(s,G_("     to the console, Ctrl+X first copy then paste\n"));
    strcat(s,G_("  Misc:\n"));
    strcat(s,G_("     Ctrl+L: Clear the console.\n"));
    strcat(s,G_("     Ctrl+O: Toggle overwrite mode: initially off.\n"));
    strcat(s,G_("     Ctrl+T: Interchange current char with one to the left.\n"));
    strcat(s,G_("\nNote: Console is updated only when some input is required.\n"));
    strcat(s,G_("  Use Ctrl+W to toggle this feature off/on.\n\n"));
    strcat(s,G_("Use ESC to stop the interpreter.\n\n"));
    strcat(s,G_("TAB starts completion of the current word.\n\n"));
    strcat(s,G_("Standard Windows hotkeys can be used to switch to the\n"));
    strcat(s,G_("graphics device (Ctrl+Tab or Ctrl+F6 in MDI, Alt+Tab in SDI)"));
    askok(s);
}

void consoleclear(control c)
{
    ConsoleData p = getdata(c);

    xbuf l = p->lbuf;
    int oldshift = l->shift;

    l->shift = (l->ns - 1);
    xbufshift(l);
    l->shift = oldshift;
    NEWFV = 0;
    p->r = 0;
    REDRAW;
}
