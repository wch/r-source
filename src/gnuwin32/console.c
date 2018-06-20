/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file console.c
 *  Copyright (C) 1998--2003  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004-8      The R Foundation
 *  Copyright (C) 2004-2018   The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "win-nls.h"
#include <R_ext/Boolean.h>
extern Rboolean mbcslocale;

#define USE_MDI 1
extern void R_ProcessEvents(void);
extern void R_WaitEvent(void);

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <string.h>
#include <ctype.h>
#include <wchar.h>
#include <limits.h>
#include <rlocale.h>
#include <R_ext/Memory.h>
#include "graphapp/ga.h"
#ifdef USE_MDI
#include "graphapp/stdimg.h"
#endif
#include "console.h"
#include "consolestructs.h"
#include "rui.h"
#include "getline/wc_history.h"
#include "Startup.h" /* for CharacterMode */
#include <Fileio.h>

#include <stdint.h>

/* Surrogate Pairs Macro */
#define SURROGATE_PAIRS_HI_MIN  ((uint16_t)0xd800)
#define SURROGATE_PAIRS_HI_MAX  ((uint16_t)0xdbff)
#define SURROGATE_PAIRS_LO_MIN  ((uint16_t)0xdc00)
#define SURROGATE_PAIRS_LO_MAX  ((uint16_t)0xdfff)
#define SURROGATE_PAIRS_BIT_SZ  ((uint32_t)10)
#define SURROGATE_PAIRS_MASK    (((uint16_t)1 << SURROGATE_PAIRS_BIT_SZ)-1)
#define IsSurrogatePairsHi(_h)  (SURROGATE_PAIRS_HI_MIN == \
		      ((uint16_t)(_h) &~ (uint16_t)SURROGATE_PAIRS_MASK ))
#define IsSurrogatePairsLo(_l)  (SURROGATE_PAIRS_LO_MIN == \
		      ((uint16_t)(_l) &~ (uint16_t)SURROGATE_PAIRS_MASK ))

#ifdef __GNUC__
# undef alloca
# define alloca(x) __builtin_alloca((x))
#endif

static void performCompletion(control c);


static inline int wcswidth(const wchar_t *s)
{
    return mbcslocale ? Ri18n_wcswidth(s, wcslen(s)) : wcslen(s);
}

static inline int wcwidth(const wchar_t s)
{
    return mbcslocale ? Ri18n_wcwidth(s) : 1;
}

static void setCURCOL(ConsoleData p)
{
    wchar_t *P = LINE(NUMLINES - 1);
    int w0 = 0;

    for (; P < LINE(NUMLINES - 1) + prompt_len + cur_pos; P++)
	if(*P == L'\r') w0 = 0; else w0 += wcwidth(*P);

    CURCOL = w0;
}


/* xbuf */

xbuf newxbuf(xlong dim, xint ms, xint shift)
{
    xbuf  p;

    p = (xbuf) malloc(sizeof(struct structXBUF));
    if (!p)
	return NULL;
    p->b = (wchar_t *) malloc((dim + 1) * sizeof(wchar_t));
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
    p->s = (wchar_t **) malloc(ms * sizeof(wchar_t *));
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
    *p->b = L'\0';
    return p;
}

/* reallocate increased buffer sizes and update pointers */
void xbufgrow(xbuf p, xlong dim, xint ms)
{
    if(dim > p->dim) {
	wchar_t *ret = (wchar_t *) realloc(p->b, (dim + 1)*sizeof(wchar_t));
	if(ret) {
	    int i, change;
	    change = ret - p->b;
	    p->b = ret;
	    p->av += change;
	    p->free += change;
	    for (i = 0; i < p->ns; i++)  p->s[i] += change;
	    p->dim = dim;
	}
    }
    if(ms > p->ms) {
	wchar_t **ret = (wchar_t **) realloc(p->s, ms * sizeof(wchar_t *));
	if(ret) {
	    int *ret2 = (int *) realloc(p->user, ms * sizeof(int));
	    if(ret2) {
		p->s = ret;
		p->user = ret2;
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
    wchar_t *new0;

    if (p->shift >= p->ns) {
	p->ns = 1;
	p->av = p->dim;
	p->free = p->b;
	p->s[0] = p->b;
	*p->b = L'\0';
	p->user[0] = -1;
	return;
    }
    new0 = p->s[p->shift];
    mshift = new0 - p->s[0];
    memmove(p->b, p->s[p->shift], (p->dim - mshift) * sizeof(wchar_t));
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

void xbufaddxc(xbuf p, wchar_t c)
{
    int   i;

    switch (c) {
    case L'\a':
	gabeep();
	break;
    case L'\b':
	if ((p->s[p->ns - 1])[0]) {
	    p->free--;
	    p->av++;
	}
	break;
    case L'\t':
	XPUTC(L' ');
	*p->free = '\0';
	/* Changed to  width in 2.7.0 */
	for (i = wcswidth(p->s[p->ns - 1]); (i % TABSIZE); i++) XPUTC(L' ');
	break;
    case L'\n':
	XPUTC(L'\0');
	p->s[p->ns] = p->free;
	p->user[p->ns++] = -1;
	break;
    default:
	XPUTC(c);
    }
    *p->free = L'\0';
}

void xbufaddxs(xbuf p, const wchar_t *s, int user)
{
    const wchar_t *ps;
    int   l;

    l = user ? (p->s[p->ns - 1])[0] : -1;
    for (ps = s; *ps; ps++) xbufaddxc(p, *ps);
    p->user[p->ns - 1] = l;
}

#define IN_CONSOLE
#include "rgui_UTF8.h"
extern size_t Rf_utf8towcs(wchar_t *wc, const char *s, size_t n);
static size_t enctowcs(wchar_t *wc, char *s, int n)
{
    size_t nc = 0;
    char *pb, *pe;
    if((pb = strchr(s, UTF8in[0])) && *(pb+1) == UTF8in[1] &&
       *(pb+2) == UTF8in[2]) {
	*pb = '\0';
	nc += mbstowcs(wc, s, n);
	*pb = UTF8in[0]; /* preserve input string, needed in consolewrites */
	pb += 3; pe = pb;
	while(*pe &&
	      !((pe = strchr(pe, UTF8out[0])) && *(pe+1) == UTF8out[1] &&
	      *(pe+2) == UTF8out[2])) pe++;
	if(!*pe) return nc; /* FIXME */;
	*pe = '\0';
	/* convert string starting at pb from UTF-8 */
	nc += Rf_utf8towcs(wc+nc, pb, (pe-pb));
	*pe = UTF8out[0]; /* preserve input string, needed in consolewrites */
	pe += 3;
	nc += enctowcs(wc+nc, pe, n-nc);
    } else nc = mbstowcs(wc, s, n);
    return nc;
}

static void xbufadds(xbuf p, const char *s, int user)
{
    int n = strlen(s) + 1; /* UCS-2 must be no more chars */
    if (n < 1000) {
	wchar_t tmp[n];
	enctowcs(tmp, (char *) s, n);
	xbufaddxs(p, tmp, user);
    } else {
	/* very long line */
	wchar_t *tmp = (wchar_t*) malloc(n * sizeof(wchar_t));
	enctowcs(tmp, (char *) s, n);
	xbufaddxs(p, tmp, user);
	free(tmp);
    }
}

static void xbuffixl(xbuf p)
{
    wchar_t *ps;

    if (!p->ns) return;
    ps = p->s[p->ns - 1];
    p->free = ps + wcslen(ps);
    p->av = p->dim - (p->free - p->b);
}


/* console */

rgb guiColors[numGuiColors] = {
	White, Black, gaRed, /* consolebg, consolefg, consoleuser, */
	White, Black, gaRed, /* pagerbg, pagerfg, pagerhighlight,  */
	White, Black, gaRed, /* dataeditbg, dataeditfg, dataedituser */
	White, Black         /* editorbg, editorfg                 */
};

extern int R_HistorySize;  /* from Defn.h */

ConsoleData
newconsoledata(font f, int rows, int cols, int bufbytes, int buflines,
	       rgb *guiColors, int kind, int buffered, int cursor_blink)
{
    ConsoleData p;

    initapp(0, 0);
    p = (ConsoleData) malloc(sizeof(struct structConsoleData));
    if (!p)
	return NULL;
    p->kind = kind;
    /* PR#14624 claimed this was needed, with no example */
    p->chbrk = p->modbrk = '\0';
    if (kind == CONSOLE) {
	p->lbuf = newxbuf(bufbytes, buflines, SLBUF);
	if (!p->lbuf) {
	    free(p);
	    return NULL;
	}
	p->kbuf = malloc(NKEYS * sizeof(wchar_t));
	if (!p->kbuf) {
	    xbufdel(p->lbuf);
	    free(p);
	    return NULL;
	}
    } else {
	p->lbuf = NULL;
	p->kbuf = NULL;
    }
    BM = NULL;
    p->rows = rows;
    p->cols = cols;
    for (int i=0; i<numGuiColors; i++)
	p->guiColors[i] = guiColors[i];
    p->f = f;
    FH = fontheight(f);
    FW = fontwidth(f);
    WIDTH = (COLS + 1) * FW;
    HEIGHT = (ROWS + 1) * FH + 1; /* +1 avoids size problems in MDI */
    FV = FC = 0;
    NEWFV = NEWFC = 0;
    p->firstkey = p->numkeys = 0;
    p->clp = NULL;
    CURROW = -1;
    p->overwrite = 0;
    p->needredraw = 0;
    p->wipe_completion = 0;
    p->my0 = p->my1 = -1;
    p->mx0 = 5;
    p->mx1 = 14;
    p->sel = 0;
    p->input = 0;
    p->lazyupdate = buffered;
    p->cursor_blink = cursor_blink;
    return (p);
}

static int col_to_pos(ConsoleData p, int x)
{
    if(mbcslocale) {
	int w0 = 0, cnt = 0;
	wchar_t *P = LINE(CURROW);
	for (; w0 < x && *P && P - LINE(CURROW) <= max_pos + prompt_len; ) {
	    w0 += Ri18n_wcwidth(*P++);
	    cnt++;
	}
	return(cnt - prompt_len);
    } else
	return(min(x - prompt_len, max_pos));
}

static int within_input(ConsoleData p, int mx, int my)
{
    return(my == CURROW && mx >= prompt_wid && col_to_pos(p, mx) < max_pos);
}

/* Intersect the mouse selection with the input region. If no overlap or !apply, do nothing*/
static int intersect_input(ConsoleData p, int apply)
{
    int my0 = p->my0, my1 = p->my1, mx0 = p->mx0, mx1 = p->mx1, temp;
    if (my0 > my1 || (my0 == my1 && mx0 > my1)) { /* put them in order */
	temp = my0;
	my0 = my1;
	my1 = temp;
	temp = mx0;
	mx0 = mx1;
	mx1 = temp;
    }

    if (my1 < CURROW || my0 > CURROW) return(0);
    if (my0 < CURROW) mx0 = 0;
    if (my1 > CURROW) mx1 = COLS;
    if (mx1 < CURCOL || col_to_pos(p, mx0) >= max_pos) return(0);
    mx0 = max(mx0, prompt_wid);
    while (col_to_pos(p, mx1) >= max_pos) mx1--;
    if (apply) {
	p->mx0 = mx0;
	p->mx1 = mx1;
	p->my0 = my0;
	p->my1 = my1;
    }
    return(1);
}

/* Here fch and lch are columns, and we have to cope with both MBCS
   and double-width chars. */

static void writelineHelper(ConsoleData p, int fch, int lch,
			    rgb fgr, rgb bgr, int j, int len, wchar_t *s)
{
    rect  r;

    /* This is right, since columns are of fixed size */
    r = rect(BORDERX + fch * FW, BORDERY + j * FH, (lch - fch + 1) * FW, FH);
    gfillrect(BM, bgr, r);

    if (len > FC+fch) {
	/* Some of the string is visible: */
	if(mbcslocale) {
	    int i, w0, nc;
	    wchar_t *P = s, *q;
	    Rboolean leftedge;

	    nc = (wcslen(s) + 1) * sizeof(wchar_t); /* overkill */
	    wchar_t *buff = (wchar_t*) R_alloc(nc, sizeof(wchar_t));
	    q = buff;
	    leftedge = FC && (fch == 0);
	    if(leftedge) fch++;
	    for (w0 = -FC; w0 < fch && *P; P++) /* should have enough ... */
		w0 += Ri18n_wcwidth(*P);
	    /* Now we have got to on or just after the left edge.
	       Possibly have a widechar hanging over.
	       If so, fill with blanks.
	    */
	    if(w0 > fch) for(i = 0; i < w0 - fch; i++) *q++ = L' ';

	    if (leftedge) *q++ = L'$';

	    while (w0 < lch) {
		if(!*P) break;
		w0 += Ri18n_wcwidth(*P);
		if(w0 > lch) break; /* char straddling the right edge
				       is not displayed */
		*q++ = *P++;
	    }
	    if((len > FC+COLS) && (lch == COLS - 1)) *q++ = L'$';
	    else *q++ = *P++;
	    *q = L'\0';
	    gdrawwcs(BM, p->f, fgr, pt(r.x, r.y), buff);
	} else {
	    int last;
	    wchar_t ch, chf, chl;
	    /* we don't know the string length, so modify it in place */
	    if (FC && (fch == 0)) {chf = s[FC]; s[FC] = '$';} else chf = L'\0';
	    if ((len > FC+COLS) && (lch == COLS - 1)) {
		chl = s[FC+lch]; s[FC+lch] = '$';
	    } else chl = L'\0';
	    last = FC + lch + 1;
	    if (len > last) {ch = s[last]; s[last] = L'\0';} else ch = L'\0';
	    gdrawwcs(BM, p->f, fgr, pt(r.x, r.y), &s[FC+fch]);
	    /* restore the string */
	    if (ch) s[last] = ch;
	    if (chl) s[FC+lch] = chl;
	    if (chf) s[FC] = chf;
	}
    }
}

#define WLHELPER(a, b, c, d) writelineHelper(p, a, b, c, d, j, len, s)

/* write line i of the buffer at row j on bitmap */
static int writeline(control c, ConsoleData p, int i, int j)
{
    wchar_t *s, *stmp, *p0;
    int   insel, len, col1, d;
    int   c1, c2, c3, x0, y0, x1, y1;
    rect r;
    int   bg, fg, highlight, base;

    if (p->kind == CONSOLE) base = consolebg;
    else if (p->kind == PAGER) base = pagerbg;
    else base = dataeditbg;

    bg = p->guiColors[base];
    fg = p->guiColors[base+1];
    highlight = p->guiColors[base+2];

    if ((i < 0) || (i >= NUMLINES)) return 0;
    stmp = s = LINE(i);
    len = wcswidth(stmp);
    /* If there is a \r in the line, we need to preprocess it */
    if((p0 = wcschr(s, L'\r'))) {
	int l, l1;
	stmp = LINE(i);
	s = (wchar_t *) alloca((wcslen(stmp) + 1) * sizeof(wchar_t));
	l = p0 - stmp;
	wcsncpy(s, stmp, l);
	stmp = p0 + 1;
	while((p0 = wcschr(stmp, L'\r'))) {
	    l1 = p0 - stmp;
	    wcsncpy(s, stmp, l1);
	    if(l1 > l) l = l1;
	    stmp = p0 + 1;
	}
	l1 = wcslen(stmp);
	wcsncpy(s, stmp, l1);
	if(l1 > l) l = l1;
	s[l] = L'\0';
	len = l; /* for redraw that uses len */
	/* and reset cursor position */
	{
	    wchar_t *P = s;
	    int w0;
	    for (w0 = 0; *P; P++) w0 += wcwidth(*P);
	    CURCOL = w0;
	}
    }
    col1 = COLS - 1;
    insel = p->sel ? ((i - p->my0) * (i - p->my1)) : 1;
    if (insel < 0) {
	WLHELPER(0, col1, bg, fg);
	return len;
    }
    if ((USER(i) >= 0) && (USER(i) < FC + COLS)) {
	if (USER(i) <= FC)
	    WLHELPER(0, col1, highlight, bg);
	else {
	    d = USER(i) - FC;
	    WLHELPER(0, d - 1, fg, bg);
	    WLHELPER(d, col1, highlight, bg);
	}
    } else if (USER(i) == -2) {
	WLHELPER(0, col1, highlight, bg);
    } else
	WLHELPER(0, col1, fg, bg);
    /* This is the cursor, and it may need to be variable-width */
    if ((CURROW >= 0) && (CURCOL >= FC) && (CURCOL < FC + COLS) &&
	(i == NUMLINES - 1) && (p->sel == 0 || !intersect_input(p, 0))) {
	if (!p->overwrite) {
	    if (p->cursor_blink) {
	    	setcaret(c, BORDERX + (CURCOL - FC) * FW, BORDERY + j * FH, 
	    	            p->cursor_blink == 1 ? 1 : FW/4, FH);
	    	showcaret(c, 1);
	    } else showcaret(c, 0);
	    
	    if (p->cursor_blink < 2) {
	    	r = rect(BORDERX + (CURCOL - FC) * FW, BORDERY + j * FH, FW/4, FH);
	    	gfillrect(BM, highlight, r);
	    }
	} else if(mbcslocale) { /* determine the width of the current char */
	    int w0;
	    wchar_t *P = s, wc = 0, nn[2] = L" ";
	    for (w0 = 0; w0 <= CURCOL; P++) {
		wc = *P;
		if(!*P) break;
		w0 += Ri18n_wcwidth(wc);
	    }
	    /* term string '\0' box width = 1 fix */
	    w0 = wc ? Ri18n_wcwidth(wc) : 1;
	    nn[0] = wc;
	    if (p->cursor_blink) {
	    	setcaret(c, BORDERX + (CURCOL - FC) * FW, BORDERY + j * FH, 
	    		    p->cursor_blink == 1 ? 1 : FW/4, FH);
	    	showcaret(c, 1);
	    } else showcaret(c, 0);
	    if (p->cursor_blink < 2) {
	    	r = rect(BORDERX + (CURCOL - FC) * FW, BORDERY + j * FH,
		         w0 * FW, FH);
	    	gfillrect(BM, highlight, r);
	    	gdrawwcs(BM, p->f, bg, pt(r.x, r.y), nn);
	    }
	} else {
	    if (p->cursor_blink) {
		setcaret(c, BORDERX + (CURCOL - FC) * FW, BORDERY + j * FH, 
		            p->cursor_blink == 1 ? 1 : FW, FH);
	    	showcaret(c, 1);
	    } else showcaret(c, 0);
	    if (p->cursor_blink < 2) 
	    	WLHELPER(CURCOL - FC, CURCOL - FC, bg, highlight); 
	}
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
	    int w0, w1 = 1;
	    wchar_t *P = s;
	    for (w0 = 0; w0 < x0; P++) {
		if(!*P) break;
		w1 = Ri18n_wcwidth(*P);
		w0 += w1;
	    }
	    if(w0 > x0) x0 = w0 - w1;
	}
	c1 = (x0 > FC) ? (x0 - FC) : 0;
    } else
	c1 = 0;
    if (i == y1) {
	if (FC > x1) return len;
	if(mbcslocale) {
	    int w0;
	    wchar_t *P = s;
	    for (w0 = 0; w0 <= x1; P++) {
		if(!*P) break;
		w0 += Ri18n_wcwidth(*P);
	    }
	    x1 = w0 - 1;
	}
	c2 = (x1 > FC + COLS) ? (COLS - 1) : (x1 - FC);
    } else
	c2 = COLS - 1;
    WLHELPER(c1, c2, bg, fg);
    return len;
}

void drawconsole(control c, rect r) /* r is unused here */
{
    ConsoleData p = getdata(c);

    int i, ll, wd, maxwd = 0;

    ll = min(NUMLINES, ROWS);
    if(!BM) return;;     /* This is a workaround for PR#1711.
			    BM should never be null here */
    if (p->kind == PAGER)
	gfillrect(BM, p->guiColors[pagerbg], getrect(BM));
    else
	gfillrect(BM, p->guiColors[consolebg], getrect(BM));
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
	writeline(c, p, rw, ww);
	if (ds == 0) {
	    RSHOW(RLINE(ww));
	    return;;
	}
    }
    if (ds == 1) {
	gscroll(BM, pt(0, -FH), RMLINES(0, ROWS - 1));
	if (p->kind == PAGER)
	    gfillrect(BM, p->guiColors[pagerbg], RLINE(ROWS - 1));
	else
	    gfillrect(BM, p->guiColors[consolebg], RLINE(ROWS - 1));
	WRITELINE(fv + ROWS - 1, ROWS - 1);
    }
    else if (ds == -1) {
	gscroll(BM, pt(0, FH), RMLINES(0, ROWS - 1));
	if (p->kind == PAGER)
	    gfillrect(BM, p->guiColors[pagerbg], RLINE(0));
	else
	    gfillrect(BM, p->guiColors[consolebg], RLINE(0));
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
	    li = wcswidth(LINE(NEWFV + i));
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
	p->sel = 1;

	if (within_input(p, p->mx1, p->my1)) {
	    cur_pos = col_to_pos(p, p->mx1);
	    setCURCOL(p);
	}
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
	p->needredraw = 1;
    }
    if (button & LeftButton) {
	p->my0 = FV + pt.y/FH;
	p->mx0 = FC + pt.x/FW;
	if (within_input(p, p->mx0, p->my0) ||
	    (p->my0 == CURROW && p->mx0 > prompt_wid)) {
	    cur_pos = col_to_pos(p, p->mx0);
	    setCURCOL(p);
	    p->needredraw = 1;
	}
    }
    if (p->needredraw) REDRAW;
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
    static wchar_t *free;
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

static void storekey(control c, int k)
{
    ConsoleData p = getdata(c);

    if (p->wipe_completion) {
	p->wipe_completion = 0;
	checkpointpos(p->lbuf, 0);
	/* mark whole of current line as user input */
	USER(NUMLINES-1) = 0;
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

static void storetab(control c)
{
    ConsoleData p = getdata(c);
    p->kbuf[(p->firstkey + p->numkeys) % NKEYS] = L' ';
    p->numkeys++;
}


#include <Rinternals.h>
#include <R_ext/Parse.h>

static int completion_available = -1;

void set_completion_available(int x)
{
    completion_available = x;
}


static void performCompletion(control c)
{
    ConsoleData p = getdata(c);
    int i, alen, alen2, max_show = 10, cursor_position = CURCOL - prompt_wid;
    wchar_t *partial_line = LINE(NUMLINES - 1) + prompt_wid;
    const char *additional_text;
    SEXP cmdSexp, cmdexpr, ans = R_NilValue;
    ParseStatus status;

    if(!completion_available) {
	storetab(c);
	return;
    }

    if(completion_available < 0) {
	char *p = getenv("R_COMPLETION");
	if(p && strcmp(p, "FALSE") == 0) {
	    completion_available = 0;
	    storetab(c);
	    return;
	}
	/* First check if namespace is loaded */
	if(findVarInFrame(R_NamespaceRegistry, install("utils"))
	   != R_UnboundValue) completion_available = 1;
	else { /* Then try to load it */
	    char *p = "try(loadNamespace('utils'), silent=TRUE)";
	    PROTECT(cmdSexp = mkString(p));
	    cmdexpr = PROTECT(R_ParseVector(cmdSexp, -1, &status, R_NilValue));
	    if(status == PARSE_OK) {
		for(i = 0; i < length(cmdexpr); i++)
		    eval(VECTOR_ELT(cmdexpr, i), R_GlobalEnv);
	    }
	    UNPROTECT(2);
	    if(findVarInFrame(R_NamespaceRegistry, install("utils"))
	       != R_UnboundValue) completion_available = 1;
	    else {
		completion_available = 0;
		return;
	    }
	}
    }

    alen = wcslen(partial_line);
    wchar_t orig[alen + 1], pline[2*alen + 1],
            *pchar = pline, achar;
    wcscpy(orig, partial_line);
    for (i = 0; i < alen; i++) {
        achar = orig[i];
	if (achar == '"' || achar == '\\') *pchar++ = '\\';
	*pchar++ = achar;
    }
    *pchar = 0;
    size_t len = wcslen(pline) + 100; 
    char cmd[len];
    snprintf(cmd, len, "utils:::.win32consoleCompletion(\"%ls\", %d)",
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
	wchar_t p1[wcslen(LINE(NUMLINES - 1)) + 1];
	wcscpy(p1, LINE(NUMLINES - 1));
	checkpointpos(p->lbuf, 1);
	size_t len = MB_CUR_MAX * wcslen(p1) + 1; 
	char buf1[len+1];
	snprintf(buf1, len+1, "%ls\n", p1);
	consolewrites(c, buf1);

	for (i = 0; i < min(alen, max_show); i++) {
            consolewrites(c, "\n");
	    consolewrites(c, CHAR(STRING_ELT(VECTOR_ELT(ans, POSSIBLE), i)));
	}
	if (alen > max_show)
	    consolewrites(c, "\n[...truncated]");
	consolewrites(c, "\n");
	p->wipe_completion = 1;
    }

    if (alen2)
	for (i = 0; i < alen2; i++) storekey(c, additional_text[i]);
    return;
}

/* deletes that part of the selection which is on the input line */
static void deleteselected(ConsoleData p)
{
    if (p->sel) {
	int s0, s1;
	wchar_t *cur_line;
	if (intersect_input(p, 1)) {
	    /* convert to bytes after the prompt */
	    s0 = col_to_pos(p, p->mx0);
	    s1 = col_to_pos(p, p->mx1);
	    cur_line = LINE(CURROW) + prompt_len;
	    for(int i = s0; i <= max_pos; i++)
		cur_line[i] = cur_line[i + s1 - s0 + 1];
	    max_pos -= s1 - s0 + 1;
	    cur_line[max_pos] = L'\0';
	    if (cur_pos > s0)
		cur_pos = cur_pos > s1 ? cur_pos - (s1 - s0 + 1) : s0;
	    setCURCOL(p);
	    p->needredraw = 1;
	}
    }
}

/* cmd is in native encoding */
void consolecmd(control c, const char *cmd)
{
    ConsoleData p = getdata(c);

    int i;
    if (p->sel) {
	deleteselected(p);
	p->sel = 0;
	p->needredraw = 1;
	REDRAW;
    }
    storekey(c, BEGINLINE);
    storekey(c, KILLRESTOFLINE);
    if(isUnicodeWindow(c)) {
	size_t sz = (strlen(cmd) + 1) * sizeof(wchar_t);
	wchar_t *wcs = (wchar_t*) R_alloc(strlen(cmd) + 1, sizeof(wchar_t));
	memset(wcs, 0, sz);
	mbstowcs(wcs, cmd, sz-1);
	for(i = 0; wcs[i]; i++) storekey(c, wcs[i]);
    } else {
	const char *ch;
	for (ch = cmd; *ch; ch++) storekey(c, (unsigned char) *ch);
    }
    storekey(c, '\n');
/* if we are editing we save the actual line
   FIXME: not right if Unicode */
    if (CURROW > -1) {
	char buf[2000], *cp; /* maximum 2 bytes/char */
	wchar_t *wc = &(LINE(NUMLINES - 1)[prompt_len]);
	memset(buf, 0, 2000);
	wcstombs(buf, wc, 1000);
	for (cp = buf; *cp; cp++) storekey(c, *cp);
	for (i = max_pos; i > cur_pos; i--) storekey(c, CHARLEFT);
    }
}

static int CleanTranscript(wchar_t *tscpt, wchar_t *cmds)
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
    wchar_t nonprefix[] = L">+ \t\n\r";
    while (*tscpt) {
	if (startofline) {
	    /* skip initial whitespace */
	    while (*tscpt == L' ' || *tscpt == L'\t') tscpt++;
	    /* skip over the prompt prefix */
	    while (*tscpt && !wcschr(nonprefix, *tscpt)) tscpt++;
	    if (*tscpt == L'>' || (incommand && *tscpt == L'+')) {
		tscpt++;
		if (*tscpt == L' ' || *tscpt == L'\t') tscpt++;
		incommand = 1;
	    } else {
		incommand = 0;
	    }
	    startofline = 0;
	} else {
	    if (incommand) {
		if (cmds) *(cmds++) = *tscpt;
		len++;
	    }
	    if (*tscpt == L'\n') startofline = 1;
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
    wchar_t *pc, *new = NULL;
    if (p->sel) {
	deleteselected(p);
	p->sel = 0;
	p->needredraw = 1;
	REDRAW;
     }
    if (p->kind == PAGER) return;
    if ( OpenClipboard(NULL) &&
	 (hglb = GetClipboardData(CF_UNICODETEXT)) &&
	 (pc = (wchar_t *) GlobalLock(hglb)))
    {
	if (p->clp) {
	   new = realloc((void *)p->clp,
			 (wcslen(p->clp) + wcslen(pc) + 1) * sizeof(wchar_t));
	}
	else {
	   new = malloc((wcslen(pc) + 1) * sizeof(wchar_t)) ;
	   if (new) new[0] = L'\0';
	   p->already = p->numkeys;
	   p->pclp = 0;
	}
	if (new) {
	   int i;
	   p->clp = new;
	   /* Surrogate Pairs Block */
	   for (i = 0; i < wcslen(pc); i++)
	       if (IsSurrogatePairsHi(pc[i]) && i+1 < wcslen(pc) &&
		    IsSurrogatePairsLo(pc[i+1]) ) {
		   pc[i] = L'?';
		   pc[i+1] = L'?';
		   i++;
	       }
	   wcscat(p->clp, pc);
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
    wchar_t *pc, *new = NULL;
    if (p->sel) {
	deleteselected(p);
	p->sel = 0;
	p->needredraw = 1;
	REDRAW;
     }
    if (p->kind == PAGER) return;;
    if ( OpenClipboard(NULL) &&
	 (hglb = GetClipboardData(CF_UNICODETEXT)) &&
	 (pc = (wchar_t *) GlobalLock(hglb)))
    {
	if (p->clp) {
	    new = realloc((void *)p->clp,
			  (wcslen(p->clp) + CleanTranscript(pc, 0))
			  * sizeof(wchar_t));
	}
	else {
	    new = malloc(CleanTranscript(pc, 0) * sizeof(wchar_t));
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
    wchar_t *s;

    if(mbcslocale) {
	int w0, x00 = x0, x11=100000;
	i = y0; ll = 1; /* terminator */
	while (i <= y1) {
	    wchar_t *P = LINE(i);
	    for (w0 = 0; w0 < x00 && *P; P++) w0 += Ri18n_wcwidth(*P);
	    x00 = 0;
	    if(i == y1) x11 = x1+1; /* cols are 0-based */
	    while (w0 < x11 && *P) {
		ll++;
		w0 += Ri18n_wcwidth(*P++);
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
	    } else {
		ll += 2;
		i++;
		j = 0;
	    }
	}
    }


    if (!(hglb = GlobalAlloc(GHND, ll * sizeof(wchar_t)))){
	R_ShowMessage(G_("Insufficient memory: text not copied to the clipboard"));
	return;
    }
    if (!(s = (wchar_t *)GlobalLock(hglb))){
	R_ShowMessage(G_("Insufficient memory: text not copied to the clipboard"));
	return;
    }
    if(mbcslocale) {
	int w0, x00 = x0, x11=100000;
	wchar_t *P;
	i = y0;
	while (i <= y1) {
	    P = LINE(i);
	    for (w0 = 0; w0 < x00 && *P; P++) w0 += Ri18n_wcwidth(*P);
	    x00 = 0;
	    if(i == y1) x11 = x1+1;
	    while (w0 < x11 && *P) {
		w0 += Ri18n_wcwidth(*P);
		*s++ = *P++;
	    }
	    if(w0 < x11) *s++ = L'\r'; *s++ = L'\n';
	    i++;
	}
    } else {
	i = y0; j = x0;
	while ((i < y1) || ((i == y1) && (j <= x1))) {
	    wchar_t ch = LINE(i)[j];
	    if (ch) {
		*s++ = ch;
		j++;
	    } else {
		*s++ = L'\r'; *s++ = L'\n';
		i++;
		j = 0;
	    }
	}
    }
    *s = L'\0';
    GlobalUnlock(hglb);
    if (!OpenClipboard(NULL) || !EmptyClipboard()) {
	R_ShowMessage(G_("Unable to open the clipboard"));
	GlobalFree(hglb);
	return;;
    }
    SetClipboardData(CF_UNICODETEXT, hglb);
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
	len = wcswidth(LINE(p->my0));
	if (p->mx0 >= len) p->mx0 = len - 1;
	if (p->mx0 < 0) p->mx0 = 0;
	if (p->my1 >= NUMLINES) p->my1 = NUMLINES - 1;
	if (p->my1 < 0) p->my1 = 0;
	len = wcswidth(LINE(p->my1));
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
       p->mx1 = wcslen(LINE(p->my1));
       REDRAW;
    }
}

/*
   This works in CJK as the IME puts CJK characters in the
   input buffer as 2 bytes, and they are retrieved successively
*/
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
	    p->needredraw = 1;
	    st = -1;
	    break;
	}
    if (p->sel) {
	if (st != -1) deleteselected(p);
	p->needredraw = 1;
	p->sel = 0;
    }
    if (p->needredraw) REDRAW;
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
	 if (p->sel) {
	     if (st == ShiftKey) consolecopy(c);
	     deleteselected(p);
	     p->sel = 0;
	 } else  if (st == CtrlKey)
	     storekey(c, KILLRESTOFLINE);
	 else
	     storekey(c, DELETECHAR);
	 break;
     case ENTER:
	 deleteselected(p);
	 storekey(c, '\n');
	 break;
     case INS:
	 if (st == ShiftKey) {
	     deleteselected(p);
	     consolepaste(c);
	 } else {
	     p->overwrite = !p->overwrite;
	     p->needredraw = 1;
	 }
	 break;
    }
    if (p->sel) {
	p->sel = 0;
	p->needredraw = 1;
    }
    if (p->needredraw) REDRAW;
}

static Rboolean incomplete = FALSE;
int consolewrites(control c, const char *s)
{
    ConsoleData p = getdata(c);

    wchar_t buf[1001];
    if(p->input) {
	int i, len = wcslen(LINE(NUMLINES - 1));
	/* save the input line */
	wcsncpy(buf, LINE(NUMLINES - 1), 1000);
	buf[1000] = L'\0';
	/* now zap it */
	for(i = 0; i < len; i++) xbufaddxc(p->lbuf, L'\b');
	if (incomplete) {
	    NUMLINES--;
	    p->lbuf->free--;
	    p->lbuf->av++;
	}
	USER(NUMLINES - 1) = -1;
    }
    xbufadds(p->lbuf, s, 0);
    FC = 0;
    if(p->input) {
	incomplete = (s[strlen(s) - 1] != '\n');
	if (incomplete) xbufaddxc(p->lbuf, L'\n');
	xbufaddxs(p->lbuf, buf, 1);
    }
    if (strchr(s, '\n')) p->needredraw = 1;
    if (!p->lazyupdate) {
	setfirstvisible(c, NUMLINES - ROWS);
	REDRAW;
    } else if (CURROW >= 0)
	setfirstvisible(c, NUMLINES - ROWS);
    else {
	NEWFV = NUMLINES - ROWS;
	if (NEWFV < 0) NEWFV = 0;
    }
    if(p->input) REDRAW;
    return 0;
}

void freeConsoleData(ConsoleData p)
{
    if (!p) return;
    if (BM) del(BM);
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
static wchar_t consolegetc(control c)
{
    ConsoleData p;
    wchar_t ch;

    p = getdata(c);
    while((p->numkeys == 0) && (!p->clp))
    {
	R_WaitEvent();
	R_ProcessEvents();
    }
    if (p->sel) {
	deleteselected(p);
	p->sel = 0;
	p->needredraw = 1;
	setCURCOL(p); /* Needed? */
	REDRAW;
    }
    if (!p->already && p->clp) {
	ch = p->clp[p->pclp++];
	if (!(p->clp[p->pclp])) {
	    free(p->clp);
	    p->clp = NULL;
	}
    } else {
	if(isUnicodeWindow(c)) {
	    ch = p->kbuf[p->firstkey];
	    p->firstkey = (p->firstkey + 1) % NKEYS;
	    p->numkeys--;
	    if (p->already) p->already--;
	} else {
	    if(mbcslocale) {
		/* Possibly multiple 'keys' for a single keystroke */
		char tmp[20];
		unsigned int used, i;

		for(i = 0; i < MB_CUR_MAX; i++)
		    tmp[i] = p->kbuf[(p->firstkey + i) % NKEYS];
		used = mbrtowc(&ch, tmp, MB_CUR_MAX, NULL);
		p->firstkey = (p->firstkey + used) % NKEYS;
		p->numkeys -= used;
		if (p->already) p->already -= used;
	    } else {
		ch = (unsigned char) p->kbuf[p->firstkey];
		if(ch >=128) {
		    char tmp[2] = " ";
		    tmp[0] = ch;
		    mbrtowc(&ch, tmp, 2, NULL);
		}
		p->firstkey = (p->firstkey + 1) % NKEYS;
		p->numkeys--;
		if (p->already) p->already--;
	    }
	}
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
	WRITELINE(NUMLINES - 1, CURROW);
	RSHOW(RLINE(CURROW));
    }
}

/* This needs to convert the nul-terminated wchar_t string 'in' to a
   sensible strinf in buf[len].  It must not be empty, as R will
   interpret that as EOF, and it should end in \n, as 'in' should do.

   Our strategy is to convert character by character to the current
   Windows locale, using \uxxxx escapes for invalid characters.
*/

static void wcstobuf(char *buf, int len, const wchar_t *in)
{
    int used, tot = 0;
    char *p = buf, tmp[7];
    const wchar_t *wc = in;

    for(; wc; wc++, p+=used, tot+=used) {
	if(tot >= len - 2) break;
	used = wctomb(p, *wc);
	if (used < 0) {
	    snprintf(tmp, 7, "\\u%x", *wc);
	    used = strlen(tmp);
	    memcpy(p, tmp, used);
	}
    }
    *p++ = '\n'; *p = '\0';
}

int consolereads(control c, const char *prompt, char *buf, int len,
		 int addtohistory)
{
    ConsoleData p = getdata(c);

    wchar_t *cur_line, *P;
    wchar_t *aLine;
    int ns0 = NUMLINES, w0 = 0, pre_prompt_len;

    pre_prompt_len = wcslen(LINE(NUMLINES - 1));
    /* print the prompt */
    xbufadds(p->lbuf, prompt, 1);
    if (!xbufmakeroom(p->lbuf, len + 1)) return 1;
    P = aLine = LINE(NUMLINES - 1);
    prompt_len = wcslen(aLine);
    for (; P < aLine + pre_prompt_len; P++)
	if(*P == L'\r') w0 = 0;
	else w0 += mbcslocale ? Ri18n_wcwidth(*P) : 1;
    USER(NUMLINES - 1) = w0;
    prompt_wid = wcswidth(aLine);
    if (NUMLINES > ROWS) {
	CURROW = ROWS - 1;
	NEWFV = NUMLINES - ROWS;
    } else {
	CURROW = NUMLINES - 1;
	NEWFV = 0;
    }
    CURCOL = prompt_wid;
    FC = 0;
    cur_pos = 0;
    max_pos = 0;
    cur_line = &aLine[prompt_len];
    cur_line[0] = L'\0';
    showcaret(c, 1);
    REDRAW;
    for(;;) {
	wchar_t cur_char;
	char chtype; /* boolean */
	p->input = 1;
	cur_char = consolegetc(c);
	p->input = 0;
	chtype = ((unsigned int) cur_char > 0x1f);
	if(NUMLINES != ns0) { /* we scrolled, e.g. cleared screen */
	    cur_line = LINE(NUMLINES - 1) + prompt_len;
	    ns0 = NUMLINES;
	    if (NUMLINES > ROWS) {
		CURROW = ROWS - 1;
		NEWFV = NUMLINES - ROWS;
	    } else {
		CURROW = NUMLINES - 1;
		NEWFV = 0;
	    }
	    USER(NUMLINES - 1) = prompt_wid;
	    p->needredraw = 1;
	}
	if(chtype && (max_pos <= len - 2)) {
	    /* not a control char: we need to fit in the char\n\0 */
	    int i;
	    if(!p->overwrite) {
		for(i = max_pos; i > cur_pos; i--)
		    cur_line[i] = cur_line[i - 1];
	    }
	    cur_line[cur_pos] = cur_char;
	    if(!p->overwrite || cur_pos == max_pos) {
		max_pos += 1;
		cur_line[max_pos] = L'\0';
	    }
	    cur_pos++;
	} else { /* a control char */
	    /* do normal editing commands */
	    int i;
	    switch(cur_char) {
	    case BEGINLINE:
		cur_pos = 0;
		break;
	    case CHARLEFT:
		if(cur_pos > 0) cur_pos--;
		break;
	    case ENDLINE:
		cur_pos = max_pos;
		break;
	    case CHARRIGHT:
		if(cur_pos < max_pos) cur_pos ++;
		break;
	    case KILLRESTOFLINE:
		max_pos = cur_pos;
		cur_line[max_pos] = L'\0';
		break;
	    case KILLLINE:
		max_pos = cur_pos = 0;
		cur_line[max_pos] = L'\0';
		break;
	    case PREVHISTORY:
		P = wgl_hist_prev();
		xbufmakeroom(p->lbuf, wcslen(P) + 1);
		wcscpy(cur_line, P);
		cur_pos = max_pos = wcslen(cur_line);
		break;
	    case NEXTHISTORY:
		P = wgl_hist_next();
		xbufmakeroom(p->lbuf, wcslen(P) + 1);
		wcscpy(cur_line, P);
		cur_pos = max_pos = wcslen(cur_line);
		break;
	    case BACKCHAR:
		if(cur_pos > 0) {
		    cur_pos--;
		    for(i = cur_pos; i <= max_pos - 1; i++)
			cur_line[i] = cur_line[i + 1];
		    max_pos--;
		}
		break;
	    case DELETECHAR:
		if(max_pos == 0) break;
		if(cur_pos < max_pos) {
		    for(i = cur_pos; i <= max_pos - 1; i++)
			cur_line[i] = cur_line[i + 1];
		    max_pos--;
		}
		break;
	    case CHARTRANS:
		if(cur_pos < 1) break;
		if(cur_pos >= max_pos) break;
		cur_char = cur_line[cur_pos];
		cur_line[cur_pos] = cur_line[cur_pos-1];
		cur_line[cur_pos-1] = cur_char;
		break;
	    default:   /* Another control char, or overflow */
		if (chtype || (cur_char == L'\n') || (cur_char == EOFKEY)) {
		    if (chtype) {
			if (cur_pos == max_pos) {
			    consoleunputc(c);
			} else {
			    gabeep();
			    break;
			}
		    }
		    if((cur_char == L'\n') || (cur_char == EOFKEY)) {
			cur_line[max_pos] = L'\n';
			cur_line[max_pos + 1] = L'\0';
		    } else
			cur_line[max_pos] = L'\0';
		    wcstobuf(buf, len, cur_line);
		    //sprintf(buf, "%ls", cur_line);
		    //if(strlen(buf) == 0) strcpy(buf, "invalid input\n");
		    CURROW = -1;
		    cur_line[max_pos] = L'\0';
		    if (max_pos && addtohistory) wgl_histadd(cur_line);
		    xbuffixl(p->lbuf);
		    consolewrites(c, "\n");
		    showcaret(c, 0);
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

void console_im(control c, font *f, point *pt)
{
  ConsoleData p = getdata(c);
  pt->x = BORDERX + CURCOL * FW;
  pt->y = BORDERY + CURROW * FH;
  *f = consolefn;
}

void Rconsolesetwidth(int);
int setWidthOnResize = 0;

int consolecols(console c)
{
    ConsoleData p = getdata(c);

    return COLS;
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
    NEWFV = NUMLINES - ROWS;
    if (NEWFV < 0) NEWFV = 0;
    del(BM);
    BM = newbitmap(r.width, r.height, 2);
    if (!BM) {
       R_ShowMessage(G_("Insufficient memory. Please close the console"));
       return ;
    }
    if(!p->lbuf) return;;    /* don't implement resize if no content
				   yet in pager */
    if (CURROW >= 0) {
	if (NUMLINES > ROWS) {
	    CURROW = ROWS - 1;
	} else
	    CURROW = NUMLINES - 1;
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
char fontname[LF_FACESIZE+4];
int fontsty, pointsize;
int consoler = 25, consolec = 80, consolex = 0, consoley = 0;
int pagerrow = 25, pagercol = 80;
int pagerMultiple = 1, haveusedapager = 0;
int consolebufb = DIMLBUF, consolebufl = MLBUF, consolebuffered = 1;
static int consoleblink = 1;

void
setconsoleoptions(const char *fnname,int fnsty, int fnpoints,
		  int rows, int cols, int consx, int consy,
		  rgb *nguiColors,
		  int pgr, int pgc, int multiplewindows, int widthonresize,
		  int bufbytes, int buflines, int buffered, int cursor_blink)
{
    char msg[LF_FACESIZE + 128];
    strncpy(fontname, fnname, LF_FACESIZE);
    fontname[LF_FACESIZE] = L'\0';
    fontsty =   fnsty;
    pointsize = fnpoints;
    if (consolefn) del(consolefn);
    consolefn = NULL;
    if (strcmp(fontname, "FixedFont")) {
	consolefn = gnewfont(NULL, fnname, fnsty | FixedWidth, fnpoints, 0.0, 1);
	if (!consolefn) {
	    /* This is unlikely to happen: it will find some match */
	    snprintf(msg, LF_FACESIZE + 128,
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
    for (int i=0; i<numGuiColors; i++)
	guiColors[i] = nguiColors[i];
    pagerrow = pgr;
    pagercol = pgc;
    pagerMultiple = multiplewindows;
    setWidthOnResize = widthonresize;
    consolebufb = bufbytes;
    consolebufl = buflines;
    consolebuffered = buffered;
    consoleblink = cursor_blink;
}

void consoleprint(console c)
{
    ConsoleData p = getdata(c);


    printer lpr;
    int cc, rr, fh, cl, cp, clinp, i;
    int top, left;
    int x0, y0, x1, y1;
    font f;
    wchar_t *s = L"";
    char msg[LF_FACESIZE + 128], title[60];
    wchar_t buf[1024];
    cursor cur;
    if (!(lpr = newprinter(0.0, 0.0, ""))) return;;
    show(c);
/*
 * If possible, we avoid to use FixedFont for printer since it hasn't the
 * right size
 */
    f = gnewfont(lpr, strcmp(fontname, "FixedFont") ? fontname : "Courier New",
		 fontsty, pointsize, 0.0, 1);
    if (!f) {
	/* Should not happen but....*/
	snprintf(msg, LF_FACESIZE + 128,
		 G_("Font %s-%d-%d  not found.\nUsing system fixed font"),
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
	len = wcslen(LINE(p->my0));
	if (p->mx0 >= len) p->mx0 = len - 1;
	if (p->mx0 < 0) p->mx0 = 0;
	if (p->my1 >= NUMLINES) p->my1 = NUMLINES - 1;
	if (p->my1 < 0) p->my1 = 0;
	len = wcslen(LINE(p->my1));
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
	x1 = wcslen(LINE(y1));
    }

    cl = y0; /* current line */
    clinp = rr;
    cp = 1; /* current page */

    /* s is possible continuation line */
    while ((cl <= y1) || (*s)) {
	if (clinp + fh >= rr) {
	    if (cp > 1) nextpage(lpr);
	    gdrawstr(lpr, f, Black, pt(left, top), title);
	    snprintf(msg, LF_FACESIZE + 128, "Page %d", cp++);
	    gdrawstr(lpr, f, Black,
		     pt(cc - gstrwidth(lpr, f, msg) - 1, top),
		     msg);
	    clinp = top + 2 * fh;
	}
	if (!*s) {
	    if (cl == y0) s = LINE(cl++) + x0;
	    else if (cl < y1) s = LINE(cl++);
	    else if (cl == y1) {
		s = wcsncpy(buf, LINE(cl++), 1023);
		s[min(x1, 1023) + 1] = L'\0';
	    } else break;
	}
	if (!*s) {
	    clinp += fh;
	} else {
	    wchar_t lc = L'\0';
	    for (i = wcslen(s); i > 0; i--) {
		lc = s[i];
		s[i] = L'\0';
		if (gwcswidth(lpr, f, s) < cc) break;
		s[i] = lc;
	    }
	    gdrawwcs(lpr, f, Black, pt(left, clinp), s);
	    clinp += fh;
	    s[i] = lc;
	    s = s + i;
	}
    }

    if (f != FixedFont) del(f);
    del(lpr);
    setcursor(cur);
}

FILE *R_wfopen(const wchar_t *filename, const wchar_t *mode);

void consolesavefile(console c, int pager)
{
    ConsoleData p = getdata(c);

    wchar_t *fn;
    cursor cur;
    FILE *fp;
    int x0, y0, x1, y1, cl;
    wchar_t *s, buf[1024];

    setuserfilterW(L"Text files (*.txt)\0*.txt\0All files (*.*)\0*.*\0\0");
    if(p->sel)
	fn = askfilesaveW(G_("Save selection to"), "lastsave.txt");
    else
	fn = askfilesaveW(G_("Save console contents to"), "lastsave.txt");
    show(c);
    if (fn) {
	fp = R_wfopen(fn, L"wt");
	if (!fp) return;
	cur = currentcursor();
	setcursor(WatchCursor);

	/* Look for a selection */
	if (p->sel) {
	    int len, c1, c2, c3;
	    if (p->my0 >= NUMLINES) p->my0 = NUMLINES - 1;
	    if (p->my0 < 0) p->my0 = 0;
	    len = wcslen(LINE(p->my0));
	    if (p->mx0 >= len) p->mx0 = len - 1;
	    if (p->mx0 < 0) p->mx0 = 0;
	    if (p->my1 >= NUMLINES) p->my1 = NUMLINES - 1;
	    if (p->my1 < 0) p->my1 = 0;
	    len = wcslen(LINE(p->my1));
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
	    x1 = wcslen(LINE(y1));
	}

	for (cl = y0; cl <= y1; cl++) {
	    if (cl == y0) s = LINE(cl) + x0;
	    else if (cl < y1) s = LINE(cl);
	    else if (cl == y1) {
		s = wcsncpy(buf, LINE(cl), 1023);
		s[min(x1, 1023) + 1] = L'\0';
	    } else break;
	    fputws(s, fp); fputc('\n', fp);
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
		       guiColors,
		       CONSOLE, consolebuffered, consoleblink);
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
    setbackground(c, guiColors[consolebg]);
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
    setim(c, console_im);
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
    strcat(s,G_("     Del or Ctrl+D: delete current character or selection;\n"));
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
    strcat(s,G_("     Ctrl+O or INS: Toggle overwrite mode: initially off.\n"));
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
    CURROW = 0;
    REDRAW;
}
