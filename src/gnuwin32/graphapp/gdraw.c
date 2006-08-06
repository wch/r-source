/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2004  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2005-6      The R Development Core Team
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

/*
   A version of drawing.c without current/state.
   More safe in case of multiple redrawing
 */

#include <config.h> /* for SUPPORT_UTF8 */

#include "internal.h"
extern unsigned int TopmostDialogs; /* from dialogs.c */
#include <winbase.h>
#include <wchar.h>
#define alloca(x) __builtin_alloca((x)) /* always GNUC */

static int getcharset(void);

static HDC GETHDC(drawing d)
{
    if (!d) {
	DebugBreak();
	return (HDC) 0; /* We should never get here, but we do? */
    }
    if ( (d->kind == PrinterObject) || (d->kind == MetafileObject)) {
	HDC dc = (HDC) d->handle ;
	SelectObject(dc, GetStockObject(NULL_PEN));
	SelectObject(dc, GetStockObject(NULL_BRUSH));
	return dc ;
    }
    else
	return get_context(d);
}


/*
 *  Some clipping functions.
 */
rect ggetcliprect(drawing d)
{
    RECT R;
    rect r;
    HDC dc = GETHDC(d);
    GetClipBox(dc, &R);
    r.x = R.left;
    r.y = R.top;
    r.width = R.right - R.left;
    r.height = R.bottom - R.top;
    return r;
}

void gsetcliprect(drawing d, rect r)
{
    HRGN rgn;
    HDC dc = GETHDC(d);
    rgn = CreateRectRgn(r.x, r.y, r.x + r.width, r.y + r.height);
    SelectClipRgn(dc, rgn);
    DeleteObject(rgn);
}


void gbitblt(drawing db, drawing sb, point p, rect r)
{
    HDC src;
    HDC dst;

    dst = GETHDC(db);
    src = GETHDC(sb);
    BitBlt(dst, p.x, p.y, r.width, r.height, src, r.x, r.y, SRCCOPY);
}



/* dp gives the amount to scroll; r the full rectangle to scroll */
void gscroll(drawing d, point dp, rect r)
{
    HDC dc = GETHDC(d);
    RECT rr ;
    rr.left = r.x;
    rr.top = r.y;
    rr.right = r.x + r.width;
    rr.bottom = r.y + r.height;
    ScrollDC(dc, dp.x , dp.y , &rr, &rr, 0, NULL);
}

void ginvert(drawing d, rect r)
{
    HDC dc = GETHDC(d);

    PatBlt(dc, r.x, r.y, r.width, r.height, DSTINVERT);
}

rgb ggetpixel(drawing d, point p)
{
    rgb c;
    HDC dc = GETHDC(d);

    c = GetPixel(dc, p.x, p.y);
    c = ((c&0x000000FFL)<<16) | (c&0x0000FF00L) |
	((c&0x00FF0000L)>>16);
    return c;
}

static COLORREF getwinrgb(drawing d, rgb c)
{
    int r, g, b;
    long luminance;
    int depth;

    r = (int) ((c >> 16) & 0x000000FFL);
    g = (int) ((c >>  8) & 0x000000FFL);
    b = (int) ((c >>  0) & 0x000000FFL);
    depth = getdepth(d);
    if (depth <= 2)	/* map to black or white, or grey if c == Grey */
    {
	luminance = (r*3 + g*5 + b) / 9;
	if (luminance > 0x0087)		r = g = b = 0x00FF;
	else if (luminance <= 0x0077)	r = g = b = 0x0000;
	else				r = g = b = 0x0080;
	c = rgb(r, g, b);
    }
    return RGB(r, g, b);
}

void gsetpixel(drawing d, point p, rgb c)
{
    HDC dc = GETHDC(d);
    HBRUSH br = CreateSolidBrush(getwinrgb(d, c));

    fix_brush(dc, d, br);
    SelectObject(dc, br);
    PatBlt(dc, p.x, p.y, 1, 1, PATCOPY);
    SelectObject(dc, GetStockObject(NULL_BRUSH));
    DeleteObject(br);
}

typedef struct {
    HDC dc;
    int len2; /* squared length of current dash */
    int curseg, on; /* current dash (0-7), on/off flag */
    int style, width;
    int curx, cury; /* start of current dash */
} DashStruct;

static int npieces;

static void CALLBACK  gLineHelper(int x, int y, LPARAM aa)
{
    DashStruct *a = (DashStruct *) aa;
    int distx, disty;

    npieces++;
    distx = x - (a->curx);
    disty = y - (a->cury);
    if (distx*distx + disty*disty >= (a->len2)) {
	if (a->on)
	    LineTo(a->dc, x, y);
	else
	    MoveToEx(a->dc, x, y, NULL);
	a->curx = x;
	a->cury = y;
	a->len2 = 0;
	while (!a->len2) {
	    a->curseg = (a->curseg + 4) % 32;
	    a->len2 = (((a->style) >> (a->curseg)) & 15) * (a->width);
	    a->len2 = (a->len2) * (a->len2);
	    a->on = (a->on) ? 0 : 1;
	}
    }
}

void gdrawline(drawing d, int width, int style, rgb c, point p1, point p2,
	       int fast, int lend, int ljoin, float lmitre)
{
    point p[2];
    p[0] = p1;
    p[1] = p2;
    gdrawpolyline( d, width, style, c, p, 2, 0, fast, lend, ljoin, lmitre);
}

void gdrawpolyline(drawing d, int width, int style, rgb c,
                   point p[], int n, int closepath, int fast,
		   int lend, int ljoin, float lmitre)
{
    int tmpx, tmpy, tmp;
    HDC dc = GETHDC(d);
    COLORREF winrgb = getwinrgb(d, c);
    LOGBRUSH lb;
    HPEN gpen;
    int i;
    float oldmitre;

    if (n < 2) return;
    lb.lbStyle = BS_SOLID;
    lb.lbColor = winrgb;
    lb.lbHatch = 0;
    SetMiterLimit(dc, lmitre, &oldmitre);
    if (!style) {
	if (fast)
	    gpen = CreatePen(PS_INSIDEFRAME, width, winrgb);
	else
	    gpen = ExtCreatePen(PS_GEOMETRIC|PS_SOLID|lend|ljoin,
				width, &lb, 0, NULL);
	SelectObject(dc, gpen);
	SetROP2(dc, R2_COPYPEN);
	npieces = 0;
	BeginPath(dc);
        MoveToEx(dc, p[0].x, p[0].y, NULL);
        for (i = 1; i < n ; i++) {
	    LineTo(dc, p[i].x, p[i].y);
	    npieces++;
	    if (npieces > 1000) {
		EndPath(dc);
		StrokePath(dc);
		npieces = 0;
		BeginPath(dc);
	    }
	}
        if (closepath) LineTo(dc, p[0].x, p[0].y);
	EndPath(dc);
	StrokePath(dc);
	SelectObject(dc, GetStockObject(NULL_PEN));
	DeleteObject(gpen);
    }
    else {
	DashStruct a;
	gpen = ExtCreatePen(PS_GEOMETRIC|PS_SOLID|lend|ljoin,
			    width, &lb, 0, NULL);
	SelectObject(dc, gpen);
	SetROP2(dc, R2_COPYPEN);
	a.on = 1;
	a.dc = dc;
	a.len2 = (style & 15) * width;
	a.len2 = (a.len2) * (a.len2);
	a.curseg = 0;
	a.style = style;
	a.width = width;
	a.curx = p[0].x;
	a.cury = p[0].y;
	MoveToEx(dc, p[0].x, p[0].y, NULL);
	npieces = 0;
	BeginPath(dc);
        for (i = 1; i < n; i++) {
	    LineDDA(p[i-1].x, p[i-1].y, p[i].x, p[i].y, gLineHelper,
		    (LPARAM) &a);
	    if ((p[i].x != a.curx) || (p[i].y != a.cury)) {
		if (a.on) LineTo(dc, p[i].x, p[i].y);
		else MoveToEx(dc, p[i].x, p[i].y, NULL);
		tmpx = (a.curx-p[i].x);
		tmpy = (a.cury-p[i].y);
		tmp = tmpx*tmpx + tmpy*tmpy;
		a.len2 = a.len2 + tmp - 2*sqrt((double)(tmp*a.len2));
		a.curx = p[i].x;
		a.cury = p[i].y;
	    }
	    npieces++;
	    if (npieces > 1000) {
		EndPath(dc);
		StrokePath(dc);
		npieces = 0;
		BeginPath(dc);
	    }
        }
        if (closepath) {
	    LineDDA(p[n-1].x, p[n-1].y, p[0].x, p[0].y, gLineHelper,
		    (LPARAM) &a);
	    if (a.on) LineTo(dc,p[0].x,p[0].y);
        }
	EndPath(dc);
	StrokePath(dc);
	SelectObject(dc, GetStockObject(NULL_PEN));
	DeleteObject(gpen);
    }
}

void gdrawrect(drawing d, int width, int style, rgb c, rect r, int fast,
	       int lend, int ljoin, float lmitre)
{
    int x0 = r.x;
    int y0 = r.y;
    int x1 = r.x + r.width;
    int y1 = r.y + r.height;
    point p[4];
    p[0] = pt(x0,y0);
    p[1] = pt(x1,y0);
    p[2] = pt(x1,y1);
    p[3] = pt(x0,y1);
    gdrawpolyline(d, width, style, c, p, 4, 1, fast, lend, ljoin, lmitre);
}

void gfillrect(drawing d, rgb fill, rect r)
{
    HDC dc = GETHDC(d);
    HBRUSH br = CreateSolidBrush(getwinrgb(d, fill));
    fix_brush(dc, d, br);
    SelectObject(dc, br);
    PatBlt(dc, r.x, r.y, r.width, r.height, PATCOPY);
    SelectObject(dc, GetStockObject(NULL_BRUSH));
    DeleteObject(br);
}

void gdrawellipse(drawing d, int width, rgb border, rect r, int fast,
		  int lend, int ljoin, float lmitre)
{
    HDC dc = GETHDC(d);
    LOGBRUSH lb;
    HPEN gpen;
    float oldmitre;

    if (fast)
	gpen = CreatePen(PS_INSIDEFRAME, width, getwinrgb(d, border));
    else {
	SetMiterLimit(dc, lmitre, &oldmitre);
	lb.lbStyle = BS_SOLID;
	lb.lbColor = getwinrgb(d, border);
	lb.lbHatch = 0;
	gpen = ExtCreatePen(PS_GEOMETRIC|PS_SOLID|lend|ljoin,
			    width, &lb, 0, NULL);
    }
    SelectObject(dc, gpen);
    SetROP2(dc, R2_COPYPEN);
    Ellipse(dc, r.x, r.y, r.x+r.width, r.y+r.height);
    SelectObject(dc, GetStockObject(NULL_PEN));
    DeleteObject(gpen);
}

void goldfillellipse(drawing d, rgb fill, rect r)
{
    HDC dc = GETHDC(d);
    HBRUSH br = CreateSolidBrush(getwinrgb(d, fill));
    fix_brush(dc, d, br);
    SelectObject(dc, br);
    Ellipse(dc, r.x, r.y, r.x+r.width, r.y+r.height);
    SelectObject(dc, GetStockObject(NULL_BRUSH));
    DeleteObject(br);
}


#ifndef fastfillrect
#define fastfillrect(x, y, w, h) PatBlt(dc, (x), (y), (w), (h), mode)
#endif

void gfillellipse(drawing d, rgb fill, rect r)
{			/* e(x,y) = b*b*x*x + a*a*y*y - a*a*b*b */
    register long mode = PATCOPY;
    int w_odd = (r.width & 0x0001);
    int h_odd = (r.height & 0x0001);
    int a = r.width >> 1;
    int b = r.height >> 1;
    point c = pt(r.x+a,r.y+b);
    int x = 0;
    int y = b;
    long a2 = a*a;
    long b2 = b*b;
    long xcrit = ((a2+a2+a2) >> 2) + 1;
    long ycrit = ((b2+b2+b2) >> 2) + 1;
    long t = b2 + a2 - (a2+a2)*b;	/* t = e(x+1,y-1) */
    long dxt = b2*(3+x+x);
    long dyt = a2*(3-y-y);
    int d2xt = b2+b2;
    int d2yt = a2+a2;
    int stored = 0;
    int sx = 0, sy = 0, sh = 0; /* stored values of x, y, height */
    HDC dc;
    HBRUSH br;

    if ((r.width > 31) && (r.height > 31)) {
	goldfillellipse(d, fill, r);
	return;
    }
    if ((r.width < 3) || (r.height < 3)) {
	gfillrect(d, fill, r);
	return;
    }
    dc = GETHDC(d);
    br = CreateSolidBrush(getwinrgb(d, fill));
    fix_brush(dc, d, br);
    SelectObject(dc, br);

    if (w_odd == 0) {
	fastfillrect(c.x-1,c.y-b,2,r.height);
    }

    while (y > 0) {
	if (stored) {
	    if (sx != x) { /* output stored rect */
		fastfillrect(c.x-sx,c.y-sy, sx+sx+w_odd,sh);
		fastfillrect(c.x-sx,c.y+sy+h_odd-sh, sx+sx+w_odd,sh);
		stored = 0;
	    }
	    else /* increment height of stored rect */
		sh++;
	}
	if (t + a2*y < xcrit) { /* e(x+1,y-1/2) <= 0 */
	    /* move left and right to encounter edge */
	    x += 1;
	    t += dxt;
	    dxt += d2xt;
	} else if (t - b2*x >= ycrit) { /* e(x+1/2,y-1) > 0 */
	    /* drop down one line */
	    if (!stored) {
		sx = x;
		sy = y;
		sh = 1;
		stored = 1;
	    }
	    y -= 1;
	    t += dyt;
	    dyt += d2yt;
	} else {
	    /* drop diagonally down and out */
	    if (!stored) {
		sx = x;
		sy = y;
		sh = 1;
		stored = 1;
	    }

	    x += 1;
	    y -= 1;
	    t += dxt + dyt;
	    dxt += d2xt;
	    dyt += d2yt;
	}
    }
    if (stored) { /* output stored rectangle */
	fastfillrect(c.x-sx, c.y-sy, sx+sx+w_odd, sh);
	fastfillrect(c.x-sx, c.y+sy+h_odd-sh, sx+sx+w_odd, sh);
	stored = 0;
    }
    if (x <= a){
	fastfillrect(c.x-a, c.y-y, a+a+w_odd, 1);
	fastfillrect(c.x-a, c.y+y-1+h_odd, a+a+w_odd, 1);
    }
    SelectObject(dc, GetStockObject(NULL_BRUSH));
    DeleteObject(br);
}


void gfillpolygon(drawing d, rgb fill, point *p, int n)
{
    HDC dc = GETHDC(d);
    HBRUSH br = CreateSolidBrush(getwinrgb(d,fill));
    fix_brush(dc, d, br);
    SelectObject(dc, br);
    Polygon(dc, (POINT FAR *) p, n);
    SelectObject(dc, GetStockObject(NULL_BRUSH));
    DeleteObject(br);
}

/* For ordinary text, e.g. in console */
int gdrawstr(drawing d, font f, rgb c, point p, char *s)
{
    POINT curr_pos;
    int width;
    HFONT old;
    HDC dc = GETHDC(d);

    SetTextColor(dc, getwinrgb(d,c));
    old = SelectObject(dc, f->handle);
    MoveToEx(dc, p.x, p.y, NULL);
    SetBkMode(dc, TRANSPARENT);
    SetTextAlign(dc, TA_TOP | TA_LEFT | TA_UPDATECP);

    if (is_NT && (localeCP != GetACP())) {
	/* This allows us to change locales and output in the new locale */
	wchar_t *wc; int n = strlen(s), cnt;
	wc = alloca((n+1) * sizeof(wchar_t));
	cnt = mbstowcs(wc, s, n);
	TextOutW(dc, p.x, p.y, wc, cnt);
    } else
	TextOut(dc, p.x, p.y, s, strlen(s));

    GetCurrentPositionEx(dc, &curr_pos);
    width = curr_pos.x - p.x;
    SelectObject(dc, old);

    return width;
}

/* This version aligns on baseline, and allows hadj = 0, 0.5, 1 */
void gdrawstr1(drawing d, font f, rgb c, point p, char *s, double hadj)
{
    HFONT old;
    HDC dc = GETHDC(d);
    UINT flags = TA_BASELINE | TA_UPDATECP;

    SetTextColor(dc, getwinrgb(d,c));
    old = SelectObject(dc, f->handle);
    MoveToEx(dc, p.x, p.y, NULL);
    SetBkMode(dc, TRANSPARENT);
    if (hadj < 0.25) flags |= TA_LEFT;
    else if (hadj < 0.75) flags |= TA_CENTER;
    else flags |= TA_RIGHT;
    SetTextAlign(dc, flags);
    TextOut(dc, p.x, p.y, s, strlen(s));
    SelectObject(dc, old);
}

/* This version interprets 's' as MBCS in the current locale */
void gwdrawstr1(drawing d, font f, rgb c, point p, char *s, double hadj)
{
    HFONT old;
    HDC dc = GETHDC(d);
    UINT flags = TA_BASELINE | TA_UPDATECP;
    wchar_t *wc; int n = strlen(s), cnt;
    wc = alloca((n+1) * sizeof(wchar_t));

    SetTextColor(dc, getwinrgb(d,c));
    old = SelectObject(dc, f->handle);
    MoveToEx(dc, p.x, p.y, NULL);
    SetBkMode(dc, TRANSPARENT);
    if (hadj < 0.25) flags |= TA_LEFT;
    else if (hadj < 0.75) flags |= TA_CENTER;
    else flags |= TA_RIGHT;
    SetTextAlign(dc, flags);
    cnt = mbstowcs(wc, s, n); /* This is OK if we get an error */
    TextOutW(dc, p.x, p.y, wc, cnt);
    SelectObject(dc, old);
}

#ifdef SUPPORT_UTF8
#include <wchar.h>
size_t Rmbstowcs(wchar_t *wc, const char *s, size_t n);

void gwdrawstr(drawing d, font f, rgb c, point p, char *s, double hadj)
{
    HFONT old;
    HDC dc = GETHDC(d);
    UINT flags = TA_BASELINE | TA_UPDATECP;
    wchar_t wc[1000]; int cnt;

    cnt = Rmbstowcs(wc, s, 1000);
    SetTextColor(dc, getwinrgb(d,c));
    old = SelectObject(dc, f->handle);
    MoveToEx(dc, p.x, p.y, NULL);
    SetBkMode(dc, TRANSPARENT);
    if (hadj < 0.25) flags |= TA_LEFT;
    else if (hadj < 0.75) flags |= TA_CENTER;
    else flags |= TA_RIGHT;
    SetTextAlign(dc, flags);
    TextOutW(dc, p.x, p.y, wc, cnt);
    SelectObject(dc, old);
}
#endif

rect gstrrect(drawing d, font f, char *s)
{
    SIZE size;
    HFONT old;
    HDC dc;
    if (! f)
	f = SystemFont;
    if (d)
	dc = GETHDC(d);
    else
	dc = GetDC(0);
    old = SelectObject(dc, f->handle);
    GetTextExtentPoint32(dc, (LPSTR)s, strlen(s), &size);
    SelectObject(dc, old);
    if (!d) ReleaseDC(0,dc);
    return rect(0, 0, size.cx, size.cy);
}

point gstrsize(drawing d, font f, char *s)
{
    rect r = gstrrect(d, f, s);
    return pt(r.width, r.height);
}

int gstrwidth(drawing d, font f, char *s)
{
    rect r = gstrrect(d, f ,s);
    return r.width;
}

#ifdef SUPPORT_UTF8
static rect gwstrrect(drawing d, font f, char *s)
{
    SIZE size;
    HFONT old;
    HDC dc;
    wchar_t wc[1000]; int cnt;

    cnt = Rmbstowcs(wc, s, 1000);
    if (! f)
	f = SystemFont;
    if (d)
	dc = GETHDC(d);
    else
	dc = GetDC(0);
    old = SelectObject(dc, f->handle);
    GetTextExtentPoint32W(dc, wc, cnt, &size);
    SelectObject(dc, old);
    if (!d) ReleaseDC(0,dc);
    return rect(0, 0, size.cx, size.cy);
}

int gwstrwidth(drawing d, font f, char *s)
{
    rect r = gwstrrect(d,f,s);
    return r.width;
}
#endif

int ghasfixedwidth(font f)
{
    TEXTMETRIC tm;
    HFONT old;
    HDC dc = GetDC(0);
    old = SelectObject(dc, (HFONT)f->handle);
    GetTextMetrics(dc, &tm);
    SelectObject(dc, old);
    ReleaseDC(0,dc);
    return !(tm.tmPitchAndFamily & TMPF_FIXED_PITCH);
}


void gcharmetric(drawing d, font f, int c, int *ascent, int *descent,
		 int *width)
{
    int first, last, extra;
    TEXTMETRIC tm;
    HFONT old;
    HDC dc = GETHDC(d);
    old = SelectObject(dc, (HFONT)f->handle);
    GetTextMetrics(dc, &tm);
    first = tm.tmFirstChar;
    last = tm.tmLastChar;
    extra = tm.tmExternalLeading + tm.tmInternalLeading - 1;
    if (c < 0) { /* used for setting cra */
	SIZE size;
	char *cc = "M";
	GetTextExtentPoint32(dc,(LPSTR) cc, 1, &size);
	*descent = tm.tmDescent ;
	*ascent = size.cy - *descent;
	*width = size.cx;
	if (*width > size.cy) *width = size.cy;
    } else if (c == 0) {
	*descent = tm.tmDescent ;
        *ascent = tm.tmHeight - *descent - extra ;
	*width = tm.tmMaxCharWidth ;
    } else if ((first <= c) && (c <= last)) {
	SIZE size;
	GetTextExtentPoint32(dc, (LPSTR) &c, 1, &size);
	*descent = tm.tmDescent ;
	*ascent = size.cy - *descent - extra ;
	*width = size.cx;
	/*
	  Under NT, ' ' gives 0 ascent and descent, which seems
	  correct but this : (i) makes R engine to center in random way;
	  (ii) doesn't correspond to what 98 and X do (' ' is there
	  high as the full font)
	*/
	if ((c != ' ') && (tm.tmPitchAndFamily & TMPF_TRUETYPE)) {
	    GLYPHMETRICS gm;
	    MAT2 m2;
	    m2.eM11.value = m2.eM22.value = (WORD) 1 ;
	    m2.eM21.value = m2.eM12.value = (WORD) 0 ;
	    m2.eM11.fract = m2.eM12.fract = m2.eM21.fract = m2.eM22.fract =
		(short) 0 ;
	    if (GetGlyphOutline(dc, c, GGO_METRICS, &gm, 0, NULL, &m2)
		!= GDI_ERROR) {
		*descent = gm.gmBlackBoxY - gm.gmptGlyphOrigin.y ;
		*ascent  = gm.gmptGlyphOrigin.y + 1;
	    }
	}
    } else {
	*ascent = 0;
	*descent = 0;
	*width = 0;
    }
    SelectObject(dc, old);
}

/* This needs to work even when not on NT */
void gwcharmetric(drawing d, font f, int c, int *ascent, int *descent,
		  int *width)
{
    if (is_NT) {
	int first, last, extra;
	TEXTMETRICW tm;
	HFONT old;
	HDC dc = GETHDC(d);
	old = SelectObject(dc, (HFONT)f->handle);
	GetTextMetricsW(dc, &tm);
	first = tm.tmFirstChar;
	last = tm.tmLastChar;
	extra = tm.tmExternalLeading + tm.tmInternalLeading - 1;
	if (c < 0) { /* used for setting cra */
	    SIZE size;
	    char *cc = "M";
	    GetTextExtentPoint32(dc,(LPSTR) cc, 1, &size);
	    *descent = tm.tmDescent ;
	    *ascent = size.cy - *descent;
	    *width = size.cx;
	    if (*width > size.cy) *width = size.cy;
	} else if (c == 0) {
	    *descent = tm.tmDescent ;
	    *ascent = tm.tmHeight - *descent - extra ;
	    *width = tm.tmMaxCharWidth ;
	} else if ((first <= c) && (c <= last)) {
	    SIZE size;
	    wchar_t wc = c;
	    GetTextExtentPoint32W(dc, &wc, 1, &size);
	    *descent = tm.tmDescent ;
	    *ascent = size.cy - *descent - extra ;
	    *width = size.cx;
	    /*
	      Under NT, ' ' gives 0 ascent and descent, which seems
	      correct but this : (i) makes R engine to center in random way;
	      (ii) doesn't correspond to what 98 and X do (' ' is there
	      high as the full font)
	    */
	    if ((c!=' ') && (tm.tmPitchAndFamily & TMPF_TRUETYPE)) {
		GLYPHMETRICS gm;
		MAT2 m2;
		m2.eM11.value = m2.eM22.value = (WORD) 1 ;
		m2.eM21.value = m2.eM12.value = (WORD) 0 ;
		m2.eM11.fract = m2.eM12.fract =
		    m2.eM21.fract = m2.eM22.fract =  (short) 0 ;
		if (GetGlyphOutlineW(dc, c, GGO_METRICS, &gm, 0, NULL, &m2)
		    != GDI_ERROR) {
		    *descent = gm.gmBlackBoxY - gm.gmptGlyphOrigin.y ;
		    *ascent  = gm.gmptGlyphOrigin.y + 1;
		}
	    }
	} else { /* Unicode char out of range */
	    *ascent = 0;
	    *descent = 0;
	    *width = 0;
	}
	SelectObject(dc, old);
    } else {
	if (c > 127) {
	    int extra;
	    TEXTMETRIC tm;
	    HFONT old;
	    HDC dc = GETHDC(d);
	    SIZE size;
	    char s[3];
	    old = SelectObject(dc, (HFONT)f->handle);
	    GetTextMetrics(dc, &tm);
	    extra = tm.tmExternalLeading + tm.tmInternalLeading - 1;
	    /* choose some reasonable fallback values */
	    *ascent = tm.tmAscent; *descent = tm.tmDescent; 
	    *width = tm.tmAveCharWidth;
	    if (wctomb(s, (wchar_t) c) >= 1 &&
	       GetTextExtentPoint32(dc, (LPSTR) s, 1, &size))
	    {
		*ascent = size.cy - *descent - extra;
		*width = size.cx;
	    }
	    SelectObject(dc, old);
	} else
	    gcharmetric(d, f, c, ascent, descent, width);
    }
}

font gnewfont(drawing d, char *face, int style, int size, double rot)
{
    font obj;
    HFONT hf;
    LOGFONT lf;

    if ((rot <= 45.0) || ((rot > 135) && (rot <= 225)) || (rot > 315))
	lf.lfHeight = -MulDiv(size, devicepixelsy(d), 72);
    else
	lf.lfHeight = -MulDiv(size, devicepixelsx(d), 72);

    lf.lfWidth = 0 ;
    lf.lfEscapement = lf.lfOrientation = (int) 10*rot;
    lf.lfWeight = FW_NORMAL;
    lf.lfItalic = lf.lfUnderline = lf.lfStrikeOut = 0;
    if ((! strcmp(face, "Symbol")) || (! strcmp(face, "Wingdings")))
	lf.lfCharSet = SYMBOL_CHARSET;
    else
        lf.lfCharSet = getcharset();
    lf.lfClipPrecision = CLIP_DEFAULT_PRECIS;
    lf.lfQuality = DEFAULT_QUALITY;
    lf.lfPitchAndFamily = DEFAULT_PITCH | FF_DONTCARE;
    if ((strlen(face) > 1) && (face[0] == 'T') && (face[1] == 'T')) {
        char *pf;
        lf.lfOutPrecision = OUT_TT_ONLY_PRECIS;
        for (pf = &face[2]; isspace(*pf) ; pf++);
        strncpy(lf.lfFaceName, pf, LF_FACESIZE-1);
    }
    else {
        lf.lfOutPrecision = OUT_DEFAULT_PRECIS;
        strncpy(lf.lfFaceName, face, LF_FACESIZE-1);
    }
    if (style & Italic)
	lf.lfItalic = 1;
    if (style & Bold)
	lf.lfWeight = FW_BOLD;
    if (style & FixedWidth)
	lf.lfPitchAndFamily |= FIXED_PITCH;
    if (style & SansSerif)
	lf.lfPitchAndFamily |= FF_SWISS;

    if ((hf = CreateFontIndirect(&lf)) == 0)
	return NULL;

    obj = new_font_object(hf);
    if (obj)
	obj->text = new_string(face);
    if (d && ((d->kind == PrinterObject) ||
	      (d->kind == MetafileObject))) {
	TEXTMETRIC tm;
	HFONT old = SelectObject((HDC)d->handle, hf);
	GetTextMetrics((HDC)d->handle, &tm);
	obj->rect.width = tm.tmAveCharWidth;
	obj->rect.height = tm.tmHeight;
	obj->rect.x = tm.tmAscent - tm.tmInternalLeading;
	obj->rect.y = tm.tmDescent;
	SelectObject((HDC)d->handle, old);
    }

    return (font) obj;
}


static int measuredev(drawing dev, int what)
{
    HDC hDC;
    int n;
    if (dev)
	hDC = GETHDC(dev);
    else
	hDC = GetDC(NULL);
    n = GetDeviceCaps(hDC, what);
    if (!dev) ReleaseDC(NULL, hDC);
    return n;
}

#define MEASUREDEV(a) {return measuredev(dev,a);}

int devicewidth(drawing dev) MEASUREDEV(HORZRES)
int deviceheight(drawing dev) MEASUREDEV(VERTRES)
int devicewidthmm(drawing dev) MEASUREDEV(HORZSIZE)
int deviceheightmm(drawing dev) MEASUREDEV(VERTSIZE)
int devicepixelsx(drawing dev) MEASUREDEV(LOGPIXELSX)
int devicepixelsy(drawing dev) MEASUREDEV(LOGPIXELSY)

int isTopmost(window c)
{
    return GetWindowLong(c->handle, GWL_EXSTYLE) & WS_EX_TOPMOST;
}

static void setMessageBoxTopmost(window obj)
{
    if ((obj->kind == WindowObject) && (isTopmost(obj)))
	TopmostDialogs |= MB_TOPMOST;
}

int getHandle(window c)
{
    return (int)c->handle;
}

void BringToTop(window c, int stay) /* stay=0 for regular, 1 for topmost, 2 for toggle */
{
    SetForegroundWindow(c->handle); /* needed in Rterm */
    if (ismdi()) BringWindowToTop(hwndFrame);
    BringWindowToTop(c->handle);

    if (stay == 2) stay = !isTopmost(c);

    if (stay) SetWindowPos(c->handle, HWND_TOPMOST, 0, 0, 0, 0,
			   SWP_NOMOVE | SWP_NOSIZE);
    else SetWindowPos(c->handle, HWND_NOTOPMOST, 0, 0, 0, 0,
		      SWP_NOMOVE | SWP_NOSIZE);
    TopmostDialogs &= !MB_TOPMOST;
    apply_to_list(c->parent->child, setMessageBoxTopmost);
}

typedef struct {
    int codepage;
    int charset;
} cp2charset_table;

static cp2charset_table cp2charset [] = {
    {874, THAI_CHARSET},
    {932, SHIFTJIS_CHARSET},
    {936, GB2312_CHARSET},
    {949, HANGUL_CHARSET},
    {950, CHINESEBIG5_CHARSET},
    {1250,EASTEUROPE_CHARSET},
    {1251,RUSSIAN_CHARSET},
    {1252,ANSI_CHARSET},
    {1253,GREEK_CHARSET},
    {1254,TURKISH_CHARSET},
    {1255,HEBREW_CHARSET},
    {1256,ARABIC_CHARSET},
    {1257,BALTIC_CHARSET},
    {1258,VIETNAMESE_CHARSET},
    {1361,JOHAB_CHARSET},
};

/* Used to set the charset for the font in the console/pager/editor.
   As from 2.3.0 these use wchar on NT, but the charset still 
   seems to affect the font chosen. */
static int getcharset(void)
{
    int i, cp = localeCP;
    if (is_NT) 
	return (DEFAULT_CHARSET);
    else
	/* If SHIFTJIS_CHARSET is not specified in the case of Windows9x,
	   even if it will output this to emf, a
	   character does not come out exactly. */
	for (i = 0; i < sizeof(cp2charset)/sizeof(cp2charset_table); i++)
	    if (cp == cp2charset[i].codepage) return(cp2charset[i].charset);
    return(ANSI_CHARSET);
}
