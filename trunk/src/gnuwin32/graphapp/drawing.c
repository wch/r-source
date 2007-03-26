/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: drawing.c -- all the drawing functions are here.
 * Platform: Windows  Version: 2.40  Date: 1998/05/05
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 * Version: 1.60  Changes: drawarc/fillarc(r,0,360) now encloses.
 *    New fillellipse() function replaces Windows Ellipse().
 * Version: 2.00  Changes: New class system implemented.
 * Version: 2.02  Changes: Added support for functions like MoveToEx.
 * Version: 2.15  Changes: Fixed brush origins problem.
 * Version: 2.20  Changes: Moved some arrays from context.c to here.
 * Version: 2.40  Changes: Moved drawimage to this file.
 */

/* Copyright (C) 1993-1998 Lachlan Patrick

   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.
*/

/*  Copyright (C) 2004 	The R Foundation

    Changes for R:

    Remove assumption of current->dest being non-NULL

 */

#include "internal.h"

#if (WINVER < 0x030a)

	#define MoveToEx move_to_ex
	#define GetCurrentPositionEx get_current_position_ex
	#define GetTextExtentPoint get_text_extent_point

	int move_to_ex(HDC hdc, int x, int y, POINT *p);
	int get_current_position_ex(HDC hdc, POINT *p);
	int get_text_extent_point(HDC hdc, char *str, int len, SIZE *s);

static int move_to_ex(HDC hdc, int x, int y, POINT *p)
{
	DWORD result = MoveTo(hdc, x, y);
	if (p) {
		p->x = LOWORD(result);
		p->y = HIWORD(result);
	}
	return 1;
}

static int get_current_position_ex(HDC hdc, POINT *p)
{
	DWORD result = GetCurrentPosition(hdc);
	if (p) {
		p->x = LOWORD(result);
		p->y = HIWORD(result);
	}
	return 1;
}

static int get_text_extent_point(HDC hdc, char *str, int len, SIZE *s)
{
	DWORD result = GetTextExtent(hdc, str, len);
	if (s) {
		s->cx = LOWORD(result);
		s->cy = HIWORD(result);
	}
	return 1;
}

#endif  /* WINVER < 0x030a */

/*
 *  Windows transfer modes corresponding to bitblt operations.
 */
static long copy_mode[16] = {
	BLACKNESS,	/* Zeros */
	NOTSRCERASE,	/* DnorS */
	0x00220326L,	/* DandnotS */
	NOTSRCCOPY,	/* notS */
	SRCERASE,	/* notDandS */
	DSTINVERT,	/* notD */
	SRCINVERT,	/* DxorS */
	0x007700E6L,	/* DnandS */
	SRCAND,		/* DandS */
	0x00990066L,	/* DxnorS */
	0x00AA0029L,	/* D */			/* = no-op */
	MERGEPAINT,	/* DornotS */
	SRCCOPY,	/* S */
	0x00DD0228L,	/* notDorS */
	SRCPAINT,	/* DorS */
	WHITENESS	/* Ones */
};

/*
 *  Windows transfer modes corresponding to patblt operations.
 */
static long pat_mode[16] = {
	BLACKNESS,	/* Zeros */
	0x000500A9L,	/* DnorP */
	0x000A0329L,	/* DandnotP */
	0x000F0001L,	/* notP */
	0x00500325L,	/* notDandP */
	DSTINVERT,	/* notD */
	PATINVERT,	/* DxorP */
	0x005F00E9L,	/* DnandP */
	0x00A000C9L,	/* DandP */
	0x00A50065L,	/* DxnorP */
	0x00AA0029L,	/* D */			/* = no-op */
	0x00AF0229L,	/* DornotP */
	PATCOPY,	/* P */
	0x00F50225L,	/* notDorP */
	0x00FA0089L,	/* DorP */
	WHITENESS	/* Ones */
};

/*
 *  Windows transfer modes corresponding to pen drawing.
 */
static int pen_mode[16] = {
	R2_BLACK,	/* Zeros */
	R2_NOTMERGEPEN,	/* DnorS */
	R2_MASKNOTPEN,	/* DandnotS */
	R2_NOTCOPYPEN,	/* notS */
	R2_MASKPENNOT,	/* notDandS */
	R2_NOT,		/* notD */
	R2_XORPEN,	/* DxorS */
	R2_NOTMASKPEN,	/* DnandS */
	R2_MASKPEN,	/* DandS */
	R2_NOTXORPEN,	/* DxnorS */
	R2_NOP,		/* D */			/* = no-op */
	R2_MERGENOTPEN,	/* DornotS */
	R2_COPYPEN,	/* S */
	R2_MERGEPENNOT,	/* notDorS */
	R2_MERGEPEN,	/* DorS */
	R2_WHITE	/* Ones */
};

/*
 *  Some clipping functions.
 */
rect getcliprect(void)
{
	RECT R;
	rect r;

	GetClipBox(dc, &R);
	r.x = R.left;
	r.y = R.top;
	r.width = R.right - R.left;
	r.height = R.bottom - R.top;
	return r;
}

void setcliprect(rect r)
{
	HRGN rgn;

	rgn = CreateRectRgn(r.x, r.y, r.x+r.width, r.y+r.height);
	SelectClipRgn(dc, rgn);
	DeleteObject(rgn);
}

/*
 *  Ensure that drawing is possible by creating DCs and windows
 *  as necessary.
 */
PROTECTED
window simple_window(void)
{
	window w;
	w = newwindow("Graphics", rect(0,0,0,0), StandardWindow);
	show(w);
	return w;
}

/*
 *  Fix brush origins.
 */
PROTECTED
void fix_brush(HDC dc, drawing obj, HBRUSH brush)
{
	POINT p;
	HWND hwnd;
	drawing parent;

	parent = parentwindow(obj);
	if (! parent)
		return;
	hwnd = parent->handle;
	p.x = p.y = 0;
	ClientToScreen(hwnd, &p);
	if (brush)
		UnrealizeObject(brush);
#if (WINVER <= 0x030a)
	/* Microsoft keeps changing which functions they include in GDI.DLL */
	/* But this function should work on systems before Win 95 */
	SetBrushOrg(dc, p.x, p.y);
#else
	/* And this function should work on Win 95 and NT */
	SetBrushOrgEx(dc, p.x, p.y, &p);
#endif
}

/*
 *  All drawing functions call enabledrawing.
 */
static void enable_drawing(void)
{
	if (! current->dest) {
		if (! current_window)
			current_window = simple_window();
		show(current_window);
		drawto(current_window);
	}
	if (! dc)
		dc = get_context(current->dest);

	fix_brush(dc, current->dest, the_brush);
}

/*
 *  Drawing functions begin here.
 */
void bitblt(bitmap db, bitmap sb, point p, rect r, int mode)
{
	HDC src;
	HDC dst;

	dst = get_context((object)db);
	src = get_context((object)sb);

	BitBlt(dst, p.x, p.y, r.width, r.height, src, r.x, r.y,
		copy_mode[mode&0x0F]);
}

void scrollrect(point dp, rect r)
{
	rect cliprect;
	if (current->dest) cliprect = getrect(current->dest);
	else return;

	enable_drawing();
	ScrollDC(dc, dp.x-r.x, dp.y-r.y,
		(RECT *)&r, (RECT *) &cliprect, 0, NULL);
}

void copyrect(bitmap sb, point p, rect r)
{
	enable_drawing();
	if (current->dest) bitblt(current->dest, sb, p, r, S);
}

void texturerect(bitmap sb, rect dr)
{
	long x, y, sw, sh, sdx, sdy;
	long right, bottom;
	rect sr,r;

	enable_drawing();
	sr = getrect(sb);
	sw = sr.width;
	sh = sr.height;
	right = dr.x + dr.width;
	bottom = dr.y + dr.height;

	for (y = dr.y; y <= bottom; y += sh)
		for (x = dr.x; x <= right; x += sw) {

			/* reduce size of source rectangle for clipping */
			if (x+sw > right)	sdx = right - x;
			else			sdx = sw;
			if (y+sh > bottom)	sdy = bottom - y;
			else			sdy = sh;

			r = rect(sr.x, sr.y, sdx, sdy);
			copyrect(sb, pt(x,y), r);
		}
}

void invertrect(rect r)
{
	enable_drawing();
	PatBlt(dc, r.x, r.y, r.width, r.height, DSTINVERT);
}

rgb getpixel(point p)
{
	rgb c;

	enable_drawing();
	c = GetPixel(dc, p.x, p.y);
	c = ((c&0x000000FFL)<<16) | (c&0x0000FF00L) |
		((c&0x00FF0000L)>>16);
	return c;
}

void setpixel(point p, rgb c)
{
	rgb old = current->hue;

	enable_drawing();
	setrgb(c);
	SelectObject(dc, the_brush);
	PatBlt(dc, p.x, p.y, 1, 1, pat_mode[current->mode]);
	SelectObject(dc, GetStockObject(NULL_BRUSH));
	setrgb(old);
}

void moveto(point p)
{
	current->p = p;
}

void lineto(point p)
{
	drawline(current->p, p);
	moveto(p);
}

void drawpoint(point p)
{
	setpixel(p, current->hue);
}

void drawline(point p1, point p2)
{
	if ((p1.x == p2.x) && (p1.y == p2.y))
		return; /* same point so draw nothing */

	enable_drawing();
	SelectObject(dc, the_pen);
	SetROP2(dc, pen_mode[current->mode]);
	MoveToEx(dc, p1.x, p1.y, NULL);
	LineTo(dc, p2.x, p2.y);
	SelectObject(dc, GetStockObject(NULL_PEN));
}

void drawrect(rect r)
{
	enable_drawing();
	SelectObject(dc, the_pen);
	SetROP2(dc, pen_mode[current->mode]);
	Rectangle(dc, r.x, r.y, r.x+r.width, r.y+r.height);
	SelectObject(dc, GetStockObject(NULL_PEN));
}

void fillrect(rect r)
{
	enable_drawing();
	SelectObject(dc, the_brush);
	PatBlt(dc, r.x, r.y, r.width, r.height, pat_mode[current->mode]);
	SelectObject(dc, GetStockObject(NULL_BRUSH));
}

#define deg2rad(deg) ((deg)*2*Pi/360)

void drawarc(rect r, int start_angle, int end_angle)
{
	point p0, p1, p2;
	double start, end;
	int fudge;

	enable_drawing();

	if (start_angle == end_angle)
		return;
	if (((end_angle - start_angle) % 360) == 0) {
		drawarc(r, 0, 180);
		drawarc(r, 180, 360);
		return;
	}

	start = deg2rad(start_angle);
	end = deg2rad(end_angle);
	p0 = midpt(topleft(r), bottomright(r));
	p1.x = p0.x + (int)(cos(start) * 512); /* arbitrary hypotenuse */
	p1.y = p0.y - (int)(sin(start) * 512);
	p2.x = p0.x + (int)(cos(end) * 512);
	p2.y = p0.y - (int)(sin(end) * 512);

	SelectObject(dc, the_pen);
	SetROP2(dc, pen_mode[current->mode]);
	if (current->linewidth % 2) /* Ask Bill Gates why we need this */
		fudge = 0;
	else
		fudge = 1;
	Arc(dc, r.x, r.y, r.x+r.width+fudge, r.y+r.height+fudge,
		p1.x, p1.y, p2.x, p2.y);
	SelectObject(dc, GetStockObject(NULL_PEN));
}

void fillarc(rect r, int start_angle, int end_angle)
{
	point p0, p1, p2;
	double start, end;

	enable_drawing();

	if (start_angle == end_angle)
		return;
	if (((end_angle - start_angle) % 360) == 0) {
		fillellipse(r);
		return;
	}

	start = deg2rad(start_angle);
	end = deg2rad(end_angle);
	p0 = midpt(topleft(r), bottomright(r));
	p1.x = p0.x + (int)(cos(start) * 512); /* arbitrary hypotenuse */
	p1.y = p0.y - (int)(sin(start) * 512);
	p2.x = p0.x + (int)(cos(end) * 512);
	p2.y = p0.y - (int)(sin(end) * 512);

	SelectObject(dc, the_brush);
	SetROP2(dc, pen_mode[current->mode]);
	Pie(dc, r.x, r.y, r.x+r.width+1, r.y+r.height+1,
		p1.x, p1.y, p2.x, p2.y);
	SelectObject(dc, GetStockObject(NULL_BRUSH));
}

void drawellipse(rect r)
{
	enable_drawing();
	SelectObject(dc, the_pen);
	SetROP2(dc, pen_mode[current->mode]);
	Ellipse(dc, r.x, r.y, r.x+r.width, r.y+r.height);
	SelectObject(dc, GetStockObject(NULL_PEN));
}

/*
 *  The old fillellipse function.
 *
 *  This function used the inbuilt Windows Ellipse function,
 *  which is not symmetric and also produces strange results
 *  at low sizes (the 'ellipses' look egg-shaped).
 */
void oldfillellipse(rect r)
{
	enable_drawing();
	SelectObject(dc, the_brush);
	SetROP2(dc, pen_mode[current->mode]);
	Ellipse(dc, r.x, r.y, r.x+r.width+1, r.y+r.height+1);
	SelectObject(dc, GetStockObject(NULL_BRUSH));
}

/*
 *  Fill an ellipse using a stored rectangle algorithm.
 *
 *  This algorithm is horizontally and vertically symmetrical,
 *  unlike the inbuilt Windows 3.1 algorithm, and also
 *  produces better looking ellipses at small sizes.
 */
#define fastfillrect(x,y,w,h) PatBlt(dc,(x),(y),(w),(h),mode)

void fillellipse(rect r)
{			/* e(x,y) = b*b*x*x + a*a*y*y - a*a*b*b */
	register long mode = pat_mode[current->mode];

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

	if ((r.width > 31) && (r.height > 31)) {
		oldfillellipse(r);
		return;
	}
	if ((r.width < 3) || (r.height < 3)) {
		fillrect(r);
		return;
	}

	enable_drawing();
	SelectObject(dc, the_brush);

	if (w_odd == 0) {
		fastfillrect(c.x-1,c.y-b,2,r.height);
	}

	while (y > 0) {

		if (stored) {
			if (sx != x) { /* output stored rect */
				fastfillrect(c.x-sx,c.y-sy,
					sx+sx+w_odd,sh);
				fastfillrect(c.x-sx,c.y+sy+h_odd-sh,
					sx+sx+w_odd,sh);
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
		fastfillrect(c.x-sx,c.y-sy,sx+sx+w_odd,sh);
		fastfillrect(c.x-sx,c.y+sy+h_odd-sh,sx+sx+w_odd,sh);
		stored = 0;
	}
	if (x <= a){
		fastfillrect(c.x-a,c.y-y,a+a+w_odd,1);
		fastfillrect(c.x-a,c.y+y-1+h_odd,a+a+w_odd,1);
	}

	SelectObject(dc, GetStockObject(NULL_BRUSH));
}

void drawroundrect(rect r)
{
	int minimum, radius;

	enable_drawing();
	SelectObject(dc, the_pen);
	SetROP2(dc, pen_mode[current->mode]);
	minimum = min(r.width, r.height);
	if ((radius = minimum/2) < 16)
		radius = 16;
	RoundRect(dc, r.x, r.y, r.x+r.width, r.y+r.height,
		radius, radius);
	SelectObject(dc, GetStockObject(NULL_PEN));
}

void fillroundrect(rect r)
{
	int minimum, radius;

	enable_drawing();
	SelectObject(dc, the_brush);
	SetROP2(dc, pen_mode[current->mode]);
	minimum = min(r.width, r.height);
	if ((radius = minimum/2) < 16)
		radius = 16;
	RoundRect(dc, r.x, r.y, r.x+r.width+1, r.y+r.height+1,
		radius, radius);
	SelectObject(dc, GetStockObject(NULL_BRUSH));
}

void drawpolygon(point *p, int n)
{
	enable_drawing();
	SelectObject(dc, the_pen);
	SetROP2(dc, pen_mode[current->mode]);
	Polyline(dc, (POINT FAR *) p, n);
	SelectObject(dc, GetStockObject(NULL_PEN));
}

void fillpolygon(point *p, int n)
{
	enable_drawing();
	SelectObject(dc, the_brush);
	SetROP2(dc, pen_mode[current->mode]);
	Polygon(dc, (POINT FAR *) p, n);
	SelectObject(dc, GetStockObject(NULL_BRUSH));
}

/*
 *  String drawing functions.
 */

/*
 *  Drawstr returns the width of the string drawn.
 */
int drawstr(point p, char *s)
{
	POINT curr_pos;
	int width;
	HFONT old;

	enable_drawing();
	SetTextColor(dc, win_rgb); /* set colour */
	if (! current->fnt)
		current->fnt = SystemFont;
	old = SelectObject(dc, current->fnt->handle);
	MoveToEx(dc, p.x, p.y, NULL);
	SetBkMode(dc, TRANSPARENT);
	SetTextAlign(dc, TA_LEFT | TA_UPDATECP);

	TextOut(dc, p.x, p.y, s, strlen(s));

	GetCurrentPositionEx(dc, &curr_pos);
	width = curr_pos.x - p.x;
	SelectObject(dc, old);
	/* always leave a DC with no real font selected */

	return width;
}

rect strrect(font f, char *s)
{
	SIZE size;
	long h;
	HFONT old;
	HDC dc;

	if (! f)
		f = SystemFont;

	h = getheight(f);

	dc = GetDC(0); /* get screen dc */
	old = SelectObject(dc, f->handle);
	GetTextExtentPoint(dc, (LPSTR) s, strlen(s), &size);
	SelectObject(dc, old);
	ReleaseDC(0, dc);

	return rect(0,0,size.cx, h);
}

point strsize(font f, char *s)
{
	rect r = strrect(f,s);
	return pt(r.width, r.height);
}

int strwidth(font f, char *s)
{
	rect r = strrect(f,s);
	return r.width;
}

/*
 *  Draw an image:
 */
void drawimage(image img, rect dr, rect sr)
{
	bitmap b;
	image i = img;

	if (! img)
		return;
	enable_drawing();
	dr = rcanon(dr);
	if ((dr.width != img->width) || (dr.height != img->height))
		i = scaleimage(img, rect(0,0,dr.width,dr.height), sr);
	b = imagetobitmap(i);
	copyrect(b, pt(dr.x,dr.y), getrect(b));
	del(b);
	if (i != img)
		del(i);
}
