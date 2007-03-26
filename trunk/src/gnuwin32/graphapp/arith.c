/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: arith.c -- point and rectangle arithmetic.
 * Platform: Neutral  Version: 2.40  Date: 1998/05/05
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 * Version: 1.04  Changes: Put the rcanon function back in.
 * Version: 1.50  Changes: Change to rect structure & functions.
 * Version: 2.40  Changes: Fix for rinr and clipr by Angus North.
 */

/* Copyright (C) 1993-1998 Lachlan Patrick

   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.
*/

#include "graphapp.h"

point newpoint(int x, int y)
{
	point p;

	p.x = x;
	p.y = y;
	return p;
}

rect newrect(int x, int y, int width, int height)
{
	rect r;

	r.x = x;
	r.y = y;
	r.width = width;
	r.height = height;
	return r;
}

rect rpt(point min, point max)
{
	rect r;

	r.x = min.x;
	r.y  = min.y;
	r.width = max.x - min.x;
	r.height = max.y - min.y;
	return r;
}

point topleft(rect r)
{
	point p;

	p.x = r.x;
	p.y = r.y;

	return p;
}

point topright(rect r)
{
	point p;

	p.x = r.x + r.width - 1;
	p.y = r.y;

	return p;
}

point bottomright(rect r)
{
	point p;

	p.x = r.x + r.width - 1;
	p.y = r.y + r.height - 1;

	return p;
}

point bottomleft(rect r)
{
	point p;

	p.x = r.x;
	p.y = r.y + r.height - 1;

	return p;
}

point addpt(point p1, point p2)
{
	p1.x += p2.x;
	p1.y += p2.y;
	return p1;
}

point subpt(point p1, point p2)
{
	p1.x -= p2.x;
	p1.y -= p2.y;
	return p1;
}

point midpt(point p1, point p2)
{
	point p;

	p.x = (p1.x + p2.x)/2;
	p.y = (p1.y + p2.y)/2;
	return p;
}

point mulpt(point p, int i)
{
	p.x *= i;
	p.y *= i;
	return p;
}

point divpt(point p, int i)
{
	p.x /= i;
	p.y /= i;
	return p;
}

rect rmove(rect r, point p)
{
	r.x = p.x;
	r.y  = p.y;
	return r;
}

rect raddpt(rect r, point p)
{
	r.x += p.x;
	r.y += p.y;
	return r;
}

rect rsubpt(rect r, point p)
{
	r.x -= p.x;
	r.y  -= p.y;
	return r;
}

rect rmul(rect r, int i)
{
	if (i != 1) {
		r.x *= i;
		r.y  *= i;
		r.width *= i;
		r.height *= i;
	}
	return r;
}

rect rdiv(rect r, int i)
{
	if (i != 1) {
		r.x /= i;
		r.y  /= i;
		r.width /= i;
		r.height /= i;
	}
	return r;
}

rect growr(rect r, int w, int h)
{
	r.x -= w;
	r.y  -= h;
	r.width += 2*w;
	r.height += 2*h;
	return r;
}

rect insetr(rect r, int i)
{
	r.x += i;
	r.y  += i;
	r.width -= 2*i;
	r.height -= 2*i;
	return r;
}

rect rcenter(rect r1, rect r2) /* center r1 on r2 */
{
	rect r;

	r.x = r2.x + (r2.width-r1.width)/2;
	r.y  = r2.y + (r2.height-r1.height)/2;
	r.width = r1.width;
	r.height = r1.height;

	return r;
}

int ptinr(point p, rect r)
{
	if ((p.x>=r.x) && (p.x<r.x+r.width) &&
			(p.y>=r.y) && (p.y<r.y+r.height))
		return 1;
	else
		return 0;
}

int rinr(rect r1, rect r2)
{
	if ((r1.x>=r2.x) && (r1.y>=r2.y) &&
		(r1.x+r1.width<=r2.x+r2.width) &&
		(r1.y+r1.height<=r2.y+r2.height))
		return 1;
	else
		return 0;
}

int rxr(rect r1, rect r2)
{
	if ((r1.x<r2.x+r2.width) &&
		(r2.x<r1.x+r1.width) &&
		(r1.y<r2.y+r2.height) &&
		(r2.y<r1.y+r1.height))
		return 1;
	else
		return 0;
}

int equalpt(point p1, point p2)
{
	if ((p1.x==p2.x) && (p1.y==p2.y))
		return 1;
	else
		return 0;
}

int equalr(rect r1, rect r2)
{
	if ((r1.x==r2.x) && (r1.width==r2.width) &&
		(r1.y==r2.y) && (r1.height==r2.height))
		return 1;
	else
		return 0;
}

rect clipr(rect r1, rect r2)
{
	if (rxr(r1,r2) == 0)
		return rect(0,0,0,0); /* they don't overlap */

	if (r1.x < r2.x) {
		r1.width -= (r2.x - r1.x);
		r1.x = r2.x;
	}
	if (r1.y < r2.y) {
		r1.height -= (r2.y - r1.y);
		r1.y = r2.y;
	}
	if (r1.x + r1.width > r2.x + r2.width)
		r1.width = r2.x + r2.width - r1.x;
	if (r1.y + r1.height > r2.y + r2.height)
		r1.height = r2.y + r2.height - r1.y;
	return r1; /* they do overlap */
}

rect rcanon(rect r)
{
	if (r.width < 0) {
		r.x += r.width;
		r.width = -r.width;
	}
	if (r.height < 0) {
		r.y += r.height;
		r.height = -r.height;
	}
	return r;
}
