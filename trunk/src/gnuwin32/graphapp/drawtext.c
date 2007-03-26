/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: drawtext.c -- cross-platform portable drawing functions.
 * Platform: Neutral  Version: 2.30  Date: 1998/01/01
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 * Version: 1.50  Changes: gprintf now has internal state.
 * Version: 1.60  Changes: Major bugfixes!
 * Version: 2.00  Changes: Update to version 2.
 * Version: 2.01  Changes: Changed unnecessary longs to ints.
 * Version: 2.20  Changes: Added underlining ability.
 * Version: 2.30  Changes: Now uses getheight, getdescent.
 */

/* Copyright (C) 1993-1998 Lachlan Patrick

   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.
*/

#include "internal.h"
#include <stdarg.h>

#define COMPRESS_WHITESPACE 0

static char *get_next_line(char *line, int width, char *s)
{
	char *t, *k;
	int	word_width, line_width;
	int	sp, tab, nl;

	line_width  = 0;

	for (t=s, k=line; *t!='\0'; line=k, s=t)
	{
		for (nl=tab=sp=0; (*t!='\0') && isspace(*t) && !nl; t++)
		{
#if COMPRESS_WHITESPACE
			if (*t == '\n') nl++;
			else if (*t == '\t') tab++;
			else sp++;
#else
			if (*t == '\n') nl++;
			else if (*t == '\t') { /* expand tabs */
				for (tab=4; tab; tab--)
					*k++ = ' ';
			}
			else
				*k++ = *t;
#endif
		}

		s = t;

		if (nl)      	{ *k++ = '\n'; break; }
		else if (tab)	{ *k++ = ' '; *k++ = ' '; }
		else if (sp) 	{ *k++ = ' ';  }

		/* fetch the next word to draw */

		for (*k='\0'; (*t!='\0') && !isspace(*t); *k='\0') {
			if (*t == '-') {
				*k++ = *t++;
				*k = '\0';
				break;
			}
			*k++ = *t++;
		}

		/* determine where the word should be drawn */

		while (((word_width = strwidth(current->fnt,line)) > width)
				&& (k>line))
		{
			*(--k) = '\0';
			--t;
		}

		if (word_width > (width - line_width)) {
			k = line;
			break;
		} else
			line_width += word_width;
	}

	*k = '\0';

	return (*s == '\0') ? NULL : s;
}

int textheight(int width, char *s)
{
	int y;
	char line[256];
	int height;

	height = getheight(current->fnt);

	for(y=0; s; y+=height)
	{
		s = get_next_line(line, width, s);
	}

	return y;
}

static char *draw_text_left(char *line, rect r, int line_height,
			int underline, char *s)
{
	char *k;
	point p;
	int width, height;
	font f;

	f = current->fnt;

	for(p=pt(r.x,r.y); (p.y<=r.y+r.height) && (s); p.y+=line_height)
	{
		s = get_next_line(line, r.width, s);

		for (k=line; *k!='\0'; k++)
			continue;
		for (--k; (k>=line) && isspace(*k); k--)
			*k = '\0';

		drawstr(p, line);

		if (underline) {
			width = strwidth(f, line);
			height = p.y+getheight(f)-getdescent(f)+2;
			drawline(pt(p.x+1, height), pt(p.x+width-1, height));
		}
	}

	return s;
}

static char *draw_text_right(char *line, rect r, int line_height,
			int underline, char *s)
{
	char *k;
	int w;
	point p;
	int width, height;
	font f;

	f = current->fnt;

	for(p=pt(r.x,r.y); (p.y<=r.y+r.height) && (s); p.y+=line_height)
	{
		s = get_next_line(line, r.width, s);

		for (k=line; *k!='\0'; k++)
			continue;
		for (--k; (k>=line) && isspace(*k); k--)
			*k = '\0';
		for (k=line; (*k!='\0') && isspace(*k); k++)
			continue;

		w = strwidth(current->fnt, k);
		p.x = r.x+r.width - w;

		drawstr(p, k);

		if (underline) {
			width = strwidth(f, k);
			height = p.y+getheight(f)-getdescent(f)+2;
			drawline(pt(p.x+1, height), pt(p.x+width-1, height));
		}
	}

	return s;
}

static char *draw_text_centered(char *line, rect r, int line_height,
			int underline, char *s)
{
	char *k;
	int w;
	point p;
	int width, height;
	font f;

	f = current->fnt;

	for(p=pt(r.x,r.y); (p.y<=r.y+r.height) && (s); p.y+=line_height)
	{
		s = get_next_line(line, r.width, s);

		for (k=line; *k!='\0'; k++)
			continue;
		for (--k; (k>=line) && isspace(*k); k--)
			*k = '\0';
		for(k=line; (*k!='\0') && isspace(*k); k++)
			continue;

		w = strwidth(current->fnt, k);
		p.x = r.x + (r.width-w)/2;

		drawstr(p, k);

		if (underline) {
			width = strwidth(f, k);
			height = p.y+getheight(f)-getdescent(f)+2;
			drawline(pt(p.x+1, height), pt(p.x+width-1, height));
		}
	}

	return s;
}

static char *draw_text_justified(char *line, rect r, int line_height,
			int underline, char *s)
{
	char *j, *k;
	int w, xw, nl, sc, sw, space_width;
	point p;
	int width, height;
	font f;

	space_width = strwidth(current->fnt, " ");
	f = current->fnt;

	for(p=pt(r.x,r.y); (p.y<=r.y+r.height) && (s); p.y+=line_height)
	{
		s = get_next_line(line, r.width, s);

		p.x = r.x;

		for(j=line; (*j!='\0') && isspace(*j); j++)
			p.x += space_width;
		for (sc=0, k=j; *k!='\0'; k++)
			if (isspace(*k))
				sc++;
		for (nl=0, --k; (k>=j) && isspace(*k); k--) {
			if (*k == '\n')
				nl++;
			*k = '\0';
			sc--;
		}

		if ((sc==0) || nl || (! s)) {
			drawstr(p, j);
			width = strwidth(f, j);
		}
		else {
			w = strwidth(f, j);
			sw = space_width + (r.x+r.width-p.x-w)/sc;
			xw = (r.x+r.width-p.x-w)%sc;

			for(j=strtok(j," "); j; j=strtok(NULL," "))
			{
				drawstr(p, j);
				p.x += sw + strwidth(f, j);
				if (xw) {
					p.x++;
					xw--;
				}
			}
			width = r.width;
		}
		if (underline) {
			height = p.y+getheight(f)-getdescent(f)+2;
			drawline(pt(p.x+1, height), pt(p.x+width-1, height));
		}
	}

	return s;
}

char *drawtext(rect r, int alignment, char *s)
{
	int h;
	int nlines;
	int line_height;
	int u = 0;
	rect clip;
	char *remains;
	char line[256];

	initapp(0,0);
	if (! s)
		return s;
	if (! current->fnt)
		current->fnt = SystemFont;

	clip = getcliprect();
	setcliprect(r);

	line_height = getheight(current->fnt);

	if ((alignment & VCenter) == VCenter) {
		h = textheight(r.width, s);
		if (h < r.height)
			r.y += (r.height-h)/2;
	}
	else if ((alignment & VJustify) == VJustify) {
		h = textheight(r.width, s);
		if (h < r.height) {
			nlines = h / line_height;
			if (nlines > 1)
				line_height += ((r.height-h) / (nlines-1));
		}
	}
	else if ((alignment & AlignBottom) == AlignBottom) {
		h = textheight(r.width, s);
		if (h < r.height)
			r.y += (r.height-h);
	}

	u = (alignment & Underline);

	if ((alignment & Center) == Center)
		remains = draw_text_centered(line, r, line_height, u, s);
	else if ((alignment & Justify) == Justify)
		remains = draw_text_justified(line, r, line_height, u, s);
	else if ((alignment & AlignRight) == AlignRight)
		remains = draw_text_right(line, r, line_height, u, s);
	else
		remains = draw_text_left(line, r, line_height, u, s);

	setcliprect(clip);
	return remains;
}

int gprintf(char *fmt, ...)
{
	static point p = {0,0};
	int count;
	int line_height;
	char *s, *t;
	va_list argptr;
	char str[256];

	va_start(argptr, fmt);
	count = vsprintf(str, fmt, argptr);

	initapp(0,0);
	if (! current->fnt)
		current->fnt = SystemFont;
	line_height = getheight(current->fnt);

	for (s=t=str; *s!='\0'; t++) {
		if (current->p.y != p.y) {
			/* typewriter ping! */
			p = current->p;
		}
		if (*t == '\n') {
		/* print everything from s to t and move point down */
			*t = '\0';
			drawstr(p, s);
			current->p.y += line_height;
			/* go past the substring just printed */
			s = t+1;
		}
		else if (*t == '\0') {
			/* print final string without newline */
			p.x += drawstr(p, s);
			/* go to end of string, signal termination */
			s = t;
		}
	}

	va_end(argptr);

	return count;
}
