/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

#define BUFSIZE 512

typedef struct {
	short FontBBox[4];
	short CapHeight;
	short XHeight;
	short Descender;
	short Ascender;
	struct {
		short WX;
		short BBox[4];
	} CharInfo[256];
} FontMetricInfo;

int    PostScriptLoadFontMetrics(char*, FontMetricInfo*);
double PostScriptStringWidth(unsigned char*, FontMetricInfo*);
void   PostScriptMetricInfo(int, double*, double*, double*, FontMetricInfo*);
void   PostScriptFilledCircle(FILE*, double, double, double);
void   PostScriptOpenCircle(FILE*, double, double, double);
void   PostScriptEndPage(FILE*);
void   PostScriptEndPath(FILE*);
void   PostScriptFileHeader(FILE*, char**, char*, double, double, int, double, double, double, double);
void   PostScriptFileTrailer(FILE*, int);
void   PostScriptLineTo(FILE*, double, double);
void   PostScriptMoveTo(FILE*, double, double);
void   PostScriptOrientPage(FILE*, int, double, double);
void   PostScriptFilledPolygon(FILE*, double*, double*, int);
void   PostScriptOpenPolygon(FILE*, double*, double*, int);
void   PostScriptFilledRectangle(FILE*, double, double, double, double);
void   PostScriptOpenRectangle(FILE*, double, double, double, double);
void   PostScriptSetClipRect(FILE*, double, double, double, double);
void   PostScriptSetColor(FILE*, double, double, double);
void   PostScriptSetFont(FILE*, int, double);
void   PostScriptSetLineTexture(FILE*, int*, int, double);
void   PostScriptSetLineWidth(FILE*, double);
void   PostScriptStartPage(FILE*, int);
void   PostScriptStartPath(FILE*);
void   PostScriptText(FILE*, double, double, char*, double, double, double);
