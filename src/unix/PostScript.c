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

#include <stdio.h>
#include <ctype.h>
#include "PS.h"
#include "Fileio.h"

	/*  PostScript Graphics Utilities			  */
	/*  This file contains all those utilities used to draw   */
	/*  PostScript graphics under R.  This includes the       */
	/*  processing of font metric information as well as the  */
	/*  actual output of postscript drawing instructions.     */


		/*  Part 1.  AFM File Parsing.  */

	/* These are the basic entities in the AFM file */

enum {
	Empty,
	StartFontMetrics,
	Comment,
	FontName,
	EncodingScheme,
	FullName,
	FamilyName,
	Weight,
	ItalicAngle,
	IsFixedPitch,
	UnderlinePosition,
	UnderlineThickness,
	Version,
	Notice,
	FontBBox,
	CapHeight,
	XHeight,
	Descender,
	Ascender,
	StartCharMetrics,
	C,
	EndCharMetrics,
	StartKernData,
	StartKernPairs,
	KPX,
	EndKernPairs,
	EndKernData,
	StartComposites,
	CC,
	EndComposites,
	EndFontMetrics,
	Unknown
};

static struct { char *keyword; int code; }
KeyWordDictionary[] = {
	{ "StartFontMetrics",	StartFontMetrics },
	{ "Comment",		Comment },
	{ "FontName",		FontName },
	{ "EncodingScheme",	EncodingScheme },
	{ "FullName",		FullName },
	{ "FamilyName",		FamilyName },
	{ "Weight",		Weight },
	{ "ItalicAngle",	ItalicAngle },
	{ "IsFixedPitch",	IsFixedPitch },
	{ "UnderlinePosition",	UnderlinePosition },
	{ "UnderlineThickness",	UnderlineThickness },
	{ "Version",		Version },
	{ "Notice",		Notice },
	{ "FontBBox",		FontBBox },
	{ "CapHeight",		CapHeight },
	{ "XHeight",		XHeight },
	{ "Descender",		Descender },
	{ "Ascender",		Ascender },
	{ "StartCharMetrics",	StartCharMetrics },
	{ "C ",			C },
	{ "EndCharMetrics",	EndCharMetrics },
	{ "StartKernData",	StartKernData },
	{ "StartKernPairs",	StartKernPairs },
	{ "KPX ",		KPX },
	{ "EndKernPairs",	EndKernPairs },
	{ "EndKernData",	EndKernData },
	{ "StartComposites ",	StartComposites },
	{ "CC ",		CC },
	{ "EndComposites",	EndComposites },
	{ "EndFontMetrics",	EndFontMetrics },
	{ NULL,			Unknown },
};


static int MatchKey(char *l, char *k)
{
	while(*k) if(*k++ != *l++) return 0;
	return 1;
}

static int KeyType(char *s)
{
	int i;
	if(*s == '\n')
		return Empty;
	for(i=0 ; KeyWordDictionary[i].keyword ; i++)
		if(MatchKey(s, KeyWordDictionary[i].keyword))
			return KeyWordDictionary[i].code;
	return Unknown;
}

static char *SkipToNextItem(char *p)
{
	while(!isspace(*p)) p++;
	while(isspace(*p)) p++;
	return p;
}

static char *SkipToNextKey(char *p)
{
	while(*p != ';') p++;
	p++;
	while(isspace(*p)) p++;
	return p;
}

static int GetFontBBox(char *buf, FontMetricInfo *metrics)
{
	if(sscanf(buf, "FontBBox %hd %hd %hd %hd",
		&(metrics->FontBBox[0]),
		&(metrics->FontBBox[1]),
		&(metrics->FontBBox[2]),
		&(metrics->FontBBox[3])) != 4) return 0;
#ifdef DEBUG
	printf("FontBBox %d %d %d %d\n",
		(metrics->FontBBox[0]),
		(metrics->FontBBox[1]),
		(metrics->FontBBox[2]),
		(metrics->FontBBox[3]));
#endif
	return 1;
}

static int GetCharInfo(char *buf, FontMetricInfo *metrics)
{
	char *p = buf;
	int nchar;

	if(!MatchKey(buf, "C ")) return 0;
	p = SkipToNextItem(p);
	sscanf(p, "%d", &nchar);
	if(nchar < 0) return 1;
	p = SkipToNextKey(p);

	if(!MatchKey(p, "WX")) return 0;
	p = SkipToNextItem(p);
	sscanf(p, "%hd", &(metrics->CharInfo[nchar].WX));
	p = SkipToNextKey(p);

	if(!MatchKey(p, "N ")) return 0;
	p = SkipToNextKey(p);

	if(!MatchKey(p, "B ")) return 0;
	p = SkipToNextItem(p);
	sscanf(p, "%hd %hd %hd %hd",
		&(metrics->CharInfo[nchar].BBox[0]),
		&(metrics->CharInfo[nchar].BBox[1]),
		&(metrics->CharInfo[nchar].BBox[2]),
		&(metrics->CharInfo[nchar].BBox[3]));

#ifdef DEBUG
	printf("nchar = %d %d %d %d %d %d\n", nchar,
		metrics->CharInfo[nchar].WX,
		metrics->CharInfo[nchar].BBox[0],
		metrics->CharInfo[nchar].BBox[1],
		metrics->CharInfo[nchar].BBox[2],
		metrics->CharInfo[nchar].BBox[3]);
#endif
	return 1;
}


	/* Load Fontmetrics from a File */

int PostScriptLoadFontMetrics(char *fontname, FontMetricInfo *metrics)
{
	char buf[BUFSIZE];
	int mode;
	FILE *fp;

	if(!(fp = R_fopen(fontname, "r"))) return 0;

	mode = 0;
	while(fgets(buf, BUFSIZE, fp)) {
		switch(KeyType(buf)) {

			case StartFontMetrics:
				mode = StartFontMetrics;
				break;

			case EndFontMetrics:
				mode = 0;
				break;

			case FontBBox:
				if(!GetFontBBox(buf, metrics)) goto error;
				break;

			case C:
				if(mode != StartFontMetrics) goto error;
				if(!GetCharInfo(buf, metrics)) goto error;
				break;

			case Unknown:
				printf("Warning: unknown AFM entity encountered\n");
				break;

			case Empty:
			default:
				break;
		}
	}
	fclose(fp);
	return 1;
error:
	fclose(fp);
	return 0;
}

double PostScriptStringWidth(unsigned char *p, FontMetricInfo *metrics)
{
	int sum = 0;
	for( ; *p ; p++) {
		if(*p == '-' && isdigit(p[1]))
			sum += metrics->CharInfo[177].WX;	/* n-dash */
		else
			sum += metrics->CharInfo[*p].WX;
	}
	return 0.001 * sum;
}

void PostScriptMetricInfo(int c, double *ascent, double *descent,
	double *width, FontMetricInfo *metrics)
{
	if(c == 0) {
		*ascent = 0.001 * metrics->FontBBox[3];
		*descent = -0.001 * metrics->FontBBox[1];
		*width = 0.001 * (metrics->FontBBox[2] - metrics->FontBBox[0]);

	}
	else {
		*ascent = 0.001 * metrics->CharInfo[c].BBox[3];
		*descent = -0.001 * metrics->CharInfo[c].BBox[1];
		*width = 0.001 * metrics->CharInfo[c].WX;
	}
}


		/*  Part 2.  Graphics Support Code.  */


static char *TypeFaceDef[] = { "R", "B", "I", "BI", "S" };

	/*  The variables "paperwidth" and "paperheight" give  */
	/*  the dimensions of the (unrotated) printer page in  */
	/*  points whereas the graphics region box is for the  */
	/*  rotated page.                                      */

void PostScriptFileHeader(
	FILE *fp,
	char **font,
	char *papername,
	double paperwidth,
	double paperheight,
	int landscape,
	double left,
	double bottom,
	double right,
	double top)
{
	fprintf(fp, "%%!PS-Adobe-3.0\n");
	fprintf(fp, "%%%%DocumentFonts: %s %s %s\n", font[2], font[4], font[6]);
	fprintf(fp, "%%%%+ %s %s\n", font[8], font[10]);
	fprintf(fp, "%%%%DocumentMedia: %s %.0f %.0f 0 ()\n",
		papername, paperwidth, paperheight);
	fprintf(fp, "%%%%Title: R Graphics Output\n");
	fprintf(fp, "%%%%Creator: R Software\n");
	fprintf(fp, "%%%%Pages: (atend)\n");
	if(landscape) {
		fprintf(fp, "%%%%Orientation: Landscape\n");
		fprintf(fp, "%%%%BoundingBox: %.0f %.0f %.0f %.0f\n",
			bottom, left, top, right);
	}
	else {
		fprintf(fp, "%%%%Orientation: Portrait\n");
		fprintf(fp, "%%%%BoundingBox: %.0f %.0f %.0f %.0f\n",
			left, bottom, right, top);
	}
	fprintf(fp, "%%%%EndComments\n");
	fprintf(fp, "%%%%BeginProlog\n");
	fprintf(fp, "/gs  { gsave } def\n");
	fprintf(fp, "/gr  { grestore } def\n");
        if(landscape)
	fprintf(fp, "/bp  { gs %.2f 0 translate 90 rotate} def\n", paperwidth);
	else
	fprintf(fp, "/bp  { gs } def\n");
	fprintf(fp, "/ep  { showpage gr } def\n");
	fprintf(fp, "/m   { moveto } def\n");
	fprintf(fp, "/l   { lineto } def\n");
	fprintf(fp, "/np  { newpath } def\n");
	fprintf(fp, "/cp  { closepath } def\n");
	fprintf(fp, "/f   { fill } def\n");
	fprintf(fp, "/o   { stroke } def\n");
	fprintf(fp, "/cf  { newpath 0 360 arc fill } def\n");
	fprintf(fp, "/co  { newpath 0 360 arc stroke } def\n");
	fprintf(fp, "/r   { 3 index 3 index moveto 1 index 4 -1 roll\n");
	fprintf(fp, "       lineto exch 1 index lineto lineto closepath } def\n");
	fprintf(fp, "/rf  { r f } def\n");
	fprintf(fp, "/ro  { r o } def\n");
	fprintf(fp, "/t   { 6 -2 roll moveto gsave 3 index true\n");
	fprintf(fp, "       charpath flattenpath pathbbox grestore gsave\n");
	fprintf(fp, "       5 -1 roll rotate 6 -1 roll neg 3 -1 roll 5 -1\n");
	fprintf(fp, "       roll sub mul 4 -1 roll neg 3 -1 roll 4 -1 roll\n");
	fprintf(fp, "       sub mul rmoveto show grestore } def\n");
	fprintf(fp, "/cl  { initclip newpath 3 index 3 index moveto 1 index\n");
	fprintf(fp, "       4 -1 roll lineto  exch 1 index lineto lineto\n");
	fprintf(fp, "       closepath clip newpath } def\n");
	fprintf(fp, "/rgb { setrgbcolor } def\n");
	fprintf(fp, "/s   { scalefont setfont } def\n");
	fprintf(fp, "/R   { /%s findfont } def\n", font[2]);
	fprintf(fp, "/B   { /%s findfont } def\n", font[4]);
	fprintf(fp, "/I   { /%s findfont } def\n", font[6]);
	fprintf(fp, "/BI  { /%s findfont } def\n", font[8]);
	fprintf(fp, "/S   { /%s findfont } def\n", font[10]);
	fprintf(fp, "%%%%EndProlog\n");
}

void PostScriptFileTrailer(FILE *fp, int pageno)
{
	fprintf(fp, "ep\n");
	fprintf(fp, "%%%%Trailer\n");
	fprintf(fp, "%%%%Pages: %d\n", pageno);
	fprintf(fp, "%%%%EOF\n");
}

void PostScriptStartPage(FILE *fp, int pageno)
{
	fprintf(fp, "%%%%Page: %d %d\n", pageno, pageno);
	fprintf(fp, "bp\n");

}

void PostScriptEndPage(FILE *fp)
{
	fprintf(fp, "ep\n");
}

void PostScriptSetLineWidth(FILE *fp, double linewidth)
{
	fprintf(fp, "%.2f setlinewidth\n", linewidth);
}

void PostScriptSetClipRect(FILE *fp, double x0, double x1, double y0, double y1)
{
	fprintf(fp, "%.2f %.2f %.2f %.2f cl\n", x0, y0, x1, y1);
}

void PostScriptSetFont(FILE *fp, int typeface, double size)
{
	fprintf(fp, "%s %.0f s\n", TypeFaceDef[typeface], size);
}

void PostScriptSetColor(FILE *fp, double r, double g, double b)
{
	fprintf(fp,"%.4f %.4f %.4f rgb\n", r, g, b);
}

void PostScriptSetLineTexture(FILE *fp, int *lty, int nlty)
{
	int i;
	fprintf(fp,"[");
	for(i=0 ; i<nlty ; i++)
		fprintf(fp," %d", lty[i]);
	fprintf(fp,"] 0 setdash\n");
}

void PostScriptMoveTo(FILE *fp, double x, double y)
{
	fprintf(fp, "%.2f %.2f m\n", x, y);
}

void PostScriptLineTo(FILE *fp, double x, double y)
{
	fprintf(fp, "%.2f %.2f l\n", x, y);
}

void PostScriptStartPath(FILE *fp)
{
	fprintf(fp, "np\n");
}

void PostScriptEndPath(FILE *fp)		    
{
	fprintf(fp, "o\n");    
}

void PostScriptFilledRectangle(FILE *fp,
	double x0, double y0, double x1, double y1)
{
	fprintf(fp, "%.2f %.2f %.2f %.2f rf\n", x0, y0, x1, y1);
}

void PostScriptOpenRectangle(FILE *fp,
	double x0, double y0, double x1, double y1)
{
	fprintf(fp, "%.2f %.2f %.2f %.2f ro\n", x0, y0, x1, y1);
}

void PostScriptFilledCircle(FILE *fp, double x, double y, double r)
{
	fprintf(fp, "%.2f %.2f %.2f cf\n", x, y, r);
}

void PostScriptOpenCircle(FILE *fp, double x, double y, double r)
{
	fprintf(fp, "%.2f %.2f %.2f co\n", x, y, r);
}

void PostScriptFilledPolygon(FILE *fp, double *x, double *y, int nxy)
{
	int i;
	fprintf(fp, "np\n");
	fprintf(fp, "  %.2f %.2f m\n", x[0], y[0]);
	for(i=1 ; i<nxy ; i++)
		fprintf(fp, "  %.2f %.2f l\n", x[i], y[i]);
	fprintf(fp, "cp f\n");
}

void PostScriptOpenPolygon(FILE *fp, double *x, double *y, int nxy)
{
	int i;
	fprintf(fp, "np\n");
	fprintf(fp, "%.2f %.2f m\n", x[0], y[0]);
	for(i=1 ; i<nxy ; i++)
		fprintf(fp, "%.2f %.2f l\n", x[i], y[i]);
	fprintf(fp, "cp o\n");
}

static void PostScriptWriteString(FILE *fp, char *str)
{
	fputc('(', fp);
	for( ; *str ; str++)
		switch(*str) {
			case '\n':
				fprintf(fp, "\\n");
				break;
			case '-':
				if(isdigit(str[1]))
					fputc(177, fp);
					/* fprintf(fp, "\\177"); */
				else
					fputc(*str, fp);
				break;
			case '(':
			case ')':
				fprintf(fp, "\\%c", *str);
				break;
			default:
				fputc(*str, fp);
				break;
		}
	fputc(')', fp);
}

void PostScriptText(FILE *fp, double x, double y, char *str, double xc, double yc, double rot)
{
	fprintf(fp, "%.2f %.2f ", x, y);
	PostScriptWriteString(fp, str);
	fprintf(fp, " %.2f %.2f %.2f t\n", xc, yc, rot);
}
