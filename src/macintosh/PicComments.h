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

#ifndef __TYPES__
#include <Types.h>
#endif

#ifndef __QUICKDRAW__
#include <QuickDraw.h>
#endif

enum {
	picDwgBeg = 130,
	picDwgEnd = 131,
	TextBegin = 150,
	TextEnd = 151,
	StringBegin = 152,
	StringEnd = 153,
	TextCenter = 154,
	LineLayoutOff = 155,
	LineLayoutOn = 156,
	ClientLineLayout = 157,
	PolyBegin = 160,
	PolyEnd = 161,
	PolyIgnore = 163,
	PolySmooth = 164,
	PolyClose = 165,
	DashedLine = 180,
	DashedStop = 181,
	SetLineWidth = 182,
	PostScriptBegin = 190,
	PostScriptEnd = 191,
	PostScriptHandle = 192,
	PostScriptFile = 193,
	TextIsPostScript = 194,
	ResourcePS = 195,
	PSBeginNoSave = 196,
	SetGrayLevel = 197,
	RotateBegin = 200,
	RotateEnd = 201,
	RotateCenter = 202,
	FormsPrinting = 210,
	EndFormsPrinting = 211
};

enum {
	tJustNone = 0,
	tJustLeft = 1,
	tJustCenter = 2,
	tJustRight = 3,
	tJustFull = 4
};

enum {
	tFlipNone = 0,
	tFlipHorizontal = 1,
	tFlipVertical = 2
};

typedef struct TTxtPicRec {
	Byte	tJus;
	Byte	tFlip;
	short	tAngle;					/* Clockwise rotation in degrees 0..360 */
	Byte	tLine;					/* Unused/Ignored */
	Byte	tCmnt;					/* Reserved */
	Fixed	tAngleFixed;			/* Same as "tAngle" in Fixed precision */
}
TTxtPicRec, *TTxtPicPtr, **TTxtPicHdl;


typedef struct TRotationRec {
	short	rFlip;
	short	rAngle;					/* Clockwise rotation in degrees 0..360 */
	Fixed	rAngleFixed;			/* Same as "tAngle" in Fixed precision */
}
TRotationRec, *TRotationPtr, **TRotationHdl;


typedef struct TCenterRec {
	Fixed x;
	Fixed y;
}
TCenterRec, *TCenterPtr, **TCenterHdl;


typedef struct TPolyVerbRec {
	Boolean	f7, f6, f5, f4, f3;		/* reserved */
	Boolean	fPolyClose;				/* TRUE = smoothing across endpoint. */
	Boolean	fPolyFill;				/* TRUE = Polygon should be filled. */
	Boolean	fPolyFrame;				/* TRUE = Polygon should be framed. */
}
TPolyVerbRec, *TPolyVerbPtr, **TPolyVerbHdl;


typedef struct TDashedLineRec {
	char offset;					/* offset into pattern for first dash. */
	char centered;					/* Ignored */
	char intervals[5];				/* Array of dash intervals, intervals[0] = number */
}
TDashedLineRec, *TDashedLinePtr, **TDashedLineHdl;


typedef Point TLineWidth, *TLineWidthPtr, **TLineWidthHdl;


typedef struct TClientLLRec {
	short	chCount;
	Fixed	major;
	short	spcChar;
	Fixed	minor;
	Fixed	ulLength;
}
TClientLLRec, *TClientLLPtr, **TClientLLHdl;
