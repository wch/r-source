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

#include "Graphics.h"
#include <MacHeaders>
#include <stdio.h>
#include <FixMath.h>
#include "::macintosh:console.h"
#include <THINK.h>
#include <Printing.h>
#include "PicComments.h"

#define FONT_SIZE	9

/* Basic Mac Objects */
WindowRecord	graphicsWindowRecord;
WindowPtr		graphicsWindow;
PicHandle		graphicsPicture;
Rect			windowRect;
Rect			graphicsRect = { 0, 0, 300, 400 };
Rect			pictureRect;
int				fontw, fonth;

static void CopyGraphToScrap(void);

static void doGraphicsCursor()
{
	Point mousePt;
	
	GetMouse(&mousePt);
	if(PtInRect(mousePt, &(graphicsWindow->portRect)))
		SetCursor(*cross);
	else
		SetCursor(&arrow);
}

static void doGraphicsKey(int theChar, EventRecord *theEvent)
{
	SysBeep(2);
}

static void doGraphicsMenu(long code)
{
	WindowPtr savePort;
	PenState savePen;
	
	switch (HiWord(code)) {
		case 1:
			DoMenu(code);
			return;
		case 2:
			switch(LoWord(code)) {
			case pageSetCommand:
				SetupPrinter();
				break;
			case printCommand:
				DoPlotPrint(graphicsPicture);
				break;
			case quitCommand:
				HiliteMenu(0);
				cleanup();
				break;
			}
			break;
		case 3:		/* Edit */	
			switch(LoWord(code)) {
			/*
			case undoCommand:
			case cutCommand:
			case copyCommand:
			case pasteCommand:
			case clearCommand:
			*/
			case copyCommand:
				CopyGraphToScrap();
				GetPort(&savePort);
				SetPort(graphicsWindow);
				GetPenState(&savePen);
				PenMode(patXor);
				PaintRect(&(graphicsWindow->portRect));
				PaintRect(&(graphicsWindow->portRect));
				SetPenState(&savePen);
				SetPort(savePort);
				break;
			}
			break;
	}
	HiliteMenu(0);

}

static void doGraphicsGrow(WindowPtr theWindow, Point where)
{
	long newsize;
	newsize = GrowWindow(theWindow, where, &dragRect);
	SizeWindow(theWindow, LoWord(newsize), HiWord(newsize), true);
	graphicsRect.right = LoWord(newsize);
	graphicsRect.bottom = HiWord(newsize);
}

static void doGraphicsActive(EventRecord *theEvent, WindowPtr theWindow, short windowCode)
{

	SetPort(theWindow);
	switch(windowCode) {
		case inContent:
			SysBeep(2);
			break;
		case inDrag:
			DragWindow(theWindow, theEvent->where, &dragRect);
			break;
		case inGrow:
			doGraphicsGrow(theWindow, theEvent->where);
			break;
		case inSysWindow:
			SystemClick(theEvent, theWindow);
			break;
	}
}


static void doGraphicsInactive(EventRecord *theEvent, WindowPtr theWindow, short windowCode)
{
	switch(windowCode) {
		case inContent:
			SelectWindow(theWindow);
			break;
		case inDrag:
			DragWindow(theWindow, theEvent->where, &dragRect);
			break;
		case inSysWindow:
			SystemClick(theEvent, theWindow);
			break;
	}
}

static void doGraphicsNotWindow(EventRecord *theEvent, WindowPtr theWindow, short windowCode)
{
	switch(windowCode) {
		case inMenuBar:
			doGraphicsMenu(MenuSelect(theEvent->where));
			break;
		case inDesk:
			SystemClick(theEvent, theWindow);
			break;
	}
}

static void doGraphicsUpdate(WindowPtr theWindow)
{
	/* The window manager handles this */
	/*	
	WindowPtr savePort;
	GetPort(&savePort);
	SetPort(graphicsWindow);
	DrawPicture(graphicsPicture, &graphicsRect);
	SetPort(&savePort);
	*/
}

static void doGraphicsActivate(int theCode)
{
	if(theCode) {
		SetPort(graphicsWindow);
		
		DisableItem(fileMenu, newCommand);
		DisableItem(fileMenu, openCommand);
		DisableItem(fileMenu, saveCommand);
		DisableItem(fileMenu, saveAsCommand);
		EnableItem(fileMenu, pageSetCommand);
		EnableItem(fileMenu, printCommand);
		EnableItem(fileMenu, quitCommand);

		DisableItem(editMenu, undoCommand);
		DisableItem(editMenu, cutCommand);
		EnableItem(editMenu, copyCommand);
		DisableItem(editMenu, pasteCommand);
		DisableItem(editMenu, clearCommand);
	}
	else {
		EnableItem(fileMenu, newCommand);
		EnableItem(fileMenu, openCommand);
		EnableItem(fileMenu, saveCommand);
		EnableItem(fileMenu, saveAsCommand);
		EnableItem(fileMenu, pageSetCommand);
		EnableItem(fileMenu, printCommand);
		EnableItem(fileMenu, newCommand);
		
		EnableItem(editMenu, undoCommand);
		EnableItem(editMenu, cutCommand);
		EnableItem(editMenu, copyCommand);
		EnableItem(editMenu, pasteCommand);
		EnableItem(editMenu, clearCommand);
	}
}


void InitGraphicsContext(void)
{
	WindowPtr savePort;
	FontInfo info;
	Point linesize;
	Point *lp;
	linesize.h = 2;
	linesize.v = 1;
	lp = &linesize;
	
	GetPort(&savePort);
	windowRect.left = screenBits.bounds.right - graphicsRect.right - 4;
	windowRect.top = screenBits.bounds.bottom - graphicsRect.bottom - 4;
	windowRect.right = screenBits.bounds.right - 4;
	windowRect.bottom = screenBits.bounds.bottom - 4;
	graphicsWindow = NewWindow(0L, &windowRect, "\pR Graphics", true, documentProc, (WindowPtr) 0L, false, 0);
	SetPort(graphicsWindow);
	/*
	pictureRect.top = graphicsRect.top * 10;
	pictureRect.left = graphicsRect.left *10;
	pictureRect.bottom = graphicsRect.bottom *10;
	pictureRect.right = graphicsRect.right *10;
	*/
	TextSize(FONT_SIZE);
	GetFontInfo(&info);
	fonth = (info.ascent+info.descent);
	fontw = (info.widMax+info.leading);
	
	graphicsPicture = OpenPicture(&graphicsRect);
	PicComment(SetLineWidth, 4, &lp);
	ClosePicture();
	
	SetWindowPic(graphicsWindow, graphicsPicture);

	SetPort(savePort);
	graphContext.window = graphicsWindow;
	graphContext.active = 0;					/* 1 only when locator in action */
	graphContext.doCursor = doGraphicsCursor;
	graphContext.doKey = doGraphicsKey;
	graphContext.doMenu = doGraphicsMenu;
	graphContext.doActive = doGraphicsActive;
	graphContext.doInactive = doGraphicsInactive;
	graphContext.doNotWindow = doGraphicsNotWindow;
	graphContext.doUpdate = doGraphicsUpdate;
	graphContext.doActivate = doGraphicsActivate;
}

/* Current Clipping Rectangle */
static double Clipxl, Clipxr, Clipyb, Clipyt;

/* Last Point Coordinates */
static int xlast = 0;
static int ylast = 0;

/* Open a Graphics Window and set Graphics State */
static int Mac_Open(void)
{
	InitGraphicsContext();
	DevInit = 1;
	return 1;
}

/* Set the Clipping Rectangle */
static void Mac_Clip(int x0, int x1, int y0, int y1)
{
	if(x0 < x1) {
		Clipxl = x0;
		Clipxr = x1;
	}
	else {
		Clipxl = x1;
		Clipxr = x0;
	}
	if(y0 < y1) {
		Clipyb = y0;
		Clipyt = y1;
	}
	else {
		Clipyb = y1;
		Clipyt = y0;
	}
}

/* Actions on Window Resize */
static void Mac_Resize()
{
	DP->right = graphicsRect.right;
	DP->bottom = graphicsRect.bottom;
}

/* Begin a New Plot */
static void Mac_NewPlot()
{
	Point linesize;
	Point *lp;
	int xoff, yoff;
	linesize.h = 2;
	linesize.v = 1;
	lp = &linesize;
	
	SetPort(graphicsWindow);
	EraseRect(&graphicsRect);
	KillPicture(graphicsPicture);
	graphicsPicture = OpenPicture(&graphicsRect);
	PicComment(SetLineWidth, 4, &lp);
	ClosePicture();
	SetWindowPic(graphicsWindow, graphicsPicture);
}

/* Close the Graphics Window */
static void Mac_Close()
{
	KillPicture(graphicsPicture);
	DisposeWindow(graphicsWindow);
}

/* MoveTo */
static void Mac_MoveTo(int x, int y)
{
	xlast = x;
	ylast = y;
	MoveTo(x,y);
}

/* Dot */
static void Mac_Dot(void)
{
}



/* LineTo */
static void Mac_LineTo(int x, int y)
{
	LineTo(x, y);
	xlast = x;
	ylast = y;
}

/* Draw a Filled Rectangle */
static void Mac_Rect(int x0, int y0, int x1, int y1, int col, int fill)
{
}

static double cex;
static int fontsize;

/* Horizontal Text Drawing */
static void Mac_Text(char *str, double xc, double yc)
{
	int x, y;
	if(cex != GP->cex) {
		cex = GP->cex;
		fontsize = FONT_SIZE * cex;
		TextSize(fontsize);
	}
	x = -xc*TextWidth(str,0,strlen(str));
	y = yc*fonth;
	Move(x,y);
	DrawText(str,0,strlen(str));
}

/* Rotated Text Utilities */
static void NewBitMap(BitMap *theBitMap)
{
	int i;
	theBitMap->rowBytes = ((theBitMap->bounds.right - theBitMap->bounds.left + 15)/16)*2;
	theBitMap->baseAddr = NewPtr(theBitMap->rowBytes*(theBitMap->bounds.bottom-theBitMap->bounds.top));
	if(MemError() != noErr) theBitMap->baseAddr = NULL;
	else
		for(i=0 ; i<theBitMap->rowBytes*(theBitMap->bounds.bottom-theBitMap->bounds.top); i++)
			theBitMap->baseAddr[i] = 0;
}

static void DisposeBitMap(BitMap *theBitMap)
{
	DisposePtr(theBitMap->baseAddr);
}

static Rotate(BitMap *sourceMap, BitMap *destMap, int rot)
{
	int i, j;
	int height = sourceMap->bounds.bottom - sourceMap->bounds.top;
	int width = sourceMap->bounds.right - sourceMap->bounds.left;
	int sourceBits = 8 * sourceMap->rowBytes;
	int destBits = 8 * destMap->rowBytes;
	
	switch(rot) {
		case 1:
			for(i=0 ; i<height ; i++)
				for(j=0 ; j<width ; j++)
					if(BitTst(sourceMap->baseAddr, i*sourceBits+j))
						BitSet(destMap->baseAddr, (width-j-1)*destBits+i);
			break;
		case 2:
			for(i=0 ; i<height ; i++)
				for(j=0 ; j<width ; j++)
					if(BitTst(sourceMap->baseAddr, i*sourceBits+j))
						BitSet(destMap->baseAddr, (height-i-1)*destBits+(width-j-1));
			break;
		case 3:
			for(i=0 ; i<height ; i++)
				for(j=0 ; j<width ; j++)
					if(BitTst(sourceMap->baseAddr, i*sourceBits+j))
						BitSet(destMap->baseAddr, j*destBits+(height-i-1));
	}

}

static void RasterTextRotation(char *str, int nstr, int just, int rot)
{
	FontInfo myFontInfo;
	short strWidth, strHeight, mapLength, mapHeight, xOffset, yOffset;
	BitMap sourceMap, destMap;
	Rect sourceRect, destRect, myClipRect;
	Point cp;
	RgnHandle myRgn;
	GrafPtr origPort, offScrGrafPort;
	Fixed Tx, Ty;
	
	if(rot <= 0 || rot >=4) {	/* Unrotated */
		DrawText(str, 0, nstr);
		return;
	}
	
	GetPen(&cp);
	GetFontInfo(&myFontInfo);
	strWidth = TextWidth(str, 0, nstr);
	strHeight = myFontInfo.ascent + 2 * myFontInfo.descent;
	
	/* Unrotated and Rotated BitMaps */
	mapLength = ((strWidth -1) / 16 + 1) * 16;
	mapHeight = strHeight;
	SetRect(&sourceRect, 0, 0, mapLength, mapHeight);
	sourceMap.bounds = sourceRect;
	NewBitMap(&sourceMap);
	
	switch(rot) {
		case 1:
		case 3:
			SetRect(&destRect, 0, 0, mapHeight, mapLength);
			break;
		case 2:
			SetRect(&destRect, 0, 0, mapLength, mapHeight);
			break;		
	}
	destMap.bounds = destRect;
	NewBitMap(&destMap);
	
	GetPort(&origPort);
	offScrGrafPort = (GrafPtr)NewPtr(sizeof(GrafPort));
	OpenPort(offScrGrafPort);
	offScrGrafPort->portRect = sourceMap.bounds;
	SetPortBits(&sourceMap);
	offScrGrafPort->txFont = origPort->txFont;
	offScrGrafPort->txSize = origPort->txSize;
	offScrGrafPort->txFace = origPort->txFace;
	offScrGrafPort->txFace = origPort->txMode;
	offScrGrafPort->txFace = origPort->spExtra;
	
	GetFontInfo(&myFontInfo);
	MoveTo(0,myFontInfo.ascent);
	DrawText(str, 0, nstr);
	
	SetPort(origPort);
	Rotate(&sourceMap, &destMap, rot);
	
	/* Adjust Origin for Rotation */
	xOffset = cp.h;
	yOffset = cp.v;
	switch(rot) {
		case 1:
			xOffset -= myFontInfo.ascent;
			yOffset -= (sourceRect.right - sourceRect.left);
			break;
		case 2:
			xOffset -= (sourceRect.right - sourceRect.left);
			yOffset -= (sourceRect.bottom - sourceRect.top - myFontInfo.ascent);
			break;
		case 3:
			xOffset -= (sourceRect.bottom - sourceRect.top - myFontInfo.ascent);
			yOffset -= 0;
			break;
	}
	OffsetRect(&destRect,xOffset,yOffset);
	CopyBits(&destMap,  &origPort->portBits, &destMap.bounds, &destRect, srcOr, NULL);
	
	DisposeBitMap(&destMap);
	DisposeBitMap(&sourceMap);
	DisposPtr(offScrGrafPort);
}

/* Rotated Text */
static void Mac_RText(char *str, double xc, double yc, int rot)
{
	TTxtPicHdl hT;
	TCenterHdl cT;
	Rect zeroRect;
	Point pt;
	RgnHandle oldClip;
	int x, y, nstr;
	
	if(cex != GP->cex) {
		cex = GP->cex;
		fontsize = FONT_SIZE * cex;
		TextSize(fontsize);
	}
	nstr = strlen(str);
	rot = (rot/90)%4;
	if(rot == 0) {
		DrawText(str, 0, nstr);
		return;
	}
	GetPen(&pt);
	
	hT = (TTxtPicHdl)NewHandle(sizeof(TTxtPicRec));
	cT = (TCenterHdl)NewHandle(sizeof(TCenterRec));
	
	(**hT).tJus = tJustLeft;
	(**hT).tFlip = tFlipNone;
	(**hT).tAngle = -90*rot;
	(**hT).tLine = 0;
	(**hT).tCmnt = 0;
	(**hT).tAngleFixed = Long2Fix(-90*rot);
	
	(**cT).y = 0;
	(**cT).x = 0;
	
	x = - xc * TextWidth(str, 0, strlen(str));
	y = yc * fonth;
	switch(rot) {
		case 0:
			pt.h += x;
			pt.v += y;
			break;
		case 1:
			pt.h -= y;
			pt.v -= x;
			break;
		case 2:
			pt.h -= x;
			pt.v -= y;
			break;
		case 3:
			pt.h += y;
			pt.v += x;
			break;
	}
	
	PicComment(picDwgBeg, 0, NULL);
	PicComment(TextBegin, sizeof(TTxtPicRec), (Handle)hT);
	PicComment(TextCenter, sizeof(TCenterRec), (Handle)cT);
	
	/* PostScript */
	MoveTo(pt.h, pt.v);
	oldClip = NewRgn();
	GetClip(oldClip);
	SetRect(&zeroRect, 0, 0, 0, 0);
	ClipRect(&zeroRect);
	DrawText(str, 0, strlen(str));
	ClipRect(&(**oldClip).rgnBBox);
	
	/* QuickDraw */
	MoveTo(pt.h, pt.v);
	RasterTextRotation(str, nstr, 0, rot);
	
	PicComment(TextEnd, 0, NULL);
	PicComment(picDwgEnd, 0, NULL);
	DisposHandle((Handle)hT);
	DisposHandle((Handle)cT);
}

/* Return the Pointer Location */
static int Mac_Locator(int *x, int *y)
{
	return 0;
}

static WindowPtr savePort;

/* Set the Graphics Mode */
static void Mac_Mode(int mode)
{
	PicHandle newPicture;

	if(mode == 1) {
		GetPort(&savePort);
		SetPort(graphicsWindow);
		SetWindowPic(graphicsWindow, newPicture=OpenPicture(&graphicsRect));
		ShowPen();
		DrawPicture(graphicsPicture, &graphicsRect);
		KillPicture(graphicsPicture);
		graphicsPicture = newPicture;
	}
	else {
		ClosePicture();
		SetPort(savePort);
	}
}

/* Keep the Graphics Window in Front */
static void Mac_Hold(void)
{
	/* Ross - Nov 22, 1993 */
	/* Removed this, because it felt wrong in situations */
	/* where it takes multiple commands to produce a graph */
	/* Bringing the window to the font should happen manually */
	/* with the exception of graphical input. */
	/* What happens when control returns to the program ? */
	/* Possible ans: active should only be set by a locator */
	/* and the same hooks used to keep the window in front */
	/* before the next prompt */
}

static void CopyGraphToScrap(void)
{
	long picSize;
	
	picSize = GetHandleSize(graphicsPicture);
	ZeroScrap();
	HLock(graphicsPicture);
	PutScrap(picSize, 'PICT', *graphicsPicture);
	HUnlock(graphicsPicture);
}

/* Device Driver */
MacDeviceDriver()
{
	DevInit = 0;
	if( ! Mac_Open() ) return 0;

	DevOpen = Mac_Open;
	DevClose = Mac_Close;
	DevResize = Mac_Resize;
	DevNewPlot = Mac_NewPlot;
	DevClip = Mac_Clip;
	DevMoveTo = Mac_MoveTo;
	DevLineTo = Mac_LineTo;
	DevText = Mac_Text;
	DevRText = Mac_RText;
	DevDot = Mac_Dot;
	DevRect = Mac_Rect;
	DevLocator = Mac_Locator;
	DevMode = Mac_Mode;
	DevHold = Mac_Hold;

	GP->left = 0;
	GP->right = windowRect.right - windowRect.left;
	GP->bottom = windowRect.bottom - windowRect.top;
	GP->top = 0;

	GP->cra[0] = fontw;
	GP->cra[1] = FONT_SIZE;

	GP->xCharOffset = 0.0;
	GP->yCharOffset = 0.0;

	GP->ipr[0] = 1/72.0;
	GP->ipr[1] = 1/72.0;

	GP->canResizePlot = 1;
	GP->canChangeFont = 0;
	GP->canRotateText = 1;
	GP->canResizeText = 0;
	GP->canClip = 0;

	DevInit = 1;
	cex = 1.0;
	return 1;
}
