/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file RText.c
 *  Copyright (C) 1998-1999  Ross Ihaka
 *                2000-2001  Stefano M. Iacus and the R core team
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

#include <fp.h>
#include "RIntf.h"

short mapHeight =0, mapLength = 0;

void NewBitMap(BitMap *theBitMap);
void DisposeBitMap(BitMap *theBitMap);
static Rotate(BitMap*, BitMap*, int );
void RasterTextRotation(char *str, int nstr, int just, int rot);
Boolean inStrRange(int x, int y);
int BackTranX(int x, int y, int rot);
int BackTranY(int x, int y, int rot);
int tranStrY(int y);
int tranBitX(int x, int rot);
int tranBitY(int y, int checkHeight, int rot);
int gcon;



/* Rotated Text Utilities */
void NewBitMap(BitMap *theBitMap)
{
    int i;
    theBitMap->rowBytes = ((theBitMap->bounds.right - theBitMap->bounds.left
			    + 15)/16)*2;
    theBitMap->baseAddr = NewPtr(theBitMap->rowBytes*(theBitMap->bounds.bottom-theBitMap->bounds.top));
    if(MemError() != noErr) theBitMap->baseAddr = NULL;
    else
	for(i=0 ; i<theBitMap->rowBytes*(theBitMap->bounds.bottom-theBitMap->bounds.top); i++)
	    theBitMap->baseAddr[i] = 0;
}

void DisposeBitMap(BitMap *theBitMap)
{
    DisposePtr(theBitMap->baseAddr);
}

static Rotate(BitMap *sourceMap, BitMap *destMap, int rot)
{
    int i, j;
    int height = sourceMap->bounds.bottom - sourceMap->bounds.top;
    int width = sourceMap->bounds.right - sourceMap->bounds.left;
    int checkHeight = destMap->bounds.bottom - destMap->bounds.top;
    int diffHeight = checkHeight - height;
    int checkWidth = destMap->bounds.right - destMap->bounds.left;
    int sourceBits = 8 * sourceMap->rowBytes;
    int destBits = 8 * destMap->rowBytes;

    if (rot <=360){
	for (i=0; i <checkHeight; i++){
	    for (j=0; j<checkWidth; j++){
		if (inStrRange(BackTranX(tranBitX(j, rot), tranBitY(i, checkHeight, rot), rot), BackTranY(tranBitX(j, rot), tranBitY(i, checkHeight, rot), rot)))
		    if (BitTst(sourceMap->baseAddr, tranStrY(BackTranY(tranBitX(j, rot), tranBitY(i, checkHeight, rot), rot) )*sourceBits + BackTranX(tranBitX(j, rot), tranBitY(i, checkHeight, rot), rot))){
			BitSet(destMap->baseAddr, i*destBits+j);
		    }
	    }
	}
    }
}

void RasterTextRotation(char *str, int nstr, int just, int rot)
{
    FontInfo myFontInfo;
    short strWidth, strHeight, xOffset, yOffset;
    short newWidth, newHeight;
    BitMap sourceMap, destMap;
    Rect sourceRect, destRect;
    Point cp;
    GrafPtr origPort, offScrGrafPort;
    /* Unrotated */

    if(rot <= 0 || rot >=360) {
	DrawText(str, 0, nstr);
	return;
    }
    GetPen(&cp);
    GetFontInfo(&myFontInfo);
    strWidth = TextWidth(str, 0, nstr);
    strHeight = myFontInfo.ascent + myFontInfo.descent;
    gcon = myFontInfo.descent;
    /* Unrotated and Rotated BitMaps */
    mapLength = ((strWidth -1) / 16 + 1) * 16;
    mapHeight = strHeight;
    SetRect(&sourceRect, 0, 0, mapLength, mapHeight);
    sourceMap.bounds = sourceRect;
    NewBitMap(&sourceMap);
    if (rot<=90){
	newWidth = mapLength*cos(toRadian(rot)) +
	    mapHeight*sin(toRadian(rot));
	newHeight = mapLength*sin(toRadian(rot)) +
	    mapHeight*cos(toRadian(rot));
	SetRect(&destRect, -mapHeight*sin(toRadian(rot)), -newHeight,
		mapLength*cos(toRadian(rot)) + gcon, 0); //gcon at the end?
    }else if (rot <= 180){
	newWidth = mapLength*cos(toRadian(180-rot)) +
	    mapHeight*sin(toRadian(180-rot));
	newHeight = mapLength*sin(toRadian(180-rot)) +
	    mapHeight*cos(toRadian(180-rot));
	SetRect(&destRect, -newWidth, -mapLength*sin(toRadian(180-rot)) - gcon,
		gcon, mapHeight*cos(toRadian(180-rot)));
    }else if (rot <=270){
	newWidth = mapLength*cos(toRadian(rot-180)) +
	    mapHeight*sin(toRadian(rot-180));
	newHeight = mapLength*sin(toRadian(rot-180)) +
	    mapHeight*cos(toRadian(rot-180));
	SetRect(&destRect, -mapLength*cos(toRadian(rot-180)) - gcon,
		-gcon, mapHeight*sin(toRadian(rot-180)), newHeight);
    }else if (rot <360){
	newWidth = mapLength*cos(toRadian(360-rot)) +
	    mapHeight*sin(toRadian(360-rot));
	newHeight = mapLength*sin(toRadian(360-rot)) +
	    mapHeight*cos(toRadian(360-rot));
	SetRect(&destRect, -gcon, -mapHeight*cos(toRadian(360-rot)),
		newWidth, mapLength*sin(toRadian(360-rot))+gcon);

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
    OffsetRect(&destRect,xOffset,yOffset);
    //DRWrite(xOffset);
    //DRWrite(yOffset);
    CopyBits(&destMap,  &origPort->portBits, &destMap.bounds, &destRect,
	     srcOr, NULL);

    DisposeBitMap(&destMap);
    DisposeBitMap(&sourceMap);
    DisposePtr((char*)offScrGrafPort);
}


int tranStrY(int y)
{
    return (mapHeight - y)- gcon ;
}

int tranBitY(int y, int checkHeight, int rot)
{
    if (rot <= 90)
	return (checkHeight - y) ;
    else if (rot <= 180)
	return (checkHeight - y) - mapHeight*cos(toRadian(180-rot));
    else if (rot <= 270)
	return (checkHeight - y) - (mapLength*sin(toRadian(rot-180)) +
				    mapHeight*cos(toRadian(rot-180)));
    else if (rot < 360)
	return (checkHeight - y) - mapLength*sin(toRadian(360-rot));
}

int tranBitX(int y, int rot)
{
    if (rot <= 90)
	return y -mapHeight*sin(toRadian(rot));
    else if (rot <=180)
	return y - (mapLength*cos(toRadian(180-rot)) + 
		    mapHeight*sin(toRadian(180-rot)));
    else if (rot <= 270)
	return y - mapLength*cos(toRadian(rot-180));
    else if (rot < 360)
	return y;
    //return y;
}

Boolean inStrRange(int x, int y)
{
    if ((x <= 0) || (y <= - gcon) || (x >= mapLength) || 
	(y >= mapHeight - gcon)) return false;
    return true;
}


// Input rot in degree
// x and y are in normal coordinates
int BackTranX(int x, int y, int rot)
{
    return x*cos(toRadian(rot)) + y*sin(toRadian(rot));
}

// Input rot in degree
// x and y are in normal coordinates
int BackTranY(int x, int y, int rot)
{
    return - x*sin(toRadian(rot)) + y*cos(toRadian(rot));
}
