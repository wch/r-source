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

#include <RCarbon.h>

#include <fp.h>
#include "RIntf.h"
#include <Fonts.h>
#include <QuickDraw.h>

#define R_RGB(r,g,b)	((r)|((g)<<8)|((b)<<16))
#define R_RED(col)	(((col)	   )&255)
#define R_GREEN(col)	(((col)>> 8)&255)
#define R_BLUE(col)	(((col)>>16)&255)


#include <ATSUnicode.h>
#include <TextEncodingConverter.h>

short mapHeight =0, mapLength = 0;

  int gcon;

 
 
 



void CStrToUniCode(char *someText, UniCharArrayPtr *ucap, UniCharCount *ucc);
void TryToSetFontTo(ATSUStyle theStyle, Str255 fontName);
ATSUFontID GetFontIDFromMacFontName(Str255 fontName);
OSStatus atsuSetIntAttribute( ATSUStyle iStyle, SInt32 iValue, ATSUAttributeTag iTag );
long MyStrLen(char * str);
OSErr NewRasterTextRotation(char *str, int face, int size, int color, int xx, int yy, double rot,  WindowPtr window);



OSStatus atsuSetFixedLineControl( ATSUTextLayout iLayout, UniCharArrayOffset iLineStart, Fixed iValue, ATSUAttributeTag iTag );

#define atsuSetFont( iStyle, iFontID )		atsuSetIntAttribute( iStyle, iFontID, kATSUFontTag )

 

#ifndef IntToFixed
#define IntToFixed(a)	   ((Fixed)(a) << 16)
#endif

#ifndef ff
#define ff(a)			   IntToFixed(a)
#endif


void CStrToUniCode(char *someText, UniCharArrayPtr *ucap, UniCharCount *ucc)
	{
	TECObjectRef ec;
	OSStatus status;
	ByteCount ail, aol, iLen, oLen ;
	Ptr buffer;
    int i;
        
	status = TECCreateConverter(&ec, kTextEncodingMacRoman, kTextEncodingUnicodeDefault);
	if (status != noErr) warning("TECCreateConverter failed [Internal MacError]");
	
	iLen = MyStrLen(someText);
	oLen = 2 * iLen;
	buffer = NewPtr(oLen);
	
	status = TECConvertText(ec, (ConstTextPtr)someText, iLen, &ail, (TextPtr)buffer, oLen, &aol);
	if (status != noErr) warning("TECConvertText failed [Internal MacError]");
	status = TECDisposeConverter(ec);
	if (status != noErr) warning("TECDisposeConverter failed [Internal MacError]");
	*ucap = (UniCharArrayPtr)NewPtr(aol);
	BlockMove(buffer, (*ucap), aol);
	DisposePtr(buffer);
	*ucc = aol / 2;
	
}


long MyStrLen(char * str)
	{
	long result = 0;
	char *p;
	for (p=str; *p; p++, result++) {};
	return result;
	}



extern  Str255		PostFont;
extern  Str255		MacSymbolFont;
extern  Boolean 	WeArePrinting, WeArePasting;

extern	PMPageFormat	pageFormat;
extern	PMPrintSettings	printSettings;
extern	PMPrintSession	printSession;
extern	Graphic_Ref		gGReference[MAX_NUM_G_WIN + 1];

extern	SInt32			systemVersion;

/* This routine writes a string of text using ATSUI technology when possibile.
   It also takes care of the graphic port used: device, printer or Clipboard.
   Jago April 2001, Stefano M. Iacus
*/
   
OSErr NewRasterTextRotation(char *str, int face, int size, int color, int xx, int yy, double rot,  WindowPtr window)
	{
	OSStatus status = noErr;
	Rect boundsRect = {50, 50, 200, 500};
	ATSUStyle tempS;
	ATSUTextLayout tempTL;
	ItemCount numberOfRuns;
	UniCharCount *runLengths;
	ATSUStyle *styles;
	UniCharArrayPtr theUnicodeText;			// the Text in Unicode
	UniCharCount uTextLength;	
	ATSUTextMeasurement xLocation;			// where it starts drawing at x
	ATSUTextMeasurement yLocation;			//                        and y
	ATSUTextMeasurement maxAscent;			// maximum ascent for the whole text
	ATSUTextMeasurement maxDescent;			// maximum descent for the whole text
	ATSUTextMeasurement lineHeight;			// its line height (maxAscent + maxDescent)
	Rect thePortRect;
	ATSUTextLayout textLayout;
    ByteCount				theSize;
	ATSUAttributeValuePtr	thePtr;
	Fixed iAngle;
	ATSUAttributeTag iTag; 
	int realFace;
    ATSUAttributeTag tags[] = {kATSUSizeTag, kATSUQDItalicTag, kATSUQDBoldfaceTag, kATSUColorTag, kATSUFontTag };
 	ByteCount sizes[] = {sizeof(Fixed), sizeof(Boolean), sizeof(Boolean), sizeof(RGBColor), sizeof(ATSUFontID)};
    RGBColor fontColor = {0x0, 0x0, 0x0};
    Boolean italic = false;
	Boolean bold = false;
    ATSUAttributeValuePtr values[5]; 
    Fixed fontPointSize;
    ATSUFontID fontID;
    ATSUFontFeatureType feature;
    ATSUFontFeatureSelector selector;
    UniCharCount i;
    CGrafPtr   port = nil;
    FMFontFamily postFontId;


    if(WeArePrinting  || WeArePasting )
	 port = gGReference[isGraphicWindow(window)].activePort;
    else
     port = GetWindowPort(window);
     
    SetPort(port);

    if(face==5){
    
    realFace = 0;	/* plain symbol */
    if(systemVersion > kMinSystemVersion)
    postFontId = FMGetFontFamilyFromName(MacSymbolFont);
    else
     GetFNum(MacSymbolFont,&postFontId);
 
    TextFont(postFontId);
    TextFace(realFace);
    TextSize(size);
    
    DrawText(str, 0, strlen(str));

    }
    else{
    
    // let's use some text
	CStrToUniCode(str, &theUnicodeText, &uTextLength);
      
  	// only 1 style thus only 1 run
	numberOfRuns = 1;
	runLengths = (UniCharCount *)NewPtr(numberOfRuns * sizeof(UniCharCount));
	runLengths[0] = uTextLength;

	// and it's the default style
	styles = (ATSUStyle *)NewPtr(numberOfRuns * sizeof(ATSUStyle));
	status = ATSUCreateStyle(&tempS);
	if (status != noErr) warning("ATSUCreateStyle failed [Internal MacError]");
    styles[0] = tempS;

	// and we create the text layout
	status = ATSUCreateTextLayoutWithTextPtr(theUnicodeText, 0, uTextLength, uTextLength, 
		numberOfRuns, runLengths, styles, &tempTL);
	if (status != noErr) warning("ATSUCreateTextLayoutWithTextPtr failed [Internal MacError]");
	textLayout = tempTL;
	
	// to be drawn at
	xLocation = ff(xx);
	
	     
	//GetPortBounds(port, &thePortRect);
	yLocation = ff(yy);
	
	iTag = kATSULineRotationTag;
	iAngle = ff((int)rot);
	theSize = sizeof( iAngle ); 
	thePtr = &iAngle;

    /* we rotate the text here */
    
    status = ATSUSetLineControls( textLayout, 0, 1, &iTag, &theSize, &thePtr );


    realFace = 0;
    if (face == 1) { 
     realFace = 0;    /* normal */
     bold = false;
     italic = false;
     }
    if (face == 2) {
     realFace = 1;    /* bold */
     bold = true;
     italic = false;
     }
    if (face == 3) {
     realFace = 2;    /* italic */
     italic = true;
     bold = false;
     }
    if (face == 4) {
     realFace = 3;    /* bold & italic */
     bold = true;
     italic = true;
     }

     fontID = GetFontIDFromMacFontName(PostFont);
     
    if (fontID == kATSUInvalidFontID) 
     warning("can't find font [Internal MacError]");

    fontPointSize = ff(size);
	
    fontColor.red = R_RED(color) * 255;
	fontColor.green = R_GREEN(color) * 255;
	fontColor.blue = R_BLUE(color) * 255;

	values[0] = &fontPointSize;
	values[1] = &italic;
	values[2] = &bold;
    values[3] = &fontColor;
	values[4] = &fontID;
    
    status = ATSUSetAttributes(tempS, 5, tags, sizes, values);
	if (status != noErr) warning("ATSUSetAttributes failed [Internal MacError]");
	
    /* we finally draw the text */
    
	status = ATSUDrawText(textLayout, 0, uTextLength, xLocation, yLocation);
				if (status != noErr) warning("ATSUDrawText failed [Internal MacError]");
	if(runLengths)
	 DisposePtr((char *)runLengths);			
	if(styles)
	 DisposePtr((char *)styles);			
	return status;
	} /* if face==5  else */
}

 

OSStatus
atsuSetIntAttribute( ATSUStyle iStyle, SInt32 iValue, ATSUAttributeTag iTag )
{
	ByteCount				theSize = sizeof(iValue);
	ATSUAttributeValuePtr	thePtr = &iValue;

	return ATSUSetAttributes( iStyle, 1, &iTag, &theSize, &thePtr );
}

ATSUFontID GetFontIDFromMacFontName(Str255 fontName)
	{
	ATSUFontID result = kATSUInvalidFontID, found;
	short iFONDNumber;
	OSStatus status;
	GetFNum(fontName, &iFONDNumber);
	if (!iFONDNumber) return kATSUInvalidFontID;	// GetFNum return 0 if not found.
	status = ATSUFONDtoFontID(iFONDNumber, NULL, &found);
	if (status == noErr) result = found;
	return result;
	}

 

/* This function converts any C string to the equivalent Unicode version 
   Jago: Mar 2001, Stefano M. Iacus
*/
void CStringToUnicode(char *someText, UniCharArrayPtr *ucap, UniCharCount *ucc)
{
	TECObjectRef ec;
	OSStatus status;
	ByteCount ail, aol, iLen, oLen;
	Ptr buffer;
	
	status = TECCreateConverter(&ec, kTextEncodingMacRoman, kTextEncodingUnicodeDefault);
	if (status != noErr) R_ShowMessage("\p TECCreateConverter failed");
	
	iLen = strlen(someText);
	oLen = 2 * iLen;
	
	buffer = NewPtr(oLen);
	status = TECConvertText(ec, (ConstTextPtr)someText, iLen, &ail, (TextPtr)buffer, oLen, &aol);
	if (status != noErr) R_ShowMessage("\p TECConvertText failed");
	status = TECDisposeConverter(ec);
	if (status != noErr) R_ShowMessage("\p TECDisposeConverter failed");
	*ucap = (UniCharArrayPtr)NewPtr(aol);

	 BlockMove(buffer, (*ucap), aol);
	DisposePtr(buffer);
	*ucc = aol / 2;

}

 



  
 
 