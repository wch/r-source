/*
 *	WEExtraHooks.c
 *
 *	Hooks for adding no-wrap behavior and "show invisibles" to WASTE
 *	Written by Jonathan Kew
 *
 */

#include "WEExtraHooks.h"

static const Point kOneToOneScaling = { 1, 1 } ;

pascal StyledLineBreakCode _WENoWrapLineBreak
	(
		Ptr pText ,
		SInt32 textLength ,
		SInt32 textStart ,
		SInt32 textEnd ,
		Fixed * textWidth ,
		SInt32 * textOffset ,
		WEReference we
	)
{
#pragma unused ( textLength, textWidth, we )

	SInt32 ii ;
	StyledLineBreakCode breakCode = smBreakOverflow ;

	for ( ii = textStart ; ii < textEnd ; ii ++ )
	{
		if ( pText [ ii ] == kEOL )
		{
			breakCode = smBreakWord ;
			*textOffset = ii + 1 ;
			break ;
		}
	}

	if ( breakCode == smBreakOverflow )
	{
		* textOffset = ii ;
	}

	return breakCode ;
}

// This "show invisibles" hook is *not* ready for prime time -- it's inefficient and not
// compatible with other features such as RL text. This is just an experiment!
pascal void _WEShowInvisiblesDrawText
	(
		Ptr pText ,
		SInt32 textLength ,
		Fixed slop ,
		JustStyleCode styleRunPosition ,
		WEReference we
	)
{
	//	these are "static" to avoid the overhead of setting them up on the fly
	//	(note that the both the baseAddr field and the rowBytes field of a BitMap
	//	*must* be even!)
	static long tabIcon [ ] = { 0x00000800, 0x0c007e00, 0x7f007e00, 0x0c000800} ;
	static BitMap tabBitMap = { (Ptr) tabIcon, 2, { 0, 0, 8, 8 } } ;
	static long parIcon [ ] = { 0x3f006500, 0x65006500, 0x3d000500, 0x05000500} ;
	static BitMap parBitMap = { (Ptr) parIcon, 2, { 0, 0, 8, 8 } } ;

	WEDrawTextUPP oldDrawTextProc = nil ;
	WECharToPixelUPP charToPixelProc = nil ;
	GrafPtr port ;
	Rect dstRect ;
	Point startPt ;
	SInt32 i, width, halfSpaceWidth = 0x80000000 ;
	RGBColor color ;

	GetPort ( & port ) ;
	GetPen ( & startPt ) ;

	//	first draw the text using the original draw hook,
	//	which was saved by WEInstallShowInvisiblesHook
	if ( WEGetUserInfo ( kInvisiblesOldDrawTextProcTag, ( SInt32 * ) & oldDrawTextProc, we ) == noErr )
	{
		CallWEDrawTextProc ( pText, textLength, slop, styleRunPosition, we, oldDrawTextProc ) ;
	}

	if ( IsColorPort ( port ) )
	{
		SInt32 temp ;

		//	default invisibles color is light gray
		color . red = 0xAAAA ;
		color . green = 0xAAAA ;
		color . blue = 0xAAAA ;

		if ( WEGetUserInfo ( kInvisiblesColorRedGreenTag , & temp, we ) == noErr )
		{
			* ( SInt32 * ) & color = temp ;
			if ( WEGetUserInfo ( kInvisiblesColorBlueTag, & temp, we ) == noErr )
			{
				color . blue = temp ;
			}
		}
		RGBForeColor ( & color ) ;
	}

	//	retrieve CharToPixel hook
	if ( WEGetInfo ( weCharToPixelHook, & charToPixelProc, we ) != noErr )
	{
		return ;
	}

	for ( i = 0 ; i < textLength ; ++ i )
	{
		if ( pText [ i ] == ' ' )
		{
			width = CallWECharToPixelProc ( pText, textLength, slop, i, leftCaret,
				styleRunPosition, startPt . h, we, charToPixelProc ) ;
			if ( halfSpaceWidth == 0x80000000 )
			{
				halfSpaceWidth = CallWECharToPixelProc ( pText, textLength, slop, i + 1, leftCaret,
					styleRunPosition, startPt . h, we, charToPixelProc ) ;
				halfSpaceWidth = ( halfSpaceWidth - width ) >> 1 ;
			}
			dstRect . left = startPt . h + width + halfSpaceWidth ;
			dstRect . right = dstRect . left + 1 ;
			dstRect . top = startPt . v - 3 ;
			dstRect . bottom = startPt . v - 2 ;
			PaintRect ( & dstRect ) ;
		}
		else if ( pText [ i ] == '\t' )
		{
			width = CallWECharToPixelProc ( pText, textLength, slop, i, leftCaret,
				styleRunPosition, startPt . h, we, charToPixelProc ) ;
			dstRect . left = startPt . h + width ;
			dstRect . right = dstRect . left + 8 ;
			dstRect . top = startPt . v - 8 ;
			dstRect . bottom = startPt . v ;
			CopyBits ( & tabBitMap, & port -> portBits, & tabBitMap . bounds, & dstRect, srcCopy, nil ) ;
		}
	}

	if ( ( textLength > 0 ) &&
	     ( pText + textLength <= * WEGetText ( we ) + WEGetTextLength ( we ) ) &&
	     ( pText [ textLength - 1 ] == kEOL ) )
	{
		GetPen ( & startPt ) ;		// that's really the endpoint now!
		dstRect . left = startPt.h ;
		dstRect . right = dstRect.left + 8 ;
		dstRect . top = startPt.v - 8 ;
		dstRect . bottom = startPt.v ;
		CopyBits ( & parBitMap, & port -> portBits, & parBitMap . bounds, & dstRect, srcCopy, nil ) ;
	}
}
