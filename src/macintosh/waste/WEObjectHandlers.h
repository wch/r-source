/*
 *	WASTE Demo Project:
 *	Sample WASTE Object Handlers
 *
 *	Copyright © 1993-2000 Marco Piovanelli
 *	All Rights Reserved
 *
 *	C port by John C. Daub
 *
 *	<mailto:waste@merzwaren.com>
 *	<http://www.merzwaren.com/waste/>
 */


#ifndef __WASTE__
#include "WASTE.h"
#endif


// PICTURES

pascal OSErr HandleNewPicture(Point *defaultObjectSize, WEObjectReference objectRef);
pascal OSErr HandleDisposePicture(WEObjectReference objectRef);
pascal OSErr HandleDrawPicture(const Rect *destRect, WEObjectReference objectRef);

// SOUNDS

enum
{
	kSoundIconID	=	550
};

pascal OSErr HandleNewSound(Point *defaultObjectSize, WEObjectReference objectRef);
pascal OSErr HandleDrawSound(const Rect *destRect, WEObjectReference objectRef);
pascal Boolean HandleClickSound(Point hitPt, EventModifiers modifiers,
					UInt32 clickTime, WEObjectReference objectRef);
