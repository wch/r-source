/*
 *	WEExtras.h
 *
 *	Routines for installing and removing various "extras"
 *
 *	Written by Jonathan Kew
 *
 */

#ifndef _WASTE_
#include "WASTE.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

pascal OSErr WEInstallCrOnlyHook( WEReference we );
pascal OSErr WERemoveCrOnlyHook( WEReference we );
pascal Boolean WEIsCrOnly( WEReference we );

pascal OSErr WEInstallShowInvisiblesHook(WEReference we);
pascal OSErr WERemoveShowInvisiblesHook( WEReference we );
pascal OSErr WESetInvisiblesColor( const RGBColor *color, WEReference we );
pascal OSErr WEGetInvisiblesColor( RGBColor *color, WEReference we );
pascal Boolean WEIsShowInvisibles( WEReference we );

#ifdef __cplusplus
}
#endif
