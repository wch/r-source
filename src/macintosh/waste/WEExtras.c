/*
 *	WEExtras.c
 *
 *	Routines for installing and removing various hooks
 *
 *	Written by Jonathan Kew
 *
 */

#include "WEExtras.h"

// prototypes for the hook routines defined in WEExtraHooks.c

#ifdef __cplusplus
extern "C" {
#endif

extern pascal void _WEShowInvisiblesDrawText(Ptr, long, Fixed, JustStyleCode, WEReference);
extern pascal StyledLineBreakCode _WENoWrapLineBreak(Ptr, long, long, long,
					Fixed *, long *, WEReference);

#ifdef __cplusplus
}
#endif

/* user tags for invisibles color*/
enum
{
	kInvisiblesOldDrawTextProcTag	=	'icDT' ,
	kInvisiblesColorRedGreenTag 	=	'icRG' ,
	kInvisiblesColorBlueTag			=	'icB_'
} ;

// static UPP's
static WELineBreakUPP		_weNoWrapLineBreakProc = nil;
static WEDrawTextUPP		_weShowInvisiblesDrawTextProc = nil;

pascal OSErr WEInstallCrOnlyHook(WEReference we)
{
	OSErr err;

	// if first time, create routine descriptor
	if (_weNoWrapLineBreakProc == nil)
	{
		_weNoWrapLineBreakProc = NewWELineBreakProc(_WENoWrapLineBreak);
	}

	err = WESetInfo( weLineBreakHook, &_weNoWrapLineBreakProc, we );

	return err;
}

pascal OSErr WERemoveCrOnlyHook( WEReference we )
{
	UniversalProcPtr hook = nil;
	OSErr err;

	err = WESetInfo( weLineBreakHook, &hook, we );

	return err;
}

pascal Boolean WEIsCrOnly( WEReference we )
{
	WELineBreakUPP hook = nil;

	// return true if our no-wrap hook is installed

	return 	( _weNoWrapLineBreakProc != nil ) &&
			( WEGetInfo( weLineBreakHook, &hook, we ) == noErr) &&
			( _weNoWrapLineBreakProc == hook );
}

pascal OSErr WEInstallShowInvisiblesHook ( WEReference we )
{
	WEDrawTextUPP drawTextProc = nil ;
	OSErr err;

	//	if first time, create routine descriptor
	if ( _weShowInvisiblesDrawTextProc == nil )
	{
		_weShowInvisiblesDrawTextProc = NewWEDrawTextProc ( _WEShowInvisiblesDrawText ) ;
	}

	//	get current text drawing hook
	if ( ( err = WEGetInfo ( weDrawTextHook, & drawTextProc, we ) ) != noErr )
	{
		return err ;
	}

	//	save it back in the same instance as user data
	if ( ( err = WESetUserInfo ( kInvisiblesOldDrawTextProcTag, ( SInt32 ) drawTextProc, we ) ) != noErr )
	{
		return err ;
	}

	//	replace the old text drawing hook with our show invisibles function
	return WESetInfo ( weDrawTextHook, & _weShowInvisiblesDrawTextProc, we ) ;
}

pascal OSErr WERemoveShowInvisiblesHook ( WEReference we )
{
	WEDrawTextUPP drawTextProc = nil ;
	OSErr err;

	//	retrieve the original text drawing hook
	if ( ( err = WEGetUserInfo ( kInvisiblesOldDrawTextProcTag, ( SInt32 * ) & drawTextProc, we ) ) != noErr )
	{
		return err ;
	}

	//	put it back in place
	return WESetInfo ( weDrawTextHook, & drawTextProc, we ) ;
}

pascal Boolean WEIsShowInvisibles( WEReference we )
{
	WEDrawTextUPP hook = nil;

	// return true if our no-wrap hook is installed

	return 	( _weShowInvisiblesDrawTextProc != nil ) &&
			( WEGetInfo( weDrawTextHook, &hook, we ) == noErr) &&
			( _weShowInvisiblesDrawTextProc == hook );
}

pascal OSErr WESetInvisiblesColor( const RGBColor *color, WEReference we )
{
	OSErr err ;

	if ( we == nil )
	{
		return nilHandleErr ;
	}

	if ( ( err = WESetUserInfo ( kInvisiblesColorRedGreenTag, * ( SInt32 * ) color, we ) ) != noErr )
	{
		return err ;
	}

	if ( ( err = WESetUserInfo ( kInvisiblesColorBlueTag, color -> blue, we ) ) != noErr )
	{
		return err ;
	}

	return noErr ;
}

pascal OSErr WEGetInvisiblesColor( RGBColor *color, WEReference we )
{
	SInt32 tmp ;
	OSErr err ;

	if ( we == nil )
	{
		return nilHandleErr ;
	}

	if ( ( err = WEGetUserInfo ( kInvisiblesColorRedGreenTag, ( SInt32 * ) color, we ) ) != noErr )
	{
		return err ;
	}

	if ( ( err = WEGetUserInfo ( kInvisiblesColorBlueTag, & tmp, we ) ) != noErr )
	{
		return err ;
	}

	color -> blue = tmp ;
	return noErr ;
}
