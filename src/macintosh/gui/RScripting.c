/*
 *  R : A Computer Language for Statistical Data Analysis
 *  File RScripting.c
 *  Copyright (C) 1998-1999  Ross Ihaka
 *                2000-2001  Stefano M. Iacus the R core team
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
 *
 *
 *  This file is adapted from the public demos coming with the Waste library
 *  distribution:  WASTE Text Engine © 1993-2000 Marco Piovanelli.
 *   
 *  This file was originally written by: Wing Kwong (Tiki), WAN 3/2/99
 *  Updated to last version of WasteLib library: Stefano M. Iacus, 2001
 *
 *  Original file was:
 *
 *	WASTE Demo Project:
	Macintosh Controls with Long Values

	Copyright © 1993-1998 Marco Piovanelli
	All Rights Reserved

	C port by John C. Daub
*/

#include <RCarbon.h>

#include <AppleEvents.h>


#ifndef __AEOBJECTS__
#include <AEObjects.h>
#endif

#ifndef __AEREGISTRY__
#include <AERegistry.h>
#endif

#ifndef __WEDEMOAPP__
#include "RIntf.h"
#endif

enum
{
	kMaxPropLevel	=	2
} ;

typedef DescType PropArray[kMaxPropLevel];

void InitDesc ( AEDesc * desc )
{
	desc->descriptorType = typeNull;
	desc->dataHandle = nil;
}


OSStatus GetAEDescDataAsHandle ( const AEDesc * inDesc, Handle * outData )
{
	Size		dataSize ;
	Handle		dataHandle =NULL;
	OSStatus	err ;

	* outData = nil ;

	//	do nothing if inDesc is a null descriptor
	if ( ( inDesc->descriptorType == typeNull ) || ( inDesc->dataHandle == nil ) )
	{
		return noErr ;
	}

	//	get data size
	dataSize = AEGetDescDataSize ( inDesc ) ;

	//	allocate a handle this size
	dataHandle = NewHandle ( dataSize ) ;
	if ( ( err = MemError ( ) ) != noErr )
	{
		goto cleanup ;
	}

	//	fill the handle
	HLock ( dataHandle ) ;
	err = AEGetDescData ( inDesc, *dataHandle, dataSize ) ;
	HUnlock ( dataHandle ) ;
	* outData = dataHandle ;

cleanup :
	if ( err != noErr )
	{
		ForgetHandle ( outData ) ;
	}

	//	return error code
	return err ;
}

static Boolean PropertyOf ( const AERecord * spec, SInt16 * propLevel, PropArray properties )
{
	AERecord objSpec;
	AEKeyword key;
	DescType theType;
	DescType actualType;
	Size actualSize;
	Boolean retVal = false;

	InitDesc(&objSpec);

	// if spec is an Apple event (*propLevel = 0), extract its direct parameter
	// otherwise spec is an object specifier record: extract its container param
	key = (*propLevel == 0) ? keyDirectObject : keyAEContainer;

	// extract object specifier
	if (AEGetParamDesc(spec, key, typeAERecord, &objSpec) != noErr)
		goto cleanup;

	// does this object specifier specify a property?
	if (AEGetParamPtr(&objSpec, keyAEDesiredClass, typeType, &actualType,
					&theType, sizeof(theType), &actualSize) != noErr)
		goto cleanup;

	// sanity check: make sure the key form is formPropertyID
	// this is probably redundant, but checking doesn't hurt
	if (AEGetParamPtr(&objSpec, keyAEKeyForm, typeEnumerated, &actualType,
					&theType, sizeof(theType), &actualSize) != noErr)
		goto cleanup;
	if (theType != formPropertyID)
		goto cleanup;

	// which property does this object specifier specify?
	if (AEGetParamPtr(&objSpec, keyAEKeyData, typeType, &actualType,
					&theType, sizeof(theType), &actualSize) != noErr)
		goto cleanup;

	// bump property level and save property tag into property array
	properties[(*propLevel)++] = theType;

	// property of what?
	if (AESizeOfParam(&objSpec, keyAEContainer, &actualType, &actualSize) == noErr)
		if (actualType == typeNull)
			// property of application (i.e., null container): we are done
			retVal = true;
		else if ((actualType == typeObjectSpecifier) && (*propLevel < kMaxPropLevel))
			// property of another object, so do a recursive call
			// unless we have already reached max recursion depth
			retVal = PropertyOf(&objSpec, propLevel, properties);

cleanup:
	AEDisposeDesc(&objSpec);
	return retVal;
}




OSStatus GetContentsOfSelection ( DescType requestedType, AEDesc * desc, WEReference we )
{
	Handle		dataHandle = nil ;
	OSStatus	err;

	//	allocate a handle to hold the contents data
	dataHandle = NewHandle(0);
	if ((err = MemError()) != noErr)
	{
		goto cleanup;
	}

	//	get the text for the specified range
	if ((err = WEStreamRange(kCurrentSelection, kCurrentSelection, requestedType, 0, dataHandle, we )) != noErr)
	{
		goto cleanup ;
	}

	//	create the AE descriptor
	HLock(dataHandle);
	err = AECreateDesc(requestedType, *dataHandle, GetHandleSize(dataHandle), desc);
	HUnlock(dataHandle);
	if (err != noErr)
	{
		goto cleanup ;
	}

	//	clear result code
	err = noErr;

cleanup:
	ForgetHandle(&dataHandle);

	//	return result code
	return err;
}

OSStatus SetContentsOfSelection ( const AEDesc * desc, WEReference we )
{
	Handle		textHandle = nil ;
	Handle		stylesHandle = nil ;
	AEDesc		textDesc ;
	AEDesc		stylesDesc ;
	AEDesc		recordDesc ;
	OSStatus	err ;

	InitDesc(&textDesc);
	InitDesc(&stylesDesc);
	InitDesc(&recordDesc);

	// we expect desc type to be either TEXT or STXT
	if (desc->descriptorType == typeStyledText)
	{
		// STYLED TEXT
		// coerce the styled text descriptor to an Apple event record
		if ((err = AECoerceDesc(desc, typeAERecord, &recordDesc)) != noErr)
			goto cleanup;

		// extract text + styles from the record
		if ((err = AEGetParamDesc(&recordDesc, keyAEText, typeChar, &textDesc)) != noErr)
			goto cleanup;
		if ((err = AEGetParamDesc(&recordDesc, keyAEStyles, typeScrapStyles, &stylesDesc)) != noErr)
			goto cleanup;
	}
	else
	{
		// UNSTYLED TEXT
		if ((err = AECoerceDesc(desc, typeChar, &textDesc)) != noErr)
			goto cleanup;
	}

	//	get data handles from descs
	if ((err = GetAEDescDataAsHandle(&textDesc, &textHandle)) != noErr)
	{
		goto cleanup ;
	}
	if ((err = GetAEDescDataAsHandle(&stylesDesc, &stylesHandle)) != noErr)
	{
		goto cleanup;
	}

	// replace the specified range with the given text
	HLock(textHandle);
	err = WEInsert(*textHandle, GetHandleSize(textHandle), (StScrpHandle) stylesHandle, nil, we);
	HUnlock(textHandle);

	// clear result code
	err = noErr;

cleanup:
	AEDisposeDesc(&recordDesc);
	AEDisposeDesc(&textDesc);
	AEDisposeDesc(&stylesDesc);
	ForgetHandle(&textHandle);
	ForgetHandle(&stylesHandle);

	return err;
}

static pascal OSErr HandleGetData(const AppleEvent *ae, AppleEvent *reply, UInt32 refCon)
{
#pragma unused (refCon)

	AEDesc				textDesc;
	DocumentHandle		hDocument;
	SInt16				propLevel = 0;
	PropArray			properties;
	FlavorType			requestedType;
	DescType			actualType;
	Size				actualSize;
	OSStatus			err;

	InitDesc(&textDesc);

	// the only Apple event object we recognize is "contents of selection"
	err = errAENoSuchObject;
	if (!PropertyOf(ae, &propLevel, properties) || (propLevel != 2) ||
		(properties[0] != pContents) || (properties[1] != pSelection))
	{
		goto cleanup;
	}

	// extract the optional parameter keyAERequestedType, if present
	// The Apple Event Registry says this parameter can be a list
	// of type tags, but in most cases it is just a single tag, as we assume here.
	if (AEGetParamPtr(ae, keyAERequestedType, typeType, &actualType,
		&requestedType, sizeof(requestedType), &actualSize) != noErr)
	{
		requestedType = kTypeText;		//	return raw TEXT by default
	}

	// make sure there is a document window in front
	err = errAENoUserSelection;
	if ((hDocument = GetWindowDocument(FrontWindow())) == nil)
	{
		goto cleanup;
	}

	// create an Apple event descriptor for the selected text
	if ((err = GetContentsOfSelection(requestedType, &textDesc, (*hDocument)->we)) != noErr)
	{
		goto cleanup;
	}

	// put the text descriptor into the reply event
	if ((err = AEPutParamDesc(reply, keyDirectObject, &textDesc)) != noErr)
	{
		goto cleanup;
	}

	// clear result code
	err = noErr;

cleanup:
	AEDisposeDesc(&textDesc);

	return err;
}

static pascal OSErr HandleSetData(const AppleEvent *ae, AppleEvent *reply, UInt32 refCon)
{
#pragma unused (reply, refCon)

	AEDesc				textDesc;
	DocumentHandle		hDocument;
	SInt16				propLevel = 0;
	PropArray			properties;
	OSStatus			err;

	InitDesc(&textDesc);

	// the only Apple event object we recognize is "contents of selection"
	err = errAENoSuchObject;
	if (!PropertyOf(ae, &propLevel, properties) || (propLevel != 2) ||
		(properties[0] != pContents) || (properties[1] != pSelection))
	{
		goto cleanup;
	}

	// make sure there is a document window in front
	err = errAENoUserSelection;
	if ((hDocument = GetWindowDocument(FrontWindow())) == nil)
	{
		goto cleanup;
	}

	// extract the required keyAEData parameter
	if ((err = AEGetParamDesc(ae, keyAEData, typeWildCard, &textDesc)) != noErr)
		goto cleanup;

	// set the contents of the selection
	if ((err = SetContentsOfSelection(&textDesc, (*hDocument)->we)) != noErr)
	{
		goto cleanup;
	}

	// clear result code
	err = noErr;

cleanup:
	AEDisposeDesc(&textDesc);

	return err;
}

 

OSStatus InstallCoreHandlers(void)
{
	OSStatus err;

	if ((err = AEInstallEventHandler(kAECoreSuite, kAEGetData,
			NewAEEventHandlerUPP(HandleGetData), 0L, false)) != noErr)
	{
		return err;
	}

	if ((err = AEInstallEventHandler(kAECoreSuite, kAESetData,
			NewAEEventHandlerUPP(HandleSetData), 0L, false)) != noErr)
	{
		return err;
	}

	return noErr;
}

OSStatus CreatePSNBasedAppleEvent
	(
		const ProcessSerialNumber *		inTargetPSN,
		AEEventClass					inEventClass,
		AEEventID						inEventID,
		AppleEvent *					outAE
	)
{
	AEAddressDesc	targetAddress ;
	OSStatus		err ;

	InitDesc ( outAE ) ;
	InitDesc ( & targetAddress ) ;

	//	create an address descriptor for the target application based on the PSN
	if ( ( err = AECreateDesc ( typeProcessSerialNumber, inTargetPSN,
		sizeof ( * inTargetPSN ), & targetAddress ) ) != noErr )
	{
		goto cleanup ;
	}

	//	create the Apple event
	if ( ( err = AECreateAppleEvent ( inEventClass, inEventID, & targetAddress,
		kAutoGenerateReturnID, kAnyTransactionID, outAE ) ) != noErr )
	{
		goto cleanup ;
	}

	//	clear result code
	err = noErr ;

cleanup :
	AEDisposeDesc ( & targetAddress ) ;

	//	return result code
	return err ;
}

OSStatus CreateObjectSpecifier
	(
		const AEDesc *		inContainerDesc,
		DescType			inDesiredClass,
		AEKeyword			inKeyForm,
		const AEDesc *		inKeyData,
		AEDesc *			outObjectSpecifier
	)
{
	AEDesc			recordDesc ;
	OSStatus		err ;

	InitDesc ( & recordDesc ) ;

	//	create a record
	if ( ( err = AECreateList ( nil, 0, true, & recordDesc ) ) != noErr )
	{
		goto cleanup ;
	}

	//	add container field
	if ( ( err = AEPutParamDesc ( & recordDesc, keyAEContainer, inContainerDesc ) ) != noErr )
	{
		goto cleanup ;
	}

	//	add desired class field
	if ( ( err = AEPutParamPtr ( & recordDesc, keyAEDesiredClass, typeType,
		& inDesiredClass, sizeof ( inDesiredClass ) ) ) != noErr )
	{
		goto cleanup ;
	}

	//	add key form field
	if ( ( err = AEPutParamPtr ( & recordDesc, keyAEKeyForm, typeEnumerated,
		& inKeyForm, sizeof ( inKeyForm ) ) ) != noErr )
	{
		goto cleanup ;
	}

	//	add key data field
	if ( ( err = AEPutParamDesc ( & recordDesc, keyAEKeyData, inKeyData ) ) != noErr )
	{
		goto cleanup ;
	}

	//	coerce the AE record to typeObjectSpecifier
	if ( ( err = AECoerceDesc ( & recordDesc, typeObjectSpecifier, outObjectSpecifier ) ) != noErr )
	{
		goto cleanup ;
	}

	//	clear result code
	err = noErr ;

cleanup :
	AEDisposeDesc ( & recordDesc ) ;

	//	return result code
	return err ;
}

OSStatus CreatePropertySpecifier
	(
		const AEDesc *		inContainerSpec,
		AEKeyword			inPropertyTag,
		AEDesc *			outPropertySpecifier
	)
{
	AEDesc			propertyDesc ;
	OSStatus		err ;

	InitDesc ( & propertyDesc ) ;

	//	create property descriptor
	if ( ( err = AECreateDesc ( typeType, & inPropertyTag, sizeof ( inPropertyTag ), & propertyDesc ) ) != noErr )
	{
		goto cleanup ;
	}

	//	create object specifier
	if ( ( err = CreateObjectSpecifier ( inContainerSpec, cProperty,
		formPropertyID, & propertyDesc, outPropertySpecifier ) ) != noErr )
	{
		goto cleanup ;
	}

	//	clear result code
	err = noErr ;

cleanup :
	AEDisposeDesc ( & propertyDesc ) ;

	//	return result code
	return err ;
}

OSStatus CreateFinderObjectSpecifier
	(
		const FSSpec *		inThing,
		AEDesc *			outThingSpec
	)
{
	Handle			alias = nil ;
	AEDesc			nullDesc ;
	AEDesc			aliasDesc ;
	OSStatus		err ;

	InitDesc ( outThingSpec ) ;
	InitDesc ( & nullDesc ) ;
	InitDesc ( & aliasDesc ) ;

	// make an alias for the given Finder object
	if ( ( err = NewAliasMinimal ( inThing, ( AliasHandle * ) & alias ) ) != noErr )
	{
		goto cleanup ;
	}

	//	create an alias descriptor
	HLock ( alias ) ;
	err = AECreateDesc ( typeAlias, * alias, GetHandleSize ( alias ), & aliasDesc ) ;
	HUnlock ( alias ) ;
	if ( err != noErr )
	{
		goto cleanup ;
	}

	//	make an object specifier for the thing
	if ( ( err = CreateObjectSpecifier ( & nullDesc, typeWildCard, formAlias, & aliasDesc, outThingSpec ) ) != noErr )
	{
		goto cleanup ;
	}

	// clear result code
	err = noErr ;

cleanup :
	ForgetHandle ( & alias ) ;
	AEDisposeDesc ( & aliasDesc ) ;

	return err ;
}

OSStatus SendGetDataEvent
	(
		const ProcessSerialNumber *		inTargetPSN,
		const AEDesc *					inObjectSpec,
		DescType						inRequestedType,
		AEDesc *						outResult
	)
{
	AppleEvent		ae ;		// the "get data" event
	AppleEvent		reply ;		// the reply event
	OSStatus		err ;

	InitDesc ( & ae ) ;
	InitDesc ( & reply ) ;

	//	create a "get data" Apple event for the specified process
	if ( ( err = CreatePSNBasedAppleEvent ( inTargetPSN, kAECoreSuite, kAEGetData, & ae ) ) != noErr )
	{
		goto cleanup ;
	}

	//	put the given specifier into the direct parameter of the event
	if ( ( err = AEPutParamDesc ( & ae, keyDirectObject, inObjectSpec ) ) != noErr )
	{
		goto cleanup ;
	}

	//	add the optional keyAERequestedType parameter to the event (unless we don't care)
	if ( inRequestedType != typeWildCard )
	{
		if ( ( err = AEPutParamPtr ( & ae, keyAERequestedType, typeType, & inRequestedType, sizeof ( inRequestedType ) ) ) != noErr )
		{
			goto cleanup ;
		}
	}

	//	send the event and wait for the reply
	if ( ( err = AESend ( & ae, & reply, kAEWaitReply + kAENeverInteract, kAENormalPriority, kAEDefaultTimeout, nil, nil ) ) != noErr )
	{
		goto cleanup ;
	}

	// extract direct parameter from the reply
	if ( ( err = AEGetParamDesc ( & reply, keyDirectObject, inRequestedType, outResult ) ) != noErr )
	{
		goto cleanup ;
	}

	// clear result code
	err = noErr ;

cleanup:
	AEDisposeDesc ( & ae ) ;
	AEDisposeDesc ( & reply ) ;

	return err ;
}


OSStatus GetFileRect
	(
		const FSSpec *	inThing,
		Rect *			outFileRect
	)
{
	ProcessSerialNumber		finderPSN ;			// the Finder's process serial number
	AEDesc					thingSpec ;			// specifier for inThing
	AEDesc					containerSpec ;		// specifier for "container of inThing"
	AEDesc					windowSpec ;		// specifier for "window of container of inThing"
	AEDesc					windowBoundsSpec ;	// specifier for "bounds of window of container of inThing"
	AEDesc					iconBoundsSpec ;	// specifier for "bounds of inThing"
	AEDesc					result ;
	Rect					windowBounds ;
	SInt16					deskVRefNum ;
	SInt32					deskDirID ;
	OSStatus				err ;

	InitDesc ( & thingSpec ) ;
	InitDesc ( & containerSpec ) ;
	InitDesc ( & windowSpec ) ;
	InitDesc ( & windowBoundsSpec ) ;
	InitDesc ( & iconBoundsSpec )  ;
	InitDesc ( & result ) ;

	//	get the Finder's process serial number
	if ( ( err = FindProcess ( FOUR_CHAR_CODE ( 'FNDR' ), FOUR_CHAR_CODE ( 'MACS' ), & finderPSN ) ) != noErr )
	{
	}

	//	make an object specifier for the given Finder object
	if ( ( err = CreateFinderObjectSpecifier ( inThing, & thingSpec ) ) != noErr )
	{
		goto cleanup ;
	}

	//	make an object specifier for "container of" object
	if ( ( err = CreatePropertySpecifier ( & thingSpec, pContainer, & containerSpec ) ) != noErr )
	{
		goto cleanup ;
	}

	//	make an object specifier for "window of container of" object
	if ( ( err = CreatePropertySpecifier ( & containerSpec, pWindow, & windowSpec ) ) != noErr )
	{
		goto cleanup ;
	}

	//	make an object specifier for "bounds of window of container of" object
	if ( ( err = CreatePropertySpecifier ( & windowSpec, pBounds, & windowBoundsSpec ) ) != noErr )
	{
		goto cleanup ;
	}

	//	get bounds of object's enclosing window, in global coordinates
	if ( ( err = SendGetDataEvent ( & finderPSN, & windowBoundsSpec, typeQDRectangle, & result ) ) != noErr )
	{
		goto cleanup ;
	}
	if ( ( err = AEGetDescData ( & result, & windowBounds, sizeof ( windowBounds ) ) ) != noErr )
	{
		goto cleanup ;
	}
	AEDisposeDesc ( & result ) ;

	//	make an object specifier for "bounds of" object
	if ( ( err = CreatePropertySpecifier ( & thingSpec, pBounds, & iconBoundsSpec ) ) != noErr )
	{
		goto cleanup ;
	}

	// get bounds of object, relative to the enclosing window
	if ( ( err = SendGetDataEvent ( & finderPSN, & iconBoundsSpec, typeQDRectangle, & result ) ) != noErr )
	{
		goto cleanup ;
	}
	if ( ( err = AEGetDescData ( & result, outFileRect, sizeof ( * outFileRect ) ) ) != noErr )
	{
		goto cleanup ;
	}
	AEDisposeDesc ( & result ) ;

	// calculate object's rectangle, in global coordinates
	OffsetRect ( outFileRect, windowBounds . left, windowBounds . top ) ;

	// SPECIAL CASE: check if the object is on the desktop
	if ( ( err = FindFolder ( inThing -> vRefNum, kDesktopFolderType, kDontCreateFolder, & deskVRefNum, & deskDirID ) ) != noErr )
	{
		goto cleanup ;
	}

	// if the object is on the desktop, the rectangle returned by the Finder
	// is 20 pixels below the actual icon
	if ( inThing -> parID == deskDirID )
	{
		OffsetRect ( outFileRect, 0, -20 ) ;
	}

	// clear result code
	err = noErr ;

cleanup :
	AEDisposeDesc ( & thingSpec ) ;
	AEDisposeDesc ( & containerSpec ) ;
	AEDisposeDesc ( & windowSpec ) ;
	AEDisposeDesc ( & windowBoundsSpec ) ;
	AEDisposeDesc ( & iconBoundsSpec ) ;
	AEDisposeDesc ( & result ) ;

	return err ;
}


