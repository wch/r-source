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
	Handle		dataHandle ;
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
			NewAEEventHandlerProc(HandleGetData), 0L, false)) != noErr)
	{
		return err;
	}

	if ((err = AEInstallEventHandler(kAECoreSuite, kAESetData,
			NewAEEventHandlerProc(HandleSetData), 0L, false)) != noErr)
	{
		return err;
	}

	return noErr;
}



