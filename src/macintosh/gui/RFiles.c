/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file RFiles.c
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
 *
 *	WASTE Demo Project:
 *	Macintosh Controls with Long Values
 *
 *	Copyright © 1993-1998 Marco Piovanelli
 *	All Rights Reserved
 *
 *	C port by John C. Daub
 */


#ifndef __WEDEMOAPP__
#include "RIntf.h"
#endif


#ifndef __ERRORS__
#include <Errors.h>
#endif

#ifndef __FOLDERS__
#include <Folders.h>
#endif


extern	SInt16		Edit_Window;
extern 	WindowPtr	Edit_Windows[MAX_NUM_E_WIN + 1];
#define	kFileNotOpened	-1

typedef struct FlavorLookupTable
{
	FlavorType		flavorType ;
	SInt16			nextItemOnSuccess ;
	SInt16			nextItemOnFailure ;
} FlavorLookupTable ;

/*	the sFlavorLookupTable determines how the resource fork
	of a TEXT file is searched for formatting resources
*/
static const FlavorLookupTable sFlavorLookupTable [ ] =
{
	/* 0 */		kTypeCharFormat,	1,	2,
	/* 1 */		kTypeStyleScrap,	4,	2,
	/* 2 */		kTypeStyles,		3,	4,
	/* 3 */		kTypeFontTable,		4,	4,
	/* 4 */		kTypeParaFormat,	5,	6,
	/* 5 */		kTypeRulerScrap,	6,	6,
	/* 6 */		kTypeSoup,			7,	7,
	/* 7 */		0,					0,	0		/*	end of table */
} ;

/*	the sUnicodeFlavorLookupTable determines how the resource fork
	of a utxt file is searched for formatting resources
*/
static const FlavorLookupTable sUnicodeFlavorLookupTable [ ] =
{
	/* 0 */		kTypeCharFormat,	1,	4,
	/* 1 */		kTypeStyleScrap,	2,	4,
	/* 2 */		kTypeParaFormat,	3,	4,
	/* 3 */		kTypeRulerScrap,	4,	4,
	/* 4 */		0,					0,	0		/*	end of table */
} ;

int ChooseExistFile(char *fileBuf, int buflen);
int ChooseNewFile(char *fileBuf, int buflen);
int R_ChooseFile(int isNewFile, char *fileBuf, int buflen);
extern pascal	OSErr	FSpGetFullPath(const FSSpec *, short *,  Handle *);


extern RGBColor	                   tempTypeColour;

OSStatus ReadTextFile ( const FSSpec * pFileSpec, WEReference we )
{
	const int		kMaxFlavors = 7 ;
	SInt16			dataForkRefNum = kFileNotOpened ;
	SInt16			resForkRefNum = kFileNotOpened ;
	Handle			hText = nil ;
	ItemCount		flavorCount = 0 ;
	UInt32			index ;
	FlavorType		flavorTypes [ kMaxFlavors ] ;
	Handle			hFlavors [ kMaxFlavors ] ;
	Handle			hPageFormat = nil ;
	Handle			hPrintRecord = nil ;
	Handle			hPageMargins = nil ;
	Handle			hNewPageMargins = nil ;
	Size			textSize = 0 ;
	Size			textSizeRed = 0 ;
	Size			byteCount ;
	UInt16			possibleBOM ;
	OSStatus		err ;
    int 			i,k=0,j=0;
    SInt32   		*textPos;
    int 			startRange;
    char 			*testo;


	BlockZero ( hFlavors, sizeof ( hFlavors ) ) ;

	/*	open the data fork with read-only permission */
	if ( ( err = FSpOpenDF ( pFileSpec, fsRdPerm, & dataForkRefNum ) ) != noErr )
	{
		goto cleanup ;
	}

	/*	get data fork size */
	if ( ( err = GetEOF ( dataForkRefNum, & textSize ) ) != noErr )
	{
		goto cleanup ;
	}

	/*	set the position in the file from where to start reading */
	if ( ( err = SetFPos ( dataForkRefNum, fsFromStart, 0L ) ) != noErr )
	{
		goto cleanup ;
	}

	if ( ( textSize > 2 ) && ( ( textSize & 1 ) == 0 ) )
	{
		/*	read the first two bytes of the TEXT file
			if they are 0xFEFF or 0xFFFE, treat this file as a 'utxt' file
		*/
		byteCount = sizeof ( possibleBOM ) ;
		if ( ( err = FSRead ( dataForkRefNum, & byteCount, & possibleBOM ) ) != noErr )
		{
			goto cleanup ;
		}

 		if ( ( possibleBOM == 0xFEFF ) || ( possibleBOM == 0xFFFE ) )
 		{
			FSClose ( dataForkRefNum ) ;
			dataForkRefNum = kFileNotOpened ;
			err = ReadUnicodeTextFile ( pFileSpec, we ) ;
			goto cleanup ;
 		}
	}

	/*	reset the file marker to the very beginning of the file
	*/
	if ( ( err = SetFPos ( dataForkRefNum, fsFromStart, 0L ) ) != noErr )
	{
		goto cleanup ;
	}

	/*	try to allocate a handle that large, use temporary memory if available
	*/
	if ( ( err = NewHandleTemp ( textSize, & hText ) ) != noErr )
	{
		goto cleanup ;
	}

	/*	read in the text
	*/
	HLock ( hText ) ;
	byteCount = textSize ;
	err = FSRead ( dataForkRefNum, & byteCount, * hText ) ;
	HUnlock ( hText ) ;

	if ( err != noErr )
	{
		goto cleanup ;
	}

	/*	see if the file has a resource fork
		FSpOpenResFile will return -1 if it fails
	*/
	if ( ( resForkRefNum = FSpOpenResFile ( pFileSpec, fsRdPerm ) ) != kFileNotOpened )
	{
		/*	look for formatting flavors
		*/
		for ( index = 0 ; sFlavorLookupTable [ index ] . flavorType != 0 ; )
		{
			flavorTypes [ flavorCount ] = sFlavorLookupTable [ index ] . flavorType ;

			/*	look for the first resource of the given type (the ID doesn't matter)
			*/
			if ( ( hFlavors [ flavorCount ] = Get1IndResource ( flavorTypes [ flavorCount ], 1 ) ) != nil )
			{
				/*	found: detach this resource
				*/
				DetachResource ( hFlavors [ flavorCount ] ) ;
				flavorCount ++ ;

				/*	determine the next flavor we're supposed to look for
				*/
				index = sFlavorLookupTable [ index ] . nextItemOnSuccess ;
			}
			else
			{
				/*	not found
				*/
				index = sFlavorLookupTable [ index ] . nextItemOnFailure ;
			}
		}

#if TARGET_API_MAC_CARBON
		/*	look for a flattened page format
		*/
		if ( ( hPageFormat = Get1IndResource ( kTypePageFormat, 1 ) ) != nil )
		{
			DetachResource ( hPageFormat ) ;
		}
#else
		/*	look for a print record
		*/
		if ( ( hPrintRecord = Get1IndResource ( kTypePrintRecord, 1 ) ) != nil )
		{
			DetachResource ( hPrintRecord ) ;
		}
#endif

		/*	look for a page margin record
		*/
		if ( ( hNewPageMargins = Get1IndResource ( kTypePageMargins, 1 ) ) != nil )
		{
			DetachResource ( hNewPageMargins ) ;
		}
	}

	/*	insert the text into the WE record
*/

/* From here we start removing esc codes 0x5F 0x08 from the text.
   This codes takes into account the position of text to be
   outlined. This second part is not really good, can be improved.
  Jago Dic 2000 (Stefano M. Iacus)
*/
    testo = (char *) malloc(textSize+1);
    textSizeRed = textSize;
    textPos = (SInt32  *) malloc(textSize+1);

    startRange = FALSE;

    for(i=0;i<textSize;i++)
    {
     if(  ( (*hText)[i]==0x5F ) || ( (*hText)[i]==0x08 ) )
      {
       textSizeRed--;
       if( (*hText)[i]==0x08 && !startRange)
        {
         startRange = TRUE;
         textPos[j] = k;
         j++;
        }
      }
     else
      {
        if( (*hText)[i] == 0x0A )    /* strips also cr escape char  */
          testo[k]='\r';
        else
          testo[k]=(*hText)[i];
        k++;
      }

      if( ((*hText)[i]==':' ) && startRange)
       {
         startRange = FALSE;
         textPos[j] = k;
         j++;
       }
    }
    testo[k]='\0';

    strcpy(*hText, testo);
    textSize = textSizeRed;
    free(testo);

/* The text is now ready to be printed
 Jago Dic 2000 (Stefano M. Iacus)
*/
	HLock ( hText ) ;

	err = WEPut ( 0, 0, * hText, textSize, kTextEncodingMultiRun, 0, flavorCount, flavorTypes, hFlavors, we ) ;

	HUnlock(hText);

/* The text is now outlined
 Jago Dic 2000 (Stefano M. Iacus)
*/
  for(i=0;i<j;i=i+2)
   Change_Color_Range(textPos[i],  textPos[i+1], tempTypeColour.red, tempTypeColour.green, tempTypeColour.blue,we);


	free(textPos);






	if ( err != noErr )
	{
		goto cleanup ;
	}

	/*	set the insertion point at the beginning of the text */
	WESetSelection ( 0, 0, we ) ;

	/*	reset the WE instance modification count
	*/
	WEResetModCount ( we ) ;

	/*	put the page format / print record where we can find it later */

	if ( hPageFormat != nil )
	{
		if ( ( err = WESetUserInfo ( kPageFormatTag, ( SInt32 ) hPageFormat, we ) ) != noErr )
		{
			goto cleanup ;
		}
		hPageFormat = nil ;
	}


	/*	remember page margins
	*/
	if ( hNewPageMargins != nil )
	{
		if ( ( WEGetUserInfo ( kPageMarginsTag, ( SInt32 * ) & hPageMargins, we ) == noErr ) && ( hPageMargins != nil ) )
		{
			BlockMoveData ( * hNewPageMargins, * hPageMargins, sizeof ( PageMarginRec ) ) ;
		}
	}

cleanup :
	/*	dispose of temporary storage
	*/
	ForgetHandle ( & hText ) ;
	ForgetHandle ( & hPageFormat ) ;
	ForgetHandle ( & hPrintRecord ) ;
	ForgetHandle ( & hNewPageMargins ) ;
	for ( index = 0 ; index < flavorCount ; index ++ )
	{
		ForgetHandle ( & hFlavors [ index ] ) ;
	}

	if ( dataForkRefNum != kFileNotOpened )
	{
		FSClose ( dataForkRefNum ) ;
		dataForkRefNum = kFileNotOpened ;
	}

	if ( resForkRefNum != kFileNotOpened )
	{
		CloseResFile ( resForkRefNum ) ;
		resForkRefNum = kFileNotOpened ;
	}

	/*	display an alert box if anything went wrong
	*/
	if ( (err != noErr)  && (err != -43))  /* we handle 'File not found' error elsewhere  */
	{
		ErrorAlert ( err ) ;
	}

    if(pFileSpec)
     SetWTitle ( FrontWindow(), &pFileSpec->name ) ;
   
   
	return err ;
}



OSStatus ReadUnicodeTextFile ( const FSSpec * pFileSpec, WEReference we )
{
	const int	kMaxFlavors = 4 ;
	SInt16		dataForkRefNum = kFileNotOpened ;
	SInt16		resForkRefNum = kFileNotOpened ;
	Handle		hUnicodeText = nil ;
	ItemCount	flavorCount = 0 ;
	UInt32		index ;
	FlavorType	flavorTypes [ kMaxFlavors ] ;
	Handle		hFlavors [ kMaxFlavors ] ;
	Size		fileSize ;
	OSStatus	err ;

	/*	open the data fork with read-only permission
	*/
	if ( ( err = FSpOpenDF ( pFileSpec, fsRdPerm, & dataForkRefNum ) ) != noErr )
	{
		goto cleanup ;
	}

	/*	get file size
	*/
	if ( ( err = GetEOF ( dataForkRefNum, & fileSize ) ) != noErr )
	{
		goto cleanup ;
	}

	/*	set the position in the file from where to start reading
	*/
	if ( ( err = SetFPos ( dataForkRefNum, fsFromStart, 0L ) ) != noErr )
	{
		goto cleanup ;
	}

	/*	try to allocate a handle that large, use temporary memory if available
	*/
	if ( ( err = NewHandleTemp ( fileSize, & hUnicodeText ) ) != noErr )
	{
		goto cleanup ;
	}

	/*	read in the Unicode text
	*/
	HLock ( hUnicodeText ) ;
	err = FSRead ( dataForkRefNum, & fileSize, * hUnicodeText ) ;
	HUnlock ( hUnicodeText ) ;

	if ( err != noErr )
	{
		goto cleanup ;
	}

	/*	see if the file has a resource fork
		FSpOpenResFile will return -1 if it fails
	*/
	if ( ( resForkRefNum = FSpOpenResFile ( pFileSpec, fsRdPerm ) ) != kFileNotOpened )
	{
		/*	look for formatting flavors
		*/
		for ( index = 0 ; sUnicodeFlavorLookupTable [ index ] . flavorType != 0 ; )
		{
			flavorTypes [ flavorCount ] = sUnicodeFlavorLookupTable [ index ] . flavorType ;

			/*	look for the first resource of the given type (the ID doesn't matter)
			*/
			if ( ( hFlavors [ flavorCount ] = Get1IndResource ( flavorTypes [ flavorCount ], 1 ) ) != nil )
			{
				/*	found: detach this resource
				*/
				DetachResource ( hFlavors [ flavorCount ] ) ;
				flavorCount ++ ;

				/*	determine the next flavor we're supposed to look for
				*/
				index = sUnicodeFlavorLookupTable [ index ] . nextItemOnSuccess ;
			}
			else
			{
				/*	not found
				*/
				index = sUnicodeFlavorLookupTable [ index ] . nextItemOnFailure ;
			}
		}
	}

	/*	insert the text into the WE record
	*/
	HLock ( hUnicodeText ) ;
	err = WEPut ( 0, 0, * hUnicodeText, fileSize, kTextEncodingUnicodeDefault, wePutDetectUnicodeBOM, flavorCount, flavorTypes, hFlavors, we ) ;
	HUnlock ( hUnicodeText ) ;

	if ( err != noErr )
	{
		goto cleanup ;
	}

	/*	set the insertion point at the beginning of the text
	*/
	WESetSelection ( 0, 0, we ) ;

	/*	reset the WE instance modification count
	*/
	WEResetModCount ( we ) ;

cleanup :
	/*	dispose of temporary storage
	*/
	ForgetHandle ( & hUnicodeText ) ;

	for ( index = 0 ; index < flavorCount ; index ++ )
	{
		ForgetHandle ( & hFlavors [ index ] ) ;
	}

	if ( dataForkRefNum != kFileNotOpened )
	{
		FSClose ( dataForkRefNum ) ;
		dataForkRefNum = kFileNotOpened ;
	}

	if ( resForkRefNum != kFileNotOpened )
	{
		CloseResFile ( resForkRefNum ) ;
		resForkRefNum = kFileNotOpened ;
	}

	/*	display an alert box if anything went wrong
	*/
	if ( err != noErr )
	{
		ErrorAlert ( err ) ;
	}

	return err ;
}


OSStatus WriteTextFile ( const FSSpec * pFileSpec, WEReference we )
{
	static const ResType	formats [ ] =
	{
		kTypeCharFormat,
		kTypeStyleScrap,
		kTypeParaFormat,
		kTypeRulerScrap,
		kTypeSoup,
		kTypeStyles,		/*	for compatibility with apps based on TextEdit or WASTE 1.x */
		kTypeFontTable,		/*	for compatibility with apps based on WASTE 1.x */
		0					/*	end of table */
	} ;
	const FSSpec *			targetSpec = pFileSpec ;
	const ResType *			format ;
	FInfo					fileInfo ;
	Size					textSize ;
	Boolean					replacing = false ;
	SInt16					dataForkRefNum = kFileNotOpened ;
	SInt16					resForkRefNum = kFileNotOpened ;
	Handle					hText = nil ;
	Handle					hFormatting = nil ;
	Handle					hPageFormat = nil ;
	Handle					hPrintRecord = nil ;
	Handle					hPageMargins = nil ;
	UInt32					theTime ;
	Str255					tempFileName ;
	FSSpec					tempFileSpec ;
	SInt16					tempVRef ;		/* volume reference # for the temp file */
	SInt32					tempDirID ;		/* directory ID of the temp file */
	OSStatus				err ;

	/*	will we be replacing an existing file?
	*/
	err = FSpGetFInfo ( pFileSpec, & fileInfo ) ;
	if ( err == noErr )
	{
		replacing = true;
	}
	else if ( err != fnfErr )
	{
		goto cleanup;
	}

	if ( replacing )
	{
		/*	if the existing file is locked, we cannot save changes
		*/
		if ( ( err = FSpCheckObjectLock ( pFileSpec ) ) != noErr )
		{
			goto cleanup ;
		}

		/*	make up a temporary file name -- the name doesn't have
			to make sense, just be unique
		*/
		GetDateTime ( & theTime ) ;
		NumToString ( theTime, tempFileName ) ;

		/*	find the temporary items folder on the file's volume; create it if necessary
		*/
		if ( ( err = FindFolder ( pFileSpec -> vRefNum, kTemporaryFolderType, kCreateFolder, & tempVRef, & tempDirID ) ) != noErr )
		{
			goto cleanup ;
		}

		/*	make an FSSpec for the temp file
		*/
		err = FSMakeFSSpec ( tempVRef, tempDirID, tempFileName, & tempFileSpec ) ;
		if ( ( err != noErr) && ( err != fnfErr ) )
		{
			goto cleanup ;
		}
		targetSpec = & tempFileSpec ;
	}

	/*	create a new file.  if we're replacing, make a temp file.  if it's a
	  new file from the onset, just create the file
	*/
	FSpCreateResFile ( targetSpec, 'ttxt', kTypeText, smSystemScript ) ;
	if ( ( err = ResError ( ) ) != noErr )
	{
		goto cleanup;
	}

	/*	open the data fork for writing
	*/
	if ( ( err = FSpOpenDF ( targetSpec, fsRdWrPerm, & dataForkRefNum ) ) != noErr )
	{
		goto cleanup ;
	}

	/*	set the end-of-file
	*/
	if ( ( err = SetEOF ( dataForkRefNum, 0 ) ) != noErr )
	{
		goto cleanup;
	}

	/*	set the position in the file to write from
	*/
	if ( ( err = SetFPos ( dataForkRefNum, fsFromStart, 0 ) ) != noErr )
	{
		goto cleanup;
	}

	/*	get the text handle from the WE instance
		WEGetText returns the original handle, not a copy, so don't dispose of it!!
	*/
	hText = WEGetText ( we ) ;
	textSize = GetHandleSize ( hText ) ;

	/*	write the text
	*/
	HLock ( hText ) ;
	err = FSWrite ( dataForkRefNum, & textSize, * hText ) ;
	HUnlock ( hText ) ;

	if ( err != noErr )
	{
		goto cleanup;
	}

	/*	open the resource file for writing
	*/
	resForkRefNum = FSpOpenResFile ( targetSpec, fsRdWrPerm ) ;
	if ( ( err = ResError ( ) ) != noErr )
	{
		goto cleanup;
	}

	/*	write formatting resources
	*/
	for ( format = formats ; * format != 0 ; format ++ )
	{
		/*	allocate a temporary handle to hold the formatting
		*/
		if ( ( err = NewHandleTemp ( 0, & hFormatting ) ) != noErr )
		{
			goto cleanup ;
		}

		/*	create the formatting
		*/
		if ( ( err = WEStreamRange ( 0, 0x7FFFFFFF, * format, 0, hFormatting, we ) ) != noErr )
		{
			goto cleanup ;
		}

		/*	make hFormatting a resource handle
		*/
		AddResource ( hFormatting, * format, 128, "\p" ) ;
		if ( ( err = ResError ( ) ) != noErr )
		{
			goto cleanup ;
		}

		/*	mark it as changed and write it to the resource file
		*/
		ChangedResource ( hFormatting ) ;
		WriteResource ( hFormatting ) ;
		if ( ( err = ResError ( ) ) != noErr )
		{
			goto cleanup ;
		}

		/*	since hFormatting is now a resource handle, it will be automatically
			released when its resource file is closed
	    */
	}

#if TARGET_API_MAC_CARBON
	/*	write the page format, if any
	*/
	if ( ( WEGetUserInfo ( kPageFormatTag, ( SInt32 * ) & hPageFormat, we ) == noErr ) &&
		 ( hPageFormat != nil ) )
	{
		/*	make the flattened page format a resource handle
		*/
		AddResource ( hPageFormat, kTypePageFormat, 128, "\pPage format" ) ;
		if ( ( err = ResError ( ) ) != noErr )
		{
			goto cleanup ;
		}

		/*	mark it as changed and write it to the resource file
		*/
		ChangedResource ( hPageFormat ) ;
		WriteResource ( hPageFormat ) ;
		if ( ( err = ResError ( ) ) != noErr )
		{
			goto cleanup ;
		}

		/*	detach the handle from the resource file so it won't be disposed
			when the resource file is closed
		*/
		DetachResource ( hPageFormat ) ;
	}
#else
	/*	write the print record, if any
	*/
	if ( ( WEGetUserInfo ( kPrintRecordTag, ( SInt32 * ) & hPrintRecord, we ) == noErr ) && ( hPrintRecord != nil ) )
	{
		/*	make the print record a resource handle
		*/
		AddResource ( hPrintRecord, kTypePrintRecord, 128, "\pprint record" ) ;
		if ( ( err = ResError ( ) ) != noErr )
		{
			goto cleanup ;
		}

		/*	mark it as changed and write it to the resource file
		*/ChangedResource ( hPrintRecord ) ;
		WriteResource ( hPrintRecord ) ;

		/*	detach the handle from the resource file so it won't be disposed
			when the resource file is closed
		*/
		DetachResource ( hPrintRecord ) ;
	}
#endif

	/*	write the page margin record
	*/
	if ( ( WEGetUserInfo ( kPageMarginsTag, ( SInt32 * ) & hPageMargins, we ) == noErr ) && ( hPageMargins != nil ) )
	{
		/*	make the page margin record a resource handle
		*/
		AddResource ( hPageMargins, kTypePageMargins, 128, "\pprint margins" ) ;
		if ( ( err = ResError ( ) ) != noErr )
		{
			goto cleanup ;
		}

		/*	mark it as changed and write it to the resource file
		*/
		ChangedResource ( hPageMargins ) ;
		WriteResource ( hPageMargins ) ;

		/*	detach the handle from the resource file so it won't be disposed
			when the resource file is closed
		*/
		DetachResource ( hPageMargins ) ;
	}

	/*	"clean" this document by resetting the WE instance modification count
	*/
	WEResetModCount ( we ) ;

	err = noErr;

cleanup:
	/* display an alert box if anything went wrong
	*/
	if ( err != noErr )
	{
		ErrorAlert ( err ) ;
	}

	/*	close the data fork, if we opened it
	*/
	if ( dataForkRefNum != kFileNotOpened )
	{
		FSClose ( dataForkRefNum ) ;
		dataForkRefNum = kFileNotOpened ;
	}

	/*	close the resource fork, if we opened it
	*/
	if ( resForkRefNum != kFileNotOpened )
	{
		CloseResFile ( resForkRefNum ) ;
		resForkRefNum = kFileNotOpened ;
	}

	if ( replacing )
	{
		/*	since we were replacing an existing file, let's now swap the original
			and the temp file.  let's hear it for safe saves.
		*/
		if ( ( err = FSpExchangeFiles ( & tempFileSpec, pFileSpec ) ) != noErr )
		{
			/* handle the error */
			return err;
		}

		/*	can the temp file since we don't need it anymore
		*/
		if ( ( err = FSpDelete ( & tempFileSpec ) ) != noErr )
		{
			return err;
		}
	}

	/* and update the disk with any unwritten data
	*/
	FlushVol ( nil, pFileSpec->vRefNum ) ;

	return err;
}




int ChooseExistFile(char *fileBuf, int buflen){
    StandardFileReply        fileReply;
    SFTypeList               fileTypes;
    Handle                   fileName=NULL;
    SInt16                   fileLen;

    fileTypes[0] = 'TEXT';

return(0);

//    StandardGetFile(nil,1,fileTypes,&fileReply);
    if (fileReply.sfGood){
       FSpGetFullPath (&fileReply.sfFile, &fileLen, &fileName);
       HLock((Handle) fileName);
       if (fileLen < buflen){
          strncpy(fileBuf, *fileName, fileLen);
          RnWrite( &fileBuf[fileLen-1], 1);
          fileBuf[fileLen] = '\0';
          HUnlock((Handle) fileName);
          return fileLen ;
       }else{
          strncpy(fileBuf, *fileName, buflen - 1);
          fileBuf[buflen-1] = '\0';
          HUnlock((Handle) fileName);
          return buflen-1;
       }
    }else{
        fileBuf[0] = '\0';
        return 0;
    }
}


int ChooseNewFile(char *fileBuf, int buflen){
   StandardFileReply	fileReply;
   Handle                   fileName=NULL;
   SInt16                   fileLen;
   OSErr                    osError;

return(noErr);
//   StandardPutFile("\pNew File","\pUntitled",&fileReply);
   if(fileReply.sfGood){
       osError = FSpCreate(&fileReply.sfFile, nil,'TEXT',smSystemScript);

       FSpGetFullPath (&fileReply.sfFile, &fileLen, &fileName);
       HLock((Handle) fileName);
       if (fileLen  < buflen){
          strncpy(fileBuf, *fileName, fileLen);
          /* RnWrite( &fileBuf[fileLen-1], 1);
          */
          fileBuf[fileLen] = '\0';
          HUnlock((Handle) fileName);
          osError = FSpDelete(&fileReply.sfFile);
          return fileLen ;
       }else{
          strncpy(fileBuf, *fileName, buflen - 1);
          fileBuf[buflen-1] = '\0';
          HUnlock((Handle) fileName);
          osError = FSpDelete(&fileReply.sfFile);
          return buflen-1;
       }

   }else{

      fileBuf[0] = '\0';
      return 0;
   }
}

int R_ChooseFile(int isNewFile, char *fileBuf, int buflen){
   if (isNewFile){
      return ChooseNewFile(fileBuf, buflen);
   }else{
      return ChooseExistFile(fileBuf, buflen);
   }
}
