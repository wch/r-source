/*
 *  R : A Computer Language for Statistical Data Analysis
 *  File RPreference.c
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
 *	WASTE Demo Project:
 *	Preferences
 *
 *	Copyright © 1993-1998 Marco Piovanelli
 *	All Rights Reserved
 *
 *	C port by John C. Daub

	************************************************************************************************ 
	 
	RPreference
	by wing kwong (Tiki), WAN 3/2/99
	Temporarily removed preferences because using Environments.
	
	************************************************************************************************ 
	Description
	
    This file is used to control the Menu item, Preference. It includes  routine to load the 
    preference when the program start; routine to display the current preference when user click on
    the Menu; routine to save the preference.
    
    In here, the appearence of the Preference is design by the resource "DLOG" and "DITL". As long 
    as you didn't change the resource ID, you can feel free to change the appearence of the dialog 
    box without recomplies the codes	
	************************************************************************************************ 
*/

#include <RCarbon.h>

/* ************************************************************************************************ */
/*                                    INCLUDE HEADER FILE                                           */
/* ************************************************************************************************ */
#include "RIntf.h"
#include "WETabs.h"
#include "WETabHooks.h"
#include <ColorPicker.h>
#include <ATSUnicode.h>

/* ************************************************************************************************ */
/*                                      DEFINE CONSTANT                                             */
/* ************************************************************************************************ */
#define  rPreStringList      132
#define  rPrefsDialog        128
#define  itabSize            1  
#define  iHistorySize        4
#define  iPrefsFileName      1
#define  rTypeAppMiss        'STR '
#define  kAppMissID          -16397
#define  eTabSize            7
#define  eHistorySize        8
#define  eTextSize           13
#define  eR_NSize            14
#define  eR_VSize            15
#define  eScreenRes          16
#define  kWordSize           9

int		EIGHTY, F_HEIGHT;
#define FixedToInt(a)	((int)(a) / fixed1)

extern SInt32		systemVersion ;
extern Boolean      Have_Console;
/* ************************************************************************************************ */
/*                          Constant, Global variables and prototype                                */
/* ************************************************************************************************ */

/*
Here include all the Control in the Preference dialog, if you want to add more control, 
you needed to add the corresponding item in here.
*/
enum
{
   kActiveTextButton            = 9,
   kCompletedTextButton         = 2,
   kComputerResponseButton      = 3,
   kActiveTextField             = 4,
   kCompletedTextField          = 5,
   kComputerResponseField       = 6,
   kTabSize                     = 7,
   kHistoryLength               = 8,
   kConsTextSize                = 21,  // Jago
   kInterrupt                   = 24,
   kOnOpenSource                = 25,
   kOkButton                    = 1,       // 1 is default button.
   kCancelButton                = 10,
   kApplyButton					= 23,   
   kEditRect                    = 27,
   kConsoleFont					= 28,
   kGraphFont					= 27,
   kScreenRes                   = 19,
   kMemoryRect                  = 30   
   
} ;

/*
Here include all the content that the preference will store, if you wanted to add more.
You need to add the corresponding item in here.
*/

/* ************************************************************************************************ */
/*                                        Global variables                                          */
/* ************************************************************************************************ */ 
SInt16                                     gPrefsFileRefNum =0;
FSSpec                                     gHelpFile;
StandardFileReply                          gsfr;
RGBColor	         	         	       tempTypeColour, gTypeColour;
RGBColor              	         	       tempFinishedColour,gFinishedColour;
RGBColor             	         	       tempComputerColour,gComputerColour;
SInt16                                     gTextSize;
SInt16                                     storeHistory; 
int                                        PR_NSize;
int                                        PR_VSize;
int                                        gScreenRes;

// you can't change the Global variable HISTORY when you do change on Preference

/* ************************************************************************************************ */
/*                                Extern  Global variables                                          */
/* ************************************************************************************************ */ 
extern SInt16        	         	       gtabSize;
extern SInt16       	         	       gAppResFileRefNum ;
extern Ptr          	         	       *Cmd_Hist;
extern SInt16         	         	       HISTORY;
extern WindowPtr     	         	       Console_Window;
extern SInt16     	         	           Edit_Window;
extern WindowPtr      	         	       Edit_Windows[MAX_NUM_E_WIN + 1];
extern SInt16                              Help_Window;
extern WindowPtr                           Help_Windows[MAX_NUM_H_WIN + 1];
extern char    	         	               genvString[255];
extern int                                 R_NSize;  
extern int	                               R_VSize;
extern Str255							   MacSymbolFont;
extern Str255							   PostFont;
extern Str255							   UserFont;

void RPrefs(void);
int GetTextSize(void);
int GetScreenRes(void);


/* ************************************************************************************************ */
/*                                        Protocols                                                 */
/* ************************************************************************************************ */ 
unsigned char*                            StrToPStr(char*, SInt8);
void                                      doSavePreference(DialogPtr );
void                                      doGetPreferences(void);
void                                      savePreference(void);
void                                      DrawBox (DialogPtr PreferenceBox);
void                                      SetTab(Boolean);
pascal	OSErr	                          FSpGetFullPath (const FSSpec*, short*, Handle*);
void                                      pickColor(DialogPtr, SInt16 , RGBColor , RGBColor*);
void                                      SetTextSize(Boolean);
void                                      SetTextFontAndSize(void);
extern int                                R_SetOptionWidth(int w);

int EightyWidth(void);



typedef struct
{
   long						   prefsTypeVers;
   SInt16                      gtabSize;
   SInt16                      storeHistory;
   SInt16                      gTextSize;
   int                         gScreenRes;
   Boolean					   OnOpenSource;
   Boolean  				   Interrupt;
   Str255					   UserFont;
   Str255					   PostFont;
   RGBColor	                   TypeColour;
   RGBColor                    FinishedColour;
   RGBColor                    ComputerColour;
}  appPrefs, *appPrefsPointer, **appPrefsHandle;

void GetDialogPrefs(DialogPtr PreferenceBox, Boolean saveit);
void savePreference(void);
void setDefaultPrefs(void);


#define kPrefsFileType 'pref'
#define kPrefsCreatorType '????'
#define kFinderMessageStrID -16397 /* ID of STR for default finder message */
#define kPrefsResourceType 'Pref'
#define kPrefsResourceID 128
#define kPrefsNameStrID 132         /* name of prefs file */
#define kStrType 'STR '


void GetOrGeneratePrefs(appPrefs * thePrefsTypePtr, long minVers);
short OpenPrefsResFile(SignedByte prefsPerm, Boolean createFlag);
OSErr SavePrefs(appPrefs * thePrefsTypePtr);



/* ************************************************************************************************
StrToPStr: Transfer a string to Pascal String with size 'size'
************************************************************************************************ */
Str255 returnString;
unsigned char* StrToPStr(char* buf, SInt8 size){
    // Str255 returnString;
    returnString[0] = size;
    BlockMoveData(buf,returnString + 1,(SInt32) size);
    return returnString;
}


extern char *mac_getenv(const char *name);
Boolean Interrupt =false;
Boolean OnOpenSource = false;


void setDefaultPrefs(void)
{
     gTextSize = 12;
	 gScreenRes = 72;
	 gtabSize = 5;
	 storeHistory = 512;
	 
	 Interrupt = false;
     OnOpenSource = false;
	 
     gTypeColour.red =  0xffff;
     gTypeColour.green =  0x0000;
     gTypeColour.blue =  0x0000;
     gFinishedColour.red = 0x0000;
     gFinishedColour.green = 0x0000;
     gFinishedColour.blue = 0xffff;
     gComputerColour.red = 0x0000;
     gComputerColour.green = 0x0000;
     gComputerColour.blue = 0x0000;
	 
	 doCopyPString("\pmonaco", UserFont);
	 doCopyPString("\phelvetica", PostFont); 

}



void GetOrGeneratePrefs(appPrefs * thePrefsTypePtr, long minVers)
{
	short prefsResRefNum;
	Handle tempHandle;
	
	/* initialize prefs structure in case we can't get a valid set */

    
	
	 thePrefsTypePtr->prefsTypeVers = 0L;
	 thePrefsTypePtr->gTextSize = gTextSize;
	 thePrefsTypePtr->gScreenRes = gScreenRes;
	 thePrefsTypePtr->gtabSize = gtabSize;
	 thePrefsTypePtr->storeHistory = storeHistory;
	 
	 thePrefsTypePtr->Interrupt = Interrupt;
     thePrefsTypePtr->OnOpenSource = OnOpenSource;
	 
     thePrefsTypePtr->TypeColour = gTypeColour;
	 thePrefsTypePtr->FinishedColour = gFinishedColour;
	 thePrefsTypePtr->ComputerColour = gComputerColour;
	 
	 
	 doCopyPString(UserFont, thePrefsTypePtr->UserFont);
	 doCopyPString(PostFont, thePrefsTypePtr->PostFont); 

	
	/* open (but don't create) the prefs file */
	prefsResRefNum = OpenPrefsResFile(fsRdPerm, false);
	if (prefsResRefNum != -1) {
	
		/* file opened successfully, get the prefs resource */
		tempHandle = Get1Resource(kPrefsResourceType, kPrefsResourceID);
		
		/* if the resource is there and it's the right size and version, copy it */
		/* (in C these can be combined with &&, but don't use AND in Pascal)     */
		
		if (tempHandle != nil)
			if (GetHandleSize(tempHandle) == sizeof(appPrefs)) 
			//	if ((*(appPrefs *)*tempHandle).prefsTypeVers == minVers) {
				
					/* copy the prefs struct */
					*thePrefsTypePtr = *(appPrefs *)*tempHandle;
					
					
		//		}
		/* release the pref resource and close the file */
		CloseResFile(prefsResRefNum);
	}
}


/* Only resolutions 72, 144, 300 and 600 are allowable */
int GetScreenRes(void)
{
   appPrefs StartupPrefs;

  GetOrGeneratePrefs(&StartupPrefs, 0L);

   gScreenRes = StartupPrefs.gScreenRes;

  return(gScreenRes); 
}




int GetTextSize(void)
{
   appPrefs StartupPrefs;

   GetOrGeneratePrefs(&StartupPrefs, 0L);

   gTextSize = StartupPrefs.gTextSize;

   return(gTextSize);
}


void  doGetPreferences(void)
{
   Str255               prefsFileName;
   OSErr                osError;
   SInt16               volRefNum;
   long                 directoryID;
   FSSpec               fileSSpec;
   SInt16               fileRefNum;
   appPrefsHandle       appPrefsHdl;
   char 				userfont[25];
   FMFontFamily 		postFontId;
   appPrefs StartupPrefs;
   short prefsResRefNum;

   setDefaultPrefs();
   
   GetOrGeneratePrefs(&StartupPrefs, 0L);

   strcpy(genvString, ".Renviron");
      
   SetTab(false);
    
   storeHistory = StartupPrefs.storeHistory;
	  
	  if(storeHistory < 1 || storeHistory > 512)  
       storeHistory = 512;
      HISTORY = storeHistory +1;
      Cmd_Hist = malloc(HISTORY * sizeof(Ptr));
  
         
      gTextSize = StartupPrefs.gTextSize; 
	//  if(gTextSize < 8 || gTextSize > 14)  
  	//   gTextSize = 12;

	  
      gScreenRes = StartupPrefs.gScreenRes;	  
	  if( gScreenRes < 72 || gScreenRes >600)
	   gScreenRes = 72;

	  gtabSize = StartupPrefs.gtabSize;
	  if(gtabSize < 1 || gtabSize > 99)
       gtabSize = 5;
   
	   
      tempTypeColour= gTypeColour = StartupPrefs.TypeColour;
      tempFinishedColour=gFinishedColour = StartupPrefs.FinishedColour;
      tempComputerColour=gComputerColour = StartupPrefs.ComputerColour;

       doCopyPString("\psymbol", MacSymbolFont);  /* Jago */


       PStringCopy(StartupPrefs.PostFont,PostFont);
       PStringCopy(StartupPrefs.UserFont,UserFont);


	  if(GetFontIDFromMacFontName(PostFont) == kATSUInvalidFontID)
         doCopyPString("\phelvetica", PostFont); /* Emergency font ! */

  
      if(systemVersion > kMinSystemVersion){
      if( FMGetFontFamilyFromName(UserFont) == kInvalidFontFamily)
         doCopyPString("\pmonaco", UserFont); /* Emergency font ! */
      }
      else {
       GetFNum(UserFont, &postFontId);
       if( postFontId == kInvalidFontFamily)
          doCopyPString("\pmonaco", UserFont); /* Emergency font ! */
      }

      EIGHTY = EightyWidth();
 
//    SetTextFontAndSize();

	  OnOpenSource = StartupPrefs.OnOpenSource; 
      Interrupt = StartupPrefs.Interrupt; 


}

/* ************************************************************************************************
doGetPreferences :	This function will be called at the beginning when R application starts.
					It sets up some parameters specified by the user in the .Renviron file
					such as the graphic device screen resolution and font, the Console font and
					size and the tab-size of all the text windows. Etc.
Jago, April 2001, Stefano M. Iacus	
************************************************************************************************ */
void  doGetPreferences1(void)
{
   Str255               prefsFileName;
   OSErr                osError;
   SInt16               volRefNum;
   long                 directoryID;
   FSSpec               fileSSpec;
   SInt16               fileRefNum;
//   appPrefsHandle       appPrefsHdl;
   char 				userfont[25];
   FMFontFamily 		postFontId;
     
      strcpy(genvString, ".Renviron");
      
      SetTab(false);
    
      storeHistory = atoi(mac_getenv("RHISTSIZE"));
       
       
      if(storeHistory < 1 || storeHistory > 512)  
       storeHistory = 512;
      HISTORY = storeHistory +1;
      Cmd_Hist = malloc(HISTORY * sizeof(Ptr));
  
         
      gTextSize = GetTextSize();

      gScreenRes = GetScreenRes();
      
      doCopyPString("\psymbol", MacSymbolFont);  /* Jago */

      strncpy(userfont,mac_getenv("GraphFontName"),20);
 
      if(strlen(userfont)>0)
       CopyCStringToPascal(userfont,PostFont);
 
	  if(GetFontIDFromMacFontName(PostFont) == kATSUInvalidFontID)
         doCopyPString("\phelvetica", PostFont); /* Emergency font ! */

   	  strncpy(userfont,mac_getenv("FontName"),20);
      
      if(strlen(userfont)>0)
       CopyCStringToPascal(userfont,UserFont);
  
      if(systemVersion > kMinSystemVersion){
      if( FMGetFontFamilyFromName(UserFont) == kInvalidFontFamily)
         doCopyPString("\pmonaco", UserFont); /* Emergency font ! */
      }
      else {
       GetFNum(UserFont, &postFontId);
       if( postFontId == kInvalidFontFamily)
          doCopyPString("\pmonaco", UserFont); /* Emergency font ! */
      }

      EIGHTY = EightyWidth();
      

      
      tempTypeColour.red = gTypeColour.red =  0xffff;
      tempTypeColour.green = gTypeColour.green =  0x0000;
      tempTypeColour.blue = gTypeColour.blue =  0x0000;
      tempFinishedColour.red = gFinishedColour.red = 0x0000;
      tempFinishedColour.green = gFinishedColour.green = 0x0000;
      tempFinishedColour.blue = gFinishedColour.blue = 0xffff;
 
      tempComputerColour.red = gComputerColour.red = 0x0000;
      tempComputerColour.green = gComputerColour.green = 0x0000;
      tempComputerColour.blue = gComputerColour.blue = 0x0000;
      
      if(strcmp(mac_getenv("Interrupt"),"TRUE")==0)
       Interrupt = true;
      else  
       Interrupt = false;
       
       
      if(strcmp(mac_getenv("OnOpenSource"),"TRUE")==0)
       OnOpenSource = true;
      else  
       OnOpenSource = false;
  
}

int EightyWidth(void)
{
	CGrafPtr 		tempPort=NULL;
	char 			userfont[26];
	char			eighty[81];
	Str255			ottanta;
	int 			eightywidth = 50*12,i;
	FMFontFamily	fontFamily = 0 ;
    FMetricRec 		myFMetric;

    F_HEIGHT = gTextSize+2;
    
    tempPort = CreateNewPort();
	
	if(tempPort == NULL)
	 return(eightywidth);
	  
    if(systemVersion > kMinSystemVersion)	  
    fontFamily = FMGetFontFamilyFromName(UserFont);
    else
     GetFNum(UserFont,&fontFamily);
  
    if(fontFamily == kInvalidFontFamily)
     return(eightywidth);
     
    TextFont(fontFamily);
    TextSize(gTextSize);
    
	for(i=0;i<80;i++)
	 eighty[i]=0x52; /* R ! */
	eighty[80]='\0';
	
	CopyCStringToPascal(eighty,ottanta);
	eightywidth = StringWidth(ottanta);
	
	FontMetrics(&myFMetric);

    F_HEIGHT = FixedToInt(myFMetric.ascent + myFMetric.descent + myFMetric.leading);
	
	if(tempPort)
	 DisposePort(tempPort);
	
	return(eightywidth);
}


/*
************************************************************************************************
pickColor routine :
************************************************************************************************
Desciption :
This procedure is used to redraw (Update) the update the color of the text that you selected.
The purpose of this routine is to give the end user a impression of the color that they picked.
************************************************************************************************
*/
void pickColor(DialogPtr PreferenceBox, SInt16 itemID, RGBColor inColour, RGBColor *outColor){
   RGBColor	          blackColour;
   short              type; 
   Rect               itemRect;
   Handle             itemHandle=NULL;
   Str255		     prompt = "\pChoose a rectangle colour:";
   Point			 where ={0,0};
   GrafPtr           savePort;
   long              SaveColor;
   Boolean		okButton;
   
   blackColour.red	 = 0x0000;
   blackColour.green = 0x0000;
   blackColour.blue	 = 0x0000;
   GetDialogItem(PreferenceBox,  itemID, &type, &itemHandle, &itemRect);
   okButton = GetColor(where,prompt,&inColour,outColor);
   if (okButton){
      RGBForeColor(outColor);
      SetDialogItemText(itemHandle, "\pText Color"); 
      RGBForeColor(&blackColour);
   }else{
      *outColor = inColour;
   }

}


/*
************************************************************************************************
DrawBox routine :
************************************************************************************************
Desciption :
This procedure is used to redraw (Update) the preference dialog box.
When you open the dialog box, you may also open dialog like color picker, the preference dialog
box didn't know how to recover the content, thus, we need to call this procedure and update the
bitmap by ourself.
************************************************************************************************
*/
void DrawBox(DialogPtr PreferenceBox){
   short              type; 
   Handle             itemHandle=NULL;
   Rect               itemRect;
   Pattern	          myPat;
   RGBColor          blackColour;
   CGrafPtr savedPort,port;
   Rect myRect = {400, 130, 430, 160};

   blackColour.red	 = 0x0000;
   blackColour.green = 0x0000;
   blackColour.blue	 = 0x0000;

//   GetIndPattern( &myPat, sysPatListID,2 ); 	/* brickwork */
   
//   GetPort(&savedPort);

   PenPat( &myPat );
//   GetDialogItem(PreferenceBox,kEditRect,&type,&itemHandle,&itemRect);
//   FrameRect(&itemRect);
//   GetDialogItem(PreferenceBox,kMemoryRect,&type,&itemHandle,&itemRect);
//   FrameRect(&itemRect);
//   GetIndPattern( &myPat, sysPatListID,1 ); 	/* Restore */
  //SetColor 
   GetDialogItem(PreferenceBox,   kActiveTextField, &type, &itemHandle, &itemRect);
   RGBForeColor(&tempTypeColour);
   SetDialogItemText(itemHandle, "\pActive Text Color");
   GetDialogItem(PreferenceBox,  kCompletedTextField, &type, &itemHandle, &itemRect);
   RGBForeColor(&tempFinishedColour);
   SetDialogItemText(itemHandle, "\pCompleted Text Color");
   GetDialogItem(PreferenceBox,  kComputerResponseField, &type, &itemHandle, &itemRect);
   

  //  SetPort ( GetDialogPort(PreferenceBox) ) ;
//	GetQDGlobalsBlack(&myPat);
//	 PenPat( &myPat );
//   GetIndPattern( &myPat, sysPatListID,1 ); 	/* Restore */
  
   // RGBForeColor(&blackColour);
   RGBForeColor(&tempComputerColour);
  //  PaintRect(&myRect);
  //  SetPort(savedPort);
   
   
   SetDialogItemText(itemHandle, "\pComputer Text Color");
   RGBForeColor(&blackColour); 

//   PenPat( &myPat );
}

/*
************************************************************************************************
SetTab routine :
************************************************************************************************
Desciption :
A procedure which used to set the Tab space of the Console window and the Edit window according
to the Preference (Global Variables). Actually, you can set different Tab Sapce in different 
text editing window. However, I predict it will confuse the user.
************************************************************************************************
*/
void SetTab(Boolean newtab){
   GrafPtr savePort;
   WEReference we ;
   WEStyleMode    mode;
   TextStyle      ts;
   SInt16         i;  
   char 		tabsize[10];
   
   if(!newtab)
   gtabSize = atoi(mac_getenv("TabSize"));
     
   if(gtabSize < 1 || gtabSize > 20)
    gtabSize = 5;
       
   if (!(Console_Window == nil)){
       we = GetWindowWE ( Console_Window);
       WESetTabSize(gtabSize,we); 
	   WEInstallTabHooks(we);
   }
   
   for (i=1; i<Edit_Window; i++){
       we = GetWindowWE ( Edit_Windows[i]); 
       WESetTabSize(gtabSize,we);
       // The following three lines is working, however, we only allow fix size font.
       // It is much more better than query the WASTE about the font size you are 
       // currently using. Cause they can use different font size in one page.       
       mode = weDoAll ;  // query about all attributes
       WEContinuousStyle ( & mode, & ts, we ) ;
       WESetTabSize(gtabSize*(ts . tsSize),we);

	   WEInstallTabHooks(we);
   
   }
   
    for (i=1; i<Help_Window; i++){
       we = GetWindowWE ( Help_Windows[i]); 
       WESetTabSize(gtabSize,we);
       mode = weDoAll ;  // query about all attributes
       WEContinuousStyle ( & mode, & ts, we ) ;
       WESetTabSize(gtabSize*(ts . tsSize),we);

	   WEInstallTabHooks(we);
   
   }
}


void SetTextSize(Boolean newsize){
   SInt16 i;
   SInt32 selStart, selEnd;
   WEReference we ;
   SInt16 Console_Width, NumofChar;
   GrafPtr savePort;    
   Rect portRect;
   
   we = GetWindowWE ( Console_Window);
   if(!we)
    return;
   WEGetSelection ( & selStart, & selEnd, we );
   WESetSelection ( 0, WEGetTextLength(we), we );
   if(!newsize)
   gTextSize = GetTextSize();
   changeSize(Console_Window, gTextSize);
   WESetSelection (selStart, selEnd, we);
   GetWindowPortBounds ( Console_Window, & portRect ) ;
   Console_Width = portRect.right - portRect.left ;
   GetPort(&savePort);
   SetPortWindowPort(Console_Window);
   TextFont(4);
   TextSize(gTextSize);
   NumofChar =    (int)((((Console_Width - 15) / CharWidth('M'))) - 0.5) ; 
   R_SetOptionWidth(NumofChar);
   SetPort(savePort);

    for(i=1; i<Help_Window; i++){
       we = GetWindowWE ( Help_Windows[i]);
       WEGetSelection ( & selStart, & selEnd, we );
       WESetSelection ( 0, WEGetTextLength(we), we );
       changeSize(Help_Windows[i], gTextSize);
       WESetSelection (selStart, selEnd, we);
    }

   for(i=1; i < Edit_Window; i++){
      we = GetWindowWE ( Edit_Windows[i]);
      WEGetSelection ( & selStart, & selEnd, we );
      WESetSelection ( 0, WEGetTextLength(we), we );
      changeSize(Edit_Windows[i], gTextSize);
      WESetSelection (selStart, selEnd, we);
   }


}

void changeFontAndSize(WindowPtr window);

void changeFontAndSize(WindowPtr window)
{

   FMFontFamily fontFamily=0;
   SInt16	newSize = gTextSize;
   

   
   if(systemVersion > kMinSystemVersion)
     fontFamily = FMGetFontFamilyFromName(UserFont);
   else
     GetFNum(UserFont,&fontFamily);
 
   if(window){
    WESetOneAttribute ( kCurrentSelection, kCurrentSelection, weTagFontSize,
      & newSize, sizeof ( Fixed ),	GetWindowWE ( window ) );
    WESetOneAttribute ( kCurrentSelection, kCurrentSelection, weTagFontFamily,
	  & fontFamily, sizeof ( fontFamily ), GetWindowWE ( window ) ) ;
   }
}

void SetTextFontAndSize(void){
   SInt16 i;
   SInt32 selStart, selEnd;
   WEReference we ;
   SInt16 Console_Width, NumofChar;
   GrafPtr savePort;    
   Rect portRect;
      FMFontFamily fontFamily=0;

   we = GetWindowWE ( Console_Window);
   if(!we)
    return;
   WEGetSelection ( & selStart, & selEnd, we );
   WESetSelection ( 0, WEGetTextLength(we), we );
 
   changeFontAndSize(Console_Window);
   WESetSelection (selStart, selEnd, we);
   GetWindowPortBounds ( Console_Window, & portRect ) ;
   Console_Width = portRect.right - portRect.left ;
   GetPort(&savePort);
   SetPortWindowPort(Console_Window);
   if(systemVersion > kMinSystemVersion)
     fontFamily = FMGetFontFamilyFromName(UserFont);
   else
     GetFNum(UserFont,&fontFamily);
   TextFont(fontFamily);
   TextSize(gTextSize);
   NumofChar =    (int)((((Console_Width - 15) / CharWidth('M'))) - 0.5) ; 
   R_SetOptionWidth(NumofChar);
   SetPort(savePort);

    for(i=1; i<Help_Window; i++){
       we = GetWindowWE ( Help_Windows[i]);
       WEGetSelection ( & selStart, & selEnd, we );
       WESetSelection ( 0, WEGetTextLength(we), we );
       changeFontAndSize(Help_Windows[i]);
       WESetSelection (selStart, selEnd, we);
    }

   for(i=1; i < Edit_Window; i++){
      we = GetWindowWE ( Edit_Windows[i]);
      WEGetSelection ( & selStart, & selEnd, we );
      WESetSelection ( 0, WEGetTextLength(we), we );
      changeFontAndSize(Edit_Windows[i]);
      WESetSelection (selStart, selEnd, we);
   }


}




void RPrefs ( void )
{
   //****
   DialogPtr          PreferenceBox = nil ;
   WEReference        we = nil ;
   GrafPtr            savePort ;
   short              type; 
   Rect               itemRect;
   Str255             buf;
   char               tempSpace[10];
   StandardFileReply  folder;
   Point			  where = {-1, -1}; 
   SInt16             itemHit = 0;
   Handle              itemHandle;
   Boolean            ChangeDir= false;
   RGBColor           outColor;
   SInt16              fileLen;
   Handle              fileName;
   Boolean				tempInterrupt = Interrupt;
   Boolean				tempOnOpenSource = OnOpenSource;

   PreferenceBox = GetNewDialog ( kPreferences, nil, ( WindowPtr ) -1L ) ;
   if ( PreferenceBox == nil )
   {
      goto cleanup ;
   }

   // set up the port
   GetPort ( & savePort ) ;
   SetPort ( GetDialogPort(PreferenceBox) ) ;
   TextSize(12);
   // Set the Dialog Title
   SetWTitle ( GetDialogWindow(PreferenceBox), "\pR Preferences" ) ;
   DrawBox(PreferenceBox);

   // Handle tab size
   GetDialogItem(PreferenceBox, kTabSize, &type, &itemHandle, &itemRect);
   sprintf(tempSpace, "%d", gtabSize);
   SetDialogItemText(itemHandle, StrToPStr(tempSpace, strlen(tempSpace)));
  
   GetDialogItem(PreferenceBox, kConsoleFont, &type, &itemHandle, &itemRect);
   SetDialogItemText(itemHandle, UserFont);
   
   GetDialogItem(PreferenceBox, kGraphFont, &type, &itemHandle, &itemRect);
   SetDialogItemText(itemHandle, PostFont);
    
   // Handle History size
   GetDialogItem(PreferenceBox, kHistoryLength, &type, &itemHandle, &itemRect);
   sprintf(tempSpace, "%d", storeHistory);   
   SetDialogItemText(itemHandle, StrToPStr(tempSpace, strlen(tempSpace)));
   
   //Handle Text Size
   GetDialogItem(PreferenceBox, kConsTextSize, &type, &itemHandle, &itemRect);
   sprintf(tempSpace, "%d", gTextSize);
   SetDialogItemText(itemHandle, StrToPStr(tempSpace, strlen(tempSpace)));
 
   
   //Handle Screen Resolution
   GetDialogItem(PreferenceBox, kScreenRes, &type, &itemHandle, &itemRect);
   sprintf(tempSpace, "%d", (gScreenRes));
   SetDialogItemText(itemHandle, StrToPStr(tempSpace, strlen(tempSpace)));
   //show the dialog
   
   
    GetDialogItem(PreferenceBox, kInterrupt, &type, &itemHandle, &itemRect);
    tempInterrupt = Interrupt;
    SetControlValue((ControlHandle) itemHandle, (tempInterrupt ? 1 : 0));

    GetDialogItem(PreferenceBox, kOnOpenSource, &type, &itemHandle, &itemRect);
    tempOnOpenSource = OnOpenSource;
    SetControlValue((ControlHandle) itemHandle, (tempOnOpenSource ? 1 : 0));

   ShowWindow ( GetDialogWindow(PreferenceBox) ) ;
   

   
   while(true){
      // wait for a click in the picture
      ModalDialog ( GetMyStandardDialogFilter ( ), & itemHit ) ;
   
      if (itemHit == kActiveTextButton){
         pickColor(PreferenceBox, kActiveTextField, tempTypeColour, &outColor);
         tempTypeColour = outColor;
         DrawBox(PreferenceBox);
      }
      if (itemHit == kCompletedTextButton){
         pickColor(PreferenceBox, kCompletedTextField, tempFinishedColour, &outColor);
         tempFinishedColour = outColor;
         DrawBox(PreferenceBox);
      }
      if (itemHit == kComputerResponseButton){
         pickColor(PreferenceBox, kComputerResponseField, tempComputerColour, &outColor);
         tempComputerColour = outColor;
         DrawBox(PreferenceBox);

      }

	  if (itemHit == kInterrupt){
          GetDialogItem(PreferenceBox, kInterrupt, &type, &itemHandle, &itemRect);
          tempInterrupt = (tempInterrupt ? 0 : 1);
		  SetControlValue((ControlHandle) itemHandle, (tempInterrupt ? 1 : 0));

      }
	   
	   if (itemHit == kOnOpenSource){
          GetDialogItem(PreferenceBox, kOnOpenSource, &type, &itemHandle, &itemRect);
          tempOnOpenSource = (tempOnOpenSource ? 0 : 1);
		  SetControlValue((ControlHandle) itemHandle, (tempOnOpenSource ? 1 : 0));

      }

	

      if ( itemHit == kOkButton )
      { 
         //Handle Tab Size
         GetDialogItem(PreferenceBox, kTabSize, &type, &itemHandle, &itemRect);
         GetDialogItemText(itemHandle, buf);
         // Didn't allow you to use a tab with size bigger than 99
         if (buf[0] > 2){
            GWdoErrorAlert(eTabSize);
            break;
         }
         
         // Handle History Length
         GetDialogItem(PreferenceBox, kHistoryLength, &type, &itemHandle, &itemRect);
         GetDialogItemText(itemHandle, buf);
         // Didn't allow you to use a tab with size bigger than 999
         if (buf[0] > 3){
            GWdoErrorAlert(eHistorySize);
            break;
         }
         
         //Handle Global Text Size
         GetDialogItem(PreferenceBox, kConsTextSize, &type, &itemHandle, &itemRect);
         GetDialogItemText(itemHandle, buf);
         // Didn't allow you to use a tab with size bigger than 99
         if (buf[0] > 2){
            GWdoErrorAlert(eTextSize);
            break;
         }         
         
		 
         //Handle ScreenRes
         GetDialogItem(PreferenceBox, kScreenRes, &type, &itemHandle, &itemRect);
         GetDialogItemText(itemHandle, buf);
         buf[buf[0] + 1] = '\0';
         if ((atoi((char *)&buf[1]) > 144) || (atoi((char *)&buf[1]) < 36)){
            GWdoErrorAlert(eScreenRes);  //???
            break;
         }            
         
         gTypeColour = tempTypeColour;
         gComputerColour = tempComputerColour;
         gFinishedColour = tempFinishedColour;
         GetDialogPrefs(PreferenceBox,true);

         break;
  
      }
  
      if (itemHit == kCancelButton){
         tempTypeColour = gTypeColour;
         tempComputerColour = gComputerColour;
         tempFinishedColour = gFinishedColour;
         // When you click Cancel button, no action followed
         break;
      }
      
      
      if (itemHit == kApplyButton){
         GetDialogPrefs(PreferenceBox,false);
         // When you click Cancel button, no action followed
         break;
      }
      
      
   }


// Release the resource
cleanup :
   if ( we != nil )
   {
      WEDispose ( we ) ;
   }
   if ( PreferenceBox != nil )
   {
      DisposeDialog ( PreferenceBox ) ;
      // Restore the Port
      SetPort ( savePort ) ;
   }
   
   
   
}






/* ************************************************************************************************
savePreference: This function is used to save the global variable into the preference without 
prompt out the preference dialog.
************************************************************************************************ */
void savePreference(void){
   appPrefsHandle     appPrefsHdl;
   Handle             existingResHdl;
   Str255             resourceName = "\pPreferences";
   char               buf[50];
   appPrefs myPref;

    myPref.gtabSize = gtabSize;
    myPref.gTextSize = gTextSize;
    myPref.storeHistory = storeHistory;

    myPref.gScreenRes = gScreenRes;

    PStringCopy(PostFont,myPref.PostFont);
	PStringCopy(UserFont,myPref.UserFont);
	
	CopyPascalStringToC(myPref.UserFont,buf);
	
    myPref.OnOpenSource = OnOpenSource;
    myPref.Interrupt = Interrupt;
   
    myPref.TypeColour = gTypeColour;
    myPref.FinishedColour = gFinishedColour;
    myPref.ComputerColour = gComputerColour;
    myPref.prefsTypeVers = 0L;
	
     SavePrefs(&myPref);
}



OSErr SavePrefs(appPrefs * thePrefsTypePtr)
/* save the prefs structure in the prefs resource file */
{
	OSErr retCode;
	short prefsResRefNum;
	Handle prefHandle, finderMessageHandle;
	
	/* open (and, if necessary, create) the prefs file */
	prefsResRefNum = OpenPrefsResFile(fsRdWrPerm, true);
	if (prefsResRefNum != -1) {
	
		/* file opened successfully, get the prefs resource */
		prefHandle = Get1Resource(kPrefsResourceType, kPrefsResourceID);
		
		if (prefHandle == nil) {
		
			/* create a new resource */
			prefHandle = NewHandle(sizeof(appPrefs));
			if (prefHandle != nil) {
			
				/* copy the prefs struct into the handle
				   and make it into a resource */
				
				*(appPrefs *)*prefHandle = *thePrefsTypePtr;
				AddResource(prefHandle, kPrefsResourceType, kPrefsResourceID, 
					"\pRPrefs");
				retCode = ResError();
				if (retCode != noErr) DisposeHandle(prefHandle);
			} 
			
			else retCode = MemError(); /* NewHandle failed */
		}
		
		else {  /* prefHandle != nil */
		
			/* update the existing resource */
			SetHandleSize(prefHandle, sizeof(appPrefs));
			retCode = MemError();
			if (retCode == noErr) {

				/* copy the prefs struct into the handle and tell the rsrc manager */
				*(appPrefs *)*prefHandle = *thePrefsTypePtr;
				ChangedResource(prefHandle);
			}
		}
		
		if (retCode == noErr) {
			/* now, get rid of the old fileAlias and, if the fileAliasID field
			   of the prefs struct indicates that there is a new one, add it to
			   the resource file */
			   
			
			/* add the message to be displayed if the user tries
			   to open the prefs file in the Finder (but don't add it
			   if it's already in the preferences file) */
			   
			finderMessageHandle = (Handle) GetString(kFinderMessageStrID);
			if (finderMessageHandle != nil &&
				HomeResFile((Handle) finderMessageHandle) != prefsResRefNum) {
			
				/* copy the resource into the prefs file */
				DetachResource(finderMessageHandle);
				AddResource(finderMessageHandle, kStrType, kFinderMessageStrID,
					"\pFinder message");
					
				/* if AddResource failed, dispose of the handle */
				retCode = ResError();
				if (retCode != noErr) DisposeHandle(finderMessageHandle);
			}
		}
		
		/* update and close the preference resource file, 
		   releasing its resources from memory */
		CloseResFile(prefsResRefNum);
	}
	
	else {
		/* couldn't open the res file */
		retCode = ResError();
		if (retCode == noErr) retCode = resFNotFound;
	}
	
	return retCode;
}


void GetDialogPrefs(DialogPtr PreferenceBox, Boolean saveit)
{
   short              type, i; 
   Handle             itemHandle;
   Rect               itemRect;
   Str255             buf;
   char               tempSpace[10];
   Str255             resourceName = "\pPreferences";
   FMFontFamily 		postFontId;

   
   // It is used to save the tab Size
   GetDialogItem(PreferenceBox, kTabSize, &type, &itemHandle, &itemRect);
   GetDialogItemText(itemHandle, buf);
   CopyPascalStringToC(buf,tempSpace);
   gtabSize = atoi(buf);
   SetTab(true);
   
   //It is used to save the history size
   // this will take effect next time you start R
   GetDialogItem(PreferenceBox, kHistoryLength, &type, &itemHandle, &itemRect);
   GetDialogItemText(itemHandle, buf);
   CopyPascalStringToC(buf,tempSpace);
   storeHistory = atoi(tempSpace);
   
   //Handle textSize
   GetDialogItem(PreferenceBox, kConsTextSize, &type, &itemHandle, &itemRect);
   GetDialogItemText(itemHandle, buf);
   CopyPascalStringToC(buf,tempSpace);
   gTextSize = atoi(tempSpace);

  
   //It is used to get the console font name
   GetDialogItem(PreferenceBox, kConsoleFont, &type, &itemHandle, &itemRect);
   GetDialogItemText(itemHandle, UserFont);
   if(systemVersion > kMinSystemVersion){
     if( FMGetFontFamilyFromName(UserFont) == kInvalidFontFamily)
        doCopyPString("\pmonaco", UserFont);
      }
      else {
       GetFNum(UserFont, &postFontId);
       if( postFontId == kInvalidFontFamily)
         doCopyPString("\pmonaco", UserFont); 
      }

   EIGHTY = EightyWidth();
  
   SetTextFontAndSize();
  
   GetDialogItem(PreferenceBox, kGraphFont, &type, &itemHandle, &itemRect);
   GetDialogItemText(itemHandle, PostFont);
  
   if(GetFontIDFromMacFontName(PostFont) == kATSUInvalidFontID)
     doCopyPString("\phelvetica", PostFont);

   // Handle Screen Resolution
   GetDialogItem(PreferenceBox, kScreenRes, &type, &itemHandle, &itemRect);
   GetDialogItemText(itemHandle, buf);
   CopyPascalStringToC(buf,tempSpace);
   gScreenRes = atoi(tempSpace);
 
   gTypeColour = tempTypeColour;
   gComputerColour = tempComputerColour;
   gFinishedColour = tempFinishedColour;
     
   
   
    GetDialogItem(PreferenceBox, kOnOpenSource, &type, &itemHandle, &itemRect);
    OnOpenSource = GetControlValue((ControlHandle) itemHandle);
	
	GetDialogItem(PreferenceBox, kInterrupt, &type, &itemHandle, &itemRect);
    Interrupt = GetControlValue((ControlHandle) itemHandle);
	
	
   
   if(saveit)
    savePreference();

}










short OpenPrefsResFile(SignedByte prefsPerm, Boolean createFlag)
/* open the preferences file with the given permission; if createFlag is set,
   create a preferences file if necessary */
{
	OSErr retCode;
	short prefsVRefNum;
	long prefsDirID;
	Str255 prefsNameStr;
	FSSpec prefsFSSpec;
	short prefsResRefNum = -1;

	
	/* get the name of the prefs file */

	GetIndString(prefsNameStr,rPreStringList,iPrefsFileName);
		
    retCode = FindFolder(kOnSystemDisk, kPreferencesFolderType, kCreateFolder,
			&prefsVRefNum, &prefsDirID);
	
		if (retCode == noErr) {
		
			/* make a file spec for the prefs file */
			
			retCode = FSMakeFSSpec(prefsVRefNum, prefsDirID, prefsNameStr,
				&prefsFSSpec);
				
			if (retCode == fnfErr && createFlag) {
				/* prefs file doesn't already exist, so create it */
				FSpCreateResFile(&prefsFSSpec, kPrefsCreatorType, kPrefsFileType,
					smSystemScript);
				retCode = ResError();
			}
			
			/* open the prefs file */
			if (retCode == noErr) {
				prefsResRefNum = FSpOpenResFile(&prefsFSSpec, prefsPerm);
			}
		}
	
	return prefsResRefNum;
}

