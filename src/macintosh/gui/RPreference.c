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


extern Boolean              Have_Console;
/* ************************************************************************************************ */
/*                          Constant, Global variables and prototype                                */
/* ************************************************************************************************ */

/*
Here include all the Control in the Preference dialog, if you want to add more control, 
you needed to add the corresponding item in here.
*/
enum
{
   kActiveTextButton            = 12,
   kCompletedTextButton         = 2,
   kComputerResponseButton      = 3,
   kActiveTextField             = 4,
   kCompletedTextField          = 5,
   kComputerResponseField       = 6,
   kTabSize                     = 7,
   kHistoryLength               = 8,
//   kTextSize                    = 9,  // Jago
   kR_Vsize                     = 10,
   kR_Nsize                     = 11,
   kOkButton                    = 1,       // 1 is default button.
   kCancelButton                = 13,   
   kEditRect                    = 27,
   kScreenRes                   = 29,
   kMemoryRect                  = 30   
   
} ;

/*
Here include all the content that the preference will store, if you wanted to add more.
You need to add the corresponding item in here.
*/
typedef struct
{
   char                        tabSize[3];
   char                        textSize[3];
   char                        ScreenR[5];
   RGBColor	                   TypeColour;
   RGBColor                    FinishedColour;
   RGBColor                    ComputerColour;
   FSSpec                      sfFile;
   StandardFileReply           sfr;
   char                        envString[255];
}  appPrefs, *appPrefsPointer, **appPrefsHandle;

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


/* ************************************************************************************************ */
/*                                        Protocols                                                 */
/* ************************************************************************************************ */ 
unsigned char*                            StrToPStr(char*, SInt8);
void                                      doSavePreference(DialogPtr );
void                                      doGetPreferences(void);
void                                      savePreference(void);
void                                      DrawBox (DialogPtr PreferenceBox);
void                                      SetTab(void);
pascal	OSErr	                          FSpGetFullPath (const FSSpec*, short*, Handle*);
void                                      pickColor(DialogPtr, SInt16 , RGBColor , RGBColor*);
void                                      SetTextSize(void);
extern int                                R_SetOptionWidth(int w);

extern int GetTextSize(void);
int EightyWidth(void);





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

/* ************************************************************************************************
doGetPreferences :	This function will be called at the beginning when R application starts.
					It sets up some parameters specified by the user in the .Renviron file
					such as the graphic device screen resolution and font, the Console font and
					size and the tab-size of all the text windows. Etc.
Jago, April 2001, Stefano M. Iacus	
************************************************************************************************ */
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
   
     
      strcpy(genvString, ".Renviron");
      SetTab();
    
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
  
      if( FMGetFontFamilyFromName(UserFont) == kInvalidFontFamily)
         doCopyPString("\pmonaco", UserFont); /* Emergency font ! */

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
	  
    fontFamily = FMGetFontFamilyFromName(UserFont);
  
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
   blackColour.red	 = 0x0000;
   blackColour.green = 0x0000;
   blackColour.blue	 = 0x0000;
   GetIndPattern( &myPat, sysPatListID,2 ); 	/* brickwork */
   PenPat( &myPat );
   GetDialogItem(PreferenceBox,kEditRect,&type,&itemHandle,&itemRect);
   FrameRect(&itemRect);
   GetDialogItem(PreferenceBox,kMemoryRect,&type,&itemHandle,&itemRect);
   FrameRect(&itemRect);
   GetIndPattern( &myPat, sysPatListID,1 ); 	/* Restore */
  //SetColor 
   GetDialogItem(PreferenceBox,   kActiveTextField, &type, &itemHandle, &itemRect);
   RGBForeColor(&tempTypeColour);
   SetDialogItemText(itemHandle, "\pActive Text Color");
   GetDialogItem(PreferenceBox,  kCompletedTextField, &type, &itemHandle, &itemRect);
   RGBForeColor(&tempFinishedColour);
   SetDialogItemText(itemHandle, "\pCompleted Text Color");
   GetDialogItem(PreferenceBox,  kComputerResponseField, &type, &itemHandle, &itemRect);
   RGBForeColor(&tempComputerColour);
   SetDialogItemText(itemHandle, "\pComputer Text Color");
   RGBForeColor(&blackColour); 

   PenPat( &myPat );
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
void SetTab(){
   GrafPtr savePort;
   WEReference we ;
   WEStyleMode    mode;
   TextStyle      ts;
   SInt16         i;  
   char 		tabsize[10];
   

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


void SetTextSize(){
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

