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
int                                        gScreenRes, gScreenRes;

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

/* ************************************************************************************************ */
/*                                        Protocols                                                 */
/* ************************************************************************************************ */ 
unsigned char*                            StrToPStr(char*, SInt8);
void                                      doSavePreference(DialogPtr );
OSErr                                     doCopyResource(ResType ,SInt16,SInt16, SInt16);
void                                      doGetPreferences(void);
void                                      savePreference(void);
void                                      DrawBox (DialogPtr PreferenceBox);
void                                      SetTab(void);
pascal	OSErr	                          FSpGetFullPath (const FSSpec*, short*, Handle*);
void                                      pickColor(DialogPtr, SInt16 , RGBColor , RGBColor*);
void                                      SetTextSize(void);
extern int                                R_SetOptionWidth(int w);

/* ************************************************************************************************
DoPreference : Open the Preference dialog and then call save preference.
************************************************************************************************ */
void DoPreference ( SInt16 dialogID )
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

   PreferenceBox = GetNewDialog ( kPreferneces, nil, ( WindowPtr ) -1L ) ;
   if ( PreferenceBox == nil )
   {
      goto cleanup ;
   }

   // set up the port
   GetPort ( & savePort ) ;
   SetPort ( PreferenceBox ) ;
   TextSize(12);
   // Set the Dialog Title
   SetWTitle ( PreferenceBox, "\p R Preference !" ) ;
   DrawBox(PreferenceBox);

   // Handle tab size
   GetDialogItem(PreferenceBox, kTabSize, &type, &itemHandle, &itemRect);
   sprintf(tempSpace, "%d", gtabSize);
   SetDialogItemText(itemHandle, StrToPStr(tempSpace, strlen(tempSpace)));
    
   // Handle History size
/* GetDialogItem(PreferenceBox, kHistoryLength, &type, &itemHandle, &itemRect);
   sprintf(tempSpace, "%d", storeHistory);   
   SetDialogItemText(itemHandle, StrToPStr(tempSpace, strlen(tempSpace)));
*/
   
   //Handle Text Size
   GetDialogItem(PreferenceBox, kTextSize, &type, &itemHandle, &itemRect);
   sprintf(tempSpace, "%d", gTextSize);
   SetDialogItemText(itemHandle, StrToPStr(tempSpace, strlen(tempSpace)));
 
/*   
   //Handle R_NSize
   GetDialogItem(PreferenceBox, kR_Nsize, &type, &itemHandle, &itemRect);
   sprintf(tempSpace, "%d", (PR_NSize)); 
   SetDialogItemText(itemHandle, StrToPStr(tempSpace, strlen(tempSpace)));
 
   //Handle R_VSize
   GetDialogItem(PreferenceBox, kR_Vsize, &type, &itemHandle, &itemRect);
   sprintf(tempSpace, "%d", (PR_VSize));
   SetDialogItemText(itemHandle, StrToPStr(tempSpace, strlen(tempSpace)));
 */
   //Handle Screen Resolution
   GetDialogItem(PreferenceBox, kScreenRes, &type, &itemHandle, &itemRect);
   sprintf(tempSpace, "%d", (gScreenRes));
   SetDialogItemText(itemHandle, StrToPStr(tempSpace, strlen(tempSpace)));
   //show the dialog
   ShowWindow ( PreferenceBox ) ;
   
   
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
   /*      GetDialogItem(PreferenceBox, kHistoryLength, &type, &itemHandle, &itemRect);
         GetDialogItemText(itemHandle, buf);
         // Didn't allow you to use a tab with size bigger than 999
         if (buf[0] > 3){
            GWdoErrorAlert(eHistorySize);
            break;
         }
     */    
         //Handle Global Text Size
         GetDialogItem(PreferenceBox, kTextSize, &type, &itemHandle, &itemRect);
         GetDialogItemText(itemHandle, buf);
         // Didn't allow you to use a tab with size bigger than 99
         if (buf[0] > 2){
            GWdoErrorAlert(eTextSize);
            break;
         }         
         
/*         //Handle R_NSize
         GetDialogItem(PreferenceBox, kR_Nsize, &type, &itemHandle, &itemRect);
         GetDialogItemText(itemHandle, buf);
         buf[buf[0] + 1] = '\0';
         if ((atoi((char *)&buf[1]) > 1000) || (atoi((char *)&buf[1]) < 200)){
            GWdoErrorAlert(eR_NSize);
            break;
         }           
  */       
    /*     //Handle R_VSize
         GetDialogItem(PreferenceBox, kR_Vsize, &type, &itemHandle, &itemRect);
         GetDialogItemText(itemHandle, buf);
         buf[buf[0] + 1] = '\0';
         if ((atoi((char *)&buf[1]) > 500) || (atoi((char *)&buf[1]) < 1)){
            GWdoErrorAlert(eR_VSize);
            break;
         }  
         
         //Handle R_VSize
         GetDialogItem(PreferenceBox, kScreenRes, &type, &itemHandle, &itemRect);
         GetDialogItemText(itemHandle, buf);
         buf[buf[0] + 1] = '\0';
         if ((atoi((char *)&buf[1]) > 144) || (atoi((char *)&buf[1]) < 36)){
            GWdoErrorAlert(eScreenRes);  //???
            break;
         }            
      */   
         gTypeColour = tempTypeColour;
         gComputerColour = tempComputerColour;
         gFinishedColour = tempFinishedColour;
         // When you click the OK button, save the preference
         doSavePreference(PreferenceBox);
         break;
  
      }
  
      if (itemHit == kCancelButton){
         tempTypeColour = gTypeColour;
         tempComputerColour = gComputerColour;
         tempFinishedColour = gFinishedColour;
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
doSavePreference : Query the contents inside the PreferenceBox, and then call savePreference
************************************************************************************************ */
void doSavePreference(DialogPtr PreferenceBox){
   short              type, i; 
   Handle             itemHandle;
   Rect               itemRect;
   Str255             buf;
   char               tempSpace[10];
   Str255             resourceName = "\pPreferences";
   
   // It is used to save the tab Size
   GetDialogItem(PreferenceBox, kTabSize, &type, &itemHandle, &itemRect);
   GetDialogItemText(itemHandle, buf);
   for (i=0; i<buf[0]; i++)
      tempSpace[i] = buf[i+1];
   tempSpace[i] = '\0';
   gtabSize = atoi(tempSpace);
   SetTab();
   //It is used to save the history size
/*   GetDialogItem(PreferenceBox, kHistoryLength, &type, &itemHandle, &itemRect);
   GetDialogItemText(itemHandle, buf);
   for (i=0; i<buf[0]; i++)
      tempSpace[i] = buf[i+1];
   tempSpace[i] = '\0';
  */    
   // After you change History length, you can only have effect after you restart the program
   // it is because you need to allocate memory for history record when you start the computer.
/*   storeHistory = atoi(tempSpace);
  */
   
   //Handle textSize
   GetDialogItem(PreferenceBox, kTextSize, &type, &itemHandle, &itemRect);
   GetDialogItemText(itemHandle, buf);
   for (i=0; i<buf[0]; i++)
      tempSpace[i] = buf[i+1];
   tempSpace[i] = '\0';
   gTextSize = atoi(tempSpace);
   SetTextSize();
   //Handle R_Nsize
/*   GetDialogItem(PreferenceBox, kR_Nsize, &type, &itemHandle, &itemRect);
   GetDialogItemText(itemHandle, buf);
   for (i=0; i<buf[0]; i++)
      tempSpace[i] = buf[i+1];
   tempSpace[i] = '\0';
   PR_NSize = atoi(tempSpace);
   
   
   // Handle R_Vsize
   GetDialogItem(PreferenceBox, kR_Vsize, &type, &itemHandle, &itemRect);
   GetDialogItemText(itemHandle, buf);
   for (i=0; i<buf[0]; i++)
      tempSpace[i] = buf[i+1];
   tempSpace[i] = '\0';
   PR_VSize = atoi(tempSpace);
  */ 
   
   // Handle Screen Resolution
   GetDialogItem(PreferenceBox, kScreenRes, &type, &itemHandle, &itemRect);
   GetDialogItemText(itemHandle, buf);
   for (i=0; i<buf[0]; i++)
      tempSpace[i] = buf[i+1];
   tempSpace[i] = '\0';
   gScreenRes = atoi(tempSpace);
   
   savePreference();

}



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


char *mac_getenv(const char *name);

/* ************************************************************************************************
doGetPreferences : This function will be called at the beginning when your application start to
run, it will load the preference out, and assign it to the global variable.
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
   char 				hist[50];
   
      gtabSize = 3;
      strcpy(genvString, ".Renviron");
      SetTab();
      storeHistory = 500;
      HISTORY = storeHistory +1;
      Cmd_Hist = malloc(HISTORY * sizeof(Ptr));
      gTextSize = 12;
      gScreenRes = 72;
      
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

void  doGetPreferences1(void)
{
   Str255               prefsFileName;
   OSErr                osError;
   SInt16               volRefNum;
   long                 directoryID;
   FSSpec               fileSSpec;
   SInt16               fileRefNum;
   appPrefsHandle       appPrefsHdl;
   char 				hist[50];
   GetIndString(prefsFileName,rPreStringList,iPrefsFileName);

   osError = FindFolder(kOnSystemDisk,kPreferencesFolderType,kDontCreateFolder,&volRefNum,
                                  &directoryID);

   if(osError == noErr)
      osError = FSMakeFSSpec(volRefNum,directoryID,prefsFileName,&fileSSpec);
   if(osError == noErr || osError == fnfErr)
      fileRefNum = FSpOpenResFile(&fileSSpec,fsCurPerm);
  
  // This will be call when you have no preference file in the beginning.
   if(fileRefNum == -1)
   {
      FSpCreateResFile(&fileSSpec,'PpPp','pref',smSystemScript);
      osError = ResError();

      if(osError == noErr)
      {
         fileRefNum = FSpOpenResFile(&fileSSpec,fsCurPerm);
         if(fileRefNum != -1 )
         {
            UseResFile(gAppResFileRefNum);
               
            osError = doCopyResource(rTypePrefs,kPrefsID,gAppResFileRefNum,fileRefNum);
            if(osError == noErr)
               osError = doCopyResource(rTypeAppMiss,kAppMissID,gAppResFileRefNum,fileRefNum);
            if(osError != noErr)
            {
               CloseResFile(fileRefNum);
               osError = FSpDelete(&fileSSpec);
               fileRefNum = -1;
            }
         }
      }
   }
   
  // This will be call, when you already have preference file
   if(fileRefNum != -1)
   {
      UseResFile(fileRefNum); 

      appPrefsHdl = (appPrefsHandle) Get1Resource(rTypePrefs,kPrefsID);
      if(appPrefsHdl == NULL)
         return;
         
         
      //Insert NULL character at the end of the String.
      (*appPrefsHdl)->tabSize[2] = '\0';
    //  (*appPrefsHdl)->HistRecordSize[3] = '\0';
      (*appPrefsHdl)->textSize[2] = '\0';
//      (*appPrefsHdl)->R_Vsize[4] = '\0';
 //     (*appPrefsHdl)->R_Nsize[4] = '\0';
      (*appPrefsHdl)->ScreenR[4] = '\0';
      gHelpFile = (*appPrefsHdl)->sfFile;
      gtabSize = atoi((*appPrefsHdl)->tabSize);
      strcpy(genvString, (*appPrefsHdl)->envString);
      SetTab();
 //     storeHistory = atoi((*appPrefsHdl)->HistRecordSize);
      /*HISTORY = atoi((*appPrefsHdl)->HistRecordSize) + 1;
      */
      
      storeHistory = 100;
  /*    strcpy(hist,mac_getenv("R_HISTSIZE"));
       
      if(hist)
       HISTORY=atoi(hist);
      else 
    */   HISTORY = storeHistory +1;
       
        
      Cmd_Hist = malloc(HISTORY * sizeof(Ptr));
            
      gTextSize = atoi((*appPrefsHdl)->textSize);
      
/*      PR_NSize =  atoi((*appPrefsHdl)->R_Nsize);
//      R_NSize = PR_NSize * (1 <<10);
      PR_VSize =  atoi((*appPrefsHdl)->R_Vsize);
//      R_VSize = PR_VSize * (1 << 20);
*/      gScreenRes = gScreenRes = atoi((*appPrefsHdl)->ScreenR);
      
      gPrefsFileRefNum = fileRefNum;
      gsfr = (*appPrefsHdl)->sfr;
      tempTypeColour= gTypeColour = (*appPrefsHdl)->TypeColour;
      tempFinishedColour=gFinishedColour = (*appPrefsHdl)->FinishedColour;
      tempComputerColour=gComputerColour = (*appPrefsHdl)->ComputerColour;
      UseResFile(gAppResFileRefNum);
   }
}


/* ************************************************************************************************
doCopyResource
************************************************************************************************ */
OSErr  doCopyResource(ResType resType,SInt16 resID,SInt16 sourceFileRefNum,
                                 SInt16 destFileRefNum)
{
   SInt16         oldResFileRefNum;
   Handle         sourceResourceHdl;
   ResType        ignoredType;
   SInt16         ignoredID;
   Str255         resourceName;
   SInt16         resAttributes;
   OSErr          osError;

   oldResFileRefNum = CurResFile();
   UseResFile(sourceFileRefNum);

   sourceResourceHdl = Get1Resource(resType,resID);

   if(sourceResourceHdl != NULL)
   {
      GetResInfo(sourceResourceHdl,&ignoredID,&ignoredType,resourceName);
      resAttributes = GetResAttrs(sourceResourceHdl);
      DetachResource(sourceResourceHdl);
      UseResFile(destFileRefNum);
      if(ResError() == noErr)
         AddResource(sourceResourceHdl,resType,resID,resourceName);
      if(ResError() == noErr)
         SetResAttrs(sourceResourceHdl,resAttributes);
      if(ResError() == noErr)
         ChangedResource(sourceResourceHdl);
      if(ResError() == noErr)
         WriteResource(sourceResourceHdl);
   }

   osError = ResError();

   ReleaseResource(sourceResourceHdl);
   UseResFile(oldResFileRefNum);

   return(osError);
}

void savePreference(void){
   
   return;
}


/* ************************************************************************************************
savePreference: This function is used to save the global variable into the preference without 
prompt out the preference dialog.
************************************************************************************************ */
void savePreference1(void){
   appPrefsHandle     appPrefsHdl;
   Handle             existingResHdl;
   Str255             resourceName = "\pPreferences";
   char               buf[10];
   if(gPrefsFileRefNum == -1)
      return;

   // Save it into the preference file
   appPrefsHdl = (appPrefsHandle) NewHandleClear(sizeof(appPrefs));

   HLock((Handle) appPrefsHdl);
   sprintf(buf, "%d", gtabSize);
   strncpy((*appPrefsHdl)->tabSize, buf, 3);
   sprintf(buf, "%d", storeHistory );
//   strncpy((*appPrefsHdl)->HistRecordSize,buf, 4);
   sprintf(buf, "%d", gTextSize);
   strncpy((*appPrefsHdl)->textSize, buf, 3);
//   sprintf(buf, "%d", PR_NSize);
//   strncpy((*appPrefsHdl)->R_Nsize, buf, 5);
//   sprintf(buf, "%d", PR_VSize);
//   strncpy((*appPrefsHdl)->R_Vsize, buf, 5);
   sprintf(buf, "%d", gScreenRes);
   strncpy((*appPrefsHdl)->ScreenR, buf, 5);
      
   (*appPrefsHdl)->sfFile = gHelpFile;
   (*appPrefsHdl)->sfr = gsfr;
   (*appPrefsHdl)->TypeColour = gTypeColour;
   (*appPrefsHdl)->FinishedColour = gFinishedColour;
   (*appPrefsHdl)->ComputerColour = gComputerColour;
   strcpy((*appPrefsHdl)->envString, genvString);
   UseResFile(gPrefsFileRefNum);

   existingResHdl = Get1Resource(rTypePrefs,kPrefsID);
   if(existingResHdl != NULL)
   {
      RemoveResource(existingResHdl);
      if(ResError() == noErr)
         AddResource((Handle) appPrefsHdl,rTypePrefs,kPrefsID,resourceName);
      if(ResError() == noErr)
         WriteResource((Handle) appPrefsHdl);
   }

   HUnlock((Handle) appPrefsHdl);

   ReleaseResource((Handle) appPrefsHdl);
   UseResFile(gAppResFileRefNum);
}


/*
************************************************************************************************
setPathName routine :
************************************************************************************************
Desciption :
This Procedure is called when you need to display the path that you selected in the "Directory
dialog" into the preference dialog. 
************************************************************************************************
*/

#ifdef XXX
void setPathName(StandardFileReply  folder, DialogPtr PreferenceBox){
   short              type; 
   Rect               itemRect;
   Str255             buf;
   SInt16              fileLen,i;
   Handle              fileName, itemHandle;
   char               tempString[255];
   
   
   FSpGetFullPath (&folder.sfFile, &fileLen, &fileName);
   GetDialogItem(PreferenceBox, iFileName, &type, &itemHandle, &itemRect);
   HLock((Handle) fileName);
   strcpy(tempString, *fileName);
   HUnlock((Handle) fileName);
   buf[0] = fileLen;
   for (i=1 ; i <= fileLen; i++){
       buf[i] = tempString[i-1];
   }
   TextSize(9);
   SetDialogItemText(itemHandle, buf);
   TextSize(12);
}

#endif
/*
************************************************************************************************
displayPathName routine :
************************************************************************************************
Desciption :
This Procedure is called when you need to display the path.
************************************************************************************************
*/

#ifdef XXX
void displayPathName( DialogPtr PreferenceBox){
   short              type; 
   Rect               itemRect;
   Str255             buf;
   SInt16              fileLen,i;
   Handle              fileName, itemHandle;
   char               tempString[255];
   
   GetDialogItem(PreferenceBox, iFileName, &type, &itemHandle, &itemRect);
   fileLen = strlen(genvString);
   buf[0] = fileLen;
   for (i=1 ; i <= fileLen; i++){
       buf[i] = genvString[i-1];
   }
   TextSize(9);
   SetDialogItemText(itemHandle, buf);
   TextSize(12);
}

#endif
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
   Handle             itemHandle;
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
   Handle             itemHandle;
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
   if (!(Console_Window == nil)){
       we = GetWindowWE ( Console_Window);
       WESetTabSize(gtabSize*kWordSize,we); 
      
	   WEInstallTabHooks(we);
   }
   
   for (i=1; i<Edit_Window; i++){
       we = GetWindowWE ( Edit_Windows[i]); 
       WESetTabSize(gtabSize*kWordSize,we);
/*     
       // The following three lines is working, however, we only allow fix size font.
       // It is much more better than query the WASTE about the font size you are 
       // currently using. Cause they can use different font size in one page.       
       mode = weDoAll ;  // query about all attributes
       WEContinuousStyle ( & mode, & ts, we ) ;
       WESetTabSize(gtabSize*(ts . tsSize),we);
*/
	   WEInstallTabHooks(we);
   
   }
}


void SetTextSize(){
   SInt16 i;
   SInt32 selStart, selEnd;
   WEReference we ;
   SInt16 Console_Width, NumofChar;
   GrafPtr savePort;    
   
   we = GetWindowWE ( Console_Window);
   WEGetSelection ( & selStart, & selEnd, we );
   WESetSelection ( 0, WEGetTextLength(we), we );
   changeSize(Console_Window, gTextSize);
   WESetSelection (selStart, selEnd, we);
   Console_Width = (Console_Window ->portRect).right - (Console_Window ->portRect).left ;
   GetPort(&savePort);
   SetPort(Console_Window);
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

