/*******************************************************************************
* StandardGetFolder.c                                                          *
*                                                                              *
*    This little chunk o' code implements a way to let the user choose a       *
*    folder to save files in via a StandardFile Dialog.                        *
*                                                                              *
*    Since the code uses the CustomGetFile function and depends on the use of  *
*    FSSpec records, it only works under System 7.0 or later.                  *                        
*                                                                              *
*    And don't forget to include the custom dialog resources ( a 'DITL' and    *
*   'DLOG') in your project.                                                   *                                                                   *
*                                                                              *
*    Portions of this code were originally provided by Paul Forrester          *
*    (paulf@apple.com) to the think-c internet mailing list in response to my  *
*    my question on how to do exactly what this code does.  I've added a       *
*    couple of features, such as the ability to handle aliased folders and     *
*    the programmer definable prompt.  I also cleaned and tightened the code,  *
*    stomped a couple of bugs, and packaged it up neatly.  Bunches of work,    *
*    but I learned A LOT about Standard File, the File Manager, the Dialog     *
*    Manager, and the Alias Manager.  I tried to include in the comments some  *
*    of the neat stuff I discovered in my hours of pouring over Inside Mac.    *
*    Hope you find it educational as well as useful.                           *
*******************************************************************************/

#if 0
#include <stdio.h>
#include <String.h>
#endif
#include <Script.h>
#include <Aliases.h>
#include <LowMem.h>
#include <StandardFile.h>
#include "StandardGetFolder.h"

static FileFilterYDUPP 	gFilterProc = nil;
static DlgHookYDUPP		gDlgHookProc = nil;


/*=============================================================================+
|                            Function Prototypes                               |
+=============================================================================*/
long GetSFCurDir(void);

short GetSFVRefNum(void);

pascal Boolean MyCustomGetDirectoryFileFilter(  CInfoPBPtr  myPB, 
                                                Ptr         myDataPtr);

void SetButtonTitle ( Handle    ButtonHdl, 
                      Str255    name, 
                      Rect      *ButtonRect);

pascal short MyCustomGetDirectoryDlogHook( short        item, 
                                           DialogPtr    theDialog,
                                           Ptr          myDataPtr);

char *StrCpy (char *s1, char *s2);

char *StrCat (char *s1, char *s2);                                      


/*=============================================================================+
|                               Resource IDs                                   |
+=============================================================================*/
#define rGetFolderButton            10
#define rGetFolderMessage           11
#define kFolderBit                  0x0010  
#define rGetFolderDialog            2008


/*=============================================================================+
|                             Global Variables                                 |
+=============================================================================*/
static  char    gCurrentSelectedFolder [256];


/*******************************************************************************
* StandardGetFolder                                                            *
*                                                                              *
*     The StandardGetFolder function. You pass it the point where you want the *
*     standard file dialog box drawn, the prompt to display above the file     *
*     list, and a pointer to an StandardFileReply record.                      *
*                                                                              *
*     Upon return, the sfFile field of the SFReply record contains the volume  *
*     reference number and directory ID that specify the folder the user       *
*     chose. It also passes back the name of the chosen folder.  The sfGood    *
*     field is set to true if the user chose a folder, or false if not.        *
*******************************************************************************/

void RealStandardGetFolder (    Point               where,
                            Str255              message,
                            StandardFileReply   *mySFReply)
                            
{
    SFTypeList              theTypeList;
    short                   numTypes;
    ProcPtr                 myModalFilter;
    short                   theItem;

    /*-------------------------------------------------------------------------+
    | Setting num types to -1 tells CustomGetFile to pass all files and        |
    | folders to the file filter function.                                     |
    +-------------------------------------------------------------------------*/
    numTypes = - 1;         

    /*-------------------------------------------------------------------------+
    | Copy the prompt to be displayed above the file list into the name field  |
    | of the SFReply record. When MyCustomGetDirectoryDlogHook is called for   |
    | the first time, it will use this info to draw the prompt.                |
    +-------------------------------------------------------------------------*/
    StrCpy ( (char *) mySFReply->sfFile.name, (char *) message);
    
    /*-------------------------------------------------------------------------+
    | Call CustomGetFile. Pass it a pointer to the file filter and dialog      |  
    | hook functions. Also pass a pointer to mySFReply in the user data field. |
    +-------------------------------------------------------------------------*/
    if( gFilterProc == nil )
    	{
    	gFilterProc = NewFileFilterYDProc( MyCustomGetDirectoryFileFilter );
    	gDlgHookProc = NewDlgHookYDProc( MyCustomGetDirectoryDlogHook );
    	}
    	
    CustomGetFile(  gFilterProc, 
                    numTypes,
                    theTypeList,
                    mySFReply, 
                    rGetFolderDialog, 
                    where,
                    gDlgHookProc, 
                    NULL, 
                    (short*)NULL,
                    NULL, 
                    (void *)(mySFReply));


}



void StandardGetFolder (    Point               where,
                            Str255              message,
                            StandardFileReply   *mySFReply)
	{
    CInfoPBRec              pb;
    OSErr                   err;
	
	RealStandardGetFolder( where, message, mySFReply );
	
	
    /*-------------------------------------------------------------------------+
    | Ok, now the reply record contains the volume reference number and the    |
    | name of the selected folder. We need to use PBGetCatInfo to get the      |    
    | directory ID of the selected folder.                                     |
    +-------------------------------------------------------------------------*/
    pb.hFileInfo.ioCompletion = NULL;
    pb.hFileInfo.ioNamePtr = mySFReply->sfFile.name;
    pb.hFileInfo.ioVRefNum = mySFReply->sfFile.vRefNum;
    pb.hFileInfo.ioFDirIndex = 0;
    pb.hFileInfo.ioDirID = mySFReply->sfFile.parID;

    err = PBGetCatInfoSync( &pb);
    
    /*-------------------------------------------------------------------------+
    | Insert your error handler here. I couldn't think of one so I left it     |
    | empty. Works fine without it.                                            |
    +-------------------------------------------------------------------------*/
    if (  err != noErr);
    
    /*-------------------------------------------------------------------------+
    | Copy the directory ID of the selected folder to the sfFile field of the  |
    | SFReply record.                                                          |
    +-------------------------------------------------------------------------*/
    mySFReply->sfFile.parID = pb.dirInfo.ioDrDirID;
	}

/*******************************************************************************
* MyCustomGetDirectoryFileFilter                                               *
*                                                                              *
*     This is the file filter passed to CustomGetFile. It passes folders only. *
*******************************************************************************/
pascal Boolean
MyCustomGetDirectoryFileFilter( CInfoPBPtr  myPB, 
                                Ptr         myDataPtr )
{
    return( ! (myPB->hFileInfo.ioFlAttrib & kFolderBit ) );
}


/*******************************************************************************
* MyCustomGetDirectoryDlogHook                                                 *
*                                                                              *
*     This function lets us process item hits in the GetFolderDialog.  We're   *
*     only interested if the user hit the selectFolder button. We pass all     *
*     other item hits back to ModalDialog.                                     *
*******************************************************************************/

pascal short MyCustomGetDirectoryDlogHook(  short       item, 
                                            DialogPtr   theDialog, 
                                            Ptr         myDataPtr )
{

    WindowPeek      dlgPeek;
    Str255          selectedName;
    CInfoPBRec      pb;
    short           MyCustomGetDirectoryDlogHook;
    OSErr           err;
    short           itemType;           
    Rect            itemRect;                                   
    Handle          itemHandle;
    Boolean         isAlias,
                    isFolder;

    StandardFileReply           *mySFRPtr;

    
    
    /*-------------------------------------------------------------------------+
    | Set the return value to default to the item that was passed in.          |
    +-------------------------------------------------------------------------*/
    MyCustomGetDirectoryDlogHook = item;

    /*-------------------------------------------------------------------------+
    | CustomGet calls dialog hook for both main and subsidiary dialog boxes.   |
    | Make sure that dialog record indicates that this is the main GetFolder   |
    | dialog.                                                                  |
    +-------------------------------------------------------------------------*/
    dlgPeek = (WindowPeek)(theDialog);
    if ( (OSType)(dlgPeek->refCon) == sfMainDialogRefCon )
    {
        /*---------------------------------------------------------------------+
        | Get a handle to the select folder button, in case we need to change  |
        | the label.                                                           |
        +---------------------------------------------------------------------*/
        GetDialogItem(theDialog, rGetFolderButton, &itemType, &itemHandle, &itemRect);

        /*---------------------------------------------------------------------+
        | If this is the first time the dialog hook has been called...         |
        +---------------------------------------------------------------------*/
        if ( item == sfHookFirstCall )
        {
            /*-----------------------------------------------------------------+
            | Set the prompt displayed above the file list...                  |
            +-----------------------------------------------------------------*/
            GetDialogItem ( theDialog, rGetFolderMessage, &itemType, &itemHandle, 
                        &itemRect );
            mySFRPtr = (StandardFileReply *)(myDataPtr);
            SetDialogItemText ( itemHandle, mySFRPtr->sfFile.name );

            /*-----------------------------------------------------------------+
            | And the name of the currently selected folder in the select      |
            | folder button.                                                   |
            +-----------------------------------------------------------------*/
            pb.hFileInfo.ioCompletion = NULL;
            pb.hFileInfo.ioNamePtr = (StringPtr)selectedName;
            pb.hFileInfo.ioVRefNum = GetSFVRefNum();
            pb.hFileInfo.ioDirID = GetSFCurDir();
            pb.hFileInfo.ioFDirIndex = -1;
            err = PBGetCatInfoSync( &pb);

            /*-----------------------------------------------------------------+
            | Note that this error return is important! When the dialog hook   |
            | is called for the first time, Super Boomerang (and possibly      |
            | Norton directory assistance aren't finished doing their          |
            | rebounting, so the values returned by GetSFVRefNum and           |
            | GetSFCurDir may not be valid, and hence PBGetCatInfo will return |
            | an error.  That one took me a while to figure out.               |
            +-----------------------------------------------------------------*/
            if ( err != noErr )
            {
                return (MyCustomGetDirectoryDlogHook);
            }
            
            GetDialogItem(theDialog, rGetFolderButton, &itemType, &itemHandle, 
                     &itemRect);
            SetButtonTitle( itemHandle, selectedName, &itemRect);
        }
        
        else
        {
            /*-----------------------------------------------------------------+
            | Cast myDataPtr back to a SFReply pointer.                        |
            +-----------------------------------------------------------------*/
            mySFRPtr = (StandardFileReply *)( myDataPtr );


            /*-----------------------------------------------------------------+
            | If the selected folder is an alias, resolve it. isFolder will    |
            | be set to true if a folder or aliased folder is selected.        |
            +-----------------------------------------------------------------*/
            ResolveAliasFile (&(mySFRPtr->sfFile), TRUE, &isFolder, &isAlias);
            if ( (isAlias) && (isFolder) )
                StrCpy( (char *)selectedName, (char *)mySFRPtr->sfFile.name );
                
            /*-----------------------------------------------------------------+
            | If the selected item is a folder or volume, just copy the name   |
            | into selectedName...                                             |
            +-----------------------------------------------------------------*/
            else if (( mySFRPtr->sfIsFolder) || (mySFRPtr->sfIsVolume) )
                StrCpy( (char *)selectedName, (char *)mySFRPtr->sfFile.name );

            /*-----------------------------------------------------------------+
            | Otherwise, copy the name of the selected item's parent directory |
            | into selectedName.                                               |
            +-----------------------------------------------------------------*/
            else
            {
                pb.hFileInfo.ioCompletion = NULL;
                pb.hFileInfo.ioNamePtr = (StringPtr)selectedName;
                pb.hFileInfo.ioVRefNum = mySFRPtr->sfFile.vRefNum;
                pb.hFileInfo.ioDirID = mySFRPtr->sfFile.parID;
                pb.hFileInfo.ioFDirIndex = -1;
                err = PBGetCatInfoSync( &pb);
                if ( err != noErr)
                    return (MyCustomGetDirectoryDlogHook);
            }
            
            /*-----------------------------------------------------------------+
            | If the selected folder has changed since the last call to this   |
            | dialog hook function, re-draw the button with the new selected   |
            | folder name.                                                     |
            +-----------------------------------------------------------------*/
            if ( !EqualString( selectedName, (StringPtr)gCurrentSelectedFolder, 
                                FALSE, FALSE ) ) 
                SetButtonTitle(itemHandle, selectedName, &itemRect);

            /*-----------------------------------------------------------------+
            | If the user clicked the select folder button, force a cancel and |
            | set the sfGood field of the Reply record to true.                |
            +-----------------------------------------------------------------*/
            if (item == rGetFolderButton)
            {
                MyCustomGetDirectoryDlogHook = sfItemCancelButton;
                mySFRPtr->sfGood = TRUE;
            }
                
        }
    }
    
    return  (MyCustomGetDirectoryDlogHook );
}



/*******************************************************************************
* SetButtonTitle                                                               *
*                                                                              *
*     Whenever the selected folder is changed, SetButtonTitle is called to     *
*     redraw the get folder button.  Pass it a handle to the button, the new   *
*     string to be drawn in the button, and a pointer to the rect the button   *
*     is drawn within.                                                         *
*******************************************************************************/
void SetButtonTitle(    Handle      ButtonHdl, 
                        Str255      name, 
                        Rect        *ButtonRect )
{
    short   resultCode;
    short   width;
    char    TmpStr[ 256 ];

    StrCpy( gCurrentSelectedFolder, (char*) name );
    
    /*-------------------------------------------------------------------------+
    | Find the width left over in the button after drawing the word 'Select'   |
    | the quotation marks. Truncate the new name to this length.               |
    +-------------------------------------------------------------------------*/
    width = (ButtonRect->right - ButtonRect->left) -
            (StringWidth((StringPtr)"\pSelect \"\"") +
             CharWidth('J'));
    
    resultCode = TruncString(width, name, smTruncEnd );
    if ( resultCode < 0 );
    
    /*-------------------------------------------------------------------------+
    | Redraw the button.                                                       |
    +-------------------------------------------------------------------------*/
    StrCpy ( TmpStr, (char *)"\pSelect \"");
    StrCat ( TmpStr, (char *)name);
    StrCat ( TmpStr, (char *)"\p\"");
    SetControlTitle((ControlHandle)(ButtonHdl), (StringPtr)TmpStr );
    ValidRect(ButtonRect);
}


/*******************************************************************************
* GetSFCurDir, GetSFVRefNum                                                    *
*                                                                              *
* The following set of routines are used to access a couple of low memory      *
* globals that are necessary when extending Standard File.  One example is     *
* trying to get the current directory while in a file filter.  These routines  *
* were used to bottleneck all the low memory usage.  If the system one day     *
* supports them with a trap call, then we can easily update these routines.    *
*******************************************************************************/

long GetSFCurDir()
{
    return( LMGetCurDirStore() );
}

short GetSFVRefNum()
{
    return( - LMGetSFSaveDisk() );
}


/*******************************************************************************
* StrCpy                                                                       *
*                                                                              *
*     Just like strcpy except that it takes pascal strings as arguments.       *                                                                               *
*******************************************************************************/
char *StrCpy( char *s1, char *s2 )
{
    BlockMove( s2, s1, 1+*s2);
    return s1;
}



/*******************************************************************************
* StrCat                                                                       *
*                                                                              *
*     Just like strcat except that it takes pascal strings as arguments.       *                                                                               *
*******************************************************************************/
char *StrCat( char *s1, char *s2 )
{
    BlockMove( s2+1, s1+*s1+1, *s2);
    *s1 += *s2;
    return s1;
}




