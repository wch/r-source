/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2003  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef __AQUA_DATAMANGER_
#define __AQUA_DATAMANGER_


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <Defn.h>

#include <R.h>
#include <R_ext/Boolean.h>
#include <R_ext/Rdynload.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "Print.h"

#ifdef HAVE_AQUA
#define __DEBUGGING__

# include <Carbon/Carbon.h>

#include <limits.h>


#ifndef kDataBrowserListViewAppendColumn
#define kDataBrowserListViewAppendColumn ULONG_MAX
#endif

extern  void	Raqua_ProcessEvents(void);

static void ConfigureDataManager(ControlRef);
static void CreateDataManager(WindowRef, ControlRef*);
static ControlRef GetDataBrowserFromWindow(WindowRef window);
static void InstallDataManagerCallbacks(ControlRef);

static pascal OSStatus dmGetSetItemData(ControlRef browser, 
    DataBrowserItemID itemID, DataBrowserPropertyID property, 
    DataBrowserItemDataRef itemData, Boolean changeValue);
    
static pascal void dmItemNotification(
	ControlRef browser, 
	DataBrowserItemID itemID, 
	DataBrowserItemNotification message);


pascal OSStatus dmEventHandler(EventHandlerCallRef, EventRef, void*);
	
	
Boolean OpenDataManager(void);
void CloseDataManager(void);


WindowRef DataManagerWindow = NULL;
ControlRef DataManagerControl = NULL;


#define MaxRows  65000
#define MaxCols  65000 
int NumOfDSets=0;
DataBrowserItemID *DSetID;
Boolean *InstallDSet;

extern bool		DataManagerFinished;

extern TXNControlTag	RReadOnlyTag[];
extern TXNControlData   RReadOnlyData[];

extern TXNObject	RConsoleInObject;


SEXP dsets, pkg, desc;


#ifndef max
#define max(a, b) (((a)>(b))?(a):(b))
#endif
#ifndef min
#define min(a, b) (((a)<(b))?(a):(b))
#endif



Rect dmBounds = { 400, 400, 600, 800 };

void EmptyDataManager(void);

static const EventTypeSpec	RCloseWinEvent[] = 
{
        { kEventClassWindow, kEventWindowClose }        
};

extern OSStatus DoCloseHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );




Boolean OpenDataManager(void)
{
	OSStatus err = noErr;
	int i,j,k;
        Rect	bounds;
         EventTypeSpec dmEvents[] = {
		{ kEventClassCommand,	kEventCommandProcess }, 
		{ kEventClassCommand,	kEventCommandUpdateStatus }, 
		{ kEventClassWindow,	kEventWindowGetIdealSize },
		{ kEventClassWindow,	kEventWindowBoundsChanged }, 
		{ kEventClassWindow,	kEventWindowGetClickActivation }
			};
  
    
     CreateNewWindow(kDocumentWindowClass,  kWindowStandardHandlerAttribute |
            kWindowStandardDocumentAttributes, &dmBounds, &DataManagerWindow);
	RepositionWindow (DataManagerWindow,  NULL, kWindowCenterOnMainScreen);


    if(DataManagerWindow == NULL)
     return(FALSE);
     
     	
     InstallWindowEventHandler(DataManagerWindow, 
    		NewEventHandlerUPP(dmEventHandler), 
    		sizeof(dmEvents)/sizeof(EventTypeSpec), dmEvents, NULL, NULL);
     
     InstallWindowEventHandler( DataManagerWindow, NewEventHandlerUPP(DoCloseHandler), 
                                          1,
                                          RCloseWinEvent, (void *)DataManagerWindow, NULL);
                    
 
     SetWindowTitleWithCFString(DataManagerWindow, CFSTR("RAqua: Datasets in installed packages"));
     
    
    /* Create the DataBrowser */
     CreateDataManager(DataManagerWindow, &DataManagerControl);
    /* Configure the DataBrowser */
	
    ConfigureDataManager(DataManagerControl);
    err = SetDataBrowserTarget(DataManagerControl, 1);
    
    /* Set the keyboard focus */
	SetKeyboardFocus(DataManagerWindow, DataManagerControl, kControlDataBrowserPart);
	
	/* Store DB as a window property */
    SetWindowProperty(DataManagerWindow,'RMAC', 'DMAN',sizeof(DataManagerControl), &DataManagerControl);

	InstallDataManagerCallbacks(DataManagerControl);

    
    
	if(DataManagerControl == NULL){              
	 CloseDataManager();	 
	 return;
	}
	
	
   NumOfDSets = LENGTH(dsets);	
   DSetID = (DataBrowserItemID*)malloc(NumOfDSets * sizeof(DataBrowserItemID));
   InstallDSet = (Boolean*)malloc(NumOfDSets * sizeof(Boolean));
   for(i=1;i<=NumOfDSets;i++){
    DSetID[i-1] = i;
    InstallDSet[i-1] = false;
   }
  AddDataBrowserItems(DataManagerControl, kDataBrowserNoItem, 
				NumOfDSets, DSetID, kDataBrowserItemNoProperty);

  

    ShowWindow(DataManagerWindow);

    GetPortBounds(GetWindowPort(DataManagerWindow), &bounds);
    SizeControl(DataManagerControl, bounds.right - bounds.left, bounds.bottom - bounds.top);


}



void EmptyDataManager(void)
{
  RemoveDataBrowserItems (DataManagerControl, kDataBrowserNoItem, 0, 
          NULL, kDataBrowserItemNoProperty);
} 

void CloseDataManager(void)
{
    if(DataManagerWindow){
     if( GetWindowBounds(DataManagerWindow,kWindowStructureRgn,&dmBounds) != noErr)
      SetRect(&dmBounds, 400, 400, 600, 800);
    }
    else 
     SetRect(&dmBounds, 400, 400, 600, 800);

    DisposeWindow(DataManagerWindow);
    DataManagerWindow = NULL;
    
    
    DataManagerControl = NULL;
    if(DSetID)
     free(DSetID);
   } 

static void CreateDataManager(WindowRef window, ControlRef *browser)
{
    Rect bounds;
    Boolean frameAndFocus = false;
    
    /* Create a DataBrowser */
    GetWindowBounds(window, kWindowContentRgn, &bounds);

    bounds.top = bounds.left = 0;
    bounds.right = bounds.right - bounds.left;
    bounds.bottom = bounds.bottom - bounds.top;
    
    CreateDataBrowserControl(window, 
    	&bounds, kDataBrowserListView, browser);

    /* Turn off DB's focus frame */
	SetControlData(
		*browser, kControlNoPart, 
		kControlDataBrowserIncludesFrameAndFocusTag,
		sizeof(frameAndFocus), &frameAndFocus);
}

char *dmNames[] = { "Load It", "Dataset", "Package", "Description"};


static void ConfigureDataManager(ControlRef browser)
{
	Rect insetRect;
	DataBrowserViewStyle viewStyle;
        int i;
        
	GetDataBrowserViewStyle(browser, &viewStyle);
	
	GetDataBrowserScrollBarInset(browser, &insetRect);
	
	insetRect.right = 16 - 1;
	SetDataBrowserScrollBarInset(browser, &insetRect);

	switch (viewStyle)
	{
		case kDataBrowserListView:
		{	DataBrowserListViewColumnDesc columnDesc;
			
                        columnDesc.headerBtnDesc.titleOffset = 0;
			
			columnDesc.headerBtnDesc.version = 
				kDataBrowserListViewLatestHeaderDesc;
				
			columnDesc.headerBtnDesc.btnFontStyle.flags	= 
				kControlUseFontMask | kControlUseJustMask;
			
			columnDesc.headerBtnDesc.btnContentInfo.contentType = kControlNoContent;
			
                        columnDesc.headerBtnDesc.minimumWidth = 50;
			columnDesc.headerBtnDesc.maximumWidth = 150;
			
			columnDesc.headerBtnDesc.btnFontStyle.just = teFlushLeft;
			
			columnDesc.headerBtnDesc.btnFontStyle.font = kControlFontViewSystemFont;
			columnDesc.headerBtnDesc.btnFontStyle.style = normal;
		
			columnDesc.propertyDesc.propertyType = kDataBrowserCheckboxType;
                        
                        
                        columnDesc.propertyDesc.propertyID = 10000;
			
                        
                        columnDesc.headerBtnDesc.titleString = CFStringCreateWithCString(
				CFAllocatorGetDefault(),dmNames[0], kCFStringEncodingMacRoman);
                        
                        columnDesc.propertyDesc.propertyFlags = kDataBrowserPropertyIsMutable;
			AddDataBrowserListViewColumn(browser, 
				&columnDesc, kDataBrowserListViewAppendColumn);
		
                        
                        columnDesc.headerBtnDesc.titleOffset = 0;
			
			columnDesc.headerBtnDesc.version = 
				kDataBrowserListViewLatestHeaderDesc;
				
			columnDesc.headerBtnDesc.btnFontStyle.flags	= 
				kControlUseFontMask | kControlUseJustMask;
			
			columnDesc.headerBtnDesc.btnContentInfo.contentType = kControlNoContent;
			
                        columnDesc.headerBtnDesc.minimumWidth = 30;
			columnDesc.headerBtnDesc.maximumWidth = 200;
			
			columnDesc.headerBtnDesc.btnFontStyle.just = teFlushLeft;
			
			columnDesc.headerBtnDesc.btnFontStyle.font = kControlFontViewSystemFont;
			columnDesc.headerBtnDesc.btnFontStyle.style = normal;
		
			columnDesc.propertyDesc.propertyType = kDataBrowserTextType;
                        
			columnDesc.propertyDesc.propertyFlags = kDataBrowserPropertyIsMutable |									  		kDataBrowserListViewDefaultColumnFlags;
			for(i=1;i<=3;i++){
                     
			columnDesc.propertyDesc.propertyID = i*1000;
			if(i==3){
                         columnDesc.headerBtnDesc.minimumWidth = 100;
 			 columnDesc.headerBtnDesc.maximumWidth = 600;
                        }
		
		
                        
                        columnDesc.headerBtnDesc.titleString = CFStringCreateWithCString(
				CFAllocatorGetDefault(),dmNames[i], kCFStringEncodingMacRoman);
                        
                        
			AddDataBrowserListViewColumn(browser, 
				&columnDesc, kDataBrowserListViewAppendColumn);
			
                        
			SetDataBrowserListViewDisclosureColumn(browser, columnDesc.propertyDesc.propertyID, false);
                        }
                        
		}	break;
	}
       }


static ControlRef GetDataBrowserFromWindow(WindowRef window)
{
	ControlRef browser = NULL;
	
	if (window != NULL)
		GetWindowProperty(window, 'RMAC', 
		'DMAN', sizeof(browser), NULL, &browser);
	
	return browser;
}


void InstallDataManagerCallbacks(ControlRef browser)
{
    DataBrowserCallbacks myCallbacks;
    

    myCallbacks.version = kDataBrowserLatestCallbacks;
    InitDataBrowserCallbacks(&myCallbacks);
    
    myCallbacks.u.v1.itemDataCallback = 
        NewDataBrowserItemDataUPP(dmGetSetItemData);
	

    myCallbacks.u.v1.itemNotificationCallback = 
        NewDataBrowserItemNotificationUPP(dmItemNotification);
     

    SetDataBrowserCallbacks(browser, &myCallbacks);
}







static pascal OSStatus dmGetSetItemData(ControlRef browser, 
    DataBrowserItemID itemID, DataBrowserPropertyID property, 
    DataBrowserItemDataRef itemData, Boolean changeValue)
{
#pragma unused (browser)
	Str255 pascalString;
	OSStatus err = noErr;
        CFStringRef text,value;
	SEXP tmp;
        int i,col,row;
        char buf[1000];

	  if(property>=1000){
          row = itemID;
         }

        if(!changeValue) {
       
         if(property>=1000 & property<10000 & row>0 & row<=NumOfDSets){  
            switch(property)
            {
          
                case 1000:
                    strcpy( buf, CHAR(STRING_ELT(dsets, row-1)) );
                break;
                case 2000:
                    strcpy( buf, CHAR(STRING_ELT(pkg, row-1)) );
                break;
                case 3000:
                    strcpy( buf, CHAR(STRING_ELT(desc, row-1)) );
                break;
                default:
                break;
          }       
          CopyCStringToPascal(buf,pascalString);
          text = CFStringCreateWithPascalString(CFAllocatorGetDefault(), pascalString, kCFStringEncodingMacRoman);
          if(text){
			err = SetDataBrowserItemDataText(itemData, text); 
			CFRelease(text);
			text = NULL;
		   } 
         }
                   
         
        } else {
        
        if(property == 10000){
         InstallDSet[row-1] = !InstallDSet[row-1];
         err = SetDataBrowserItemDataBooleanValue(itemData, InstallDSet[row-1]);
	 SetWindowModified(DataManagerWindow, true);
        }
        else 
        err = errDataBrowserPropertyNotSupported;
	}
        
	return err;
}
 
 

 
 static pascal void dmItemNotification(
	ControlRef browser, 
	DataBrowserItemID itemID, 
	DataBrowserItemNotification message)
{
	UInt32 i;
	UInt32 numSelectedItems;
	UInt16 j;
        OSStatus err;
        DataBrowserItemID tempid;
         CFMutableStringRef text;
         char buf[1000];
               
	switch (message)
	{
		case kDataBrowserItemSelected:
		{	Handle handle = NewHandle(0);
			GetDataBrowserItems(browser, 
				kDataBrowserNoItem, true, kDataBrowserItemIsSelected, handle);
		    numSelectedItems = GetHandleSize(handle)/sizeof(DataBrowserItemID);
            	}	
                break;
                
          
	}
}

pascal OSStatus dmEventHandler(
	EventHandlerCallRef a, EventRef inEvent, void*b)
{
    OSStatus result = noErr;

	switch (GetEventClass(inEvent))
	{
    	default:
    	{	result = eventNotHandledErr;
    	}	break;
		

		
	    case kEventClassWindow:
		{
		    WindowRef window = NULL;
			GetEventParameter(inEvent, 
		        kEventParamDirectObject, typeWindowRef, 
		        NULL, sizeof(window), NULL, &window);

		    switch (GetEventKind(inEvent))
		 	{
		    	default:
		    	{	result = eventNotHandledErr;
		    	}	break;
				
				
                        case kEventWindowBoundsChanged:
		    	{	ControlRef browser = GetDataBrowserFromWindow(window);
		    		Rect bounds; GetPortBounds(GetWindowPort(window), &bounds);
		        	SizeControl(browser, bounds.right - bounds.left, bounds.bottom - bounds.top);
		    	}	break;
				
							
				 
			}
		}	break;
	}
	   
    return result;
}


SEXP Raqua_datamanger(SEXP call, SEXP op, SEXP args, SEXP env)
{
  char *vm;
  SEXP ans; 
  int i;
  checkArity(op, args);

  vm = vmaxget();
  dsets = CAR(args); args = CDR(args);
  pkg = CAR(args); args = CDR(args);
  desc = CAR(args); args = CDR(args);
  
  
  

   
  if(!isString(dsets) | !isString(pkg) | !isString(desc) )
	errorcall(call, "invalid arguments");
   
  TXNSetTXNObjectControls(RConsoleInObject, false, 1, RReadOnlyTag, RReadOnlyData);
  DataManagerFinished = false;
  OpenDataManager();
  while(!DataManagerFinished)
   Raqua_ProcessEvents();

  PROTECT(ans = NEW_LOGICAL(NumOfDSets));
  for(i=1;i<=NumOfDSets;i++)
   LOGICAL(ans)[i-1] = InstallDSet[i-1];
   
  vmaxset(vm);
  
  if(InstallDSet)
     free(InstallDSet);
  UNPROTECT(1);
  return ans;
}





#endif  /* HAVE_AQUA */

#endif /* __AQUA_DATAMANGER_ */



