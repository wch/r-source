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

#ifndef __AQUA_PACKAGEMANGER_
#define __AQUA_PACKAGEMANGER_


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





static void ConfigurePackageManager(ControlRef);
static void CreatePackageManager(WindowRef, ControlRef*);
static ControlRef GetDataBrowserFromWindow(WindowRef window);
static void InstallPackageManagerCallbacks(ControlRef);

static pascal OSStatus pmGetSetItemData(ControlRef browser, 
    DataBrowserItemID itemID, DataBrowserPropertyID property, 
    DataBrowserItemDataRef itemData, Boolean changeValue);
    
static pascal void pmItemNotification(
	ControlRef browser, 
	DataBrowserItemID itemID, 
	DataBrowserItemNotification message);


pascal OSStatus pmEventHandler(EventHandlerCallRef, EventRef, void*);
	
	
Boolean OpenPackageManager(void);
void ClosePackageManager(void);


WindowRef PackageManagerWindow = NULL;
ControlRef PackageManagerControl = NULL;


#define MaxRows  65000
#define MaxCols  65000 
int NumOfAllPkgs=0;
DataBrowserItemID *PNameID;
Boolean *LoadThese;

extern bool	PackageManagerFinished;
extern void	Raqua_ProcessEvents(void);

extern TXNControlTag	RReadOnlyTag[];
extern TXNControlData   RReadOnlyData[];

extern TXNObject	RConsoleInObject;


SEXP pkgname, pkgstatus, pkgdesc;


#ifndef max
#define max(a, b) (((a)>(b))?(a):(b))
#endif
#ifndef min
#define min(a, b) (((a)<(b))?(a):(b))
#endif



Rect pmBounds = { 400, 400, 600, 800 };

void EmptyPackageManager(void);

static const EventTypeSpec	RCloseWinEvent[] = 
{
        { kEventClassWindow, kEventWindowClose }        
};

extern OSStatus DoCloseHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );




Boolean OpenPackageManager(void)
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
            kWindowStandardDocumentAttributes, &pmBounds, &PackageManagerWindow);

    if(PackageManagerWindow == NULL)
     return(FALSE);
     
     	
     InstallWindowEventHandler(PackageManagerWindow, 
    		NewEventHandlerUPP(pmEventHandler), 
    		sizeof(dmEvents)/sizeof(EventTypeSpec), dmEvents, NULL, NULL);
     
     InstallWindowEventHandler( PackageManagerWindow, NewEventHandlerUPP(DoCloseHandler), 
                                          1,
                                          RCloseWinEvent, (void *)PackageManagerWindow, NULL);
                    
 
     SetWindowTitleWithCFString(PackageManagerWindow, CFSTR("Raqua: Load/Unload Packages"));
     
     
    
    /* Create the DataBrowser */
     CreatePackageManager(PackageManagerWindow, &PackageManagerControl);
    /* Configure the DataBrowser */
	
    ConfigurePackageManager(PackageManagerControl);
    err = SetDataBrowserTarget(PackageManagerControl, 1);
    
    /* Set the keyboard focus */
	SetKeyboardFocus(PackageManagerWindow, PackageManagerControl, kControlDataBrowserPart);
	
	/* Store DB as a window property */
    SetWindowProperty(PackageManagerWindow,'RMAC', 'PMAN',sizeof(PackageManagerControl), &PackageManagerControl);

	InstallPackageManagerCallbacks(PackageManagerControl);

    
    
	if(PackageManagerControl == NULL){              
	 ClosePackageManager();	 
	 return;
	}
	
	
   NumOfAllPkgs = LENGTH(pkgname);	
   PNameID = (DataBrowserItemID*)malloc(NumOfAllPkgs * sizeof(DataBrowserItemID));
   LoadThese = (Boolean*)malloc(NumOfAllPkgs * sizeof(Boolean));
   for(i=1;i<=NumOfAllPkgs;i++){
    PNameID[i-1] = i;
    LoadThese[i-1] = LOGICAL(pkgstatus)[i-1] ? true : false;
   }
  AddDataBrowserItems(PackageManagerControl, kDataBrowserNoItem, 
				NumOfAllPkgs, PNameID, kDataBrowserItemNoProperty);

  

    ShowWindow(PackageManagerWindow);

    GetPortBounds(GetWindowPort(PackageManagerWindow), &bounds);
    SizeControl(PackageManagerControl, bounds.right - bounds.left, bounds.bottom - bounds.top);


}



void EmptyPackageManager(void)
{
  RemoveDataBrowserItems (PackageManagerControl, kDataBrowserNoItem, 0, 
          NULL, kDataBrowserItemNoProperty);
} 

void ClosePackageManager(void)
{
    if(PackageManagerWindow){
     if( GetWindowBounds(PackageManagerWindow,kWindowStructureRgn,&pmBounds) != noErr)
      SetRect(&pmBounds, 400, 400, 600, 800);
    }
    else 
     SetRect(&pmBounds, 400, 400, 600, 800);

    DisposeWindow(PackageManagerWindow);
    PackageManagerWindow = NULL;
    
    
    PackageManagerControl = NULL;
    if(PNameID)
     free(PNameID);
   } 

static void CreatePackageManager(WindowRef window, ControlRef *browser)
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

char *pmNames[] = { "Load", "(current)", "Package", "Description"};


static void ConfigurePackageManager(ControlRef browser)
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
				CFAllocatorGetDefault(),pmNames[0], kCFStringEncodingMacRoman);
                        
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
				CFAllocatorGetDefault(),pmNames[i], kCFStringEncodingMacRoman);
                        
                        
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
		'PMAN', sizeof(browser), NULL, &browser);
	
	return browser;
}


void InstallPackageManagerCallbacks(ControlRef browser)
{
    DataBrowserCallbacks myCallbacks;
    

    myCallbacks.version = kDataBrowserLatestCallbacks;
    InitDataBrowserCallbacks(&myCallbacks);
    
    myCallbacks.u.v1.itemDataCallback = 
        NewDataBrowserItemDataUPP(pmGetSetItemData);
	

    myCallbacks.u.v1.itemNotificationCallback = 
        NewDataBrowserItemNotificationUPP(pmItemNotification);
     

    SetDataBrowserCallbacks(browser, &myCallbacks);
}







static pascal OSStatus pmGetSetItemData(ControlRef browser, 
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
       
         if(property>=1000 & property<10000 & row>0 & row<=NumOfAllPkgs){  
            switch(property)
            {
          
                case 2000:
                    strcpy( buf, CHAR(STRING_ELT(pkgname, row-1)) );
                break;
                case 1000:
		  if (LOGICAL(pkgstatus)[row-1])
                    strcpy( buf, "Yes" );
		  else 
		    strcpy(buf, "No");
                break;
                case 3000:
                    strcpy( buf, CHAR(STRING_ELT(pkgdesc, row-1)) );
                break;
                default:
                break;
          }       
          CopyCStringToPascal(buf,pascalString);
          text = CFStringCreateWithPascalString(CFAllocatorGetDefault(), pascalString, kCFStringEncodingMacRoman);
          err = SetDataBrowserItemDataText(itemData, text); 
          CFRelease(text); 
         }
                   
         
	 if(property == 10000){
	   err = SetDataBrowserItemDataBooleanValue(itemData, LOGICAL(pkgstatus)[row-1]); 
	 }  
        } else {
        
        if(property == 10000){
         LoadThese[row-1] = !LoadThese[row-1];
         err = SetDataBrowserItemDataBooleanValue(itemData, LoadThese[row-1]);
	 SetWindowModified(PackageManagerWindow, true);
        }
        else 
        err = errDataBrowserPropertyNotSupported;
	}
        
	return err;
}
 
 

 
 static pascal void pmItemNotification(
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

pascal OSStatus pmEventHandler(
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


SEXP Raqua_packagemanger(SEXP call, SEXP op, SEXP args, SEXP env)
{
  char *vm;
  SEXP ans; 
  int i;
  checkArity(op, args);

  vm = vmaxget();
  pkgstatus = CAR(args); args = CDR(args);
  pkgname = CAR(args); args = CDR(args);
  pkgdesc = CAR(args); args = CDR(args);
  
  
  

   
  if(!isString(pkgname) | !isLogical(pkgstatus) | !isString(pkgdesc) )
	errorcall(call, "invalid arguments");
   
  TXNSetTXNObjectControls(RConsoleInObject, false, 1, RReadOnlyTag, RReadOnlyData);
  PackageManagerFinished = false;
  OpenPackageManager();
  while(!PackageManagerFinished)
    Raqua_ProcessEvents();
    
  PROTECT(ans = NEW_LOGICAL(NumOfAllPkgs));
  for(i=1;i<=NumOfAllPkgs;i++)
   LOGICAL(ans)[i-1] = LoadThese[i-1];
   
  vmaxset(vm);
  
  if(LoadThese)
     free(LoadThese);
  UNPROTECT(1);
  return ans;
}







#endif  /* HAVE_AQUA */

#endif /* __AQUA_PACKAGEMANGER_ */



