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

#ifndef __AQUA_HSEARCH_
#define __AQUA_HSEARCH_


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

extern void	Raqua_ProcessEvents(void);

static void ConfigureHelpSearchBrowser(ControlRef);
static void CreateHelpSearchBrowser(WindowRef, ControlRef*);
static ControlRef GetDataBrowserFromWindow(WindowRef window);
static void InstallHelpSearchBrowserCallbacks(ControlRef);

static pascal OSStatus hsGetSetItemData(ControlRef browser, 
    DataBrowserItemID itemID, DataBrowserPropertyID property, 
    DataBrowserItemDataRef itemData, Boolean changeValue);
    
static pascal void hsItemNotification(
	ControlRef browser, 
	DataBrowserItemID itemID, 
	DataBrowserItemNotification message);


pascal OSStatus hsEventHandler(EventHandlerCallRef, EventRef, void*);
	
	
Boolean OpenHelpSearchBrowser(void);
void CloseHelpSearchBrowser(void);


WindowRef HelpSearchBrowserWindow = NULL;
ControlRef HelpSearchBrowserControl = NULL;


#define MaxRows  65000
#define MaxCols  65000 
static int NumOfDSets=0;
static DataBrowserItemID *DSetID;
static Boolean *InstallDSet;

extern bool		HelpSearchBrowserFinished;

extern TXNControlTag	RReadOnlyTag[];
extern TXNControlData   RReadOnlyData[];

extern TXNObject	RConsoleInObject;


static SEXP dsets, pkg, desc, wtitle;


#ifndef max
#define max(a, b) (((a)>(b))?(a):(b))
#endif
#ifndef min
#define min(a, b) (((a)<(b))?(a):(b))
#endif



Rect hsBounds = { 400, 400, 600, 800 };

void EmptyHelpSearchBrowser(void);

static const EventTypeSpec	RCloseWinEvent[] = 
{
        { kEventClassWindow, kEventWindowClose }        
};

extern OSStatus DoCloseHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );




Boolean OpenHelpSearchBrowser(void)
{
	OSStatus err = noErr;
	int i,j,k;
        Rect	bounds;
         EventTypeSpec hsEvents[] = {
		{ kEventClassCommand,	kEventCommandProcess }, 
		{ kEventClassCommand,	kEventCommandUpdateStatus }, 
		{ kEventClassWindow,	kEventWindowGetIdealSize },
		{ kEventClassWindow,	kEventWindowBoundsChanged }, 
		{ kEventClassWindow,	kEventWindowGetClickActivation }
			};
  
    
     CreateNewWindow(kDocumentWindowClass,  kWindowStandardHandlerAttribute |
            kWindowStandardDocumentAttributes, &hsBounds, &HelpSearchBrowserWindow);

    if(HelpSearchBrowserWindow == NULL)
     return(FALSE);
     
     	
     InstallWindowEventHandler(HelpSearchBrowserWindow, 
    		NewEventHandlerUPP(hsEventHandler), 
    		sizeof(hsEvents)/sizeof(EventTypeSpec), hsEvents, NULL, NULL);
     
     InstallWindowEventHandler( HelpSearchBrowserWindow, NewEventHandlerUPP(DoCloseHandler), 
                                          1,
                                          RCloseWinEvent, (void *)HelpSearchBrowserWindow, NULL);
                    
 
     SetWindowTitleWithCFString(HelpSearchBrowserWindow,
				CFStringCreateWithCString(CFAllocatorGetDefault(), 
							  CHAR(STRING_ELT(wtitle,0)),
							  kCFStringEncodingMacRoman));
     
    
    /* Create the DataBrowser */
     CreateHelpSearchBrowser(HelpSearchBrowserWindow, &HelpSearchBrowserControl);
    /* Configure the DataBrowser */
	
    ConfigureHelpSearchBrowser(HelpSearchBrowserControl);
    err = SetDataBrowserTarget(HelpSearchBrowserControl, 1);
    
    /* Set the keyboard focus */
	SetKeyboardFocus(HelpSearchBrowserWindow, HelpSearchBrowserControl, kControlDataBrowserPart);
	
	/* Store DB as a window property */
    SetWindowProperty(HelpSearchBrowserWindow,'RMAC', 'HSBR',sizeof(HelpSearchBrowserControl), &HelpSearchBrowserControl);

	InstallHelpSearchBrowserCallbacks(HelpSearchBrowserControl);

    
    
	if(HelpSearchBrowserControl == NULL){              
	 CloseHelpSearchBrowser();	 
	 return;
	}
	
	
   NumOfDSets = LENGTH(dsets);	
   DSetID = (DataBrowserItemID*)malloc(NumOfDSets * sizeof(DataBrowserItemID));
   InstallDSet = (Boolean*)malloc(NumOfDSets * sizeof(Boolean));
   for(i=1;i<=NumOfDSets;i++){
    DSetID[i-1] = i;
    InstallDSet[i-1] = false;
   }
  AddDataBrowserItems(HelpSearchBrowserControl, kDataBrowserNoItem, 
				NumOfDSets, DSetID, kDataBrowserItemNoProperty);

  

    ShowWindow(HelpSearchBrowserWindow);

    GetPortBounds(GetWindowPort(HelpSearchBrowserWindow), &bounds);
    SizeControl(HelpSearchBrowserControl, bounds.right - bounds.left, bounds.bottom - bounds.top);


}



void EmptyHelpSearchBrowser(void)
{
  RemoveDataBrowserItems (HelpSearchBrowserControl, kDataBrowserNoItem, 0, 
          NULL, kDataBrowserItemNoProperty);
} 

void CloseHelpSearchBrowser(void)
{
    if(HelpSearchBrowserWindow){
     if( GetWindowBounds(HelpSearchBrowserWindow,kWindowStructureRgn,&hsBounds) != noErr)
      SetRect(&hsBounds, 400, 400, 600, 800);
    }
    else 
     SetRect(&hsBounds, 400, 400, 600, 800);

    DisposeWindow(HelpSearchBrowserWindow);
    HelpSearchBrowserWindow = NULL;
    
    
    HelpSearchBrowserControl = NULL;
    if(DSetID)
     free(DSetID);
   } 

static void CreateHelpSearchBrowser(WindowRef window, ControlRef *browser)
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

char *hsNames[] = { "Show help?", "Topic", "Package", "Description"};


static void ConfigureHelpSearchBrowser(ControlRef browser)
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
				CFAllocatorGetDefault(),hsNames[0], kCFStringEncodingMacRoman);
                        
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
				CFAllocatorGetDefault(),hsNames[i], kCFStringEncodingMacRoman);
                        
                        
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
		'HSBR', sizeof(browser), NULL, &browser);
	
	return browser;
}


void InstallHelpSearchBrowserCallbacks(ControlRef browser)
{
    DataBrowserCallbacks myCallbacks;
    

    myCallbacks.version = kDataBrowserLatestCallbacks;
    InitDataBrowserCallbacks(&myCallbacks);
    
    myCallbacks.u.v1.itemDataCallback = 
        NewDataBrowserItemDataUPP(hsGetSetItemData);
	

    myCallbacks.u.v1.itemNotificationCallback = 
        NewDataBrowserItemNotificationUPP(hsItemNotification);
     

    SetDataBrowserCallbacks(browser, &myCallbacks);
}







static pascal OSStatus hsGetSetItemData(ControlRef browser, 
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
          err = SetDataBrowserItemDataText(itemData, text); 
          CFRelease(text); 
         }
                   
         
        } else {
        
        if(property == 10000){
	  InstallDSet[row-1] = !InstallDSet[row-1];
	  err = SetDataBrowserItemDataBooleanValue(itemData, InstallDSet[row-1]);
	  SetWindowModified(HelpSearchBrowserWindow, true);
        }
        else 
	  err = errDataBrowserPropertyNotSupported;
	}
        
	return err;
}
 
 

 
 static pascal void hsItemNotification(
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

pascal OSStatus hsEventHandler(
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


SEXP Raqua_helpsearchbrowser(SEXP call, SEXP op, SEXP args, SEXP env)
{
  char *vm;
  SEXP ans; 
  int i;
  checkArity(op, args);

  vm = vmaxget();
  dsets = CAR(args); args = CDR(args);
  pkg = CAR(args); args = CDR(args);
  desc = CAR(args); args = CDR(args);
  wtitle = CAR(args);
  
  

   
  if(!isString(dsets) | !isString(pkg) | !isString(desc) )
	errorcall(call, "invalid arguments");
   
  TXNSetTXNObjectControls(RConsoleInObject, false, 1, RReadOnlyTag, RReadOnlyData);
  HelpSearchBrowserFinished = false;
  OpenHelpSearchBrowser();
  while(!HelpSearchBrowserFinished)
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

#endif /* __AQUA_HSEARCH_ */



