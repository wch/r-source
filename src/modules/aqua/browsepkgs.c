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

#ifndef __AQUA_BROWSEPKGS_
#define __AQUA_BROWSEPKGS_


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

typedef enum { UP, DOWN, LEFT, RIGHT } DE_DIRECTION;

typedef enum {UNKNOWNN, NUMERIC, CHARACTER} CellType;

/* EXPORTS : */
SEXP Raqua_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho);


static void ConfigureBrowsePkg(ControlRef);
static void CreateBrowsePkg(WindowRef, ControlRef*);
static ControlRef GetDataBrowserFromWindow(WindowRef window);
static void InstallBrowsePkgCallbacks(ControlRef);

static pascal OSStatus bpGetSetItemData(ControlRef browser, 
    DataBrowserItemID itemID, DataBrowserPropertyID property, 
    DataBrowserItemDataRef itemData, Boolean changeValue);
    
static pascal void bpItemNotification(
	ControlRef browser, 
	DataBrowserItemID itemID, 
	DataBrowserItemNotification message);


pascal OSStatus bpEventHandler(EventHandlerCallRef, EventRef, void*);
	
	
Boolean OpenBrowsePkg(void);
void CloseBrowsePkg(void);


Boolean isBrowsePkgOpen = false;
WindowRef BrowsePkgWindow = NULL;
ControlRef BrowsePkgControl = NULL;


#define MaxRows  65000
#define MaxCols  65000 

int NumOfPkgs=0;
DataBrowserItemID *PkgID;

Boolean *InstallPkg;


extern bool		BrowsePkgFinished;
extern void	Raqua_ProcessEvents(void);

extern TXNControlTag	RReadOnlyTag[];
extern TXNControlData   RReadOnlyData[];

extern TXNObject	RConsoleInObject;


static SEXP cpkgs, cvers, ivers, wwwhere, install_dflt, binary_dflt;

#ifndef max
#define max(a, b) (((a)>(b))?(a):(b))
#endif
#ifndef min
#define min(a, b) (((a)<(b))?(a):(b))
#endif



Rect bpBounds = { 400, 400, 600, 800 };

void EmptyBrowsePkg(void);

static const EventTypeSpec	RCloseWinEvent[] = 
{
        { kEventClassWindow, kEventWindowClose }        
};

extern OSStatus DoCloseHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );




Boolean OpenBrowsePkg(void)
{
	OSStatus err = noErr;
	int i,j,k;
	CFStringRef wintitle;
        Rect	bounds;
         EventTypeSpec bpEvents[] = {
		{ kEventClassCommand,	kEventCommandProcess }, 
		{ kEventClassCommand,	kEventCommandUpdateStatus }, 
		{ kEventClassWindow,	kEventWindowGetIdealSize },
		{ kEventClassWindow,	kEventWindowBoundsChanged }, 
		{ kEventClassWindow,	kEventWindowGetClickActivation }
			};
  
    
     CreateNewWindow(kDocumentWindowClass,  kWindowStandardHandlerAttribute |
            kWindowStandardDocumentAttributes, &bpBounds, &BrowsePkgWindow);
	RepositionWindow (BrowsePkgWindow,  NULL, kWindowCenterOnMainScreen);

    if(BrowsePkgWindow == NULL)
     return(FALSE);
     
     	
     InstallWindowEventHandler(BrowsePkgWindow, 
    		NewEventHandlerUPP(bpEventHandler), 
    		sizeof(bpEvents)/sizeof(EventTypeSpec), bpEvents, NULL, NULL);
     
     InstallWindowEventHandler( BrowsePkgWindow, NewEventHandlerUPP(DoCloseHandler), 
                                          1,
                                          RCloseWinEvent, (void *)BrowsePkgWindow, NULL);
                    
 
     wintitle = CFStringCreateWithCString( CFAllocatorGetDefault(), CHAR(STRING_ELT(wwwhere,0)),
					  kCFStringEncodingMacRoman);

     SetWindowTitleWithCFString(BrowsePkgWindow, wintitle);
     
     
    
    /* Create the DataBrowser */
     CreateBrowsePkg(BrowsePkgWindow, &BrowsePkgControl);
    /* Configure the DataBrowser */
	
    ConfigureBrowsePkg(BrowsePkgControl);
    err = SetDataBrowserTarget(BrowsePkgControl, 1);
    
    /* Set the keyboard focus */
	SetKeyboardFocus(BrowsePkgWindow, BrowsePkgControl, kControlDataBrowserPart);
	
	/* Store DB as a window property */
    SetWindowProperty(BrowsePkgWindow,'RMAC', 'PKGB',sizeof(BrowsePkgControl), &BrowsePkgControl);

	InstallBrowsePkgCallbacks(BrowsePkgControl);

    
    
	if(BrowsePkgControl == NULL){              
	 CloseBrowsePkg();	 
	 return;
	}
	
	
   NumOfPkgs = LENGTH(cpkgs);	
   PkgID = malloc(NumOfPkgs * sizeof(DataBrowserItemID));
   InstallPkg = malloc(NumOfPkgs * sizeof(Boolean));

   for(i=1;i<=NumOfPkgs;i++){
    PkgID[i-1] = i;
    if (LOGICAL(install_dflt)[i-1])
      InstallPkg[i-1] = true;
    else
      InstallPkg[i-1] = false;
   }


   AddDataBrowserItems(BrowsePkgControl, kDataBrowserNoItem, 
		       NumOfPkgs, PkgID, kDataBrowserItemNoProperty);

  

   ShowWindow(BrowsePkgWindow);
   
   GetPortBounds(GetWindowPort(BrowsePkgWindow), &bounds);
   SizeControl(BrowsePkgControl, bounds.right - bounds.left, bounds.bottom - bounds.top);
   

}



void EmptyBrowsePkg(void)
{
  RemoveDataBrowserItems (BrowsePkgControl, kDataBrowserNoItem, 0, 
          NULL, kDataBrowserItemNoProperty);
} 

void CloseBrowsePkg(void)
{
    if(BrowsePkgWindow){
     if( GetWindowBounds(BrowsePkgWindow,kWindowStructureRgn,&bpBounds) != noErr)
      SetRect(&bpBounds, 400, 400, 600, 800);
    }
    else 
     SetRect(&bpBounds, 400, 400, 600, 800);

    DisposeWindow(BrowsePkgWindow);
    BrowsePkgWindow = NULL;
    
    
    BrowsePkgControl = NULL;
    if(PkgID)
     free(PkgID);
   } 

static void CreateBrowsePkg(WindowRef window, ControlRef *browser)
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

char *bpNames[] = { "Install/Update",  "Package", 
		    "Available version", "Installed version"};


static void ConfigureBrowsePkg(ControlRef browser)
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
				CFAllocatorGetDefault(), bpNames[0], kCFStringEncodingMacRoman);
                        
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
                        
			columnDesc.propertyDesc.propertyFlags = kDataBrowserPropertyIsMutable |	
			  kDataBrowserListViewDefaultColumnFlags;

			for(i=1;i<=3;i++){
			  
			  columnDesc.propertyDesc.propertyID = i*1000;
			
			  //columnDesc.headerBtnDesc.btnContentInfo.contentType = kControlContentIconRef;
			  
                        
			  columnDesc.headerBtnDesc.titleString = CFStringCreateWithCString(
					   CFAllocatorGetDefault(),bpNames[i], kCFStringEncodingMacRoman);
			  
                        
			  AddDataBrowserListViewColumn(browser, &columnDesc,
						       kDataBrowserListViewAppendColumn);
			
                        
			  SetDataBrowserListViewDisclosureColumn(browser, 
								 columnDesc.propertyDesc.propertyID, false);
                        }
                        
                        	 
                                                
//			ReleaseIconRef(columnDesc.headerBtnDesc.btnContentInfo.u.iconRef);
		}	break;
	}
       }


static ControlRef GetDataBrowserFromWindow(WindowRef window)
{
	ControlRef browser = NULL;
	
	if (window != NULL)
		GetWindowProperty(window, 'RMAC', 
		'PKGB', sizeof(browser), NULL, &browser);
	
	return browser;
}


void InstallBrowsePkgCallbacks(ControlRef browser)
{
    DataBrowserCallbacks myCallbacks;
    

    myCallbacks.version = kDataBrowserLatestCallbacks;
    InitDataBrowserCallbacks(&myCallbacks);
    
    myCallbacks.u.v1.itemDataCallback = 
        NewDataBrowserItemDataUPP(bpGetSetItemData);
	

    myCallbacks.u.v1.itemNotificationCallback = 
        NewDataBrowserItemNotificationUPP(bpItemNotification);
     

    SetDataBrowserCallbacks(browser, &myCallbacks);
}







static pascal OSStatus bpGetSetItemData(ControlRef browser, 
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
       
         if(property>=1000 & property<10000 & row>0 & row<=NumOfPkgs){  
            switch(property)
            {
          
                case 1000:
                    strcpy( buf, CHAR(STRING_ELT(cpkgs, row-1)) );
                break;
                case 2000:
                    strcpy( buf, CHAR(STRING_ELT(cvers, row-1)) );
                break;
                case 3000:
                    strcpy( buf, CHAR(STRING_ELT(ivers, row-1)) );
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
         
	 if(property == 10000){
	   err = SetDataBrowserItemDataBooleanValue(itemData, InstallPkg[row-1]); 
	 }  
        } else {
        
        if(property == 10000){
         InstallPkg[row-1] = !InstallPkg[row-1];
         err = SetDataBrowserItemDataBooleanValue(itemData, InstallPkg[row-1]);
	 SetWindowModified(BrowsePkgWindow, true);
	}   else 
        err = errDataBrowserPropertyNotSupported;
	}
        
	return err;
}
 
 

 
 static pascal void bpItemNotification(
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

pascal OSStatus bpEventHandler(
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


SEXP Raqua_browsepkgs(SEXP call, SEXP op, SEXP args, SEXP env)
{
  char *vm;
  SEXP ans; 
  int i;
  int do_any=0;
  DialogItemIndex   userAction=kAlertStdAlertCancelButton;

  checkArity(op, args);

  vm = vmaxget();
  cpkgs = CAR(args); args = CDR(args);
  cvers = CAR(args); args = CDR(args);
  ivers = CAR(args); args = CDR(args);
  wwwhere = CAR(args); args=CDR(args);
  install_dflt = CAR(args); 
  
  
  if(!isString(cpkgs) | !isString(cvers) | !isString(ivers) | !isString(wwwhere) )
	errorcall(call, "invalid arguments");
  if(!isLogical(install_dflt))
    errorcall(call, "invalid arguments");
   
  TXNSetTXNObjectControls(RConsoleInObject, false, 1, RReadOnlyTag, RReadOnlyData);
  BrowsePkgFinished = false;
  OpenBrowsePkg();
  while(!BrowsePkgFinished)
    Raqua_ProcessEvents();

  PROTECT(ans =  NEW_LOGICAL(NumOfPkgs));

  
  for(i=1;i<=NumOfPkgs;i++){
    LOGICAL(ans)[i-1] = InstallPkg[i-1];
    do_any = do_any | InstallPkg[i-1];
  }
  

  if (do_any){
    userAction = YesOrNot("Install Packages", "Download and install these packages?","Ok","Cancel");
    if (userAction == kAlertStdAlertCancelButton){
      for(i=1;i<=NumOfPkgs;i++){
	LOGICAL(ans)[i-1] = 0;
      }
    }
    
  }
  

  vmaxset(vm);
  
  if(InstallPkg){
    free(InstallPkg);
    InstallPkg=NULL;
  }

  UNPROTECT(1);  /*ans*/
  return ans;
}





 

















#endif  /* HAVE_AQUA */

#endif /* __AQUA_BROWSEPKGS_ */



