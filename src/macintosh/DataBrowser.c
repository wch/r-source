/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file DataBrowser.c
 *  Copyright (C) 1998-1999  Ross Ihaka
 *                2002       Stefano M. Iacus and the R core team
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
 *  Experimental but working code of a graphical workspace browser.
 *  This is Macintosh only. Can be adapted for the Darwin version
 *  too without problems.
 *  Today, Aug 4 2002, S.M. Iacus
*/

#ifdef __APPLE_CC__
#include <Carbon/Carbon.h>
#else
#ifndef __CARBON__
#include <Carbon.h>
#endif
#endif

// ANSI Headers
#include <limits.h>

#include <RIntf.h>


#include <R.h>
#include <R_ext/Mathlib.h>
#include <R_ext/Boolean.h>
#include <R_ext/Rdynload.h>
#include <Rdefines.h>
#include <Rinternals.h>

// --------------------------------------------------------------------





static void ConfigureDataBrowser(ControlRef);
static void CreateDataBrowser(WindowRef, ControlRef*);
static ControlRef GetDataBrowserFromWindow(WindowRef window);
static void InstallDataBrowserCallbacks(ControlRef);

static pascal OSStatus MyGetSetItemData(ControlRef browser, 
    DataBrowserItemID itemID, DataBrowserPropertyID property, 
    DataBrowserItemDataRef itemData, Boolean changeValue);
    
static pascal void MyItemNotification(
	ControlRef browser, 
	DataBrowserItemID itemID, 
	DataBrowserItemNotification message);
pascal OSStatus BrowserEventHandler(EventHandlerCallRef, EventRef, void*);

static pascal Boolean MyItemComparison(
	ControlRef browser, DataBrowserItemID itemOneID, 
	DataBrowserItemID itemTwoID, DataBrowserPropertyID sortProperty);
	
void OpenDataBrowser(void);
void CloseDataBrowser(void);
void FreeBrowserStuff(void);

Boolean isBrowserOpen = false;
WindowRef BrowserWindow = NULL;
ControlRef WSpaceBrowser = NULL;
Rboolean isEventHandlerOn = FALSE;

#define MaxItems  2500

/*
  How works the workspace browser? Suppose you have 4 objects
  in the workspace. The should be prepeared as follows:

  w vector
  x list of 3 objects
  y list of 2 object (d,e)
  z vector

  number of 
  total items  1  <-  w
             3+1  <-  x
             1+2  <-  y
            1  <-  z
            ---------
               9  Totale
            
object	ID			subitems	IsRoot	IsContainer
w		1			0			*
x		2			3			*		*
y		3			2			*		*
z		4			0			*

from x 

a		5			0
b		6			0
c		7			0

from y
d		8			0					
e		9			0


*/

int NumOfRoots=0;
int *RootItems = NULL; 
int *SubItemsID[MaxItems];
int CurrentID=0;

void InitContainers(void);
void SetSubItems(int i);





/*
  id : integer, id number in the browser
  isroot : boolean, this id is a root item
  iscont : boolean, is container?
  numofit : integer, number of items
  name   : char, name of objects
  type   : type, class or mode
  objsize: can be dim, length or number of levels 
*/

/* The following are local copies of the above */

int *IDNum;              /* id          */
Rboolean *IsRoot;        /* isroot      */
Rboolean *IsContainer;   /* iscontainer */
int *NumberOfItems;      /* numofit     */
char **Names;             /* name        */
char **Types;             /* type        */
char **Sizes;             /* objsize     */
int NumOfID = 0;                /* length of the vectors    */
                                /* We do not check for this */ 
 
SEXP do_wsbrowser(SEXP call, SEXP op, SEXP args, SEXP env);

SEXP do_wsbrowser(SEXP call, SEXP op, SEXP args, SEXP env)
{
  int i;
   SEXP id, isroot, iscont, numofit;
  SEXP name, type, objsize;
  char *vm;
   
  NumOfID =0;
   
  FreeBrowserStuff();
    
  checkArity(op, args);

  vm = vmaxget();
  id = CAR(args); args = CDR(args);
  isroot = CAR(args); args = CDR(args);
  iscont = CAR(args); args = CDR(args);
  numofit = CAR(args); args = CDR(args);
  name = CAR(args); args = CDR(args);
  type = CAR(args); args = CDR(args);
  objsize = CAR(args); 

  if(!isInteger(id)) {
   CloseDataBrowser();
   errorcall(call,"`id' must be integer");
   }
   
  NumOfID = LENGTH(id);
  if(NumOfID > MaxItems)
   NumOfID = MaxItems;

   
  if(!isString(name)){
    CloseDataBrowser();
	errorcall(call, "invalid objects' name");
  }
    
  if(!isString(type)){
    CloseDataBrowser();
    errorcall(call, "invalid objects' type");
   }
   
  if(!isString(objsize)){
    CloseDataBrowser();
	errorcall(call, "invalid objects' size");
  } 
  if(!isLogical(isroot)){
   CloseDataBrowser();
   errorcall(call, "invalid `isroot' definition");
  }
  
  if(!isLogical(iscont)){
    CloseDataBrowser();
	errorcall(call, "invalid `iscont' definition");
  } 
  
  if(!isInteger(numofit)){
   CloseDataBrowser(); 
   errorcall(call,"`numofit' must be integer");
  }
   
  
   
  Names = (char**)malloc(NumOfID * sizeof(char*));
  Sizes = (char**)malloc(NumOfID * sizeof(char*));
  Types = (char**)malloc(NumOfID * sizeof(char*));
 
  IDNum = (int*)malloc(NumOfID * sizeof(int));
  IsRoot = (Rboolean*)malloc(NumOfID * sizeof(Rboolean));
  IsContainer = (Rboolean*)malloc(NumOfID * sizeof(Rboolean));
  NumberOfItems = (int*)malloc(NumOfID * sizeof(int));
 
   
  for(i=0; i<NumOfID; i++){

   if (!isNull(STRING_ELT(name, i)))
	    Names[i] = CHAR(STRING_ELT(name, i));
	else
	    Names[i] = CHAR(R_BlankString);

   if (!isNull(STRING_ELT(type, i)))
	    Types[i] = CHAR(STRING_ELT(type, i));
	else
	    Types[i] = CHAR(R_BlankString);

   if (!isNull(STRING_ELT(objsize, i)))
	    Sizes[i] = CHAR(STRING_ELT(objsize, i));
	else
	    Sizes[i] = CHAR(R_BlankString);  

	IDNum[i] = INTEGER(id)[i];
    NumberOfItems[i] = INTEGER(numofit)[i];
    IsRoot[i] = LOGICAL(isroot)[i];
    IsContainer[i] = LOGICAL(iscont)[i];
    
    
        
  }

    
  /*
   for(i=0; i<NumOfID; i++)
    Rprintf("\n id=%d names=%s, type=%s, size=%s, isroot=%d, iscont=%d, items=%d",IDNum[i],
     Names[i],Types[i],Sizes[i],IsRoot[i],IsContainer[i],NumberOfItems[i]);   
  */


  OpenDataBrowser();


  vmaxset(vm);
  return R_NilValue;

 
}










void InitContainers(void){

	int i,j,k=0;
	NumOfRoots = 0;
	
	for(i=0;i<NumOfID; i++)
	 if(IsRoot[i])
	  NumOfRoots++;
	if(RootItems) free(RootItems);
	  
	RootItems = (int)malloc(sizeof(int)*NumOfRoots);
	//Rprintf("\n NumOfRoots=%d",NumOfRoots);
	CurrentID = NumOfRoots;
	for(i = 0; i < NumOfID; i++){
     if(IsRoot[i]) { 
 //    Rprintf("\n RootItem[%d]=%d",k,IDNum[i]);
     RootItems[k] = IDNum[i]; k++;}     
     SubItemsID[i] = NULL;    
     if(IsContainer[i]){
      SubItemsID[i] = malloc(sizeof(int)*NumberOfItems[i]);
      for(j=0; j<NumberOfItems[i]; j++){
        CurrentID++; 
        SubItemsID[i][j] = CurrentID;
       }
     }   /* if */
	} /* for */
 //   Rprintf("\n exit init cont");
}




// --------------------------------------------------------------------
void OpenDataBrowser(void)
{
	OSStatus err = noErr;
	int i;
    Rect bounds = { 400, 400, 600, 800 };
    EventTypeSpec windowEvents[] = {
		{ kEventClassCommand,	kEventCommandProcess }, 
		{ kEventClassCommand,	kEventCommandUpdateStatus }, 
		{ kEventClassWindow,	kEventWindowClose }, 
		{ kEventClassWindow,	kEventWindowGetIdealSize },
		{ kEventClassWindow,	kEventWindowBoundsChanged }, 
		{ kEventClassWindow,	kEventWindowGetClickActivation }
			};
    
     
    if(BrowserWindow == NULL) 
    CreateNewWindow(kDocumentWindowClass,  kWindowStandardHandlerAttribute |
            kWindowStandardDocumentAttributes, &bounds, &BrowserWindow);

    if(BrowserWindow == NULL)
     return;
     
     	
    if(!isEventHandlerOn){
     InstallWindowEventHandler(BrowserWindow, 
    		NewEventHandlerUPP(BrowserEventHandler), 
    		sizeof(windowEvents)/sizeof(EventTypeSpec), windowEvents, NULL, NULL);
     isEventHandlerOn = TRUE;
     SetWindowTitleWithCFString(BrowserWindow, CFSTR("R Workspace Browser"));
    }
     
     
    
    // Create the DataBrowser
    if(WSpaceBrowser==NULL){
     CreateDataBrowser(BrowserWindow, &WSpaceBrowser);
	 InstallDataBrowserCallbacks(WSpaceBrowser);
    }
    
	if(WSpaceBrowser == NULL){              
	 /* rimuovere eventhandler?*/
	 CloseDataBrowser();	 
	 return;
	}
	
    // Configure the DataBrowser
	if(!isBrowserOpen)
	 ConfigureDataBrowser(WSpaceBrowser);
    err = SetDataBrowserTarget(WSpaceBrowser, 1);
    
    // Set the keyboard focus
	SetKeyboardFocus(BrowserWindow, WSpaceBrowser, kControlDataBrowserPart);
	
	// Store DB as a window property
    SetWindowProperty(BrowserWindow,
        kMyCreator, kMyDataBrowser,
        sizeof(WSpaceBrowser), &WSpaceBrowser);

   
   InitContainers();
   
   for(i=0;i<NumOfRoots;i++)
    //Rprintf("\n RootItem[%d]=%d",i,RootItems[i]);
    
   AddDataBrowserItems(WSpaceBrowser, kDataBrowserNoItem, 
				NumOfRoots, RootItems, kDataBrowserItemNoProperty);


    // Show window & run
    ShowWindow(BrowserWindow);

    isBrowserOpen = true;
}

void FreeBrowserStuff(void)
{    
    int i;

	for(i = 0; i < NumOfID; i++){
     if(SubItemsID[i])
      { free(SubItemsID[i]); SubItemsID[i] = NULL; }
    }

    NumOfID = 0;
    
    if(RootItems)
     { free(RootItems); RootItems = NULL; }

    if(Names)   { free(Names); Names = NULL; }
    if(Sizes)   { free(Sizes); Sizes = NULL; }
    if(Types)   { free(Types); Types = NULL; }
    if(IDNum)   { free(IDNum); IDNum = NULL; }
    if(IsRoot)  { free(IsRoot); IsRoot = NULL; }
    if(IsContainer)  { free(IsContainer); IsContainer = NULL; }
    if(NumberOfItems) { free(NumberOfItems); NumberOfItems = NULL; }

}

void CloseDataBrowser(void)
{
	int i;
	
    DisposeWindow(BrowserWindow);
    BrowserWindow = NULL;
    
	isEventHandlerOn = FALSE;
	
    FreeBrowserStuff();
          
    isBrowserOpen = false;
    WSpaceBrowser = NULL;
} 

// --------------------------------------------------------------------
static void CreateDataBrowser(WindowRef window, ControlRef *browser)
{
    Rect bounds;
    Boolean frameAndFocus = false;
    
    // Create a DataBrowser
    bounds.top = bounds.left = 0;
    bounds.right = 400; bounds.bottom = 200;
    CreateDataBrowserControl(window, 
    	&bounds, kDataBrowserListView, browser);

    // Turn off DB's focus frame
	SetControlData(
		*browser, kControlNoPart, 
		kControlDataBrowserIncludesFrameAndFocusTag,
		sizeof(frameAndFocus), &frameAndFocus);
}

// --------------------------------------------------------------------
	#ifndef kDataBrowserListViewAppendColumn
	#define kDataBrowserListViewAppendColumn ULONG_MAX
	#endif
static void ConfigureDataBrowser(ControlRef browser)
{
	Rect insetRect;
	DataBrowserViewStyle viewStyle;

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
			GetIconRef(kOnSystemDisk, kSystemIconsCreator, kGenericFolderIcon, 
				&columnDesc.headerBtnDesc.btnContentInfo.u.iconRef);
			
			// Add the Object column
			
			columnDesc.propertyDesc.propertyID = kObjectColumn;
			columnDesc.propertyDesc.propertyType = kDataBrowserTextType;
			columnDesc.propertyDesc.propertyFlags = kDataBrowserPropertyIsMutable | 
													kDataBrowserListViewDefaultColumnFlags;
			
			columnDesc.headerBtnDesc.btnContentInfo.contentType = kControlContentIconRef;
		
			columnDesc.headerBtnDesc.minimumWidth = 30;
			columnDesc.headerBtnDesc.maximumWidth = 200;
			
			columnDesc.headerBtnDesc.btnFontStyle.just = teFlushLeft;
			
			columnDesc.headerBtnDesc.btnFontStyle.font = kControlFontViewSystemFont;
			columnDesc.headerBtnDesc.btnFontStyle.style = normal;
			
			columnDesc.headerBtnDesc.titleString = CFStringCreateWithPascalString(
				CFAllocatorGetDefault(), "\pObject", kCFStringEncodingMacRoman);
			
			AddDataBrowserListViewColumn(browser, 
				&columnDesc, kDataBrowserListViewAppendColumn),
			
			// Add the Type column
			
			columnDesc.propertyDesc.propertyID = kTypeColumn;
			
			columnDesc.headerBtnDesc.btnFontStyle.just = teFlushLeft;

			columnDesc.propertyDesc.propertyFlags = kDataBrowserPropertyIsMutable | 
													kDataBrowserListViewDefaultColumnFlags;

			columnDesc.headerBtnDesc.titleString = CFStringCreateWithPascalString(
				CFAllocatorGetDefault(), "\pType", kCFStringEncodingMacRoman);
			
			AddDataBrowserListViewColumn(browser, 
				&columnDesc, kDataBrowserListViewAppendColumn);
			
			// Add the Size column
			
			columnDesc.propertyDesc.propertyID = kSizeColumn;
			columnDesc.propertyDesc.propertyType = kDataBrowserTextType;
			columnDesc.propertyDesc.propertyFlags = kDataBrowserPropertyIsMutable | 
													kDataBrowserListViewDefaultColumnFlags;
			
		
			columnDesc.headerBtnDesc.minimumWidth = 30;
			columnDesc.headerBtnDesc.maximumWidth = 200;
			columnDesc.headerBtnDesc.btnFontStyle.just = teFlushLeft;
			
			columnDesc.headerBtnDesc.titleString =CFStringCreateWithPascalString(
				CFAllocatorGetDefault(), "\pProperties", kCFStringEncodingMacRoman);
		
			AddDataBrowserListViewColumn(browser, 
				&columnDesc, kDataBrowserListViewAppendColumn);
			
			
			 
			 
			// Finish formatting the table
			//::SetDataBrowserTableViewRowHeight(browser, 10);
			//::SetDataBrowserSortProperty(browser, kFlavorColumn);
			SetDataBrowserListViewDisclosureColumn(browser, kObjectColumn, false);
			
			ReleaseIconRef(columnDesc.headerBtnDesc.btnContentInfo.u.iconRef);
		}	break;
		
		case kDataBrowserColumnView:
		{	//DataBrowserItemID path[] = { 48, 485 };
			//::SetDataBrowserColumnViewPath(browser, 2, path);
			//::SetDataBrowserTarget(browser, 50695);
		}	break;
	}
	
	//RestoreUserState();
}


static ControlRef GetDataBrowserFromWindow(WindowRef window)
{
	ControlRef browser = NULL;
	
	if (window != NULL)
		GetWindowProperty(window, kMyCreator, 
		kMyDataBrowser, sizeof(browser), NULL, &browser);
	
	return browser;
}


void InstallDataBrowserCallbacks(ControlRef browser)
{
    DataBrowserCallbacks myCallbacks;
    

    myCallbacks.version = kDataBrowserLatestCallbacks;
    InitDataBrowserCallbacks(&myCallbacks);
    
    myCallbacks.u.v1.itemDataCallback = 
        NewDataBrowserItemDataUPP(MyGetSetItemData);
	
	myCallbacks.u.v1.itemCompareCallback = 
		NewDataBrowserItemCompareUPP(MyItemComparison);

    myCallbacks.u.v1.itemNotificationCallback = 
        NewDataBrowserItemNotificationUPP(MyItemNotification);
     

    SetDataBrowserCallbacks(browser, &myCallbacks);
}



static pascal Boolean MyItemComparison(
	ControlRef browser, DataBrowserItemID itemOneID, 
	DataBrowserItemID itemTwoID, DataBrowserPropertyID sortProperty)
{
	SInt16 compareResult = 0;
	
	#define Compare(i1,i2,p) MyItemComparison(browser,i1,i2,p)
	
	switch (sortProperty)
	{
		case kObjectColumn:
		{	Str255 s1, s2;
		
		    //Rprintf("\ncompare: itemone=%d itemtwo=%d",itemOneID,itemTwoID);
		
			compareResult = strcmp(Names[itemOneID-1],Names[itemTwoID-1]);
			if (compareResult < 0) return true;
			else if (compareResult > 0) return false;
			else return Compare(itemOneID, itemTwoID, '????');
		}	break;
		
		default:
		{	return itemOneID < itemTwoID;
		}	break;
	}
}



static pascal OSStatus MyGetSetItemData(ControlRef browser, 
    DataBrowserItemID itemID, DataBrowserPropertyID property, 
    DataBrowserItemDataRef itemData, Boolean changeValue)
{
#pragma unused (browser)
	Str255 pascalString;
	OSStatus err = noErr;
	//char mstr[300];
//	char mstr2[300];
	
	if (!changeValue) 
	 switch (property)
	{
		
		case kObjectColumn:
		{
			CFStringRef text;
			//Rprintf("\nkobjcol ItemID=%d",itemID);
			CopyCStringToPascal(Names[itemID-1],pascalString);
			text = CFStringCreateWithPascalString(
				CFAllocatorGetDefault(), pascalString, kCFStringEncodingMacRoman);
			err = SetDataBrowserItemDataText(itemData, text); 
		CFRelease(text);
		}	
		break;

		case kTypeColumn:
		{	CFStringRef text;
			CopyCStringToPascal(Types[itemID-1],pascalString);
			 text = CFStringCreateWithPascalString(
				CFAllocatorGetDefault(), pascalString, kCFStringEncodingMacRoman);
			err = SetDataBrowserItemDataText(itemData, text); 
			CFRelease(text);
		}	
		break;
				

		case kSizeColumn:
		{
			CFStringRef text=NULL;
				CopyCStringToPascal(Sizes[itemID-1],pascalString);
			text = CFStringCreateWithPascalString(
				CFAllocatorGetDefault(), pascalString, kCFStringEncodingMacRoman);
			err = SetDataBrowserItemDataText(itemData, text); 
			CFRelease(text);
		}	
		break;
		

		
		case kDataBrowserItemIsEditableProperty:
		{	err = SetDataBrowserItemDataBooleanValue(itemData, true);
		}	break;
		
		case kDataBrowserItemIsContainerProperty:
		{	err = SetDataBrowserItemDataBooleanValue(itemData, IsContainer[itemID-1]);
		}	break;
		
		
/*		case kDataBrowserItemParentContainerProperty:
		{	err = SetDataBrowserItemDataItemID(itemData, (itemID-1) / NumberOfItems[itemID-1]);
		}	break;

*/

		default:
		{	err = errDataBrowserPropertyNotSupported;
		}	break;
	}
	else err = errDataBrowserPropertyNotSupported;
	
	return err;
}
 
 
 static pascal void MyItemNotification(
	ControlRef browser, 
	DataBrowserItemID itemID, 
	DataBrowserItemNotification message)
{
	UInt32 i;
	UInt32 numSelectedItems;
	UInt16 j;
	//DataBrowserItemID *myItems=NULL;
	
	switch (message)
	{
		case kDataBrowserItemSelected:
		{	Handle handle = NewHandle(0);
			GetDataBrowserItems(browser, 
				kDataBrowserNoItem, true, kDataBrowserItemIsSelected, handle);
		    numSelectedItems = GetHandleSize(handle)/sizeof(DataBrowserItemID);
		}	break;
		
		case kDataBrowserContainerOpened:
		{	
			
			AddDataBrowserItems(browser, itemID, NumberOfItems[itemID-1], SubItemsID[itemID-1], kObjectColumn);
			
			{	
				Boolean variableHeightRows;
				GetDataBrowserTableViewGeometry(
					browser, NULL, &variableHeightRows);
					
				if (variableHeightRows)
					for ( j = 0; j < NumberOfItems[itemID-1]; j++)
						SetDataBrowserTableViewItemRowHeight(
							browser, SubItemsID[itemID-1][j], 20 + (SubItemsID[itemID-1][j] - 1) % 10 * 3);
			}
		}	
		
		break;
		
	/*	case kDataBrowserSelectionSetChanged:
		{	::DrawOneControl(fPlacard);
		}	break;	*/
	}
}

pascal OSStatus BrowserEventHandler(
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
				
				
				 case kEventWindowClose:
		     	{
                  	CloseDataBrowser();
			 	}	break;

		    	case kEventWindowBoundsChanged:
		    	{	ControlRef browser = GetDataBrowserFromWindow(window);
		    		Rect bounds; GetPortBounds(GetWindowPort(window), &bounds);
		    	//	Rect bounds; GetWindowBounds(window, kWindowContentRgn, &bounds);
		        	SizeControl(browser, bounds.right - bounds.left, bounds.bottom - bounds.top);
		    	}	break;
				
							
				 
			}
		}	break;
	}
	   
    return result;
}