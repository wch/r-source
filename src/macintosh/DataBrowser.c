/*
	File:		DataBrowser.c
	
	Contains:	Basic Data Browser sample, Carbon API
	
	Very experimental code. Mac only.

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

// --------------------------------------------------------------------

#define MaxObjects 256

char *str[MaxObjects];

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
void CloseDataBrowser(WindowRef window);

Boolean isBrowserOpen = false;



#define MaxItems  1000

/*
  Ad esempio ci sono 4 oggetti
  w vettore
  x lista di 3 elementi (a,b,c)
  y lista di 2 elementi (d,e) in cui d e' una lista di 3 (h,i,l)
  z vettore

  in tutto  1  <-  w
            3  <-  x
            6  <-  y
            1  <-  z
            ---------
           11  Totale
            
oggetto	ID			subitems	Root	Container
w		1			0			*
x		2			3			*		*
y		3			2			*		*
z		4			0			*

da x 

a		5			0
b		6			0
c		7			0

da y
d		8			3					*
e		9			0

da d
h		10			0
i		11			0
l		12			0

*/

/* qui vanno messi gli ID degli oggetti iniziali */
int NumOfRoots=0;

int RootItems[] = {1,2,3,4};

Boolean Container[MaxItems];
int ItemsPerContainer[MaxItems];
int *SubItemsID[MaxItems];

int CurrentID=0;

void InitContainers(void);
void SetSubItems(int i);

void InitContainers(void){

	int i,j;
	
	NumOfRoots = sizeof(RootItems)/sizeof(int);
	
	CurrentID = NumOfRoots;
	
	for(i = 0; i < MaxItems; i++){

     Container[i] = false;
     ItemsPerContainer[i] = 0; 
     SubItemsID[i] = NULL;    
	}

/* defines the containers */	
	 Container[2] = true;
     ItemsPerContainer[2] = 3;     
     SetSubItems(2);   
    

	 Container[3] = true;
     ItemsPerContainer[3] = 2;     
     SetSubItems(3);   
    
	
	
	 Container[8] = true;
     ItemsPerContainer[8] = 3; 
     SetSubItems(8);   
    
    
}



void SetSubItems(int i)
{
  int j;
  
  SubItemsID[i] = malloc(sizeof(int)*ItemsPerContainer[i]);
  for(j=0; j<ItemsPerContainer[i]; j++){
        CurrentID++; 
        SubItemsID[i][j] = CurrentID;
       }
}

void ClearContainers(void);

void ClearContainers(void)
{
	int i;

	for(i = 0; i < MaxItems; i++){
     if(SubItemsID[i])
      free(SubItemsID[i]);
    }
}

// --------------------------------------------------------------------
void OpenDataBrowser(void)
{
	WindowRef window = NULL;
    ControlRef browser = NULL;
	OSStatus err = noErr;
	char mystr[300];
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
    
     if(isBrowserOpen)
     return;
     
    CreateNewWindow(kDocumentWindowClass,  kWindowStandardHandlerAttribute |
            kWindowStandardDocumentAttributes, &bounds, &window);

    
    if(window == NULL)
     return;
    
    for(i = 0; i < MaxObjects; i++)
     { 
       sprintf(mystr,"it = %d",i); 
       str[i] = malloc(sizeof(mystr+1));
       strcpy(str[i],mystr);
     }
     
     
	
     InstallWindowEventHandler(window, 
    		NewEventHandlerUPP(BrowserEventHandler), 
    		sizeof(windowEvents)/sizeof(EventTypeSpec), windowEvents, NULL, NULL);
 
     SetWindowTitleWithCFString(window, CFSTR("R Workspace Browser"));
    
    // Create the DataBrowser
    CreateDataBrowser(window, &browser);
	InstallDataBrowserCallbacks(browser);

	if(browser == NULL){
      
      for(i = 0; i < MaxObjects; i++)
       if(str[i]) 
        free(str[i]);
        
	 DisposeWindow(window);
	 return;
	}
	
    // Configure the DataBrowser
	ConfigureDataBrowser(browser);
    err = SetDataBrowserTarget(browser, 1);
    
    // Set the keyboard focus
	SetKeyboardFocus(window, browser, kControlDataBrowserPart);
	
	// Store DB as a window property
    SetWindowProperty(window,
        kMyCreator, kMyDataBrowser,
        sizeof(browser), &browser);

   
   InitContainers();
   
   AddDataBrowserItems(browser, kDataBrowserNoItem, 
				NumOfRoots, RootItems, kDataBrowserItemNoProperty);


    // Show window & run
    ShowWindow(window);

    isBrowserOpen = true;
}


void CloseDataBrowser(WindowRef window)
{
	ControlRef browser;
	int i;
	
	if(window == NULL)
	 return;
	 
	browser = GetDataBrowserFromWindow(window);
		   
		     		
  /*  const UInt16 IDs[] = { 
		     			kSettingsDialog, kCustomizeDialog };
		     		
		     		for (UInt16 i = 0; i < sizeof(IDs); i++)
		     		{
			     		const UInt16 ID = IDs[i];
			     		DialogRef customizer = 
			     			GetCustomizer(browser, ID);
			     		
			     		if (customizer != NULL)
			     			DisposeDialog(customizer);
		     		}
	*/	     		
		     		DisposeWindow(window);
	for(i = 0; i < MaxObjects; i++)
       if(str[i]) 
        free(str[i]);
    
    ClearContainers();
          
    isBrowserOpen = false;
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
			                                        kDataBrowserListViewSelectionColumn | 
													kDataBrowserListViewDefaultColumnFlags;
			
			columnDesc.headerBtnDesc.btnContentInfo.contentType = kControlContentIconRef;
		
			columnDesc.headerBtnDesc.minimumWidth = 30;
			columnDesc.headerBtnDesc.maximumWidth = 200;
			
			columnDesc.headerBtnDesc.btnFontStyle.just = teCenter;
			
			columnDesc.headerBtnDesc.btnFontStyle.font = kControlFontViewSystemFont;
			columnDesc.headerBtnDesc.btnFontStyle.style = normal;
			
			columnDesc.headerBtnDesc.titleString = CFStringCreateWithPascalString(
				CFAllocatorGetDefault(), "\pObject", kCFStringEncodingMacRoman);
			
			AddDataBrowserListViewColumn(browser, 
				&columnDesc, kDataBrowserListViewAppendColumn),
			
			// Add the Type column
			
			columnDesc.propertyDesc.propertyID = kTypeColumn;
			
			columnDesc.headerBtnDesc.btnFontStyle.just = teFlushRight;

			columnDesc.propertyDesc.propertyFlags = kDataBrowserPropertyIsMutable | 
													kDataBrowserListViewDefaultColumnFlags;

			columnDesc.headerBtnDesc.titleString = CFStringCreateWithPascalString(
				CFAllocatorGetDefault(), "\pType", kCFStringEncodingMacRoman);
			
			AddDataBrowserListViewColumn(browser, 
				&columnDesc, kDataBrowserListViewAppendColumn);
			
			// Add the Size column
			
			columnDesc.propertyDesc.propertyID = kSizeColumn;
			columnDesc.propertyDesc.propertyType = kDataBrowserTextType;
			columnDesc.propertyDesc.propertyFlags = kDataBrowserListViewSelectionColumn | 
													kDataBrowserListViewDefaultColumnFlags;
			
		
			columnDesc.headerBtnDesc.minimumWidth = 30;
			columnDesc.headerBtnDesc.maximumWidth = 100;
			columnDesc.headerBtnDesc.btnFontStyle.just = teCenter;
			
			columnDesc.headerBtnDesc.titleString =CFStringCreateWithPascalString(
				CFAllocatorGetDefault(), "\pSize", kCFStringEncodingMacRoman);
		
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
			/*GetIndString(s1, 128, itemOneID % 5 + 1);
			GetIndString(s2, 128, itemTwoID % 5 + 1);
			
			compareResult = CompareString(s1, s2, NULL);
			*/
			compareResult = strcmp(str[itemOneID],str[itemTwoID]);
			if (compareResult < 0) return true;
			else if (compareResult > 0) return false;
			else return Compare(itemOneID, itemTwoID, '????');
		}	break;
		
	/*	case kColorColumn:
		{	Str255 s1, s2;
			::GetIndString(s1, 129, itemOneID % 5 + 1);
			::GetIndString(s2, 129, itemTwoID % 5 + 1);
			compareResult = ::CompareString(s1, s2, NULL);
			
			if (compareResult < 0) return true;
			else if (compareResult > 0) return false;
			else return Compare(itemOneID, itemTwoID, 'foo');
		}	break;

		case kProgressBarColumn:
		{	SInt32 val1 = (itemOneID % 5) * 20;
			SInt32 val2 = (itemTwoID % 5) * 20;
			
			if (val1 < val2) return true;
			else if (val1 > val2) return false;
			else return Compare(itemOneID, itemTwoID, '????');
		}	break;
*/		
		default:
		{	return itemOneID < itemTwoID;
		}	break;
	}
}

static Boolean AppendString(
	Str255 ioDestString, ConstStr255Param inSourceString)
{
	SInt16 oldSize = ioDestString[0];
	
	if (oldSize + inSourceString[0] <= 255)
	{
		BlockMoveData(&(inSourceString[1]), 
			&(ioDestString[oldSize+1]), inSourceString[0]);
		ioDestString[0] += inSourceString[0];
		return true;
	}
	
	return false;
}

// --------------------------------------------------------------------
static void GenerateString(DataBrowserItemID itemID, 
	DataBrowserPropertyID property, StringPtr string)
{
	Str255 numString; 
	Boolean itemXProperty = property == kTypeColumn || 
		property == kDataBrowserItemSelfIdentityProperty;
		
	string[0] = 0; 
	NumToString(itemID, numString);
	
	if (itemXProperty) AppendString(string, "\pItem ");
		
	AppendString(string, numString);
	
	if (!itemXProperty)
	{
		numString[0] = sizeof(property);
		BlockMoveData(&property, &(numString[1]), sizeof(property));
		AppendString(string, "\p, "); 
		AppendString(string, numString);
	}
}

 
 


static pascal OSStatus MyGetSetItemData(ControlRef browser, 
    DataBrowserItemID itemID, DataBrowserPropertyID property, 
    DataBrowserItemDataRef itemData, Boolean changeValue)
{
#pragma unused (browser)
	Str255 pascalString;
	OSStatus err = noErr;
	char mstr[300];
	char mstr2[300];
	
	if (!changeValue) 
	 switch (property)
	{
		
		case kObjectColumn:
		{
			CFStringRef text;
			CopyCStringToPascal(str[itemID],pascalString);
			text = CFStringCreateWithPascalString(
				CFAllocatorGetDefault(), pascalString, kCFStringEncodingMacRoman);
			err = SetDataBrowserItemDataText(itemData, text); 
		CFRelease(text);
		}	
		break;

		case kTypeColumn:
		{	CFStringRef text;
		    GenerateString(itemID, property, pascalString);
			 text = CFStringCreateWithPascalString(
				CFAllocatorGetDefault(), pascalString, kCFStringEncodingMacRoman);
			err = SetDataBrowserItemDataText(itemData, text); 
			CFRelease(text);
		}	
		break;
				

		case kSizeColumn:
		{
			CFStringRef text=NULL;
			sprintf(mstr,"size=%5d",itemID);	
			CopyCStringToPascal(mstr,pascalString);
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
		{	err = SetDataBrowserItemDataBooleanValue(itemData, Container[itemID]);
		}	break;
		
		
		case kDataBrowserItemParentContainerProperty:
		{	err = SetDataBrowserItemDataItemID(itemData, (itemID-1) / ItemsPerContainer[itemID]);
		}	break;



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
		{	// Generate some valid itemIDs
			//myItems = (DataBrowserItemID *)malloc(sizeof(int)*ItemsPerContainer[itemID]);
			//for (i = 0; i < ItemsPerContainer[itemID]; i++)
				//myItems[i] = itemID  + i + 1;
			
			AddDataBrowserItems(browser, itemID, ItemsPerContainer[itemID], SubItemsID[itemID], kObjectColumn);
			
			{	// Set up variable height rows
				Boolean variableHeightRows;
				GetDataBrowserTableViewGeometry(
					browser, NULL, &variableHeightRows);
					
				if (variableHeightRows)
					for ( j = 0; j < ItemsPerContainer[itemID]; j++)
						SetDataBrowserTableViewItemRowHeight(
							browser, SubItemsID[itemID][j], 20 + (SubItemsID[itemID][j] - 1) % 10 * 3);
			}
		// if(myItems)
		  //free(myItems);
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
                  	CloseDataBrowser(WindowRef window);
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