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

#ifndef __AQUA_DATA_ENTRY__
#define __AQUA_DATA_ENTRY__


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <Defn.h>

#include <R.h>
#include <R_ext/Mathlib.h>
#include <R_ext/Boolean.h>
#include <R_ext/Rdynload.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "Print.h"

#ifdef HAVE_AQUA

# include <Carbon/Carbon.h>

#include <limits.h>


#ifndef kDataBrowserListViewAppendColumn
#define kDataBrowserListViewAppendColumn ULONG_MAX
#endif

typedef enum { UP, DOWN, LEFT, RIGHT } DE_DIRECTION;

typedef enum {UNKNOWNN, NUMERIC, CHARACTER} CellType;

/* EXPORTS : */
SEXP Raqua_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho);


static void ConfigureDataEntry(ControlRef);
static void CreateDataEntry(WindowRef, ControlRef*);
static ControlRef GetDataBrowserFromWindow(WindowRef window);
static void InstallDataEntryCallbacks(ControlRef);

static pascal OSStatus deGetSetItemData(ControlRef browser, 
    DataBrowserItemID itemID, DataBrowserPropertyID property, 
    DataBrowserItemDataRef itemData, Boolean changeValue);
    
static pascal void deItemNotification(
	ControlRef browser, 
	DataBrowserItemID itemID, 
	DataBrowserItemNotification message);
static pascal Boolean deItemComparison(
	ControlRef browser, DataBrowserItemID itemOneID, 
	DataBrowserItemID itemTwoID, DataBrowserPropertyID sortProperty);

pascal OSStatus deEventHandler(EventHandlerCallRef, EventRef, void*);
	
	
Boolean OpenDataEntry(void);
void CloseDataEntry(void);


Boolean isDataEntryOpen = false;
WindowRef DataEntryWindow = NULL;
ControlRef DataEntryControl = NULL;


#define MaxRows  65000
#define MaxCols  65000 
int NumOfRows=0;
int *RowID;

static SEXP work, names, lens;
static PROTECT_INDEX wpi, npi, lpi;
static SEXP ssNA_STRING;
static double ssNA_REAL;

/* Global variables needed for the graphics */
 
static int box_w;                       /* width of a box */
static int boxw[100];
static int box_h;                       /* height of a box */
static int windowWidth;                 /* current width of the window */
static int fullwindowWidth;
static int windowHeight;                /* current height of the window */
static int fullwindowHeight;
static int currentexp;                  /* boolean: whether an cell is active */
static int crow;                        /* current row */
static int ccol;                        /* current column */
static int nwide, nhigh;
static int colmax, colmin, rowmax, rowmin;
static int ndecimal;                    /* count decimal points */
static int ne;                          /* count exponents */
static int nneg;			/* indicate whether its a negative */
static int clength;                     /* number of characters currently entered */
static char buf[30];
static char *bufp;
static int bwidth;			/* width of the border */
static int hwidth;			/* width of header  */
static int text_offset;
 
static SEXP ssNewVector(SEXPTYPE, int);
extern bool		EditingFinished;

extern TXNControlTag	RReadOnlyTag[];
extern TXNControlData   RReadOnlyData[];

extern TXNObject	RConsoleInObject;

static char *get_col_name(int col);


static Rboolean newcol, CellModified;
static int nboxchars;
static int xmaxused, ymaxused;
static int box_coords[6];
static char copycontents[30] = "";
static int labdigs=4;
static char labform[6];


 
   
#ifndef max
#define max(a, b) (((a)>(b))?(a):(b))
#endif
#ifndef min
#define min(a, b) (((a)<(b))?(a):(b))
#endif
#define BOXW(x) (min(((x<100 && nboxchars==0)?boxw[x]:box_w), fullwindowWidth-boxw[0]-2*bwidth-2))

/*
  Underlying assumptions (for this version R >= 1.8.0)

  The data are stored in a list `work', with unused columns having
  NULL entries.  The names for the list are in `names', which should
  have a name for all displayable columns (up to xmaxused). 
  The *used* lengths of the columns are in `lens': this needs only be
  set for non-NULL columns.

  If the list was originally length(0), that should work with 
  0 pre-defined rows.  (It used to have 1 pre-defined numeric column.)

  All row and col numbers are 1-based.

  BDR May 2003
 */

/*
   The spreadsheet function returns a list of vectors. The types of
   these vectors can be specified by the user as can their names. It
   the names are specified they are set during initialization. The
   user can change these via a menu interface, they can also change
   the type.

   The vectors are created too long and if they need to be increased
   this is done by using the next higher power of 2. They start 100
   long. To cut them to the correct length for return you need to know
   the largest row number that was assigned to. LEVELS (sxpinfo.gp) is
   used to keep track of this, separately for each vector. Vectors are
   initialized to NA when they are created so that NA is returned for
   any cell that was not set by the user.  So that coercion back and
   forth maintains values of ssNA_REAL and ssNA_STRING I have set
   ssNA_STRING to be coerceVector(ssNA_REAL), very weird but easy.

   In Macintosh we needed to call the main event loop to get
   events. This ensures that the spreadsheet interacts well with the
   other windows. Under X windows we let the window manager handle
   those sorts of details.

 */

static char *menu_label[] =
{
    "Real",
    "Character",
    "Change Name",
};

/*
   ssNewVector is just an interface to allocVector but it lets us
   set the fields to NA. We need to have a special NA for reals and
   strings so that we can differentiate between uninitialized elements
   in the vectors and user supplied NA's; hence ssNA_REAL and ssNA_STRING
 */

static SEXP ssNewVector(SEXPTYPE type, int vlen)
{
    SEXP tvec;
    int j;

    tvec = allocVector(type, vlen);
    for (j = 0; j < vlen; j++)
	if (type == REALSXP)
	    REAL(tvec)[j] = ssNA_REAL;
	else if (type == STRSXP)
	    SET_STRING_ELT(tvec, j, STRING_ELT(ssNA_STRING, 0));
    SETLEVELS(tvec, 0);
    return (tvec);
}







Rect deBounds = { 400, 400, 600, 800 };

/*
  id : integer, id number in the browser
  isroot : boolean, this id is a root item
  iscont : boolean, is container?
  numofit : integer, number of items
  parid   : parent id number
  name   : char, name of objects
  type   : type, class or mode
  objsize: can be dim, length or number of levels 
*/

/* The following are local copies of the above */

                                /* We do not check for this */ 
void EmptyDataEntry(void);





static const EventTypeSpec	RCloseWinEvent[] = 
{
        { kEventClassWindow, kEventWindowClose }        
};

extern OSStatus DoCloseHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );




Boolean OpenDataEntry(void)
{
	OSStatus err = noErr;
	int i,j,k;
         EventTypeSpec deEvents[] = {
		{ kEventClassCommand,	kEventCommandProcess }, 
		{ kEventClassCommand,	kEventCommandUpdateStatus }, 
		{ kEventClassWindow,	kEventWindowGetIdealSize },
		{ kEventClassWindow,	kEventWindowBoundsChanged }, 
		{ kEventClassWindow,	kEventWindowGetClickActivation }
			};
    
     CreateNewWindow(kDocumentWindowClass,  kWindowStandardHandlerAttribute |
            kWindowStandardDocumentAttributes, &deBounds, &DataEntryWindow);

    if(DataEntryWindow == NULL)
     return(FALSE);
     
     	
     InstallWindowEventHandler(DataEntryWindow, 
    		NewEventHandlerUPP(deEventHandler), 
    		sizeof(deEvents)/sizeof(EventTypeSpec), deEvents, NULL, NULL);
     
     InstallWindowEventHandler( DataEntryWindow, NewEventHandlerUPP(DoCloseHandler), 
                                          1,
                                          RCloseWinEvent, (void *)DataEntryWindow, NULL);
                    
 
     SetWindowTitleWithCFString(DataEntryWindow, CFSTR("R DataEntryWindow"));
     
     
    
    /* Create the DataBrowser */
     CreateDataEntry(DataEntryWindow, &DataEntryControl);
    /* Configure the DataBrowser */
	
    ConfigureDataEntry(DataEntryControl);
    err = SetDataBrowserTarget(DataEntryControl, 1);
    
    /* Set the keyboard focus */
	SetKeyboardFocus(DataEntryWindow, DataEntryControl, kControlDataBrowserPart);
	
	/* Store DB as a window property */
    SetWindowProperty(DataEntryWindow,'RMAC', 'RDEY',sizeof(DataEntryControl), &DataEntryControl);

	InstallDataEntryCallbacks(DataEntryControl);

    
    
	if(DataEntryControl == NULL){              
	 CloseDataEntry();	 
	 return;
	}
	
	
   NumOfRows = ymaxused;	
   RowID = (int*)malloc(NumOfRows * sizeof(int));
   for(i=1;i<=NumOfRows;i++)
    RowID[i-1] = i;
    
  AddDataBrowserItems(DataEntryControl, kDataBrowserNoItem, 
				NumOfRows, RowID, kDataBrowserItemNoProperty);

   //SetDataBrowserSortProperty (DataEntryControl,kDataBrowserItemNoProperty);
 

    ShowWindow(DataEntryWindow);


}



void EmptyDataEntry(void)
{
  RemoveDataBrowserItems (DataEntryControl, kDataBrowserNoItem, 0, 
          NULL, kDataBrowserItemNoProperty);
} 

void CloseDataEntry(void)
{

    if(DataEntryWindow){
     if( GetWindowBounds(DataEntryWindow,kWindowStructureRgn,&deBounds) != noErr)
      SetRect(&deBounds, 400, 400, 600, 800);
    }
    else 
     SetRect(&deBounds, 400, 400, 600, 800);
      
	
    DisposeWindow(DataEntryWindow);
    DataEntryWindow = NULL;
    
    
    DataEntryControl = NULL;
    if(RowID)
     free(RowID);
    
} 

static void CreateDataEntry(WindowRef window, ControlRef *browser)
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


static void ConfigureDataEntry(ControlRef browser)
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
			
                        columnDesc.headerBtnDesc.minimumWidth = 30;
			columnDesc.headerBtnDesc.maximumWidth = 200;
			
			columnDesc.headerBtnDesc.btnFontStyle.just = teFlushRight;
			
			columnDesc.headerBtnDesc.btnFontStyle.font = kControlFontViewSystemFont;
			columnDesc.headerBtnDesc.btnFontStyle.style = normal;
		
			columnDesc.propertyDesc.propertyType = kDataBrowserTextType;
			columnDesc.propertyDesc.propertyFlags = kDataBrowserPropertyIsMutable |									  		kDataBrowserListViewDefaultColumnFlags;
			for(i=0;i<=xmaxused;i++){
                     
			columnDesc.propertyDesc.propertyID = i+1000;
			
			//columnDesc.headerBtnDesc.btnContentInfo.contentType = kControlContentIconRef;
		
                        if(i==0)
                          columnDesc.headerBtnDesc.titleString = CFStringCreateWithCString(
				CFAllocatorGetDefault(),"Row Labels", kCFStringEncodingMacRoman);
                        else
                         columnDesc.headerBtnDesc.titleString = CFStringCreateWithCString(
				CFAllocatorGetDefault(), get_col_name(i), kCFStringEncodingMacRoman);
			
			AddDataBrowserListViewColumn(browser, 
				&columnDesc, kDataBrowserListViewAppendColumn);
			
                        
			SetDataBrowserListViewDisclosureColumn(browser, columnDesc.propertyDesc.propertyID, false);
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
		'RDEY', sizeof(browser), NULL, &browser);
	
	return browser;
}


void InstallDataEntryCallbacks(ControlRef browser)
{
    DataBrowserCallbacks myCallbacks;
    

    myCallbacks.version = kDataBrowserLatestCallbacks;
    InitDataBrowserCallbacks(&myCallbacks);
    
    myCallbacks.u.v1.itemDataCallback = 
        NewDataBrowserItemDataUPP(deGetSetItemData);
	

    myCallbacks.u.v1.itemNotificationCallback = 
        NewDataBrowserItemNotificationUPP(deItemNotification);
     
    myCallbacks.u.v1.itemCompareCallback = 
		NewDataBrowserItemCompareUPP(deItemComparison);

    SetDataBrowserCallbacks(browser, &myCallbacks);
}





void printelt(SEXP invec, int vrow, char *str);


static pascal OSStatus deGetSetItemData(ControlRef browser, 
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
          col = property-1000;
          row = itemID;
         }

        if(!changeValue) {
       
       
         if(property==1000){
             sprintf(buf,"%d",row);
             CopyCStringToPascal(buf,pascalString);
             text = CFStringCreateWithPascalString(CFAllocatorGetDefault(), pascalString, kCFStringEncodingMacRoman);
             err = SetDataBrowserItemDataText(itemData, text); 
             CFRelease(text); 
         }
         
         if(property>1000 & row>0 & row<=ymaxused){          
          tmp = VECTOR_ELT(work, col-1);
          if (!isNull(tmp)) {
             strcpy(buf,"     ");
             if(LENGTH(tmp)>row-1)
              printelt(tmp, row-1, buf);
             } 
             CopyCStringToPascal(buf,pascalString);
             text = CFStringCreateWithPascalString(CFAllocatorGetDefault(), pascalString, kCFStringEncodingMacRoman);
             err = SetDataBrowserItemDataText(itemData, text); 
             CFRelease(text); 
                       
     
        }   
        switch (property)
	{
		case kDataBrowserItemIsEditableProperty:
		{	err = SetDataBrowserItemDataBooleanValue(itemData, true);
		}	break;
		
		

		default:
		{	err = errDataBrowserPropertyNotSupported;
		}	break;
	}
}
	else {
        
          if(property>1000 & row>0 & row<=ymaxused){          
            err = GetDataBrowserItemDataText(itemData, &text); 
            CFStringGetCString (text, buf, 1000,  kCFStringEncodingMacRoman);
            if(strlen(buf)>0){
                tmp = VECTOR_ELT(work, col-1);
                    if(get_col_type(col) == CHARACTER)
                        SET_STRING_ELT(tmp, row-1, mkChar(buf));
                    else {
                        char *endp;
                        double new = R_strtod(buf, &endp);
                        if(isBlankString(endp))
                            REAL(tmp)[row - 1] = new;
                    }
             } else { 
                if(get_col_type(col) == CHARACTER)
                    SET_STRING_ELT(tmp, row - 1, NA_STRING);
                else
                    REAL(tmp)[row - 1] = NA_REAL;
            }
            
            CFRelease(text); 
          }             
     


	}
        
	return err;
}
 
 
void printelt(SEXP invec, int vrow, char *strp)
{

    if(!strp)
     return;
     
     
    PrintDefaults(R_NilValue);
    if (TYPEOF(invec) == REALSXP) {
	if (REAL(invec)[vrow] != ssNA_REAL) {
	    strcpy(strp, EncodeElement(invec, vrow, 0));
	    return;
	}
    }
    else if (TYPEOF(invec) == STRSXP) {
    if(CHAR(STRING_ELT(invec, vrow))){
	if (!streql(CHAR(STRING_ELT(invec, vrow)),
		    CHAR(STRING_ELT(ssNA_STRING, 0)))) {
	    strcpy(strp, EncodeElement(invec, vrow, 0));
	    return;
	}
    }
    }
    else
	error("dataentry: internal memory error");
}

  
 static pascal void deItemNotification(
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

pascal OSStatus deEventHandler(
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

SEXP Raqua_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP colmodes, tnames, tvec, tvec2, work2;
    SEXPTYPE type;
    int i, j, cnt, len, nprotect;
    RCNTXT cntxt;
    char clab[25];

    nprotect = 0;/* count the PROTECT()s */
    PROTECT_WITH_INDEX(work = duplicate(CAR(args)), &wpi); nprotect++;
    colmodes = CADR(args);
    tnames = getAttrib(work, R_NamesSymbol);

    if (TYPEOF(work) != VECSXP || TYPEOF(colmodes) != VECSXP)
	errorcall(call, "invalid argument");

    /* initialize the constants */

    bufp = buf;
    ne = 0;
    currentexp = 0;
    nneg = 0;
    ndecimal = 0;
    clength = 0;
    ccol = 1;
    crow = 1;
    colmin = 1;
    rowmin = 1;
    ssNA_REAL = -NA_REAL;
    tvec = allocVector(REALSXP, 1);
    REAL(tvec)[0] = ssNA_REAL;
    PROTECT(ssNA_STRING = coerceVector(tvec, STRSXP)); nprotect++;
    bwidth = 5;
    hwidth = 30;

    /* setup work, names, lens  */
    xmaxused = length(work); ymaxused = 0;
    PROTECT_WITH_INDEX(lens = allocVector(INTSXP, xmaxused), &lpi);
    nprotect++;

    if (isNull(tnames)) {
	PROTECT_WITH_INDEX(names = allocVector(STRSXP, xmaxused), &npi);
	for(i = 0; i < xmaxused; i++) {
	    sprintf(clab, "var%d", i);
	    SET_STRING_ELT(names, i, mkChar(clab));
	}
    } else
	PROTECT_WITH_INDEX(names = duplicate(tnames), &npi);
    nprotect++;

    for (i = 0; i < xmaxused; i++) {
	int len = LENGTH(VECTOR_ELT(work, i));
	INTEGER(lens)[i] = len;
	ymaxused = max(len, ymaxused);
        type = TYPEOF(VECTOR_ELT(work, i));
    if (LENGTH(colmodes) > i && !isNull(VECTOR_ELT(colmodes, i)))
	    type = str2type(CHAR(STRING_ELT(VECTOR_ELT(colmodes, i), 0)));
	if (type != STRSXP) type = REALSXP;
	if (isNull(VECTOR_ELT(work, i))) {
	    if (type == NILSXP) type = REALSXP;
	    SET_VECTOR_ELT(work, i, ssNewVector(type, 100));
	} else if (!isVector(VECTOR_ELT(work, i)))
	    errorcall(call, "invalid type for value");
	else {
	    if (TYPEOF(VECTOR_ELT(work, i)) != type)
		SET_VECTOR_ELT(work, i, 
			       coerceVector(VECTOR_ELT(work, i), type));
	}
    }


    /* start up the window, more initializing in here */
   
    TXNSetTXNObjectControls(RConsoleInObject, false, 1, RReadOnlyTag, RReadOnlyData);
    EditingFinished = false;
    OpenDataEntry();
    QuitApplicationEventLoop();
    
    RunApplicationEventLoop();  /* waits till the user close the dataentry window */
    
    /* drop out unused columns */
    for(i = 0, cnt = 0; i < xmaxused; i++)
	if(!isNull(VECTOR_ELT(work, i))) cnt++;
    if (cnt < xmaxused) {
	PROTECT(work2 = allocVector(VECSXP, cnt)); nprotect++;
	for(i = 0, j = 0; i < xmaxused; i++) {
	    if(!isNull(VECTOR_ELT(work, i))) {
		SET_VECTOR_ELT(work2, j, VECTOR_ELT(work, i));
		INTEGER(lens)[j] = INTEGER(lens)[i];
		SET_STRING_ELT(names, j, STRING_ELT(names, i));
		j++;
	    }
	}
	REPROTECT(names = lengthgets(names, cnt), npi);
    } else work2 = work;

    for (i = 0; i < LENGTH(work2); i++) {
	len = INTEGER(lens)[i];
	tvec = VECTOR_ELT(work2, i);
	if (LENGTH(tvec) != len) {
	    tvec2 = ssNewVector(TYPEOF(tvec), len);
	    for (j = 0; j < len; j++) {
		if (TYPEOF(tvec) == REALSXP) {
		    if (REAL(tvec)[j] != ssNA_REAL)
			REAL(tvec2)[j] = REAL(tvec)[j];
		    else
			REAL(tvec2)[j] = NA_REAL;
		} else if (TYPEOF(tvec) == STRSXP) {
		    if (!streql(CHAR(STRING_ELT(tvec, j)),
				CHAR(STRING_ELT(ssNA_STRING, 0))))
			SET_STRING_ELT(tvec2, j, STRING_ELT(tvec, j));
		    else
			SET_STRING_ELT(tvec2, j, NA_STRING);
		} else
		    error("dataentry: internal memory problem");
	    }
	    SET_VECTOR_ELT(work2, i, tvec2);
	}
    }

    setAttrib(work2, R_NamesSymbol, names);    
    UNPROTECT(nprotect);
    return work2;
}




 
static char *get_col_name(int col)
{
    static char clab[25];
    if (col <= xmaxused) {
	/* don't use NA labels */
	SEXP tmp = STRING_ELT(names, col - 1);
	if(tmp != NA_STRING) return(CHAR(tmp));
    }
    sprintf(clab, "var%d", col);
    return clab;
}

 
static CellType get_col_type(int col)
{
    SEXP tmp;
    CellType res = UNKNOWNN;

    if (col <= xmaxused) {
	tmp = VECTOR_ELT(work, col - 1);
	if(TYPEOF(tmp) == REALSXP) res = NUMERIC;
	if(TYPEOF(tmp) == STRSXP) res = CHARACTER;
    }
    return res;
}





static pascal Boolean deItemComparison(
	ControlRef browser, DataBrowserItemID itemOneID, 
	DataBrowserItemID itemTwoID, DataBrowserPropertyID property)
{
	SInt16 compareResult = 0;
	char buf1[500], buf2[500];
	SEXP tmp;
        int row;
        
        row = max(itemOneID,itemTwoID);
     #define Compare(i1,i2,p) deItemComparison(browser,i1,i2,p)
	 if(property>1000 & property<2000 & row>0 & row<=ymaxused){
            tmp = VECTOR_ELT(work, property-1000-1);
            if (!isNull(tmp)) {
                strcpy(buf1," ");
                strcpy(buf2," ");
                if(LENGTH(tmp)>itemOneID-1)
                    printelt(tmp, itemOneID-1, buf1);
                 if(LENGTH(tmp)>itemTwoID-1)
                    printelt(tmp, itemTwoID-1, buf2);
           
                if(get_col_type(property-1000) == CHARACTER)
                        compareResult = strcmp(buf1,buf2);
                    else {
                        char *endp1,*endp2;
                        double new1 = R_strtod(buf1, &endp1);
                        double new2 = R_strtod(buf2, &endp2);
                        if(isBlankString(endp1) & isBlankString(endp2))
                            if(new1 < new2) compareResult = -1;
                            else if(new1>new2) compareResult = 1;
                         }
                
                if (compareResult < 0) return true;
			else if (compareResult > 0) return false;
			else return Compare(itemOneID, itemTwoID, '????');
                }
        }
            
       
        return itemOneID < itemTwoID;      

       			
}



















#endif  /* HAVE_AQUA */

#endif /* __AQUA_DATA_ENTRY__ */



