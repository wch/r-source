/*
 *  R : A Computer Language for Statistical Data Analysis
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
 *  This file is adapted from the public demos coming with the Waste library
 *  distribution:  WASTE Text Engine © 1993-2000 Marco Piovanelli.
 *   
 *  Original file was:
 *
 *	WASTE Demo Project:
 *	Dialog Utilities
 *
 *	Copyright © 1993-1998 Marco Piovanelli
 *	All Rights Reserved
 *
 *	C port by John C. Daub
 */

#ifndef __APPLEEVENTS__
# include <AppleEvents.h>
#endif

#ifndef __AEREGISTRY__
# include <AERegistry.h>
#endif

#ifndef __DISKINIT__
# include <DiskInit.h>
#endif

#ifndef __TEXTSERVICES__
# include <TextServices.h>
#endif

#ifndef __WEDEMOAPP__
# include "RIntf.h"
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "Defn.h"
#include "Graphics.h"
#include "IOStuff.h"
#include "Parse.h"

pascal Handle  gEditHdl;
SInt32  gEditLen, gEditPos, startPoint;
WEReference Ewe ;

extern SInt16 Edit_Window;
extern WindowPtr Edit_Windows[MAX_NUM_E_WIN + 1];
extern WindowPtr Console_Window;
extern SInt16  gTextSize;


void R_Edit(char** lines, int nlines);
void R_EditWindow(char** lines, int nlines, WindowPtr window);
void DoLineTo();
void GoTo_Line(SInt32 line_Num, WindowPtr window);
void Do_EditObject();
void R_Edit_Object(char* ObjectName);
void Do_HelpOnTopic(void);
void Do_RunExample(void);
void Do_SearchHelp(void);


void LoadEditEnvironment()
{
    char returnChar;
    char*    CharPtr;
    SInt32  EditLen;
    pascal Handle  EditHdl=NULL;
    gEditPos = startPoint = 0;
    Ewe = GetWindowWE ( FrontWindow());

    EditHdl = WEGetText(Ewe);
    EditLen = WEGetTextLength(Ewe); 

    /* Add '\r' at the end of the Text */
    gEditLen = EditLen + 1;
    gEditHdl = NewHandle((gEditLen) *sizeof(char)); 
    HLock(gEditHdl);  
    HLock(EditHdl);
    strncpy(*gEditHdl, *EditHdl, EditLen);
    (*gEditHdl)[EditLen] = '\r';
    HUnlock(EditHdl);
    HUnlock(gEditHdl);  
    
}

int ggetc()
{
    WindowPtr window;
    char returnChar;
    char*    CharPtr;
   
    if (gEditPos >= gEditLen){
	return EOF;
    }else{
	HLock(gEditHdl);
	CharPtr = *gEditHdl;
	CharPtr = CharPtr + gEditPos;
	returnChar = *CharPtr;
	HUnlock(gEditHdl);
	gEditPos ++;
    }
    if (returnChar == '\r'){
	startPoint = gEditPos;
	return '\n';

    }
    else{  
	return returnChar;
    }
}

int gungetc(int c)
{
    if (c != EOF) 
	gEditPos--;
    return c;

}

void ParseIncomplete()
{  
    ErrorDia("The parse was incomplete!!!");
}

void ParseError()
{
    char *errorString;
    SInt32 errStart, errEnd;
    char returnChar;
    char*    CharPtr;

    errorString = malloc(255*sizeof(char));
    sprintf(errorString, "A syntax error occured at line %d", R_ParseError); 

    errEnd = gEditPos;
    while  ((errEnd < gEditLen) && (returnChar != '\r') && (returnChar != '\n')){
	HLock(gEditHdl);
	CharPtr = *gEditHdl;
	CharPtr = CharPtr + errEnd;
	returnChar = *CharPtr;
	HUnlock(gEditHdl);
	errEnd ++;
    }
    errEnd --;
    errStart = gEditPos;

    returnChar = '\0';
    while ((errStart >= 0) && (returnChar != '\r') && (returnChar != '\n')){
	HLock(gEditHdl);
	CharPtr = *gEditHdl;
	CharPtr = CharPtr + errStart;
	returnChar = *CharPtr;
	HUnlock(gEditHdl);
	errStart --;
    }
    if (errStart == -1)
	errStart++;
    else
	errStart = errStart + 2;
    ErrorDia(errorString);
    WESetSelection(errStart, errEnd, Ewe);
}

void ErrorDia(char* errorMessage)
{
    AlertStdAlertParamRec paramRec;
    Str255 labelText;
    Str255 narrativeText;
    SInt16 itemHit;

    labelText[0] = strlen(errorMessage);
    strncpy((char*)&labelText[1], errorMessage, labelText[0]);
    	
    paramRec.movable			= false;
    paramRec.helpButton			= false;
    paramRec.filterProc			= NULL;
    paramRec.defaultText		= (StringPtr) kAlertDefaultOKText;
    paramRec.cancelText			= NULL;
    paramRec.otherText			= NULL;
    paramRec.defaultButton		= kAlertStdAlertOKButton;
    paramRec.cancelButton		= 0;
    paramRec.position			= kWindowAlertPositionMainScreen;

    Do_StandardAlert(labelText);
    /*(kAlertStopAlert,labelText,0,&paramRec,&itemHit);
     */
}

void LoadWindow()
{
    int status;
    SEXP code;

    /* Set up any state variable for ggetc, gungetc */

    R_CurrentExpr  = R_ParseGeneral(ggetc, gungetc, -1, &status);

    switch(status) {
    
    case PARSE_NULL:
    case PARSE_EOF:
	/* The window contained no data. */
	/* We just return. */
	break;                              
                            
    case PARSE_OK:
  	/* The window contents parsed correctly */
	/* so we execute the parse tree. */
	R_EvalDepth = 0;
	PROTECT(R_CurrentExpr);
	R_Busy(1);             
	R_CurrentExpr = eval(R_CurrentExpr, R_GlobalEnv);
	UNPROTECT(1);
	SelectWindow ( Console_Window ) ;
	break;                              
                            
    case PARSE_ERROR:    
  	ParseError();                           
	/* A syntax error occured at line R_ParseError */
	/* in the input buffer. We should now set the */
	/* insertion point at the offending point in */
	/* the window and highlight that line.  We should */
	/* also display a modal dialog box with the message */
	/* syntax error at line ??  Having done that we return */
	/* to the event loop. */
	break;                              
                            
    case PARSE_INCOMPLETE:
  	ParseIncomplete();
	/* The parse was incomplete.  We should display a */
	/* modal dialog box with the message "incomplete statement */
	/* When done, we should return to the event loop. */
	break;                             
    }
}

void GoTo_Line(SInt32 line_Num, WindowPtr window)
{
    WEReference we ;
    pascal Handle  EditHdl=NULL;
    SInt32  EditLen, currentLine = 1, currentPos = 0;
    char returnChar;
    char* CharPtr;

    we = GetWindowWE ( window);
    EditHdl = WEGetText(we);
    EditLen = WEGetTextLength(we);  
    HLock(EditHdl);
    CharPtr = *EditHdl;
    while(line_Num > currentLine){
	returnChar = *CharPtr;    
	if ( returnChar == '\r')
	    currentLine++;
	CharPtr = CharPtr + 1;
	currentPos ++;
    }
   
    HUnlock(EditHdl); 

    WESetSelection(currentPos, currentPos, we);
}

void DoLineTo()
{
    DialogPtr          lineToBox = nil ;
    WEReference        we = nil ;
    GrafPtr            savePort ;
    SInt16             itemHit = 0;
    Handle	       itemHandle=NULL;
    Rect               itemRect;
    Str255             buf;
    short              type; 
    WindowPtr          window;
    SInt32             GotoLine;
    window = FrontWindow();
    lineToBox = GetNewDialog ( kLineTo, nil, ( WindowPtr ) -1L ) ;
    if ( lineToBox == nil ) {
	goto cleanup ;
    }
    GetPort ( & savePort ) ;
    SetPort ( lineToBox ) ;
   
    /* Set the Dialog Title */
    SetWTitle ( lineToBox, "\p Line Number" ) ;

    while(true){
	ModalDialog ( GetMyStandardDialogFilter ( ), & itemHit ) ;
	if ( itemHit ==1){   /* OK button */
	    GetDialogItem(lineToBox, 3, &type, &itemHandle, &itemRect);  /* 3 is text field */
	    GetDialogItemText(itemHandle, buf);
	    buf[buf[0]+1] = '\0';
	    GotoLine = atoi((char *) &buf[1]);
	    GoTo_Line(GotoLine, window);
	    break; 
	}
	if (itemHit == 2){   /* Cancel Button */
	    break;
	}   
    }

    cleanup :
	if ( we != nil )
	{
	    WEDispose ( we ) ;
	}
    if ( lineToBox != nil )
    {
	DisposeDialog ( lineToBox ) ;
	/* Restore the Port */
	SetPort ( savePort ) ;
    }

}


void R_Edit(char** lines, int nlines)
{
    DoNew(true);
    if(Edit_Window>2)
     RepositionWindow(Edit_Windows[Edit_Window - 1], Edit_Windows[Edit_Window - 2],kWindowCascadeOnParentWindow);
    R_EditWindow(lines, nlines, Edit_Windows[Edit_Window -1]);
        
}

void R_EditWindow(char** lines, int nlines, WindowPtr window)
{
    WEReference we ;
    SInt32  i;
    /* unpgrated to Waste 2.x Jago */
    we = GetWindowWE (window);
    HLock(lines);
    for (i=0; i < nlines ; i++){
	WEPut(kCurrentSelection, kCurrentSelection, lines[i], 
	      strlen(lines[i]), kTextEncodingMultiRun, 0, 0, nil, nil, we );  
	WEPut(kCurrentSelection,kCurrentSelection, "\r", 1,kTextEncodingMultiRun, 0,0,nil,nil,we );  
    }
    HUnlock(lines);
}

/* This routine allows the user to have help on a topic. It is just an
   interface to help() function, all the work is done by R itself.
   Jago April 2001, Stefano M. Iacus
*/   

void Do_HelpOnTopic(void)
{
    DialogPtr          HelpObjectBox = nil ;
    WEReference        we = nil ;
    GrafPtr            savePort ;
    SInt16             itemHit = 0;
    Handle	       itemHandle=NULL;
    Rect               itemRect;
    Str255             buf;
    char				topic[260];
    char				cmd[300];
    short              type; 
    WindowPtr          window;
    SInt32             GotoLine;
    window = FrontWindow();
    
    HelpObjectBox = GetNewDialog ( kHelpObject, nil, ( WindowPtr ) -1L ) ;
    if ( HelpObjectBox == nil )
    {
	goto cleanup ;
    }
    GetPort ( & savePort ) ;
    SetPort ( HelpObjectBox ) ;
   
    /* Set the Dialog Title */
    SetWTitle ( HelpObjectBox, "\pR Help" ) ;

    while(true){
	ModalDialog ( GetMyStandardDialogFilter ( ), & itemHit ) ;
	if ( itemHit ==1){   /* OK button */
	    GetDialogItem(HelpObjectBox, 3, &type, &itemHandle, &itemRect);  /* 3 is text field */
	    GetDialogItemText(itemHandle, buf);
	    CopyPascalStringToC(buf,topic);
	    if(strlen(topic)==0)
	     break;
	    sprintf(cmd,"?%s",topic);
		consolecmd(cmd);
	    break; 
	}
	if (itemHit == 2){   /* Cancel Button */
	    break;
	}   
    }

    cleanup :
	if ( we != nil )
	{
	    WEDispose ( we ) ;
	}
    if ( HelpObjectBox != nil )
    {
	DisposeDialog ( HelpObjectBox ) ;
	/* Restore the Port */
	SetPort ( savePort ) ;
    }

}

/* This routine allows the user torun an example. It is just an
   interface to example() function, all the work is done by R itself.
   Jago April 2001, Stefano M. Iacus
*/   

void Do_RunExample(void)
{
    DialogPtr          ExampleObjectBox = nil ;
    WEReference        we = nil ;
    GrafPtr            savePort ;
    SInt16             itemHit = 0;
    Handle	       itemHandle=NULL;
    Rect               itemRect;
    Str255             buf;
    char				topic[260];
    char				cmd[300];
    short              type; 
    WindowPtr          window;
    SInt32             GotoLine;
    window = FrontWindow();
    
    ExampleObjectBox = GetNewDialog ( kExampleObject, nil, ( WindowPtr ) -1L ) ;
    if ( ExampleObjectBox == nil )
    {
	goto cleanup ;
    }
    GetPort ( & savePort ) ;
    SetPort ( ExampleObjectBox ) ;
   
    /* Set the Dialog Title */
    SetWTitle ( ExampleObjectBox, "\pR examples" ) ;

    while(true){
	ModalDialog ( GetMyStandardDialogFilter ( ), & itemHit ) ;
	if ( itemHit ==1){   /* OK button */
	    GetDialogItem(ExampleObjectBox, 3, &type, &itemHandle, &itemRect);  /* 3 is text field */
	    GetDialogItemText(itemHandle, buf);
	    CopyPascalStringToC(buf,topic);
	    if(strlen(topic)==0)
	     break;
		sprintf(cmd,"example(%s)",topic);
		consolecmd(cmd);
	    break; 
	}
	if (itemHit == 2){   /* Cancel Button */
	    break;
	}   
    }

    cleanup :
	if ( we != nil )
	{
	    WEDispose ( we ) ;
	}
    if ( ExampleObjectBox != nil )
    {
	DisposeDialog ( ExampleObjectBox ) ;
	/* Restore the Port */
	SetPort ( savePort ) ;
    }

}

/* This routine allows the user to search for a topic. It is just an
   interface to help.search() function, all the work is done by R itself.
   Jago April 2001, Stefano M. Iacus
*/   


void Do_SearchHelp(void)
{
    DialogPtr          SearchObjectBox = nil ;
    WEReference        we = nil ;
    GrafPtr            savePort ;
    SInt16             itemHit = 0;
    Handle	       itemHandle=NULL;
    Rect               itemRect;
    Str255             buf;
    char				topic[260];
    char				cmd[300];
    short              type; 
    WindowPtr          window;
    SInt32             GotoLine;
    window = FrontWindow();
    
    SearchObjectBox = GetNewDialog ( kSearchObject, nil, ( WindowPtr ) -1L ) ;
    if ( SearchObjectBox == nil )
    {
	goto cleanup ;
    }
    GetPort ( & savePort ) ;
    SetPort ( SearchObjectBox ) ;
   
    /* Set the Dialog Title */
    SetWTitle ( SearchObjectBox, "\pSearch for R Help" ) ;

    while(true){
	ModalDialog ( GetMyStandardDialogFilter ( ), & itemHit ) ;
	if ( itemHit ==1){   /* OK button */
	    GetDialogItem(SearchObjectBox, 3, &type, &itemHandle, &itemRect);  /* 3 is text field */
	    GetDialogItemText(itemHandle, buf);
	    CopyPascalStringToC(buf,topic);
		if(strlen(topic)==0)
	     break;
		sprintf(cmd,"help.search(\"%s\")",topic);
		consolecmd(cmd);
	    break; 
	}
	if (itemHit == 2){   /* Cancel Button */
	    break;
	}   
    }

    cleanup :
	if ( we != nil )
	{
	    WEDispose ( we ) ;
	}
    if ( SearchObjectBox != nil )
    {
	DisposeDialog ( SearchObjectBox ) ;
	/* Restore the Port */
	SetPort ( savePort ) ;
    }

}

/* This routine allows the user to edit an existing object. It is just an
   interface, all the work is done by R itself.
   Jago April 2001, Stefano M. Iacus
*/   
void Do_EditObject()
{
    DialogPtr          EditObjectBox = nil ;
    WEReference        we = nil ;
    GrafPtr            savePort ;
    SInt16             itemHit = 0;
    Handle	           itemHandle=NULL;
    Rect               itemRect;
    Str255             buf;
    short              type; 
    WindowPtr          window;
    SInt32             GotoLine;
    char				topic[260],cmd[300];

    window = FrontWindow();
    EditObjectBox = GetNewDialog ( kEditObject, nil, ( WindowPtr ) -1L ) ;
    if ( EditObjectBox == nil )
    {
	goto cleanup ;
    }
    GetPort ( & savePort ) ;
    SetPort ( EditObjectBox ) ;
   
    /* Set the Dialog Title */
    SetWTitle ( EditObjectBox, "\p Edit Object" ) ;

    while(true){
	ModalDialog ( GetMyStandardDialogFilter ( ), & itemHit ) ;
	if ( itemHit ==1){   /* OK button */
	    GetDialogItem(EditObjectBox, 3, &type, &itemHandle, &itemRect);  /* 3 is text field */
	    GetDialogItemText(itemHandle, buf);
	    CopyPascalStringToC(buf,topic);
		if(strlen(topic)==0)
	     break;
	    sprintf(cmd,"%s <- edit(%s)",topic,topic); 
	    consolecmd(cmd);
	    //R_Edit_Object(topic);
	    
	    break; 
	}
	if (itemHit == 2){   /* Cancel Button */
	    break;
	}   
    }

    cleanup :
	if ( we != nil )
	{
	    WEDispose ( we ) ;
	}
    if ( EditObjectBox != nil )
    {
	DisposeDialog ( EditObjectBox ) ;
	/* Restore the Port */
	SetPort ( savePort ) ;
    }

}


void Do_StandardAlert(Str255 LabelText)
{
    DialogPtr          ErrorBox = nil ;
    WEReference        we = nil ;
    GrafPtr            savePort ;
    SInt16             itemHit = 0;
    Handle             itemHandle=NULL;
    Rect               itemRect;
    Str255             buf;
    short              type; 
    WindowPtr          window;
    SInt32             GotoLine;
    window = FrontWindow();
    ErrorBox = GetNewDialog ( kError, nil, ( WindowPtr ) -1L ) ;
    if ( ErrorBox == nil )
    {
	goto cleanup ;
    }
    GetPort ( & savePort ) ;
    SetPort ( ErrorBox ) ;
   
    /* Set the Dialog Title */
    SetWTitle ( ErrorBox, "\p Error Alert" ) ;
    GetDialogItem(ErrorBox, 2, &type, &itemHandle, &itemRect);  /* 3 is text field */
    SetDialogItemText(itemHandle, LabelText);
    while(true){
	ModalDialog ( GetMyStandardDialogFilter ( ), & itemHit ) ;
	if ( itemHit ==1){   /* OK button */
	    break; 
	} 
    }

    cleanup :
	if ( we != nil )
	{
	    WEDispose ( we ) ;
	}
    if ( ErrorBox != nil )
    {
	DisposeDialog ( ErrorBox ) ;
	/* Restore the Port */
	SetPort ( savePort ) ;
    }

}

void Do_About()
{
    DialogPtr          AboutBox = nil ;
    WEReference        we = nil ;
    GrafPtr            savePort ;
    SInt16             itemHit = 0;
    Handle             itemHandle=NULL;
    Rect               itemRect;
    Str255             buf;
    short              type; 
    WindowPtr          window;
    SInt32             GotoLine;
    Handle	       sourceResourceHdl=NULL;
  
    window = FrontWindow();
    AboutBox = GetNewDialog ( kAbout, nil, ( WindowPtr ) -1L ) ;

    if ( AboutBox == nil )
    {
	goto cleanup ;
    }
    GetPort ( & savePort ) ;
    SetPort ( AboutBox ) ;
   
    /* Set the Dialog Title */
    SetWTitle ( AboutBox, "\p About R" ) ;

    while(true) {
	ModalDialog ( GetMyStandardDialogFilter ( ), & itemHit ) ;
	if ( itemHit == 1){   /* The Screen */
	    break; 
	}
    }

    cleanup :
	if ( we != nil )
	{
	    WEDispose ( we ) ;
	}
    if ( AboutBox != nil )
    {
	DisposeDialog ( AboutBox ) ;
	/* Restore the Port */
	SetPort ( savePort ) ;
    }
}
