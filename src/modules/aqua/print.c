/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2003  Robert Gentleman, Ross Ihaka
 *			      and the R Development Core Team
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

#define __DEBUGGING__
#include <Carbon/Carbon.h>

#include <Defn.h>
#include <Graphics.h>
#include <Rdevices.h>


extern WindowRef 	ConsoleWindow;
extern TXNObject	RConsoleOutObject;

OSStatus OpenPageSetup(WindowRef window);
OSStatus OpenPrintDialog(WindowRef window);


OSStatus OpenPageSetup(WindowRef window){
    TXNObject tmpObj = NULL; 

    if(window == ConsoleWindow)
        return( TXNPageSetup(RConsoleOutObject) );

    if( GetWindowProperty(window, 'RHLP', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr)
        return( TXNPageSetup(tmpObj) );

    if( GetWindowProperty(window, 'REDT', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr)
        return( TXNPageSetup(tmpObj) );
 
    return( noErr );
}



OSStatus OpenPrintDialog(WindowRef window){
    TXNObject tmpObj = NULL; 

    if(window == ConsoleWindow)
        return( TXNPrint(RConsoleOutObject) );
    
    if( GetWindowProperty(window, 'RHLP', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr)
        return( TXNPrint(tmpObj) );
    
    if( GetWindowProperty(window, 'REDT', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr)
        return( TXNPrint(tmpObj) );
          
    return( noErr );
}

 
