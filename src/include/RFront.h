
/*
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	 
	R
	A Computer Language for Statistical Data Analysis
	Copyright 1995 Robert Gentlemen and Ross Ihaka
	Extended by Julian Harris

	Auckland University
	Auckland
	New Zealand

	<< RFrontEnd_api.h >>
	<< DOS:frontapi.h >>
	
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Copyright Info
	
	This program is free software; you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation; either version 2 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
 	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
	
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Description

	This file contains the interface headers for a collection
	of functions that need to be written to fulfill the
	platform-specific front end.
	
	Along with the function names, there are the old names
	firstly as #defines and later as comments as the rest of the
	code is integrated.
	
	## This file formatted for use with 4 spaces to one tab.
	
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Issues
	
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

*/

#ifndef RFrontEnd_api_h
#define RFrontEnd_api_h



/* = = = = = = = = = = = = = = INCLUDES = = = = = = = = = = = = = = = = */

#include <stdio.h>

/* = = = = = = = = = = = = = = PROTOTYPES = = = = = = = = = = = = = = = = */





/*
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	General functions
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*/

/*							StartUp
							----------------
							This is called at startup. Use it to 
							initialise any structures you need. You can use it
							to load up any images the user may have specified.
							Required:set R_Init to 1 and set the the R_Unnamed
							to the appropriate value.
*/
#define RStartUp			This function has gone
/*void						RFrontEnd_StartUp( void );*/


/*							CleanUp
							----------------
							Check whether there is any unsaved data in the
							workspace and if so, prompt the user to save it.
							If the user chooses to save, save the image and call
							KillDevice() and exit() to finally quit. 
							If the user chooses cancel, call jump_to_toplevel().
							option:
								3: save automatically
								2: don't save
								1: prompt to save changes if they have been changed
*/
#define RCleanUp			RFrontEnd_CleanUp
void						RFrontEnd_CleanUp( int option );


/*							InitialiseGraphicsSystem
							------------------------
							Set the Dev* globals (defined in <Defn.h>) to the 
							appropriate rendering and graphics manipulation functions,
							setting any that aren't used to the special function DevNull()

							Initialise the GP globals (in <Defn.h>) to appropriate values.
							These variables specify the output characteristicsn and
							capabilities of the system. 
							
*/
#define MacDeviceDriver		RFrontEnd_InitialiseGraphicsSystem
int							RFrontEnd_InitialiseGraphicsSystem( char**, int, double*, int );


/*							AdjustCursor
							------------
							Set the cursor to whatever is appropriate. If inIsBusy is true
							set the cursor to a watch cursor or something similar.
*/
#define RBusy				RFrontEnd_AdjustCursor
void						RFrontEnd_AdjustCursor( int inIsBusy );


/*							R_InitialData
							------------
							Calls R_RestoreGlobalEnv which loads the .RData file.
*/
/*define R_InitialData		*/
void						R_InitialData( void );



/*							R_OpenLibraryFile
							-----------------
							Calls R_RestoreGlobalEnv which loads the .RData file.
*/
/*define R_OpenLibraryFile		*/
FILE*						R_OpenLibraryFile( char* inFileName );




/*
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Command window functions
	
	The only two that really matter here are the first two.
	The others can just have stubs.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*/

/*							ReadStdInput
							------------
							Put characters entered by the user into the buffer specified, 
							not exceeding the inBufferSize parameter.
*/
#define ReadKBD				RFrontEnd_ReadStdInput
int							RFrontEnd_ReadStdInput( char* outBuffer, int inBufferSize );


/*							WriteStdOutput
							--------------
							Display characters passed in the command window.
*/
#define RWriteConsole		RFrontEnd_WriteStdOutput
int							RFrontEnd_WriteStdOutput( char* inBuffer, int inBufferSize );


/*							WriteStdError
							-------------
							Display characters passed in the command window.
*/
int							RFrontEnd_WriteStdError( char* inBuffer, int inBufferSize );



/*							ResetStdOutput
							--------------
							Set the R_Console flag to 1 so that the system knows that new
							commands can be read in from the command window.
*/
#define ResetConsole		RFrontEnd_ResetStdOutput
void						RFrontEnd_ResetStdOutput( void );


/*							ClearError
							----------
							Clear errors displayed from the error window.
							Optional. Mac version does nothing.
*/
#define ClearerrConsole		RFrontEnd_ClearError
void						RFrontEnd_ClearError( void );


/*							FlushStdOutput
							--------------
							Ensure that any characters that haven't been sent out to the
							command window are.
*/
#define FlushConsole		RFrontEnd_FlushStdOutput
void						RFrontEnd_FlushStdOutput( void );







/*
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Command-line function calls
	
	These functions are 'S' expression functions, and their
	entries are in the function lookup table of built-in C
	functions. These are executed when the user types the string
	listed just below the main description.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*/

/*							EditObject
							----------
							Allow the user to textually edit the contents of the 
							specified object, returning the object in a vector of
							lines (see an existing version for an example).

							edit( <object to edit> ) */
#define do_macedit			RFrontEnd_EditObject
SEXP 						RFrontEnd_EditObject(SEXP call, SEXP op, SEXP args, SEXP env);


/*							DumpImage
							----------
							Dump an image of the current workspace to disk.
							
							dump( <file name to dump image to> ) */
#define do_dumpb			RFrontEnd_DumpImage
SEXP						RFrontEnd_DumpImage(SEXP call, SEXP op, SEXP args, SEXP env);


/*							SystemCall
							----------
							Allow the user to execute host-system calls.
							Not supported on Mac and Windows.
							
							system( <call>, <parameters> )*/
#define do_system			RFrontEnd_SystemCall
SEXP						RFrontEnd_SystemCall(SEXP call, SEXP op, SEXP args, SEXP env);


/*							GetMachineDetails
							-----------------
							Return a string of the name of the system. Mac version does this:
	
								return( mkString("Macintosh" );
							
							Later it will return other environment details
													
							machine() */
#define do_machine			RFrontEnd_GetMachineDetails
SEXP						RFrontEnd_GetMachineDetails(SEXP call, SEXP op, SEXP args, SEXP env);


#endif
