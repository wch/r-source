/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999  Wing Kwong (Tiki) WAN
 *  Copyright (C) 2000--2001  Stefano Iacus and the R core team
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
 */
/*
	 
	RandTool.c
	by wing kwong (Tiki), WAN 3/2/99
	
	Description
	
  This file is used some small routines which are used to link 
  with the internal R.
*/


#include "RIntf.h"

extern long start_Time;
extern long last_Time;
char genvString[256];

/* doCopyPString
*/
void  doCopyPString(Str255 sourceString,Str255 destinationString)
{
    SInt16 stringLength;

    stringLength = sourceString[0];
    BlockMove(sourceString + 1,destinationString + 1,stringLength);
    destinationString[0] = stringLength;
}




/* R_OpenFile
*/
FILE* R_OpenFile1(char *file)
{
    FILE* fp;

    /* Max file length is 256 characters */
    fp = fopen(file, "r");
    return fp;
}

/* R_OpenLibraryFile
*/
FILE* R_OpenLibraryFile1(char *file){
   FILE* fp;
   SInt16 totLen;
   char str[256] = ":library:base:R:";

   /* Max file length is 256 characters */
   strcpy(&str[16], file);
   totLen = strlen(file) + 16;
   str[totLen] = '\0';
   fp = fopen(str, "rt");
   return fp;
}


/* R_OpenSysInitFile
*/
FILE* R_OpenSysInitFile1(void)
{
    return R_OpenLibraryFile1("Rprofile");
}





