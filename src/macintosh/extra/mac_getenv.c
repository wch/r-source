/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file mac_getenv.c
 *  Copyright (C) 1998--2000  Guido Masarotto and Brian Ripley
 *                2001        Stefano M. Iacus and the R core team
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
 *
 *****************************************************************************
 *
 *  This file implement the mac_getenv by reading an '.Renviron' file.
 *  This is absolutely non standard on MacOS system pre MacOS X.
 *
 *  Implemented in R 1.2.1.  Stefano M. Iacus, Jan 2001
 *
 * 
 *
 * mac_getenv() is adapted from the code of 'getenv()' of the
 * Info-Zip distribution (license terms follows):
 *
 **************************************************************************
 *                                                                        *
 * Copyright (c) 1990-2000 Info-ZIP.  All rights reserved.                *
 *                                                                        *
 * See the accompanying file LICENSE, version 2000-Apr-09 or later        *
 * (the contents of which are also included in zip.h) for terms of use.   *
 * If, for some reason, all these files are missing, the Info-ZIP license *
 * also may be found at:  ftp://ftp.info-zip.org/pub/infozip/license.html *
 *                                                                        *
 **************************************************************************
 *
 *
 * Background:
 * Under Unix: Each Process (= running Program) has a set of
 * associated variables. The variables are called enviroment
 * variables and, together, constitute the process environment.
 * These variables include the search path, the terminal type,
 * and the user's login name.
 *
 * Unfortunatelly the MacOS has no equivalent. So we need
 * a file to define the environment variables.
 * Name of this file is ".Renviron". It can be placed
 * in the same folder as  R folder or in the
 * preference folder of the system disk.
 * If R founds the ".Renviron" file in the current
 * folder of the ".Renviron" file in the
 * preference folder will be ignored.
 *
 * An environment variable has a name and a value:
 * Name=Value
 * Note: Spaces are significant:
 * R_OPT=-r  and
 * R_OPT = -r are different !!!
 *
 * Please note that this routine is case sensitive, i.e. R_Opt != R_OPT
 *   
 *
 * What is changed from the Info-Zip code of getenv() :
 *
 * 1) does not depend on the several functions coming from the
 *    the Info-Zip distribution
 * 2) uses standard Apple interfaces to access files and directories instead
 *    of the private functions of the Info-Zip distribution.
 * 3) we use several functions from the Info-Zip distribution. They are:
 *     get_char, unget_char, get_string, skip_comments, load_entry,
 *     ParseLine, Set_LineNum.
 *
 *   
 *  Stefano M. Iacus - Jan 2001
 */

/*****************************************************************************/
/*  Includes                                                                 */
/*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
//#include <unix.h>
#include <Files.h>
#include <Folders.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "Defn.h"

extern char*	R_Home;	


typedef struct _EnviromentPair {
    char *key;
    char *value;
} EnviromentPair;


EnviromentPair *ParseLine(char *line)
{
    char *tmpPtr;
    static EnviromentPair Env;
    unsigned short length = strlen(line);

   // Env->key   = "";
   // Env->value = "";

    for (tmpPtr = line; *tmpPtr; tmpPtr++)
    {
	if (*tmpPtr == '=')
        {
	    *tmpPtr = 0;
	    Env.key = line;
	    if (strlen(Env.key) < length)
            {
		Env.value = ++tmpPtr;
            }
	    return &Env;
        }
    }
    return &Env;
}


/*****************************************************************************/
/*  Module level Vars                                                        */
/*****************************************************************************/

static char ListAllRKeyValues = 0;
extern unsigned LineNumber = 0;
Boolean IgnoreREnvironment = false;  /* used by mac_getenv */

/*****************************************************************************/
/*  Macros, typedefs                                                         */
/*****************************************************************************/


#define MAX_COMMAND 1024


/*****************************************************************************/
/*  Prototypes: They come from the Info-Zip distribution                     */
/*****************************************************************************/

int  get_char(FILE *file);
void unget_char(int ch,FILE *file);
int  get_string(char *string,int size, FILE *file, char *terms);
void skip_comments(FILE *file);
char *load_entry(FILE *file);
EnviromentPair *ParseLine(char *line);
void Set_LineNum(unsigned ln);
OSErr FSpFindFolder_Name(short vRefNum, OSType folderType, 
			 Boolean createFolder,FSSpec *spec, 
			 unsigned char *name);

/* Other prototypes */

FILE * FSp_fopen(ConstFSSpecPtr spec, const char * open_mode);

FILE * FSp_fopen(ConstFSSpecPtr spec, const char * open_mode){

    FILE *fp;
    SInt16 				pathLen;
    Handle 				pathName=NULL;
    char				filetoopen[FILENAME_MAX];
    
    FSpGetFullPath(spec, &pathLen, &pathName);
    HLock((Handle) pathName);
	strncpy(filetoopen, *pathName, pathLen);
	filetoopen[pathLen] = '\0';
	HUnlock((Handle) pathName);

	fp = fopen(filetoopen,"r");

    return(fp);
}

char *mac_getenv(const char *name);

char err_str[] = "\0";
extern Boolean Have_Console;

char *mac_getenv(const char *name)
{
    FILE *fp; 
    char *LineStr = NULL;
    EnviromentPair *Env1;
    FSSpec spec;
    OSErr err;
    char temp_path[PATH_MAX];

    if (IgnoreREnvironment)
	return NULL;  /* user wants to ignore the environment vars */

    if (name == NULL)
	return err_str;//NULL;
    
    if(strcmp(name,"R_HOME")==0)
     return R_Home;

    sprintf(temp_path,"%s:.Renviron",R_Home);
    
    err = FSpLocationFromFullPath(strlen(temp_path),temp_path,&spec);
  
/* try open the file in the folder R_HOME:etc */


    fp = FSp_fopen(&spec,"r");
    if (fp == NULL)
    { /* Okay, let's try to open the file in the preference folder */
	FSpFindFolder_Name(
	    kOnSystemDisk,
	    kPreferencesFolderType,
	    kDontCreateFolder,
	    &spec,
	    "\p.Renviron");
	fp = FSp_fopen(&spec,"r");

	if (fp == NULL)
        {
	    return err_str;//NULL; /* there is no enviroment-file */
        }
    }

    LineStr = load_entry(fp);
    while (LineStr != NULL)
    {   /* parse the file line by line */
	Env1 = ParseLine(LineStr);
	if (strlen(Env1->value) > 0)
        {       /* we found a key/value pair */
	    if (ListAllRKeyValues)
		Rprintf("\n   Line:%3d   [%s] = [%s]", LineNumber,
			Env1->key,Env1->value);
	    if (strcmp(name,Env1->key) == 0)
            {   /* we found the value of a given key */
		return Env1->value;
            }
        }
	LineStr = load_entry(fp);  /* read next line */
    }
    fclose(fp);

    return err_str; 
}


/***************************************************************/
/*  Functions  from the Info-Zip distribution                  */
/***************************************************************/


OSErr FSpFindFolder_Name(
    short vRefNum,          /* Volume reference number. */
    OSType folderType,      /* Folder type taken by FindFolder. */
    Boolean createFolder,   /* Should we create it if non-existant. */
    FSSpec *spec,           /* Pointer to resulting directory. */
    unsigned char *name)    /* Name of the file in the folder */
{
    short foundVRefNum;
    long foundDirID;
    OSErr err;

    err = FindFolder(vRefNum, folderType, createFolder,
                     &foundVRefNum, &foundDirID);
    if (err != noErr) {
        return err;
    }

    err = FSMakeFSSpec(foundVRefNum, foundDirID, name, spec);
    return err;
}

/* this function reads one file entry -- the next -- from a file.
*  it skips any leading blank lines, ignores comments, and returns
*  NULL if for any reason the entry can't be read and parsed.
*/

char *load_entry(FILE *file)
{
    int ch;
    static char cmd[MAX_COMMAND];

    skip_comments(file);

    ch = get_string(cmd, MAX_COMMAND, file, "\n");

    if (ch == EOF) {
        return NULL;
    }

    return cmd;
}


/* get_string(str, max, file, termstr) : like fgets() but
 *      (1) has terminator string which should include \n
 *      (2) will always leave room for the null
 *      (3) uses get_char() so LineNumber will be accurate
 *      (4) returns EOF or terminating character, whichever
 */
int get_string(char *string, int size, FILE *file, char *terms)
{
    int ch;

    while (EOF != (ch = get_char(file)) && !strchr(terms, ch)) {
        if (size > 1) {
            *string++ = (char) ch;
            size--;
        }
    }

    if (size > 0) {
        *string = '\0';
    }

    return ch;
}


/* skip_comments(file) : read past comment (if any)
 */
void skip_comments(FILE *file)
{
    int ch;

    while (EOF != (ch = get_char(file)))
    {
        /* ch is now the first character of a line.
         */

        while (ch == ' ' || ch == '\t') {
            ch = get_char(file);
	}

        if (ch == EOF) {
            break;
	}

        /* ch is now the first non-blank character of a line.
         */

        if (ch != '\n' && ch != '#') {
            break;
	}

        /* ch must be a newline or comment as first non-blank
         * character on a line.
         */

        while (ch != '\n' && ch != EOF) {
            ch = get_char(file);
	}

        /* ch is now the newline of a line which we're going to
         * ignore.
         */
    }

    if (ch != EOF) {
        unget_char(ch, file);
    }
}


/* get_char(file) : like getc() but increment LineNumber on newlines
 */
int get_char(FILE *file)
{
    int ch;

    ch = getc(file);
    if (ch == '\n')
        {
        Set_LineNum(LineNumber + 1);
        }

    return ch;
}

/* unget_char(ch, file) : like ungetc but do LineNumber processing
 */
void unget_char(int ch, FILE *file)
{
    ungetc(ch, file);
    if (ch == '\n')
    {
        Set_LineNum(LineNumber - 1);
    }
}

void Set_LineNum(unsigned ln)
{
    LineNumber = ln;
}




