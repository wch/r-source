/* Stefano M. Iacus - Jan 2001 
   getenv() is adapted for R for MacOS from the following code:
*/
/*
  Copyright (c) 1990-2000 Info-ZIP.  All rights reserved.

  See the accompanying file LICENSE, version 2000-Apr-09 or later
  (the contents of which are also included in zip.h) for terms of use.
  If, for some reason, all these files are missing, the Info-ZIP license
  also may be found at:  ftp://ftp.info-zip.org/pub/infozip/license.html
*/
/*

This file implements the getenv() function.

#  Background:
#  Under Unix: Each Process (= running Program) has a set of
#  associated variables. The variables are called enviroment
#  variables and, together, constitute the process environment.
#  These variables include the search path, the terminal type,
#  and the user's login name.

#  Unfortunatelly the MacOS has no equivalent. So we need
#  a file to define the environment variables.
#  Name of this file is "MacZip.Env". It can be placed
#  in the current folder of MacZip or in the
#  preference folder of the system disk.
#  If MacZip founds the "MacZip.Env" file in the current
#  the folder of MacZip the "MacZip.Env" file in the
#  preference folder will be ignored.

#  An environment variable has a name and a value:
#  Name=Value
#  Note: Spaces are significant:
#  ZIPOPT=-r  and
#  ZIPOPT = -r are different !!!
*/

/* This routine is adapted to search for a file ".Renviron" in the
   directory ":.etc" supposed to be found in the same directory
   of the R application.
   
   Stefano M. Iacus - Jan 2001
*/

/*****************************************************************************/
/*  Includes                                                                 */
/*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <unix.h>
#include <Files.h>
#include <Folders.h>

#include "pathname.h"
#include "helpers.h"

/*****************************************************************************/
/*  Module level Vars                                                        */
/*****************************************************************************/

static char ListAllRKeyValues = 0;
extern unsigned LineNumber = 0;
static char CompleteEnvPath[NAME_MAX];
Boolean IgnoreREnvironment    = false;  /* used by dialog.c and initfunc.c
                                          of the Mainapp */

/*****************************************************************************/
/*  Macros, typedefs                                                         */
/*****************************************************************************/

typedef struct _EnviromentPair {
    char *key;
    char *value;
} EnviromentPair;


#define MAX_COMMAND 1024


/*****************************************************************************/
/*  Prototypes                                                               */
/*****************************************************************************/


int get_char(FILE *file);
void unget_char(int ch,FILE *file);
int get_string(char *string,int size, FILE *file, char *terms);
void skip_comments(FILE *file);
char *load_entry(FILE *file);
char *mac_getenv(const char *name);
EnviromentPair *ParseLine(char *line);
OSErr FSpFindFolder_Name(short vRefNum, OSType folderType,
                         Boolean createFolder,FSSpec *spec, unsigned char *name);
FILE * FSp_fopen(ConstFSSpecPtr spec, const char * open_mode);
EnviromentPair *ParseLine(char *line);
void Set_LineNum(unsigned ln);

/***************************************************************/
/*  Functions                                                  */
/*  These functions are already defined in the macunzip        */
/*  distribution. File: src/macintosh/unzip/source/getenv.c    */
/*  Stefano M. Iacus -- Jan 2001                               */
/***************************************************************/


/* get_string(str, max, file, termstr) : like fgets() but
 *      (1) has terminator string which should include \n
 *      (2) will always leave room for the null
 *      (3) uses get_char() so LineNumber will be accurate
 *      (4) returns EOF or terminating character, whichever
 */

/* 
   void Set_LineNum(unsigned ln)
*/


/* get_char(file) : like getc() but increment LineNumber on newlines
 */

/* skip_comments(file) : read past comment (if any)
 */

/* unget_char(ch, file) : like ungetc but do LineNumber processing
 */


/* this function reads one file entry -- the next -- from a file.
*  it skips any leading blank lines, ignores comments, and returns
*  NULL if for any reason the entry can't be read and parsed.
*/
/*
  char *load_entry(FILE *file)
*/

/*
  EnviromentPair *ParseLine(char *line)
*/

extern char*	R_Home;	

char *mac_getenv(const char *name)
{
FILE *fp;
char *LineStr = NULL;
EnviromentPair *Env1;
FSSpec spec;
OSErr err;
char temp_path[NAME_MAX];


if (IgnoreREnvironment)
    return NULL;  /* user wants to ignore the environment vars */

if (name == NULL)
    return NULL;
    
strcpy(temp_path,"etc:.Renviron");

GetCompletePath(CompleteEnvPath,temp_path,&spec,&err);

/* try open the file in the current folder */
fp = FSp_fopen(&spec,"r");
if (fp == NULL)
    { /* Okey, lets try open the file in the preference folder */
    FSpFindFolder_Name(
                   kOnSystemDisk,
                   kPreferencesFolderType,
                   kDontCreateFolder,
                   &spec,
                   "\p.Renviron");
    fp = FSp_fopen(&spec,"r");
    if (fp == NULL)
        {
        return NULL; /* there is no enviroment-file */
        }
    }

LineStr = load_entry(fp);
while (LineStr != NULL)
    {   /* parse the file line by line */
    Env1 = ParseLine(LineStr);
    if (strlen(Env1->value) > 0)
        {       /* we found a key/value pair */
        if (ListAllRKeyValues)
            Rprintf("\n   Line:%3d   [%s] = [%s]",LineNumber,Env1->key,Env1->value);
        if (stricmp(name,Env1->key) == 0)
            {   /* we found the value of a given key */
            return Env1->value;
            }
        }
    LineStr = load_entry(fp);  /* read next line */
    }
fclose(fp);

return NULL;
}




/*
OSErr FSpFindFolder_Name(
    short vRefNum,          // Volume reference number. 
    OSType folderType,      // Folder type taken by FindFolder. 
    Boolean createFolder,   // Should we create it if non-existant. 
    FSSpec *spec,           // Pointer to resulting directory. 
    unsigned char *name)    // Name of the file in the folder 
*/











