 /*  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995  Robert Gentleman and Ross Ihaka
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

#include "wincons.h"
#include "Defn.h"
#include <stdio.h>
#include <string.h>
#include <winbase.h>
#include "Fileio.h"

/*
 *
 * R versions of the standard C file operations
 *
 */
extern char RFName[RBuffLen];

FILE *R_fopen(const char *filename, const char *mode)
{
    FILE *fp;
    int i=0,j=0,k;
    char *fnp,*sufp,*fnp2;
    
        fp = fopen(filename, mode);
        if( fp == NULL ) 
            if( R_WinVersion < 4.0 ) { /* see if it's a long file name */
                fnp=strrchr(filename,'\\');
                fnp2 = strrchr(filename,'/');
                sufp=strrchr(filename,'.');
                if(fnp != NULL )
                        i = strlen(fnp);
                if(fnp2 != NULL )
                        j = strlen(fnp2);
                if(fnp == NULL && fnp2==NULL ) {
                        i = strlen(filename);
                        j = i;
                }
                k=strlen(sufp);
                i = min(i,j);
                if( i-k > 9 || k > 4 ) {
                    RFName[0] ='\0';
                    Win_ROpenDlg(RClient, filename);
                    fp = fopen(RFName, mode);
                }
            }               
        return(fp);
}

SEXP do_getenv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    char *envval;
    int i, j;

    checkArity(op, args);

    if( !isString(CAR(args)) )
        errorcall(call,"wrong type for argument\n");
    
    i= LENGTH(CAR(args));
    PROTECT(ans=allocVector(STRSXP,i));
    for(j=0; j<i; j++ ) {   
        envval = getenv(CHAR(STRING(CAR(args))[j]));
        if (envval == NULL )
          STRING(ans)[j]=mkChar("");
        else      
          STRING(ans)[j]=mkChar(envval);
    }
    UNPROTECT(1);
    return(ans);
}
/*
        args should contain:
        1) the directory to look in
        2) the name of the file wanted
        3) .lib.loc telling all libraries to be searched
*/

SEXP do_sysfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
        SEXP ans;
        char tmp[RBuffLen],tmp2[RBuffLen], *home, pbuf[MAX_PATH];
        LPTSTR lpFile;
        WIN32_FIND_DATA FileData;
        HANDLE hSearch;
        int done=0, more=1;
        

        checkArity(op, args);
        PROTECT(ans = allocVector(STRSXP,1));

        if((home = getenv("RHOME")) == NULL)
                error("RHOME is not set in your environment\n");
        
        if( !isString(CAR(args)) || LENGTH(CAR(args)) != 1 )
                errorcall(call, "invalid dir argument\n");
        if( !isString(CADR(args)) || LENGTH(CADR(args)) != 1 )
                errorcall(call, "invalid name argument\n");

        sprintf(tmp2,"%s",home);
        sprintf(tmp,"%s\\%s", home,CHAR(STRING(CAR(args))[0]));
        if( !SearchPath(tmp, CHAR(STRING(CADR(args))[0]), NULL, MAX_PATH, pbuf, &lpFile) ) {
               /*need to look at all subdirectories */
               sprintf(tmp,"%s\\%s\\*.*", home,CHAR(STRING(CAR(args))[0]));
               hSearch = FindFirstFile(tmp, &FileData);
               SetLastError(NO_ERROR);
               while ( more && !done )  {
                  if( (FileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0 ) {
                     sprintf(tmp2,"%s\\%s\\%s",home,CHAR(STRING(CAR(args))[0]),FileData.cFileName);
                     done = SearchPath(tmp2, CHAR(STRING(CADR(args))[0]), NULL, MAX_PATH, pbuf, &lpFile);                         
                  }
                  more = FindNextFile(hSearch, &FileData);
               }
               FindClose(hSearch);
               if( !more && GetLastError() != ERROR_NO_MORE_FILES)
                   error("file handling error\n");
               if( !done ) {
                   STRING(ans)[0]=mkChar("");
                   UNPROTECT(1);
                   return(ans);
                }
        }
        STRING(ans)[0]=mkChar(pbuf);
        UNPROTECT(1);
        return (ans);
}
