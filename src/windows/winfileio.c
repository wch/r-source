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
#include <direct.h>

/*
 *
 * R versions of the standard C file operations
 *
 */
extern char RFName[RBuffLen];
extern BOOL CALLBACK RSysDat(HWND, UINT, WPARAM, LPARAM);
extern BOOL CALLBACK RUserDat(HWND, UINT, WPARAM, LPARAM);

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
static HWND RFileWnd, RDirWnd;

extern void sysdata(void)
{
    DialogBox(RInst, "SysDatFile", RConsole, (DLGPROC) RSysDat);
    SetFocus(RConsole);
}

extern void userdata(void)
{
    DialogBox(RInst, "UserDatFile", RConsole, (DLGPROC) RUserDat);
    SetFocus(RConsole);
}

    
static void SetFileDir(void)
{   
    SEXP ddirs, ncall;
    int i, len;

    PROTECT(ncall=allocList(1));
    CAR(ncall) = install("datadirs");
    TYPEOF(ncall) = LANGSXP;
    ddirs = eval(ncall,R_NilValue);
    UNPROTECT(1);
    PROTECT(ddirs);
    len = length(ddirs);
    SendMessage(RDirWnd, WM_SETREDRAW, FALSE, 0);
    for(i=0;i<len;i++) 
        SendMessage(RDirWnd, LB_ADDSTRING,0, (LPARAM) CHAR(STRING(ddirs)[i]));
    if( len >= 1 ) /* select item 1 */
        SendMessage(RDirWnd, LB_SETCURSEL, 0, 0);
    SendMessage(RDirWnd, WM_SETREDRAW, TRUE, 0);
    UNPROTECT(1);
}

/* get the currently selected directory and list all the doc files */
static void SetFileNames(void)
{
    int i, slen;
    char fbuff[255];

    if( RDirWnd != NULL ) {
        i = SendMessage(RDirWnd, LB_GETCURSEL, 0, 0);
        slen = SendMessage(RDirWnd, LB_GETTEXTLEN, i, 0);
        if(slen < 255)
                slen = SendMessage(RDirWnd, LB_GETTEXT, i, (LPARAM) fbuff);
        strcat((char*) fbuff,"\\base\\*.doc");
    }
    else {
        getcwd(fbuff, 255);
        strcat((char*) fbuff,"\\*.*");
    }
    
    SendMessage(RFileWnd, LB_RESETCONTENT, 0, 0L);

    i = SendMessage(RFileWnd, LB_DIR, DDL_READWRITE,(LONG) (LPTSTR) fbuff);

    i = SendMessage(RFileWnd, LB_SETCURSEL, 0, 0);
}
   
static void ReadDataFile(char *filename)
{

    SEXP fname, ncall, t;

    PROTECT(ncall=allocList(2));
    CAR(ncall) = install("scan");
    TYPEOF(ncall) = LANGSXP;
    PROTECT(fname = allocVector(STRSXP, 1));
    PROTECT(t = allocString(strlen(filename)));
    strcpy(CHAR(t), filename);
    STRING(fname)[0]=t;
    CADR(ncall) = fname;
    t = eval(ncall,R_NilValue);
    UNPROTECT(3);
}


extern BOOL CALLBACK RSysDat(HWND hDlg, UINT message,
        WPARAM wParam,LPARAM lParam)
{
    int i;
    char fname[255], tname[255];

        switch( message) {
                case WM_INITDIALOG:  /* initialize the list boxes */
                        RFileWnd = GetDlgItem(hDlg, RDG_FILE);
                        RDirWnd = GetDlgItem(hDlg, RDG_DIR);
                        if( RFileWnd != NULL && RDirWnd != NULL ) {
                                SetFileDir();
                                SetFocus(RFileWnd);
                        }
                        return TRUE;
                case WM_COMMAND:
                        switch( LOWORD(wParam) ) {                           
                            case IDOK:
                                i = SendMessage(RFileWnd, LB_GETCURSEL, 0, 0);
                                SendMessage(RFileWnd, LB_GETTEXT, i, (LPARAM) fname);
                                i = SendMessage(RDirWnd, LB_GETCURSEL, 0, 0);
                                SendMessage(RDirWnd, LB_GETTEXT, i, (LPARAM) tname);
                                strcat(tname,"\\base\\");
                                strcat(tname,fname);
                                ReadDataFile(tname);
                                EndDialog(hDlg, TRUE);
                                return TRUE;
                            case IDCANCEL:
                                EndDialog(hDlg, TRUE);
                                return TRUE;
                            case RDG_DIR:
                                SetFileNames();
                                return TRUE; 
                        }
                        break;
        }
        return FALSE;
}

#define USERSTRLEN 30

static getdlg(HWND hDlg, int item, char* name, char* value)
{
    HWND hwndEdit;
    int i;
    char outname[30];
    
        hwndEdit = GetDlgItem(hDlg, item);
        i = GetWindowTextLength(hwndEdit);
        if (i < USERSTRLEN ) 
                i = GetWindowText(hwndEdit, value, USERSTRLEN);
        else {
                sprintf(outname, "%s string is too long",name);
                MessageBox(RFrame, outname,"R read.file", MB_OK);
        }
}

extern BOOL CALLBACK RUserDat(HWND hDlg, UINT message,
        WPARAM wParam, LPARAM lParam)
{
    static int header, asis;
    int skip, i, sepn;
    char sep[USERSTRLEN], nastrings[USERSTRLEN];
    HWND hwndEdit;
    
    switch(message ) {
        case WM_INITDIALOG: /* initialize the damn thing */
                asis = 0;
                CheckRadioButton(hDlg, RDG_ASISF,RDG_ASIST,RDG_ASISF);
                header = 0;
                CheckRadioButton(hDlg, RDG_HEADF, RDG_HEADT, RDG_HEADF);
                hwndEdit = GetDlgItem(hDlg, RDG_SEP);
                SetWindowText(hwndEdit,"");
                hwndEdit = GetDlgItem(hDlg, RDG_NASTRINGS);
                SetWindowText(hwndEdit,"NA");
                hwndEdit = GetDlgItem(hDlg, RDG_SKIP);
                SetWindowText(hwndEdit,"0");
                RFileWnd = GetDlgItem(hDlg, RDG_FILE);
                RDirWnd = NULL;
                if( RFileWnd != NULL ) {
                    SetFileNames();
                    SetFocus(RFileWnd);
                }
                return TRUE;
        case WM_COMMAND:
                switch( LOWORD(wParam) ) {
                     case IDOK:
                        getdlg(hDlg, RDG_SKIP, "skip", sep);
                        if( isdigit(*sep) )
                                skip = atoi(sep);
                        else {
                            MessageBox(RFrame, "invalid skip, ignored","R read.file", MB_OK);
                            skip = 0;
                        }
                        getdlg(hDlg, RDG_SEP, "sep", sep);
                        sepn = strlen(sep);                            
                        getdlg(hDlg, RDG_NASTRINGS, "na.strings", nastrings);
                        i = strlen(nastrings);
                        ReadDataFile("aaa");
                        EndDialog(hDlg, TRUE);
                        return TRUE;
                     case IDCANCEL:
                        EndDialog(hDlg, TRUE);
                        return TRUE;
                     case RDG_ASISF:
                     case RDG_ASIST:
                        if (!asis && LOWORD(wParam) == RDG_ASIST) {
                                CheckRadioButton(hDlg, RDG_ASISF, RDG_ASIST, RDG_ASIST);
                                asis = 1;
                        }
                        else if ( asis && LOWORD(wParam) == RDG_ASISF) {
                                CheckRadioButton(hDlg, RDG_ASISF, RDG_ASIST, RDG_ASISF);
                                asis = 0;
                        }
                        return TRUE;
                     case RDG_HEADT:
                     case RDG_HEADF:
                        if (!header && LOWORD(wParam) == RDG_HEADT) {
                                CheckRadioButton(hDlg, RDG_HEADF, RDG_HEADT, RDG_HEADT);
                                header = 1;
                        }
                        else if ( header && LOWORD(wParam) == RDG_HEADF) {
                                CheckRadioButton(hDlg, RDG_HEADF, RDG_HEADT, RDG_HEADF);
                                header = 0;
                        }                        
                        return TRUE;
                }
        break;
    }
    return FALSE;
}
            
