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
extern BOOL CALLBACK RRtab(HWND, UINT, WPARAM, LPARAM);

/* static variables for the read.table options */

#define USERSTRLEN 30
static int RT_header, RT_asis, RT_skip;
static char RT_sep[USERSTRLEN], RT_nastrings[USERSTRLEN];


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

        /* return a STRSXP with a single element that is a valid temporary
           file name
        */

SEXP do_tempfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
        SEXP ans;
        char tmpP[MAX_PATH+1], tmpF[MAX_PATH+1];

        GetTempPath(MAX_PATH, tmpP);
        GetTempFileName(tmpP, CHAR(STRING(CAR(args))[0]), 0, tmpF);
        ans = mkString(tmpF);
        return ans;
}

/* wincat cat's infile to outfile. If they are disk files then append is used to
determine whether to append the information. If outfile is R_NilValue then infile
will be printed across the terminal, a line at a time
*/

SEXP do_wincat(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fname;
    FILE *fpin, *fpout;
    int append, c;
    char flags[1], ibuff[255];

    checkArity(op, args);

    fname = CAR(args);
    if (! isString(fname) || length(fname) != 1 )
        errorcall(call, "incorrect infile name");

    if( ! isString(CADR(args)) || length(CADR(args)) != 1 )
        errorcall(call, "incorrect outfile name");

    if( ! isLogical(CADDR(args)) || length(CADDR(args)) != 1 )
        errorcall(call, "incorrect append argument");

    if ((fpin = R_fopen(CHAR(STRING(fname)[0]), "r")) == NULL )
        errorcall(call, "could not open infile for reading");

    fname = CADR(args);

    if( strlen(CHAR(STRING(fname)[0])) == 0 ) { /* write to the console */
        while (fgets(ibuff,100,fpin) != NULL) 
            R_WriteConsole(ibuff, strlen(ibuff));
        fclose(fpout);
        return R_NilValue;
    }
    append = asLogical(CADDR(args));

    if ( !append )
        flags[0] = 'w';
    else
        flags[0] = 'a';
    
    if ((fpout = R_fopen(CHAR(STRING(fname)[0]), flags)) == NULL )
        errorcall(call, "could not open outfile for writing");

    while ((c = getc(fpin)) != EOF )
        putc(c, fpout);
    fclose(fpin);
    fclose(fpout);    
    return R_NilValue;
}

/* getbasename  finds the directory path to the file being opened */


static int getbasename(char *bn, char *fn, char *ifile)
{

    char *p, t[MAX_PATH], fname[MAX_PATH], *delims={"/\\"}, *bs={"/"};
    int n, ebs;

    strcpy(fname, ifile);

    n = strlen(fname);
    if (fname[n-1] == delims[0] || fname[n-1] == delims[1] )
        ebs = 1;
    else
        ebs = 0;
        
    bn[0] = '\0';
    p = strtok(fname, delims);
    strcpy(t, p);
    
    while (p != NULL) { 
        p = strtok(NULL, delims);
        if (p != NULL) {
                strcat(bn,t);
                strcat(bn,bs);
                strcpy(t,p);
        }
    }
    strcpy(fn, t);  /* stick the file name into fn */
    return ebs;    
}

SEXP do_sysfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
        SEXP ans;
        char tmp[MAX_PATH],tmp2[MAX_PATH], bn[MAX_PATH], *bp;
        WIN32_FIND_DATA FileData;
        HANDLE hSearch;
        int j, len, nfiles=0, i, ebs;
        

        checkArity(op, args);
        
        if( !isString(CAR(args)) )
                errorcall(call, "invalid dir argument\n");
        len = length(CAR(args));

        for( i=0; i<len; i++) {
			sprintf(tmp,"%s",CHAR(STRING(CAR(args))[i]));
			bp = tmp;
			if ( tmp[0] == '\\' || tmp[0] == '/' )
				bp++;
            hSearch = FindFirstFile(bp, &FileData);
            if( hSearch != INVALID_HANDLE_VALUE ) {
                nfiles++;
                while ( FindNextFile(hSearch, &FileData) )
                        nfiles++;
                FindClose(hSearch);
            }
        }
        
        if (nfiles == 0 ) {
            ans = mkString("");
            return ans;
        }
        
        PROTECT(ans = allocVector(STRSXP,nfiles));
        j = 0;
        for( i=0; i<len; i++) {
            sprintf(tmp,"%s",CHAR(STRING(CAR(args))[i]));
			bp = tmp;
			if( tmp[0] == '\\' || tmp[0] == '/' )
				bp++;
            hSearch = FindFirstFile(bp, &FileData);
            if( hSearch != INVALID_HANDLE_VALUE ) {
                ebs = getbasename(bn, tmp2, bp);
                if( strlen( FileData.cAlternateFileName ) > 0 ) 
                        sprintf(tmp2,"%s%s", bn, FileData.cAlternateFileName);
                else
                        sprintf(tmp2,"%s%s", bn, FileData.cFileName);
                if( ebs )
                        strcat(tmp2,"/");
                STRING(ans)[j++]=mkChar(tmp2);
                while ( FindNextFile(hSearch, &FileData) ) {
                    if( strlen( FileData.cAlternateFileName ) > 0 )
                        sprintf(tmp2,"%s%s", bn, FileData.cAlternateFileName);
                    else
                        sprintf(tmp2,"%s%s", bn, FileData.cFileName);
                   STRING(ans)[j++]=mkChar(tmp2);
                } 
                FindClose(hSearch);
            }
        }
        UNPROTECT(1);
        return ans;
}


#ifdef OLD
            if( !more && GetLastError() != ERROR_NO_MORE_FILES)
                error("file handling error\n");            
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
                STRING(ans)[i]=mkChar(pbuf);
        }
        UNPROTECT(1);
        return (ans);
}
#endif


static HWND RFileWnd, RDirWnd;

extern void sysdata(void)
{
    DialogBox(RInst, "SysDatFile", RConsole, (DLGPROC) RSysDat);
    SetFocus(RConsole);
}

extern void userdata(void)
{
    DialogBox(RInst, "RRtab", RConsole, (DLGPROC) RRtab);
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

/* get the currently selected directory and list all the .R files */
static void SetFileNames(void)
{
    int i, slen;
    char fbuff[255];

    if( RDirWnd != NULL ) {
        i = SendMessage(RDirWnd, LB_GETCURSEL, 0, 0);
        slen = SendMessage(RDirWnd, LB_GETTEXTLEN, i, 0);
        if(slen < 255)
                slen = SendMessage(RDirWnd, LB_GETTEXT, i, (LPARAM) fbuff);
        strcat((char*) fbuff,"\\*.R");
    }
    else {
        getcwd(fbuff, 255);
        strcat((char*) fbuff,"\\*.*");
    }
    
    SendMessage(RFileWnd, LB_RESETCONTENT, 0, 0L);

    i = SendMessage(RFileWnd, LB_DIR, DDL_READWRITE,(LONG) (LPTSTR) fbuff);
    if( RDirWnd != NULL ) {
        i = SendMessage(RDirWnd, LB_GETCURSEL, 0, 0);
        slen = SendMessage(RDirWnd, LB_GETTEXTLEN, i, 0);
        if(slen < 255)
                slen = SendMessage(RDirWnd, LB_GETTEXT, i, (LPARAM) fbuff);
        strcat((char*) fbuff,"\\*.tab");
        i = SendMessage(RFileWnd, LB_DIR, DDL_READWRITE,(LONG) (LPTSTR) fbuff);
    }
    
    SendMessage(RFileWnd, LB_SETCURSEL, (WPARAM) 0, 0);
}
   
static void ReadDataFile(char *filename)
{

    SEXP fname, ncall, t;

    PROTECT(ncall=allocList(2));
    CAR(ncall) = install("source");
    TYPEOF(ncall) = LANGSXP;
    PROTECT(fname = mkString(filename));
    CADR(ncall) = fname;
    t = eval(ncall,R_NilValue);
    UNPROTECT(2);
}

/* ReadTable is an interface to the interpreted read.table */



static void ReadTable(char *filename, int header, char *sep, int asis, char *nas, int skip)
{
    SEXP call, acall, t, s, u;
    int len=2, i;
    char *cp, bn[255], fn[255];

    if (header != 0 )   len++;
    if (strlen(sep) != 0 ) len++;
    if (asis != 0 ) len++;
    if (strcmp(nas,"NA") != 0) len++;
    if (skip != 0) len++;
    

    getbasename(bn, fn, filename);
    PROTECT(call = allocList(len));
    TYPEOF(call) = LANGSXP;
    CAR(call) = install("read.table");
    t = CDR(call);
    CAR(t) = mkString(filename);
    if( header != 0 ) {
        t=CDR(t);
        TAG(t) = install("header");
        s = allocVector(LGLSXP, 1);
        LOGICAL(s)[0] = header;
        CAR(t) = s;
    }
    if(asis != 0 ) {
        t = CDR(t);
        TAG(t) = install("as.is");
        s = allocVector(LGLSXP, 1);
        LOGICAL(s)[0] = asis;
        CAR(t) = s;
    }
    if (skip != 0 ) {
        t = CDR(t);
        TAG(t) = install("skip");
        s = allocVector(INTSXP, 1);
        INTEGER(s)[0] = skip;
        CAR(t) = s;
    }
    if (strcmp(nas,"NA") != 0) {
        t = CDR(t);
        TAG(t) = install("na.strings");
        len = 1;
        cp = nas;
        while (*cp) 
            if (*cp++ == ',' ) len++;
        PROTECT(u = allocVector(STRSXP,len));
        i = 0;
        cp = strtok(nas, ",");
        STRING(u)[i] = mkChar(cp);    
        while (cp != NULL) { 
                cp = strtok(NULL, ",");
                if (cp != NULL) {
                        i++;
                        STRING(u)[i] = mkChar(cp);
                }
        }
        CAR(t) = u;
        UNPROTECT(1);
    }  
    if (strlen(sep) != 0 ) {
        t = CDR(t);
        TAG(t) = install("sep");
        CAR(t) = mkString(sep);
    }
    cp = strtok(fn,".");
    t = mkString(cp);
    s = install("assign");
    PROTECT(acall = allocList(4));
    TYPEOF(acall) = LANGSXP;
    u = acall;
    CAR(acall) = s;
    u = CDR(u);
    CAR(u) = t;
    u = CDR(u);
    CAR(u) = call;
    u = CDR(u);
    TAG(u) = install("envir");
    CAR(u) = install(".GlobalEnv");
    eval(acall, R_NilValue);
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
                                strcat(tname,"\\");
                                strcat(tname,fname);
                                EndDialog(hDlg, TRUE);
                                if( !strcmp( strrchr(tname, '.'), ".tab" ) ) {
                                        i = MessageBox(RFrame, "Would you like to set read.table options",
                                                "R System Data", MB_YESNO );
                                        if( i == IDYES ) {
                                            strcpy(RFName, tname);
                                            DialogBox(RInst, "UserDatFile", RConsole, (DLGPROC) RUserDat);
                                            ReadTable(RFName, RT_header, RT_sep, RT_asis, RT_nastrings, RT_skip);
                                        }
                                        else       
                                            ReadTable(tname, 0, "", 0, "NA", 0);
                                }
                                else
                                        ReadDataFile(tname);
                                return TRUE;
                            case IDCANCEL:
                                EndDialog(hDlg, TRUE);
                                return TRUE;
                            case RDG_DIR:
                                SetFileNames();
                                SetFocus(RFileWnd);
                                return TRUE; 
                        }
                        break;
        }
        return FALSE;
}


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

extern BOOL CALLBACK RRtab(HWND hDlg, UINT message,
        WPARAM wParam, LPARAM lParam)
{   
    switch(message ) {
        case WM_INITDIALOG:
                RT_asis = 0;
                RT_header = 0;
                RT_skip = 0;
                strcpy(RT_sep,"");
                strcpy(RT_nastrings,"NA");
                RFileWnd = GetDlgItem(hDlg, RDG_FILE);
                if (strlen(RFName) > 0 )
                   SetWindowText(RFileWnd, RFName);
                SetFocus(RFileWnd);
                return TRUE;
        case WM_COMMAND:
                switch( LOWORD(wParam) ) {
                     case IDOK:
                        GetWindowText(RFileWnd, RFName, 255);
                        if( strlen(RFName) == 0 )
                                return TRUE;                       
                        EndDialog(hDlg, TRUE);
                        ReadTable(RFName, RT_header, RT_sep, RT_asis, RT_nastrings, RT_skip);
                        return TRUE;
                     case IDCANCEL:
                        EndDialog(hDlg, TRUE);
                        return TRUE;
                     case RRR_ARRA:
                        if( strlen(RFName) == 0 )
                                GetWindowText(RFileWnd, RFName, 255);
                        DialogBox(RInst, "UserDatFile", RConsole, (DLGPROC) RUserDat);
                        return TRUE;
                     case RDG_DIR:
                        Win_ROpenDlg(RClient, 3);
                        SetWindowText(RFileWnd, RFName);
                        return TRUE;
                }              
                break;
    }
    return FALSE;
}
        
extern BOOL CALLBACK RUserDat(HWND hDlg, UINT message,
        WPARAM wParam, LPARAM lParam)
{
    HWND  hwndEdit;
    
    switch(message ) {
        case WM_INITDIALOG: /* initialize the damn thing */
                RT_asis = 0;
                CheckRadioButton(hDlg, RDG_ASISF,RDG_ASIST,RDG_ASISF);
                RT_header = 0;
                CheckRadioButton(hDlg, RDG_HEADF, RDG_HEADT, RDG_HEADF);
                hwndEdit = GetDlgItem(hDlg, RDG_SEP);
                SetWindowText(hwndEdit,"");
                hwndEdit = GetDlgItem(hDlg, RDG_NASTRINGS);
                SetWindowText(hwndEdit,"NA");
                hwndEdit = GetDlgItem(hDlg, RDG_SKIP);
                SetWindowText(hwndEdit,"0");
                hwndEdit = GetDlgItem(hDlg, RDG_FILE);
                SetWindowText(hwndEdit, RFName);
                SetFocus(GetDlgItem(hDlg, IDOK));
                return TRUE;
        case WM_COMMAND:
                switch( LOWORD(wParam) ) {
                     case IDOK:
                        getdlg(hDlg, RDG_SKIP, "skip", RT_sep);
                        if( isdigit(*RT_sep) )
                                RT_skip = atoi(RT_sep);
                        else {
                            MessageBox(RFrame, "invalid skip, ignored","R read.file", MB_OK);
                            RT_skip = 0;
                        }
                        getdlg(hDlg, RDG_SEP, "sep", RT_sep);                      
                        getdlg(hDlg, RDG_NASTRINGS, "na.strings", RT_nastrings);
                        EndDialog(hDlg, TRUE);
                        return TRUE;
                     case IDCANCEL:
                        EndDialog(hDlg, TRUE);
                        return TRUE;
                     case RDG_ASISF:
                     case RDG_ASIST:
                        if (!RT_asis && LOWORD(wParam) == RDG_ASIST) {
                                CheckRadioButton(hDlg, RDG_ASISF, RDG_ASIST, RDG_ASIST);
                                RT_asis = 1;
                        }
                        else if ( RT_asis && LOWORD(wParam) == RDG_ASISF) {
                                CheckRadioButton(hDlg, RDG_ASISF, RDG_ASIST, RDG_ASISF);
                                RT_asis = 0;
                        }
                        return TRUE;
                     case RDG_HEADT:
                     case RDG_HEADF:
                        if (!RT_header && LOWORD(wParam) == RDG_HEADT) {
                                CheckRadioButton(hDlg, RDG_HEADF, RDG_HEADT, RDG_HEADT);
                                RT_header = 1;
                        }
                        else if ( RT_header && LOWORD(wParam) == RDG_HEADF) {
                                CheckRadioButton(hDlg, RDG_HEADF, RDG_HEADT, RDG_HEADF);
                                RT_header = 0;
                        }                        
                        return TRUE;
                }
        break;
    }
    return FALSE;
}
            
