/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file extra.c
 *  Copyright (C) 1998--1999  Guido Masarotto and Brian Ripley
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


/* extra commands for R */

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include <stdio.h>
#include "Defn.h"
#include "Fileio.h"
#include <io.h>
#include <time.h>
#include <windows.h>
#include "graphapp/ga.h"

static char DefaultFileName[MAX_PATH];

/* 
 * replacement for Windows function that uses root directory
 */
char * tmpnam(char * str)
{
    char *tmp, *tmp2;

    if(str) tmp2 = str; else tmp2 = DefaultFileName;
    tmp = getenv("TMP");
    if (!tmp) tmp = getenv("TEMP");
    if (!tmp) tmp = getenv("R_USER");
    sprintf(tmp2, "%s/RtmpXXXXXX", tmp);
    mktemp(tmp2); /* Windows function to replace X's */
    return(tmp2);
}


SEXP do_tempfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  ans;
    char *tmp, *tn, tm[MAX_PATH];
    unsigned int n, done = 0;

    WIN32_FIND_DATA fd;
    HANDLE h;
    checkArity(op, args);
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	errorcall(call, "invalid file name argument\n");
    tn = CHAR(STRING(CAR(args))[0]);
    /* try to get a new file name */
    tmp = getenv("TMP");
    if (!tmp) tmp = getenv("TEMP");
    if (!tmp) tmp = getenv("R_USER");
    for (n = 0; n < 100; n++) {
	/* try a random number at the end */
        sprintf(tm, "%s\\%s%d", tmp, tn, rand());
        if ((h = FindFirstFile(tm, &fd)) == INVALID_HANDLE_VALUE) {
	    done = 1;
	    break;
	}
        FindClose(h);
        tm[0] = '\0';
    }
    if(!done)
	error("cannot find unused tempfile name\n");
    PROTECT(ans = allocVector(STRSXP, 1));
    STRING(ans)[0] = mkChar(tm);
    UNPROTECT(1);
    return (ans);
}

SEXP do_unlink(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  fn, ans;
    char *p, tmp[MAX_PATH], dir[MAX_PATH];
    WIN32_FIND_DATA find_data;
    HANDLE fh;
    int i, nfiles, failures = 0;

    checkArity(op, args);
    fn = CAR(args);
    nfiles = length(fn);
    if (!isString(fn) || nfiles < 1)
	errorcall(call, "invalid file name argument\n");
    for(i = 0; i < nfiles; i++) {
	strcpy(tmp, CHAR(STRING(fn)[i]));
	for(p = tmp; *p != '\0'; p++)
	    if(*p == '/') *p = '\\';
	strcpy(dir, tmp);
	if ((p = strrchr(dir, '\\'))) *(++p) = '\0'; else *dir = '\0';
	/* check for wildcard matches */
	fh = FindFirstFile(tmp, &find_data);
	if (fh != INVALID_HANDLE_VALUE) {
	    strcpy(tmp, dir); strcat(tmp, find_data.cFileName);
	    failures += (unlink(tmp) !=0);
	    while(FindNextFile(fh, &find_data)) {
		strcpy(tmp, dir); strcat(tmp, find_data.cFileName);
		failures += (unlink(tmp) !=0);
	    }
	    FindClose(fh);
	} else failures++;
    }
    PROTECT(ans = allocVector(STRSXP, 1));
    if (!failures)
	STRING(ans)[0] = mkChar("0");
    else
	STRING(ans)[0] = mkChar("1");
    UNPROTECT(1);
    return (ans);
}

SEXP do_helpstart(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  ans;
    char *home, buf[MAX_PATH];
    FILE *ff;

    checkArity(op, args);
    home = getenv("R_HOME");
    if (home == NULL)
	error("R_HOME not set\n");
    sprintf(buf, "%s\\doc\\html\\index.html", home);
    ff = fopen(buf, "r");
    if (!ff) {
	sprintf(buf, "%s\\doc\\html\\index.htm", home);
	ff = fopen(buf, "r");
	if (!ff) {
	    sprintf(buf, "%s\\doc\\html\\index.htm[l] not found\n", home);
	    error(buf);
	}
    }
    fclose(ff);
    ShellExecute(NULL, "open", buf, NULL, home, SW_SHOW);
    PROTECT(ans = allocVector(STRSXP, 1));
    STRING(ans)[0] = mkChar("");
    UNPROTECT(1);
    return (ans);
}

static int nhfiles = 0;
static char *hfiles[50];


SEXP do_helpitem(SEXP call, SEXP op, SEXP args, SEXP env)
{
/*
 * type = 1: launch html file.
 *        2: "topic", 2, Windows help file.
 *        3: notify are finished with the help file.
 */

    char *item, *hfile;
    char *home, buf[MAX_PATH];
    FILE *ff;
    int   type;

    checkArity(op, args);
    if (!isString(CAR(args)))
	errorcall(call, "invalid topic argument\n");
    item = CHAR(STRING(CAR(args))[0]);
    type = asInteger(CADR(args));
    if (type == 1) {
	ff = fopen(item, "r");
	if (!ff) {
	    sprintf(buf, "%s not found\n", item);
	    error(buf);
	}
	fclose(ff);
	home = getenv("R_HOME");
	if (home == NULL)
	    error("R_HOME not set\n");
	ShellExecute(NULL, "open", item, NULL, home, SW_SHOW);
    } else if (type == 2) {
	if (!isString(CADDR(args)))
	    errorcall(call, "invalid hlpfile argument\n");
	hfile = CHAR(STRING(CADDR(args))[0]);
	if (!WinHelp((HWND) 0, hfile, HELP_KEY, (DWORD) item))
	    warning("WinHelp call failed\n");
	else {
	    if (nhfiles >= 50)
		error("too many .hlp files opened\n");
	    hfiles[nhfiles] = malloc(strlen(hfile) * sizeof(char));
	    strcpy(hfiles[nhfiles++], hfile);
	}
    } else if (type == 3) {
	if (!isString(CADDR(args)))
	    warningcall(call, "invalid hlpfile argument\n");
	hfile = CHAR(STRING(CADDR(args))[0]);
	if (!WinHelp((HWND) 0, hfile, HELP_QUIT, (DWORD) 0))
	    error("WinHelp call failed\n");
    } else
	warning("type not yet implemented\n");
    return R_NilValue;
}

void closeAllHlpFiles()
{
    int   i;

    for (i = nhfiles - 1; i >= 0; i--)
	WinHelp((HWND) 0, hfiles[i], HELP_QUIT, (DWORD) 0);
}


SEXP do_flushconsole(SEXP call, SEXP op, SEXP args, SEXP env)
{
    R_FlushConsole();
    return R_NilValue;
}

#include <winbase.h>
/* typedef struct _OSVERSIONINFO{  
    DWORD dwOSVersionInfoSize; 
    DWORD dwMajorVersion; 
    DWORD dwMinorVersion; 
    DWORD dwBuildNumber; 
    DWORD dwPlatformId; 
    TCHAR szCSDVersion[ 128 ]; 
    } OSVERSIONINFO; */
 

SEXP do_winver(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char isNT[8]="??", ver[256];
    SEXP ans;
    OSVERSIONINFO verinfo;
    
    checkArity(op, args);
    verinfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&verinfo);
    switch(verinfo.dwPlatformId) {
    case VER_PLATFORM_WIN32_NT:
	strcpy(isNT, "NT");
	break;
    case VER_PLATFORM_WIN32_WINDOWS:
	strcpy(isNT, "9x");
	break;
    case VER_PLATFORM_WIN32s:
	strcpy(isNT, "win32s");
	break;	
    default:
	sprintf(isNT, "ID=%d", (int)verinfo.dwPlatformId);
	break;
    }
    
    sprintf(ver, "Windows %s %d.%d (build %d) %s", isNT,
	    (int)verinfo.dwMajorVersion, (int)verinfo.dwMinorVersion,
	    LOWORD(verinfo.dwBuildNumber), verinfo.szCSDVersion);
    
    PROTECT(ans = allocVector(STRSXP, 1));
    STRING(ans)[0] = mkChar(ver);
    UNPROTECT(1);
    return (ans);
}
