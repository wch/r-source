/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file dounzip.c
 *  Copyright (C) 1998--2001  Guido Masarotto and Brian Ripley
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
void R_ShowMessage(char *s); /* from rui.h */

static int unzip_is_loaded = 0;
static int Load_Unzip_DLL();
static int do_unzip(char *zipname, char *dest, int nfiles, char **files,
		    int nxfiles, char **xfiles, int over);

SEXP do_int_unzip(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  fn, ans;
    char zipname[MAX_PATH], *topics[500], dest[MAX_PATH];
    int i, ntopics, rc;

    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	errorcall(call, "invalid zip name argument");
    strcpy(zipname, CHAR(STRING_ELT(CAR(args), 0)));
    args = CDR(args);
    fn = CAR(args);
    ntopics = length(fn);
    if (ntopics > 0) {
	if (!isString(fn) || ntopics > 500)
	    errorcall(call, "invalid topics argument");
	for(i = 0; i < ntopics; i++)
	    topics[i] = CHAR(STRING_ELT(fn, i));
    }
    args = CDR(args);
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	errorcall(call, "invalid destination argument");
    strcpy(dest, CHAR(STRING_ELT(CAR(args), 0)));
    rc = Load_Unzip_DLL();
    if (rc > 0) {
	rc = 10;
    } else {
	rc = do_unzip(zipname, dest, ntopics, topics, 0 , NULL, 1);
    }
    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = rc;
    UNPROTECT(1);
    return ans;
}


#include <windows.h>
#include <string.h>
#include "graphapp/graphapp.h"
#include "unzip/structs.h"

#define UNZ_DLL_NAME "unzip32.dll\0"
typedef int (WINAPI * _DLL_UNZIP)(int, char **, int, char **,
                                  LPDCL, LPUSERFUNCTIONS);
_DLL_UNZIP Wiz_SingleEntryUnzip;
HINSTANCE hUnzipDll;

#define UNZ_DLL_VERSION "5.42\0"
#define COMPANY_NAME "Info-ZIP\0"
#define DLL_VERSION_WARNING "%s is missing\nor has the wrong version number.\nEnsure that you have the correct DLL installed"

static int Load_Unzip_DLL()
{
    char szFullPath[PATH_MAX];
    DWORD dwVerInfoSize;
    DWORD dwVerHnd;

    if (unzip_is_loaded) return !(unzip_is_loaded >0);
    strcpy(szFullPath, R_HomeDir());
    strcat(szFullPath, "\\unzip\\");
    strcat(szFullPath, UNZ_DLL_NAME);

    dwVerInfoSize = GetFileVersionInfoSize(szFullPath, &dwVerHnd);

    if (dwVerInfoSize) {
	BOOL  fRet, fRetName;
	LPSTR lpstrVffInfo;
	LPSTR lszVer = NULL;
	LPSTR lszVerName = NULL;
	UINT  cchVer = 0;

	lpstrVffInfo = (LPSTR) malloc(dwVerInfoSize);

	/* Get the version information */
	if (GetFileVersionInfo(szFullPath, 0L, dwVerInfoSize, lpstrVffInfo))
	{
	    fRet = VerQueryValue(lpstrVffInfo,
				 TEXT("\\StringFileInfo\\040904E4\\FileVersion"),
				 (LPVOID)&lszVer,
				 &cchVer);
	    fRetName = VerQueryValue(lpstrVffInfo,
				     TEXT("\\StringFileInfo\\040904E4\\CompanyName"),
				     (LPVOID)&lszVerName,
				     &cchVer);
	    if (!fRet || !fRetName ||
		(lstrcmpi(lszVer, UNZ_DLL_VERSION) != 0) ||
		(lstrcmpi(lszVerName, COMPANY_NAME) != 0))
		unzip_is_loaded = -1;
	}
	free(lpstrVffInfo);
    } else unzip_is_loaded = -1;

    if (unzip_is_loaded < 0) {
	char str[256];
	sprintf (str, DLL_VERSION_WARNING, szFullPath);
	R_ShowMessage(str);
	return 1;
    }    
    hUnzipDll = LoadLibrary(szFullPath);
    if (hUnzipDll != NULL) {
	Wiz_SingleEntryUnzip =
	    (_DLL_UNZIP)GetProcAddress(hUnzipDll, "Wiz_SingleEntryUnzip");
    } else {
	unzip_is_loaded = -1;
	return 1;
    }
    return 0;
}

void UnLoad_Unzip_Dll()
{
    if (unzip_is_loaded) FreeLibrary(hUnzipDll);
}


LPUSERFUNCTIONS lpUserFunctions;
HANDLE hUF = (HANDLE) NULL;
LPDCL lpDCL = NULL;
HANDLE hDCL = (HANDLE) NULL;
HANDLE hZCL = (HANDLE) NULL;

static int WINAPI UnzDisplayBuf(LPSTR msg, unsigned long size)
{
    return size;
}

static void WINAPI
ReceiveDllMessage(unsigned long ucsize, unsigned long csiz,
    unsigned cfactor,
    unsigned mo, unsigned dy, unsigned yr, unsigned hh, unsigned mm,
    char c, LPSTR filename, LPSTR methbuf, unsigned long crc, char fCrypt)
{
}

static int WINAPI password(char *p, int n, const char *m, const char *name)
{
    return 1;
}

static int WINAPI ReplaceYes(char *filename) {return 1;}

static int WINAPI ReplaceNo(char *filename) {return 0;}

static int WINAPI UnzipCallBack(LPCSTR member, unsigned long size)
{
    if(peekevent()) doevent();
    return 0;
}

static int do_unzip(char *zipname, char *dest, int nfiles, char **files,
	    int nxfiles, char **xfiles, int over)
{
    int retcode;

    hDCL = GlobalAlloc(GPTR, (DWORD) sizeof(DCL));
    if (!hDCL) return 1;
    lpDCL = (LPDCL) GlobalLock(hDCL);
    if (!lpDCL) { GlobalFree(hDCL); return 1; }

    hUF = GlobalAlloc( GPTR, (DWORD) sizeof(USERFUNCTIONS));
    if (!hUF) { GlobalUnlock(hDCL); GlobalFree(hDCL); return 1;}

    lpUserFunctions = (LPUSERFUNCTIONS) GlobalLock(hUF);
    if (!lpUserFunctions)
    {
	GlobalUnlock(hDCL); GlobalFree(hDCL); GlobalFree(hUF);
	return 1;
    }
    lpUserFunctions->password = password;
    lpUserFunctions->print = UnzDisplayBuf;
    lpUserFunctions->sound = NULL;
    if(over) lpUserFunctions->replace = ReplaceYes;
    else     lpUserFunctions->replace = ReplaceNo;
    lpUserFunctions->SendApplicationMessage = ReceiveDllMessage;
    lpUserFunctions->ServCallBk = UnzipCallBack;


    lpDCL->ncflag = 0; /* Write to stdout if true */
    lpDCL->fQuiet = 2; /* We want all messages.
			  1 = fewer messages,
			  2 = no messages */
    lpDCL->ntflag = 0; /* test zip file if true */
    lpDCL->nvflag = 0; /* give a verbose listing if true */
    lpDCL->ExtractOnlyNewer = 0; /* Do not extract only newer */
    lpDCL->nzflag = 0; /* display a zip file comment if true */
    lpDCL->ndflag = 1; /* Recreate directories if true */
    lpDCL->nfflag = 0; /* Do not freshen existing files only */
    lpDCL->noflag = over > 0; /* Over-write all files if true */
    lpDCL->naflag = 0; /* Do not convert CR to CRLF */
    lpDCL->lpszZipFN = zipname; /* The archive name */
    lpDCL->lpszExtractDir = dest; /* The directory to extract to. This is set
				     to NULL if you are extracting to the
				     current directory.
				  */
    retcode = Wiz_SingleEntryUnzip(nfiles, files, nxfiles, xfiles,
				      lpDCL, lpUserFunctions);
    if (hDCL) { GlobalUnlock(hDCL); GlobalFree(hDCL); }
    if (hUF) { GlobalUnlock(hUF); GlobalFree(hUF); }
    return 0;
}

