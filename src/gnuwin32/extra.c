/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file extra.c
 *  Copyright (C) 1998--2003  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004	      The R Foundation
 *  Copyright (C) 2005--2019  The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */


/* extra commands for R */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "win-nls.h"


#include <stdio.h>
#include <string.h>
#include <time.h>
#include "Defn.h"
#include <Internal.h>
#include "Fileio.h"
#include <direct.h>
#include "graphapp/ga.h"
#include "rlocale.h"
/* Mingw-w64 defines this to be 0x0502 */
#ifndef _WIN32_WINNT
# define _WIN32_WINNT 0x0502 /* for GetLongPathName, KEY_WOW64_64KEY */
#endif
#include <windows.h>
#include "rui.h"
#undef ERROR
#include <R_ext/RS.h> /* for Calloc */

#include <winbase.h>


/* used in rui.c */
void internal_shellexec(const char * file)
{
    const char *home;
    char home2[10000], *p;
    uintptr_t ret;

    home = getenv("R_HOME");
    if (home == NULL)
	error(_("R_HOME not set"));
    strncpy(home2, home, 10000);
    for(p = home2; *p; p++) if(*p == '/') *p = '\\';
    ret = (uintptr_t) ShellExecute(NULL, "open", file, NULL, home2, SW_SHOW);
    if(ret <= 32) { /* an error condition */
	if(ret == ERROR_FILE_NOT_FOUND  || ret == ERROR_PATH_NOT_FOUND
	   || ret == SE_ERR_FNF || ret == SE_ERR_PNF)
	    error(_("'%s' not found"), file);
	if(ret == SE_ERR_ASSOCINCOMPLETE || ret == SE_ERR_NOASSOC)
	    error(_("file association for '%s' not available or invalid"),
		  file);
	if(ret == SE_ERR_ACCESSDENIED || ret == SE_ERR_SHARE)
	    error(_("access to '%s' denied"), file);
	error(_("problem in displaying '%s'"), file);
    }
}

/* used by shell.exec() with rhome=FALSE.  2.13.0 and earlier were
   like rhome=TRUE, but without fixing the path */
static void internal_shellexecW(const wchar_t * file, Rboolean rhome)
{
    const wchar_t *home;
    wchar_t home2[10000], *p;
    uintptr_t ret;
    
    if (rhome) {
    	home = _wgetenv(L"R_HOME");
    	if (home == NULL)
	    error(_("R_HOME not set"));
    	wcsncpy(home2, home, 10000);
    	for(p = home2; *p; p++) if(*p == L'/') *p = L'\\';
	home = home2;
    } else home = NULL;
    
    ret = (uintptr_t) ShellExecuteW(NULL, L"open", file, NULL, home, SW_SHOW);
    if(ret <= 32) { /* an error condition */
	if(ret == ERROR_FILE_NOT_FOUND  || ret == ERROR_PATH_NOT_FOUND
	   || ret == SE_ERR_FNF || ret == SE_ERR_PNF)
	    error(_("'%ls' not found"), file);
	if(ret == SE_ERR_ASSOCINCOMPLETE || ret == SE_ERR_NOASSOC)
	    error(_("file association for '%ls' not available or invalid"),
		  file);
	if(ret == SE_ERR_ACCESSDENIED || ret == SE_ERR_SHARE)
	    error(_("access to '%ls' denied"), file);
	error(_("problem in displaying '%ls'"), file);
    }
}

SEXP do_shellexec(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP file;

    checkArity(op, args);
    file = CAR(args);
    if (!isString(file) || length(file) != 1)
	errorcall(call, _("invalid '%s' argument"), "file");
    internal_shellexecW(filenameToWchar(STRING_ELT(file, 0), FALSE), FALSE);
    return R_NilValue;
}

int check_doc_file(const char * file)
{
    const char *home;
    char path[MAX_PATH];

    home = getenv("R_HOME");
    if (home == NULL)
	error(_("R_HOME not set"));
    if(strlen(home) + strlen(file) + 1 >= MAX_PATH) return(1); /* cannot exist */
    strcpy(path, home);
    strcat(path, "/");
    strcat(path, file);
    return access(path, 4) == 0; /* read access */
}

#include "Startup.h"

void Rwin_fpset(void)
{
    /* Under recent MinGW this is what fpreset does.  It sets the
       control word to 0x37f which corresponds to 0x8001F as used by
       _controlfp.  That is all errors are masked, 64-bit mantissa and
       rounding are selected. */

    __asm__ ( "fninit" ) ;
}


#include <preferences.h>

/* utils::loadRconsole */
SEXP in_loadRconsole(SEXP sfile)
{
    struct structGUI gui;
    const void *vmax = vmaxget();

    if (!isString(sfile) || LENGTH(sfile) < 1)
	error(_("invalid '%s' argument"), "file");
    getActive(&gui);  /* Will get defaults if there's no active console */
    if (loadRconsole(&gui, translateChar(STRING_ELT(sfile, 0)))) applyGUI(&gui);
    if (strlen(gui.warning)) warning(gui.warning);
    vmaxset(vmax);
    return R_NilValue;
}

#include <lmcons.h>
typedef void (WINAPI *PGNSI)(LPSYSTEM_INFO);

/* base::Sys.info */
// keep in step with src/library/utils/src/windows/util.c
SEXP do_sysinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, ansnames;
    OSVERSIONINFOEX osvi;
    char ver[256], buf[1000];
    wchar_t name[MAX_COMPUTERNAME_LENGTH + 1], user[UNLEN+1];
    DWORD namelen = MAX_COMPUTERNAME_LENGTH + 1, userlen = UNLEN+1;

    checkArity(op, args);
    PROTECT(ans = allocVector(STRSXP, 8));
    osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
    if(!GetVersionEx((OSVERSIONINFO *)&osvi))
	error(_("unsupported version of Windows"));

    SET_STRING_ELT(ans, 0, mkChar("Windows"));

    /* Here for unknown future versions */
    snprintf(ver, 256, "%d.%d", 
	     (int)osvi.dwMajorVersion, (int)osvi.dwMinorVersion);

    if((int)osvi.dwMajorVersion >= 5) {
	PGNSI pGNSI;
	SYSTEM_INFO si;
	if(osvi.dwMajorVersion == 10 && osvi.dwMinorVersion == 0) {
	    if(osvi.wProductType == VER_NT_WORKSTATION) strcpy(ver, "10");
	    else strcpy(ver, "Server");
	}
	if(osvi.dwMajorVersion == 6) {
	    char *desc = "";
	    if(osvi.wProductType == VER_NT_WORKSTATION) {
		if(osvi.dwMinorVersion == 0) desc = "Vista";
		else if(osvi.dwMinorVersion == 1) desc = "7";
		else if(osvi.dwMinorVersion == 2) desc = ">= 8";
		else if(osvi.dwMinorVersion == 3) desc = "8.1";
		else desc = "> 8.1";
	    } else {
		if(osvi.dwMinorVersion == 0) desc = "Server 2008";
		else if(osvi.dwMinorVersion == 1) desc = "Server 2008 R2";
		else if(osvi.dwMinorVersion == 2) desc = "Server >= 2012";
		else if(osvi.dwMinorVersion == 3) desc = "Server 2012 R2";
		else desc = "Server > 2012";
	    }
	    strcpy(ver, desc);
	}
	if(osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0)
	    strcpy(ver, "2000");
	if(osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1)
	    strcpy(ver, "XP");
	if(osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2) {
	    if(osvi.wProductType == VER_NT_WORKSTATION)
		strcpy(ver, "XP Professional");
	    else strcpy(ver, "Server 2003");
	}
	/* GetNativeSystemInfo is XP or later */
	pGNSI = (PGNSI)
	    GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
			   "GetNativeSystemInfo");
	if(NULL != pGNSI) pGNSI(&si); else GetSystemInfo(&si);
	if(si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64)
	    strcat(ver, " x64");
    }
    SET_STRING_ELT(ans, 1, mkChar(ver));

    if((int)osvi.dwMajorVersion >= 5) {
	if(osvi.wServicePackMajor > 0)
	    snprintf(ver, 256, "build %d, Service Pack %d",
		     LOWORD(osvi.dwBuildNumber),
		     (int) osvi.wServicePackMajor);
	else snprintf(ver, 256, "build %d", LOWORD(osvi.dwBuildNumber));
    } else
	snprintf(ver, 256, "build %d, %s",
		 LOWORD(osvi.dwBuildNumber), osvi.szCSDVersion);
    SET_STRING_ELT(ans, 2, mkChar(ver));
    GetComputerNameW(name, &namelen);
    wcstoutf8(buf, name, sizeof(buf));
    SET_STRING_ELT(ans, 3, mkCharCE(buf, CE_UTF8));
#ifdef _WIN64
    SET_STRING_ELT(ans, 4, mkChar("x86-64"));
#else
    SET_STRING_ELT(ans, 4, mkChar("x86"));
#endif
    GetUserNameW(user, &userlen);
    wcstoutf8(buf, user, sizeof(buf));
    SET_STRING_ELT(ans, 5, mkCharCE(buf, CE_UTF8));
    SET_STRING_ELT(ans, 6, STRING_ELT(ans, 5));
    SET_STRING_ELT(ans, 7, STRING_ELT(ans, 5));
    PROTECT(ansnames = allocVector(STRSXP, 8));
    SET_STRING_ELT(ansnames, 0, mkChar("sysname"));
    SET_STRING_ELT(ansnames, 1, mkChar("release"));
    SET_STRING_ELT(ansnames, 2, mkChar("version"));
    SET_STRING_ELT(ansnames, 3, mkChar("nodename"));
    SET_STRING_ELT(ansnames, 4, mkChar("machine"));
    SET_STRING_ELT(ansnames, 5, mkChar("login"));
    SET_STRING_ELT(ansnames, 6, mkChar("user"));
    SET_STRING_ELT(ansnames, 7, mkChar("effective_user"));
    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}

void Rsleep(double timeint)
{
    int ntime = 1000*timeint + 0.5;
    DWORD mtime;
    while (ntime > 0) {
	mtime = min(500, ntime);
	ntime -= mtime;
	Sleep(mtime);
	R_ProcessEvents();
    }

}


#define MALLINFO_FIELD_TYPE size_t
struct mallinfo {
    MALLINFO_FIELD_TYPE arena;    /* non-mmapped space allocated from system */
    MALLINFO_FIELD_TYPE ordblks;  /* number of free chunks */
    MALLINFO_FIELD_TYPE smblks;   /* number of fastbin blocks */
    MALLINFO_FIELD_TYPE hblks;    /* number of mmapped regions */
    MALLINFO_FIELD_TYPE hblkhd;   /* space in mmapped regions */
    MALLINFO_FIELD_TYPE usmblks;  /* maximum total allocated space */
    MALLINFO_FIELD_TYPE fsmblks;  /* space available in freed fastbin blocks */
    MALLINFO_FIELD_TYPE uordblks; /* total allocated space */
    MALLINFO_FIELD_TYPE fordblks; /* total free space */
    MALLINFO_FIELD_TYPE keepcost; /* top-most, releasable (via malloc_trim) space */
};
extern R_size_t R_max_memory;

struct mallinfo mallinfo(void);

SEXP in_memsize(SEXP ssize)
{
    SEXP ans;
    int maxmem = NA_LOGICAL;

    if(isLogical(ssize)) 
	maxmem = asLogical(ssize);
    else if(isReal(ssize)) {
	R_size_t newmax;
	double mem = asReal(ssize);
	if (!R_FINITE(mem))
	    error(_("incorrect argument"));
#ifndef _WIN64
	if(mem >= 4096)
	    error(_("don't be silly!: your machine has a 4Gb address limit"));
#endif
	newmax = mem * 1048576.0;
	if (newmax < R_max_memory)
	    warning(_("cannot decrease memory limit: ignored"));
	else
	    R_max_memory = newmax;
    } else
	error(_("incorrect argument"));
	
    PROTECT(ans = allocVector(REALSXP, 1));
    if(maxmem == NA_LOGICAL)
	REAL(ans)[0] = R_max_memory;
    else if(maxmem)
	REAL(ans)[0] = mallinfo().usmblks;
    else
	REAL(ans)[0] = mallinfo().uordblks;
    REAL(ans)[0] /= 1048576.0;
    UNPROTECT(1);
    return ans;
}

SEXP do_dllversion(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP path = R_NilValue, ans;
    const wchar_t *dll;
    DWORD dwVerInfoSize;
    DWORD dwVerHnd;

    checkArity(op, args);
    path = CAR(args);
    if(!isString(path) || LENGTH(path) != 1)
	errorcall(call, _("invalid '%s' argument"), "path");
    dll = filenameToWchar(STRING_ELT(path, 0), FALSE);
    dwVerInfoSize = GetFileVersionInfoSizeW(dll, &dwVerHnd);
    PROTECT(ans = allocVector(STRSXP, 2));
    SET_STRING_ELT(ans, 0, mkChar(""));
    SET_STRING_ELT(ans, 1, mkChar(""));
    if (dwVerInfoSize) {
	BOOL  fRet;
	LPSTR lpstrVffInfo;
	LPSTR lszVer = NULL;
	UINT  cchVer = 0;

	lpstrVffInfo = (LPSTR) malloc(dwVerInfoSize);
	if (GetFileVersionInfoW(dll, 0L, dwVerInfoSize, lpstrVffInfo)) {

	    fRet = VerQueryValue(lpstrVffInfo,
				 TEXT("\\StringFileInfo\\040904E4\\FileVersion"),
				 (LPVOID)&lszVer, &cchVer);
	    if(fRet) SET_STRING_ELT(ans, 0, mkChar(lszVer));

	    fRet = VerQueryValue(lpstrVffInfo,
				 TEXT("\\StringFileInfo\\040904E4\\R Version"),
				 (LPVOID)&lszVer, &cchVer);
	    if(fRet) SET_STRING_ELT(ans, 1, mkChar(lszVer));
	    else {
		fRet = VerQueryValue(lpstrVffInfo,
				     TEXT("\\StringFileInfo\\040904E4\\Compiled under R Version"),
				     (LPVOID)&lszVer, &cchVer);
		if(fRet) SET_STRING_ELT(ans, 1, mkChar(lszVer));
	    }

	} else ans = R_NilValue;
	free(lpstrVffInfo);
    } else ans = R_NilValue;
    UNPROTECT(1);
    return ans;
}

/* Retry renaming a few times to recover from possible anti-virus interference,
   which has been reported e.g. during installation of packages. */

int Rwin_rename(const char *from, const char *to)
{
    for(int retries = 0; retries < 10; retries++) {
	/* coreutils first call MoveFileEx without flags; only if it fails
	   with ERROR_FILE_EXISTS or ERROR_ALREADY_EXISTING, they call again
	   with MOVEFILE_REPLACE_EXISTING */
	if (MoveFileEx(from, to, MOVEFILE_REPLACE_EXISTING | MOVEFILE_COPY_ALLOWED | MOVEFILE_WRITE_THROUGH))
	    return 0;
	DWORD err = GetLastError();
	if (err != ERROR_SHARING_VIOLATION && err != ERROR_ACCESS_DENIED)
	    return 1;
	Sleep(500);
	R_ProcessEvents();
    }
    return 1;
}

int Rwin_wrename(const wchar_t *from, const wchar_t *to)
{
    for(int retries = 0; retries < 10; retries++) {
	if (MoveFileExW(from, to, MOVEFILE_REPLACE_EXISTING | MOVEFILE_COPY_ALLOWED | MOVEFILE_WRITE_THROUGH))
	    return 0;
	DWORD err = GetLastError();
	if (err != ERROR_SHARING_VIOLATION && err != ERROR_ACCESS_DENIED)
	    return 1;
	Sleep(500);
	R_ProcessEvents();
    }
    return 1;
}


const char *formatError(DWORD res)
{
    static char buf[1000], *p;
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
		  NULL, res,
		  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		  buf, 1000, NULL);
    p = buf+strlen(buf) -1;
    if(*p == '\n') *p = '\0';
    p = buf+strlen(buf) -1;
    if(*p == '\r') *p = '\0';
    p = buf+strlen(buf) -1;
    if(*p == '.') *p = '\0';
    return buf;
}

#if _WIN32_WINNT < 0x0600
/* available from Windows Vista */
typedef DWORD (WINAPI *LPFN_GFPNBH) (HANDLE, LPSTR, DWORD, DWORD);
typedef DWORD (WINAPI *LPFN_GFPNBHW) (HANDLE, LPWSTR, DWORD, DWORD);
/*
DWORD GetFinalPathNameByHandle(
    HANDLE hFile,
    LPSTR lpszFilePath,
    DWORD cchFilePath,
    DWORD dwFlags);

DWORD GetFinalPathNameByHandleW(
    HANDLE hFile,
    LPWSTR lpszFilePath,
    DWORD  cchFilePath,
    DWORD  dwFlags
);
*/
#endif

/*
   Returns TRUE on success. On failure, "res" may be modified but not useful.
*/
static Rboolean getFinalPathName(const char *orig, char *res)
{
    HANDLE h;
    int ret;
    static LPFN_GFPNBH gfpnbh = NULL;
    static Rboolean initialized = FALSE;

    if (!initialized) {
	initialized = TRUE;
	gfpnbh = (LPFN_GFPNBH) GetProcAddress(
	    GetModuleHandle(TEXT("kernel32")),
	    "GetFinalPathNameByHandleA");
    }
    if (gfpnbh == NULL)
	return FALSE;

    h = CreateFile(orig, 0,
                   FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
		   NULL, OPEN_EXISTING,
                   /* FILE_FLAG_BACKUP_SEMANTICS needed to open a directory */
		   FILE_ATTRIBUTE_HIDDEN | FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (h == INVALID_HANDLE_VALUE) 
	return FALSE;

    ret = gfpnbh(h, res, MAX_PATH, VOLUME_NAME_DOS);
    CloseHandle(h);

    if (!ret || ret > MAX_PATH)
	return FALSE;
    
    /* get rid of the \\?\ prefix */
    int len = strlen(res);
    int strip = 0;
    if (len < 4 || strncmp("\\\\?\\", res, 4))
	/* res should start with \\?\ */
	return FALSE;
    
    if (len > 8 && !strncmp("UNC\\", res+4, 4))
	/* UNC path \\?\UNC */
	strip = 7;	
    else if (len >= 6 && isalpha(res[4]) && res[5] == ':' && res[6] == '\\')
	/* \\?\D: */
	strip = 4;
    else
	return FALSE;
    memmove(res, res+strip, len-strip+1);

    /* sanity check if the file exists using the normalized path, a normalized
       path to an existing file should still be working */
    h = CreateFile(orig, 0,
                   FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
		   NULL, OPEN_EXISTING,
                   /* FILE_FLAG_BACKUP_SEMANTICS needed to open a directory */
		   FILE_ATTRIBUTE_HIDDEN | FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (h == INVALID_HANDLE_VALUE)
	return FALSE;
    CloseHandle(h);

    return TRUE;	
}

/*
   Returns TRUE on success. On failure, "res" may be modified but not useful.
*/
static Rboolean getFinalPathNameW(const wchar_t *orig, wchar_t *res)
{
    HANDLE h;
    int ret;
    static LPFN_GFPNBHW gfpnbhw = NULL;
    static Rboolean initialized = FALSE;

    if (!initialized) {
	initialized = TRUE;
	gfpnbhw = (LPFN_GFPNBHW) GetProcAddress(
	    GetModuleHandle(TEXT("kernel32")),
	    "GetFinalPathNameByHandleW");
    }
    if (gfpnbhw == NULL)
	return FALSE;

    h = CreateFileW(orig, 0,
                   FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
		   NULL, OPEN_EXISTING,
                   /* FILE_FLAG_BACKUP_SEMANTICS needed to open a directory */
		   FILE_ATTRIBUTE_HIDDEN | FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (h == INVALID_HANDLE_VALUE) 
	return FALSE;

    ret = gfpnbhw(h, res, 32767, VOLUME_NAME_DOS);
    CloseHandle(h);

    if (!ret || ret > 32768)
	return FALSE;
    
    /* get rid of the \\?\ prefix */
    size_t len = wcslen(res);
    int strip = 0;
    if (len < 4 || wcsncmp(L"\\\\?\\", res, 4))
	/* res should start with \\?\ */
	return FALSE;
    
    if (len > 8 && !wcsncmp(L"UNC\\", res+4, 4))
	/* UNC path \\?\UNC */
	strip = 7;	
    else if (len >= 6 && Ri18n_iswctype(res[4], Ri18n_wctype("alpha"))
	     && res[5] == L':' && res[6] == L'\\')
	/* \\?\D: */
	strip = 4;
    else
	return FALSE;
    wmemmove(res, res+strip, len-strip+1);

    /* sanity check if the file exists using the normalized path, a normalized
       path to an existing file should still be working */
    h = CreateFileW(orig, 0,
                   FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
		   NULL, OPEN_EXISTING,
                   /* FILE_FLAG_BACKUP_SEMANTICS needed to open a directory */
		   FILE_ATTRIBUTE_HIDDEN | FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (h == INVALID_HANDLE_VALUE)
	return FALSE;
    CloseHandle(h);

    return TRUE;	
}



void R_UTF8fixslash(char *s); /* from main/util.c */
SEXP do_normalizepath(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, paths = CAR(args), el, slash;
    int i, n = LENGTH(paths), res;
    char tmp[4*MAX_PATH+1], longpath[4*MAX_PATH+1], *tmp2;
    wchar_t wtmp[32768], wlongpath[32768], *wtmp2;
    int mustWork, fslash = 0;

    checkArity(op, args);
    if(!isString(paths))
	errorcall(call, _("'path' must be a character vector"));

    slash = CADR(args);
    if(!isString(slash) || LENGTH(slash) != 1)
	errorcall(call, "'winslash' must be a character string");
    const char *sl = CHAR(STRING_ELT(slash, 0));
    if (strcmp(sl, "/") && strcmp(sl, "\\"))
	errorcall(call, "'winslash' must be '/' or '\\\\'");
    if (strcmp(sl, "/") == 0) fslash = 1;
    
    mustWork = asLogical(CADDR(args));

    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
    	SEXP result;
	Rboolean ok = FALSE;
	el = STRING_ELT(paths, i);
	result = el;
	if (el == NA_STRING) {
	    result = NA_STRING;
	    if(mustWork == 1)
		errorcall(call, "path[%d]=NA", i+1);
	    else if(mustWork == NA_LOGICAL)
		warningcall(call, "path[%d]=NA", i+1);
	} else if(getCharCE(el) == CE_UTF8) {
	    wchar_t *norm = NULL;
	    const wchar_t* wel = filenameToWchar(el, FALSE);

	    if (getFinalPathNameW(wel, wtmp)) {
		norm = wtmp;
		ok = TRUE;
	    } else {
		/* silently fall back to GetFullPathNameW/GetLongPathNameW */
		res = GetFullPathNameW(wel, 32768, wtmp, &wtmp2);
		if (res && res <= 32768) {
		    norm = wtmp;
		    res = GetLongPathNameW(wtmp, wlongpath, 32768);
		    if (res && res <= 32768) {
			norm = wlongpath;
			ok = TRUE;
		    }
		}
	    }
	    if (!ok) {
		if (mustWork == 1) {
		    errorcall(call, "path[%d]=\"%ls\": %s", i+1, 
			      wel, formatError(GetLastError()));
		} else if (mustWork == NA_LOGICAL) {
		    warningcall(call, "path[%d]=\"%ls\": %s", i+1, 
				wel, formatError(GetLastError()));
		}
	    }

	    char *normutf8 = tmp;
	    if (norm)
		wcstoutf8(tmp, norm, sizeof(tmp));
	    else if (fslash)
		strcpy(tmp, translateCharUTF8(el));
	    else
		normutf8 = (char *)translateCharUTF8(el);

	    if (fslash) R_UTF8fixslash(normutf8);
	    result = mkCharCE(normutf8, CE_UTF8);
	} else {
	    char *norm = NULL;
	    const char *tel = translateChar(el);
	    if (getFinalPathName(tel, tmp)) {
		norm = tmp;
		ok = TRUE;
	    } else {
		/* silently fall back to GetFullPathName/GetLongPathName */
		res = GetFullPathName(tel, MAX_PATH, tmp, &tmp2);
		if (res && res <= MAX_PATH) {
		    norm = tmp;
		    res = GetLongPathName(tmp, longpath, MAX_PATH);
		    if (res && res <= MAX_PATH) {
			norm = longpath;
			ok = TRUE;
		    }
		}
	    }
	    if (!ok) {
		if (mustWork == 1) {
		    errorcall(call, "path[%d]=\"%s\": %s", i+1, 
			      tel, formatError(GetLastError()));
		} else if (mustWork == NA_LOGICAL) {
		    warningcall(call, "path[%d]=\"%s\": %s", i+1, 
				tel, formatError(GetLastError()));
		}
		if (!norm) {
		    if (fslash) {
			strcpy(tmp, tel);
			norm = tmp;
		    } else
			norm = (char *)tel;
		}
	    }
	    if (fslash) R_fixslash(norm);
	    result = mkChar(norm);
	}
	SET_STRING_ELT(ans, i, result);
    }
    UNPROTECT(1);
    return ans;
}

/* utils::shortPathName */
SEXP in_shortpath(SEXP paths)
{
    SEXP ans, el;
    int i, n = LENGTH(paths);
    char tmp[4*MAX_PATH+1];
    wchar_t wtmp[32768];
    DWORD res;
    const void *vmax = vmaxget();

    if(!isString(paths)) error(_("'path' must be a character vector"));

    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	el = STRING_ELT(paths, i);
	if(getCharCE(el) == CE_UTF8) {
	    res = GetShortPathNameW(filenameToWchar(el, FALSE), wtmp, 32768);
	    if (res && res <= 32768)
		wcstoutf8(tmp, wtmp, sizeof(tmp));
	    else
		strcpy(tmp, translateChar(el));
	    /* documented to return paths using \, which the API call does
	       not necessarily do */
	    R_fixbackslash(tmp);
	    SET_STRING_ELT(ans, i, mkCharCE(tmp, CE_UTF8));
	} else {
	    res = GetShortPathName(translateChar(el), tmp, MAX_PATH);
	    if (res == 0 || res > MAX_PATH) strcpy(tmp, translateChar(el));
	    /* documented to return paths using \, which the API call does
	       not necessarily do */
	    R_fixbackslash(tmp);
	    SET_STRING_ELT(ans, i, mkChar(tmp));
	}
    }
    UNPROTECT(1);
    vmaxset(vmax);
    return ans;
}
    
#include "devWindows.h"
#include <R_ext/GraphicsEngine.h> /* GEgetDevice */

/* grDevices::bringToTop */
SEXP bringtotop(SEXP sdev, SEXP sstay)
{
    int dev, stay;
    pGEDevDesc gdd;
    gadesc *xd;

    dev = asInteger(sdev);
    stay = asInteger(sstay);

    if(dev == -1) { /* console */
	if(CharacterMode == RGui) BringToTop(RConsole, stay);
    } else {
	if(dev < 1 || dev > R_MaxDevices || dev == NA_INTEGER)
	    error(_("invalid '%s' argument"), "which");
	gdd = GEgetDevice(dev - 1);
	if(!gdd) error(_("invalid device"));
	xd = (gadesc *) gdd->dev->deviceSpecific;
	if(!xd) error(_("invalid device"));
	if(stay && ismdi()) error(_("requires SDI mode"));
	BringToTop(xd->gawin, stay);
    }
    return R_NilValue;
}

/* grDevices::msgWindow */
SEXP msgwindow(SEXP sdev, SEXP stype)
{
    int dev, type;
    pGEDevDesc gdd;
    gadesc *xd;

    dev = asInteger(sdev);
    type = asInteger(stype);

    if(dev == -1) { /* console */
	if(CharacterMode == RGui) GA_msgWindow(RConsole, type);
    } else {
	if(dev < 1 || dev > R_MaxDevices || dev == NA_INTEGER)
	    error(_("invalid '%s' argument"), "which");
	gdd = GEgetDevice(dev - 1);
	if(!gdd) error(_("invalid device"));
	xd = (gadesc *) gdd->dev->deviceSpecific;
	if(!xd) error(_("invalid device"));
	if(type == 5) {
	    xd->recording = TRUE;
	    check(xd->mrec);
	} else if(type == 6) {
	    xd-> recording = FALSE;
	    uncheck(xd->mrec);
	} else
	    GA_msgWindow(xd->gawin, type);
    }
    return R_NilValue;
}


/* This assumes a menuname of the form 
   $Graph<nn>Main, $Graph<nn>Popup, $Graph<nn>LocMain,
   or $Graph<nn>LocPopup where <nn> is the
   device number.  We've already checked the $Graph prefix. */

/* called from rui.c, only */
menu getGraphMenu(const char* menuname)
{
    int devnum;
    pGEDevDesc gdd;
    gadesc *xd;

    menuname = menuname + 6;
    devnum = atoi(menuname);
    if(devnum < 1 || devnum > R_MaxDevices)
	error(_("invalid graphical device number"));

    while (('0' <= *menuname) && (*menuname <= '9')) menuname++;

    gdd = GEgetDevice(devnum - 1);

    if(!gdd) error(_("invalid device"));

    xd = (gadesc *) gdd->dev->deviceSpecific;

    if(!xd || xd->kind != SCREEN) error(_("bad device"));

    if (strcmp(menuname, "Main") == 0) return(xd->mbar);
    else if (strcmp(menuname, "Popup") == 0) return(xd->grpopup);
    else return(NULL);
}

/*
   Replacement for MSVCRT's access.
   Coded looking at tcl's tclWinFile.c
*/

int winAccessW(const wchar_t *path, int mode)
{
    DWORD attr = GetFileAttributesW(path);

    if(attr == INVALID_FILE_ATTRIBUTES)
	/* file does not exist or may be locked */
	return -1;

    if(mode == F_OK) return 0;
    
    if ((mode & W_OK)
	&& !(attr & FILE_ATTRIBUTE_DIRECTORY)
	&& (attr & FILE_ATTRIBUTE_READONLY)) return -1;

    if(mode & X_OK)
	if(!(attr & FILE_ATTRIBUTE_DIRECTORY)) { /* Directory, so OK */
	    /* Look at extension for executables */
	    wchar_t *p = wcsrchr(path, '.');
	    if(p == NULL ||
	       !((wcsicmp(p, L".exe") == 0) || (wcsicmp(p, L".com") == 0) ||
		 (wcsicmp(p, L".bat") == 0) || (wcsicmp(p, L".cmd") == 0)) )
		return -1;
	}
    {
	/* Now look for file security info */
	SECURITY_DESCRIPTOR *sdPtr = NULL;
	DWORD size = 0;
	PSID sid = 0;
	BOOL sidDefaulted;
	SID_IDENTIFIER_AUTHORITY samba_unmapped = {{0, 0, 0, 0, 0, 22}};
	GENERIC_MAPPING genMap;
	HANDLE hToken = NULL;
	DWORD desiredAccess = 0;
	DWORD grantedAccess = 0;
	BOOL accessYesNo = FALSE;
	PRIVILEGE_SET privSet;
	DWORD privSetSize = sizeof(PRIVILEGE_SET);
	int error;

	/* get size */
	GetFileSecurityW(path,
			 OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION
			 | DACL_SECURITY_INFORMATION | LABEL_SECURITY_INFORMATION,
			 0, 0, &size);
	error = GetLastError();
	if (error == ERROR_NOT_SUPPORTED)
	    /* happens for some remote shares */
	    return _waccess(path, mode);
	if (error != ERROR_INSUFFICIENT_BUFFER) 
	    return -1;
	sdPtr = (SECURITY_DESCRIPTOR *) alloca(size);
	if(!GetFileSecurityW(path,
			     OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION
			     | DACL_SECURITY_INFORMATION | LABEL_SECURITY_INFORMATION, sdPtr, size, &size))
	    return -1;
	/* rely on attrib checks for unmapped samba owners and groups */
	if (!GetSecurityDescriptorOwner(sdPtr, &sid, &sidDefaulted))
	    return 0;
	if (IsValidSid(sid) &&
	    !memcmp(GetSidIdentifierAuthority(sid), &samba_unmapped, sizeof(SID_IDENTIFIER_AUTHORITY)))
	    return 0;
	/*
	 * Perform security impersonation of the user and open the
	 * resulting thread token.
	 */
	if(!ImpersonateSelf(SecurityImpersonation)) return -1;
	if(!OpenThreadToken(GetCurrentThread (),
			    TOKEN_DUPLICATE | TOKEN_QUERY, FALSE,
			    &hToken)) return -1;
	if (mode & R_OK) desiredAccess |= FILE_GENERIC_READ;
	if (mode & W_OK) desiredAccess |= FILE_GENERIC_WRITE;
	if (mode & X_OK) desiredAccess |= FILE_GENERIC_EXECUTE;

	memset(&genMap, 0x0, sizeof (GENERIC_MAPPING));
	genMap.GenericRead = FILE_GENERIC_READ;
	genMap.GenericWrite = FILE_GENERIC_WRITE;
	genMap.GenericExecute = FILE_GENERIC_EXECUTE;
	genMap.GenericAll = FILE_ALL_ACCESS;
	if(!AccessCheck(sdPtr, hToken, desiredAccess, &genMap, &privSet,
			&privSetSize, &grantedAccess, &accessYesNo)) {
	    CloseHandle(hToken);
	    return -1;
	}
	CloseHandle(hToken);
	if (!accessYesNo) return -1;

    }
    return 0;
}

#include <Rversion.h>
char *getDLLVersion(void)
{
    static char DLLversion[25];
    OSVERSIONINFO osvi;
    osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&osvi);
    /* 95, 98, ME are 4.x */
    if(osvi.dwMajorVersion < 5)
	R_Suicide("Windows 2000 or later is required");
    snprintf(DLLversion, 25, "%s.%s", R_MAJOR, R_MINOR);
    return (DLLversion);
}



/* UTF-8 support ----------------------------------------------- */

#ifdef SUPPORT_UTF8_WIN32
/* This is currently unused: for faking UTF-8 locale conversions */

#define FAKE_UTF8 1


size_t Rmbrtowc(wchar_t *wc, const char *s)
{
#ifdef FAKE_UTF8
    unsigned int byte;
    wchar_t local, *w;
    byte = *((unsigned char *)s);
    w = wc ? wc: &local;

    if (byte == 0) {
	*w = (wchar_t) 0;
	return 0;
    } else if (byte < 0xC0) {
	*w = (wchar_t) byte;
	return 1;
    } else if (byte < 0xE0) {
	if(strlen(s) < 2) return -2;
	if ((s[1] & 0xC0) == 0x80) {
	    *w = (wchar_t) (((byte & 0x1F) << 6) | (s[1] & 0x3F));
	    return 2;
	} else return -1;
    } else if (byte < 0xF0) {
	if(strlen(s) < 3) return -2;
	if (((s[1] & 0xC0) == 0x80) && ((s[2] & 0xC0) == 0x80)) {
	    *w = (wchar_t) (((byte & 0x0F) << 12)
			    | ((s[1] & 0x3F) << 6) | (s[2] & 0x3F));
	    byte = *w;
	    if(byte >= 0xD800 && byte <= 0xDFFF) return -1; /* surrogate */
	    if(byte == 0xFFFE || byte == 0xFFFF) return -1;
	    return 3;
	} else return -1;
    }
    return -2;
#else
    return mbrtowc(wc, s, MB_CUR_MAX, NULL);
#endif
}

size_t Rmbstowcs(wchar_t *wc, const char *s, size_t n)
{
#ifdef FAKE_UTF8
    int m, res=0;
    const char *p;

    if(wc) {
	for(p = s; ; p+=m) {
	    m = Rmbrtowc(wc+res, p);
	    if(m < 0) error(_("invalid input in 'Rmbstowcs'"));
	    if(m <= 0) break;
	    res++;
	    if(res >= n) break;
	}
    } else {
	for(p = s; ; p+=m) {
	    m  = Rmbrtowc(NULL, p);
	    if(m < 0) error(_("invalid input in 'Rmbstowcs'"));
	    if(m <= 0) break;
	    res++;
	}
    }
    return res;
#else
    return mbstowcs(wc, s, n);
#endif
}
#endif

/* base::file.choose */
SEXP attribute_hidden do_filechoose(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    wchar_t *fn;
    char str[4*MAX_PATH+1];

    checkArity(op, args);
    setuserfilterW(L"All files (*.*)\0*.*\0\0");
    fn = askfilenameW(G_("Select file"), "");
    if (!fn)
	error(_("file choice cancelled"));
    wcstoutf8(str, fn, sizeof(str));
    PROTECT(ans = allocVector(STRSXP, 1));
    SET_STRING_ELT(ans, 0, mkCharCE(str, CE_UTF8));
    UNPROTECT(1);
    return ans;
}

const char *getTZinfo(void);  // src/extra/tzone/registryTZ.c

SEXP attribute_hidden do_tzone_name(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return mkString(getTZinfo());
}

