/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file extra.c
 *  Copyright (C) 1998--2003  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004	      The R Foundation
 *  Copyright (C) 2005--2024  The R Core Team
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

#include <float.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "Defn.h"
#include <Internal.h>
#include "Fileio.h"
#include <direct.h>
#include "graphapp/ga.h"
#include "rlocale.h"
#include <windows.h>
#include "rui.h"
#undef ERROR
#include <R_ext/RS.h> /* formerly for Calloc */

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
    strncpy(home2, home, 10000 - 1);
    home2[10000 - 1] = '\0';
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

int check_doc_file(const char *file)
{
    const char *home;
    char *path;

    home = getenv("R_HOME");
    if (home == NULL)
	error(_("R_HOME not set"));
    path = (char *) malloc(strlen(home) + 1 + strlen(file) + 1);
    if (!path)
	return 0; /* treat error as no access, used in GUI */
    strcpy(path, home);
    strcat(path, "/");
    strcat(path, file);
    int res = (access(path, 4) == 0); /* read access is granted */
    free(path);
    return res;
}

#include "Startup.h"

void Rwin_fpset(void)
{
    /* Under recent MinGW this is what fpreset does.  It sets the
       control word to 0x37f which corresponds to 0x8001F as used by
       _controlfp.  That is all errors are masked, 64-bit mantissa and
       rounding are selected:

       __asm__ ( "fninit" ) ;
    */
    _fpreset();
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
    if (strlen(gui.warning)) warning("%s", gui.warning);
    vmaxset(vmax);
    return R_NilValue;
}

/* returns R_alloc'd results */
static int getCurrentUserAndDomain(char **user, char **domain)
{
    int ok = 0;
    HANDLE h = INVALID_HANDLE_VALUE;
    DWORD err;
    DWORD tilen = 0;
    PTOKEN_USER t = NULL;
    char *ubuf = NULL;
    DWORD ulen = 0;
    char *dbuf = NULL;
    DWORD dlen = 0;
    SID_NAME_USE suse = SidTypeUnknown;

    ok = OpenThreadToken(GetCurrentThread(), TOKEN_QUERY, TRUE, &h);
    if (!ok) {
	err = GetLastError();
	if (err == ERROR_NO_TOKEN || err == ERROR_NO_IMPERSONATION_TOKEN)
	    ok = OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &h);
    }

    ok = ok && !GetTokenInformation(h, TokenUser, NULL, 0, &tilen)
	 && GetLastError() == ERROR_INSUFFICIENT_BUFFER;

    if (ok) {
	t = (PTOKEN_USER) R_alloc(tilen, 1);
	ok = GetTokenInformation(h, TokenUser, t, tilen, &tilen);
    }

    if (ok) {
	ok = !LookupAccountSid(NULL, t->User.Sid, ubuf, &ulen, dbuf, &dlen,
	                       &suse)
	     && GetLastError() == ERROR_INSUFFICIENT_BUFFER;

	if (ok) {
	    ubuf = R_alloc(ulen, 1);
	    dbuf = R_alloc(dlen, 1);
	    ok = LookupAccountSid(NULL, t->User.Sid, ubuf, &ulen, dbuf, &dlen,
	                          &suse);
	}
    }

    if (!ok)
	err = GetLastError();

    if (h != INVALID_HANDLE_VALUE)
	CloseHandle(h);

    if (ok) {
	if (user)
	    *user = ubuf;
	if (domain)
	    *domain = dbuf;
	return 1;
    } else {
	if (user)
	    *user = NULL;
	if (domain)
	    *domain = NULL;
	SetLastError(err);
	return 0;
    }
}


#include <lmcons.h>
typedef void (WINAPI *PGNSI)(LPSYSTEM_INFO);

const char *formatError(DWORD res);

/* base::Sys.info */
// keep in step with src/library/utils/src/windows/util.c
SEXP do_sysinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, ansnames;
    OSVERSIONINFOEX osvi;
    char ver[256], buf[1000];
    wchar_t name[MAX_COMPUTERNAME_LENGTH + 1];
    DWORD namelen = MAX_COMPUTERNAME_LENGTH + 1;
    const void *vmax = vmaxget();
    char *uname;
    char *udomain;

    checkArity(op, args);
    PROTECT(ans = allocVector(STRSXP, 9));
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
	else if(si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_ARM64)
	    strcat(ver, " arm64");
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
#ifdef __aarch64__
    SET_STRING_ELT(ans, 4, mkChar("aarch64"));
#elif defined(_WIN64)
    SET_STRING_ELT(ans, 4, mkChar("x86-64"));
#else
    SET_STRING_ELT(ans, 4, mkChar("x86"));
#endif
    if (!getCurrentUserAndDomain(&uname, &udomain)) {
	SET_STRING_ELT(ans, 5, mkChar("unknown"));
	SET_STRING_ELT(ans, 8, mkChar("unknown"));
	warning(_("cannot resolve current user or domain: '%s'"),
	       formatError(GetLastError()));
    } else {
	SET_STRING_ELT(ans, 5, mkChar(uname));
	SET_STRING_ELT(ans, 8, mkChar(udomain));
    }
    SET_STRING_ELT(ans, 6, STRING_ELT(ans, 5));
    SET_STRING_ELT(ans, 7, STRING_ELT(ans, 5));
    PROTECT(ansnames = allocVector(STRSXP, 9));
    SET_STRING_ELT(ansnames, 0, mkChar("sysname"));
    SET_STRING_ELT(ansnames, 1, mkChar("release"));
    SET_STRING_ELT(ansnames, 2, mkChar("version"));
    SET_STRING_ELT(ansnames, 3, mkChar("nodename"));
    SET_STRING_ELT(ansnames, 4, mkChar("machine"));
    SET_STRING_ELT(ansnames, 5, mkChar("login"));
    SET_STRING_ELT(ansnames, 6, mkChar("user"));
    SET_STRING_ELT(ansnames, 7, mkChar("effective_user"));
    SET_STRING_ELT(ansnames, 8, mkChar("udomain"));
    setAttrib(ans, R_NamesSymbol, ansnames);
    vmaxset(vmax);
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

#if _WIN32_WINNT < 0x0602
/* These constants were added to FILE_INFO_BY_HANDLE_CLASS in Windows 8 */
enum {
  FileStorageInfo = FileFullDirectoryRestartInfo + 1,
  FileAlignmentInfo,
  FileIdInfo,
  FileIdExtdDirectoryInfo,
  FileIdExtdDirectoryRestartInfo
};
#endif

#if _WIN32_WINNT < 0x602 || !defined(__MINGW32__)
/* Available in Windows Server 2012, but also in MinGW from Windows 8.  */
typedef struct _FILE_ID_INFO {
  ULONGLONG   VolumeSerialNumber;
  FILE_ID_128 FileId;
} FILE_ID_INFO, *PFILE_ID_INFO;
#endif

typedef BOOL (WINAPI *LPFN_GFIBH_EX) (HANDLE, FILE_INFO_BY_HANDLE_CLASS,
                                      LPVOID, DWORD);

static int isSameFile(HANDLE a, HANDLE b)
{
    FILE_ID_INFO aid, bid;

    memset(&aid, 0, sizeof(FILE_ID_INFO));
    memset(&bid, 0, sizeof(FILE_ID_INFO));
    if (!GetFileInformationByHandleEx(a, FileIdInfo, &aid, sizeof(FILE_ID_INFO)) ||
        !GetFileInformationByHandleEx(b, FileIdInfo, &bid, sizeof(FILE_ID_INFO)))
	/* on Vista and Win7 it is expected to fail because FileIdInfo
	   is not supported */
	return -1;

    if (aid.VolumeSerialNumber == bid.VolumeSerialNumber &&
	!memcmp(&aid.FileId, &bid.FileId, sizeof(FILE_ID_128)))

	return 1;
    else
	return 0;
}

/* returns R_alloc'd result */
static char *getFinalPathName(const char *orig)
{
    HANDLE horig, hres;
    int ret, ret1;

    /* FILE_FLAG_BACKUP_SEMANTICS needed to open a directory */
    horig = CreateFile(orig, 0,
                       FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                       NULL, OPEN_EXISTING,
	               FILE_ATTRIBUTE_HIDDEN | FILE_FLAG_BACKUP_SEMANTICS,
                       NULL);
    if (horig == INVALID_HANDLE_VALUE) 
	return NULL;

    ret = GetFinalPathNameByHandle(horig, NULL, 0, VOLUME_NAME_DOS);
    if (ret <= 0) {
	CloseHandle(horig);
	return NULL;
    }
    /* while the documentation says that "ret" shall include the size needed
       including the terminator, apparently it does not include the terminator.
       (seen on Windows 10, build 19045), so increase the size. */
    ret++;

    char *res = R_alloc(ret, 1);
    ret1 = GetFinalPathNameByHandle(horig, res, ret, VOLUME_NAME_DOS);
    if (ret1 <= 0 || ret1 >= ret) {
	CloseHandle(horig);
	return NULL;
    }
    
    /* get rid of the \\?\ prefix */
    int len = ret;
    int strip = 0;
    if (len < 4 || strncmp("\\\\?\\", res, 4)) {
	/* res should start with \\?\ */
	CloseHandle(horig);
	return NULL;
    }
    
    if (len > 8 && !strncmp("UNC\\", res+4, 4)) {
	/* UNC path \\?\UNC */
	res[6] = '\\'; /* replace the "C" in "UNC" to get "\\" prefix */
	strip = 6;
    } else if (len >= 6 && isalpha(res[4]) && res[5] == ':' && res[6] == '\\')
	/* \\?\D: */
	strip = 4;
    else {
	CloseHandle(horig);
	return NULL;
    }
    memmove(res, res+strip, len-strip+1);

    /* sanity check if the file exists using the normalized path, a normalized
       path to an existing file should still be working */
    /* FILE_FLAG_BACKUP_SEMANTICS needed to open a directory */
    hres = CreateFile(res, 0,
                      FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                      NULL, OPEN_EXISTING,
                      FILE_ATTRIBUTE_HIDDEN | FILE_FLAG_BACKUP_SEMANTICS,
                      NULL);
    if (hres == INVALID_HANDLE_VALUE) {
	CloseHandle(horig);
	return NULL;
    }

    /* check that the handles point to the same file, which may not be
       always the case because of silent best-fit encoding conversion
       done by Windows */
    ret = isSameFile(horig, hres);
    CloseHandle(horig);
    CloseHandle(hres);

    return (ret == 1) ? res : NULL;
}

/* returns R_alloc'd result */
static wchar_t *getFinalPathNameW(const wchar_t *orig)
{
    HANDLE horig, hres;
    int ret, ret1;

    /* FILE_FLAG_BACKUP_SEMANTICS needed to open a directory */
    horig = CreateFileW(orig, 0,
                        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                        NULL, OPEN_EXISTING,
                        FILE_ATTRIBUTE_HIDDEN | FILE_FLAG_BACKUP_SEMANTICS,
                        NULL);
    if (horig == INVALID_HANDLE_VALUE) 
	return NULL;

    ret = GetFinalPathNameByHandleW(horig, NULL, 0, VOLUME_NAME_DOS);
    if (ret <= 0) {
	CloseHandle(horig);
	return NULL;
    }

    wchar_t *wres = (wchar_t *)R_alloc(ret, sizeof(wchar_t));
    ret1 = GetFinalPathNameByHandleW(horig, wres, ret, VOLUME_NAME_DOS);
    if (ret1 <= 0 || ret1 >= ret) {
	CloseHandle(horig);
	return NULL;
    }
    
    /* get rid of the \\?\ prefix */
    size_t len = ret;
    int strip = 0;
    if (len < 4 || wcsncmp(L"\\\\?\\", wres, 4)) {
	/* res should start with \\?\ */
	CloseHandle(horig);
	return NULL;
    }
    
    if (len > 8 && !wcsncmp(L"UNC\\", wres+4, 4)) {
	/* UNC path \\?\UNC */
	wres[6] = L'\\';
	strip = 6;
    } else if (len >= 6 && Ri18n_iswctype(wres[4], Ri18n_wctype("alpha"))
	     && wres[5] == L':' && wres[6] == L'\\')
	/* \\?\D: */
	strip = 4;
    else {
	CloseHandle(horig);
	return NULL;
    }
    wmemmove(wres, wres+strip, len-strip+1);

    /* sanity check if the file exists using the normalized path, a normalized
       path to an existing file should still be working */
    /* FILE_FLAG_BACKUP_SEMANTICS needed to open a directory */
    hres = CreateFileW(wres, 0,
                       FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                       NULL, OPEN_EXISTING,
                       FILE_ATTRIBUTE_HIDDEN | FILE_FLAG_BACKUP_SEMANTICS,
                       NULL);
    if (hres == INVALID_HANDLE_VALUE) {
	CloseHandle(horig);
	return NULL;
    }

    /* sanity check that the handles point to the same file; they should, but
       better be safe wrt to undocumented features/changes of gfpnbhw */
    ret = isSameFile(horig, hres);
    CloseHandle(horig);
    CloseHandle(hres);

    /* return wres elso when isSameFile fails with -1 */
    return ret ? wres : NULL;
}

/* returns R_alloc'd result */
attribute_hidden wchar_t *R_getFullPathNameW(const wchar_t *orig)
{
    DWORD ret, ret1;

    ret = GetFullPathNameW(orig, 0, NULL, NULL);
    if (ret <= 0)
	return NULL;
    wchar_t *wres = (wchar_t*)R_alloc(ret, sizeof(wchar_t));
    ret1 = GetFullPathNameW(orig, ret, wres, NULL);
    if (ret1 <= 0 || ret1 >= ret) 
	return NULL;
    else
	return wres;
}

/* returns R_alloc'd result */
attribute_hidden char *R_getFullPathName(const char *orig)
{
    DWORD ret, ret1;

    ret = GetFullPathName(orig, 0, NULL, NULL);
    if (ret == 0 && GetLastError() == ERROR_FILENAME_EXCED_RANGE) {
	/* GetFullPathNameA unfortunately does not work with long paths
	   (tested on Windows 10 19045), it fails with
	   ERROR_FILENAME_EXCED_RANGE even when long paths are enabled. */
	size_t cnt = mbstowcs(NULL, orig, 0);
	if (cnt != (size_t)-1) {
	    cnt++;
	    wchar_t *worig = (wchar_t*) R_alloc(cnt, sizeof(wchar_t));
	    mbstowcs(worig, orig, cnt);
	    wchar_t *wres = R_getFullPathNameW(worig);
	    if (wres) {
		cnt = wcstombs(NULL, wres, 0) + 1;
		if (cnt != (size_t)-1) {
		    char *res = R_alloc(cnt, 1);
		    wcstombs(res, wres, cnt);
		    return res;
		}
	    }
	}
    }
    if (ret <= 0)
	return NULL;
    char *res = R_alloc(ret, 1);
    ret1 = GetFullPathName(orig, ret, res, NULL);
    if (ret1 <= 0 || ret1 >= ret) 
	return NULL;
    else
	return res;
}

/* returns R_alloc'd result */
static wchar_t *getLongPathNameW(const wchar_t *orig)
{
    DWORD ret, ret1;

    ret = GetLongPathNameW(orig, NULL, 0);
    if (ret <= 0)
	return NULL;
    wchar_t *wres = (wchar_t*)R_alloc(ret, sizeof(wchar_t));
    ret1 = GetLongPathNameW(orig, wres, ret);
    if (ret1 <= 0 || ret1 >= ret)
	return NULL;
    else
	return wres;
}

/* returns R_alloc'd result */
static char *getLongPathName(const char *orig)
{
    DWORD ret, ret1;

    ret = GetLongPathName(orig, NULL, 0);
    if (ret <= 0)
	return NULL;
    char *res = R_alloc(ret, 1);
    ret1 = GetLongPathName(orig, res, ret);
    if (ret1 <= 0 || ret1 >= ret) 
	return NULL;
    else 
	return res;
}

void R_UTF8fixslash(char *s); /* from main/util.c */
SEXP do_normalizepath(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, paths = CAR(args), el, slash;
    int i, n = LENGTH(paths);
    int mustWork, fslash = 0;
    const void *vmax = vmaxget();

    checkArity(op, args);
    if(!isString(paths))
	errorcall(call, _("'path' must be a character vector"));

    slash = CADR(args);
    if(!isString(slash) || LENGTH(slash) != 1)
	errorcall(call, "'winslash' must be a character string");
    const char *sl = translateCharFP(STRING_ELT(slash, 0));
    if (strcmp(sl, "/") && strcmp(sl, "\\"))
	errorcall(call, "'winslash' must be '/' or '\\\\'");
    if (strcmp(sl, "/") == 0) fslash = 1;
    
    mustWork = asLogical(CADDR(args));

    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
    	SEXP result;
	el = STRING_ELT(paths, i);
	result = el;
	if (el == NA_STRING) {
	    result = NA_STRING;
	    if(mustWork == 1)
		errorcall(call, "path[%d]=NA", i+1);
	    else if(mustWork == NA_LOGICAL)
		warningcall(call, "path[%d]=NA", i+1);
	} else if(getCharCE(el) == CE_UTF8) {
	    const wchar_t *wel = filenameToWchar(el, FALSE);
	    wchar_t *wfull = R_getFullPathNameW(wel);
	    wchar_t *wnorm = getFinalPathNameW(wel);

	    /* if normalized to UNC path but full path is D:..., fall back
	       to GetLongPathName */
	    if (wnorm && wnorm[0] == L'\\' && wnorm[1] == L'\\' &&
		wfull && Ri18n_iswctype(wfull[0], Ri18n_wctype("alpha")) &&
	        wfull[1] == L':') wnorm = NULL;
	    if (!wnorm && wfull)
		/* silently fall back to GetFullPathName/GetLongPathName */
		/* getLongPathName will fail for non-existent paths */
		wnorm = getLongPathNameW(wfull);
	    if (wnorm) {
		if (fslash)
		    R_wfixslash(wnorm);
		result = mkCharWUTF8(wnorm);
	    } else {
		if (mustWork == 1) {
		    errorcall(call, "path[%d]=\"%ls\": %s", i+1, 
			      wel, formatError(GetLastError()));
		} else if (mustWork == NA_LOGICAL) {
		    warningcall(call, "path[%d]=\"%ls\": %s", i+1, 
				wel, formatError(GetLastError()));
		}
		if (wfull) {
		    if (fslash)
			R_wfixslash(wfull);
		    result = mkCharWUTF8(wfull);
		} else {
		    const char *elutf8 = translateCharUTF8(el);
		    if (fslash) {
			char *normutf8 = R_alloc(strlen(elutf8) + 1, 1);
			strcpy(normutf8, elutf8);
			R_UTF8fixslash(normutf8);
			result = mkCharCE(normutf8, CE_UTF8);
		    } else
			result = mkCharCE(elutf8, CE_UTF8);
		}
	    }
	} else {
	    const char *tel = translateChar(el);
	    char *full = R_getFullPathName(tel);
	    char *norm = getFinalPathName(tel);

	    /* if normalized to UNC path but full path is D:..., fall back
	       to GetLongPathName */
	    if (norm && norm[0] == '\\' && norm[1] == '\\' &&
		full && isalpha(full[0]) && full[1] == ':') norm = NULL;
	    if (!norm && full)
		/* silently fall back to GetFullPathName/GetLongPathName */
		/* getLongPathName will fail for non-existent paths */
		norm = getLongPathName(full);
	    if (norm) {
		if (fslash)
		    R_fixslash(norm);
		result = mkChar(norm);
	    } else {
		if (mustWork == 1) {
		    errorcall(call, "path[%d]=\"%s\": %s", i+1, 
			      tel, formatError(GetLastError()));
		} else if (mustWork == NA_LOGICAL) {
		    warningcall(call, "path[%d]=\"%s\": %s", i+1, 
				tel, formatError(GetLastError()));
		}
		if (full) {
		    if (fslash)
			R_fixslash(full);
		    result = mkChar(full);
		} else if (fslash) {
		    norm = R_alloc(strlen(tel) + 1, 1);
		    strcpy(norm, tel);
		    R_fixslash(norm);
		    result = mkChar(norm);
		} else
		    result = mkChar(tel);
	    }
	}
	SET_STRING_ELT(ans, i, result);
    }
    vmaxset(vmax);
    UNPROTECT(1);
    return ans;
}

/* utils::shortPathName */
SEXP in_shortpath(SEXP paths)
{
    SEXP ans, el;
    int i, n = LENGTH(paths);
    DWORD res;
    const void *vmax = vmaxget();

    if(!isString(paths)) error(_("'path' must be a character vector"));

    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	el = STRING_ELT(paths, i);
	if(getCharCE(el) == CE_UTF8) {
	    wchar_t *wfn = filenameToWchar(el, FALSE);
	    res = GetShortPathNameW(wfn, NULL, 0);
	    if (res > 0) {
		wchar_t *wsfn = (wchar_t*)R_alloc(res, sizeof(wchar_t));
		DWORD res1 = GetShortPathNameW(wfn, wsfn, res);
		if (res1 > 0 && res1 < res) {
		    /* documented to return paths using \, which the API call
		       does not necessarily do */
		    R_wfixbackslash(wsfn);
		    SET_STRING_ELT(ans, i, mkCharWUTF8(wsfn));
		    continue;
		}
	    }
	} else {
	    const char *fn = translateChar(el);
	    res = GetShortPathName(fn, NULL, 0);
	    if (res > 0) {
		char *sfn = R_alloc(res, 1);
		DWORD res1 = GetShortPathName(fn, sfn, res);
		if (res1 > 0 && res1 < res) {
		    /* documented to return paths using \, which the API call
		       does not necessarily do */
		    R_fixbackslash(sfn);
		    SET_STRING_ELT(ans, i, mkChar(sfn));
		    continue;
		}
	    }
	}
	/* we didn't get a short name, so return the original with
	   backslashes as separators (as documented) */
	const char *fn = translateChar(el);
	char *ffn = R_alloc(strlen(fn) + 1, 1);
	strcpy(ffn, fn);
	R_fixbackslash(ffn);
	SET_STRING_ELT(ans, i, mkChar(ffn));
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

/* base::file.choose */
SEXP attribute_hidden do_filechoose(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    wchar_t *fn;

    checkArity(op, args);
    setuserfilterW(L"All files (*.*)\0*.*\0\0");
    fn = askfilenameW(G_("Select file"), "");
    if (!fn)
	error(_("file choice cancelled"));
    PROTECT(ans = allocVector(STRSXP, 1));
    SET_STRING_ELT(ans, 0, mkCharWUTF8(fn));
    UNPROTECT(1);
    return ans;
}

const char *getTZinfo(void);  // src/extra/tzone/registryTZ.c

SEXP attribute_hidden do_tzone_name(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return mkString(getTZinfo());
}

