/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file extra.c
 *  Copyright (C) 1998--2003  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004	      The R Foundation
 *  Copyright (C) 2005--2007  The R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */


/* extra commands for R */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "win-nls.h"

#include <stdio.h>
#include "Defn.h"
#include "Fileio.h"
#include <direct.h>
#include <time.h>
#include <windows.h>
#include "graphapp/ga.h"
#include "rui.h"


#include <sys/types.h>
#include <sys/stat.h>

static int R_unlink(char *names, int recursive);

static int R_unlink_one(char *dir, char *name, int recursive)
{
    char tmp[MAX_PATH];

    if(strcmp(name, ".") == 0) return 0;
    if(strcmp(name, "..") == 0) return 0;
    if(strlen(dir)) {
	strcpy(tmp, dir);
	if(*(dir + strlen(dir) - 1) != '\\') strcat(tmp, "\\");
	strcat(tmp, name);
    } else strcpy(tmp, name);
    return (recursive ? R_unlink(tmp, 1): unlink(tmp)) !=0;
}

static int R_unlink(char *names, int recursive)
{
    int failures = 0;
    char *p, tmp[MAX_PATH], dir[MAX_PATH+2];
    WIN32_FIND_DATA find_data;
    HANDLE fh;
    struct stat sb;

    if(strlen(names) >= MAX_PATH) error(_("invalid 'names' in 'R_unlink'"));
    strcpy(tmp, names);
    for(p = tmp; *p != '\0'; p++) if(*p == '/') *p = '\\';
    if(stat(tmp, &sb) == 0) {
	/* Is this a directory? */
	if(sb.st_mode & _S_IFDIR) {
	    if(recursive) {
		strcpy(dir, tmp); strcat(tmp, "\\*");
		fh = FindFirstFile(tmp, &find_data);
		if (fh != INVALID_HANDLE_VALUE) {
		    failures += R_unlink_one(dir, find_data.cFileName, 1);
		    while(FindNextFile(fh, &find_data))
			failures += R_unlink_one(dir, find_data.cFileName, 1);
		    FindClose(fh);
		}
		/* Use short path as e.g. ' test' fails */
		GetShortPathName(dir, tmp, MAX_PATH);
		if(rmdir(tmp)) failures++;
	    } else failures++; /* don't try to delete dirs */
	} else {/* Regular file */
	    failures += R_unlink_one("", names, 0);
	}
    }
    return failures;
}


SEXP do_unlink(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  fn, ans;
    int i, nfiles, failures = 0, recursive;

    checkArity(op, args);
    fn = CAR(args);
    nfiles = length(fn);
    if (nfiles > 0) {
    	if (!isString(fn))
	    errorcall(call, _("invalid '%s' argument"), "x");
	recursive = asLogical(CADR(args));
    	if (recursive == NA_LOGICAL)
	    errorcall(call, _("invalid '%s' argument"), "recursive");
    	for(i = 0; i < nfiles; i++)
	    failures += R_unlink(CHAR(STRING_ELT(fn, i)), recursive);
    }
    PROTECT(ans = allocVector(INTSXP, 1));
    if (!failures)
	INTEGER(ans)[0] = 0;
    else
	INTEGER(ans)[0] = 1;
    UNPROTECT(1);
    return (ans);
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
    OSVERSIONINFO verinfo;

    checkArity(op, args);
    verinfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&verinfo);
    switch(verinfo.dwPlatformId) {
    case VER_PLATFORM_WIN32_NT:
	strcpy(isNT, "NT");
	break;
    case VER_PLATFORM_WIN32_WINDOWS:
	switch(verinfo.dwMinorVersion ) {
	case 0:
	    strcpy(isNT, "95");
	    if (verinfo.szCSDVersion[1] == 'C') strcat(isNT, " OSR2" );
	    break;
	case 10:
	    strcpy(isNT, "98");
	    if (verinfo.szCSDVersion[1] == 'A') strcat(isNT, " SE" );
	    break;
	case 90:
	    strcpy(isNT, "ME");
	    break;
	default:
	    strcpy(isNT, "9x");
	}
	break;
    case VER_PLATFORM_WIN32s:
	strcpy(isNT, "win32s");
	break;
    default:
	sprintf(isNT, "ID=%d", (int)verinfo.dwPlatformId);
	break;
    }

    if((int)verinfo.dwMajorVersion >= 5) {
	OSVERSIONINFOEX osvi;
	osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
	if(GetVersionEx((OSVERSIONINFO *)&osvi)) {
	    char tmp[]="", *desc= tmp, *type = tmp;
	    if(osvi.dwMajorVersion == 6)
		desc = "Vista";
	    if(osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0)
		desc = "2000";
	    if(osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1)
		desc = "XP";
	    if(osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2)
		desc = "Sever 2003";
            if ( osvi.wProductType == VER_NT_WORKSTATION ) {
               if( osvi.wSuiteMask & VER_SUITE_PERSONAL ) type = " Home";
            } else if ( osvi.wProductType == VER_NT_SERVER )
            {
               if ( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1 )
		   desc = " .NET";
               if( osvi.wSuiteMask & VER_SUITE_DATACENTER )
		   type = " DataCenter Server";
               else if( osvi.wSuiteMask & VER_SUITE_ENTERPRISE )
		   type = " Advanced Server";
               else if ( osvi.wSuiteMask == VER_SUITE_BLADE )
		   type = " Web Server";
               else
		   type = " Server";
            }

	    sprintf(ver,
		    "Windows %s%s (build %d) Service Pack %d.%d",
		    desc, type,
		    LOWORD(osvi.dwBuildNumber),
		    (int)osvi.wServicePackMajor,
		    (int)osvi.wServicePackMinor);
	} else {
	    sprintf(ver, "Windows 2000 %d.%d (build %d) %s",
		    (int)verinfo.dwMajorVersion, (int)verinfo.dwMinorVersion,
		    LOWORD(verinfo.dwBuildNumber), verinfo.szCSDVersion);
	}
    } else {
	sprintf(ver, "Windows %s %d.%d (build %d) %s", isNT,
		(int)verinfo.dwMajorVersion, (int)verinfo.dwMinorVersion,
		LOWORD(verinfo.dwBuildNumber), verinfo.szCSDVersion);
    }

    return mkString(ver);
}

/* also used in rui.c */
void internal_shellexec(char * file)
{
    char *home;
    unsigned int ret;

    home = getenv("R_HOME");
    if (home == NULL)
	error(_("R_HOME not set"));
    ret = (unsigned int) ShellExecute(NULL, "open", file, NULL, home, SW_SHOW);
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

SEXP do_shellexec(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP file;

    checkArity(op, args);
    file = CAR(args);
    if (!isString(file) || length(file) != 1)
	errorcall(call, _("invalid '%s' argument"), "file");
    internal_shellexec(CHAR(STRING_ELT(file, 0)));
    return R_NilValue;
}

int check_doc_file(char * file)
{
    char *home, path[MAX_PATH];
    struct stat sb;

    home = getenv("R_HOME");
    if (home == NULL)
	error(_("R_HOME not set"));
    if(strlen(home) + strlen(file) + 1 >= MAX_PATH) return(1); /* cannot exist */
    strcpy(path, home);
    strcat(path, "/");
    strcat(path, file);
    return stat(path, &sb) == 0;
}

SEXP do_windialog(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP message;
    char * type;
    int res=YES;

    checkArity(op, args);
    type = CHAR(STRING_ELT(CAR(args), 0));
    message = CADR(args);
    if(!isString(message) || length(message) != 1 || 
       strlen(CHAR(STRING_ELT(message, 0))) > 255)
	error(_("invalid '%s' argument"), "message");
    if (strcmp(type, "ok")  == 0) {
	askok(CHAR(STRING_ELT(message, 0)));
	res = 10;
    } else if (strcmp(type, "okcancel")  == 0) {
	res = askokcancel(CHAR(STRING_ELT(message, 0)));
	if(res == YES) res = 2;
    } else if (strcmp(type, "yesno")  == 0) {
	res = askyesno(CHAR(STRING_ELT(message, 0)));
    } else if (strcmp(type, "yesnocancel")  == 0) {
	res = askyesnocancel(CHAR(STRING_ELT(message, 0)));
    } else
	errorcall(call, _("unknown type"));
    return ScalarInteger(res);
}

SEXP do_windialogstring(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  message, def;
    char *string;

    checkArity(op, args);
    message = CAR(args);
    if(!isString(message) || length(message) != 1 || 
       strlen(CHAR(STRING_ELT(message, 0))) > 255)
	error(_("invalid '%s' argument"), "message");
    def = CADR(args);
    if(!isString(def) || length(def) != 1)
	error(_("invalid '%s' argument"), "default");
    string = askstring(CHAR(STRING_ELT(message, 0)), CHAR(STRING_ELT(def, 0)));
    if (string) return mkString(string);
    else return R_NilValue;
}

#include "Startup.h"
extern UImode CharacterMode;
static char msgbuf[256];

SEXP do_winmenunames(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP menuNames;
    int i, nmenus;

    checkArity(op, args);
    if (CharacterMode != RGui)
	errorcall(call, _("Menu functions can only be used in the GUI"));

    nmenus = numwinmenus();

    PROTECT(menuNames = allocVector(STRSXP, nmenus));

    for (i = 0; i < nmenus; i++) {
	SET_STRING_ELT(menuNames, i, mkChar(getusermenuname(i)));
    }

    UNPROTECT(1);
    return(menuNames);
}

SEXP do_wingetmenuitems(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP mname, ans, ansnames;
    menuItems *items;
    char errmsg[50];
    int i;


    checkArity(op, args);
    if (CharacterMode != RGui)
	errorcall(call, _("Menu functions can only be used in the GUI"));

    mname = CAR(args);
    if (!isString(mname) || length(mname) != 1)
	error(_("invalid '%s' argument"), "menuname");

    items = wingetmenuitems(CHAR(STRING_ELT(mname,0)), errmsg);
    if (items->numItems == 0) {
	sprintf(msgbuf, _("unable to retrieve items for %s (%s)"),
		CHAR(STRING_ELT(mname,0)), errmsg);
	freemenuitems(items);
	errorcall(call, msgbuf);
    }

    PROTECT(ans = allocVector(STRSXP, items->numItems));
    PROTECT(ansnames = allocVector(STRSXP, items->numItems));
    for (i = 0; i < items->numItems; i++) {
	SET_STRING_ELT(ans, i, mkChar(items->mItems[i]->action));
	SET_STRING_ELT(ansnames, i, mkChar(items->mItems[i]->name));
    }

    setAttrib(ans, R_NamesSymbol, ansnames);

    freemenuitems(items);

    UNPROTECT(2);
    return(ans);
}


SEXP do_winmenuadd(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP smenu, sitem;
    int res;
    char errmsg[50];

    checkArity(op, args);
    if (CharacterMode != RGui)
	errorcall(call, _("Menu functions can only be used in the GUI"));
    smenu = CAR(args);
    if(!isString(smenu) || length(smenu) != 1)
	error(_("invalid '%s' argument"), "menuname");
    sitem = CADR(args);
    if (isNull(sitem)) { /* add a menu */
	res = winaddmenu (CHAR(STRING_ELT(smenu, 0)), errmsg);
	if (res > 0) {
	    sprintf(msgbuf, _("unable to add menu (%s)"), errmsg);
	    errorcall(call, msgbuf);
	}

    } else { /* add an item */
	if(!isString(sitem) || length(sitem) != 1)
	    error(_("invalid '%s' argument"), "itemname");
	res = winaddmenuitem (CHAR(STRING_ELT(sitem, 0)),
			      CHAR(STRING_ELT(smenu, 0)),
			      CHAR(STRING_ELT(CADDR(args), 0)),
			      errmsg);
	if (res > 0) {
	    sprintf(msgbuf, _("unable to add menu item (%s)"), errmsg);
	    errorcall(call, msgbuf);
	}
    }
    return (R_NilValue);
}

SEXP do_winmenudel(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP smenu, sitem;
    int res;
    char errmsg[50];

    checkArity(op, args);
    if (CharacterMode != RGui)
	errorcall(call, _("Menu functions can only be used in the GUI"));
    smenu = CAR(args);
    if(!isString(smenu) || length(smenu) != 1)
	error(_("invalid '%s' argument"), "menuname");
    sitem = CADR(args);
    if (isNull(sitem)) { /* delete a menu */
	res = windelmenu (CHAR(STRING_ELT(smenu, 0)), errmsg);
	if (res > 0)
	    errorcall(call, _("menu does not exist"));
    } else { /* delete an item */
	if(!isString(sitem) || length(sitem) != 1)
	    error(_("invalid '%s' argument"), "itemname");
	res = windelmenuitem (CHAR(STRING_ELT(sitem, 0)),
			      CHAR(STRING_ELT(smenu, 0)), errmsg);
	if (res > 0) {
	    sprintf(msgbuf, _("unable to delete menu item (%s)"), errmsg);
	    errorcall(call, msgbuf);
	}
    }
    return (R_NilValue);
}


void Rwin_fpset()
{
    /* Under recent MinGW this is what fpreset does.  It sets the
       control word to 0x37f which corresponds to 0x8001F as used by
       _controlfp.  That is all errors are masked, 64-bit mantissa and
       rounding are selected. */

    __asm__ ( "fninit" ) ;
}

#include "getline/getline.h"  /* for gl_load/savehistory */
SEXP do_savehistory(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile;

    checkArity(op, args);
    sfile = CAR(args);
    if (!isString(sfile) || LENGTH(sfile) < 1)
	errorcall(call, _("invalid '%s' argument"), "file");
    if (CharacterMode == RGui || (R_Interactive && CharacterMode == RTerm)) {
	R_setupHistory(); /* re-read the history size */
	gl_savehistory(CHAR(STRING_ELT(sfile, 0)), R_HistorySize);
    } else
	errorcall(call, _("'savehistory' can only be used in Rgui and Rterm"));
    return R_NilValue;
}

SEXP do_loadhistory(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile;

    checkArity(op, args);
    sfile = CAR(args);
    if (!isString(sfile) || LENGTH(sfile) < 1)
	errorcall(call, _("invalid '%s' argument"), "file");
    if (CharacterMode == RGui || (R_Interactive && CharacterMode == RTerm))
	gl_loadhistory(CHAR(STRING_ELT(sfile, 0)));
    else
	errorcall(call, _("'loadhistory' can only be used in Rgui and Rterm"));
    return R_NilValue;
}

SEXP do_addhistory(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP stamp;
    int i;
    
    checkArity(op, args);
    stamp = CAR(args);
    if (!isString(stamp))
    	errorcall(call, _("invalid timestamp"));
    if (CharacterMode == RGui || (R_Interactive && CharacterMode == RTerm))  	
	for (i = 0; i < LENGTH(stamp); i++) 
	    gl_histadd(CHAR(STRING_ELT(stamp, i)));
    return R_NilValue;
}

#include <preferences.h>

SEXP do_loadRconsole(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile;
    struct structGUI gui;

    checkArity(op, args);
    sfile = CAR(args);
    if (!isString(sfile) || LENGTH(sfile) < 1)
	errorcall(call, _("invalid '%s' argument"), "file");
    if (CharacterMode == RGui) {
    	getActive(&gui);
	if (loadRconsole(&gui, (CHAR(STRING_ELT(sfile, 0))))) applyGUI(&gui);
    } else
	errorcall(call, _("'loadRconsole' can only be used in Rgui"));
    return R_NilValue;
}
    
#include <lmcons.h>

SEXP do_sysinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, ansnames;
    OSVERSIONINFO verinfo;
    char isNT[8]="??", ver[256],
	name[MAX_COMPUTERNAME_LENGTH + 1], user[UNLEN+1];
    DWORD namelen = MAX_COMPUTERNAME_LENGTH + 1, userlen = UNLEN+1;

    checkArity(op, args);
    PROTECT(ans = allocVector(STRSXP, 7));
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

    SET_STRING_ELT(ans, 0, mkChar("Windows"));
    sprintf(ver, "%s %d.%d", isNT,
	    (int)verinfo.dwMajorVersion, (int)verinfo.dwMinorVersion);
    SET_STRING_ELT(ans, 1, mkChar(ver));
    sprintf(ver, "(build %d) %s", LOWORD(verinfo.dwBuildNumber),
	    verinfo.szCSDVersion);
    SET_STRING_ELT(ans, 2, mkChar(ver));
    GetComputerName(name, &namelen);
    SET_STRING_ELT(ans, 3, mkChar(name));
    SET_STRING_ELT(ans, 4, mkChar("x86"));
    GetUserName(user, &userlen);
    SET_STRING_ELT(ans, 5, mkChar(user));
    SET_STRING_ELT(ans, 6, STRING_ELT(ans, 5));
    PROTECT(ansnames = allocVector(STRSXP, 7));
    SET_STRING_ELT(ansnames, 0, mkChar("sysname"));
    SET_STRING_ELT(ansnames, 1, mkChar("release"));
    SET_STRING_ELT(ansnames, 2, mkChar("version"));
    SET_STRING_ELT(ansnames, 3, mkChar("nodename"));
    SET_STRING_ELT(ansnames, 4, mkChar("machine"));
    SET_STRING_ELT(ansnames, 5, mkChar("login"));
    SET_STRING_ELT(ansnames, 6, mkChar("user"));
    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}

SEXP do_syssleep(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    DWORD mtime;
    int ntime;
    double time;

    checkArity(op, args);
    time = asReal(CAR(args));
    if (ISNAN(time) || time < 0)
	errorcall(call, _("invalid '%s' value"), "time");
    ntime = 1000*(time) + 0.5;
    while (ntime > 0) {
	mtime = min(500, ntime);
	ntime -= mtime;
	Sleep(mtime);
	R_ProcessEvents();
    }
    return R_NilValue;
}

#ifdef LEA_MALLOC
struct mallinfo {
  int arena;    /* non-mmapped space allocated from system */
  int ordblks;  /* number of free chunks */
  int smblks;   /* number of fastbin blocks */
  int hblks;    /* number of mmapped regions */
  int hblkhd;   /* space in mmapped regions */
  int usmblks;  /* maximum total allocated space */
  int fsmblks;  /* space available in freed fastbin blocks */
  int uordblks; /* total allocated space */
  int fordblks; /* total free space */
  int keepcost; /* top-most, releasable (via malloc_trim) space */
};
extern unsigned int R_max_memory;

struct mallinfo mallinfo();
#endif

SEXP do_memsize(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    int maxmem;

    checkArity(op, args);
    if(isLogical(CAR(args))) {
	maxmem = asLogical(CAR(args));
	/* changed to real in 1.8.1 as might exceed 2G */
	PROTECT(ans = allocVector(REALSXP, 1));
#ifdef LEA_MALLOC
	if(maxmem == NA_LOGICAL)
	    REAL(ans)[0] = R_max_memory;
	else if(maxmem)
	    REAL(ans)[0] = mallinfo().usmblks;
	else
	    REAL(ans)[0] = mallinfo().uordblks;
#else
	REAL(ans)[0] = NA_REAL;
#endif
	UNPROTECT(1);
	return ans;
    } else if(isReal(CAR(args))) {
	unsigned int newmax;
	double mem = asReal(CAR(args));
	if (!R_FINITE(mem))
	    errorcall(call, _("incorrect argument"));
#ifdef LEA_MALLOC
	if(mem >= 4096)
	    errorcall(call, _("don't be silly!: your machine has a 4Gb address limit"));
	newmax = mem * 1048576.0;
	if (newmax < R_max_memory)
	    errorcall(call, _("cannot decrease memory limit"));
	R_max_memory = newmax;
#endif
    } else
	errorcall(call, _("incorrect argument"));
    return R_NilValue;
}

SEXP do_dllversion(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP path=R_NilValue, ans;
    char *dll;
    DWORD dwVerInfoSize;
    DWORD dwVerHnd;

    checkArity(op, args);
    path = CAR(args);
    if(!isString(path) || LENGTH(path) != 1)
	errorcall(call, _("invalid '%s' argument"), "path");
    dll = CHAR(STRING_ELT(path, 0));
    dwVerInfoSize = GetFileVersionInfoSize(dll, &dwVerHnd);
    PROTECT(ans = allocVector(STRSXP, 2));
    SET_STRING_ELT(ans, 0, mkChar(""));
    SET_STRING_ELT(ans, 1, mkChar(""));
    if (dwVerInfoSize) {
	BOOL  fRet;
	LPSTR lpstrVffInfo;
	LPSTR lszVer = NULL;
	UINT  cchVer = 0;

	lpstrVffInfo = (LPSTR) malloc(dwVerInfoSize);
	if (GetFileVersionInfo(dll, 0L, dwVerInfoSize, lpstrVffInfo))
	{

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

static window wselect;
static button bFinish, bCancel;
static listbox f_list;
static char selected[100];
static int done;

static void cleanup()
{
    hide(wselect);
    delobj(f_list); delobj(bFinish); delobj(bCancel);
    delobj(wselect);
}


static void cancel(button b)
{
    strcpy(selected, "");
    done = 2;
}

static void finish(button b)
{
    strncpy(selected, GA_gettext(f_list), 100);
    done = 1;
}

static void key1(control c, int ch)
{
    if(ch == '\n') finish(NULL);
    if(ch == ESC)  cancel(NULL);
}

rect getSysFontSize(); /* in graphapp/fonts.c */
RECT *RgetMDIsize(); /* in rui.c */

SEXP do_selectlist(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP list, preselect, ans = R_NilValue;
    char **clist;
    int i, j = -1, n, mw = 0, multiple, nsel = 0;
    int xmax, ymax, ylist, fht, h0;
    Rboolean haveTitle;

    checkArity(op, args);
    list = CAR(args);
    if(!isString(list)) error(_("invalid '%s' argument"), "list");
    preselect = CADR(args);
    if(!isNull(preselect) && !isString(preselect))
	error(_("invalid '%s' argument"), "preselect");
    multiple = asLogical(CADDR(args));
    if(multiple == NA_LOGICAL) multiple = 0;
    haveTitle = isString(CADDDR(args));
    if(!multiple && isString(preselect) && LENGTH(preselect) != 1)
	error(_("invalid '%s' argument"), "preselect");

    n = LENGTH(list);
    clist = (char **) R_alloc(n + 1, sizeof(char *));
    for(i = 0; i < n; i++) {
	clist[i] = CHAR(STRING_ELT(list, i));
	mw = max(mw, gstrwidth(NULL, SystemFont, clist[i]));
    }
    clist[n] = NULL;

    fht = getSysFontSize().height;
    
    xmax = max(170, mw+60); /* allow for scrollbar */
    if(ismdi()) {
	RECT *pR = RgetMDIsize();
	h0 = pR->bottom;
    } else {
	h0 = deviceheight(NULL);
    }
    ymax = min(80+fht*n, h0-100); /* allow for window widgets, toolbar */
    ylist = ymax - 60;
    wselect = newwindow(haveTitle ? CHAR(STRING_ELT(CADDDR(args), 0)):
			(multiple ? _("Select one or more") : _("Select one")),
			rect(0, 0, xmax, ymax),
			Titlebar | Centered | Modal | Floating);
    setbackground(wselect, dialog_bg());
    if(multiple)
	f_list = newmultilist(clist, rect(10, 10, xmax-25, ylist), NULL);
    else
	f_list = newlistbox(clist, rect(10, 10, xmax-25, ylist), NULL);
    if(!isNull(preselect) && LENGTH(preselect)) {
	for(i = 0; i < n; i++)
	    for(j = 0; j < LENGTH(preselect); j++)
		if(strcmp(clist[i], CHAR(STRING_ELT(preselect, j))) == 0) {
		    setlistitem(f_list, i);
		    break;
		}
    }
    bFinish = newbutton(G_("OK"), rect(xmax-160, ymax-40, 70, 25), finish);
    bCancel = newbutton(G_("Cancel"), rect(xmax-80, ymax-40, 70, 25), cancel);
    setkeydown(wselect, key1);
    show(wselect);
    done = 0;
    while(!done) {
	Sleep(100);
	R_ProcessEvents();
    }

    if(multiple) {
	if (done == 1) { /* Finish */
	    for(i = 0; i < n; i++)  if(isselected(f_list, i)) nsel++;
	    PROTECT(ans = allocVector(STRSXP, nsel));
	    for(i = 0, j = 0; i < n; i++)
		if(isselected(f_list, i))
		    SET_STRING_ELT(ans, j++, mkChar(clist[i]));
	} else { /* cancel */
	    PROTECT(ans = allocVector(STRSXP, 0));
	}
    } else {
	PROTECT(ans = allocVector(STRSXP, 1));
	SET_STRING_ELT(ans, 0, mkChar(selected));
    }
    cleanup();
    show(RConsole);
    UNPROTECT(1);
    return ans;
}

int Rwin_rename(char *from, char *to)
{
    int res = 0;
    OSVERSIONINFO verinfo;

    verinfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&verinfo);
    switch(verinfo.dwPlatformId) {
    case VER_PLATFORM_WIN32_NT:
	res = (MoveFileEx(from, to, MOVEFILE_REPLACE_EXISTING) == 0);
	break;
    default:
	if (!DeleteFile(to) && GetLastError() != ERROR_FILE_NOT_FOUND)
	    return 1;
	res = (MoveFile(from, to) == 0);
    }
    return res;
}


void R_CleanTempDir()
{
    if(Sys_TempDir) {
	/* Windows cannot delete the current working directory */
	SetCurrentDirectory(R_HomeDir());
	R_unlink(Sys_TempDir, 1);
    }
}

SEXP do_getClipboardFormats(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = R_NilValue;
    int j, size, format = 0;
    
    checkArity(op, args);
    
    if(OpenClipboard(NULL)) {
    	size = CountClipboardFormats();
    	PROTECT(ans = allocVector(INTSXP, size));
    	for (j = 0; j < size; j++) {
    	    format = EnumClipboardFormats(format);
    	    INTEGER(ans)[j] = format;
    	}
    	UNPROTECT(1);
    	CloseClipboard();
    }
    return ans;
}

SEXP do_readClipboard(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = R_NilValue;
    HGLOBAL hglb;
    char *pc;
    int j, format, raw, size;

    checkArity(op, args);
    format = asInteger(CAR(args));
    raw = asLogical(CADR(args));

    if(OpenClipboard(NULL)) {
    	if(IsClipboardFormatAvailable(format) &&
    	   	(hglb = GetClipboardData(format)) &&
    	   	(pc = (char *)GlobalLock(hglb))) {
	    if(!raw) {
		PROTECT(ans = allocVector(STRSXP, 1));
		SET_STRING_ELT(ans, 0, mkChar(pc));
	    } else {
		size = GlobalSize(hglb);
		PROTECT(ans = allocVector(RAWSXP, size));
		for (j = 0; j < size; j++) RAW(ans)[j] = *pc++;
	    }
	    GlobalUnlock(hglb);
	    UNPROTECT(1);	
	}    
	CloseClipboard();
    }
    return ans;
}

SEXP do_writeClipboard(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP text;
    int i, n, format;
    HGLOBAL hglb;
    char *s, *p;
    Rboolean success = FALSE, raw = FALSE;

    checkArity(op, args);
    text = CAR(args);
    format = asInteger(CADR(args));

    if (TYPEOF(text) == RAWSXP) raw = TRUE;
    else if(!isString(text)) 
    	errorcall(call, _("argument must be a character vector or a raw vector"));
    
    n = length(text);  
    if(n > 0) {
    	int len = 1;
    	if(!raw) 
    	    for(i = 0; i < n; i++) len += strlen(CHAR(STRING_ELT(text, i))) + 2;
    	else len = n;
	if ( (hglb = GlobalAlloc(GHND, len)) &&
	     (s = (char *)GlobalLock(hglb)) ) {
	    if(!raw) {
		for(i = 0; i < n; i++) {
		    p = CHAR(STRING_ELT(text, i));
		    while(*p) *s++ = *p++;
		    *s++ = '\r'; *s++ = '\n';
		}
		*s = '\0';
	    } else 
	    	for(i = 0; i < n; i++) *s++ = RAW(text)[i];
				
	    GlobalUnlock(hglb);
	    if (!OpenClipboard(NULL) || !EmptyClipboard()) {
		warningcall(call, _("Unable to open the clipboard"));
		GlobalFree(hglb);
	    } else {
		success = SetClipboardData(CF_TEXT, hglb) != 0;
		if(!success) {
		    warningcall(call, _("Unable to write to the clipboard"));
		    GlobalFree(hglb);
		}
		CloseClipboard();
	    }
	}
    }
    return ScalarLogical(success);
}

/* We cannot use GetLongPathName (missing on W95/NT4) so write our own
   based on that in Perl.  NB: may not be MBCS-correct.
 */

#define isSLASH(c) ((c) == '/' || (c) == '\\')
#define SKIP_SLASHES(s) while (*(s) && isSLASH(*(s))) ++(s);
#define COPY_NONSLASHES(d,s) while (*(s) && !isSLASH(*(s))) *(d)++ = *(s)++;

static void longpathname(char *path)
{
    WIN32_FIND_DATA fdata;
    HANDLE fhand;
    char tmpbuf[MAX_PATH+1], *tmpstart = tmpbuf, *start = path, sep;
    if(!path) return;
    
    /* drive prefix */
    if (isalpha(path[0]) && path[1] == ':') {
        start = path + 2;
        *tmpstart++ = path[0];
        *tmpstart++ = ':';
    }
    /* UNC prefix */
    else if (isSLASH(path[0]) && isSLASH(path[1])) {
        start = path + 2;
        *tmpstart++ = path[0];
        *tmpstart++ = path[1];
        SKIP_SLASHES(start);
        COPY_NONSLASHES(tmpstart,start);        /* copy machine name */
        if (*start) {
            *tmpstart++ = *start++;
            SKIP_SLASHES(start);
            COPY_NONSLASHES(tmpstart,start);    /* copy share name */
        }
    }
    *tmpstart = '\0';
    while (*start) {
        /* copy initial slash, if any */
        if (isSLASH(*start)) {
            *tmpstart++ = *start++;
            *tmpstart = '\0';
            SKIP_SLASHES(start);
        }

        /* FindFirstFile() expands "." and "..", so we need to pass
         * those through unmolested */
        if (*start == '.'
            && (!start[1] || isSLASH(start[1])
                || (start[1] == '.' && (!start[2] || isSLASH(start[2]))))) {
            COPY_NONSLASHES(tmpstart,start);    /* copy "." or ".." */
            *tmpstart = '\0';
            continue;
        }

        if (!*start) break;

        /* now we're at a non-slash; walk up to next slash */
        while (*start && !isSLASH(*start)) ++start;

        /* stop and find full name of component */
        sep = *start;
        *start = '\0';
        fhand = FindFirstFile(path,&fdata);
        *start = sep;
        if (fhand != INVALID_HANDLE_VALUE) {
            size_t len = strlen(fdata.cFileName);
            if ((size_t)(tmpbuf + sizeof(tmpbuf) - tmpstart) > len) {
                strcpy(tmpstart, fdata.cFileName);
                tmpstart += len;
                FindClose(fhand);
            } else {
                FindClose(fhand);
                return;
            }
        } else return; 
    }
    strcpy(path, tmpbuf);
}


SEXP do_normalizepath(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, paths = CAR(args);
    int i, n = LENGTH(paths);
    char tmp[MAX_PATH], *tmp2;
    
    checkArity(op, args);
    if(!isString(paths))
       errorcall(call, _("'path' must be a character vector"));

    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	GetFullPathName(CHAR(STRING_ELT(paths, i)), MAX_PATH, tmp, &tmp2);
	longpathname(tmp);
	SET_STRING_ELT(ans, i, mkChar(tmp));
    }
    UNPROTECT(1);
    return ans;
}

SEXP do_shortpath(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, paths = CAR(args);
    int i, n = LENGTH(paths);
    char tmp[MAX_PATH];
    
    checkArity(op, args);
    if(!isString(paths))
       errorcall(call, _("'path' must be a character vector"));

    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	GetShortPathName(CHAR(STRING_ELT(paths, i)), tmp, MAX_PATH);
	/* documented to return paths using \, which the API call does
	   not necessarily do */
	R_fixbackslash(tmp);
	SET_STRING_ELT(ans, i, mkChar(tmp));
    }
    UNPROTECT(1);
    return ans;
}

SEXP do_chooseFiles(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, def, caption, filters;
    char *temp, *cfilters, list[65520],*p;
    char path[MAX_PATH], filename[MAX_PATH];
    int multi, filterindex, i, count, lfilters, pathlen;

    checkArity(op, args);
    def = CAR(args);
    caption = CADR(args);
    multi = asLogical(CADDR(args));
    filters = CADDDR(args);
    filterindex = asInteger(CAD4R(args));
    if(length(def) != 1 )
	errorcall(call, _("'default' must be a character string"));
    p = CHAR(STRING_ELT(def, 0));
    if(strlen(p) >= MAX_PATH) errorcall(call, _("'default' is overlong"));
    strcpy(path, R_ExpandFileName(p));
    R_fixbackslash(path);
/*    temp = Rf_strchr(path,'/');
    while (temp) {
	*temp = '\\';
	temp = strchr(temp,'/');
	}*/
    if(length(caption) != 1 )
	errorcall(call, _("'caption' must be a character string"));
    if(multi == NA_LOGICAL)
	errorcall(call, _("'multi' must be a logical value"));
    if(filterindex == NA_INTEGER)
	errorcall(call, _("'filterindex' must be an integer value"));
    lfilters = 1 + length(filters);
    for (i = 0; i < length(filters); i++)
	lfilters += strlen(CHAR(STRING_ELT(filters,i)));
    cfilters = R_alloc(lfilters, sizeof(char));
    temp = cfilters;
    for (i = 0; i < length(filters)/2; i++) {
	strcpy(temp,CHAR(STRING_ELT(filters,i)));
	temp += strlen(temp)+1;
	strcpy(temp,CHAR(STRING_ELT(filters,i+length(filters)/2)));
	temp += strlen(temp)+1;
    }
    *temp = 0;

    *list = '\0'; /* no initialization */
    askfilenames(CHAR(STRING_ELT(caption, 0)), path,
		 multi, cfilters, filterindex,
                 list, 65500, NULL);  /* list declared larger to protect against overwrites */

    if(!multi) {
	/* only one filename possible */
	count = 1;
    } else {
	count = countFilenames(list);
    }

    if (count < 2) PROTECT(ans = allocVector(STRSXP, count));
    else PROTECT(ans = allocVector(STRSXP, count-1));

    switch (count) {
    case 0: break;
    case 1: SET_STRING_ELT(ans, 0, mkChar(list));
	break;
    default:
	strncpy(path,list,sizeof(path));
	pathlen = strlen(path);
	if (path[pathlen-1] == '\\') path[--pathlen] = '\0';
    	temp = list;
    	for (i = 0; i < count-1; i++) {
	    temp += strlen(temp) + 1;
	    if (Rf_strchr(temp,':') || *temp == '\\' || *temp == '/')
		SET_STRING_ELT(ans, i, mkChar(temp));
	    else {
		strncpy(filename, path, sizeof(filename));
		filename[pathlen] = '\\';
		strncpy(filename+pathlen+1, temp, sizeof(filename)-pathlen-1);
		SET_STRING_ELT(ans, i, mkChar(filename));
	    }
	}
    }
    UNPROTECT(1);
    return ans;
}

SEXP do_chooseDir(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, def, caption;
    char *p, path[MAX_PATH];

    checkArity(op, args);
    def = CAR(args);
    caption = CADR(args);
    if(!isString(def) || length(def) != 1 )
	errorcall(call, _("'default' must be a character string"));
    p = CHAR(STRING_ELT(def, 0));
    if(strlen(p) >= MAX_PATH) errorcall(call, _("'default' is overlong"));
    strcpy(path, R_ExpandFileName(p));
    R_fixbackslash(path);
    if(!isString(caption) || length(caption) != 1 )
	errorcall(call, _("'caption' must be a character string"));
    p = askcdstring(CHAR(STRING_ELT(caption, 0)), path);
    
    PROTECT(ans = allocVector(STRSXP, 1));
    SET_STRING_ELT(ans, 0, p ? mkChar(p) : NA_STRING);
    UNPROTECT(1);
    return ans;
}

extern window RFrame; /* from rui.c */

SEXP getIdentification()
{
    char *res = "" /* -Wall */;

    switch(CharacterMode) {
    case RGui:
	if(RguiMDI & RW_MDI) res = "RGui"; else res = "R Console";
	break;
    case RTerm:
	res = "Rterm";
	break;
    default:
	/* do nothing */
	break; /* -Wall */
    }
    return mkString(res);
}

SEXP getWindowTitle()
{
    char buf[512], *res = "";

    switch(CharacterMode) {
    case RGui:
	if(RguiMDI & RW_MDI) res = GA_gettext(RFrame);
	else res = GA_gettext(RConsole);
	break;
    case RTerm:
    	GetConsoleTitle(buf, 512);
    	buf[511] = '\0';
	res = buf;
	break;
    default:
	/* do nothing */
	break;
    }
    return mkString(res);
}

SEXP setTitle(char *title)
{
    SEXP result = getWindowTitle();

    switch(CharacterMode) {
    case RGui:
	if(RguiMDI & RW_MDI) settext(RFrame, title);
	else settext(RConsole, title);
	break;
    case RTerm:
	SetConsoleTitle(title);
	break;
    default:
	/* do nothing */
	break; /* -Wall */
    }
    return result;
}

SEXP do_getIdentification(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return getIdentification();
}

SEXP do_setTitle(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP title = CAR(args);

    checkArity(op, args);
    if(!isString(title)  || LENGTH(title) != 1 || 
       STRING_ELT(title, 0) == NA_STRING)
	errorcall(call, _("'title' must be a character string"));
    return setTitle(CHAR(STRING_ELT(title, 0)));
}

SEXP do_getWindowTitle(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return getWindowTitle();
}

SEXP do_setStatusBar(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP text = CAR(args);

    checkArity(op, args);
    if(!isString(text)  || LENGTH(text) != 1 || 
       STRING_ELT(text, 0) == NA_STRING)
	errorcall(call, _("'text' must be a character string"));
    showstatusbar();
    setstatus(CHAR(STRING_ELT(text, 0)));
    return R_NilValue;
}


int getConsoleHandle(char *which)
{
    if (CharacterMode != RGui) return(0);
    else if (strcmp(which, "Console") == 0 && RConsole) 
	return getHandle(RConsole);
    else if (strcmp(which, "Frame") == 0 && RFrame) 
	return getHandle(RFrame);
    else if (strcmp(which, "Process") == 0) 
	return (int)GetCurrentProcess();
    else if (strcmp(which, "ProcessId") == 0) 
	return (int)GetCurrentProcessId();
    else return 0;
}

static int getDeviceHandle(int);

SEXP do_getWindowHandle(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int handle;
    SEXP which = CAR(args);

    checkArity(op, args);
    if(LENGTH(which) != 1)
	errorcall(call, _("'which' must be length 1"));
    if (isString(which)) handle = getConsoleHandle(CHAR(STRING_ELT(which,0)));
    else if (isInteger(which)) handle = getDeviceHandle(INTEGER(which)[0]);
    else handle = 0;

    return ScalarInteger(handle);
}

#include "devWindows.h"
#include "Startup.h"
extern UImode CharacterMode;

SEXP do_bringtotop(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int dev, stay;
    GEDevDesc *gdd;
    gadesc *xd;

    checkArity(op, args);
    dev = asInteger(CAR(args));
    stay = asInteger(CADR(args));

    if(dev == -1) { /* console */
	if(CharacterMode == RGui) BringToTop(RConsole, stay);
    } else {
	if(dev < 1 || dev > R_MaxDevices || dev == NA_INTEGER)
	    errorcall(call, _("invalid value for '%s'"), "which");
	gdd = (GEDevDesc *) GetDevice(dev - 1);
	if(!gdd) errorcall(call, _("invalid device"));
	xd = (gadesc *) gdd->dev->deviceSpecific;
	if(!xd) errorcall(call, _("invalid device"));
	if(stay && ismdi()) error(_("requires SDI mode"));
	BringToTop(xd->gawin, stay);
    }
    return R_NilValue;
}

static int getDeviceHandle(int dev)
{
    GEDevDesc *gdd;
    gadesc *xd;

    if (dev == -1) return(getHandle(RConsole));
    if (dev < 1 || dev > R_MaxDevices || dev == NA_INTEGER) return(0);
    gdd = (GEDevDesc *) GetDevice(dev - 1);
    if (!gdd) return(0);
    xd = (gadesc *) gdd->dev->deviceSpecific;
    if (!xd) return(0);
    return(getHandle(xd->gawin));
}

/* This assumes a menuname of the form $Graph<nn>Main, $Graph<nn>Popup, $Graph<nn>LocMain,
   or $Graph<nn>LocPopup where <nn> is the
   device number.  We've already checked the $Graph prefix. */

menu getGraphMenu(char* menuname)
{
    int devnum;
    GEDevDesc *gdd;
    gadesc *xd;

    menuname = menuname + 6;
    devnum = atoi(menuname);
    if(devnum < 1 || devnum > R_MaxDevices)
    	error(_("invalid graphical device number"));

    while (('0' <= *menuname) && (*menuname <= '9')) menuname++;

    gdd = (GEDevDesc*) GetDevice(devnum - 1);

    if(!gdd) error(_("invalid device"));

    xd = (gadesc *) gdd->dev->deviceSpecific;

    if(!xd || xd->kind != SCREEN) error(_("bad device"));

    if (strcmp(menuname, "Main") == 0) return(xd->mbar);
    else if (strcmp(menuname, "Popup") == 0) return(xd->grpopup);
    else return(NULL);
}

Rboolean winNewFrameConfirm(void)
{
    GEDevDesc *gdd = GEcurrentDevice();
    gadesc *xd = gdd->dev->deviceSpecific;
    return xd->newFrameConfirm();
}

/* 
   Replacement for MSVCRT's access.
   Coded looking at tcl's tclWinFile.c
*/

extern int GA_isNT;

int winAccess(const char *path, int mode)
{
    DWORD attr = GetFileAttributes(path);
    
    if(attr == 0xffffffff) return -1;
    if(mode == F_OK) return 0;

    if(mode & X_OK)
	if(!(attr & FILE_ATTRIBUTE_DIRECTORY)) { /* Directory, so OK */
	    /* Look at extension for executables */
	    char *p = strrchr(path, '.');
	    if(p == NULL || 
	       !((stricmp(p, ".exe") == 0) || (stricmp(p, ".com") == 0) ||
		 (stricmp(p, ".bat") == 0) || (stricmp(p, ".cmd") == 0)) )
		return -1;
	}
    if(GA_isNT) {
	/* Now look for file security info, which is NT only */
	SECURITY_DESCRIPTOR *sdPtr = NULL;
	unsigned long size = 0;
	GENERIC_MAPPING genMap;
	HANDLE hToken = NULL;
	DWORD desiredAccess = 0;
	DWORD grantedAccess = 0;
	BOOL accessYesNo = FALSE;
	PRIVILEGE_SET privSet;
	DWORD privSetSize = sizeof(PRIVILEGE_SET);
	int error;

	/* get size */
	GetFileSecurity(path, 
			OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION
			| DACL_SECURITY_INFORMATION, 0, 0, &size);
	error = GetLastError();
	if (error != ERROR_INSUFFICIENT_BUFFER) return -1;
	sdPtr = (SECURITY_DESCRIPTOR *) alloca(size);
	if(!GetFileSecurity(path, 
			    OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION
			    | DACL_SECURITY_INFORMATION, sdPtr, size, &size))
	    return -1;
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

	if ((mode & W_OK)
	    && !(attr & FILE_ATTRIBUTE_DIRECTORY)
	    && (attr & FILE_ATTRIBUTE_READONLY)) return -1;
    } else {
	if((mode & W_OK) && (attr & FILE_ATTRIBUTE_READONLY)) return -1;
    }
    return 0;
}



/* UTF-8 support ----------------------------------------------- */

/* This is currently only used for faking UTF-8 locale conversions */

#ifdef SUPPORT_UTF8
extern char *alloca(size_t);
int Rstrcoll(const char *s1, const char *s2)
{
    wchar_t *w1, *w2;
    w1 = (wchar_t *) alloca((strlen(s1)+1)*sizeof(wchar_t));
    w2 = (wchar_t *) alloca((strlen(s2)+1)*sizeof(wchar_t));
    R_CheckStack();
    Rmbstowcs(w1, s1, strlen(s1));
    Rmbstowcs(w2, s2, strlen(s2));
    return wcscoll(w1, w2);
}

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

/* based on pcre.c, but will only be used for UCS-2 */
static const int utf8_table1[] =
  { 0x7f, 0x7ff, 0xffff, 0x1fffff, 0x3ffffff, 0x7fffffff};
static const int utf8_table2[] = { 0, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc};

size_t Rwcrtomb(char *s, const wchar_t wc)
{
#ifdef FAKE_UTF8
    register int i, j;
    unsigned int cvalue = wc;
    char buf[10], *b;

    b = s ? s : buf;
    if(cvalue == 0) {*b = 0; return 0;}
    for (i = 0; i < sizeof(utf8_table1)/sizeof(int); i++)
	if (cvalue <= utf8_table1[i]) break;
    b += i;
    for (j = i; j > 0; j--) {
	*b-- = 0x80 | (cvalue & 0x3f);
	cvalue >>= 6;
    }
    *b = utf8_table2[i] | cvalue;
    return i + 1;
#else
    return wcrtomb(s, wc, NULL);
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
	    if(m < 0) error(_("invalid input in Rmbstowcs"));
	    if(m <= 0) break;
	    res++;
	    if(res >= n) break;
	}
    } else {
	for(p = s; ; p+=m) {
	    m  = Rmbrtowc(NULL, p);
	    if(m < 0) error(_("invalid input in Rmbstowcs"));
	    if(m <= 0) break;
	    res++;
	}
    }
    return res;
#else
    return mbstowcs(wc, s, n);
#endif
}

size_t Rwcstombs(char *s, const wchar_t *wc, size_t n)
{
#ifdef FAKE_UTF8
    int m, res=0;
    char *t;
    const wchar_t *p;
    if(s) {
	for(p = wc, t = s; ; p++) {
	    m  = Rwcrtomb(t, *p);
	    if(m <= 0) break;
	    res += m;
	    if(res >= n) break;
	    t += m;
	}
    } else {
	for(p = wc; ; p++) {
	    m  = Rwcrtomb(NULL, *p);
	    if(m <= 0) break;
	    res += m;
	}
    }
    return res;
#else
    return wcstombs(s, wc, n);
#endif
}
#endif
