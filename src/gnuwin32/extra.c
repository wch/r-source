/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file extra.c
 *  Copyright (C) 1998--2002  Guido Masarotto and Brian Ripley
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
#include <config.h>
#endif

#include <stdio.h>
#include "Defn.h"
#include "Fileio.h"
#include <io.h>
#include <direct.h>
#include <time.h>
#include <windows.h>
#include "graphapp/ga.h"
#include "rui.h"

char * R_tmpnam(const char * prefix)
{
    char tm[MAX_PATH], tmp1[MAX_PATH], *res;
    unsigned int n, done = 0;
    WIN32_FIND_DATA fd;
    HANDLE h;

    if(!prefix) prefix = "";	/* NULL */
    strcpy(tmp1, R_TempDir);
    for (n = 0; n < 100; n++) {
	/* try a random number at the end */
        sprintf(tm, "%s\\%s%d", tmp1, prefix, rand());
        if ((h = FindFirstFile(tm, &fd)) == INVALID_HANDLE_VALUE) {
	    done = 1;
	    break;
	}
        FindClose(h);
        tm[0] = '\0';
    }
    if(!done)
	error("cannot find unused tempfile name");
    res = (char *) malloc((strlen(tm)+1) * sizeof(char));
    strcpy(res, tm);
    return res;
}


SEXP do_dircreate(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  path, ans;
    char *p, dir[MAX_PATH];
    int res;

    checkArity(op, args);
    path = CAR(args);
    if (!isString(path) || length(path) != 1)
	errorcall(call, "invalid path argument");
    strcpy(dir, CHAR(STRING_ELT(path, 0)));
    for(p = dir; *p != '\0'; p++)
	if(*p == '/') *p = '\\';
    res = mkdir(dir);
    PROTECT(ans = allocVector(LGLSXP, 1));
    LOGICAL(ans)[0] = (res==0);
    UNPROTECT(1);
    return (ans);
}


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
    char *p, tmp[MAX_PATH], dir[MAX_PATH];
    WIN32_FIND_DATA find_data;
    HANDLE fh;
    struct stat sb;

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
		if(rmdir(dir)) failures++;
	    } else failures++; /* don't try to delete dirs */
	} else {/* Regular file (or several) */
	    strcpy(dir, tmp);
	    if ((p = strrchr(dir, '\\'))) *(++p) = '\0'; else *dir = '\0';
	    /* check for wildcard matches */
	    fh = FindFirstFile(tmp, &find_data);
	    if (fh != INVALID_HANDLE_VALUE) {
		failures += R_unlink_one(dir, find_data.cFileName, 0);
		while(FindNextFile(fh, &find_data))
		    failures += R_unlink_one(dir, find_data.cFileName, 0);
		FindClose(fh);
	    }
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
    if (!isString(fn) || nfiles < 1)
	errorcall(call, "invalid file name argument");
    recursive = asLogical(CADR(args));
    if (recursive == NA_LOGICAL)
	errorcall(call, "invalid recursive argument");
    for(i = 0; i < nfiles; i++)
	failures += R_unlink(CHAR(STRING_ELT(fn, i)), recursive);
    PROTECT(ans = allocVector(INTSXP, 1));
    if (!failures)
	INTEGER(ans)[0] = 0;
    else
	INTEGER(ans)[0] = 1;
    UNPROTECT(1);
    return (ans);
}

SEXP do_helpstart(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char *home, buf[MAX_PATH];
    FILE *ff;

    checkArity(op, args);
    home = getenv("R_HOME");
    if (home == NULL)
	error("R_HOME not set");
    sprintf(buf, "%s\\doc\\html\\rwin.html", home);
    ff = fopen(buf, "r");
    if (!ff) {
	sprintf(buf, "%s\\doc\\html\\rwin.htm", home);
	ff = fopen(buf, "r");
	if (!ff) {
	    sprintf(buf, "%s\\doc\\html\\rwin.htm[l] not found", home);
	    error(buf);
	}
    }
    fclose(ff);
    ShellExecute(NULL, "open", buf, NULL, home, SW_SHOW);
    return R_NilValue;
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
	errorcall(call, "invalid topic argument");
    item = CHAR(STRING_ELT(CAR(args), 0));
    type = asInteger(CADR(args));
    if (type == 1) {
	ff = fopen(item, "r");
	if (!ff) {
	    sprintf(buf, "%s not found", item);
	    error(buf);
	}
	fclose(ff);
	home = getenv("R_HOME");
	if (home == NULL)
	    error("R_HOME not set");
	ShellExecute(NULL, "open", item, NULL, home, SW_SHOW);
    } else if (type == 2) {
	if (!isString(CADDR(args)))
	    errorcall(call, "invalid hlpfile argument");
	hfile = CHAR(STRING_ELT(CADDR(args), 0));
	if (!WinHelp((HWND) 0, hfile, HELP_KEY, (DWORD) item))
	    warning("WinHelp call failed");
	else {
	    if (nhfiles >= 50)
		error("too many .hlp files opened");
	    hfiles[nhfiles] = malloc(strlen(hfile) * sizeof(char));
	    strcpy(hfiles[nhfiles++], hfile);
	}
    } else if (type == 3) {
	if (!isString(CADDR(args)))
	    warningcall(call, "invalid hlpfile argument");
	hfile = CHAR(STRING_ELT(CADDR(args), 0));
	if (!WinHelp((HWND) 0, hfile, HELP_QUIT, (DWORD) 0))
	    error("WinHelp call failed");
    } else
	warning("type not yet implemented");
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


/* defined in w32api 1.2, but not in 1.1 or earlier */
#ifndef VER_NT_WORKSTATION
#define VER_NT_WORKSTATION              0x0000001
#define VER_NT_DOMAIN_CONTROLLER        0x0000002
#define VER_NT_SERVER                   0x0000003

#define VER_SERVER_NT                       0x80000000
#define VER_WORKSTATION_NT                  0x40000000
#define VER_SUITE_SMALLBUSINESS             0x00000001
#define VER_SUITE_ENTERPRISE                0x00000002
#define VER_SUITE_BACKOFFICE                0x00000004
#define VER_SUITE_COMMUNICATIONS            0x00000008
#define VER_SUITE_TERMINAL                  0x00000010
#define VER_SUITE_SMALLBUSINESS_RESTRICTED  0x00000020
#define VER_SUITE_EMBEDDEDNT                0x00000040
#define VER_SUITE_DATACENTER                0x00000080
#define VER_SUITE_SINGLEUSERTS              0x00000100
#define VER_SUITE_PERSONAL                  0x00000200

typedef struct _OSVERSIONINFOEX {
  DWORD dwOSVersionInfoSize;
  DWORD dwMajorVersion;
  DWORD dwMinorVersion;
  DWORD dwBuildNumber;
  DWORD dwPlatformId;
  TCHAR szCSDVersion[ 128 ];
  WORD wServicePackMajor;
  WORD wServicePackMinor;
  WORD wSuiteMask;
  BYTE wProductType;
  BYTE wReserved;
} OSVERSIONINFOEX;
#endif
/* next is from Nov 2001 Platform SDK */
#ifndef VER_SUITE_BLADE
#define VER_SUITE_BLADE                     0x00000400
#endif

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
	    if(osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0)
		desc = "2000";
	    if(osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1)
		desc = "XP";
            if ( osvi.wProductType == VER_NT_WORKSTATION ) {
               if( osvi.wSuiteMask & VER_SUITE_PERSONAL )
                  type = "Home Edition";
               else
                  type = "Professional";
            } else if ( osvi.wProductType == VER_NT_SERVER )
            {
               if ( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1 )
                  desc = ".NET";
               if( osvi.wSuiteMask & VER_SUITE_DATACENTER )
                  type = "DataCenter Server";
               else if( osvi.wSuiteMask & VER_SUITE_ENTERPRISE )
                  type = "Advanced Server";
               else if ( osvi.wSuiteMask == VER_SUITE_BLADE )
                  type = "Web Server";
               else
		   type = "Server";
            }

	    sprintf(ver,
		    "Windows %s %s (build %d) Service Pack %d.%d",
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

    PROTECT(ans = allocVector(STRSXP, 1));
    SET_STRING_ELT(ans, 0, mkChar(ver));
    UNPROTECT(1);
    return (ans);
}

void internal_shellexec(char * file)
{
    char *home, buf[MAX_PATH];

    home = getenv("R_HOME");
    if (home == NULL)
	error("R_HOME not set");
    strcpy(buf, file);
    ShellExecute(NULL, "open", buf, NULL, home, SW_SHOW);
}

SEXP do_shellexec(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP file;

    checkArity(op, args);
    file = CAR(args);
    if (!isString(file) || length(file) != 1)
	errorcall(call, "invalid file argument");
    internal_shellexec(CHAR(STRING_ELT(file, 0)));
    return R_NilValue;
}

int check_doc_file(char * file)
{
    char *home, path[MAX_PATH];
    struct stat sb;

    home = getenv("R_HOME");
    if (home == NULL)
	error("R_HOME not set");
    strcpy(path, home);
    strcat(path, "/");
    strcat(path, file);
    return stat(path, &sb) == 0;
}

SEXP do_windialog(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP message, ans;
    char * type;
    int res=YES;

    checkArity(op, args);
    type = CHAR(STRING_ELT(CAR(args), 0));
    message = CADR(args);
    if(!isString(message) || length(message) != 1)
	error("invalid `message' argument");
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
	errorcall(call, "unknown type");
    ans = allocVector(INTSXP, 1);
    INTEGER(ans)[0] = res;
    return (ans);
}

SEXP do_windialogstring(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  message, def, ans;
    char *string;

    checkArity(op, args);
    message = CAR(args);
    if(!isString(message) || length(message) != 1)
	error("invalid `message' argument");
    def = CADR(args);
    if(!isString(def) || length(def) != 1)
	error("invalid `default' argument");
    string = askstring(CHAR(STRING_ELT(message, 0)), CHAR(STRING_ELT(def, 0)));
    if (string) {
	ans = allocVector(STRSXP, 1);
	SET_STRING_ELT(ans, 0, mkChar(string));
	return (ans);
    } else
	return (R_NilValue);
}

#include "Startup.h"
extern UImode CharacterMode;
static char msgbuf[256];

SEXP do_winmenuadd(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP smenu, sitem;
    int res;
    char errmsg[50];

    checkArity(op, args);
    if (CharacterMode != RGui)
	errorcall(call, "Menu functions can only be used in the GUI");
    smenu = CAR(args);
    if(!isString(smenu) || length(smenu) != 1)
	error("invalid `menuname' argument");
    sitem = CADR(args);
    if (isNull(sitem)) { /* add a menu */
	res = winaddmenu (CHAR(STRING_ELT(smenu, 0)), errmsg);
	if (res > 0) {
	    sprintf(msgbuf, "unable to add menu (%s)", errmsg);
	    errorcall(call, msgbuf);
	}

    } else { /* add an item */
	if(!isString(sitem) || length(sitem) != 1)
	    error("invalid `itemname' argument");
	res = winaddmenuitem (CHAR(STRING_ELT(sitem, 0)),
			      CHAR(STRING_ELT(smenu, 0)),
			      CHAR(STRING_ELT(CADDR(args), 0)),
			      errmsg);
	if (res > 0) {
	    sprintf(msgbuf, "unable to add menu item (%s)", errmsg);
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
	errorcall(call, "Menu functions can only be used in the GUI");
    smenu = CAR(args);
    if(!isString(smenu) || length(smenu) != 1)
	error("invalid `menuname' argument");
    sitem = CADR(args);
    if (isNull(sitem)) { /* delete a menu */
	res = windelmenu (CHAR(STRING_ELT(smenu, 0)), errmsg);
	if (res > 0)
	    errorcall(call, "menu does not exist");
    } else { /* delete an item */
	if(!isString(sitem) || length(sitem) != 1)
	    error("invalid `itemname' argument");
	res = windelmenuitem (CHAR(STRING_ELT(sitem, 0)),
			      CHAR(STRING_ELT(smenu, 0)), errmsg);
	if (res > 0) {
	    sprintf(msgbuf, "unable to delete menu item (%s)", errmsg);
	    errorcall(call, msgbuf);
	}
    }
    return (R_NilValue);
}


void Rwin_fpset()
{
    _fpreset();
    _controlfp(_MCW_EM, _MCW_EM);
    _controlfp(_PC_64, _MCW_PC);
}

#include "getline/getline.h"  /* for gl_load/savehistory */
SEXP do_savehistory(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile;

    checkArity(op, args);
    sfile = CAR(args);
    if (!isString(sfile) || LENGTH(sfile) < 1)
	errorcall(call, "invalid file argument");
    if (CharacterMode == RGui || (R_Interactive && CharacterMode == RTerm))
	gl_savehistory(CHAR(STRING_ELT(sfile, 0)));
    else
	errorcall(call, "savehistory can only be used in Rgui and Rterm");
    return R_NilValue;
}

SEXP do_loadhistory(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile;

    checkArity(op, args);
    sfile = CAR(args);
    if (!isString(sfile) || LENGTH(sfile) < 1)
	errorcall(call, "invalid file argument");
    if (CharacterMode == RGui || (R_Interactive && CharacterMode == RTerm))
	gl_loadhistory(CHAR(STRING_ELT(sfile, 0)));
    else
	errorcall(call, "savehistory can only be used in Rgui and Rterm");
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
	errorcall(call, "invalid time value");
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
  int arena;    /* total space allocated from system */
  int ordblks;  /* number of non-inuse chunks */
  int smblks;   /* unused -- always zero */
  int hblks;    /* number of mmapped regions */
  int hblkhd;   /* total space in mmapped regions */
  int usmblks;  /* unused -- always zero */
  int fsmblks;  /* unused -- always zero */
  int uordblks; /* total allocated space */
  int fordblks; /* total non-inuse space */
  int keepcost; /* top-most, releasable (via malloc_trim) space */
};
extern unsigned long max_total_mem;
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
	PROTECT(ans = allocVector(INTSXP, 1));
#ifdef LEA_MALLOC
	if(maxmem == NA_LOGICAL)
	    INTEGER(ans)[0] = R_max_memory;
	else if(maxmem)
	    INTEGER(ans)[0] = max_total_mem;
	else
	    INTEGER(ans)[0] = mallinfo().uordblks;
#else
	INTEGER(ans)[0] = NA_INTEGER;
#endif
	UNPROTECT(1);
	return ans;
    } else if(isReal(CAR(args))) {
	unsigned int newmax;
	double mem = asReal(CAR(args));
	if (!R_FINITE(mem))
	    errorcall(call, "incorrect argument");
	newmax = mem * 1048576.0;
	if (newmax < R_max_memory)
	    errorcall(call, "cannot decrease memory limit");
	R_max_memory = newmax;
    } else
	errorcall(call, "incorrect argument");
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
	errorcall(call, "invalid `path' argument");
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
    strncpy(selected, gettext(f_list), 100);
    done = 1;
}

static void key1(control c, int ch)
{
    if(ch == '\n') finish(NULL);
    if(ch == ESC)  cancel(NULL);
}


SEXP do_selectlist(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP list, preselect, ans = R_NilValue;
    char **clist, *cps;
    int i, j = -1, n, mw = 0, multiple, nsel = 0;
    int xmax = 550, ymax  = 400, ylist;

    checkArity(op, args);
    list = CAR(args);
    if(!isString(list)) error("invalid `list' argument");
    preselect = CADR(args);
    if(!isNull(preselect) &&
       (!isString(preselect) || length(preselect) != 1))
	error("invalid `preselect' argument");
    if(isNull(preselect)) cps = CHAR(STRING_ELT(preselect, 0));
    else cps = "";
    multiple = asLogical(CADDR(args));
    if(multiple == NA_LOGICAL) multiple = 0;

    n = LENGTH(list);
    clist = (char **) R_alloc(n + 1, sizeof(char *));
    for(i = 0; i < n; i++) {
	clist[i] = CHAR(STRING_ELT(list, i));
	if(strcmp(clist[i], cps) == 0) j = i;
	mw = max(mw, strlen(clist[i]));
    }
    clist[n] = NULL;
    mw = min(mw, 25);
    xmax = max(170, 8*mw+60);
    ylist = min(20*n, 300);
    ymax = ylist + 60;
    wselect = newwindow(multiple ? "Select" : "Select one",
			rect(0, 0, xmax, ymax),
			Titlebar | Centered | Modal);
    setbackground(wselect, dialog_bg());
    if(multiple)
	f_list = newmultilist(clist, rect(10, 10, 35+8*mw, ylist), NULL);
    else
	f_list = newlistbox(clist, rect(10, 10, 35+8*mw, ylist), NULL);
    setlistitem(f_list, j);
    bFinish = newbutton("OK", rect(xmax-160, ymax-40, 70, 25), finish);
    bCancel = newbutton("Cancel", rect(xmax-80, ymax-40, 70, 25), cancel);
    setkeydown(wselect, key1);
    show(wselect);
    done = 0;
    while(!done) R_ProcessEvents();

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

void InitTempDir()
{
    char *tmp, tm[MAX_PATH], tmp1[MAX_PATH], *p;
    unsigned int n;
    int hasspace = 0, len, done = 0, res;
    WIN32_FIND_DATA fd;
    HANDLE h;

    tmp = getenv("TMP");
    if (!tmp) tmp = getenv("TEMP");
    if (!tmp) tmp = getenv("R_USER"); /* this one will succeed */
    /* make sure no spaces in path */
    for (p = tmp; *p; p++)
	if (isspace(*p)) { hasspace = 1; break; }
    if (hasspace)
	GetShortPathName(tmp, tmp1, MAX_PATH);
    else
	strcpy(tmp1, tmp);
    /* now try a random addition */
    srand( (unsigned)time( NULL ) );
    for (n = 0; n < 100; n++) {
	/* try a random number at the end */
        sprintf(tm, "%s\\%s%d", tmp1, "Rtmp", rand());
        if ((h = FindFirstFile(tm, &fd)) == INVALID_HANDLE_VALUE) {
	    done = 1;
	    break;
	}
        FindClose(h);
        tm[0] = '\0';
    }
    if(!done)
	R_Suicide("cannot find unused tempdir name");
    /* Now try to create it */
    res = mkdir(tm);
    if(res) R_Suicide("Can't mkdir R_TempDir");
    len = strlen(tm);
    p = (char *) malloc(len+1);
    if(!p) R_Suicide("Can't allocate R_TempDir");
    else {
	R_TempDir = p;
	strcpy(R_TempDir, tm);
    }
}

void CleanTempDir()
{
    R_unlink(R_TempDir, 1);
}

SEXP do_readClipboard(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = allocVector(STRSXP, 0);
    HGLOBAL hglb;
    char *pc;

    checkArity(op, args);
    if(clipboardhastext() &&
       OpenClipboard(NULL) &&
       (hglb = GetClipboardData(CF_TEXT)) &&
       (pc = (char *)GlobalLock(hglb))) {
	    PROTECT(ans = allocVector(STRSXP, 1));
	    SET_STRING_ELT(ans, 0, mkChar(pc));
	    GlobalUnlock(hglb);
	    CloseClipboard();
	    UNPROTECT(1);
    }
    return ans;
}

SEXP do_writeClipboard(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, text;
    int i, n;
    HGLOBAL hglb;
    char *s, *p;
    Rboolean success = FALSE;

    checkArity(op, args);
    text = CAR(args);
    if(!isString(text))
	errorcall(call, "argument must be a character vector");
    n = length(text);
    if(n > 0) {
	int len = 1;
	for(i = 0; i < n; i++) len += strlen(CHAR(STRING_ELT(text, i))) + 2;
	if ( (hglb = GlobalAlloc(GHND, len)) &&
	     (s = (char *)GlobalLock(hglb)) ) {
	    for(i = 0; i < n; i++) {
		p = CHAR(STRING_ELT(text, i));
		while(*p) *s++ = *p++;
		*s++ = '\r'; *s++ = '\n';
	    }
	    *s = '\0';
	    GlobalUnlock(hglb);
	    if (!OpenClipboard(NULL) || !EmptyClipboard()) {
		warningcall(call, "Unable to open the clipboard");
		GlobalFree(hglb);
	    } else {
		success = SetClipboardData(CF_TEXT, hglb) != 0;
		if(!success) {
		    warningcall(call, "Unable to write to the clipboard");
		    GlobalFree(hglb);
		}
		CloseClipboard();
	    }
	}
    }
    PROTECT(ans = allocVector(LGLSXP, 1));
    LOGICAL(ans)[0] = success;
    UNPROTECT(1);
    return ans;
}
