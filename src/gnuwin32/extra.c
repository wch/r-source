/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file extra.c
 *  Copyright (C) 1998--2003  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004	      The R Foundation
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

#include "win-nls.h"

#include <stdio.h>
#include "Defn.h"
#include "Fileio.h"
#include <direct.h>
#include <time.h>
#define _WIN32_WINNT 0x0500 /* for GetLongPathName */
#include <windows.h>
#include "graphapp/ga.h"
#include "rui.h"


/* RAND_MAX is 0x7FFF on Windows */
char * R_tmpnam(const char * prefix, const char * tempdir)
{
    char tm[MAX_PATH], tmp1[MAX_PATH], *res;
    unsigned int n, done = 0;
    WIN32_FIND_DATA fd;
    HANDLE h;

    if(!prefix) prefix = "";	/* NULL */
    if(strlen(tempdir) >= MAX_PATH) error(_("invalid 'tempdir' in R_tmpnam"));
    strcpy(tmp1, tempdir);
    for (n = 0; n < 100; n++) {
	/* try a random number at the end */
        sprintf(tm, "%s\\%s%x%x", tmp1, prefix, rand(), rand());
        if ((h = FindFirstFile(tm, &fd)) == INVALID_HANDLE_VALUE) {
	    done = 1;
	    break;
	}
        FindClose(h);
        tm[0] = '\0';
    }
    if(!done)
	error(_("cannot find unused tempfile name"));
    res = (char *) malloc((strlen(tm)+1) * sizeof(char));
    strcpy(res, tm);
    return res;
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
    char *p, tmp[MAX_PATH], dir[MAX_PATH+2];
    WIN32_FIND_DATA find_data;
    HANDLE fh;
    struct stat sb;

    if(strlen(names) >= MAX_PATH) error(_("invalid 'names' in R_unlink"));
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
	    if ((p = Rf_strrchr(dir, '\\'))) *(++p) = '\0'; else *dir = '\0';
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

SEXP do_helpstart(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char *home, buf[MAX_PATH];
    FILE *ff;

    checkArity(op, args);
    home = getenv("R_HOME");
    if (home == NULL)
	error(_("R_HOME not set"));
    sprintf(buf, "%s\\doc\\html\\rwin.html", home);
    ff = fopen(buf, "r");
    if (!ff) {
	sprintf(buf, "%s\\doc\\html\\rwin.htm", home);
	ff = fopen(buf, "r");
	if (!ff) {
	    sprintf(buf, _("%s\\doc\\html\\rwin.htm[l] not found"), home);
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
	errorcall(call, _("invalid '%s' argument"), "topic");
    item = CHAR(STRING_ELT(CAR(args), 0));
    type = asInteger(CADR(args));
    if (type == 1) {
	ff = fopen(item, "r");
	if (!ff) {
	    sprintf(buf, _("%s not found"), item);
	    error(buf);
	}
	fclose(ff);
	home = getenv("R_HOME");
	if (home == NULL)
	    error(_("R_HOME not set"));
	ShellExecute(NULL, "open", item, NULL, home, SW_SHOW);
    } else if (type == 2) {
	if (!isString(CADDR(args)))
	    errorcall(call, _("invalid '%s' argument"), "hlpfile");
	hfile = CHAR(STRING_ELT(CADDR(args), 0));
	if (!WinHelp((HWND) 0, hfile, HELP_KEY, (DWORD) item))
	    warning(_("WinHelp call failed"));
	else {
	    if (nhfiles >= 50)
		error(_("too many .hlp files opened"));
	    hfiles[nhfiles] = malloc(strlen(hfile) * sizeof(char));
	    strcpy(hfiles[nhfiles++], hfile);
	}
    } else if (type == 3) {
	if (!isString(CADDR(args)))
	    warningcall(call, _("invalid '%s' argument"), "hlpfile");
	hfile = CHAR(STRING_ELT(CADDR(args), 0));
	if (!WinHelp((HWND) 0, hfile, HELP_QUIT, (DWORD) 0))
	    error(_("WinHelp call failed"));
    } else
	warning(_("type not yet implemented"));
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
    char *home;

    home = getenv("R_HOME");
    if (home == NULL)
	error(_("R_HOME not set"));
    ShellExecute(NULL, "open", file, NULL, home, SW_SHOW);
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
    SEXP message, ans;
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
    if(!isString(message) || length(message) != 1 || 
       strlen(CHAR(STRING_ELT(message, 0))) > 255)
	error(_("invalid '%s' argument"), "message");
    def = CADR(args);
    if(!isString(def) || length(def) != 1)
	error(_("invalid '%s' argument"), "default");
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
	errorcall(call, _("invalid '%s' argument"), "file");
    if (CharacterMode == RGui || (R_Interactive && CharacterMode == RTerm)) {
	R_setupHistory(); /* re-read the history size */
	gl_savehistory(CHAR(STRING_ELT(sfile, 0)), R_HistorySize);
    } else
	errorcall(call, _("savehistory can only be used in Rgui and Rterm"));
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
	errorcall(call, _("loadhistory can only be used in Rgui and Rterm"));
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
    strncpy(selected, gettext(f_list), 100);
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
			(multiple ? "Select one or more" : "Select one"),
			rect(0, 0, xmax, ymax),
			Titlebar | Centered | Modal);
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

extern char * mkdtemp (char *template);

void InitTempDir()
{
    char *tmp, *tm, tmp1[MAX_PATH], tmp2[MAX_PATH+11], *p;
    int hasspace = 0, len;

    tmp = getenv("TMP");
    if(access(tmp, W_OK) != 0) tmp = NULL;
    if (!tmp) tmp = getenv("TEMP");
    if(access(tmp, W_OK) != 0) tmp = NULL;
    if (!tmp) tmp = getenv("R_USER"); /* this one will succeed */
    /* make sure no spaces in path */
    for (p = tmp; *p; p++)
	if (isspace(*p)) { hasspace = 1; break; }
    if (hasspace)
	GetShortPathName(tmp, tmp1, MAX_PATH);
    else
	strcpy(tmp1, tmp); /* length must be valid as access has been checked */
    sprintf(tmp2, "%s/RtmpXXXXXX", tmp1);
    tm = mkdtemp(tmp2);
    if(!tm) R_Suicide(_("cannot mkdir R_TempDir"));

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
    if(R_TempDir) R_unlink(R_TempDir, 1);
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
	errorcall(call, _("argument must be a character vector"));
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
    PROTECT(ans = allocVector(LGLSXP, 1));
    LOGICAL(ans)[0] = success;
    UNPROTECT(1);
    return ans;
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
       errorcall(call, "'path' must be a character vector");

    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	GetFullPathName(CHAR(STRING_ELT(paths, i)), MAX_PATH, tmp, &tmp2);
	longpathname(tmp);
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
                 list, 65500);  /* list declared larger to protect against overwrites */
    Rwin_fpset();

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

extern window RFrame; /* from rui.c */

SEXP getIdentification()
{
    SEXP result;

    PROTECT(result = allocVector(STRSXP, 1));
    switch(CharacterMode) {
    case RGui:
	if(RguiMDI & RW_MDI) SET_STRING_ELT(result, 0, mkChar("RGui"));
	else SET_STRING_ELT(result, 0, mkChar("R Console"));
	break;
    case RTerm:
	SET_STRING_ELT(result, 0, mkChar("Rterm"));
    default:
	/* do nothing */
	break; /* -Wall */
    }
    UNPROTECT(1);
    return result;
}

SEXP getWindowTitle()
{
    SEXP result;
    char buf[512];

    PROTECT(result = allocVector(STRSXP, 1));
    switch(CharacterMode) {
    case RGui:
	if(RguiMDI & RW_MDI) SET_STRING_ELT(result, 0, mkChar(gettext(RFrame)));
	else SET_STRING_ELT(result, 0, mkChar(gettext(RConsole)));
	break;
    case RTerm:
    	GetConsoleTitle(buf, 512);
    	buf[511] = '\0';
	SET_STRING_ELT(result, 0, mkChar(buf));
    default:
	/* do nothing */
	break; /* -Wall */
    }
    UNPROTECT(1);
    return result;
}

SEXP setTitle(char *title)
{
    SEXP result;

    PROTECT(result = getWindowTitle());

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
    UNPROTECT(1);
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
    if(!isString(title)  || LENGTH(title) != 1)
	errorcall(call, _("'title' must be a character string"));
    return setTitle(CHAR(STRING_ELT(title, 0)));
}

SEXP do_getWindowTitle(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return getWindowTitle();
}

int getConsoleHandle(char *which)
{
    if (CharacterMode != RGui) return(0);
    else if (strcmp(which, "Console") == 0 && RConsole) return(getHandle(RConsole));
    else if (strcmp(which, "Frame") == 0 && RFrame) return(getHandle(RFrame));
    else if (strcmp(which, "Process") == 0) return((int)GetCurrentProcess());
    else if (strcmp(which, "ProcessId") == 0) return((int)GetCurrentProcessId());
    else return(0);
}

static int getDeviceHandle(int);

SEXP do_getWindowHandle(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP result;
    int handle;
    SEXP which = CAR(args);

    result = R_NilValue; /* to avoid warnings */

    checkArity(op, args);
    if(LENGTH(which) != 1)
	errorcall(call, _("'which' must be length 1"));
    if (isString(which)) handle = getConsoleHandle(CHAR(STRING_ELT(which,0)));
    else if (isInteger(which)) handle = getDeviceHandle(INTEGER(which)[0]);
    else handle = 0;

    PROTECT(result = allocVector(INTSXP, 1));
    INTEGER(result)[0] = handle;
    UNPROTECT(1);

    return result;
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
	    errorcall(call, _("invalid value of 'which'"));
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

/* wc[s]width -------------------------------------------------- */

/* From http://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c */

struct interval {
    int first;
    int last;
};

static int bisearch(wchar_t ucs, const struct interval *table, int max) 
{
    int min = 0;
    int mid;

    if (ucs < table[0].first || ucs > table[max].last) return 0;
    while (max >= min) {
	mid = (min + max) / 2;
	if (ucs > table[mid].last) min = mid + 1;
	else if (ucs < table[mid].first) max = mid - 1;
	else return 1;
    }
    return 0;
}


/* The following two functions define the column width of an ISO 10646
 * character as follows:
 *
 *    - The null character (U+0000) has a column width of 0.
 *
 *    - Other C0/C1 control characters and DEL will lead to a return
 *      value of -1.
 *
 *    - Non-spacing and enclosing combining characters (general
 *      category code Mn or Me in the Unicode database) have a
 *      column width of 0.
 *
 *    - SOFT HYPHEN (U+00AD) has a column width of 1.
 *
 *    - Other format characters (general category code Cf in the Unicode
 *      database) and ZERO WIDTH SPACE (U+200B) have a column width of 0.
 *
 *    - Hangul Jamo medial vowels and final consonants (U+1160-U+11FF)
 *      have a column width of 0.
 *
 *    - Spacing characters in the East Asian Wide (W) or East Asian
 *      Full-width (F) category as defined in Unicode Technical
 *      Report #11 have a column width of 2.
 *
 *    - All remaining characters (including all printable
 *      ISO 8859-1 and WGL4 characters, Unicode control characters,
 *      etc.) have a column width of 1.
 *
 * This implementation assumes that wchar_t characters are encoded
 * in ISO 10646.
 */

int wcwidth(wchar_t ucs)
{
    /* sorted list of non-overlapping intervals of non-spacing characters */
    /* generated by "uniset +cat=Me +cat=Mn +cat=Cf -00AD +1160-11FF +200B c" 
     */
    static const struct interval combining[] = {
	{ 0x0300, 0x0357 }, { 0x035D, 0x036F }, { 0x0483, 0x0486 },
	{ 0x0488, 0x0489 }, { 0x0591, 0x05A1 }, { 0x05A3, 0x05B9 },
	{ 0x05BB, 0x05BD }, { 0x05BF, 0x05BF }, { 0x05C1, 0x05C2 },
	{ 0x05C4, 0x05C4 }, { 0x0600, 0x0603 }, { 0x0610, 0x0615 },
	{ 0x064B, 0x0658 }, { 0x0670, 0x0670 }, { 0x06D6, 0x06E4 },
	{ 0x06E7, 0x06E8 }, { 0x06EA, 0x06ED }, { 0x070F, 0x070F },
	{ 0x0711, 0x0711 }, { 0x0730, 0x074A }, { 0x07A6, 0x07B0 },
	{ 0x0901, 0x0902 }, { 0x093C, 0x093C }, { 0x0941, 0x0948 },
	{ 0x094D, 0x094D }, { 0x0951, 0x0954 }, { 0x0962, 0x0963 },
	{ 0x0981, 0x0981 }, { 0x09BC, 0x09BC }, { 0x09C1, 0x09C4 },
	{ 0x09CD, 0x09CD }, { 0x09E2, 0x09E3 }, { 0x0A01, 0x0A02 },
	{ 0x0A3C, 0x0A3C }, { 0x0A41, 0x0A42 }, { 0x0A47, 0x0A48 },
	{ 0x0A4B, 0x0A4D }, { 0x0A70, 0x0A71 }, { 0x0A81, 0x0A82 },
	{ 0x0ABC, 0x0ABC }, { 0x0AC1, 0x0AC5 }, { 0x0AC7, 0x0AC8 },
	{ 0x0ACD, 0x0ACD }, { 0x0AE2, 0x0AE3 }, { 0x0B01, 0x0B01 },
	{ 0x0B3C, 0x0B3C }, { 0x0B3F, 0x0B3F }, { 0x0B41, 0x0B43 },
	{ 0x0B4D, 0x0B4D }, { 0x0B56, 0x0B56 }, { 0x0B82, 0x0B82 },
	{ 0x0BC0, 0x0BC0 }, { 0x0BCD, 0x0BCD }, { 0x0C3E, 0x0C40 },
	{ 0x0C46, 0x0C48 }, { 0x0C4A, 0x0C4D }, { 0x0C55, 0x0C56 },
	{ 0x0CBC, 0x0CBC }, { 0x0CBF, 0x0CBF }, { 0x0CC6, 0x0CC6 },
	{ 0x0CCC, 0x0CCD }, { 0x0D41, 0x0D43 }, { 0x0D4D, 0x0D4D },
	{ 0x0DCA, 0x0DCA }, { 0x0DD2, 0x0DD4 }, { 0x0DD6, 0x0DD6 },
	{ 0x0E31, 0x0E31 }, { 0x0E34, 0x0E3A }, { 0x0E47, 0x0E4E },
	{ 0x0EB1, 0x0EB1 }, { 0x0EB4, 0x0EB9 }, { 0x0EBB, 0x0EBC },
	{ 0x0EC8, 0x0ECD }, { 0x0F18, 0x0F19 }, { 0x0F35, 0x0F35 },
	{ 0x0F37, 0x0F37 }, { 0x0F39, 0x0F39 }, { 0x0F71, 0x0F7E },
	{ 0x0F80, 0x0F84 }, { 0x0F86, 0x0F87 }, { 0x0F90, 0x0F97 },
	{ 0x0F99, 0x0FBC }, { 0x0FC6, 0x0FC6 }, { 0x102D, 0x1030 },
	{ 0x1032, 0x1032 }, { 0x1036, 0x1037 }, { 0x1039, 0x1039 },
	{ 0x1058, 0x1059 }, { 0x1160, 0x11FF }, { 0x1712, 0x1714 },
	{ 0x1732, 0x1734 }, { 0x1752, 0x1753 }, { 0x1772, 0x1773 },
	{ 0x17B4, 0x17B5 }, { 0x17B7, 0x17BD }, { 0x17C6, 0x17C6 },
	{ 0x17C9, 0x17D3 }, { 0x17DD, 0x17DD }, { 0x180B, 0x180D },
	{ 0x18A9, 0x18A9 }, { 0x1920, 0x1922 }, { 0x1927, 0x1928 },
	{ 0x1932, 0x1932 }, { 0x1939, 0x193B }, { 0x200B, 0x200F },
	{ 0x202A, 0x202E }, { 0x2060, 0x2063 }, { 0x206A, 0x206F },
	{ 0x20D0, 0x20EA }, { 0x302A, 0x302F }, { 0x3099, 0x309A },
	{ 0xFB1E, 0xFB1E }, { 0xFE00, 0xFE0F }, { 0xFE20, 0xFE23 },
	{ 0xFEFF, 0xFEFF }, { 0xFFF9, 0xFFFB }, { 0x1D167, 0x1D169 },
	{ 0x1D173, 0x1D182 }, { 0x1D185, 0x1D18B }, { 0x1D1AA, 0x1D1AD },
	{ 0xE0001, 0xE0001 }, { 0xE0020, 0xE007F }, { 0xE0100, 0xE01EF }
    };

    /* test for 8-bit control characters */
    if (ucs == 0) return 0;
    if (ucs < 32 || (ucs >= 0x7f && ucs < 0xa0)) return -1;

    /* binary search in table of non-spacing characters */
    if (bisearch(ucs, combining, sizeof(combining)/sizeof(struct interval) - 1))
	return 0;

    return 1 +
	(ucs >= 0x1100 &&
	 (ucs <= 0x115f || ucs == 0x2329 || ucs == 0x232a ||
	  (ucs >= 0x2e80 && ucs <= 0xa4cf && ucs != 0x303f) || 
	  (ucs >= 0xac00 && ucs <= 0xd7a3) ||
	  (ucs >= 0xf900 && ucs <= 0xfaff) ||
	  (ucs >= 0xfe30 && ucs <= 0xfe6f) ||
	  (ucs >= 0xff00 && ucs <= 0xff60) ||
	  (ucs >= 0xffe0 && ucs <= 0xffe6)));
}

int wcswidth(wchar_t *s)
{
    int i, w0 = 0, len = wcslen(s);
    for(i = 0; i < len; i++) w0 += wcwidth(s[i]);
    return w0;
}


/* UTF-8 support ----------------------------------------------- */

/* This is currently only used for faking UTF-8 locale conversions */

#ifdef SUPPORT_UTF8
extern char *alloca(size_t);
int Rstrcoll(const char *s1, const char *s2)
{
    wchar_t *w1, *w2;
    w1 = (wchar_t *) alloca((strlen(s1)+1)*sizeof(wchar_t));
    Rmbstowcs(w1, s1, strlen(s1));
    w2 = (wchar_t *) alloca((strlen(s2)+1)*sizeof(wchar_t));
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
