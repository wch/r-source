/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file extra.c
 *  Copyright (C) 1998--2003  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004	      The R Foundation
 *  Copyright (C) 2005--2010  The R Core Team
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
 *  http://www.r-project.org/Licenses/
 */


/* extra commands for R */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "win-nls.h"


#include <stdio.h>
#include <time.h>
#include "Defn.h"
#include "Fileio.h"
#include <direct.h>
#include "graphapp/ga.h"
/* Mingw-w64 defines this to be 0x0502 */
#ifndef _WIN32_WINNT
# define _WIN32_WINNT 0x0502 /* for GetLongPathName, KEY_WOW64_64KEY */
#endif
#include <windows.h>
#include "rui.h"
#undef ERROR
#include <R_ext/RS.h> /* for Calloc */

SEXP do_flushconsole(SEXP call, SEXP op, SEXP args, SEXP env)
{
    R_FlushConsole();
    return R_NilValue;
}

#include <winbase.h>

typedef void (WINAPI *PGNSI)(LPSYSTEM_INFO);

SEXP do_winver(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char ver[256];
    OSVERSIONINFOEX osvi;

    checkArity(op, args);
    osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
    if(!GetVersionEx((OSVERSIONINFO *)&osvi))
	error(_("unsupported version of Windows"));

    /* see http://msdn2.microsoft.com/en-us/library/ms724429.aspx
       for ways to get more info.
       Pre-NT versions are all 4.x, so no need to separate test.
    */
    if(osvi.dwMajorVersion >= 5) {
	char *desc = "", *type="";
	PGNSI pGNSI;
	SYSTEM_INFO si;
	if(osvi.dwMajorVersion > 6) { /* future proof */
	    sprintf(ver, "Windows %d.%d (build %d)",
		    (int) osvi.dwMajorVersion, (int) osvi.dwMinorVersion,
		    LOWORD(osvi.dwBuildNumber));
	} else if(osvi.dwMajorVersion == 6) {
	    if(osvi.wProductType == VER_NT_WORKSTATION) {
		if(osvi.dwMinorVersion == 0) desc = "Vista";
		else desc = "7";
	    } else desc = "Server 2008";
	} else if(osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0)
	    desc = "2000";
	else if(osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1)
	    desc = "XP";
	else if(osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2) {
	    if(osvi.wProductType == VER_NT_WORKSTATION)
		desc = "XP Professional";
	    else
		desc = "Server 2003";
	}
	pGNSI = (PGNSI)
	    GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
			   "GetNativeSystemInfo");
	if(NULL != pGNSI) pGNSI(&si); else GetSystemInfo(&si);
	if(si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64)
	    type = " x64";

	if(osvi.wServicePackMajor > 0)
	    sprintf(ver,
		    "Windows %s%s (build %d) Service Pack %d",
		    desc, type,
		    LOWORD(osvi.dwBuildNumber),
		    (int) osvi.wServicePackMajor);
	else
	    sprintf(ver,
		    "Windows %s%s (build %d)",
		    desc, type,
		    LOWORD(osvi.dwBuildNumber));
    } else { /* should not get here */
	sprintf(ver, "Windows %d.%d (build %d) %s",
		(int) osvi.dwMajorVersion, (int) osvi.dwMinorVersion,
		LOWORD(osvi.dwBuildNumber), osvi.szCSDVersion);
    }

    return mkString(ver);
}

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

SEXP do_windialog(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP message;
    const char * type;
    int res=YES;

    checkArity(op, args);
    type = translateChar(STRING_ELT(CAR(args), 0));
    message = CADR(args);
    if(!isString(message) || length(message) != 1 ||
       strlen(translateChar(STRING_ELT(message, 0))) > 255)
	error(_("invalid '%s' argument"), "message");
    if (strcmp(type, "ok")  == 0) {
	askok(translateChar(STRING_ELT(message, 0)));
	res = 10;
    } else if (strcmp(type, "okcancel")  == 0) {
	res = askokcancel(translateChar(STRING_ELT(message, 0)));
	if(res == YES) res = 2;
    } else if (strcmp(type, "yesno")  == 0) {
	res = askyesno(translateChar(STRING_ELT(message, 0)));
    } else if (strcmp(type, "yesnocancel")  == 0) {
	res = askyesnocancel(translateChar(STRING_ELT(message, 0)));
    } else
	errorcall(call, _("unknown type"));
    return ScalarInteger(res);
}

SEXP do_windialogstring(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  message, def;
    const char *string;

    checkArity(op, args);
    message = CAR(args);
    if(!isString(message) || length(message) != 1 ||
       strlen(translateChar(STRING_ELT(message, 0))) > 255)
	error(_("invalid '%s' argument"), "message");
    def = CADR(args);
    if(!isString(def) || length(def) != 1)
	error(_("invalid '%s' argument"), "default");
    string = askstring(translateChar(STRING_ELT(message, 0)),
		       translateChar(STRING_ELT(def, 0)));
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

    items = wingetmenuitems(translateChar(STRING_ELT(mname,0)), errmsg);
    if (items->numItems == 0) {
	sprintf(msgbuf, _("unable to retrieve items for %s (%s)"),
		translateChar(STRING_ELT(mname,0)), errmsg);
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
	res = winaddmenu (translateChar(STRING_ELT(smenu, 0)), errmsg);
	if (res > 0) {
	    sprintf(msgbuf, _("unable to add menu (%s)"), errmsg);
	    errorcall(call, msgbuf);
	}

    } else { /* add an item */
	if(!isString(sitem) || length(sitem) != 1)
	    error(_("invalid '%s' argument"), "itemname");
	res = winaddmenuitem (translateChar(STRING_ELT(sitem, 0)),
			      translateChar(STRING_ELT(smenu, 0)),
			      translateChar(STRING_ELT(CADDR(args), 0)),
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
	res = windelmenu (translateChar(STRING_ELT(smenu, 0)), errmsg);
	if (res > 0)
	    errorcall(call, _("menu does not exist"));
    } else { /* delete an item */
	if(!isString(sitem) || length(sitem) != 1)
	    error(_("invalid '%s' argument"), "itemname");
	res = windelmenuitem (translateChar(STRING_ELT(sitem, 0)),
			      translateChar(STRING_ELT(smenu, 0)), errmsg);
	if (res > 0) {
	    sprintf(msgbuf, _("unable to delete menu item (%s)"), errmsg);
	    errorcall(call, msgbuf);
	}
    }
    return (R_NilValue);
}


typedef struct {
    window wprog;
    progressbar pb;
    label lab;
    int width;
    double min, max, val;
} winprogressbar;

static void pbarFinalizer(SEXP ptr)
{
    winprogressbar *pbar;

    if(TYPEOF(ptr) != EXTPTRSXP) return;
    pbar = R_ExternalPtrAddr(ptr);
    if(!pbar) return;
    hide(pbar->wprog);
    if(pbar-> lab) del(pbar->lab);
    del(pbar->pb);
    del(pbar->wprog);
    Free(pbar);
    R_ClearExternalPtr(ptr); /* not really needed */
}


/* winProgressBar(width, title, label, min, max, initial) */
SEXP do_winprogressbar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP tmp, ptr;
    int width, iv;
    double d;
    const char *title, *label;
    winprogressbar *pbar;
    Rboolean haveLabel;

    checkArity(op, args);

    pbar = Calloc(1, winprogressbar);
    width = asInteger(CAR(args)); args = CDR(args);
    if(width == NA_INTEGER || width < 0) width = 200;
    tmp = CAR(args); args = CDR(args);
    if(!isString(tmp) || length(tmp) < 1 || STRING_ELT(tmp, 0) == NA_STRING)
	errorcall(call, "invalid '%s' argument", "title");
    title = translateChar(STRING_ELT(tmp, 0));
    tmp = CAR(args); args = CDR(args);
    if(!isString(tmp) || length(tmp) < 1 || STRING_ELT(tmp, 0) == NA_STRING)
	errorcall(call, "invalid '%s' argument", "Label");
    label = translateChar(STRING_ELT(tmp, 0));
    haveLabel = strlen(label) > 0;
    d = asReal(CAR(args)); args = CDR(args);
    if (!R_FINITE(d)) errorcall(call, "invalid '%s' argument", "min");
    pbar->min = d;
    d = asReal(CAR(args)); args = CDR(args);
    if (!R_FINITE(d)) errorcall(call, "invalid '%s' argument", "max");
    pbar->max = d;
    d = asReal(CAR(args)); args = CDR(args);
    if (!R_FINITE(d)) errorcall(call, "invalid '%s' argument", "initial");
    pbar->val = d;

    pbar->width = width;
    pbar->wprog = newwindow(title, rect(0, 0, width+40, haveLabel ? 100: 80),
			    Titlebar | Centered);
    setbackground(pbar->wprog, dialog_bg());
    if(haveLabel)
	pbar->lab = newlabel(label, rect(10, 15, width+20, 25), AlignCenter);
    pbar->pb = newprogressbar(rect(20, haveLabel ? 50 : 30, width, 20),
			      0, width, 1, 1);
    iv = pbar->width * (pbar->val - pbar->min)/(pbar->max - pbar->min);
    setprogressbar(pbar->pb, iv);
    show(pbar->wprog);
    ptr = R_MakeExternalPtr(pbar, install("winProgressBar"), R_NilValue);
    R_RegisterCFinalizerEx(ptr, pbarFinalizer, TRUE);

    return ptr;
}

SEXP do_closewinprogressbar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    pbarFinalizer(CAR(args));
    return R_NilValue;
}

SEXP do_setwinprogressbar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ptr = CAR(args);
    winprogressbar *pbar;
    double value;

    checkArity(op, args);
    pbar = R_ExternalPtrAddr(ptr);
    if(!pbar)
	error("invalid progressbar -- has it been closed?");
    value = pbar->val;
    if(!isNull(CADR(args))) {
	int iv;
	double val = asReal(CADR(args));
	SEXP title = CADDR(args), label = CADDDR(args);
	if (R_FINITE(val) && val >= pbar->min && val <= pbar->max) {
	    iv = pbar->width * (val - pbar->min)/(pbar->max - pbar->min);
	    setprogressbar(pbar->pb, iv);
	    pbar->val = val;
	}
	if (!isNull(title)) {
	    SEXP ctxt;
	    if(!isString(title) || length(title) < 1)
		errorcall(call, "invalid '%s' argument", "title");
	    ctxt = STRING_ELT(title, 0);
	    if (ctxt != NA_STRING)
		settext(pbar->wprog, translateChar(ctxt));
	}
	if(pbar->lab && !isNull(label)) {
	    SEXP clab;
	    if(!isString(label) || length(label) < 1)
		errorcall(call, "invalid '%s' argument", "label");
	    clab = STRING_ELT(label, 0);
	    if (clab != NA_STRING)
		settext(pbar->lab, translateChar(clab));
	}
    }
    return ScalarReal(value);
}



void Rwin_fpset(void)
{
    /* Under recent MinGW this is what fpreset does.  It sets the
       control word to 0x37f which corresponds to 0x8001F as used by
       _controlfp.  That is all errors are masked, 64-bit mantissa and
       rounding are selected. */

    __asm__ ( "fninit" ) ;
}

#include "getline/getline.h"     /* for gl_load/savehistory */
#include "getline/wc_history.h"  /* for wgl_load/savehistory */
SEXP do_savehistory(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile;

    checkArity(op, args);
    sfile = CAR(args);
    if (!isString(sfile) || LENGTH(sfile) < 1)
	errorcall(call, _("invalid '%s' argument"), "file");
    if (CharacterMode == RGui) {
	R_setupHistory(); /* re-read the history size */
	wgl_savehistoryW(filenameToWchar(STRING_ELT(sfile, 0), 0), 
			 R_HistorySize);
    } else if (R_Interactive && CharacterMode == RTerm) {
	R_setupHistory(); /* re-read the history size */
	gl_savehistory(translateChar(STRING_ELT(sfile, 0)), R_HistorySize);
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
    if (CharacterMode == RGui)
	wgl_loadhistoryW(filenameToWchar(STRING_ELT(sfile, 0), 0));
    else if (R_Interactive && CharacterMode == RTerm)
	gl_loadhistory(translateChar(STRING_ELT(sfile, 0)));
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
    if (CharacterMode == RGui) {   
	for (i = 0; i < LENGTH(stamp); i++) 
	    wgl_histadd(wtransChar(STRING_ELT(stamp, i)));
    } else if (R_Interactive && CharacterMode == RTerm) {
    	for (i = 0; i < LENGTH(stamp); i++)
	    gl_histadd(translateChar(STRING_ELT(stamp, i)));
    }
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
    getActive(&gui);  /* Will get defaults if there's no active console */
    if (loadRconsole(&gui, translateChar(STRING_ELT(sfile, 0)))) applyGUI(&gui);
    if (strlen(gui.warning)) warning(gui.warning);
    return R_NilValue;
}

#include <lmcons.h>

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
    sprintf(ver, "%d.%d", (int)osvi.dwMajorVersion, (int)osvi.dwMinorVersion);

    if((int)osvi.dwMajorVersion >= 5) {
	PGNSI pGNSI;
	SYSTEM_INFO si;
	if(osvi.dwMajorVersion == 6) {
	    if(osvi.wProductType == VER_NT_WORKSTATION) {
		if(osvi.dwMinorVersion == 0)
		    strcpy(ver, "Vista");
		else strcpy(ver, "7");
	    } else
		strcpy(ver, "Server 2008");
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
	    sprintf(ver, "build %d, Service Pack %d",
		    LOWORD(osvi.dwBuildNumber),
		    (int) osvi.wServicePackMajor);
	else sprintf(ver, "build %d", LOWORD(osvi.dwBuildNumber));
    } else
	sprintf(ver, "build %d, %s", LOWORD(osvi.dwBuildNumber),
		osvi.szCSDVersion);
    SET_STRING_ELT(ans, 2, mkChar(ver));
    GetComputerNameW(name, &namelen);
    wcstoutf8(buf, name, 1000);
    SET_STRING_ELT(ans, 3, mkCharCE(buf, CE_UTF8));
#ifdef WIN64
    SET_STRING_ELT(ans, 4, mkChar("x86-64"));
#else
    SET_STRING_ELT(ans, 4, mkChar("x86"));
#endif
    GetUserNameW(user, &userlen);
    wcstoutf8(buf, user, 1000);
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
#endif

SEXP do_memsize(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    int maxmem = NA_LOGICAL;

    checkArity(op, args);
    if(isLogical(CAR(args))) 
	maxmem = asLogical(CAR(args));
    else if(isReal(CAR(args))) {
	R_size_t newmax;
	double mem = asReal(CAR(args));
	if (!R_FINITE(mem))
	    errorcall(call, _("incorrect argument"));
#ifdef LEA_MALLOC
#ifndef WIN64
	if(mem >= 4096)
	    errorcall(call, _("don't be silly!: your machine has a 4Gb address limit"));
#endif
	newmax = mem * 1048576.0;
	if (newmax < R_max_memory)
	    warningcall(call, _("cannot decrease memory limit: ignored"));
	else
	    R_max_memory = newmax;
#endif
    } else
	errorcall(call, _("incorrect argument"));
	
    PROTECT(ans = allocVector(REALSXP, 1));
#ifdef LEA_MALLOC
    if(maxmem == NA_LOGICAL)
	REAL(ans)[0] = R_max_memory;
    else if(maxmem)
	REAL(ans)[0] = mallinfo().usmblks;
    else
	REAL(ans)[0] = mallinfo().uordblks;
    REAL(ans)[0] /= 1048576.0;
#else
    REAL(ans)[0] = NA_REAL;
#endif
    UNPROTECT(1);
    return ans;
}

SEXP do_dllversion(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP path=R_NilValue, ans;
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
	if (GetFileVersionInfoW(dll, 0L, dwVerInfoSize, lpstrVffInfo))
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

static void cleanup(void)
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

rect getSysFontSize(void); /* in graphapp/fonts.c */
RECT *RgetMDIsize(void); /* in rui.c */

SEXP do_selectlist(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP choices, preselect, ans = R_NilValue;
    const char **clist;
    int i, j = -1, n, mw = 0, multiple, nsel = 0;
    int xmax, ymax, ylist, fht, h0;
    Rboolean haveTitle;

    checkArity(op, args);
    choices = CAR(args);
    if(!isString(choices)) error(_("invalid '%s' argument"), "choices");
    preselect = CADR(args);
    if(!isNull(preselect) && !isString(preselect))
	error(_("invalid '%s' argument"), "preselect");
    multiple = asLogical(CADDR(args));
    if(multiple == NA_LOGICAL) multiple = 0;
    haveTitle = isString(CADDDR(args));
    if(!multiple && isString(preselect) && LENGTH(preselect) != 1)
	error(_("invalid '%s' argument"), "preselect");

    n = LENGTH(choices);
    clist = (const char **) R_alloc(n + 1, sizeof(char *));
    for(i = 0; i < n; i++) {
	clist[i] = translateChar(STRING_ELT(choices, i));
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
    wselect = newwindow(haveTitle ? translateChar(STRING_ELT(CADDDR(args), 0)):
			(multiple ? _("Select one or more") : _("Select one")),
			rect(0, 0, xmax, ymax),
			Titlebar | Centered | Modal | Floating);
    setbackground(wselect, dialog_bg());
    if(multiple)
	f_list = newmultilist(clist, rect(10, 10, xmax-25, ylist), NULL, finish);
    else
	f_list = newlistbox(clist, rect(10, 10, xmax-25, ylist), NULL, finish);
    if(!isNull(preselect) && LENGTH(preselect)) {
	for(i = 0; i < n; i++)
	    for(j = 0; j < LENGTH(preselect); j++)
		if(strcmp(clist[i], translateChar(STRING_ELT(preselect, j))) == 0) {
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
    } else
	PROTECT(ans = mkString(selected));

    cleanup();
    show(RConsole);
    R_ProcessEvents();
    UNPROTECT(1);
    return ans;
}

int Rwin_rename(const char *from, const char *to)
{
    return (MoveFileEx(from, to, MOVEFILE_REPLACE_EXISTING | MOVEFILE_COPY_ALLOWED | MOVEFILE_WRITE_THROUGH) == 0);
}

int Rwin_wrename(const wchar_t *from, const wchar_t *to)
{
    return (MoveFileExW(from, to, MOVEFILE_REPLACE_EXISTING | MOVEFILE_COPY_ALLOWED | MOVEFILE_WRITE_THROUGH) == 0);
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

#define STRICT_R_HEADERS
#include <R_ext/RS.h>

/* split on \r\n or just one */
static SEXP splitClipboardText(const char *s, int ienc)
{
    int cnt_r= 0, cnt_n = 0, n, nc, nl, line_len = 0;
    const char *p;
    char *line, *q, eol = '\n';
    Rboolean last = TRUE; /* does final line have EOL */
    Rboolean CRLF = FALSE;
    SEXP ans;

    for(p = s, nc = 0; *p; p++, nc++)
	switch(*p) {
	case '\n':
	    cnt_n++;
	    last = TRUE;
	    line_len = max(line_len, nc);
	    nc = -1;
	    break;
	case '\r':
	    cnt_r++;
	    last = TRUE;
	    break;
	default:
	    last = FALSE;
	}
    if (!last) line_len = max(line_len, nc);  /* the unterminated last might be the longest */
    n = max(cnt_n, cnt_r) + (last ? 0 : 1);
    if (cnt_n == 0 && cnt_r > 0) eol = '\r';
    if (cnt_r == cnt_n) CRLF = TRUE;
    /* over-allocate a line buffer */
    line = R_chk_calloc(1+line_len, 1);
    PROTECT(ans = allocVector(STRSXP, n));
    for(p = s, q = line, nl = 0; *p; p++) {
	if (*p == eol) {
	    *q = '\0';
	    SET_STRING_ELT(ans, nl++, mkCharCE(line, ienc));
	    q = line;
	    *q = '\0';
	} else if(CRLF && *p == '\r')
	    ;
	else *q++ = *p;
    }
    if (!last) {
	*q = '\0';
	SET_STRING_ELT(ans, nl, mkCharCE(line, ienc));
    }
    R_chk_free(line);
    UNPROTECT(1);
    return(ans);
}

SEXP do_readClipboard(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = R_NilValue;
    HGLOBAL hglb;
    const char *pc;
    int j, format, raw, size;

    checkArity(op, args);
    format = asInteger(CAR(args));
    raw = asLogical(CADR(args));

    if(OpenClipboard(NULL)) {
	if(IsClipboardFormatAvailable(format) &&
	   (hglb = GetClipboardData(format)) &&
	   (pc = (const char *) GlobalLock(hglb))) {
	    if(raw) {
		Rbyte *pans;
		size = GlobalSize(hglb);
		ans = allocVector(RAWSXP, size); /* no R allocation below */
		pans = RAW(ans);
		for (j = 0; j < size; j++) pans[j] = *pc++;
	    } else if (format == CF_UNICODETEXT) {
		int n, ienc = CE_NATIVE;
		const wchar_t *wpc = (wchar_t *) pc;
		n = wcslen(wpc);
		char text[2 * (n+1)];  /* UTF-8 is at most 1.5x longer */
		R_CheckStack();
		wcstoutf8(text, wpc, n+1);
		if(!strIsASCII(text)) ienc = CE_UTF8;
		ans = splitClipboardText(text, ienc);
	    } else if (format == CF_TEXT || format == CF_OEMTEXT || format == CF_DIF) {
		/* can we get the encoding out of a CF_LOCALE entry? */
		ans = splitClipboardText(pc, 0);
	    } else
		error("'raw = FALSE' and format is a not a known text format");
	    GlobalUnlock(hglb);
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
    char *s;
    const char *p;
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
	if(raw) len = n;
	else if (format == CF_UNICODETEXT)
	    for(i = 0; i < n; i++)
		len += 2 * (wcslen(wtransChar(STRING_ELT(text, i))) + 2);
	else
	    for(i = 0; i < n; i++)
		len += strlen(translateChar(STRING_ELT(text, i))) + 2;

	if ( (hglb = GlobalAlloc(GHND, len)) &&
	     (s = (char *)GlobalLock(hglb)) ) {
	    if(raw)
		for(i = 0; i < n; i++) *s++ = RAW(text)[i];
	    else if (format == CF_UNICODETEXT) {
		const wchar_t *wp;
		wchar_t *ws = (wchar_t *) s;
		for(i = 0; i < n; i++) {
		    wp = wtransChar(STRING_ELT(text, i));
		    while(*wp) *ws++ = *wp++;
		    *ws++ = L'\r'; *ws++ = L'\n';
		}
		*ws = L'\0';
	    } else {
		for(i = 0; i < n; i++) {
		    p = translateChar(STRING_ELT(text, i));
		    while(*p) *s++ = *p++;
		    *s++ = '\r'; *s++ = '\n';
		}
		*s = '\0';
	    }

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

const char *formatError(DWORD res);


void R_UTF8fixslash(char *s); /* from main/util.c */
SEXP do_normalizepath(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, paths = CAR(args), el, slash;
    int i, n = LENGTH(paths), res;
    char tmp[MAX_PATH], longpath[MAX_PATH], *tmp2;
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
    	int warn = 0;
    	SEXP result;
	el = STRING_ELT(paths, i);
	result = el;
	if(getCharCE(el) == CE_UTF8) {
	    if ((res = GetFullPathNameW(filenameToWchar(el, FALSE), 32768, 
					wtmp, &wtmp2)) && res <= 32768) {
		if ((res = GetLongPathNameW(wtmp, wlongpath, 32768))
		    && res <= 32768) {
	    	    wcstoutf8(longpath, wlongpath, wcslen(wlongpath)+1);
		    if(fslash) R_UTF8fixslash(longpath);
	    	    result = mkCharCE(longpath, CE_UTF8);
		} else if(mustWork == 1) {
		    errorcall(call, "path[%d]=\"%s\": %s", i+1, 
			      translateChar(el), 
			      formatError(GetLastError()));	
	    	} else {
	    	    wcstoutf8(tmp, wtmp, wcslen(wtmp)+1);
		    if(fslash) R_UTF8fixslash(tmp);
	    	    result = mkCharCE(tmp, CE_UTF8);
	    	    warn = 1;
	    	}
	    } else if(mustWork == 1) {
		errorcall(call, "path[%d]=\"%s\": %s", i+1, 
			  translateChar(el), 
			  formatError(GetLastError()));	
	    } else {
		if (fslash) {
		    strcpy(tmp, translateCharUTF8(el));
		    R_UTF8fixslash(tmp);
	    	    result = mkCharCE(tmp, CE_UTF8);
		}
	    	warn = 1;
	    }
	    if (warn && (mustWork == NA_LOGICAL))
	    	warningcall(call, "path[%d]=\"%ls\": %s", i+1, 
			    filenameToWchar(el,FALSE), 
			    formatError(GetLastError()));
	} else {
	    if ((res = GetFullPathName(translateChar(el), MAX_PATH, tmp, &tmp2)) 
		&& res <= MAX_PATH) {
	    	if ((res = GetLongPathName(tmp, longpath, MAX_PATH))
		    && res <= MAX_PATH) {
		    if(fslash) R_fixslash(longpath);
	    	    result = mkChar(longpath);
		} else if(mustWork == 1) {
		    errorcall(call, "path[%d]=\"%s\": %s", i+1, 
			      translateChar(el), 
			      formatError(GetLastError()));	
	    	} else {
		    if(fslash) R_fixslash(tmp);
	    	    result = mkChar(tmp);
	    	    warn = 1;
	    	}
	    } else if(mustWork == 1) {
		errorcall(call, "path[%d]=\"%s\": %s", i+1, 
			  translateChar(el), 
			  formatError(GetLastError()));	
	    } else {
		if (fslash) {
		    strcpy(tmp, translateChar(el));
		    R_fixslash(tmp);
		    result = mkChar(tmp);
		}
	    	warn = 1;
	    }
	    if (warn && (mustWork == NA_LOGICAL))
		warningcall(call, "path[%d]=\"%s\": %s", i+1, 
			    translateChar(el), 
			    formatError(GetLastError()));	
	}
	SET_STRING_ELT(ans, i, result);
    }
    UNPROTECT(1);
    return ans;
}

SEXP do_shortpath(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, paths = CAR(args), el;
    int i, n = LENGTH(paths);
    char tmp[MAX_PATH];
    wchar_t wtmp[32768];
    DWORD res;

    checkArity(op, args);
    if(!isString(paths))
	errorcall(call, _("'path' must be a character vector"));

    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	el = STRING_ELT(paths, i);
	if(getCharCE(el) == CE_UTF8) {
	    res = GetShortPathNameW(filenameToWchar(el, FALSE), wtmp, 32768);
	    if (res && res <= 32768)
		wcstoutf8(tmp, wtmp, wcslen(wtmp)+1);
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
    return ans;
}

static int countFilenamesW(const wchar_t *list)
{
    const wchar_t *temp;
    int count;
    count = 0;
    for (temp = list; *temp; temp += wcslen(temp)+1) count++;
    return count;
}


static SEXP mkCharUTF8(const wchar_t *wc)
{
    char s[4*MAX_PATH];
    wcstoutf8(s, wc, 4*MAX_PATH);
    return mkCharCE(s, CE_UTF8);
}

SEXP do_chooseFiles(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, def, caption, filters;
    wchar_t *temp, *res, *cfilters;
    const wchar_t *p;
    wchar_t path[32768], filename[32768];
    int multi, filterindex, i, count, lfilters, pathlen;

    checkArity(op, args);
    def = CAR(args);
    caption = CADR(args);
    multi = asLogical(CADDR(args));
    filters = CADDDR(args);
    filterindex = asInteger(CAD4R(args));
    if(length(def) != 1 )
	errorcall(call, _("'default' must be a character string"));
    p = filenameToWchar(STRING_ELT(def, 0), 1);
    if(wcslen(p) >= 32768) errorcall(call, _("'default' is overlong"));
    wcscpy(path, p);
    for(temp = path; *temp; temp++) if(*temp == L'/') *temp = L'\\';
    if(length(caption) != 1 )
	errorcall(call, _("'caption' must be a character string"));
    if(multi == NA_LOGICAL)
	errorcall(call, _("'multi' must be a logical value"));
    if(filterindex == NA_INTEGER)
	errorcall(call, _("'filterindex' must be an integer value"));
    lfilters = 1 + length(filters);
    for (i = 0; i < length(filters); i++)
	lfilters += wcslen(filenameToWchar(STRING_ELT(filters, i), 0));
    cfilters = (wchar_t *) R_alloc(lfilters, sizeof(wchar_t));
    temp = cfilters;
    for (i = 0; i < length(filters)/2; i++) {
	wcscpy(temp, filenameToWchar(STRING_ELT(filters, i), 0));
	temp += wcslen(temp)+1;
	wcscpy(temp, filenameToWchar(STRING_ELT(filters, i+length(filters)/2),
				     0));
	temp += wcslen(temp)+1;
    }
    *temp = 0;

    res = askfilenamesW(filenameToWchar(STRING_ELT(caption, 0), 0), path,
			multi, cfilters, filterindex, NULL);

    if(!multi) {
	/* only one filename possible */
	count = 1;
    } else {
	count = countFilenamesW(res);
    }

    if (count < 2) PROTECT(ans = allocVector(STRSXP, count));
    else PROTECT(ans = allocVector(STRSXP, count-1));

    switch (count) {
    case 0: break;
    case 1: SET_STRING_ELT(ans, 0, mkCharUTF8(res));
	break;
    default:
	wcsncpy(path, res, 32768);
	pathlen = wcslen(path);
	if (path[pathlen-1] == L'\\') path[--pathlen] = L'\0';
	temp = res;
	for (i = 0; i < count-1; i++) {
	    temp += wcslen(temp) + 1;
	    if (wcschr(temp,L':') || *temp == L'\\' || *temp == L'/')
		SET_STRING_ELT(ans, i, mkCharUTF8(temp));
	    else {
		wcsncpy(filename, path, 32768);
		filename[pathlen] = L'\\';
		wcsncpy(filename+pathlen+1, temp, 32768-pathlen-1);
		SET_STRING_ELT(ans, i, mkCharUTF8(filename));
	    }
	}
    }
    UNPROTECT(1);
    return ans;
}

SEXP do_chooseDir(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, def, caption;
    const char *p;
    char path[MAX_PATH];

    checkArity(op, args);
    def = CAR(args);
    caption = CADR(args);
    if(!isString(def) || length(def) != 1 )
	errorcall(call, _("'default' must be a character string"));
    p = translateChar(STRING_ELT(def, 0));
    if(strlen(p) >= MAX_PATH) errorcall(call, _("'default' is overlong"));
    strcpy(path, R_ExpandFileName(p));
    R_fixbackslash(path);
    if(!isString(caption) || length(caption) != 1 )
	errorcall(call, _("'caption' must be a character string"));
    p = askcdstring(translateChar(STRING_ELT(caption, 0)), path);

    PROTECT(ans = allocVector(STRSXP, 1));
    SET_STRING_ELT(ans, 0, p ? mkChar(p): NA_STRING);
    UNPROTECT(1);
    return ans;
}

extern window RFrame; /* from rui.c */

SEXP getIdentification(void)
{
    const char *res = "" /* -Wall */;

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

SEXP getWindowTitle(void)
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

SEXP setTitle(const char *title)
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
    return setTitle(translateChar(STRING_ELT(title, 0)));
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
    setstatus(translateChar(STRING_ELT(text, 0)));
    return R_NilValue;
}

static void * getConsoleHandle(const char *which)
{
    if (CharacterMode != RGui) return(NULL);
    else if (strcmp(which, "Console") == 0 && RConsole)
	return getHandle(RConsole);
    else if (strcmp(which, "Frame") == 0 && RFrame)
	return getHandle(RFrame);
    else if (strcmp(which, "Process") == 0)
	return GetCurrentProcess();
    else return NULL;
}

static void * getDeviceHandle(int);

SEXP do_getWindowHandle(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    void * handle;
    SEXP which = CAR(args);

    checkArity(op, args);
    if(LENGTH(which) != 1)
	errorcall(call, _("'%s' must be length 1"), "which");
    if (isString(which)) handle = getConsoleHandle(CHAR(STRING_ELT(which,0)));
    else if (isInteger(which)) handle = getDeviceHandle(INTEGER(which)[0]);
    else handle = NULL;

    if (handle)
	return R_MakeExternalPtr(handle,R_NilValue,R_NilValue);
    else
	return R_NilValue;
}

static SEXP          EnumResult;
static int           EnumCount;
static PROTECT_INDEX EnumIndex;
static int           EnumMinimized;
static DWORD         EnumProcessId;

static BOOL CALLBACK EnumWindowsProc(HWND handle, LPARAM param) 
{
    char title[1024];
    if (IsWindowVisible(handle)) {
    	if (EnumProcessId) { /* restrict to R windows only */
    	    DWORD processId;
    	    GetWindowThreadProcessId(handle, &processId);
    	    if (processId != EnumProcessId) return TRUE;
    	}
    	if (!EnumMinimized && IsIconic(handle)) return TRUE;
    	if (EnumCount >= length(EnumResult)) {
    	    int newlen = 2*length(EnumResult);
    	    REPROTECT(EnumResult = lengthgets(EnumResult, newlen), EnumIndex);
    	    setAttrib(EnumResult, R_NamesSymbol, 
    	              lengthgets(getAttrib(EnumResult, R_NamesSymbol), newlen));
    	}
    	SET_VECTOR_ELT(EnumResult, EnumCount, R_MakeExternalPtr(handle,R_NilValue,R_NilValue));
    	if (GetWindowText(handle, title, 1024)) 
    	    SET_STRING_ELT(getAttrib(EnumResult, R_NamesSymbol), EnumCount, mkChar(title));
    	EnumCount++;
    }
    return TRUE;
}

SEXP do_getWindowHandles(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    PROTECT_WITH_INDEX(EnumResult = allocVector(VECSXP, 8), &EnumIndex);
    setAttrib(EnumResult, R_NamesSymbol, allocVector(STRSXP, 8));
    EnumCount = 0;
    const char * w;

    checkArity(op, args);
    w = CHAR(STRING_ELT(CAR(args), 0));
    EnumMinimized = LOGICAL(CADR(args))[0];

    if (strcmp(w, "R") == 0) 
    	EnumProcessId = GetCurrentProcessId();
    else EnumProcessId = 0;

    if (ismdi() && EnumProcessId) 
    	EnumChildWindows(GetParent(getHandle(RConsole)), EnumWindowsProc, 0);    
    else
    	EnumWindows(EnumWindowsProc, 0);
    	
    EnumResult = lengthgets(EnumResult, EnumCount);
    UNPROTECT(1);
    return EnumResult;
}

static void ArrangeWindows(int n, void** windows, int action, int preserve, int outer) {
    int j;
    if (action == MINIMIZE || action == RESTORE) {
    	for (j=0; j<n; j++)
    	    ShowWindow((HWND)windows[j], action == MINIMIZE ? SW_MINIMIZE : SW_RESTORE);
    } else {
    	RECT rect = {0,0,0,0};
    	RECT *prect = &rect;
    	HWND parent;
    	if (preserve) {
	    WINDOWPLACEMENT wp;
	    wp.length = sizeof(wp);
	    for (j=0; j<n; j++) {
		if (GetWindowPlacement((HWND)windows[j], &wp)) {
		    UnionRect(prect, prect, &wp.rcNormalPosition);
		    if (wp.showCmd == SW_SHOWMINIMIZED || wp.showCmd == SW_SHOWMAXIMIZED) {
			wp.showCmd = SW_RESTORE;
			SetWindowPlacement((HWND)windows[j], &wp);
		    }
		}
	    }
	}
        if (rect.left == rect.right || rect.top == rect.bottom) prect = NULL;
        
        if (!outer && ismdi())
            parent = GetParent(getHandle(RConsole));
        else
            parent = NULL;
	switch (action) {
	case CASCADE: CascadeWindows(parent, 0, prect, n, (HWND FAR *)windows);
		      break;
	case TILEHORIZ: TileWindows(parent, MDITILE_HORIZONTAL, prect, n, (HWND FAR *)windows);
		      break;
	case TILEVERT: TileWindows(parent, MDITILE_VERTICAL, prect, n, (HWND FAR *)windows);
		      break;    
        }
    }
}

SEXP do_arrangeWindows(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP windows;
    int i, action, preserve, outer;
    void **handles;
    
    checkArity(op, args);
    windows = CAR(args);
    if (length(windows)) {
	if (TYPEOF(windows) != VECSXP) error(_("'%s' must be a list"), "windows");
	handles = (void **)R_alloc(length(windows), sizeof(void *));
	for (i=0; i<length(windows); i++) {
	    if (TYPEOF(VECTOR_ELT(windows, i)) != EXTPTRSXP)
		error(_("'%s' element %d is not a window handle"), "windows", i+1);
	    handles[i] = R_ExternalPtrAddr(VECTOR_ELT(windows, i));
	}
	action = asInteger(CADR(args));
	preserve = asInteger(CADDR(args));
	outer = asInteger(CADDDR(args));
	ArrangeWindows(length(windows), handles, action, preserve, outer);
    }
    return windows;
}
    
#include "devWindows.h"
#include <Startup.h>
#include <R_ext/GraphicsEngine.h> /* GEgetDevice */
extern UImode CharacterMode;

SEXP do_bringtotop(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int dev, stay;
    pGEDevDesc gdd;
    gadesc *xd;

    checkArity(op, args);
    dev = asInteger(CAR(args));
    stay = asInteger(CADR(args));

    if(dev == -1) { /* console */
	if(CharacterMode == RGui) BringToTop(RConsole, stay);
    } else {
	if(dev < 1 || dev > R_MaxDevices || dev == NA_INTEGER)
	    errorcall(call, _("invalid '%s' argument"), "which");
	gdd = GEgetDevice(dev - 1);
	if(!gdd) errorcall(call, _("invalid device"));
	xd = (gadesc *) gdd->dev->deviceSpecific;
	if(!xd) errorcall(call, _("invalid device"));
	if(stay && ismdi()) error(_("requires SDI mode"));
	BringToTop(xd->gawin, stay);
    }
    return R_NilValue;
}

SEXP do_msgwindow(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int dev, type;
    pGEDevDesc gdd;
    gadesc *xd;

    checkArity(op, args);
    dev = asInteger(CAR(args));
    type = asInteger(CADR(args));

    if(dev == -1) { /* console */
	if(CharacterMode == RGui) GA_msgWindow(RConsole, type);
    } else {
	if(dev < 1 || dev > R_MaxDevices || dev == NA_INTEGER)
	    errorcall(call, _("invalid '%s' argument"), "which");
	gdd = GEgetDevice(dev - 1);
	if(!gdd) errorcall(call, _("invalid device"));
	xd = (gadesc *) gdd->dev->deviceSpecific;
	if(!xd) errorcall(call, _("invalid device"));
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

static void * getDeviceHandle(int dev)
{
    pGEDevDesc gdd;
    gadesc *xd;

    if (dev == -1) return(getHandle(RConsole));
    if (dev < 1 || dev > R_MaxDevices || dev == NA_INTEGER) return(0);
    gdd = GEgetDevice(dev - 1);
    if (!gdd) return(NULL);
    xd = (gadesc *) gdd->dev->deviceSpecific;
    if (!xd) return(NULL);
    return getHandle(xd->gawin);
}

/* This assumes a menuname of the form 
   $Graph<nn>Main, $Graph<nn>Popup, $Graph<nn>LocMain,
   or $Graph<nn>LocPopup where <nn> is the
   device number.  We've already checked the $Graph prefix. */

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

    if(attr == 0xffffffff) return -1;
    if(mode == F_OK) return 0;

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
			 | DACL_SECURITY_INFORMATION, 0, 0, &size);
	error = GetLastError();
	if (error != ERROR_INSUFFICIENT_BUFFER) return -1;
	sdPtr = (SECURITY_DESCRIPTOR *) alloca(size);
	if(!GetFileSecurityW(path,
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
    sprintf(DLLversion, "%s.%s", R_MAJOR, R_MINOR);
    return (DLLversion);
}

#include <winreg.h>

const static struct {
    const char * reg;
    HKEY key;
} 
KeyTable[] = {
    { "HCC", HKEY_CURRENT_CONFIG },
    { "HCR", HKEY_CLASSES_ROOT },
    { "HCU", HKEY_CURRENT_USER },
    { "HLM", HKEY_LOCAL_MACHINE },
    { "HPD", HKEY_PERFORMANCE_DATA },
    { "HU" , HKEY_USERS },
    {NULL, NULL}
};

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


static HKEY find_hive(const char *hkey)
{
    int i;
    for(i = 0;  KeyTable[i].reg; i++)
	if(!strcmp(hkey, KeyTable[i].reg)) return KeyTable[i].key;
    error(_("invalid '%s' value"),  "hive");
    return HKEY_LOCAL_MACHINE; /* -Wall */
}

static SEXP mkCharUcs(wchar_t *name)
{
    int n = wcslen(name), N = 3*n+1;
    char buf[N];
    R_CheckStack();
    wcstombs(buf, name, N); buf[N-1] = '\0';
    return mkCharCE(buf, CE_UTF8);
}

static SEXP readRegistryKey1(HKEY hkey, const wchar_t *name)
{
    SEXP ans = R_NilValue;
    LONG res;
    DWORD type, size0 = 10000, size = size0;
    BYTE data[10000], *d = data;

    res = RegQueryValueExW(hkey, name, NULL, &type, d, &size);
    while (res == ERROR_MORE_DATA) {
	size0 *= 10;
	size = size0;
	d = (BYTE *) R_alloc(size0, sizeof(char));
	res = RegQueryValueExW(hkey, name, NULL, &type, d, &size);
    }
    if (res != ERROR_SUCCESS) return ans;

    switch(type) {
    case REG_NONE:
	/* NULL */
	break;
    case REG_DWORD:
	ans = allocVector(INTSXP, 1);
	memcpy(INTEGER(ans), d, 4);
	break;
    case REG_DWORD_BIG_ENDIAN:
    {
	BYTE d4[4];
	int i;
	for(i = 0; i < 4; i++) d4[3-i] = d[i];
	ans = allocVector(INTSXP, 1);
	memcpy(INTEGER(ans), d4, 4);
	break;
    }
    case REG_SZ:
    case REG_EXPAND_SZ:
    {
	PROTECT(ans = allocVector(STRSXP, 1));
	SET_STRING_ELT(ans, 0, mkCharUcs((wchar_t *)d));
	UNPROTECT(1);
	break;
    }
    case REG_BINARY:
	ans = allocVector(RAWSXP, size);
	memcpy(RAW(ans), d, size);
	break;
    case REG_MULTI_SZ:
    {
	int i, n;
	wchar_t *p = (wchar_t *)d;
	for (n = 0; *p; n++) { for(; *p; p++) {}; p++; }
	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0, p = (wchar_t *)d; i < n; i++) {
	    SET_STRING_ELT(ans, i, mkCharUcs(p));
	    for(; *p; p++) {};
	    p++;
	}
	UNPROTECT(1);
	break;
    }
    case REG_LINK:
	warning("unhandled key type %s\n", "REG_LINK");
	ans = mkString("<REG_LINK>");
	break;
    case REG_RESOURCE_LIST:
	warning("unhandled key type %s\n", "REG_RESOURCE_LIST");
	ans = mkString("<REG_RESOURCE_LIST>");
	break;
    default:
	warning("unhandled key type %d\n", type);
    }
    return ans;
}

static SEXP readRegistryKey(HKEY hkey, int depth, int view)
{
    int i, k = 0, size0, *indx;
    SEXP ans, nm, ans0, nm0, tmp, sind;
    DWORD res, nsubkeys, maxsubkeylen, nval, maxvalnamlen, size;
    wchar_t *name;
    HKEY sub;
    REGSAM acc = KEY_READ;

    if (depth <= 0) return mkString("<subkey>");

    if(view == 2) acc |= KEY_WOW64_32KEY;
    else if(view == 3) acc |= KEY_WOW64_64KEY;

    res = RegQueryInfoKey(hkey, NULL, NULL, NULL,
			  &nsubkeys, &maxsubkeylen, NULL, &nval,
			  &maxvalnamlen, NULL, NULL, NULL);
    if (res != ERROR_SUCCESS)
	error("RegQueryInfoKey error code %d: '%s'", (int) res,
	      formatError(res));
    size0 = max(maxsubkeylen, maxvalnamlen) + 1;
    name = (wchar_t *) R_alloc(size0, sizeof(wchar_t));
    tmp = readRegistryKey1(hkey, L"");
    if (tmp != R_NilValue) {
	PROTECT(ans = allocVector(VECSXP, nval + nsubkeys + 1));
	PROTECT(nm = allocVector(STRSXP, nval+ nsubkeys + 1));
	SET_VECTOR_ELT(ans, 0, tmp);
	SET_STRING_ELT(nm, 0, mkChar("(Default)"));
	k++;
    } else {
	PROTECT(ans = allocVector(VECSXP, nval + nsubkeys));
	PROTECT(nm = allocVector(STRSXP, nval+ nsubkeys));
    }
    if (nval > 0) {
	PROTECT(ans0 = allocVector(VECSXP, nval));
	PROTECT(nm0 = allocVector(STRSXP, nval));
	for (i = 0; i < nval; i++) {
	    size = size0;
	    res  = RegEnumValueW(hkey, i, (LPWSTR) name, &size,
				 NULL, NULL, NULL, NULL);
	    if (res != ERROR_SUCCESS) break;
	    SET_VECTOR_ELT(ans0, i, readRegistryKey1(hkey, name));
	    SET_STRING_ELT(nm0, i, mkCharUcs(name));
	}
	/* now sort by name */
	PROTECT(sind = allocVector(INTSXP, nval));  indx = INTEGER(sind);
	for (i = 0; i < nval; i++) indx[i] = i;
	orderVector1(indx, nval, nm0, TRUE, FALSE, R_NilValue);
	for (i = 0; i < nval; i++, k++) {
	    SET_VECTOR_ELT(ans, k, VECTOR_ELT(ans0, indx[i]));
	    SET_STRING_ELT(nm, k, STRING_ELT(nm0, indx[i]));
	}
	UNPROTECT(3);
    }
    if (nsubkeys > 0) {
	PROTECT(ans0 = allocVector(VECSXP, nsubkeys));
	PROTECT(nm0 = allocVector(STRSXP, nsubkeys));
	for (i = 0; i < nsubkeys; i++) {
	    size = size0;
	    res = RegEnumKeyExW(hkey, i, (LPWSTR) name, &size,
				NULL, NULL, NULL, NULL);
	    if (res != ERROR_SUCCESS) break;
	    res = RegOpenKeyExW(hkey, (LPWSTR) name, 0, acc, &sub);
	    if (res != ERROR_SUCCESS) break;
	    SET_VECTOR_ELT(ans0, i, readRegistryKey(sub, depth-1, view));
	    SET_STRING_ELT(nm0, i, mkCharUcs(name));
	    RegCloseKey(sub);
	}
	/* now sort by name */
	PROTECT(sind = allocVector(INTSXP, nsubkeys));  indx = INTEGER(sind);
	for (i = 0; i < nsubkeys; i++) indx[i] = i;
	orderVector1(indx, nsubkeys, nm0, TRUE, FALSE, R_NilValue);
	for (i = 0; i < nsubkeys; i++, k++) {
	    SET_VECTOR_ELT(ans, k, VECTOR_ELT(ans0, indx[i]));
	    SET_STRING_ELT(nm, k, STRING_ELT(nm0, indx[i]));
	}
	UNPROTECT(3);
    }
    setAttrib(ans, R_NamesSymbol, nm);
    UNPROTECT(2);
    return ans;
}


SEXP do_readRegistry(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    HKEY hive, hkey;
    LONG res;
    const wchar_t *key;
    int maxdepth, view;
    REGSAM acc = KEY_READ;

    checkArity(op, args);
    if(!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	error(_("invalid '%s' value"),  "key");
    key = filenameToWchar(STRING_ELT(CAR(args), 0), 0);
    if(!isString(CADR(args)) || LENGTH(CADR(args)) != 1)
	error(_("invalid '%s' value"),  "hive");
    maxdepth = asInteger(CADDR(args));
    if(maxdepth == NA_INTEGER || maxdepth < 1)
	error(_("invalid '%s' value"),  "maxdepth");
    hive = find_hive(CHAR(STRING_ELT(CADR(args), 0)));
    view = asInteger(CADDDR(args));
    /* Or KEY_READ with KEY_WOW64_64KEY or KEY_WOW64_32KEY to
       explicitly access the 64- or 32- bit registry view.  See
       http://msdn.microsoft.com/en-us/library/aa384129(VS.85).aspx
    */
    if(view == 2) acc |= KEY_WOW64_32KEY;
    else if(view == 3) acc |= KEY_WOW64_64KEY;

    res = RegOpenKeyExW(hive, key, 0, acc, &hkey);
    if (res == ERROR_FILE_NOT_FOUND)
	error(_("Registry key '%ls' not found"), key);
    if (res != ERROR_SUCCESS)
	error("RegOpenKeyEx error code %d: '%s'", (int) res, formatError(res));
    ans = readRegistryKey(hkey, maxdepth, view);
    RegCloseKey(hkey);
    return ans;
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
#endif

SEXP attribute_hidden do_filechoose(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    wchar_t *fn;
    char str[MAX_PATH+1];

    checkArity(op, args);
    setuserfilterW(L"All files (*.*)\0*.*\0\0");
    fn = askfilenameW(G_("Select file"), "");
    if (!fn)
	error(_("file choice cancelled"));
    wcstoutf8(str, fn, MAX_PATH+1);
    PROTECT(ans = allocVector(STRSXP, 1));
    SET_STRING_ELT(ans, 0, mkCharCE(str, CE_UTF8));
    UNPROTECT(1);
    return ans;
}
