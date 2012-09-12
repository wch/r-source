/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file util.c
 *  Copyright (C) 1998--2003  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004	      The R Foundation
 *  Copyright (C) 2005--2012  The R Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <windows.h>

#include "win-nls.h"

typedef void (WINAPI *PGNSI)(LPSYSTEM_INFO);

SEXP winver(void)
{
    char ver[256];
    OSVERSIONINFOEX osvi;

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

SEXP getClipboardFormats(void)
{
    SEXP ans = R_NilValue;
    int j, size, format = 0;

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

SEXP readClipboard(SEXP sformat, SEXP sraw)
{
    SEXP ans = R_NilValue;
    HGLOBAL hglb;
    const char *pc;
    int j, format, raw, size;

    format = asInteger(sformat);
    raw = asLogical(sraw);

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

SEXP writeClipboard(SEXP text, SEXP sformat)
{
    int i, n, format;
    HGLOBAL hglb;
    char *s;
    const char *p;
    Rboolean success = FALSE, raw = FALSE;

    format = asInteger(sformat);

    if (TYPEOF(text) == RAWSXP) raw = TRUE;
    else if(!isString(text))
	error(_("argument must be a character vector or a raw vector"));

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
		warning(_("unable to open the clipboard"));
		GlobalFree(hglb);
	    } else {
		success = SetClipboardData(CF_TEXT, hglb) != 0;
		if(!success) {
		    warning(_("unable to write to the clipboard"));
		    GlobalFree(hglb);
		}
		CloseClipboard();
	    }
	}
    }
    return ScalarLogical(success);
}

#include "Startup.h"
extern UImode CharacterMode;

#include <graphapp/ga.h>
#include "rui.h"

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


static SEXP in_setTitle(const char *title)
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

SEXP setWindowTitle(SEXP title)
{
    if(!isString(title)  || LENGTH(title) != 1 ||
       STRING_ELT(title, 0) == NA_STRING)
	error(_("'title' must be a character string"));
    return in_setTitle(translateChar(STRING_ELT(title, 0)));
}


SEXP setStatusBar(SEXP text)
{
    if(!isString(text)  || LENGTH(text) != 1 ||
       STRING_ELT(text, 0) == NA_STRING)
	error(_("'text' must be a character string"));
    showstatusbar();
    setstatus(translateChar(STRING_ELT(text, 0)));
    return R_NilValue;
}
