/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file registry.c
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

#include "win-nls.h"


#include "Defn.h"
/* Mingw-w64 defines this to be 0x0502 */
#ifndef _WIN32_WINNT
# define _WIN32_WINNT 0x0502 /* for KEY_WOW64_64KEY */
#endif

#include <windows.h>


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


SEXP readRegistry(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    HKEY hive, hkey;
    LONG res;
    const wchar_t *key;
    int maxdepth, view;
    REGSAM acc = KEY_READ;

    args = CDR(args);
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
