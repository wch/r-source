/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-8 The R Development Core Team
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

/* A widechar version of code in platform.c for Unix-alikes */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <sys/stat.h>
#include <dirent.h>
#include <wchar.h>

static Rboolean attribute_hidden R_HiddenWFile(const wchar_t *name)
{
    if (name && name[0] != L'.') return 0;
    else return 1;
}

#define CBUFSIZE 2*PATH_MAX+1
static SEXP filename(const wchar_t *dir, const wchar_t *file)
{
    SEXP ans;
    char cbuf[2*CBUFSIZE];
    wchar_t wb[CBUFSIZE];
    int ienc= UTF8_MASK;

    if (dir) {
	switch (dir[wcslen(dir)-1])
	{
	case L'/':
	case L'\\':
	case ':':
	    wcscpy(wb, dir); wcscat(wb, file);
	    break;
	default:
	    wcscpy(wb, dir); wcscat(wb, L"/"); wcscat(wb, file);
    	}
    } else wcscpy(wb, file);
    wcstoutf8(cbuf, wb, wcslen(wb)+1);
    ans = mkCharEnc(cbuf, ienc);
    return ans;
}

static void count_files(const wchar_t *dnp, int *count,
			Rboolean allfiles, Rboolean recursive)
{
    _WDIR *dir;
    struct _wdirent *de;
    wchar_t p[PATH_MAX];
    struct _stati64 sb;

    if (wcslen(dnp) >= PATH_MAX)  /* should not happen! */
	error(_("directory/folder path name too long"));
    if ((dir = _wopendir(dnp)) == NULL) {
	warning(_("list.files: '%s' is not a readable directory"), dnp);
    } else {
	while ((de = _wreaddir(dir))) {
	    if (allfiles || !R_HiddenWFile(de->d_name)) {
		if(recursive) {
		    wcscpy(p, dnp); wcscat(p, L"/"); wcscat(p, de->d_name);
		    _wstati64(p, &sb);
		    if((sb.st_mode & S_IFDIR) > 0) {
			if (wcscmp(de->d_name, L".") && wcscmp(de->d_name, L".."))
				count_files(p, count, allfiles, recursive);
			continue;
		    }
		}
		(*count)++;
	    }
	}
	_wclosedir(dir);
    }
}

static void list_files(const wchar_t *dnp, const wchar_t *stem, int *count, SEXP ans,
		       Rboolean allfiles, Rboolean recursive)
{
    _WDIR *dir;
    struct _wdirent *de;
    wchar_t p[PATH_MAX], stem2[PATH_MAX];
    struct _stati64 sb;

    if ((dir = _wopendir(dnp)) != NULL) {
	while ((de = _wreaddir(dir))) {
	    if (allfiles || !R_HiddenWFile(de->d_name)) {
		if(recursive) {
		    wcscpy(p, dnp); wcscat(p, L"/"); wcscat(p, de->d_name);
		    _wstati64(p, &sb);
		    if((sb.st_mode & S_IFDIR) > 0) {
			if (wcscmp(de->d_name, L".") && wcscmp(de->d_name, L"..")) {
			    if(stem) {
				wcscpy(stem2, stem);
				wcscat(stem2, L"/"); 
				wcscat(stem2, de->d_name);
			    } else wcscpy(stem2, de->d_name);
			    list_files(p, stem2, count, ans, allfiles, recursive);
		    	}
			continue;
		    }
		}
		SET_STRING_ELT(ans, (*count)++, filename(stem, de->d_name));
	    }
	}
	_wclosedir(dir);
    }
}

SEXP attribute_hidden do_listfiles(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP d, ans;
    Rboolean allfiles, fullnames, recursive;
    int i, count, ndir;
    const wchar_t *dnp;

    checkArity(op, args);
    d = CAR(args);  args = CDR(args);
    if (!isString(d))
	error(_("invalid '%s' argument"), "directory");
    allfiles = asLogical(CAR(args)); args = CDR(args);
    fullnames = asLogical(CAR(args)); args = CDR(args);
    recursive = asLogical(CAR(args));
    ndir = length(d);
    count = 0;
    for (i = 0; i < ndir ; i++) {
	dnp = filenameToWchar(STRING_ELT(d, i), TRUE);
	count_files(dnp, &count, allfiles, recursive);
    }
    PROTECT(ans = allocVector(STRSXP, count));
    count = 0;
    for (i = 0; i < ndir ; i++) {
	dnp = filenameToWchar(STRING_ELT(d, i), TRUE);
	if (fullnames)
	    list_files(dnp, dnp, &count, ans, allfiles, recursive);
	else
	    list_files(dnp, NULL, &count, ans, allfiles, recursive);
    }
    ssort(STRING_PTR(ans), count);
    UNPROTECT(1);
    return ans;
}
