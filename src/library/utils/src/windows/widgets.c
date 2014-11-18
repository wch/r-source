/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file selectlist.c
 *  Copyright (C) 1998--2003  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004	      The R Foundation
 *  Copyright (C) 2005--2013  The R Core Team
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
#include "graphapp/ga.h"
#include <rui.h> // RConsole
#include <windows.h>

#include "win-nls.h"

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

SEXP Win_selectlist(SEXP args)
{
    SEXP choices, preselect, ans = R_NilValue;
    const char **clist;
    int i, j = -1, n, mw = 0, multiple, nsel = 0;
    int xmax, ymax, ylist, fht, h0;
    Rboolean haveTitle;

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
	R_WaitEvent();
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


SEXP chooseFiles(SEXP def, SEXP caption, SEXP smulti, SEXP filters, SEXP sindex)
{
    wchar_t *temp, *res, *cfilters;
    const wchar_t *p;
    wchar_t path[32768], filename[32768];
    int multi, filterindex, i, count, lfilters, pathlen;

    multi = asLogical(smulti);
    filterindex = asInteger(sindex);
    if(length(def) != 1 )
	error(_("'default' must be a character string"));
    p = filenameToWchar(STRING_ELT(def, 0), 1);
    if(wcslen(p) >= 32768) error(_("'default' is overlong"));
    wcscpy(path, p);
    for(temp = path; *temp; temp++) if(*temp == L'/') *temp = L'\\';
    if(length(caption) != 1 )
	error(_("'caption' must be a character string"));
    if(multi == NA_LOGICAL)
	error(_("'multi' must be a logical value"));
    if(filterindex == NA_INTEGER)
	error(_("'filterindex' must be an integer value"));
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

    count = countFilenamesW(res);

    SEXP ans;
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

SEXP chooseDir(SEXP def, SEXP caption)
{
    const char *p;
    char path[MAX_PATH];

    if(!isString(def) || length(def) != 1 )
	error(_("'default' must be a character string"));
    p = translateChar(STRING_ELT(def, 0));
    if(strlen(p) >= MAX_PATH) error(_("'default' is overlong"));
    strcpy(path, R_ExpandFileName(p));
    R_fixbackslash(path);
    if(!isString(caption) || length(caption) != 1 )
	error(_("'caption' must be a character string"));
    p = askcdstring(translateChar(STRING_ELT(caption, 0)), path);

    SEXP ans = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(ans, 0, p ? mkChar(p): NA_STRING);
    UNPROTECT(1);
    return ans;
}
