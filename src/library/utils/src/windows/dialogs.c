/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file dialogs.c
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
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include "graphapp/ga.h"
#include <windows.h>
#undef ERROR
#include <R_ext/RS.h> /* for Calloc */

#include "win-nls.h"
#include "rui.h"

#include "Startup.h"

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
SEXP winProgressBar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP tmp, ptr;
    int width, iv;
    double d;
    const char *title, *label;
    winprogressbar *pbar;
    Rboolean haveLabel;

    args = CDR(args);
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

SEXP closeWinProgressBar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    pbarFinalizer(CADR(args));
    return R_NilValue;
}

SEXP setWinProgressBar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    args = CDR(args);
    SEXP ptr = CAR(args);
    winprogressbar *pbar;
    double value;

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

SEXP winDialog(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP message;
    const char * type;
    int res=YES;

    args = CDR(args);
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

SEXP winDialogString(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  message, def;
    const char *string;

    args = CDR(args);
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

static char msgbuf[256];

SEXP winMenuNames(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP menuNames;
    int i, nmenus;

    args = CDR(args);
    if (CharacterMode != RGui)
	errorcall(call, _("menu functions can only be used in the GUI"));

    nmenus = numwinmenus();

    PROTECT(menuNames = allocVector(STRSXP, nmenus));

    for (i = 0; i < nmenus; i++) {
	SET_STRING_ELT(menuNames, i, mkChar(getusermenuname(i)));
    }

    UNPROTECT(1);
    return(menuNames);
}

SEXP winMenuItems(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP mname, ans, ansnames;
    menuItems *items;
    char errmsg[50];
    int i;

    args = CDR(args);

    if (CharacterMode != RGui)
	errorcall(call, _("menu functions can only be used in the GUI"));

    mname = CAR(args);
    if (!isString(mname) || length(mname) != 1)
	error(_("invalid '%s' argument"), "menuname");

    items = wingetmenuitems(translateChar(STRING_ELT(mname,0)), errmsg);
    if (items->numItems == 0) {
	snprintf(msgbuf, 256, _("unable to retrieve items for %s (%s)"),
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


SEXP winMenuAdd(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP smenu, sitem;
    int res;
    char errmsg[50];

    args = CDR(args);
    if (CharacterMode != RGui)
	errorcall(call, _("menu functions can only be used in the GUI"));
    smenu = CAR(args);
    if(!isString(smenu) || length(smenu) != 1)
	error(_("invalid '%s' argument"), "menuname");
    sitem = CADR(args);
    if (isNull(sitem)) { /* add a menu */
	res = winaddmenu (translateChar(STRING_ELT(smenu, 0)), errmsg);
	if (res > 0) {
	    snprintf(msgbuf, 256, _("unable to add menu (%s)"), errmsg);
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
	    snprintf(msgbuf, 256, _("unable to add menu item (%s)"), errmsg);
	    errorcall(call, msgbuf);
	}
    }
    return (R_NilValue);
}

SEXP winMenuDel(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP smenu, sitem;
    int res;
    char errmsg[50];

    args = CDR(args);
    if (CharacterMode != RGui)
	errorcall(call, _("menu functions can only be used in the GUI"));
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
	    snprintf(msgbuf, 256, _("unable to delete menu item (%s)"), errmsg);
	    errorcall(call, msgbuf);
	}
    }
    return (R_NilValue);
}
