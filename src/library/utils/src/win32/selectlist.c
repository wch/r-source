/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file selectlist.c
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
