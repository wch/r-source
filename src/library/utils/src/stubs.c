/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-4     the R Core Team
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

#include <Defn.h> /* for checkArity */

#ifdef Win32
#include "Startup.h"
extern UImode CharacterMode;
#include "getline/getline.h"     /* for gl_load/savehistory */
#include "getline/wc_history.h"  /* for wgl_load/savehistory */
SEXP savehistory(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile;

    args = CDR(args);
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

SEXP loadhistory(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile;

    args = CDR(args);
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

SEXP addhistory(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP stamp;

    args = CDR(args);
    stamp = CAR(args);
    if (!isString(stamp))
	errorcall(call, _("invalid timestamp"));
    if (CharacterMode == RGui) {   
	for (int i = 0; i < LENGTH(stamp); i++) 
	    wgl_histadd(wtransChar(STRING_ELT(stamp, i)));
    } else if (R_Interactive && CharacterMode == RTerm) {
    	for (int i = 0; i < LENGTH(stamp); i++)
	    gl_histadd(translateChar(STRING_ELT(stamp, i)));
    }
    return R_NilValue;
}

#else

#define R_INTERFACE_PTRS 1
#include <Rinterface.h>

SEXP loadhistory(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    args = CDR(args);
    ptr_R_loadhistory(call, op, args, rho);
    return R_NilValue;
}

SEXP savehistory(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    args = CDR(args);
    ptr_R_savehistory(call, op, args, rho);
    return R_NilValue;
}

SEXP addhistory(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    args = CDR(args);
    if(ptr_R_addhistory) ptr_R_addhistory(call, op, args, rho);
    return R_NilValue;
}
#endif
