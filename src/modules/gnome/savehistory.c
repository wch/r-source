/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000   the R Development Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "system.h"
#include "Rinterface.h"
#include "gtkconsole.h"
#include "terminal.h"

extern void Rf_errorcall(SEXP, const char*, ...);

void Rgnome_loadhistory(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile;
    char file[PATH_MAX];
   
    sfile = CAR(args);
    if (!isString(sfile) || LENGTH(sfile) < 1)
	Rf_errorcall(call, "invalid file argument");
    strcpy(file, R_ExpandFileName(CHAR(STRING_ELT(sfile,0))));
    gtk_console_clear_history(GTK_CONSOLE(R_gtk_terminal_text));
    gtk_console_restore_history(GTK_CONSOLE(R_gtk_terminal_text), 
				file, R_HistorySize, NULL);
}

void Rgnome_savehistory(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile;
    char file[PATH_MAX];
    
    sfile = CAR(args);
    if (!isString(sfile) || LENGTH(sfile) < 1)
	Rf_errorcall(call, "invalid file argument");
    strcpy(file, R_ExpandFileName(CHAR(STRING_ELT(sfile, 0))));
    gtk_console_save_history(GTK_CONSOLE(R_gtk_terminal_text), 
			     file, R_HistorySize, NULL);
}
