/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2012-2013  The R Core Team
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

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("utils", String)
#else
#define _(String) (String)
#endif

SEXP objectSize(SEXP s);
SEXP unzip(SEXP args);
SEXP Rprof(SEXP args);
SEXP Rprofmem(SEXP args);

SEXP countfields(SEXP args);
SEXP flushconsole(void);
SEXP menu(SEXP args);
SEXP readtablehead(SEXP args);
SEXP typeconvert(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP writetable(SEXP call, SEXP op, SEXP args, SEXP env);

SEXP crc64(SEXP in);
SEXP nsl(SEXP hostname);
SEXP download(SEXP args);

SEXP sockconnect(SEXP sport, SEXP shost);
SEXP sockread(SEXP sport, SEXP smaxlen);
SEXP sockclose(SEXP sport);
SEXP sockopen(SEXP sport);
SEXP socklisten(SEXP sport);
SEXP sockwrite(SEXP sport, SEXP sstring);

SEXP addhistory(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP loadhistory(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP savehistory(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP dataentry(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP dataviewer(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP edit(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP fileedit(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP selectlist(SEXP call, SEXP op, SEXP args, SEXP rho);

SEXP processevents(void);

SEXP octsize(SEXP);

#ifdef Win32
SEXP winProgressBar(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP closeWinProgressBar(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP setWinProgressBar(SEXP call, SEXP op, SEXP args, SEXP rho);

SEXP winDialog(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP winDialogString(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP winMenuNames(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP winMenuItems(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP winMenuAdd(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP winMenuDel(SEXP call, SEXP op, SEXP args, SEXP env);

SEXP readRegistry(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP winver(void);
SEXP getClipboardFormats(void);
SEXP readClipboard(SEXP sformat, SEXP sraw);
SEXP writeClipboard(SEXP text, SEXP sformat);
SEXP getIdentification(void);
SEXP getWindowTitle(void);
SEXP setWindowTitle(SEXP title);
SEXP setStatusBar(SEXP text);
SEXP chooseFiles(SEXP def, SEXP action, SEXP smulti, SEXP filters, SEXP sindex);
SEXP chooseDir(SEXP def, SEXP caption);

SEXP getWindowsHandle(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP getWindowsHandles(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP arrangeWindows(SEXP call, SEXP op, SEXP args, SEXP env);

SEXP dllversion(SEXP path);

SEXP loadRconsole(SEXP file);
SEXP memsize(SEXP size);
SEXP shortpath(SEXP paths);
#endif
