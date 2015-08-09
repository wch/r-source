/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2012   The R Core Team.
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

#include <R.h>
#include <Rinternals.h>

#include "utils.h"
#include <R_ext/Rdynload.h>


#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef CallEntries[] = {
    CALLDEF(crc64, 1),
    CALLDEF(flushconsole, 0),
    CALLDEF(menu, 1),
    CALLDEF(nsl, 1),
    CALLDEF(objectSize, 1),
    CALLDEF(processevents, 0),
    CALLDEF(octsize, 1),

    /* Sockets */
    CALLDEF(sockconnect, 2),
    CALLDEF(sockread, 2),
    CALLDEF(sockclose, 1),
    CALLDEF(sockopen, 1),
    CALLDEF(socklisten, 1),
    CALLDEF(sockwrite, 2),

#ifdef Win32
    CALLDEF(winver, 0),
    CALLDEF(dllversion, 1),
    CALLDEF(getClipboardFormats, 0),
    CALLDEF(readClipboard, 2),
    CALLDEF(writeClipboard, 2),
    CALLDEF(getIdentification, 0),
    CALLDEF(getWindowTitle, 0),
    CALLDEF(setWindowTitle, 1),
    CALLDEF(setStatusBar, 1),
    CALLDEF(chooseFiles, 5),
    CALLDEF(chooseDir, 2),
    CALLDEF(getWindowsHandle, 1),
    CALLDEF(getWindowsHandles, 2),
    CALLDEF(loadRconsole, 1),
    CALLDEF(memsize, 1),
    CALLDEF(shortpath, 1),
#endif

    {NULL, NULL, 0}
};

#define EXTDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_ExternalMethodDef ExtEntries[] = {
#ifdef Win32
    EXTDEF(download, 6),
#else
    EXTDEF(download, 5),
#endif
    EXTDEF(unzip, 7),
    EXTDEF(Rprof, 8),
    EXTDEF(Rprofmem, 3),

    EXTDEF(countfields, 6),
    EXTDEF(readtablehead, 7),
    EXTDEF(typeconvert, 5),
    EXTDEF(writetable, 11),

    EXTDEF(addhistory, 1),
    EXTDEF(loadhistory, 1),
    EXTDEF(savehistory, 1),

    EXTDEF(dataentry, 2),
    EXTDEF(dataviewer, 2),
    EXTDEF(edit, 4),
    EXTDEF(fileedit, 3),
    EXTDEF(selectlist, 4),

#ifdef Win32
    EXTDEF(winProgressBar, 6),
    EXTDEF(closeWinProgressBar, 1),
    EXTDEF(setWinProgressBar, 4),
    EXTDEF(winDialog, 2),
    EXTDEF(winDialogString, 2),
    EXTDEF(winMenuNames, 0),
    EXTDEF(winMenuItems, 1),
    EXTDEF(winMenuAdd, 3),
    EXTDEF(winMenuDel, 2),

    EXTDEF(readRegistry, 4),
    EXTDEF(arrangeWindows, 4),
#endif

    {NULL, NULL, 0}
};


void
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_utils(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, ExtEntries);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
