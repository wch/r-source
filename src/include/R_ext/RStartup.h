/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2010  The R Core Team
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

/*
  C functions to be called from alternative front-ends.

  Part of the API for such front-ends but not for packages.
*/

#ifndef R_EXT_RSTARTUP_H_
#define R_EXT_RSTARTUP_H_

#include <R_ext/Boolean.h>	/* TRUE/FALSE */

#ifdef __cplusplus
extern "C" {
#endif

#ifdef Win32
typedef int (*blah1) (const char *, char *, int, int);
typedef void (*blah2) (const char *, int);
typedef void (*blah3) (void);
typedef void (*blah4) (const char *);
/* Return value here is expected to be 1 for Yes, -1 for No and 0 for Cancel:
   symbolic constants in graphapp.h */
typedef int (*blah5) (const char *);
typedef void (*blah6) (int);
typedef void (*blah7) (const char *, int, int);
typedef enum {RGui, RTerm, LinkDLL} UImode;
#endif

/* Startup Actions */
typedef enum {
    SA_NORESTORE,/* = 0 */
    SA_RESTORE,
    SA_DEFAULT,/* was === SA_RESTORE */
    SA_NOSAVE,
    SA_SAVE,
    SA_SAVEASK,
    SA_SUICIDE
} SA_TYPE;

typedef struct
{
    Rboolean R_Quiet;
    Rboolean R_Slave;
    Rboolean R_Interactive;
    Rboolean R_Verbose;
    Rboolean LoadSiteFile;
    Rboolean LoadInitFile;
    Rboolean DebugInitFile;
    SA_TYPE RestoreAction;
    SA_TYPE SaveAction;
    size_t vsize;
    size_t nsize;
    size_t max_vsize;
    size_t max_nsize;
    size_t ppsize;
    int NoRenviron;

#ifdef Win32
    char *rhome;               /* R_HOME */
    char *home;                /* HOME  */
    blah1 ReadConsole;
    blah2 WriteConsole;
    blah3 CallBack;
    blah4 ShowMessage;
    blah5 YesNoCancel;
    blah6 Busy;
    UImode CharacterMode;
    blah7 WriteConsoleEx; /* used only if WriteConsole is NULL */
#endif
} structRstart;

typedef structRstart *Rstart;

void R_DefParams(Rstart);
void R_SetParams(Rstart);
void R_SetWin32(Rstart);
void R_SizeFromEnv(Rstart);
void R_common_command_line(int *, char **, Rstart);

void R_set_command_line_arguments(int argc, char **argv);

void setup_Rmainloop(void); // also in Rembedded.h

#ifdef __cplusplus
}
#endif

#endif
