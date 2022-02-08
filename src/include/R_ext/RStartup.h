/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2022  The R Core Team
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

#if defined(__cplusplus) && !defined(DO_NOT_USE_CXX_HEADERS)
# include <cstddef>
# define R_SIZE_T std::size_t
#else
# include <stddef.h> /* for size_t */
# define R_SIZE_T size_t
#endif

#include <R_ext/Boolean.h>	/* TRUE/FALSE */

#ifdef __cplusplus
extern "C" {
#endif

#ifdef Win32
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
    Rboolean R_NoEcho;
    Rboolean R_Interactive;
    Rboolean R_Verbose;
    Rboolean LoadSiteFile;
    Rboolean LoadInitFile;
    Rboolean DebugInitFile;
    SA_TYPE RestoreAction;
    SA_TYPE SaveAction;
    R_SIZE_T vsize;
    R_SIZE_T nsize;
    R_SIZE_T max_vsize;
    R_SIZE_T max_nsize;
    R_SIZE_T ppsize;
    int NoRenviron;

#ifdef Win32
    char *rhome;               /* R_HOME */
    char *home;                /* HOME  */

    int (*ReadConsole) (const char *, unsigned char *, int, int);
    void (*WriteConsole) (const char *, int);
    void (*CallBack) (void);
    void (*ShowMessage) (const char *);
    int (*YesNoCancel) (const char *);
	/* Return value here is expected to be 1 for Yes, -1 for No and
	   0 for Cancel: symbolic constants in graphapp.h
	*/
    void (*Busy) (int);
    UImode CharacterMode;
    void (*WriteConsoleEx) (const char *, int, int);
	/* used only if WriteConsole is NULL */
    Rboolean EmitEmbeddedUTF8;
	/* R may embed UTF-8 sections into strings otherwise in current native
	   encoding, escaped by UTF8in and UTF8out (rgui_UTF8.h). The setting
	   currently has no effect in Rgui (always enabled) and in Rterm (never
	   enabled). The setting has no effect when UTF-8 is the native
	   encoding, which it is from R 4.2 on recent Windows systems when
	   the embedding application/front-end uses UTF-8 as the active code
	   page (system encoding) via its fusion manifest.
	*/
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
