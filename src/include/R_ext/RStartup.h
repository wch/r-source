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

#define RSTART_VERSION 1 /* version 1 introduced in R 4.2.0 */

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
    Rboolean NoRenviron : 16;
	/* RstartVersion has been added in R 4.2.0. Earlier, NoRenviron was an
	   int (normally 32-bit like Rboolean), so on most machines the
	   version would become 0 when setting NoRenviron to FALSE in
	   R_DefParams by R older than 4.2.0. To be safe, embedding
	   applications should be compiled using the same version of R they
	   embed, as shown in rtest.c and WRE, and definitely recompiled with
	   R 4.2.0.
	*/
    int RstartVersion : 16;
    
#ifdef Win32
    char *rhome;               /* R_HOME */
    char *home;                /* HOME  */

    int (*ReadConsole) (const char *, unsigned char *, int, int);
    void (*WriteConsole) (const char *, int);
    void (*CallBack) (void);   /* ProcessEvents under Unix */
    void (*ShowMessage) (const char *);
    int (*YesNoCancel) (const char *);
	/* Return value here is expected to be 1 for Yes, -1 for No and
	   0 for Cancel: symbolic constants in graphapp.h
	*/
    void (*Busy) (int);
    UImode CharacterMode;
	/* The following field has been added in R 2.5.0 */
    void (*WriteConsoleEx) (const char *, int, int);
	/* used only if WriteConsole is NULL */

	/* The following field has been added in R 4.0.0. */
    Rboolean EmitEmbeddedUTF8;
	/* R may embed UTF-8 sections into strings otherwise in current native
	   encoding, escaped by UTF8in and UTF8out (rgui_UTF8.h). The setting
	   has no effect in Rgui (escaping happens iff the system codepage is 
	   not UTF-8) neither in Rterm (never enabled). For UTF-8 to be the
	   system codepage, the embeddeding application must set UTF-8 as the
	   active code page (system encoding) via its fusion manifest. When
	   using version 0 of the structure, this field must be initialized
	   by the embedding application/front-end.
	*/

	/* The following fields have been added in R 4.2.0 and are only
	   available with RstarVersion 1.
	*/
    void (*CleanUp)(SA_TYPE, int, int);
    void (*ClearerrConsole)(void);
    void (*FlushConsole)(void);
    void (*ResetConsole) (void);
    void (*Suicide) (const char *s);
#endif
} structRstart;

typedef structRstart *Rstart;

void R_DefParams(Rstart); /* only for RstartVersion 0 */
int R_DefParamsEx(Rstart, int);
    /* New code should always use R_DefParamsEx(Rstart, RSTART_VERSION) to
       inform R about the version of the structure used. R_DefParams(Rstart)
       only supports version 0 of the structure.
    */
void R_SetParams(Rstart);
void R_DefCallbacks(Rstart, int);
void R_SetWin32(Rstart);
void R_SizeFromEnv(Rstart);
void R_common_command_line(int *, char **, Rstart);

void R_set_command_line_arguments(int argc, char **argv);

void setup_Rmainloop(void); // also in Rembedded.h

#ifdef __cplusplus
}
#endif

#endif
