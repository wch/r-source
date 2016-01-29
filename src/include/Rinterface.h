/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2016  The R Core Team.
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

/* This header file is to provide hooks for alternative front-ends,
   e.g. GUIs such as GNOME and Cocoa.  It is only used on Unix-alikes.
   All entries here should be documented in doc/manual/R-exts.texi.

   It should not be included by package sources unless they are
   providing such a front-end.
*/

#ifndef RINTERFACE_H_
#define RINTERFACE_H_

// Support for NO_C_HEADERS added in R 3.3.0
#ifdef __cplusplus
# ifndef NO_C_HEADERS
#  include <cstdio>
#  ifdef __SUNPRO_CC
using std::FILE;
#  endif
# endif
extern "C" {
#else
# ifndef NO_C_HEADERS
#  include <stdio.h>
#endif
#endif

#if defined(__GNUC__) && __GNUC__ >= 3
# define NORET __attribute__((noreturn))
#else
# define NORET
#endif

#include <R_ext/Boolean.h>
#include <R_ext/RStartup.h>

/* from Defn.h */
/* this duplication will be removed in due course */

extern Rboolean R_Interactive;	/* TRUE during interactive use*/
extern Rboolean	R_Slave;	/* Run as a slave process */

extern void R_RestoreGlobalEnv(void);
extern void R_RestoreGlobalEnvFromFile(const char *, Rboolean);
extern void R_SaveGlobalEnv(void);
extern void R_SaveGlobalEnvToFile(const char *);
extern void R_FlushConsole(void);
extern void R_ClearerrConsole(void);
extern void R_Suicide(const char *);
extern char *R_HomeDir(void);
extern int R_DirtyImage;	/* Current image dirty */
extern char *R_GUIType;
extern void R_setupHistory(void);
extern char *R_HistoryFile;	/* Name of the history file */
extern int R_HistorySize;	/* Size of the history file */
extern int R_RestoreHistory;	/* restore the history file? */
extern char *R_Home;		    /* Root of the R tree */

# define jump_to_toplevel	Rf_jump_to_toplevel
# define mainloop		Rf_mainloop
# define onintr			Rf_onintr
void NORET jump_to_toplevel(void);
void mainloop(void);
void onintr(void);
#ifndef DEFN_H_
extern void* R_GlobalContext;    /* Need opaque pointer type for export */
#endif

void process_site_Renviron(void);
void process_system_Renviron(void);
void process_user_Renviron(void);

extern FILE * R_Consolefile;
extern FILE * R_Outputfile;


/* in ../unix/sys-unix.c */
void R_setStartTime(void);
void fpu_setup(Rboolean);

/* in ../unix/system.c */
extern int R_running_as_main_program;

#ifdef CSTACK_DEFNS
/* duplicating Defn.h */
#if !defined(HAVE_UINTPTR_T) && !defined(uintptr_t)
 typedef unsigned long uintptr_t;
#endif

extern uintptr_t R_CStackLimit;	/* C stack limit */
extern uintptr_t R_CStackStart;	/* Initial stack address */
#endif

/* formerly in src/unix/devUI.h */

#ifdef R_INTERFACE_PTRS
#include <Rinternals.h>

#ifdef __SYSTEM__
# define extern
#endif

extern void (*ptr_R_Suicide)(const char *);
extern void (*ptr_R_ShowMessage)(const char *);
extern int  (*ptr_R_ReadConsole)(const char *, unsigned char *, int, int);
extern void (*ptr_R_WriteConsole)(const char *, int);
extern void (*ptr_R_WriteConsoleEx)(const char *, int, int);
extern void (*ptr_R_ResetConsole)(void);
extern void (*ptr_R_FlushConsole)(void);
extern void (*ptr_R_ClearerrConsole)(void);
extern void (*ptr_R_Busy)(int);
extern void (*ptr_R_CleanUp)(SA_TYPE, int, int);
extern int  (*ptr_R_ShowFiles)(int, const char **, const char **,
			       const char *, Rboolean, const char *);
extern int  (*ptr_R_ChooseFile)(int, char *, int);
extern int  (*ptr_R_EditFile)(const char *);
extern void (*ptr_R_loadhistory)(SEXP, SEXP, SEXP, SEXP);
extern void (*ptr_R_savehistory)(SEXP, SEXP, SEXP, SEXP);
extern void (*ptr_R_addhistory)(SEXP, SEXP, SEXP, SEXP);

// added in 3.0.0
extern int  (*ptr_R_EditFiles)(int, const char **, const char **, const char *);
// naming follows earlier versions in R.app
extern SEXP (*ptr_do_selectlist)(SEXP, SEXP, SEXP, SEXP);
extern SEXP (*ptr_do_dataentry)(SEXP, SEXP, SEXP, SEXP);
extern SEXP (*ptr_do_dataviewer)(SEXP, SEXP, SEXP, SEXP);
extern void (*ptr_R_ProcessEvents)();


/* These two are not used by R itself, but are used by the tcltk package */
extern int  (*R_timeout_handler)(void);
extern long R_timeout_val;

#endif /* R_INTERFACE_PTRS */

#ifdef __SYSTEM__
# undef extern
#endif

extern int R_SignalHandlers;

#ifdef __cplusplus
}
#endif

#endif /* RINTERFACE_H_ */
