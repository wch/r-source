/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2004  the R Development Core Team
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

#include <Startup.h>

/* defined in GNOME GUI */

void Rgnome_Suicide(char *s);
void Rgnome_ShowMessage(char *s);
void Rgnome_Busy(int which);
void Rgnome_CleanUp(SA_TYPE saveact, int status, int runLast);
int  Rgnome_ShowFiles(int nfile, char **file, char **headers, char *wtitle,
		      Rboolean del, char *pager);
int  Rgnome_ChooseFile(int new, char *buf, int len);

void Rgnome_StartConsole(Rboolean OpenConsole);
int  Rgnome_ReadConsole(char *prompt, unsigned char *buf, int len, 
			int addtohistory);
void Rgnome_WriteConsole(char *buf, int len);
void Rgnome_ResetConsole(void);
void Rgnome_FlushConsole(void);
void Rgnome_ClearerrConsole(void);
void Rgnome_loadhistory(SEXP call, SEXP op, SEXP args, SEXP env);
void Rgnome_savehistory(SEXP call, SEXP op, SEXP args, SEXP env);

/* From elsewhere */

void R_setStartTime(void); /* in sys-unix.c */
void fpu_setup(Rboolean);  /* in sys-unix.c */

/* src/unix/devUI.h */

extern void (*ptr_R_Suicide)(char *);
extern void (*ptr_R_ShowMessage)();
extern int  (*ptr_R_ReadConsole)(char *, unsigned char *, int, int);
extern void (*ptr_R_WriteConsole)(char *, int);
extern void (*ptr_R_ResetConsole)();
extern void (*ptr_R_FlushConsole)();
extern void (*ptr_R_ClearerrConsole)();
extern void (*ptr_R_Busy)(int);
extern void (*ptr_R_CleanUp)(SA_TYPE, int, int);
extern int  (*ptr_R_ShowFiles)(int, char **, char **, char *, Rboolean, char *);
extern int  (*ptr_R_ChooseFile)(int, char *, int);
extern void (*ptr_R_loadhistory)(SEXP, SEXP, SEXP, SEXP);
extern void (*ptr_R_savehistory)(SEXP, SEXP, SEXP, SEXP);
extern int (*R_timeout_handler)();
extern long R_timeout_val;
