/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "Defn.h"
#include "Fileio.h"

#ifdef HAVE_LIBREADLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

	/*--- I / O -- S u p p o r t -- C o d e ---*/

	/* These routines provide hooks for supporting console */
	/* I/O.	 Under raw Unix these routines simply provide a */
	/* connection to the stdio library.  Under a Motif */
	/* interface the routines would be considerably more */
	/* complex. */


	/* Fill a text buffer with user typed console input. */

static int R_UsingReadline = 1;

int R_ReadConsole(char *prompt, char *buf, int len, int addtohistory)
{
	int l;
	char *rline;

	if(!isatty(0)) {
		if(!R_Quiet) fputs(prompt, stdout);
		if (fgets(buf, len, stdin) == NULL)
			return 0;
		if(!R_Quiet) fputs(buf,stdout);
		return 1;
	}
#ifdef HAVE_LIBREADLINE
	else if(R_UsingReadline) {
		rline = readline(prompt);
		if (rline) {
			if (strlen(rline) && addtohistory)
				add_history(rline);
			l = (((len-2) > strlen(rline))?strlen(rline):(len-2));
			strncpy(buf, rline, l);
			buf[l] = '\n';
			buf[l+1]= '\0';
			free(rline);
			return 1;
		}
		else return 0;
	}
#endif
	else {
		if(fgets(buf, len, stdin) == NULL)
			return 0;
		else
			return 1;
	}
}

	/* Write a text buffer to the console. */
	/* All system output is filtered through this routine. */

void R_WriteConsole(char *buf, int len)
{
	printf("%s", buf);
}


	/* Indicate that input is redirected from the console */

void R_ResetConsole()
{
	R_Console = 1;
}


	/* This is stdio support to ensure that console file buffers */
	/* are flushed. */

void R_FlushConsole()
{
	if (R_Console == 1)
		fflush(stdin);
}


	/* This is stdio support to reset if the used types EOF */
	/* on the console. */

void R_ClearerrConsole()
{
	if (R_Console == 1)
		clearerr(stdin);
}


	/*--- F i l e	 H an d l i n g	   C o d e ---*/

	/* Note: Readline has code to do expansion of ~ */

#ifdef HAVE_LIBREADLINE
char *tilde_expand(char*);

char *R_ExpandFileName(char *s)
{
	return tilde_expand(s);
}
#else
char *R_ExpandFileName(char *s)
{
	return s;
}
#endif

FILE *R_OpenLibraryFile(char *file)
{
	char buf[256], *home;
	FILE *fp;

	if((home = getenv("RHOME")) == NULL)
		return NULL;
	sprintf(buf, "%s/library/base/R/%s", home, file);
	fp = R_fopen(buf,"r");
	return fp;
}

FILE *R_OpenSysInitFile(void)
{
	char buf[256];
	FILE *fp;

	sprintf(buf, "%s/library/base/R/Rprofile", getenv("RHOME"));
	fp = R_fopen(buf, "r");
	return fp;
}

FILE *R_OpenInitFile(void)
{
	char buf[256];
	FILE *fp;

	fp = NULL;

	if(fp = R_fopen(".Rprofile", "r"))
		return fp;

	sprintf(buf, "%s/.Rprofile", getenv("HOME"));
	if(fp = R_fopen(buf, "r"))
		return fp;

	return fp;
}


	/*--- I n i t i a l i z a t i o n    C o d e ---*/

#ifdef HAVE_TIMES
#include <sys/times.h>

static clock_t StartTime;
static struct tms timeinfo;
#endif

#ifdef __FreeBSD__
#include <floatingpoint.h>
#endif

#ifdef linux
#include <fpu_control.h>
#endif

int main(int ac, char **av)
{
	int value;
	char *p;

#ifdef HAVE_TIMES
	StartTime = times(&timeinfo);
#endif
	R_Quiet = 0;

	while(--ac) {
	 if(**++av == '-') {
		switch((*av)[1]) {
		case 'v':
			if((*av)[2] == '\0') {
				ac--; av++; p = *av;
			}
			else p = &(*av)[2];
			value = strtol(p, &p, 10);
			if(*p) goto badargs;
			if(value < 1 || value > 1000)
			 REprintf("warning: invalid vector heap size ignored\n");
			else
			  R_VSize = value * 1048576; /* 1 MByte := 2^20 Bytes*/
			break;
		case 'n':
			if((*av)[2] == '\0') {
				ac--; av++; p = *av;
			}
			else p = &(*av)[2];
			value = strtol(p, &p, 10);
			if(*p) goto badargs;
			if(value < R_NSize || value > 1000000)
			 REprintf("warning: invalid language heap size ignored\n");
			else
			 R_NSize = value;
			break;
		case 'q':
			R_Quiet = 1;
			break;
		default:
			REprintf("warning: unknown option %s\n", *av);
			break;
		}
	 }
	 else {
		printf("ARGUMENT %s\n", *av);
	 }

	}

	/* On Unix the console is a file; we just use stdio to write on it */

	R_Interactive = isatty(0);
	R_Consolefile = stdout;
	R_Outputfile = stdout;
	R_Sinkfile = NULL;

#ifdef __FreeBSD__
	fpsetmask(0);
#endif

#ifdef linux
	__setfpucw(_FPU_IEEE);
#endif

#ifdef HAVE_LIBREADLINE
	if(isatty(0))
		read_history(".Rhistory");
#endif
	mainloop();
	/*++++++  in ../main/main.c */
	return 0;

badargs:
	REprintf("invalid argument passed to R\n");
	exit(1);
}

void R_InitialData(void)
{
	R_RestoreGlobalEnv();
}

void R_CleanUp(int ask)
{
	char buf[128];

	if( R_DirtyImage ) {
qask:
		R_Console = 1;
		R_ClearerrConsole();
		R_FlushConsole();
		if(!isatty(0) && ask==1)
			ask = 3;

		if( ask==1 ) {
			R_ReadConsole("Save workspace image? [y/n/c]: ",
				buf, 128, 0);
		}
		else if(ask == 2)
			buf[0] = 'n';
		else if (ask == 3)
			buf[0] = 'y';

		switch (buf[0]) {
		case 'y':
		case 'Y':
			R_SaveGlobalEnv();
#ifdef HAVE_LIBREADLINE
			if(isatty(0))
				write_history(".Rhistory");
#endif
			break;
		case 'n':
		case 'N':
			break;
		case 'c':
		case 'C':
			jump_to_toplevel();
			break;
		default:
			goto qask;
		}
	}
	KillDevice();

#ifdef __FreeBSD__
	fpsetmask(~0);
#endif

#ifdef linux
	__setfpucw(_FPU_DEFAULT);
#endif

	exit(0);
}

void R_Busy(int which)
{
}

	/* Saving and Restoring the Global Environment */

void R_SaveGlobalEnv(void)
{
	FILE *fp = R_fopen(".RData", "w");
	if (!fp)
		error("can't save data -- unable to open ./.RData\n");
	R_SaveToFile(FRAME(R_GlobalEnv), fp, 0);
	fclose(fp);
}

void R_RestoreGlobalEnv(void)
{
	FILE *fp;
	if(!(fp = R_fopen(".RData","r"))) {
		/* warning here perhaps */
		return;
	}
	if(!R_Quiet)
		Rprintf("[Previously saved workspace restored]\n\n");
	FRAME(R_GlobalEnv) = R_LoadFromFile(fp);
}

	/*--- P l a t f o r m -- D e p e n d e n t -- F u n c t i o n s ---*/

#ifdef HAVE_TIMES
#ifndef CLK_TCK
/* this is in ticks/second, generally 60 on BSD style Unix, 100? on SysV */
#ifdef HZ
#define CLK_TCK HZ
#else
#define CLK_TCK	60
#endif

#endif

SEXP do_proctime(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP ans;
	clock_t elapsed;
	elapsed = (times(&timeinfo) - StartTime) / (double)CLK_TCK;
	ans = allocVector(REALSXP, 5);
	REAL(ans)[0] = timeinfo.tms_utime / (double)CLK_TCK;
	REAL(ans)[1] = timeinfo.tms_stime / (double)CLK_TCK;
	REAL(ans)[2] = elapsed;
	REAL(ans)[3] = timeinfo.tms_cutime / (double)CLK_TCK;
	REAL(ans)[4] = timeinfo.tms_cstime / (double)CLK_TCK;
	return ans;
}
#endif

SEXP do_machine(SEXP call, SEXP op, SEXP args, SEXP env)
{
	return mkString("Unix");
}

SEXP do_system(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	FILE *fp;
	char *x = "r", buf[120];
	int read, i, j;
	SEXP tlist = R_NilValue, tchar, rval;

	checkArity(op, args);
	if (!isString(CAR(args)))
		errorcall(call, "character argument expected\n");
	if (isLogical(CADR(args)))
		read = INTEGER(CADR(args))[0];
	if (read) {
		PROTECT(tlist);
		fp = popen(CHAR(STRING(CAR(args))[0]), x);
		for (i = 0; fgets(buf, 120, fp); i++) {
			read = strlen(buf);
			buf[read - 1] = '\0';
			tchar = mkChar(buf);
			UNPROTECT(1);
			PROTECT(tlist = CONS(tchar, tlist));
		}
		pclose(fp);
		rval = allocVector(STRSXP, i);;
		for (j = (i - 1); j >= 0; j--) {
			STRING(rval)[j] = CAR(tlist);
			tlist = CDR(tlist);
		}
		UNPROTECT(1);
		return (rval);
	}
	else {
		tlist = allocVector(INTSXP, 1);
		INTEGER(tlist)[0] = system(CHAR(STRING(CAR(args))[0]));
		R_Visible = 0;
		return tlist;
	}
}

SEXP do_interactive(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP rval;

	rval=allocVector(LGLSXP, 1);
	if( isatty(0) )
		LOGICAL(rval)[0]=1;
	else
		LOGICAL(rval)[0]=0;
	return rval;
}

SEXP do_quit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	char *tmp;
	int ask;

	if(R_BrowseLevel) {
		warning("can't quit from browser\n");
		return R_NilValue;
	}
	if( !isString(CAR(args)) )
		errorcall(call,"one of \"yes\", \"no\" or \"ask\" expected.\n");
	tmp = CHAR(STRING(CAR(args))[0]);
	if( !strcmp(tmp,"ask") )
		ask=1;
	else if( !strcmp(tmp,"no") )
		ask=2;
	else if( !strcmp(tmp,"yes") )
		ask=3;
	else
		errorcall(call,"unrecognized value of ask\n");
	R_CleanUp(ask);
	exit(0);
	/*NOTREACHED*/
}

void suicide(char *s)
{
	REprintf("Fatal error: %s\n", s);
	R_CleanUp(2);
	/*	 2 means don't save anything and it's an unrecoverable abort */
}


	/* Declarations to keep f77 happy */

int MAIN_()  {return 0;}
int MAIN__() {return 0;}
int __main() {return 0;}
