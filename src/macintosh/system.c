/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--1999  Tiki Wan, Ross Ihaka
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

/*
 *  SYSTEM DEPENDENT CODE
 *
 *  This source file contains the platform dependent code for
 *  the Macintosh port of R.
 *
 *  The first group of functions is concerned with reading and
 *  writing to the system console.
 *
 *    int   R_ReadConsole(char *prompt, char *buf, int buflen, int hist)
 *
 *  This function prints the given prompt at the console and then
 *  does a gets(3)-like operation, transfering up to "buflen" characters
 *  into the buffer "buf".  The last two characters are set to "\n\0"
 *  to preserve sanity.	 If "hist" is non-zero, then the line is added
 *  to any command history which is being maintained.  Note that this
 *  is one natural place from which to run an event loop.
 *
 *    void  R_WriteConsole(char *buf, int buflen)
 *
 *  This function writes the given buffer out to the console.  No
 *  special actions are required.  Under Unix the characters are
 *  just appended to stdout.
 *
 *    void  R_ResetConsole(void)
 *
 *  This function is called when the system is reset after an error.
 *  It probably isn't really needed.
 *
 *    void  R_FlushConsole(void)
 *
 *  This called to flush any output to the system console.  Under Unix
 *  this is just fflush(stdout).  Other systems may not need this.
 *
 *    void  R_ClearerrConsole(void)
 *
 *  This function clears any errors associated with reading from the
 *  console.  In Unix is is used to clear any EOF condition associated
 *  with stdin.
 *
 *    void  R_Suicide(char *msg)
 *
 *  This function displays the given message and the causes R to
 *  die immediately.  It is used for non-recoverable errors such as
 *  not having enough memory to launch etc.  The phrase "dialog box"
 *  springs to mind for non-unix platforms.
 *
 *    void  R_Busy(int which)
 *
 *  This function invokes actions (such as change of cursor) when
 *  R embarks on an extended computation (which=1) and when such a
 *  state terminates (which=0).
 *
 *    void  R_CleanUp(int ask)
 *
 *  This function invokes any actions which occur at system termination.
 *
 *    char* R_ExpandFileName(char *s)
 *
 *  This is a utility function which can be used to expand special
 *  characters in file names.  In Unix it's sole function is to expand
 *  and "~"s which occur in filenames (and then only when the readline
 *  library is available.  The minimal action is to return the argument
 *  unaltered.
 *
 *    void  R_InitialData(void)
 *    FILE* R_OpenInitFile(void)
 *    FILE* R_OpenLibraryFile(char *file)
 *    FILE* R_OpenSysInitFile(void)
 *
 *  The following two functions save and restore the user's global
 *  environment.  The system specific aspect of this what files
 *  are used for this.
 *
 *    void  R_RestoreGlobalEnv(void)
 *    void  R_SaveGlobalEnv(void)
 *
 *  Platform dependent functions.
 *
 *    SEXP  do_interactive(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP  do_machine(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP  do_proctime(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP  do_quit(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP  do_system(SEXP call, SEXP op, SEXP args, SEXP rho)
 */

#include "Defn.h"
#include "Fileio.h"
#include "Graphics.h"
#include "devMacintosh.h"

static int DefaultSaveAction = 0;
static int DefaultRestoreAction = 1;
static int LoadSiteFile = 1;
static int LoadInitFile = 1;
static int DebugInitFile = 0;

	/*--- I/O Support Code ---*/

	/* These routines provide hooks for supporting console I/O.
	 * Under raw Unix these routines simply provide a
	 * connection to the stdio library.
	 * Under a Motif interface the routines would be
	 * considerably more complex.
	 */


	/* Fill a text buffer with user typed console input. */

int R_ReadConsole(char *prompt, char *buf, int len, int addtohistory)
{
  /* Fill a text buffer with user typed console input. */
  /* ... */
}

	/* Write a text buffer to the console. */
	/* All system output is filtered through this routine. */

void R_WriteConsole(char *buf, int len)
{
  /* Write a text buffer to the console. */
  /* ... */
}


	/* Indicate that input is coming from the console */
	/* No longer used ??? */

void R_ResetConsole()
{
}

	/* Make sure that pending output is flushed */
	/* Unneeded for the Macintosh */

void R_FlushConsole()
{
}


	/* Clear Console EOF */
	/* Unneeded for the Macintosh */

void R_ClearerrConsole()
{
}


	/*--- File Handling Code ---*/


	/* Tab induced filename expansion */
	/* Unimplemented for the Macintosh */

char *R_ExpandFileName(char *s)
{
    /* return the string unmodified */
    return s;
}


	/* RHOME is the Folder where the R binary resides */

FILE *R_OpenLibraryFile(char *file)
{
    /* This code finds where the R application was invoked */
    /* and descends from there to "library:base:R and opens */
    /* the specified file within that directory.  It returns */
    /* the resulting file pointer. */

    /* ... */
}

FILE *R_OpenSysInitFile(void)
{
    /* This code finds where the R application was invoked */
    /* and descends from there to the folder "library:base:R" */
    /* and opens the file "Rprofile" within that directory. */
    /* It returns the resulting file pointer. */
}

FILE *R_OpenSiteFile(void)
{
    /* This code finds where the R application was invoked */
    /* and descends from there to the folder "etc" and opens */
    /* the file "Rprofile" within that directory.  It returns */
    /* the resulting file pointer. */

    /* ... */
}

FILE *R_OpenInitFile(void)
{
    /* This code attempts to open the file ".Rprofile" in the */
    /* current folder and returns the resulting file pointer. */
    /* Does thos make sense on the Mac.  Probably not. */
    return NULL;
}


	/*--- Initialization Code ---*/

	/* NOTE: The timing code below will have to be adpated */
	/* to use the macintosh specific timing code. */

int main(int ac, char **av)
{
    int value;
    char *p;

    gc_inhibit_torture = 1;

    /* FIXME HERE: record the time at which the program started. */
    /* This is probably zero on the mac as we have direct */
    /* access to the number of ticks since process start */

    /* ... */

    /* FIXME HERE: Command line options are not available. */
    /* Application resources must be inspected here */
    /* and used to modify the compiled-in defaults. */
    /* Compare with the Unix code. */

    /* ... */

    /* Set up the file handling defaults. */

    R_Quiet = 0;
    R_Interactive = 1;		/* On the Mac we must be interactive */
    R_Consolefile = NULL;	/* We don't use a file for console input. */
    R_Outputfile = stdout;	/* We don't use a file for console output. */
    R_Sinkfile = NULL;		/* We begin writing to the console. */

    /* FIXME HERE: Initialize the floating point system for true IEEE. */
    /* I don't know what is required here.  We want to handle NaNs and */
    /* infinities.  R uses them. */

    /* ... */

    /* Call the real R main program (in ../main/main.c) */
    mainloop();
    return 0;
}

void R_InitialData(void)
{
    R_RestoreGlobalEnv();
}

	/* R_CleanUp is invoked at the end of the session to give */
	/* the user the option of saving their data.  If ask=1 the */
	/* user is asked their preference, if ask=2 the answer is */
	/* assumed to be "no" and if ask=3 the answer is assumed to */
	/* be "yes".  When R is being used non-interactively, and */
	/* ask=1, the value is changed to 3.  The philosophy is */
	/* that saving unwanted data is less bad than non saving */
	/* data that is wanted. */

void R_CleanUp(int ask)
{
    char buf[128];

    if( R_DirtyImage ) {
    qask:
	R_ClearerrConsole();
	R_FlushConsole();
	if(!isatty(0) && ask==1)
	    ask = DefaultSaveAction;

	if(ask == 1) {
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
    KillAllDevices();

    /* FIXME HERE: Reset the floating-point system here */

    /* ... */

    exit(0);
}

void R_Busy(int which)
{
    /* This can be used to gray out menus and change the */
    /* cursor (to a watch or equivalent) to indicate that an */
    /* extended computation is taking place. */
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
    if(DefaultRestoreAction) {
	if(!(fp = R_fopen(".RData","r"))) {
	    /* warning here perhaps */
	    return;
	}
	FRAME(R_GlobalEnv) = R_LoadFromFile(fp);
	if(!R_Quiet)
	    Rprintf("[Previously saved workspace restored]\n\n");
    }
}


	/*--- Platform Dependent Functions ---*/


SEXP do_proctime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = allocVector(REALSXP, 5);
    /* FIXME HERE: elapsed should be  the number of seconds since startup */
    double elapsed = 0; 
    REAL(ans)[0] = elapsed;
    REAL(ans)[1] = 0;
    REAL(ans)[2] = elapsed;
    REAL(ans)[3] = 0;
    REAL(ans)[4] = 0;
    return ans;
}

SEXP do_getenv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    errorcall(call, "There is no environment on the Macintosh\n");
    return R_NilValue;
}

SEXP do_machine(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return mkString("Macintosh");
}

SEXP do_system(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    errorcall(call, "There is no \"system\" function on the Macintosh\n");
    return R_NilValue;
}

SEXP do_interactive(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval = allocVector(LGLSXP, 1);
    LOGICAL(rval)[0] = 1;
    return rval;
}

/* DON'T FIXME HERE:  This function is invoked when the user types q() */
/* It should not pop up a dialog, but should query the user */
/* in the console window.  Use a popup dialog when quite is selected */
/* from the file menu. */

SEXP do_quit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    char *tmp;
    int ask=0;

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
}

void R_Suicide(char *s)
{
    /* FIXME HERE: This should pop up a dialog box with the given */
    /* error message displayed, and the quit when the user hits the */
    /* OK button. */

    /* printf("Fatal error: %s\n", s); */
    R_CleanUp(2);
    /* 2 means don't save anything and it's an unrecoverable abort */
}


	/* Declarations to keep f77 happy */

int MAIN_()  {return 0;}
int MAIN__() {return 0;}
int __main() {return 0;}
