/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-1999   Robert Gentleman, Ross Ihaka
 *                            and the R Development Core Team
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

         /* See system.txt for a description of functions */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "Fileio.h"
#include "Graphics.h"		/* KillAllDevices() [nothing else?] */

#include "devX11.h"

#include "Startup.h"

#ifdef HAVE_LIBREADLINE
#include <readline/readline.h>
#ifdef HAVE_READLINE_HISTORY_H
#include <readline/history.h>
#endif
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* necessary for some (older, i.e., ~ <= 1997) Linuxen, and apparently
   also some AIX systems.
   */
#ifndef FD_SET
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#endif

void fpu_setup(int);     /* in sys-unix.c */


static int UsingReadline = 1;
int SaveAction = SA_SAVEASK;
int RestoreAction = SA_RESTORE;
int LoadSiteFile = True;
int LoadInitFile = True;
int DebugInitFile = False;

/*
 *  1) FATAL MESSAGES AT STARTUP
 */

void R_Suicide(char *s)
{
    REprintf("Fatal error: %s\n", s);
    R_CleanUp(SA_SUICIDE, 2, 0);
}

/*
 *  2. CONSOLE I/O
 */



	/*--- I/O Support Code ---*/

	/* These routines provide hooks for supporting console I/O.
	 * Under raw Unix these routines simply provide a
	 * connection to the stdio library.
	 * Under a Motif interface the routines would be
	 * considerably more complex.
	 */


/* block on select until either stdin or X11 connection is ready to read
   (return 1 if X11 connection ready to read, 2 if stdin ready to read)
   */

#define XActivity 1
#define StdinActivity 2

static int waitForActivity()
{
    int maxfd;
    fd_set readMask;
    int stdinfd = fileno(stdin);
    int connectionfd = X11ConnectionNumber();

    FD_ZERO(&readMask);
    FD_SET(stdinfd, &readMask);
    maxfd = stdinfd;
    if (connectionfd > 0) {
	FD_SET(connectionfd, &readMask);
	if (connectionfd > stdinfd)
	    maxfd = connectionfd;
    }
    select(maxfd+1, &readMask, NULL, NULL, NULL);

    if (connectionfd > 0)
	if (FD_ISSET(connectionfd, &readMask))
	    return XActivity;
    if (FD_ISSET(stdinfd, &readMask))
	return StdinActivity;
    return 0;			/* for -Wall*/
}


#ifdef HAVE_LIBREADLINE
	/* callback for rl_callback_read_char */

static int readline_gotaline;
static int readline_addtohistory;
static int readline_len;
static int readline_eof;
static unsigned char *readline_buf;

static void readline_handler(unsigned char *line)
{
    int l;
    rl_callback_handler_remove();
    if ((readline_eof = !line)) /* Yes, I don't mean ==...*/
	return;
    if (line[0]) {
#ifdef HAVE_READLINE_HISTORY_H
	if (strlen(line) && readline_addtohistory)
	    add_history(line);
#endif
	l = (((readline_len-2) > strlen(line))?
	     strlen(line): (readline_len-2));
	strncpy(readline_buf, line, l);
	readline_buf[l] = '\n';
	readline_buf[l+1] = '\0';
    }
    else {
	readline_buf[0] = '\n';
	readline_buf[1] = '\0';
    }
    readline_gotaline = 1;
}
#endif

	/* Fill a text buffer with user typed console input. */

int R_ReadConsole(char *prompt, unsigned char *buf, int len, int addtohistory)
{
    if(!R_Interactive) {
	if (!R_Slave)
	    fputs(prompt, stdout);
	if (fgets(buf, len, stdin) == NULL)
	    return 0;
	if (!R_Slave)
	    fputs(buf, stdout);
	return 1;
    }
    else {
#ifdef HAVE_LIBREADLINE
	if (UsingReadline) {
	    readline_gotaline = 0;
	    readline_buf = buf;
	    readline_addtohistory = addtohistory;
	    readline_len = len;
	    readline_eof = 0;
	    rl_callback_handler_install(prompt, readline_handler);
	}
	else
#endif
	{
	    fputs(prompt, stdout);
	    fflush(stdout);
	}

	for (;;) {
	    int what = waitForActivity();
	    switch (what) {
	    case XActivity:
		R_ProcessEvents();
		break;
	    case StdinActivity:
#ifdef HAVE_LIBREADLINE
		if (UsingReadline) {
		    rl_callback_read_char();
		    if (readline_eof)
			return 0;
		    if (readline_gotaline)
			return 1;
		}
		else
#endif
		{
		    if(fgets(buf, len, stdin) == NULL)
			return 0;
		    else
			return 1;
		}
	    }
	}
    }
}

	/* Write a text buffer to the console. */
	/* All system output is filtered through this routine. */

void R_WriteConsole(char *buf, int len)
{
    printf("%s", buf);
}


	/* Indicate that input is coming from the console */

void R_ResetConsole()
{
}


	/* Stdio support to ensure the console file buffer is flushed */

void R_FlushConsole()
{
    fflush(stdin);
}

	/* Reset stdin if the user types EOF on the console. */

void R_ClearerrConsole()
{
    clearerr(stdin);
}

/*
 *  3) ACTIONS DURING (LONG) COMPUTATIONS
 */

void R_Busy(int which)
{
}

/*
 *  4) INITIALIZATION AND TERMINATION ACTIONS
 */

/*
   R_CleanUp is invoked at the end of the session to give the user the
   option of saving their data.
   If ask == SA_SAVEASK the user should be asked if possible (and this
   option should not occur in non-interactive use).
   If ask = SA_SAVE or SA_NOSAVE the decision is known.
   If ask = SA_DEFAULT use the SaveAction set at startup.
   In all these cases run .Last() unless quitting is cancelled.
   If ask = SA_SUICIDE, no save, no .Last, possibly other things.
 */

void R_dot_Last(void);		/* in main.c */

void R_CleanUp(int saveact, int status, int runLast)
{
    char buf[128];

    if(saveact == SA_DEFAULT) /* The normal case apart from R_Suicide */
	saveact = SaveAction;

    if(saveact == SA_SAVEASK) {
	if(R_Interactive) {
	qask:
	    R_ClearerrConsole();
	    R_FlushConsole();
	    R_ReadConsole("Save workspace image? [y/n/c]: ", buf, 128, 0);
	    switch (buf[0]) {
	    case 'y':
	    case 'Y':
		saveact = SA_SAVE;
		break;
	    case 'n':
	    case 'N':
		saveact = SA_NOSAVE;
		break;
	    case 'c':
	    case 'C':
		jump_to_toplevel();
		break;
	    default:
		goto qask;
	    }
	} else
	    saveact = SaveAction;
    }
    switch (saveact) {
    case SA_SAVE:
	if(runLast) R_dot_Last();
	if(R_DirtyImage) R_SaveGlobalEnv();
#ifdef HAVE_LIBREADLINE
#ifdef HAVE_READLINE_HISTORY_H
	if(R_Interactive && UsingReadline)
	    stifle_history(R_HistorySize);
	write_history(R_HistoryFile);
#endif
#endif
	break;
    case SA_NOSAVE:
	if(runLast) R_dot_Last();
	break;
    case SA_SUICIDE:
    default:
        break;
    }
    CleanEd();
    KillAllDevices();
    fpu_setup(0);

    exit(status);
}

/*
 *  7) PLATFORM DEPENDENT FUNCTIONS
 */

    /*
       This function can be used to display the named files with the
       given titles and overall title.  On GUI platforms we could
       use a read-only window to display the result.  Here we just
       make up a temporary file and invoke a pager on it.
    */

    /*
     *     nfile   = number of files
     *     file    = array of filenames
     *     headers = the `headers' args of file.show. Printed before each file.
     *     wtitle  = title for window: the `title' arg of file.show
     *     del     = flag for whether files should be deleted after use
     *     pager   = pager to be used.
     */

int R_ShowFiles(int nfile, char **file, char **headers, char *wtitle,
		int del, char *pager)
{
    int c, i, res;
    char *filename;
    FILE *fp, *tfp;
    char buf[1024];

    if (nfile > 0) {
        if (pager == NULL || strlen(pager) == 0) pager = "more";
	filename = tmpnam(NULL);
        if ((tfp = fopen(filename, "w")) != NULL) {
	    for(i = 0; i < nfile; i++) {
		if (headers[i] && *headers[i])
		    fprintf(tfp, "%s\n\n", headers[i]);
		if ((fp = R_fopen(R_ExpandFileName(file[i]), "r"))
		    != NULL) {
		    while ((c = fgetc(fp)) != EOF)
			fputc(c, tfp);
		    fprintf(tfp, "\n");
		    fclose(fp);
		    if(del)
			unlink(R_ExpandFileName(file[i]));
		}
		else
		    fprintf(tfp, "NO FILE %s\n\n", file[i]);
	    }
	    fclose(tfp);
	}
	sprintf(buf, "%s < %s", pager, filename);
	res = system(buf);
	unlink(filename);
	return (res != 0);
    }
    return 1;
}


    /*
       Prompt the user for a file name.  Return the length of
       the name typed.  On Gui platforms, this should bring up
       a dialog box so a user can choose files that way.
    */

int R_ChooseFile(int new, char *buf, int len)
{
    int namelen;
    char *bufp;
    R_ReadConsole("Enter file name: ", buf, len, 0);
    namelen = strlen(buf);
    bufp = &buf[namelen - 1];
    while (bufp >= buf && isspace((int)*bufp))
	*bufp-- = '\0';
    return strlen(buf);
}


	/*--- Initialization Code ---*/


void R_ShowMessage(char *s)
{
    REprintf(s);
}

void R_setStartTime(); /* in sys-unix.c */

int main(int ac, char **av)
{
    int value, ierr;
    char *p, msg[1024];
    structRstart rstart;
    Rstart Rp = &rstart;

#ifdef HAVE_TIMES
    R_setStartTime();
#endif

    R_DefParams(Rp);
    R_SizeFromEnv(Rp);
    /* Store the command line arguments before they are processed
       by the R option handler. These are stored in Rp and then moved 
       to the global variable CommandLineArgs in R_SetParams.
     */
    R_set_command_line_arguments(ac, av, Rp);

    R_common_command_line(&ac, av, Rp);
    while (--ac) {
	if (**++av == '-') {
	    if(!strcmp(*av, "--no-readline")) {
		UsingReadline = 0;
	    } else {
		sprintf(msg, "WARNING: unknown option %s\n", *av);
		R_ShowMessage(msg);
		break;
	    }
	} else {
	    sprintf(msg, "ARGUMENT '%s' __ignored__\n", *av);
	    R_ShowMessage(msg);
	}
    }
    R_SetParams(Rp);

    /* On Unix the console is a file; we just use stdio to write on it */

    R_Interactive = isatty(0);
    R_Consolefile = stdout;
    R_Outputfile = stdout;
    R_Sinkfile = NULL;
    if((R_Home = R_HomeDir()) == NULL) {
	R_Suicide("R home directory is not defined");
    }
/*
 *  Since users' expectations for save/no-save will differ, we decided
 *  that they should be forced to specify in the non-interactive case.
 */
    if (!R_Interactive && SaveAction != SA_SAVE && SaveAction != SA_NOSAVE)
	R_Suicide("you must specify `--save', `--no-save' or `--vanilla'");

    if ((R_HistoryFile = getenv("R_HISTFILE")) == NULL)
	R_HistoryFile = ".Rhistory";
    R_HistorySize = 512;
    if ((p = getenv("R_HISTSIZE"))) {
	value = Decode2Long(p, &ierr);
	if (ierr != 0 || value < 0)
	    REprintf("WARNING: invalid R_HISTSIZE ignored;");
	else
	    R_HistorySize = value;
    }

#ifdef HAVE_LIBREADLINE
#ifdef HAVE_READLINE_HISTORY_H
    if(R_Interactive && UsingReadline) {
	read_history(R_HistoryFile);
    }
#endif
#endif
    fpu_setup(1);

    mainloop();
    /*++++++  in ../main/main.c */
    return 0;
}



	/* Declarations to keep f77 happy */

int MAIN_()  {return 0;}
int MAIN__() {return 0;}
int __main() {return 0;}


