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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 *  SYSTEM DEPENDENT CODE
 *
 *  This source file contains the platform dependent code for
 *  the Unix (reference) port of R.
 *
 *
 *  1) FATAL MESSAGES AT STARTUP
 *
 *    void  R_Suicide(char *msg)
 *
 *  This function displays the given message and the causes R to
 *  die immediately.  It is used for non-recoverable errors such as
 *  not having enough memory to launch etc.  The phrase "dialog box"
 *  springs to mind for non-Unix platforms.
 *
 *
 *  2. CONSOLE I/O
 *
 *  The first group of functions is concerned with reading and
 *  writing to the system console.
 *
 *    int   R_ReadConsole(char *prompt, char *buf, int buflen, int hist)
 *
 *  This function prints the given prompt at the console and then
 *  does a gets(3)-like operation, transferring up to "buflen" characters
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
 *
 *  3) ACTIONS DURING (LONG) COMPUTATIONS
 *
 *    void  R_Busy(int which)
 *
 *  This function invokes actions (such as change of cursor) when
 *  R embarks on an extended computation (which=1) and when such a
 *  state terminates (which=0).
 *
 *
 *  4) INITIALIZATION AND TERMINATION ACTIONS
 *
 *    void  R_InitialData(void)
 *    FILE* R_OpenInitFile(void)
 *    FILE* R_OpenLibraryFile(char *file)
 *    FILE* R_OpenSysInitFile(void)
 *    FILE* R_OpenSiteFile()
 *
 *  These functions load the initial system and user data into R.
 *
 *    void  R_RestoreGlobalEnv(void)
 *    void  R_SaveGlobalEnv(void)
 *
 *  These functions save and restore the user's global environment.
 *  The system specific aspect of this is what files are used.
 *
 *    void  R_CleanUp(int saveact)

 *  This function invokes any actions which occur at system termination.
 *
 *
 *  5) FILESYSTEM INTERACTION
 *
 *    int FileExists(char *file)
 *
 *  This function returns 1 if the named file exists and 0 otherwise.
 *  On Unix this is just an interface to "stat".
 *
 *    int R_HiddenFile(char *file)
 *
 *  This function returns 1 if the named file is "hidden".  In Unix,
 *  this is the case if the file name begins with a '.'.  On the Mac
 *  a file is hidden if the file name ends in '\r'.
 *
 *    int R_ShowFiles(int nfile, char **file, char **headers, char *wtitle,
 *		      int del, char *pager)
 *
 *  This function is used to display the contents of files.  On (raw)
 *  Unix this means invoking a pager on the file.  On Gui-based platforms
 *  the file would probably be displayed in a window with the given
 *  title.
 *
 *    char* R_ExpandFileName(char *s)
 *
 *  This is a utility function which can be used to expand special
 *  characters in file names.  In Unix it's sole function is to expand
 *  and "~"s which occur in filenames (and then only when the readline
 *  library is available.  The minimal action is to return the argument
 *  unaltered.
 *
 *    FILE *R_fopen(const char *filename, const char *mode);
 *
 *  This is a (probably unnecessary) wrapper function for ``fopen''.
 *
 *
 *  6) SYSTEM INFORMATION
 *
 *    char *R_HomeDir(void)
 *
 *  Get the R ``home directory'' as a string.
 *
 *
 *  7) PLATFORM DEPENDENT FUNCTIONS
 *
 *    SEXP do_getenv(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP do_interactive(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP do_machine(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP do_proctime(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP do_system(SEXP call, SEXP op, SEXP args, SEXP rho)
 *
 */

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Defn.h"
#include "Fileio.h"
#include "Graphics.h"		/* KillAllDevices() [nothing else?] */

#include "devX11.h"

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

static int UsingReadline = 1;
static int SaveAction = SA_DEFAULT;
static int RestoreAction = SA_RESTORE;
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
		ProcessEvents();
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


	/*--- File Handling Code ---*/


#ifdef HAVE_LIBREADLINE
char *tilde_expand(char*);

char *R_ExpandFileName(char *s)
{
    return( tilde_expand(s) );
}
#else
static HaveHOME=-1;
static UserHOME[PATH_MAX]
static newFileName[PATH_MAX]
char *R_ExpandFileName(char *s)
{
    char *p;
    
    if(s[0] != '~') return s;
    if(HaveHOME < 0) {
	p = getenv("HOME");
	if(p && strlen(p)) {
	    strcpy(UserHOME, p);
	    strcat(UserHOME, FILESEP);
	    HaveHOME = 1;
	} else
	    HaveHOME = 0;
    }
    if(HaveHOME > 0){
	strcpy(newFileName, UserHOME);
	strcat(newFileName, s);
	return newFileName;
    } else return s;
}
#endif

FILE *R_fopen(const char *filename, const char *mode)
{
    return( fopen(filename, mode) );
}

FILE *R_OpenLibraryFile(char *file)
{
    char buf[256];
    FILE *fp;

    sprintf(buf, "%s/library/base/R/%s", R_Home, file);
    fp = R_fopen(buf, "r");
    return fp;
}

FILE *R_OpenSysInitFile(void)
{
    char buf[256];
    FILE *fp;

    sprintf(buf, "%s/library/base/R/Rprofile", R_Home);
    fp = R_fopen(buf, "r");
    return fp;
}

FILE *R_OpenSiteFile(void)
{
    char buf[256];
    FILE *fp;

    fp = NULL;

    if (LoadSiteFile) {
	if ((fp = R_fopen(getenv("R_PROFILE"), "r")))
	    return fp;
	if ((fp = R_fopen(getenv("RPROFILE"), "r")))
	    return fp;
	sprintf(buf, "%s/etc/Rprofile", R_Home);
	if ((fp = R_fopen(buf, "r")))
	    return fp;
    }

    return fp;
}

FILE *R_OpenInitFile(void)
{
    char buf[256], *home;
    FILE *fp;

    fp = NULL;

    if (LoadInitFile) {
	if ((fp = R_fopen(".Rprofile", "r")))
	    return fp;
	if ((home = getenv("HOME")) == NULL)
	    return NULL;
	sprintf(buf, "%s/.Rprofile", home);
	if ((fp = R_fopen(buf, "r")))
	    return fp;
    }

    return fp;
}


	/*--- Initialization Code ---*/

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

#define Max_Nsize 20000000	/* must be < LONG_MAX (= 2^32 - 1 =)
				   2147483647 = 2.1e9 */
#define Max_Vsize (2048*Mega)	/* must be < LONG_MAX */

#define Min_Nsize 200000
#define Min_Vsize (2*Mega)

int main(int ac, char **av)
{
    int value, ierr;
    char *p, msg[1024];

    gc_inhibit_torture = 1;
#ifdef HAVE_TIMES
    StartTime = times(&timeinfo);
#endif
    R_Quiet = 0;

    if((p = getenv("R_VSIZE"))) {
	value = Decode2Long(p, &ierr);
	if(ierr != 0 || value > Max_Vsize || value < Min_Vsize)
	    REprintf("WARNING: invalid R_VSIZE ignored;");
	else
	    R_VSize = value;
    }
    if((p = getenv("R_NSIZE"))) {
	value = Decode2Long(p, &ierr);
	if(ierr != 0 || value > Max_Nsize || value < Min_Nsize)
	    REprintf("WARNING: invalid R_NSIZE ignored;");
	else
	    R_NSize = value;
    }

    while(--ac) {
	if(**++av == '-') {
	    if (!strcmp(*av, "--version")) {
		PrintVersion(msg);
		Rprintf(msg);
		exit(0);
	    }
	    else if(!strcmp(*av, "--save")) {
		SaveAction = SA_SAVE;
	    }
	    else if(!strcmp(*av, "--no-save")) {
		SaveAction = SA_NOSAVE;
	    }
	    else if(!strcmp(*av, "--restore")) {
		RestoreAction = SA_RESTORE;
	    }
	    else if(!strcmp(*av, "--no-restore")) {
		RestoreAction = SA_NORESTORE;
	    }
	    else if(!strcmp(*av, "--no-readline")) {
		UsingReadline = 0;
	    }
	    else if (!strcmp(*av, "--silent") ||
		     !strcmp(*av, "--quiet") ||
		     !strcmp(*av, "-q")) {
		R_Quiet = 1;
	    }
	    else if (!strcmp(*av, "--vanilla")) {
		SaveAction = SA_NOSAVE; /* --no-save */
		RestoreAction = SA_NORESTORE; /* --no-restore */
		LoadSiteFile = 0; /* --no-site-file */
		LoadInitFile = 0; /* --no-init-file */
	    }
	    else if (!strcmp(*av, "--verbose")) {
		R_Verbose = 1;
	    }
	    else if (!strcmp(*av, "--slave") ||
		     !strcmp(*av, "-s")) {
		R_Quiet = 1;
		R_Slave = 1;
		SaveAction = SA_NOSAVE;
	    }
	    else if (!strcmp(*av, "--no-site-file")) {
		LoadSiteFile = 0;
	    }
	    else if (!strcmp(*av, "--no-init-file")) {
		LoadInitFile = 0;
	    }
	    else if (!strcmp(*av, "--debug-init")) {
		DebugInitFile = 1;
	    }
	    else if (!strcmp(*av, "-save") ||
		     !strcmp(*av, "-nosave") ||
		     !strcmp(*av, "-restore") ||
		     !strcmp(*av, "-norestore") ||
		     !strcmp(*av, "-noreadline") ||
		     !strcmp(*av, "-quiet") ||
		     !strcmp(*av, "-V")) {
		REprintf("WARNING: option %s no longer supported\n", *av);
	    }
	    else if((value = (*av)[1] == 'v') || !strcmp(*av, "--vsize")) {
		if(value)
		    REprintf("WARNING: option `-v' is deprecated.  "
			     "Use `--vsize' instead.\n");
		if(!value || (*av)[2] == '\0') {
		    ac--; av++; p = *av;
		}
		else p = &(*av)[2];
		if (p == NULL) {
		    REprintf("WARNING: no vsize given");
		    break;
		}
		value = Decode2Long(p,&ierr);
		if(ierr) {
		    if(ierr < 0) goto badargs; /* if(*p) goto badargs; */
		    REprintf("--vsize %ld'%c': too large", value,
			     (ierr == 1)?'M':((ierr == 2)?'K':'k'));
		}
		if (value < 1000) {
		    REprintf("WARNING: vsize ridiculously low, Megabytes assumed\n");
		    value *= Mega;
		}
		if(value < Min_Vsize || value > Max_Vsize)
		    REprintf("WARNING: invalid v(ector heap)size '%d' ignored;"
			     "using default = %gM\n", value, R_VSize / Mega);
		else
		    R_VSize = value;
	    }
	    else if((value = (*av)[1] == 'n') || !strcmp(*av, "--nsize")) {
		if(value)
		    REprintf("WARNING: option `-n' is deprecated.  "
			     "Use `--nsize' instead.\n");
		if(!value || (*av)[2] == '\0') {
		    ac--; av++; p = *av;
		}
		else p = &(*av)[2];
		if (p == NULL) {
		    REprintf("WARNING: no nsize given");
		    break;
		}
		value = Decode2Long(p,&ierr);
		if(ierr) {
		    if(ierr < 0) goto badargs;
		    REprintf("--nsize %ld'%c': too large", value,
			     (ierr == 1)?'M':((ierr == 2)?'K':'k'));
		}
		if(value < Min_Nsize || value > Max_Nsize)
		    REprintf("WARNING: invalid language heap (n)size '%d' ignored,"
			     " using default = %d\n", value, R_NSize);
		else
		    R_NSize = value;
	    }
	    else {
		REprintf("WARNING: unknown option %s\n", *av);
		break;
	    }
	}
	else {
	    printf("ARGUMENT '%s' __ignored__\n", *av);
	}
    }

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

#ifdef __FreeBSD__
    fpsetmask(0);
#endif

#ifdef NEED___SETFPUCW
    __setfpucw(_FPU_IEEE);
#endif

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

void R_CleanUp(int saveact)
{
    char buf[128];

    if(saveact == SA_DEFAULT) /* The normal case apart from R_Suicide */
	saveact = SaveAction;

    if(saveact == SA_SAVEASK)
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
	}

    switch (saveact) {
    case SA_SAVE:
	R_dot_Last();
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
	R_dot_Last();
	break;
    case SA_SUICIDE:
    default:
    }
    CleanEd();
    KillAllDevices();

#ifdef __FreeBSD__
    fpsetmask(~0);
#endif

#ifdef NEED___SETFPUCW
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
    FILE *fp = R_fopen(".RData", "wb"); /* binary file */
    if (!fp)
	error("can't save data -- unable to open ./.RData\n");
    R_SaveToFile(FRAME(R_GlobalEnv), fp, 0);
    fclose(fp);
}

void R_RestoreGlobalEnv(void)
{
    FILE *fp;
    if(RestoreAction == SA_RESTORE) {
	if(!(fp = R_fopen(".RData", "rb"))) { /* binary file */
	    /* warning here perhaps */
	    return;
	}
	FRAME(R_GlobalEnv) = R_LoadFromFile(fp);
	if(!R_Quiet)
	    Rprintf("[Previously saved workspace restored]\n\n");
        fclose(fp);
    }
}


	/*--- Platform Dependent Functions ---*/


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
extern char ** environ;

SEXP do_getenv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, j;
    char *s;
    char **e;
    SEXP ans;

    checkArity(op, args);

    if(!isString(CAR(args)))
	errorcall(call, "wrong type for argument\n");

    i = LENGTH(CAR(args));
    if (i == 0) {
	for (i = 0, e = environ; *e != NULL; i++, e++);
	PROTECT(ans = allocVector(STRSXP, i));
	for (i = 0, e = environ; *e != NULL; i++, e++)
	    STRING(ans)[i] = mkChar(*e);
    } else {
	PROTECT(ans = allocVector(STRSXP,i));
	for (j = 0; j < i; j++) {
	    s = getenv(CHAR(STRING(CAR(args))[j]));
	    if (s == NULL)
		STRING(ans)[j] = mkChar("");
	    else
		STRING(ans)[j] = mkChar(s);
	}
    }
    UNPROTECT(1);
    return(ans);
}

SEXP do_machine(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return mkString("Unix");
}

SEXP do_system(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    FILE *fp;
    char *x = "r", buf[120];
    int read=0, i, j;
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
	fflush(stdout);
	INTEGER(tlist)[0] = system(CHAR(STRING(CAR(args))[0]));
	R_Visible = 0;
	return tlist;
    }
}

SEXP do_interactive(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval;

    rval=allocVector(LGLSXP, 1);
    if( R_Interactive )
	LOGICAL(rval)[0]=1;
    else
	LOGICAL(rval)[0]=0;
    return rval;
}


void R_Suicide(char *s)
{
    REprintf("Fatal error: %s\n", s);
    R_CleanUp(SA_SUICIDE);
}


	/* Declarations to keep f77 happy */

int MAIN_()  {return 0;}
int MAIN__() {return 0;}
int __main() {return 0;}



/* This function can be used to display the named files with the */
/* given titles and overall title.  On GUI platforms we could */
/* use a read-only window to display the result.  Here we just */
/* make up a temporary file and invoke a pager on it. */

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


/* The location of the R system files */

char *R_HomeDir()
{
    return getenv("R_HOME");
}

/* Prompt the user for a file name.  Return the length of */
/* the name typed.  On Gui platforms, this should bring up */
/* a dialog box so a user can choose files that way. */

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

/* Unix file names which begin with "." are invisible. */
/* Macintosh file names which end with "\r" are invisible. */
/* More complex tests may be needed on other platforms. */

int R_HiddenFile(char *name)
{
    if (name && name[0] != '.') return 0;
    else return 1;
}

/* This call provides a simple interface to the "stat" */
/* system call.  This is available on the Macintosh too. */

#include <sys/types.h>
#include <sys/stat.h>

int R_FileExists(char *path)
{
    struct stat sb;
    return stat(R_ExpandFileName(path), &sb) == 0;
}
