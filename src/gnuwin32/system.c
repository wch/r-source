/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--1999  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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
 *  SYSTEM DEPENDENT CODE (Windows version: G.M. and B.D.R.)
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
 *    void  R_CleanUp(int ask)

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
 *  7) PLATFORM INDEPENDENT FUNCTIONS
 *
 *    SEXP do_getenv(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP do_interactive(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP do_machine(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP do_proctime(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP do_quit(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP do_system(SEXP call, SEXP op, SEXP args, SEXP rho)
 *
 */

#include "Defn.h"
#include "Fileio.h"
#include "Arith.h"		/* R_NaInt */
#include "Graphics.h"		/* KillAllDevices() [nothing else?] */
#include "graphapp/ga.h"
#include "console.h"
#include "rui.h"
#include "getline/getline.h"
#include "devga.h"
#include <windows.h>
#include "run.h"

static int DefaultSaveAction = 0;
static int DefaultRestoreAction = 1;
static int LoadSiteFile = 1;
static int LoadInitFile = 1;
static int DebugInitFile = 0;

int   CharacterMode;
static int PipedInput = 0;
int   ConsoleAcceptCmd;

/* used to avoid some flashing during cleaning up */
int   AllDevicesKilled = 0;
int   setupui(void);
void  delui(void);

int   UserBreak = 0;

void ProcessEvents(void)
{
    while (peekevent()) {
	doevent();
    }
    if (UserBreak) {
	UserBreak = 0;
	error("user break\n");
    }
}

static void my_message(char *s)
{
    if (!s) return;
    if (CharacterMode)
	R_WriteConsole(s, strlen(s));
    else
	askok(s);
}

static int my_yesnocancel(char *s)
{
    char  a[3], ss[128];

    if (CharacterMode) {
	sprintf(ss, "%s [y/n/c]: ", s);
	R_ReadConsole(ss, a, 3, 0);
	switch (a[0]) {
	  case 'y':
	  case 'Y':
	    return YES;
	  case 'n':
	  case 'N':
	    return NO;
	  default:
	    return CANCEL;
	}
    } else
	return askyesnocancel(s);
}


/*
 * I realized that we are supporting 4 different type of input.
 * 1) from the gui console;
 * 2) from a character mode console (interactive);
 * 3) from a pipe under -ess, i.e, interactive.
 * 4) from a file or from a pipe (not interactive)
 *
 * Hence, it is better to have a different function for every
 * situation.
 * Same, it is true for output (but in this case, 3==4)
 *
 * BTW, 3 and 4 are different on input  since fgets,ReadFile...
 * "blocks" =>
 * (e.g.) you cannot give focus to the graphics device if
 * you are wating for input. For this reason, under 3,
 * fgets is runned in a different thread (Windows is wonderful,
 * I never used 'threads', hence, after made this running
 * I was very, very happy "Wuah, fgets in a thread!!!")
 */


/*1:*/
static int R_is_running = 0;

void Rconsolesetwidth(int cols)
{
    if(R_is_running && setWidthOnResize)
	R_SetOptionWidth(cols);
}

static int GuiReadConsole(char *prompt, char *buf, int len, int addtohistory)
{
    char *p;
    char *NormalPrompt =
	(char *) CHAR(STRING(GetOption(install("prompt"), R_NilValue))[0]);

    if(!R_is_running) {
	R_is_running = 1;
	Rconsolesetwidth(consolecols(RConsole));
    }
    ConsoleAcceptCmd = !strcmp(prompt, NormalPrompt);
    consolereads(RConsole, prompt, buf, len, addtohistory);
    for (p = buf; *p; p++)
	if (*p == EOF)
	    *p = '\001';
    ConsoleAcceptCmd = 0;
    return 1;
}

static void GuiWriteConsole(char *buf,int len)
{
    char *p;

    for (p = buf; *p; p++)
	if (*p == '\001')
	    *p = EOF;
    consolewrites(RConsole, buf);
}

/*2:*/
static char LastLine[512];

static int CharReadConsole(char *prompt, char *buf, int len, int addtohistory)
{
    static char *gl = NULL;
    int   i;

    if (!gl) {
	strcat(LastLine, prompt);
	gl = getline(LastLine);
	LastLine[0] = '\0';
	if (addtohistory)
	    gl_histadd(gl);
    }
    for (i = 0; *gl && (*gl != '\n') && (i < len - 2); gl++, i++)
	buf[i] = *gl;
    buf[i] = '\n';
    buf[i + 1] = '\0';
    if (!*gl || (*gl == '\n'))
	gl = NULL;
    return 1;
}

static void CharWriteConsole(char *buf, int len)
{
    char *p = strrchr(buf, '\n');

    if (p)
	strcpy(LastLine, p + 1);
    else
	strcat(LastLine, buf);
    printf("%s", buf);
}

/*3:*/

/*
 * Variables used to communicate between thread and main process
 */
static int lineavailable, lengthofbuffer;
static char *inputbuffer;

static DWORD CALLBACK
threadedfgets(LPVOID unused)
{
    inputbuffer = fgets(inputbuffer, lengthofbuffer, stdin);
    lineavailable = 1;
    return 0;
}

static int
PipeReadConsole(char *prompt, char *buf, int len, int addhistory)
{
    HANDLE rH;
    int   id;

    if (!R_Slave) {
	fputs(prompt, stdout);
	fflush(stdout);
    }
    lineavailable = 0;
    lengthofbuffer = len;
    inputbuffer = buf;
    rH = CreateThread(NULL, 0, threadedfgets, NULL, 0, &id);
    if (!rH) {
	/* failure! Use standard fgets. */
	inputbuffer = fgets(buf, len, stdin);
	lineavailable = 1;
    }
    while (!lineavailable)
	doevent();
    if (rH)
	CloseHandle(rH);
    if (!inputbuffer)
	return 0;
    else
	return 1;
}

/*4:*/
static int
FileReadConsole(char *prompt, char *buf, int len, int addhistory)
{
    if (!R_Slave) {
	fputs(prompt, stdout);
	fflush(stdout);
    }
    if (!fgets(buf, len, stdin))
	return 0;
    if (!R_Slave)
	fputs(buf, stdout);
    return 1;
}

static void
FileWriteConsole(char *buf, int len)
{
    printf("%s", buf);
}


static int (*TrueReadConsole) (char *, char *, int, int);
static void (*TrueWriteConsole) (char *, int);

 /* Fill a text buffer with user typed console input. */
int
R_ReadConsole(char *prompt, unsigned char *buf, int len, int addtohistory)
{
    ProcessEvents();
    return TrueReadConsole(prompt, buf, len, addtohistory);
}

	/* Write a text buffer to the console. */
	/* All system output is filtered through this routine. */

void R_WriteConsole(char *buf, int len)
{
    ProcessEvents();
    TrueWriteConsole(buf, len);
}


	/* Indicate that input is coming from the console */

void R_ResetConsole()
{
}


	/* Stdio support to ensure the console file buffer is flushed */

void R_FlushConsole()
{
    if (CharacterMode)
	fflush(stdin);
    else
	consoleflush(RConsole);
}


	/* Reset stdin if the user types EOF on the console. */

void R_ClearerrConsole()
{
    if (CharacterMode)  clearerr(stdin);
}


	/*--- File Handling Code ---*/



char *R_ExpandFileName(char *s)
{
    return s;
}


FILE *R_OpenLibraryFile(char *file)
{
    char  buf[256];
    FILE *fp;

    sprintf(buf, "%s/library/base/R/%s", R_Home, file);
    fp = R_fopen(buf, "r");
    return fp;
}

FILE *R_OpenSysInitFile(void)
{
    char  buf[256];
    FILE *fp;

    sprintf(buf, "%s/library/base/R/Rprofile", R_Home);
    fp = R_fopen(buf, "r");
    return fp;
}

FILE *R_OpenSiteFile(void)
{
    char  buf[256];
    FILE *fp;

    fp = NULL;

    if (LoadSiteFile) {
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
    char  buf[256];
    FILE *fp;

    fp = NULL;

    if (LoadInitFile) {
	if ((fp = R_fopen(".Rprofile", "r")))
	    return fp;
	sprintf(buf, "%s/.Rprofile", getenv("R_HOME"));
	if ((fp = R_fopen(buf, "r")))
	    return fp;
    }
    return fp;
}


	/*--- Initialization Code ---*/

#ifdef HAVE_TIMES
static long StartTime;
#endif

static char RHome[MAX_PATH + 6];
static char UserRHome[MAX_PATH + 6];
char *getRHOME();
void  closeAllHlpFiles();
void UnLoad_Unzip_Dll();


/* Process ~/.Renviron, if it exists */
#include "opt.h"

/* like putenv, but allocate storage */
static void Putenv(char *str)
{
    char *buf;
    buf = (char *) malloc((strlen(str) + 1) * sizeof(char));
    strcpy(buf, str);
    putenv(buf);
}

static void processRenviron()
{
    char *opt[2], optf[MAX_PATH], buf[80];
    int   ok;

    sprintf(optf, "%s/.Renviron", getenv("R_HOME"));
    if (!optopenfile(optf))
	return;
    while ((ok = optread(opt, '='))) {
	sprintf(buf, "%s=%s", opt[0], opt[1]);
	Putenv(buf);
    }
    optclosefile();
}



#define Max_Nsize 20000000	/* must be < LONG_MAX (= 2^32 - 1 =)
				   2147483647 = 2.1e9 */
#define Max_Vsize (2048*Mega)	/* must be < LONG_MAX */

#define Min_Nsize 200000
#define Min_Vsize (2*Mega)

int cmdlineoptions(int ac, char **av)
{
    int   value, ierr, nset = 0, vset = 0;
    char *p;
    char  s[1024];

/* Here so that -ess and similar can change */
    PipedInput = 0;
    if (CharacterMode) {
	if (isatty(0)) {
	    R_Interactive = 1;
	    R_Consolefile = NULL;
	    R_Outputfile = NULL;
	    gl_events_hook = ProcessEvents;
	    LastLine[0] = 0;
	    TrueReadConsole = CharReadConsole;
	    TrueWriteConsole = CharWriteConsole;
	} else {
	    R_Interactive = 0;
	    R_Consolefile = stdout;
	    R_Outputfile = stdout;
	    TrueReadConsole = FileReadConsole;
	    TrueWriteConsole = FileWriteConsole;
	}
    } else {
	R_Interactive = 1;
	R_Consolefile = NULL;
	R_Outputfile = NULL;
	TrueReadConsole = GuiReadConsole;
	TrueWriteConsole = GuiWriteConsole;
    }
    R_Sinkfile = NULL;

#ifdef HAVE_TIMES
    StartTime = currenttime();
#endif
    R_Quiet = 0;

    DefaultSaveAction = 1;

    while (--ac) {
	if (**++av == '-') {
	    if (!strcmp(*av, "-V") || !strcmp(*av, "--version")) {
		sprintf(s, "Version %s.%s %s (%s %s, %s)\nCopyright (C) %s R Core Team\n\n",
		R_MAJOR, R_MINOR, R_STATUS, R_MONTH, R_DAY, R_YEAR, R_YEAR);
		strcat(s, "R is free software and comes with ABSOLUTELY NO WARRANTY.\n");
		strcat(s, "You are welcome to redistribute it under the terms of the\n");
		strcat(s, "GNU General Public License.  For more information about\n");
		strcat(s, "these matters, see http://www.gnu.org/copyleft/gpl.html.\n");
		my_message(s);
		exit(0);
	    } else if (!strcmp(*av, "--save")) {
		DefaultSaveAction = 3;
	    } else if (!strcmp(*av, "--no-save")) {
		DefaultSaveAction = 2;
	    } else if (!strcmp(*av, "--restore")) {
		DefaultRestoreAction = 1;
	    } else if (!strcmp(*av, "--no-restore")) {
		DefaultRestoreAction = 0;
	    } else if (!strcmp(*av, "--silent") ||
		       !strcmp(*av, "--quiet") ||
		       !strcmp(*av, "-q")) {
		R_Quiet = 1;
	    } else if (!strcmp(*av, "--vanilla")) {
		DefaultSaveAction = 2;	/* --no-save */
		DefaultRestoreAction = 0;	/* --no-restore */
		LoadSiteFile = 0;	/* --no-site-file */
		LoadInitFile = 0;	/* --no-init-file */
	    } else if (!strcmp(*av, "--verbose")) {
		R_Verbose = 1;
	    } else if (!strcmp(*av, "--slave") ||
		       !strcmp(*av, "-s")) {
		R_Quiet = 1;
		R_Slave = 1;
		DefaultSaveAction = 2;
	    } else if (!strcmp(*av, "--no-site-file")) {
		LoadSiteFile = 0;
	    } else if (!strcmp(*av, "--no-init-file")) {
		LoadInitFile = 0;
	    } else if (!strcmp(*av, "--debug-init")) {
		DebugInitFile = 1;
	    } else if (!strcmp(*av, "-save") ||
		       !strcmp(*av, "-nosave") ||
		       !strcmp(*av, "-restore") ||
		       !strcmp(*av, "-norestore") ||
		       !strcmp(*av, "-noreadline") ||
		       !strcmp(*av, "-quiet") ||
		       !strcmp(*av, "-V")) {
		sprintf(s, "WARNING: option %s no longer supported", *av);
		my_message(s);
	    } else if ((value = (*av)[1] == 'v') || !strcmp(*av, "--vsize")) {
		if (value)
		    my_message("WARNING: option `-v' is deprecated. Use `--vsize' instead.\n");
		if (!value || (*av)[2] == '\0') {
		    ac--;
		    av++;
		    p = *av;
		} else
		    p = &(*av)[2];
		if (p == NULL) {
		    my_message("WARNING: no vsize given");
		    break;
		}
		value = Decode2Long(p, &ierr);
		if (ierr) {
		    if (ierr < 0)
			goto badargs;	/* if(*p) goto badargs; */
		    sprintf(s, "--vsize %d'%c': too large", value,
			    (ierr == 1) ? 'M' : ((ierr == 2) ? 'K' : 'k'));
		    my_message(s);
		}
		if (value < 1000) {
		    my_message("WARNING: vsize ridiculously low, Megabytes assumed\n");
		    value *= Mega;
		}
		if (value < Min_Vsize || value > Max_Vsize) {
		    sprintf(s, "WARNING: invalid v(ector heap)size '%d' ignored;"
			    "using default = %gM\n", value, R_VSize / Mega);
		    my_message(s);
		} else {
		    vset = 1;
		    R_VSize = value;
		}
	    } else if ((value = (*av)[1] == 'n') || !strcmp(*av, "--nsize")) {
		if (value)
		    my_message("WARNING: option `-n' is deprecated.  "
			       "Use `--nsize' instead.\n");
		if (!value || (*av)[2] == '\0') {
		    ac--;
		    av++;
		    p = *av;
		} else
		    p = &(*av)[2];
		if (p == NULL) {
		    my_message("WARNING: no nsize given");
		    break;
		}
		value = Decode2Long(p, &ierr);
		if (ierr) {
		    if (ierr < 0)
			goto badargs;
		    sprintf(s, "--nsize %d'%c': too large", value,
			    (ierr == 1) ? 'M' : ((ierr == 2) ? 'K' : 'k'));
		    my_message(s);
		}
		if (value < Min_Nsize || value > Max_Nsize) {
		    sprintf(s, "WARNING: invalid language heap (n)size '%d' ignored,"
			    " using default = %d\n", value, R_NSize);
		    my_message(s);
		} else {
		    nset = 1;
		    R_NSize = value;
		}
	    } else if (!strcmp(*av, "--ess")) {
/* Assert that we are interactive even if input is from a file */
		PipedInput = 1;
		R_Interactive = 1;
		TrueReadConsole = PipeReadConsole;
	    } else if (!strcmp(*av, "--mdi")) {
		MDIset = 1;
	    } else if (!strcmp(*av, "--sdi") || !strcmp(*av, "--no-mdi")) {
		MDIset = -1;
	    } else {
		sprintf(s, "WARNING: unknown option %s\n", *av);
		my_message(s);
		break;
	    }
	} else {
/* Allow NAME=value pairs and set as environment variables */
	    p = strchr(*av, '=');
	    if (p) {
		putenv(*av);
	    } else {
		sprintf(s, "ARGUMENT '%s' __ignored__\n", *av);
		my_message(s);
	    }
	}
    }
    R_Home = getRHOME();
    sprintf(RHome, "RHOME=%s", R_Home);
    putenv(RHome);

/*
 * try R_HOME then HOME then working directory
 * put these here to allow R_HOME or HOME to be set on the command line.
 */
    if (!getenv("R_HOME")) {
	if (getenv("HOME")) {
	    sprintf(UserRHome, "R_HOME=%s", getenv("HOME"));
	    p = UserRHome + (strlen(UserRHome) - 1);
	    if (*p == '/' || *p == '\\')
		*p = '\0';
	} else {
	    strcpy(UserRHome, "R_HOME=");
	    GetCurrentDirectory(MAX_PATH, &UserRHome[7]);
	}
	putenv(UserRHome);
    }

/* Process ~/.Renviron, if it exists */
    processRenviron();

    if (!vset && (p = getenv("R_VSIZE"))) {
	value = Decode2Long(p, &ierr);
	if (ierr != 0 || value > Max_Vsize || value < Min_Vsize)
	    REprintf("WARNING: invalid R_VSIZE ignored;");
	else
	    R_VSize = value;
    }
    if (!nset && (p = getenv("R_NSIZE"))) {
	value = Decode2Long(p, &ierr);
	if (ierr != 0 || value > Max_Nsize || value < Min_Nsize)
	    REprintf("WARNING: invalid R_NSIZE ignored;");
	else
	    R_NSize = value;
    }
    if (!R_Interactive && DefaultSaveAction == 0)
	R_Suicide("you must specify `--save', `--no-save' or `--vanilla'");

    _controlfp(_MCW_EM, _MCW_EM);

    return 0;

badargs:
    REprintf("invalid argument passed to R\n");
    exit(1);
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
    int   ans = 0;

    if (R_DirtyImage) {
	R_ClearerrConsole();
	R_FlushConsole();
	if (CharacterMode && !R_Interactive && ask == 1)
	    ask = DefaultSaveAction;
	if (ask == 1)
	    ans = my_yesnocancel("Save workspace image?");
	else if (ask == 2)
	    ans = NO;
	else if (ask == 3)
	    ans = YES;

	switch (ans) {
	  case YES:
	    R_SaveGlobalEnv();
	    break;
	  case NO:
	    break;
	  case CANCEL:
	    jump_to_toplevel();
	    break;
	}
    }
    closeAllHlpFiles();
    KillAllDevices();
    AllDevicesKilled = 1;
    if (!CharacterMode)
	savehistory(RConsole, ".Rhistory");
    UnLoad_Unzip_Dll();
    exitapp();
}

void R_Busy(int which)
{
/* currently cursor is never set off busy */
    if(!CharacterMode) {
	if (which == 1) gsetcursor(RConsole, WatchCursor);
	if (which == 0) gsetcursor(RConsole, ArrowCursor);
    }
}

	/* Saving and Restoring the Global Environment */

void R_SaveGlobalEnv(void)
{
    FILE *fp = R_fopen(".RData", "wb");
    if (!fp)
	error("can't save data -- unable to open ./.RData\n");
    R_SaveToFile(FRAME(R_GlobalEnv), fp, 0);
    fclose(fp);
}

void R_RestoreGlobalEnv(void)
{
    FILE *fp;

    if (DefaultRestoreAction) {
	if (!(fp = R_fopen(".RData", "rb"))) {
	    /* warning here perhaps */
	    return;
	}
	FRAME(R_GlobalEnv) = R_LoadFromFile(fp);
	if (!R_Quiet)
	    Rprintf("[Previously saved workspace restored]\n\n");
	fclose(fp);
    }
}


	/*--- Platform Dependent Functions ---*/


#ifdef HAVE_TIMES

SEXP do_proctime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  ans;
    long  elapsed;

    elapsed = (currenttime() - StartTime) / 10;
    ans = allocVector(REALSXP, 5);
    REAL(ans)[0] = R_NaReal;
    REAL(ans)[1] = R_NaReal;
    REAL(ans)[2] = (double) elapsed / 100.0;
    REAL(ans)[3] = R_NaReal;
    REAL(ans)[4] = R_NaReal;
    return ans;
}
#endif

SEXP do_getenv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int   i, j;
    char *s;
    char **e;
    SEXP  ans;

    char *_env[1];

    _env[0] = NULL;
    checkArity(op, args);

    if (!isString(CAR(args)))
	errorcall(call, "wrong type for argument\n");

    i = LENGTH(CAR(args));
    if (i == 0) {
	for (i = 0, e = _env; *e != NULL; i++, e++);
	PROTECT(ans = allocVector(STRSXP, i));
	for (i = 0, e = _env; *e != NULL; i++, e++)
	    STRING(ans)[i] = mkChar(*e);
    } else {
	PROTECT(ans = allocVector(STRSXP, i));
	for (j = 0; j < i; j++) {
	    s = getenv(CHAR(STRING(CAR(args))[j]));
	    if (s == NULL)
		STRING(ans)[j] = mkChar("");
	    else
		STRING(ans)[j] = mkChar(s);
	}
    }
    UNPROTECT(1);
    return (ans);
}

SEXP do_machine(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return mkString("Win32");
}


/*
 * flag =0 don't wait/ignore stdout
 * flag =1 wait/ignore stdout
 * flag =2 wait/copy stdout to the console
 * flag =3 wait/return stdout
 * Add 10 to minimize application
 * Add 20 to make application "invisible"
*/


SEXP do_system(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    rpipe *fp;
    char  buf[120];
    int   vis = 0, flag = 2, i = 0, j, ll;
    SEXP  tlist = R_NilValue, tchar, rval;

    checkArity(op, args);
    if (!isString(CAR(args)))
	errorcall(call, "character string expected as first argument\n");
    if (isInteger(CADR(args)))
	flag = INTEGER(CADR(args))[0];
    if (flag > 20) {
	vis = -1;
	flag -= 20;
    } else if (flag > 10) {
	vis = 0;
	flag -= 10;
    } else
	vis = 1;
    if (!isString(CADDR(args)))
	errorcall(call, "character string expected as third argument\n");
    if (CharacterMode && (flag == 2))
	flag = 1;
    if (!CharacterMode) {
	SetStdHandle(STD_INPUT_HANDLE, INVALID_HANDLE_VALUE);
	SetStdHandle(STD_OUTPUT_HANDLE, INVALID_HANDLE_VALUE);
	SetStdHandle(STD_ERROR_HANDLE, INVALID_HANDLE_VALUE);
    }
    if (flag < 2) {
	ll = runcmd(CHAR(STRING(CAR(args))[0]), flag, vis,
		    CHAR(STRING(CADDR(args))[0]));
	if (ll == NOLAUNCH)
	    warning(runerror());
    } else {
	fp = rpipeOpen(CHAR(STRING(CAR(args))[0]), vis,
		       CHAR(STRING(CADDR(args))[0]));
	if (!fp) {
	    /* If we are returning standard output generate an error */
	    if (flag == 3)
		error(runerror());
	    warning(runerror());
	    ll = NOLAUNCH;
	} else {
	    if (flag == 3)
		PROTECT(tlist);
	    for (i = 0; rpipeGets(fp, buf, 120); i++) {
		if (flag == 3) {
		    ll = strlen(buf) - 1;
		    if ((ll >= 0) && (buf[ll] == '\n'))
			buf[ll] = '\0';
		    tchar = mkChar(buf);
		    UNPROTECT(1);
		    PROTECT(tlist = CONS(tchar, tlist));
		} else
		    R_WriteConsole(buf, strlen(buf));
	    }
	    ll = rpipeClose(fp);
	}
    }
    if (flag == 3) {
	rval = allocVector(STRSXP, i);;
	for (j = (i - 1); j >= 0; j--) {
	    STRING(rval)[j] = CAR(tlist);
	    tlist = CDR(tlist);
	}
	UNPROTECT(1);
	return (rval);
    } else {
	tlist = allocVector(INTSXP, 1);
	INTEGER(tlist)[0] = ll;
	R_Visible = 0;
	return tlist;
    }
}

SEXP do_interactive(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP  rval;

    rval = allocVector(LGLSXP, 1);
    LOGICAL(rval)[0] = R_Interactive;
    return rval;
}

SEXP do_quit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    char *tmp;
    int   ask = 0;

    if (R_BrowseLevel) {
	warning("can't quit from browser\n");
	return R_NilValue;
    }
    if (!isString(CAR(args)))
	errorcall(call, "one of \"yes\", \"no\" or \"ask\" expected.\n");
    tmp = CHAR(STRING(CAR(args))[0]);
    if (!strcmp(tmp, "ask"))
	ask = 1;
    else if (!strcmp(tmp, "no"))
	ask = 2;
    else if (!strcmp(tmp, "yes"))
	ask = 3;
    else
	errorcall(call, "unrecognized value of ask\n");
    R_CleanUp(ask);
    exit(0);
    /* NOTREACHED */
}

void R_Suicide(char *s)
{
    char  pp[1024];

    sprintf(pp, "Fatal error: %s\n", s);
    my_message(pp);
    R_CleanUp(2);
    /* 2 means don't save anything and it's an unrecoverable abort */
}


	/* Declarations to keep f77 happy */
/*
int MAIN_()  {return 0;}
int MAIN__() {return 0;}
int __main() {return 0;}
*/

/* New / Experimental API elements */

#ifdef DEFUNCT
int R_ShowFile(char *file, char *title)
{
    FILE *fp;
    char  buf[1024];
    char *pager;
    int   c;

    pager = getenv("PAGER");
    if (pager == NULL)
	pager = "notepad.exe";
    sprintf(buf, "%s %s", pager, file);
    if (runcmd(buf, 0, 1, "") != 0)
	return 0;
    else
	return 1;
}
#endif

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
    int   i;
    char  buf[1024];

    if (nfile > 0) {
	if (pager == NULL || strlen(pager) == 0)
	    pager = "internal";
	for (i = 0; i < nfile; i++) {
	    if (!strcmp(pager, "internal")) {
		newpager(wtitle, file[i], headers[i], del);
            } if (!strcmp(pager, "console")) {
                DWORD len = 1;
                HANDLE f = CreateFile(file[i], GENERIC_READ, FILE_SHARE_WRITE,
		   NULL, OPEN_EXISTING, 0, NULL);
                if (f != INVALID_HANDLE_VALUE) {
                  while (ReadFile(f,buf,1023,&len,NULL) && len) {
                    buf[len] = '\0';
                    R_WriteConsole(buf,strlen(buf));
                  }
                  CloseHandle(f);
                  if (del) DeleteFile(file[i]);
                }
                else {
                  sprintf(buf,"Impossible to open file '%s'. Does it exist?\n",file[i]);
                  warning(buf);
                }
	    } else {
		sprintf(buf, "%s  %s", pager, file[i]);
		runcmd(buf, 0, 1, "");
	    }
	}
	return 0;
    }
    return 1;
}


/* The location of the R system files */

char *R_HomeDir()
{
    return getenv("RHOME");
}

/* Prompt the user for a file name.  Return the length of */
/* the name typed.  On Gui platforms, this should bring up */
/* a dialog box so a user can choose files that way. */

/* from rui.c */
extern int
DialogSelectFile(char *buf,int len);

int R_ChooseFile(int new, char *buf, int len)
{
    return (DialogSelectFile(buf, len));
}

/* Unix file names which begin with "." are invisible. */
/* Macintosh file names which end with "\r" are invisible. */
/* More complex tests may be needed on other platforms. */

int R_HiddenFile(char *name)
{
    if (name && name[0] != '.')
	return 0;
    else
	return 1;
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
