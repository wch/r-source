/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2001  Robert Gentleman, Ross Ihaka and the
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

         /* See ../unix/system.txt for a description of functions */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "Fileio.h"
#include "Rdevices.h"		/* KillAllDevices() [nothing else?] */
#include "graphapp/ga.h"
#include "console.h"
#include "rui.h"
#include "getline/getline.h"
#include <windows.h>  /* for CreateEvent,.. */
#include <process.h> /* for _beginthread,... */
#include "run.h"
#include "Startup.h"

unsigned int R_max_memory = INT_MAX;

SA_TYPE SaveAction = SA_DEFAULT;
SA_TYPE RestoreAction = SA_RESTORE;
Rboolean LoadSiteFile = TRUE;
Rboolean LoadInitFile = TRUE;
Rboolean DebugInitFile = FALSE;

UImode  CharacterMode;
int ConsoleAcceptCmd;
void closeAllHlpFiles();
void UnLoad_Unzip_Dll();
void UnLoad_Rbitmap_Dll();

/* used to avoid some flashing during cleaning up */
Rboolean AllDevicesKilled = FALSE;
int   setupui(void);
void  delui(void);
int (*R_yesnocancel)(char *s);

static DWORD mainThreadId;

static char oldtitle[512];

Rboolean UserBreak = FALSE;

/* callbacks */
static void (*R_CallBackHook) ();
static void R_DoNothing() {}
static void (*my_R_Busy)(int);

/*
 *   Called at I/O, during eval etc to process GUI events.
 */

void (* R_tcldo)();
static void tcl_do_none() {}

void R_ProcessEvents(void)
{
    while (peekevent()) doevent();
    if (UserBreak) {
	UserBreak = FALSE;
	raise(SIGINT);
    }
    R_CallBackHook();
    if(R_tcldo) R_tcldo();
}


/*
 *  1) FATAL MESSAGES AT STARTUP
 */

void R_Suicide(char *s)
{
    char  pp[1024];

    sprintf(pp, "Fatal error: %s\n", s);
    R_ShowMessage(pp);
    R_CleanUp(SA_SUICIDE, 2, 0);
}

/*
 *  2. CONSOLE I/O
 */

/*
 * We support 4 different type of input.
 * 1) from the gui console;
 * 2) from a character mode console (interactive);
 * 3) from a pipe under --ess, i.e, interactive.
 * 4) from a file or from a pipe (not interactive)
 *
 * Hence, it is better to have a different function for every
 * situation.
 * Same, it is true for output (but in this case, 2=3=4)
 *
 * BTW, 3 and 4 are different on input  since fgets,ReadFile...
 * "blocks" =>  (e.g.) you cannot give focus to the graphics device if
 * you are wating for input. For this reason, input is got in a different
 * thread
 *
 * All works in this way:
 * R_ReadConsole calls TrueReadConsole which points to:
 * case 1: GuiReadConsole
 * case 2 and 3: ThreadedReadConsole
 * case 4: FileReadConsole
 * ThreadedReadConsole wake up our 'reader thread' and wait until
 * a new line of input is available. The 'reader thread' uses
 * InThreadReadConsole to get it. InThreadReadConsole points to:
 * case 2: CharReadConsole
 * case 3: FileReadConsole
*/

/* Global variables */
static int (*TrueReadConsole) (char *, char *, int, int);
static int (*InThreadReadConsole) (char *, char *, int, int);
static void (*TrueWriteConsole) (char *, int);
HANDLE EhiWakeUp;
static char *tprompt, *tbuf;
static  int tlen, thist, lineavailable;

 /* Fill a text buffer with user typed console input. */
int
R_ReadConsole(char *prompt, unsigned char *buf, int len, int addtohistory)
{
    R_ProcessEvents();
    return TrueReadConsole(prompt, (char *) buf, len, addtohistory);
}

	/* Write a text buffer to the console. */
	/* All system output is filtered through this routine. */

void R_WriteConsole(char *buf, int len)
{
    R_ProcessEvents();
    TrueWriteConsole(buf, len);
}



/*1: from GUI console */
static int R_is_running = 0;

void Rconsolesetwidth(int cols)
{
    if(R_is_running && setWidthOnResize)
	R_SetOptionWidth(cols);
}

static int
GuiReadConsole(char *prompt, char *buf, int len, int addtohistory)
{
    char *p;
    char *NormalPrompt =
	(char *) CHAR(STRING_ELT(GetOption(install("prompt"), R_NilValue), 0));

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


/* 2 and 3: reading in a thread */


/* 'Reader thread' main function */
static void __cdecl ReaderThread(void *unused)
{
  while(1) {
    WaitForSingleObject(EhiWakeUp,INFINITE);
    tlen = InThreadReadConsole(tprompt,tbuf,tlen,thist);
    lineavailable = 1;
    PostThreadMessage(mainThreadId, 0, 0, 0);
  }
}

static int
ThreadedReadConsole(char *prompt, char *buf, int len, int addtohistory)
{
  sighandler_t oldint,oldbreak;
  /*
   *   SIGINT/SIGBREAK when ESS is waiting for output are a real pain:
   *   they get processed after user hit <return>.
   *   The '^C\n' in raw Rterm is nice. But, do we really need it ?
  */
  oldint = signal(SIGINT, SIG_IGN);
  oldbreak = signal(SIGBREAK, SIG_IGN);
  mainThreadId = GetCurrentThreadId();
  lineavailable = 0;
  tprompt = prompt;
  tbuf = buf;
  tlen = len;
  thist = addtohistory;
  SetEvent(EhiWakeUp);
  while (1) {
    WaitMessage();
    if (lineavailable) break;
    doevent();
    if(R_tcldo) R_tcldo();
  }
  lineavailable = 0;
  /* restore handler  */
  signal(SIGINT,oldint);
  signal(SIGBREAK,oldbreak);
  return tlen;
}


/*2: from character console with getline (only used as InThreadReadConsole)*/
static int
CharReadConsole(char *prompt, char *buf, int len, int addtohistory)
{
   getline(prompt,buf,len);
   if (addtohistory) gl_histadd(buf);
   return 1;
}

/*3: (as InThreadReadConsole) and 4: non-interactive */
static int
FileReadConsole(char *prompt, char *buf, int len, int addhistory)
{
    int ll;
    if (!R_Slave) {
	fputs(prompt, stdout);
	fflush(stdout);
    }
    if (fgets(buf, len, stdin) == NULL)
	return 0;
/* according to system.txt, should be terminated in \n, so check this
   at eof */
    ll = strlen((char *)buf);
    if (feof(stdin) && buf[ll - 1] != '\n' && ll < len) {
	buf[ll++] = '\n'; buf[ll] = '\0';
    }
    if (!R_Interactive && !R_Slave)
	fputs(buf, stdout);
    return 1;
}


/* Rgui */
static void
GuiWriteConsole(char *buf,int len)
{
    char *p;

    for (p = buf; *p; p++)
	if (*p == '\001')
	    *p = EOF;
    consolewrites(RConsole, buf);
}

/* Rterm write */
static void
TermWriteConsole(char *buf, int len)
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
    if (CharacterMode == RTerm && R_Interactive) fflush(stdin);
    else if (CharacterMode == RGui) consoleflush(RConsole);
}


	/* Reset stdin if the user types EOF on the console. */

void R_ClearerrConsole()
{
    if (CharacterMode == RTerm)  clearerr(stdin);
}


/*
 *  3) ACTIONS DURING (LONG) COMPUTATIONS
 */

void GuiBusy(int which)
{
	if (which == 1) gsetcursor(RConsole, WatchCursor);
	if (which == 0) gsetcursor(RConsole, ArrowCursor);
}

void CharBusy(int which)
{
}

void R_Busy(int which)
{
    my_R_Busy(which);
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


void R_CleanUp(SA_TYPE saveact, int status, int runLast)
{
    if(saveact == SA_DEFAULT) /* The normal case apart from R_Suicide */
	saveact = SaveAction;

    if(saveact == SA_SAVEASK) {
	if(R_Interactive) {
	    switch (R_yesnocancel("Save workspace image?")) {
	    case YES:
		saveact = SA_SAVE;
		break;
	    case NO:
		saveact = SA_NOSAVE;
		break;
	    case CANCEL:
		jump_to_toplevel();
		break;

	    }
	} else saveact = SaveAction;
    }

    switch (saveact) {
    case SA_SAVE:
	if(runLast) R_dot_Last();
	if(R_DirtyImage) R_SaveGlobalEnv();
	if (CharacterMode == RGui ||
	    (R_Interactive && CharacterMode == RTerm))
	    gl_savehistory(R_HistoryFile);
	break;
    case SA_NOSAVE:
	if(runLast) R_dot_Last();
	break;
    case SA_SUICIDE:
    default:
	break;
    }
    CleanEd();
    closeAllHlpFiles();
    KillAllDevices();
    AllDevicesKilled = TRUE;
    if (R_Interactive && CharacterMode == RTerm) 
	SetConsoleTitle(oldtitle);
    UnLoad_Unzip_Dll();
    UnLoad_Rbitmap_Dll();
    if (R_CollectWarnings && saveact != SA_SUICIDE
	&& CharacterMode == RTerm)
	PrintWarnings();
    app_cleanup();
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
		Rboolean del, char *pager)
{
    int   i;
    char  buf[1024];
    WIN32_FIND_DATA fd;

    if (nfile > 0) {
	if (pager == NULL || strlen(pager) == 0)
	    pager = "internal";
	for (i = 0; i < nfile; i++) {
	    if (FindFirstFile(file[i], &fd) != INVALID_HANDLE_VALUE) {
		if (!strcmp(pager, "internal")) {
		    newpager(wtitle, file[i], headers[i], del);
		} else if (!strcmp(pager, "console")) {
/*		    DWORD len = 1;
		    HANDLE f = CreateFile(file[i], GENERIC_READ,
					  FILE_SHARE_WRITE,
					  NULL, OPEN_EXISTING, 0, NULL);
		    if (f != INVALID_HANDLE_VALUE) {
			while (ReadFile(f, buf, 1023, &len, NULL) && len) {
			    buf[len] = '\0';
			    R_WriteConsole(buf,strlen(buf));
			}
			CloseHandle(f);*/
/* The above causes problems with lack of CRLF translations */
		    size_t len;
		    FILE *f = fopen(file[i], "rt");
		    if(f) {
			while((len = fread(buf, 1, 1023, f))) {
			    buf[len] = '\0';
			    R_WriteConsole(buf, strlen(buf));
			}
			fclose(f);
			if (del) DeleteFile(file[i]);
		    }
		    else {
			sprintf(buf,"Unable to open file '%s'", file[i]);
			warning(buf);
		    }
		} else {
                    /* Quote path if necessary */
		    if(pager[0] != '"' && strchr(pager, ' '))
			sprintf(buf, "\"%s\" \"%s\"", pager, file[i]);
		    else
			sprintf(buf, "%s \"%s\"", pager, file[i]);
		    runcmd(buf, 0, 1, "");
		}
	    } else {
		sprintf(buf, "file.show(): file %s does not exist\n", file[i]);
		warning(buf);
	    }
	}
	return 0;
    }
    return 1;
}

int internal_ShowFile(char *file, char *header)
{
    SEXP pager = GetOption(install("pager"), R_NilValue);
    char *files[1], *headers[1];

    files[0] = file;
    headers[0] = header;
    return R_ShowFiles(1, files, headers, "File", 0, CHAR(STRING_ELT(pager, 0)));
}


/* Prompt the user for a file name.  Return the length of */
/* the name typed.  On Gui platforms, this should bring up */
/* a dialog box so a user can choose files that way. */

extern int DialogSelectFile(char *buf, int len); /* from rui.c */

int R_ChooseFile(int new, char *buf, int len)
{
    return (DialogSelectFile(buf, len));
}

/* code for R_ShowMessage, R_YesNoCancel */

void (*pR_ShowMessage)(char *s);
void R_ShowMessage(char *s)
{
    (*pR_ShowMessage)(s);
}


static void char_message(char *s)
{
    if (!s) return;
    R_WriteConsole(s, strlen(s));
}

static int char_yesnocancel(char *s)
{
    char  ss[128];
    unsigned char a[3];

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
}


	/*--- Initialization Code ---*/

static char RHome[MAX_PATH + 7];
static char UserRHome[MAX_PATH + 7];
static char RUser[MAX_PATH];
char *getRHOME(); /* in rhome.c */
void R_setStartTime();


void R_SetWin32(Rstart Rp)
{
    R_Home = Rp->rhome;
    sprintf(RHome, "R_HOME=%s", R_Home);
    putenv(RHome);
    strcpy(UserRHome, "R_USER=");
    strcat(UserRHome, Rp->home);
    putenv(UserRHome);

    CharacterMode = Rp->CharacterMode;
    switch(CharacterMode){
    case RGui:
	R_GUIType = "Rgui";
	break;
    case RTerm:
	R_GUIType = "RTerm";
	break;
    default:
	R_GUIType = "unknown";
    }
    TrueReadConsole = Rp->ReadConsole;
    TrueWriteConsole = Rp->WriteConsole;
    R_CallBackHook = Rp->CallBack;
    pR_ShowMessage = Rp->message;
    R_yesnocancel = Rp->yesnocancel;
    my_R_Busy = Rp->busy;
    /* Process .Renviron or ~/.Renviron, if it exists. 
       Only used here in embedded versions */
    if(!Rp->NoRenviron)
	process_users_Renviron();
    _controlfp(_MCW_EM, _MCW_EM);
}


/* Remove and process NAME=VALUE command line arguments */

static void Putenv(char *str)
{
    char *buf;
    buf = (char *) malloc((strlen(str) + 1) * sizeof(char));
    if(!buf) R_ShowMessage("allocation failure in reading Renviron");
    strcpy(buf, str);
    putenv(buf);
   /* no free here: storage remains in use */
}


static void env_command_line(int *pac, char **argv)
{
    int ac = *pac, newac = 1; /* Remember argv[0] is process name */
    char **av = argv;

    while(--ac) {
	++av;
	if(**av != '-' && strchr(*av, '='))
	    Putenv(*av);
	else
	    argv[newac++] = *av;
    }
    *pac = newac;
}

int cmdlineoptions(int ac, char **av)
{
    int   i, ierr;
    long value;
    char *p;
    char  s[1024];
    structRstart rstart;
    Rstart Rp = &rstart;
    MEMORYSTATUS ms;

#ifdef HAVE_TIMES
    R_setStartTime();
#endif

    /* Store the command line arguments before they are processed
       by the different option handlers. We do this here so that
       we get all the name=value pairs. Otherwise these will
       have been removed by the time we get to call
       R_common_command_line().
     */
    R_set_command_line_arguments(ac, av, Rp);


    /* set defaults for R_max_memory. This is set here so that
       embedded applications get no limit */
    GlobalMemoryStatus(&ms);
    R_max_memory = min(256 * Mega, ms.dwTotalPhys);
    /* need enough to start R: fails on a 8Mb system */
    R_max_memory = max(16 * Mega, R_max_memory);
    
    R_DefParams(Rp);
    Rp->CharacterMode = CharacterMode;
    for (i = 1; i < ac; i++)
	if (!strcmp(av[i], "--no-environ") || !strcmp(av[i], "--vanilla"))
		Rp->NoRenviron = TRUE;

/* Here so that --ess and similar can change */
    Rp->CallBack = R_DoNothing;
    InThreadReadConsole = NULL;
    if (CharacterMode == RTerm) {
	if (isatty(0)) {
	    Rp->R_Interactive = TRUE;
	    Rp->ReadConsole = ThreadedReadConsole;
            InThreadReadConsole = CharReadConsole;
	} else {
	    Rp->R_Interactive = FALSE;
	    Rp->ReadConsole = FileReadConsole;
	}
	R_Consolefile = stdout; /* used for errors */
	R_Outputfile = stdout;  /* used for sink-able output */
        Rp->WriteConsole = TermWriteConsole;
	Rp->message = char_message;
	Rp->yesnocancel = char_yesnocancel;
	Rp->busy = CharBusy;
    } else {
	Rp->R_Interactive = TRUE;
	Rp->ReadConsole = GuiReadConsole;
	Rp->WriteConsole = GuiWriteConsole;
	Rp->message = askok;
	Rp->yesnocancel = askyesnocancel;
	Rp->busy = GuiBusy;
    }

    pR_ShowMessage = Rp->message; /* used here */
    TrueWriteConsole = Rp->WriteConsole;
    R_CallBackHook = Rp->CallBack;

    /* process environment variables
     * precedence:  command-line, .Renviron, inherited
     */
    if(!Rp->NoRenviron) {
	process_users_Renviron();
	Rp->NoRenviron = TRUE;
    }
    env_command_line(&ac, av);
/*    R_SizeFromEnv(Rp); */

    R_common_command_line(&ac, av, Rp);

    while (--ac) {
	if (**++av == '-') {
	    if (!strcmp(*av, "--no-environ")) {
		Rp->NoRenviron = TRUE;
	    } else if (!strcmp(*av, "--ess")) {
/* Assert that we are interactive even if input is from a file */
		Rp->R_Interactive = TRUE;
		Rp->ReadConsole = ThreadedReadConsole;
                InThreadReadConsole = FileReadConsole;
	    } else if (!strcmp(*av, "--mdi")) {
		MDIset = 1;
	    } else if (!strcmp(*av, "--sdi") || !strcmp(*av, "--no-mdi")) {
		MDIset = -1;
	    } else if (!strncmp(*av, "--max-mem-size", 14)) {
		if(strlen(*av) < 16) {
		    ac--; av++; p = *av;
		}
		else
		    p = &(*av)[15];
		if (p == NULL) {
		    R_ShowMessage("WARNING: no max-mem-size given\n");
		    break;
		}
		value = Decode2Long(p, &ierr);
		if(ierr) {
		    if(ierr < 0)
			sprintf(s, "WARNING: --max-mem-size value is invalid: ignored\n");
		    else
		    sprintf(s, "WARNING: --max-mem-size=%ld`%c': too large and ignored\n",
			    value,
			    (ierr == 1) ? 'M': ((ierr == 2) ? 'K':'k'));
		    R_ShowMessage(s);
		} else if (value < 10*Mega) {
		    sprintf(s, "WARNING: max-mem-size =%4.1fM too small and ignored\n", value/(1024.0 * 1024.0));
		    R_ShowMessage(s);
		} else
		    R_max_memory = value;
	    } else {
		sprintf(s, "WARNING: unknown option %s\n", *av);
		R_ShowMessage(s);
		break;
	    }
	} else {
	    sprintf(s, "ARGUMENT '%s' __ignored__\n", *av);
	    R_ShowMessage(s);
	}
    }
    Rp->rhome = getRHOME();

    R_tcldo = tcl_do_none;
/*
 * try R_USER then HOME then working directory
 */
    if (getenv("R_USER")) {
	strcpy(RUser, getenv("R_USER"));
    } else if (getenv("HOME")) {
	    strcpy(RUser, getenv("HOME"));
    } else if (getenv("HOMEDRIVE")) {
	    strcpy(RUser, getenv("HOMEDRIVE"));
	    strcat(RUser, getenv("HOMEPATH"));
    } else
	GetCurrentDirectory(MAX_PATH, RUser);
    p = RUser + (strlen(RUser) - 1);
    if (*p == '/' || *p == '\\') *p = '\0';
    Rp->home = RUser;
    R_SetParams(Rp);

/*
 *  Since users' expectations for save/no-save will differ, we decided
 *  that they should be forced to specify in the non-interactive case.
 */
    if (!R_Interactive && SaveAction != SA_SAVE && SaveAction != SA_NOSAVE)
	R_Suicide("you must specify `--save', `--no-save' or `--vanilla'");

    if (InThreadReadConsole &&
        (!(EhiWakeUp = CreateEvent(NULL, FALSE, FALSE, NULL)) ||
	 (_beginthread(ReaderThread, 0, NULL) == -1)))
      R_Suicide("impossible to create 'reader thread'; you must free some system resources");

    if ((R_HistoryFile = getenv("R_HISTFILE")) == NULL)
	R_HistoryFile = ".Rhistory";
    R_HistorySize = 512;
    if ((p = getenv("R_HISTSIZE"))) {
	int value, ierr;
	value = Decode2Long(p, &ierr);
	if (ierr != 0 || value < 0)
	    REprintf("WARNING: invalid R_HISTSIZE ignored;");
	else
	    R_HistorySize = value;
    }
    return 0;
}

void setup_term_ui()
{
    initapp(0, 0);
    R_tcldo = tcl_do_none;
    readconsolecfg();
}

void saveConsoleTitle()
{
    GetConsoleTitle(oldtitle, 512);
}
