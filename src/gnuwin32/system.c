/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2025  The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/* See ../unix/system.txt for a description of functions */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#define R_USE_SIGNALS 1
#include "Defn.h"
#include <R_ext/Riconv.h>
#include "Fileio.h"
#include "graphapp/ga.h"
#include "console.h"
#include "rui.h"
#include "editor.h"
#include "getline/getline.h"
#include "getline/wc_history.h"
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>		/* for CreateEvent,.. */
#include <shlobj.h>		/* for SHGetKnownFolderPath */
#include <knownfolders.h>
#include <process.h>		/* for _beginthread,... */
#include <io.h>			/* for isatty */
#include "run.h"
#include "Startup.h"
#include <stdlib.h>		/* for exit */

#include "win-nls.h"

/* Callbacks also available under Unix */
static void (*ptr_Busy) (int);
static void (*ptr_CleanUp) (SA_TYPE, int, int);
static void (*ptr_ClearerrConsole) (void);
static void (*ptr_FlushConsole) (void);
static void (*ptr_ProcessEvents) (void); /* aka CallBack on Windows */
static int  (*ptr_ReadConsole) (const char *, unsigned char *, int, int);
static void (*ptr_ResetConsole) (void);
static void (*ptr_ShowMessage) (const char *s);
static void (*ptr_Suicide) (const char *s);
static void (*ptr_WriteConsole) (const char *, int);
static void (*ptr_WriteConsoleEx) (const char *, int, int);

/* Windows-specific callbacks */
static int R_YesNoCancel(const char *s);
static int (*ptr_YesNoCancel)(const char *s);

/* Default implementations of callbacks added in version 1 of structRstart */
static void Rstd_CleanUp(SA_TYPE saveact, int status, int runLast);
static void Rstd_ClearerrConsole(void);
static void Rstd_FlushConsole(void);
static void Rstd_ResetConsole(void);
static void Rstd_Suicide(const char *s);

void R_CleanTempDir(void);		/* from platform.c */
void editorcleanall(void);                  /* from editor.c */

int Rwin_graphicsx = -25, Rwin_graphicsy = 0;

extern SA_TYPE SaveAction; /* from ../main/startup.c */
Rboolean DebugMenuitem = FALSE;  /* exported for rui.c */
static FILE *ifp = NULL;
static char *ifile = NULL;

UImode  CharacterMode = RGui; /* some compilers want initialized for export */
int EmitEmbeddedUTF8 = FALSE;
int ConsoleAcceptCmd;
Rboolean set_workspace_name(const char *fn); /* ../main/startup.c */

/* used to avoid some flashing during cleaning up */
Rboolean AllDevicesKilled = FALSE;

static char oldtitle[512];

Rboolean UserBreak = FALSE;

/* callbacks */
static void R_DoNothing(void) {}

/*
 *   Called at I/O, during eval etc to process GUI events.
 */

typedef void (*DO_FUNC)(void);
static void (* R_Tcl_do)(void) = NULL; /* Initialized to be sure */

void set_R_Tcldo(DO_FUNC ptr)
{
    if (R_Tcl_do)
	error("Thief about! Something other than package tcltk has set or is attempting to set R_Tcl_do");
    R_Tcl_do = ptr;
    return;
}

void unset_R_Tcldo(DO_FUNC ptr)
{
    /* This needs to be a warning not an error, or tcltk will not be able
       to be detached. */
    if (R_Tcl_do != ptr)
	warning("Thief about! Something other than package tcltk has set or is attempting to unset R_Tcl_do");
    R_Tcl_do = NULL;
    return;
}

void R_ProcessEvents(void)
{
    while (peekevent()) doevent();
    if (cpuLimit > 0.0 || elapsedLimit > 0.0) {
#ifdef HAVE_CHECK_TIME_LIMITS
	/* switch to using R_CheckTimeLimits after testing on Windows */
	R_CheckTimeLimits();
#else
	double cpu, data[5];
	R_getProcTime(data);
	cpu = data[0] + data[1];  /* children? */
	if (elapsedLimit > 0.0 && data[2] > elapsedLimit) {
	    cpuLimit = elapsedLimit = -1;
	    if (elapsedLimit2 > 0.0 && data[2] > elapsedLimit2) {
		elapsedLimit2 = -1.0;
		error(_("reached session elapsed time limit"));
	    } else
		error(_("reached elapsed time limit"));
	}
	if (cpuLimit > 0.0 && cpu > cpuLimit) {
	    cpuLimit = elapsedLimit = -1;
	    if (cpuLimit2 > 0.0 && cpu > cpuLimit2) {
		cpuLimit2 = -1.0;
		error(_("reached session CPU time limit"));
	    } else
		error(_("reached CPU time limit"));
	}
#endif
    }
    if (UserBreak) {
	UserBreak = FALSE;
	onintr();
    }
    ptr_ProcessEvents();
    if(R_Tcl_do) R_Tcl_do();
}

void R_WaitEvent(void)
{
    if (!peekevent()) waitevent();
}


/*
 *  1) FATAL MESSAGES AT STARTUP
 */

void R_Suicide(const char *s)
{
    ptr_Suicide(s); /* should not return */
    exit(2); 
}

static void Rstd_Suicide(const char *s)
{
    char  pp[1024];

    snprintf(pp, 1024, _("Fatal error: %s\n"), s);
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
 * R_ReadConsole calls ptr_ReadConsole which points to:
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
static int (*InThreadReadConsole) (const char *, unsigned char *, int, int);

/* EhiWakeUp is a synchronization Event between the main thread and the reader
   thread. It is set by the main thread to inform the reader thread it should
   start reading a line.

   ReadMsgWindow is a message-only window used for synchronization between the
   main thread and the reader thread. The reader thread sends a message to the
   window when it needs the main thread to perform completion or when the line
   is available. The window procedure will set "lineavailable" or run the R
   code for performing completion. */

static HANDLE EhiWakeUp;
static HWND ReadMsgWindow;
#define WM_RREADMSG_EVENT ( WM_USER + 2 )

static LRESULT CALLBACK
ReadMsgWindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

static const char *tprompt;
static unsigned char *tbuf;
static int tlen, thist;
static int lineavailable;

static int ReaderThreadTabHook(char *, int, int *);
static int (*InThreadTabHook)(char *, int, int *);
static struct {
    char *buf;
    int offset;
    int *loc;
    int result;
    HANDLE done;
} completionrequest;

 /* Fill a text buffer with user typed console input. */
int
R_ReadConsole(const char *prompt, unsigned char *buf, int len,
	      int addtohistory)
{
    R_ProcessEvents();
    return ptr_ReadConsole(prompt, buf, len, addtohistory);
}

	/* Write a text buffer to the console. */
	/* All system output is filtered through this routine. */

void R_WriteConsole(const char *buf, int len)
{
    R_ProcessEvents();
    if (ptr_WriteConsole) ptr_WriteConsole(buf, len);
    else ptr_WriteConsoleEx(buf, len, 0);
}


void R_WriteConsoleEx(const char *buf, int len, int otype)
{
    R_ProcessEvents();
    if (ptr_WriteConsole) ptr_WriteConsole(buf, len);
    else ptr_WriteConsoleEx(buf, len, otype);
}



/*1: from GUI console */
int R_is_running = 0;

void Rconsolesetwidth(int cols)
{
    if(R_is_running && setWidthOnResize)
	R_SetOptionWidth(cols);
}

static int
GuiReadConsole(const char *prompt, unsigned char *buf, int len,
               int addtohistory)
{
    int res;
    const void *vmax = vmaxget();
    const char *NormalPrompt =
	translateChar(STRING_ELT(GetOption1(install("prompt")), 0));

    if(!R_is_running) {
	R_is_running = 1;
	Rconsolesetwidth(consolecols(RConsole));
    }
    ConsoleAcceptCmd = !strcmp(prompt, NormalPrompt);
    res = consolereads(RConsole, prompt, (char *)buf, len, addtohistory);
    ConsoleAcceptCmd = 0;
    vmaxset(vmax);

    return !res;
}


/* 2 and 3: reading in a thread */

/* runs in the main R thread */
static void RunCompletion(void *dummy)
{
    completionrequest.result = InThreadTabHook(
			completionrequest.buf,
			completionrequest.offset,
			completionrequest.loc);
}


/* runs in the main R thread */
static LRESULT CALLBACK
ReadMsgWindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
    if (hwnd == ReadMsgWindow && uMsg == WM_RREADMSG_EVENT) {
	int what = (int) lParam;
	switch(what) {
	case 1:
	    lineavailable = 1;
	    return 0;
	case 2:
	    if (!R_ToplevelExec(RunCompletion, NULL))
		completionrequest.result = -1;
	    SetEvent(completionrequest.done);
	    return 0;
	}
    }
    return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

/* runs in the 'Reader thread', needs to execute the completion request
   on the main R thread */
static int ReaderThreadTabHook(char *buf, int offset, int *loc)
{
    completionrequest.buf = buf;
    completionrequest.offset = offset;
    completionrequest.loc = loc;
    PostMessage(ReadMsgWindow, WM_RREADMSG_EVENT, 0,
	       (LPARAM) 2 /* completion needed */);
    WaitForSingleObject(completionrequest.done, INFINITE);
    return completionrequest.result;
}

/* 'Reader thread' main function */
static void __cdecl ReaderThread(void *unused)
{
    while(1) {
	WaitForSingleObject(EhiWakeUp,INFINITE);
	tlen = InThreadReadConsole(tprompt,tbuf,tlen,thist);
	PostMessage(ReadMsgWindow, WM_RREADMSG_EVENT, 0,
	           (LPARAM) 1 /* line available */);
    }
}

/* runs in the main R thread */
static int
ThreadedReadConsole(const char *prompt, unsigned char *buf, int len,
                    int addtohistory)
{
    sighandler_t oldint,oldbreak;
    /*
     *   SIGINT/SIGBREAK when ESS is waiting for output are a real pain:
     *   they get processed after user hit <return>.
     *   The '^C\n' in raw Rterm is nice. But, do we really need it ?
     */
    oldint = signal(SIGINT, SIG_IGN);
    oldbreak = signal(SIGBREAK, SIG_IGN);
    lineavailable = 0;
    tprompt = prompt;
    tbuf = buf;
    tlen = len;
    thist = addtohistory;
    SetEvent(EhiWakeUp);
    while (1) {
	R_WaitEvent();
	doevent();
	if (lineavailable) break;
	if(R_Tcl_do) R_Tcl_do();
    }
    lineavailable = 0;
    /* restore handler  */
    signal(SIGINT, oldint);
    signal(SIGBREAK, oldbreak);
    return tlen;
}


/*2: from character console with getline (only used as InThreadReadConsole)*/
static int
CharReadConsole(const char *prompt, unsigned char *buf, int len,
                int addtohistory)
{
    /* Long lines are returned in multiple consecutive calls to
       CharReadConsole() */
    static char *line = NULL;
    static size_t offset = 0;
    static size_t remaining = 0;
    static int res = 0;

    if (!line) {
	res = getline2(prompt, &line);
	if (addtohistory) gl_histadd(line);
	offset = 0;
	remaining = strlen(line); /* may be zero */
    }

    int tocopy = remaining;
    if (tocopy > len - 1) tocopy = len - 1;

    if (tocopy)
	memcpy(buf, line + offset, tocopy);
    buf[tocopy] = '\0';
    remaining -= tocopy;
    offset += tocopy;

    if (!remaining) {
	gl_free(line);
	line = NULL;
	return !res; /* return 0 on EOF */
    } else
	return 1;
}

/*3: (as InThreadReadConsole) and 4: non-interactive */
static void *cd = NULL;

static int
FileReadConsole(const char *prompt, unsigned char *buf, int len, int addhistory)
{
    int ll, err = 0;

    if (!R_NoEcho) {
	fputs(prompt, stdout);
	fflush(stdout);
    }
    if (fgets((char *)buf, len, ifp ? ifp : stdin) == NULL) return 0;
    /* translate if necessary */
    if(strlen(R_StdinEnc) && strcmp(R_StdinEnc, "native.enc")) {
	size_t res, inb = strlen((char *)buf), onb = len;
	const char *ib = (char *)buf; 
	char obuf[len+1], *ob = obuf;
	if(!cd) {
	    cd = Riconv_open("", R_StdinEnc);
	    if(cd == (void *)-1) error(_("encoding '%s' is not recognised"), R_StdinEnc);
	}
	res = Riconv(cd, &ib, &inb, &ob, &onb);
	*ob = '\0';
	err = (res == (size_t)(-1));
	/* errors lead to part of the input line being ignored */
	if(err) {
	    /* Should re-set with a stateful encoding, but some iconv
	       implementations forget byte-order learned from BOM. 

	    Riconv(cd, NULL, NULL, &ob, &onb);
	    *ob = '\0';
	    */
	    printf(_("<ERROR: re-encoding failure from encoding '%s'>\n"),
		       R_StdinEnc);
	}
	strncpy((char *)buf, obuf, len);
    }

/* according to system.txt, should be terminated in \n, so check this
   at eof or error */
    ll = strlen((char *)buf);
    if ((err || feof(ifp ? ifp: stdin))
	&& buf[ll - 1] != '\n' && ll < len) {
	buf[ll++] = '\n'; buf[ll] = '\0';
    }

    if (!R_Interactive && !R_NoEcho) {
	fputs((char *)buf, stdout);
	fflush(stdout);
    }
    return 1;
}


/* Rgui */
static void
GuiWriteConsole(const char *buf,int len)
{
    if (RConsole) consolewrites(RConsole, buf);
    else MessageBox(NULL, buf, "Console not found", MB_OK | MB_ICONEXCLAMATION);
}

/* Rterm write */
static void
TermWriteConsole(const char *buf, int len)
{
    printf("%s", buf);
}





	/* Indicate that input is coming from the console */

void R_ResetConsole(void)
{
    ptr_ResetConsole();
}

static void Rstd_ResetConsole(void)
{
}

	/* Stdio support to ensure the console file buffer is flushed */

void R_FlushConsole(void)
{
    ptr_FlushConsole();
}

static void Rstd_FlushConsole(void)
{
    if (CharacterMode == RTerm && R_Interactive) fflush(stdout);
    else if (CharacterMode == RGui && RConsole) consoleflush(RConsole);
}


	/* Reset stdin if the user types EOF on the console. */

void R_ClearerrConsole(void)
{
    ptr_ClearerrConsole();
}

static void Rstd_ClearerrConsole(void)
{
    if (CharacterMode == RTerm) clearerr(stdin);
}

/*
 *  3) ACTIONS DURING (LONG) COMPUTATIONS
 */

static void GuiBusy(int which)
{
    if (which == 1) gsetcursor(RConsole, WatchCursor);
    if (which == 0) gsetcursor(RConsole, ArrowCursor);
}

static void CharBusy(int which)
{
}

void R_Busy(int which)
{
    ptr_Busy(which);
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

void R_CleanUp(SA_TYPE saveact, int status, int runLast)
{
    ptr_CleanUp(saveact, status, runLast); /* should not return */
    exit(status);
}

static void Rstd_CleanUp(SA_TYPE saveact, int status, int runLast)
{
    if(saveact == SA_DEFAULT) /* The normal case apart from R_Suicide */
	saveact = SaveAction;

    if(saveact == SA_SAVEASK) {
	if(R_Interactive) {
	    switch (R_YesNoCancel(G_("Save workspace image?"))) {
	    case YES:
		saveact = SA_SAVE;
		break;
	    case NO:
		saveact = SA_NOSAVE;
		break;
	    case CANCEL:
		// There might be residual events with destroyed handles
		R_ProcessEvents();
		jump_to_toplevel();
		break;

	    }
	} else saveact = SaveAction;
    }

    switch (saveact) {
    case SA_SAVE:
	if(runLast) R_dot_Last();
	if(R_DirtyImage) R_SaveGlobalEnv();
	if (CharacterMode == RGui) {
	    R_setupHistory(); /* re-read the history size and filename */
	    wgl_savehistory(R_HistoryFile, R_HistorySize);
	} else if(R_Interactive && CharacterMode == RTerm) {
	    R_setupHistory(); /* re-read the history size and filename */
	    gl_savehistory(R_HistoryFile, R_HistorySize);
	}
	break;
    case SA_NOSAVE:
	if(runLast) R_dot_Last();
	break;
    case SA_SUICIDE:
    default:
	break;
    }
    R_RunExitFinalizers();
    editorcleanall();
    CleanEd();
    KillAllDevices(); /* Unix does not do this under SA_SUICIDE */
    AllDevicesKilled = TRUE; /* used in devWindows.c to inhibit callbacks */
    R_CleanTempDir(); /* changes directory */
    if (R_Interactive && CharacterMode == RTerm)
	SetConsoleTitle(oldtitle);
    if (R_CollectWarnings && saveact != SA_SUICIDE
	&& CharacterMode == RTerm)   /* no point in doing this for Rgui
					as the console is about to close */
	PrintWarnings();        /* from device close and (if run) .Last */
    app_cleanup();
    RConsole = NULL;
    // Add some protection against calling this more than once:
    // caused by signals on Unix, so maybe cannot happen here.
    if(ifp) { 
	fclose(ifp);    /* input file from -f or --file= */
	ifp = NULL; 
    }
    if(ifile) {
	unlink(ifile); /* input file from -e */
	free(ifile);
	ifile = NULL;
    }
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
     *     headers = the 'headers' args of file.show. Printed before each file.
     *     wtitle  = title for window: the 'title' arg of file.show
     *     del     = flag for whether files should be deleted after use
     *     pager   = pager to be used.
     */


extern FILE *R_wfopen(const wchar_t *filename, const wchar_t *mode);
extern size_t Rf_utf8towcs(wchar_t *wc, const char *s, size_t n);

int R_ShowFiles(int nfile, const char **file, const char **headers,
		const char *wtitle, Rboolean del, const char *pager)
{
    int   i, ll;
    char  buf[1024];

    if (nfile > 0) {
	if (pager == NULL || strlen(pager) == 0)
	    pager = "internal";
	for (i = 0; i < nfile; i++) {
	    if(!access(file[i], R_OK)) {
		if (!strcmp(pager, "internal")) {
		    newpager(wtitle, file[i], CE_NATIVE, headers[i], del);
		} else if (!strcmp(pager, "console")) {
		    size_t len;
		    FILE *f;
		    f = R_fopen(file[i], "rt");
		    if(f) {
			while((len = fread(buf, 1, 1023, f))) {
			    buf[len] = '\0';
			    R_WriteConsole(buf, strlen(buf));
			}
			fclose(f);
			if (del) DeleteFile(file[i]);
			/* add a blank line */
			R_WriteConsole("", 0);
		    }
		    else {
			snprintf(buf, 1024,
				 _("cannot open file '%s': %s"),
				 file[i], strerror(errno));
			warning("%s", buf);
		    }
		} else {
		    /* Quote path if not quoted */
		    if(pager[0] != '"')
			snprintf(buf, 1024, "\"%s\" \"%s\"", pager, file[i]);
		    else
			snprintf(buf, 1024, "%s \"%s\"", pager, file[i]);
		    ll = runcmd(buf, CE_NATIVE, 0, 1, NULL, NULL, NULL);
		    if (ll == NOLAUNCH) warning("%s", runerror());
		}
	    } else {
		snprintf(buf, 1024,
			 _("file.show(): file '%s' does not exist\n"),
			 file[i]);
		warning("%s", buf);
	    }
	}
	return 0;
    }
    return 1;
}


    /*
       This function can be used to open the named files in text editors, with the
       given titles and overall title.
       If the file does not exist then the editor should be opened to create a new file.
    */

    /*
     *     nfile   = number of files
     *     file    = array of filenames
     *     editor  = editor to be used.
     */

/* As from R 2.7.0 we assume file, editor are in UTF-8 */
int R_EditFiles(int nfile, const char **file, const char **title,
		const char *editor)
{
    int   i, ll;
    char  buf[1024];

    if (nfile > 0) {
	if (editor == NULL || strlen(editor) == 0)
	    editor = "internal";
	for (i = 0; i < nfile; i++) {
	    if (!strcmp(editor, "internal")) {
		Rgui_Edit(file[i], CE_UTF8, title[i], 0);
	    } else {
		/* Quote path if not quoted */
		if (editor[0] != '"')
		    snprintf(buf, 1024, "\"%s\" \"%s\"", editor, file[i]);
		else
		    snprintf(buf, 1024, "%s \"%s\"", editor, file[i]);
		ll = runcmd(buf, CE_UTF8, 0, 1, NULL, NULL, NULL);
		if (ll == NOLAUNCH) warning("%s", runerror());
	    }

	}
	return 0;
    }
    return 1;
}

#if 0
/* Prompt the user for a file name.  Return the length of */
/* the name typed.  On Gui platforms, this should bring up */
/* a dialog box so a user can choose files that way. */

extern int DialogSelectFile(char *buf, int len); /* from rui.c */

int R_ChooseFile(int new, char *buf, int len)
{
    return DialogSelectFile(buf, len);
}
#endif


void R_ShowMessage(const char *s)
{
    ptr_ShowMessage(s);
}

static int R_YesNoCancel(const char *s)
{
    return ptr_YesNoCancel(s);
}

static void char_message(const char *s)
{
    if (!s) return;
    if (R_Consolefile) {
	/* flush out standard output in case it uses R_Consolefile */
	if (R_Outputfile) fflush(R_Outputfile);
	fprintf(R_Consolefile, "%s\n", s);
	fflush(R_Consolefile);
    } else R_WriteConsole(s, strlen(s));
}

static int char_YesNoCancel(const char *s)
{
    char  ss[128];
    unsigned char a[3];

    snprintf(ss, 128, "%s [y/n/c]: ", s);
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

extern char *getRHOME(int), *getRUser(void); /* in rhome.c */
extern void freeRHOME(char *), freeRUser(char *);
extern void R_putenv_path_cpy(char *, char *, int);
void R_setStartTime(void);

void R_DefCallbacks(Rstart Rp, int RstartVersion)
{
    Rp->ReadConsole = NULL;
    Rp->WriteConsole = NULL;
    Rp->WriteConsoleEx = NULL;
    Rp->CallBack = NULL;
    Rp->ShowMessage = NULL;
    Rp->YesNoCancel = NULL;
    Rp->Busy = NULL;

    if (RstartVersion > 0) {
	Rp->CleanUp = Rstd_CleanUp;
	Rp->ClearerrConsole = Rstd_ClearerrConsole;
	Rp->FlushConsole = Rstd_FlushConsole;
	Rp->ResetConsole = Rstd_ResetConsole;
	Rp->Suicide = Rstd_Suicide;
    }
}

void R_SetWin32(Rstart Rp)
{
    int dummy = 0; /* -Wmaybe-uninitialized */

    {
	/* Idea here is to ask about the memory block an automatic
	   variable is in.  VirtualQuery rounds down to the beginning
	   of the page, and tells us where the allocation started and
	   how many bytes the pages go up */

	MEMORY_BASIC_INFORMATION buf;
	uintptr_t bottom, top;

	VirtualQuery(&dummy, &buf, sizeof(buf));
	bottom = (uintptr_t) buf.AllocationBase;
	top = (uintptr_t) buf.BaseAddress + buf.RegionSize;
	/* printf("stackbase %lx, size %lx\n", top, top-bottom); */
	R_CStackStart = top;
	R_CStackLimit = top - bottom;

	/* The stack detection above is not precise, in fact the stack will
	   not be able to grow that large. As documented, at least one page
	   from the space will be used as a guard page. Starting from the
	   top (high address), the stack is formed by committed area, the
	   guard page, and reserved area. The guard is used for on-demand
	   growing of the committed area and shrinking of the reserve.
	   Experiments show that the reserve would not shrink to less than
	   2 pages (Win7, 32bit). This is not documented and was not tested 
	   in other versions of Windows.*/
	if (R_CStackLimit > 4*4096)
	    R_CStackLimit -= 4*4096;

	/* setup_Rmainloop includes (disabled) code to test stack detection */
    }

    R_CStackDir = 1;
    if(!Rp->rhome)
	R_Suicide("Invalid R_HOME");
    R_Home = (char *)malloc(strlen(Rp->rhome) + 1);
    if (!R_Home)
	R_Suicide("Allocation error");
    strcpy(R_Home, Rp->rhome);
    R_putenv_path_cpy("R_HOME", Rp->rhome, 1);
    R_putenv_path_cpy("R_USER", Rp->home, 0);
    
    if( !getenv("HOME") ) {
	char *RUser = getRUser();
	R_putenv_path_cpy("HOME", RUser, 0);
	freeRUser(RUser);
    }
    putenv("MSYS2_ENV_CONV_EXCL=R_ARCH");

    
    /* This is here temporarily while the GCC version is chosen */
    char *gccversion = (char *)malloc(30);
    if (!gccversion)
	R_Suicide("Allocation error");
#ifdef __clang__
    snprintf(gccversion, 30, "R_COMPILED_BY=clang %d.%d.%d", __clang_major__, __clang_minor__, __clang_patchlevel__);
#else
    snprintf(gccversion, 30, "R_COMPILED_BY=gcc %d.%d.%d", __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__);
#endif
    putenv(gccversion);
    /* no free here: storage remains in use */

    /* Rterm and Rgui set CharacterMode during startup, then set Rp->CharacterMode
       from it in cmdlineoptions().  Rproxy never calls cmdlineoptions, so we need the
       line below */

    CharacterMode = Rp->CharacterMode;

    /* Be careful when relying on Rp->EmitEmbeddedUTF8 due to potential
       embedding applications using old structRstart. */
    switch(CharacterMode) {
    case RGui:
	R_GUIType = "Rgui";
	EmitEmbeddedUTF8 = TRUE;
	break;
    case RTerm:
	R_GUIType = "RTerm";
	EmitEmbeddedUTF8 = FALSE;
	break;
    default:
	R_GUIType = "unknown";
	EmitEmbeddedUTF8 = (GetACP() != 65001) &&
	                   (Rp->EmitEmbeddedUTF8 == TRUE);
    }

    ptr_CleanUp = Rstd_CleanUp;
    ptr_ClearerrConsole = Rstd_ClearerrConsole;
    ptr_FlushConsole = Rstd_FlushConsole;
    ptr_ResetConsole = Rstd_ResetConsole;
    ptr_Suicide = Rstd_Suicide;

    if (Rp->RstartVersion == 1) {
	ptr_CleanUp = Rp->CleanUp;
	ptr_ClearerrConsole = Rp->ClearerrConsole;
	ptr_FlushConsole = Rp->FlushConsole;
	ptr_ResetConsole = Rp->ResetConsole;
	ptr_Suicide = Rp->Suicide;
    }

    EmitEmbeddedUTF8 = Rp->EmitEmbeddedUTF8;
    ptr_ReadConsole = Rp->ReadConsole;
    ptr_WriteConsole = Rp->WriteConsole;
    ptr_WriteConsoleEx = Rp->WriteConsoleEx;
    ptr_ProcessEvents = Rp->CallBack;
    ptr_ShowMessage = Rp->ShowMessage;
    ptr_YesNoCancel = Rp->YesNoCancel;
    ptr_Busy = Rp->Busy;
    /* Process R_HOME/etc/Renviron.site, then
       .Renviron or ~/.Renviron, if it exists.
       Only used here in embedded versions */
    if(!Rp->NoRenviron) {
	process_site_Renviron();
	process_user_Renviron();
    }
    Rwin_fpset();  /* in extra.c */
}


/* Remove and process NAME=VALUE command line arguments */

static void Putenv(const char *str)
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
    Rboolean hadE = FALSE;

    /* We don't want to parse -e expressions */
    while(--ac) {
	++av;
	if(strcmp(*av, "-e") == 0) {
	    hadE = TRUE;
	    argv[newac++] = *av;
	    continue;
	}
	if(!hadE && **av != '-' && Rf_strchr(*av, '='))
	    Putenv(*av);
	else
	    argv[newac++] = *av;
	hadE = FALSE;
    }
    *pac = newac;
}

char *PrintUsage(void)
{
    static char msg[5000];
    char msg0[] =
	"Start R, a system for statistical computation and graphics, with the\nspecified options\n\nEnvVars: Environmental variables can be set by NAME=value strings\n\nOptions:\n  -h, --help            Print usage message and exit\n  --version             Print version info and exit\n  --encoding=enc        Specify encoding to be used for stdin\n  --encoding enc        ditto\n  --save                Do save workspace at the end of the session\n  --no-save             Don't save it\n",
	msg1[] =
	"  --no-environ          Don't read the site and user environment files\n  --no-site-file        Don't read the site-wide Rprofile\n  --no-init-file        Don't read the .Rprofile or ~/.Rprofile files\n  --restore             Do restore previously saved objects at startup\n  --no-restore-data     Don't restore previously saved objects\n  --no-restore-history  Don't restore the R history file\n  --no-restore          Don't restore anything\n  --workspace=file      Workspace to be restored\n",
	msg2[] =
	"  --vanilla             Combine --no-save, --no-restore, --no-site-file,\n                          --no-init-file and --no-environ\n",
	msg2b[] =
	"  --max-ppsize=N        Set max size of protect stack to N\n",
	msg2c[] =
	"-  -max-connections=N   Set max number of connections to N\n",
	msg3[] =
	"  -q, --quiet           Don't print startup message\n  --silent              Same as --quiet\n  --no-echo             Make R run as quietly as possible\n  --verbose             Print more information about progress\n  --args                Skip the rest of the command line\n",
	msg4[] =
	"  --ess                 Don't use getline for command-line editing\n                          and assert interactive use\n  -f file               Take input from 'file'\n  --file=file           ditto\n  -e expression         Use 'expression' as input\n\nOne or more -e options can be used, but not together with -f or --file\n",
	msg5[] = "\nAn argument ending in .RData (in any case) is taken as the path\nto the workspace to be restored (and implies --restore)";
    if(CharacterMode == RTerm)
	strcpy(msg, "Usage: Rterm [options] [EnvVars]\n\n");
    else strcpy(msg, "Usage: Rgui [options] [EnvVars]\n\n");
    strcat(msg, msg0);
    strcat(msg, msg1);
    strcat(msg, msg2);
    strcat(msg, msg2b);
    strcat(msg, msg2c);
    strcat(msg, msg3);
    if(CharacterMode == RTerm) strcat(msg, msg4);
    strcat(msg, msg5);
    strcat(msg, "\n");
    return msg;
}

void R_setupHistory(void)
{
    int value, ierr;
    char *p;

    if ((R_HistoryFile = getenv("R_HISTFILE")) == NULL)
	R_HistoryFile = ".Rhistory";
    R_HistorySize = 512;
    if ((p = getenv("R_HISTSIZE"))) {
	value = R_Decode2Long(p, &ierr);
	if (ierr != 0 || value < 0)
	    R_ShowMessage("WARNING: invalid R_HISTSIZE ignored;");
	else
	    R_HistorySize = value;
    }
}

extern int R_isWriteableDir(char *path);

static Rboolean use_workspace(Rstart Rp, char *name, Rboolean usedRdata)
{
    char s[1024];
    char *path, *p;

    if(!usedRdata) {
	if (!set_workspace_name(name)) {
	    snprintf(s, 1024, _("Not enough memory"));
	    R_ShowMessage(s);
	} else {
	    path = (char *)malloc(strlen(name) + 1);	
	    strcpy(path, name);
	    for (p = path; *p; p++) if (*p == '\\') *p = '/';
	    p = Rf_strrchr(path, '/');
	    if(p) {
		*p = '\0';
		SetCurrentDirectory(path);
	    }
	    usedRdata = TRUE;
	    Rp->RestoreAction = SA_RESTORE;
	    return TRUE;
	}
    } else {
	snprintf(s, 1024, _("ARGUMENT '%s' __ignored__\n"), name);
	R_ShowMessage(s);
    }
    return FALSE;
}

int cmdlineoptions(int ac, char **av)
{
    int   i;
    char  s[1024], cmdlines[10000], *RUser, *RHome;
    structRstart rstart;
    Rstart Rp = &rstart;
    Rboolean usedRdata = FALSE, processing = TRUE;

    /* ensure R_Home gets set early: we are in rgui or rterm here */
    int dirstrip = 2;
#ifdef R_ARCH
    if (strlen(R_ARCH) > 0)
	dirstrip++;
#endif 
    RHome = getRHOME(dirstrip);
    if(!RHome)
	R_Suicide("Invalid R_HOME");
    R_Home = RHome;
    /* need this for moduleCdynload for iconv.dll */
    InitFunctionHashing();
    R_putenv_path_cpy("R_HOME", RHome, 1);
    BindDomain(RHome);

    R_setStartTime();

    /* Store the command line arguments before they are processed
       by the different option handlers. We do this here so that
       we get all the name=value pairs. Otherwise these will
       have been removed by the time we get to call
       R_common_command_line().
    */
    R_set_command_line_arguments(ac, av);

    R_DefParamsEx(Rp, RSTART_VERSION);
    Rp->CharacterMode = CharacterMode;
    for (i = 1; i < ac; i++)
	if (!strcmp(av[i], "--no-environ") || !strcmp(av[i], "--vanilla"))
	    Rp->NoRenviron = TRUE;
	else if (!strcmp(av[i], "--cd-to-userdocs")) {
	    /* This is used in shortcuts created by the installer. Previously, the
	       installer resolved the user documents folder at installation time,
	       but that is not good for installation under SCCM/system context where
	       it resolved to documents folder in systemprofile. This has do be done
	       before process_user_Renviron(), because user .Renviron may be read from
	       the current directory, which is expected to be userdocs. */
	    wchar_t *mydocs;
	    if (SHGetKnownFolderPath(&FOLDERID_Documents, KF_FLAG_CREATE, NULL,
	                             &mydocs) == S_OK)
		SetCurrentDirectoryW(mydocs);
	    CoTaskMemFree(mydocs);
	}

    Rp->CallBack = R_DoNothing;
    /* Here so that --ess and similar can change */
    InThreadReadConsole = NULL;
    if (CharacterMode == RTerm) {
	if (isatty(0) && isatty(1)) {
	    Rp->R_Interactive = TRUE;
	    Rp->ReadConsole = ThreadedReadConsole;
	    InThreadReadConsole = CharReadConsole;
	} else if (R_is_redirection_tty(0) && R_is_redirection_tty(1)) {
	    /* Note it is not currently possible to use line editing with Msys2
	       terminals such as mintty, because we cannot disable buffering in
	       the terminal. One can only do that from applications linked
	       against the Cygwin runtime, but R is linked against Msvcrt
	       via Mingw and using multiple runtimes is not possible.

	       Msys2/cygwin is handled in AppMain() by re-executing using
	       winpty. This branch is taken when winpty is not available.
	    */
	    Rp->R_Interactive = TRUE;
	    Rp->ReadConsole = ThreadedReadConsole;
	    InThreadReadConsole = FileReadConsole;
	    setvbuf(stdout, NULL, _IONBF, 0);
	} else {
	    Rp->R_Interactive = FALSE;
	    Rp->ReadConsole = FileReadConsole;
	}
	/* Windows 95/98/ME have a shell that cannot redirect stderr,
	   so don't use that on those OSes */
	{
	    OSVERSIONINFO verinfo;
	    verinfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	    GetVersionEx(&verinfo);
	    switch(verinfo.dwPlatformId) {
	    case VER_PLATFORM_WIN32_WINDOWS:
		R_Consolefile = stdout; /* used for errors */
		break;
	    default:
		R_Consolefile = stderr; /* used for errors */
	    }
	}
	R_Consolefile = stderr; /* used for errors */
	R_Outputfile = stdout;  /* used for sink-able output */
	Rp->WriteConsole = TermWriteConsole;
	Rp->ShowMessage = char_message;
	Rp->YesNoCancel = char_YesNoCancel;
	Rp->Busy = CharBusy;
    } else {
	Rp->R_Interactive = TRUE;
	Rp->ReadConsole = GuiReadConsole;
	Rp->WriteConsole = GuiWriteConsole;
	Rp->ShowMessage = askok;
	Rp->YesNoCancel = askyesnocancel;
	Rp->Busy = GuiBusy;
    }

    ptr_ShowMessage = Rp->ShowMessage; /* used here */
    ptr_WriteConsole = Rp->WriteConsole;
    /* Rp->WriteConsole is guaranteed to be set above,
       so we know WriteConsoleEx is not used */
    ptr_ProcessEvents = Rp->CallBack;

    /* process environment variables
     * precedence:  command-line, .Renviron, inherited
     */
    if(!Rp->NoRenviron) {
	process_site_Renviron();
	process_user_Renviron();
	Rp->NoRenviron = TRUE;

	/* allow for R_MAX_[VN]SIZE and R_[VN]SIZE in user/site Renviron */
	R_SizeFromEnv(Rp);
    }
    env_command_line(&ac, av);

    R_common_command_line(&ac, av, Rp);

    cmdlines[0] = '\0';
    while (--ac) {
	if (processing && **++av == '-') {
	    if (!strcmp(*av, "--help") || !strcmp(*av, "-h")) {
		R_ShowMessage(PrintUsage());
		freeRHOME(RHome);
		exit(0);
	    } else if (!strcmp(*av, "--cd-to-userdocs")) {
		/* handled above before processing Renviron */
	    } else if (!strcmp(*av, "--no-environ")) {
		Rp->NoRenviron = TRUE;
	    } else if (!strcmp(*av, "--ess")) {
/* Assert that we are interactive even if input is from a file */
		Rp->R_Interactive = TRUE;
		Rp->ReadConsole = ThreadedReadConsole;
		InThreadReadConsole = FileReadConsole;
		setvbuf(stdout, NULL, _IONBF, 0);
	    } else if (!strcmp(*av, "--internet2")) {
/*	        This is now the default */
	    } else if (!strcmp(*av, "--mdi")) {
		MDIset = 1;
	    } else if (!strcmp(*av, "--sdi") || !strcmp(*av, "--no-mdi")) {
		MDIset = -1;
	    } else if(!strcmp(*av, "--debug")) {
		DebugMenuitem = TRUE;
		breaktodebugger();
	    } else if(!strcmp(*av, "--args")) {
		break;
	    } else if(CharacterMode == RTerm && !strcmp(*av, "-f")) {
		ac--; av++;
		if (!ac) {
		    snprintf(s, 1024,
			    _("option '%s' requires an argument"),
			    "-f");
		    R_Suicide(s);
		}
		Rp->R_Interactive = FALSE;
		Rp->ReadConsole = FileReadConsole;
		if(strcmp(*av, "-")) {
		    ifp = R_fopen(*av, "r");
		    if(!ifp) {
			snprintf(s, 1024,
				 _("cannot open file '%s': %s"),
				 *av, strerror(errno));
			R_Suicide(s);
		    }
		}
	    } else if(CharacterMode == RTerm && !strncmp(*av, "--file=", 7)) {
		Rp->R_Interactive = FALSE;
		Rp->ReadConsole = FileReadConsole;
		if(strcmp((*av)+7, "-")) {
		    ifp = R_fopen( (*av)+7, "r");
		    if(!ifp) {
			snprintf(s, 1024,
				 _("cannot open file '%s': %s"),
				 (*av)+7, strerror(errno));
			R_Suicide(s);
		    }
		}
	    } else if (!strncmp(*av, "--workspace=", 12)) {
		usedRdata = use_workspace(Rp, *av + 12, usedRdata);
	    } else if(CharacterMode == RTerm && !strcmp(*av, "-e")) {
		ac--; av++;
		if (!ac || !strlen(*av)) {
		    snprintf(s, 1024,
			    _("option '%s' requires a non-empty argument"),
			    "-e");
		    R_Suicide(s);
		}
		if(strlen(cmdlines) + strlen(*av) + 2 <= 10000) {
		    strcat(cmdlines, *av);
		    strcat(cmdlines, "\n");
		} else {
		    snprintf(s, 1024, _("WARNING: '-e %s' omitted as input is too long\n"), *av);
		    R_ShowMessage(s);
		}
	    } else {
		snprintf(s, 1024, _("WARNING: unknown option '%s'\n"), *av);
		R_ShowMessage(s);
	    }
	} else {
	    /* Look for *.RData, as given by drag-and-drop 
	       and file association */

	    if (strlen(*av) >= 6 && stricmp(*av+strlen(*av)-6, ".RData") == 0)
		usedRdata = use_workspace(Rp, *av, usedRdata);
	}
    }
    RUser = getRUser();
    if(strlen(cmdlines)) {
	if(ifp) R_Suicide(_("cannot use -e with -f or --file"));
	Rp->R_Interactive = FALSE;
	Rp->ReadConsole = FileReadConsole;
	{
	    char *tm;
	    tm = getenv("TMPDIR");
	    if (!R_isWriteableDir(tm)) {
		tm = getenv("TMP");
		if (!R_isWriteableDir(tm)) {
		    tm = getenv("TEMP");
		    if (!R_isWriteableDir(tm)) 
			tm = RUser;
		}
	    }
	    /* in case getpid() is not unique -- has been seen under Windows */
	    size_t needed;
	    needed = snprintf(NULL, 0, "%s/Rscript%x%x", tm, getpid(), 
		              (unsigned int) GetTickCount()) + 1;
	    ifile = (char *)malloc(needed);
	    if (ifile) {
		snprintf(ifile, needed, "%s/Rscript%x%x", tm, getpid(), 
			 (unsigned int) GetTickCount());
		ifp = fopen(ifile, "w+b");
	    }
	    if(!ifp) R_Suicide(_("creation of tmpfile failed -- set TMPDIR suitably?"));
	    /* Unix does unlink(ifile) here, but Windows cannot delete open files */
	}
	if (fwrite(cmdlines, 1, strlen(cmdlines), ifp) != strlen(cmdlines))
	    R_Suicide("fwrite error in cmdlineoptions");
	fflush(ifp);
	rewind(ifp);
    }
    if (ifp && Rp->SaveAction != SA_SAVE) Rp->SaveAction = SA_NOSAVE;

    Rp->rhome = RHome;
    Rp->home = RUser;
    R_SetParams(Rp); /* will re-set R_Home to a copy */
    freeRUser(RUser);
    /* Do not free RHome in case some code running before R_SetParams()
       captured it via global variable R_Home
    
       freeRHOME(RHome);
    */

/*
 *  Since users' expectations for save/no-save will differ, we decided
 *  that they should be forced to specify in the non-interactive case.
 */
    if (!R_Interactive && Rp->SaveAction != SA_SAVE &&
	Rp->SaveAction != SA_NOSAVE)
	R_Suicide(_("you must specify '--save', '--no-save' or '--vanilla'"));

    if (InThreadReadConsole) {
	Rboolean ok = TRUE;
	if (InThreadReadConsole == CharReadConsole) {
	    /* Need to arrange for the getline completion tab hook to execute
	       on the main R thread. Executing it in another thread can cause
	       crashes due to at least stack overflow checking (part of the
	       completion is implemented in R). */

	    InThreadTabHook = gl_tab_hook;
	    gl_tab_hook = ReaderThreadTabHook;
	    completionrequest.done = CreateEvent(NULL, FALSE, FALSE, NULL);
	    if (!completionrequest.done)
		ok = FALSE;
	}
	if (ok) {
	    HINSTANCE instance = GetModuleHandle(NULL);
	    WNDCLASS wndclass = { 0, ReadMsgWindowProc, 0, 0, instance, NULL,
	                          0, 0, NULL, "RReadMsg" };
	    ReadMsgWindow = NULL;
	    if (RegisterClass(&wndclass)) {
		ReadMsgWindow = CreateWindow("RReadMsg", "RReadMsg", 0, 1, 1,
		                             1, 1, HWND_MESSAGE, NULL, instance,
		                             NULL);
	    }
	    if (!ReadMsgWindow)
		ok = FALSE;
	}
	if (!ok || !(EhiWakeUp = CreateEvent(NULL, FALSE, FALSE, NULL)) ||
	    (_beginthread(ReaderThread, 0, NULL) == -1))

	    R_Suicide(_("impossible to create 'reader thread'; you must free some system resources"));
    }
    R_setupHistory();
    return 0;
}

/* only for back-compatibility: used by Rserve */
void setup_term_ui(void)
{
    initapp(0, 0);
    readconsolecfg();
}

void saveConsoleTitle(void)
{
    GetConsoleTitle(oldtitle, 512);
}


/* On Windows, the number of open files is essentially unlimited.
 * This function returns 16,777,216 based on
 * https://blogs.technet.microsoft.com/markrussinovich/2009/09/29/pushing-the-limits-of-windows-handles
 */
int R_GetFDLimit(void)
{
    long limit = 16L*1024L*1024L;
    return (limit > INT_MAX) ? INT_MAX : limit;
}

int R_EnsureFDLimit(int desired)
{
    long limit = 16L*1024L*1024L;
    return (desired <= limit) ? desired : (int)limit;
}

