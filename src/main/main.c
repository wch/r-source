/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2001   The R Development Core Team
 *  Copyright (C) 2002--2004  The R Foundation
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
 *  A copy of the GNU General Public License is available via WWW at
 *  http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 *  writing to the Free Software Foundation, Inc., 59 Temple Place,
 *  Suite 330, Boston, MA  02111-1307  USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define __MAIN__
#include "Defn.h"
#include "Graphics.h"
#include "Rdevices.h"		/* for InitGraphics */
#include "IOStuff.h"
#include "Fileio.h"
#include "Parse.h"
#include "Startup.h"

#ifdef HAVE_LOCALE_H
# include <locale.h>
#endif

#ifdef HAVE_LANGINFO_CODESET
# include <langinfo.h>
#endif

#ifdef HAVE_AQUA
extern void InitAquaIO(void);			/* from src/modules/aqua/aquaconsole.c */
extern void RSetConsoleWidth(void);		/* from src/modules/aqua/aquaconsole.c */
extern Rboolean CocoaGUI;				/* from src/unix/system.c              */
extern Rboolean useCocoa;				/* from src/unix/system.c              */
#endif

/* The `real' main() program is in ../<SYSTEM>/system.c */
/* e.g. ../unix/system.c */

/* Global Variables:  For convenience, all interpeter global symbols
 * ================   are declared in Defn.h as extern -- and defined here.
 *
 * NOTE: This is done by using some preprocessor trickery.  If __MAIN__
 * is defined as above, there is a sneaky
 *     #define extern
 * so that the same code produces both declarations and definitions.
 *
 * This does not include user interface symbols which are included
 * in separate platform dependent modules.
 */

void Rf_callToplevelHandlers(SEXP expr, SEXP value, Rboolean succeeded, Rboolean visible);

int Rf_ParseBrowser(SEXP, SEXP);


static void onpipe(int);

extern void InitDynload();


	/* Read-Eval-Print Loop [ =: REPL = repl ] with input from a file */

static void R_ReplFile(FILE *fp, SEXP rho, int savestack, int browselevel)
{
    ParseStatus status;
    int count=0;

    for(;;) {
	R_PPStackTop = savestack;
	R_CurrentExpr = R_Parse1File(fp, 1, &status);
	switch (status) {
	case PARSE_NULL:
	    break;
	case PARSE_OK:
	    R_Visible = 0;
	    R_EvalDepth = 0;
	    count++;
	    PROTECT(R_CurrentExpr);
	    R_CurrentExpr = eval(R_CurrentExpr, rho);
	    SET_SYMVALUE(R_LastvalueSymbol, R_CurrentExpr);
	    UNPROTECT(1);
	    if (R_Visible)
		PrintValueEnv(R_CurrentExpr, rho);
	    if( R_CollectWarnings )
		PrintWarnings();
	    break;
	case PARSE_ERROR:
	    error("syntax error: evaluating expression %d", count);
	    break;
	case PARSE_EOF:
	    return;
	    break;
	case PARSE_INCOMPLETE:
	    /* can't happen: just here to quieten -Wall */
	    break;
	}
    }
}

/* Read-Eval-Print loop with interactive input */
static int prompt_type;
static char BrowsePrompt[20];


char *R_PromptString(int browselevel, int type)
{
    if (R_Slave) {
	BrowsePrompt[0] = '\0';
	return BrowsePrompt;
    }
    else {
	if(type == 1) {
	    if(browselevel) {
		sprintf(BrowsePrompt, "Browse[%d]> ", browselevel);
		return BrowsePrompt;
	    }
	    return (char*)CHAR(STRING_ELT(GetOption(install("prompt"),
						    R_NilValue), 0));
	}
	else {
	    return (char*)CHAR(STRING_ELT(GetOption(install("continue"),
						    R_NilValue), 0));
	}
    }
}

/*
  This is a reorganization of the REPL (Read-Eval-Print Loop) to separate
  the loop from the actions of the body. The motivation is to make the iteration
  code (Rf_ReplIteration) available as a separately callable routine
  to avoid cutting and pasting it when one wants a single iteration
  of the loop. This is needed as we allow different implementations
  of event loops. Currently (summer 2002), we have a package in
  preparation that uses Rf_ReplIteration within either the
  Tcl or Gtk event loop and allows either (or both) loops to
  be used as a replacement for R's loop and take over the event
  handling for the R process.

  The modifications here are intended to leave the semantics of the REPL
  unchanged, just separate into routines. So the variables that maintain
  the state across iterations of the loop are organized into a structure
  and passed to Rf_ReplIteration() from Rf_ReplConsole().
*/


/**
  (local) Structure for maintaining and exchanging the state between
  Rf_ReplConsole and its worker routine Rf_ReplIteration which is the
  implementation of the body of the REPL.

  In the future, we may need to make this accessible to packages
  and so put it into one of the public R header files.
 */
typedef struct {
  ParseStatus    status;
  int            prompt_type;
  int            browselevel;
  unsigned char  buf[1025];
  unsigned char *bufp;
} R_ReplState;


/**
  This is the body of the REPL.
  It attempts to parse the first line or expression of its input,
  and optionally request input from the user if none is available.
  If the input can be parsed correctly,
     i) the resulting expression is evaluated,
    ii) the result assigned to .Last.Value,
   iii) top-level task handlers are invoked.

 If the input cannot be parsed, i.e. there is a syntax error,
 it is incomplete, or we encounter an end-of-file, then we
 change the prompt accordingly.

 The "cursor" for the input buffer is moved to the next starting
 point, i.e. the end of the first line or after the first ;.
 */
int
Rf_ReplIteration(SEXP rho, int savestack, int browselevel, R_ReplState *state)
{
    int c, browsevalue;
    SEXP value;
    Rboolean wasDisplayed = FALSE;

    if(!*state->bufp) {
	    R_Busy(0);
	    if (R_ReadConsole(R_PromptString(browselevel, state->prompt_type),
			      state->buf, 1024, 1) == 0) return(-1);
	    state->bufp = state->buf;
    }
#ifdef SHELL_ESCAPE
    if (*state->bufp == '!') {
#ifdef HAVE_SYSTEM
	    R_system(&(state->buf[1]));
#else
	    Rprintf("error: system commands are not supported in this version of R.\n");
#endif /* HAVE_SYSTEM */
	    state->buf[0] = '\0';
	    return(0);
    }
#endif /* SHELL_ESCAPE */
    while((c = *state->bufp++)) {
	    R_IoBufferPutc(c, &R_ConsoleIob);
	    if(c == ';' || c == '\n') break;
    }

    R_PPStackTop = savestack;
    R_CurrentExpr = R_Parse1Buffer(&R_ConsoleIob, 0, &state->status);

    switch(state->status) {

    case PARSE_NULL:

	    if (browselevel)
		    return(-1);
	    R_IoBufferWriteReset(&R_ConsoleIob);
	    state->prompt_type = 1;
	    return(1);

    case PARSE_OK:

 	    R_IoBufferReadReset(&R_ConsoleIob);
	    R_CurrentExpr = R_Parse1Buffer(&R_ConsoleIob, 1, &state->status);
	    if (browselevel) {
		    browsevalue = ParseBrowser(R_CurrentExpr, rho);
		    if(browsevalue == 1 )
			    return(-1);
		    if(browsevalue == 2 ) {
			    R_IoBufferWriteReset(&R_ConsoleIob);
			    return(0);
		    }
	    }
	    R_Visible = 0;
	    R_EvalDepth = 0;
	    PROTECT(R_CurrentExpr);
	    R_Busy(1);
	    value = eval(R_CurrentExpr, rho);
	    SET_SYMVALUE(R_LastvalueSymbol, value);
	    wasDisplayed = R_Visible;
	    if (R_Visible)
		    PrintValueEnv(value, rho);
	    if (R_CollectWarnings)
		PrintWarnings();
	    Rf_callToplevelHandlers(R_CurrentExpr, value, TRUE, wasDisplayed);
	    R_CurrentExpr = value; /* Necessary? Doubt it. */
	    UNPROTECT(1);
	    R_IoBufferWriteReset(&R_ConsoleIob);
	    state->prompt_type = 1;
	    return(1);

    case PARSE_ERROR:

	    state->prompt_type = 1;
	    error("syntax error");
	    R_IoBufferWriteReset(&R_ConsoleIob);
	    return(1);

    case PARSE_INCOMPLETE:

	    R_IoBufferReadReset(&R_ConsoleIob);
	    state->prompt_type = 2;
	    return(2);

    case PARSE_EOF:

	    return(-1);
	    break;
    }

    return(0);
}

static void R_ReplConsole(SEXP rho, int savestack, int browselevel)
{
    int status;
    R_ReplState state = {0, 1, 0, "", NULL};

    R_IoBufferWriteReset(&R_ConsoleIob);
    state.buf[0] = '\0';
    state.buf[1024] = '\0'; /* stopgap measure if line > 1024 chars */
    state.bufp = state.buf;
    if(R_Verbose)
	REprintf(" >R_ReplConsole(): before \"for(;;)\" {main.c}\n");
    for(;;) {
	status = Rf_ReplIteration(rho, savestack, browselevel, &state);
	if(status < 0)
  	  return;
    }
}


static unsigned char DLLbuf[1024], *DLLbufp;

void R_ReplDLLinit()
{
    R_IoBufferInit(&R_ConsoleIob);
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
    R_IoBufferWriteReset(&R_ConsoleIob);
    prompt_type = 1;
    DLLbuf[0] = '\0';
    DLLbufp = DLLbuf;
}


int R_ReplDLLdo1()
{
    int c;
    ParseStatus status;
    SEXP rho = R_GlobalEnv;

    if(!*DLLbufp) {
	R_Busy(0);
	if (R_ReadConsole(R_PromptString(0, prompt_type), DLLbuf, 1024, 1) == 0)
	    return -1;
	DLLbufp = DLLbuf;
    }
    while((c = *DLLbufp++)) {
	R_IoBufferPutc(c, &R_ConsoleIob);
	if(c == ';' || c == '\n') break;
    }
    R_PPStackTop = 0;
    R_CurrentExpr = R_Parse1Buffer(&R_ConsoleIob, 0, &status);

    switch(status) {
    case PARSE_NULL:
	R_IoBufferWriteReset(&R_ConsoleIob);
	prompt_type = 1;
	break;
    case PARSE_OK:
	R_IoBufferReadReset(&R_ConsoleIob);
	R_CurrentExpr = R_Parse1Buffer(&R_ConsoleIob, 1, &status);
	R_Visible = 0;
	R_EvalDepth = 0;
	PROTECT(R_CurrentExpr);
	R_Busy(1);
	R_CurrentExpr = eval(R_CurrentExpr, rho);
	SET_SYMVALUE(R_LastvalueSymbol, R_CurrentExpr);
	UNPROTECT(1);
	if (R_Visible)
	    PrintValueEnv(R_CurrentExpr, rho);
	if (R_CollectWarnings)
	    PrintWarnings();
	R_IoBufferWriteReset(&R_ConsoleIob);
	R_Busy(0);
	prompt_type = 1;
	break;
    case PARSE_ERROR:
	error("syntax error");
	R_IoBufferWriteReset(&R_ConsoleIob);
	prompt_type = 1;
	break;
    case PARSE_INCOMPLETE:
	R_IoBufferReadReset(&R_ConsoleIob);
	prompt_type = 2;
	break;
    case PARSE_EOF:
	return -1;
	break;
    }
    return prompt_type;
}

/* Main Loop: It is assumed that at this point that operating system */
/* specific tasks (dialog window creation etc) have been performed. */
/* We can now print a greeting, run the .First function and then enter */
/* the read-eval-print loop. */


FILE* R_OpenSysInitFile(void);
FILE* R_OpenSiteFile(void);
FILE* R_OpenInitFile(void);

static void handleInterrupt(int dummy)
{
    R_interrupts_pending = 1;
    signal(SIGINT, handleInterrupt);
}

static void R_LoadProfile(FILE *fparg, SEXP env)
{
    FILE * volatile fp = fparg; /* is this needed? */
    if (fp != NULL) {
	if (! SETJMP(R_Toplevel.cjmpbuf)) {
	    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
#ifdef REINSTALL_SIGNAL_HANDLERS
	    signal(SIGINT, handleInterrupt);
#endif
	    R_ReplFile(fp, env, 0, 0);
	}
	fclose(fp);
    }
}

/* Use this to allow e.g. Win32 malloc to call warning.
   Don't use R-specific type, e.g. Rboolean */
/* int R_Is_Running = 0; now in Defn.h */

void setup_Rmainloop(void)
{
    volatile int doneit;
    volatile SEXP baseEnv;
    SEXP cmd;
    FILE *fp;

    InitConnections(); /* needed to get any output at all */

    /* Initialize the interpreter's internal structures. */

#ifdef HAVE_LOCALE_H
#ifdef Win32
    {
	char *p, Rlocale[1000]; /* Windows' locales can be very long */
	p = getenv("LC_ALL");
	if(p) strcpy(Rlocale, p); else strcpy(Rlocale, "");
	if((p = getenv("LC_CTYPE"))) setlocale(LC_CTYPE, p);
	else setlocale(LC_CTYPE, Rlocale);
	if((p = getenv("LC_COLLATE"))) setlocale(LC_COLLATE, p);
	else setlocale(LC_COLLATE, Rlocale);
	if((p = getenv("LC_TIME"))) setlocale(LC_TIME, p);
	else setlocale(LC_TIME, Rlocale);
    }
#else
    setlocale(LC_CTYPE, "");/*- make ISO-latin1 etc. work for LOCALE users */
    setlocale(LC_COLLATE, "");/*- alphabetically sorting */
    setlocale(LC_TIME, "");/*- names and defaults for date-time formats */
    /* setlocale(LC_MESSAGES,""); */
#endif
#endif
#if defined(Unix) || defined(Win32)
    InitTempDir(); /* must be before InitEd */
#endif
    InitMemory();
    InitNames();
    InitGlobalEnv();
    InitDynload();
    InitOptions();
    InitEd();
    InitArithmetic();
    InitColors();
    InitGraphics();
    R_Is_Running = 1;
#ifdef HAVE_AQUA 
    if( (strcmp(R_GUIType, "AQUA") == 0) && !CocoaGUI && !useCocoa){ 
		InitAquaIO(); /* must be after InitTempDir() */
		RSetConsoleWidth();
	}
#endif
#ifdef HAVE_LANGINFO_CODESET
    utf8locale = strcmp(nl_langinfo(CODESET), "UTF-8") == 0;
#endif
    /* gc_inhibit_torture = 0; */

    /* Initialize the global context for error handling. */
    /* This provides a target for any non-local gotos */
    /* which occur during error handling */

    R_Toplevel.nextcontext = NULL;
    R_Toplevel.callflag = CTXT_TOPLEVEL;
    R_Toplevel.cstacktop = 0;
    R_Toplevel.promargs = R_NilValue;
    R_Toplevel.callfun = R_NilValue;
    R_Toplevel.call = R_NilValue;
    R_Toplevel.cloenv = R_NilValue;
    R_Toplevel.sysparent = R_NilValue;
    R_Toplevel.conexit = R_NilValue;
    R_Toplevel.vmax = NULL;
#ifdef BYTECODE
    R_Toplevel.nodestack = R_BCNodeStackTop;
# ifdef BC_INT_STACK
    R_Toplevel.intstack = R_BCIntStackTop;
# endif
#endif
    R_Toplevel.cend = NULL;
    R_Toplevel.intsusp = FALSE;
#ifdef NEW_CONDITION_HANDLING
    R_Toplevel.handlerstack = R_HandlerStack;
    R_Toplevel.restartstack = R_RestartStack;
#endif
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;

    R_Warnings = R_NilValue;

    baseEnv = R_BaseNamespace;

    /* Set up some global variables */
    Init_R_Variables(baseEnv);

    /* On initial entry we open the base language package and begin by
       running the repl on it.
       If there is an error we pass on to the repl.
       Perhaps it makes more sense to quit gracefully?
    */

    fp = R_OpenLibraryFile("base");
    if (fp == NULL) {
	R_Suicide("unable to open the base package\n");
    }

    doneit = 0;
    SETJMP(R_Toplevel.cjmpbuf);
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
    signal(SIGINT, handleInterrupt);
    signal(SIGUSR1,onsigusr1);
    signal(SIGUSR2,onsigusr2);
#ifdef Unix
    signal(SIGPIPE, onpipe);
#endif
    if (!doneit) {
	doneit = 1;
	R_ReplFile(fp, baseEnv, 0, 0);
    }
    fclose(fp);

    /* This is where we source the system-wide, the site's and the
       user's profile (in that order).  If there is an error, we
       drop through to further processing.
    */

    R_LoadProfile(R_OpenSysInitFile(), baseEnv);

    if (strcmp(R_GUIType, "Tk") == 0) {
	char buf[256];

	snprintf(buf, 256, "%s/library/tcltk/exec/Tk-frontend.R", R_Home);
	R_LoadProfile(R_fopen(buf, "r"), R_GlobalEnv);
    }

    /* Print a platform and version dependent greeting and a pointer to
     * the copyleft.
     */
    if(!R_Quiet) {
	PrintGreeting();
	if(utf8locale)
	    R_ShowMessage("WARNING: UTF-8 locales are not currently supported\n");
    }

    R_LoadProfile(R_OpenSiteFile(), baseEnv);
    R_LoadProfile(R_OpenInitFile(), R_GlobalEnv);

    /* This is where we try to load a user's saved data.
       The right thing to do here is very platform dependent.
       E.g. under Unix we look in a special hidden file and on the Mac
       we look in any documents which might have been double clicked on
       or dropped on the application.
    */
    doneit = 0;
    SETJMP(R_Toplevel.cjmpbuf);
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
#ifdef REINSTALL_SIGNAL_HANDLERS
    signal(SIGINT, handleInterrupt);
    signal(SIGUSR1,onsigusr1);
    signal(SIGUSR2,onsigusr2);
#ifdef Unix
    signal(SIGPIPE, onpipe);
#endif
#endif
    if (!doneit) {
	doneit = 1;
	R_InitialData();
    }
    else
    	R_Suicide("unable to restore saved data in .RData\n");

    /* Initial Loading is done.
       At this point we try to invoke the .First Function.
       If there is an error we continue. */

    doneit = 0;
    SETJMP(R_Toplevel.cjmpbuf);
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
#ifdef REINSTALL_SIGNAL_HANDLERS
    signal(SIGINT, handleInterrupt);
#endif
    if (!doneit) {
	doneit = 1;
	PROTECT(cmd = install(".First"));
	R_CurrentExpr = findVar(cmd, R_GlobalEnv);
	if (R_CurrentExpr != R_UnboundValue &&
	    TYPEOF(R_CurrentExpr) == CLOSXP) {
	        PROTECT(R_CurrentExpr = lang1(cmd));
	        R_CurrentExpr = eval(R_CurrentExpr, R_GlobalEnv);
	        UNPROTECT(1);
	}
	UNPROTECT(1);
    }
    /* Try to invoke the .First.sys function, which loads the default packages.
       If there is an error we continue. */

    doneit = 0;
    SETJMP(R_Toplevel.cjmpbuf);
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
#ifdef REINSTALL_SIGNAL_HANDLERS
    signal(SIGINT, handleInterrupt);
#endif
    if (!doneit) {
	doneit = 1;
	PROTECT(cmd = install(".First.sys"));
	R_CurrentExpr = findVar(cmd, baseEnv);
	if (R_CurrentExpr != R_UnboundValue &&
	    TYPEOF(R_CurrentExpr) == CLOSXP) {
	        PROTECT(R_CurrentExpr = lang1(cmd));
	        R_CurrentExpr = eval(R_CurrentExpr, R_GlobalEnv);
	        UNPROTECT(1);
	}
	UNPROTECT(1);
    }
    /* gc_inhibit_torture = 0; */
}

void end_Rmainloop(void)
{
    Rprintf("\n");
    /* run the .Last function. If it gives an error, will drop back to main
       loop. */
    R_CleanUp(SA_DEFAULT, 0, 1);
}

static void onpipe(int dummy)
{
    /* do nothing */
    signal(SIGPIPE, onpipe);
}

void run_Rmainloop(void)
{
    /* Here is the real R read-eval-loop. */
    /* We handle the console until end-of-file. */
    R_IoBufferInit(&R_ConsoleIob);
    SETJMP(R_Toplevel.cjmpbuf);
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
#ifdef REINSTALL_SIGNAL_HANDLERS
    signal(SIGINT, handleInterrupt);
    signal(SIGUSR1,onsigusr1);
    signal(SIGUSR2,onsigusr2);
#ifdef Unix
    signal(SIGPIPE, onpipe);
#endif
#endif
    R_ReplConsole(R_GlobalEnv, 0, 0);
    end_Rmainloop(); /* must go here */
}

void mainloop(void)
{
    setup_Rmainloop();
    run_Rmainloop();
    /* NO! Don't do that! It ends up in a longjmp for which the
       setjmp is inside run_Rmainloop! -pd
    end_Rmainloop();
    */
}

/*this functionality now appears in 3
  places-jump_to_toplevel/profile/here */

static void printwhere(void)
{
  RCNTXT *cptr;
  int lct = 1;

  for (cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext) {
    if ((cptr->callflag & CTXT_FUNCTION) &&
	(TYPEOF(cptr->call) == LANGSXP)) {
	Rprintf("where %d: ",lct++);
	PrintValue(cptr->call);
    }
  }
  Rprintf("\n");
}


int Rf_ParseBrowser(SEXP CExpr, SEXP rho)
{
    int rval=0;
    if (isSymbol(CExpr)) {
	if (!strcmp(CHAR(PRINTNAME(CExpr)),"n")) {
	    SET_DEBUG(rho, 1);
	    rval=1;
	}
	if (!strcmp(CHAR(PRINTNAME(CExpr)),"c")) {
	    rval=1;
	    SET_DEBUG(rho, 0);
	}
	if (!strcmp(CHAR(PRINTNAME(CExpr)),"cont")) {
	    rval=1;
	    SET_DEBUG(rho, 0);
	}
	if (!strcmp(CHAR(PRINTNAME(CExpr)),"Q")) {

	    /* Run onexit/cend code for everything above the target.
               The browser context is still on the stack, so any error
               will drop us back to the current browser.  Not clear
               this is a good thing.  Also not clear this should still
               be here now that jump_to_toplevel is used for the
               jump. */
	    R_run_onexits(R_ToplevelContext);

	    /* this is really dynamic state that should be managed as such */
	    R_BrowseLevel = 0;
	    SET_DEBUG(rho,0); /*PR#1721*/

	    jump_to_toplevel();
	}
	if (!strcmp(CHAR(PRINTNAME(CExpr)),"where")) {
	    printwhere();
	    SET_DEBUG(rho, 1);
	    rval=2;
	}
    }
    return rval;
}

/* registering this as a cend exit procedure makes sure R_BrowseLevel
   is maintained across LONGJMP's */
static void browser_cend(void *data)
{
    int *psaved = data;
    R_BrowseLevel = *psaved - 1;
}

SEXP do_browser(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    RCNTXT *saveToplevelContext;
    RCNTXT *saveGlobalContext;
    RCNTXT thiscontext, returncontext, *cptr;
    int savestack;
    int savebrowselevel;
    SEXP topExp;

    /* Save the evaluator state information */
    /* so that it can be restored on exit. */

    savebrowselevel = R_BrowseLevel + 1;
    savestack = R_PPStackTop;
    PROTECT(topExp = R_CurrentExpr);
    saveToplevelContext = R_ToplevelContext;
    saveGlobalContext = R_GlobalContext;

    if (!DEBUG(rho)) {
	cptr=R_GlobalContext;
	while ( !(cptr->callflag & CTXT_FUNCTION) && cptr->callflag )
	    cptr = cptr->nextcontext;
	Rprintf("Called from: ");
	PrintValueRec(cptr->call,rho);
    }

    R_ReturnedValue = R_NilValue;

    /* Here we establish two contexts.  The first */
    /* of these provides a target for return */
    /* statements which a user might type at the */
    /* browser prompt.  The (optional) second one */
    /* acts as a target for error returns. */

    begincontext(&returncontext, CTXT_BROWSER, call, rho,
		 R_NilValue, R_NilValue, R_NilValue);
    returncontext.cend = browser_cend;
    returncontext.cenddata = &savebrowselevel;
    if (!SETJMP(returncontext.cjmpbuf)) {
	begincontext(&thiscontext, CTXT_RESTART, R_NilValue, rho,
		     R_NilValue, R_NilValue, R_NilValue);
	if (SETJMP(thiscontext.cjmpbuf)) {
	    SET_RESTART_BIT_ON(thiscontext.callflag);
	    R_ReturnedValue = R_NilValue;
	    R_Visible = 0;
	}
	R_GlobalContext = &thiscontext;
#ifdef NEW_CONDITION_HANDLING
	R_InsertRestartHandlers(&thiscontext, TRUE);
#endif
	R_BrowseLevel = savebrowselevel;
#ifdef REINSTALL_SIGNAL_HANDLERS
	signal(SIGINT, handleInterrupt);
#endif
	R_ReplConsole(rho, savestack, R_BrowseLevel);
	endcontext(&thiscontext);
    }
    endcontext(&returncontext);

    /* Reset the interpreter state. */

    R_CurrentExpr = topExp;
    UNPROTECT(1);
    R_PPStackTop = savestack;
    R_CurrentExpr = topExp;
    R_ToplevelContext = saveToplevelContext;
    R_GlobalContext = saveGlobalContext;
    R_BrowseLevel--;
    return R_ReturnedValue;
}

void R_dot_Last(void)
{
    SEXP cmd;

    /* Run the .Last function. */
    /* Errors here should kick us back into the repl. */

    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
    PROTECT(cmd = install(".Last"));
    R_CurrentExpr = findVar(cmd, R_GlobalEnv);
    if (R_CurrentExpr != R_UnboundValue && TYPEOF(R_CurrentExpr) == CLOSXP) {
	PROTECT(R_CurrentExpr = lang1(cmd));
	R_CurrentExpr = eval(R_CurrentExpr, R_GlobalEnv);
	UNPROTECT(1);
    }
    UNPROTECT(1);
}

SEXP do_quit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    char *tmp;
    int ask=SA_DEFAULT, status, runLast;

    if(R_BrowseLevel) {
	warning("can't quit from browser");
	return R_NilValue;
    }
    if( !isString(CAR(args)) )
	errorcall(call,"one of \"yes\", \"no\", \"ask\" or \"default\" expected.");
    tmp = CHAR(STRING_ELT(CAR(args), 0));
    if( !strcmp(tmp, "ask") ) {
	ask = SA_SAVEASK;
	if(!R_Interactive)
	    warningcall(call, "save=\"ask\" in non-interactive use: command-line default will be used");
    } else if( !strcmp(tmp, "no") )
	ask = SA_NOSAVE;
    else if( !strcmp(tmp, "yes") )
	ask = SA_SAVE;
    else if( !strcmp(tmp, "default") )
	ask = SA_DEFAULT;
    else
	errorcall(call, "unrecognized value of save");
    status = asInteger(CADR(args));
    if (status == NA_INTEGER) {
        warningcall(call, "invalid status, 0 assumed");
	runLast = 0;
    }
    runLast = asLogical(CADDR(args));
    if (runLast == NA_LOGICAL) {
        warningcall(call, "invalid runLast, FALSE assumed");
	runLast = 0;
    }
    /* run the .Last function. If it gives an error, will drop back to main
       loop. */
    R_CleanUp(ask, status, runLast);
    exit(0);
    /*NOTREACHED*/
}


#include <R_ext/Callbacks.h>

static R_ToplevelCallbackEl *Rf_ToplevelTaskHandlers = NULL;

/**
  This is the C-level entry point for registering a handler
  that is to be called when each top-level task completes.

  Perhaps we need names to make removing them handlers easier
  since they could be more identified by an invariant (rather than
  position).
 */
R_ToplevelCallbackEl *
Rf_addTaskCallback(R_ToplevelCallback cb, void *data, 
		   void (*finalizer)(void *), const char *name, int *pos)
{
    int which;
    R_ToplevelCallbackEl *el;
    el = (R_ToplevelCallbackEl *) malloc(sizeof(R_ToplevelCallbackEl));
    if(!el)
	error("cannot allocate space for toplevel callback element.");

    el->data = data;
    el->cb = cb;
    el->next = NULL;
    el->finalizer = finalizer;

    if(Rf_ToplevelTaskHandlers == NULL) {
	Rf_ToplevelTaskHandlers = el;
	which = 0;
    } else {
	R_ToplevelCallbackEl *tmp;
        tmp = Rf_ToplevelTaskHandlers;
	which = 1;
	while(tmp->next) {
	    which++;
	    tmp = tmp->next;
	}
        tmp->next = el;
    }

    if(!name) {
        char buf[5];
	sprintf(buf, "%d", which+1);
        el->name = strdup(buf);
    } else
	el->name = strdup(name);

    if(pos)
	*pos = which;

    return(el);
}

Rboolean
Rf_removeTaskCallbackByName(const char *name)
{
    R_ToplevelCallbackEl *el = Rf_ToplevelTaskHandlers, *prev = NULL;
    Rboolean status = TRUE;

    if(!Rf_ToplevelTaskHandlers) {
	return(FALSE); /* error("there are no task callbacks registered"); */
    }

    while(el) {
        if(strcmp(el->name, name) == 0) {
	    if(prev == NULL) {
		Rf_ToplevelTaskHandlers = el->next;
	    } else {
		prev->next = el->next;
	    }
	    break;
	}
	prev = el;
	el = el->next;
    }
    if(el) {
	if(el->finalizer)
	    el->finalizer(el->data);
	free(el->name);
	free(el);
    } else {
	status = FALSE;
    }
    return(status);
}

/**
  Remove the top-level task handler/callback identified by
  its position in the list of callbacks.
 */
Rboolean
Rf_removeTaskCallbackByIndex(int id)
{
    R_ToplevelCallbackEl *el = Rf_ToplevelTaskHandlers, *tmp = NULL;
    Rboolean status = TRUE;

    if(id < 0)
	error("negative index passed to R_removeTaskCallbackByIndex");

    if(Rf_ToplevelTaskHandlers) {
	if(id == 0) {
	    tmp = Rf_ToplevelTaskHandlers;
	    Rf_ToplevelTaskHandlers = Rf_ToplevelTaskHandlers->next;
	} else {
	    int i = 0;
	    while(el && i < (id-1)) {
		el = el->next;
		i++;
	    }

	    if(i == (id -1) && el) {
		tmp = el->next;
		el->next = (tmp ? tmp->next : NULL);
	    }
	}
    }
    if(tmp) {
	if(tmp->finalizer)
	    tmp->finalizer(tmp->data);
	free(tmp->name);
	free(tmp);
    } else {
	status = FALSE;
    }

    return(status);
}


/**
  R-level entry point to remove an entry from the
  list of top-level callbacks. `which' should be an
  integer and give us the 0-based index of the element
  to be removed from the list.

  @see Rf_RemoveToplevelCallbackByIndex(int)
 */
SEXP
R_removeTaskCallback(SEXP which)
{
    int id;
    Rboolean val;
    SEXP status;

    if(TYPEOF(which) == STRSXP) {
	val = Rf_removeTaskCallbackByName(CHAR(STRING_ELT(which, 0)));
    } else {
	id = asInteger(which) - 1;
	val = Rf_removeTaskCallbackByIndex(id);
    }
    status = allocVector(LGLSXP, 1);
    LOGICAL(status)[0] = val;

    return(status);
}

SEXP
R_getTaskCallbackNames()
{
    SEXP ans;
    R_ToplevelCallbackEl *el;
    int n = 0;

    el = Rf_ToplevelTaskHandlers;
    while(el) {
	n++;
	el = el->next;
    }
    PROTECT(ans = allocVector(STRSXP, n));
    n = 0;
    el = Rf_ToplevelTaskHandlers;
    while(el) {
	SET_STRING_ELT(ans, n, allocString(strlen(el->name)));
	strcpy(CHAR(STRING_ELT(ans, n)), el->name);
	n++;
	el = el->next;
    }
    UNPROTECT(1);
    return(ans);
}

/**
  Invokes each of the different handlers giving the
  top-level expression that was just evaluated,
  the resulting value from the evaluation, and
  whether the task succeeded. The last may be useful
  if a handler is also called as part of the error handling.
  We also have information about whether the result was printed or not.
  We currently do not pass this to the handler.
 */

  /* Flag to ensure that the top-level handlers aren't called recursively.
     Simple state to indicate that they are currently being run. */
static Rboolean Rf_RunningToplevelHandlers = FALSE;

void
Rf_callToplevelHandlers(SEXP expr, SEXP value, Rboolean succeeded, 
			Rboolean visible)
{
    R_ToplevelCallbackEl *h, *prev = NULL;
    Rboolean again;

    if(Rf_RunningToplevelHandlers == TRUE)
	return;

    h = Rf_ToplevelTaskHandlers;
    Rf_RunningToplevelHandlers = TRUE;
    while(h) {
	again = (h->cb)(expr, value, succeeded, visible, h->data);
	if(R_CollectWarnings) {
	    REprintf("warning messages from top-level task callback `%s'\n", 
		     h->name);
	    PrintWarnings();
	}
        if(again) {
	    prev = h;
	    h = h->next;
	} else {
	    R_ToplevelCallbackEl *tmp;
	    tmp = h;
	    if(prev)
		prev->next = h->next;
	    h = h->next;
	    if(tmp == Rf_ToplevelTaskHandlers)
		Rf_ToplevelTaskHandlers = h;
            if(tmp->finalizer)
		tmp->finalizer(tmp->data);
            free(tmp);
	}
    }

    Rf_RunningToplevelHandlers = FALSE;
}


Rboolean
R_taskCallbackRoutine(SEXP expr, SEXP value, Rboolean succeeded,
		      Rboolean visible, void *userData)
{
    SEXP f = (SEXP) userData;
    SEXP e, tmp, val, cur;
    int errorOccurred;
    Rboolean again;
    Rboolean useData;
    useData = LOGICAL(VECTOR_ELT(f, 2))[0];

    PROTECT(e = allocVector(LANGSXP, 5 + useData));
    SETCAR(e, VECTOR_ELT(f, 0));
    cur = CDR(e);
    SETCAR(cur, tmp = allocVector(LANGSXP, 2));
        SETCAR(tmp, install("quote"));
        SETCAR(CDR(tmp), expr);
    cur = CDR(cur);
    SETCAR(cur, value);
    cur = CDR(cur);
    SETCAR(cur, tmp = allocVector(LGLSXP, 1));
    LOGICAL(tmp)[0] = succeeded;
    cur = CDR(cur);
    SETCAR(cur, tmp = allocVector(LGLSXP, 1));
    LOGICAL(tmp)[0] = visible;
    if(useData) {
	cur = CDR(cur);
	SETCAR(cur, VECTOR_ELT(f, 1));
    }

    val = R_tryEval(e, NULL, &errorOccurred);
    if(!errorOccurred) {
	PROTECT(val);
	if(TYPEOF(val) != LGLSXP) {
              /* It would be nice to identify the function. */
	    warning("top-level task callback did not return a logical value");
	}
	again = asLogical(val);
	UNPROTECT(1);
    } else {
        /* warning("error occurred in top-level task callback\n"); */
	again = FALSE;
    }
    return(again);
}

SEXP
R_addTaskCallback(SEXP f, SEXP data, SEXP useData, SEXP name)
{
    SEXP internalData;
    SEXP index;
    R_ToplevelCallbackEl *el;
    char *tmpName = NULL;

    internalData = allocVector(VECSXP, 3);
    R_PreserveObject(internalData);
    SET_VECTOR_ELT(internalData, 0, f);
    SET_VECTOR_ELT(internalData, 1, data);
    SET_VECTOR_ELT(internalData, 2, useData);

    if(length(name))
	tmpName = CHAR(STRING_ELT(name, 0));

    PROTECT(index = allocVector(INTSXP, 1));
    el = Rf_addTaskCallback(R_taskCallbackRoutine,  internalData,
			    (void (*)(void*)) R_ReleaseObject, tmpName, 
			    INTEGER(index));

    if(length(name) == 0) {
	PROTECT(name = allocVector(STRSXP, 1));
        SET_STRING_ELT(name, 0, allocString(strlen(el->name)));
	strcpy(CHAR(STRING_ELT(name, 0)), el->name);

        setAttrib(index, R_NamesSymbol, name);
	UNPROTECT(1);
    } else {
        setAttrib(index, R_NamesSymbol, name);
    }

    UNPROTECT(1);
    return(index);
}

#undef __MAIN__
