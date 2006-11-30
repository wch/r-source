/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2006   The R Development Core Team
 *  Copyright (C) 2002-2005  The R Foundation
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
 *  writing to the Free Software Foundation, Inc., 51 Franklin Street
 *  Fifth Floor, Boston, MA 02110-1301  USA.
 */

/* <UTF8> char here is either ASCII or handled as a whole */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define __MAIN__
#include "Defn.h"
#include "Rinterface.h"
#include "Graphics.h"
#include <Rdevices.h>		/* for InitGraphics */
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

#ifdef ENABLE_NLS
void attribute_hidden nl_Rdummy()
{
    /* force this in as packages use it */
    dgettext("R", "dummy - do not translate");
}
#endif


/* The 'real' main() program is in ../<SYSTEM>/system.c */
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

void Rf_callToplevelHandlers(SEXP expr, SEXP value, Rboolean succeeded,
			     Rboolean visible);

static int ParseBrowser(SEXP, SEXP);

 
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
	    R_Visible = FALSE;
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
	    parseError(R_NilValue, count);
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
						    R_BaseEnv), 0));
	}
	else {
	    return (char*)CHAR(STRING_ELT(GetOption(install("continue"),
						    R_BaseEnv), 0));
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
  unsigned char  buf[CONSOLE_BUFFER_SIZE+1];
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
			      state->buf, CONSOLE_BUFFER_SIZE, 1) == 0)
		return(-1);
	    state->bufp = state->buf;
    }
#ifdef SHELL_ESCAPE
    if (*state->bufp == '!') {
#ifdef HAVE_SYSTEM
	    R_system(&(state->buf[1]));
#else
	    REprintf(_("error: system commands are not supported in this version of R.\n"));
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

	/* The intention here is to break on CR but not on other 
	   null statements: see PR#9063 */
	if (browselevel && !strcmp((char *) state->buf, "\n")) return -1;
	R_IoBufferWriteReset(&R_ConsoleIob);
	state->prompt_type = 1;
	return(1);

    case PARSE_OK:

	R_IoBufferReadReset(&R_ConsoleIob);
	R_CurrentExpr = R_Parse1Buffer(&R_ConsoleIob, 1, &state->status);
	if (browselevel) {
	    browsevalue = ParseBrowser(R_CurrentExpr, rho);
	    if(browsevalue == 1) return(-1);
	    if(browsevalue == 2) {
		R_IoBufferWriteReset(&R_ConsoleIob);
		return(0);
	    }
	}
	R_Visible = FALSE;
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
	parseError(R_NilValue, 0);
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
    R_ReplState state = { PARSE_NULL, 1, 0, "", NULL};

    R_IoBufferWriteReset(&R_ConsoleIob);
    state.buf[0] = '\0';
    state.buf[CONSOLE_BUFFER_SIZE] = '\0'; 
    /* stopgap measure if line > CONSOLE_BUFFER_SIZE chars */
    state.bufp = state.buf;
    if(R_Verbose)
	REprintf(" >R_ReplConsole(): before \"for(;;)\" {main.c}\n");
    for(;;) {
	status = Rf_ReplIteration(rho, savestack, browselevel, &state);
	if(status < 0)
  	  return;
    }
}


static unsigned char DLLbuf[CONSOLE_BUFFER_SIZE+1], *DLLbufp;

void R_ReplDLLinit()
{
    R_IoBufferInit(&R_ConsoleIob);
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
    R_IoBufferWriteReset(&R_ConsoleIob);
    prompt_type = 1;
    DLLbuf[0] = DLLbuf[CONSOLE_BUFFER_SIZE] = '\0';
    DLLbufp = DLLbuf;
}


int R_ReplDLLdo1()
{
    int c;
    ParseStatus status;
    SEXP rho = R_GlobalEnv;

    if(!*DLLbufp) {
	R_Busy(0);
	if (R_ReadConsole(R_PromptString(0, prompt_type), DLLbuf,
			  CONSOLE_BUFFER_SIZE, 1) == 0)
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
	R_Visible = FALSE;
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
	parseError(R_NilValue, 0);
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

static RETSIGTYPE handleInterrupt(int dummy)
{
    R_interrupts_pending = 1;
    signal(SIGINT, handleInterrupt);
}


#ifdef Win32
static int num_caught = 0;

static void win32_segv(int signum)
{
    /* NB: stack overflow is not an access violation on Win32 */
    {   /* A simple customized print of the traceback */
	SEXP trace, p, q;
	int line = 1, i;
	PROTECT(trace = R_GetTraceback(0));
	if(trace != R_NilValue) {
	    REprintf("\nTraceback:\n");
	    for(p = trace; p != R_NilValue; p = CDR(p), line++) {
		q = CAR(p); /* a character vector */
		REprintf("%2d: ", line);
		for(i = 0; i < LENGTH(q); i++)
		    REprintf("%s", CHAR(STRING_ELT(q, i)));
		REprintf("\n");
	    }
	    UNPROTECT(1);
	}
    }
    num_caught++;
    if(num_caught < 10) signal(signum, win32_segv);
    if(signum == SIGILL)  
	error("caught access violation - continue with care");
    else
	error("caught access violation - continue with care");
}
#endif

#if defined(HAVE_SIGALTSTACK) && defined(HAVE_SIGACTION) && defined(HAVE_SIGEMPTYSET)

/* NB: this really isn't safe, but suffices for experimentation for now.
   In due course just set a flag and do this after the return.  OTOH,
   if we do want to bail out with a core dump, need to do that here.

   2005-12-17 BDR */

static unsigned char ConsoleBuf[CONSOLE_BUFFER_SIZE];
extern void R_CleanTempDir();

static void sigactionSegv(int signum, siginfo_t *ip, void *context)
{
    char *s;

    /* First check for stack overflow if we know the stack position.
       We assume anything within 16Mb beyond the stack end is a stack overflow.
     */
    if(signum == SIGSEGV && (ip != (siginfo_t *)0) && 
       (long) R_CStackStart != -1) {
	long addr = (long) ip->si_addr;
	long diff = (R_CStackDir > 0) ? R_CStackStart - addr:
	    addr - R_CStackStart;
	long upper = 0x1000000;  /* 16Mb */
	if((long) R_CStackLimit != -1) upper += R_CStackLimit;
	if(diff > 0 && diff < upper) {
	    REprintf(_("Error: segfault from C stack overflow\n"));
	    jump_to_toplevel();	
	}    
    }

    /* need to take off stack checking as stack base has changed */
    R_CStackLimit = (unsigned long)-1;

    /* Do not translate these messages */
    REprintf("\n *** caught %s ***\n", 
	     signum == SIGILL ? "illegal operation" : 
	     signum == SIGBUS ? "bus error" : "segfault");
    if(ip != (siginfo_t *)0) {
	if(signum == SIGILL) {
	    
	    switch(ip->si_code) {
#ifdef ILL_ILLOPC
	    case ILL_ILLOPC:
		s = "illegal opcode";
		break;
#endif
#ifdef ILL_ILLOPN
	    case ILL_ILLOPN:
		s = "illegal operand";
		break;
#endif
#ifdef ILL_ILLADR
	    case ILL_ILLADR:
		s = "illegal addressing mode";
		break;
#endif
#ifdef ILL_ILLTRP
	    case ILL_ILLTRP:
		s = "illegal trap";
		break;
#endif
#ifdef ILL_COPROC
	    case ILL_COPROC:
		s = "coprocessor error";
		break;
#endif
	    default:
		s = "unknown";
		break;
	    }
	} else if(signum == SIGBUS)
	    switch(ip->si_code) {
#ifdef BUS_ADRALN
	    case BUS_ADRALN:
		s = "invalid alignment";
		break;
#endif
#ifdef BUS_ADRERR /* not on MacOS X, apparently */
	    case BUS_ADRERR:
		s = "non-existent physical address";
		break;
#endif
#ifdef BUS_OBJERR /* not on MacOS X, apparently */
	    case BUS_OBJERR:
		s = "object specific hardware error";
		break;
#endif
	    default:
		s = "unknown";
		break;
	    }
	else
	    switch(ip->si_code) {
#ifdef SEGV_MAPERR
	    case SEGV_MAPERR:
		s = "memory not mapped";
		break;
#endif
#ifdef SEGV_ACCERR
	    case SEGV_ACCERR:
		s = "invalid permissions";
		break;
#endif
	    default:
		s = "unknown";
		break;
	    }
	REprintf("address %p, cause '%s'\n", ip->si_addr, s);
    }
    {   /* A simple customized print of the traceback */
	SEXP trace, p, q;
	int line = 1, i;
	PROTECT(trace = R_GetTraceback(0));
	if(trace != R_NilValue) {
	    REprintf("\nTraceback:\n");
	    for(p = trace; p != R_NilValue; p = CDR(p), line++) {
		q = CAR(p); /* a character vector */
		REprintf("%2d: ", line);
		for(i = 0; i < LENGTH(q); i++)
		    REprintf("%s", CHAR(STRING_ELT(q, i)));
		REprintf("\n");
	    }
	    UNPROTECT(1);
	}
    }
    if(R_Interactive) {
	REprintf("\nPossible actions:\n1: %s\n2: %s\n3: %s\n4: %s\n", 
		 "abort (with core dump)", 
		 "normal R exit", 
		 "exit R without saving workspace",
		 "exit R saving workspace");
	while(1) {
	    if(R_ReadConsole("Selection: ", ConsoleBuf, CONSOLE_BUFFER_SIZE, 
			     0) > 0) {
		if(ConsoleBuf[0] == '1') break;
		if(ConsoleBuf[0] == '2') R_CleanUp(SA_DEFAULT, 0, 1);
		if(ConsoleBuf[0] == '3') R_CleanUp(SA_NOSAVE, 70, 0);
		if(ConsoleBuf[0] == '4') R_CleanUp(SA_SAVE, 71, 0);
	    }
	}
    }
    REprintf("aborting ...\n");
    R_CleanTempDir();
    /* now do normal behaviour, e.g. core dump */
    signal(signum, SIG_DFL);
    raise(signum);
}

#ifndef SIGSTKSZ
# define SIGSTKSZ 8192    /* just a guess */
#endif

#ifdef HAVE_STACK_T
static stack_t sigstk;
#else
static struct sigaltstack sigstk;
#endif
static void *signal_stack;

#define R_USAGE 100000 /* Just a guess */
static void init_signal_handlers()
{
    /* <FIXME> may need to reinstall this if we do recover. */
    struct sigaction sa;
    signal_stack = malloc(SIGSTKSZ + R_USAGE);
    if (signal_stack != NULL) {
        sigstk.ss_sp = signal_stack;
        sigstk.ss_size = SIGSTKSZ + R_USAGE;
        sigstk.ss_flags = 0;
        if(sigaltstack(&sigstk, NULL) < 0)
	    warning("failed to set alternate signal stack");
    } else
	warning("failed to allocate alternate signal stack");
    sa.sa_sigaction = sigactionSegv;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_ONSTACK | SA_SIGINFO;
    sigaction(SIGSEGV, &sa, NULL);
    sigaction(SIGILL, &sa, NULL);
#ifdef SIGBUS
    sigaction(SIGBUS, &sa, NULL);
#endif

    signal(SIGINT,  handleInterrupt);
    signal(SIGUSR1, onsigusr1);
    signal(SIGUSR2, onsigusr2);
    signal(SIGPIPE, SIG_IGN);
}

#else /* not sigaltstack and sigaction and sigemptyset*/
static void init_signal_handlers()
{
    signal(SIGINT,  handleInterrupt);
    signal(SIGUSR1, onsigusr1);
    signal(SIGUSR2, onsigusr2);
#ifndef Win32
    signal(SIGPIPE, SIG_IGN);
#else
    signal(SIGSEGV, win32_segv);
    signal(SIGILL, win32_segv);
#endif
}
#endif


static void R_LoadProfile(FILE *fparg, SEXP env)
{
    FILE * volatile fp = fparg; /* is this needed? */
    if (fp != NULL) {
	if (! SETJMP(R_Toplevel.cjmpbuf)) {
	    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
	    R_ReplFile(fp, env, 0, 0);
	}
	fclose(fp);
    }
}


int R_SignalHandlers = 1;  /* Exposed in R_interface.h */

/* Use this to allow e.g. Win32 malloc to call warning.
   Don't use R-specific type, e.g. Rboolean */
/* int R_Is_Running = 0; now in Defn.h */

void setup_Rmainloop(void)
{
    volatile int doneit;
    volatile SEXP baseEnv;
    SEXP cmd;
    FILE *fp;
#ifdef ENABLE_NLS
    char localedir[PATH_MAX+20];
#endif

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
	/* LC_CTYPE=C bombs in mingwex */
	if(strcmp(setlocale(LC_CTYPE, NULL), "C") == 0) 
	    setlocale(LC_CTYPE, "en");
	if((p = getenv("LC_COLLATE"))) setlocale(LC_COLLATE, p);
	else setlocale(LC_COLLATE, Rlocale);
	if((p = getenv("LC_TIME"))) setlocale(LC_TIME, p);
	else setlocale(LC_TIME, Rlocale);
	if((p = getenv("LC_MONETARY"))) setlocale(LC_MONETARY, p);
	else setlocale(LC_MONETARY, Rlocale);
	/* Windows does not have LC_MESSAGES */
    }
#else
    setlocale(LC_CTYPE, "");/*- make ISO-latin1 etc. work for LOCALE users */
    setlocale(LC_COLLATE, "");/*- alphabetically sorting */
    setlocale(LC_TIME, "");/*- names and defaults for date-time formats */
    setlocale(LC_MONETARY, "");/*- currency units */
#ifdef ENABLE_NLS
    setlocale(LC_MESSAGES,""); /* language for messages */
#endif
    /* NB: we do not set LC_NUMERIC */
#ifdef LC_PAPER
    setlocale(LC_PAPER,"");
#endif
#ifdef LC_MEASUREMENT
    setlocale(LC_MEASUREMENT,"");
#endif
#endif
#ifdef ENABLE_NLS
    /* This ought to have been done earlier, but be sure */
    textdomain(PACKAGE);
    {
	char *p = getenv("R_SHARE_DIR");
	if(p) {
	    strcpy(localedir, p);
	    strcat(localedir, "/locale");
	} else {
	    strcpy(localedir, R_Home);
	    strcat(localedir, "/share/locale");
	}
    }
    bindtextdomain(PACKAGE, localedir);
    strcpy(localedir, R_Home); strcat(localedir, "/library/base/po");
    bindtextdomain("R-base", localedir);
#endif
#endif

    InitTempDir(); /* must be before InitEd */
    InitMemory();
    InitNames();
    InitBaseEnv();
    InitGlobalEnv();
    InitDynload();
    InitOptions();
    InitEd();
    InitArithmetic();
    InitColors();
    InitGraphics();
    R_Is_Running = 1;
#ifdef HAVE_LANGINFO_CODESET
    utf8locale = strcmp(nl_langinfo(CODESET), "UTF-8") == 0;
#endif
#ifdef SUPPORT_MBCS
    mbcslocale = MB_CUR_MAX > 1;
#endif
#ifdef Win32
    {
	char *ctype = setlocale(LC_CTYPE, NULL), *p;
	p = strrchr(ctype, '.');
	if(p && isdigit(p[1])) localeCP = atoi(p+1); else localeCP = 1252;
    }
#endif
#if defined(Win32) && defined(SUPPORT_UTF8)
    utf8locale = mbcslocale = TRUE;
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
    R_Toplevel.cloenv = R_BaseEnv;
    R_Toplevel.sysparent = R_BaseEnv;
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
    R_Toplevel.handlerstack = R_HandlerStack;
    R_Toplevel.restartstack = R_RestartStack;
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;

    R_Warnings = R_NilValue;

    /* This is the same as R_BaseEnv, but this marks the environment
       of functions as the namespace and not the package. */
    baseEnv = R_BaseNamespace;

    /* Set up some global variables */
    Init_R_Variables(baseEnv);

    /* On initial entry we open the base language package and begin by
       running the repl on it.
       If there is an error we pass on to the repl.
       Perhaps it makes more sense to quit gracefully?
    */

    fp = R_OpenLibraryFile("base");
    if (fp == NULL)
	R_Suicide(_("unable to open the base package\n"));

    doneit = 0;
    SETJMP(R_Toplevel.cjmpbuf);
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
    if (R_SignalHandlers) init_signal_handlers();
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
    /* These are the same bindings, so only lock them once */
    R_LockEnvironment(R_BaseNamespace, TRUE);
#ifdef NOTYET
    /* methods package needs to trample here */
    R_LockEnvironment(R_BaseEnv, TRUE);
#endif
    /* At least temporarily unlock some bindings uses in graphics */
    R_unLockBinding(install(".Device"), R_BaseEnv);
    R_unLockBinding(install(".Devices"), R_BaseEnv);

    /* require(methods) if it is in the default packages */
    doneit = 0;
    SETJMP(R_Toplevel.cjmpbuf);
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
    if (!doneit) {
	doneit = 1;
	PROTECT(cmd = install(".OptRequireMethods"));
	R_CurrentExpr = findVar(cmd, R_GlobalEnv);
	if (R_CurrentExpr != R_UnboundValue &&
	    TYPEOF(R_CurrentExpr) == CLOSXP) {
	        PROTECT(R_CurrentExpr = lang1(cmd));
	        R_CurrentExpr = eval(R_CurrentExpr, R_GlobalEnv);
	        UNPROTECT(1);
	}
	UNPROTECT(1);
    }

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
#ifndef SUPPORT_UTF8
	if(utf8locale)
	    R_ShowMessage(_("WARNING: UTF-8 locales are not supported in this build of R\n"));
#endif
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
    if (!doneit) {
	doneit = 1;
	R_InitialData();
    }
    else
    	R_Suicide(_("unable to restore saved data in .RData\n"));

    /* Initial Loading is done.
       At this point we try to invoke the .First Function.
       If there is an error we continue. */

    doneit = 0;
    SETJMP(R_Toplevel.cjmpbuf);
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
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
    if (R_CollectWarnings) {
	REprintf(_("During startup - "));
	PrintWarnings();
    }
}

extern SA_TYPE SaveAction; /* from src/main/startup.c */

void end_Rmainloop(void)
{
    /* refrain from printing trailing '\n' only if quiet flag is on and
       the save action is known (e.g. implied by --slave) */
    if (!R_Quiet ||
	(SaveAction != SA_NOSAVE && SaveAction != SA_SAVE))
        Rprintf("\n");
    /* run the .Last function. If it gives an error, will drop back to main
       loop. */
    R_CleanUp(SA_DEFAULT, 0, 1);
}

void run_Rmainloop(void)
{
    /* Here is the real R read-eval-loop. */
    /* We handle the console until end-of-file. */
    R_IoBufferInit(&R_ConsoleIob);
    SETJMP(R_Toplevel.cjmpbuf);
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
    R_ReplConsole(R_GlobalEnv, 0, 0);
    end_Rmainloop(); /* must go here */
}

void mainloop(void)
{
    setup_Rmainloop();
    run_Rmainloop();
}

/*this functionality now appears in 3
  places-jump_to_toplevel/profile/here */

static void printwhere(void)
{
  RCNTXT *cptr;
  int lct = 1;

  for (cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext) {
    if ((cptr->callflag & (CTXT_FUNCTION | CTXT_BUILTIN)) &&
	(TYPEOF(cptr->call) == LANGSXP)) {
	Rprintf("where %d: ", lct++);
	PrintValue(cptr->call);
    }
  }
  Rprintf("\n");
}


static int ParseBrowser(SEXP CExpr, SEXP rho)
{
    int rval = 0;
    if (isSymbol(CExpr)) {
	char *expr = CHAR(PRINTNAME(CExpr));
	if (!strcmp(expr, "n")) {
	    SET_DEBUG(rho, 1);
	    rval = 1;
	}
	if (!strcmp(expr, "c")) {
	    rval = 1;
	    SET_DEBUG(rho, 0);
	}
	if (!strcmp(expr, "cont")) {
	    rval = 1;
	    SET_DEBUG(rho, 0);
	}
	if (!strcmp(expr, "Q")) {

	    /* Run onexit/cend code for everything above the target.
               The browser context is still on the stack, so any error
               will drop us back to the current browser.  Not clear
               this is a good thing.  Also not clear this should still
               be here now that jump_to_toplevel is used for the
               jump. */
	    R_run_onexits(R_ToplevelContext);

	    /* this is really dynamic state that should be managed as such */
	    R_BrowseLevel = 0;
	    SET_DEBUG(rho, 0); /*PR#1721*/

	    jump_to_toplevel();
	}
	if (!strcmp(expr, "where")) {
	    printwhere();
	    /* SET_DEBUG(rho, 1); */
	    rval = 2;
	}
    }
    return rval;
}

/* registering this as a cend exit procedure makes sure R_BrowseLevel
   is maintained across LONGJMP's */
static void browser_cend(void *data)
{
    int *psaved = (int *) data;
    R_BrowseLevel = *psaved - 1;
}

SEXP attribute_hidden do_browser(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    RCNTXT *saveToplevelContext;
    RCNTXT *saveGlobalContext;
    RCNTXT thiscontext, returncontext, *cptr;
    int savestack, savebrowselevel, tmp;
    SEXP topExp;

    /* Save the evaluator state information */
    /* so that it can be restored on exit. */

    savebrowselevel = R_BrowseLevel + 1;
    savestack = R_PPStackTop;
    PROTECT(topExp = R_CurrentExpr);
    saveToplevelContext = R_ToplevelContext;
    saveGlobalContext = R_GlobalContext;

    if (!DEBUG(rho)) {
	cptr = R_GlobalContext;
	while ( !(cptr->callflag & CTXT_FUNCTION) && cptr->callflag )
	    cptr = cptr->nextcontext;
	Rprintf("Called from: ");
	tmp = asInteger(GetOption(install("deparse.max.lines"), R_BaseEnv));
	if(tmp != NA_INTEGER && tmp > 0) R_BrowseLines = tmp;
	PrintValueRec(cptr->call,rho);
	R_BrowseLines = 0;
    }

    R_ReturnedValue = R_NilValue;

    /* Here we establish two contexts.  The first */
    /* of these provides a target for return */
    /* statements which a user might type at the */
    /* browser prompt.  The (optional) second one */
    /* acts as a target for error returns. */

    begincontext(&returncontext, CTXT_BROWSER, call, rho,
		 R_BaseEnv, R_NilValue, R_NilValue);
    returncontext.cend = browser_cend;
    returncontext.cenddata = &savebrowselevel;
    if (!SETJMP(returncontext.cjmpbuf)) {
	begincontext(&thiscontext, CTXT_RESTART, R_NilValue, rho,
		     R_BaseEnv, R_NilValue, R_NilValue);
	if (SETJMP(thiscontext.cjmpbuf)) {
	    SET_RESTART_BIT_ON(thiscontext.callflag);
	    R_ReturnedValue = R_NilValue;
	    R_Visible = FALSE;
	}
	R_GlobalContext = &thiscontext;
	R_InsertRestartHandlers(&thiscontext, TRUE);
	R_BrowseLevel = savebrowselevel;
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

SEXP attribute_hidden do_quit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    char *tmp;
    SA_TYPE ask=SA_DEFAULT;
    int status, runLast;

    if(R_BrowseLevel) {
	warning(_("cannot quit from browser"));
	return R_NilValue;
    }
    if( !isString(CAR(args)) )
	errorcall(call, _("one of \"yes\", \"no\", \"ask\" or \"default\" expected."));
    tmp = CHAR(STRING_ELT(CAR(args), 0));
    if( !strcmp(tmp, "ask") ) {
	ask = SA_SAVEASK;
	if(!R_Interactive)
	    warningcall(call, _("save=\"ask\" in non-interactive use: command-line default will be used"));
    } else if( !strcmp(tmp, "no") )
	ask = SA_NOSAVE;
    else if( !strcmp(tmp, "yes") )
	ask = SA_SAVE;
    else if( !strcmp(tmp, "default") )
	ask = SA_DEFAULT;
    else
	errorcall(call, _("unrecognized value of 'save'"));
    status = asInteger(CADR(args));
    if (status == NA_INTEGER) {
        warningcall(call, _("invalid 'status', 0 assumed"));
	runLast = 0;
    }
    runLast = asLogical(CADDR(args));
    if (runLast == NA_LOGICAL) {
        warningcall(call, _("invalid 'runLast', FALSE assumed"));
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
	error(_("cannot allocate space for toplevel callback element"));

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
	error(_("negative index passed to R_removeTaskCallbackByIndex"));

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
  list of top-level callbacks. 'which' should be an
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
	    REprintf(_("warning messages from top-level task callback '%s'\n"), 
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
	    warning(_("top-level task callback did not return a logical value"));
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
