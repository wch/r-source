/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-1999	    The R Development Core Team.
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

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#define __MAIN__
#include "Defn.h"
#include "Graphics.h"
#include "IOStuff.h"
#include "Parse.h"


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


static int ParseBrowser(SEXP, SEXP);



	/* Read-Eval-Print Loop [ =: REPL = repl ] with input from a file */

static void R_ReplFile(FILE *fp, SEXP rho, int savestack, int browselevel)
{
    int status;

    for(;;) {
	Reset_C_alloc();
	R_PPStackTop = savestack;
	R_CurrentExpr = R_Parse1File(fp, 1, &status);
	switch (status) {
	case PARSE_NULL:
	    break;
	case PARSE_OK:
	    R_Visible = 0;
	    R_EvalDepth = 0;
	    PROTECT(R_CurrentExpr);
	    R_CurrentExpr = eval(R_CurrentExpr, rho);
	    SYMVALUE(R_LastvalueSymbol) = R_CurrentExpr;
	    UNPROTECT(1);
	    if (R_Visible)
		PrintValueEnv(R_CurrentExpr, rho);
	    if( R_CollectWarnings )
		PrintWarnings();
	    break;
	case PARSE_ERROR:
	    error("syntax error");
	    break;
	case PARSE_EOF:
	    return;
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
	    return (char*)CHAR(STRING(GetOption(install("prompt"),
						R_NilValue))[0]);
	}
	else {
	    return (char*)CHAR(STRING(GetOption(install("continue"),
						R_NilValue))[0]);
	}
    }
}

static void R_ReplConsole(SEXP rho, int savestack, int browselevel)
{
    int c, status;
    char *bufp, buf[1024];

    R_IoBufferWriteReset(&R_ConsoleIob);
    prompt_type = 1;
    buf[0] = '\0';
    bufp = buf;
    if(R_Verbose)
	REprintf(" >R_ReplConsole(): before \"for(;;)\" {main.c}\n");
    for(;;) {
	if(!*bufp) {
	    R_Busy(0);
	    if (R_ReadConsole(R_PromptString(browselevel, prompt_type),
			     buf, 1024, 1) == 0) return;
	    bufp = buf;
	}
#ifdef SHELL_ESCAPE
	if (*bufp == '!') {
#ifdef HAVE_SYSTEM
	    system(&buf[1]);
#else
	    Rprintf("error: system commands are not supported in this version of R.\n");
#endif
	    buf[0] = '\0';
	    continue;
	}
#endif
	while((c = *bufp++)) {
	    R_IoBufferPutc(c, &R_ConsoleIob);
	    if(c == ';' || c == '\n') break;
	}
	if (browselevel)
	    Reset_C_alloc();

	R_PPStackTop = savestack;
	R_CurrentExpr = R_Parse1Buffer(&R_ConsoleIob, 0, &status);

	switch(status) {

	case PARSE_NULL:

	    if (browselevel)
		return;
	    R_IoBufferWriteReset(&R_ConsoleIob);
	    prompt_type = 1;
	    break;

	case PARSE_OK:

	    R_IoBufferReadReset(&R_ConsoleIob);
	    R_CurrentExpr = R_Parse1Buffer(&R_ConsoleIob, 1, &status);
	    if (browselevel && ParseBrowser(R_CurrentExpr, rho))
		return;
	    R_Visible = 0;
	    R_EvalDepth = 0;
	    PROTECT(R_CurrentExpr);
	    R_Busy(1);
	    R_CurrentExpr = eval(R_CurrentExpr, rho);
	    SYMVALUE(R_LastvalueSymbol) = R_CurrentExpr;
	    UNPROTECT(1);
	    if (R_Visible)
		PrintValueEnv(R_CurrentExpr, rho);
	    if (R_CollectWarnings)
		PrintWarnings();
	    R_IoBufferWriteReset(&R_ConsoleIob);
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

	    return;
	    break;
	}
    }
}


static char DLLbuf[1024], *DLLbufp;

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
    int c, status;
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
	SYMVALUE(R_LastvalueSymbol) = R_CurrentExpr;
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


/* The following variable must be external to mainloop because gcc -O */
/* seems to eliminate a local one? */

static int doneit;

FILE* R_OpenSysInitFile(void);
#ifndef Macintosh
FILE* R_OpenSiteFile(void);
FILE* R_OpenInitFile(void);
#endif

#ifdef OLD
static void R_LoadProfile(FILE *fp)
#else
static void R_LoadProfile(FILE *fp, SEXP env)
#endif
{
    if (fp != NULL) {
	R_Inputfile = fp;
	doneit = 0;
	SETJMP(R_Toplevel.cjmpbuf);
	R_GlobalContext = R_ToplevelContext = &R_Toplevel;
	signal(SIGINT, onintr);
	if (!doneit) {
	    doneit = 1;
#ifdef OLD
	    R_ReplFile(R_Inputfile, R_NilValue, 0, 0);
#else
	    R_ReplFile(R_Inputfile, env, 0, 0);
#endif
	}
        R_Inputfile = NULL;
    }
}

void setup_Rmainloop(void)
{
    SEXP cmd;
    FILE *fp;

    /* Print a platform and version dependent */
    /* greeting and a pointer to the copyleft. */

    if(!R_Quiet)
	PrintGreeting();

    /* Initialize the interpreter's */
    /* internal structures. */

#ifdef HAVE_LOCALE_H
    setlocale(LC_CTYPE,"");/*- make ISO-latin1 etc. work LOCALE users */
    setlocale(LC_COLLATE,"");/*- alphabetically sorting */
    /* setlocale(LC_MESSAGES,""); */
#endif
    InitMemory();
    InitNames();
    InitGlobalEnv();
    InitFunctionHashing();
    InitOptions();
    InitEd();
    InitArithmetic();
    InitColors();
    InitGraphics();
    Init_C_alloc();

    /* gc_inhibit_torture = 0; */

    /* Initialize the global context for error handling. */
    /* This provides a target for any non-local gotos */
    /* which occur during error handling */

    R_Toplevel.nextcontext = NULL;
    R_Toplevel.callflag = CTXT_TOPLEVEL;
    R_Toplevel.cstacktop = 0;
    R_Toplevel.promargs = R_NilValue;
    R_Toplevel.call = R_NilValue;
    R_Toplevel.cloenv = R_NilValue;
    R_Toplevel.sysparent = R_NilValue;
    R_Toplevel.conexit = R_NilValue;
    R_Toplevel.cend = NULL;
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;

    R_Warnings = R_NilValue;

    /* On initial entry we open the base language */
    /* package and begin by running the repl on it. */
    /* If there is an error we pass on to the repl. */
    /* Perhaps it makes more sense to quit gracefully? */

    fp = R_OpenLibraryFile("base");
    R_Inputfile = fp;
    if (fp == NULL) {
	R_Suicide("unable to open the base package\n");
    }

    doneit = 0;
    SETJMP(R_Toplevel.cjmpbuf);
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
    signal(SIGINT, onintr);
    if (!doneit) {
	doneit = 1;
	R_ReplFile(fp, R_NilValue, 0, 0);
    }
    R_Inputfile = NULL;
    fclose(fp);

    /* This is where we try to load a user's */
    /* saved data.	The right thing to do here */
    /* is very platform dependent.	E.g. Under */
    /* Unix we look in a special hidden file and */
    /* on the Mac we look in any documents which */
    /* might have been double clicked on or dropped */
    /* on the application */

    doneit = 0;
    SETJMP(R_Toplevel.cjmpbuf);
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
    signal(SIGINT, onintr);
    if (!doneit) {
	doneit = 1;
	R_InitialData();
    }
    else
    	R_Suicide("unable to restore saved data\n (remove .RData or increase memory)\n");

    /* This is where we source the system-wide, the site's and the
       user's profile (in that order).  If there is an error, we
       drop through to further processing. */
#ifdef OLD
    R_LoadProfile(R_OpenSysInitFile());
#ifndef Macintosh
    R_LoadProfile(R_OpenSiteFile());
    R_LoadProfile(R_OpenInitFile());
#endif
#else
    R_LoadProfile(R_OpenSysInitFile(), R_NilValue);
#ifndef Macintosh
    R_LoadProfile(R_OpenSiteFile(), R_NilValue); 
    R_LoadProfile(R_OpenInitFile(), R_GlobalEnv);
#endif
#endif
    /* Initial Loading is done.  At this point */
    /* we try to invoke the .First Function. */
    /* If there is an error we continue */

    doneit = 0;
    SETJMP(R_Toplevel.cjmpbuf);
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
    signal(SIGINT, onintr);
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
    /* gc_inhibit_torture = 0; */
}


void run_Rmainloop(void)
{
    /* Here is the real R read-eval-loop. */
    /* We handle the console until end-of-file. */

    R_IoBufferInit(&R_ConsoleIob);
    SETJMP(R_Toplevel.cjmpbuf);
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
    signal(SIGINT, onintr);

    R_ReplConsole(R_GlobalEnv, 0, 0);
}

void end_Rmainloop(void)
{
    Rprintf("\n");
    /* run the .Last function. If it gives an error, will drop back to main
       loop. */
    R_CleanUp(SA_DEFAULT, 0, 1);
}

void mainloop(void)
{
    setup_Rmainloop();
    run_Rmainloop();
    end_Rmainloop();
}


static int ParseBrowser(SEXP CExpr, SEXP rho)
{
    int rval=0;
    if (isSymbol(CExpr)) {
	if (!strcmp(CHAR(PRINTNAME(CExpr)),"n")) {
	    DEBUG(rho)=1;
	    rval=1;
	}
	if (!strcmp(CHAR(PRINTNAME(CExpr)),"c")) {
	    rval=1;
	    DEBUG(rho)=0;
	}
	if (!strcmp(CHAR(PRINTNAME(CExpr)),"cont")) {
	    rval=1;
	    DEBUG(rho)=0;
	}
	if (!strcmp(CHAR(PRINTNAME(CExpr)),"Q")) {
	    R_BrowseLevel = 0;
            LONGJMP(R_Toplevel.cjmpbuf, CTXT_TOPLEVEL);
	}
    }
    return rval;
}

SEXP do_browser(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    RCNTXT *saveToplevelContext;
    RCNTXT *saveGlobalContext;
    RCNTXT thiscontext, returncontext, *cptr;
    int savestack;
    int savebrowselevel;
    int saveEvalDepth;
    SEXP topExp;

    /* Save the evaluator state information */
    /* so that it can be restored on exit. */

    savebrowselevel = R_BrowseLevel + 1;
    savestack = R_PPStackTop;
    PROTECT(topExp = R_CurrentExpr);
    saveToplevelContext = R_ToplevelContext;
    saveGlobalContext = R_GlobalContext;
    saveEvalDepth = R_EvalDepth;

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
		 R_NilValue, R_NilValue);
    if (!SETJMP(returncontext.cjmpbuf)) {
	begincontext(&thiscontext, CTXT_TOPLEVEL, R_NilValue, rho,
		     R_NilValue, R_NilValue);
	SETJMP(thiscontext.cjmpbuf);
	R_GlobalContext = R_ToplevelContext = &thiscontext;
	signal(SIGINT, onintr);
	R_BrowseLevel = savebrowselevel;
        signal(SIGINT, onintr);
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
    R_EvalDepth = saveEvalDepth;
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
    tmp = CHAR(STRING(CAR(args))[0]);
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

#undef __MAIN__
