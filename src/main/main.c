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
#include "Graphics.h"
#include "IOStuff.h"
#include "Parse.h"


/* The `real' main() program is in ../<SYSTEM>/system.c */
/* e.g. ../unix/system.c */

/* Global Variables:  For convenience all interpeter */
/* global symbols are declared here.  This does not */
/* include user interface symbols which are included */
/* in separate platform dependent modules. */


/* The R Home Directory */

char*	R_Home;			    /* The Root of the R Tree */

/* Memory Management */

int	R_NSize = R_NSIZE;	    /* Size of cons cell heap */
int	R_VSize = R_VSIZE;	    /* Size of the vector heap */
SEXP	R_NHeap;		    /* Start of the cons cell heap */
SEXP	R_FreeSEXP;		    /* Cons cell free list */
VECREC*	R_VHeap;		    /* Base of the vector heap */
VECREC*	R_VTop;			    /* Current top of the vector heap */
VECREC*	R_VMax;			    /* bottom of R_alloc'ed heap */
long	R_Collected;		    /* Number of free cons cells (after gc) */


/* The Pointer Protection Stack */

int	R_PPStackSize = R_PPSSIZE;  /* The stack size (elements) */
int	R_PPStackTop;		    /* The top of the stack */
SEXP*	R_PPStack;		    /* The pointer protection stack */


/* Evaluation Environment */

SEXP	R_Call;			    /* The current call */
SEXP	R_GlobalEnv;		    /* The "global" environment */
SEXP	R_CurrentExpr;		    /* Currently evaluating expression */
SEXP	R_ReturnedValue;	    /* Slot for return-ing values */
SEXP*	R_SymbolTable;		    /* The symbol table */
RCNTXT	R_Toplevel;		    /* Storage for the toplevel environment */
RCNTXT*	R_ToplevelContext;	    /* The toplevel environment */
RCNTXT*	R_GlobalContext;	    /* The global environment */
int	R_Visible;		    /* Value visibility flag */
int	R_EvalDepth = 0;	    /* Evaluation recursion depth */
int	R_EvalCount = 0;	    /* Evaluation count */


/* File Input/Output */

int	R_Interactive = 1;	    /* Interactive? */
int	R_Quiet = 0;		    /* Be quiet */
int	R_Slave = 0;		    /* Run as a slave process */
int	R_Verbose = 0;		    /* Be verbose */
int	R_Console;		    /* Console active flag */
IoBuffer R_ConsoleIob;		    /* Console IO Buffer */
FILE*	R_Inputfile = NULL;	    /* Current input flag */
FILE*	R_Consolefile = NULL;	    /* Console output file */
FILE*	R_Outputfile = NULL;	    /* Output file */
FILE*	R_Sinkfile = NULL;	    /* Sink file */


/* Objects Used In Parsing  */

SEXP	R_CommentSxp;		    /* Comments accumulate here */
SEXP	R_ParseText;		    /* Text to be parsed */
int	R_ParseCnt;		    /* Count of lines of text to be parsed */
int	R_ParseError = 0;	    /* Line where parse error occured */


/* Special Values */

SEXP	R_NilValue;		    /* The nil object */
SEXP	R_UnboundValue;		    /* Unbound marker */
SEXP	R_MissingArg;		    /* Missing argument marker */


/* Symbol Table Shortcuts */

SEXP	R_Bracket2Symbol;	    /* "[[" */
SEXP	R_BracketSymbol;	    /* "[" */
SEXP	R_ClassSymbol;		    /* "class" */
SEXP	R_DimNamesSymbol;	    /* "dimnames" */
SEXP	R_DimSymbol;		    /* "dim" */
SEXP	R_DollarSymbol;		    /* "$" */
SEXP	R_DotsSymbol;		    /* "..." */
SEXP	R_DropSymbol;		    /* "drop" */
SEXP	R_LevelsSymbol;		    /* "levels" */
SEXP	R_ModeSymbol;		    /* "mode" */
SEXP	R_NamesSymbol;		    /* "names" */
SEXP	R_NaRmSymbol;		    /* "na.rm" */
SEXP	R_RowNamesSymbol;	    /* "row.names" */
SEXP	R_SeedsSymbol;		    /* ".Random.seed" */
SEXP	R_LastvalueSymbol;	    /* ".Last.value" */
SEXP	R_TspSymbol;		    /* "tsp" */
SEXP	R_CommentSymbol;	    /* "comment" */


/* Arithmetic Values */

double	R_tmp;			    /* Temporary Value */
double	R_NaN;			    /* NaN or -DBL_MAX */
double	R_PosInf;		    /* IEEE Inf or DBL_MAX */
double	R_NegInf;		    /* IEEE -Inf or -DBL_MAX */
int	R_NaInt;		    /* NA_INTEGER */
double	R_NaReal;		    /* NA_REAL */
SEXP	R_NaString;		    /* NA_STRING */
SEXP	R_BlankString;		    /* "" as a CHARSXP */


/* Image Dump/Restore */

char	R_ImageName[256];	    /* Default image name */
int	R_Unnamed = 1;		    /* Use default name? */
int	R_DirtyImage = 0;	    /* Current image dirty */
int	R_Init = 0;		    /* Do we have an image loaded */


static int ParseBrowser(SEXP, SEXP);


void InitGlobalEnv()
{
    R_GlobalEnv = emptyEnv();
}



	/* Read-Eval-Print loop with input from a file */

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
	    R_Busy(1);
	    R_CurrentExpr = eval(R_CurrentExpr, rho);
	    SYMVALUE(R_LastvalueSymbol) = R_CurrentExpr;
	    UNPROTECT(1);
	    if (R_Visible)
		PrintValueEnv(R_CurrentExpr, rho);
	    R_Busy(0);
	    break;
	case PARSE_ERROR:
	    error("syntax error\n");
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
    int c, status, prompt;
    char *bufp, buf[1024];

    R_IoBufferWriteReset(&R_ConsoleIob);
    prompt_type = 1;
    buf[0] = '\0';
    bufp = buf;
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
	    if(c == ';' || c == '\n') {
		prompt = (c == '\n');
		break;
	    }
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
	    R_Busy(0);
	    R_IoBufferWriteReset(&R_ConsoleIob);
	    prompt_type = 1;
	    break;

	case PARSE_ERROR:

	    error("syntax error\n");
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

static void R_LoadProfile(FILE *fp)
{
    if (fp != NULL) {
	R_Inputfile = fp;
	doneit = 0;
	SETJMP(R_Toplevel.cjmpbuf);
	R_GlobalContext = R_ToplevelContext = &R_Toplevel;
	signal(SIGINT, onintr);
	if (!doneit) {
	    doneit = 1;
	    R_ReplFile(R_Inputfile, R_NilValue, 0, 0);
	}
        R_Inputfile = NULL;
    }
}

void mainloop(void)
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
    R_LoadProfile(R_OpenSysInitFile());
#ifndef Macintosh
    R_LoadProfile(R_OpenSiteFile());
    R_LoadProfile(R_OpenInitFile());
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

    /* Here is the real R read-eval-loop. */
    /* We handle the console until end-of-file. */

    R_IoBufferInit(&R_ConsoleIob);
    SETJMP(R_Toplevel.cjmpbuf);
    R_GlobalContext = R_ToplevelContext = &R_Toplevel;
    signal(SIGINT, onintr);
    R_ReplConsole(R_GlobalEnv, 0, 0);
    Rprintf("\n");


    /* We have now exited from the read-eval loop. */
    /* Now we run the .Last function and exit. */
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
    R_CleanUp(1);	/* query save */
}

int R_BrowseLevel = 0;

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
	while (cptr->callflag != CTXT_RETURN && cptr->callflag )
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
