#include <stdio.h>
#include <string.h>
#include <tcl.h>
#include <tk.h>


/*
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
*/

#include <Rinternals.h>
#include "R_ext/PrtUtil.h"
#ifndef Win32
#include "R_ext/eventloop.h"
#endif

/* From Parse.h -- must find better solution: */
#define PARSE_NULL              0
#define PARSE_OK                1
#define PARSE_INCOMPLETE        2
#define PARSE_ERROR             3
#define PARSE_EOF               4

SEXP R_ParseVector(SEXP, int, int *);

#include "tcltk.h" /* declarations of our `public' interface */


static Tcl_Interp *Tcl_interp;      /* Interpreter for this application. */

static int R_eval(ClientData clientData,
		  Tcl_Interp *interp,
		  int argc,
		  char *argv[])
{
    int status, i;
    SEXP text, expr, ans;

    text = PROTECT(allocVector(STRSXP, argc - 1));
    for (i = 1 ; i < argc ; i++)
	SET_STRING_ELT(text, i-1, mkChar(argv[i]));

    expr = PROTECT(R_ParseVector(text, -1, &status));
    if (status != PARSE_OK) {
	UNPROTECT(2);
	Tcl_SetResult(interp, "parse error in R expression", TCL_STATIC);
	return TCL_ERROR;
    }

    /* Note that expr becomes an EXPRSXP and hence we need the loop
       below (a straight eval(expr, R_GlobalEnv) won't work) */
    {
	int n = length(expr);
	for(i = 0 ; i < n ; i++)
	    ans = eval(VECTOR_ELT(expr, i), R_GlobalEnv);
    }
    UNPROTECT(2);
    return TCL_OK;
}

/* Functions to evaluate callbacks. Notice that these have no error
   checks, since the calls are assumed to be generated internally, and
   not by users. There are two forms:

   R_call 0xnnnnnnnn arg1 arg2 arg3...
   R_call_lang 0xnnnnnnnn 0xmmmmmmmm

   In the former, the hex address is assumed to correspond to a
   function closure which gets called with the arguments given.  The
   latter assumes that the first hex address is a LANGSXP and the
   second is an ENVSXP. */

static int R_call(ClientData clientData,
		  Tcl_Interp *interp,
		  int argc,
		  char *argv[])
{
    int i;
    SEXP expr, fun, alist;

    alist = R_NilValue;
    for (i = argc - 1 ; i > 1 ; i--){
	PROTECT(alist);
	alist = LCONS(mkString(argv[i]), alist);
	UNPROTECT(1);
    }

    fun = (SEXP) strtoul(argv[1], NULL, 16);

    expr = LCONS(fun, alist);
    expr = LCONS(install("try"), LCONS(expr, R_NilValue));

    eval(expr, R_GlobalEnv);

    return TCL_OK;
}

static int R_call_lang(ClientData clientData,
		  Tcl_Interp *interp,
		  int argc,
		  char *argv[])
{
    SEXP expr, env;

    expr = (SEXP) strtoul(argv[1], NULL, 16);
    env  = (SEXP) strtoul(argv[2], NULL, 16);

    expr = LCONS(install("try"), LCONS(expr, R_NilValue));

    eval(expr, env);

    return TCL_OK;
}


char* tk_eval(char *cmd)
{
    if (Tcl_Eval(Tcl_interp, cmd) == TCL_ERROR)
    {
	char p[512];
	if (strlen(Tcl_interp->result)>500)
	    strcpy(p,"tcl error.\n");
	else
	    sprintf(p,"[tcl] %s.\n",Tcl_interp->result);
	error(p);
    }
    return Tcl_interp->result;
}

/* FIXME get rid of fixed size buffers in do_Tclcallback et al. (low
   priority, since there is no meaningful way to override the current
   buffer length) */

SEXP dotTcl(SEXP args)
{
    SEXP ans;
    char *cmd;
    char *val;
    if(!isValidString(CADR(args)))
	error("invalid argument");
    cmd = CHAR(STRING_ELT(CADR(args), 0));
    val = tk_eval(cmd);
    ans = mkString(val);
    return ans;
}

/* Warning: These two functions return a pointer to internal static
   data. Copy immediately. */

static char * callback_closure(SEXP closure)
{
    static char buf[256], tmp[20];
    SEXP formals;

    formals = FORMALS(closure);

    sprintf(buf, "R_call 0x%lx", (unsigned long) closure);

    while ( formals != R_NilValue )
    {
	if (TAG(formals) ==  R_DotsSymbol) break;
	sprintf(tmp, " %%%s", CHAR(PRINTNAME(TAG(formals))));
	strcat(buf, tmp);
	formals = CDR(formals);
    }
    return buf;
}

static char * callback_lang(SEXP call, SEXP env)
{
    static char buf[256];

    sprintf(buf, "R_call_lang 0x%lx 0x%lx", 
	    (unsigned long) call,
	    (unsigned long) env);

    return buf;
}

/* Setup to invoke callback from Tcl. Notice that something needs to
   ensure that the callback is protected from GC as long as anything
   is around to invoke it. This is handled in interpreted code by
   assigning into the environment of the window with which the
   callback is associated */

SEXP dotTclcallback(SEXP args)
{
    SEXP ans, callback = CADR(args), env;

    char *rval = NULL; /* -Wall */

    if (isFunction(callback))
        rval = callback_closure(callback);
    else if (isLanguage(callback)) {
        env = CADDR(args);
        rval = callback_lang(callback, env);
    }
    else
    	error("argument is not of correct type");

    ans = mkString(rval);
    return ans;
}



#ifndef Win32
/* Add/delete Tcl/Tk event handler */

static void (* OldHandler)(void);
static int OldTimeout;
static int Tcl_loaded = 0;

void TclHandler(void)
{
    while (Tcl_DoOneEvent(TCL_DONT_WAIT))
	;
    OldHandler();
}

void addTcl(void)
{
    if (Tcl_loaded)
	error("Tcl already loaded");
    Tcl_loaded = 1;
    OldHandler = R_PolledEvents;
    OldTimeout = R_wait_usec;
    R_PolledEvents = TclHandler;
    if ( R_wait_usec > 10000 || R_wait_usec == 0)
	R_wait_usec = 10000;
}

void delTcl(void)
{
    if (!Tcl_loaded)
	error("Tcl is not loaded");
    if (R_PolledEvents != TclHandler)
	error("Tcl is not last loaded handler");
    R_PolledEvents = OldHandler;
    R_wait_usec = OldTimeout;
    Tcl_loaded = 0;
}
#endif

void tcltk_init(void)
{
    int code;

    Tcl_interp = Tcl_CreateInterp();
    code = Tcl_Init(Tcl_interp); /* Undocumented... If omitted, you
				    get the windows but no event
				    handling. */
    if (code != TCL_OK)
	error(Tcl_interp->result);

    code = Tk_Init(Tcl_interp);  /* Load Tk into interpreter */
    if (code != TCL_OK)
	error(Tcl_interp->result);

    Tcl_StaticPackage(Tcl_interp, "Tk", Tk_Init, Tk_SafeInit);

    code = Tcl_Eval(Tcl_interp, "wm withdraw .");  /* Hide window */
    if (code != TCL_OK)
	error(Tcl_interp->result);

    Tcl_CreateCommand(Tcl_interp,
		      "R_eval",
		      R_eval,
		      (ClientData) NULL,
		      (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(Tcl_interp,
		      "R_call",
		      R_call,
		      (ClientData) NULL,
		      (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(Tcl_interp,
		      "R_call_lang",
		      R_call_lang,
		      (ClientData) NULL,
		      (Tcl_CmdDeleteProc *) NULL);

#ifdef Win32
    Tcl_SetServiceMode(TCL_SERVICE_ALL);
#else
    addTcl();
#endif

/*** We may want to revive this at some point ***/

#if 0
  code = Tcl_EvalFile(Tcl_interp, "init.tcl");
  if (code != TCL_OK)
    error("%s\n", Tcl_interp->result);
#endif

}
