#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "globalvar.h"
#include <stdio.h>
#include <string.h>
#include <tcl.h>
#include <tk.h>

#include "Defn.h"
#include "IOStuff.h"
#include "Parse.h"

static Tcl_Interp *Tcl_interp;      /* Interpreter for this application. */

/*  void R_out(char *str) */
/*  { */
/*    Tcl_VarEval(Tcl_interp, ".out insert end {", str, "}",NULL); */
/*  } */

static int R_eval(ClientData clientData,
		  Tcl_Interp *interp,
		  int argc,
		  char *argv[])
{
    int status, i;
    SEXP text, expr, ans;

    text = PROTECT(allocVector(STRSXP, argc - 1));
    for (i = 1 ; i < argc ; i++)
	STRING(text)[i-1] = mkChar(argv[i]);

    expr = PROTECT(R_ParseVector(text, -1, &status));
    if (status != PARSE_OK) {
	UNPROTECT(2);
	Tcl_SetResult(interp, "parse error in R expression", TCL_STATIC);
	return TCL_ERROR;
    }

    /* Note that expr becomes an EXPRSXP and hence we need the loop
       below (a straight eval(expr, R_GlobalEnv) won't work) */
    {
	int i, n;
	n = length(expr);
	for(i = 0 ; i < n ; i++)
	    ans = eval(VECTOR(expr)[i], R_GlobalEnv);
    }
    UNPROTECT(2);
    return TCL_OK;
}

static int R_call(ClientData clientData,
		  Tcl_Interp *interp,
		  int argc,
		  char *argv[])
{
    int i;
    SEXP expr, ans, fun, alist;

    alist = R_NilValue;
    for (i = argc - 1 ; i > 1 ; i--){
	PROTECT(alist);
	alist = LCONS(mkString(argv[i]), alist);
	UNPROTECT(1);
    }

    fun = (SEXP) strtoul(argv[1], NULL, 16);

    expr = LCONS(fun, alist);

    ans = eval(expr, R_GlobalEnv);

    return TCL_OK;
}

extern void R_ShowMessage(char *);

static void tcl_err(char *s)
{
    char buff[1000];
    sprintf(buff, "%s\n", s);
    R_ShowMessage(buff);
}


int tcltk_init()
{
    int code;
    
    Tcl_interp = Tcl_CreateInterp(); 
    code = Tcl_Init(Tcl_interp); /* Undocumented... If omitted, you
				    get the windows but no event
				    handling. */
    if (code != TCL_OK) {
	tcl_err(Tcl_interp->result); 
	return 1;
    }
  
    code = Tk_Init(Tcl_interp);  /* Load Tk into interpreter */
    if (code != TCL_OK) {
	tcl_err(Tcl_interp->result); 
	return 1;
    }
    
    Tcl_StaticPackage(Tcl_interp, "Tk", Tk_Init, Tk_SafeInit);

    code = Tcl_Eval(Tcl_interp, "wm withdraw .");  /* Hide window */
    if (code != TCL_OK) {
	tcl_err(Tcl_interp->result); 
	return 1;
    }

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
    
    Tcl_SetServiceMode(TCL_SERVICE_ALL);

    return 0;
}

char* _tk_eval(char *cmd) 
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

void R_tcldo()
{
    Tcl_ServiceAll();
}
