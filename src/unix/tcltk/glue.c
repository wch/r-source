#include <stdio.h>
#include <string.h>
#include <tcl.h>
#include <tk.h>


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "IOStuff.h"
#include "Parse.h"
#include "Fileio.h"
#include "Graphics.h"		/* for devX11.h */
#include "R_ext/PrtUtil.h"		/* for REprintf */
#include "../devUI.h"
#include "../Runix.h"

static int stdin_activity; /* flag to say that input is present */


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




static void stdin_setflag(int dummy) {stdin_activity = 1;}

void _tcltk_init()
{
    int code;
    
    Tcl_interp = Tcl_CreateInterp(); 
    code = Tcl_Init(Tcl_interp); /* Undocumented... If omitted, you
				    get the windows but no event
				    handling. */
    if (code != TCL_OK)
	REprintf("%s\n", Tcl_interp->result);
  
    code = Tk_Init(Tcl_interp);  /* Load Tk into interpreter */
    if (code != TCL_OK)
	REprintf("%s\n", Tcl_interp->result);
    
    Tcl_StaticPackage(Tcl_interp, "Tk", Tk_Init, Tk_SafeInit);
    Tcl_CreateFileHandler(0, TCL_READABLE, (Tcl_FileProc *)stdin_setflag, 0);

    code = Tcl_Eval(Tcl_interp, "wm withdraw .");  /* Hide window */
    if (code != TCL_OK)
	REprintf("%s\n", Tcl_interp->result);

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
    

/*    code = Tcl_EvalFile(Tcl_interp, "init.tcl"); */
/*    if (code != TCL_OK) */
/*      REprintf("%s\n", Tcl_interp->result); */

}


#ifdef HAVE_LIBREADLINE
#include <readline/readline.h>
#ifdef HAVE_READLINE_HISTORY_H
#include <readline/history.h>
#endif
#endif
#ifdef HAVE_LIBREADLINE
	/* callback for rl_callback_read_char */

extern int UsingReadline;

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

int _tcltk_ReadConsole(char *prompt, unsigned char *buf, int len,
		     int addtohistory)
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

	if(R_InputHandlers == NULL)
	    initStdinHandler();

        for (;;) {
	    stdin_activity = 0;
            Tcl_DoOneEvent(0);
	    if (stdin_activity)
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




/* --------------  Input handler interface  -------------- */

/* These actually came out much more similar to the standard ones than
   anticipated. They really only differ by the Tcl_*FileHandler
   calls. Perhaps this should be done with an if() construct in the
   standard code instead. */

InputHandler *
tcltk_addInputHandler(InputHandler *handlers, int fd, InputHandlerProc handler, 
		int activity)
{
    InputHandler *input, *tmp;
    input = (InputHandler*) calloc(1, sizeof(InputHandler));

    input->activity = activity;
    input->fileDescriptor = fd;
    input->handler = handler;

    Tcl_CreateFileHandler (fd, TCL_READABLE, handler, 0);

    tmp = handlers;

    if(handlers == NULL) {
	R_InputHandlers = input;
	return(input);
    }

    /* Go to the end of the list to append the new one.  */
    while(tmp->next != NULL) {
	tmp = tmp->next;
    }
    tmp->next = input;

    return(handlers);

}

int
tcltk_removeInputHandler(InputHandler **handlers, InputHandler *it)
{
    InputHandler *tmp;


    Tcl_DeleteFileHandler(it->fileDescriptor);
    /* If the handler is the first one in the list, move the list to point
       to the second element. That's why we use the address of the first 
       element as the first argument.
    */
    if(*handlers == it) {
	*handlers = (*handlers)->next;
	return(1);
    }

    tmp = *handlers;

    while(tmp) {
	if(tmp->next == it) {
	    tmp->next = it->next;
	    return(1);
	}
    }

    return(0);

}

InputHandler *
tcltk_getInputHandler(InputHandler *handlers, int fd)
{
    InputHandler *tmp;
    tmp = handlers;

    while(tmp != NULL) {
	if(tmp->fileDescriptor == fd)
	    return(tmp);
	tmp = tmp->next;
    }

    return(tmp);

}
