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

void jump_to_toplevel();
static void jump_now();

static int inError=0;

void onintr()
{
	REprintf("\n");
	jump_to_toplevel();
}

void warningcall(SEXP call, char *format,...)
{
	va_list(ap);
	char *dcall;
	dcall = CHAR(STRING(deparse1(call, 0))[0]);
	REprintf("Warning in %s : ", dcall);
	va_start(ap, format);
	REvprintf(format, ap);
	va_end(ap);
}
void warning(const char *format,...)
{
	va_list(ap);
	REprintf("Warning: ");
	va_start(ap, format);
	REvprintf(format, ap);
	va_end(ap);
}

void errorcall(SEXP call, char *format,...)
{
	va_list(ap);
	char *dcall;

	if(inError )
		jump_now();

	dcall = CHAR(STRING(deparse1(call, 0))[0]);
	REprintf("Error in %s : ", dcall);
	va_start(ap, format);
	REvprintf(format, ap);
	va_end(ap);
	jump_to_toplevel();
}

void error(const char *format, ...)
{
	va_list(ap);

	if( inError ) 
		jump_now();
	REprintf("Error: ");
	va_start(ap, format);
	REvprintf(format, ap);
	va_end(ap);
	jump_to_toplevel();
}

/* Unwind the call stack in an orderly fashion */
/* calling the code installed by on.exit along the way */
/* and finally longjmping to the top repl loop */

void jump_to_toplevel()
{
	RCNTXT *c;
	SEXP s, t;
	int nback=0;

	inError = 1;
	if (R_Console == 0) fclose(R_Inputfile);
#ifdef OLD
#else
	R_SetInput(0);
#endif
	ResetConsole();
	FlushConsole();
	R_ParseError = 0;
	ResetComment();
	vmaxset(NULL);
	if (R_GlobalContext->cend != NULL)
		(R_GlobalContext->cend) ();
	for (c = R_GlobalContext; c; c = c->nextcontext) {
		if (c->cloenv != R_NilValue && c->conexit != R_NilValue)
			eval(c->conexit, c->cloenv);
		if(c->callflag == CTXT_RETURN)
			nback++;
	}
	if (R_Sinkfile) R_Outputfile = R_Sinkfile;
	else R_Outputfile = R_Consolefile;

	PROTECT(s=allocList(nback));
	t=s;
	for( c = R_GlobalContext; c; c = c->nextcontext) 
		if(c->callflag == CTXT_RETURN) {
			CAR(t) = deparse1(c->call,0);
			t=CDR(t);
		}
	setVar(install(".Traceback"), s, R_GlobalEnv);
	UNPROTECT(1);

	jump_now();
}

void jump_now()
{
	inError=0;
	R_PPStackTop = 0;
	if(R_Interactive) longjmp(R_ToplevelContext->cjmpbuf, 0);
	else REprintf("Execution halted\n");
	exit(1);
}

#ifdef Macintosh

#include <signal.h>
#include <errno.h>

void isintrpt()
{
	register EvQElPtr q;

	for (q = (EvQElPtr) GetEvQHdr()->qHead; q; q = (EvQElPtr) q->qLink)
		if (q->evtQWhat == keyDown && (char) q->evtQMessage == '.')
			if (q->evtQModifiers & cmdKey) {
				FlushEvents(keyDownMask, 0);
				raise(SIGINT);
				/* errno = EINTR; */
				return;
			}
}
#endif

SEXP do_stop(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	CAR(args) = coerceVector(CAR(args), STRSXP);
	if (length(CAR(args)) <= 0)
		error("\n");
	else
		error("%s\n", CHAR(STRING(CAR(args))[0]));
	/*NOTREACHED*/
}

SEXP do_warning(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	if (CAR(args) != R_NilValue) {
		CAR(args) = coerceVector(CAR(args), STRSXP);
		warning("%s\n", CHAR(STRING(CAR(args))[0]));
	}
	else
		warning("%s\n", "");
	return CAR(args);
}

void  UNIMPLEMENTED(char *s)
{
	error("Unimplemented feature in %s\n", s);
}

static struct {
	int	index;
	char*	format;
}
ErrorDB[] = {
	{ ERROR_NUMARGS,		"invalid number of arguments\n"       },
	{ ERROR_ARGTYPE,		"invalid argument type\n"             },

	{ ERROR_TSVEC_MISMATCH,		"time-series/vector length mismatch\n"},
	{ ERROR_INCOMPAT_ARGS,		"incompatible arguments\n"            },
	{ ERROR_INCOMPAT_FACTORS,	"incompatible factors\n",             },

	{ ERROR_UNIMPLEMENTED,		"unimplemented feature in %s\n",      },
	{ ERROR_UNKNOWN,		"unknown error (report this!)\n",     }
};

void  ErrorMessage(SEXP call, int which_error, ...)
{
	int i;
	va_list(ap);
	char *dcall;
	if(inError ) jump_now();
	if(call != R_NilValue) {
		dcall = CHAR(STRING(deparse1(call, 0))[0]);
		REprintf("Error in %s : ", dcall);
	}
	else REprintf("Error: ", dcall);
	i = 0;
	while(ErrorDB[i].index != ERROR_UNKNOWN) {
		if(ErrorDB[i].index == which_error)
			break;
		i++;
	}
	va_start(ap, which_error);
	REvprintf(ErrorDB[i].format, ap);
	va_end(ap);
	jump_to_toplevel();
}

static struct {
        int     index;
        char*   format;
}
WarningDB[] = {
        { WARNING_UNKNOWN,                "unknown warning (report this!)\n", }
};

void  WarningMessage(SEXP call, int which_warn, ...)
{
        int i;
        va_list(ap);
        char *dcall;
        if(inError ) jump_now();
        if(call != R_NilValue) {
                dcall = CHAR(STRING(deparse1(call, 0))[0]);
                REprintf("Warning in %s : ", dcall);
        }
        else REprintf("Warning: ", dcall);
        i = 0;
        while(WarningDB[i].index != WARNING_UNKNOWN) {
                if(WarningDB[i].index == which_warn)
                        break;
                i++;
        }
        va_start(ap, which_warn);
        REvprintf(WarningDB[i].format, ap);
        va_end(ap);
}
