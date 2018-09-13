/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2018	The R Core Team.
 *  Copyright (C) 1995, 1996	Robert Gentleman and Ross Ihaka
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


#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include <Rinterface.h>
#include <Fileio.h>
#include <R_ext/Print.h>


static SEXP bcEval(SEXP, SEXP, Rboolean);

/* BC_PROFILING needs to be enabled at build time. It is not enabled
   by default as enabling it disables the more efficient threaded code
   implementation of the byte code interpreter. */
#ifdef BC_PROFILING
static Rboolean bc_profiling = FALSE;
#endif

static int R_Profiling = 0;

#ifdef R_PROFILING

/* BDR 2000-07-15
   Profiling is now controlled by the R function Rprof(), and should
   have negligible cost when not enabled.
*/

/* A simple mechanism for profiling R code.  When R_PROFILING is
   enabled, eval will write out the call stack every PROFSAMPLE
   microseconds using the SIGPROF handler triggered by timer signals
   from the ITIMER_PROF timer.  Since this is the same timer used by C
   profiling, the two cannot be used together.  Output is written to
   the file PROFOUTNAME.  This is a plain text file.  The first line
   of the file contains the value of PROFSAMPLE.  The remaining lines
   each give the call stack found at a sampling point with the inner
   most function first.

   To enable profiling, recompile eval.c with R_PROFILING defined.  It
   would be possible to selectively turn profiling on and off from R
   and to specify the file name from R as well, but for now I won't
   bother.

   The stack is traced by walking back along the context stack, just
   like the traceback creation in jump_to_toplevel.  One drawback of
   this approach is that it does not show BUILTIN's since they don't
   get a context.  With recent changes to pos.to.env it seems possible
   to insert a context around BUILTIN calls to that they show up in
   the trace.  Since there is a cost in establishing these contexts,
   they are only inserted when profiling is enabled. [BDR: we have since
   also added contexts for the BUILTIN calls to foreign code.]

   One possible advantage of not tracing BUILTIN's is that then
   profiling adds no cost when the timer is turned off.  This would be
   useful if we want to allow profiling to be turned on and off from
   within R.

   One thing that makes interpreting profiling output tricky is lazy
   evaluation.  When an expression f(g(x)) is profiled, lazy
   evaluation will cause g to be called inside the call to f, so it
   will appear as if g is called by f.

   L. T.  */

#ifdef Win32
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h>		/* for CreateEvent, SetEvent */
# include <process.h>		/* for _beginthread, _endthread */
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif
# include <signal.h>
#endif /* not Win32 */

static FILE *R_ProfileOutfile = NULL;
static int R_Mem_Profiling=0;
static int R_GC_Profiling = 0;                     /* indicates GC profiling */
static int R_Line_Profiling = 0;                   /* indicates line profiling, and also counts the filenames seen (+1) */
static char **R_Srcfiles;			   /* an array of pointers into the filename buffer */
static size_t R_Srcfile_bufcount;                  /* how big is the array above? */
static SEXP R_Srcfiles_buffer = NULL;              /* a big RAWSXP to use as a buffer for filenames and pointers to them */
static int R_Profiling_Error;		   /* record errors here */

#ifdef Win32
HANDLE MainThread;
HANDLE ProfileEvent;
#endif /* Win32 */

/* Careful here!  These functions are called asynchronously, maybe in the middle of GC,
   so don't do any allocations */

/* This does a linear search through the previously recorded filenames.  If
   this one is new, we try to add it.  FIXME:  if there are eventually
   too many files for an efficient linear search, do hashing. */

static int getFilenum(const char* filename) {
    int fnum;

    for (fnum = 0; fnum < R_Line_Profiling-1
		   && strcmp(filename, R_Srcfiles[fnum]); fnum++);

    if (fnum == R_Line_Profiling-1) {
	size_t len = strlen(filename);
	if (fnum >= R_Srcfile_bufcount) { /* too many files */
	    R_Profiling_Error = 1;
	    return 0;
	}
	if (R_Srcfiles[fnum] - (char*)RAW(R_Srcfiles_buffer) + len + 1 > length(R_Srcfiles_buffer)) {
	      /* out of space in the buffer */
	    R_Profiling_Error = 2;
	    return 0;
	}
	strcpy(R_Srcfiles[fnum], filename);
	R_Srcfiles[fnum+1] = R_Srcfiles[fnum] + len + 1;
	*(R_Srcfiles[fnum+1]) = '\0';
	R_Line_Profiling++;
    }

    return fnum + 1;
}

/* These, together with sprintf/strcat, are not safe -- we should be
   using snprintf and such and computing needed sizes, but these
   settings are better than what we had. LT */

#define PROFBUFSIZ 10500
#define PROFITEMMAX  500
#define PROFLINEMAX (PROFBUFSIZ - PROFITEMMAX)

/* It would also be better to flush the buffer when it gets full,
   even if the line isn't complete. But this isn't possible if we rely
   on writing all line profiling files first.  With these sizes
   hitting the limit is fairly unlikely, but if we do then the output
   file is wrong. Maybe writing an overflow marker of some sort would
   be better.  LT */

static void lineprof(char* buf, SEXP srcref)
{
    size_t len;
    if (srcref && !isNull(srcref) && (len = strlen(buf)) < PROFLINEMAX) {
	int fnum, line = asInteger(srcref);
	SEXP srcfile = getAttrib(srcref, R_SrcfileSymbol);
	const char *filename;

	if (!srcfile || TYPEOF(srcfile) != ENVSXP) return;
	srcfile = findVar(install("filename"), srcfile);
	if (TYPEOF(srcfile) != STRSXP || !length(srcfile)) return;
	filename = CHAR(STRING_ELT(srcfile, 0));

	if ((fnum = getFilenum(filename)))
	    snprintf(buf+len, PROFBUFSIZ - len, "%d#%d ", fnum, line);
    }
}

#if !defined(Win32) && defined(HAVE_PTHREAD)
// <signal.h> is needed for pthread_kill on most platforms (and by POSIX
//  but apparently not FreeBSD): it is included above.
# include <pthread.h>
static pthread_t R_profiled_thread;
#endif

static void doprof(int sig)  /* sig is ignored in Windows */
{
    RCNTXT *cptr;
    char buf[PROFBUFSIZ];
    size_t bigv, smallv, nodes;
    size_t len;
    int prevnum = R_Line_Profiling;

    buf[0] = '\0';

#ifdef Win32
    SuspendThread(MainThread);
#elif defined(HAVE_PTHREAD)
    if (! pthread_equal(pthread_self(), R_profiled_thread)) {
	pthread_kill(R_profiled_thread, sig);
	return;
    }
#endif /* Win32 */

    if (R_Mem_Profiling){
	    get_current_mem(&smallv, &bigv, &nodes);
	    if((len = strlen(buf)) < PROFLINEMAX)
		snprintf(buf+len, PROFBUFSIZ - len,
			 ":%lu:%lu:%lu:%lu:",
			 (unsigned long) smallv, (unsigned long) bigv,
			 (unsigned long) nodes, get_duplicate_counter());
	    reset_duplicate_counter();
    }

    if (R_GC_Profiling && R_gc_running())
	strcat(buf, "\"<GC>\" ");

    if (R_Line_Profiling)
	lineprof(buf, R_getCurrentSrcref());

    for (cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext) {
	if ((cptr->callflag & (CTXT_FUNCTION | CTXT_BUILTIN))
	    && TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
	    if(strlen(buf) < PROFLINEMAX) {
		strcat(buf, "\"");

		char itembuf[PROFITEMMAX];

		if (TYPEOF(fun) == SYMSXP) {
		    snprintf(itembuf, PROFITEMMAX-1, "%s", CHAR(PRINTNAME(fun)));

		} else if ((CAR(fun) == R_DoubleColonSymbol ||
			    CAR(fun) == R_TripleColonSymbol ||
			    CAR(fun) == R_DollarSymbol) &&
			   TYPEOF(CADR(fun)) == SYMSXP &&
			   TYPEOF(CADDR(fun)) == SYMSXP) {
		    /* Function accessed via ::, :::, or $. Both args must be
		       symbols. It is possible to use strings with these
		       functions, as in "base"::"list", but that's a very rare
		       case so we won't bother handling it. */
		    snprintf(itembuf, PROFITEMMAX-1, "%s%s%s",
			     CHAR(PRINTNAME(CADR(fun))),
			     CHAR(PRINTNAME(CAR(fun))),
			     CHAR(PRINTNAME(CADDR(fun))));

		} else if (CAR(fun) == R_Bracket2Symbol &&
			   TYPEOF(CADR(fun)) == SYMSXP &&
			   ((TYPEOF(CADDR(fun)) == SYMSXP ||
			     TYPEOF(CADDR(fun)) == STRSXP ||
			     TYPEOF(CADDR(fun)) == INTSXP ||
			     TYPEOF(CADDR(fun)) == REALSXP) &&
			    length(CADDR(fun)) > 0)) {
		    /* Function accessed via [[. The first arg must be a symbol
		       and the second can be a symbol, string, integer, or
		       real. */
		    SEXP arg1 = CADR(fun);
		    SEXP arg2 = CADDR(fun);
		    char arg2buf[PROFITEMMAX-5];

		    if (TYPEOF(arg2) == SYMSXP) {
			snprintf(arg2buf, PROFITEMMAX-6, "%s", CHAR(PRINTNAME(arg2)));

		    } else if (TYPEOF(arg2) == STRSXP) {
			snprintf(arg2buf, PROFITEMMAX-6, "\"%s\"", CHAR(STRING_ELT(arg2, 0)));

		    } else if (TYPEOF(arg2) == INTSXP) {
			snprintf(arg2buf, PROFITEMMAX-6, "%d", INTEGER(arg2)[0]);

		    } else if (TYPEOF(arg2) == REALSXP) {
			snprintf(arg2buf, PROFITEMMAX-6, "%.0f", REAL(arg2)[0]);

		    } else {
			/* Shouldn't get here, but just in case. */
			arg2buf[0] = '\0';
		    }

		    snprintf(itembuf, PROFITEMMAX-1, "%s[[%s]]",
			     CHAR(PRINTNAME(arg1)),
			     arg2buf);

		} else {
		    sprintf(itembuf, "<Anonymous>");
		}

		strcat(buf, itembuf);
		strcat(buf, "\" ");
		if (R_Line_Profiling) {
		    if (cptr->srcref == R_InBCInterpreter)
			lineprof(buf,
				 R_findBCInterpreterSrcref(cptr));
		    else
			lineprof(buf, cptr->srcref);
		}
	    }
	}
    }

    /* I believe it would be slightly safer to place this _after_ the
       next two bits, along with the signal() call. LT */
#ifdef Win32
    ResumeThread(MainThread);
#endif /* Win32 */

    for (int i = prevnum; i < R_Line_Profiling; i++)
	fprintf(R_ProfileOutfile, "#File %d: %s\n", i, R_Srcfiles[i-1]);

    if(strlen(buf))
	fprintf(R_ProfileOutfile, "%s\n", buf);

#ifndef Win32
    signal(SIGPROF, doprof);
#endif /* not Win32 */

}

#ifdef Win32
/* Profiling thread main function */
static void __cdecl ProfileThread(void *pwait)
{
    int wait = *((int *)pwait);

    SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST);
    while(WaitForSingleObject(ProfileEvent, wait) != WAIT_OBJECT_0) {
	doprof(0);
    }
}
#else /* not Win32 */
static void doprof_null(int sig)
{
    signal(SIGPROF, doprof_null);
}
#endif /* not Win32 */


static void R_EndProfiling(void)
{
#ifdef Win32
    SetEvent(ProfileEvent);
    CloseHandle(MainThread);
#else /* not Win32 */
    struct itimerval itv;

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = 0;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = 0;
    setitimer(ITIMER_PROF, &itv, NULL);
    signal(SIGPROF, doprof_null);

#endif /* not Win32 */
    if(R_ProfileOutfile) fclose(R_ProfileOutfile);
    R_ProfileOutfile = NULL;
    R_Profiling = 0;
    if (R_Srcfiles_buffer) {
	R_ReleaseObject(R_Srcfiles_buffer);
	R_Srcfiles_buffer = NULL;
    }
    if (R_Profiling_Error)
	warning(_("source files skipped by Rprof; please increase '%s'"),
		R_Profiling_Error == 1 ? "numfiles" : "bufsize");
}

static void R_InitProfiling(SEXP filename, int append, double dinterval,
			    int mem_profiling, int gc_profiling,
			    int line_profiling, int numfiles, int bufsize)
{
#ifndef Win32
    struct itimerval itv;
#else
    int wait;
    HANDLE Proc = GetCurrentProcess();
#endif
    int interval;

    interval = (int)(1e6 * dinterval + 0.5);
    if(R_ProfileOutfile != NULL) R_EndProfiling();
    R_ProfileOutfile = RC_fopen(filename, append ? "a" : "w", TRUE);
    if (R_ProfileOutfile == NULL)
	error(_("Rprof: cannot open profile file '%s'"),
	      translateChar(filename));
    if(mem_profiling)
	fprintf(R_ProfileOutfile, "memory profiling: ");
    if(gc_profiling)
	fprintf(R_ProfileOutfile, "GC profiling: ");
    if(line_profiling)
	fprintf(R_ProfileOutfile, "line profiling: ");
    fprintf(R_ProfileOutfile, "sample.interval=%d\n", interval);

    R_Mem_Profiling=mem_profiling;
    if (mem_profiling)
	reset_duplicate_counter();

    R_Profiling_Error = 0;
    R_Line_Profiling = line_profiling;
    R_GC_Profiling = gc_profiling;
    if (line_profiling) {
	/* Allocate a big RAW vector to use as a buffer.  The first len1 bytes are an array of pointers
	   to strings; the actual strings are stored in the second len2 bytes. */
	R_Srcfile_bufcount = numfiles;
	size_t len1 = R_Srcfile_bufcount*sizeof(char *), len2 = bufsize;
	R_PreserveObject( R_Srcfiles_buffer = Rf_allocVector(RAWSXP, len1 + len2) );
 //	memset(RAW(R_Srcfiles_buffer), 0, len1+len2);
	R_Srcfiles = (char **) RAW(R_Srcfiles_buffer);
	R_Srcfiles[0] = (char *)RAW(R_Srcfiles_buffer) + len1;
	*(R_Srcfiles[0]) = '\0';
    }

#ifdef Win32
    /* need to duplicate to make a real handle */
    DuplicateHandle(Proc, GetCurrentThread(), Proc, &MainThread,
		    0, FALSE, DUPLICATE_SAME_ACCESS);
    wait = interval/1000;
    if(!(ProfileEvent = CreateEvent(NULL, FALSE, FALSE, NULL)) ||
       (_beginthread(ProfileThread, 0, &wait) == -1))
	R_Suicide("unable to create profiling thread");
    Sleep(wait/2); /* suspend this thread to ensure that the other one starts */
#else /* not Win32 */
#ifdef HAVE_PTHREAD
    R_profiled_thread = pthread_self();
#else
    error("profiling requires 'pthread' support");
#endif

    signal(SIGPROF, doprof);

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = interval;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = interval;
    if (setitimer(ITIMER_PROF, &itv, NULL) == -1)
	R_Suicide("setting profile timer failed");
#endif /* not Win32 */
    R_Profiling = 1;
}

SEXP do_Rprof(SEXP args)
{
    SEXP filename;
    int append_mode, mem_profiling, gc_profiling, line_profiling;
    double dinterval;
    int numfiles, bufsize;

#ifdef BC_PROFILING
    if (bc_profiling) {
	warning("cannot use R profiling while byte code profiling");
	return R_NilValue;
    }
#endif
    if (!isString(filename = CAR(args)) || (LENGTH(filename)) != 1)
	error(_("invalid '%s' argument"), "filename");
					      args = CDR(args);
    append_mode = asLogical(CAR(args));       args = CDR(args);
    dinterval = asReal(CAR(args));            args = CDR(args);
    mem_profiling = asLogical(CAR(args));     args = CDR(args);
    gc_profiling = asLogical(CAR(args));      args = CDR(args);
    line_profiling = asLogical(CAR(args));    args = CDR(args);
    numfiles = asInteger(CAR(args));	      args = CDR(args);
    if (numfiles < 0)
	error(_("invalid '%s' argument"), "numfiles");
    bufsize = asInteger(CAR(args));
    if (bufsize < 0)
	error(_("invalid '%s' argument"), "bufsize");

    filename = STRING_ELT(filename, 0);
    if (LENGTH(filename))
	R_InitProfiling(filename, append_mode, dinterval, mem_profiling,
			gc_profiling, line_profiling, numfiles, bufsize);
    else
	R_EndProfiling();
    return R_NilValue;
}
#else /* not R_PROFILING */
SEXP do_Rprof(SEXP args)
{
    error(_("R profiling is not available on this system"));
    return R_NilValue;		/* -Wall */
}
#endif /* not R_PROFILING */

/* NEEDED: A fixup is needed in browser, because it can trap errors,
 *	and currently does not reset the limit to the right value. */

void attribute_hidden check_stack_balance(SEXP op, int save)
{
    if(save == R_PPStackTop) return;
    REprintf("Warning: stack imbalance in '%s', %d then %d\n",
	     PRIMNAME(op), save, R_PPStackTop);
}


static SEXP forcePromise(SEXP e)
{
    if (PRVALUE(e) == R_UnboundValue) {
	RPRSTACK prstack;
	SEXP val;
	if(PRSEEN(e)) {
	    if (PRSEEN(e) == 1)
		errorcall(R_GlobalContext->call,
			  _("promise already under evaluation: recursive default argument reference or earlier problems?"));
	    else {
		/* set PRSEEN to 1 to avoid infinite recursion */
		SET_PRSEEN(e, 1);
		warningcall(R_GlobalContext->call,
			     _("restarting interrupted promise evaluation"));
	    }
	}
	/* Mark the promise as under evaluation and push it on a stack
	   that can be used to unmark pending promises if a jump out
	   of the evaluation occurs. */
	SET_PRSEEN(e, 1);
	prstack.promise = e;
	prstack.next = R_PendingPromises;
	R_PendingPromises = &prstack;

	val = eval(PRCODE(e), PRENV(e));

	/* Pop the stack, unmark the promise and set its value field.
	   Also set the environment to R_NilValue to allow GC to
	   reclaim the promise environment; this is also useful for
	   fancy games with delayedAssign() */
	R_PendingPromises = prstack.next;
	SET_PRSEEN(e, 0);
	SET_PRVALUE(e, val);
	ENSURE_NAMEDMAX(val);
	SET_PRENV(e, R_NilValue);
    }
    return PRVALUE(e);
}

/* Return value of "e" evaluated in "rho". */

/* some places, e.g. deparse2buff, call this with a promise and rho = NULL */
SEXP eval(SEXP e, SEXP rho)
{
    SEXP op, tmp;
    static int evalcount = 0;

    R_Visible = TRUE;

    /* this is needed even for self-evaluating objects or something like
       'while (TRUE) NULL' will not be interruptable */
    if (++evalcount > 1000) { /* was 100 before 2.8.0 */
	R_CheckUserInterrupt();
#ifndef IMMEDIATE_FINALIZERS
	/* finalizers are run here since this should only be called at
	   points where running arbitrary code should be safe */
	R_RunPendingFinalizers();
#endif
	evalcount = 0 ;
    }

    /* handle self-evaluating objects with minimal overhead */
    switch (TYPEOF(e)) {
    case NILSXP:
    case LISTSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case STRSXP:
    case CPLXSXP:
    case RAWSXP:
    case S4SXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case ENVSXP:
    case CLOSXP:
    case VECSXP:
    case EXTPTRSXP:
    case WEAKREFSXP:
    case EXPRSXP:
	/* Make sure constants in expressions are NAMED before being
	   used as values.  Setting NAMED to NAMEDMAX makes sure weird calls
	   to replacement functions won't modify constants in
	   expressions.  */
	ENSURE_NAMEDMAX(e);
	return e;
    default: break;
    }

    int bcintactivesave = R_BCIntActive;
    R_BCIntActive = 0;

    if (!rho)
	error("'rho' cannot be C NULL: detected in C-level eval");
    if (!isEnvironment(rho))
	error("'rho' must be an environment not %s: detected in C-level eval",
	      type2char(TYPEOF(rho)));

    /* Save the current srcref context. */

    SEXP srcrefsave = R_Srcref;

    /* The use of depthsave below is necessary because of the
       possibility of non-local returns from evaluation.  Without this
       an "expression too complex error" is quite likely. */

    int depthsave = R_EvalDepth++;

    /* We need to explicit set a NULL call here to circumvent attempts
       to deparse the call in the error-handler */
    if (R_EvalDepth > R_Expressions) {
	R_Expressions = R_Expressions_keep + 500;
	errorcall(R_NilValue,
		  _("evaluation nested too deeply: infinite recursion / options(expressions=)?"));
    }
    R_CheckStack();

    tmp = R_NilValue;		/* -Wall */
#ifdef Win32
    /* This is an inlined version of Rwin_fpreset (src/gnuwin/extra.c)
       and resets the precision, rounding and exception modes of a ix86
       fpu.
     */
    __asm__ ( "fninit" );
#endif

    switch (TYPEOF(e)) {
    case BCODESXP:
	tmp = bcEval(e, rho, TRUE);
	    break;
    case SYMSXP:
	if (e == R_DotsSymbol)
	    error(_("'...' used in an incorrect context"));
	if( DDVAL(e) )
	    tmp = ddfindVar(e,rho);
	else
	    tmp = findVar(e, rho);
	if (tmp == R_UnboundValue)
	    error(_("object '%s' not found"), EncodeChar(PRINTNAME(e)));
	/* if ..d is missing then ddfindVar will signal */
	else if (tmp == R_MissingArg && !DDVAL(e) ) {
	    const char *n = CHAR(PRINTNAME(e));
	    if(*n) error(_("argument \"%s\" is missing, with no default"),
			 CHAR(PRINTNAME(e)));
	    else error(_("argument is missing, with no default"));
	}
	else if (TYPEOF(tmp) == PROMSXP) {
	    if (PRVALUE(tmp) == R_UnboundValue) {
		/* not sure the PROTECT is needed here but keep it to
		   be on the safe side. */
		PROTECT(tmp);
		tmp = forcePromise(tmp);
		UNPROTECT(1);
	    }
	    else tmp = PRVALUE(tmp);
	    ENSURE_NAMEDMAX(tmp);
	}
	else ENSURE_NAMED(tmp); /* should not really be needed - LT */
	break;
    case PROMSXP:
	if (PRVALUE(e) == R_UnboundValue)
	    /* We could just unconditionally use the return value from
	       forcePromise; the test avoids the function call if the
	       promise is already evaluated. */
	    forcePromise(e);
	tmp = PRVALUE(e);
	/* This does _not_ change the value of NAMED on the value tmp,
	   in contrast to the handling of promises bound to symbols in
	   the SYMSXP case above.  The reason is that one (typically
	   the only) place promises appear in source code is as
	   wrappers for the RHS value in replacement function calls for
	   complex assignment expression created in applydefine().  If
	   the RHS value is freshly created it will have NAMED = 0 and
	   we want it to stay that way or a BUILTIN or SPECIAL
	   replacement function might have to duplicate the value
	   before inserting it to avoid creating cycles.  (Closure
	   replacement functions will get the value via the SYMSXP case
	   from evaluating their 'value' argument so the value will
	   end up getting duplicated if NAMED > 1.) LT */
	break;
    case LANGSXP:
	if (TYPEOF(CAR(e)) == SYMSXP) {
	    /* This will throw an error if the function is not found */
	    SEXP ecall = e;

	    /* This picks the correct/better error expression for
	       replacement calls running in the AST interpreter. */
	    if (R_GlobalContext != NULL &&
		    (R_GlobalContext->callflag == CTXT_CCODE))
		ecall = R_GlobalContext->call;
	    PROTECT(op = findFun3(CAR(e), rho, ecall));
	} else
	    PROTECT(op = eval(CAR(e), rho));

	if(RTRACE(op) && R_current_trace_state()) {
	    Rprintf("trace: ");
	    PrintValue(e);
	}
	if (TYPEOF(op) == SPECIALSXP) {
	    int save = R_PPStackTop, flag = PRIMPRINT(op);
	    const void *vmax = vmaxget();
	    PROTECT(e);
	    R_Visible = flag != 1;
	    tmp = PRIMFUN(op) (e, op, CDR(e), rho);
#ifdef CHECK_VISIBILITY
	    if(flag < 2 && R_Visible == flag) {
		char *nm = PRIMNAME(op);
		if(strcmp(nm, "for")
		   && strcmp(nm, "repeat") && strcmp(nm, "while")
		   && strcmp(nm, "[[<-") && strcmp(nm, "on.exit"))
		    printf("vis: special %s\n", nm);
	    }
#endif
	    if (flag < 2) R_Visible = flag != 1;
	    UNPROTECT(1);
	    check_stack_balance(op, save);
	    vmaxset(vmax);
	}
	else if (TYPEOF(op) == BUILTINSXP) {
	    int save = R_PPStackTop, flag = PRIMPRINT(op);
	    const void *vmax = vmaxget();
	    RCNTXT cntxt;
	    PROTECT(tmp = evalList(CDR(e), rho, e, 0));
	    if (flag < 2) R_Visible = flag != 1;
	    /* We used to insert a context only if profiling,
	       but helps for tracebacks on .C etc. */
	    if (R_Profiling || (PPINFO(op).kind == PP_FOREIGN)) {
		SEXP oldref = R_Srcref;
		begincontext(&cntxt, CTXT_BUILTIN, e,
			     R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
		R_Srcref = NULL;
		tmp = PRIMFUN(op) (e, op, tmp, rho);
		R_Srcref = oldref;
		endcontext(&cntxt);
	    } else {
		tmp = PRIMFUN(op) (e, op, tmp, rho);
	    }
#ifdef CHECK_VISIBILITY
	    if(flag < 2 && R_Visible == flag) {
		char *nm = PRIMNAME(op);
		printf("vis: builtin %s\n", nm);
	    }
#endif
	    if (flag < 2) R_Visible = flag != 1;
	    UNPROTECT(1);
	    check_stack_balance(op, save);
	    vmaxset(vmax);
	}
	else if (TYPEOF(op) == CLOSXP) {
	    SEXP pargs = promiseArgs(CDR(e), rho);
	    PROTECT(pargs);
	    tmp = applyClosure(e, op, pargs, rho, R_NilValue);
#ifdef ADJUST_ENVIR_REFCNTS
	    unpromiseArgs(pargs);
#endif
	    UNPROTECT(1);
	}
	else
	    error(_("attempt to apply non-function"));
	UNPROTECT(1);
	break;
    case DOTSXP:
	error(_("'...' used in an incorrect context"));
    default:
	UNIMPLEMENTED_TYPE("eval", e);
    }
    R_EvalDepth = depthsave;
    R_Srcref = srcrefsave;
    R_BCIntActive = bcintactivesave;
    return (tmp);
}

attribute_hidden
void SrcrefPrompt(const char * prefix, SEXP srcref)
{
    /* If we have a valid srcref, use it */
    if (srcref && srcref != R_NilValue) {
	if (TYPEOF(srcref) == VECSXP) srcref = VECTOR_ELT(srcref, 0);
	SEXP srcfile = getAttrib(srcref, R_SrcfileSymbol);
	if (TYPEOF(srcfile) == ENVSXP) {
	    SEXP filename = findVar(install("filename"), srcfile);
	    if (isString(filename) && length(filename)) {
		Rprintf(_("%s at %s#%d: "), prefix,
			CHAR(STRING_ELT(filename, 0)),
			asInteger(srcref));
		return;
	    }
	}
    }
    /* default: */
    Rprintf("%s: ", prefix);
}

/* JIT support */
typedef unsigned long R_exprhash_t;

static R_exprhash_t hash(unsigned char *str, int n, R_exprhash_t hash)
{
    // djb2 from http://www.cse.yorku.ca/~oz/hash.html
    // (modified for n-byte lengths)

    int i;

    for(i = 0; i < n; i++)
        hash = ((hash << 5) + hash) + str[i]; /* hash * 33 + c */

    return hash;
}

#define HASH(x, h) hash((unsigned char *) &x, sizeof(x), h)

static R_exprhash_t hashexpr1(SEXP e, R_exprhash_t h)
{
#define SKIP_NONSCALAR 	if (len != 1) break /* non-scalars hashed by address */
    int len = length(e);
    int type = TYPEOF(e);
    h = HASH(type, h);
    h = HASH(len, h);

    switch(type) {
    case LANGSXP:
    case LISTSXP:
	/**** safer to only follow while CDR is LANGSXP/LISTSXP */
	for (; e != R_NilValue; e = CDR(e))
	    h = hashexpr1(CAR(e), h);
	return h;
    case LGLSXP:
	SKIP_NONSCALAR;
	for (int i = 0; i < len; i++) {
	    int ival = LOGICAL(e)[i];
	    h = HASH(ival, h);
	}
	return h;
    case INTSXP:
	SKIP_NONSCALAR;
	for (int i = 0; i < len; i++) {
	    int ival = INTEGER(e)[i];
	    h = HASH(ival, h);
	}
	return h;
    case REALSXP:
	SKIP_NONSCALAR;
	for (int i = 0; i < len; i++) {
	    double dval = REAL(e)[i];
	    h = HASH(dval, h);
	}
	return h;
    case STRSXP:
	SKIP_NONSCALAR;
	for (int i = 0; i < len; i++) {
	    SEXP cval = STRING_ELT(e, i);
	    h = hash((unsigned char *) CHAR(cval), LENGTH(cval), h);
	}
	return h;
    }

    return HASH(e, h);
#undef SKIP_NONSCALAR
}

static R_INLINE SEXP getSrcref(SEXP srcrefs, int ind);
static R_exprhash_t hashsrcref(SEXP e, R_exprhash_t h)
{
    if (TYPEOF(e) == INTSXP && LENGTH(e) >= 6) {
	for(int i = 0; i < 6; i++) {
	    int ival = INTEGER(e)[i];
	    h = HASH(ival, h);
	}
	/* FIXME: update this when deep-comparison of srcref is available */
	SEXP srcfile = getAttrib(e, R_SrcfileSymbol);
	h = HASH(srcfile, h);
    }
    return h;
}
#undef HASH

static R_exprhash_t hashexpr(SEXP e)
{
    return hashexpr1(e, 5381);
}

static R_exprhash_t hashfun(SEXP f)
{
    R_exprhash_t h = hashexpr(BODY(f));
    if (getAttrib(BODY(f), R_SrcrefSymbol) == R_NilValue)
	h = hashsrcref(getAttrib(f, R_SrcrefSymbol), h);
    return h;
}

static void loadCompilerNamespace(void)
{
    SEXP fun, arg, expr;

    PROTECT(fun = install("getNamespace"));
    PROTECT(arg = mkString("compiler"));
    PROTECT(expr = lang2(fun, arg));
    eval(expr, R_GlobalEnv);
    UNPROTECT(3);
}

static void checkCompilerOptions(int jitEnabled)
{
    int old_visible = R_Visible;
    SEXP packsym, funsym, call, fcall, arg;

    packsym = install("compiler");
    funsym = install("checkCompilerOptions");

    PROTECT(arg = ScalarInteger(jitEnabled));
    PROTECT(fcall = lang3(R_TripleColonSymbol, packsym, funsym));
    PROTECT(call = lang2(fcall, arg));
    eval(call, R_GlobalEnv);
    UNPROTECT(3);
    R_Visible = old_visible;
}

static SEXP R_IfSymbol = NULL;
static SEXP R_ForSymbol = NULL;
static SEXP R_WhileSymbol = NULL;
static SEXP R_RepeatSymbol = NULL;

#define JIT_CACHE_SIZE 1024
static SEXP JIT_cache = NULL;
static R_exprhash_t JIT_cache_hashes[JIT_CACHE_SIZE];

/**** allow MIN_JIT_SCORE, or both, to be changed by environment variables? */
static int MIN_JIT_SCORE = 50;
#define LOOP_JIT_SCORE MIN_JIT_SCORE

static struct { unsigned long count, envcount, bdcount; } jit_info = {0, 0, 0};

void attribute_hidden R_init_jit_enabled(void)
{
    /* Need to force the lazy loading promise to avoid recursive
       promise evaluation when JIT is enabled. Might be better to do
       this in baseloader.R. */
    eval(install(".ArgsEnv"), R_BaseEnv);

    int val = 3; /* turn JIT on by default */
    char *enable = getenv("R_ENABLE_JIT");
    if (enable != NULL)
	val = atoi(enable);
    if (val) {
	loadCompilerNamespace();
	checkCompilerOptions(val);
    }
    R_jit_enabled = val;

    if (R_compile_pkgs <= 0) {
	char *compile = getenv("_R_COMPILE_PKGS_");
	if (compile != NULL) {
	    int val = atoi(compile);
	    if (val > 0)
		R_compile_pkgs = TRUE;
	    else
		R_compile_pkgs = FALSE;
	}
    }

    if (R_disable_bytecode <= 0) {
	char *disable = getenv("R_DISABLE_BYTECODE");
	if (disable != NULL) {
	    int val = atoi(disable);
	    if (val > 0)
		R_disable_bytecode = TRUE;
	    else
		R_disable_bytecode = FALSE;
	}
    }

    /* -1 ... duplicate constants on LDCONST and PUSHCONSTARG, no checking
        0 ... no checking (no duplication for >= 0) [DEFAULT]
	1 ... check at error, session exit and reclamation
	2 ... check also at full GC
	3 ... check also at partial GC
	4 ... check also at .Call
	5 ... (very) verbose report on modified constants
    */
    if (R_check_constants <= 1) {
	char *check = getenv("R_CHECK_CONSTANTS");
	if (check != NULL)
	    R_check_constants = atoi(check);
    }

    /* initialize JIT variables */
    R_IfSymbol = install("if");
    R_ForSymbol = install("for");
    R_WhileSymbol = install("while");
    R_RepeatSymbol = install("repeat");

    R_PreserveObject(JIT_cache = allocVector(VECSXP, JIT_CACHE_SIZE));
}

static int JIT_score(SEXP e)
{
    if (TYPEOF(e) == LANGSXP) {
	SEXP fun = CAR(e);
	if (fun == R_IfSymbol) {
	    int cons = JIT_score(CADR(e));
	    int alt =  JIT_score(CADDR(e));
	    return cons > alt ? cons : alt;
	}
	else if (fun == R_ForSymbol ||
		 fun == R_WhileSymbol ||
		 fun == R_RepeatSymbol)
	    return LOOP_JIT_SCORE;
	else {
	    int score = 1;
	    for (SEXP args = CDR(e); args != R_NilValue; args = CDR(args))
		score += JIT_score(CAR(args));
	    return score;
	}
    }
    else return 1;
}

#define STRATEGY_NO_SMALL 0
#define STRATEGY_TOP_SMALL_MAYBE 1
#define STRATEGY_ALL_SMALL_MAYBE 2
#define STRATEGY_NO_SCORE 3
#define STRATEGY_NO_CACHE 4
/* max strategy index is hardcoded in R_CheckJIT */

/*
  NO_CACHE
      functions are compiled 1st time seen
        code is never cached

  NO_SCORE
      functions are compiled 1st time seen
        code is cached
	in case of conflict function may be marked NOJIT

  ALL_SMALL_MAYBE
      functions with small score are compiled 2nd time seen
      function with high score are compiled
          1st time seen if top-level, 2nd time seen otherwise

  TOP_SMALL_MAYBE
      functions with small score compiled
          2nd time seen if top-level, never otherwise
      functions with high score compiled
          1st time seen if top-level, 2nd time seen otherwise
*/

static int jit_strategy = -1;

static R_INLINE Rboolean R_CheckJIT(SEXP fun)
{
    /* to help with testing */
    if (jit_strategy < 0) {
	int dflt = R_jit_enabled == 1 ?
	    STRATEGY_NO_SMALL : STRATEGY_TOP_SMALL_MAYBE;
	int val = dflt;
	char *valstr = getenv("R_JIT_STRATEGY");
	if (valstr != NULL)
	    val = atoi(valstr);
	if (val < 0 || val > 4)
	    jit_strategy = dflt;
	else
	    jit_strategy = val;

	valstr = getenv("R_MIN_JIT_SCORE");
	if (valstr != NULL)
	    MIN_JIT_SCORE = atoi(valstr);
    }

    SEXP body = BODY(fun);

    if (R_jit_enabled > 0 && TYPEOF(body) != BCODESXP &&
	! R_disable_bytecode && ! NOJIT(fun)) {

	if (MAYBEJIT(fun)) {
	    /* function marked as MAYBEJIT the first time now seen
	       twice, so go ahead and compile */
	    UNSET_MAYBEJIT(fun);
	    return TRUE;
	}

	if (jit_strategy == STRATEGY_NO_SCORE ||
	    jit_strategy == STRATEGY_NO_CACHE)
	    return TRUE;

	int score = JIT_score(body);
	if (jit_strategy == STRATEGY_ALL_SMALL_MAYBE)
	    if (score < MIN_JIT_SCORE) { SET_MAYBEJIT(fun); return FALSE; }

	if (CLOENV(fun) == R_GlobalEnv) {
	    /* top level functions are only compiled if score is high enough */
	    if (score < MIN_JIT_SCORE) {
		if (jit_strategy == STRATEGY_TOP_SMALL_MAYBE)
		    SET_MAYBEJIT(fun);
		else
		    SET_NOJIT(fun);
		return FALSE;
	    }
	    else return TRUE;
	}
	else {
	    /* only compile non-top-level function if score is high
	       enough and seen twice */
	    if (score < MIN_JIT_SCORE) {
		SET_NOJIT(fun);
		return FALSE;
	    }
	    else {
		SET_MAYBEJIT(fun);
		return FALSE;
	    }
	}
    }
    return FALSE;
}

#ifdef DEBUG_JIT
# define PRINT_JIT_INFO							\
    REprintf("JIT cache hits: %ld; env: %ld; body %ld\n",		\
	     jit_info.count, jit_info.envcount, jit_info.bdcount)
#else
# define PRINT_JIT_INFO	do { } while(0)
#endif


/* FIXME: this should not depend on internals from envir.c but does for now. */
/* copied from envir.c for now */
#define IS_USER_DATABASE(rho)  (OBJECT((rho)) && inherits((rho), "UserDefinedDatabase"))
#define IS_STANDARD_UNHASHED_FRAME(e) (! IS_USER_DATABASE(e) && HASHTAB(e) == R_NilValue)
#define IS_STANDARD_HASHED_FRAME(e) (! IS_USER_DATABASE(e) && HASHTAB(e) != R_NilValue)

/* This makes a snapshot of the local variables in cmpenv and creates
   a new environment with the same top level environment and bindings
   with value R_NilValue for the local variables. This guards against
   the cmpenv changing after being entered in the cache, and also
   allows large values that might be bound to local variables in
   cmpenv to be reclaimed (also, some package tests, e.g. in shiny, test
   when things get reclaimed). Standard local frames are processed directly,
   hashed frames are processed via lsInternal3, which involves extra
   allocations, but should be used rarely. If a local environment is
   of unsupported type, topenv is returned as a valid conservative
   answer.

   Since we compute the local variables at compile
   time we should record them in the byte code object and use the
   recorded value. */
static R_INLINE void cmpenv_enter_frame(SEXP frame, SEXP newenv)
{
    for (; frame != R_NilValue; frame = CDR(frame))
	defineVar(TAG(frame), R_NilValue, newenv);
}

static R_INLINE SEXP make_cached_cmpenv(SEXP fun)
{
    SEXP frmls = FORMALS(fun);
    SEXP cmpenv = CLOENV(fun);
    SEXP top = topenv(R_NilValue, cmpenv);
    if (cmpenv == top && frmls == R_NilValue)
	return cmpenv;
    else {
	SEXP newenv = PROTECT(NewEnvironment(R_NilValue, R_NilValue, top));
	for (; frmls != R_NilValue; frmls = CDR(frmls))
	    defineVar(TAG(frmls), R_NilValue, newenv);
	for (SEXP env = cmpenv; env != top; env = CDR(env)) {
	    if (IS_STANDARD_UNHASHED_FRAME(env))
		cmpenv_enter_frame(FRAME(env), newenv);
	    else if (IS_STANDARD_HASHED_FRAME(env)) {
		SEXP h = HASHTAB(env);
		int n = length(h);
		for (int i = 0; i < n; i++)
		    cmpenv_enter_frame(VECTOR_ELT(h, i), newenv);
	    } else {
		UNPROTECT(1); /* newenv */
		return top;
	    }
		/* topenv is a safe conservative answer; if a closure
		   defines anything, its environment will not match, and
		   it will never be compiled */
		/* FIXME: would it be safe to simply ignore elements of
		   of these environments? */
	}
	UNPROTECT(1); /* newenv */
	return newenv;
    }
}

/* Cache entries are CONS cells with the body in CAR, the environment
   in CDR, and the Srcref in the TAG. */
static R_INLINE void set_jit_cache_entry(R_exprhash_t hash, SEXP val)
{
    int hashidx = hash % JIT_CACHE_SIZE;

    PROTECT(val);
    SEXP entry = CONS(BODY(val), make_cached_cmpenv(val));
    SET_VECTOR_ELT(JIT_cache, hashidx, entry);
    SET_TAG(entry, getAttrib(val, R_SrcrefSymbol));
    UNPROTECT(1); /* val */

    JIT_cache_hashes[hashidx] = hash;
}

static R_INLINE SEXP jit_cache_code(SEXP entry)
{
    return CAR(entry);
}

static R_INLINE SEXP jit_cache_env(SEXP entry)
{
    return CDR(entry);
}

static R_INLINE SEXP jit_cache_srcref(SEXP entry)
{
    return TAG(entry);
}

/* forward declaration */
static SEXP bytecodeExpr(SEXP);

static R_INLINE SEXP jit_cache_expr(SEXP entry)
{
    return bytecodeExpr(jit_cache_code(entry));
}

static R_INLINE SEXP get_jit_cache_entry(R_exprhash_t hash)
{
    int hashidx = hash % JIT_CACHE_SIZE;
    if (JIT_cache_hashes[hashidx] == hash) {
	SEXP entry = VECTOR_ELT(JIT_cache, hashidx);
	if (TYPEOF(jit_cache_code(entry)) == BCODESXP)
	    return entry;
	else
	    /* function has been de-compiled; clear the cache entry */
	    SET_VECTOR_ELT(JIT_cache, hashidx, R_NilValue);
    }
    return R_NilValue;
}

static R_INLINE Rboolean jit_expr_match(SEXP expr, SEXP body)
{
    /*** is 16 right here??? does this need to be faster??? */
    return R_compute_identical(expr, body, 16);
}

static R_INLINE SEXP cmpenv_topenv(SEXP cmpenv)
{
    return topenv(R_NilValue, cmpenv);
}

static R_INLINE Rboolean cmpenv_exists_local(SEXP sym, SEXP cmpenv, SEXP top)
{
    if (cmpenv != top)
	for (SEXP frame = FRAME(cmpenv);
	     frame != R_NilValue;
	     frame = CDR(frame))
	    if (TAG(frame) == sym)
		return TRUE;
    return FALSE;
}

static R_INLINE Rboolean jit_env_match(SEXP cmpenv, SEXP fun)
{
    /* Can code compiled for environment cmpenv be used as compiled
       code for environment env?  These tests rely on the assumption
       that compilation is only affected by what variables are bound,
       not their values. So as long as both cmpenv and env have the
       same top level environment and all local bindings present in
       the formals and environment of fun are also present in cmpenv
       the code for cmpenv can be reused, though it might be less
       efficient if a binding in cmpenv prevents an optimization that
       would be possible in env. */

    SEXP env = CLOENV(fun);
    SEXP top = topenv(R_NilValue, env);

    if (top == cmpenv_topenv(cmpenv)) {
	for (SEXP frmls = FORMALS(fun); frmls != R_NilValue; frmls = CDR(frmls))
	    if (! cmpenv_exists_local(TAG(frmls), cmpenv, top))
		return FALSE;
	for (; env != top; env = ENCLOS(env)) {
	    if (IS_STANDARD_UNHASHED_FRAME(env)) {
		/* To keep things simple, for a match this code
		   requires that the local frames be standard unhashed
		   frames. */
		for (SEXP frame = FRAME(env);
		     frame != R_NilValue;
		     frame = CDR(frame))
		    if (! cmpenv_exists_local(TAG(frame), cmpenv, top))
			return FALSE;
	    }
	    else return FALSE;
	}
	return TRUE;
    }
    else return FALSE;
}

static R_INLINE Rboolean jit_srcref_match(SEXP cmpsrcref, SEXP srcref)
{
    return R_compute_identical(cmpsrcref, srcref, 0);
}

SEXP attribute_hidden R_cmpfun1(SEXP fun)
{
    int old_visible = R_Visible;
    SEXP packsym, funsym, call, fcall, val;

    packsym = install("compiler");
    funsym = install("tryCmpfun");

    PROTECT(fcall = lang3(R_TripleColonSymbol, packsym, funsym));
    PROTECT(call = lang2(fcall, fun));
    val = eval(call, R_GlobalEnv);
    if (TYPEOF(BODY(val)) != BCODESXP)
	/* Compilation may have failed because R alocator could not malloc
	   memory to extend the R heap, so we run GC to release some pages.
	   This problem has been observed while byte-compiling packages on
	   installation: serialization uses malloc to allocate buffers and
	   fails when the compiler makes R allocator exhaust malloc memory.
	   A more general solution might be to run the GC conditionally inside
	   error handling. */
	R_gc();
    UNPROTECT(2);

    R_Visible = old_visible;
    return val;
}

/* fun is modified in-place when compiled */
static void R_cmpfun(SEXP fun)
{
    R_exprhash_t hash = 0;
    if (jit_strategy != STRATEGY_NO_CACHE) {
	hash = hashfun(fun);
	SEXP entry = get_jit_cache_entry(hash);

	if (entry != R_NilValue) {
	    jit_info.count++;
	    if (jit_env_match(jit_cache_env(entry), fun)) {
		jit_info.envcount++;
		if (jit_expr_match(jit_cache_expr(entry), BODY(fun))) {
		    jit_info.bdcount++;
		    /* if function body has a srcref, all srcrefs compiled
		       in that function only depend on the body srcref;
		       but, otherwise the srcrefs compiled in are taken
		       from the function (op) */
		    if (getAttrib(BODY(fun), R_SrcrefSymbol) != R_NilValue ||
			jit_srcref_match(jit_cache_srcref(entry),
					 getAttrib(fun, R_SrcrefSymbol))) {
			PRINT_JIT_INFO;
			SET_BODY(fun, jit_cache_code(entry));
			/**** reset the cache here?*/
			return;
		    }
		}
		/* The functions probably differ only in source references
		   (for functions with bodies that have no source references
		   we know for sure, for other functions we speculate).
		   Therefore, we allow re-compilation and re-caching. This
		   situation may be caused e.g. by re-sourcing the same source
		   file or re-pasting the same definitions for a function in
		   interactive R session. Note srcref information includes
		   environments (srcfile), which are now compared by address,
		   so it may be we actually have logically identical source
		   references, anyway. */
		/* FIXME: revisit this when deep comparison of environments
			  (and srcrefs) is available */
	    } else {
		SET_NOJIT(fun);
		/**** also mark the cache entry as NOJIT, or as need to see
		      many times? */
		return;
	    }
	}
	PRINT_JIT_INFO;
    }

    SEXP val = R_cmpfun1(fun);

    if (TYPEOF(BODY(val)) != BCODESXP)
	SET_NOJIT(fun);
    else {
	if (jit_strategy != STRATEGY_NO_CACHE)
	    set_jit_cache_entry(hash, val); /* val is protected by callee */
	SET_BODY(fun, BODY(val));
    }
}

static SEXP R_compileExpr(SEXP expr, SEXP rho)
{
    int old_visible = R_Visible;
    SEXP packsym, funsym, quotesym;
    SEXP qexpr, call, fcall, val;

    packsym = install("compiler");
    funsym = install("tryCompile");
    quotesym = install("quote");

    PROTECT(fcall = lang3(R_TripleColonSymbol, packsym, funsym));
    PROTECT(qexpr = lang2(quotesym, expr));
    /* compile(e, env, options, srcref) */
    PROTECT(call = lang5(fcall, qexpr, rho, R_NilValue, R_getCurrentSrcref()));
    val = eval(call, R_GlobalEnv);
    UNPROTECT(3);
    R_Visible = old_visible;
    return val;
}

static Rboolean R_compileAndExecute(SEXP call, SEXP rho)
{
    int old_enabled = R_jit_enabled;
    SEXP code;
    Rboolean ans = FALSE;

    R_jit_enabled = 0;
    PROTECT(call);
    PROTECT(rho);
    PROTECT(code = R_compileExpr(call, rho));
    R_jit_enabled = old_enabled;

    if (TYPEOF(code) == BCODESXP) {
	bcEval(code, rho, TRUE);
	ans = TRUE;
    }

    UNPROTECT(3);
    return ans;
}

SEXP attribute_hidden do_enablejit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int old = R_jit_enabled, new;
    checkArity(op, args);
    new = asInteger(CAR(args));
    if (new >= 0) {
	if (new > 0)
	    loadCompilerNamespace();
	checkCompilerOptions(new);
	R_jit_enabled = new;
    }
    /* negative 'new' just returns 'old' */
    return ScalarInteger(old);
}

SEXP attribute_hidden do_compilepkgs(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int old = R_compile_pkgs, new;
    checkArity(op, args);
    new = asLogical(CAR(args));
    if (new != NA_LOGICAL && new)
	loadCompilerNamespace();
    R_compile_pkgs = new;
    return ScalarLogical(old);
}

/* this function gets the srcref attribute from a statement block,
   and confirms it's in the expected format */

static R_INLINE SEXP getBlockSrcrefs(SEXP call)
{
    SEXP srcrefs = getAttrib(call, R_SrcrefSymbol);
    if (TYPEOF(srcrefs) == VECSXP) return srcrefs;
    return R_NilValue;
}

/* this function extracts one srcref, and confirms the format */
/* It assumes srcrefs has already been validated to be a VECSXP or NULL */

static R_INLINE SEXP getSrcref(SEXP srcrefs, int ind)
{
    SEXP result;
    if (!isNull(srcrefs)
	&& length(srcrefs) > ind
	&& !isNull(result = VECTOR_ELT(srcrefs, ind))
	&& TYPEOF(result) == INTSXP
	&& length(result) >= 6)
	return result;
    else
	return R_NilValue;
}

#ifdef ADJUST_ENVIR_REFCNTS
static R_INLINE Rboolean R_isReplaceSymbol(SEXP fun)
{
    /* fun is a replacement function name if it contains '<-'
       anywhere. For internally dispatched replacement functions this
       may occur in the middle; in other cases it will be at the
       end. */
    if (TYPEOF(fun) == SYMSXP &&
	strstr(CHAR(PRINTNAME(fun)), "<-"))
	return TRUE;
    else return FALSE;
}	
#endif

/* There's another copy of this in main.c */
static void PrintCall(SEXP call, SEXP rho)
{
    int old_bl = R_BrowseLines,
        blines = asInteger(GetOption1(install("deparse.max.lines")));
    if(blines != NA_INTEGER && blines > 0)
	R_BrowseLines = blines;

    R_PrintData pars;
    PrintInit(&pars, rho);
    PrintValueRec(call, &pars);

    R_BrowseLines = old_bl;
}

#ifdef ADJUST_ENVIR_REFCNTS
/* After executing a closure call the environment created for the call
   may no longer be reachable. If this is the case, then its bindings
   can be cleared to reduce the reference counts on the binding
   values.

   The environment will no longer be reachable if it is not being
   returned as the value of the closure and has no references. It will
   also no longer be reachable be the case if all references to it are
   internal cycles through its bindings. A full check for internal
   cycles would be too expensive, but the two most important cases can
   be checked at reasonable cost:

   - a promise with no other references, most likely from an
     unevaluated argument default expression;

   - a closure with no further references and not returned as the
     value, most likely a local helper function.

   The promises created for a closure call can also be cleared one the
   call is complete and the promises are no longer reachable. This
   drops reference counts on the values and the environments.
*/

static int countCycleRefs(SEXP rho, SEXP val)
{
    /* check for simple cycles */
    int crefs = 0;
    for (SEXP b = FRAME(rho);
	 b != R_NilValue && REFCNT(b) == 1;
	 b = CDR(b)) {
	SEXP v = CAR(b);
	if (val != v) {
	    switch(TYPEOF(v)) {
	    case PROMSXP:
		if (REFCNT(v) == 1 && PRENV(v) == rho)
		    crefs++;
		break;
	    case CLOSXP:
		if (REFCNT(v) == 1 && CLOENV(v) == rho)
		    crefs++;
		break;
	    case ENVSXP: /* is this worth bothering with? */
		if (v == rho)
		    crefs++;
		break;
	    }
	}
    }
    return crefs;
}

static R_INLINE void cleanupEnvDots(SEXP d)
{
    for (; d != R_NilValue && REFCNT(d) == 1; d = CDR(d)) {
	SEXP v = CAR(d);
	if (REFCNT(v) == 1 && TYPEOF(v) == PROMSXP) {
	    SET_PRVALUE(v, R_UnboundValue);
	    SET_PRENV(v, R_NilValue);
	}
	SETCAR(d, R_NilValue);
    }
}

static R_INLINE void cleanupEnvVector(SEXP v)
{
    /* This is mainly for handling results of list(...) stored as a
       local variable. It would be cheaper to just use
       DECREMENT_REFCNT. It might also make sense to max out at len =
       10 or so. But this may still be too expensive. */

    /* FIXME: Disabled for now since a BUILTIN that saves its (NR)
       list can cause problems. .External.graphics does this for
       recording. Probably the best option is to not have the args go
       down as NR. */
    return;

    R_xlen_t len = LENGTH(v);
    for (R_xlen_t i = 0; i < len; i++)
	SET_VECTOR_ELT(v, i, R_NilValue);
}

static R_INLINE void R_CleanupEnvir(SEXP rho, SEXP val)
{
    if (val != rho) {
	/* release the bindings and promises in rho if rho is no
	   longer accessible from R */
	int refs = REFCNT(rho);
	if (refs > 0)
	    refs -= countCycleRefs(rho, val);
	if (refs == 0) {
	    for (SEXP b = FRAME(rho);
		 b != R_NilValue && REFCNT(b) == 1;
		 b = CDR(b)) {
		SEXP v = CAR(b);
		if (REFCNT(v) == 1 && v != val) {
		    switch(TYPEOF(v)) {
		    case PROMSXP:
			SET_PRVALUE(v, R_UnboundValue);
			SET_PRENV(v, R_NilValue);
			break;
		    case DOTSXP:
			cleanupEnvDots(v);
			break;
		    case VECSXP: /* mainly for list(...) */
			cleanupEnvVector(v);
			break;
		    }
		}
		SETCAR(b, R_NilValue);
	    }
	    SET_ENCLOS(rho, R_EmptyEnv);
	}
    }
}

void attribute_hidden unpromiseArgs(SEXP pargs)
{
    /* This assumes pargs will no longer be references. We could
       double check the refcounts on pargs as a sanity check. */
    for (; pargs != R_NilValue; pargs = CDR(pargs)) {
	SEXP v = CAR(pargs);
	if (TYPEOF(v) == PROMSXP && REFCNT(v) == 1) {
	    SET_PRVALUE(v, R_UnboundValue);
	    SET_PRENV(v, R_NilValue);
	}
	SETCAR(pargs, R_NilValue);
    }
}
#endif

/* Note: GCC will not inline execClosure because it calls setjmp */
static R_INLINE SEXP R_execClosure(SEXP call, SEXP newrho, SEXP sysparent,
                                   SEXP rho, SEXP arglist, SEXP op);

/* Apply SEXP op of type CLOSXP to actuals */
SEXP applyClosure(SEXP call, SEXP op, SEXP arglist, SEXP rho, SEXP suppliedvars)
{
    SEXP formals, actuals, savedrho, newrho;
    SEXP f, a;

    /* formals = list of formal parameters */
    /* actuals = values to be bound to formals */
    /* arglist = the tagged list of arguments */

    /* protection against rho = NULL */
    // these are deliberately not translated
    if (!rho)
	errorcall(call,
		  "'rho' cannot be C NULL: detected in C-level applyClosure");
    if (!isEnvironment(rho))
	errorcall(call, "'rho' must be an environment not %s: detected in C-level applyClosure",
		  type2char(TYPEOF(rho)));

    formals = FORMALS(op);
    savedrho = CLOENV(op);

    /*  Build a list which matches the actual (unevaluated) arguments
	to the formal paramters.  Build a new environment which
	contains the matched pairs.  matchArgs_RC is used since the
	result becomes part of the environment frame and so needs
	reference couting enabled. */

    actuals = matchArgs_RC(formals, arglist, call);
    PROTECT(newrho = NewEnvironment(formals, actuals, savedrho));

    /*  Use the default code for unbound formals.  FIXME: It looks like
	this code should preceed the building of the environment so that
	this will also go into the hash table.  */

    /* This piece of code is destructively modifying the actuals list,
       which is now also the list of bindings in the frame of newrho.
       This is one place where internal structure of environment
       bindings leaks out of envir.c.  It should be rewritten
       eventually so as not to break encapsulation of the internal
       environment layout.  We can live with it for now since it only
       happens immediately after the environment creation.  LT */

    f = formals;
    a = actuals;
    while (f != R_NilValue) {
	if (CAR(a) == R_MissingArg && CAR(f) != R_MissingArg) {
	    SETCAR(a, mkPROMISE(CAR(f), newrho));
	    SET_MISSING(a, 2);
	}
	f = CDR(f);
	a = CDR(a);
    }

    /*  Fix up any extras that were supplied by usemethod. */

    if (suppliedvars != R_NilValue)
	addMissingVarsToNewEnv(newrho, suppliedvars);

    if (R_envHasNoSpecialSymbols(newrho))
	SET_NO_SPECIAL_SYMBOLS(newrho);

#ifdef ADJUST_ENVIR_REFCNTS
    Rboolean is_getter_call =
	(CADR(call) == R_TmpvalSymbol && ! R_isReplaceSymbol(CAR(call)));
#endif

    UNPROTECT(1); /* newrho - below protected via context */

    /*  If we have a generic function we need to use the sysparent of
	the generic as the sysparent of the method because the method
	is a straight substitution of the generic.  */

    SEXP val = R_execClosure(call, newrho,
			     (R_GlobalContext->callflag == CTXT_GENERIC) ?
			     R_GlobalContext->sysparent : rho,
			     rho, arglist, op);
#ifdef ADJUST_ENVIR_REFCNTS
    R_CleanupEnvir(newrho, val);
    if (MAYBE_REFERENCED(val) && is_getter_call)
    	val = shallow_duplicate(val);
#endif
    return val;
}

static R_INLINE SEXP R_execClosure(SEXP call, SEXP newrho, SEXP sysparent,
                                   SEXP rho, SEXP arglist, SEXP op)
{
    volatile SEXP body;
    RCNTXT cntxt;
    Rboolean dbg = FALSE;

    begincontext(&cntxt, CTXT_RETURN, call, newrho, sysparent, arglist, op);

    body = BODY(op);
    if (R_CheckJIT(op)) {
	int old_enabled = R_jit_enabled;
	R_jit_enabled = 0;
	R_cmpfun(op);
	body = BODY(op);
	R_jit_enabled = old_enabled;
    }

    /* Get the srcref record from the closure object. The old srcref was
       saved in cntxt. */

    R_Srcref = getAttrib(op, R_SrcrefSymbol);

    /* Debugging */

    if ((RDEBUG(op) && R_current_debug_state()) || RSTEP(op)
         || (RDEBUG(rho) && R_BrowserLastCommand == 's')) {

	dbg = TRUE;
	SET_RSTEP(op, 0);
	SET_RDEBUG(newrho, 1);
	cntxt.browserfinish = 0; /* Don't want to inherit the "f" */
	/* switch to interpreted version when debugging compiled code */
	if (TYPEOF(body) == BCODESXP)
	    body = bytecodeExpr(body);
	Rprintf("debugging in: ");
	PrintCall(call, rho);
	SrcrefPrompt("debug", R_Srcref);
	PrintValue(body);
	do_browser(call, op, R_NilValue, newrho);
    }

    /*  Set a longjmp target which will catch any explicit returns
	from the function body.  */

    if ((SETJMP(cntxt.cjmpbuf))) {
	if (!cntxt.jumptarget) {
	    /* ignores intermediate jumps for on.exits */
	    if (R_ReturnedValue == R_RestartToken) {
		cntxt.callflag = CTXT_RETURN;  /* turn restart off */
		R_ReturnedValue = R_NilValue;  /* remove restart token */
		cntxt.returnValue = eval(body, newrho);
	    } else
		cntxt.returnValue = R_ReturnedValue;
	}
	else
	    cntxt.returnValue = NULL; /* undefined */
    }
    else
	/* make it available to on.exit and implicitly protect */
	cntxt.returnValue = eval(body, newrho);

    R_Srcref = cntxt.srcref;
    endcontext(&cntxt);

    if (dbg) {
	Rprintf("exiting from: ");
	PrintCall(call, rho);
    }

    /* clear R_ReturnedValue to allow GC to reclaim old value */
    R_ReturnedValue = R_NilValue;

    return cntxt.returnValue;
}

SEXP R_forceAndCall(SEXP e, int n, SEXP rho)
{
    SEXP fun, tmp;
    if (TYPEOF(CAR(e)) == SYMSXP)
	/* This will throw an error if the function is not found */
	PROTECT(fun = findFun(CAR(e), rho));
    else
	PROTECT(fun = eval(CAR(e), rho));

    if (TYPEOF(fun) == SPECIALSXP) {
	int flag = PRIMPRINT(fun);
	PROTECT(e);
	R_Visible = flag != 1;
	tmp = PRIMFUN(fun) (e, fun, CDR(e), rho);
	if (flag < 2) R_Visible = flag != 1;
	UNPROTECT(1);
    }
    else if (TYPEOF(fun) == BUILTINSXP) {
	int flag = PRIMPRINT(fun);
	PROTECT(tmp = evalList(CDR(e), rho, e, 0));
	if (flag < 2) R_Visible = flag != 1;
	/* We used to insert a context only if profiling,
	   but helps for tracebacks on .C etc. */
	if (R_Profiling || (PPINFO(fun).kind == PP_FOREIGN)) {
	    RCNTXT cntxt;
	    SEXP oldref = R_Srcref;
	    begincontext(&cntxt, CTXT_BUILTIN, e,
			 R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
	    R_Srcref = NULL;
	    tmp = PRIMFUN(fun) (e, fun, tmp, rho);
	    R_Srcref = oldref;
	    endcontext(&cntxt);
	} else {
	    tmp = PRIMFUN(fun) (e, fun, tmp, rho);
	}
	if (flag < 2) R_Visible = flag != 1;
	UNPROTECT(1);
    }
    else if (TYPEOF(fun) == CLOSXP) {
	PROTECT(tmp = promiseArgs(CDR(e), rho));
	SEXP a;
	int i;
	for (a = tmp, i = 0; i < n && a != R_NilValue; a = CDR(a), i++) {
	    SEXP p = CAR(a);
	    if (TYPEOF(p) == PROMSXP)
		eval(p, rho);
	    else if (p == R_MissingArg)
		errorcall(e, _("argument %d is empty"), i + 1);
	    else error("something weird happened");
	}
	SEXP pargs = tmp;
	tmp = applyClosure(e, fun, pargs, rho, R_NilValue);
#ifdef ADJUST_ENVIR_REFCNTS
	unpromiseArgs(pargs);
#endif
	UNPROTECT(1);
    }
    else {
	tmp = R_NilValue; /* -Wall */
	error(_("attempt to apply non-function"));
    }

    UNPROTECT(1);
    return tmp;
}

SEXP attribute_hidden do_forceAndCall(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int n = asInteger(eval(CADR(call), rho));
    SEXP e = CDDR(call);

    /* this would not be needed if CDDR(call) was a LANGSXP */
    PROTECT(e = LCONS(CAR(e), CDR(e)));
    SEXP val = R_forceAndCall(e, n, rho);
    UNPROTECT(1);
    return val;
}

/* **** FIXME: Temporary code to execute S4 methods in a way that
   **** preserves lexical scope. */

/* called from methods_list_dispatch.c */
SEXP R_execMethod(SEXP op, SEXP rho)
{
    SEXP call, arglist, callerenv, newrho, next, val;
    RCNTXT *cptr;

    /* create a new environment frame enclosed by the lexical
       environment of the method */
    PROTECT(newrho = Rf_NewEnvironment(R_NilValue, R_NilValue, CLOENV(op)));

    /* copy the bindings for the formal environment from the top frame
       of the internal environment of the generic call to the new
       frame.  need to make sure missingness information is preserved
       and the environments for any default expression promises are
       set to the new environment.  should move this to envir.c where
       it can be done more efficiently. */
    for (next = FORMALS(op); next != R_NilValue; next = CDR(next)) {
	SEXP symbol =  TAG(next);
	R_varloc_t loc;
	int missing;
	loc = R_findVarLocInFrame(rho,symbol);
	if(R_VARLOC_IS_NULL(loc))
	    error(_("could not find symbol \"%s\" in environment of the generic function"),
		  CHAR(PRINTNAME(symbol)));
	missing = R_GetVarLocMISSING(loc);
	val = R_GetVarLocValue(loc);
	SET_FRAME(newrho, CONS(val, FRAME(newrho)));
	SET_TAG(FRAME(newrho), symbol);
	if (missing) {
	    SET_MISSING(FRAME(newrho), missing);
	    if (TYPEOF(val) == PROMSXP && PRENV(val) == rho) {
		SEXP deflt;
		SET_PRENV(val, newrho);
		/* find the symbol in the method, copy its expression
		 * to the promise */
		for(deflt = CAR(op); deflt != R_NilValue; deflt = CDR(deflt)) {
		    if(TAG(deflt) == symbol)
			break;
		}
		if(deflt == R_NilValue)
		    error(_("symbol \"%s\" not in environment of method"),
			  CHAR(PRINTNAME(symbol)));
		SET_PRCODE(val, CAR(deflt));
	    }
	}
    }

    /* copy the bindings of the special dispatch variables in the top
       frame of the generic call to the new frame */
    defineVar(R_dot_defined, findVarInFrame(rho, R_dot_defined), newrho);
    defineVar(R_dot_Method, findVarInFrame(rho, R_dot_Method), newrho);
    defineVar(R_dot_target, findVarInFrame(rho, R_dot_target), newrho);

    /* copy the bindings for .Generic and .Methods.  We know (I think)
       that they are in the second frame, so we could use that. */
    defineVar(R_dot_Generic, findVar(R_dot_Generic, rho), newrho);
    defineVar(R_dot_Methods, findVar(R_dot_Methods, rho), newrho);

    /* Find the calling context.  Should be R_GlobalContext unless
       profiling has inserted a CTXT_BUILTIN frame. */
    cptr = R_GlobalContext;
    if (cptr->callflag & CTXT_BUILTIN)
	cptr = cptr->nextcontext;

    /* The calling environment should either be the environment of the
       generic, rho, or the environment of the caller of the generic,
       the current sysparent. */
    callerenv = cptr->sysparent; /* or rho? */

    /* get the rest of the stuff we need from the current context,
       execute the method, and return the result */
    call = cptr->call;
    arglist = cptr->promargs;
    val = R_execClosure(call, newrho, callerenv, callerenv, arglist, op);
#ifdef ADJUST_ENVIR_REFCNTS
    R_CleanupEnvir(newrho, val);
#endif
    UNPROTECT(1);
    return val;
}

static SEXP EnsureLocal(SEXP symbol, SEXP rho)
{
    SEXP vl;

    if ((vl = findVarInFrame3(rho, symbol, TRUE)) != R_UnboundValue) {
	vl = eval(symbol, rho);	/* for promises */
	if(MAYBE_SHARED(vl)) {
	    PROTECT(vl = shallow_duplicate(vl));
	    defineVar(symbol, vl, rho);
	    INCREMENT_NAMED(vl);
	    UNPROTECT(1);
	}
	return vl;
    }

    vl = eval(symbol, ENCLOS(rho));
    if (vl == R_UnboundValue)
	error(_("object '%s' not found"), EncodeChar(PRINTNAME(symbol)));

    PROTECT(vl = shallow_duplicate(vl));
    defineVar(symbol, vl, rho);
    INCREMENT_NAMED(vl);
    UNPROTECT(1);
    return vl;
}


/* Note: If val is a language object it must be protected */
/* to prevent evaluation.  As an example consider */
/* e <- quote(f(x=1,y=2); names(e) <- c("","a","b") */

static SEXP R_valueSym = NULL; /* initialized in R_initAssignSymbols below */

static SEXP replaceCall(SEXP fun, SEXP val, SEXP args, SEXP rhs)
{
    SEXP tmp, ptmp;
    PROTECT(fun);
    PROTECT(args);
    PROTECT(rhs);
    PROTECT(val);
    ptmp = tmp = allocList(length(args)+3);
    UNPROTECT(4);
    SETCAR(ptmp, fun); ptmp = CDR(ptmp);
    SETCAR(ptmp, val); ptmp = CDR(ptmp);
    while(args != R_NilValue) {
	SETCAR(ptmp, CAR(args));
	SET_TAG(ptmp, TAG(args));
	ptmp = CDR(ptmp);
	args = CDR(args);
    }
    SETCAR(ptmp, rhs);
    SET_TAG(ptmp, R_valueSym);
    SET_TYPEOF(tmp, LANGSXP);
    return tmp;
}


static SEXP assignCall(SEXP op, SEXP symbol, SEXP fun,
		       SEXP val, SEXP args, SEXP rhs)
{
    PROTECT(op);
    PROTECT(symbol);
    val = replaceCall(fun, val, args, rhs);
    UNPROTECT(2);
    return lang3(op, symbol, val);
}


/* rho only needed for _R_CHECK_LENGTH_1_CONDITION_=package:name */
static R_INLINE Rboolean asLogicalNoNA(SEXP s, SEXP call, SEXP rho)
{
    Rboolean cond = NA_LOGICAL;

    /* handle most common special case directly */
    if (IS_SCALAR(s, LGLSXP)) {
	cond = SCALAR_LVAL(s);
	if (cond != NA_LOGICAL)
	    return cond;
    }
    else if (IS_SCALAR(s, INTSXP)) {
	int val = SCALAR_IVAL(s);
	if (val != NA_INTEGER)
	    return val != 0;
    }

    int len = length(s);
    if (len > 1) {
	PROTECT(s);	 /* needed as per PR#15990.  call gets protected by warningcall() */
	char *check = getenv("_R_CHECK_LENGTH_1_CONDITION_");
	const void *vmax = vmaxget();
	Rboolean err = check && StringTrue(check); /* warn by default */
	if (!err && check) {
	    /* err when the condition is evaluated in given package */
	    const char *pprefix  = "package:";
	    size_t lprefix = strlen(pprefix);
	    if (!strncmp(pprefix, check, lprefix)) {
		/* check starts with "package:" */
		SEXP spkg = R_NilValue;
		for(; spkg == R_NilValue && rho != R_EmptyEnv; rho = ENCLOS(rho))
		    if (R_IsPackageEnv(rho))
			spkg = R_PackageEnvName(rho);
		    else if (R_IsNamespaceEnv(rho))
			spkg = R_NamespaceEnvSpec(rho);
		if (spkg != R_NilValue) {
		    const char *pkgname = translateChar(STRING_ELT(spkg, 0));
		    if (!strcmp(check + lprefix, pkgname))
			err = TRUE;
		    if (!strcmp(check + lprefix, "_R_CHECK_PACKAGE_NAME_")) {
			/* package name specified in _R_CHECK_PACKAGE_NAME */
			const char *envpname = getenv("_R_CHECK_PACKAGE_NAME_");
			if (envpname && !strcmp(envpname, pkgname))
			    err = TRUE;
		    }
		}
	    }
	}
	if (err)
	    errorcall(call, _("the condition has length > 1"));
        else
	    warningcall(call,
		    _("the condition has length > 1 and only the first element will be used"));
	vmaxset(vmax);
	UNPROTECT(1);
    }
    if (len > 0) {
	/* inline common cases for efficiency */
	switch(TYPEOF(s)) {
	case LGLSXP:
	    cond = LOGICAL(s)[0];
	    break;
	case INTSXP:
	    cond = INTEGER(s)[0]; /* relies on NA_INTEGER == NA_LOGICAL */
	    break;
	default:
	    cond = asLogical(s);
	}
    }

    if (cond == NA_LOGICAL) {
	char *msg = len ? (isLogical(s) ?
			   _("missing value where TRUE/FALSE needed") :
			   _("argument is not interpretable as logical")) :
	    _("argument is of length zero");
	PROTECT(s);	/* Maybe needed in some weird circumstance. */
	errorcall(call, msg);
	UNPROTECT(1);
    }
    return cond;
}


#define BodyHasBraces(body) \
    ((isLanguage(body) && CAR(body) == R_BraceSymbol) ? 1 : 0)

/* Allocate space for the loop variable value the first time through
   (when v == R_NilValue) and when the value has been assigned to
   another variable (NAMED(v) > 1). This should be safe and avoid
   allocation in many cases. */
#define ALLOC_LOOP_VAR(v, val_type, vpi) do { \
	if (v == R_NilValue || MAYBE_SHARED(v)) { \
	    REPROTECT(v = allocVector(val_type, 1), vpi); \
	    INCREMENT_NAMED(v);				  \
	} \
    } while(0)

SEXP attribute_hidden do_if(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP Cond, Stmt=R_NilValue;
    int vis=0;

    PROTECT(Cond = eval(CAR(args), rho));
    if (asLogicalNoNA(Cond, call, rho))
	Stmt = CAR(CDR(args));
    else {
	if (length(args) > 2)
	    Stmt = CAR(CDR(CDR(args)));
	else
	    vis = 1;
    }
    if( !vis && RDEBUG(rho) && !BodyHasBraces(Stmt) && !R_GlobalContext->browserfinish) {
	SrcrefPrompt("debug", R_Srcref);
	PrintValue(Stmt);
	do_browser(call, op, R_NilValue, rho);
    }
    UNPROTECT(1);
    if( vis ) {
	R_Visible = FALSE; /* case of no 'else' so return invisible NULL */
	return Stmt;
    }
    return (eval(Stmt, rho));
}

static R_INLINE SEXP GET_BINDING_CELL(SEXP symbol, SEXP rho)
{
    if (rho == R_BaseEnv || rho == R_BaseNamespace)
	return R_NilValue;
    else {
	R_varloc_t loc = R_findVarLocInFrame(rho, symbol);
	return (! R_VARLOC_IS_NULL(loc)) ? loc.cell : R_NilValue;
    }
}

static R_INLINE Rboolean SET_BINDING_VALUE(SEXP loc, SEXP value) {
    /* This depends on the current implementation of bindings */
    if (loc != R_NilValue &&
	! BINDING_IS_LOCKED(loc) && ! IS_ACTIVE_BINDING(loc)) {
	if (CAR(loc) != value) {
	    SETCAR(loc, value);
	    if (MISSING(loc))
		SET_MISSING(loc, 0);
	}
	return TRUE;
    }
    else
	return FALSE;
}

SEXP attribute_hidden do_for(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* Need to declare volatile variables whose values are relied on
       after for_next or for_break longjmps and might change between
       the setjmp and longjmp calls. Theoretically this does not
       include n and bgn, but gcc -O2 -Wclobbered warns about these so
       to be safe we declare them volatile as well. */
    volatile int i = 0, n, bgn;
    volatile SEXP v, val, cell;
    int dbg, val_type;
    SEXP sym, body;
    RCNTXT cntxt;
    PROTECT_INDEX vpi;

    checkArity(op, args);
    sym = CAR(args);
    val = CADR(args);
    body = CADDR(args);

    if ( !isSymbol(sym) ) errorcall(call, _("non-symbol loop variable"));

    dbg = RDEBUG(rho);
    if (R_jit_enabled > 2 && !dbg && !R_disable_bytecode
	    && rho == R_GlobalEnv
	    && isUnmodifiedSpecSym(CAR(call), rho)
	    && R_compileAndExecute(call, rho))
	return R_NilValue;

    PROTECT(args);
    PROTECT(rho);
    PROTECT(val = eval(val, rho));

    /* deal with the case where we are iterating over a factor
       we need to coerce to character - then iterate */

    if ( inherits(val, "factor") ) {
	SEXP tmp = asCharacterFactor(val);
	UNPROTECT(1); /* val from above */
	PROTECT(val = tmp);
    }

    if (isList(val) || isNull(val))
	n = length(val);
    else
	n = LENGTH(val);

    val_type = TYPEOF(val);

    defineVar(sym, R_NilValue, rho);
    PROTECT(cell = GET_BINDING_CELL(sym, rho));
    bgn = BodyHasBraces(body);

    /* bump up links count of sequence to avoid modification by loop code */
    INCREMENT_LINKS(val);

    PROTECT_WITH_INDEX(v = R_NilValue, &vpi);

    begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue,
		 R_NilValue);
    switch (SETJMP(cntxt.cjmpbuf)) {
    case CTXT_BREAK: goto for_break;
    case CTXT_NEXT: goto for_next;
    }

    for (i = 0; i < n; i++) {

	switch (val_type) {

	case EXPRSXP:
	case VECSXP:
	    /* make sure loop variable is not modified via other vars */
	    ENSURE_NAMEDMAX(VECTOR_ELT(val, i));
	    /* defineVar is used here and below rather than setVar in
	       case the loop code removes the variable. */
	    defineVar(sym, VECTOR_ELT(val, i), rho);
	    break;

	case LISTSXP:
	    /* make sure loop variable is not modified via other vars */
	    ENSURE_NAMEDMAX(CAR(val));
	    defineVar(sym, CAR(val), rho);
	    val = CDR(val);
	    break;

	default:

	    switch (val_type) {
	    case LGLSXP:
		ALLOC_LOOP_VAR(v, val_type, vpi);
		SET_SCALAR_LVAL(v, LOGICAL_ELT(val, i));
		break;
	    case INTSXP:
		ALLOC_LOOP_VAR(v, val_type, vpi);
		SET_SCALAR_IVAL(v, INTEGER_ELT(val, i));
		break;
	    case REALSXP:
		ALLOC_LOOP_VAR(v, val_type, vpi);
		SET_SCALAR_DVAL(v, REAL_ELT(val, i));
		break;
	    case CPLXSXP:
		ALLOC_LOOP_VAR(v, val_type, vpi);
		SET_SCALAR_CVAL(v, COMPLEX_ELT(val, i));
		break;
	    case STRSXP:
		ALLOC_LOOP_VAR(v, val_type, vpi);
		SET_STRING_ELT(v, 0, STRING_ELT(val, i));
		break;
	    case RAWSXP:
		ALLOC_LOOP_VAR(v, val_type, vpi);
		SET_SCALAR_BVAL(v, RAW(val)[i]);
		break;
	    default:
		errorcall(call, _("invalid for() loop sequence"));
	    }
	    if (CAR(cell) == R_UnboundValue || ! SET_BINDING_VALUE(cell, v))
		defineVar(sym, v, rho);
	}
	if (!bgn && RDEBUG(rho) && !R_GlobalContext->browserfinish) {
	    SrcrefPrompt("debug", R_Srcref);
	    PrintValue(body);
	    do_browser(call, op, R_NilValue, rho);
	}
	eval(body, rho);

    for_next:
	; /* needed for strict ISO C compliance, according to gcc 2.95.2 */
    }
 for_break:
    endcontext(&cntxt);
    DECREMENT_LINKS(val);
    UNPROTECT(5);
    SET_RDEBUG(rho, dbg);
    return R_NilValue;
}


SEXP attribute_hidden do_while(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int dbg;
    volatile int bgn;
    volatile SEXP body;
    RCNTXT cntxt;

    checkArity(op, args);

    dbg = RDEBUG(rho);
    if (R_jit_enabled > 2 && !dbg && !R_disable_bytecode
	    && rho == R_GlobalEnv
	    && isUnmodifiedSpecSym(CAR(call), rho)
	    && R_compileAndExecute(call, rho))
	return R_NilValue;

    body = CADR(args);
    bgn = BodyHasBraces(body);

    begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue,
		 R_NilValue);
    if (SETJMP(cntxt.cjmpbuf) != CTXT_BREAK) {
	while (asLogicalNoNA(eval(CAR(args), rho), call, rho)) {
	    if (RDEBUG(rho) && !bgn && !R_GlobalContext->browserfinish) {
		SrcrefPrompt("debug", R_Srcref);
		PrintValue(body);
		do_browser(call, op, R_NilValue, rho);
	    }
	    eval(body, rho);
	    if (RDEBUG(rho) && !R_GlobalContext->browserfinish) {
		SrcrefPrompt("debug", R_Srcref);
		Rprintf("(while) ");
		PrintValue(CAR(args));
		do_browser(call, op, R_NilValue, rho);
	    }
	}
    }
    endcontext(&cntxt);
    SET_RDEBUG(rho, dbg);
    return R_NilValue;
}


SEXP attribute_hidden do_repeat(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int dbg;
    volatile SEXP body;
    RCNTXT cntxt;

    checkArity(op, args);

    dbg = RDEBUG(rho);
    if (R_jit_enabled > 2 && !dbg && !R_disable_bytecode
	    && rho == R_GlobalEnv
	    && isUnmodifiedSpecSym(CAR(call), rho)
	    && R_compileAndExecute(call, rho))
	return R_NilValue;

    body = CAR(args);

    begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue,
		 R_NilValue);
    if (SETJMP(cntxt.cjmpbuf) != CTXT_BREAK) {
	for (;;) {
	    eval(body, rho);
	}
    }
    endcontext(&cntxt);
    SET_RDEBUG(rho, dbg);
    return R_NilValue;
}


SEXP attribute_hidden NORET do_break(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    findcontext(PRIMVAL(op), rho, R_NilValue);
}


SEXP attribute_hidden do_paren(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return CAR(args);
}

SEXP attribute_hidden do_begin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s = R_NilValue;
    if (args != R_NilValue) {
	SEXP srcrefs = getBlockSrcrefs(call);
	PROTECT(srcrefs);
	int i = 1;
	while (args != R_NilValue) {
	    PROTECT(R_Srcref = getSrcref(srcrefs, i++));
	    if (RDEBUG(rho) && !R_GlobalContext->browserfinish) {
		SrcrefPrompt("debug", R_Srcref);
		PrintValue(CAR(args));
		do_browser(call, op, R_NilValue, rho);
	    }
	    s = eval(CAR(args), rho);
	    UNPROTECT(1);
	    args = CDR(args);
	}
	R_Srcref = R_NilValue;
	UNPROTECT(1); /* srcrefs */
    }
    return s;
}


SEXP attribute_hidden NORET do_return(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP v;

    if (args == R_NilValue) /* zero arguments provided */
	v = R_NilValue;
    else if (CDR(args) == R_NilValue) /* one argument */
	v = eval(CAR(args), rho);
    else {
	v = R_NilValue; /* to avoid compiler warnings */
	errorcall(call, _("multi-argument returns are not permitted"));
    }

    findcontext(CTXT_BROWSER | CTXT_FUNCTION, rho, v);
}

/* Declared with a variable number of args in names.c */
SEXP attribute_hidden do_function(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval, srcref;

    if (TYPEOF(op) == PROMSXP) {
	op = forcePromise(op);
	ENSURE_NAMEDMAX(op);
    }
    if (length(args) < 2) WrongArgCount("function");
    CheckFormals(CAR(args));
    rval = mkCLOSXP(CAR(args), CADR(args), rho);
    srcref = CADDR(args);
    if (!isNull(srcref)) setAttrib(rval, R_SrcrefSymbol, srcref);
    return rval;
}


/*
 *  Assignments for complex LVAL specifications. This is the stuff that
 *  nightmares are made of ...	Note that "evalseq" preprocesses the LHS
 *  of an assignment.  Given an expression, it builds a list of partial
 *  values for the exression.  For example, the assignment x$a[3] <- 10
 *  with LHS x$a[3] yields the (improper) list:
 *
 *	 (eval(x$a[3])	eval(x$a)  eval(x)  .  x)
 *
 *  (Note the terminating symbol).  The partial evaluations are carried
 *  out efficiently using previously computed components.
 */

/*
  For complex superassignment  x[y==z]<<-w
  we want x required to be nonlocal, y,z, and w permitted to be local or
  nonlocal.
*/

static SEXP evalseq(SEXP expr, SEXP rho, int forcelocal,  R_varloc_t tmploc)
{
    SEXP val, nval, nexpr;
    if (isNull(expr))
	error(_("invalid (NULL) left side of assignment"));
    if (isSymbol(expr)) {
	PROTECT(expr);
	if(forcelocal) {
	    nval = EnsureLocal(expr, rho);
	}
	else {/* now we are down to the target symbol */
	  nval = eval(expr, ENCLOS(rho));
	}
	if (MAYBE_SHARED(nval))
	    nval = shallow_duplicate(nval);
	UNPROTECT(1);
	return CONS_NR(nval, expr);
    }
    else if (isLanguage(expr)) {
	PROTECT(expr);
	PROTECT(val = evalseq(CADR(expr), rho, forcelocal, tmploc));
	R_SetVarLocValue(tmploc, CAR(val));
	PROTECT(nexpr = LCONS(R_GetVarLocSymbol(tmploc), CDDR(expr)));
	PROTECT(nexpr = LCONS(CAR(expr), nexpr));
	nval = eval(nexpr, rho);
	/* duplicate nval if it might be shared _or_ if the container,
	   CAR(val), has become possibly shared by going through a
	   closure.  This is taken to indicate that the corresponding
	   replacement function might be a closure and will need to
	   see an unmodified LHS value. This heuristic fails if the
	   accessor function called here is not a closure but the
	   replacement function is. */
	if (MAYBE_REFERENCED(nval) &&
	    (MAYBE_SHARED(nval) || MAYBE_SHARED(CAR(val))))
	    nval = shallow_duplicate(nval);
	UNPROTECT(4);
	return CONS_NR(nval, val);
    }
    else error(_("target of assignment expands to non-language object"));
    return R_NilValue;	/*NOTREACHED*/
}

/* Main entry point for complex assignments */
/* We have checked to see that CAR(args) is a LANGSXP */

static const char * const asym[] = {":=", "<-", "<<-", "="};
#define NUM_ASYM (sizeof(asym) / sizeof(char *))
static SEXP asymSymbol[NUM_ASYM];

static SEXP R_ReplaceFunsTable = NULL;
static SEXP R_SubsetSym = NULL;
static SEXP R_SubassignSym = NULL;
static SEXP R_Subset2Sym = NULL;
static SEXP R_Subassign2Sym = NULL;
static SEXP R_DollarGetsSymbol = NULL;
static SEXP R_AssignSym = NULL;

void attribute_hidden R_initAssignSymbols(void)
{
    for (int i = 0; i < NUM_ASYM; i++)
	asymSymbol[i] = install(asym[i]);

    R_ReplaceFunsTable = R_NewHashedEnv(R_EmptyEnv, ScalarInteger(1099));
    R_PreserveObject(R_ReplaceFunsTable);

    R_SubsetSym = install("[");
    R_SubassignSym = install("[<-");
    R_Subset2Sym = install("[[");
    R_Subassign2Sym = install("[[<-");
    R_DollarGetsSymbol = install("$<-");
    R_valueSym = install("value");
    R_AssignSym = install("<-");
}

static R_INLINE SEXP lookupAssignFcnSymbol(SEXP fun)
{
    return findVarInFrame(R_ReplaceFunsTable, fun);
}

static void enterAssignFcnSymbol(SEXP fun, SEXP val)
{
    defineVar(fun, val, R_ReplaceFunsTable);
}

static void tmp_cleanup(void *data)
{
    unbindVar(R_TmpvalSymbol, (SEXP) data);
}

/* This macro stores the current assignment target in the saved
   binding location. It duplicates if necessary to make sure
   replacement functions are always called with a target with NAMED ==
   1. The SET_CAR is intended to protect against possible GC in
   R_SetVarLocValue; this might occur it the binding is an active
   binding. */
#define SET_TEMPVARLOC_FROM_CAR(loc, lhs) do { \
	SEXP __lhs__ = (lhs); \
	SEXP __v__ = CAR(__lhs__); \
	if (MAYBE_SHARED(__v__)) { \
	    __v__ = shallow_duplicate(__v__); \
	    ENSURE_NAMED(__v__); \
	    SETCAR(__lhs__, __v__); \
	} \
	R_SetVarLocValue(loc, __v__); \
    } while(0)

/* This macro makes sure the RHS NAMED value is 0 or NAMEDMAX. This is
   necessary to make sure the RHS value returned by the assignment
   expression is correct when the RHS value is part of the LHS
   object. */
#define FIXUP_RHS_NAMED(r) do { \
	SEXP __rhs__ = (r); \
	if (NAMED(__rhs__)) \
	    ENSURE_NAMEDMAX(__rhs__); \
    } while (0)

#define ASSIGNBUFSIZ 32
static SEXP installAssignFcnSymbol(SEXP fun)
{
    char buf[ASSIGNBUFSIZ];

    /* install the symbol */
    if(strlen(CHAR(PRINTNAME(fun))) + 3 > ASSIGNBUFSIZ)
	error(_("overlong name in '%s'"), EncodeChar(PRINTNAME(fun)));
    sprintf(buf, "%s<-", CHAR(PRINTNAME(fun)));
    SEXP val = install(buf);

    enterAssignFcnSymbol(fun, val);
    return val;
}

static R_INLINE SEXP getAssignFcnSymbol(SEXP fun)
{
    /* handle [<-, [[<-, and $<- efficiently */
    if (fun == R_SubsetSym)
	return R_SubassignSym;
    else if (fun == R_Subset2Sym)
	return R_Subassign2Sym;
    else if (fun == R_DollarSymbol)
	return R_DollarGetsSymbol;

    /* look up in the replacement functions table */
    SEXP val = lookupAssignFcnSymbol(fun);
    if (val != R_UnboundValue)
	return val;

    /* instal symbol, entern in table,  and return */
    return installAssignFcnSymbol(fun);
}

static R_INLINE SEXP mkRHSPROMISE(SEXP expr, SEXP rhs)
{
    return R_mkEVPROMISE_NR(expr, rhs);
}

static SEXP applydefine(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP expr, lhs, rhs, saverhs, tmp, afun, rhsprom;
    R_varloc_t tmploc;
    RCNTXT cntxt;
    int nprot;

    expr = CAR(args);

    /*  It's important that the rhs get evaluated first because
	assignment is right associative i.e.  a <- b <- c is parsed as
	a <- (b <- c).  */

    PROTECT(saverhs = rhs = eval(CADR(args), rho));
    INCREMENT_REFCNT(saverhs);

    /*  FIXME: We need to ensure that this works for hashed
	environments.  This code only works for unhashed ones.  the
	syntax error here is a deliberate marker so I don't forget that
	this needs to be done.  The code used in "missing" will help
	here.  */

    /*  FIXME: This strategy will not work when we are working in the
	data frame defined by the system hash table.  The structure there
	is different.  Should we special case here?  */

    /*  We need a temporary variable to hold the intermediate values
	in the computation.  For efficiency reasons we record the
	location where this variable is stored.  We need to protect
	the location in case the biding is removed from its
	environment by user code or an assignment within the
	assignment arguments */

    /*  There are two issues with the approach here:

	    A complex assignment within a complex assignment, like
	    f(x, y[] <- 1) <- 3, can cause the value temporary
	    variable for the outer assignment to be overwritten and
	    then removed by the inner one.  This could be addressed by
	    using multiple temporaries or using a promise for this
	    variable as is done for the RHS.  Printing of the
	    replacement function call in error messages might then need
	    to be adjusted.

	    With assignments of the form f(g(x, z), y) <- w the value
	    of 'z' will be computed twice, once for a call to g(x, z)
	    and once for the call to the replacement function g<-.  It
	    might be possible to address this by using promises.
	    Using more temporaries would not work as it would mess up
	    replacement functions that use substitute and/or
	    nonstandard evaluation (and there are packages that do
	    that -- igraph is one).

	    LT */

    FIXUP_RHS_NAMED(rhs);

    if (rho == R_BaseNamespace)
	errorcall(call, _("cannot do complex assignments in base namespace"));
    if (rho == R_BaseEnv)
	errorcall(call, _("cannot do complex assignments in base environment"));
    defineVar(R_TmpvalSymbol, R_NilValue, rho);
    tmploc = R_findVarLocInFrame(rho, R_TmpvalSymbol);
    PROTECT(tmploc.cell);
    DISABLE_REFCNT(tmploc.cell);
    DECREMENT_REFCNT(CDR(tmploc.cell));

    /* Now set up a context to remove it when we are done, even in the
     * case of an error.  This all helps error() provide a better call.
     */
    begincontext(&cntxt, CTXT_CCODE, call, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &tmp_cleanup;
    cntxt.cenddata = rho;

    /*  Do a partial evaluation down through the LHS. */
    lhs = evalseq(CADR(expr), rho,
		  PRIMVAL(op)==1 || PRIMVAL(op)==3, tmploc);

    PROTECT(lhs);
    PROTECT(rhsprom = mkRHSPROMISE(CADR(args), rhs));

    while (isLanguage(CADR(expr))) {
	nprot = 1; /* the PROTECT of rhs below from this iteration */
	if (TYPEOF(CAR(expr)) == SYMSXP)
	    tmp = getAssignFcnSymbol(CAR(expr));
	else {
	    /* check for and handle assignments of the form
	       foo::bar(x) <- y or foo:::bar(x) <- y */
	    tmp = R_NilValue; /* avoid uninitialized variable warnings */
	    if (TYPEOF(CAR(expr)) == LANGSXP &&
		(CAR(CAR(expr)) == R_DoubleColonSymbol ||
		 CAR(CAR(expr)) == R_TripleColonSymbol) &&
		length(CAR(expr)) == 3 && TYPEOF(CADDR(CAR(expr))) == SYMSXP) {
		tmp = getAssignFcnSymbol(CADDR(CAR(expr)));
		PROTECT(tmp = lang3(CAAR(expr), CADR(CAR(expr)), tmp));
		nprot++;
	    }
	    else
		error(_("invalid function in complex assignment"));
	}
	SET_TEMPVARLOC_FROM_CAR(tmploc, lhs);
	PROTECT(rhs = replaceCall(tmp, R_TmpvalSymbol, CDDR(expr), rhsprom));
	rhs = eval(rhs, rho);
	SET_PRVALUE(rhsprom, rhs);
	SET_PRCODE(rhsprom, rhs); /* not good but is what we have been doing */
	UNPROTECT(nprot);
	lhs = CDR(lhs);
	expr = CADR(expr);
    }
    nprot = 5; /* the commont case */
    if (TYPEOF(CAR(expr)) == SYMSXP)
	afun = getAssignFcnSymbol(CAR(expr));
    else {
	/* check for and handle assignments of the form
	   foo::bar(x) <- y or foo:::bar(x) <- y */
	afun = R_NilValue; /* avoid uninitialized variable warnings */
	if (TYPEOF(CAR(expr)) == LANGSXP &&
	    (CAR(CAR(expr)) == R_DoubleColonSymbol ||
	     CAR(CAR(expr)) == R_TripleColonSymbol) &&
	    length(CAR(expr)) == 3 && TYPEOF(CADDR(CAR(expr))) == SYMSXP) {
	    afun = getAssignFcnSymbol(CADDR(CAR(expr)));
	    PROTECT(afun = lang3(CAAR(expr), CADR(CAR(expr)), afun));
	    nprot++;
	}
	else
	    error(_("invalid function in complex assignment"));
    }
    SET_TEMPVARLOC_FROM_CAR(tmploc, lhs);
    PROTECT(expr = assignCall(asymSymbol[PRIMVAL(op)], CDR(lhs),
			      afun, R_TmpvalSymbol, CDDR(expr), rhsprom));
    expr = eval(expr, rho);
    endcontext(&cntxt); /* which does not run the remove */
    UNPROTECT(nprot);
    unbindVar(R_TmpvalSymbol, rho);
#ifdef OLD_RHS_NAMED
    /* we do not duplicate the value, so to be conservative mark the
       value as NAMED = NAMEDMAX */
    ENSURE_NAMEDMAX(saverhs);
#else
    INCREMENT_NAMED(saverhs);
#endif
    DECREMENT_REFCNT(saverhs);
    return saverhs;
}

/*  Assignment in its various forms  */

SEXP attribute_hidden do_set(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP lhs, rhs;

    if (args == R_NilValue ||
	CDR(args) == R_NilValue ||
	CDDR(args) != R_NilValue)
	WrongArgCount(asym[PRIMVAL(op)]);

    lhs = CAR(args);

    switch (TYPEOF(lhs)) {
    case STRSXP:
	lhs = installTrChar(STRING_ELT(lhs, 0));
	/* fall through */
    case SYMSXP:
	rhs = eval(CADR(args), rho);
	INCREMENT_NAMED(rhs);
	if (PRIMVAL(op) == 2)                       /* <<- */
	    setVar(lhs, rhs, ENCLOS(rho));
	else                                        /* <-, = */
	    defineVar(lhs, rhs, rho);
	R_Visible = FALSE;
	return rhs;
    case LANGSXP:
	R_Visible = FALSE;
	return applydefine(call, op, args, rho);
    default:
	errorcall(call, _("invalid (do_set) left-hand side to assignment"));
    }

    return R_NilValue;/*NOTREACHED*/
}


/* Evaluate each expression in "el" in the environment "rho".  This is
   a naturally recursive algorithm, but we use the iterative form below
   because it is does not cause growth of the pointer protection stack,
   and because it is a little more efficient.
*/

#define COPY_TAG(to, from) do { \
  SEXP __tag__ = TAG(from); \
  if (__tag__ != R_NilValue) SET_TAG(to, __tag__); \
} while (0)

/* Used in eval and applyMethod (object.c) for builtin primitives,
   do_internal (names.c) for builtin .Internals
   and in evalArgs.

   'n' is the number of arguments already evaluated and hence not
   passed to evalArgs and hence to here.
 */
SEXP attribute_hidden evalList(SEXP el, SEXP rho, SEXP call, int n)
{
    SEXP head, tail, ev, h, val;

    head = R_NilValue;
    tail = R_NilValue; /* to prevent uninitialized variable warnings */

    while (el != R_NilValue) {
	n++;

	if (CAR(el) == R_DotsSymbol) {
	    /* If we have a ... symbol, we look to see what it is bound to.
	     * If its binding is Null (i.e. zero length)
	     *	we just ignore it and return the cdr with all its expressions evaluated;
	     * if it is bound to a ... list of promises,
	     *	we force all the promises and then splice
	     *	the list of resulting values into the return value.
	     * Anything else bound to a ... symbol is an error
	     */
	    PROTECT(h = findVar(CAR(el), rho));
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    val = eval(CAR(h), rho);
		    if (CDR(el) != R_NilValue)
			INCREMENT_LINKS(val);
		    ev = CONS_NR(val, R_NilValue);
		    if (head == R_NilValue) {
			UNPROTECT(1); /* h */
			PROTECT(head = ev);
			PROTECT(h); /* put current h on top of protect stack */
		    }
		    else
			SETCDR(tail, ev);
		    COPY_TAG(ev, h);
		    tail = ev;
		    h = CDR(h);
		}
	    }
	    else if (h != R_MissingArg)
		error(_("'...' used in an incorrect context"));
	    UNPROTECT(1); /* h */
	} else if (CAR(el) == R_MissingArg) {
	    /* It was an empty element: most likely get here from evalArgs
	       which may have been called on part of the args. */
	    errorcall(call, _("argument %d is empty"), n);
#ifdef CHECK_IS_MISSING_IN_evalList
	    /* Radford Newl drops this R_isMissing check in pqR in
	       03-zap-isMissing (but it seems to creep in again later
	       with helper thread stuff?)  as it takes quite a bit of
	       time (essentially the equivalent of evaluating the
	       symbol, but maybe not as efficiently as eval) and only
	       serves to change the error message, not always for the
	       better. Also, the byte code interpreter does not do
	       this, so dropping this makes compiled and interreted
	       cod emore consistent. */
	} else if (isSymbol(CAR(el)) && R_isMissing(CAR(el), rho)) {
	    /* It was missing */
	    errorcall_cpy(call,
	                  _("'%s' is missing"),
	                  EncodeChar(PRINTNAME(CAR(el))));
#endif
	} else {
	    val = eval(CAR(el), rho);
	    if (CDR(el) != R_NilValue)
		INCREMENT_LINKS(val);
	    ev = CONS_NR(val, R_NilValue);
	    if (head == R_NilValue)
		PROTECT(head = ev);
	    else
		SETCDR(tail, ev);
	    COPY_TAG(ev, el);
	    tail = ev;
	}
	el = CDR(el);
    }

    for(el = head; el != R_NilValue; el = CDR(el))
	if (CDR(el) != R_NilValue)
	    DECREMENT_LINKS(CAR(el));

    if (head != R_NilValue)
	UNPROTECT(1);

    return head;

} /* evalList() */


/* A slight variation of evaluating each expression in "el" in "rho". */

/* used in evalArgs, arithmetic.c, seq.c */
SEXP attribute_hidden evalListKeepMissing(SEXP el, SEXP rho)
{
    SEXP head, tail, ev, h, val;

    head = R_NilValue;
    tail = R_NilValue; /* to prevent uninitialized variable warnings */

    while (el != R_NilValue) {

	/* If we have a ... symbol, we look to see what it is bound to.
	 * If its binding is Null (i.e. zero length)
	 *	we just ignore it and return the cdr with all its expressions evaluated;
	 * if it is bound to a ... list of promises,
	 *	we force all the promises and then splice
	 *	the list of resulting values into the return value.
	 * Anything else bound to a ... symbol is an error
	*/
	if (CAR(el) == R_DotsSymbol) {
	    PROTECT(h = findVar(CAR(el), rho));
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    if (CAR(h) == R_MissingArg)
			val = R_MissingArg;
		    else
			val = eval(CAR(h), rho);
		    if (CDR(el) != R_NilValue)
			INCREMENT_LINKS(val);
		    ev = CONS_NR(val, R_NilValue);
		    if (head == R_NilValue) {
			UNPROTECT(1); /* h */
			PROTECT(head = ev);
			PROTECT(h);
		    } else
			SETCDR(tail, ev);
		    COPY_TAG(ev, h);
		    tail = ev;
		    h = CDR(h);
		}
	    }
	    else if(h != R_MissingArg)
		error(_("'...' used in an incorrect context"));
	    UNPROTECT(1); /* h */
	}
	else {
	    if (CAR(el) == R_MissingArg ||
		(isSymbol(CAR(el)) && R_isMissing(CAR(el), rho)))
		val = R_MissingArg;
	    else
		val = eval(CAR(el), rho);
	    if (CDR(el) != R_NilValue)
		INCREMENT_LINKS(val);
	    ev = CONS_NR(val, R_NilValue);
	    if (head==R_NilValue)
		PROTECT(head = ev);
	    else
		SETCDR(tail, ev);
	    COPY_TAG(ev, el);
	    tail = ev;
	}
	el = CDR(el);
    }

    for(el = head; el != R_NilValue; el = CDR(el))
	if (CDR(el) != R_NilValue)
	    DECREMENT_LINKS(CAR(el));

    if (head!=R_NilValue)
	UNPROTECT(1);

    return head;
}


/* Create a promise to evaluate each argument.	Although this is most */
/* naturally attacked with a recursive algorithm, we use the iterative */
/* form below because it is does not cause growth of the pointer */
/* protection stack, and because it is a little more efficient. */

SEXP attribute_hidden promiseArgs(SEXP el, SEXP rho)
{
    SEXP ans, h, tail;

    PROTECT(ans = tail = CONS(R_NilValue, R_NilValue));

    while(el != R_NilValue) {

	/* If we have a ... symbol, we look to see what it is bound to.
	 * If its binding is Null (i.e. zero length)
	 * we just ignore it and return the cdr with all its
	 * expressions promised; if it is bound to a ... list
	 * of promises, we repromise all the promises and then splice
	 * the list of resulting values into the return value.
	 * Anything else bound to a ... symbol is an error
	 */

	/* Is this double promise mechanism really needed? */

	if (CAR(el) == R_DotsSymbol) {
	    PROTECT(h = findVar(CAR(el), rho));
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    if (TYPEOF(CAR(h)) == PROMSXP || CAR(h) == R_MissingArg)
		      SETCDR(tail, CONS(CAR(h), R_NilValue));
                    else
		      SETCDR(tail, CONS(mkPROMISE(CAR(h), rho), R_NilValue));
		    tail = CDR(tail);
		    COPY_TAG(tail, h);
		    h = CDR(h);
		}
	    }
	    else if (h != R_MissingArg)
		error(_("'...' used in an incorrect context"));
	    UNPROTECT(1); /* h */
	}
	else if (CAR(el) == R_MissingArg) {
	    SETCDR(tail, CONS(R_MissingArg, R_NilValue));
	    tail = CDR(tail);
	    COPY_TAG(tail, el);
	}
	else {
	    SETCDR(tail, CONS(mkPROMISE(CAR(el), rho), R_NilValue));
	    tail = CDR(tail);
	    COPY_TAG(tail, el);
	}
	el = CDR(el);
    }
    UNPROTECT(1);
    ans = CDR(ans);
    DECREMENT_REFCNT(ans);
    return ans;
}


/* Check that each formal is a symbol */

/* used in coerce.c */
void attribute_hidden CheckFormals(SEXP ls)
{
    if (isList(ls)) {
	for (; ls != R_NilValue; ls = CDR(ls))
	    if (TYPEOF(TAG(ls)) != SYMSXP)
		goto err;
	return;
    }
 err:
    error(_("invalid formal argument list for \"function\""));
}


static SEXP VectorToPairListNamed(SEXP x)
{
    SEXP xptr, xnew, xnames;
    int i, len = 0, named;
    const void *vmax = vmaxget();

    PROTECT(x);
    PROTECT(xnames = getAttrib(x, R_NamesSymbol)); /* isn't this protected via x? */
    named = (xnames != R_NilValue);
    if(named)
	for (i = 0; i < length(x); i++)
	    if (CHAR(STRING_ELT(xnames, i))[0] != '\0') len++;

    if(len) {
	PROTECT(xnew = allocList(len));
	xptr = xnew;
	for (i = 0; i < length(x); i++) {
	    if (CHAR(STRING_ELT(xnames, i))[0] != '\0') {
		SETCAR(xptr, VECTOR_ELT(x, i));
		SET_TAG(xptr, installTrChar(STRING_ELT(xnames, i)));
		xptr = CDR(xptr);
	    }
	}
	UNPROTECT(1);
    } else xnew = allocList(0);
    UNPROTECT(2);
    vmaxset(vmax);
    return xnew;
}

#define simple_as_environment(arg) (IS_S4_OBJECT(arg) && (TYPEOF(arg) == S4SXP) ? R_getS4DataSlot(arg, ENVSXP) : R_NilValue)

/* "eval": Evaluate the first argument
   in the environment specified by the second argument. */

SEXP attribute_hidden do_eval(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP encl, x;
    volatile SEXP expr, env, tmp;

    int frame;
    RCNTXT cntxt;

    checkArity(op, args);
    expr = CAR(args);
    env = CADR(args);
    encl = CADDR(args);
    SEXPTYPE tEncl = TYPEOF(encl);
    if (isNull(encl)) {
	/* This is supposed to be defunct, but has been kept here
	   (and documented as such) */
	encl = R_BaseEnv;
    } else if ( !isEnvironment(encl) &&
		!isEnvironment((encl = simple_as_environment(encl))) ) {
	error(_("invalid '%s' argument of type '%s'"),
	      "enclos", type2char(tEncl));
    }
    if(IS_S4_OBJECT(env) && (TYPEOF(env) == S4SXP))
	env = R_getS4DataSlot(env, ANYSXP); /* usually an ENVSXP */
    switch(TYPEOF(env)) {
    case NILSXP:
	env = encl;     /* so eval(expr, NULL, encl) works */
	/* falls through */
    case ENVSXP:
	PROTECT(env);	/* so we can unprotect 2 at the end */
	break;
    case LISTSXP:
	/* This usage requires all the pairlist to be named */
	env = NewEnvironment(R_NilValue, duplicate(CADR(args)), encl);
	PROTECT(env);
	break;
    case VECSXP:
	/* PR#14035 */
	x = VectorToPairListNamed(CADR(args));
	for (SEXP xptr = x ; xptr != R_NilValue ; xptr = CDR(xptr))
	    ENSURE_NAMEDMAX(CAR(xptr));
	env = NewEnvironment(R_NilValue, x, encl);
	PROTECT(env);
	break;
    case INTSXP:
    case REALSXP:
	if (length(env) != 1)
	    error(_("numeric 'envir' arg not of length one"));
	frame = asInteger(env);
	if (frame == NA_INTEGER)
	    error(_("invalid '%s' argument of type '%s'"),
		  "envir", type2char(TYPEOF(env)));
	PROTECT(env = R_sysframe(frame, R_GlobalContext));
	break;
    default:
	error(_("invalid '%s' argument of type '%s'"),
	      "envir", type2char(TYPEOF(env)));
    }

    /* isLanguage include NILSXP, and that does not need to be
       evaluated
    if (isLanguage(expr) || isSymbol(expr) || isByteCode(expr)) { */
    if (TYPEOF(expr) == LANGSXP || TYPEOF(expr) == SYMSXP || isByteCode(expr)) {
	PROTECT(expr);
	begincontext(&cntxt, CTXT_RETURN, R_GlobalContext->call,
	             env, rho, args, op);
	if (!SETJMP(cntxt.cjmpbuf))
	    expr = eval(expr, env);
	else {
	    expr = R_ReturnedValue;
	    if (expr == R_RestartToken) {
		cntxt.callflag = CTXT_RETURN;  /* turn restart off */
		error(_("restarts not supported in 'eval'"));
	    }
	}
	UNPROTECT(1);
	PROTECT(expr);
	endcontext(&cntxt);
	UNPROTECT(1);
    }
    else if (TYPEOF(expr) == EXPRSXP) {
	SEXP srcrefs = getBlockSrcrefs(expr);
	PROTECT(expr);
	tmp = R_NilValue;
	begincontext(&cntxt, CTXT_RETURN, R_GlobalContext->call,
	             env, rho, args, op);
	if (!SETJMP(cntxt.cjmpbuf)) {
	    int n = LENGTH(expr);
	    for(int i = 0 ; i < n ; i++) {
		R_Srcref = getSrcref(srcrefs, i);
		tmp = eval(VECTOR_ELT(expr, i), env);
	    }
	} else {
	    tmp = R_ReturnedValue;
	    if (tmp == R_RestartToken) {
		cntxt.callflag = CTXT_RETURN;  /* turn restart off */
		error(_("restarts not supported in 'eval'"));
	    }
	}
	UNPROTECT(1);
	PROTECT(tmp);
	endcontext(&cntxt);
	UNPROTECT(1);
	expr = tmp;
    }
    else if( TYPEOF(expr) == PROMSXP ) {
	expr = eval(expr, rho);
    } /* else expr is returned unchanged */
    UNPROTECT(1);
    return expr;
}

/* This is a special .Internal */
SEXP attribute_hidden do_withVisible(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, nm, ret;

    checkArity(op, args);
    x = CAR(args);
    x = eval(x, rho);
    PROTECT(x);
    PROTECT(ret = allocVector(VECSXP, 2));
    PROTECT(nm = allocVector(STRSXP, 2));
    SET_STRING_ELT(nm, 0, mkChar("value"));
    SET_STRING_ELT(nm, 1, mkChar("visible"));
    SET_VECTOR_ELT(ret, 0, x);
    SET_VECTOR_ELT(ret, 1, ScalarLogical(R_Visible));
    setAttrib(ret, R_NamesSymbol, nm);
    UNPROTECT(3);
    return ret;
}

/* This is a special .Internal */
SEXP attribute_hidden do_recall(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    RCNTXT *cptr;
    SEXP s, ans ;
    cptr = R_GlobalContext;
    /* get the args supplied */
    while (cptr != NULL) {
	if (cptr->callflag == CTXT_RETURN && cptr->cloenv == rho)
	    break;
	cptr = cptr->nextcontext;
    }
    if (cptr != NULL) {
	args = cptr->promargs;
    }
    /* get the env recall was called from */
    s = R_GlobalContext->sysparent;
    while (cptr != NULL) {
	if (cptr->callflag == CTXT_RETURN && cptr->cloenv == s)
	    break;
	cptr = cptr->nextcontext;
    }
    if (cptr == NULL)
	error(_("'Recall' called from outside a closure"));

    /* If the function has been recorded in the context, use it
       otherwise search for it by name or evaluate the expression
       originally used to get it.
    */
    if (cptr->callfun != R_NilValue)
	PROTECT(s = cptr->callfun);
    else if( TYPEOF(CAR(cptr->call)) == SYMSXP)
	PROTECT(s = findFun(CAR(cptr->call), cptr->sysparent));
    else
	PROTECT(s = eval(CAR(cptr->call), cptr->sysparent));
    if (TYPEOF(s) != CLOSXP)
	error(_("'Recall' called from outside a closure"));
    ans = applyClosure(cptr->call, s, args, cptr->sysparent, R_NilValue);
    UNPROTECT(1);
    return ans;
}


static SEXP evalArgs(SEXP el, SEXP rho, int dropmissing, SEXP call, int n)
{
    if(dropmissing) return evalList(el, rho, call, n);
    else return evalListKeepMissing(el, rho);
}


/* A version of DispatchOrEval that checks for possible S4 methods for
 * any argument, not just the first.  Used in the code for `c()` in do_c()
 * and previously used in the code for `[` in do_subset.
 * Differs in that all arguments are evaluated
 * immediately, rather than after the call to R_possible_dispatch.
 */
attribute_hidden
int DispatchAnyOrEval(SEXP call, SEXP op, const char *generic, SEXP args,
		      SEXP rho, SEXP *ans, int dropmissing, int argsevald)
{
    if(R_has_methods(op)) {
	SEXP argValue, el,  value;
	/* Rboolean hasS4 = FALSE; */
	int nprotect = 0, dispatch;
	if(!argsevald) {
	    PROTECT(argValue = evalArgs(args, rho, dropmissing, call, 0));
	    nprotect++;
	    argsevald = TRUE;
	}
	else argValue = args;
	for(el = argValue; el != R_NilValue; el = CDR(el)) {
	    if(IS_S4_OBJECT(CAR(el))) {
		value = R_possible_dispatch(call, op, argValue, rho, TRUE);
		if(value) {
		    *ans = value;
		    UNPROTECT(nprotect);
		    return 1;
		}
		else break;
	    }
	}
	 /* else, use the regular DispatchOrEval, but now with evaluated args */
	dispatch = DispatchOrEval(call, op, generic, argValue, rho, ans, dropmissing, argsevald);
	UNPROTECT(nprotect);
	return dispatch;
    }
    return DispatchOrEval(call, op, generic, args, rho, ans, dropmissing, argsevald);
}


/* DispatchOrEval is used in internal functions which dispatch to
 * object methods (e.g. "[" or "[[").  The code either builds promises
 * and dispatches to the appropriate method, or it evaluates the
 * (unevaluated) arguments it comes in with and returns them so that
 * the generic built-in C code can continue.

 * To call this an ugly hack would be to insult all existing ugly hacks
 * at large in the world.
 */
attribute_hidden
int DispatchOrEval(SEXP call, SEXP op, const char *generic, SEXP args,
		   SEXP rho, SEXP *ans, int dropmissing, int argsevald)
{
/* DispatchOrEval is called very frequently, most often in cases where
   no dispatching is needed and the isObject or the string-based
   pre-test fail.  To avoid degrading performance it is therefore
   necessary to avoid creating promises in these cases.  The pre-test
   does require that we look at the first argument, so that needs to
   be evaluated.  The complicating factor is that the first argument
   might come in with a "..." and that there might be other arguments
   in the "..." as well.  LT */

    SEXP x = R_NilValue;
    int dots = FALSE, nprotect = 0;;

    if( argsevald )
	{PROTECT(x = CAR(args)); nprotect++;}
    else {
	/* Find the object to dispatch on, dropping any leading
	   ... arguments with missing or empty values.  If there are no
	   arguments, R_NilValue is used. */
	for (; args != R_NilValue; args = CDR(args)) {
	    if (CAR(args) == R_DotsSymbol) {
		SEXP h = findVar(R_DotsSymbol, rho);
		if (TYPEOF(h) == DOTSXP) {
#ifdef DODO
		    /**** any self-evaluating value should be OK; this
			  is used in byte compiled code. LT */
		    /* just a consistency check */
		    if (TYPEOF(CAR(h)) != PROMSXP)
			error(_("value in '...' is not a promise"));
#endif
		    dots = TRUE;
		    x = eval(CAR(h), rho);
		    break;
		}
		else if (h != R_NilValue && h != R_MissingArg)
		    error(_("'...' used in an incorrect context"));
	    }
	    else {
		dots = FALSE;
		x = eval(CAR(args), rho);
		break;
	    }
	}
	PROTECT(x); nprotect++;
    }
	/* try to dispatch on the object */
    if( isObject(x) ) {
	char *pt;
	/* Try for formal method. */
	if(IS_S4_OBJECT(x) && R_has_methods(op)) {
	    SEXP value, argValue;
	    /* create a promise to pass down to applyClosure  */
	    if(!argsevald) {
		argValue = promiseArgs(args, rho);
		SET_PRVALUE(CAR(argValue), x);
	    } else argValue = args;
	    PROTECT(argValue); nprotect++;
	    /* This means S4 dispatch */
	    value = R_possible_dispatch(call, op, argValue, rho, TRUE);
	    if(value) {
		*ans = value;
		UNPROTECT(nprotect);
		return 1;
	    }
	    else {
		/* go on, with the evaluated args.  Not guaranteed to have
		   the same semantics as if the arguments were not
		   evaluated, in special cases (e.g., arg values that are
		   LANGSXP).
		   The use of the promiseArgs is supposed to prevent
		   multiple evaluation after the call to possible_dispatch.
		*/
		if (dots)
		    PROTECT(argValue = evalArgs(argValue, rho, dropmissing,
						call, 0));
		else {
		    PROTECT(argValue = CONS_NR(x, evalArgs(CDR(argValue), rho,
							   dropmissing, call, 1)));
		    SET_TAG(argValue, CreateTag(TAG(args)));
		}
		nprotect++;
		args = argValue;
		argsevald = 1;
	    }
	}
	if (TYPEOF(CAR(call)) == SYMSXP)
	    pt = Rf_strrchr(CHAR(PRINTNAME(CAR(call))), '.');
	else
	    pt = NULL;

	if (pt == NULL || strcmp(pt,".default")) {
	    RCNTXT cntxt;
	    SEXP pargs, rho1;
	    PROTECT(pargs = promiseArgs(args, rho)); nprotect++;
	    /* The context set up here is needed because of the way
	       usemethod() is written.  DispatchGroup() repeats some
	       internal usemethod() code and avoids the need for a
	       context; perhaps the usemethod() code should be
	       refactored so the contexts around the usemethod() calls
	       in this file can be removed.

	       Using rho for current and calling environment can be
	       confusing for things like sys.parent() calls captured
	       in promises (Gabor G had an example of this).  Also,
	       since the context is established without a SETJMP using
	       an R-accessible environment allows a segfault to be
	       triggered (by something very obscure, but still).
	       Hence here and in the other usemethod() uses below a
	       new environment rho1 is created and used.  LT */
	    PROTECT(rho1 = NewEnvironment(R_NilValue, R_NilValue, rho)); nprotect++;
	    SET_PRVALUE(CAR(pargs), x);
	    begincontext(&cntxt, CTXT_RETURN, call, rho1, rho, pargs, op);
	    if(usemethod(generic, x, call, pargs, rho1, rho, R_BaseEnv, ans))
	    {
		endcontext(&cntxt);
		UNPROTECT(nprotect);
#ifdef ADJUST_ENVIR_REFCNTS
		R_CleanupEnvir(rho1, *ans);
		unpromiseArgs(pargs);
#endif
		return 1;
	    }
	    endcontext(&cntxt);
#ifdef ADJUST_ENVIR_REFCNTS
	    R_CleanupEnvir(rho1, R_NilValue);
	    unpromiseArgs(pargs);
#endif
	}
    }
    if(!argsevald) {
	if (dots)
	    /* The first call argument was ... and may contain more than the
	       object, so it needs to be evaluated here.  The object should be
	       in a promise, so evaluating it again should be no problem. */
	    *ans = evalArgs(args, rho, dropmissing, call, 0);
	else {
	    PROTECT(*ans = CONS_NR(x, evalArgs(CDR(args), rho, dropmissing, call, 1)));
	    SET_TAG(*ans, CreateTag(TAG(args)));
	    UNPROTECT(1);
	}
    }
    else *ans = args;
    UNPROTECT(nprotect);
    return 0;
}

static R_INLINE void updateObjFromS4Slot(SEXP objSlot, const char *className) {
    SEXP obj = CAR(objSlot);

    if(IS_S4_OBJECT(obj) && isBasicClass(className)) {
	/* This and the similar test below implement the strategy
	 for S3 methods selected for S4 objects.  See ?Methods */
	if(NAMED(obj)) ENSURE_NAMEDMAX(obj);
	obj = R_getS4DataSlot(obj, S4SXP); /* the .S3Class obj. or NULL*/
	if(obj != R_NilValue) /* use the S3Part as the inherited object */
	    SETCAR(objSlot, obj);
    }
}

/* gr needs to be protected on return from this function */
static void findmethod(SEXP Class, const char *group, const char *generic,
		       SEXP *sxp,  SEXP *gr, SEXP *meth, int *which,
		       SEXP objSlot, SEXP rho)
{
    int len, whichclass;
    const void *vmax = vmaxget();

    len = length(Class);

    /* Need to interleave looking for group and generic methods
       e.g. if class(x) is c("foo", "bar)" then x > 3 should invoke
       "Ops.foo" rather than ">.bar"
    */
    for (whichclass = 0 ; whichclass < len ; whichclass++) {
	const char *ss = translateChar(STRING_ELT(Class, whichclass));
	*meth = installS3Signature(generic, ss);
	*sxp = R_LookupMethod(*meth, rho, rho, R_BaseEnv);
	if (isFunction(*sxp)) {
	    *gr = R_BlankScalarString;
	    if (whichclass > 0) updateObjFromS4Slot(objSlot, ss);
	    break;
	}
	*meth = installS3Signature(group, ss);
	*sxp = R_LookupMethod(*meth, rho, rho, R_BaseEnv);
	if (isFunction(*sxp)) {
	    *gr = mkString(group);
	    if (whichclass > 0) updateObjFromS4Slot(objSlot, ss);
	    break;
	}
    }
    vmaxset(vmax);
    *which = whichclass;
}

static SEXP classForGroupDispatch(SEXP obj) {

    return IS_S4_OBJECT(obj) ? R_data_class2(obj)
	    : getAttrib(obj, R_ClassSymbol);
}

attribute_hidden
int DispatchGroup(const char* group, SEXP call, SEXP op, SEXP args, SEXP rho,
		  SEXP *ans)
{
    int i, nargs, lwhich, rwhich;
    SEXP lclass, s, t, m, lmeth, lsxp, lgr, newvars;
    SEXP rclass, rmeth, rgr, rsxp, value;
    char *generic;
    Rboolean useS4 = TRUE, isOps = FALSE;

    /* pre-test to avoid string computations when there is nothing to
       dispatch on because either there is only one argument and it
       isn't an object or there are two or more arguments but neither
       of the first two is an object -- both of these cases would be
       rejected by the code following the string examination code
       below */
    if (args != R_NilValue && ! isObject(CAR(args)) &&
	(CDR(args) == R_NilValue || ! isObject(CADR(args))))
	return 0;

    isOps = strcmp(group, "Ops") == 0;

    /* try for formal method */
    if(length(args) == 1 && !IS_S4_OBJECT(CAR(args))) useS4 = FALSE;
    if(length(args) == 2 &&
       !IS_S4_OBJECT(CAR(args)) && !IS_S4_OBJECT(CADR(args))) useS4 = FALSE;
    if(useS4) {
	/* Remove argument names to ensure positional matching */
	if(isOps)
	    for(s = args; s != R_NilValue; s = CDR(s)) SET_TAG(s, R_NilValue);
	if(R_has_methods(op) &&
	   (value = R_possible_dispatch(call, op, args, rho, FALSE))) {
	       *ans = value;
	       return 1;
	}
	/* else go on to look for S3 methods */
    }

    /* check whether we are processing the default method */
    if ( isSymbol(CAR(call)) ) {
	const char *cstr = strchr(CHAR(PRINTNAME(CAR(call))), '.');
	if (cstr && !strcmp(cstr + 1, "default"))
	    return 0;
    }

    if(isOps)
	nargs = length(args);
    else
	nargs = 1;

    if( nargs == 1 && !isObject(CAR(args)) )
	return 0;

    generic = PRIMNAME(op);

    PROTECT(lclass = classForGroupDispatch(CAR(args)));

    if( nargs == 2 )
	rclass = classForGroupDispatch(CADR(args));
    else
	rclass = R_NilValue;

    PROTECT(rclass);
    lsxp = R_NilValue; lgr = R_NilValue; lmeth = R_NilValue;
    rsxp = R_NilValue; rgr = R_NilValue; rmeth = R_NilValue;

    findmethod(lclass, group, generic, &lsxp, &lgr, &lmeth, &lwhich,
	       args, rho);
    PROTECT(lgr);

    if( nargs == 2 )
	findmethod(rclass, group, generic, &rsxp, &rgr, &rmeth,
		   &rwhich, CDR(args), rho);
    else
	rwhich = 0;

    PROTECT(rgr);

    if( !isFunction(lsxp) && !isFunction(rsxp) ) {
	UNPROTECT(4);
	return 0; /* no generic or group method so use default */
    }

    if( lsxp != rsxp ) {
	if ( isFunction(lsxp) && isFunction(rsxp) ) {
	    /* special-case some methods involving difftime */
	    const char *lname = CHAR(PRINTNAME(lmeth)),
		*rname = CHAR(PRINTNAME(rmeth));
	    if( streql(rname, "Ops.difftime") &&
		(streql(lname, "+.POSIXt") || streql(lname, "-.POSIXt") ||
		 streql(lname, "+.Date") || streql(lname, "-.Date")) )
		rsxp = R_NilValue;
	    else if (streql(lname, "Ops.difftime") &&
		     (streql(rname, "+.POSIXt") || streql(rname, "+.Date")) )
		lsxp = R_NilValue;
	    else {
		warning(_("Incompatible methods (\"%s\", \"%s\") for \"%s\""),
			lname, rname, generic);
		UNPROTECT(4);
		return 0;
	    }
	}
	/* if the right hand side is the one */
	if( !isFunction(lsxp) ) { /* copy over the righthand stuff */
	    lsxp = rsxp;
	    lmeth = rmeth;
	    lgr = rgr;
	    lclass = rclass;
	    lwhich = rwhich;
	}
    }

    /* we either have a group method or a class method */

    PROTECT(m = allocVector(STRSXP,nargs));
    const void *vmax = vmaxget();
    s = args;
    const char *dispatchClassName = translateChar(STRING_ELT(lclass, lwhich));
    for (i = 0 ; i < nargs ; i++) {
	t = classForGroupDispatch(CAR(s));
	if (isString(t) && (stringPositionTr(t, dispatchClassName) >= 0))
	    SET_STRING_ELT(m, i, PRINTNAME(lmeth));
	else
	    SET_STRING_ELT(m, i, R_BlankString);
	s = CDR(s);
    }
    vmaxset(vmax);

    newvars = PROTECT(createS3Vars(
	PROTECT(mkString(generic)),
	lgr,
	PROTECT(stringSuffix(lclass, lwhich)),
	m,
	rho,
	R_BaseEnv
    ));

    PROTECT(t = LCONS(lmeth, CDR(call)));

    /* the arguments have been evaluated; since we are passing them */
    /* out to a closure we need to wrap them in promises so that */
    /* they get duplicated and things like missing/substitute work. */

    PROTECT(s = promiseArgs(CDR(call), rho));
    if (length(s) != length(args))
	error(_("dispatch error in group dispatch"));
    for (m = s ; m != R_NilValue ; m = CDR(m), args = CDR(args) ) {
	SET_PRVALUE(CAR(m), CAR(args));
	/* ensure positional matching for operators */
	if(isOps) SET_TAG(m, R_NilValue);
    }

    *ans = applyClosure(t, lsxp, s, rho, newvars);
#ifdef ADJUST_ENVIR_REFCNTS
    unpromiseArgs(s);
#endif
    UNPROTECT(10);
    return 1;
}

/* start of bytecode section */
static int R_bcVersion = 10;
static int R_bcMinVersion = 9;

static SEXP R_AddSym = NULL;
static SEXP R_SubSym = NULL;
static SEXP R_MulSym = NULL;
static SEXP R_DivSym = NULL;
static SEXP R_ExptSym = NULL;
static SEXP R_SqrtSym = NULL;
static SEXP R_ExpSym = NULL;
static SEXP R_EqSym = NULL;
static SEXP R_NeSym = NULL;
static SEXP R_LtSym = NULL;
static SEXP R_LeSym = NULL;
static SEXP R_GeSym = NULL;
static SEXP R_GtSym = NULL;
static SEXP R_AndSym = NULL;
static SEXP R_OrSym = NULL;
static SEXP R_NotSym = NULL;
static SEXP R_CSym = NULL;
static SEXP R_LogSym = NULL;
static SEXP R_DotInternalSym = NULL;
static SEXP R_DotExternalSym = NULL;
static SEXP R_DotExternal2Sym = NULL;
static SEXP R_DotExternalgraphicsSym = NULL;
static SEXP R_DotCallSym = NULL;
static SEXP R_DotCallgraphicsSym = NULL;
static SEXP R_DotFortranSym = NULL;
static SEXP R_DotCSym = NULL;

/* R_ConstantsRegistry allows runtime detection of modification of compiler
   constants. It is a linked list of weak references. Each weak reference
   refers to a byte-code object (BCODESXPs) as key and to a deep copy of the
   object's constants as value. The head of the list has a nil payload
   instead of a weak reference, stays in the list forever, and is a GC root.*/
static SEXP R_ConstantsRegistry = NULL;

#if defined(__GNUC__) && ! defined(BC_PROFILING) && (! defined(NO_THREADED_CODE))
# define THREADED_CODE
#endif

attribute_hidden
void R_initialize_bcode(void)
{
  R_AddSym = install("+");
  R_SubSym = install("-");
  R_MulSym = install("*");
  R_DivSym = install("/");
  R_ExptSym = install("^");
  R_SqrtSym = install("sqrt");
  R_ExpSym = install("exp");
  R_EqSym = install("==");
  R_NeSym = install("!=");
  R_LtSym = install("<");
  R_LeSym = install("<=");
  R_GeSym = install(">=");
  R_GtSym = install(">");
  R_AndSym = install("&");
  R_OrSym = install("|");
  R_NotSym = install("!");
  R_CSym = install("c");
  R_LogSym = install("log");
  R_DotInternalSym = install(".Internal");
  R_DotExternalSym = install(".External");
  R_DotExternal2Sym = install(".External2");
  R_DotExternalgraphicsSym = install(".External.graphics");
  R_DotCallSym = install(".Call");
  R_DotCallgraphicsSym = install(".Call.graphics");
  R_DotFortranSym = install(".Fortran");
  R_DotCSym = install(".C");

#ifdef THREADED_CODE
  bcEval(NULL, NULL, FALSE);
#endif

  /* the first constants record always stays in place for protection */
  R_ConstantsRegistry = allocVector(VECSXP, 2);
  R_PreserveObject(R_ConstantsRegistry);
  SET_VECTOR_ELT(R_ConstantsRegistry, 0, R_NilValue);
  SET_VECTOR_ELT(R_ConstantsRegistry, 1, R_NilValue);
}

enum {
  BCMISMATCH_OP,
  RETURN_OP,
  GOTO_OP,
  BRIFNOT_OP,
  POP_OP,
  DUP_OP,
  PRINTVALUE_OP,
  STARTLOOPCNTXT_OP,
  ENDLOOPCNTXT_OP,
  DOLOOPNEXT_OP,
  DOLOOPBREAK_OP,
  STARTFOR_OP,
  STEPFOR_OP,
  ENDFOR_OP,
  SETLOOPVAL_OP,
  INVISIBLE_OP,
  LDCONST_OP,
  LDNULL_OP,
  LDTRUE_OP,
  LDFALSE_OP,
  GETVAR_OP,
  DDVAL_OP,
  SETVAR_OP,
  GETFUN_OP,
  GETGLOBFUN_OP,
  GETSYMFUN_OP,
  GETBUILTIN_OP,
  GETINTLBUILTIN_OP,
  CHECKFUN_OP,
  MAKEPROM_OP,
  DOMISSING_OP,
  SETTAG_OP,
  DODOTS_OP,
  PUSHARG_OP,
  PUSHCONSTARG_OP,
  PUSHNULLARG_OP,
  PUSHTRUEARG_OP,
  PUSHFALSEARG_OP,
  CALL_OP,
  CALLBUILTIN_OP,
  CALLSPECIAL_OP,
  MAKECLOSURE_OP,
  UMINUS_OP,
  UPLUS_OP,
  ADD_OP,
  SUB_OP,
  MUL_OP,
  DIV_OP,
  EXPT_OP,
  SQRT_OP,
  EXP_OP,
  EQ_OP,
  NE_OP,
  LT_OP,
  LE_OP,
  GE_OP,
  GT_OP,
  AND_OP,
  OR_OP,
  NOT_OP,
  DOTSERR_OP,
  STARTASSIGN_OP,
  ENDASSIGN_OP,
  STARTSUBSET_OP,
  DFLTSUBSET_OP,
  STARTSUBASSIGN_OP,
  DFLTSUBASSIGN_OP,
  STARTC_OP,
  DFLTC_OP,
  STARTSUBSET2_OP,
  DFLTSUBSET2_OP,
  STARTSUBASSIGN2_OP,
  DFLTSUBASSIGN2_OP,
  DOLLAR_OP,
  DOLLARGETS_OP,
  ISNULL_OP,
  ISLOGICAL_OP,
  ISINTEGER_OP,
  ISDOUBLE_OP,
  ISCOMPLEX_OP,
  ISCHARACTER_OP,
  ISSYMBOL_OP,
  ISOBJECT_OP,
  ISNUMERIC_OP,
  VECSUBSET_OP,
  MATSUBSET_OP,
  VECSUBASSIGN_OP,
  MATSUBASSIGN_OP,
  AND1ST_OP,
  AND2ND_OP,
  OR1ST_OP,
  OR2ND_OP,
  GETVAR_MISSOK_OP,
  DDVAL_MISSOK_OP,
  VISIBLE_OP,
  SETVAR2_OP,
  STARTASSIGN2_OP,
  ENDASSIGN2_OP,
  SETTER_CALL_OP,
  GETTER_CALL_OP,
  SWAP_OP,
  DUP2ND_OP,
  SWITCH_OP,
  RETURNJMP_OP,
  STARTSUBSET_N_OP,
  STARTSUBASSIGN_N_OP,
  VECSUBSET2_OP,
  MATSUBSET2_OP,
  VECSUBASSIGN2_OP,
  MATSUBASSIGN2_OP,
  STARTSUBSET2_N_OP,
  STARTSUBASSIGN2_N_OP,
  SUBSET_N_OP,
  SUBSET2_N_OP,
  SUBASSIGN_N_OP,
  SUBASSIGN2_N_OP,
  LOG_OP,
  LOGBASE_OP,
  MATH1_OP,
  DOTCALL_OP,
  COLON_OP,
  SEQALONG_OP,
  SEQLEN_OP,
  BASEGUARD_OP,
  OPCOUNT
};


SEXP R_unary(SEXP, SEXP, SEXP);
SEXP R_binary(SEXP, SEXP, SEXP, SEXP);
SEXP do_math1(SEXP, SEXP, SEXP, SEXP);
SEXP do_relop_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_logic(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassign_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_c_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset2_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassign2_dflt(SEXP, SEXP, SEXP, SEXP);

static SEXP seq_int(int n1, int n2)
{
#define USE_ALTREP_COMPACT_INTRANGE
#ifdef USE_ALTREP_COMPACT_INTRANGE
    return R_compact_intrange(n1, n2);
#else
    int n = n1 <= n2 ? n2 - n1 + 1 : n1 - n2 + 1;
    SEXP ans = allocVector(INTSXP, n);
    int *data = INTEGER(ans);
    if (n1 <= n2)
	for (int i = 0; i < n; i++)
	    data[i] = n1 + i;
    else
	for (int i = 0; i < n; i++)
	    data[i] = n1 - i;
    return ans;
#endif
}

#ifdef TYPED_STACK
# define COMPACT_INTSEQ
# ifdef COMPACT_INTSEQ
#  define INTSEQSXP 9999
# endif
#define CACHE_SCALARS
static R_INLINE SEXP GETSTACK_PTR_TAG(R_bcstack_t *s)
{
    /* no error checking since only called with tag != 0 */
    SEXP value;
    switch (s->tag) {
    case REALSXP:
#ifdef CACHE_SCALARS
	if (R_CachedScalarReal != NULL) {
	    value = R_CachedScalarReal;
	    R_CachedScalarReal = NULL;
	    SET_SCALAR_DVAL(value, s->u.dval);
	}
	else
#endif
	value = ScalarReal(s->u.dval);
	break;
    case INTSXP:
#ifdef CACHE_SCALARS
	if (R_CachedScalarInteger != NULL) {
	    value = R_CachedScalarInteger;
	    R_CachedScalarInteger = NULL;
	    SET_SCALAR_IVAL(value, s->u.ival);
	}
	else
#endif
	value = ScalarInteger(s->u.ival);
	break;
    case LGLSXP:
	value = ScalarLogical(s->u.ival);
	break;
#ifdef COMPACT_INTSEQ
    case INTSEQSXP:
	{
	    int *seqinfo = INTEGER(s->u.sxpval);
	    value = seq_int(seqinfo[0], seqinfo[1]);
	}
	break;
#endif
    default: /* not reached */
	value = NULL;
    }
    s->tag = 0;
    s->u.sxpval = value;
    return value;
}
#define GETSTACK_PTR(s) ((s)->tag ? GETSTACK_PTR_TAG(s) : (s)->u.sxpval)

#define GETSTACK_SXPVAL_PTR(s) ((s)->u.sxpval)

#define GETSTACK_IVAL_PTR(s) ((s)->u.ival)

#define SETSTACK_PTR(s, v) do { \
    SEXP __v__ = (v); \
    (s)->tag = 0; \
    (s)->u.sxpval = __v__; \
} while (0)

#define SETSTACK_REAL_PTR(s, v) do { \
    double __v__ = (v); \
    (s)->tag = REALSXP; \
    (s)->u.dval = __v__; \
} while (0)

#define SETSTACK_INTEGER_PTR(s, v) do { \
    int __v__ = (v); \
    (s)->tag = INTSXP; \
    (s)->u.ival = __v__; \
} while (0)

#define SETSTACK_LOGICAL_PTR(s, v) do {		\
	int __v__ = (v);			\
	(s)->tag = LGLSXP;			\
	if (__v__ == NA_LOGICAL)		\
	    (s)->u.ival = NA_LOGICAL;		\
	else					\
	    (s)->u.ival = __v__ ? TRUE : FALSE;	\
    } while (0)

#define IS_STACKVAL_BOXED(idx)	(R_BCNodeStackTop[idx].tag == 0)
#else
#define GETSTACK_PTR(s) (*(s))

#define GETSTACK_SXPVAL_PTR(s) (*(s))

#define GETSTACK_IVAL_PTR(s) INTEGER(*(s))[0]

#define SETSTACK_PTR(s, v) do { \
    SEXP __v__ = (v); \
    *(s) = __v__; \
} while (0)

#define SETSTACK_REAL_PTR(s, v) SETSTACK_PTR(s, ScalarReal(v))
#define SETSTACK_INTEGER_PTR(s, v) SETSTACK_PTR(s, ScalarInteger(v))
#define SETSTACK_LOGICAL_PTR(s, v) SETSTACK_PTR(s, ScalarLogical(v))

#define IS_STACKVAL_BOXED(idx)	(TRUE)
#endif

#if defined(TYPED_STACK) && defined(COMPACT_INTSEQ)
#define SETSTACK_INTSEQ(idx, rn1, rn2) do {	\
	SEXP info = allocVector(INTSXP, 2);	\
	INTEGER(info)[0] = (int) rn1;		\
	INTEGER(info)[1] = (int) rn2;		\
	R_BCNodeStackTop[idx].u.sxpval = info;	\
	R_BCNodeStackTop[idx].tag = INTSEQSXP;	\
    } while (0)
#else
#define SETSTACK_INTSEQ(idx, rn1, rn2) \
    SETSTACK(idx, seq_int((int) rn1, (int) rn2))
#endif

#define GETSTACK_SXPVAL(i) GETSTACK_SXPVAL_PTR(R_BCNodeStackTop + (i))

#define GETSTACK(i) GETSTACK_PTR(R_BCNodeStackTop + (i))

#define SETSTACK(i, v) SETSTACK_PTR(R_BCNodeStackTop + (i), v)

#define SETSTACK_REAL(i, v) SETSTACK_REAL_PTR(R_BCNodeStackTop + (i), v)

#define SETSTACK_INTEGER(i, v) SETSTACK_INTEGER_PTR(R_BCNodeStackTop + (i), v)

#define SETSTACK_LOGICAL(i, v) SETSTACK_LOGICAL_PTR(R_BCNodeStackTop + (i), v)


/* The next two macros will allow reuse a scalar box, if provided. The
   box is assumed to be of the correct type and size and to have no
   attributes. */
#ifdef CACHE_SCALARS
#define SETSTACK_REAL_EX(idx, dval, ans) do {		\
	SEXP __ans__ = (ans);				\
	if (__ans__ && R_CachedScalarReal == NULL)	\
	    R_CachedScalarReal = __ans__;		\
	SETSTACK_REAL(idx, dval);			\
    } while (0)

#define SETSTACK_INTEGER_EX(idx, ival, ans) do {	\
	SEXP __ans__ = (ans);				\
	if (__ans__ && R_CachedScalarInteger == NULL)	\
	    R_CachedScalarInteger = __ans__;		\
	SETSTACK_INTEGER(idx, ival);			\
    } while (0)
#else
#define SETSTACK_REAL_EX(idx, dval, ans) do { \
	if (ans) {			      \
	    SET_SCALAR_DVAL(ans, dval);	      \
	    SETSTACK(idx, ans);		      \
	}				      \
	else SETSTACK_REAL(idx, dval);	      \
    } while (0)

#define SETSTACK_INTEGER_EX(idx, ival, ans) do { \
	if (ans) {				 \
	    INTEGER(ans)[0] = ival;		 \
	    SETSTACK(idx, ans);			 \
	}					 \
	else SETSTACK_INTEGER(idx, ival);	 \
    } while (0)
#endif

typedef union { double dval; int ival; } scalar_value_t;

/* bcStackScalar() checks whether the object in the specified stack
   location is a simple real, integer, or logical scalar (i.e. length
   one and no attributes.  If so, the type is returned as the function
   value and the value is returned in the structure pointed to by the
   second argument; if not, then zero is returned as the function
   value. The boxed value can be returned through a pointer argument
   if it is suitable for re-use. */
static R_INLINE int bcStackScalarEx(R_bcstack_t *s, scalar_value_t *v,
				    SEXP *pv)
{
#ifdef TYPED_STACK
    int tag = s->tag;

    if (tag)
	switch(tag) {
	case REALSXP: v->dval = s->u.dval; return tag;
	case INTSXP: v->ival = s->u.ival; return tag;
	case LGLSXP: v->ival = s->u.ival; return tag;
	}
#endif
    SEXP x = GETSTACK_SXPVAL_PTR(s);
    if (IS_SIMPLE_SCALAR(x, REALSXP)) {
#ifndef NO_SAVE_ALLOC
	if (pv && NO_REFERENCES(x)) *pv = x;
#endif
	v->dval = SCALAR_DVAL(x);
	return REALSXP;
    }
    else if (IS_SIMPLE_SCALAR(x, INTSXP)) {
#ifndef NO_SAVE_ALLOC
	if (pv && NO_REFERENCES(x)) *pv = x;
#endif
	v->ival = SCALAR_IVAL(x);
	return INTSXP;
    }
    else if (IS_SIMPLE_SCALAR(x, LGLSXP)) {
	v->ival = SCALAR_LVAL(x);
	return LGLSXP;
    }
    else return 0;
}

#define bcStackScalar(s, v) bcStackScalarEx(s, v, NULL)

#define INTEGER_TO_LOGICAL(x) \
    ((x) == NA_INTEGER ? NA_LOGICAL : (x) ? TRUE : FALSE)
#define INTEGER_TO_REAL(x) ((x) == NA_INTEGER ? NA_REAL : (x))
#define LOGICAL_TO_REAL(x) ((x) == NA_LOGICAL ? NA_REAL : (x))

static R_INLINE int bcStackScalarRealEx(R_bcstack_t *s, scalar_value_t *px,
					SEXP *pv)
{
    int typex = bcStackScalarEx(s, px, pv);
    if (typex == INTSXP) {
	typex = REALSXP;
	px->dval = INTEGER_TO_REAL(px->ival);
	if (pv) *pv = NULL;
    }
    return typex;
}

#define DO_FAST_RELOP2(op,a,b) do { \
    SKIP_OP(); \
    SETSTACK_LOGICAL(-2, ((a) op (b)) ? TRUE : FALSE);	\
    R_BCNodeStackTop--; \
    R_Visible = TRUE; \
    NEXT(); \
} while (0)

# define FastRelop2(op,opval,opsym) do { \
    scalar_value_t vx; \
    scalar_value_t vy; \
    int typex = bcStackScalar(R_BCNodeStackTop - 2, &vx); \
    int typey = bcStackScalar(R_BCNodeStackTop - 1, &vy); \
    if (typex == REALSXP && ! ISNAN(vx.dval)) { \
	if (typey == REALSXP && ! ISNAN(vy.dval)) \
	    DO_FAST_RELOP2(op, vx.dval, vy.dval); \
	else if (typey == INTSXP && vy.ival != NA_INTEGER) \
	    DO_FAST_RELOP2(op, vx.dval, vy.ival); \
    } \
    else if (typex == INTSXP && vx.ival != NA_INTEGER) { \
	if (typey == REALSXP && ! ISNAN(vy.dval)) \
	    DO_FAST_RELOP2(op, vx.ival, vy.dval); \
	else if (typey == INTSXP && vy.ival != NA_INTEGER) { \
	    DO_FAST_RELOP2(op, vx.ival, vy.ival); \
	} \
    } \
    Relop2(opval, opsym); \
} while (0)

static R_INLINE SEXP getPrimitive(SEXP symbol, SEXPTYPE type)
{
    SEXP value = SYMVALUE(symbol);
    if (TYPEOF(value) == PROMSXP) {
	value = forcePromise(value);
	ENSURE_NAMEDMAX(value);
    }
    if (TYPEOF(value) != type) {
	/* probably means a package redefined the base function so
	   try to get the real thing from the internal table of
	   primitives */
	value = R_Primitive(CHAR(PRINTNAME(symbol)));
	if (TYPEOF(value) != type)
	    /* if that doesn't work we signal an error */
	    error(_("\"%s\" is not a %s function"),
		  CHAR(PRINTNAME(symbol)),
		  type == BUILTINSXP ? "BUILTIN" : "SPECIAL");
    }
    return value;
}

static SEXP cmp_relop(SEXP call, int opval, SEXP opsym, SEXP x, SEXP y,
		      SEXP rho)
{
    SEXP op = getPrimitive(opsym, BUILTINSXP);
    if (isObject(x) || isObject(y)) {
	SEXP args, ans;
	args = CONS_NR(x, CONS_NR(y, R_NilValue));
	PROTECT(args);
	if (DispatchGroup("Ops", call, op, args, rho, &ans)) {
	    UNPROTECT(1);
	    return ans;
	}
	UNPROTECT(1);
    }
    return do_relop_dflt(call, op, x, y);
}

static SEXP cmp_arith1(SEXP call, SEXP opsym, SEXP x, SEXP rho)
{
    SEXP op = getPrimitive(opsym, BUILTINSXP);
    if (isObject(x)) {
	SEXP args, ans;
	args = CONS_NR(x, R_NilValue);
	PROTECT(args);
	if (DispatchGroup("Ops", call, op, args, rho, &ans)) {
	    UNPROTECT(1);
	    return ans;
	}
	UNPROTECT(1);
    }
    return R_unary(call, op, x);
}

static SEXP cmp_arith2(SEXP call, int opval, SEXP opsym, SEXP x, SEXP y,
		       SEXP rho)
{
    SEXP op = getPrimitive(opsym, BUILTINSXP);
    if (isObject(x) || isObject(y)) {
	SEXP args, ans;
	args = CONS_NR(x, CONS_NR(y, R_NilValue));
	PROTECT(args);
	if (DispatchGroup("Ops", call, op, args, rho, &ans)) {
	    UNPROTECT(1);
	    return ans;
	}
	UNPROTECT(1);
    }
    return R_binary(call, op, x, y);
}

#define Builtin1(do_fun,which,rho) do { \
  SEXP call = VECTOR_ELT(constants, GETOP()); \
  SETSTACK(-1, CONS_NR(GETSTACK(-1), R_NilValue));		     \
  SETSTACK(-1, do_fun(call, getPrimitive(which, BUILTINSXP), \
		      GETSTACK(-1), rho));		     \
  R_Visible = TRUE;					     \
  NEXT(); \
} while(0)

#define Builtin2(do_fun,which,rho) do {		     \
  SEXP stack1 = GETSTACK(-1); \
  SEXP stack2 = GETSTACK(-2); \
  SEXP call = VECTOR_ELT(constants, GETOP()); \
  SEXP tmp = CONS_NR(stack1, R_NilValue); \
  SETSTACK(-2, CONS_NR(stack2, tmp));     \
  R_BCNodeStackTop--; \
  SETSTACK(-1, do_fun(call, getPrimitive(which, BUILTINSXP),	\
		      GETSTACK(-1), rho));			\
  R_Visible = TRUE;						\
  NEXT(); \
} while(0)

#define NewBuiltin2(do_fun,opval,opsym,rho) do {	\
  SEXP call = VECTOR_ELT(constants, GETOP()); \
  SEXP x = GETSTACK(-2); \
  SEXP y = GETSTACK(-1); \
  SETSTACK(-2, do_fun(call, opval, opsym, x, y,rho));	\
  R_BCNodeStackTop--; \
  R_Visible = TRUE; \
  NEXT(); \
} while(0)

#define Arith1(opsym) do {		\
  SEXP call = VECTOR_ELT(constants, GETOP()); \
  SEXP x = GETSTACK(-1); \
  SETSTACK(-1, cmp_arith1(call, opsym, x, rho)); \
  R_Visible = TRUE; \
  NEXT(); \
} while(0)


#define Arith2(opval,opsym) NewBuiltin2(cmp_arith2,opval,opsym,rho)
#define Relop2(opval,opsym) NewBuiltin2(cmp_relop,opval,opsym,rho)

#define R_MSG_NA	_("NaNs produced")
#define CMP_ISNAN ISNAN
//On Linux this is quite a bit faster; not on macOS El Capitan:
//#define CMP_ISNAN(x) ((x) != (x))
#define FastMath1(fun, sym) do {					\
	scalar_value_t vx;						\
	SEXP sa = NULL;							\
	int typex = bcStackScalarEx(R_BCNodeStackTop - 1, &vx, &sa);	\
	if (typex == REALSXP) {						\
	    double dval = fun(vx.dval);					\
	    if (CMP_ISNAN(dval)) {					\
		SEXP call = VECTOR_ELT(constants, GETOP());		\
		if (ISNAN(vx.dval)) dval = vx.dval;			\
		else warningcall(call, R_MSG_NA);			\
	    }								\
	    else SKIP_OP();						\
	    SETSTACK_REAL_EX(-1, dval, sa);				\
	    R_Visible = TRUE;						\
	    NEXT();							\
	}								\
	else if (typex == INTSXP && vx.ival != NA_INTEGER) {		\
	    SKIP_OP();							\
	    SETSTACK_REAL_EX(-1, fun(vx.ival), NULL);			\
	    R_Visible = TRUE;						\
	    NEXT();							\
	}								\
	Builtin1(do_math1,sym,rho);					\
    } while (0)

#ifdef NO_SAVE_ALLOC
# define DO_FAST_BINOP(fun,a,b,v) do {		\
    SKIP_OP(); \
    SETSTACK_REAL(-2, fun(a, b));		\
    R_BCNodeStackTop--; \
    R_Visible = TRUE; \
    NEXT(); \
} while (0)

# define DO_FAST_BINOP_INT(fun, a, b, v) do {	    \
    double dval = fun((double) (a), (double) (b));	\
    if (dval <= INT_MAX && dval >= INT_MIN + 1) { \
	SKIP_OP(); \
	SETSTACK_INTEGER(-2, (int) dval); \
	R_BCNodeStackTop--; \
	R_Visible = TRUE; \
	NEXT(); \
    } \
} while(0)
#else
/* these reuse one of the two values on the top of the stack if it is
   of the right type and has no references. It is known that both of
   these will have length one and have no attributes. */
# define DO_FAST_BINOP(fun, a, b, ans) do {				\
	SKIP_OP();							\
	double dval = fun(a, b);					\
	SETSTACK_REAL_EX(-2, dval, ans);				\
	R_BCNodeStackTop--;						\
	R_Visible = TRUE;						\
	NEXT();								\
    } while (0)

# define DO_FAST_BINOP_INT(fun, a, b, ans) do {				\
	double dval = fun((double) (a), (double) (b));			\
	if (dval <= INT_MAX && dval >= INT_MIN + 1) {			\
	    int val = (int) dval;					\
	    SKIP_OP();							\
	    SETSTACK_INTEGER_EX(-2, val, ans);				\
	    R_BCNodeStackTop--;						\
	    R_Visible = TRUE;						\
	    NEXT();							\
	}								\
    } while(0)
#endif

#define FastUnary(op, opsym) do {					\
	scalar_value_t vx;						\
	SEXP sa = NULL;							\
	int typex = bcStackScalarEx(R_BCNodeStackTop - 1, &vx, &sa);	\
	if (typex == REALSXP) {						\
	    SKIP_OP();							\
	    SETSTACK_REAL_EX(-1, op vx.dval, sa);			\
	    R_Visible = TRUE;						\
	    NEXT();							\
	}								\
	else if (typex == INTSXP && vx.ival != NA_INTEGER) {		\
	    SKIP_OP();							\
	    SETSTACK_INTEGER_EX(-1, op vx.ival, sa);			\
	    R_Visible = TRUE;						\
	    NEXT();							\
	}								\
	Arith1(opsym);							\
    } while (0)

# define FastBinary(op,opval,opsym) do { \
    scalar_value_t vx; \
    scalar_value_t vy; \
    SEXP sa = NULL; \
    SEXP sb = NULL; \
    int typex = bcStackScalarEx(R_BCNodeStackTop - 2, &vx, &sa);	\
    int typey = bcStackScalarEx(R_BCNodeStackTop - 1, &vy, &sb);	\
    if (typex == REALSXP) { \
	if (typey == REALSXP) \
	    DO_FAST_BINOP(op, vx.dval, vy.dval, sa ? sa : sb);	\
	else if (typey == INTSXP && vy.ival != NA_INTEGER) \
	    DO_FAST_BINOP(op, vx.dval, vy.ival, sa);	   \
    } \
    else if (typex == INTSXP && vx.ival != NA_INTEGER) { \
	if (typey == REALSXP) \
	    DO_FAST_BINOP(op, vx.ival, vy.dval, sb);	     \
	else if (typey == INTSXP && vy.ival != NA_INTEGER) { \
	    if (opval == DIVOP || opval == POWOP) \
		DO_FAST_BINOP(op, (double) vx.ival, (double) vy.ival, NULL); \
	    else \
		DO_FAST_BINOP_INT(op, vx.ival, vy.ival, sa ? sa : sb);	\
	} \
    } \
    Arith2(opval, opsym); \
} while (0)

#define R_ADD(x, y) ((x) + (y))
#define R_SUB(x, y) ((x) - (y))
#define R_MUL(x, y) ((x) * (y))
#define R_DIV(x, y) ((x) / (y))

#include "arithmetic.h"

/* The current (as of r67808) Windows toolchain compiles explicit sqrt
   calls in a way that returns a different NaN than NA_real_ when
   called with NA_real_. Not sure this is a bug in the Windows
   toolchain or in our expectations, but these defines attempt to work
   around this. */
#if (defined(_WIN32) || defined(_WIN64)) && defined(__GNUC__) && \
    __GNUC__ <= 4
# define R_sqrt(x) (ISNAN(x) ? x : sqrt(x))
#else
# define R_sqrt sqrt
#endif

#define DO_LOG() do {							\
	scalar_value_t vx;						\
	SEXP sa = NULL;							\
	int typex = bcStackScalarRealEx(R_BCNodeStackTop - 1, &vx, &sa); \
	if (typex == REALSXP) {						\
	    double dval = R_log(vx.dval);				\
	    if (CMP_ISNAN(dval)) {					\
		SEXP call = VECTOR_ELT(constants, GETOP());		\
		if (ISNAN(vx.dval)) dval = vx.dval;			\
		else warningcall(call, R_MSG_NA);			\
	    }								\
	    else SKIP_OP();						\
	    SETSTACK_REAL_EX(-1, dval, sa);				\
	    R_Visible = TRUE;						\
	    NEXT();							\
	}								\
	SEXP call = VECTOR_ELT(constants, GETOP());			\
	SEXP args = CONS_NR(GETSTACK(-1), R_NilValue);			\
	SETSTACK(-1, args); /* to protect */				\
	SEXP op = getPrimitive(R_LogSym, SPECIALSXP);			\
	SETSTACK(-1, do_log_builtin(call, op, args, rho));		\
	R_Visible = TRUE;						\
	NEXT();								\
 } while (0)

#define DO_LOGBASE() do {						\
	scalar_value_t vx, vy;						\
	SEXP sa = NULL;							\
	SEXP sb = NULL;							\
	int typex = bcStackScalarRealEx(R_BCNodeStackTop - 2, &vx, &sa); \
	int typey = bcStackScalarRealEx(R_BCNodeStackTop - 1, &vy, &sb); \
	if (typex == REALSXP && typey == REALSXP) {			\
	    double dval = logbase(vx.dval, vy.dval);			\
	    if (ISNAN(dval)) {						\
		SEXP call = VECTOR_ELT(constants, GETOP());		\
		if (ISNAN(vx.dval)) dval = vx.dval;			\
		else if (ISNAN(vy.dval)) dval = vy.dval;		\
		else warningcall(call, R_MSG_NA);			\
	    }								\
	    else SKIP_OP();						\
	    R_BCNodeStackTop--;						\
	    SETSTACK_REAL_EX(-1, dval, sa);				\
	    R_Visible = TRUE;						\
	    NEXT();							\
	}								\
	SEXP call = VECTOR_ELT(constants, GETOP());			\
	SEXP tmp = GETSTACK(-2);					\
	SEXP args = CONS_NR(tmp, CONS_NR(GETSTACK(-1), R_NilValue));	\
	R_BCNodeStackTop--;						\
	SETSTACK(-1, args); /* to protect */				\
	SEXP op = getPrimitive(R_LogSym, SPECIALSXP);			\
	SETSTACK(-1, do_log_builtin(call, op, args, rho));		\
	R_Visible = TRUE;						\
	NEXT();								\
    } while (0)

#include <Rmath.h>
/* Keep the order consistent with the order in the byte code compiler! */
static struct { const char *name; SEXP sym; double (*fun)(double); }
    math1funs[] = {
	{"floor", NULL, floor},
	{"ceiling", NULL, ceil},
	{"sign", NULL, sign},

	{"expm1", NULL, expm1},
	{"log1p", NULL, log1p},

	{"cos", NULL, cos},
	{"sin", NULL, sin},
	{"tan", NULL, tan},
	{"acos", NULL, acos},
	{"asin", NULL, asin},
	{"atan", NULL, atan},

	{"cosh", NULL, cosh},
	{"sinh", NULL, sinh},
	{"tanh", NULL, tanh},
	{"acosh", NULL, acosh},
	{"asinh", NULL, asinh},
	{"atanh", NULL, atanh},

	{"lgamma", NULL, lgammafn},
	{"gamma", NULL, gammafn},
	{"digamma", NULL, digamma},
	{"trigamma", NULL, trigamma},

	{"cospi", NULL, cospi},
	{"sinpi", NULL, sinpi},
#ifndef HAVE_TANPI
	{"tanpi", NULL, tanpi}
#else
	{"tanpi", NULL, Rtanpi}
#endif
    };

static R_INLINE double (*getMath1Fun(int i, SEXP call))(double) {
    if (math1funs[i].sym == NULL)
	math1funs[i].sym = install(math1funs[i].name);
    if (CAR(call) != math1funs[i].sym)
	error("math1 compiler/interpreter mismatch");
    return math1funs[i].fun;
}

#define DO_MATH1() do {							\
	SEXP call = VECTOR_ELT(constants, GETOP());			\
	double (*fun)(double) = getMath1Fun(GETOP(), call);		\
	scalar_value_t vx;						\
	SEXP sa = NULL;							\
	int typex = bcStackScalarRealEx(R_BCNodeStackTop - 1, &vx, &sa); \
	if (typex == REALSXP) {						\
	    double dval = fun(vx.dval);					\
            if (ISNAN(dval)) {						\
		if (ISNAN(vx.dval)) dval = vx.dval;			\
		else warningcall(call, R_MSG_NA);			\
	    }								\
	    SETSTACK_REAL_EX(-1, dval, sa);				\
	    R_Visible = TRUE;						\
	    NEXT();							\
	}								\
	SEXP args = CONS_NR(GETSTACK(-1), R_NilValue);			\
	SEXP sym = CAR(call);						\
	SETSTACK(-1, args); /* to protect */				\
	SEXP op = getPrimitive(sym, BUILTINSXP);			\
	SETSTACK(-1, do_math1(call, op, args, rho));			\
	R_Visible = TRUE;						\
	NEXT();								\
    } while (0)

#include <Rdynpriv.h>

#define DOTCALL_MAX 16
#define DO_DOTCALL() do {						\
	SEXP call = VECTOR_ELT(constants, GETOP());			\
	int nargs = GETOP();						\
	DL_FUNC ofun = R_dotCallFn(GETSTACK(- nargs - 1), call, nargs);	\
	if (ofun && nargs <= DOTCALL_MAX) {				\
	    SEXP cargs[DOTCALL_MAX];					\
	    for (int i = 0; i < nargs; i++)				\
		cargs[i] = GETSTACK(i - nargs);				\
	    void *vmax = vmaxget();					\
	    SEXP val = R_doDotCall(ofun, nargs, cargs, call);		\
	    vmaxset(vmax);						\
	    R_BCNodeStackTop -= nargs;					\
	    SETSTACK(-1, val);						\
	    R_Visible = TRUE;						\
	    NEXT();							\
	}								\
	SEXP args = R_NilValue;						\
	BCNPUSH(args); /* allocate space for protecting args */		\
	while (nargs-- >= 0) {						\
	    args = CONS_NR(GETSTACK(-2), args);				\
	    SETSTACK(-2, args); /* to protect */			\
	    BCNPOP_IGNORE_VALUE();					\
	}								\
	SEXP sym = CAR(call);						\
	SEXP op = getPrimitive(sym, BUILTINSXP);			\
	SETSTACK(-1, do_dotcall(call, op, args, rho));			\
	R_Visible = TRUE;						\
	NEXT();								\
    } while (0)

#define DO_COLON() do {							\
	scalar_value_t vx;						\
	scalar_value_t vy;						\
	int typex = bcStackScalarRealEx(R_BCNodeStackTop - 2, &vx, NULL); \
	int typey = bcStackScalarRealEx(R_BCNodeStackTop - 1, &vy, NULL); \
	if (typex == REALSXP && typey == REALSXP) {			\
	    double rn1 = vx.dval;					\
	    double rn2 = vy.dval;					\
	    if (R_FINITE(rn1) && R_FINITE(rn2) &&			\
		INT_MIN <= rn1 && INT_MAX >= rn1 &&			\
		INT_MIN <= rn2 && INT_MAX >= rn2 &&			\
		rn1 == (int) rn1 && rn2 == (int) rn2) {			\
		SKIP_OP(); /* skip 'call' index */			\
		R_BCNodeStackTop--;					\
		SETSTACK_INTSEQ(-1, rn1, rn2);				\
		R_Visible = TRUE;					\
		NEXT();							\
	    }								\
	}								\
	Builtin2(do_colon, R_ColonSymbol, rho);				\
    } while (0)

#define DO_SEQ_ALONG() do {					\
	SEXP x = GETSTACK(-1);					\
	if (! OBJECT(x)) {					\
	    R_xlen_t len = xlength(x);				\
	    if (len >= 1 && len <= INT_MAX) {			\
		SKIP_OP(); /* skip 'call' index */		\
		SETSTACK_INTSEQ(-1, 1, len);			\
		R_Visible = TRUE;				\
		NEXT();						\
	    }							\
	}							\
	Builtin1(do_seq_along, install("seq_along"), rho);	\
    } while (0)

#define DO_SEQ_LEN() do {						\
	scalar_value_t vx;						\
	int typex = bcStackScalarRealEx(R_BCNodeStackTop - 1, &vx, NULL); \
	if (typex == REALSXP) {						\
	    double rlen = vx.dval;					\
	    if (1 <= rlen && INT_MAX >= rlen &&				\
		rlen == (int) rlen) {					\
		SKIP_OP(); /* skip 'call' index */			\
		SETSTACK_INTSEQ(-1, 1, rlen);				\
		R_Visible = TRUE;					\
		NEXT();							\
	    }								\
	}								\
	Builtin1(do_seq_len, install("seq_len"), rho);			\
    } while (0)

static R_INLINE SEXP getForLoopSeq(int offset, Rboolean *iscompact)
{
#if defined(TYPED_STACK) && defined(COMPACT_INTSEQ)
    R_bcstack_t *s = R_BCNodeStackTop + offset;
    if (s->tag == INTSEQSXP) {
	*iscompact = TRUE;
	return s->u.sxpval;
    }
#endif
    *iscompact = FALSE;
    return GETSTACK(offset);
}

#define BCNPUSH(v) do { \
  SEXP __value__ = (v); \
  R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
  if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
  SETSTACK(0, __value__); \
  R_BCNodeStackTop = __ntop__; \
} while (0)

#ifdef TYPED_STACK
#define BCNPUSH_REAL(v) do { \
  double __value__ = (v); \
  R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
  if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
  __ntop__[-1].u.dval = __value__; \
  __ntop__[-1].tag = REALSXP; \
  R_BCNodeStackTop = __ntop__; \
} while (0)

#define BCNPUSH_INTEGER(v) do { \
  int __value__ = (v); \
  R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
  if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
  __ntop__[-1].u.ival = __value__; \
  __ntop__[-1].tag = INTSXP; \
  R_BCNodeStackTop = __ntop__; \
} while (0)
#endif

#define BCNDUP() do { \
    R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
    if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
    __ntop__[-1] = __ntop__[-2]; \
    R_BCNodeStackTop = __ntop__; \
} while(0)

#define BCNDUP2ND() do { \
    R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
    if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
    __ntop__[-1] = __ntop__[-3]; \
    R_BCNodeStackTop = __ntop__; \
} while(0)

#define BCNPOP() (R_BCNodeStackTop--, GETSTACK(0))
#define BCNPOP_IGNORE_VALUE() R_BCNodeStackTop--

#define BCNSTACKCHECK(n)  do {						\
	if (R_BCNodeStackTop + (n) > R_BCNodeStackEnd) nodeStackOverflow(); \
    } while (0)

#define BCIPUSHPTR(v)  do { \
  void *__value__ = (v); \
  IStackval *__ntop__ = R_BCIntStackTop + 1; \
  if (__ntop__ > R_BCIntStackEnd) intStackOverflow(); \
  *__ntop__[-1].p = __value__; \
  R_BCIntStackTop = __ntop__; \
} while (0)

#define BCIPUSHINT(v)  do { \
  int __value__ = (v); \
  IStackval *__ntop__ = R_BCIntStackTop + 1; \
  if (__ntop__ > R_BCIntStackEnd) intStackOverflow(); \
  __ntop__[-1].i = __value__; \
  R_BCIntStackTop = __ntop__; \
} while (0)

#define BCIPOPPTR() ((--R_BCIntStackTop)->p)
#define BCIPOPINT() ((--R_BCIntStackTop)->i)

#define BCCONSTS(e) BCODE_CONSTS(e)

static void NORET nodeStackOverflow()
{
    error(_("node stack overflow"));
}

#ifdef BC_INT_STACK
static void NORET intStackOverflow()
{
    error(_("integer stack overflow"));
}
#endif

#ifdef TYPED_STACK

/* Allocate consecutive space of nelems node stack elements */
static R_INLINE void* BCNALLOC(int nelems) {
    void *ans;

    BCNSTACKCHECK(nelems + 1);
    R_BCNodeStackTop->tag = RAWMEM_TAG;
    R_BCNodeStackTop->u.ival = nelems;
    R_BCNodeStackTop++;
    ans = R_BCNodeStackTop;
    R_BCNodeStackTop += nelems;
    return ans;
}
#else
# error BCNALLOC and such for untyped stack not available yet
#endif

/* Allocate R context on the node stack */
#define RCNTXT_ELEMS ((sizeof(RCNTXT) + sizeof(R_bcstack_t) - 1) \
			/ sizeof(R_bcstack_t))

#define BCNALLOC_CNTXT() (RCNTXT *)BCNALLOC(RCNTXT_ELEMS)

static R_INLINE void BCNPOP_AND_END_CNTXT() {
    RCNTXT* cntxt = (RCNTXT *)(R_BCNodeStackTop - RCNTXT_ELEMS);
    endcontext(cntxt);
    R_BCNodeStackTop -= RCNTXT_ELEMS + 1;
}

static SEXP bytecodeExpr(SEXP e)
{
    if (isByteCode(e)) {
	if (LENGTH(BCCONSTS(e)) > 0)
	    return VECTOR_ELT(BCCONSTS(e), 0);
	else return R_NilValue;
    }
    else return e;
}

SEXP R_BytecodeExpr(SEXP e)
{
    return bytecodeExpr(e);
}

SEXP R_PromiseExpr(SEXP p)
{
    return bytecodeExpr(PRCODE(p));
}

SEXP R_ClosureExpr(SEXP p)
{
    return bytecodeExpr(BODY(p));
}

#ifdef THREADED_CODE
typedef union { void *v; int i; } BCODE;

/* Declare opinfo volatile to prevent gcc 6 from making a local copy
   in bcEval stack frames and thus increasing stack usage
   dramatically */
volatile
static struct { void *addr; int argc; char *instname; } opinfo[OPCOUNT];

#define OP(name,n) \
  case name##_OP: opinfo[name##_OP].addr = (__extension__ &&op_##name); \
    opinfo[name##_OP].argc = (n); \
    opinfo[name##_OP].instname = #name; \
    goto loop; \
    op_##name

#define BEGIN_MACHINE  NEXT(); init: { loop: switch(which++)
#define LASTOP } retvalue = R_NilValue; goto done
#define INITIALIZE_MACHINE() if (body == NULL) goto init

#define NEXT() (__extension__ ({currentpc = pc; goto *(*pc++).v;}))
#define GETOP() (*pc++).i
#define SKIP_OP() (pc++)

#define BCCODE(e) (BCODE *) INTEGER(BCODE_CODE(e))
#else
typedef int BCODE;

#define OP(name,argc) case name##_OP

#ifdef BC_PROFILING
#define BEGIN_MACHINE  loop: currentpc = pc; current_opcode = *pc; switch(*pc++)
#else
#define BEGIN_MACHINE  loop: currentpc = pc; switch(*pc++)
#endif
#define LASTOP  default: error(_("bad opcode"))
#define INITIALIZE_MACHINE()

#define NEXT() goto loop
#define GETOP() *pc++
#define SKIP_OP() (pc++)

#define BCCODE(e) INTEGER(BCODE_CODE(e))
#endif

static R_INLINE SEXP BINDING_VALUE(SEXP loc)
{
    if (loc != R_NilValue && ! IS_ACTIVE_BINDING(loc))
	return CAR(loc);
    else
	return R_UnboundValue;
}

#define BINDING_SYMBOL(loc) TAG(loc)

/* Defining USE_BINDING_CACHE enables a cache for GETVAR, SETVAR, and
   others to more efficiently locate bindings in the top frame of the
   current environment.  The index into of the symbol in the constant
   table is used as the cache index.  Two options can be used to chose
   among implementation strategies:

       If CACHE_ON_STACK is defined the cache is allocated on the
       byte code stack. Otherwise it is allocated on the heap as a
       VECSXP.  The stack-based approach is more efficient, but runs
       the risk of running out of stack space.

       If CACHE_MAX is defined, then a cache of at most that size is
       used. The value must be a power of 2 so a modulus computation x
       % CACHE_MAX can be done as x & (CACHE_MAX - 1). More than 90%
       of the closures in base have constant pools with fewer than 128
       entries when compiled, to that is a good value to use. But
       increasing to 256 handles some benchmark scripts a bit better.

   On average about 1/3 of constant pool entries are symbols, so this
   approach wastes some space.  This could be avoided by grouping the
   symbols at the beginning of the constant pool and recording the
   number.

   Bindings recorded may become invalid if user code removes a
   variable.  The code in envir.c has been modified to insert
   R_unboundValue as the value of a binding when it is removed, and
   code using cached bindings checks for this.

   It would be nice if we could also cache bindings for variables
   found in enclosing environments. These would become invalid if a
   new variable is defined in an intervening frame. Some mechanism for
   invalidating the cache would be needed. This is certainly possible,
   but finding an efficient mechanism does not seem to be easy.   LT */

#define USE_BINDING_CACHE
# ifdef USE_BINDING_CACHE
/* CACHE_MAX must be a power of 2 for modulus using & CACHE_MASK to work*/
# define CACHE_MAX 256
# ifdef CACHE_MAX
#  define CACHE_MASK (CACHE_MAX - 1)
#  define CACHEIDX(i) ((i) & CACHE_MASK)
# else
#  define CACHEIDX(i) (i)
# endif

# define CACHE_ON_STACK
# ifdef CACHE_ON_STACK
typedef R_bcstack_t * R_binding_cache_t;
#  define VCACHE(i) GETSTACK_SXPVAL_PTR(vcache + (i))
#  define GET_CACHED_BINDING_CELL(vcache, sidx) \
    (vcache ? VCACHE(CACHEIDX(sidx)) : R_NilValue)
#  define GET_SMALLCACHE_BINDING_CELL(vcache, sidx) \
    (vcache ? VCACHE(sidx) : R_NilValue)

#  define SET_CACHED_BINDING(vcache, sidx, cell) \
    do { if (vcache) VCACHE(CACHEIDX(sidx)) = (cell); } while (0)
# else
typedef SEXP R_binding_cache_t;
#  define GET_CACHED_BINDING_CELL(vcache, sidx) \
    (vcache ? VECTOR_ELT(vcache, CACHEIDX(sidx)) : R_NilValue)
#  define GET_SMALLCACHE_BINDING_CELL(vcache, sidx) \
    (vcache ? VECTOR_ELT(vcache, sidx) : R_NilValue)

#  define SET_CACHED_BINDING(vcache, sidx, cell) \
    do { if (vcache) SET_VECTOR_ELT(vcache, CACHEIDX(sidx), cell); } while (0)
# endif
#else
typedef void *R_binding_cache_t;
# define GET_CACHED_BINDING_CELL(vcache, sidx) R_NilValue
# define GET_SMALLCACHE_BINDING_CELL(vcache, sidx) R_NilValue

# define SET_CACHED_BINDING(vcache, sidx, cell)
#endif

static R_INLINE SEXP GET_BINDING_CELL_CACHE(SEXP symbol, SEXP rho,
					    R_binding_cache_t vcache, int idx)
{
    SEXP cell = GET_CACHED_BINDING_CELL(vcache, idx);
    /* The value returned by GET_CACHED_BINDING_CELL is either a
       binding cell or R_NilValue.  TAG(R_NilValue) is R_NilValue, and
       that will not equal symbol. So a separate test for cell !=
       R_NilValue is not needed. */
    if (TAG(cell) == symbol && CAR(cell) != R_UnboundValue)
	return cell;
    else {
	SEXP ncell = GET_BINDING_CELL(symbol, rho);
	if (ncell != R_NilValue)
	    SET_CACHED_BINDING(vcache, idx, ncell);
	else if (cell != R_NilValue && CAR(cell) == R_UnboundValue)
	    SET_CACHED_BINDING(vcache, idx, R_NilValue);
	return ncell;
    }
}

static void NORET MISSING_ARGUMENT_ERROR(SEXP symbol)
{
    const char *n = CHAR(PRINTNAME(symbol));
    if(*n) error(_("argument \"%s\" is missing, with no default"), n);
    else error(_("argument is missing, with no default"));
}

#define MAYBE_MISSING_ARGUMENT_ERROR(symbol, keepmiss) \
    do { if (! keepmiss) MISSING_ARGUMENT_ERROR(symbol); } while (0)

static void NORET UNBOUND_VARIABLE_ERROR(SEXP symbol)
{
    error(_("object '%s' not found"), EncodeChar(PRINTNAME(symbol)));
}

static R_INLINE SEXP FORCE_PROMISE(SEXP value, SEXP symbol, SEXP rho,
				   Rboolean keepmiss)
{
    if (PRVALUE(value) == R_UnboundValue) {
	/**** R_isMissing is inefficient */
	if (keepmiss && R_isMissing(symbol, rho))
	    value = R_MissingArg;
	else value = forcePromise(value);
    }
    else value = PRVALUE(value);
    ENSURE_NAMEDMAX(value);
    return value;
}

static R_INLINE SEXP FIND_VAR_NO_CACHE(SEXP symbol, SEXP rho, SEXP cell)
{
    SEXP value;
    /* only need to search the current frame again if
       binding was special or frame is a base frame */
    if (cell != R_NilValue ||
	rho == R_BaseEnv || rho == R_BaseNamespace)
	value =  findVar(symbol, rho);
    else
	value =  findVar(symbol, ENCLOS(rho));
    return value;
}

static R_INLINE SEXP getvar(SEXP symbol, SEXP rho,
			    Rboolean dd, Rboolean keepmiss,
			    R_binding_cache_t vcache, int sidx)
{
    SEXP value;
    if (dd)
	value = ddfindVar(symbol, rho);
    else if (vcache != NULL) {
	SEXP cell = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	value = BINDING_VALUE(cell);
	if (value == R_UnboundValue)
	    value = FIND_VAR_NO_CACHE(symbol, rho, cell);
    }
    else
	value = findVar(symbol, rho);

    if (value == R_UnboundValue)
	UNBOUND_VARIABLE_ERROR(symbol);
    else if (value == R_MissingArg)
	MAYBE_MISSING_ARGUMENT_ERROR(symbol, keepmiss);
    else if (TYPEOF(value) == PROMSXP) {
	PROTECT(value);
	value = FORCE_PROMISE(value, symbol, rho, keepmiss);
	UNPROTECT(1);
    } else ENSURE_NAMED(value); /* should not really be needed - LT */
    return value;
}

#define INLINE_GETVAR
#ifdef INLINE_GETVAR
/* Try to handle the most common case as efficiently as possible.  If
   smallcache is true then a modulus operation on the index is not
   needed, nor is a check that a non-null value corresponds to the
   requested symbol. The symbol from the constant pool is also usually
   not needed. The test TYPOF(value) != SYMBOL rules out R_MissingArg
   and R_UnboundValue as these are implemented s symbols.  It also
   rules other symbols, but as those are rare they are handled by the
   getvar() call. */
#define DO_GETVAR(dd,keepmiss) do { \
    int sidx = GETOP(); \
    if (!dd && smallcache) { \
	SEXP cell = GET_SMALLCACHE_BINDING_CELL(vcache, sidx); \
	/* try fast handling of REALSXP, INTSXP, LGLSXP */ \
	/* (cell won't be R_NilValue or an active binding) */ \
	SEXP value = CAR(cell); \
	int type = TYPEOF(value); \
	switch(type) { \
	case REALSXP: \
	case INTSXP: \
	case LGLSXP: \
	    ENSURE_NAMED(value); /* should not really be needed - LT */ \
	    R_Visible = TRUE; \
	    BCNPUSH(value); \
	    NEXT(); \
	} \
	if (cell != R_NilValue && ! IS_ACTIVE_BINDING(cell)) { \
	    value = CAR(cell); \
	    if (TYPEOF(value) != SYMSXP) {	\
		if (TYPEOF(value) == PROMSXP) {		\
		    SEXP pv = PRVALUE(value);		\
		    if (pv == R_UnboundValue) {		\
			SEXP symbol = VECTOR_ELT(constants, sidx);	\
			value = FORCE_PROMISE(value, symbol, rho, keepmiss); \
		    }							\
		    else value = pv;					\
		}							\
		/* should not really be needed - LT */			\
		else ENSURE_NAMED(value);				\
		R_Visible = TRUE;					\
		BCNPUSH(value);						\
		NEXT();							\
	    }								\
	}								\
    }									\
    SEXP symbol = VECTOR_ELT(constants, sidx);				\
    R_Visible = TRUE;							\
    BCNPUSH(getvar(symbol, rho, dd, keepmiss, vcache, sidx));		\
    NEXT();								\
} while (0)
#else
#define DO_GETVAR(dd,keepmiss) do { \
  int sidx = GETOP(); \
  SEXP symbol = VECTOR_ELT(constants, sidx); \
  R_Visible = TRUE; \
  BCNPUSH(getvar(symbol, rho, dd, keepmiss, vcache, sidx));	\
  NEXT(); \
} while (0)
#endif

/* call frame accessors */
#define CALL_FRAME_FUN() GETSTACK(-3)
#define CALL_FRAME_ARGS() GETSTACK(-2)
#define CALL_FRAME_FTYPE() TYPEOF(CALL_FRAME_FUN())
#define CALL_FRAME_SIZE() (3)

static R_INLINE SEXP BUILTIN_CALL_FRAME_ARGS()
{
    SEXP args = CALL_FRAME_ARGS();
    for (SEXP a = args; a  != R_NilValue; a = CDR(a))
	DECREMENT_LINKS(CAR(a));
    return args;
}

static R_INLINE SEXP CLOSURE_CALL_FRAME_ARGS()
{
    SEXP args = CALL_FRAME_ARGS();
    /* it would be better not to build this arglist with CONS_NR in
       the first place */
    for (SEXP a = args; a  != R_NilValue; a = CDR(a)) {
	DECREMENT_LINKS(CAR(a));
	if (! TRACKREFS(a)) {
	    ENABLE_REFCNT(a);
	    INCREMENT_REFCNT(CAR(a));
	    INCREMENT_REFCNT(CDR(a));
	}
    }
    return args;
}

#define GETSTACK_BELOW_CALL_FRAME(n) GETSTACK((n) - CALL_FRAME_SIZE())
#define SETSTACK_BELOW_CALL_FRAME(n, v) SETSTACK((n) - CALL_FRAME_SIZE(), v)

/* create room for accumulating the arguments. */
#define INIT_CALL_FRAME_ARGS() do { \
	BCNSTACKCHECK(2);	  \
	SETSTACK(0, R_NilValue);  \
	SETSTACK(1, R_NilValue);  \
	R_BCNodeStackTop += 2;	  \
    } while (0)

/* push the function and create room for accumulating the arguments. */
#define INIT_CALL_FRAME(fun) do { \
	BCNPUSH(fun);		\
	INIT_CALL_FRAME_ARGS();	\
    } while (0)

/* remove the call frame from the stack and push the return value */
#define POP_CALL_FRAME(value) POP_CALL_FRAME_PLUS(0, value)

#define POP_CALL_FRAME_PLUS(n, value) do {	\
	R_BCNodeStackTop -= (2 + (n));		\
	SETSTACK(-1, value);			\
    } while (0)

/* push an argument to existing call frame */
/* a call frame always uses boxed stack values, so GETSTACK will not allocate */
#define PUSHCALLARG(v) do {					\
	SEXP __cell__ = CONS_NR(v, R_NilValue);			\
	if (GETSTACK(-2) == R_NilValue) SETSTACK(-2, __cell__); \
	else SETCDR(GETSTACK(-1), __cell__);			\
	SETSTACK(-1, __cell__);					\
	INCREMENT_LINKS(CAR(__cell__));				\
} while (0)

/* place a tag on the most recently pushed call argument */
#define SETCALLARG_TAG(t) do {			\
	SEXP __tag__ = (t);			\
	if (__tag__ != R_NilValue) {		\
	    SEXP __cell__ = GETSTACK(-1);	\
	    if (__cell__ != R_NilValue)		   \
		SET_TAG(__cell__, CreateTag(__tag__));	\
	}						\
    } while (0)

/* same, but tag is known to be a symbol */
#define SETCALLARG_TAG_SYMBOL(t) do {			\
	SEXP __cell__ = GETSTACK(-1);			\
	if (__cell__ != R_NilValue)			\
	    SET_TAG(__cell__, t);			\
    } while (0)

static int tryDispatch(char *generic, SEXP call, SEXP x, SEXP rho, SEXP *pv)
{
  RCNTXT cntxt;
  SEXP pargs, rho1;
  int dispatched = FALSE;
  SEXP op = SYMVALUE(install(generic)); /**** avoid this */

  PROTECT(pargs = promiseArgs(CDR(call), rho));
  SET_PRVALUE(CAR(pargs), x);

  /**** Minimal hack to try to handle the S4 case.  If we do the check
	and do not dispatch then some arguments beyond the first might
	have been evaluated; these will then be evaluated again by the
	compiled argument code. */
  if (IS_S4_OBJECT(x) && R_has_methods(op)) {
    SEXP val = R_possible_dispatch(call, op, pargs, rho, TRUE);
    if (val) {
      *pv = val;
      UNPROTECT(1);
      return TRUE;
    }
  }

  /* See comment at first usemethod() call in this file. LT */
  PROTECT(rho1 = NewEnvironment(R_NilValue, R_NilValue, rho));
  begincontext(&cntxt, CTXT_RETURN, call, rho1, rho, pargs, op);
  if (usemethod(generic, x, call, pargs, rho1, rho, R_BaseEnv, pv))
    dispatched = TRUE;
  endcontext(&cntxt);
  UNPROTECT(2);
#ifdef ADJUST_ENVIR_REFCNTS
  R_CleanupEnvir(rho1, dispatched ? *pv : R_NilValue);
  unpromiseArgs(pargs);
#else
  if (! dispatched) DECREMENT_REFCNT(x);
#endif
  return dispatched;
}

static int tryAssignDispatch(char *generic, SEXP call, SEXP lhs, SEXP rhs,
			     SEXP rho, SEXP *pv)
{
    int result;
    SEXP ncall, last, prom;

    PROTECT(ncall = duplicate(call));
    last = ncall;
    while (CDR(last) != R_NilValue)
	last = CDR(last);
    prom = mkRHSPROMISE(CAR(last), rhs);
    SETCAR(last, prom);
    result = tryDispatch(generic, ncall, lhs, rho, pv);
    UNPROTECT(1);
    return result;
}

#define DO_STARTDISPATCH(generic) do { \
  SEXP call = VECTOR_ELT(constants, GETOP()); \
  int label = GETOP(); \
  SEXP value = GETSTACK(-1); \
  if (isObject(value) && tryDispatch(generic, call, value, rho, &value)) {\
    SETSTACK(-1, value);						\
    BC_CHECK_SIGINT(); \
    pc = codebase + label; \
  } \
  else { \
    SEXP tag = TAG(CDR(call)); \
    BCNPUSH(call); \
    INIT_CALL_FRAME(R_NilValue); \
    PUSHCALLARG(value); \
    SETCALLARG_TAG(tag);   \
  } \
  NEXT(); \
} while (0)

#define DO_DFLTDISPATCH(fun, symbol) do { \
  SEXP call = GETSTACK_BELOW_CALL_FRAME(-1); \
  SEXP args = BUILTIN_CALL_FRAME_ARGS(); \
  SEXP value = fun(call, symbol, args, rho); \
  POP_CALL_FRAME_PLUS(2, value); \
  R_Visible = TRUE; \
  NEXT(); \
} while (0)

#define DO_START_ASSIGN_DISPATCH(generic) do { \
  SEXP call = VECTOR_ELT(constants, GETOP()); \
  int label = GETOP(); \
  SEXP lhs = GETSTACK(-2); \
  SEXP rhs = GETSTACK(-1); \
  if (MAYBE_SHARED(lhs)) { \
    lhs = shallow_duplicate(lhs); \
    SETSTACK(-2, lhs); \
    ENSURE_NAMED(lhs); \
  } \
  SEXP value = NULL; \
  if (isObject(lhs) && \
      tryAssignDispatch(generic, call, lhs, rhs, rho, &value)) { \
    R_BCNodeStackTop--;	\
    SETSTACK(-1, value); \
    BC_CHECK_SIGINT(); \
    pc = codebase + label; \
  } \
  else { \
    SEXP tag = TAG(CDR(call)); \
    BCNPUSH(call); \
    INIT_CALL_FRAME(R_NilValue); \
    PUSHCALLARG(lhs); \
    SETCALLARG_TAG(tag);   \
  } \
  NEXT(); \
} while (0)

#define DO_DFLT_ASSIGN_DISPATCH(fun, symbol) do { \
  SEXP rhs = GETSTACK_BELOW_CALL_FRAME(-2); \
  SEXP call = GETSTACK_BELOW_CALL_FRAME(-1); \
  SEXP args = BUILTIN_CALL_FRAME_ARGS(); \
  PUSHCALLARG(rhs); \
  SEXP value = fun(call, symbol, args, rho); \
  POP_CALL_FRAME_PLUS(3, value); \
  NEXT(); \
} while (0)

#define DO_STARTDISPATCH_N(generic) do { \
    int callidx = GETOP(); \
    int label = GETOP(); \
    SEXP value = GETSTACK(-1); \
    if (isObject(value)) { \
	SEXP call = VECTOR_ELT(constants, callidx); \
	if (tryDispatch(generic, call, value, rho, &value)) { \
	    SETSTACK(-1, value); \
	    BC_CHECK_SIGINT(); \
	    pc = codebase + label; \
	} \
    } \
    NEXT(); \
} while (0)

#define DO_START_ASSIGN_DISPATCH_N(generic) do { \
    int callidx = GETOP(); \
    int label = GETOP(); \
    SEXP lhs = GETSTACK(-2); \
    if (isObject(lhs)) { \
	SEXP call = VECTOR_ELT(constants, callidx); \
	SEXP rhs = GETSTACK(-1); \
	if (MAYBE_SHARED(lhs)) { \
	    lhs = shallow_duplicate(lhs); \
	    SETSTACK(-2, lhs); \
	    ENSURE_NAMED(lhs); \
	} \
	SEXP value = NULL; \
	if (tryAssignDispatch(generic, call, lhs, rhs, rho, &value)) { \
	    R_BCNodeStackTop--; \
	    SETSTACK(-1, value); \
	    BC_CHECK_SIGINT(); \
	    pc = codebase + label; \
	} \
    } \
    NEXT(); \
} while (0)

#define DO_ISTEST(fun) do { \
  SETSTACK(-1, fun(GETSTACK(-1)) ? R_TrueValue : R_FalseValue);	\
  R_Visible = TRUE; \
  NEXT(); \
} while(0)
#define DO_ISTYPE(type) do { \
  SETSTACK(-1, TYPEOF(GETSTACK(-1)) == type ? R_TrueValue : R_FalseValue); \
  R_Visible = TRUE; \
  NEXT(); \
} while (0)
#define isNumericOnly(x) (isNumeric(x) && ! isLogical(x))

#ifdef BC_PROFILING
#define NO_CURRENT_OPCODE -1
static int current_opcode = NO_CURRENT_OPCODE;
static int opcode_counts[OPCOUNT];
#endif

#define BC_COUNT_DELTA 1000

#ifndef IMMEDIATE_FINALIZERS
/* finalizers are run here since this should only be called at
   points where running arbitrary code should be safe */
#define BC_CHECK_SIGINT() do { \
  if (++evalcount > BC_COUNT_DELTA) { \
      R_CheckUserInterrupt(); \
      R_RunPendingFinalizers(); \
      evalcount = 0; \
  } \
} while (0)
#else
#define BC_CHECK_SIGINT() do { \
  if (++evalcount > BC_COUNT_DELTA) { \
      R_CheckUserInterrupt(); \
      evalcount = 0; \
  } \
} while (0)
#endif

static R_INLINE R_xlen_t bcStackIndex(R_bcstack_t *s)
{
#ifdef TYPED_STACK
    switch(s->tag) {
    case INTSXP:
	if (s->u.ival != NA_INTEGER)
	    return s->u.ival;
	else return -1;
    case REALSXP:
	{
	    double val = s->u.dval;
	    if (! ISNAN(val) && val <= R_XLEN_T_MAX && val > 0)
		return (R_xlen_t) s->u.dval;
	    else return -1;
	}
    case LGLSXP: return -1;
    default: break;
    }
#endif
    SEXP idx = GETSTACK_SXPVAL_PTR(s);
    if (IS_SCALAR(idx, INTSXP)) {
	int ival = SCALAR_IVAL(idx);
	if (ival != NA_INTEGER)
	    return ival;
	else return -1;
    }
    else if (IS_SCALAR(idx, REALSXP)) {
	double val = SCALAR_DVAL(idx);
	if (! ISNAN(val) && val <= R_XLEN_T_MAX && val > 0)
	    return (R_xlen_t) val;
	else return -1;
    }
    else return -1;
}

static R_INLINE SEXP mkVector1(SEXP s)
{
    SEXP t = allocVector(VECSXP, 1);
    SET_VECTOR_ELT(t, 0, s);
    return t;
}

#define DO_FAST_VECELT(sv, vec,  i, subset2) do {		\
	switch (TYPEOF(vec)) {					\
	case REALSXP:						\
	    if (XLENGTH(vec) <= i) break;			\
	    SETSTACK_REAL_PTR(sv, REAL_ELT(vec, i));		\
	    return;						\
	case INTSXP:						\
	    if (XLENGTH(vec) <= i) break;			\
	    SETSTACK_INTEGER_PTR(sv, INTEGER_ELT(vec, i));	\
	    return;						\
	case LGLSXP:						\
	    if (XLENGTH(vec) <= i) break;			\
	    SETSTACK_LOGICAL_PTR(sv, LOGICAL_ELT(vec, i));	\
	    return;						\
	case CPLXSXP:						\
	    if (XLENGTH(vec) <= i) break;			\
	    SETSTACK_PTR(sv, ScalarComplex(COMPLEX_ELT(vec, i)));	\
	    return;						\
	case RAWSXP:						\
	    if (XLENGTH(vec) <= i) break;			\
	    SETSTACK_PTR(sv, ScalarRaw(RAW(vec)[i]));		\
	    return;						\
	case VECSXP:						\
	    if (XLENGTH(vec) <= i) break;			\
	    SEXP elt = VECTOR_ELT(vec, i);			\
	    RAISE_NAMED(elt, NAMED(vec));			\
	    if (subset2)					\
		SETSTACK_PTR(sv, elt);				\
	    else						\
		SETSTACK_PTR(sv, mkVector1(elt));		\
	    return;						\
	}							\
    } while (0)

#define FAST_VECELT_OK(vec) \
    (ATTRIB(vec) == R_NilValue ||		\
     (TAG(ATTRIB(vec)) == R_DimSymbol &&	\
      CDR(ATTRIB(vec)) == R_NilValue))

static R_INLINE void VECSUBSET_PTR(R_bcstack_t *sx, R_bcstack_t *si,
				   R_bcstack_t *sv, SEXP rho,
				   SEXP consts, int callidx,
				   Rboolean subset2)
{
    SEXP idx, args, value;
    SEXP vec = GETSTACK_PTR(sx);
    R_xlen_t i = bcStackIndex(si) - 1;

    if (i >= 0 && (subset2 || FAST_VECELT_OK(vec)))
	DO_FAST_VECELT(sv, vec, i, subset2);

    /* fall through to the standard default handler */
    idx = GETSTACK_PTR(si);
    args = CONS_NR(idx, R_NilValue);
    args = CONS_NR(vec, args);
    PROTECT(args);
    SEXP call = callidx < 0 ? consts : VECTOR_ELT(consts, callidx);
    if (subset2)
	value = do_subset2_dflt(call, R_Subset2Sym, args, rho);
    else
	value = do_subset_dflt(call, R_SubsetSym, args, rho);
    UNPROTECT(1);
    SETSTACK_PTR(sv, value);
}

#define DO_VECSUBSET(rho, sub2) do {					\
	int callidx = GETOP();						\
	VECSUBSET_PTR(R_BCNodeStackTop - 2, R_BCNodeStackTop - 1,	\
		      R_BCNodeStackTop - 2, rho,			\
		      constants, callidx, sub2);			\
	R_BCNodeStackTop--;						\
	R_Visible = TRUE;						\
    } while(0)

static R_INLINE SEXP getMatrixDim(SEXP mat)
{
    SEXP attr = ATTRIB(mat);
    /* look for the common case of 'dim' as the only attribute first */
    SEXP dim = TAG(attr) == R_DimSymbol ? CAR(attr) :
	getAttrib(mat, R_DimSymbol);
    if (TYPEOF(dim) == INTSXP && LENGTH(dim) == 2)
	return dim;
    else return R_NilValue;
}

static R_INLINE SEXP getArrayDim(SEXP mat)
{
    SEXP attr = ATTRIB(mat);
    /* look for the common case of 'dim' as the only attribute first */
    SEXP dim = TAG(attr) == R_DimSymbol ? CAR(attr) :
	getAttrib(mat, R_DimSymbol);
    if (TYPEOF(dim) == INTSXP && LENGTH(dim) > 0)
	return dim;
    else return R_NilValue;
}

static R_INLINE R_xlen_t colMajorStackIndex(SEXP dim, int rank, R_bcstack_t *si)
{
    if (rank != LENGTH(dim))
    return -1;

    int *idim = INTEGER(dim);

    R_xlen_t mul = idim[0];
    R_xlen_t idx = bcStackIndex(si);

    if (idx < 1 || idx > idim[0])
	return -1;

    R_xlen_t k = idx - 1;
    for (int i = 1; i < rank; i++) {
	idx = bcStackIndex(si + i);
	if (idx < 1 || idx > idim[i])
	    return -1;
	k = k + mul * (idx - 1);
	mul = mul * idim[i];
    }
    return k;
}

static R_INLINE void MATSUBSET_PTR(R_bcstack_t *sx,
				   R_bcstack_t *si, R_bcstack_t *sj,
				   R_bcstack_t *sv, SEXP rho,
				   SEXP consts, int callidx,
				   Rboolean subset2)
{
    SEXP idx, jdx, args, value;
    SEXP mat = GETSTACK_PTR(sx);

    if (subset2 || FAST_VECELT_OK(mat)) {
	SEXP dim = getMatrixDim(mat);
	if (dim != R_NilValue) {
	    R_xlen_t i = bcStackIndex(si);
	    R_xlen_t j = bcStackIndex(sj);
	    R_xlen_t nrow = INTEGER(dim)[0];
	    R_xlen_t ncol = INTEGER(dim)[1];
	    if (i > 0 && j > 0 && i <= nrow && j <= ncol) {
		R_xlen_t k = i - 1 + nrow * (j - 1);
		DO_FAST_VECELT(sv, mat, k, subset2);
	    }
	}
    }

    /* fall through to the standard default handler */
    idx = GETSTACK_PTR(si);
    jdx = GETSTACK_PTR(sj);
    args = CONS_NR(jdx, R_NilValue);
    args = CONS_NR(idx, args);
    args = CONS_NR(mat, args);
    PROTECT(args);
    SEXP call = callidx < 0 ? consts : VECTOR_ELT(consts, callidx);
    if (subset2)
	value = do_subset2_dflt(call, R_Subset2Sym, args, rho);
    else
	value = do_subset_dflt(call, R_SubsetSym, args, rho);
    UNPROTECT(1);
    SETSTACK_PTR(sv, value);
}

#define DO_MATSUBSET(rho, sub2) do {					\
	int callidx = GETOP();						\
	MATSUBSET_PTR(R_BCNodeStackTop - 3,				\
		      R_BCNodeStackTop - 2, R_BCNodeStackTop - 1,	\
		      R_BCNodeStackTop - 3, rho,			\
		      constants, callidx, sub2);			\
	R_BCNodeStackTop -= 2;						\
	R_Visible = TRUE;						\
    } while (0)

static R_INLINE SEXP addStackArgsList(int n, R_bcstack_t *start, SEXP val)
{
    R_bcstack_t *p = start + n - 1;
    BCNPUSH(val); /* to protect */
    for (int i = 0; i < n; i++, p--) {
	val = CONS_NR(GETSTACK_PTR(p), val);
	SETSTACK(-1, val); /* to protect */
    }
    BCNPOP_IGNORE_VALUE();
    return val;
}

static R_INLINE SEXP getStackArgsList(int n, R_bcstack_t *start)
{
    return addStackArgsList(n, start, R_NilValue);
}

static R_INLINE void SUBSET_N_PTR(R_bcstack_t *sx, int rank,
				  R_bcstack_t *si, R_bcstack_t *sv,
				  SEXP rho, SEXP consts, int callidx,
				  Rboolean subset2)
{
    SEXP args, value;
    SEXP x = GETSTACK_PTR(sx);

    if (subset2 || FAST_VECELT_OK(x)) {
	SEXP dim = getArrayDim(x);
	if (dim != R_NilValue) {
	    R_xlen_t k = colMajorStackIndex(dim, rank, si);
	    if (k >= 0)
		DO_FAST_VECELT(sv, x, k, subset2);
	}
    }

    /* fall through to the standard default handler */
    PROTECT(args = CONS_NR(x, getStackArgsList(rank, si)));
    SEXP call = callidx < 0 ? consts : VECTOR_ELT(consts, callidx);
    if (subset2)
	value = do_subset2_dflt(call, R_Subset2Sym, args, rho);
    else
	value = do_subset_dflt(call, R_SubsetSym, args, rho);
    UNPROTECT(1);
    SETSTACK_PTR(sv, value);
}

#define DO_SUBSET_N(rho, sub2) do {					\
	int callidx = GETOP();						\
	int rank = GETOP();						\
	SUBSET_N_PTR(R_BCNodeStackTop - rank - 1, rank,			\
		     R_BCNodeStackTop - rank,				\
		     R_BCNodeStackTop - rank - 1, rho,			\
		     constants, callidx, sub2);				\
	R_BCNodeStackTop -= rank;					\
	R_Visible = TRUE;						\
    } while (0)

static R_INLINE Rboolean setElementFromScalar(SEXP vec, R_xlen_t i, int typev,
					      scalar_value_t *v)
{
    if (i < 0) return FALSE;

    if (TYPEOF(vec) == REALSXP) {
	if (XLENGTH(vec) <= i) return FALSE;
	switch(typev) {
	case REALSXP: REAL(vec)[i] = v->dval; return TRUE;
	case INTSXP: REAL(vec)[i] = INTEGER_TO_REAL(v->ival); return TRUE;
	case LGLSXP: REAL(vec)[i] = LOGICAL_TO_REAL(v->ival); return TRUE;
	}
    }
    else if (typev == TYPEOF(vec)) {
	switch(typev) {
	case INTSXP:
	    if (XLENGTH(vec) <= i) return FALSE;
	    INTEGER(vec)[i] = v->ival;
	    return TRUE;
	case LGLSXP:
	    if (XLENGTH(vec) <= i) return FALSE;
	    LOGICAL(vec)[i] = INTEGER_TO_LOGICAL(v->ival);
	    return TRUE;
	}
    }
    return FALSE;
}

#define DO_FAST_SETVECELT(sv, srhs, vec,  i, subset2) do {		\
	scalar_value_t v;						\
	int typev = bcStackScalar(srhs, &v);				\
	if (setElementFromScalar(vec, i, typev, &v)) {			\
	    SETSTACK_PTR(sv, vec);					\
	    SETTER_CLEAR_NAMED(vec);					\
	    return;							\
	}								\
	else if (subassign2 && TYPEOF(vec) == VECSXP &&			\
		 i < XLENGTH(vec)) {					\
	    SEXP rhs = R_FixupRHS(vec, GETSTACK_PTR(srhs));		\
	    if (rhs != R_NilValue) {					\
		SET_VECTOR_ELT(vec, i, rhs);				\
		SETTER_CLEAR_NAMED(vec);				\
		SETSTACK_PTR(sv, vec);					\
		return;							\
	    }								\
	}								\
    } while (0)

static R_INLINE void VECSUBASSIGN_PTR(R_bcstack_t *sx, R_bcstack_t *srhs,
				      R_bcstack_t *si, R_bcstack_t *sv,
				      SEXP rho, SEXP consts, int callidx,
				      Rboolean subassign2)
{
    SEXP idx, args, value;
    SEXP vec = GETSTACK_PTR(sx);

    if (MAYBE_SHARED(vec)) {
	vec = duplicate(vec);
	SETSTACK_PTR(sx, vec);
    }

    R_xlen_t i = bcStackIndex(si) - 1;
    if (i >= 0)
	DO_FAST_SETVECELT(sv, srhs, vec,  i, subset2);

    /* fall through to the standard default handler */
    value = GETSTACK_PTR(srhs);
    idx = GETSTACK_PTR(si);
    args = CONS_NR(value, R_NilValue);
    SET_TAG(args, R_valueSym);
    args = CONS_NR(idx, args);
    args = CONS_NR(vec, args);
    PROTECT(args);
    SEXP call = callidx < 0 ? consts : VECTOR_ELT(consts, callidx);
    if (subassign2)
	vec = do_subassign2_dflt(call, R_Subassign2Sym, args, rho);
    else
	vec = do_subassign_dflt(call, R_SubassignSym, args, rho);
    UNPROTECT(1);
    SETSTACK_PTR(sv, vec);
}

#define DO_VECSUBASSIGN(rho, sub2) do {					\
	int callidx = GETOP();						\
	VECSUBASSIGN_PTR(R_BCNodeStackTop - 3, R_BCNodeStackTop - 2,	\
			 R_BCNodeStackTop - 1, R_BCNodeStackTop - 3,	\
			 rho, constants, callidx, sub2);		\
	R_BCNodeStackTop -= 2;						\
    } while (0)

static R_INLINE void MATSUBASSIGN_PTR(R_bcstack_t *sx, R_bcstack_t *srhs,
				      R_bcstack_t *si, R_bcstack_t *sj,
				      R_bcstack_t *sv,
				      SEXP rho, SEXP consts, int callidx,
				      Rboolean subassign2)
{
    SEXP dim, idx, jdx, args, value;
    SEXP mat = GETSTACK_PTR(sx);

    if (MAYBE_SHARED(mat)) {
	mat = duplicate(mat);
	SETSTACK_PTR(sx, mat);
    }

    dim = getMatrixDim(mat);

    if (dim != R_NilValue) {
	R_xlen_t i = bcStackIndex(si);
	R_xlen_t j = bcStackIndex(sj);
	R_xlen_t nrow = INTEGER(dim)[0];
	R_xlen_t ncol = INTEGER(dim)[1];
	if (i > 0 && j > 0 && i <= nrow && j <= ncol) {
	    R_xlen_t k = i - 1 + nrow * (j - 1);
	    DO_FAST_SETVECELT(sv, srhs, mat,  k, subset2);
	}
    }

    /* fall through to the standard default handler */
    value = GETSTACK_PTR(srhs);
    idx = GETSTACK_PTR(si);
    jdx = GETSTACK_PTR(sj);
    args = CONS_NR(value, R_NilValue);
    SET_TAG(args, R_valueSym);
    args = CONS_NR(jdx, args);
    args = CONS_NR(idx, args);
    args = CONS_NR(mat, args);
    PROTECT(args);
    SEXP call = callidx < 0 ? consts : VECTOR_ELT(consts, callidx);
    if (subassign2)
	mat = do_subassign2_dflt(call, R_Subassign2Sym, args, rho);
    else
	mat = do_subassign_dflt(call, R_SubassignSym, args, rho);
    UNPROTECT(1);
    SETSTACK_PTR(sv, mat);
}

#define DO_MATSUBASSIGN(rho, sub2) do {					\
	int callidx = GETOP();						\
	MATSUBASSIGN_PTR(R_BCNodeStackTop - 4, R_BCNodeStackTop - 3,	\
			 R_BCNodeStackTop - 2, R_BCNodeStackTop - 1,	\
			 R_BCNodeStackTop - 4,				\
			 rho, constants, callidx, sub2);		\
	R_BCNodeStackTop -= 3;						\
    } while (0)

static R_INLINE void SUBASSIGN_N_PTR(R_bcstack_t *sx, int rank,
				     R_bcstack_t *srhs,
				     R_bcstack_t *si, R_bcstack_t *sv,
				     SEXP rho, SEXP consts, int callidx,
				     Rboolean subassign2)
{
    SEXP dim, args, value;
    SEXP x = GETSTACK_PTR(sx);

    if (MAYBE_SHARED(x)) {
	x = duplicate(x);
	SETSTACK_PTR(sx, x);
    }

    dim = getArrayDim(x);

    if (dim != R_NilValue) {
	R_xlen_t k = colMajorStackIndex(dim, rank, si);
	if (k >= 0)
	    DO_FAST_SETVECELT(sv, srhs, x,  k, subset2);
    }

    /* fall through to the standard default handler */
    value = GETSTACK_PTR(srhs);
    args = CONS_NR(value, R_NilValue);
    SET_TAG(args, R_valueSym);
    PROTECT(args = CONS_NR(x, addStackArgsList(rank, si, args)));
    SEXP call = callidx < 0 ? consts : VECTOR_ELT(consts, callidx);
    if (subassign2)
	x = do_subassign2_dflt(call, R_Subassign2Sym, args, rho);
    else
	x = do_subassign_dflt(call, R_SubassignSym, args, rho);
    UNPROTECT(1);
    SETSTACK_PTR(sv, x);
}

#define DO_SUBASSIGN_N(rho, sub2) do {					\
	int callidx = GETOP();						\
	int rank = GETOP();						\
	SUBASSIGN_N_PTR(R_BCNodeStackTop - rank - 2, rank,		\
			R_BCNodeStackTop - rank - 1,			\
			R_BCNodeStackTop - rank,			\
			R_BCNodeStackTop - rank - 2, rho,		\
			constants, callidx, sub2);			\
	R_BCNodeStackTop -= rank + 1;					\
    } while (0)

#define FIXUP_SCALAR_LOGICAL(callidx, arg, op, warn_level) do {		\
	SEXP val = GETSTACK(-1);					\
	if (IS_SIMPLE_SCALAR(val, LGLSXP))				\
	    SETSTACK(-1, ScalarLogical(SCALAR_LVAL(val)));		\
	else {								\
	    if (!isNumber(val))						\
		errorcall(VECTOR_ELT(constants, callidx),		\
			  _("invalid %s type in 'x %s y'"), arg, op);	\
	    SETSTACK(-1, ScalarLogical(asLogical2(			\
					   val, warn_level,		\
					   VECTOR_ELT(constants, callidx)))); \
	}								\
    } while(0)

static void signalMissingArgError(SEXP args, SEXP call)
{
    SEXP a, c;
    int n, k;
    for (a = args, n = 1; a != R_NilValue; a = CDR(a), n++)
	if (CAR(a) == R_MissingArg) {
	    /* check for an empty argument in the call -- start from
	       the beginning in case of ... arguments */
	    if (call != R_NilValue) {
		for (k = 1, c = CDR(call); c != R_NilValue; c = CDR(c), k++)
		    if (CAR(c) == R_MissingArg)
			errorcall(call, "argument %d is empty", k);
	    }
	    /* An error from evaluating a symbol will already have
	       been signaled.  The interpreter, in evalList, does
	       _not_ signal an error for a call expression that
	       produces an R_MissingArg value; for example

		   c(alist(a=)$a)

	       does not signal an error. If we decide we do want an
	       error in this case we can modify evalList for the
	       interpreter and here use the code below. */
#ifdef NO_COMPUTED_MISSINGS
	    /* otherwise signal a 'missing argument' error */
	    errorcall(call, "argument %d is missing", n);
#endif
	}
}

static R_INLINE void checkForMissings(SEXP args, SEXP call)
{
    Rboolean found = FALSE;
    for (SEXP a = args; a != R_NilValue; a = CDR(a))
	if (CAR(a) == R_MissingArg) {
	    found = TRUE;
	    break;
	}
    if (found)
	signalMissingArgError(args, call);
}

#define FOR_LOOP_STATE_SIZE 4

#define GET_VEC_LOOP_VALUE(var, pos) do {		\
    (var) = GETSTACK(pos);				\
    if (MAYBE_SHARED(var)) {				\
	(var) = allocVector(TYPEOF(seq), 1);		\
	SETSTACK(pos, var);				\
	INCREMENT_NAMED(var);				\
    }							\
} while (0)

/* Loops that cannot have their SETJMPs optimized out are bracketed by
   STARTLOOPCNTXT and ENLOOPCNTXT instructions.  The STARTLOOPCNTXT
   instruction stores the target offset for a 'break' and then the
   target offset for a 'next' on the stack. For a 'for' loop the loop
   state information is then pushed on the stack as well. The
   following functions retrieve the offsets. */

static R_INLINE int LOOP_BREAK_OFFSET(int loop_state_size)
{
    return GETSTACK_IVAL_PTR(R_BCNodeStackTop - 2 - loop_state_size);
}

static R_INLINE int LOOP_NEXT_OFFSET(int loop_state_size)
{
    return GETSTACK_IVAL_PTR(R_BCNodeStackTop - 1 - loop_state_size);
}

/* Check whether a call is to a base function; if not use AST interpeter */
/***** need a faster guard check */
static R_INLINE SEXP SymbolValue(SEXP sym)
{
    if (IS_ACTIVE_BINDING(sym))
	return eval(sym, R_BaseEnv);
    else {
	SEXP value = SYMVALUE(sym);
	if (TYPEOF(value) == PROMSXP) {
	    value = PRVALUE(value);
	    if (value == R_UnboundValue)
		value = eval(sym, R_BaseEnv);
	}
	return value;
    }
}

#define DO_BASEGUARD() do {				\
	SEXP expr = VECTOR_ELT(constants, GETOP());	\
	int label = GETOP();				\
	SEXP sym = CAR(expr);				\
	if (findFun(sym, rho) != SymbolValue(sym)) {	\
	    BCNPUSH(eval(expr, rho));			\
	    pc = codebase + label;			\
	}						\
    } while (0)

/* The CALLBUILTIN instruction handles calls to both true BUILTINs and
   to .Internals of type BUILTIN. To handle profiling in a way that is
   consistent with this instruction needs to be able to distinguish a
   true BUILTIN from a .Internal. LT */
#define IS_TRUE_BUILTIN(x) ((R_FunTab[PRIMOFFSET(x)].eval % 100 )/10 == 0)

/* rho only needed for _R_CHECK_LENGTH_1_CONDITION_=package:name */
static R_INLINE Rboolean GETSTACK_LOGICAL_NO_NA_PTR(R_bcstack_t *s, int callidx,
						    SEXP constants, SEXP rho)
{
#ifdef TYPED_STACK
    if (s->tag == LGLSXP && s->u.ival != NA_LOGICAL)
	return s->u.ival;
#endif
    SEXP value = GETSTACK_PTR(s);
    if (IS_SCALAR(value, LGLSXP)) {
	Rboolean lval = SCALAR_LVAL(value);
	if (lval != NA_LOGICAL)
	    return lval;
    }
    SEXP call = VECTOR_ELT(constants, callidx);
    return asLogicalNoNA(value, call, rho);
}

/* Find locations table in the constant pool */
static SEXP findLocTable(SEXP constants, const char *tclass)
{
    int i;
    /* location tables are at the end of the constant pool */
    for(i = LENGTH(constants) - 1; i >= 0 ; i--) {
	SEXP s = VECTOR_ELT(constants, i);
	/* could use exact check instead of inherits */
	if (TYPEOF(s) == INTSXP && inherits(s, tclass))
	    return s;
    }
    return R_NilValue;
}

/* Get a constant pool entry through locations table element */
static SEXP getLocTableElt(ptrdiff_t relpc, SEXP table, SEXP constants)
{
    if (table == R_NilValue || relpc >= LENGTH(table) || relpc < 0)
	return R_NilValue;

    int cidx = INTEGER(table)[relpc];
    if (cidx < 0 || cidx >= LENGTH(constants))
	return R_NilValue;
    return VECTOR_ELT(constants, cidx);
}

/* Return the srcref/expression for the current instruction/operand
   being executed by the byte-code interpreter, or the one that was
   current when the supplied context was created. */
static SEXP R_findBCInterpreterLocation(RCNTXT *cptr, const char *iname)
{
    SEXP body = cptr ? cptr->bcbody : R_BCbody;
    if (body == NULL)
	/* This has happened, but it is not clear how. */
	/* (R_Srcref == R_InBCInterpreter && R_BCbody == NULL) */
	return R_NilValue;
    SEXP constants = BCCONSTS(body);
    SEXP ltable = findLocTable(constants, iname);
    if (ltable == R_NilValue)
	/* location table not available */
	return R_NilValue;

    BCODE *codebase = BCCODE(body);
    ptrdiff_t relpc = (*((BCODE **)(cptr ? cptr->bcpc : R_BCpc))) - codebase;

    return getLocTableElt(relpc, ltable, constants);
}

SEXP attribute_hidden R_findBCInterpreterSrcref(RCNTXT *cptr)
{
    return R_findBCInterpreterLocation(cptr, "srcrefsIndex");
}

static SEXP R_findBCInterpreterExpression()
{
    return R_findBCInterpreterLocation(NULL, "expressionsIndex");
}

SEXP attribute_hidden R_getCurrentSrcref()
{
    if (R_Srcref != R_InBCInterpreter)
	return R_Srcref;
    else
	return R_findBCInterpreterSrcref(NULL);
}

static Rboolean maybeClosureWrapper(SEXP expr)
{
    if (TYPEOF(expr) != LANGSXP)
	return FALSE;

    SEXP sym = CAR(expr);

    if (!(sym == R_DotInternalSym || sym == R_DotExternalSym ||
	sym == R_DotExternal2Sym || sym == R_DotExternalgraphicsSym ||
	sym == R_DotCallSym || sym == R_DotFortranSym ||
	sym == R_DotCSym || sym == R_DotCallgraphicsSym))

	return FALSE;

    return CDR(expr) != R_NilValue && CADR(expr) != R_NilValue;
}

static Rboolean maybeAssignmentCall(SEXP expr)
{
    if (TYPEOF(expr) != LANGSXP)
	return FALSE;

    if (TYPEOF(CAR(expr)) != SYMSXP)
	return FALSE;
    const char *name = CHAR(PRINTNAME(CAR(expr)));
    size_t slen = strlen(name);
    return slen > 2 && name[slen-2] == '<' && name[slen-1] == '-';
}

/* Check if the given expression is a call to a name that is also
   a builtin or special (does not search the environment!). */
static Rboolean maybePrimitiveCall(SEXP expr)
{
    if (TYPEOF(expr) != LANGSXP)
	return FALSE;

    if (TYPEOF(CAR(expr)) == SYMSXP) {
	SEXP value = SYMVALUE(CAR(expr));
	if (TYPEOF(value) == PROMSXP)
	    value = PRVALUE(value);
	return TYPEOF(value) == BUILTINSXP || TYPEOF(value) == SPECIALSXP;
    }
    return FALSE;
}

/* Inflate a (single-level) compiler-flattenned assignment call.
   For example,
           `[<-`(x, c(-1, 1), value = 2)
   becomes
            x[c(-1,1)] <- 2 */
static SEXP inflateAssignmentCall(SEXP expr) {
    if (CDR(expr) == R_NilValue || CDDR(expr) == R_NilValue)
	return expr; /* need at least two arguments */

    SEXP assignForm = CAR(expr);
    if (TYPEOF(assignForm) != SYMSXP)
	return expr;
    const char *name = CHAR(PRINTNAME(assignForm));
    size_t slen = strlen(name);
    if (slen <= 2 || name[slen - 2] != '<' || name[slen - 1] != '-')
	return expr;

    // gcc 8 warns here, but this is intentional
    // ‘strncpy’ specified bound depends on the length of the source argument
    char nonAssignName[slen - 1]; /* "names" for "names<-" */
    strncpy(nonAssignName, name, slen - 2);
    nonAssignName[slen - 2] = '\0';
    SEXP nonAssignForm = install(nonAssignName);

    int nargs = length(expr) - 2;
    SEXP lhs = allocVector(LANGSXP, nargs + 1);
    SETCAR(lhs, nonAssignForm);

    SEXP porig = CDR(expr);
    SEXP pnew = CDR(lhs);

    /* copy args except the last - the "value" */
    while(CDR(porig) != R_NilValue) {
	SETCAR(pnew, CAR(porig));
	ENSURE_NAMEDMAX(CAR(porig));
	porig = CDR(porig);
	pnew = CDR(pnew);
    }
    SEXP rhs = CAR(porig);
    ENSURE_NAMEDMAX(rhs);
    if (TAG(porig) != R_valueSym)
	return expr;
    return lang3(R_AssignSym, lhs, rhs);
}

/* Get the current expression being evaluated by the byte-code interpreter. */
SEXP attribute_hidden R_getBCInterpreterExpression()
{
    SEXP exp = R_findBCInterpreterExpression();
    if (TYPEOF(exp) == PROMSXP) {
	exp = forcePromise(exp);
	ENSURE_NAMEDMAX(exp);
    }

    /* This tries to mimick the behavior of the AST interpreter to a
       reasonable level, based on relatively consistent expressions
       provided by the compiler in the constant pool. The AST
       interpreter behavior is rather inconsistent and should be fixed
       at some point. When this happens, the code below will have to
       be revisited, but the compiler code should mostly stay the
       same.

       Currently this code attempts to bypass implementation of
       closure wrappers for internals and other foreign functions
       called via a directive, hide away primitives, but show
       assignment calls. This code ignores less usual problematic
       situations such as overriding of builtins or inlining of the
       wrappers by the compiler. Simple assignment calls are inflated
       (back) into the usual form like x[1] <- y. Expressions made of
       a single symbol are hidden away (note these are e.g. for
       missing function arguments). */

    if (maybeAssignmentCall(exp)) {
	exp = inflateAssignmentCall(exp);
    } else if (TYPEOF(exp) == SYMSXP || maybeClosureWrapper(exp)
	|| maybePrimitiveCall(exp)) {

	RCNTXT *c = R_GlobalContext;
        while(c && c->callflag != CTXT_TOPLEVEL) {
	    if (c->callflag & CTXT_FUNCTION) {
		exp = c->call;
		break;
	    }
	    c = c->nextcontext;
	}
    }
    return exp;
}

static SEXP markSpecialArgs(SEXP args)
{
    SEXP arg;
    for(arg = args; arg != R_NilValue; arg = CDR(arg))
	MARK_NOT_MUTABLE(CAR(arg));
    return args;
}

Rboolean attribute_hidden R_BCVersionOK(SEXP s)
{
    if (TYPEOF(s) != BCODESXP)
	return FALSE;

    BCODE *pc = BCCODE(s);
    int version = GETOP();

    /* must be kept in sync with bcEval version check */
    return version < 2 ||
	(version >= R_bcMinVersion && version <= R_bcVersion);
}

static R_INLINE Rboolean FIND_ON_STACK(SEXP x, R_bcstack_t *base, int skip)
{
    /* Check whether the value is on the stack before modifying.  If
       'skip' is true the top value on the stack is ignored. LT */
    R_bcstack_t *checktop = skip ? R_BCNodeStackTop - 1 : R_BCNodeStackTop;
    for (R_bcstack_t *p = base; p < checktop; p++) {
	if (p->tag == RAWMEM_TAG)
	    p += p->u.ival;
	else if (p->u.sxpval == x && p->tag == 0)
	    return TRUE;
    }
    return FALSE;
}

static SEXP bcEval(SEXP body, SEXP rho, Rboolean useCache)
{
  SEXP retvalue = R_NilValue, constants;
  BCODE *pc, *codebase;
  R_bcstack_t *oldntop = R_BCNodeStackTop;
  static int evalcount = 0;
  SEXP oldsrcref = R_Srcref;
  int oldbcintactive = R_BCIntActive;
  SEXP oldbcbody = R_BCbody;
  void *oldbcpc = R_BCpc;
  BCODE *currentpc = NULL;

#ifdef BC_INT_STACK
  IStackval *olditop = R_BCIntStackTop;
#endif
#ifdef BC_PROFILING
  int old_current_opcode = current_opcode;
#endif
#ifdef THREADED_CODE
  int which = 0;
#endif

  BC_CHECK_SIGINT();

  INITIALIZE_MACHINE();
  codebase = pc = BCCODE(body);
  constants = BCCONSTS(body);

  /* allow bytecode to be disabled for testing */
  if (R_disable_bytecode)
      return eval(bytecodeExpr(body), rho);

  /* check version */
  /* must be kept in sync with R_BCVersionOK */
  {
      int version = GETOP();
      if (version < R_bcMinVersion || version > R_bcVersion) {
	  if (version >= 2) {
#ifdef BC_VERSION_MISMATCH_WARNING
	      static Rboolean warned = FALSE;
	      if (! warned) {
		  warned = TRUE;
		  warning(_("bytecode version mismatch; using eval"));
	      }
#endif
	      return eval(bytecodeExpr(body), rho);
	  }
	  else if (version < R_bcMinVersion)
	      error(_("bytecode version is too old"));
	  else error(_("bytecode version is too new"));
      }
  }

  R_Srcref = R_InBCInterpreter;
  R_BCIntActive = 1;
  R_BCbody = body;
  R_BCpc = &currentpc;
  R_binding_cache_t vcache = NULL;
  Rboolean smallcache = TRUE;
  R_bcstack_t *vcache_top = NULL;
#ifdef USE_BINDING_CACHE
  if (useCache) {
      R_len_t n = LENGTH(constants);
# ifdef CACHE_MAX
      if (n > CACHE_MAX) {
	  n = CACHE_MAX;
	  smallcache = FALSE;
      }
# endif
# ifdef CACHE_ON_STACK
      /* initialize binding cache on the stack */
      vcache = R_BCNodeStackTop;
      vcache_top = vcache + n;
      if (R_BCNodeStackTop + n > R_BCNodeStackEnd)
	  nodeStackOverflow();
      while (n > 0) {
	  SETSTACK(0, R_NilValue);
	  R_BCNodeStackTop++;
	  n--;
      }
# else
      /* allocate binding cache and protect on stack */
      vcache = allocVector(VECSXP, n);
      BCNPUSH(vcache);
      vcache_top = R_BCNodeStackTop;
# endif
  }
#endif

  BEGIN_MACHINE {
    OP(BCMISMATCH, 0): error(_("byte code version mismatch"));
    OP(RETURN, 0): retvalue = GETSTACK(-1); goto done;
    OP(GOTO, 1):
      {
	int label = GETOP();
	BC_CHECK_SIGINT();
	pc = codebase + label;
	NEXT();
      }
    OP(BRIFNOT, 2):
      {
	int callidx = GETOP();
	int label = GETOP();
	Rboolean cond = GETSTACK_LOGICAL_NO_NA_PTR(R_BCNodeStackTop - 1,
						   callidx, constants, rho);
	BCNPOP_IGNORE_VALUE();
	if (! cond) {
	    BC_CHECK_SIGINT(); /**** only on back branch?*/
	    pc = codebase + label;
	}
	NEXT();
      }
    OP(POP, 0): BCNPOP_IGNORE_VALUE(); NEXT();
    OP(DUP, 0): BCNDUP(); NEXT();
    OP(PRINTVALUE, 0): PrintValue(BCNPOP()); NEXT();
    OP(STARTLOOPCNTXT, 2):
	{
	    Rboolean is_for_loop = GETOP();
	    R_bcstack_t *oldtop = R_BCNodeStackTop;
	    RCNTXT *cntxt = BCNALLOC_CNTXT();
	    BCNPUSH_INTEGER(GETOP());       /* pc offset for 'break' */
	    BCNPUSH_INTEGER((int)(pc - codebase)); /* pc offset for 'next' */
	    if (is_for_loop) {
		/* duplicate the for loop state data on the top of the stack */
		R_bcstack_t *loopdata = oldtop - FOR_LOOP_STATE_SIZE;
		BCNSTACKCHECK(FOR_LOOP_STATE_SIZE);
		for (int i = 0; i < FOR_LOOP_STATE_SIZE; i++)
		    R_BCNodeStackTop[i] = loopdata[i];
		R_BCNodeStackTop += FOR_LOOP_STATE_SIZE;

		begincontext(cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv,
			     R_NilValue, R_NilValue);
		switch (SETJMP(cntxt->cjmpbuf)) {
		case CTXT_BREAK:
		    pc = codebase + LOOP_BREAK_OFFSET(FOR_LOOP_STATE_SIZE);
		    break;
		case CTXT_NEXT:
		    pc = codebase + LOOP_NEXT_OFFSET(FOR_LOOP_STATE_SIZE);
		    break;
		}
	    }
	    else {
		begincontext(cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv,
			     R_NilValue, R_NilValue);
		switch (SETJMP(cntxt->cjmpbuf)) {
		case CTXT_BREAK:
		    pc = codebase + LOOP_BREAK_OFFSET(0);
		    break;
		case CTXT_NEXT:
		    pc = codebase + LOOP_NEXT_OFFSET(0);
		    break;
		}
	    }
	    /* context, offsets on stack, to be popped by ENDLOOPCNTXT */
	    NEXT();
	}
    OP(ENDLOOPCNTXT, 1):
	{
	    Rboolean is_for_loop = GETOP();
	    if (is_for_loop)
		/* remove the duplicated for loop state data */
		R_BCNodeStackTop -= FOR_LOOP_STATE_SIZE;
	    BCNPOP_IGNORE_VALUE(); /* 'next' target */
	    BCNPOP_IGNORE_VALUE(); /* 'break' target */
	    BCNPOP_AND_END_CNTXT();
	    NEXT();
	}
    OP(DOLOOPNEXT, 0): findcontext(CTXT_NEXT, rho, R_NilValue);
    OP(DOLOOPBREAK, 0): findcontext(CTXT_BREAK, rho, R_NilValue);
    OP(STARTFOR, 3):
      {
	Rboolean iscompact = FALSE;
	SEXP seq = getForLoopSeq(-1, &iscompact);
	int callidx = GETOP();
	SEXP symbol = VECTOR_ELT(constants, GETOP());
	int label = GETOP();

	/* if we are iterating over a factor, coerce to character first */
	if (inherits(seq, "factor")) {
	    seq = asCharacterFactor(seq);
	    SETSTACK(-1, seq);
	}

	defineVar(symbol, R_NilValue, rho);
	BCNPUSH(GET_BINDING_CELL(symbol, rho));

	SEXP value = allocVector(INTSXP, 2);
	int *info = INTEGER0(value);
	info[0] = -1;
#ifdef COMPACT_INTSEQ
	if (iscompact) {
	    int n1 = INTEGER(seq)[0];
	    int n2 = INTEGER(seq)[1];
	    INTEGER(value)[1] = n1 <= n2 ? n2 - n1 + 1 : n1 - n2 + 1;
	}
	else
#endif
	if (isVector(seq))
	  info[1] = LENGTH(seq);
	else if (isList(seq) || isNull(seq))
	  info[1] = length(seq);
	else errorcall(VECTOR_ELT(constants, callidx),
		       _("invalid for() loop sequence"));
	BCNPUSH(value);

	/* bump up links count of seq to avoid modification by loop code */
	INCREMENT_LINKS(seq);

	/* place initial loop variable value object on stack */
	switch(TYPEOF(seq)) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	    value = allocVector(TYPEOF(seq), 1);
	    INCREMENT_NAMED(value);
	    BCNPUSH(value);
	    break;
	default: BCNPUSH(R_NilValue);
	}

	BC_CHECK_SIGINT();
	pc = codebase + label;
	NEXT();
      }
    OP(STEPFOR, 1):
      {
	int label = GETOP();
	int *loopinfo = INTEGER0(GETSTACK_SXPVAL(-2));
	int i = ++loopinfo[0];
	int n = loopinfo[1];
	if (i < n) {
	  Rboolean iscompact = FALSE;
	  SEXP seq = getForLoopSeq(-4, &iscompact);
	  SEXP cell = GETSTACK(-3);
	  SEXP value = NULL;
	  switch (TYPEOF(seq)) {
	  case LGLSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
	    SET_SCALAR_LVAL(value, LOGICAL_ELT(seq, i));
	    break;
	  case INTSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
#ifdef COMPACT_INTSEQ
	    if (iscompact) {
		int *info = INTEGER(seq);
		int n1 = info[0];
		int n2 = info[1];
		int val = n1 <= n2 ? n1 + i : n1 - i;
		INTEGER(value)[0] = val;
	    }
	    else
#endif
	    SET_SCALAR_IVAL(value, INTEGER_ELT(seq, i));
	    break;
	  case REALSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
	    SET_SCALAR_DVAL(value, REAL_ELT(seq, i));
	    break;
	  case CPLXSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
	    SET_SCALAR_CVAL(value, COMPLEX_ELT(seq, i));
	    break;
	  case STRSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
	    SET_STRING_ELT(value, 0, STRING_ELT(seq, i));
	    break;
	  case RAWSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
	    SET_SCALAR_BVAL(value, RAW(seq)[i]);
	    break;
	  case EXPRSXP:
	  case VECSXP:
	    value = VECTOR_ELT(seq, i);
	    ENSURE_NAMEDMAX(value);
	    break;
	  case LISTSXP:
	    value = CAR(seq);
	    SETSTACK(-4, CDR(seq));
	    ENSURE_NAMEDMAX(value);
	    break;
	  default:
	    error(_("invalid sequence argument in for loop"));
	  }
	  if (CAR(cell) == R_UnboundValue || ! SET_BINDING_VALUE(cell, value))
	      defineVar(BINDING_SYMBOL(cell), value, rho);
	  BC_CHECK_SIGINT();
	  pc = codebase + label;
	}
	NEXT();
      }
    OP(ENDFOR, 0):
      {
	Rboolean iscompact = FALSE;
	SEXP seq = getForLoopSeq(-4, &iscompact);
	DECREMENT_LINKS(seq);
	R_BCNodeStackTop -= FOR_LOOP_STATE_SIZE - 1;
	SETSTACK(-1, R_NilValue);
	NEXT();
      }
    OP(SETLOOPVAL, 0):
      BCNPOP_IGNORE_VALUE(); SETSTACK(-1, R_NilValue); NEXT();
    OP(INVISIBLE,0): R_Visible = FALSE; NEXT();
    OP(LDCONST, 1):
      {
	R_Visible = TRUE;
	SEXP value = VECTOR_ELT(constants, GETOP());
	if (R_check_constants < 0)
	    value = duplicate(value);
	MARK_NOT_MUTABLE(value);
	BCNPUSH(value);
	NEXT();
      }
    OP(LDNULL, 0): R_Visible = TRUE; BCNPUSH(R_NilValue); NEXT();
    OP(LDTRUE, 0): R_Visible = TRUE; BCNPUSH(R_TrueValue); NEXT();
    OP(LDFALSE, 0): R_Visible = TRUE; BCNPUSH(R_FalseValue); NEXT();
    OP(GETVAR, 1): DO_GETVAR(FALSE, FALSE);
    OP(DDVAL, 1): DO_GETVAR(TRUE, FALSE);
    OP(SETVAR, 1):
      {
	int sidx = GETOP();
	SEXP loc;
	if (smallcache)
	    loc = GET_SMALLCACHE_BINDING_CELL(vcache, sidx);
	else {
	    SEXP symbol = VECTOR_ELT(constants, sidx);
	    loc = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	}
#ifdef TYPED_STACK
	R_bcstack_t *s = R_BCNodeStackTop - 1;
	/* reading the locked bit is OK even if cell is R_NilValue */
	if (s->tag && ! BINDING_IS_LOCKED(loc)) {
	    /* if cell is R_NilValue or an active binding, or if the value
	       is R_UnboundValue, then TYPEOF(CAR(cell)) will not match the
	       immediate value tag. */
	    SEXP x = CAR(loc);  /* fast, but assumes binding is a CONS */
	    if (NOT_SHARED(x) && IS_SIMPLE_SCALAR(x, s->tag)) {
		/* if the binding value is not shared and is a simple
		   scalar of the same type as the immediate value,
		   then we can copy the stack value into the binding
		   value */

#define MAX_ON_STACK_CHECK 63
		/* Check whether the value is on the stack before
		   modifying.  Limit the number of items to check to
		   MAX_ON_STACK_CHECK; this will result in some
		   defensive boxing. This number could be tuned; in
		   some limited testing ncheck never went above
		   36. Not worrying BCNALLOC stuff could result in
		   false positives and unnecessary boxing but is
		   probably worth it for avoiding checking and
		   branching. LT */
		int tag = s->tag;
		if (R_BCNodeStackTop - vcache_top > MAX_ON_STACK_CHECK ||
		    FIND_ON_STACK(x, vcache_top, TRUE))
		    tag = 0;		

		switch (tag) {
		case REALSXP: SET_SCALAR_DVAL(x, s->u.dval); NEXT();
		case INTSXP: SET_SCALAR_IVAL(x, s->u.ival); NEXT();
		case LGLSXP: SET_SCALAR_LVAL(x, s->u.ival); NEXT();
		}
	    }
	}
#endif
	SEXP value = GETSTACK(-1);
	INCREMENT_NAMED(value);
	if (! SET_BINDING_VALUE(loc, value)) {
	    SEXP symbol = VECTOR_ELT(constants, sidx);
	    PROTECT(value);
	    defineVar(symbol, value, rho);
	    UNPROTECT(1);
	}
	NEXT();
      }
    OP(GETFUN, 1):
      {
	/* get the function */
	SEXP symbol = VECTOR_ELT(constants, GETOP());
	SEXP value = findFun(symbol, rho);
	INIT_CALL_FRAME(value);
	if(RTRACE(value)) {
	  Rprintf("trace: ");
	  PrintValue(symbol);
	}
	NEXT();
      }
    OP(GETGLOBFUN, 1):
      {
	/* get the function */
	SEXP symbol = VECTOR_ELT(constants, GETOP());
	SEXP value = findFun(symbol, R_GlobalEnv);
	INIT_CALL_FRAME(value);
	if(RTRACE(value)) {
	  Rprintf("trace: ");
	  PrintValue(symbol);
	}
	NEXT();
      }
    OP(GETSYMFUN, 1):
      {
	/* get the function */
	SEXP symbol = VECTOR_ELT(constants, GETOP());
	SEXP value = SYMVALUE(symbol);
	if (TYPEOF(value) == PROMSXP) {
	    value = forcePromise(value);
	    ENSURE_NAMEDMAX(value);
	}
	if(RTRACE(value)) {
	  Rprintf("trace: ");
	  PrintValue(symbol);
	}
	INIT_CALL_FRAME(value);
	NEXT();
      }
    OP(GETBUILTIN, 1):
      {
	/* get the function */
	SEXP symbol = VECTOR_ELT(constants, GETOP());
	SEXP value = getPrimitive(symbol, BUILTINSXP);
//#define REPORT_OVERRIDEN_BUILTINS
#ifdef REPORT_OVERRIDEN_BUILTINS
	if (value != findFun(symbol, rho)) {
	    Rprintf("Possibly overriden builtin: %s\n", PRIMNAME(value));
	}
#endif
	if (RTRACE(value)) {
	  Rprintf("trace: ");
	  PrintValue(symbol);
	}
	INIT_CALL_FRAME(value);
	NEXT();
      }
    OP(GETINTLBUILTIN, 1):
      {
	/* get the function */
	SEXP symbol = VECTOR_ELT(constants, GETOP());
	SEXP value = INTERNAL(symbol);
	if (TYPEOF(value) != BUILTINSXP)
	  error(_("there is no .Internal function '%s'"),
		CHAR(PRINTNAME(symbol)));
	INIT_CALL_FRAME(value);
	NEXT();
      }
    OP(CHECKFUN, 0):
      {
	/* check then the value on the stack is a function */
	SEXP value = GETSTACK(-1);
	if (TYPEOF(value) != CLOSXP && TYPEOF(value) != BUILTINSXP &&
	    TYPEOF(value) != SPECIALSXP)
	  error(_("attempt to apply non-function"));
	INIT_CALL_FRAME_ARGS();
	NEXT();
      }
    OP(MAKEPROM, 1):
      {
	SEXP code = VECTOR_ELT(constants, GETOP());
	SEXPTYPE ftype = CALL_FRAME_FTYPE();
	if (ftype != SPECIALSXP) {
	  SEXP value;
	  if (ftype == BUILTINSXP) {
	    if (TYPEOF(code) == BCODESXP) 
	      value = bcEval(code, rho, TRUE);
	    else
	      /* uncommon but possible, the compiler may decide not to compile
	         an argument expression */
	      value = eval(code, rho);
	  } else
	    value = mkPROMISE(code, rho);
	  PUSHCALLARG(value);
	}
	NEXT();
      }
    OP(DOMISSING, 0):
      {
	SEXPTYPE ftype = CALL_FRAME_FTYPE();
	if (ftype != SPECIALSXP)
	  PUSHCALLARG(R_MissingArg);
	NEXT();
      }
    OP(SETTAG, 1):
      {
	SEXPTYPE ftype = CALL_FRAME_FTYPE();
	int tagidx = GETOP();
	if (ftype != SPECIALSXP) {
	    SEXP tag = VECTOR_ELT(constants, tagidx);
	    SETCALLARG_TAG(tag);
	}
	NEXT();
      }
    OP(DODOTS, 0):
      {
	SEXPTYPE ftype = CALL_FRAME_FTYPE();
	if (ftype != SPECIALSXP) {
	  SEXP h = findVar(R_DotsSymbol, rho);
	  if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
	    PROTECT(h);
	    for (; h != R_NilValue; h = CDR(h)) {
	      SEXP val;
	      if (ftype == BUILTINSXP)
	        val = eval(CAR(h), rho);
	      else if (TYPEOF(CAR(h)) == PROMSXP || CAR(h) == R_MissingArg)
	        val = CAR(h);
	      else
	        val = mkPROMISE(CAR(h), rho);
	      PUSHCALLARG(val);
	      SETCALLARG_TAG(TAG(h));
	    }
	    UNPROTECT(1); /* h */
	  }
	  else if (h != R_MissingArg)
	    error(_("'...' used in an incorrect context"));
	}
	NEXT();
      }
    OP(PUSHARG, 0): PUSHCALLARG(BCNPOP()); NEXT();
    OP(PUSHCONSTARG, 1):
      {
	SEXP value = VECTOR_ELT(constants, GETOP());
	if (R_check_constants < 0)
	    value = duplicate(value);
	MARK_NOT_MUTABLE(value);
	PUSHCALLARG(value);
	NEXT();
      }
    OP(PUSHNULLARG, 0): PUSHCALLARG(R_NilValue); NEXT();
    OP(PUSHTRUEARG, 0): PUSHCALLARG(R_TrueValue); NEXT();
    OP(PUSHFALSEARG, 0): PUSHCALLARG(R_FalseValue); NEXT();
    OP(CALL, 1):
      {
	SEXP fun = CALL_FRAME_FUN();
	SEXP call = VECTOR_ELT(constants, GETOP());
	SEXP args;
	SEXP value = NULL;
	int flag;
	switch (TYPEOF(fun)) {
	case BUILTINSXP:
	  args = BUILTIN_CALL_FRAME_ARGS();
	  checkForMissings(args, call);
	  flag = PRIMPRINT(fun);
	  R_Visible = flag != 1;
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  if (flag < 2) R_Visible = flag != 1;
	  break;
	case SPECIALSXP:
	  flag = PRIMPRINT(fun);
	  R_Visible = flag != 1;
	  value = PRIMFUN(fun) (call, fun, markSpecialArgs(CDR(call)), rho);
	  if (flag < 2) R_Visible = flag != 1;
	  break;
	case CLOSXP:
	  args = CLOSURE_CALL_FRAME_ARGS();
	  value = applyClosure(call, fun, args, rho, R_NilValue);
#ifdef ADJUST_ENVIR_REFCNTS
	  unpromiseArgs(args);
#endif
	  break;
	default: error(_("bad function"));
	}
	POP_CALL_FRAME(value);
	NEXT();
      }
    OP(CALLBUILTIN, 1):
      {
	SEXP fun = CALL_FRAME_FUN();
	SEXP call = VECTOR_ELT(constants, GETOP());
	SEXP args = BUILTIN_CALL_FRAME_ARGS();
	int flag;
	const void *vmax = vmaxget();
	if (TYPEOF(fun) != BUILTINSXP)
	  error(_("not a BUILTIN function"));
	flag = PRIMPRINT(fun);
	R_Visible = flag != 1;
	SEXP value;
	if (R_Profiling && IS_TRUE_BUILTIN(fun)) {
	    RCNTXT cntxt;
	    SEXP oldref = R_Srcref;
	    begincontext(&cntxt, CTXT_BUILTIN, call,
			 R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
	    R_Srcref = NULL;
	    value = PRIMFUN(fun) (call, fun, args, rho);
	    R_Srcref = oldref;
	    endcontext(&cntxt);
	} else {
	    value = PRIMFUN(fun) (call, fun, args, rho);
	}
	if (flag < 2) R_Visible = flag != 1;
	vmaxset(vmax);
	POP_CALL_FRAME(value);
	NEXT();
      }
    OP(CALLSPECIAL, 1):
      {
	SEXP call = VECTOR_ELT(constants, GETOP());
	SEXP symbol = CAR(call);
	SEXP fun = getPrimitive(symbol, SPECIALSXP);
	int flag;
	const void *vmax = vmaxget();
	if (RTRACE(fun)) {
	  Rprintf("trace: ");
	  PrintValue(symbol);
	}
	flag = PRIMPRINT(fun);
	R_Visible = flag != 1;
	SEXP value = PRIMFUN(fun) (call, fun, markSpecialArgs(CDR(call)), rho);
	if (flag < 2) R_Visible = flag != 1;
	vmaxset(vmax);
	BCNPUSH(value);
	NEXT();
      }
    OP(MAKECLOSURE, 1):
      {
	SEXP fb = VECTOR_ELT(constants, GETOP());
	SEXP forms = VECTOR_ELT(fb, 0);
	SEXP body = VECTOR_ELT(fb, 1);
	SEXP value = mkCLOSXP(forms, body, rho);
	/* The LENGTH check below allows for byte code object created
	   by older versions of the compiler that did not record a
	   source attribute. */
	/* FIXME: bump bc version and don't check LENGTH? */
	if (LENGTH(fb) > 2) {
	  SEXP srcref = VECTOR_ELT(fb, 2);
	  if (!isNull(srcref)) setAttrib(value, R_SrcrefSymbol, srcref);
	}
	R_Visible = TRUE;
	BCNPUSH(value);
	NEXT();
      }
    OP(UMINUS, 1): FastUnary(-, R_SubSym);
    OP(UPLUS, 1): FastUnary(+, R_AddSym);
    OP(ADD, 1): FastBinary(R_ADD, PLUSOP, R_AddSym);
    OP(SUB, 1): FastBinary(R_SUB, MINUSOP, R_SubSym);
    OP(MUL, 1): FastBinary(R_MUL, TIMESOP, R_MulSym);
    OP(DIV, 1): FastBinary(R_DIV, DIVOP, R_DivSym);
    OP(EXPT, 1): FastBinary(R_POW, POWOP, R_ExptSym);
    OP(SQRT, 1): FastMath1(R_sqrt, R_SqrtSym);
    OP(EXP, 1): FastMath1(exp, R_ExpSym);
    OP(EQ, 1): FastRelop2(==, EQOP, R_EqSym);
    OP(NE, 1): FastRelop2(!=, NEOP, R_NeSym);
    OP(LT, 1): FastRelop2(<, LTOP, R_LtSym);
    OP(LE, 1): FastRelop2(<=, LEOP, R_LeSym);
    OP(GE, 1): FastRelop2(>=, GEOP, R_GeSym);
    OP(GT, 1): FastRelop2(>, GTOP, R_GtSym);
    OP(AND, 1): Builtin2(do_logic, R_AndSym, rho);
    OP(OR, 1): Builtin2(do_logic, R_OrSym, rho);
    OP(NOT, 1): Builtin1(do_logic, R_NotSym, rho);
    OP(DOTSERR, 0): error(_("'...' used in an incorrect context"));
    OP(STARTASSIGN, 1):
      {
	int sidx = GETOP();
	SEXP symbol = VECTOR_ELT(constants, sidx);
	SEXP cell = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	SEXP value = BINDING_VALUE(cell);
	if (value == R_UnboundValue ||
	    TYPEOF(value) == PROMSXP ||
#ifdef SWITCH_TO_REFCNT
	    REFCNT(value) != 1
#else
	    NAMED(value) != 1
#endif
	    )
	    value = EnsureLocal(symbol, rho);

	if (MAYBE_REFERENCED(value) &&
	    FIND_ON_STACK(value, vcache_top, FALSE))
	    value = shallow_duplicate(value);

	BCNPUSH(value);
	BCNDUP2ND();
	/* top three stack entries are now RHS value, LHS value, RHS value */
	if (IS_STACKVAL_BOXED(-1)) {
	    FIXUP_RHS_NAMED(GETSTACK(-1));
	    INCREMENT_REFCNT(GETSTACK(-1));
	}
	NEXT();
      }
    OP(ENDASSIGN, 1):
      {
	int sidx = GETOP();
	SEXP symbol = VECTOR_ELT(constants, sidx);
	SEXP cell = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	SEXP value = GETSTACK(-1); /* leave on stack for GC protection */
	INCREMENT_NAMED(value);
	if (! SET_BINDING_VALUE(cell, value))
	    defineVar(symbol, value, rho);
	R_BCNodeStackTop--; /* now pop LHS value off the stack */
	/* original right-hand side value is now on top of stack again */
#ifdef OLD_RHS_NAMED
	/* we do not duplicate the right-hand side value, so to be
	   conservative mark the value as NAMED = NAMEDMAX */
	ENSURE_NAMEDMAX(GETSTACK(-1));
#else
	if (IS_STACKVAL_BOXED(-1)) {
	    INCREMENT_NAMED(GETSTACK(-1));
	    DECREMENT_REFCNT(GETSTACK(-1));
	}
#endif
	NEXT();
      }
    OP(STARTSUBSET, 2): DO_STARTDISPATCH("[");
    OP(DFLTSUBSET, 0): DO_DFLTDISPATCH(do_subset_dflt, R_SubsetSym);
    OP(STARTSUBASSIGN, 2): DO_START_ASSIGN_DISPATCH("[<-");
    OP(DFLTSUBASSIGN, 0):
      DO_DFLT_ASSIGN_DISPATCH(do_subassign_dflt, R_SubassignSym);
    OP(STARTC, 2): DO_STARTDISPATCH("c");
    OP(DFLTC, 0): DO_DFLTDISPATCH(do_c_dflt, R_CSym);
    OP(STARTSUBSET2, 2): DO_STARTDISPATCH("[[");
    OP(DFLTSUBSET2, 0): DO_DFLTDISPATCH(do_subset2_dflt, R_Subset2Sym);
    OP(STARTSUBASSIGN2, 2): DO_START_ASSIGN_DISPATCH("[[<-");
    OP(DFLTSUBASSIGN2, 0):
      DO_DFLT_ASSIGN_DISPATCH(do_subassign2_dflt, R_Subassign2Sym);
    OP(DOLLAR, 2):
      {
	int dispatched = FALSE;
	SEXP call = VECTOR_ELT(constants, GETOP());
	SEXP symbol = VECTOR_ELT(constants, GETOP());
	SEXP x = GETSTACK(-1);
	SEXP value = NULL;
	if (isObject(x)) {
	    SEXP ncall;
	    PROTECT(ncall = duplicate(call));
	    /**** hack to avoid evaluating the symbol */
	    SETCAR(CDDR(ncall), ScalarString(PRINTNAME(symbol)));
	    dispatched = tryDispatch("$", ncall, x, rho, &value);
	    UNPROTECT(1);
	}
	if (dispatched)
	    SETSTACK(-1, value);
	else
	    SETSTACK(-1, R_subset3_dflt(x, PRINTNAME(symbol), R_NilValue));
	R_Visible = TRUE;
	NEXT();
      }
    OP(DOLLARGETS, 2):
      {
	int dispatched = FALSE;
	SEXP call = VECTOR_ELT(constants, GETOP());
	SEXP symbol = VECTOR_ELT(constants, GETOP());
	SEXP x = GETSTACK(-2);
	SEXP rhs = GETSTACK(-1);
	if (MAYBE_SHARED(x)) {
	    x = shallow_duplicate(x);
	    SETSTACK(-2, x);
	    ENSURE_NAMED(x);
	}
	SEXP value = NULL;
	if (isObject(x)) {
	    SEXP ncall, prom;
	    PROTECT(ncall = duplicate(call));
	    /**** hack to avoid evaluating the symbol */
	    SETCAR(CDDR(ncall), ScalarString(PRINTNAME(symbol)));
	    prom = mkRHSPROMISE(CADDDR(ncall), rhs);
	    SETCAR(CDDDR(ncall), prom);
	    dispatched = tryDispatch("$<-", ncall, x, rho, &value);
	    UNPROTECT(1);
	}
	if (! dispatched)
	  value = R_subassign3_dflt(call, x, symbol, rhs);
	R_BCNodeStackTop--;
	SETSTACK(-1, value);
	NEXT();
      }
    OP(ISNULL, 0): DO_ISTEST(isNull);
    OP(ISLOGICAL, 0): DO_ISTYPE(LGLSXP);
    OP(ISINTEGER, 0): {
	SEXP arg = GETSTACK(-1);
	Rboolean test = (TYPEOF(arg) == INTSXP) && ! inherits(arg, "factor");
	SETSTACK(-1, test ? R_TrueValue : R_FalseValue);
	R_Visible = TRUE;
	NEXT();
      }
    OP(ISDOUBLE, 0): DO_ISTYPE(REALSXP);
    OP(ISCOMPLEX, 0): DO_ISTYPE(CPLXSXP);
    OP(ISCHARACTER, 0): DO_ISTYPE(STRSXP);
    OP(ISSYMBOL, 0): DO_ISTYPE(SYMSXP); /**** S4 thingy allowed now???*/
    OP(ISOBJECT, 0): DO_ISTEST(OBJECT);
    OP(ISNUMERIC, 0): DO_ISTEST(isNumericOnly);
    OP(VECSUBSET, 1): DO_VECSUBSET(rho, FALSE); NEXT();
    OP(MATSUBSET, 1): DO_MATSUBSET(rho, FALSE); NEXT();
    OP(VECSUBASSIGN, 1): DO_VECSUBASSIGN(rho, FALSE); NEXT();
    OP(MATSUBASSIGN, 1): DO_MATSUBASSIGN(rho, FALSE); NEXT();
    OP(AND1ST, 2): {
	int callidx = GETOP();
	int label = GETOP();
	char *check = getenv("_R_CHECK_LENGTH_1_LOGIC2_");
	int warn_lev = (check) ? (StringTrue(check) ? 2 : 1) : 0;
	FIXUP_SCALAR_LOGICAL(callidx, "'x'", "&&", warn_lev);
	SEXP value = GETSTACK(-1);
	if (SCALAR_LVAL(value) == FALSE)
	    pc = codebase + label;
	R_Visible = TRUE;
	NEXT();
    }
    OP(AND2ND, 1): {
	int callidx = GETOP();
	char *check = getenv("_R_CHECK_LENGTH_1_LOGIC2_");
	int warn_lev = (check) ? (StringTrue(check) ? 2 : 1) : 0;
	FIXUP_SCALAR_LOGICAL(callidx, "'y'", "&&", warn_lev);
	SEXP value = GETSTACK(-1);
	/* The first argument is TRUE or NA. If the second argument is
	   not TRUE then its value is the result. If the second
	   argument is TRUE, then the first argument's value is the
	   result. */
	Rboolean val = SCALAR_LVAL(value);
	if (val == FALSE || val == NA_LOGICAL)
	    SETSTACK(-2, value);
	R_BCNodeStackTop -= 1;
	R_Visible = TRUE;
	NEXT();
    }
    OP(OR1ST, 2):  {
	int callidx = GETOP();
	int label = GETOP();
	char *check = getenv("_R_CHECK_LENGTH_1_LOGIC2_");
	int warn_lev = (check) ? (StringTrue(check) ? 2 : 1) : 0;
	FIXUP_SCALAR_LOGICAL(callidx, "'x'", "||", warn_lev);
	SEXP value = GETSTACK(-1);
	Rboolean val = SCALAR_LVAL(value);
	if (val != NA_LOGICAL &&
	    val != FALSE) /* is true */
	    pc = codebase + label;
	R_Visible = TRUE;
	NEXT();
    }
    OP(OR2ND, 1):  {
	int callidx = GETOP();
	char *check = getenv("_R_CHECK_LENGTH_1_LOGIC2_");
	int warn_lev = (check) ? (StringTrue(check) ? 2 : 1) : 0;
	FIXUP_SCALAR_LOGICAL(callidx, "'y'", "||", warn_lev);
	SEXP value = GETSTACK(-1);
	/* The first argument is FALSE or NA. If the second argument is
	   not FALSE then its value is the result. If the second
	   argument is FALSE, then the first argument's value is the
	   result. */
	if (SCALAR_LVAL(value) != FALSE)
	    SETSTACK(-2, value);
	R_BCNodeStackTop -= 1;
	R_Visible = TRUE;
	NEXT();
    }
    OP(GETVAR_MISSOK, 1): DO_GETVAR(FALSE, TRUE);
    OP(DDVAL_MISSOK, 1): DO_GETVAR(TRUE, TRUE);
    OP(VISIBLE, 0): R_Visible = TRUE; NEXT();
    OP(SETVAR2, 1):
      {
	SEXP symbol = VECTOR_ELT(constants, GETOP());
	SEXP value = GETSTACK(-1);
	INCREMENT_NAMED(value);
	setVar(symbol, value, ENCLOS(rho));
	NEXT();
      }
    OP(STARTASSIGN2, 1):
      {
	SEXP symbol = VECTOR_ELT(constants, GETOP());
	SEXP value = GETSTACK(-1);
	BCNPUSH(getvar(symbol, ENCLOS(rho), FALSE, FALSE, NULL, 0));
	BCNPUSH(value);
	/* top three stack entries are now RHS value, LHS value, RHS value */
	FIXUP_RHS_NAMED(value);
	INCREMENT_REFCNT(value);
	NEXT();
      }
    OP(ENDASSIGN2, 1):
      {
	SEXP symbol = VECTOR_ELT(constants, GETOP());
	SEXP value = BCNPOP();
	INCREMENT_NAMED(value);
	setVar(symbol, value, ENCLOS(rho));
	/* original right-hand side value is now on top of stack again */
#ifdef OLD_RHS_NAMED
	/* we do not duplicate the right-hand side value, so to be
	   conservative mark the value as NAMED = NAMEDMAX */
	ENSURE_NAMEDMAX(GETSTACK(-1));
#else
	INCREMENT_NAMED(GETSTACK(-1));
#endif
	DECREMENT_REFCNT(GETSTACK(-1));
	NEXT();
      }
    OP(SETTER_CALL, 2):
      {
	SEXP lhs = GETSTACK_BELOW_CALL_FRAME(-2);
	SEXP rhs = GETSTACK_BELOW_CALL_FRAME(-1);
	SEXP fun = CALL_FRAME_FUN();
	SEXP call = VECTOR_ELT(constants, GETOP());
	SEXP vexpr = VECTOR_ELT(constants, GETOP());
	SEXP args, prom, last;
	if (MAYBE_SHARED(lhs)) {
	  lhs = shallow_duplicate(lhs);
	  SETSTACK_BELOW_CALL_FRAME(-2, lhs);
	  ENSURE_NAMED(lhs);
	}
	SEXP value = NULL;
	switch (TYPEOF(fun)) {
	case BUILTINSXP:
	  /* push RHS value onto arguments with 'value' tag */
	  PUSHCALLARG(rhs);
	  SETCALLARG_TAG_SYMBOL(R_valueSym);
	  /* replace first argument with LHS value */
	  args = BUILTIN_CALL_FRAME_ARGS();
	  SETCAR(args, lhs);
	  /* make the call */
	  checkForMissings(args, call);
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  break;
	case SPECIALSXP:
	  /* duplicate arguments and protect */
	  PROTECT(args = duplicate(CDR(call)));
	  /* insert evaluated promise for LHS as first argument */
	  /* promise won't be captured so don't track references */
	  prom = R_mkEVPROMISE_NR(R_TmpvalSymbol, lhs);
	  SETCAR(args, prom);
	  /* insert evaluated promise for RHS as last argument */
	  last = args;
	  while (CDR(last) != R_NilValue)
	      last = CDR(last);
	  prom = mkRHSPROMISE(vexpr, rhs);
	  SETCAR(last, prom);
	  /* make the call */
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  UNPROTECT(1);
	  break;
	case CLOSXP:
	  /* push evaluated promise for RHS onto arguments with 'value' tag */
	  prom = mkRHSPROMISE(vexpr, rhs);
	  PUSHCALLARG(prom);
	  SETCALLARG_TAG_SYMBOL(R_valueSym);
	  /* replace first argument with evaluated promise for LHS */
	  /* promise might be captured, so track references */
	  args = CLOSURE_CALL_FRAME_ARGS();
	  prom = R_mkEVPROMISE(R_TmpvalSymbol, lhs);
	  SETCAR(args, prom);
	  /* make the call */
	  value = applyClosure(call, fun, args, rho, R_NilValue);
#ifdef ADJUST_ENVIR_REFCNTS
	  unpromiseArgs(args);
#endif
	  break;
	default: error(_("bad function"));
	}
	POP_CALL_FRAME_PLUS(2, value);
	NEXT();
      }
    OP(GETTER_CALL, 1):
      {
	SEXP lhs = GETSTACK_BELOW_CALL_FRAME(-2);
	SEXP fun = CALL_FRAME_FUN();
	SEXP call = VECTOR_ELT(constants, GETOP());
	SEXP value = NULL;
	SEXP args, prom;
	switch (TYPEOF(fun)) {
	case BUILTINSXP:
	  /* replace first argument with LHS value */
	  args = BUILTIN_CALL_FRAME_ARGS();
	  SETCAR(args, lhs);
	  /* make the call */
	  checkForMissings(args, call);
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  break;
	case SPECIALSXP:
	  /* duplicate arguments and put into stack for GC protection */
	  args = duplicate(CDR(call));
	  SETSTACK(-2, args);
	  /* insert evaluated promise for LHS as first argument */
	  /* promise won't be captured so don't track refrences */
	  prom = R_mkEVPROMISE_NR(R_TmpvalSymbol, lhs);
	  SETCAR(args, prom);
	  /* make the call */
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  break;
	case CLOSXP:
	  /* replace first argument with evaluated promise for LHS */
	  /* promise might be captured, so track references */
	  args = CLOSURE_CALL_FRAME_ARGS();
	  prom = R_mkEVPROMISE(R_TmpvalSymbol, lhs);
	  SETCAR(args, prom);
	  /* make the call */
	  value = applyClosure(call, fun, args, rho, R_NilValue);
#ifdef ADJUST_ENVIR_REFCNTS
	  unpromiseArgs(args);
#endif
	  break;
	default: error(_("bad function"));
	}
	POP_CALL_FRAME(value);
	NEXT();
      }
    OP(SWAP, 0): {
	R_bcstack_t tmp = R_BCNodeStackTop[-1];
	/* This instruction only occurs between accessor calls in
	   complex assignments. [It should probably be renamed to
	   reflect this.] It needs to make sure intermediate LHS
	   values in complex assignments are not shared by duplicating
	   the extracted value in tmp when necessary. Duplicating is
	   necessary if the value might be shared _or_ if the
	   container, which is in R_BCNodeStackTop[-3], has become
	   possibly shared by going through a closure in the preceding
	   accessor call.  This is taken to indicate that the
	   corresponding replacement function might be a closure and
	   will need to see an unmodified LHS value. This heuristic
	   fails if the accessor function called here is not a closure
	   but the replacement function is. */

	/* For the typed stack it might be OK just to force boxing at
	   this point, but for now this code tries to avoid doing
	   that. The macros make the code a little more reabable. */
#define STACKVAL_MAYBE_REFERENCED(idx)				\
	(IS_STACKVAL_BOXED(idx) &&				\
	 MAYBE_REFERENCED(GETSTACK_SXPVAL_PTR(R_BCNodeStackTop + (idx))))
#define STACKVAL_MAYBE_SHARED(idx)				\
	(IS_STACKVAL_BOXED(idx) &&				\
	 MAYBE_SHARED(GETSTACK_SXPVAL_PTR(R_BCNodeStackTop + (idx))))
#define STACKVAL_IS_ON_STACK(idx)					\
	FIND_ON_STACK(GETSTACK_SXPVAL(idx), vcache_top, TRUE)

	if (STACKVAL_MAYBE_REFERENCED(-1) &&
	    (STACKVAL_MAYBE_SHARED(-1) ||
	     STACKVAL_MAYBE_SHARED(-3) ||
	     STACKVAL_IS_ON_STACK(-1)))
	    GETSTACK_SXPVAL_PTR(&tmp) =
		shallow_duplicate(GETSTACK_SXPVAL_PTR(&tmp));

	R_BCNodeStackTop[-1] = R_BCNodeStackTop[-2];
	R_BCNodeStackTop[-2] = tmp;
	NEXT();
    }
    OP(DUP2ND, 0): BCNDUP2ND(); NEXT();
    OP(SWITCH, 4): {
       SEXP call = VECTOR_ELT(constants, GETOP());
       SEXP names = VECTOR_ELT(constants, GETOP());
       SEXP coffsets = VECTOR_ELT(constants, GETOP());
       SEXP ioffsets = VECTOR_ELT(constants, GETOP());
       SEXP value = BCNPOP();
       if (!isVector(value) || length(value) != 1)
	   errorcall(call, _("EXPR must be a length 1 vector"));
       if (isFactor(value))
	   warningcall(call,
		       _("EXPR is a \"factor\", treated as integer.\n"
			 " Consider using '%s' instead."),
		       "switch(as.character( * ), ...)");
       if (TYPEOF(value) == STRSXP) {
	   int i, n, which;
	   if (names == R_NilValue) {
	       if (TYPEOF(ioffsets) != INTSXP)
		   errorcall(call, _("bad numeric 'switch' offsets"));
	       if (LENGTH(ioffsets) == 1) {
		   pc = codebase + INTEGER(ioffsets)[0]; /* returns NULL */
		   warningcall(call, _("'switch' with no alternatives"));
	       }
	       else
		   errorcall(call, _("numeric EXPR required for 'switch' "
				     "without named alternatives"));
	   } else {
	       if (TYPEOF(coffsets) != INTSXP)
		   errorcall(call, _("bad character 'switch' offsets"));
	       if (TYPEOF(names) != STRSXP || LENGTH(names) != LENGTH(coffsets))
		   errorcall(call, "bad 'switch' names");
	       n = LENGTH(names);
	       which = n - 1;
	       for (i = 0; i < n - 1; i++)
		   if (pmatch(STRING_ELT(value, 0),
			      STRING_ELT(names, i), 1 /* exact */)) {
		       which = i;
		       break;
		   }
	       pc = codebase + INTEGER(coffsets)[which];
	   }
       }
       else {
	   if (TYPEOF(ioffsets) != INTSXP)
	       errorcall(call, "bad numeric 'switch' offsets");
	   int which = asInteger(value);
	   if (which != NA_INTEGER) which--;
	   if (which < 0 || which >= LENGTH(ioffsets))
	       which = LENGTH(ioffsets) - 1;
	   if (LENGTH(ioffsets) == 1)
	       warningcall(call, _("'switch' with no alternatives"));
	   pc = codebase + INTEGER(ioffsets)[which];
       }
       NEXT();
    }
    OP(RETURNJMP, 0): {
      SEXP value = BCNPOP();
      findcontext(CTXT_BROWSER | CTXT_FUNCTION, rho, value);
    }
    OP(STARTSUBSET_N, 2): DO_STARTDISPATCH_N("[");
    OP(STARTSUBASSIGN_N, 2): DO_START_ASSIGN_DISPATCH_N("[<-");
    OP(VECSUBSET2, 1): DO_VECSUBSET(rho, TRUE); NEXT();
    OP(MATSUBSET2, 1): DO_MATSUBSET(rho, TRUE); NEXT();
    OP(VECSUBASSIGN2, 1): DO_VECSUBASSIGN(rho, TRUE); NEXT();
    OP(MATSUBASSIGN2, 1): DO_MATSUBASSIGN(rho, TRUE); NEXT();
    OP(STARTSUBSET2_N, 2): DO_STARTDISPATCH_N("[[");
    OP(STARTSUBASSIGN2_N, 2): DO_START_ASSIGN_DISPATCH_N("[[<-");
    OP(SUBSET_N, 2): DO_SUBSET_N(rho, FALSE); NEXT();
    OP(SUBSET2_N, 2): DO_SUBSET_N(rho, TRUE); NEXT();
    OP(SUBASSIGN_N, 2): DO_SUBASSIGN_N(rho, FALSE); NEXT();
    OP(SUBASSIGN2_N, 2): DO_SUBASSIGN_N(rho, TRUE); NEXT();
    OP(LOG, 1): DO_LOG(); NEXT();
    OP(LOGBASE, 1): DO_LOGBASE(); NEXT();
    OP(MATH1, 2): DO_MATH1(); NEXT();
    OP(DOTCALL, 2): DO_DOTCALL(); NEXT();
    OP(COLON, 1): DO_COLON(); NEXT();
    OP(SEQALONG, 1): DO_SEQ_ALONG(); NEXT();
    OP(SEQLEN, 1): DO_SEQ_LEN(); NEXT();
    OP(BASEGUARD, 2): DO_BASEGUARD(); NEXT();
    LASTOP;
  }

 done:
  R_BCIntActive = oldbcintactive;
  R_BCbody = oldbcbody;
  R_BCpc = oldbcpc;
  R_Srcref = oldsrcref;
  R_BCNodeStackTop = oldntop;
#ifdef BC_INT_STACK
  R_BCIntStackTop = olditop;
#endif
#ifdef BC_PROFILING
  current_opcode = old_current_opcode;
#endif
  return retvalue;
}

#ifdef THREADED_CODE
SEXP R_bcEncode(SEXP bytes)
{
    SEXP code;
    BCODE *pc;
    int *ipc, i, n, m, v;

    m = (sizeof(BCODE) + sizeof(int) - 1) / sizeof(int);

    n = LENGTH(bytes);
    ipc = INTEGER(bytes);

    v = ipc[0];
    if (v < R_bcMinVersion || v > R_bcVersion) {
	code = allocVector(INTSXP, m * 2);
	pc = (BCODE *) INTEGER(code);
	pc[0].i = v;
	pc[1].v = opinfo[BCMISMATCH_OP].addr;
	return code;
    }
    else {
	code = allocVector(INTSXP, m * n);
	memset(INTEGER(code), 0, m * n * sizeof(int));
	pc = (BCODE *) INTEGER(code);

	for (i = 0; i < n; i++) pc[i].i = ipc[i];

	/* install the current version number */
	pc[0].i = R_bcVersion;

	/* Revert to version 2 to allow for some one compiling in a
	   new R, loading/saving in an old one, and then trying to run
	   in a new one. This has happened! Setting the version number
	   back tells bcEval to drop back to eval. */
	if (n == 2 && ipc[1] == BCMISMATCH_OP)
	    pc[0].i = 2;

	for (i = 1; i < n;) {
	    int op = pc[i].i;
	    if (op < 0 || op >= OPCOUNT)
		error("unknown instruction code");
	    pc[i].v = opinfo[op].addr;
	    i += opinfo[op].argc + 1;
	}

	return code;
    }
}

static int findOp(void *addr)
{
    int i;

    for (i = 0; i < OPCOUNT; i++)
	if (opinfo[i].addr == addr)
	    return i;
    error(_("cannot find index for threaded code address"));
    return 0; /* not reached */
}

SEXP R_bcDecode(SEXP code) {
    int n, i, j, *ipc;
    BCODE *pc;
    SEXP bytes;

    int m = (sizeof(BCODE) + sizeof(int) - 1) / sizeof(int);

    n = LENGTH(code) / m;
    pc = (BCODE *) INTEGER(code);

    bytes = allocVector(INTSXP, n);
    ipc = INTEGER(bytes);

    /* copy the version number */
    ipc[0] = pc[0].i;

    for (i = 1; i < n;) {
	int op = findOp(pc[i].v);
	int argc = opinfo[op].argc;
	ipc[i] = op;
	i++;
	for (j = 0; j < argc; j++, i++)
	    ipc[i] = pc[i].i;
    }

    return bytes;
}
#else
SEXP R_bcEncode(SEXP x) { return x; }
SEXP R_bcDecode(SEXP x) { return duplicate(x); }
#endif

/* Add BCODESXP bc into the constants registry, performing a deep copy of the
   bc's constants */
#define CONST_CHECK_COUNT 1000
void attribute_hidden R_registerBC(SEXP bcBytes, SEXP bcode)
{
    if (R_check_constants <= 0)
	return;
    if (TYPEOF(bcBytes) != INTSXP)
	error("registerBC requires integer vector as bcBytes");
    if (TYPEOF(bcode) != BCODESXP)
	error("registerBC requires BCODESXP object as bcode");

    static int count = CONST_CHECK_COUNT;
    if (--count <= 0) {
	count = CONST_CHECK_COUNT;
	R_checkConstants(TRUE);
    }

    /* The constants registry is a linked list of constant records. Each
       constant record is a generic vector, its first element is a pointer
       to the next constant record, the second element is a weak reference
       to the byte-code object, the third element is a reference to the whole
       constant pool, and the following elements are interleaved original and
       copied constants. A constant registry corresponds to a constant pool.
       When the weak reference gets cleared, the respective constant record
       can be removed from the list.

       One could simply compare/duplicate the lists of all constants (the whole
       constant pools), but that turned out too expensive */

    SEXP consts = BCCONSTS(bcode); /* all constants, VECSXP */

#define CHECK_ALL_CONSTANTS
#ifndef CHECK_ALL_CONSTANTS
    int *ipc = INTEGER(bcBytes);
    int n = LENGTH(bcBytes);
    int i;
    int loadableConsts = 0;

    /* add only constants loaded by certain instructions  */
    for(i = 0; i < n; i += opinfo[ipc[i]].argc + 1)
        if (ipc[i] == LDCONST_OP || ipc[i] == PUSHCONSTARG_OP ||
		ipc[i] == CALLSPECIAL_OP)
            loadableConsts++;

    SEXP constsRecord = PROTECT(allocVector(VECSXP, loadableConsts * 2 + 3));
    int crIdx = 3;
    for(i = 0; i < n; i += opinfo[ipc[i]].argc + 1)
        if (ipc[i] == LDCONST_OP || ipc[i] == PUSHCONSTARG_OP ||
		ipc[i] == CALLSPECIAL_OP) {
            SEXP corig = VECTOR_ELT(consts, ipc[i + 1]);
            SET_VECTOR_ELT(constsRecord, crIdx++, corig);
            SET_VECTOR_ELT(constsRecord, crIdx++, duplicate(corig));
        }
#else
    /* add the whole constant pool */
    SEXP constsRecord = PROTECT(allocVector(VECSXP, 2 + 3));
    SET_VECTOR_ELT(constsRecord, 3, consts);
    /* the consts reference is in the record twice to make the code simpler */
    SET_VECTOR_ELT(constsRecord, 4, duplicate(consts));
#endif

    SEXP wref = R_MakeWeakRef(bcode, R_NilValue, R_NilValue, FALSE);
    SET_VECTOR_ELT(constsRecord, 0, VECTOR_ELT(R_ConstantsRegistry, 0));
    SET_VECTOR_ELT(constsRecord, 1, wref);
    SET_VECTOR_ELT(constsRecord, 2, consts);
    SET_VECTOR_ELT(R_ConstantsRegistry, 0, constsRecord);
    UNPROTECT(1); /* constsRecord */
}

/* A potentially very verbose report for modified compiler constant. */
static void reportModifiedConstant(SEXP crec, SEXP orig, SEXP copy, int idx)
{
    if (R_check_constants < 5)
	return;

    SEXP consts = VECTOR_ELT(crec, 2);
    int n = LENGTH(consts);
    int i;
    if (idx == -1) {
	for(i = 0; i < n; i++)
	    if (VECTOR_ELT(consts, i) == orig) {
		idx = i;
		break;
	    }
    }
    int oldout = R_OutputCon; /* redirect standard to error output */
    R_OutputCon = 2;
    int oldcheck = R_check_constants; /* guard against recursive invocation */
    R_check_constants = 0;
    if (idx != 0) {
	REprintf("ERROR: the modified value of the constant is:\n");
	PrintValue(orig);
	REprintf("ERROR: the original value of the constant is:\n");
	PrintValue(copy);
	REprintf("ERROR: the modified constant is at index %d\n", idx);
	REprintf("ERROR: the modified constant is in this function body:\n");
	PrintValue(VECTOR_ELT(consts, 0));
    } else {
	REprintf("ERROR: the modified constant is function body:\n");
	PrintValue(orig);
	REprintf("ERROR: the body was originally:\n");
	PrintValue(copy);
    }
    findFunctionForBody(VECTOR_ELT(consts, 0));
    R_check_constants = oldcheck;
    R_OutputCon = oldout;
}

/* Checks whether compiler constants linked from the given record
   were modified. */
static Rboolean checkConstantsInRecord(SEXP crec, Rboolean abortOnError)
{
    int i;
    int n = LENGTH(crec);
    Rboolean constsOK = TRUE;

    for (i = 3; i < n;) {
	SEXP corig = VECTOR_ELT(crec, i++);
	SEXP ccopy = VECTOR_ELT(crec, i++);

	/* 39: not numerical comparison, not single NA, not attributes as
           set, do ignore byte-code, do ignore environments of closures,
           not ignore srcref

           srcref is not ignored because ignoring it is expensive
           (it triggers duplication)
        */
	if (!R_compute_identical(corig, ccopy, 39)) {

#ifndef CHECK_ALL_CONSTANTS
	    REprintf("ERROR: modification of compiler constant of type %s"
		", length %d\n", CHAR(type2str(TYPEOF(ccopy))), length(ccopy));
	    reportModifiedConstant(crec, corig, ccopy, -1);
#else
	    int nc = LENGTH(corig);
	    /* some variables are volatile to prevent the compiler from
	       optimizing them out, for easier debugging */
	    volatile int ci;
	    for(ci = 0; ci < nc; ci++) {
		volatile SEXP orig = VECTOR_ELT(corig, ci);
		volatile SEXP copy = VECTOR_ELT(ccopy, ci);
		if (!R_compute_identical(orig, copy, 39)) {
		    REprintf("ERROR: modification of compiler constant"
			" of type %s, length %d\n",
			CHAR(type2str(TYPEOF(copy))), length(copy));
		    reportModifiedConstant(crec, orig, copy, ci);
		}
	    }
#endif
	    constsOK = FALSE;
        }
    }

    if (!constsOK && abortOnError) {
	/* turn off constant checking to avoid infinite recursion through
	   R_Suicide -> ... -> R_RunExitFinalizers -> R_checkConstants. */
	R_check_constants = 0;
	R_Suicide("compiler constants were modified!\n");
    }

    return constsOK;
}

/* Checks if constants of any registered BCODESXP have been modified.
   Returns TRUE if the constants are ok, otherwise returns false or aborts.*/
Rboolean attribute_hidden R_checkConstants(Rboolean abortOnError)
{
    if (R_check_constants <= 0 || R_ConstantsRegistry == NULL)
	return TRUE;
    static Rboolean checkingInProgress = FALSE;
    if (checkingInProgress)
	/* recursive invocation is possible because of allocation
           in R_compute_identical */
	return TRUE;
    /* NOTE: non-local return could disable checking */
    checkingInProgress = TRUE;
    SEXP prev_crec = R_ConstantsRegistry;
    SEXP crec = VECTOR_ELT(prev_crec, 0);
    Rboolean constsOK = TRUE;
    while(crec != R_NilValue) {
	SEXP wref = VECTOR_ELT(crec, 1);
	SEXP bc = R_WeakRefKey(wref);
	if (!checkConstantsInRecord(crec, abortOnError))
	    constsOK = FALSE;
	if (bc == R_NilValue)
	    /* remove no longer needed record from the registry */
	    SET_VECTOR_ELT(prev_crec, 0, VECTOR_ELT(crec, 0));
	else
            prev_crec = crec;
	crec = VECTOR_ELT(crec, 0);
    }
    checkingInProgress = FALSE;
    return constsOK;
}

SEXP attribute_hidden do_mkcode(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP bytes, consts, ans;

    checkArity(op, args);
    bytes = CAR(args);
    consts = CADR(args);
    ans = PROTECT(CONS(R_bcEncode(bytes), consts));
    SET_TYPEOF(ans, BCODESXP);
    R_registerBC(bytes, ans);
    UNPROTECT(1); /* ans */
    return ans;
}

SEXP attribute_hidden do_bcclose(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP forms, body, env;

    checkArity(op, args);
    forms = CAR(args);
    body = CADR(args);
    env = CADDR(args);

    CheckFormals(forms);

    if (! isByteCode(body))
	error(_("invalid body"));

    if (isNull(env)) {
	error(_("use of NULL environment is defunct"));
	env = R_BaseEnv;
    } else
    if (!isEnvironment(env))
	error(_("invalid environment"));

    return mkCLOSXP(forms, body, env);
}

SEXP attribute_hidden do_is_builtin_internal(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP symbol, i;

    checkArity(op, args);
    symbol = CAR(args);

    if (!isSymbol(symbol))
	error(_("invalid symbol"));

    if ((i = INTERNAL(symbol)) != R_NilValue && TYPEOF(i) == BUILTINSXP)
	return R_TrueValue;
    else
	return R_FalseValue;
}

static SEXP disassemble(SEXP bc)
{
  SEXP ans, dconsts;
  int i;
  SEXP code = BCODE_CODE(bc);
  SEXP consts = BCODE_CONSTS(bc);
  SEXP expr = BCODE_EXPR(bc);
  int nc = LENGTH(consts);

  PROTECT(ans = allocVector(VECSXP, expr != R_NilValue ? 4 : 3));
  SET_VECTOR_ELT(ans, 0, install(".Code"));
  SET_VECTOR_ELT(ans, 1, R_bcDecode(code));
  SET_VECTOR_ELT(ans, 2, allocVector(VECSXP, nc));
  if (expr != R_NilValue)
      SET_VECTOR_ELT(ans, 3, duplicate(expr));

  dconsts = VECTOR_ELT(ans, 2);
  for (i = 0; i < nc; i++) {
    SEXP c = VECTOR_ELT(consts, i);
    if (isByteCode(c))
      SET_VECTOR_ELT(dconsts, i, disassemble(c));
    else
      SET_VECTOR_ELT(dconsts, i, duplicate(c));
  }

  UNPROTECT(1);
  return ans;
}

SEXP attribute_hidden do_disassemble(SEXP call, SEXP op, SEXP args, SEXP rho)
{
  SEXP code;

  checkArity(op, args);
  code = CAR(args);
  if (! isByteCode(code))
    error(_("argument is not a byte code object"));
  return disassemble(code);
}

SEXP attribute_hidden do_bcversion(SEXP call, SEXP op, SEXP args, SEXP rho)
{
  checkArity(op, args);
  SEXP ans = allocVector(INTSXP, 1);
  INTEGER(ans)[0] = R_bcVersion;
  return ans;
}

#ifdef UNUSED
#define R_COMPILED_EXTENSION ".Rc"

/* neither of these functions call R_ExpandFileName -- the caller
   should do that if it wants to */
char *R_CompiledFileName(char *fname, char *buf, size_t bsize)
{
    char *basename, *ext;

    /* find the base name and the extension */
    basename = Rf_strrchr(fname, FILESEP[0]);
    if (basename == NULL) basename = fname;
    ext = Rf_strrchr(basename, '.');

    if (ext != NULL && strcmp(ext, R_COMPILED_EXTENSION) == 0) {
	/* the supplied file name has the compiled file extension, so
	   just copy it to the buffer and return the buffer pointer */
	if (snprintf(buf, bsize, "%s", fname) < 0)
	    error("R_CompiledFileName: buffer too small");
	return buf;
    }
    else if (ext == NULL) {
	/* if the requested file has no extention, make a name that
	   has the extenrion added on to the expanded name */
	if (snprintf(buf, bsize, "%s%s", fname, R_COMPILED_EXTENSION) < 0)
	    error("R_CompiledFileName: buffer too small");
	return buf;
    }
    else {
	/* the supplied file already has an extension, so there is no
	   corresponding compiled file name */
	return NULL;
    }
}

FILE *R_OpenCompiledFile(char *fname, char *buf, size_t bsize)
{
    char *cname = R_CompiledFileName(fname, buf, bsize);

    if (cname != NULL && R_FileExists(cname) &&
	(strcmp(fname, cname) == 0 ||
	 ! R_FileExists(fname) ||
	 R_FileMtime(cname) > R_FileMtime(fname)))
	/* the compiled file cname exists, and either fname does not
	   exist, or it is the same as cname, or both exist and cname
	   is newer */
	return R_fopen(buf, "rb");
    else return NULL;
}
#endif

SEXP attribute_hidden do_growconst(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP constBuf, ans;
    int i, n;

    checkArity(op, args);
    constBuf = CAR(args);
    if (TYPEOF(constBuf) != VECSXP)
	error(_("constant buffer must be a generic vector"));

    n = LENGTH(constBuf);
    ans = allocVector(VECSXP, 2 * n);
    for (i = 0; i < n; i++)
	SET_VECTOR_ELT(ans, i, VECTOR_ELT(constBuf, i));

    return ans;
}

SEXP attribute_hidden do_putconst(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP constBuf, x;
    int i, constCount;

    checkArity(op, args);

    constBuf = CAR(args);
    if (TYPEOF(constBuf) != VECSXP)
	error(_("constant buffer must be a generic vector"));

    constCount = asInteger(CADR(args));
    if (constCount < 0 || constCount >= LENGTH(constBuf))
	error("bad constCount value");

    x = CADDR(args);

    /* check for a match and return index if one is found */
    for (i = 0; i < constCount; i++) {
	SEXP y = VECTOR_ELT(constBuf, i);
	/* 16 - take closure environments into account, this is necessary
	        as closures (closure literals) can get into the AST when
	        the AST is generated by a program (e.g. distr package)
	*/
	if (x == y || R_compute_identical(x, y, 16))
	    return ScalarInteger(i);
    }

    /* otherwise insert the constant and return index */
    SET_VECTOR_ELT(constBuf, constCount, x);
    return ScalarInteger(constCount);
}

SEXP attribute_hidden do_getconst(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP constBuf, ans;
    int i, n;

    checkArity(op, args);
    constBuf = CAR(args);
    n = asInteger(CADR(args));

    if (TYPEOF(constBuf) != VECSXP)
	error(_("constant buffer must be a generic vector"));
    if (n < 0 || n > LENGTH(constBuf))
	error(_("bad constant count"));

    ans = allocVector(VECSXP, n);
    for (i = 0; i < n; i++)
	SET_VECTOR_ELT(ans, i, VECTOR_ELT(constBuf, i));

    return ans;
}

#ifdef BC_PROFILING
SEXP do_bcprofcounts(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP val;
    int i;

    checkArity(op, args);
    val = allocVector(INTSXP, OPCOUNT);
    for (i = 0; i < OPCOUNT; i++)
	INTEGER(val)[i] = opcode_counts[i];
    return val;
}

static void dobcprof(int sig)
{
    if (current_opcode >= 0 && current_opcode < OPCOUNT)
	opcode_counts[current_opcode]++;
    signal(SIGPROF, dobcprof);
}

SEXP do_bcprofstart(SEXP call, SEXP op, SEXP args, SEXP env)
{
    struct itimerval itv;
    int interval;
    double dinterval = 0.02;
    int i;

    checkArity(op, args);
    if (R_Profiling)
	error(_("profile timer in use"));
    if (bc_profiling)
	error(_("already byte code profiling"));

    /* according to man setitimer, it waits until the next clock
       tick, usually 10ms, so avoid too small intervals here */
    interval = 1e6 * dinterval + 0.5;

    /* initialize the profile data */
    current_opcode = NO_CURRENT_OPCODE;
    for (i = 0; i < OPCOUNT; i++)
	opcode_counts[i] = 0;

    signal(SIGPROF, dobcprof);

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = interval;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = interval;
    if (setitimer(ITIMER_PROF, &itv, NULL) == -1)
	error(_("setting profile timer failed"));

    bc_profiling = TRUE;

    return R_NilValue;
}

static void dobcprof_null(int sig)
{
    signal(SIGPROF, dobcprof_null);
}

SEXP do_bcprofstop(SEXP call, SEXP op, SEXP args, SEXP env)
{
    struct itimerval itv;

    checkArity(op, args);
    if (! bc_profiling)
	error(_("not byte code profiling"));

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = 0;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = 0;
    setitimer(ITIMER_PROF, &itv, NULL);
    signal(SIGPROF, dobcprof_null);

    bc_profiling = FALSE;

    return R_NilValue;
}
#else
SEXP NORET do_bcprofcounts(SEXP call, SEXP op, SEXP args, SEXP env) {
    checkArity(op, args);
    error(_("byte code profiling is not supported in this build"));
}
SEXP NORET do_bcprofstart(SEXP call, SEXP op, SEXP args, SEXP env) {
    checkArity(op, args);
    error(_("byte code profiling is not supported in this build"));
}
SEXP NORET do_bcprofstop(SEXP call, SEXP op, SEXP args, SEXP env) {
    checkArity(op, args);
    error(_("byte code profiling is not supported in this build"));
}
#endif

/* end of byte code section */

SEXP attribute_hidden do_setnumthreads(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int old = R_num_math_threads, new;
    checkArity(op, args);
    new = asInteger(CAR(args));
    if (new >= 0 && new <= R_max_num_math_threads)
	R_num_math_threads = new;
    return ScalarInteger(old);
}

SEXP attribute_hidden do_setmaxnumthreads(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int old = R_max_num_math_threads, new;
    checkArity(op, args);
    new = asInteger(CAR(args));
    if (new >= 0) {
	R_max_num_math_threads = new;
	if (R_num_math_threads > R_max_num_math_threads)
	    R_num_math_threads = R_max_num_math_threads;
    }
    return ScalarInteger(old);
}

SEXP attribute_hidden do_returnValue(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP val;
    checkArity(op, args);
    if (R_ExitContext && (val = R_ExitContext->returnValue)){
	MARK_NOT_MUTABLE(val);
	return val;
    }
    return CAR(args); /* default */
}

#include <Parse.h>
SEXP R_ParseEvalString(const char *str, SEXP env)
{
    SEXP s = PROTECT(mkString(str));

    ParseStatus status;
    SEXP ps = PROTECT(R_ParseVector(s, -1, &status, R_NilValue));
    if (status != PARSE_OK ||
	TYPEOF(ps) != EXPRSXP ||
	LENGTH(ps) != 1)
	error("parse error");

    SEXP val = eval(VECTOR_ELT(ps, 0), env);
    UNPROTECT(2); /* s, ps */
    return val;
}
