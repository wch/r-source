/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996	Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2001	The R Development Core Team.
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

#undef HASHING

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"

#ifndef Unix
#error this stuff is Unix-only for now
#endif

static R_thread_context_t R_MainThread;

void R_InitThreads(void)
{
    R_ThreadTag = install("__Thread__");
    R_MutexTag = install("__Mutex__");
    R_CondvarTag = install("__Condvar__");
    R_ThreadContext = R_MakeThreadContext(R_thread_state_runnable, R_NilValue);
    R_MainThread = R_ThreadContext;
}

SEXP  do_concur(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("can only run threads from eval");
    return R_NilValue; /* not reached */
}


struct R_fd_set_st {
    int maxfd;
    fd_set readMask;
};

void R_init_fd_set(R_fd_set_t fdp)
{
    FD_ZERO(&fdp->readMask);
    fdp->maxfd = -1;
}

void R_fd_set(R_fd_t fd, R_fd_set_t fdp)
{
    FD_SET(fd, &fdp->readMask);
    if (fd >  fdp->maxfd)
	fdp->maxfd = fd;
}

DECLARE_CONTINUATION(suspend_cont, suspend_cont_fun);

void R_SuspendCurrentThread(R_code_t code)
{
    SEXP rho = R_CurrentEnv;
    PUSH_EVAL_FRAME(code, R_NilValue, R_NilValue);
    begincontext(CTXT_RETURN, R_NilValue, rho, rho, R_NilValue);
    R_GlobalContext->contcode = suspend_cont;
}

R_code_t suspend_cont_fun(R_code_t code)
{
    code = EVAL_FRAME_CODE();
    endcontext();
    POP_EVAL_FRAME();
    return code;
}

R_code_t R_ResumeThread(R_thread_context_t thread)
{
    SEXP rho;

    R_ThreadContext = thread;
    rho = thread->global_context->cloenv;
    thread->wakeup = 0.0;
    thread->eval_count = 0;
    return R_findcontext_nr(CTXT_RETURN, rho, thread->value);
}

static double current_time(void)
{
    double times[5];
    R_getProcTime(times);
    return times[2];
}
    
#ifndef HAVE_TIMES
#error **** no thread support without times ****
#endif

#define NUM_MUTEX_FIELDS 3
#define R_MUTEX_NAME(m) VECTOR_ELT(EXTPTR_PROT(m), 0)
#define R_SET_MUTEX_NAME(m, n) SET_VECTOR_ELT(EXTPTR_PROT(m), 0, n)
#define R_MUTEX_DATA(m) VECTOR_ELT(EXTPTR_PROT(m), 1)
#define R_SET_MUTEX_DATA(m, n) SET_VECTOR_ELT(EXTPTR_PROT(m), 1, n)
#define R_MUTEX_OWNER(m) VECTOR_ELT(EXTPTR_PROT(m), 2)
#define R_SET_MUTEX_OWNER(m, n) SET_VECTOR_ELT(EXTPTR_PROT(m), 2, n)
#define R_MUTEX_LOCKED(m) ((m)->sxpinfo.gp)
#define R_SET_MUTEX_LOCKED(m, v) ((m)->sxpinfo.gp = (v))

#define NUM_CONDVAR_FIELDS 2
#define R_CONDVAR_NAME(m) VECTOR_ELT(EXTPTR_PROT(m), 0)
#define R_SET_CONDVAR_NAME(m, n) SET_VECTOR_ELT(EXTPTR_PROT(m), 0, n)
#define R_CONDVAR_DATA(m) VECTOR_ELT(EXTPTR_PROT(m), 1)
#define R_SET_CONDVAR_DATA(m, n) SET_VECTOR_ELT(EXTPTR_PROT(m), 1, n)

#define R_MUTEX_QUEUE(mx) ((R_thread_context_t *) &(EXTPTR_PTR(mx)))
#define R_CONDVAR_QUEUE(cv) ((R_thread_context_t *) &(EXTPTR_PTR(cv)))

static void dequeue_thread(R_thread_context_t tc)
{
    /**** this is horrible--we need a common representation of queues
          with double linking */
    SEXP w = tc->wait_object;
    if (w != R_NilValue) {
	R_thread_context_t *pqueue;
	SEXP tag = EXTPTR_TAG(w);

	if (tag == R_ThreadTag)
	    pqueue = &(R_THREAD_CONTEXT(w)->join);
	else if (tag == R_MutexTag)
	    pqueue = R_MUTEX_QUEUE(w);
	else if (tag == R_CondvarTag)
	    pqueue = R_CONDVAR_QUEUE(w);
	else
	    pqueue = NULL;  /**** should never get here */

	if (pqueue != NULL && *pqueue != NULL) { /**** should always happen */
	    if (*pqueue == tc)
		*pqueue = tc->wait_next;
	    else {
		R_thread_context_t last;
		for (last = *pqueue;
		     last->wait_next != NULL;
		     last = last->wait_next)
		    if (last->wait_next == tc) {
			last->wait_next = tc->wait_next;
			break;
		    }
	    }
	}
	tc->wait_object = R_NilValue;
	tc->wait_next = NULL;
    }
}

static void enqueue_thread(R_thread_context_t *pqueue, R_thread_context_t tc)
{
    if (*pqueue == NULL)
	*pqueue = tc;
    else {
	R_thread_context_t last = *pqueue;
	while (last->wait_next != NULL)
	    last = last->wait_next;
	last->wait_next = tc;
    }
    tc->wait_next = NULL; /* should already be */
}

static R_thread_context_t pop_thread_queue(R_thread_context_t *pqueue)
{
    if (*pqueue != NULL) {
	R_thread_context_t tc = *pqueue;
	*pqueue = tc->wait_next;
	return tc;
    }
    else return NULL;
}

static void wake_thread(R_thread_context_t tc, SEXP val)
{
    tc->wait_object = R_NilValue;
    tc->wait_next = NULL;
    tc->value = val;
    tc->state = R_thread_state_runnable;
    tc->wakeup = 0.0;
    tc->ioready = NULL;
    tc->ioprepsleep = NULL;
}

static Rboolean thread_is_ready(R_thread_context_t tc, double *pnow)
{
    if (tc->global_context != NULL &&
	R_ThreadContext->global_context != NULL &&
	tc->global_context->dispatcher != 
	R_ThreadContext->global_context->dispatcher)
	/* this test may be too stringent */
	/**** This test should be OK for now since threads always
              start in the top level dispatcher and, with this test,
              no more than one thread can be suspended in a non-top
              level dispatcher */
	return FALSE;
    else if (R_threads_enabled || tc == R_ThreadContext) {
	if (tc->state == R_thread_state_runnable)
	    return TRUE;
	else if (tc->ioready != NULL && tc->ioready(tc->iodata)) {
	    /**** assumes this is not on a queue, so no dequeue */
	    wake_thread(tc, mkTrue());
	    return TRUE;
	}
	else if (tc->wakeup > 0.0) {
	    if (*pnow < 0.0)
		*pnow = current_time();
	    if (tc->wakeup <= *pnow) {
		dequeue_thread(tc);
		wake_thread(tc, mkFalse());
		return TRUE;
	    }
	    else return FALSE;
	}
	else return FALSE;
    }
    else return FALSE;
}


#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

R_thread_context_t R_FindNextRunnableThread(R_thread_context_t first)
{
    double now = -1.0;
    Rboolean iowaiting = FALSE;
    R_thread_context_t tc;

    for (tc = first; tc != NULL; tc = tc->next_context)
	if (thread_is_ready(tc, &now))
	    return tc;
    for (tc = R_ActiveThreads(); tc != first; tc = tc->next_context)
	if (thread_is_ready(tc, &now))
	    return tc;
    return NULL;
}

/**** Need to do something to detect and handle deadlock here.  If
      deadlock is certain, we should wake up one of the responsible
      threads and signal an error on it. */
static void block_for_activity(void)
{
    struct R_fd_set_st fds;
    R_thread_context_t tc;
    Rboolean iowaiting = FALSE;
    double when = -1.0;
    double now = current_time();

    R_init_fd_set(&fds);

    for (tc = R_ActiveThreads(); tc != NULL; tc = tc->next_context) {
	if (! iowaiting && (tc->ioready != NULL || tc->ioprepsleep != NULL))
	    iowaiting = TRUE;
	if (tc->wakeup > 0.0 && (when < 0.0 || tc->wakeup < when))
	    when = tc->wakeup;
	if (tc->ioprepsleep != NULL)
	    tc->ioprepsleep(&fds, tc->iodata);
    }

    if (! iowaiting && when < 0.0) {
	dequeue_thread(R_ThreadContext);
	wake_thread(R_ThreadContext, mkFalse());
	error("operation causes deadlock");
    }

    /**** this is the UNIX-specific part */
    /**** allow for write, exception also? */
    /**** this bit is UNIX-specific */
    /**** try to check for deadlock here too? */
    {
	struct timeval tv, *tvp;
	if (when > 0.0) {
	    double delta = when - now;
	    tvp = &tv;
	    tv.tv_sec = delta; /* truncates to integer */
	    tv.tv_usec = (1000000.0 * (delta - tv.tv_sec));
	}
	else tvp = NULL;
	/**** if (fds.maxfd < 0 && when <= 0.0) deadlock */
	select(fds.maxfd + 1, &fds.readMask, NULL, NULL, tvp);
    }
}

/* This is just a dummy used so that R_BlockUntil can reap the dead thread */
DECLARE_CONTINUATION(thread_done_cont, thread_done_cont_fun);

static R_code_t thread_done_cont_fun(R_code_t code)
{
    R_Suicide("thread exited--this should never be called");
    return NULL;
}

/**** need to lock most of this against interrupts, but allow them
      during the blocking wait */
R_code_t R_BlockUntil(Rboolean (*ioready)(SEXP),
		      void (*ioprepsleep)(R_fd_set_t, SEXP),
		      double delta, SEXP iodata, R_code_t code)
{
    R_thread_context_t next;

    if (code == thread_done_cont) {
	R_thread_context_t tc = R_ThreadContext;

	/* The main thread never exits, so if the current thread is
           exiting then there are at least two active threads. Pick
           one of the others as the new current thread. */
	if (tc->next_context != NULL)
	    R_ThreadContext = tc->next_context;
	else
	    R_ThreadContext = R_ActiveThreads();
	R_DeactivateThread(tc);
    }
    else {
	R_ReturnedValue = R_NilValue;
	R_ThreadContext->wakeup = delta >= 0.0 ? current_time() + delta : 0.0;
	R_ThreadContext->ioready = ioready;
	R_ThreadContext->ioprepsleep = ioprepsleep;
	R_ThreadContext->iodata = iodata;
        R_ThreadContext->state = R_thread_state_blocked;
	R_SuspendCurrentThread(code);
    }

    while ((next = R_FindNextRunnableThread(R_ThreadContext)) == NULL)
	block_for_activity();
    return R_ResumeThread(next);
}

/***** if joins can be broken by interrupts, then we need to remove
       threads from the join list */
R_code_t R_JoinThread(SEXP thread, R_code_t cont)
{
    R_thread_context_t tc;

    /*CHECK_THREAD(thread);*/

    tc = R_THREAD_CONTEXT(thread);
    if (tc != NULL) {
	R_ThreadContext->wait_object = thread;
	R_ThreadContext->wait_next = tc->join;
	tc->join = R_ThreadContext;
	return R_BlockUntil(NULL, NULL, -1.0, R_NilValue, cont);
    }
    else {
	R_ReturnedValue = R_THREAD_VALUE(thread);
	return cont;
    }
}

static void count_threads(char *where)
{
    R_thread_context_t threads;
    int n;

    for (n = 0, threads = R_ActiveThreads();
	 threads != NULL;
	 threads = threads->next_context)
	n++;

    REprintf("number of active threads %s: %d\n", where, n);
}
	
DECLARE_CONTINUATION(start_thread_cont, start_thread_cont_fun);
DECLARE_CONTINUATION(thread_top_cont, thread_top_cont_fun);
DECLARE_CONTINUATION(thread_topjmp_cont, thread_topjmp_cont_fun);

/**** think through catching of errors here */
/**** need to make sure right dispatcher is used for new thread */
R_code_t R_NewThread_nr(SEXP fun, R_code_t cont)
{
    SEXP call;
    R_thread_context_t thread, oldthread;
    R_dispatcher_t top_dispatcher;

    if (! R_threads_enabled)
	error("threads are not enabled");

    if (! isFunction(fun))
	error("invalid thread function");

    /* compute the top level dispatcher */
    for (top_dispatcher = R_Dispatcher;
	 top_dispatcher->next != NULL;
	 top_dispatcher = top_dispatcher->next);

    /* create new thread context and protect expr and rho in it _before_
       suspending the current thread (or the PP stack gets confused */
    PROTECT(call = LCONS(fun, R_NilValue));
    thread = R_MakeThreadContext(R_thread_state_runnable, call);
    UNPROTECT(1);

    oldthread = R_ThreadContext;
    R_SuspendCurrentThread(cont);

    /* initialize registers of the new thread */
    R_ThreadContext = thread;
    R_CurrentEnv = R_GlobalEnv;
    R_EvalDepth = 0;
    R_EvalFrame = R_NilValue;

    /* create the new thread's top level context */
    begincontext(CTXT_TOPLEVEL, R_NilValue, R_NilValue,
		 R_NilValue, R_NilValue);
    R_GlobalContext->contcode = thread_topjmp_cont;
    if (R_Dispatcher != top_dispatcher)
	R_GlobalContext->dispatcher = top_dispatcher;

    oldthread->value = thread->ref;

    /**** allow thread to be suspended until explicitly started? */
    if (R_Dispatcher == top_dispatcher)
	/* run the new thread immediately */
	return R_eval_nr(call, thread_top_cont);
    else {
	/* resume the creating thread */
	R_SuspendCurrentThread(start_thread_cont);
	if (R_Dispatcher != top_dispatcher)
	    R_GlobalContext->dispatcher = top_dispatcher;
	return R_ResumeThread(oldthread);
    }
}

static R_code_t start_thread_cont_fun(R_code_t code)
{
    SEXP call = R_ThreadContext->prot;
    return R_eval_nr(call, thread_top_cont);
}


/* continuation for ordinary returns to the thread top level */
/***** interacts badly with interrupts */
static R_code_t thread_top_cont_fun(R_code_t code)
{
    R_thread_context_t tc;

    /**** make sure the current value is NAMED -- is this enough? */
    if (NAMED(R_ReturnedValue) != 2)
	SET_NAMED(R_ReturnedValue, 2);

    /*endcontext(); *//* not really needed -- harmful if signal arrives? */

    /**** need to deal with this properly */
    if (R_ThreadContext->wait_object != R_NilValue)
	R_Suicide("improperly broken wait--don't know how to fix yet");

    while ((tc = pop_thread_queue(&(R_ThreadContext->join))) != NULL)
	wake_thread(tc, R_ReturnedValue);

    return R_BlockUntil(NULL, NULL, -1.0, R_NilValue, thread_done_cont);
}

/* continuation for jumps to the thread top level */
static R_code_t thread_topjmp_cont_fun(R_code_t code)
{
    R_ReturnedValue = R_NilValue;
    return thread_top_cont;
}


DECLARE_CONTINUATION(do_concur_start_cont, do_concur_start_cont_fun);
DECLARE_CONTINUATION(do_concur_join_cont, do_concur_join_cont_fun);

R_code_t do_concur_nr(SEXP call, SEXP op, SEXP args, SEXP rho,
			  R_code_t code)
{
    static R_thread_context_t R_MainContext = NULL;

    R_threads_enabled = TRUE;

    if (R_MainContext == NULL)
	R_MainContext = R_ThreadContext;
    else if (R_ThreadContext != R_MainContext)
	error("can only run new threads from the main thread");

    if (R_Dispatcher->level != 0)
	error("can only run threads from top level dispatcher");

    if (args == R_NilValue) {
	R_ReturnedValue = R_NilValue;
	return code;
    }
    else {
	PUSH_EVAL_FRAME(code, args, args);
	return R_NewThread_nr(CAR(args), do_concur_start_cont);
    }
}

static R_code_t do_concur_start_cont_fun(R_code_t code)
{
    SEXP args = EVAL_FRAME_VAR1();

    SETCAR(args, R_ReturnedValue);
    args = CDR(args);
    if (args == R_NilValue) {
	args = EVAL_FRAME_VAR0();
	SET_EVAL_FRAME_VAR1(args);
	return R_JoinThread(CAR(args), do_concur_join_cont);
    }
    else {
	SET_EVAL_FRAME_VAR1(args);
	return R_NewThread_nr(CAR(args), do_concur_start_cont);
    }
}

static R_code_t do_concur_join_cont_fun(R_code_t code)
{
    SEXP args = EVAL_FRAME_VAR1();

    SETCAR(args, R_ReturnedValue);

    args = CDR(args);
    if (args == R_NilValue) {
	R_ReturnedValue = PairToVectorList(EVAL_FRAME_VAR0());
	code = EVAL_FRAME_CODE();
	POP_EVAL_FRAME();
	return code;
    }
    else {
	SET_EVAL_FRAME_VAR1(args);
	return R_JoinThread(CAR(args), do_concur_join_cont);
    }
}

SEXP  do_yield(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("can only run threads from eval");
    return R_NilValue; /* not reached */
}

R_code_t R_ThreadYield(R_code_t code)
{
    R_thread_context_t next;

    next = R_FindNextRunnableThread(R_ThreadContext->next_context);
    if (next != R_ThreadContext) {
	R_SuspendCurrentThread(code);
	return R_ResumeThread(next);
    }
    else return code;
}
    
R_code_t do_yield_nr(SEXP call, SEXP op, SEXP args, SEXP rho, R_code_t code)
{
    checkArity(op, args);

    R_ReturnedValue = R_NilValue;
    return R_ThreadYield(code);
}

SEXP  do_snooze(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("can only run threads from eval");
    return R_NilValue; /* not reached */
}

R_code_t do_snooze_nr(SEXP call, SEXP op, SEXP args, SEXP rho, R_code_t code)
{
    double delta;

    checkArity(op, args);

    delta = asReal(CAR(args));
    if (ISNAN(delta))
	error("invalid sleep period");

    return R_BlockUntil(NULL, NULL, delta > 0.0 ? delta : -1.0,
			R_NilValue, code);
}

#include "IOStuff.h"
#include "Parse.h"

int R_ParseBrowser(SEXP, SEXP);
char *R_PromptString(int, int);

SEXP  do_replcon(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("can only run threads from eval");
    return R_NilValue; /* not reached */
}

struct replstate_st {
    int status, prompt_type, savestack, prompt_needed, len;
    unsigned char *bufp, buf[1024];
};

Rboolean process_system_command(char *bufp)
{
#ifdef SHELL_ESCAPE
    if (*bufp == '!') {
#ifdef HAVE_SYSTEM
	system(bufp + 1);
#else
	Rprintf("error: system commands are not supported"
		"in this version of R.\n");
#endif
	bufp[0] = '\0';
	return TRUE;
    }
#endif
    return FALSE;
}

static Rboolean console_ready(SEXP data)
{
    struct timeval tv = { 0, 0 };
    fd_set rfd;
    FD_ZERO(&rfd);
    FD_SET(fileno(stdin), &rfd);
    return select(fileno(stdin) + 1, &rfd, NULL, NULL, &tv);
}    

static void console_prep_sleep(R_fd_set_t fds, SEXP data)
{
    R_fd_set(fileno(stdin), fds);
}

static R_code_t replcon_done(void);

DECLARE_CONTINUATION(replcon_loop_cont, replcon_loop_cont_fun);
DECLARE_CONTINUATION(replcon_eval_cont, replcon_eval_cont_fun);

R_code_t do_replcon_nr(SEXP call, SEXP op, SEXP args, SEXP rho, R_code_t code)
{
    SEXP rsdata;
    struct replstate_st *rs;

    checkArity(op, args);

    if(R_Verbose)
	REprintf(" >R_ReplConsole(): before \"for(;;)\" {main.c}\n");

    rsdata = allocString(sizeof(struct replstate_st));
    PUSH_EVAL_FRAME(code, rho, rsdata);

    rs = (struct replstate_st *) CHAR(rsdata);
    rs->prompt_type = 1;
    rs->buf[0] = '\0';
    rs->bufp = rs->buf;
    rs->prompt_needed = TRUE;
    rs->len = sizeof(rs->buf);

    /***** this is really gross but seems to be needed (for now at
           least) because of the parser internals */
    rs->savestack = R_PPStackTop;

    if (! isEnvironment(CAR(args)))
	error("bad environment argument");
    R_CurrentEnv = CAR(args);
    R_IoBufferWriteReset(&R_ConsoleIob);

    return replcon_loop_cont;
}

/**** the readline support uses globals (and saves references to the buffer)
      so it desparately needs a lock */
static R_code_t replcon_loop_cont_fun(R_code_t code)
{
#ifdef HAVE_LIBREADLINE
    extern int UsingReadline;
#endif
    SEXP expr;
    int c;
    struct replstate_st *rs;
    rs = (struct replstate_st *) CHAR(EVAL_FRAME_VAR1());

    if(*rs->bufp == 0) {
	if (rs->prompt_needed) {
	    char *prompt = R_PromptString(R_BrowseLevel, rs->prompt_type);
	    R_Busy(0);
	    if(R_Interactive) {
#ifdef HAVE_LIBREADLINE
		if (UsingReadline)
		    R_StartReadline(prompt, rs->buf, sizeof(rs->buf), 1);
		else
#endif
		    {
			fputs(prompt, stdout);
			fflush(stdout);
		    }
	    }
	    else if (!R_Slave)
		fputs(prompt, stdout);
	    rs->prompt_needed = FALSE;
	}
	if (! console_ready(R_NilValue))
	    return R_BlockUntil(console_ready, console_prep_sleep, -1.0,
				R_NilValue, replcon_loop_cont);
#ifdef HAVE_LIBREADLINE
	if (R_Interactive && UsingReadline) {
	    switch(R_ReadlineReadOneChar()) {
	    case R_readline_eof: return replcon_done();
	    case R_readline_gotaline: break;
	    default: return replcon_loop_cont;
	    }
	}
	else
#endif
	    {
		if (fgets(rs->buf, sizeof(rs->buf), stdin) == NULL)
		    return replcon_done();
		if (! R_Interactive) {
		    int ll = strlen(rs->buf);
		    /* remove CR in CRLF ending */
		    if (rs->buf[ll - 1] == '\n' && rs->buf[ll - 2] == '\r') {
			rs->buf[ll - 2] = '\n';
			rs->buf[--ll] = '\0';    
		    }
		    /* according to system.txt, should be terminated
		       in \n, so check this at eof */
		    if (feof(stdin) && rs->buf[ll - 1] != '\n' &&
			ll < rs->len) {
			rs->buf[ll++] = '\n'; rs->buf[ll] = '\0';
		    }
		    if (!R_Slave)
			fputs(rs->buf, stdout);
		}
	    }
	rs->bufp = rs->buf;
	rs->prompt_needed = TRUE;
    }

    if (! process_system_command(rs->buf)) {

	while((c = *rs->bufp++)) {
	    R_IoBufferPutc(c, &R_ConsoleIob);
	    if(c == ';' || c == '\n') break;
	}

	R_PPStackTop = rs->savestack;
	R_Parse1Buffer(&R_ConsoleIob, 0, &rs->status);

	switch(rs->status) {
	case PARSE_NULL:
	    if (R_BrowseLevel)
		return replcon_done();
	    R_IoBufferWriteReset(&R_ConsoleIob);
	    rs->prompt_type = 1;
	    return replcon_loop_cont;
	case PARSE_OK:
	    R_IoBufferReadReset(&R_ConsoleIob);
	    expr = R_Parse1Buffer(&R_ConsoleIob, 1, &rs->status);
	    if (R_BrowseLevel) {
		int browsevalue = R_ParseBrowser(expr, R_CurrentEnv);
		if(browsevalue == 1 )
		    return replcon_done();
		if(browsevalue == 2 ) {
		    R_IoBufferWriteReset(&R_ConsoleIob);
		    return replcon_loop_cont;
		}
	    }
	    R_Visible = 0;
	    R_EvalDepth = 0;
	    PROTECT(expr);
	    R_Busy(1);
	    UNPROTECT(1);
	    return R_eval_nr(expr, replcon_eval_cont);
	case PARSE_ERROR: error("syntax error");
	case PARSE_INCOMPLETE:
	    R_IoBufferReadReset(&R_ConsoleIob);
	    rs->prompt_type = 2;
	    return replcon_loop_cont;
	case PARSE_EOF: return replcon_done();
	}
    }
    return replcon_done();
}

static R_code_t replcon_eval_cont_fun(R_code_t code)
{
    struct replstate_st *rs;
    SEXP val = R_ReturnedValue;

    rs = (struct replstate_st *) CHAR(EVAL_FRAME_VAR1());

    SET_SYMVALUE(R_LastvalueSymbol, val);
    if (R_Visible) {
	PROTECT(val);
	PrintValueEnv(val, R_CurrentEnv);
	UNPROTECT(1);
    }
    if (R_CollectWarnings)
	PrintWarnings();
    R_IoBufferWriteReset(&R_ConsoleIob);
    rs->prompt_type = 1;
    return replcon_loop_cont;
}

static R_code_t replcon_done(void)
{
    R_code_t code = EVAL_FRAME_CODE();
    R_CurrentEnv = EVAL_FRAME_VAR0();
    POP_EVAL_FRAME();
    R_ReturnedValue = R_NilValue;
    return code;
}

Rboolean R_HandlerInputAvailable(SEXP);
void R_HandlerPrepareToSleep(R_fd_set_t, SEXP);
void R_HandleHandlerInput(void);
extern int R_wait_usec;

SEXP  do_evntloop(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("can only run threads from eval");
    return R_NilValue; /* not reached */
}

DECLARE_CONTINUATION(evtloop_cont, evtloop_cont_fun);

R_code_t do_evntloop_nr(SEXP call, SEXP op, SEXP args, SEXP rho, R_code_t code)
{
    double delta;

    checkArity(op, args);

    delta = R_wait_usec ? ((double) R_wait_usec) / 1000000.0 : -1.0;
    return R_BlockUntil(R_HandlerInputAvailable,
			R_HandlerPrepareToSleep,
			delta, R_NilValue, evtloop_cont);
}

R_code_t evtloop_cont_fun(R_code_t code)
{
    double delta = R_wait_usec ? ((double) R_wait_usec) / 1000000.0 : -1.0;
    R_HandleHandlerInput();
    return R_BlockUntil(R_HandlerInputAvailable,
			R_HandlerPrepareToSleep,
			delta, R_NilValue, evtloop_cont);
}    

static void thread_onintr(int sig)
{
    signal(sig, R_SigintHandler);
    if (R_interrupts_suspended)
	R_interrupt_pending = TRUE;
    else {
	dequeue_thread(R_ThreadContext);
	wake_thread(R_ThreadContext, mkFalse());
	REprintf("\n");
	jump_to_toplevel();
    }
}

#define R_MAINFUN ".Main"

void R_ThreadMain(void)
{
    void R_DefaultMainFun(void);
    R_dispatcher_t top_dispatcher = R_Dispatcher;

    signal(SIGINT, R_SigintHandler);
    signal(SIGUSR1,onsigusr1);
    signal(SIGUSR2,onsigusr2);

    if (SETJMP(top_dispatcher->cjmpbuf))
	R_Suicide("Running " R_MAINFUN " failed");
    else {
	SEXP findVar1mode(SEXP, SEXP, SEXPTYPE, int);
	SEXP mainsym = install(R_MAINFUN);
	SEXP mainfun = findVar1mode(mainsym, R_GlobalEnv, FUNSXP, TRUE);
	if (mainfun != R_UnboundValue) {
	    R_code_t code;
	    SEXP call = LCONS(mainsym, R_NilValue);
	    R_CurrentEnv = R_GlobalEnv;
	    PROTECT(call);
	    R_SigintHandler = thread_onintr;
	    signal(SIGINT, R_SigintHandler);
	    if (SETJMP(top_dispatcher->cjmpbuf))
		code = R_GlobalContext->contcode;
	    else
		code = R_eval_nr(call, NULL);
	    if (code != NULL)
		R_run_dispatcher(code);
	    UNPROTECT(1);
	}
	else {
	    SETJMP(top_dispatcher->cjmpbuf);
	    	
	    signal(SIGINT, R_SigintHandler);
	    signal(SIGUSR1,onsigusr1);
	    signal(SIGUSR2,onsigusr2);

	    R_DefaultMainFun();
	}
    }
}

SEXP do_activethreads(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    R_thread_context_t tc;
    SEXP val;
    int i, n;

    checkArity(op, args);

    for (n = 0, tc = R_ActiveThreads(); tc != NULL; tc = tc->next_context)
	n++;

    val = allocVector(VECSXP, n);
    for (i = 0, tc = R_ActiveThreads(); i < n; i++, tc = tc->next_context)
	SET_VECTOR_ELT(val, i, tc->ref);

    return val;
}

SEXP do_currentthread(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    R_thread_context_t tc;
    SEXP val;
    int i, n;

    checkArity(op, args);
    return R_ThreadContext->ref;
}

#define CHECK_THREAD(s) do { \
    SEXP __s__ = (s); \
    if (TYPEOF(s) != EXTPTRSXP || EXTPTR_TAG(s) != R_ThreadTag) \
        error("bad thread object"); \
} while (0)

SEXP do_threadname(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP thread;

    checkArity(op, args);

    thread = CAR(args);
    CHECK_THREAD(thread);
    return R_THREAD_NAME(thread);
}
    
SEXP do_setthreadname(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP thread, val;

    checkArity(op, args);

    thread = CAR(args);
    CHECK_THREAD(thread);

    val = CADR(args);
    if (isNull(val))
	R_SET_THREAD_NAME(thread, R_NilValue);
    else if (isValidString(val) && LENGTH(val) == 1) {
	if (NAMED(val)) val = duplicate(val); /**** is this enough? */
	R_SET_THREAD_NAME(thread, val);
    }
    else
	error("invalid thread name");
    return val;
}
    
SEXP  do_newthread(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("can only create threads from eval");
    return R_NilValue; /* not reached */
}

R_code_t do_newthread_nr(SEXP call, SEXP op, SEXP args, SEXP rho,
			 R_code_t code)
{
    checkArity(op, args);
    return R_NewThread_nr(CAR(args), code);
}

SEXP  do_jointhread(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("can only join threads from eval");
    return R_NilValue; /* not reached */
}

R_code_t do_jointhread_nr(SEXP call, SEXP op, SEXP args, SEXP rho,
			  R_code_t code)
{
    SEXP thread;
    checkArity(op, args);
    thread = CAR(args);
    CHECK_THREAD(thread);
    return R_JoinThread(thread, code);
}

SEXP  do_threadsenabled(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    if (args != R_NilValue) {
	Rboolean val = asLogical(CAR(args));
	if (val == NA_LOGICAL)
	    error("invalid setting for threads enabled");
	R_threads_enabled = val;
    }
    return R_threads_enabled ? mkTrue() : mkFalse();
}

SEXP  do_preemptsched(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    if (args != R_NilValue) {
	Rboolean val = asLogical(CAR(args));
	if (val == NA_LOGICAL)
	    error("invalid setting for threads enabled");
	R_preemptive_scheduling = val;
    }
    return R_preemptive_scheduling ? mkTrue() : mkFalse();
}

#define CHECK_MUTEX(s) do { \
    SEXP __s__ = (s); \
    if (TYPEOF(s) != EXTPTRSXP || EXTPTR_TAG(s) != R_MutexTag) \
        error("bad mutex object"); \
} while (0)

#define CHECK_CONDVAR(s) do { \
    SEXP __s__ = (s); \
    if (TYPEOF(s) != EXTPTRSXP || EXTPTR_TAG(s) != R_CondvarTag) \
        error("bad condition variable object"); \
} while (0)

static SEXP check_name(SEXP name)
{
    if (isNull(name))
	name = R_NilValue;
    else if (isValidString(name) && LENGTH(name) == 1) {
	if (NAMED(name)) name = duplicate(name);
    }
    else
	error("invalid name string");
    return name;
}

SEXP do_newmutex(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP data, mx, name;

    checkArity(op, args);

    PROTECT(data = allocVector(VECSXP, NUM_MUTEX_FIELDS));
    PROTECT(mx = R_MakeExternalPtr(NULL, R_MutexTag, data));
    R_SET_MUTEX_NAME(mx, check_name(CAR(args)));
    R_SET_MUTEX_LOCKED(mx, FALSE);
    UNPROTECT(2);
    return mx;
}

SEXP do_newcondvar(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP data, cv, name;

    checkArity(op, args);

    PROTECT(data = allocVector(VECSXP, NUM_CONDVAR_FIELDS));
    PROTECT(cv = R_MakeExternalPtr(NULL, R_CondvarTag, data));
    R_SET_CONDVAR_NAME(cv, check_name(CAR(args)));
    UNPROTECT(2);
    return cv;
}

SEXP do_mutexname(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP mx;
    checkArity(op, args);
    mx = CAR(args);
    CHECK_MUTEX(mx);
    return duplicate(R_MUTEX_NAME(mx));
}

SEXP do_condvarname(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP cv;
    checkArity(op, args);
    cv = CAR(args);
    CHECK_CONDVAR(cv);
    return duplicate(R_CONDVAR_NAME(cv));
}

SEXP do_mutexdata(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP mx;
    checkArity(op, args);
    mx = CAR(args);
    CHECK_MUTEX(mx);
    return duplicate(R_MUTEX_DATA(mx));
}

SEXP do_condvardata(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP cv;
    checkArity(op, args);
    cv = CAR(args);
    CHECK_CONDVAR(cv);
    return duplicate(R_CONDVAR_DATA(cv));
}

SEXP do_setmutexdata(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP mx, val;
    checkArity(op, args);
    mx = CAR(args);
    CHECK_MUTEX(mx);
    val = duplicate(CADR(args));
    R_SET_MUTEX_DATA(mx, val);
    return val;
}

SEXP do_setcondvardata(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP cv, val;
    checkArity(op, args);
    cv = CAR(args);
    CHECK_CONDVAR(cv);
    val = duplicate(CADR(args));
    R_SET_CONDVAR_DATA(cv, val);
    return val;
}

SEXP do_condvarsignal(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP cv;
    R_thread_context_t tc;

    checkArity(op, args);
    cv = CAR(args);
    CHECK_CONDVAR(cv);

    tc = pop_thread_queue(R_CONDVAR_QUEUE(cv));
    if (tc != NULL)
	wake_thread(tc, mkTrue());
    
    return R_NilValue;
}

SEXP do_condvarbroadcast(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP cv;
    R_thread_context_t tc;

    checkArity(op, args);
    cv = CAR(args);
    CHECK_CONDVAR(cv);

    while ((tc = pop_thread_queue(R_CONDVAR_QUEUE(cv))) != NULL)
	wake_thread(tc, mkTrue());

    return R_NilValue;
}

SEXP  do_mutexlock(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("can only lock mutexes from eval");
    return R_NilValue; /* not reached */
}

DECLARE_CONTINUATION(do_mutexlock_cont, do_mutexlock_cont_fun);

R_code_t do_mutexlock_nr(SEXP call, SEXP op, SEXP args, SEXP rho,
			 R_code_t code)
{
    SEXP mx, thread;

    checkArity(op, args);

    mx = CAR(args);
    thread = CADDR(args);
    CHECK_MUTEX(mx);
    if (thread != R_NilValue)
	CHECK_THREAD(thread);
    
    if (! R_MUTEX_LOCKED(mx)) {
	R_SET_MUTEX_LOCKED(mx, TRUE);
	R_SET_MUTEX_OWNER(mx, thread);
	R_ReturnedValue = mkTrue();
	return code;
    }
    else {
	double delta = asReal(CADR(args));
	if (ISNAN(delta))
	    error("invalid timeout value");
	
	R_ThreadContext->wait_object = mx;
	enqueue_thread(R_MUTEX_QUEUE(mx), R_ThreadContext);
	PUSH_EVAL_FRAME(code, mx, thread);
	return R_BlockUntil(NULL, NULL, delta, R_NilValue, do_mutexlock_cont);
    }
}

static R_code_t do_mutexlock_cont_fun(R_code_t code)
{
    SEXP mx = EVAL_FRAME_VAR0();
    SEXP thread = EVAL_FRAME_VAR1();
    code = EVAL_FRAME_CODE();
    POP_EVAL_FRAME();

    if (asLogical(R_ReturnedValue))
	R_SET_MUTEX_OWNER(mx, thread); /*  putex will be locked already */
    return code;
}

SEXP  do_mutexunlock(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("can only unlock mutexes from eval");
    return R_NilValue; /* not reached */
}

R_code_t do_mutexunlock_nr(SEXP call, SEXP op, SEXP args, SEXP rho,
			  R_code_t code)
{
    SEXP mx, cv;

    checkArity(op, args);

    mx = CAR(args);
    cv = CADR(args);
    CHECK_MUTEX(mx);
    if (cv != R_NilValue)
	CHECK_CONDVAR(cv);

    if (R_MUTEX_LOCKED(mx)) {
	R_thread_context_t tc = pop_thread_queue(R_MUTEX_QUEUE(mx));
	R_SET_MUTEX_OWNER(mx, R_NilValue);
	if (tc == NULL)
	    R_SET_MUTEX_LOCKED(mx, FALSE);
	else
	    wake_thread(tc, mkTrue());
	R_ReturnedValue = mkTrue();
	if (cv != R_NilValue) {
	    double delta = asReal(CADDR(args));
	    if (ISNAN(delta))
		error("invalid timeout value");

	    R_ThreadContext->wait_object = cv;
	    enqueue_thread(R_CONDVAR_QUEUE(cv), R_ThreadContext);
	    return R_BlockUntil(NULL, NULL, delta, R_NilValue, code);
	}
    }
    else if (cv != R_NilValue)
	error("can't wait on condition variable unless mutex is locked");
    else
	R_ReturnedValue = mkTrue();
    return code;
}

/*
##### Load all this into a workspace, save it and restart
Yield <- function() .Internal(Yield())     
Snooze <- function(d) .Internal(Snooze(d))     
ActiveThreads <- function() .Internal(ActiveThreads())
CurrentThread <- function() .Internal(CurrentThread())
ThreadName <- function(t) .Internal(ThreadName(t))
SetThreadName <- function(t, n) .Internal(SetThreadName(t, n))
NewThread <- function(f) .Internal(NewThread(f))
JoinThread <- function(t) .Internal(JoinThread(t))
ThreadsEnabled <-function(val) {
    if (missing(val)) .Internal(ThreadsEnabled())
    else .Internal(ThreadsEnabled(val))
}

Concurrently<-function(...) .Internal(Concurrently(...))

Concurrently <- function(...) {
    if (! ThreadsEnabled()) ThreadsEnabled(TRUE)
    v <- list(...)
    for (i in seq(along = v))
        v[[i]] <- NewThread(v[[i]])
    for (i in seq(along = v))
        v[i] <- list(JoinThread(v[[i]]))
    v
}

ReplConsole <- function(env = .GlobalEnv) {
    op <- getOption("prompt")
    oc <- getOption("continue")
    on.exit(options(prompt = op, continue = oc))
    options(prompt = paste("REPL", op), continue = paste("REPL", oc))
    .Internal(ReplConsole(env))
}
#### need error trapping here
EventLoop <- function() .Internal(EventLoop())

.Main <- function() {
    if (exists("MainThread"))
        stop("can't call .Main twice")
    ThreadsEnabled(TRUE)
    MainThread <<- ActiveThreads()[[1]]  ### need CurrentThread
    SetThreadName(MainThread, "Main Thread")
    EventLoopThread <<- NewThread(EventLoop)
    SetThreadName(EventLoopThread, "Event Loop Thread")
    while(! is.null(try({ ReplConsole(); NULL}))) {}
}

NewMutex <- function(name = NULL) .Internal(NewMutex(name))
NewCondvar <- function(name = NULL) .Internal(NewCondvar(name))
MutexName <- function(mx) .Internal(MutexName(mx))
CondvarName <- function(cv) .Internal(CondvarName(cv))
MutexData <- function(mx) .Internal(MutexData(mx))
CondvarData <- function(cv) .Internal(CondvarData(cv))
SetMutexData <- function(mx, data) .Internal(SetMutexData(mx, data))
SetCondvarData <- function(cv, data) .Internal(SetCondvarData(cv, data))
CondvarSignal <- function(cv) .Internal(CondvarSignal(cv))
CondvarBroadcast <- function(cv) .Internal(CondvarBroadcast(cv))
MutexLock <- function(mx, timeout = -1.0, thread = CurrentThread())
    .Internal(MutexLock(mx, timeout, thread))
MutexUnlock <- function(mx, cv = NULL, timeout = -1.0)
    .Internal(MutexUnock(mx, cv, timeout))

####### end of stuff to load

Concurrently(function() 1+2, function() 3+4)
Concurrently(function() stop("A"), function() 2+3)

f<-function(s, n) for (i in 1:n) cat(paste(s, i, "\n"))
g<-function(s, n) for (i in 1:n) { cat(paste(s, i, "\n")); Yield() }
h<-function(s, n) for (i in 1:n) { cat(paste(s, i, "\n")); Snooze(0.5) }
k<-function(s, n) for (i in 1:n) if (i %% 500 == 0) cat(paste(s, i, "\n"))
Concurrently(function() f("A",3), function() f("B",3))
Concurrently(function() g("A",3), function() g("B",3))
Concurrently(function() h("A",3), function() h("B",3))
Concurrently(function() k("A",5000), function() k("B",5000))

#### need error trapping here
TclLoop <- function() repeat{ .C("TclHandlerOnly"); Snooze(0.02) }
options(no.tcltk.handler=TRUE)

library(tcltk)
NewThread(TclLoop)

plot(1:10)
demo(tkcanvas)
^D
^C
^C

f<-function() {
    MutexLock(mx)
    while (! x) {
        print("waiting")
        MutexUnlock(mx, cv)
        MutexLock(mx)
    }
    print("done")
}

x<-FALSE
mx <- NewMutex()
cv <- NewCondvar()
MutexLock(mx)
NewThread(f)
MutexUnlock(mx)
CondvarSignal(cv)
x<-T
CondvarSignal(cv)
*/
