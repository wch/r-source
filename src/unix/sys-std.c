/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2017  The R Core Team
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

/* <UTF8> char here is mainly handled as a whole string.
   Does need readline to support it.
   Appending \n\0 is OK in UTF-8, not general MBCS.
   Removal of \r is OK on UTF-8.
   ? use of isspace OK?
 */


/* See system.txt for a description of functions */

/* select() is essential here, but configure has required it */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>

#ifdef HAVE_STRINGS_H
   /* may be needed to define bzero in FD_ZERO (eg AIX) */
  #include <strings.h>
#endif

#include "Fileio.h"
#include "Runix.h"
#include "Startup.h"
#include <R_ext/Riconv.h>
#include <R_ext/Print.h> // for REprintf

#define __SYSTEM__
/* includes <sys/select.h> and <sys/time.h> */
#include <R_ext/eventloop.h>
#undef __SYSTEM__

#ifdef HAVE_UNISTD_H
# include <unistd.h>		/* for unlink */
#endif

extern SA_TYPE SaveAction;
extern Rboolean UsingReadline;
extern FILE* ifp; /* from system.c */

/*
 *  1) FATAL MESSAGES AT STARTUP
 */

void attribute_hidden Rstd_Suicide(const char *s)
{
    REprintf("Fatal error: %s\n", s);
    /* Might be called before translation is running */
    R_CleanUp(SA_SUICIDE, 2, 0);
}

/*
 *  2. CONSOLE I/O
 */



	/*--- I/O Support Code ---*/

	/* These routines provide hooks for supporting console I/O.
	 * Under raw Unix these routines simply provide a
	 * connection to the stdio library.
	 * Under a Motif interface the routines would be
	 * considerably more complex.
	 */

/*
  The following provides a version of select() that catches interrupts
  and handles them using the supplied interrupt handler or the default
  one if NULL is supplied.  The interrupt handler must exit using a
  longjmp.  If the supplied timout value os zero, select is called
  without setting up an error handler since it should return
  immediately.
 */

static SIGJMP_BUF seljmpbuf;

static RETSIGTYPE (*oldSigintHandler)(int) = SIG_DFL;

typedef void (*sel_intr_handler_t)(void);

static RETSIGTYPE NORET handleSelectInterrupt(int dummy)
{
    signal(SIGINT, oldSigintHandler);
    SIGLONGJMP(seljmpbuf, 1);
}

int R_SelectEx(int  n,  fd_set  *readfds,  fd_set  *writefds,
	       fd_set *exceptfds, struct timeval *timeout,
	       void (*intr)(void))
{
    if (timeout != NULL && timeout->tv_sec == 0 && timeout->tv_usec == 0)
	/* Is it right for select calls with a timeout to be
	   non-interruptable? LT */
	return select(n, readfds, writefds, exceptfds, timeout);
    else {
	volatile sel_intr_handler_t myintr = intr != NULL ?
	    intr : onintrNoResume;
	volatile int old_interrupts_suspended = R_interrupts_suspended;
	if (SIGSETJMP(seljmpbuf, 1)) {
	    myintr();
	    R_interrupts_suspended = old_interrupts_suspended;
	    error(_("interrupt handler must not return"));
	    return 0; /* not reached */
	}
	else {
	    int val;

	    /* make sure interrupts are enabled -- this will be
	       restored if there is a LONGJMP from myintr() to another
	       context. */
	    R_interrupts_suspended = FALSE;

	    /* install a temporary signal handler for breaking out of
	       a blocking select */
	    oldSigintHandler = signal(SIGINT, handleSelectInterrupt);

	    /* once the new sinal handler is in place we need to check
	       for and handle any pending interrupt registered by the
	       standard handler. */
	    if (R_interrupts_pending)
		myintr();

	    /* now do the (possibly blocking) select, restore the
	       signal handler, and return the result of the select. */
	    val = select(n, readfds, writefds, exceptfds, timeout);
	    signal(SIGINT, oldSigintHandler);
	    R_interrupts_suspended = old_interrupts_suspended;
	    return val;
	}
    }
}


/*
   This object is used for the standard input and its file descriptor
   value is reset by setSelectwblplotMask() each time to ensure that it points
   to the correct value of stdin.
 */
static InputHandler BasicInputHandler = {StdinActivity, -1, NULL};

/*
   This can be reset by the initialization routines which
   can ignore stdin, etc..
*/
InputHandler *R_InputHandlers = &BasicInputHandler;

/*
  Initialize the input source handlers used to check for input on the
  different file descriptors.
 */
InputHandler * initStdinHandler(void)
{
    InputHandler *inputs;

    inputs = addInputHandler(R_InputHandlers, fileno(stdin), NULL,
			     StdinActivity);
    /* Defer the X11 registration until it is loaded and actually used. */

    return(inputs);
}

/*
  Creates and registers a new InputHandler with the linked list `handlers'.
  This sets the global variable InputHandlers if it is not already set.
  In the standard interactive case, this will have been set to be the
  BasicInputHandler object.

  Returns the newly created handler which can be used in a call to
  removeInputHandler.
 */
InputHandler *
addInputHandler(InputHandler *handlers, int fd, InputHandlerProc handler,
		int activity)
{
    InputHandler *input, *tmp;
    input = (InputHandler*) calloc(1, sizeof(InputHandler));

    input->activity = activity;
    input->fileDescriptor = fd;
    input->handler = handler;

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

    return(input);
}

/*
  Removes the specified handler from the linked list.

  See getInputHandler() for first locating the target handler instance.
 */
int
removeInputHandler(InputHandler **handlers, InputHandler *it)
{
    InputHandler *tmp;

    /* If the handler is the first one in the list, move the list to point
       to the second element. That's why we use the address of the first
       element as the first argument.
    */

    if (it == NULL) return(0);

    if(*handlers == it) {
	*handlers = (*handlers)->next;
	free(it);
	return(1);
    }

    tmp = *handlers;

    while(tmp) {
	if(tmp->next == it) {
	    tmp->next = it->next;
	    free(it);
	    return(1);
	}
	tmp = tmp->next;
    }

    return(0);
}


InputHandler *
getInputHandler(InputHandler *handlers, int fd)
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

/*
 Arrange to wait until there is some activity or input pending
 on one of the file descriptors to which we are listening.

 We could make the file descriptor mask persistent across
 calls and change it only when a listener is added or deleted.
 Later.

 This replaces the previous version which looked only on stdin and the
 X11 device connection.  This allows more than one X11 device to be
 open on a different connection. Also, it allows connections a la S4
 to be developed on top of this mechanism.
*/

/* A package can enable polled event handling by making R_PolledEvents
   point to a non-dummy routine and setting R_wait_usec to a suitable
   timeout value (e.g. 100000) */

static void nop(void){}

void (* R_PolledEvents)(void) = nop;
int R_wait_usec = 0; /* 0 means no timeout */

/* For X11 devices */
void (* Rg_PolledEvents)(void) = nop;
int Rg_wait_usec = 0;


static int setSelectMask(InputHandler *, fd_set *);


fd_set *R_checkActivityEx(int usec, int ignore_stdin, void (*intr)(void))
{
    int maxfd;
    struct timeval tv;
    static fd_set readMask;

    if (R_interrupts_pending) {
	if (intr != NULL) intr();
	else onintr();
    }

    /* Solaris (but not POSIX) requires these times to be normalized.
       POSIX requires up to 31 days to be supported, and we only
       use up to 2147 secs here.
     */
    tv.tv_sec = usec/1000000;
    tv.tv_usec = usec % 1000000;
    maxfd = setSelectMask(R_InputHandlers, &readMask);
    if (ignore_stdin)
	FD_CLR(fileno(stdin), &readMask);
    if (R_SelectEx(maxfd+1, &readMask, NULL, NULL,
		   (usec >= 0) ? &tv : NULL, intr) > 0)
	return(&readMask);
    else
	return(NULL);
}

fd_set *R_checkActivity(int usec, int ignore_stdin)
{
    return R_checkActivityEx(usec, ignore_stdin, NULL);
}

/*
  Create the mask representing the file descriptors select() should
  monitor and return the maximum of these file descriptors so that
  it can be passed directly to select().

  If the first element of the handlers is the standard input handler
  then we set its file descriptor to the current value of stdin - its
  file descriptor.
 */

static int
setSelectMask(InputHandler *handlers, fd_set *readMask)
{
    int maxfd = -1;
    InputHandler *tmp = handlers;
    FD_ZERO(readMask);

    /* If we are dealing with BasicInputHandler always put stdin */
    if(handlers == &BasicInputHandler)
	handlers->fileDescriptor = fileno(stdin);

    while(tmp) {
	FD_SET(tmp->fileDescriptor, readMask);
	maxfd = maxfd < tmp->fileDescriptor ? tmp->fileDescriptor : maxfd;
	tmp = tmp->next;
    }

    return(maxfd);
}

void R_runHandlers(InputHandler *handlers, fd_set *readMask)
{
    InputHandler *tmp = handlers, *next;

    if (readMask == NULL) {
	Rg_PolledEvents();
	R_PolledEvents();
    } else
	while(tmp) {
	    /* Do this way as the handler function might call
	       removeInputHandlers */
	    next = tmp->next;
	    if(FD_ISSET(tmp->fileDescriptor, readMask)
	       && tmp->handler != NULL)
		tmp->handler((void*) tmp->userData);
	    tmp = next;
	}
}

/* The following routine is still used by the internet routines, but
 * it should eventually go away. */

InputHandler *
getSelectedHandler(InputHandler *handlers, fd_set *readMask)
{
    InputHandler *tmp = handlers;

    /*
      Temporarily skip the first one if a) there is another one, and
      b) this is the BasicInputHandler.
    */
    if(handlers == &BasicInputHandler && handlers->next)
	tmp = handlers->next;

    while(tmp) {
	if(FD_ISSET(tmp->fileDescriptor, readMask))
	    return(tmp);
	tmp = tmp->next;
    }
    /* Now deal with the first one. */
    if(FD_ISSET(handlers->fileDescriptor, readMask))
	return(handlers);

    return((InputHandler*) NULL);
}


#ifdef HAVE_LIBREADLINE
/* As from R 3.4.0, this implies we have the headers too.
   We use entry points

   rl_callback_handler_install
   rl_callback_handler_remove
   rl_callback_read_char
   rl_readline_name

   , if HAVE_RL_COMPLETION_MATCHES (>= 4.2)

   rl_attempted_completion_function
   rl_attempted_completion_over
   rl_basic_word_break_characters
   rl_completer_word_break_characters
   rl_completion_append_character
   rl_completion_matches
   rl_line_buffer

   and others conditionally:

   rl_cleanup_after_signal (>= 4.0)
   rl_done
   rl_end
   rl_free_line_state (>= 4.0)
   rl_line_buffer
   rl_mark
   rl_point
   rl_readline_state (>= 4.2)
   rl_resize_terminal (>= 4.0)
   rl_sort_completion_matches (>= 6.0)
 */

# include <readline/readline.h>

/* For compatibility with pre-readline-4.2 systems, 
   also missing in Apple's emulation via the NetBSD editline library.*/
# if !defined (_RL_FUNCTION_TYPEDEF)
typedef void rl_vcpfunc_t (char *);
# endif /* _RL_FUNCTION_TYPEDEF */

# if defined(RL_READLINE_VERSION) && RL_READLINE_VERSION >= 0x0603
/* readline 6.3's rl_callback_handler_install() no longer installs
   signal handlers, so as from that version we need an explicit
   one. (PR#16604)  (This could have been controlled in earlier versions
   by setting rl_catch_sigwinch.)
 */
#  define NEED_INT_HANDLER
# endif

attribute_hidden
char *R_ExpandFileName_readline(const char *s, char *buff)
{
#if defined(__APPLE__)
    char *s2 = tilde_expand((char *)s);
#else
    char *s2 = tilde_expand(s);
#endif

    strncpy(buff, s2, PATH_MAX);
    if(strlen(s2) >= PATH_MAX) buff[PATH_MAX-1] = '\0';
    free(s2);
    return buff;
}


# ifdef HAVE_READLINE_HISTORY_H
#  include <readline/history.h>
# endif


/* callback for rl_callback_read_char */


/*

There has been a general problem with asynchonous calls to browser and
anything that uses the standard console reading facilties asynchronously
(e.g. scan(), parse(), menu()).  The basic problem is as follows.  We
are in the usual input loop awaiting characters typed by the user.  Then
asynchronously, we enter the browser due to a callback that is invoked
from the background event loop that is active while waiting for the user
input.  At this point, we essentially are starting a new readline
session and it is important that we restore the old one when we complete
the browse-related one. But unfortunately, we are using global variables
and restoring it is not currently being done.
So this is an attempt to a) remove the global variables (which will
help with threading), and b) ensure that the relevant readline handlers
are restored when an asynchronous reader completes its task.

Cleaning up after errors is still an issue that needs investigation
and whether the current setup does the correct thing.
Related to this is whether nested calls (e.g. within a browser, we
do other calls to browser() or scan and whether these i)
accumulate on our readline stack, and ii) are unwound correctly.
If they don't accumulate, we need only keep  function pointers on
this stack. 10 seems safe for most use and is an improvement
over the abort's that we were getting due to the lack of
a readline handler being registered.
DTL.
*/

typedef struct _R_ReadlineData R_ReadlineData;

struct _R_ReadlineData {

 int readline_gotaline;
 int readline_addtohistory;
 int readline_len;
 int readline_eof;
 unsigned char *readline_buf;
 R_ReadlineData *prev;

};

static R_ReadlineData *rl_top = NULL;

#define MAX_READLINE_NESTING 10

static struct {
  int current;
  int max;
  rl_vcpfunc_t *fun[MAX_READLINE_NESTING];
} ReadlineStack = {-1, MAX_READLINE_NESTING - 1};

#ifdef NEED_INT_HANDLER
static volatile Rboolean caught_sigwinch = FALSE;

static RETSIGTYPE
R_readline_sigwinch_handler(int sig)
{
    caught_sigwinch = TRUE;
}
#endif

/*
  Registers the specified routine and prompt with readline
  and keeps a record of it on the top of the R readline stack.
 */
static void
pushReadline(const char *prompt, rl_vcpfunc_t f)
{
   if(ReadlineStack.current >= ReadlineStack.max) {
     warning(_("An unusual circumstance has arisen in the nesting of readline input. Please report using bug.report()"));
   } else
     ReadlineStack.fun[++ReadlineStack.current] = f;

   rl_callback_handler_install(prompt, f);

#ifdef NEED_INT_HANDLER
   signal(SIGWINCH, R_readline_sigwinch_handler);
#endif

   /* flush stdout in case readline wrote the prompt, but didn't flush
      stdout to make it visible. (needed for Apple's readline emulation). */
   fflush(stdout);
}

#if defined(RL_READLINE_VERSION) && RL_READLINE_VERSION >= 0x0600
/*
  Fix for PR#16603, for readline >= 6.0.

  The readline interface is somewhat messy. readline contains the
  function rl_free_line_state(), which its internal SIGINT handler
  calls. However, it only cancels keyboard macros and certain other
  things: it does not clear the line. Also, as of readline 6.3, its
  SIGINT handler is no longer triggered during our select() loop since
  rl_callback_handler_install() no longer installs signal handlers.
  So we have to catch the signal and do all the work ourselves to get
  Bash-like behavior on Ctrl-C.
 */
static void resetReadline(void)
{
    rl_free_line_state();
/* This might be helpful/needed in future, but we cannot tell until
   readline 7.0 is released.  Only info so far:
   https://lists.gnu.org/archive/html/bug-readline/2016-02/msg00000.html
#ifdef HAVE_RL_CALLBACK_SIGCLEANUP
    rl_callback_sigcleanup();
#endif
*/
    rl_cleanup_after_signal();
    RL_UNSETSTATE(RL_STATE_ISEARCH | RL_STATE_NSEARCH | RL_STATE_VIMOTION |
		  RL_STATE_NUMERICARG | RL_STATE_MULTIKEY);
    /* The following two lines should be equivalent, but doing both
       won't hurt. */
    rl_line_buffer[rl_point = rl_end = rl_mark = 0] = 0;
    rl_done = 1;
}
#endif

/*
  Unregister the current readline handler and pop it from R's readline
  stack, followed by re-registering the previous one.
*/
static void popReadline(void)
{
  if(ReadlineStack.current > -1) {
#if defined(RL_READLINE_VERSION) && RL_READLINE_VERSION >= 0x0600
     resetReadline();
#endif
     rl_callback_handler_remove();
     ReadlineStack.fun[ReadlineStack.current--] = NULL;
     if(ReadlineStack.current > -1 && ReadlineStack.fun[ReadlineStack.current])
	rl_callback_handler_install("", ReadlineStack.fun[ReadlineStack.current]);
  }
}

static void readline_handler(char *line)
{
    R_size_t buflen = rl_top->readline_len;

    popReadline();

    if ((rl_top->readline_eof = !line)) /* Yes, I don't mean ==...*/
	return;
    if (line[0]) {
# ifdef HAVE_READLINE_HISTORY_H
	if (strlen(line) && rl_top->readline_addtohistory)
	    add_history(line);
# endif
	/* We need to append a \n if the completed line would fit in the
	   buffer but not otherwise.  Byte [buflen] is zeroed in
	   the caller.
	*/
	strncpy((char *)rl_top->readline_buf, line, buflen);
	size_t l = strlen(line);
	if(l < buflen - 1) {
	    rl_top->readline_buf[l] = '\n';
	    rl_top->readline_buf[l+1] = '\0';
	}
    }
    else {
	rl_top->readline_buf[0] = '\n';
	rl_top->readline_buf[1] = '\0';
    }
    free(line);
    rl_top->readline_gotaline = 1;
}

/*
 An extension or override for the standard interrupt handler (Ctrl-C)
 that pops the readline stack and then calls the regular/standard
 interrupt handler. This could be done in a nicer and more general way.
 It may be necessary for embedding, etc. although it may not be an issue
 there (as the host application will presumably handle signals).
 by allowing us to add C routines to be called
 at the conclusion of the context. At the moment there is only one such routine
 allowed, and so we would have to chain them. This just leads to a different set of
 maintenance problems when we rely on the authors of individual routines to
 not break the chain!
 Note that the readline stack is not popped when a SIGUSR1 or SIGUSR2 occurs
 during the select. But of course, we are about to terminate the R session at
 that point so it shouldn't be relevant except in the embedded case. But
 the host application will probably not let things get that far and trap the
 signals itself.
*/
static void
handleInterrupt(void)
{
    popReadline();
    onintrNoResume();
}

#ifdef HAVE_RL_COMPLETION_MATCHES
/* ============================================================
   function-completion interface formerly in package rcompletion by
   Deepayan Sarkar, whose comments these are (mainly).
*/

static char **R_custom_completion(const char *text, int start, int end);
static char *R_completion_generator(const char *text, int state);

static SEXP
    RComp_assignBufferSym,
    RComp_assignStartSym,
    RComp_assignEndSym,
    RComp_assignTokenSym,
    RComp_completeTokenSym,
    RComp_getFileCompSym,
    RComp_retrieveCompsSym;

attribute_hidden
void set_rl_word_breaks(const char *str)
{
    static char p1[201], p2[203];
    strncpy(p1, str, 200); p1[200]= '\0';
    strncpy(p2, p1, 200); p2[200] = '\0';
    strcat(p2, "[]");
    rl_basic_word_break_characters = p2;
    rl_completer_word_break_characters = p1;
}


/* Tell the GNU Readline library how to complete. */

static int rcompgen_active = -1;
static SEXP rcompgen_rho;

#include <R_ext/Parse.h>
static void initialize_rlcompletion(void)
{
    if(rcompgen_active >= 0) return;

    /* Find if package utils is around */
    if(rcompgen_active < 0) {
	char *p = getenv("R_COMPLETION");
	if(p && streql(p, "FALSE")) {
	    rcompgen_active = 0;
	    return;
	}
	/* First check if namespace is loaded */
	if(findVarInFrame(R_NamespaceRegistry, install("utils"))
	   != R_UnboundValue) rcompgen_active = 1;
	else { /* Then try to load it */
	    SEXP cmdSexp, cmdexpr;
	    ParseStatus status;
	    int i;
	    char *p = "try(loadNamespace('rcompgen'), silent=TRUE)";

	    PROTECT(cmdSexp = mkString(p));
	    cmdexpr = PROTECT(R_ParseVector(cmdSexp, -1, &status, R_NilValue));
	    if(status == PARSE_OK) {
		for(i = 0; i < length(cmdexpr); i++)
		    eval(VECTOR_ELT(cmdexpr, i), R_GlobalEnv);
	    }
	    UNPROTECT(2);
	    if(findVarInFrame(R_NamespaceRegistry, install("utils"))
	       != R_UnboundValue) rcompgen_active = 1;
	    else {
		rcompgen_active = 0;
		return;
	    }
	}
    }

    rcompgen_rho = R_FindNamespace(mkString("utils"));

    RComp_assignBufferSym  = install(".assignLinebuffer");
    RComp_assignStartSym   = install(".assignStart");
    RComp_assignEndSym     = install(".assignEnd");
    RComp_assignTokenSym   = install(".assignToken");
    RComp_completeTokenSym = install(".completeToken");
    RComp_getFileCompSym   = install(".getFileComp");
    RComp_retrieveCompsSym = install(".retrieveCompletions");

    /* Tell the completer that we want a crack first. */
    rl_attempted_completion_function = R_custom_completion;

// This was added in readline 6.0
#ifdef HAVE_RL_SORT_COMPLETION_MATCHES
    rl_sort_completion_matches = 0;
#endif

    /* token boundaries.  Includes *,+ etc, but not $,@ because those
       are easier to handle at the R level if the whole thing is
       available.  However, this breaks filename completion if partial
       filenames contain things like $, % etc.  Might be possible to
       associate a M-/ override like bash does.  One compromise is that
       we exclude / from the breakers because that is frequently found
       in filenames even though it is also an operator.  This can be
       handled in R code (although it shouldn't be necessary if users
       surround operators with spaces, as they should).  */

    /* FIXME: quotes currently lead to filename completion without any
       further ado.  This is not necessarily the best we can do, since
       quotes after a [, $, [[, etc should be treated differently.  I'm
       not testing this now, but this should be doable by removing quote
       characters from the strings below and handle it with other things
       in 'specialCompletions()' in R.  The problem with that approach
       is that file name completion will probably have to be done
       manually in R, which is not trivial.  One way to go might be to
       forego file name completion altogether when TAB completing, and
       associate M-/ or something to filename completion (a startup
       message might say so, to remind users)

       All that might not be worth the pain though (vector names would
       be practically impossible, to begin with) */


    return;
}



/* Attempt to complete on the contents of TEXT.  START and END bound the
   region of rl_line_buffer that contains the word to complete.  TEXT is
   the word to complete.  We can use the entire contents of rl_line_buffer
   in case we want to do some simple parsing.  Return the array of matches,
   or NULL if there aren't any. */

static char **
R_custom_completion(const char *text, int start, int end)
     /*
	Make some relevant information available to R, then call
	rl_completion_matches to generate matches.  FIXME: It would be
	nice if we could figure whether we are in a partially
	completed line (R prompt == "+"), in which case we could keep
	the old line buffer around and do useful things with it.
     */
{
    char **matches = (char **)NULL;
    SEXP infile,
	linebufferCall = PROTECT(lang2(RComp_assignBufferSym,
				       mkString(rl_line_buffer))),
	startCall = PROTECT(lang2(RComp_assignStartSym, ScalarInteger(start))),
	endCall = PROTECT(lang2(RComp_assignEndSym,ScalarInteger(end)));
    SEXP filecompCall;

    /* Don't want spaces appended at the end.  Need to do this
       everytime, as readline>=6 resets it to ' ' */
    rl_completion_append_character = '\0';

    eval(linebufferCall, rcompgen_rho);
    eval(startCall, rcompgen_rho);
    eval(endCall, rcompgen_rho);
    UNPROTECT(3);
    matches = rl_completion_matches(text, R_completion_generator);
    filecompCall = PROTECT(lang1(RComp_getFileCompSym));
    infile = PROTECT(eval(filecompCall, rcompgen_rho));
    if (!asLogical(infile)) rl_attempted_completion_over = 1;
    UNPROTECT(2);
    return matches;
}

/* R_completion_generator does the actual work (it is called from
   somewhere inside rl_completion_matches repeatedly).  See readline
   documentation for details, but one important fact is that the
   return value of R_completion_generator will be free()-d by
   readline */

/* Generator function for command completion.  STATE lets us know
   whether to start from scratch: we do so when STATE == 0 */

static char *R_completion_generator(const char *text, int state)
{
    static int list_index, ncomp;
    static char **compstrings;

    /* If this is a new word to complete, initialize now.  This
       involves saving 'text' to somewhere R can get at it, calling
       completeToken(), and retrieving the completions. */

    if (!state) {
	int i;
	SEXP completions,
	    assignCall = PROTECT(lang2(RComp_assignTokenSym, mkString(text))),
	    completionCall = PROTECT(lang1(RComp_completeTokenSym)),
	    retrieveCall = PROTECT(lang1(RComp_retrieveCompsSym));
	const void *vmax = vmaxget();

	eval(assignCall, rcompgen_rho);
	eval(completionCall, rcompgen_rho);
	PROTECT(completions = eval(retrieveCall, rcompgen_rho));
	list_index = 0;
	ncomp = length(completions);
	if (ncomp > 0) {
	    compstrings = (char **) malloc(ncomp * sizeof(char*));
	    if (!compstrings) {
		UNPROTECT(4);
		return (char *)NULL;
	    }
	    for (i = 0; i < ncomp; i++)
		compstrings[i] = strdup(translateChar(STRING_ELT(completions, i)));
	}
	UNPROTECT(4);
	vmaxset(vmax);
    }

    if (list_index < ncomp)
	return compstrings[list_index++];
    else {
	/* nothing matched or remaining, so return NULL. */
	if (ncomp > 0) free(compstrings);
    }
    return (char *)NULL;
}

/* ============================================================ */
#else
attribute_hidden
void set_rl_word_breaks(const char *str)
{
}
#endif /* HAVE_RL_COMPLETION_MATCHES */

#else
static void
handleInterrupt(void)
{
    onintrNoResume();
}
#endif /* HAVE_LIBREADLINE */


/* Fill a text buffer from stdin or with user typed console input. */
static void *cd = NULL;

int attribute_hidden
Rstd_ReadConsole(const char *prompt, unsigned char *buf, int len,
		 int addtohistory)
{
    if(!R_Interactive) {
	size_t ll;
	int err = 0;
	if (!R_Slave) {
	    fputs(prompt, stdout);
	    fflush(stdout); /* make sure prompt is output */
	}
	if (fgets((char *)buf, len, ifp ? ifp: stdin) == NULL)
	    return 0;
	ll = strlen((char *)buf);
	/* remove CR in CRLF ending */
	if (ll >= 2 && buf[ll - 1] == '\n' && buf[ll - 2] == '\r') {
	    buf[ll - 2] = '\n';
	    buf[--ll] = '\0';
	}
	/* translate if necessary */
	if(strlen(R_StdinEnc) && strcmp(R_StdinEnc, "native.enc")) {
	    size_t res, inb = strlen((char *)buf), onb = len;
	    /* NB: this is somewhat dangerous.  R's main loop and
	       scan will not call it with a larger value, but
	       contributed code might. */
	    char obuf[CONSOLE_BUFFER_SIZE+1];
	    const char *ib = (const char *)buf;
	    char *ob = obuf;
	    if(!cd) {
		cd = Riconv_open("", R_StdinEnc);
		if(cd == (void *)-1) error(_("encoding '%s' is not recognised"), R_StdinEnc);
	    }
	    res = Riconv(cd, &ib, &inb, &ob, &onb);
	    *ob = '\0';
	    err = res == (size_t)(-1);
	    /* errors lead to part of the input line being ignored */
	    if(err) printf(_("<ERROR: re-encoding failure from encoding '%s'>\n"),
			   R_StdinEnc);
	    strncpy((char *)buf, obuf, len);
	}
/* according to system.txt, should be terminated in \n, so check this
   at eof and error */
	if ((err || feof(ifp ? ifp : stdin))
	    && (ll == 0 || buf[ll - 1] != '\n') && ll < (size_t)len) {
	    buf[ll++] = '\n'; buf[ll] = '\0';
	}
	if (!R_Slave) {
	    fputs((char *)buf, stdout);
	    fflush(stdout);
	}
	return 1;
    }
    else {
#ifdef HAVE_LIBREADLINE
	R_ReadlineData rl_data;
	if (UsingReadline) {
	    rl_data.readline_gotaline = 0;
	    rl_data.readline_buf = buf;
	    rl_data.readline_addtohistory = addtohistory;
	    rl_data.readline_len = len;
	    rl_data.readline_eof = 0;
	    rl_data.prev = rl_top;
	    rl_top = &rl_data;
	    /* Allow conditional parsing of the ~/.inputrc file. */
	    rl_readline_name = "R";
	    pushReadline(prompt, readline_handler);
#ifdef HAVE_RL_COMPLETION_MATCHES
	    initialize_rlcompletion();
#endif
	}
	else
#endif /* HAVE_LIBREADLINE */
	{
	    fputs(prompt, stdout);
	    fflush(stdout);
	}

	if(R_InputHandlers == NULL)
	    initStdinHandler();

	for (;;) {
	    fd_set *what;

	    int wt = -1;
	    if (R_wait_usec > 0) wt = R_wait_usec;
	    if (Rg_wait_usec > 0 && (wt < 0 || wt > Rg_wait_usec))
		wt = Rg_wait_usec;
	    what = R_checkActivityEx(wt, 0, handleInterrupt);
#ifdef NEED_INT_HANDLER
            if (UsingReadline && caught_sigwinch) {
		caught_sigwinch = FALSE;
		// introduced in readline 4.0: only used for >= 6.3
#ifdef HAVE_RL_RESIZE_TERMINAL
		rl_resize_terminal();
#endif
            }
#endif

	    /* This is slightly clumsy. We have advertised the
	     * convention that R_wait_usec == 0 means "wait forever",
	     * but we also need to enable R_checkActivity to return
	     * immediately. */

	    R_runHandlers(R_InputHandlers, what);
	    if (what == NULL)
		continue;
	    if (FD_ISSET(fileno(stdin), what)) {
		/* We could make this a regular handler, but we need
		 * to pass additional arguments. */
#ifdef HAVE_LIBREADLINE
		if (UsingReadline) {
		    rl_callback_read_char();
		    if(rl_data.readline_eof || rl_data.readline_gotaline) {
			rl_top = rl_data.prev;
			return(rl_data.readline_eof ? 0 : 1);
		    }
		}
		else
#endif /* HAVE_LIBREADLINE */
		{
		    if(fgets((char *)buf, len, stdin) == NULL)
			return 0;
		    else
			return 1;
		}
	    }
	}
    }
}

	/* Write a text buffer to the console. */
	/* All system output is filtered through this routine (unless R_Consolefile is used). */

void attribute_hidden Rstd_WriteConsole(const char *buf, int len)
{
    printf("%s", buf);
    fflush(stdout);
}

/* The extended version allows the distinction of errors and warnings.
   It is not enabled by default unless pretty-printing is desired. */
void attribute_hidden Rstd_WriteConsoleEx(const char *buf, int len, int otype)
{
    if (otype)
      printf("\033[1m%s\033[0m", buf);
    else
      printf("%s", buf);
    fflush(stdout);
}


	/* Indicate that input is coming from the console */

void attribute_hidden Rstd_ResetConsole()
{
}


	/* Stdio support to ensure the console file buffer is flushed */

void attribute_hidden Rstd_FlushConsole()
{
    /* fflush(stdin);  really work on Solaris on pipes */
}

	/* Reset stdin if the user types EOF on the console. */

void attribute_hidden Rstd_ClearerrConsole()
{
    clearerr(stdin);
}

/*
 *  3) ACTIONS DURING (LONG) COMPUTATIONS
 */

void attribute_hidden Rstd_Busy(int which)
{
}

/*
 *  4) INITIALIZATION AND TERMINATION ACTIONS
 */

/*
   R_CleanUp is invoked at the end of the session to give the user the
   option of saving their data.
   If ask == SA_SAVEASK the user should be asked if possible (and this
   option should not occur in non-interactive use).
   If ask = SA_SAVE or SA_NOSAVE the decision is known.
   If ask = SA_DEFAULT use the SaveAction set at startup.
   In all these cases run .Last() unless quitting is cancelled.
   If ask = SA_SUICIDE, no save, no .Last, possibly other things.
 */



void R_CleanTempDir(void)
{
    char buf[1024];

    if((Sys_TempDir)) {
#if defined(sun) || defined(__sun)
	/* On Solaris the working directory must be outside this one */
	chdir(R_HomeDir());
#endif
	snprintf(buf, 1024, "rm -rf %s", Sys_TempDir);
	buf[1023] = '\0';
	R_system(buf);
    }
}


void attribute_hidden NORET Rstd_CleanUp(SA_TYPE saveact, int status, int runLast)
{
    if(saveact == SA_DEFAULT) /* The normal case apart from R_Suicide */
	saveact = SaveAction;

    if(saveact == SA_SAVEASK) {
	if(R_Interactive) {
	    unsigned char buf[1024];
	qask:

	    R_ClearerrConsole();
	    R_FlushConsole();
	    int res = R_ReadConsole("Save workspace image? [y/n/c]: ",
				    buf, 128, 0);
	    if(res) {
		switch (buf[0]) {
		case 'y':
		case 'Y':
		    saveact = SA_SAVE;
		    break;
		case 'n':
		case 'N':
		    saveact = SA_NOSAVE;
		    break;
		case 'c':
		case 'C':
		    jump_to_toplevel();
		    break;
		default:
		    goto qask;
		}
	    } else saveact = SA_NOSAVE; /* probably EOF */
	} else
	    saveact = SaveAction;
    }
    switch (saveact) {
    case SA_SAVE:
	if(runLast) R_dot_Last();
	if(R_DirtyImage) R_SaveGlobalEnv();
#if defined(HAVE_LIBREADLINE) && defined(HAVE_READLINE_HISTORY_H)
	if(R_Interactive && UsingReadline) {
	    int err;
	    R_setupHistory(); /* re-read the history size and filename */
	    stifle_history(R_HistorySize);
	    err = write_history(R_HistoryFile);
	    if(err) warning(_("problem in saving the history file '%s'"),
			    R_HistoryFile);
	}
#endif
	break;
    case SA_NOSAVE:
	if(runLast) R_dot_Last();
	break;
    case SA_SUICIDE:
    default:
	break;
    }
    R_RunExitFinalizers();
    CleanEd();
    if(saveact != SA_SUICIDE) KillAllDevices();
    R_CleanTempDir();
    if(saveact != SA_SUICIDE && R_CollectWarnings)
	PrintWarnings();	/* from device close and (if run) .Last */
    if(ifp) {
	fclose(ifp);    /* input file from -f or --file= */
	ifp = NULL; 	/* To avoid trying to close it again */
    }
    fpu_setup(FALSE);

    exit(status);
}

/*
 *  7) PLATFORM DEPENDENT FUNCTIONS
 */

# include <errno.h>

int attribute_hidden
Rstd_ShowFiles(int nfile,		/* number of files */
	       const char **file,		/* array of filenames */
	       const char **headers,	/* the `headers' args of file.show.
					   Printed before each file. */
	       const char *wtitle,	/* title for window
					   = `title' arg of file.show */
	       Rboolean del,	/* should files be deleted after use? */
	       const char *pager)		/* pager to be used */

{
/*
	This function can be used to display the named files with the
	given titles and overall title.	 On GUI platforms we could
	use a read-only window to display the result.  Here we just
	make up a temporary file and invoke a pager on it.
*/

    int c, i, res;
    char *filename;
    FILE *fp, *tfp;
    char buf[1024];

    if (nfile > 0) {
	if (pager == NULL || strlen(pager) == 0) pager = "more";
	filename = R_tmpnam(NULL, R_TempDir); /* mallocs result */
	if ((tfp = R_fopen(filename, "w")) != NULL) {
	    for(i = 0; i < nfile; i++) {
		if (headers[i] && *headers[i])
		    fprintf(tfp, "%s\n\n", headers[i]);
		errno = 0; /* some systems require this */
		/* File expansion is now done in file.show(), but
		   left here in case other callers assumed it */
		if ((fp = R_fopen(R_ExpandFileName(file[i]), "r"))
		    != NULL) {
		    while ((c = fgetc(fp)) != EOF)
			fputc(c, tfp);
		    fprintf(tfp, "\n");
		    fclose(fp);
		    if(del)
			unlink(R_ExpandFileName(file[i]));
		}
		else
		    fprintf(tfp, _("Cannot open file '%s': %s\n\n"),
			    file[i], strerror(errno));
	    }
	    fclose(tfp);
	}
	snprintf(buf, 1024, "'%s' < '%s'", pager, filename); //might contain spaces
	res = R_system(buf);
	unlink(filename);
	free(filename);
	return (res != 0);
    }
    return 1;
}


    /*
       Prompt the user for a file name.  Return the length of
       the name typed.  On Gui platforms, this should bring up
       a dialog box so a user can choose files that way.
    */



int attribute_hidden Rstd_ChooseFile(int _new, char *buf, int len)
{
    size_t namelen;
    char *bufp;
    R_ReadConsole("Enter file name: ", (unsigned char *)buf, len, 0);
    namelen = strlen(buf);
    bufp = &buf[namelen - 1];
    while (bufp >= buf && isspace((int)*bufp))
	*bufp-- = '\0';
    return (int) strlen(buf);
}


void attribute_hidden Rstd_ShowMessage(const char *s)
{
    REprintf("%s\n", s);
}


void attribute_hidden Rstd_read_history(const char *s)
{
#if defined(HAVE_LIBREADLINE) && defined(HAVE_READLINE_HISTORY_H)
    if(R_Interactive && UsingReadline) {
	read_history(s);
    }
#endif
}

void attribute_hidden Rstd_loadhistory(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile;
    char file[PATH_MAX];
    const char *p;

    sfile = CAR(args);
    if (!isString(sfile) || LENGTH(sfile) < 1)
	errorcall(call, _("invalid '%s' argument"), "file");
    p = R_ExpandFileName(translateChar(STRING_ELT(sfile, 0)));
    if(strlen(p) > PATH_MAX - 1)
	errorcall(call, _("'file' argument is too long"));
    strcpy(file, p);
#if defined(HAVE_LIBREADLINE) && defined(HAVE_READLINE_HISTORY_H)
    if(R_Interactive && UsingReadline) {
	clear_history();
	read_history(file);
    } else errorcall(call, _("no history mechanism available"));
#else
    errorcall(call, _("no history mechanism available"));
#endif
}

void attribute_hidden Rstd_savehistory(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile;
    char file[PATH_MAX];
    const char *p;

    sfile = CAR(args);
    if (!isString(sfile) || LENGTH(sfile) < 1)
	errorcall(call, _("invalid '%s' argument"), "file");
    p = R_ExpandFileName(translateChar(STRING_ELT(sfile, 0)));
    if(strlen(p) > PATH_MAX - 1)
	errorcall(call, _("'file' argument is too long"));
    strcpy(file, p);
#if defined(HAVE_LIBREADLINE) && defined(HAVE_READLINE_HISTORY_H)
    if(R_Interactive && UsingReadline) {
	int err;
	err = write_history(file);
	if(err) error(_("problem in saving the history file '%s'"), file);
	/* Note that q() uses stifle_history, but here we do not want
	 * to truncate the active history when saving during a session */
#ifdef HAVE_HISTORY_TRUNCATE_FILE
	R_setupHistory(); /* re-read the history size */
	err = history_truncate_file(file, R_HistorySize);
	if(err) warning(_("problem in truncating the history file"));
#endif
    } else errorcall(call, _("no history available to save"));
#else
    errorcall(call, _("no history available to save"));
#endif
}

void attribute_hidden Rstd_addhistory(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP stamp;
    int i;

    checkArity(op, args);
    stamp = CAR(args);
    if (!isString(stamp))
	errorcall(call, _("invalid timestamp"));
#if defined(HAVE_LIBREADLINE) && defined(HAVE_READLINE_HISTORY_H)
    if(R_Interactive && UsingReadline)
	for (i = 0; i < LENGTH(stamp); i++)
	    add_history(CHAR(STRING_ELT(stamp, i))); /* ASCII */
# endif
}


#define R_MIN(a, b) ((a) < (b) ? (a) : (b))

void Rsleep(double timeint)
{
    double tm = timeint * 1e6, start = currentTime(), elapsed;
    for (;;) {
	fd_set *what;
	tm = R_MIN(tm, 2e9); /* avoid integer overflow */

	int wt = -1;
	if (R_wait_usec > 0) wt = R_wait_usec;
	if (Rg_wait_usec > 0 && (wt < 0 || wt > Rg_wait_usec))
	    wt = Rg_wait_usec;
	int Timeout = (int) (wt > 0 ? R_MIN(tm, wt) : tm);
	what = R_checkActivity(Timeout, 1);
	/* For polling, elapsed time limit ... */
	R_CheckUserInterrupt();
	/* Time up? */
	elapsed = currentTime() - start;
	if(elapsed >= timeint) break;

	/* Nope, service pending events */
	R_runHandlers(R_InputHandlers, what);

	/* Servicing events might take some time, so recheck: */
	elapsed = currentTime() - start;
	if(elapsed >= timeint) break;

	tm = 1e6*(timeint - elapsed);
    }
}
