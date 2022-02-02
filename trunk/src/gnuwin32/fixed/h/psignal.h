/*****************************************************************************
 *                                                                           *
 * signal.h                                                                  *
 *                                                                           *
 * Freely redistributable and modifiable.  Use at your own risk.             *
 *                                                                           *
 * Copyright 1994-1999 The Downhill Project                                  *
 * http://www.ede.com/free/u2nt                                              *
 *                                                                           *
 *****************************************************************************/


/* 
   Original version taken at 
   Changes (g.m.):
   10.06.1999: Made self contained - Changed some names
   11.06.1999: Added 'sigsetjmp' and 'siglongjmp'
   12.06.1999: Added pause and sigsuspend (require a version (also
               a non POSIX one) of sleep; if your system don't have it
               define DONT_HAVE_SLEEP)
   27/06/1999: (BDR) convert sigsetjmp and siglongjmp macros to (,,)
   12/07/1999: (BDR) fix sigsetjmp macro to set saved_mask
   
*/

#ifndef	_PSIGNAL_H_
#define	_PSIGNAL_H_

#include <stdlib.h>
#include <errno.h>
#include <setjmp.h>

#define	SIGHUP	1	/* hangup */
#define	SIGINT	2	/* interrupt */
#define	SIGQUIT	3	/* quit */
#define	SIGILL	4	/* illegal instruction (not reset when caught) */
#define	SIGTRAP	5	/* trace trap (not reset when caught) */
#define	SIGEMT	7	/* EMT instruction */
#define	SIGFPE	8	/* floating point exception */
#define	SIGKILL	9	/* kill (cannot be caught or ignored) */
#define	SIGBUS	10	/* bus error */
#define	SIGSEGV	11	/* segmentation violation */
#define	SIGSYS	12	/* bad argument to system call */
#define	SIGPIPE	13	/* write on a pipe with no one to read it */
#define	SIGALRM	14	/* alarm clock */
#define	SIGTERM	15	/* software termination signal from kill */
#define	SIGURG	16	/* urgent condition on IO channel */
#define	SIGSTOP	17	/* sendable stop signal not from tty */
#define	SIGTSTP	18	/* stop signal from tty */
#define	SIGCONT	19	/* continue a stopped process */
#define	SIGCHLD	20	/* to parent on child stop or exit */
#define	SIGCLD	20	/* System V name for SIGCHLD */
#define	SIGBREAK 21	/* to readers pgrp upon background tty read */
#define	SIGABRT 22	/* used by abort */
#define	SIGIO	23	/* input/output possible signal */
#define	SIGPOLL	SIGIO	/* System V name for SIGIO */
#define	SIGXCPU	24	/* exceeded CPU time limit */
#define	SIGXFSZ	25	/* exceeded file size limit */
#define	SIGVTALRM 26	/* virtual time alarm */
#define	SIGPROF	27	/* profiling time alarm */
#define	SIGWINCH 28	/* window changed */
#define	SIGLOST 29	/* resource lost (eg, record-lock lost) */
#define	SIGUSR1 30	/* user defined signal 1 */
#define	SIGUSR2 31	/* user defined signal 2 */
#define NSIG	32      /* signal 0 implied */

#ifndef	RC_INVOKED

/*
 * A pointer to a signal handler function. A signal handler takes a
 * single int, which is the signal it handles.
*/
typedef	void (*sighandler_t)(int nSig);
/* mingw-w64's sys/types.h also defines this and we want this defn */ 
#ifndef _SIGSET_T_
#define	_SIGSET_T_
typedef int sigset_t;
#endif	/* Not _SIGSET_T_ */


/*
 * These are special values of signal handler pointers which are
 * used to send a signal to the default handler (SIG_DFL), ignore
 * the signal (SIG_IGN), or indicate an error return (SIG_ERR).
*/
#define	SIG_DFL	((sighandler_t) 0)
#define	SIG_IGN	((sighandler_t) 1)
#define	SIG_ERR ((sighandler_t) -1)

#ifdef	__cplusplus
extern "C" {
#endif





/* Signal mask actions ===================================================== */
#define SIG_BLOCK   0
#define SIG_UNBLOCK 1
#define SIG_SETMASK 2

/* Signal flag actions ===================================================== */
#define SA_NOCLDSTOP 1
#define SA_RESETHAND 2


/* Struct stuff **************************************************************/struct sigaction
{
	void     (*sa_handler)(int);
	sigset_t sa_mask;
	int      sa_flags;
};

typedef struct
{    
  jmp_buf jmpbuf;     /* Calling environment.  */  
  int mask_was_saved;       /* Saved the signal mask?  */                   
  sigset_t saved_mask;      /* Saved signal mask.  */                       
} sigjmp_buf[1];                                                



/* Prototype stuff ***********************************************************/
int           sigsetmask(int signal_Block_MaskNew);
int           sigblock(int signal_Block_MaskNew);
int           sighold(int signal_Number);
int           sigrelse(int signal_Number);
int           sigaction(int signal_Number,struct sigaction* sigaction_Info,
			struct sigaction* signaction_InfoOld);
int           sigaddset(sigset_t* sigset_Info,int signal_Number);
int           sigdelset(sigset_t* sigset_Info,int signal_Number);
int           sigemptyset(sigset_t* sigset_Info);
int           sigfillset(sigset_t* sigset_Info);
int           sigismember(sigset_t* sigset_Info,int signal_Number);
int           sigpending(sigset_t* sigset_Info);
int           sigprocmask(int mask_Function,sigset_t* sigset_Info,
			  sigset_t* sigset_InfoOld);
sighandler_t  signal(int signal_Number, sighandler_t);
void          raise(int);
int pause(void);
int sigsuspend(sigset_t* sigset_Info);

/* Re-mapped functions ===================================================== */

#define sigmask(signal_Index) (1<<(signal_Index-1))

/* 
   This must be a macro, since we want setjmp working in the
   calling environment
*/
/*
#define sigsetjmp(jb, sm) (\
               sm?sigprocmask(SIG_SETMASK,NULL,&jb->saved_mask):0,\
               jb->mask_was_saved=sm,\
               setjmp(jb->jmpbuf))

we only currently use the case sm=0, so avoid compiler warnings by */

#define sigsetjmp(jb, sm) (jb->mask_was_saved=0, setjmp(jb->jmpbuf))


/* We can transform this in a function but ... */

#define siglongjmp(jb, val) (((jb->mask_was_saved)?\
               sigprocmask(SIG_SETMASK, &jb->saved_mask, 0):0),\
               longjmp(jb->jmpbuf, val))



#ifdef	__cplusplus
}
#endif

#endif	/* Not RC_INVOKED */

#endif	/* Not _PSIGNAL_H_ */
