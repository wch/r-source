/*****************************************************************************
 *                                                                           *
 * DH_SIG.C                                                                  *
 *                                                                           *
 * Freely redistributable and modifiable.  Use at your own risk.             *
 *                                                                           *
 * Copyright 1994 The Downhill Project                                       *
 *                                                                           *
 *****************************************************************************/

/* 
   Changes (g.m.):
   10.06.1999: Made self contained - Changed some names
   11.06.1999: Added 'sigsetjmp' and 'siglongjmp'
*/

#include "psignal.h"


/* Define stuff ************************************************************ */
#undef signal
sighandler_t signal(int, sighandler_t);

#ifndef TRUE
  #define TRUE 1
#endif
#ifndef FALSE
  #define FALSE 0
#endif




/* Signal groupings ======================================================== */
#define SIGNAL_DEFAULT_EXIT         case SIGABRT:               \
                                    case SIGFPE:                \
                                    case SIGILL:                \
                                    case SIGINT:                \
                                    case SIGSEGV:               \
                                    case SIGTERM:

#define IS_SIGNAL(a)        ( ((a) > 0 ) &&  ((a) < NSIG))


/* Struct stuff **************************************************************/
struct downhill_Signal_Struct
{
	void     (*signal_Handler)(int);
	int      signal_Count;
	int      signal_Extra;
	sigset_t signal_Mask;
	int      signal_Flags;
};


/* Static stuff **************************************************************/
static struct downhill_Signal_Struct* downhill_Signal_Info = NULL;
static sigset_t                       downhill_Sigset_Mask = 0;
static int                            IGotASignal;

/* Function stuff ************************************************************/


/* Handle a signal ========================================================= */
static void PrivateSignalHandler(int signal_Number)
{
        if (!IS_SIGNAL(signal_Number)) {
			errno = EINVAL;
			return;
        }
	/* Check to see if we're masking this signal */
	if (sigismember(&downhill_Sigset_Mask,signal_Number))
	{
		downhill_Signal_Info[signal_Number].signal_Count++;
	}
	else
        {
	        /* Do a signal's action */
		if (downhill_Signal_Info[signal_Number].signal_Handler ==
		 SIG_DFL)
		{
			switch (signal_Number)
			{
				SIGNAL_DEFAULT_EXIT
					exit(3);
				default:
                                        break;
			}
		}
		else if (
                 (downhill_Signal_Info[signal_Number].signal_Handler == 
                  SIG_IGN)
                )
		  {/* IGNORE */}
		else
		{
			void (*signal_HandlerOld)(int);
			/* Do we reset the handler? */
			signal_HandlerOld =
			 downhill_Signal_Info[signal_Number].signal_Handler;
			if (downhill_Signal_Info[signal_Number].signal_Flags&
			 SA_RESETHAND)
			{
				downhill_Signal_Info[signal_Number].
				 signal_Handler = SIG_DFL;
			}
			/* Do the action */
			if ((signal_Number == SIGCHLD) &&
			 (downhill_Signal_Info[signal_Number].signal_Flags&
			 SA_NOCLDSTOP))
			{
				/* Ignore SIGCHLD */
			}
			else
			{
				sigset_t sigset_MaskOriginal =
				          downhill_Sigset_Mask;
				sigset_t sigset_MaskNew = downhill_Signal_Info[
				          signal_Number].signal_Mask;

				/* Set the new signal mask */
				sigaddset(&sigset_MaskNew,signal_Number);
				sigprocmask(SIG_BLOCK,&sigset_MaskNew,NULL);

				/* Execute the handler */
				signal_HandlerOld(signal_Number);

				/* Restore the signal mask */
				sigprocmask(SIG_SETMASK,&sigset_MaskOriginal,
				 NULL);

			}
		}
                IGotASignal = TRUE;
	}
	/* In any case, restore the signal handler */
        signal(signal_Number,PrivateSignalHandler);
}

/* Init the signal re-direction ============================================ */
static int downhill_Signal_Init(void)
{
	/* Skip this if we've already done it */
	if (downhill_Signal_Info == NULL)
	{
		int signal_Index;

		/* Get some memory */
		downhill_Signal_Info =
		 calloc(sizeof(struct downhill_Signal_Struct),NSIG);
		if (downhill_Signal_Info == NULL)
		{
			errno = ENOMEM;
			return FALSE;
		}

		/* Set everything up */
		for (signal_Index = 1;signal_Index < NSIG;signal_Index++)
		{
		    if ((downhill_Signal_Info[signal_Index].signal_Handler = 
                             signal(signal_Index, PrivateSignalHandler))
                         == SIG_ERR)
                      downhill_Signal_Info[signal_Index].signal_Handler =
                         SIG_IGN; 
		}
	}
	return TRUE;
}

/* Set a signal action ===================================================== */
int sigaction(int signal_Number,struct sigaction* sigaction_Info,
     struct sigaction* sigaction_InfoOld)
{
	/* Make sure we're init'd */
	if (!downhill_Signal_Init())
	{
		return -1;
	}

	/* Set the signal */
        if (IS_SIGNAL(signal_Number)) {
			if (sigaction_InfoOld != NULL)
			{
				sigaction_InfoOld->sa_handler =
				 downhill_Signal_Info[signal_Number].
				 signal_Handler;
				sigaction_InfoOld->sa_mask =
				 downhill_Signal_Info[signal_Number].
				 signal_Mask;
				sigaction_InfoOld->sa_flags =
				 downhill_Signal_Info[signal_Number].
				 signal_Flags;
			}
			if (sigaction_Info != NULL)
			{
				downhill_Signal_Info[signal_Number].
				 signal_Handler = sigaction_Info->sa_handler;
				downhill_Signal_Info[signal_Number].
				 signal_Count = 0;
				downhill_Signal_Info[signal_Number].
				 signal_Extra = 0;
				downhill_Signal_Info[signal_Number].
				 signal_Mask = sigaction_Info->sa_mask;
				downhill_Signal_Info[signal_Number].
				 signal_Flags = sigaction_Info->sa_flags;
			}
        }
        else  {
			errno = EINVAL;
			return -1;
	}

	return 0;
}

/* Set the action of a signal ============================================== */
sighandler_t _psignal(int signal_Number, sighandler_t signal_Handler)
{
	sighandler_t signal_HandlerOld;

	/* Make sure we're init'd */
	if (!IS_SIGNAL(signal_Number) || !downhill_Signal_Init() )
	{
		return SIG_ERR;
	}
	signal_HandlerOld =
		downhill_Signal_Info[signal_Number].signal_Handler;
	downhill_Signal_Info[signal_Number].signal_Handler =
		signal_Handler;
	downhill_Signal_Info[signal_Number].signal_Count = 0;
	downhill_Signal_Info[signal_Number].signal_Extra = 0;
	downhill_Signal_Info[signal_Number].signal_Mask = 0;
	downhill_Signal_Info[signal_Number].signal_Flags = 0;
	return signal_HandlerOld;
}

/* Add a signal to a set =================================================== */
int sigaddset(sigset_t* sigset_Info,int signal_Number)
{
    if (IS_SIGNAL(signal_Number)) {
	    (*sigset_Info) |= (1 << (signal_Number - 1));
            return 0;
    }
    else {
	     errno = EINVAL;
	     return -1;
    }
    return 0;
}

/* Remove a signal from a set ============================================== */
int sigdelset(sigset_t* sigset_Info,int signal_Number)
{
    if (IS_SIGNAL(signal_Number)) {
	    *sigset_Info &= ~(1<< (signal_Number - 1));
            return 0;
    }
    else {
	     errno = EINVAL;
	     return -1;
    }
    return 0;
}

/* Empty a set ============================================================= */
int sigemptyset(sigset_t* sigset_Info)
{
	*sigset_Info = 0;
	return 0;
}

/* Fill a set ============================================================== */
int sigfillset(sigset_t* sigset_Info)
{
	*sigset_Info = (sigset_t)-1;
	return 0;
}

/* Checks if a signal is in a set ========================================== */
int sigismember(sigset_t* sigset_Info,int signal_Number)
{
      if (IS_SIGNAL(signal_Number)) {
			if ( *sigset_Info & (1 << (signal_Number-1)))
				return 1;
                        else
                                return 0;
      }
      errno = EINVAL;
      return -1;
}

/* Returns the signals pending ============================================= */
int sigpending(sigset_t* sigset_Info)
{
	int signal_Index;
	/* Make sure we're init'd */
	if (!downhill_Signal_Init())
	{
		return -1;
	}
	/* Check all the pending signals */
        sigemptyset(sigset_Info);
	for (signal_Index = 1;signal_Index < NSIG;signal_Index++)
	{
		if (downhill_Signal_Info[signal_Index].signal_Count > 0)
		{
			sigaddset(sigset_Info,signal_Index);
		}
	}

	return 0;
}

/* Change the blocked signals ============================================== */
int sigprocmask(int mask_Function,sigset_t* sigset_Info,
     sigset_t* sigset_InfoOld)
{
	int      signal_Index;
	sigset_t sigset_MaskOld = downhill_Sigset_Mask;

	/* Make sure we're init'd */
	if (!downhill_Signal_Init())
	{
		return -1;
	}

	/* Return the current value */
	if (sigset_InfoOld != NULL)
	{
		*sigset_InfoOld = sigset_MaskOld;
	}

	/* Set the new mask */
        if (sigset_Info) {
	   switch (mask_Function)
	   {
		case SIG_BLOCK:
			downhill_Sigset_Mask |= (*sigset_Info);
			break;
		case SIG_UNBLOCK:
			downhill_Sigset_Mask &= ~(*sigset_Info);
			break;
		case SIG_SETMASK:
			downhill_Sigset_Mask = *sigset_Info;
			break;
		default:
			errno = EINVAL;
			return -1;
          }
	}

	/* And release any signals that were pending */
	for (signal_Index = 1;signal_Index < NSIG;signal_Index++)
	{
		if ((!sigismember(&downhill_Sigset_Mask,signal_Index)) &&
		 (sigismember(&sigset_MaskOld,signal_Index)) &&
		 (downhill_Signal_Info[signal_Index].signal_Count > 0))
		{
	            downhill_Signal_Info[signal_Index].signal_Count = 0;
		    PrivateSignalHandler(signal_Index);
		}
	}

	return 0;
}

/* Set signal mask ========================================================= */
unsigned long sigsetmask(unsigned long signal_MaskNew)
{
	unsigned long signal_MaskOld = downhill_Sigset_Mask;

	if (sigprocmask(SIG_SETMASK,(sigset_t*)(&signal_MaskNew),NULL) == -1)
	{
		return (unsigned long)-1;
	}

	return signal_MaskOld;
}

/* Add signals to mask ===================================================== */
unsigned long sigblock(unsigned long signal_MaskNew)
{
	/* Block a specific group of signals */
	return sigsetmask(downhill_Sigset_Mask|signal_MaskNew);
}


/* Hold a signal =========================================================== */
int sighold(int signal_Number)
{
	/* Block a specific signal */
	if (sigblock(sigmask(signal_Number)) == -1)
	{
		return -1;
	}

	return 0;
}

/* Release a signal ======================================================== */
int sigrelse(int signal_Number)
{
	/* Release a specific signal */
	if (sigsetmask(downhill_Sigset_Mask&(~sigmask(signal_Number))) == -1)
	{
		return -1;
	}

	return 0;
}


void _praise(int signal_Number) {
       PrivateSignalHandler(signal_Number);
}


#ifndef DONT_HAVE_SLEEP
/* Pause until a signal ==================================================== */
int pause(void)
{
	/* Wait for a signal */
        IGotASignal = FALSE;
	for (;;)
	{
		/* And return if we were interrupted */
                sleep(1);
		if (IGotASignal)
		{
			errno = EINTR;
			return -1;
		}
	}
}


/* Suspend the process until a signal ====================================== */
int sigsuspend(sigset_t* sigset_Info)
{
	sigset_t sigset_MaskOriginal = downhill_Sigset_Mask;

	/* Set the new mask */
	sigprocmask(SIG_SETMASK,sigset_Info,NULL);

	/* Wait for the signal */
	pause();

	/* Reset the old mask */
	sigprocmask(SIG_SETMASK,&sigset_MaskOriginal,NULL);

	return -1;
}
#endif
