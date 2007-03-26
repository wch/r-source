/*****************************************************************************
 *                                                                           *
 * DH_SIG.C                                                                  *
 *                                                                           *
 * Freely redistributable and modifiable.  Use at your own risk.             *
 *                                                                           *
 * Copyright 1994 The Downhill Project                                       *
 *                                                                           *
 *****************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include "psignal.h"
extern int UserBreak;

/* Define stuff ************************************************************ */
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
static HANDLE                            IGotASignal;

/* Function stuff ************************************************************/


/* Handle a signal ========================================================= */
void raise(int signal_Number)
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
                PulseEvent(IGotASignal);
	}
}

/* Init the signal re-direction ============================================ */

/* Hardware interrupt handler.  */
static BOOL CALLBACK hwIntrHandler (DWORD type)
{
  int ret;
  switch (type) {
  case CTRL_C_EVENT : 
  case CTRL_BREAK_EVENT : 
    /*  
	Why SIGBREAK? SIGINT is used internally by R and the signal 
	handler for SIGINT ends with a 'longjmp'. But, under Windows,
	hardware interrupt handler runs in a different thread. So,
	longjmp fails (tipically it will crash R). So, we raise a SIGBREAK
	to record that the user want to stop. This is then
	processed at due time. Drawback of this approach: if R is lost
	inside a C or Fortran routine we don't break it. I (g.m.) have
	tried to raise a signal in the appropriate thread using
	SuspendThread/GetThreadContext/setting Eip to the signal handler/
	SetThreadContext/ResumeThread but I had success only under NT.
    */    
    raise(SIGBREAK);
    /* Seems that SIGBREAK is not working under 1,4,0, so do it via
       a semaphore, as RGui does */
    UserBreak = 1;
    ret = TRUE;
    break;
  default:
    ret = FALSE;
  }
  return ret ; 
}

static int downhill_Signal_Init(void)
{
	/* Skip this if we've already done it */
	if (downhill_Signal_Info == NULL)
	{
		if (!(downhill_Signal_Info =
		      calloc(sizeof(struct downhill_Signal_Struct),NSIG)) ||
		    !(IGotASignal=CreateEvent(NULL,FALSE,FALSE,NULL)) ||
                    !SetConsoleCtrlHandler (hwIntrHandler, TRUE))

		{
                  if (downhill_Signal_Info) free(downhill_Signal_Info);  
                  if (IGotASignal) CloseHandle(IGotASignal);
		  errno = ENOMEM;
		  return FALSE;
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
sighandler_t signal(int signal_Number, sighandler_t signal_Handler)
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
		    raise(signal_Index);
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



/* Pause until a signal ==================================================== */
int pause(void)
{
	/* Wait for a signal */
        WaitForSingleObject(IGotASignal,INFINITE);
	/* And return if we were interrupted */
	errno = EINTR;
	return -1;
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

