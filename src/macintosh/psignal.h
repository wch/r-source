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

#define	SIGUSR1 30	/* user defined signal 1 */
#define	SIGUSR2 31	/* user defined signal 2 */

