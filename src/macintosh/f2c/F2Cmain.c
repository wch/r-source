/* STARTUP PROCEDURE FOR TRANSLATED FORTRAN PROGRAMS */


/* Conditional compile mechanism now triggered by prefix file */


/* IMT 17Jul98  undefine somethings defined in f2c.h that collide with standard lib stuff */
#if defined(F2C_INCLUDE)  
	#undef abs
	#undef min
	#undef max
#endif

/* IMT 9Sep95  THINK Project Manager specific includes */
#ifdef TPM_F2C
	#include <console.h>
#endif					

/* IMT 9Sep95  Symantec Project Manager specific includes */
#ifdef SPM_F2C 
	#include <console.h>	
#endif					

/* IMT 9Sep95  CodeWarrior MacOS specific includes */
#ifdef CW_F2C_MAC 
 void isatty(){};
 void SystemTask(){};
 void GetFInfo(){};
 void close(){};
	#include <console.h>
	#include <SIOUX.h>
//	#include <unistd.h>
#endif

/* GES 25Feb99  CodeWarrior Win32 specific includes */
#if defined(CW_F2C_WIN32)
	#include <SIOUX.h>
#endif

/* GES 1Mar99 */
#ifndef TRUE
	#define TRUE 1
#endif
#ifndef FALSE
	#define FALSE 0
#endif

/* IMT 9Sep95  MPW CodeWarrior specific includes */
#ifdef MPW_CW_F2C
	#include <console.h>
#endif


#if defined(TPM_F2C) || defined(SPM_F2C) || defined(CW_F2C_MAC) 
	#ifndef COMMANDCONSOLE_DLG
		#define COMMANDCONSOLE_DLG 	1		/* By default, show command-console dialog at start-up */
	#endif
#endif /* Macintosh C compilers */

/* IMT 12Apr96  Include files for adjusting the stack size in 68K */
#if defined(TPM_F2C) || defined(SPM_F2C) || defined(CW_F2C_MAC) 
	#include <ConditionalMacros.h>
	#if TARGET_CPU_68K
		#include <LowMem.h>
		#include <Memory.h>
	#endif /* TARGET_CPU_68K */
#endif /* Macintosh C compilers */


/* IMT 09Sep95  Prototypes for new functions that add multitasking */
#if defined(TPM_F2C) || defined(SPM_F2C) || defined(CW_F2C_MAC) 
	#ifdef __cplusplus
	extern "C" {
	#endif
	void InitMultiTask( long sliceInMicroSecs );
	void EndMultiTask( void );
	#ifdef __cplusplus
	}
	#endif
#endif /* Macintosh C compilers */


/* IMT 10Sep95  Declare jump buffer used to recover from exception exits & aborts */
#if defined(TPM_F2C) || defined(SPM_F2C) || defined(CW_F2C_MAC) || defined(CW_F2C_WIN32) 
	#include <setjmp.h>
	#ifdef __cplusplus
	extern "C" {
	#endif
	jmp_buf gRecoverToConsole;
	#ifdef __cplusplus
	}
	#endif
#endif /* Macintosh C compilers and MW Win32 */




#include "stdio.h"

/* IMT 18Aug97  Work around because "signal1.h" not accessible in most CW configurations */
#if defined(CW_F2C_MAC) || defined(CW_F2C_WIN32) 				
	#include <signal.h>
	#define signal1(a,b) signal(a,(__signal_func_ptr)b)
#elif defined(TPM_F2C) || defined(SPM_F2C)
	#include <signal.h>
	#error "For Symantec compilers need to correct following #define!"
	/* Need to replace __signal_func_ptr in cast below with appropriate type from <signal.h> */
	/* Then uncomment the define and email me the correction at igormt@alumni.caltech.edu */
	/* #define signal1(a,b) signal(a,(__signal_func_ptr)b) */
#else
	#include "signal1.h"
#endif

#ifndef SIGIOT
#ifdef SIGABRT
#define SIGIOT SIGABRT
#endif
#endif


#ifndef KR_headers
#undef VOID
#include "stdlib.h"
#endif

#ifndef VOID
#define VOID void
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef NO__STDC
#define ONEXIT onexit
extern VOID f_exit();
#else
#ifndef KR_headers
extern void f_exit(void);
#ifndef NO_ONEXIT
#define ONEXIT atexit
// IMT 8 Apr 98.  Bug.  Prototype already in stdlib.h (#included above).
//    It is a bug because under C++ & namespaces causes an overload resolution problem
//    because function below is *not* the same as std::atexit
//extern int atexit(void (*)(void));
#endif
#else
#ifndef NO_ONEXIT
#define ONEXIT onexit
extern VOID f_exit();
#endif
#endif
#endif

#ifdef KR_headers
extern VOID f_init(), sig_die();
//extern int MAIN__();
#define Int /* int */
#else
extern void f_init(void), sig_die(char*, int);
//extern int MAIN__(void);
#define Int int
#endif

static VOID sigfdie(Int n)
{
sig_die("Floating Exception", 1);
}


static VOID sigidie(Int n)
{
sig_die("IOT Trap", 1);
}

#ifdef SIGQUIT
static VOID sigqdie(Int n)
{
sig_die("Quit signal", 1);
}
#endif


static VOID sigindie(Int n)
{
sig_die("Interrupt", 0);
}

static VOID sigtdie(Int n)
{
sig_die("Killed", 0);
}

#ifdef SIGTRAP
static VOID sigtrdie(Int n)
{
sig_die("Trace trap", 1);
}
#endif


int xargc;
char **xargv;

#ifdef __cplusplus
	}
#endif

#ifdef KR_headers
main(argc, argv) int argc; char **argv;
#else
int main(int argc, char **argv)
#endif
{

/* IMT 12Apr96  Add conditional code to increase 68K stack size */
/*              Adjust the following #define as needed */
#if defined(TPM_F2C) || defined(SPM_F2C) || defined(CW_F2C_MAC) 

	#ifndef kDesired68KStackSize
		#define kDesired68KStackSize  (40*1024L) 	/* 68K stack size set to 40K bytes */
	#endif
	
	#if TARGET_CPU_68K && defined(kDesired68KStackSize)	
		if ( kDesired68KStackSize > LMGetDefltStack() )
		{
			Ptr newApplLimit = GetApplLimit() - (kDesired68KStackSize - LMGetDefltStack());
			SetApplLimit( newApplLimit );
		}
	#endif	/* increase stack size */
	
#endif	/* MacOS Compilers */


/* IMT 09Sep95  Add file re-direction and command-line arguments dialog */
#ifndef COMMANDCONSOLE_DLG
	#define COMMANDCONSOLE_DLG 1
#endif
#if ( defined(TPM_F2C) || defined(SPM_F2C) || defined(CW_F2C_MAC) ) && COMMANDCONSOLE_DLG
	argc = ccommand( &argv );
#endif /* Macintosh C compilers */


/* IMT 14Sep95  Initialize multi-tasking code */
#if defined(TPM_F2C) || defined(SPM_F2C) || defined(CW_F2C_MAC) 
	InitMultiTask( 0 );
#endif /* Multi-tasking code */


xargc = argc;
xargv = argv;
signal1(SIGFPE, sigfdie);	/* ignore underflow, enable overflow */
#ifdef SIGIOT
signal1(SIGIOT, sigidie);
#endif
#ifdef SIGTRAP
signal1(SIGTRAP, sigtrdie);
#endif
#ifdef SIGQUIT
if(signal1(SIGQUIT,sigqdie) == SIG_IGN)
	signal1(SIGQUIT, SIG_IGN);
#endif
if(signal1(SIGINT, sigindie) == SIG_IGN)
	signal1(SIGINT, SIG_IGN);
signal1(SIGTERM,sigtdie);

#ifdef pdp11
	ldfps(01200); /* detect overflow as an exception */
#endif

f_init();
#ifndef NO_ONEXIT
ONEXIT(f_exit);
#endif


/* IMT 10Sep95:  Set-up for returning to console from exceptional exits and aborts */
#if defined(TPM_F2C) || defined(SPM_F2C) || defined(CW_F2C_MAC) || defined(CW_F2C_WIN32)
	if ( setjmp(gRecoverToConsole) == 0 )
#endif /* Macintosh C compilers and CW Win32 */

//MAIN__();

/* IMT 10Sep95:  Close out multi-tasking code for CW, SPM, TPM */
#if defined(TPM_F2C) || defined(SPM_F2C) || defined(CW_F2C_MAC) 
	EndMultiTask();
#endif /* Macintosh C compilers */


/* IMT 5Aug99:  Let user know we finished */
#if defined(TPM_F2C) || defined(SPM_F2C) || defined(CW_F2C_MAC) || defined(CW_F2C_WIN32)  
	puts( "\nExecution complete.\n" );
#endif /* Macintosh C compilers and CW Win32 */

#ifdef NO_ONEXIT
f_exit();
#endif
exit(0);	/* exit(0) rather than return(0) to bypass Cray bug */
return 0;	/* For compilers that complain of missing return values; */
		/* others will complain that this is unreachable code. */
}
