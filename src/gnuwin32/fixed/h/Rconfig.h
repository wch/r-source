#ifndef RCONFIG_H_
#define RCONFIG_H_

#define Win32

#define PLOTHISTORY

/* AIX at least. */
#ifndef _ALL_SOURCE
#undef _ALL_SOURCE
#endif

/* alloca */
#undef C_ALLOCA
#undef HAVE_ALLOCA
#undef HAVE_ALLOCA_H

/* Define to empty if the keyword does not work.  */
#undef const

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* (Long) Integers */

#define SIZEOF_LONG 4

/* Floating Point Arithmetic */
#undef HAVE_MATHERR		/* System V */
#define HAVE_ISNAN 1		/* IEEE Arith indicator */
#define HAVE_FINITE 1

#undef HAVE_IEEEFP_H		/* "-Wall" */
#undef HAVE_IEEE754_H		/* Linux */

#ifdef HAVE_ISNAN
#ifdef HAVE_FINITE
#define IEEE_754 1
#endif
#endif
extern int _isnan(double);
extern int _finite(double);
#define isnan(a) _isnan((a))
#define finite(a) _finite((a))


/* Signal Handler Type */
#define RETSIGTYPE int

/* Process ID */
#undef pid_t
/* Object size */
#undef size_t
/* Fix a GCC bug on AIX? */
#undef blkcnt_t

/* Define if you have <sys/wait.h> that is POSIX.1 compatible.  */
#undef HAVE_SYS_WAIT_H

/* Dynamic Linking */
#undef HAVE_DL_H		/* hpux */
#undef HAVE_DLFCN_H		/* Everything else */

/* ELF Binary Format */
#undef HAVE_ELF_H

/* Process Timing */
#define HAVE_TIMES 1
#undef HAVE_SYS_TIME_H
#undef HAVE_SYS_TIMES_H
#undef TIME_WITH_SYS_TIME

/* XDR Library Available */
#define HAVE_RPC_RPC_H 1
#define HAVE_RPC_XDR_H 1

/* HDF5 Library Available */
#undef HAVE_HDF5_H

/* General String Comparison */
#undef HAVE_STRCOLL

/* Inverse Hyperbolics */
#define HAVE_ASINH 1
#define HAVE_ACOSH 1
#define HAVE_ATANH 1
double asinh(double);
double acosh(double);
double atanh(double);


/* Unix commands through a subshell */
#undef HAVE_SYSTEM

/* IEEE Rounding */
#undef HAVE_RINT

/* HPUX rint is broken */
#undef USE_BUILTIN_RINT

/* POSIX Regular Expressions Available */
#define HAVE_REGCOMP 1

/* Compatibility for "memmove" on older BSD platforms */
#undef HAVE_MEMMOVE
#undef HAVE_MEMCPY
#undef HAVE_BCOPY

#define HAVE_POSIX_SETJMP 1
#define PSIGNAL

/* Some Linux systems may need this */
#undef NEED___SETFPUCW

/* Fortran and C Links */
#define HAVE_F77_UNDERSCORE 1

#ifdef HAVE_F77_UNDERSCORE
#define F77_SYMBOL(x)	x ## _
#define F77_QSYMBOL(x)	#x ## "_"
#else
#define F77_SYMBOL(x)	x
#define F77_QSYMBOL(x)	#x
#endif

/* GNU Readline Library */
#undef HAVE_LIBREADLINE
#undef HAVE_READLINE_HISTORY_H
#undef HAVE_READLINE_READLINE_H

/* Miscellaneous */ 
#define HAVE_LOCALE_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_UNISTD_H 1

/* Dirent stuff */
#define HAVE_DIRENT_H 1
#undef HAVE_SYS_NDIR_H
#undef HAVE_SYS_DIR_H
#undef HAVE_NDIR_H

/* Bug Workarounds */
#undef HAVE_OSF_SPRINTF_BUG
#undef CALLOC_BROKEN
#undef FINITE_BROKEN
#undef LOG_BROKEN

/* Some platforms other than ELF drop the leading _ */
#undef HAVE_NO_SYMBOL_UNDERSCORE
#ifndef HAVE_NO_SYMBOL_UNDERSCORE
#ifdef HAVE_ELF_H
#define HAVE_NO_SYMBOL_UNDERSCORE
#endif
#endif

/* SunOS 4 is famous for broken header files */
#undef SunOS4
#ifdef SunOS4
# ifndef NULL
#  define	NULL		0
# endif
# ifndef RAND_MAX
#  define	RAND_MAX	32767
# endif
#endif /* SunOS4 */

/* Printing Command */
#define R_PRINTCMD	""

/* Getting the working directory */
#undef HAVE_GETCWD

/* Maximal length of an entire file name */
#undef HAVE_SYS_PARAM_H

/* for platform.c to put in .Platform */
#ifdef Unix
#define OSTYPE      "Unix"
#define FILESEP     "/"
#define SHLIBEXT    ""
#define DYNLOADEXT  "." ## SHLIBEXT
#endif

#ifdef Macintosh
#define OSTYPE      "Macintosh"
#define FILESEP     ":"
#define DYNLOADEXT  ".dll"
#endif

#ifdef Win32
#define OSTYPE      "Windows"
#define FILESEP     "/"
#define DYNLOADEXT  ".dll"
#endif

#define R_PLATFORM	"Windows"
#define R_CPU		"x86"
#define R_VENDOR	"IBM"
#define R_OS		"Win32"

/* Windows-specific */
#ifndef M_PI
#define M_PI		3.141592653589793238462643383279502884197169399375
#endif
#define	M_1_PI		0.31830988618379067154	/* 1/pi */
#define	M_PI_2		1.57079632679489661923	/* pi/2 */
#define	M_LOG10E	0.43429448190325182765	/* log 10e */
#define M_SQRT_2        1.41421356237309504880  /* sqrt(2) */

#ifndef max
#define max(a,b)	(((a)>(b))?(a):(b))
#define min(a,b)	(((a)<(b))?(a):(b))
#endif
/* P.D. */
#define erf(x) 2*pnorm((x)*M_SQRT_2,0,1)-1
#define erfc(x) 2*pnorm(-(x)*M_SQRT_2,0,1)
#define gamma(x) gammafn(x)
#define lgamma(x) lgammafn(x)



#endif
