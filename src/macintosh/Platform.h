#ifndef PLATFORM_H_
#define PLATFORM_H_

#define Macintosh

#define LONG_32_BITS	1

				/* Floating Point Arithmetic */
#undef HAVE_MATHERR		/* System V */
#define HAVE_ISNAN		/* IEEE Arith indicator */
#define HAVE_FINITE		/* finite() (really) isfinite() */

#undef HAVE_IEEEFP_H		/* "-Wall" */
#undef HAVE_IEEE754_H		/* Linux */

#ifdef HAVE_ISNAN
#ifdef HAVE_FINITE
#define IEEE_754
#endif
#endif

/* Signal Handler Type */
#define RETSIGTYPE void

/* Dynamic Linking */
#undef HAVE_DL_H		/* hpux */
#undef HAVE_DLFCN_H		/* Everything else */

/* ELF Binary Format */
#undef HAVE_ELF_H

/* Process Timing */
#undef HAVE_TIMES
#undef HAVE_TIMES_H
#undef HAVE_SYS_TIMES_H

/* XDR Library Available */
#undef HAVE_RPC_XDR_H

/* HDF5 Library Available */
#undef HAVE_HDF5_H

/* General String Comparison */
#undef HAVE_STRCOLL

/* Inverse Hyperbolics */
#define HAVE_ASINH
#define HAVE_ACOSH
#define HAVE_ATANH

/* Unix commands through a subshell */
#undef HAVE_SYSTEM

/* IEEE Rounding */
#undef HAVE_RINT

/* HPUX rint is broken */
#undef USE_BUILTIN_RINT

/* POSIX Regular Expressions Available */
#define HAVE_REGCOMP

/* Compatibility for "memmove" on older BSD platforms */
#define HAVE_MEMMOVE
#define HAVE_MEMCPY
#define HAVE_BCOPY

/* Compatibility for setjmp / longjmp */
#undef HAVE_SIGSETJMP

#ifdef HAVE_SIGSETJMP
#define JMP_BUF sigjmp_buf
#define SETJMP(x) sigsetjmp(x,1)
#define LONGJMP(x,i) siglongjmp(x,i)
#else
#define JMP_BUF jmp_buf
#define SETJMP(x) setjmp(x)
#define LONGJMP(x,i) longjmp(x,i)
#endif


/* Fortran and C Links */
#define HAVE_F77_UNDERSCORE

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

#undef HAVE_LOCALE_H
#undef HAVE_UNISTD_H

/* Bug Workarounds */
#undef HAVE_OSF_SPRINTF_BUG

/* Some platforms other than ELF drop the leading _ */
#undef HAVE_NO_SYMBOL_UNDERSCORE
#ifndef HAVE_NO_SYMBOL_UNDERSCORE
#ifdef HAVE_ELF_H
#define HAVE_NO_SYMBOL_UNDERSCORE
#endif
#endif

/* Printing Command */
#define R_PRINTCMD	""

#define R_PLATFORM	"Macintosh"
#define R_CPU		"ppc"
#define R_VENDOR	"apple"
#define R_OS		"MacOS"
#define R_MAJOR		"0"
#define R_MINOR		"64.0"
#define R_STATUS	"Devel"
#define R_STATUS_REV	"0"
#define R_DAY		"9"
#define R_MONTH		"December"
#define R_YEAR		"1998"

#endif
