#ifndef PLATFORM_H_
#define PLATFORM_H_

#define Macintosh

/* Floating Point Arithmetic */
#define HAVE_MATHERR		/* System V */
#define HAVE_ISNAN		/* IEEE Arith indicator */

/* Signal Handler Type */
#define RETSIGTYPE int

/* Dynamic Linking */
#undef HAVE_DL_H		/* hpux */
#undef HAVE_DLFCN_H		/* Everything else */

/* ELF Binary Format */
#undef HAVE_ELF_H

/* Process Timing */
#undef HAVE_TIMES
#undef HAVE_TIMES_H
#undef HAVE_SYS_TIMES_H

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

/* Bug Workarounds */
#undef HAVE_OSF_SPRINTF_BUG

/* Some platforms other than ELF drop the leading _ */
#undef HAVE_NO_SYMBOL_UNDERSCORE
#ifndef HAVE_NO_SYMBOL_UNDERSCORE
#ifdef HAVE_ELF_H
#define HAVE_NO_SYMBOL_UNDERSCORE
#endif
#endif

#define R_PLATFORM	"Macintosh"
#define R_CPU		"PPC"
#define R_VENDOR	"Apple"
#define R_OS		"MACOS"
#define R_MAJOR		"16"
#define R_MINOR		"0"
#define R_STATUS	"Alpha"
#define R_STATUS_REV	"0"
#define R_DAY		"1"
#define R_MONTH		"Jan"
#define R_YEAR		"1997"

#endif
