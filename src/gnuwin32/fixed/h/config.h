#ifndef _CONFIG_H
#define _CONFIG_H

/* Define to one of `_getb67', `GETB67', `getb67' for Cray-2 and Cray-YMP
   systems. This function is required for `alloca.c' support on those systems.
   */
/* #undef CRAY_STACKSEG_END */

/* Define if using `alloca.c'. */
/* #undef C_ALLOCA */

/* Define to dummy `main' function (if any) required to link to the Fortran 77
   libraries. */
/* #undef F77_DUMMY_MAIN */

/* Define to a macro mangling the given C identifier (in lower and upper
   case), which must not contain underscores, for linking with Fortran. */
/* #undef F77_FUNC */

/* As F77_FUNC, but for C identifiers containing underscores. */
/* #undef F77_FUNC_ */

/* Define if you have the `access' function. */
#define HAVE_ACCESS 1

/* Define if you have the `acosh' function. */
/* #undef HAVE_ACOSH */

/* Define if you have `alloca', as a function or macro. */
#define HAVE_ALLOCA 1

/* Define if you have <alloca.h> and it should be used (not on Ultrix). */
/* #undef HAVE_ALLOCA_H */

/* Define if you have the <arpa/inet.h> header file. */
/* #undef HAVE_ARPA_INET_H */

/* Define if you have the `asinh' function. */
/* #undef HAVE_ASINH */

/* Define if you have the `atanh' function. */
/* #undef HAVE_ATANH */

/* Define if you have the `bcopy' function. */
/* #undef HAVE_BCOPY */

/* Define if you have BSD networking headers and libraries. */
/* #undef HAVE_BSD_NETWORKING */

/* Define if you have the `bzero' function. */
/* #undef HAVE_BZERO */

/* Define to 1 if you have the declaration of `isfinite', and to 0 if you
   don't. */
#define HAVE_DECL_ISFINITE 0

/* Define to 1 if you have the declaration of `isnan', and to 0 if you don't.
   */
#define HAVE_DECL_ISNAN 0

/* Define if you have the <dirent.h> header file, and it defines `DIR'. */
#define HAVE_DIRENT_H 1

/* Define if you have the <dlfcn.h> header file. */
/* #undef HAVE_DLFCN_H */

/* Define if you have the <dl.h> header file. */
/* #undef HAVE_DL_H */

/* Define if you don't have `vprintf' but do have `_doprnt.' */
#define HAVE_DOPRNT 1

/* Define if C's Rcomplex and Fortran's COMPLEX*16 can be interchanged, and
   can do arithmetic on the latter. */
#define HAVE_DOUBLE_COMPLEX 1

/* Define if you have the <elf.h> header file. */
/* #undef HAVE_ELF_H */

/* Define if you have the <errno.h> header file. */
#define HAVE_ERRNO_H 1

/* Define if you have the `expm1' function. */
#undef HAVE_EXPM1

/* Define if your Fortran compiler appends an underscore to external names. */
#define HAVE_F77_UNDERSCORE 1

/* Define if you have the <fcntl.h> header file. */
#undef HAVE_FCNTL_H
/* it exists, but is non-standard */

/* Define if you have the `finite' function. */
#define HAVE_FINITE 1

/* Define if you have the <floatingpoint.h> header file. */
/* #undef HAVE_FLOATINGPOINT_H */

/* Define if you have the `fork' function. */
/* #undef HAVE_FORK */

/* Define if you have the <fpu_control.h> header file. */
/* #undef HAVE_FPU_CONTROL_H */

/* Define if you have the `ftruncate' function. */
/* #undef HAVE_FTRUNCATE */

/* Define if you have the `getcwd' function. */
#define HAVE_GETCWD 1

/* Define if you have the `getgrgid' function. */
/* #undef HAVE_GETGRGID */

/* Define if you have the `getpwuid' function. */
/* #undef HAVE_GETPWUID */

/* Define if you have the `getuid' function. */
/* #undef HAVE_GETUID */

/* Define if you have the GNU C library version >= 2. This is needed to fix a
   problem with getting the prototype of strptime(). */
/* #undef HAVE_GLIBC2 */

/* Define if the GNOME headers and libraries are available, and want the GNOME
   GUI to be built. */
/* #undef HAVE_GNOME */

/* Define if you have the <grp.h> header file. */
/* #undef HAVE_GRP_H */

/* Define if you have the `hypot' function. */
#define HAVE_HYPOT 1

/* Define if you have the <ieee754.h> header file. */
/* #undef HAVE_IEEE754_H */

/* Define if you have the <ieeefp.h> header file. */
/* #undef HAVE_IEEEFP_H */

/* Define if you have support for ftp/http access. */
#define HAVE_INTERNET 1

/* Define if you have the <inttypes.h> header file. */
/* #undef HAVE_INTTYPES_H */

/* Define if you have the `isascii' function. */
#define HAVE_ISASCII 1

/* Define if you have the `isnan' function. */
#define HAVE_ISNAN 1

/* Define if you have the JPEG headers and libraries. */
#define HAVE_JPEG 1

/* Define if you have the `dl' library (-ldl). */
/* #undef HAVE_LIBDL */

/* Define if you have the `m' library (-lm). */
#undef HAVE_LIBM

/* Define if you have the `ncurses' library (-lncurses). */
/* #undef HAVE_LIBNCURSES */

/* Define if you have the `readline' library (-lreadline). */
/* #undef HAVE_LIBREADLINE */

/* Define if you have the `termcap' library (-ltermcap). */
/* #undef HAVE_LIBTERMCAP */

/* Define if you have the `termlib' library (-ltermlib). */
/* #undef HAVE_LIBTERMLIB */

/* Define if you have the `tk' library (-ltk). */
/* #undef HAVE_LIBTK */

/* Define if you have the <locale.h> header file. */
#define HAVE_LOCALE_H 1

/* Define if you have the `log1p' function. */
/* #undef HAVE_LOG1P */

/* Define if you have the `matherr' function. */
/* #undef HAVE_MATHERR */

/* Define if you have the `memcpy' function. */
#define HAVE_MEMCPY 1

/* Define if you have the `memmove' function. */
#define HAVE_MEMMOVE 1

/* Define if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define if you have the `mempcpy' function. */
/* #undef HAVE_MEMPCPY */

/* Define if you have the `mkfifo' function. */
/* #undef HAVE_MKFIFO */

/* Define if you have the <ndir.h> header file, and it defines `DIR'. */
/* #undef HAVE_NDIR_H */

/* Define if you have the <netdb.h> header file. */
/* #undef HAVE_NETDB_H */

/* Define if you have the <netinet/in.h> header file. */
/* #undef HAVE_NETINET_IN_H */

/* Define if your C compiler does not prepend an underscore to external names.
   */
#define HAVE_NO_SYMBOL_UNDERSCORE 1

/* Define if you have the PNG headers and libraries. */
#define HAVE_PNG 1

/* Define if you have the `popen' function. */
#define HAVE_POPEN 1

/* Define if your system time functions do not count leap seconds, as required
   by POSIX. */
#define HAVE_POSIX_LEAPSECONDS 1

/* Define if you have POSIX.1 compatible sigsetjmp/siglongjmp. */
#define HAVE_POSIX_SETJMP 1

/* Define if you have the `putenv' function. */
#define HAVE_PUTENV 1

/* Define if you have the <pwd.h> header file. */
/* #undef HAVE_PWD_H */

/* Define if you have the <readline/history.h> header file. */
/* #undef HAVE_READLINE_HISTORY_H */

/* Define if you have the <readline/readline.h> header file. */
/* #undef HAVE_READLINE_READLINE_H */

/* Define if you have the `rint' function. */
/* #undef HAVE_RINT */

/* Define if you have the <rpc/xdr.h> header file. */
/* #undef HAVE_RPC_XDR_H */

/* Define if you have the `setenv' function. */
/* #undef HAVE_SETENV */

/* Define if you have the `setitimer' function. */
/* #undef HAVE_SETITIMER */

/* Define if you have the `snprintf' function. */
#define HAVE_SNPRINTF 1

/* Define if you have support for sockets. */
#define HAVE_SOCKETS 1

/* Define if you have the `stat' function. */
#define HAVE_STAT 1

/* Define if you have the <stdarg.h> header file. */
#define HAVE_STDARG_H 1

/* Define if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define if you have the `strcoll' function. */
#define HAVE_STRCOLL 1

/* Define if you have the `strdup' function. */
#define HAVE_STRDUP 1

/* Define if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define if you have the `strptime' function. */
/* #undef HAVE_STRPTIME */

/* Define if you have the `system' function. */
/* #undef HAVE_SYSTEM have it but no the same as Unix */

/* Define if you have the <sys/dir.h> header file, and it defines `DIR'. */
/* #undef HAVE_SYS_DIR_H */

/* Define if you have the <sys/ndir.h> header file, and it defines `DIR'. */
/* #undef HAVE_SYS_NDIR_H */

/* Define if you have the <sys/param.h> header file. */
/* #undef HAVE_SYS_PARAM_H */

/* Define if you have the <sys/select.h> header file. */
/* #undef HAVE_SYS_SELECT_H */

/* Define if you have the <sys/socket.h> header file. */
/* #undef HAVE_SYS_SOCKET_H */

/* Define if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define if you have the <sys/times.h> header file. */
/* #undef HAVE_SYS_TIMES_H */

/* Define if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define if you have the <sys/utsname.h> header file. */
/* #undef HAVE_SYS_UTSNAME_H */

/* Define if you have <sys/wait.h> that is POSIX.1 compatible. */
/* #undef HAVE_SYS_WAIT_H */

/* Define if you have the Tcl/Tk headers and libraries and want Tcl/Tk support
   to be built. */
#define HAVE_TCLTK 1

/* Define if you have the `times' function. */
#define HAVE_TIMES 1
/* really means have timing capabilities */

/* Define if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define if you have the `unsetenv' function. */
/* #undef HAVE_UNSETENV */

/* Define if you have the `vfork' function. */
/* #undef HAVE_VFORK */

/* Define if you have the <vfork.h> header file. */
/* #undef HAVE_VFORK_H */

/* Define if you have the `vprintf' function. */
#define HAVE_VPRINTF 1

/* Define if you have the `vsnprintf' function. */
#define HAVE_VSNPRINTF 1

/* Define if calloc(0) returns a null pointer. */
#define HAVE_WORKING_CALLOC 1

/* Define if finite() is correct for -Inf/NaN/Inf. */
#define HAVE_WORKING_FINITE 1

/* Define if `fork' works. */
/* #undef HAVE_WORKING_FORK */

/* Define if log() is correct for 0/-1. */
#define HAVE_WORKING_LOG 1

/* Define if strptime() exists and does not fail pre-1970. */
/* #undef HAVE_WORKING_STRPTIME */

/* Define if `vfork' works. */
/* #undef HAVE_WORKING_VFORK */

/* Define if you have the X11 headers and libraries, and want the X11 GUI to
   be built. */
/* #undef HAVE_X11 */

/* Define if you have the XDR headers and library routines. */
/* #undef HAVE_XDR */

/* Define if you have the zlib headers and libraries. */
#define HAVE_ZLIB 1

/* Define if you have IEEE 754 floating point arithmetic. */
#define IEEE_754 1

/* Define if you have 32 bit ints. */
#define INT_32_BITS 1

/* Define according to your operating system type. */
/* #undef Macintosh */

/* Define if your system needs __setfpucw() to control FPU rounding. This was
   used to control floating point precision, rounding and floating point
   exceptions on older Linux systems. As of GLIBC 2.1 this function is not
   used anymore. */
/* #undef NEED___SETFPUCW */

/* Name of package */
#define PACKAGE "R"

/* Define as the return type of signal handlers (`int' or `void'). */
#define RETSIGTYPE void

/* Define this to be the name of the CPU of your system. */
#define R_CPU  "x86"

/* Define this to be the name of the OS of your system. */
#define R_OS "mingw32"

/* Define this to be the canonical name (cpu-vendor-os) of your system. */
#define R_PLATFORM "i386-pc-mingw32"

/* Define this to be printing command on your system. */
#define R_PRINTCMD  "UNKNOWN"

/* Define this to enable R-level profiling. */
#define R_PROFILING 1

/* Define this to be the name of the vendor of your system. */
#define R_VENDOR "pc"

/* Define this to be the extension used for shared libraries on your system.
   */
#define SHLIB_EXT ".dll"

/* The size of a `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of a `long', as computed by sizeof. */
#define SIZEOF_LONG 4

/* The size of a `long double', as computed by sizeof. */
#define SIZEOF_LONG_DOUBLE 12

/* The size of a `long long', as computed by sizeof. */
#define SIZEOF_LONG_LONG 8

/* Type for socket lengths: socklen_t, sock_t, int? */
#define SOCKLEN_T int

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at run-time.
        STACK_DIRECTION > 0 => grows toward higher addresses
        STACK_DIRECTION < 0 => grows toward lower addresses
        STACK_DIRECTION = 0 => direction of growth unknown */
/* #undef STACK_DIRECTION */

/* Define if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define if you provide support for the libxml ftp/http functions. */
#define SUPPORT_LIBXML 1

/* Define if your system is SunOS4, which is famous for broken header files.
   */
/* #undef SunOS4 */

/* Define to enable provoking compile errors on write barrier violation. */
/* #undef TESTING_WRITE_BARRIER */

/* Define if you can safely include both <sys/time.h> and <time.h>. */
#define TIME_WITH_SYS_TIME 1

/* Define if your rint() is broken on your system. Apparently needed on HPUX.
   */
/* #undef USE_BUILTIN_RINT */

/* Define according to your operating system type. */
/* #undef Unix */

/* Version number of package */
#define VERSION "1.5.0"

/* Define if your processor stores words with the most significant byte first
   (like Motorola and SPARC, unlike Intel and VAX). */
/* #undef WORDS_BIGENDIAN */

/* Define according to your operating system type. */
#define Win32 1

/* Define if the X Window System is missing or not being used. */
#define X_DISPLAY_MISSING 1

/* Define if on AIX 3.
   System headers sometimes define this.
   We just want to avoid a redefinition error message.  */
#ifndef _ALL_SOURCE
/* # undef _ALL_SOURCE */
#endif

/* Define to 'long' if <sys/types.h> does not define. Apparently necessary to
   fix a GCC bug on AIX? */
#undef blkcnt_t

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef pid_t */

/* Define to `unsigned' if <sys/types.h> does not define. */
/* #undef size_t */

/* Define as `fork' if `vfork' does not work. */
#define vfork fork

/* Windows-specific */
#define PSIGNAL
#define PLOTHISTORY

#define snprintf  _snprintf
#define vsnprintf  _vsnprintf

extern int _isnan(double);
extern int _finite(double);
#define isnan(a) _isnan((a))
#define finite(a) _finite((a))

#ifndef max
#define max(a,b)	(((a)>(b))?(a):(b))
#define min(a,b)	(((a)<(b))?(a):(b))
#endif

void R_ProcessEvents(void);

/* 27/03/2000 win32-api needs this for ANSI compliance */
#define NONAMELESSUNION

#endif /* not _CONFIG_H */
