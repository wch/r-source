/* src/macintosh/config.mac.h.  Hand edited from config.h.in.  */
/* acconfig.h
   Descriptive text for the C preprocessor macros that are needed by R.
   */

#ifndef _CONFIG_H
#define _CONFIG_H

/* Define if on AIX 3.
   System headers sometimes define this.
   We just want to avoid a redefinition error message.  */
#ifndef _ALL_SOURCE
#undef _ALL_SOURCE
#endif

/* Define if using alloca.c.  */
#undef C_ALLOCA

/* Define to empty if the keyword does not work.  */
#undef const

/* Define to one of _getb67, GETB67, getb67 for Cray-2 and Cray-YMP systems.
   This function is required for alloca.c support on those systems.  */
#undef CRAY_STACKSEG_END

/* Define if you have alloca, as a function or macro.  */
#undef HAVE_ALLOCA

/* Define if you have <alloca.h> and it should be used (not on Ultrix).  */
#define HAVE_ALLOCA_H 1

/* Define if you don't have vprintf but do have _doprnt.  */
#undef HAVE_DOPRNT

/* Define if you have <sys/wait.h> that is POSIX.1 compatible.  */
#undef HAVE_SYS_WAIT_H

/* Define if you have <vfork.h>.  */
#undef HAVE_VFORK_H

/* Define if you have the vprintf function.  */
#undef HAVE_VPRINTF

/* Define to `int' if <sys/types.h> doesn't define.  */
#undef pid_t

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE int

/* Define to `unsigned' if <sys/types.h> doesn't define.  */
#undef size_t

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at run-time.
 STACK_DIRECTION > 0 => grows toward higher addresses
 STACK_DIRECTION < 0 => grows toward lower addresses
 STACK_DIRECTION = 0 => direction of growth unknown
 */
#undef STACK_DIRECTION

/* Define if you have the ANSI C header files.  */
#undef STDC_HEADERS

/* Define if you can safely include both <sys/time.h> and <time.h>.  */
#undef TIME_WITH_SYS_TIME

/* Define vfork as fork if vfork does not work.  */
#undef vfork

/* Define if your processor stores words with the most significant
   byte first (like Motorola and SPARC, unlike Intel and VAX).  */
#define WORDS_BIGENDIAN 1

#define	SIGUSR1 30	/* user defined signal 1 */
#define	SIGUSR2 31	/* user defined signal 2 */

/* Define if the X Window System is missing or not being used.  */
#define X_DISPLAY_MISSING 1

/* Define according to your operating system type. */
#undef Unix
#undef Win32
#define Macintosh 1

/* Define if calloc(0) does not return a null pointer. */
#undef CALLOC_BROKEN

/* Define if finite() is wrong for -Inf/NaN/Inf. */
#undef FINITE_BROKEN

/* Define if you have BSD networking headers and libraries. */
#undef HAVE_BSD_NETWORKING

/* Define if your Fortran compiler appends an underscore to external
   names. */
#define HAVE_F77_UNDERSCORE 1

/* Define if you have the GNU C library version >= 2.
   This is needed to fix a problem with getting the prototype of
   strptime(). */
#undef HAVE_GLIBC2

/* Define if the GNOME headers and libraries are available, and want the
   GNOME GUI to be built. */
#undef HAVE_GNOME

/* Define if you have the JPEG headers and libraries. */
#undef HAVE_JPEG

/* Define if your C compiler does not prepend an underscore to external
   names. */
#undef HAVE_NO_SYMBOL_UNDERSCORE

/* Define if you have the PNG headers and libraries. */
#undef HAVE_PNG

/* Define if you have POSIX.1 compatible sigsetjmp/siglongjmp. */
#undef HAVE_POSIX_SETJMP

/* Define if you have the Tcl/Tk headers and libraries and want Tcl/Tk
   support to be built. */
#undef HAVE_TCLTK

/* Define if you have the X11 headers and libraries, and want the X11
   GUI to be built. */
#undef HAVE_X11

/* Define if you have IEEE 754 floating point arithmetic. */
#define IEEE_754 1

/* Define if log() is wrong for 0/-1. */
#undef LOG_BROKEN

/* Define if your system needs __setfpucw() to control FPU rounding.
   This was used to control floating point precision, rounding and
   floating point exceptions on older Linux systems.
   As of GLIBC 2.1 this function is not used anymore. */
#undef NEED___SETFPUCW

/* Define this to be the name of the CPU of your system. */
#define R_CPU		"ppc"

/* Define this to be the name of the OS of your system. */
#define R_OS		"MacOS"

/* Define this to be the canonical name (cpu-vendor-os) of your
   system. */
#define R_PLATFORM	"ppc-apple-macos"

/* Define this to be printing command on your system. */
#undef R_PRINTCMD

/* Define this to enable R-level profiling. */
#undef R_PROFILING

/* Define this to be the name of the vendor of your system. */
#define R_VENDOR	"apple"

/* Define this to be the extension used for shared libraries on your
   system. */
#define SHLIB_EXT "Lib"

/* Define if your system is SunOS4, which is famous for broken header
   files. */
#undef SunOS4

/* Define to enable provoking compile errors on write barrier
   violation. */
#undef TESTING_WRITE_BARRIER

/* Define if your system's rint() is broken.
   Apparently needed on HPUX. */
#undef USE_BUILTIN_RINT

/* Define if your system time functions count leap seconds. */
#undef USING_LEAPSECONDS

/* Define on HPUX if not using GCC. */
#undef _HPUX_SOURCE

/* Define to `long' if <sys/types.h> does not define.
   Apparently necessary to fix a GCC bug on AIX? */
#undef blkcnt_t

/* The number of bytes in a long.  */
#define SIZEOF_LONG 4

/* The number of bytes in a long double.  */
#define SIZEOF_LONG_DOUBLE 0

/* The number of bytes in a long long.  */
#define SIZEOF_LONG_LONG 0

/* Define if you have the access function.  */
#undef HAVE_ACCESS

/* Define if you have the acosh function.  */
#define HAVE_ACOSH 1

/* Define if you have the asinh function.  */
#define HAVE_ASINH 1

/* Define if you have the atanh function.  */
#define HAVE_ATANH 1

/* Define if you have the bcopy function.  */
#undef HAVE_BCOPY

/* Define if you have the finite function.  */
#define HAVE_FINITE 1

/* Define if you have the getcwd function.  */
#define HAVE_GETCWD 1

/* Define if you have the getgrgid function.  */
#undef HAVE_GETGRGID

/* Define if you have the getpwuid function.  */
#undef HAVE_GETPWUID

/* Define if you have the getuid function.  */
#define HAVE_GETUID 1

/* Define if you have the hypot function.  */
#define HAVE_HYPOT 1

/* Define if you have the isnan function.  */
#define HAVE_ISNAN 1

/* Define if you have the matherr function.  */
#undef HAVE_MATHERR

/* Define if you have the memcpy function.  */
#define HAVE_MEMCPY 1

/* Define if you have the memmove function.  */
#define HAVE_MEMMOVE 1

/* Define if you have the popen function.  */
#undef HAVE_POPEN

/* Define if you have the putenv function.  */
#undef HAVE_PUTENV

/* Define if you have the rint function.  */
#define HAVE_RINT 1

/* Define if you have the setenv function.  */
#undef HAVE_SETENV

/* Define if you have the setitimer function.  */
#undef HAVE_SETITIMER

/* Define if you have the stat function.  */
#define HAVE_STAT 1

/* Define if you have the strcoll function.  */
#define HAVE_STRCOLL 1

/* Define if you have the strdup function.  */
#undef HAVE_STRDUP

/* Define if you have the strptime function.  */
#undef HAVE_STRPTIME

/* Define if you have the system function.  */
#undef HAVE_SYSTEM

/* Define if you have the times function.  */
#define HAVE_TIMES 1

/* Define if you have the unsetenv function.  */
#undef HAVE_UNSETENV

/* Define if you have the vsnprintf function.  */
#define HAVE_VSNPRINTF 1

/* Define if you have the <dirent.h> header file.  */
#undef HAVE_DIRENT_H

/* Define if you have the <dl.h> header file.  */
#undef HAVE_DL_H

/* Define if you have the <dlfcn.h> header file.  */
#define HAVE_DLFCN_H 1

/* Define if you have the <elf.h> header file.  */
#undef HAVE_ELF_H

/* Define if you have the <floatingpoint.h> header file.  */
#undef HAVE_FLOATINGPOINT_H

/* Define if you have the <fpu_control.h> header file.  */
#undef HAVE_FPU_CONTROL_H

/* Define if you have the <grp.h> header file.  */
#undef HAVE_GRP_H

/* Define if you have the <ieee754.h> header file.  */
#undef HAVE_IEEE754_H

/* Define if you have the <ieeefp.h> header file.  */
#undef HAVE_IEEEFP_H

/* Define if you have the <locale.h> header file.  */
#define HAVE_LOCALE_H 1

/* Define if you have the <ndir.h> header file.  */
#undef HAVE_NDIR_H

/* Define if you have the <netdb.h> header file.  */
#undef HAVE_NETDB_H

/* Define if you have the <netinet/in.h> header file.  */
#undef HAVE_NETINET_IN_H

/* Define if you have the <netinet/tcp.h> header file.  */
#undef HAVE_NETINET_TCP_H

/* Define if you have the <pwd.h> header file.  */
#undef HAVE_PWD_H

/* Define if you have the <readline/history.h> header file.  */
#undef HAVE_READLINE_HISTORY_H

/* Define if you have the <readline/readline.h> header file.  */
#undef HAVE_READLINE_READLINE_H

/* Define if you have the <rpc/rpc.h> header file.  */
#define HAVE_RPC_RPC_H 1

/* Define if you have the <rpc/xdr.h> header file.  */
#define HAVE_RPC_XDR_H 1

/* Define if you have the <string.h> header file.  */
#define HAVE_STRING_H 1

/* Define if you have the <sys/dir.h> header file.  */
#undef HAVE_SYS_DIR_H

/* Define if you have the <sys/ndir.h> header file.  */
#undef HAVE_SYS_NDIR_H

/* Define if you have the <sys/param.h> header file.  */
#undef HAVE_SYS_PARAM_H

/* Define if you have the <sys/socket.h> header file.  */
#undef HAVE_SYS_SOCKET_H

/* Define if you have the <sys/stat.h> header file.  */
#define HAVE_SYS_STAT_H 1

/* Define if you have the <sys/time.h> header file.  */
#define HAVE_SYS_TIME_H 1

/* Define if you have the <sys/times.h> header file.  */
#undef HAVE_SYS_TIMES_H

/* Define if you have the <sys/utsname.h> header file.  */
#define HAVE_SYS_UTSNAME_H 1

/* Define if you have the <unistd.h> header file.  */
#define HAVE_UNISTD_H 1

/* Define if you have the dl library (-ldl).  */
#undef HAVE_LIBDL

/* Define if you have the m library (-lm).  */
#undef HAVE_LIBM

/* Define if you have the moto library (-lmoto).  */
#undef HAVE_LIBMOTO

/* Define if you have the ncurses library (-lncurses).  */
#undef HAVE_LIBNCURSES

/* Define if you have the nsl library (-lnsl).  */
#undef HAVE_LIBNSL

/* Define if you have the readline library (-lreadline).  */
#undef HAVE_LIBREADLINE

/* Define if you have the socket library (-lsocket).  */
#undef HAVE_LIBSOCKET

/* Define if you have the tk library (-ltk).  */
#undef HAVE_LIBTK

/* Define if you have the z library (-lz).  */
#undef HAVE_LIBZ

/* Name of package */
#define PACKAGE "R"

/* Version number of package */
#define VERSION "1.2.2"


#endif /* not _CONFIG_H */
