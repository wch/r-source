/* acconfig.h
   Descriptive text for the C preprocessor macros that are needed by R.
   */

#ifndef _CONFIG_H
#define _CONFIG_H

@TOP@

/* Define according to your operating system type. */
#undef Unix
#undef Win32
#undef Macintosh

/* Define if C's Rcomplex and Fortran's COMPLEX*16 can be interchanged,
   and can do arithmetic on the latter */
#undef HAVE_DOUBLE_COMPLEX

/* Define if calloc(0) does not return a null pointer. */
#undef CALLOC_BROKEN

/* Define if finite() is wrong for -Inf/NaN/Inf. */
#undef FINITE_BROKEN

/* Define if strptime() does not exist or fails pre-1970. */
#undef STRPTIME_BROKEN

/* Define if you have BSD networking headers and libraries. */
#undef HAVE_BSD_NETWORKING

/* Define if you have support for sockets. */
#undef HAVE_SOCKETS

/* Define if you have support for ftp/http access. */
#undef HAVE_INTERNET

/* Define if you provide support for libxml's ftp/http functions. */
#undef SUPPORT_LIBXML

/* Define if your Fortran compiler appends an underscore to external
   names. */
#undef HAVE_F77_UNDERSCORE

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

/* Define if you have the XDR headers and library routines. */
#undef HAVE_XDR

/* Define if you have the zlib headers and libraries. */
#undef HAVE_ZLIB

/* Define if you have IEEE 754 floating point arithmetic. */
#undef IEEE_754

/* Define if log() is wrong for 0/-1. */
#undef LOG_BROKEN

/* Define if your system needs __setfpucw() to control FPU rounding.
   This was used to control floating point precision, rounding and
   floating point exceptions on older Linux systems.
   As of GLIBC 2.1 this function is not used anymore. */
#undef NEED___SETFPUCW

/* Define this to be the name of the CPU of your system. */
#undef R_CPU

/* Define this to be the name of the OS of your system. */
#undef R_OS

/* Define this to be the canonical name (cpu-vendor-os) of your
   system. */
#undef R_PLATFORM

/* Define this to be printing command on your system. */
#undef R_PRINTCMD

/* Define this to enable R-level profiling. */
#undef R_PROFILING

/* Define this to be the name of the vendor of your system. */
#undef R_VENDOR

/* Define this to be the extension used for shared libraries on your
   system. */
#undef SHLIB_EXT

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

/* Type for socket lengths: socklen_t, sock_t, int? */
#undef SOCKLEN_T

@BOTTOM@

#endif /* not _CONFIG_H */
