#ifndef R_CONFIG_H
#define R_CONFIG_H

/* Define to one of `_getb67', `GETB67', `getb67' for Cray-2 and Cray-YMP
   systems. This function is required for `alloca.c' support on those systems.
   */
/* #undef CRAY_STACKSEG_END */

/* Define to 1 if using `alloca.c'. */
/* #undef C_ALLOCA */

/* Define to 1 if translation of program messages to the user's native
   language is requested. */
#define ENABLE_NLS 1

/* Define to dummy `main' function (if any) required to link to the Fortran
   libraries. */
/* #undef F77_DUMMY_MAIN */

/* Define to a macro mangling the given C identifier (in lower and upper
   case), which must not contain underscores, for linking with Fortran. */
#define F77_FUNC(name,NAME) name ## _

/* As F77_FUNC, but for C identifiers containing underscores. */
#define F77_FUNC_(name,NAME) name ## __

/* Define if F77 and FC dummy `main' functions are identical. */
/* #undef FC_DUMMY_MAIN_EQ_F77 */

/* Define to 1 if you have the `access' function. */
#define HAVE_ACCESS 1

/* Define to 1 if you have the `acosh' function. */
#define HAVE_ACOSH 1

/* Define to 1 if you have `alloca', as a function or macro. */
#define HAVE_ALLOCA 1

/* Define to 1 if you have <alloca.h> and it should be used (not on Ultrix).
   */
/* #undef HAVE_ALLOCA_H */

/* Define if you have the Aqua headers and libraries, and want the Aqua GUI to
   be built. */
/* #undef HAVE_AQUA */

/* Define to 1 if you have the <argz.h> header file. */
/* #undef HAVE_ARGZ_H */

/* Define to 1 if you have the <arpa/inet.h> header file. */
/* #undef HAVE_ARPA_INET_H */

/* Define to 1 if you have the `asinh' function. */
#define HAVE_ASINH 1

/* Define to 1 if you have the `asprintf' function. */
/* #undef HAVE_ASPRINTF */

/* Define to 1 if you have the `atanh' function. */
#define HAVE_ATANH 1

/* Define if you have BSD networking headers and libraries. */
/* #undef HAVE_BSD_NETWORKING */

/* Define this if you have support for C99 complex types. */
#define HAVE_C99_COMPLEX 1

/* Define to 1 if you have the MacOS X function CFLocaleCopyCurrent in the
   CoreFoundation framework. */
/* #undef HAVE_CFLOCALECOPYCURRENT */

/* Define to 1 if you have the MacOS X function CFPreferencesCopyAppValue in
   the CoreFoundation framework. */
/* #undef HAVE_CFPREFERENCESCOPYAPPVALUE */

/* Define to 1 if you have the `chdir' function. */
#define HAVE_CHDIR 1

/* Defined if framework CoreFoundation is present */
/* #undef HAVE_COREFOUNDATION_FW */

/* Define if the GNU dcgettext() function is already present or preinstalled.
   */
/* #undef HAVE_DCGETTEXT */

/* Define to 1 if you have the declaration of `acosh', and to 0 if you don't.
   */
#define HAVE_DECL_ACOSH 1

/* Define to 1 if you have the declaration of `alloca', and to 0 if you don't.
   */
#define HAVE_DECL_ALLOCA 0

/* Define to 1 if you have the declaration of `asinh', and to 0 if you don't.
   */
#define HAVE_DECL_ASINH 1

/* Define to 1 if you have the declaration of `atanh', and to 0 if you don't.
   */
#define HAVE_DECL_ATANH 1

/* Define to 1 if you have the declaration of `feof_unlocked', and to 0 if you
   don't. */
#define HAVE_DECL_FEOF_UNLOCKED 0

/* Define to 1 if you have the declaration of `fgets_unlocked', and to 0 if
   you don't. */
#define HAVE_DECL_FGETS_UNLOCKED 0

/* Define to 1 if you have the declaration of `getc_unlocked', and to 0 if you
   don't. */
#define HAVE_DECL_GETC_UNLOCKED 0

/* Define to 1 if you have the declaration of `isfinite', and to 0 if you
   don't. */
#define HAVE_DECL_ISFINITE 1

/* Define to 1 if you have the declaration of `isnan', and to 0 if you don't.
   */
#define HAVE_DECL_ISNAN 1

/* Define to 1 if you have the declaration of `mkdtemp', and to 0 if you
   don't. */
#define HAVE_DECL_MKDTEMP 0

/* Define to 1 if you have the declaration of `putenv', and to 0 if you don't.
   */
#define HAVE_DECL_PUTENV 1

/* Define to 1 if you have the declaration of `realpath', and to 0 if you
   don't. */
#define HAVE_DECL_REALPATH 0

/* Define to 1 if you have the declaration of `siglongjmp', and to 0 if you
   don't. */
#define HAVE_DECL_SIGLONGJMP 1

/* Define to 1 if you have the declaration of `sigsetjmp', and to 0 if you
   don't. */
#define HAVE_DECL_SIGSETJMP 1

/* Define to 1 if you have the declaration of `SIZE_MAX', and to 0 if you
   don't. */
#define HAVE_DECL_SIZE_MAX 1

/* Define to 1 if you have the declaration of `snprintf', and to 0 if you
   don't. */
#define HAVE_DECL_SNPRINTF 1

/* Define to 1 if you have the declaration of `strdup', and to 0 if you don't.
   */
#define HAVE_DECL_STRDUP 1

/* Define to 1 if you have the declaration of `strncasecmp', and to 0 if you
   don't. */
#define HAVE_DECL_STRNCASECMP 1

/* Define to 1 if you have the declaration of `vasprintf', and to 0 if you
   don't. */
#define HAVE_DECL_VASPRINTF 0

/* Define to 1 if you have the declaration of `vsnprintf', and to 0 if you
   don't. */
#define HAVE_DECL_VSNPRINTF 1

/* Define to 1 if you have the declaration of `_snprintf', and to 0 if you
   don't. */
#define HAVE_DECL__SNPRINTF 1

/* Define to 1 if you have the declaration of `_snwprintf', and to 0 if you
   don't. */
#define HAVE_DECL__SNWPRINTF 1

/* Define to 1 if you have the <dirent.h> header file, and it defines `DIR'.
   */
#define HAVE_DIRENT_H 1

/* Define to 1 if you have the <dlfcn.h> header file. */
/* #undef HAVE_DLFCN_H */

/* Define to 1 if you have the <dl.h> header file. */
/* #undef HAVE_DL_H */

/* Define to 1 if you have the <elf.h> header file. */
/* #undef HAVE_ELF_H */

/* Define to 1 if you have the <errno.h> header file. */
#define HAVE_ERRNO_H 1

/* Define to 1 if you have the `execv' function. */
#define HAVE_EXECV 1

/* Define to 1 if you have the `expm1' function. */
#define HAVE_EXPM1 1

/* Define if your Fortran compiler appends an extra_underscore to external
   names containing an underscore. */
#define HAVE_F77_EXTRA_UNDERSCORE 1

/* Define if your Fortran compiler appends an underscore to external names. */
#define HAVE_F77_UNDERSCORE 1

/* Define to 1 if you have the `fcntl' function. */
/* #undef HAVE_FCNTL */

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the `fdopen' function. */
#define HAVE_FDOPEN 1

/* Define to 1 if you have the `finite' function. */
#define HAVE_FINITE 1

/* Define to 1 if you have the <floatingpoint.h> header file. */
/* #undef HAVE_FLOATINGPOINT_H */

/* Define if C's Rcomplex and Fortran's COMPLEX*16 can be interchanged, and
   can do arithmetic on the latter. */
#define HAVE_FORTRAN_DOUBLE_COMPLEX 1

/* Define to 1 if you have the <fpu_control.h> header file. */
/* #undef HAVE_FPU_CONTROL_H */

/* Define to 1 if fseeko (and presumably ftello) exists and is declared. */
/* #undef HAVE_FSEEKO */

/* Define to 1 if you have the `ftello' function. */
/* #undef HAVE_FTELLO */

/* Define to 1 if you have the `ftruncate' function. */
#define HAVE_FTRUNCATE 1

/* Define to 1 if you have the `fwprintf' function. */
#define HAVE_FWPRINTF 1

/* Define to 1 if you have the `getcwd' function. */
#define HAVE_GETCWD 1

/* Define to 1 if you have the `getegid' function. */
/* #undef HAVE_GETEGID */

/* Define to 1 if you have the `geteuid' function. */
/* #undef HAVE_GETEUID */

/* Define to 1 if you have the `getgid' function. */
/* #undef HAVE_GETGID */

/* Define to 1 if you have the `getgrgid' function. */
/* #undef HAVE_GETGRGID */

/* Define to 1 if you have the `getpagesize' function. */
#define HAVE_GETPAGESIZE 1

/* Define to 1 if you have the `getpwuid' function. */
/* #undef HAVE_GETPWUID */

/* Define to 1 if you have the `getrlimit' function. */
/* #undef HAVE_GETRLIMIT */

/* Define to 1 if you have the `getrusage' function. */
/* #undef HAVE_GETRUSAGE */

/* Define if the GNU gettext() function is already present or preinstalled. */
/* #undef HAVE_GETTEXT */

/* Define to 1 if you have the `gettimeofday' function. */
/* #undef HAVE_GETTIMEOFDAY */

/* Define to 1 if you have the `getuid' function. */
/* #undef HAVE_GETUID */

/* Define if you have the GNU C library version >= 2. This is needed to fix a
   problem with getting the prototype of strptime(). */
/* #undef HAVE_GLIBC2 */

/* Define to 1 if you have the `glob' function. */
/* #undef HAVE_GLOB */

/* Define to 1 if you have the <glob.h> header file. */
/* #undef HAVE_GLOB_H */

/* Define to 1 if you have the <grp.h> header file. */
/* #undef HAVE_GRP_H */

/* Define to 1 if you have the `history_truncate_file' function. */
/* #undef HAVE_HISTORY_TRUNCATE_FILE */

/* Define to 1 if you have the `hypot' function. */
#define HAVE_HYPOT 1

/* Define if you have the iconv() function. */
#define HAVE_ICONV 1

/* Define if you have the `iconvlist' function. */
#define HAVE_ICONVLIST 1

/* Define to 1 if you have the <iconv.h> header file. */
#define HAVE_ICONV_H 1

/* Define to 1 if you have the <ieee754.h> header file. */
/* #undef HAVE_IEEE754_H */

/* Define to 1 if you have the <ieeefp.h> header file. */
/* #undef HAVE_IEEEFP_H */

/* Define if you have support for ftp/http access. */
#define HAVE_INTERNET 1

/* Define if you have the 'intmax_t' type in <stdint.h> or <inttypes.h>. */
#define HAVE_INTMAX_T 1

/* Define to 1 if the system has the type `intptr_t'. */
#define HAVE_INTPTR_T 1

/* Define if <inttypes.h> exists and doesn't clash with <sys/types.h>. */
#define HAVE_INTTYPES_H 1

/* Define if <inttypes.h> exists, doesn't clash with <sys/types.h>, and
   declares uintmax_t. */
#define HAVE_INTTYPES_H_WITH_UINTMAX 1

/* Define to 1 if you have the `isblank' function. */
#define HAVE_ISBLANK 1

/* Define to 1 if you have the `isnan' function. */
#define HAVE_ISNAN 1

/* Define to 1 if you have the `iswblank' function. */
#define HAVE_ISWBLANK 1

/* Define to 1 if you have the `iswctype' function. */
#define HAVE_ISWCTYPE 1

/* Define if you have the JPEG headers and libraries. */
#define HAVE_JPEG 1

/* Define if KERN_USRSTACK sysctl is supported. */
/* #undef HAVE_KERN_USRSTACK */

/* Define if you have KeySym defined in X11. */
/* #undef HAVE_KEYSYM */

/* Define if you have <langinfo.h> and nl_langinfo(CODESET). */
/* #undef HAVE_LANGINFO_CODESET */

/* Define if your <locale.h> file defines LC_MESSAGES. */
/* #undef HAVE_LC_MESSAGES */

/* Define to 1 if you have the `cc_dynamic' library (-lcc_dynamic). */
/* #undef HAVE_LIBCC_DYNAMIC */

/* Define if __libc_stack_end is visible. */
/* #undef HAVE_LIBC_STACK_END */

/* Define to 1 if you have the `dl' library (-ldl). */
/* #undef HAVE_LIBDL */

/* Define to 1 if you have the `m' library (-lm). */
#define HAVE_LIBM 1

/* Define to 1 if you have the `ncurses' library (-lncurses). */
/* #undef HAVE_LIBNCURSES */

/* Define to 1 if you have the `readline' library (-lreadline). */
/* #undef HAVE_LIBREADLINE */

/* Define to 1 if you have the `termcap' library (-ltermcap). */
/* #undef HAVE_LIBTERMCAP */

/* Define to 1 if you have the `termlib' library (-ltermlib). */
/* #undef HAVE_LIBTERMLIB */

/* Define to 1 if you have the `tk' library (-ltk). */
/* #undef HAVE_LIBTK */

/* Define to 1 if you have the <limits.h> header file. */
#define HAVE_LIMITS_H 1

/* Define to 1 if you have the <locale.h> header file. */
#define HAVE_LOCALE_H 1

/* Define to 1 if you have the `log10' function. */
#define HAVE_LOG10 1

/* Define to 1 if you have the `log1p' function. */
#define HAVE_LOG1P 1

/* Define to 1 if you have the `log2' function. */
#define HAVE_LOG2 1

/* Define if you have the 'long double' type. */
#define HAVE_LONG_DOUBLE 1

/* Define if you have the 'long long' type. */
#define HAVE_LONG_LONG 1

/* Define to 1 if you have the <malloc.h> header file. */
#define HAVE_MALLOC_H 1

/* Define to 1 if you have the `matherr' function. */
/* #undef HAVE_MATHERR */

/* Define to 1 if you have the `mbrtowc' function. */
#define HAVE_MBRTOWC 1

/* Define to 1 if the system has the type `mbstate_t'. */
#define HAVE_MBSTATE_T 1

/* Define to 1 if you have the `mbstowcs' function. */
#define HAVE_MBSTOWCS 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `mempcpy' function. */
/* #undef HAVE_MEMPCPY */

/* Define to 1 if you have the `mkdtemp' function. */
/* #undef HAVE_MKDTEMP */

/* Define to 1 if you have the `mkfifo' function. */
/* #undef HAVE_MKFIFO */

/* Define to 1 if you have a working `mmap' system call. */
/* #undef HAVE_MMAP */

/* Define to 1 if you have the `munmap' function. */
/* #undef HAVE_MUNMAP */

/* Define to 1 if you have the <ndir.h> header file, and it defines `DIR'. */
/* #undef HAVE_NDIR_H */

/* Define to 1 if you have the <netdb.h> header file. */
/* #undef HAVE_NETDB_H */

/* Define to 1 if you have the <netinet/in.h> header file. */
/* #undef HAVE_NETINET_IN_H */

/* Define to 1 if you have the <nl_types.h> header file. */
/* #undef HAVE_NL_TYPES_H */

/* Define if module-loading does not need an underscore to be prepended to
   external names. */
#define HAVE_NO_SYMBOL_UNDERSCORE 1

/* Define if you have off_t, fseeko and ftello. */
#define HAVE_OFF_T 1

/* Define to 1 if you have the <pcre.h> header file. */
/* #undef HAVE_PCRE_H */

/* Define to 1 if you have the <pcre/pcre.h> header file. */
/* #undef HAVE_PCRE_PCRE_H */

/* Define if you have the PNG headers and libraries. */
#define HAVE_PNG 1

/* Define to 1 if you have the `popen' function. */
#define HAVE_POPEN 1

/* Define if your system time functions do not count leap seconds, as required
   by POSIX. */
#define HAVE_POSIX_LEAPSECONDS 1

/* Define if your printf() function supports format strings with positions. */
/* #undef HAVE_POSIX_PRINTF */

/* Define if you have POSIX.1 compatible sigsetjmp/siglongjmp. */
#define HAVE_POSIX_SETJMP 1

/* Define to 1 if you have the `putenv' function. */
#define HAVE_PUTENV 1

/* Define if putenv("FOO") can unset an environment variable */
/* #undef HAVE_PUTENV_UNSET */

/* Define if putenv("FOO=") can unset an environment variable */
#define HAVE_PUTENV_UNSET2 1

/* Define to 1 if you have the <pwd.h> header file. */
/* #undef HAVE_PWD_H */

/* Define to 1 if you have the <readline/history.h> header file. */
/* #undef HAVE_READLINE_HISTORY_H */

/* Define to 1 if you have the <readline/readline.h> header file. */
/* #undef HAVE_READLINE_READLINE_H */

/* Define to 1 if you have the `realpath' function. */
/* #undef HAVE_REALPATH */

/* Define to 1 if you have the `rint' function. */
#define HAVE_RINT 1

/* Define to 1 if you have the `rl_completion_matches' function. */
/* #undef HAVE_RL_COMPLETION_MATCHES */

/* Define to 1 if you have the `setenv' function. */
/* #undef HAVE_SETENV */

/* Define to 1 if you have the `setitimer' function. */
/* #undef HAVE_SETITIMER */

/* Define to 1 if you have the `setlocale' function. */
#define HAVE_SETLOCALE 1

/* Define to 1 if you have the `sigaction' function. */
/* #undef HAVE_SIGACTION */

/* Define to 1 if you have the `sigaltstack' function. */
/* #undef HAVE_SIGALTSTACK */

/* Define to 1 if you have the `sigemptyset' function. */
/* #undef HAVE_SIGEMPTYSET */

/* Define to 1 if you have the `snprintf' function. */
#define HAVE_SNPRINTF 1

/* Define if you have support for sockets. */
#define HAVE_SOCKETS 1

/* Define to 1 if the system has the type `stack_t'. */
/* #undef HAVE_STACK_T */

/* Define to 1 if you have the `stat' function. */
#define HAVE_STAT 1

/* Define to 1 if you have the <stdarg.h> header file. */
#define HAVE_STDARG_H 1

/* Define to 1 if you have the <stdbool.h> header file. */
#define HAVE_STDBOOL_H 1

/* Define to 1 if you have the <stddef.h> header file. */
#define HAVE_STDDEF_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define if <stdint.h> exists, doesn't clash with <sys/types.h>, and declares
   uintmax_t. */
#define HAVE_STDINT_H_WITH_UINTMAX 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `stpcpy' function. */
/* #undef HAVE_STPCPY */

/* Define to 1 if you have the `strcasecmp' function. */
#define HAVE_STRCASECMP 1

/* Define to 1 if you have the `strcoll' function. */
#define HAVE_STRCOLL 1

/* Define to 1 if you have the `strdup' function. */
#define HAVE_STRDUP 1

/* Define to 1 if you have the `strerror' function. */
#define HAVE_STRERROR 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strncasecmp' function. */
#define HAVE_STRNCASECMP 1

/* Define to 1 if you have the `strtoul' function. */
#define HAVE_STRTOUL 1

/* Define to 1 if you have the `symlink' function. */
/* #undef HAVE_SYMLINK */

/* Define to 1 if you have the `sysconf' function. */
/* #undef HAVE_SYSCONF */

/* Define to 1 if you have the `system' function. */
#define HAVE_SYSTEM 1

/* Define to 1 if you have the <sys/dir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_DIR_H */

/* Define to 1 if you have the <sys/ndir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_NDIR_H */

/* Define to 1 if you have the <sys/param.h> header file. */
#define HAVE_SYS_PARAM_H 1

/* Define to 1 if you have the <sys/resource.h> header file. */
/* #undef HAVE_SYS_RESOURCE_H */

/* Define to 1 if you have the <sys/select.h> header file. */
/* #undef HAVE_SYS_SELECT_H */

/* Define to 1 if you have the <sys/socket.h> header file. */
/* #undef HAVE_SYS_SOCKET_H */

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/times.h> header file. */
/* #undef HAVE_SYS_TIMES_H */

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <sys/utsname.h> header file. */
/* #undef HAVE_SYS_UTSNAME_H */

/* Define to 1 if you have <sys/wait.h> that is POSIX.1 compatible. */
/* #undef HAVE_SYS_WAIT_H */

/* Define if you have the Tcl/Tk headers and libraries and want Tcl/Tk support
   to be built. */
#define HAVE_TCLTK 1

/* Define to 1 if you have the `time' function. */
#define HAVE_TIME 1

/* Define to 1 if you have the `times' function. */
#define HAVE_TIMES 1

/* Define to 1 if you have the <time.h> header file. */
#define HAVE_TIME_H 1

/* Define to 1 if you have the `tsearch' function. */
/* #undef HAVE_TSEARCH */

/* Define if you have the 'uintmax_t' type in <stdint.h> or <inttypes.h>. */
#define HAVE_UINTMAX_T 1

/* Define to 1 if the system has the type `uintptr_t'. */
#define HAVE_UINTPTR_T 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the `unsetenv' function. */
/* #undef HAVE_UNSETENV */

/* Define if you have the 'unsigned long long' type. */
#define HAVE_UNSIGNED_LONG_LONG 1

/* Define to 1 if you have the `vasprintf' function. */
#define HAVE_VASPRINTF 1

/* Define to 1 if you have the `va_copy' function. */
#define HAVE_VA_COPY 1

/* Defined if framework vecLib is present */
/* #undef HAVE_VECLIB_FW */

/* Define to 1 if __attribute__((visibility())) is supported */
/* #undef HAVE_VISIBILITY_ATTRIBUTE */

/* Define to 1 if you have the `vsnprintf' function. */
#define HAVE_VSNPRINTF 1

/* Define to 1 if you have the <wchar.h> header file. */
#define HAVE_WCHAR_H 1

/* Define if you have the 'wchar_t' type. */
#define HAVE_WCHAR_T 1

/* Define to 1 if you have the `wcrtomb' function. */
#define HAVE_WCRTOMB 1

/* Define to 1 if you have the `wcscoll' function. */
#define HAVE_WCSCOLL 1

/* Define to 1 if you have the `wcsftime' function. */
#define HAVE_WCSFTIME 1

/* Define to 1 if you have the `wcslen' function. */
#define HAVE_WCSLEN 1

/* Define to 1 if you have the `wcstombs' function. */
#define HAVE_WCSTOMBS 1

/* Define to 1 if you have the `wctrans' function. */
#define HAVE_WCTRANS 1

/* Define to 1 if the system has the type `wctrans_t'. */
#define HAVE_WCTRANS_T 1

/* Define to 1 if you have the `wctype' function. */
#define HAVE_WCTYPE 1

/* Define to 1 if you have the <wctype.h> header file. */
#define HAVE_WCTYPE_H 1

/* Define if you have the 'wint_t' type. */
#define HAVE_WINT_T 1

/* Define if calloc(0) returns a null pointer. */
#define HAVE_WORKING_CALLOC 1

/* Define if finite() is correct for -Inf/NaN/Inf. */
#define HAVE_WORKING_FINITE 1

/* Define if your ftell works correctly on files opened for append. */
/* #undef HAVE_WORKING_FTELL */

/* Define if isfinite() is correct for -Inf/NaN/Inf. */
#define HAVE_WORKING_ISFINITE 1

/* Define if log() is correct for 0/-1. */
#define HAVE_WORKING_LOG 1

/* Define if log1p() exists and is accurate enough. */
#define HAVE_WORKING_LOG1P 1

/* Define if sigaction() is complete enough for R's usage */
/* #undef HAVE_WORKING_SIGACTION */

/* Define if you have the X11 headers and libraries, and want the X11 GUI to
   be built. */
/* #undef HAVE_X11 */

/* Define if you have the X11/Xmu headers and libraries. */
/* #undef HAVE_X11_Xmu */

/* Define to 1 if you have the `__argz_count' function. */
/* #undef HAVE___ARGZ_COUNT */

/* Define to 1 if you have the `__argz_next' function. */
/* #undef HAVE___ARGZ_NEXT */

/* Define to 1 if you have the `__argz_stringify' function. */
/* #undef HAVE___ARGZ_STRINGIFY */

/* Define to 1 if you have the `__fsetlocking' function. */
/* #undef HAVE___FSETLOCKING */

/* Define to 1 if you have the `__va_copy' function. */
#define HAVE___VA_COPY 1

/* Define as const if the declaration of iconv() needs const. */
/* #undef ICONV_CONST */

/* Define if `iconv' accepts "UTF-8", "latin1" and "UCS-*". */
#define ICONV_LATIN1 1

/* Define if you have IEEE 754 floating point arithmetic. */
#define IEEE_754 1

/* Define if integer division by zero raises signal SIGFPE. */
#define INTDIV0_RAISES_SIGFPE 1

/* Define if you have 32 bit ints. */
#define INT_32_BITS 1

/* Define if your system needs __setfpucw() to control FPU rounding. This was
   used to control floating point precision, rounding and floating point
   exceptions on older Linux systems. As of GLIBC 2.1 this function is not
   used anymore. */
/* #undef NEED___SETFPUCW */

/* Define to disable Valgrind instrumentation */
#define NVALGRIND 1

/* Define if using GNU-style Objective C runtime. */
/* #undef OBJC_GNU_RUNTIME */

/* Define if using NeXT/Apple-style Objective C runtime. */
/* #undef OBJC_NEXT_RUNTIME */

/* Name of package */
#define PACKAGE "R"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "r-bugs@R-project.org"

/* Define to the full name of this package. */
#define PACKAGE_NAME "R"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "R 2.5.1"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "R"

/* Define to the version of this package. */
#define PACKAGE_VERSION "2.5.1"

/* Define if <inttypes.h> exists and defines unusable PRI* macros. */
/* #undef PRI_MACROS_BROKEN */

/* Define as the return type of signal handlers (`int' or `void'). */
#define RETSIGTYPE void

/* Define this to use architecture-dependent subdirectories of this name. */
#define R_ARCH ""

/* Define this to be the name of the CPU of your system. */
#define R_CPU "i386"

/* Define as `inline', or `__inline__' or `__inline' if that's what the C
   compiler calls it, or to nothing if it is not supported. */
#define R_INLINE inline

/* Define this to enable memory profiling. */
/* #undef R_MEMORY_PROFILING */

/* Define this to be the name of the OS of your system. */
#define R_OS "mingw32"

/* Define this to be the canonical name (cpu-vendor-os) of your system. */
#define R_PLATFORM "i386-pc-mingw32"

/* Define this to be printing command on your system. */
#define R_PRINTCMD ""

/* Define this to enable R-level profiling. */
#define R_PROFILING 1

/* Type for socket lengths: socklen_t, sock_t, int? */
#define R_SOCKLEN_T int

/* Define this to be the name of the vendor of your system. */
#define R_VENDOR "pc"

/* Define this to be the extension used for shared libraries on your system.
   */
#define SHLIB_EXT ".dll"

/* The size of `double', as computed by sizeof. */
#define SIZEOF_DOUBLE 8

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 4

/* The size of `long double', as computed by sizeof. */
#define SIZEOF_LONG_DOUBLE 12

/* The size of `long long', as computed by sizeof. */
#define SIZEOF_LONG_LONG 8

/* Define as the maximum value of type 'size_t', if the system doesn't define
   it. */
/* #undef SIZE_MAX */

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at runtime.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown */
/* #undef STACK_DIRECTION */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define if you provide support for the libxml ftp/http functions. */
#define SUPPORT_LIBXML 1

/* Define this to enable support for MBCS locales. */
#define SUPPORT_MBCS 1

/* Define this to enable support for UTF-8 locales. */
/* #undef SUPPORT_UTF8 */

/* Define to enable provoking compile errors on write barrier violation. */
/* #undef TESTING_WRITE_BARRIER */

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#define TIME_WITH_SYS_TIME 1

/* Define if your rint() is broken on your system. Apparently needed on HPUX.
   */
/* #undef USE_BUILTIN_RINT */

/* Define according to your operating system type. */
/* #undef Unix */

/* Define as 1 or 2 to specify levels of Valgrind instrumentation */
#define VALGRIND_LEVEL 0

/* Version number of package */
#define VERSION "2.5.1"

/* Define to 1 if your processor stores words with the most significant byte
   first (like Motorola and SPARC, unlike Intel and VAX). */
/* #undef WORDS_BIGENDIAN */

/* Define according to your operating system type. */
#define Win32 1

/* Define to 1 if the X Window System is missing or not being used. */
#define X_DISPLAY_MISSING 1

/* Number of bits in a file offset, on hosts where this is settable. */
/* #undef _FILE_OFFSET_BITS */

/* Define to 1 to make fseeko visible on some hosts (e.g. glibc 2.2). */
/* #undef _LARGEFILE_SOURCE */

/* Define for large files, on AIX-style hosts. */
/* #undef _LARGE_FILES */

/* Define to 'long' if <sys/types.h> does not define. Apparently necessary to
   fix a GCC bug on AIX? */
#define blkcnt_t long

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif

/* Define to `long int' if <sys/types.h> does not define. */
/* #undef off_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef pid_t */

/* Define as the type of the result of subtracting two pointers, if the system
   doesn't define it. */
/* #undef ptrdiff_t */

/* Define to empty if the C compiler doesn't support this keyword. */
/* #undef signed */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */

/* Define to unsigned long or unsigned long long if <stdint.h> and
   <inttypes.h> don't define. */
/* #undef uintmax_t */


#endif /* not R_CONFIG_H */
