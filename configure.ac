### configure.ac					-*- Autoconf -*-
###
### Process this file with autoconf to produce a configure script.
###
### Copyright (C) 1998-2002 R Core Team
###
### This file is part of R.
###
### R is free software; you can redistribute it and/or modify it under
### the terms of the GNU General Public License as published by the Free
### Software Foundation; either version 2 of the License, or (at your
### option) any later version.
###
### R is distributed in the hope that it will be useful, but WITHOUT ANY
### WARRANTY; without even the implied warranty of MERCHANTABILITY or
### FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
### License for more details.
###
### You should have received a copy of the GNU General Public License
### along with R; if not, you can obtain it via the World Wide Web at
### 'http://www.gnu.org/copyleft/gpl.html', or by writing to the Free
### Software Foundation, 59 Temple Place -- Suite 330, Boston, MA
### 02111-3307, USA.

AC_PREREQ(2.50)

## We want to get the version number from file 'VERSION' (rather than
## specifying the version info in 'configure.ac'.  Hence, we need a bit
## of M4 magic.  Note that M4 esyscmd has a trailing newline because the
## shell command output has one, hence the patsubst() trick.
m4_define([R_VERSION],
          patsubst(m4_esyscmd([cat VERSION]), [\([0-9.]*\)\(\w\|\W\)*], [\1]))
AC_INIT([R], R_VERSION, [r-bugs@R-project.org])
AC_CONFIG_SRCDIR([src/include/Defn.h])
AC_CONFIG_AUX_DIR([tools])

### * Information on the package.

## Automake initialization.
AM_INIT_AUTOMAKE(AC_PACKAGE_NAME, AC_PACKAGE_VERSION)

## Autoheader initialization.
AH_TOP([#ifndef _CONFIG_H
#define _CONFIG_H])
AH_BOTTOM([
#ifdef Win32
# define snprintf  _snprintf
# define vsnprintf  _vsnprintf
# define isnan(a) _isnan((a))
# define finite(a) _finite((a))
/* 27/03/2000 win32-api needs this for ANSI compliance */
# define NONAMELESSUNION
#endif /* Win32 */

#endif /* not _CONFIG_H */])

### ** Platform.

AC_CANONICAL_HOST
AC_DEFINE_UNQUOTED(R_PLATFORM, "${host}",
[Define this to be the canonical name (cpu-vendor-os) of your system.])
AC_DEFINE_UNQUOTED(R_CPU, "${host_cpu}",
[Define this to be the name of the CPU of your system.])
AC_DEFINE_UNQUOTED(R_VENDOR, "${host_vendor}",
[Define this to be the name of the vendor of your system.])
AC_DEFINE_UNQUOTED(R_OS, "${host_os}",
[Define this to be the name of the OS of your system.])

R_PLATFORM="${host}"
AC_SUBST(R_PLATFORM)

case "${host_os}" in
  macos*)
    AC_DEFINE(Macintosh, 1,
              [Define according to your operating system type.])
    ;;
  cygwin*|mingw*|windows*|winnt)
    AC_DEFINE(Win32, 1,
              [Define according to your operating system type.])
  cat >>confdefs.h <<\EOF
#define snprintf  _snprintf
#define vsnprintf  _vsnprintf
#define isnan _isnan
#define finite _finite
EOF
    ;;
  *)
    AC_DEFINE(Unix, 1,
              [Define according to your operating system type.])
   ;;
esac

### ** Defaults.

cfile="${srcdir}/config.site"
if test -r ${cfile}; then
  echo "loading site script ${cfile}"
  . ${cfile}
fi
cfile="${HOME}/.Rconf"
if test -r ${cfile}; then
  echo "loading user script ${cfile}"
  . ${cfile}
fi
cfile="./config.site"
if test -r ${cfile}; then
  echo "loading build specific script ${cfile}"
  . ${cfile}
fi

## We provide these defaults so that headers and libraries in
## '/usr/local' are found (by the native tools, mostly).
: ${CPPFLAGS="-I/usr/local/include"}
: ${LDFLAGS="-L/usr/local/lib"}

### ** Handle arguments to configure.

config_opts="${ac_configure_args}"
AC_SUBST(config_opts)

### ** Optional features.

## <FIXME>
## Remove in 1.6.
for arg in ${ac_configure_args}; do
  case "${arg}" in
    --with-g77*)
      warn_arg_with_g77="option '--with-g77' is deprecated.
Use variable F77 to specify a FORTRAN 77 compiler if necessary."
      AC_MSG_WARN([${warn_arg_with_g77}])
      ;;
    --with-f77*)
      warn_arg_with_f77="option '--with-f77' is deprecated.
Use variable F77 to specify a FORTRAN 77 compiler if necessary."
      AC_MSG_WARN([${warn_arg_with_f77}])
      ;;
    --with-f2c*)
      warn_arg_with_f2c="option '--with-f2c' is deprecated.
Use variable F2C to specify the FORTRAN-to-C converter if necessary."
      AC_MSG_WARN([${warn_arg_with_f2c}])
      ;;
  esac
done
## </FIXME>

## Allow the user to specify support for R profiling.
AC_ARG_ENABLE([R-profiling],
[AC_HELP_STRING([--enable-R-profiling],
                [attempt to compile support for Rprof() @<:@yes@:>@])],
[if test "${enableval}" = no; then
  want_R_profiling=no
elif test "${enableval}" = yes; then
  want_R_profiling=yes
else
  want_R_profiling=yes
fi],
[want_R_profiling=yes])

## Allow the user to specify building R shared libraries.
AC_ARG_ENABLE([R-shlib],
[AC_HELP_STRING([--enable-R-shlib],
                [build R as a shared library @<:@no@:>@])],
[want_R_shlib="${enableval}"],
[want_R_shlib=no])
AM_CONDITIONAL(WANT_R_SHLIB, [test "x${want_R_shlib}" = xyes])
## <FIXME>
## Should we add a check whether the user disabled shlibs in general?
## </FIXME>
if test "${want_R_shlib}" = yes; then
  LIBR="-L\$(R_HOME)/bin -lR"
else
  LIBR=
fi
AC_SUBST(LIBR)

## Enable maintainer-specific portions of Makefiles.
AC_ARG_ENABLE([maintainer-mode],
[AC_HELP_STRING([--enable-maintainer-mode],
                [enable make rules and dependencies not useful (and
                 maybe confusing) to the casual installer @<:@no@:>@])],
[use_maintainer_mode="${enableval}"],
[use_maintainer_mode=no])
AM_CONDITIONAL(MAINTAINER_MODE, [test "x${use_maintainer_mode}" = xyes])

## Enable testing the write barrier.
AC_ARG_ENABLE([strict-barrier],
[AC_HELP_STRING([--enable-strict-barrier],
                [provoke compile error on write barrier violation
                 @<:@no@:>@])], 
[if test "${enableval}" = yes; then
  AC_DEFINE(TESTING_WRITE_BARRIER, 1,
            [Define to enable provoking compile errors on write barrier
             violation.])
fi],
[])

### ** Optional packages.

## BLAS.
AC_ARG_WITH([blas],
[AC_HELP_STRING([--with-blas],
                [use BLAS library, or specify it @<:@yes@:>@])],
[R_ARG_USE(blas)],
[use_blas=yes])

## Readline.
AC_ARG_WITH([readline],
[AC_HELP_STRING([--with-readline],
                [use readline library (if available) @<:@yes@:>@])],
[R_ARG_USE(readline)],
[use_readline=yes])

## GNOME.
AC_ARG_WITH([gnome],
[AC_HELP_STRING([--with-gnome],
                [use GNOME, or specify its prefix @<:@no@:>@])],
[if test "${withval}" = no; then
  want_gnome=no
elif test "${withval}" = yes; then
  want_gnome=yes
else
  want_gnome=yes
  CPPFLAGS="${CPPFLAGS} -I${withval}/include"
  LDFLAGS="${LDFLAGS} -L${withval}/lib"
  gnome_prefix="${withval}/lib"
fi],
[want_gnome=no])

## Tcl/Tk.
AC_ARG_WITH([tcltk],
[AC_HELP_STRING([--with-tcltk],
                [use Tcl/Tk, or specify its library dir @<:@yes@:>@])],
[if test "${withval}" = no; then
  want_tcltk=no
elif test "${withval}" = yes; then
  want_tcltk=yes
else
  want_tcltk=yes
  LDFLAGS="${LDFLAGS} -L${withval}"
  tcltk_prefix="${withval}"
fi],
[want_tcltk=yes])
AC_ARG_WITH([tcl-config],
[AC_HELP_STRING([--with-tcl-config=TCL_CONFIG],
                [specify location of tclConfig.sh @<:@@:>@])],
[TCL_CONFIG="${withval}"],
[TCL_CONFIG=""])
AC_ARG_WITH([tk-config],
[AC_HELP_STRING([--with-tk-config=TK_CONFIG],
                [specify location of tkConfig.sh @<:@@:>@])],
[TK_CONFIG="${withval}"],
[TK_CONFIG=""])

## <FIXME>
## Completely disable using libtool for building shlibs until libtool
## fully supports Fortran and C++.
## AC_ARG_WITH([libtool],
## [AC_HELP_STRING([--with-libtool],
##                 [use libtool for building shared libraries @<:@yes@:>@])],
## [use_libtool="${withval}"],
## [use_libtool=yes])
## </FIXME>
AM_CONDITIONAL(USE_LIBTOOL, [test "x${use_libtool}" = xyes])

### ** Precious variables.

AC_ARG_VAR([R_PRINTCMD],
           [command used to spool PostScript files to the printer])
AC_ARG_VAR([R_PAPERSIZE],
           [paper size for the local (PostScript) printer])
AC_ARG_VAR([R_BATCHSAVE],
           [default behavior of R if it encounters EOF in batch mode])
AC_ARG_VAR([MAIN_CFLAGS],
           [additional CFLAGS used when compiling the main binary])
AC_ARG_VAR([SHLIB_CFLAGS],
           [additional CFLAGS used when building shared libraries])
AC_ARG_VAR([MAIN_FFLAGS],
           [additional FFLAGS used when compiling the main binary])
AC_ARG_VAR([SHLIB_FFLAGS],
           [additional FFLAGS used when building shared libraries])
AC_ARG_VAR([MAIN_LD],
           [command used to link the main binary])
AC_ARG_VAR([MAIN_LDFLAGS],
           [flags which are necessary for loading a main program which
            will load shared library modules (DLLs) at runtime])
AC_ARG_VAR([CPICFLAGS],
           [special flags for compiling C code to be turned into a
            shared library.])
AC_ARG_VAR([FPICFLAGS],
           [special flags for compiling FORTRAN code to be turned into a
            shared library.])
AC_ARG_VAR([SHLIB_LD],
           [command for linking shared libraries which contain object
            files from a C or FORTRAN compiler only])
AC_ARG_VAR([SHLIB_LDFLAGS],
           [special flags used by SHLIB_LD])
AC_ARG_VAR([CXXPICFLAGS],
           [special flags for compiling C++ code to be turned into a
            shared library])
AC_ARG_VAR([SHLIB_CXXLD],
           [command for linking shared libraries which contain object
            files from a C++ compiler])
AC_ARG_VAR([SHLIB_CXXLDFLAGS],
           [special flags used by SHLIB_CXXLD])
AC_ARG_VAR([TCLTK_LIBS],
           [flags needed for linking against the Tcl and Tk libraries])
AC_ARG_VAR([TCLTK_CPPFLAGS],
           [flags needed for finding the tcl.h and tk.h headers])
AC_ARG_VAR([MAKE], [Make command])
AC_ARG_VAR([F2C], [FORTRAN-to-C converter command])

### ** Check whether we build in srcdir.

AC_PATH_PROG(GETWD, pwd, pwd)
## <FIXME>
## Autoconf 2.53 or better has abs_top_srcdir and abs_top_builddir.
AC_MSG_CHECKING([whether builddir is srcdir])
if test "`cd ${srcdir} && ${GETWD}`" = "`${GETWD}`"; then
  BUILDDIR_IS_SRCDIR=yes
else
  BUILDDIR_IS_SRCDIR=no
fi
## </FIXME>
AC_SUBST(BUILDDIR_IS_SRCDIR)
AC_MSG_RESULT([${BUILDDIR_IS_SRCDIR}])

### * Checks for programs.

AC_PROG_AWK
AC_PROG_LN_S
AC_PROG_RANLIB
AC_PROG_YACC
R_PROG_AR
R_PROG_INSTALL
AC_PATH_PROG(JAVAC, javac)
## Make
: ${MAKE=make}
AC_SUBST(MAKE)
## Pager
R_PROG_PAGER
## Perl
R_PROG_PERL
## Tar
: ${TAR=tar}
AC_SUBST(TAR)
## TeXMF stuff
R_PROG_TEXMF
## Unzip && zip
AC_PATH_PROGS(R_UNZIPCMD, [${UNZIP} unzip], "")
AC_PATH_PROGS(R_ZIPCMD, [${ZIP} zip], "")
## Libtool
AM_DISABLE_STATIC
AM_PROG_LIBTOOL
AC_SUBST(LIBTOOL_DEPS)
## Makeinfo
R_PROG_MAKEINFO

AC_PROG_CC
AC_PROG_CC_STDC
AC_PROG_GCC_TRADITIONAL
AC_PROG_CPP
R_PROG_F77_OR_F2C
AC_PROG_CXX
AC_PROG_CXXCPP

### * Checks for libraries.

## Set up LD_LIBRARY_PATH or equivalent.
## <FIXME>
## What is this doing *HERE*?
## Should be needed for tests using AC_TRY_RUN()?
## Make sure that non-standard directories specified via '-L' are really
## searched in the tests.
## Note: earlier versions used LIBS rather than LDFLAGS.
for arg in ${LDFLAGS}; do
  case "${arg}" in
    -L*)
      lib=`echo ${arg} | sed "s/^-L//"`
      if test -z "${R_LD_LIBRARY_PATH}"; then
        R_LD_LIBRARY_PATH="${lib}"
      else
        R_LD_LIBRARY_PATH="${R_LD_LIBRARY_PATH}${PATH_SEPARATOR}${lib}"
      fi
      ;;
  esac
done
## </FIXME>

## Record name of environment variable which tells the dynamic linker
## where to find shlibs (typically, 'LD_LIBRARY_PATH').
ltout=`./libtool --config | grep '^shlibpath_var='`
eval "${ltout}"
AC_SUBST(shlibpath_var)
## Export LD_LIBRARY_PATH or equivalent.
if eval "test -z \"\${${shlibpath_var}}\""; then
  eval "${shlibpath_var}=\"${R_LD_LIBRARY_PATH}\""
else
  eval "${shlibpath_var}=\"${R_LD_LIBRARY_PATH}${PATH_SEPARATOR}\${${shlibpath_var}}\""
fi
eval "export ${shlibpath_var}"

## Yes, we already need this at configure time ...
case "${host_os}" in
  aix*)
    LIBS="-lc ${LIBS}"
    ;;
esac

## <NOTE>
## This actually comes from libtool.m4.
AC_CHECK_LIBM
AC_SUBST(LIBM)
## </NOTE>
## AC_CHECK_LIBM computes LIBM but does not add to LIBS, hence we do
## the following as well.
AC_CHECK_LIB(m, sin)
AC_CHECK_LIB(ncurses, main, [],
             AC_CHECK_LIB(termcap, main, [],
                          AC_CHECK_LIB(termlib, main)))
AC_CHECK_LIB(dl, dlopen)

## Readline.
if test "${use_readline}" = yes; then
  AC_CHECK_LIB(readline, rl_callback_read_char)
  use_readline="${ac_cv_lib_readline_rl_callback_read_char}"
fi

### * Checks for header files.

AC_HEADER_STDC
AC_HEADER_TIME
AC_HEADER_DIRENT
AC_HEADER_SYS_WAIT
## <NOTE>
## Some of these are also checked for when Autoconf computes the default
## includes.
AC_CHECK_HEADERS(arpa/inet.h dl.h dlfcn.h elf.h fcntl.h floatingpoint.h \
  fpu_control.h grp.h ieee754.h ieeefp.h locale.h netdb.h netinet/in.h \
  pwd.h readline/history.h readline/readline.h rpc/rpc.h strings.h \
  sys/param.h sys/select.h sys/socket.h sys/stat.h sys/time.h \
  sys/times.h sys/utsname.h unistd.h)
## </NOTE>
## <NOTE>
## These are ANSI C headers but some C code (written to work also
## without assuming ANSI C) may need the corresponding conditionals.
AC_CHECK_HEADERS(errno.h stdarg.h string.h)
## </NOTE>
## <NOTE>
## AC_CHECK_HEADERS(libintl.h)
## src/main/regex.c uses HAVE_LIBINTL_H.
## But then we want full gettext support and AM_GNU_GETTEXT.
## </NOTE>
case "${host_os}" in
  sunos4*)
    AC_DEFINE(SunOS4, 1,
              [Define if your system is SunOS4, which is famous for
               broken header files.])
  ;;
esac
R_HEADER_SETJMP
R_HEADER_GLIBC2

### * Checks for types.

AC_TYPE_SIGNAL
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_CHECK_TYPE(blkcnt_t, long)
AH_TEMPLATE([blkcnt_t],
            [Define to 'long' if <sys/types.h> does not define.
             Apparently necessary to fix a GCC bug on AIX?])
R_TYPE_SOCKLEN

### * Checks for compiler characteristics.

### ** Generic tests for the C, FORTRAN 77 and C++ compilers.

### *** C compiler.

AC_C_BIGENDIAN
AC_C_CONST
if test "${cross_compiling}" = yes; then
  warn_xcompile_sizeof_int="assuming C ints are 4 byte on ${host}"
  AC_MSG_WARN([${warn_xcompile_sizeof_int}])
fi
AC_CHECK_SIZEOF(int, 4)
if test "${ac_cv_sizeof_int}" = 4; then
  AC_DEFINE(INT_32_BITS, 1, [Define if you have 32 bit ints.])
fi
if test "${cross_compiling}" = yes; then
  warn_xcompile_sizeof_long="assuming C longs are 4 byte on ${host}"
  AC_MSG_WARN([${warn_xcompile_sizeof_long}])
fi
AC_CHECK_SIZEOF(long, 4)
AC_CHECK_SIZEOF(long long, 0)
AC_CHECK_SIZEOF(long double, 0)

R_PROG_CC_MAKEFRAG

### *** FORTRAN 77 compiler.

if test -n "${F77}"; then
  R_PROG_F77_FLIBS
  R_PROG_F77_APPEND_UNDERSCORE
  R_PROG_F77_CC_COMPAT
  R_PROG_F77_CC_COMPAT_COMPLEX
  R_PROG_F77_MAKEFRAG  
else
  R_PROG_F2C_FLIBS
  AC_DEFINE(HAVE_F77_UNDERSCORE, 1)
  AC_DEFINE(HAVE_DOUBLE_COMPLEX, 1)
  R_PROG_F2C_MAKEFRAG
  F77="\$(SHELL) \$(R_HOME)/bin/f77"
fi

AC_SUBST(F2C)
AM_CONDITIONAL(COMPILE_DOUBLE_COMPLEX,
               [test "x${HAVE_DOUBLE_COMPLEX}" != x])

### *** C++ compiler.

R_PROG_CXX_MAKEFRAG

### ** Platform-specific overrides for the C, FORTRAN 77 and C++ compilers.

case "${host_cpu}" in
  i*86)
    R_PROG_CC_FLAG_D__NO_MATH_INLINES
    R_PROG_CC_FLAG([-mieee-fp],
                   [R_XTRA_CFLAGS="${R_XTRA_CFLAGS} -mieee-fp"])
    R_PROG_CXX_FLAG([-mieee-fp],
                    [R_XTRA_CXXFLAGS="${R_XTRA_CXXFLAGS} -mieee-fp"])
    ;;
  alpha*)
    ## <NOTE>
    ## * ANSI C
    ## The native C compiler needs @option{-std1}: AC_PROG_CC_STDC
    ## passes all ANSI C tests with no extra flags but then __STDC__ is
    ## undefined which breaks compilation of the connections code.
    ## * IEEE math
    ## We really need to use @option{-ieee_with_inexact} (called
    ## @option{-mieee-with-inexact} for GCC) for the C compiler:
    ## @option{-ieee} (or @option{-mieee}) are not enough.
    ## According to <Albrecht.Gebhardt@uni-klu.ac.at> and Luke Tierney
    ## <luke@stat.umn.edu>, @option{-fpe3} is what we want for the
    ## native FORTRAN 77 compiler: seems that not all versions map
    ## @option{-ieee} to @option{-fpe3}.
    ## What about the C++ compilers?
    if test "${GCC}" = yes; then
      R_PROG_CC_FLAG([-mieee-with-inexact],
                     [R_XTRA_CFLAGS="${R_XTRA_CFLAGS} -mieee-with-inexact"])
    else
      R_PROG_CC_FLAG([-std1],
                     [R_XTRA_CFLAGS="${R_XTRA_CFLAGS} -std1"])
      R_PROG_CC_FLAG([-ieee_with_inexact],
                     [R_XTRA_CFLAGS="${R_XTRA_CFLAGS} -ieee_with_inexact"])
    fi
    if test "${G77}" = yes; then
      R_PROG_F77_FLAG([-mieee],
                      [R_XTRA_FFLAGS="${R_XTRA_FFLAGS} -mieee"])
    else
      R_PROG_F77_FLAG([-fpe3],
                      [R_XTRA_FFLAGS="${R_XTRA_FFLAGS} -fpe3"])
    fi
    if test "${GXX}" = yes; then
      R_PROG_CXX_FLAG([-mieee],
                      [R_XTRA_CXXFLAGS="${R_XTRA_CXXFLAGS} -mieee"])
    else
      R_PROG_CXX_FLAG([-ieee],
                      [R_XTRA_CXXFLAGS="${R_XTRA_CXXFLAGS} -ieee"])
    fi
    ## </NOTE>
    ;;
esac

AH_TEMPLATE([HAVE_NO_SYMBOL_UNDERSCORE],
            [Define if module-loading does not need an underscore to
             be prepended to external names.])

case "${host_os}" in
  aix*)
    AC_DEFINE(HAVE_NO_SYMBOL_UNDERSCORE)
    if test "${GCC}" = yes; then
      R_PROG_CC_FLAG([-mno-fp-in-toc],
                     [R_XTRA_CFLAGS="${R_XTRA_CFLAGS} -mno-fp-in-toc"])
    fi
    R_XTRA_LIBS="${R_XTRA_LIBS} /lib/crt0.o"
    ;;
  cygwin*|mingw*|windows*|winnt)
    AC_DEFINE(HAVE_NO_SYMBOL_UNDERSCORE)
    ;;
  darwin*)
    ## Need '-no-cpp-precomp' say J de Leeuw <deleeuw@stat.ucla.edu> and
    ## http://fink.sourceforge.net/doc/porting/basics.php.
    R_XTRA_CPPFLAGS="${R_XTRA_CPPFLAGS} -no-cpp-precomp"
    ;;
  hpux*)
    AC_DEFINE(HAVE_NO_SYMBOL_UNDERSCORE)
    AC_DEFINE(USE_BUILTIN_RINT, 1,
              [Define if your rint() is broken on your system.
               Apparently needed on HPUX.])
    case "${CC}" in
      cc|c89)
	## Luke Tierney says we also need '-Wp,-H16000' which tells the
	## pre-processor to increase the size of an internal table.  It
	## seems that src/modules/vfonts/g_her_glyph.c contains a line
	## that is too long for the pre-processor without this flag.
	R_XTRA_CPPFLAGS="${R_XTRA_CPPFLAGS} -Wp,-H16000"
	;;
    esac
    AC_CHECK_LIB(dld, shl_load, [R_XTRA_LIBS="-ldld ${R_XTRA_LIBS}"])
    ;;
  irix*)
    R_C_OPTIEEE
    ## <NOTE>
    ## We really should test explictly whether the FORTRAN and C++
    ## compilers *need* '-OPT:IEEE_NaN_inf=ON'.  Currently, all we do is
    ## check whether the non-GNU tools *accept* the flag.
    if test "${G77}" != yes; then
      R_PROG_F77_FLAG([-OPT:IEEE_NaN_inf=ON],
                      [R_XTRA_FFLAGS="${R_XTRA_FFLAGS} -OPT:IEEE_NaN_inf=ON"])
    fi
    if test "${GXX}" != yes; then
      R_PROG_CXX_FLAG([-OPT:IEEE_NaN_inf=ON],
                      [R_XTRA_CXXFLAGS="${R_XTRA_CXXFLAGS} -OPT:IEEE_NaN_inf=ON"])
    fi
    ## </NOTE>
    ;;
  osf*)
    AC_DEFINE(HAVE_NO_SYMBOL_UNDERSCORE)
    ;;  
esac

AC_SUBST(CFLAGS)
AC_SUBST(MAIN_CFLAGS)
AC_SUBST(SHLIB_CFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(SHLIB_CXXFLAGS)
AC_SUBST(FFLAGS)
AC_SUBST(MAIN_FFLAGS)
AC_SUBST(SHLIB_FFLAGS)
AC_SUBST(F2CFLAGS)
AC_SUBST(R_XTRA_CFLAGS)
AC_SUBST(R_XTRA_CPPFLAGS)
AC_SUBST(R_XTRA_CXXFLAGS)
AC_SUBST(R_XTRA_FFLAGS)
AC_SUBST(R_XTRA_LIBS)

### ** DLL stuff.

## We need to determine the following:
##
## MAIN_LD, MAINLD_FLAGS
##   command and flags for loading the main binary so that it will load
##   shared libraries (DLLs) at runtime, also for profiling.
## CPICFLAGS, CXXPICFLAGS, FPICFLAGS
##   flags for compiling C, C++, and Fortran library code.
## SHLIB_LD, SHLIB_LDFLAGS
##   command and flags for creating DLLs (which contain object files
##   from a C or Fortran compiler).
## SHLIB_CXXLD, SHLIB_CXXLDFLAGS
##   command and flags for creating DLLs which contain object files from
##   a C++ compiler.  According to autoconf, the C++ compiler/linker
##   must be used for linking in this case (since special C++-ish things
##   need to happen at link time like calling global constructors,
##   instantiating templates, enabling exception support, etc.).
##
## The procedure is as follows.
##   
## * We use applicable values from imake in case its CC is ours.
## * Irrespective of that, we think we know what to do with GNU tools
##   (GNU C, Fortran, and C++ compilers).
## * Then, use platform specific overrides.
## * As a final safeguard, values from the environment (as specified in
##   one of the configuration files or at the configure command line)
##   override anything we figure out in the case of compiler flags; for
##   linker flags (*LDFLAGS), environment settings override our results
##   if the corresponding *LD variable was set, and add otherwise.
##
## NOTE: We do not provide defaults for the *LDFLAGS, taking a defensive
## approach.  In case we cannot figure out {MAIN,SHLIB}_LDFLAGS and the
## user did not provide defaults, an error results.  A warning is given
## if nothing was obtained for SHLIB_CXXLDFLAGS.
##
## Note also that some systems (notably AIX) do not allow for unresolved
## symbols at link time.  For such systems, we link against -lm (in case
## it exists) when building a shlib module via SHLIB_LIBADD.

main_ld="${CC}"
shlib_ld="${CC}"
shlib_cxxld="${CXX}"
SHLIB_EXT=".so"
SHLIB_LIBADD=
use_exportfile=no

## Step 1.  Ask imake.
## <NOTE>
## Earlier versions had fpicflags=${cpicflags}.  As this really amounts
## to hoping rather than knowing, we no longer do this.
## </NOTE>
r_xtra_path="${PATH}"
for dir in /usr/bin/X11 /usr/X11R6/bin /usr/openwin/bin; do
  r_xtra_path="${r_xtra_path}${PATH_SEPARATOR}${dir}"
done
AC_PATH_PROG(XMKMF, xmkmf, [], [${r_xtra_path}])
if test -n "${XMKMF}"; then
  echo > Imakefile
  ${XMKMF} > /dev/null || echo > Makefile
  cc=`${srcdir}/tools/GETMAKEVAL CC`
  cc=`echo ${cc} | sed "s/ .*//"`
  if test "`which ${cc}`" = "`which ${CC}`"; then
    shlib_ldflags=`${srcdir}/tools/GETMAKEVAL SHLIBLDFLAGS`
    cpicflags=`${srcdir}/tools/GETMAKEVAL PICFLAGS`
  fi
  cxx=`${srcdir}/tools/GETMAKEVAL CXX`
  cxx=`echo ${cxx} | sed "s/ .*//"`
  if test "`which ${cxx}`" = "`which ${CXX}`"; then
    cxxpicflags=`${srcdir}/tools/GETMAKEVAL CXXPICFLAGS`
  fi
  rm -f Imakefile Makefile
fi

## Step 2.  GNU compilers.
if test "${GCC}" = yes; then
  cpicflags="-fPIC"
  shlib_ldflags="-shared"
fi
if test "${G77}" = yes; then
  fpicflags="-fPIC"
fi
if test "${GXX}" = yes; then
  cxxpicflags="-fPIC"
  shlib_cxxldflags="-shared"
fi

## Step 3.  Individual platform overrides.
case "${host_os}" in
  aix*)
    use_exportfile=yes
    ## All AIX code is PIC.
    cpicflags=
    cxxpicflags=
    fpicflags=
    wl="-Wl,"
    main_ldflags="${wl}-bdynamic ${wl}-bE:\$(top_builddir)/etc/R.exp ${wl}-bM:SRE"
    shlib_ldflags="${wl}-bM:SRE ${wl}-H512 ${wl}-T512 ${wl}-bnoentry ${wl}-bexpall ${wl}-bI:\$(R_HOME)/etc/R.exp"
    SHLIB_LIBADD="\$(LIBM)"
    ;;
  darwin*)
    cpicflags="-fno-common"
    cxxpicflags="-fno-common"
    fpicflags="-fno-common"
    case "${host_os}" in
      darwin1.[[012]])
	shlib_ldflags="-bundle -undefined suppress" ;;
      darwin1.3)
        shlib_ldflags="-bundle -flat_namespace -undefined suppress" ;;
      *)
	shlib_ldflags="-bundle -bundle_loader \$(R_HOME)/bin/R.bin" ;;
    esac
    shlib_cxxldflags="${shlib_ldflags}"
    ;;
  freebsd[[3-4]].*)
    main_ldflags="-export-dynamic"
    shlib_ldflags="-shared"
    ;;
  gnu*)				# GNU Hurd
    main_ldflags="-export-dynamic"
    ;;
  hpux*)
    SHLIB_EXT=".sl"
    case "${CC}" in
      cc|c89)
        cpicflags="+Z"
        ;;
    esac
    case "${F77}" in
      f77|fort77|f90)
        fpicflags="+Z"
        ;;
    esac
    main_ldflags="-Wl,-E"
    if test "${GCC}" = yes; then
      shlib_ldflags="-shared -fPIC"
    else
      ## <NOTE>
      ## If native cc insists on tacking on crt0.o when it calls ld, and
      ## crt0.o is not built with PIC.  As there seems to be no obvious
      ## way to tell cc not to do this, we use ld for linking shlibs.
      shlib_ld=ld
      shlib_ldflags="-b"
      ## </NOTE>
    fi
    if test "${GXX}" = yes; then
      shlib_cxxldflags="-shared -fPIC"
    fi
    ;;
  irix*)
    shlib_ldflags="-shared"
    shlib_cxxldflags="-shared"
    ## PIC is default on IRIX.
    cpicflags=
    cxxpicflags=
    fpicflags=
    ;;
  linux*aout)			# GNU Linux/aout
    sed '/HAVE_ELF_H/d' confdefs.h > tmp.h ; mv tmp.h confdefs.h
    ;;
  linux*)			# GNU Linux/ELF
    main_ldflags="-export-dynamic"
    ;;
  mingw*)
    SHLIB_EXT=".dll"
    cpicflags=
    cxxpicflags=
    fpicflags=
    ;;
  netbsd*)
    if ${CPP} - -dM < /dev/null | grep -q __ELF__; then
      main_ldflags="-export-dynamic"
      shlib_ldflags="-shared"
    else
      shlib_ldflags="-Bshareable"
    fi
    ;;
  osf*)
    cpicflags=
    cxxpicflags=
    fpicflags=
    shlib_ldflags="-shared"
    ;;
  solaris*)
    shlib_ldflags="-G"
    shlib_cxxldflags="-G"
    if test "${GCC}" = yes; then
      ld=`${CC} -print-prog-name=ld`
      ldoutput=`${ld} -v 2>&1 | grep GNU`
      if test -n "${ldoutput}"; then
	main_ldflags="-Wl,-export-dynamic"
	shlib_ldflags="-shared"
      fi
    else
      cpicflags="-KPIC"
    fi
    if test "${G77}" != yes; then
      fpicflags="-PIC"
    fi
    if test "${GXX}" = yes; then
      ld=`${CXX} -print-prog-name=ld`
      ldoutput=`${ld} -v 2>&1 | grep GNU`
      if test -n "${ldoutput}"; then
        shlib_cxxldflags="-shared"
      fi
    else
      cxxpicflags="-KPIC"
    fi
    ;;
esac

## <FIXME>
## Completely disable using libtool for building shlibs until libtool
## fully supports at least Fortran and C++.
## ## Step 4.  In case we use libtool ...
## if test "${use_libtool}" = yes; then
##   case "${host_os}" in
##     aix*)
##       ## FIXME: AIX needs special care.
##       ;;
##     *)
##       ## FIXME: Maybe we should set this here and not in R_bin_LDFLAGS?
##       main_ldflags=
##       ;;
##   esac
## fi
## </FIXME>

## Step 5.  Overrides from the environment and error checking.
if test -z "${MAIN_LD}"; then
  MAIN_LD="${main_ld}"
  if test -z "${MAIN_LDFLAGS}"; then
    MAIN_LDFLAGS="${main_ldflags}"
  else
    MAIN_LDFLAGS="${MAIN_LDFLAGS} ${main_ldflags}"
  fi
fi

: ${CPICFLAGS="${cpicflags}"}
if test -z "${CPICFLAGS}"; then
  case "${host_os}" in
    aix*|irix*|mingw*|osf*)
      ;;
    *)
      AC_MSG_WARN([I could not determine CPICFLAGS.])
      AC_MSG_ERROR([See the file INSTALL for more information.])
      ;;
  esac
fi

: ${FPICFLAGS="${fpicflags}"}
if test -z "${FPICFLAGS}" \
    && test -z "${F2C}"; then
  case "${host_os}" in
    aix*|irix*|mingw*|osf*)
      ;;
    *)
      AC_MSG_WARN([I could not determine FPICFLAGS.])
      AC_MSG_ERROR([See the file INSTALL for more information.])
      ;;
  esac
fi

: ${CXXPICFLAGS="${cxxpicflags}"}
if test -z "${CXXPICFLAGS}"; then
  case "${host_os}" in
    aix*|irix*|mingw*|osf*)
      ;;
    *)
      warn_cxxpicflags="I could not determine CXXPICFLAGS"
      AC_MSG_WARN([${warn_cxxpicflags}])
      ;;
  esac
fi

if test -z "${SHLIB_LD}"; then
  SHLIB_LD="${shlib_ld}"
  if test -z "${SHLIB_LDFLAGS}"; then
    SHLIB_LDFLAGS="${shlib_ldflags}"
  else
    SHLIB_LDFLAGS="${SHLIB_LDFLAGS} ${shlib_ldflags}"
  fi
fi
if test -z "${SHLIB_LDFLAGS}"; then
  AC_MSG_WARN([I could not determine SHLIB_LDFLAGS.])
  AC_MSG_ERROR([See the file INSTALL for more information.])
fi

if test -z "${SHLIB_CXXLD}"; then
  SHLIB_CXXLD="${shlib_cxxld}"
  if test -z "${SHLIB_CXXLDFLAGS}"; then
    SHLIB_CXXLDFLAGS="${shlib_cxxldflags}"
  else
    SHLIB_CXXLDFLAGS="${SHLIB_CXXLDFLAGS} ${shlib_cxxldflags}"
  fi
fi
if test -z "${SHLIB_CXXLDFLAGS}"; then
  warn_shlib_cxxldflags="I could not determine SHLIB_CXXLDFLAGS"
  AC_MSG_WARN([${warn_shlib_cxxldflags}])
fi

## Step 6.  We may need flags different from SHLIB_LDFLAGS and SHLIB_EXT
## for building R as a shared library to link against (the SHLIB_* vars
## just determined are really for loadable modules).  On ELF there is no
## difference, but e.g. on Mach-O for Darwin there is.
LIBR_EXT="${SHLIB_EXT}"
LIBR_LDFLAGS="${SHLIB_LDFLAGS}"
case "${host_os}" in
  darwin*)
    LIBR_EXT=".dylib"
    LIBR_LDFLAGS="-dynamiclib"
    ;;
esac

AC_SUBST(MAIN_LD)
AC_SUBST(MAIN_LDFLAGS)
AC_SUBST(CPICFLAGS)
AC_SUBST(CXXPICFLAGS)
AC_SUBST(FPICFLAGS)
AC_SUBST(SHLIB_CXXLD)
AC_SUBST(SHLIB_CXXLDFLAGS)
AC_SUBST(SHLIB_LD)
AC_SUBST(SHLIB_LDFLAGS)
AC_SUBST(SHLIB_LIBADD)
AC_SUBST(SHLIB_EXT)
AC_DEFINE_UNQUOTED(SHLIB_EXT, "${SHLIB_EXT}",
[Define this to be the extension used for shared libraries on your
system.])
AM_CONDITIONAL(USE_EXPORTFILE, [test "x${use_exportfile}" = xyes])
AC_SUBST(LIBR_EXT)
AC_SUBST(LIBR_LDFLAGS)

### * Checks for library functions.

AC_FUNC_ALLOCA
AC_CHECK_FUNCS(access chdir expm1 fcntl finite ftruncate getcwd \
  getgrgid getpwuid getuid hypot isascii isnan log1p matherr mkfifo \
  popen putenv rint setenv strcoll stat strptime system times unsetenv)
## <NOTE>
## No need checking for bcopy bzero memcpy mempcpy even though ifnames
## might stop corresponding HAVE_FOO conditionals.
## </NOTE>

## Used to build src/include/Rmath.h.
if test "${ac_cv_func_expm1}" = yes; then
  RMATH_HAVE_EXPM1="# define HAVE_EXPM1 1"
else
  RMATH_HAVE_EXPM1="# undef HAVE_EXPM1"
fi
AC_SUBST(RMATH_HAVE_EXPM1)
if test "${ac_cv_func_log1p}" = yes; then
  RMATH_HAVE_LOG1P="# define HAVE_LOG1P 1"
else
  RMATH_HAVE_LOG1P="# undef HAVE_LOG1P"
fi
AC_SUBST(RMATH_HAVE_LOG1P)

## do we need substitutes?
AC_REPLACE_FUNCS(acosh asinh atanh snprintf strdup strncasecmp vsnprintf)
## enable declarations in Defn.h?
AC_CHECK_DECLS([acosh, asinh, atanh], , , [#include <math.h>])
AC_CHECK_DECLS([snprintf, strdup, strncasecmp, vsnprintf])

AC_SEARCH_LIBS(connect, [socket])
AC_SEARCH_LIBS(gethostbyname, [nsl socket])
AC_SEARCH_LIBS(xdr_string, [nsl])
R_FUNC___SETFPUCW
R_FUNC_CALLOC
R_FUNC_FINITE
R_FUNC_LOG
R_FUNC_STRPTIME

## BLAS.
## <NOTE>
## This has to come *after* checking for FORTRAN 77 compiler/converter
## characteristics (notably name mangling and FLIBS).
if test "${use_blas}" = yes; then
  R_BLAS_LIBS
fi
## </NOTE>
if test -n "${BLAS_LIBS}"; then
  BLAS=
else
  BLAS="blas.o"
fi
AC_SUBST(BLAS)
AM_CONDITIONAL(USE_EXTERNAL_BLAS, [test "x${BLAS_LIBS}" != x])

### * Checks for system services.

## X11.
## Here we use the standard X11 search macro.
## We force the use of -lX11 (perhaps this is not necessary).
AC_PATH_XTRA
if test -z "${no_x}"; then
  X_LIBS="${X_LIBS} -lX11"
  use_X11="yes"
  R_GUIS=X11
  AC_DEFINE(HAVE_X11, 1,
            [Define if you have the X11 headers and libraries, and want
             the X11 GUI to be built.])
else
  use_X11="no"
  R_GUIS=
fi

## GNOME.
R_GNOME
if test "${use_gnome}" = yes; then
  R_GUIS="${R_GUIS} gnome";
fi

AC_SUBST(R_GUIS)

## Tcl/Tk.
R_TCLTK

## IEEE 754.
R_IEEE_754

## BSD networking.
R_BSD_NETWORKING

## Bitmap headers and libraries.
R_BITMAPS

## XDR headers and library routines.
R_XDR

## zlib headers and libraries.
R_ZLIB

## bzlib headers and libraries.
R_BZLIB

## POSIX times.
R_SYS_POSIX_LEAPSECONDS

## R profiling.
if test "${want_R_profiling}" = yes; then
  AC_CHECK_FUNCS(setitimer,
                 [AC_DEFINE(R_PROFILING, 1,
		            [Define this to enable R-level profiling.])],
                 [want_R_profiling="no"])
fi
AC_SUBST(R_PROFILING)

### * Miscellaneous.

## Printing.
## We look to see whether we have 'lpr' or 'lp'.  Some platforms
## provide both (SunOS and HPUX), and in those cases we choose lpr.
if test -z "${R_PRINTCMD}"; then
  AC_CHECK_PROGS(R_PRINTCMD, [lpr lp])
fi
AC_DEFINE_UNQUOTED(R_PRINTCMD, "${R_PRINTCMD}",
[Define this to be printing command on your system.])

## Default paper size.
AC_PATH_PROG(PAPERCONF, paperconf, false)
papersize=`${PAPERCONF}`
test -z "${papersize}" && papersize=a4
: ${R_PAPERSIZE="${papersize}"}
AC_SUBST(R_PAPERSIZE)

## Saving.
AC_SUBST(R_BATCHSAVE)

## Make sure -L terms come first in LIBS, separate out as LIBPATHS.
LIBS1=""
LIBS2=""
for arg in ${LIBS}; do
  case "${arg}" in
    -L*)
      LIBS1="${LIBS1} ${arg}"
      ;;
    *)
      LIBS2="${LIBS2} ${arg}"
      ;;
  esac
done
LIBS="${LIBS1} ${LIBS2}"
LIBPATHS="${LIBS1}"
AC_SUBST(LIBPATHS)

## R_LD_LIBRARY_PATH.
## <FIXME>
## This should not be necessary when using libtool for linking.
## In any case, is this really good enough?
for arg in ${LDFLAGS} ${FLIBS} ${BLAS_LIBS} ${X_LIBS} ${TCLTK_LIBS} \
           ${GNOME_LIBS}; do
  case "${arg}" in
    -L*)
      lib=`echo ${arg} | sed "s/^-L//"`
      if test -z "${R_LD_LIBRARY_PATH}"; then
        R_LD_LIBRARY_PATH="${lib}"
      else
        R_LD_LIBRARY_PATH="${R_LD_LIBRARY_PATH}${PATH_SEPARATOR}${lib}"
      fi
      ;;
  esac
done
## </FIXME>
AC_SUBST(R_LD_LIBRARY_PATH)

### * Win32 overrides
case "${host_os}" in
  mingw*)
    AC_DEFINE(HAVE_INTERNET, 1)
    AC_DEFINE(HAVE_JPEG, 1)
    AC_DEFINE(SUPPORT_LIBXML, 1)
    AC_DEFINE(HAVE_PNG, 1)
    AC_DEFINE(HAVE_POSIX_SETJMP, 1)
    AC_DEFINE(HAVE_SOCKETS, 1)
    AC_DEFINE(HAVE_TCLTK, 1)
    AC_DEFINE(HAVE_TIMES, 1)
    want_R_profiling=yes
    AC_DEFINE(R_PROFILING, 1)
    AC_DEFINE(SUPPORT_LIBXML, 1)
    ;;
esac

### * Output.

AC_CONFIG_HEADERS([src/include/config.h])
AC_CONFIG_FILES(
[Makeconf
  Makefile
  afm/Makefile
  doc/Makefile
  doc/html/Makefile
  doc/html/search/Makefile
  doc/manual/Makefile
  etc/Makefile
  etc/Makeconf
  etc/Renviron
  m4/Makefile
  share/Makefile
  src/Makefile
  src/appl/Makefile
  src/extra/Makefile
  src/extra/xdr/Makefile
  src/extra/zlib/Makefile
  src/include/Makefile
  src/include/Rmath.h
  src/include/R_ext/Makefile
  src/library/Makefile
  src/library/base/DESCRIPTION
  src/library/base/Makefile
  src/library/ctest/DESCRIPTION
  src/library/ctest/Makefile
  src/library/ctest/src/Makefile
  src/library/eda/DESCRIPTION
  src/library/eda/Makefile
  src/library/eda/src/Makefile
  src/library/lqs/DESCRIPTION
  src/library/lqs/Makefile
  src/library/lqs/src/Makefile
  src/library/methods/DESCRIPTION
  src/library/methods/Makefile
  src/library/methods/src/Makefile
  src/library/modreg/DESCRIPTION
  src/library/modreg/Makefile
  src/library/modreg/src/Makefile
  src/library/mva/DESCRIPTION
  src/library/mva/Makefile
  src/library/mva/src/Makefile
  src/library/nls/DESCRIPTION
  src/library/nls/Makefile
  src/library/nls/src/Makefile
  src/library/profile/Makefile
  src/library/stepfun/DESCRIPTION
  src/library/stepfun/Makefile
  src/library/splines/DESCRIPTION
  src/library/splines/Makefile
  src/library/splines/src/Makefile
  src/library/tcltk/DESCRIPTION
  src/library/tcltk/Makefile
  src/library/tcltk/src/Makefile
  src/library/tools/DESCRIPTION
  src/library/tools/Makefile
  src/library/ts/DESCRIPTION
  src/library/ts/Makefile
  src/library/ts/src/Makefile
  src/main/Makefile
  src/modules/Makefile
  src/modules/X11/Makefile
  src/modules/gnome/Makefile
  src/modules/internet/Makefile
  src/modules/lapack/Makefile
  src/modules/vfonts/Makefile
  src/nmath/Makefile
  src/nmath/standalone/Makefile
  src/scripts/Makefile
  src/scripts/COMPILE
  src/scripts/INSTALL
  src/scripts/REMOVE
  src/scripts/R.sh
  src/scripts/Rdconv
  src/scripts/Rdindex
  src/scripts/Rprof
  src/scripts/SHLIB
  src/scripts/Sd2Rd
  src/scripts/build
  src/scripts/check
  src/scripts/f77
  src/unix/Makefile
  tests/Makefile
  tests/Embedding/Makefile
  tests/Examples/Makefile
  tools/Makefile
])

AC_CONFIG_COMMANDS_PRE(
## <NOTE>
## Autoconf 2.53 makes using 'LIBOBJS' an error.
## Hence, we need to work around this as recommended in the Autoconf
## docs, section 'AC_LIBOBJ vs. LIBOBJS'.
[LTLIBOBJS=`echo "${LIB@&t@OBJS}" | sed 's/\.o/\.lo/g'`
AC_SUBST(LTLIBOBJS)
## </NOTE>
LTALLOCA=`echo "${ALLOCA}" | sed 's/\.o/.lo/g'`
AC_SUBST(LTALLOCA)])

AC_CONFIG_COMMANDS([stamp-h],
[test -f src/include/stamp-h || echo timestamp > src/include/stamp-h])

AC_OUTPUT

## Summarize configure results.
## <NOTE>
## Doing this via AC_CONFIG_COMMANDS would require explicitly passing all
## configure variables to config.status.
## </NOTE>
C_compiler="${CC} ${R_XTRA_CFLAGS} ${CFLAGS}"
CXX_compiler="${CXX} ${R_XTRA_CXXFLAGS} ${CXXFLAGS}"
if test -z "${F2C}"; then
  FORTRAN_compiler="${F77} ${R_XTRA_FFLAGS} ${FFLAGS}"
else
  FORTRAN_compiler="${F2C} ${F2CFLAGS}"
fi
AC_MSG_RESULT(
[
R is now configured for ${host}

  Source directory:          ${srcdir}
  Installation directory:    ${prefix}
  C compiler:                ${C_compiler}
  C++ compiler:              ${CXX_compiler}
  FORTRAN compiler:          ${FORTRAN_compiler}

  X11 support:               ${use_X11}
  Gnome support:             ${use_gnome}
  Tcl/Tk support:            ${use_tcltk}
  Readline support:          ${use_readline}

  R profiling support:       ${want_R_profiling}
  R as a shared library:     ${want_R_shlib}
])
if test -n "${warn_arg_var_FC}"; then
  AC_MSG_WARN([${warn_arg_var_FC}])
fi
if test -n "${warn_arg_with_g77}"; then
  AC_MSG_WARN([${warn_arg_with_g77}])
fi
if test -n "${warn_arg_with_f77}"; then
  AC_MSG_WARN([${warn_arg_with_f77}])
fi
if test -n "${warn_arg_with_f2c}"; then
  AC_MSG_WARN([${warn_arg_with_f2c}])
fi
if test -n "${warn_F77_and_F2C}"; then
  AC_MSG_WARN([${warn_F77_and_F2C}])
fi
if test -n "${warn_f2c_flibs}"; then
  AC_MSG_WARN([${warn_f2c_flibs}])
fi
if test -n "${warn_xcompile_sizeof_int}"; then
  AC_MSG_WARN([${warn_xcompile_sizeof_int}])
fi
if test -n "${warn_xcompile_sizeof_long}"; then
  AC_MSG_WARN([${warn_xcompile_sizeof_long}])
fi
if test -n "${warn_type_socklen}"; then
  AC_MSG_WARN([${warn_type_socklen}])
fi
if test -n "${warn_cxxpicflags}"; then
  AC_MSG_WARN([${warn_cxxpicflags}])
fi
if test -n "${warn_shlib_cxxldflags}"; then
  AC_MSG_WARN([${warn_shlib_cxxldflags}])
fi
if test -n "${warn_libglade_version}"; then
  AC_MSG_WARN([${warn_libglade_version}])
fi
if test -n "${warn_tcltk_version}"; then
  AC_MSG_WARN([${warn_tcltk_version}])
fi
if test -n "${warn_perl5}"; then
  AC_MSG_WARN([${warn_perl5}])
fi
if test -n "${warn_dvi}"; then
  AC_MSG_WARN([${warn_dvi}])
fi
if test -n "${warn_info}"; then
  AC_MSG_WARN([${warn_info}])
fi
if test -n "${warn_pdf}"; then
  AC_MSG_WARN([${warn_pdf}])
fi
if test -n "${warn_pager}"; then
  AC_MSG_WARN([${warn_pager}])
fi

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
