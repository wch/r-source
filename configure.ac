### configure.ac					-*- Autoconf -*-
###
### Process this file with autoconf to produce a configure script.
###
### Copyright (C) 1998-2006 R Core Team
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
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
### Boston, MA 02110-1301, USA

AC_PREREQ(2.60)

## We want to get the version number from file 'VERSION' (rather than
## specifying the version info in 'configure.ac'.  Hence, we need a bit
## of M4 magic.  Note that M4 esyscmd has a trailing newline because the
## shell command output has one, hence the patsubst() trick.
m4_define([R_VERSION],
          m4_bpatsubst(m4_esyscmd([cat VERSION]),
                       [\([0-9.]*\)\(\w\|\W\)*],
                       [\1]))
AC_INIT([R], R_VERSION, [r-bugs@R-project.org], [R])
AC_CONFIG_SRCDIR([src/include/Defn.h])
AC_CONFIG_AUX_DIR([tools])

### * Information on the package.

dnl ## Automake initialization.
dnl Not needed, and possibly resulting in non-portable configure scripts
dnl with hard-wired Automake API numbers (aclocal-1.x) ...
dnl AM_INIT_AUTOMAKE(AC_PACKAGE_NAME, AC_PACKAGE_VERSION)
dnl Nevertheless, we need to provide PACKAGE and VERSION ...
PACKAGE=[${PACKAGE_NAME}]
AC_DEFINE_UNQUOTED(PACKAGE, "${PACKAGE}", [Name of package])
AC_SUBST(PACKAGE)
VERSION=[${PACKAGE_VERSION}]
AC_DEFINE_UNQUOTED(VERSION, "${VERSION}", [Version number of package])
AC_SUBST(VERSION)
MAJ_MIN_VERSION=`echo ${VERSION} | sed 's/\.[[0-9]]$//'`
AC_SUBST(MAJ_MIN_VERSION)

## Autoheader initialization.
AH_TOP([#ifndef R_CONFIG_H
#define R_CONFIG_H])
AH_BOTTOM([
#endif /* not R_CONFIG_H */])

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
  cygwin*|mingw*|windows*|winnt)
    AC_DEFINE(Win32, 1,
              [Define according to your operating system type.])
    R_OSTYPE="windows"
    ;;
  *)
    AC_DEFINE(Unix, 1,
              [Define according to your operating system type.])
    R_OSTYPE="unix"
   ;;
esac
AC_SUBST(R_OSTYPE)

### ** Defaults.

cfile="${srcdir}/config.site"
if test -r "${cfile}"; then
  echo "loading site script '${cfile}'"
  . "${cfile}"
fi
cfile="${HOME}/.Rconfig"
if test -r "${cfile}"; then
  echo "loading user script '${cfile}'"
  . "${cfile}"
## <FIXME>
## Remove support for ~/.Rconf eventually.
else
  cfile="${HOME}/.Rconf"
  if test -r "${cfile}"; then
    echo "loading user script '${cfile}'"
    . "${cfile}"
  fi
## </FIXME>
fi
cfile="./config.site"
if test -r "${cfile}"; then
  echo "loading build specific script '${cfile}'"
  . "${cfile}"
fi

## We need to establish suitable defaults for a 64-bit OS
libnn=lib
case "${host_os}" in
  linux*)
    case "${host_cpu}" in
      x86_64|mips64|ppc64|powerpc64|sparc64|s390x)
        libnn=lib64
      ;;
    esac
    ;;
  solaris*)
    ## libnn=lib/sparcv9 ## on 64-bit only, but that's compiler-specific
    ;;  
esac
: ${LIBnn=$libnn}
## We provide these defaults so that headers and libraries in
## '/usr/local' are found (by the native tools, mostly).
if test -f "/sw/etc/fink.conf"; then
  : ${CPPFLAGS="-I/sw/include -I/usr/local/include"}
  : ${LDFLAGS="-L/sw/lib -L/usr/local/lib"}
else
  : ${CPPFLAGS="-I/usr/local/include"}
  : ${LDFLAGS="-L/usr/local/${LIBnn}"}
fi
AC_SUBST(LIBnn)
## take care not to  override the command-line setting
if test "${libdir}" = '${exec_prefix}/lib'; then
  libdir='${exec_prefix}/${LIBnn}'
fi

## R installation directories
m4_divert_once([HELP_BEGIN], [
R installation directories:
  --libdir=DIR        R files to R_HOME=DIR/R @<:@EPREFIX/$LIBnn@:>@
    rdocdir=DIR       R doc files to DIR      @<:@R_HOME/doc@:>@
    rincludedir=DIR   R include files to DIR  @<:@R_HOME/include@:>@
    rsharedir=DIR     R share files to DIR    @<:@R_HOME/share@:>@])dnl

if test -z "${rdocdir}"; then
  rdocdir='${rhome}/doc'
fi
AC_SUBST([rdocdir])

if test -z "${rincludedir}"; then
  rincludedir='${rhome}/include'
fi
AC_SUBST([rincludedir])

if test -z "${rsharedir}"; then
  rsharedir='${rhome}/share'
fi
AC_SUBST([rsharedir])

### ** Handle arguments to configure.

config_opts="${ac_configure_args}"
AC_SUBST(config_opts)

### ** Optional features.

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

## Allow the user to specify support for memory profiling.
AC_ARG_ENABLE([memory-profiling],
[AC_HELP_STRING([--enable-memory-profiling],
                [attempt to compile support for Rmemprof(), tracemem() @<:@no@:>@])],
[if test "${enableval}" = no; then
  want_memory_profiling=no
elif test "${enableval}" = yes; then
  want_memory_profiling=yes
else
  want_memory_profiling=no
fi],
[want_memory_profiling=no])

## Allow the user to specify building an R framework (Darwin).
AC_ARG_ENABLE([R-framework],
[AC_HELP_STRING([--enable-R-framework@<:@=DIR@:>@],
                [MacOS X only: build R framework (if possible), and specify
                 its installation prefix @<:@yes, /Library/Frameworks@:>@])],
[want_R_framework="${enableval}"],
[want_R_framework=yes])
## Can only build frameworks on Darwin.
if test "${want_R_framework}" != no; then
  case "${host_os}" in
    darwin*)
      if test "${want_R_framework}" = yes; then
        ## If we build a framework and 'prefix' was not given, we need
        ## to set it to '/Library/Frameworks' rather than '/usr/local'.
        ## Note that Autoconf sets things up so that by default, prefix
        ## and exec_prefix are set to 'NONE'.  Let's hope for no change.
	if test "x${prefix}" = xNONE; then
          prefix="/Library/Frameworks"
        fi
      else
	prefix="${want_R_framework}"
	want_R_framework=yes
      fi
      ## FW_VERSION is the sub-directory name used in R.framework/Version
      ## By default it's the a.b form of the full a.b.c version to simplify
      ## binary updates.
      : ${FW_VERSION=`echo "${PACKAGE_VERSION}" | sed -e "s/[[\.]][[0-9]]$//"`}
      ;;
    *)
      want_R_framework=no
      ;;
  esac
fi
AM_CONDITIONAL(WANT_R_FRAMEWORK, [test "x${want_R_framework}" = xyes])

## Allow the user to specify building R shared libraries.
## <NOTE>
## Building a framework implies building R shared libraries, hence the
## strange default.
## We might want to warn about the case where '--disable-R-shlib' was
## given explicitly ...
## </NOTE>
AC_ARG_ENABLE([R-shlib],
[AC_HELP_STRING([--enable-R-shlib],
                [build the shared/dynamic library 'libR' @<:@no@:>@])],
[want_R_shlib="${enableval}"],
[want_R_shlib="${want_R_framework}"])
AM_CONDITIONAL(WANT_R_SHLIB, [test "x${want_R_shlib}" = xyes])

## Build separate shared library containing R's BLAS if desired 
AC_ARG_ENABLE([BLAS-shlib],
[AC_HELP_STRING([--enable-BLAS-shlib],
                [build BLAS into a shared library @<:@perhaps@:>@])],
[use_blas_shlib="${enableval}"],
[use_blas_shlib="unset"])

if test "${want_R_shlib}" = yes; then
  LIBR="-L\$(R_HOME)/lib\$(R_ARCH) -lR"
else
  LIBR=
fi

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
[use_strict_barrier="${enableval}"],
[use_strict_barrier=no])
if test x"${use_strict_barrier}" = xyes; then
  AC_DEFINE(TESTING_WRITE_BARRIER, 1,
            [Define to enable provoking compile errors on write barrier
             violation.])
fi

AC_ARG_ENABLE([mbcs],
[AC_HELP_STRING([--enable-mbcs],
                [enable support for UTF-8 and other MBCS locales @<:@yes@:>@])],
[want_mbcs_support="${enableval}"],
[want_mbcs_support=yes])


### ** Optional packages.

## BLAS.
AC_ARG_WITH([blas],
[AC_HELP_STRING([--with-blas],
                [use system BLAS library (if available), or specify it @<:@no@:>@])],
[R_ARG_USE(blas)],
[use_blas=unset])
# default is "no" except on MacOS X

## LAPACK.
AC_ARG_WITH([lapack],
[AC_HELP_STRING([--with-lapack],
                [use system LAPACK library (if available), or specify it @<:@no@:>@])],
[R_ARG_USE(lapack)],
[use_lapack=unset])
# default is "no" except on MacOS X

## Readline.
AC_ARG_WITH([readline],
[AC_HELP_STRING([--with-readline], [use readline library @<:@yes@:>@])],
[R_ARG_USE(readline)],
[use_readline=yes])

## Aqua.
AC_ARG_WITH([aqua],
[AC_HELP_STRING([--with-aqua],
                [MacOS X only: use Aqua (if available) @<:@yes@:>@])],
[if test "${withval}" = no; then
  want_aqua=no
else
  want_aqua=yes
fi],
[want_aqua=yes])

## Tcl/Tk.
AC_ARG_WITH([tcltk],
[AC_HELP_STRING([--with-tcltk],
                [use Tcl/Tk (if available), or specify its library dir @<:@yes@:>@])],
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

## other libraries
AC_ARG_WITH([libpng],
[AC_HELP_STRING([--with-libpng],
                [use libpng library (if available) @<:@yes@:>@])],
[R_ARG_USE(libpng)],
[use_libpng=yes])
AC_ARG_WITH([jpeglib],
[AC_HELP_STRING([--with-jpeglib],
                [use jpeglib library (if available) @<:@yes@:>@])],
[R_ARG_USE(jpeglib)],
[use_jpeglib=yes])
AC_ARG_WITH([system-zlib],
[AC_HELP_STRING([--with-system-zlib],
                [use system zlib library (if available) @<:@no@:>@])],
[R_ARG_USE_SYSTEM(zlib)],
[use_system_zlib=no])
AC_ARG_WITH([system-bzlib],
[AC_HELP_STRING([--with-system-bzlib],
                [use system bzlib library (if available) @<:@no@:>@])],
[R_ARG_USE_SYSTEM(bzlib)],
[use_system_bzlib=no])
AC_ARG_WITH([system-pcre],
[AC_HELP_STRING([--with-system-pcre],
                [use system PCRE library (if available) @<:@no@:>@])],
[R_ARG_USE_SYSTEM(pcre)],
[use_system_pcre=no])

## Valgrind instrumentation
AC_ARG_WITH([valgrind-instrumentation],
[AC_HELP_STRING([--with-valgrind-instrumentation],
                [Level of additional instrumentation for Valgrind (0/1/2) @<:@0@:>@])],
[valgrind_level=${withval}],
[valgrind_level=0])

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

## Recommended R packages.
AC_ARG_WITH([recommended-packages],
[AC_HELP_STRING([--with-recommended-packages],
                [use/install recommended R packages @<:@yes@:>@])],
[R_ARG_USE(recommended_packages)],
[use_recommended_packages=yes])

## Iconv.
AC_ARG_WITH([iconv],
[AC_HELP_STRING([--with-iconv],
                [use iconv library @<:@yes@:>@])],
[R_ARG_USE(iconv)],
[use_iconv=yes])

### ** Precious variables.

AC_ARG_VAR([R_PRINTCMD],
           [command used to spool PostScript files to the printer])
AC_ARG_VAR([R_PAPERSIZE],
           [paper size for the local (PostScript) printer])
AC_ARG_VAR([R_BATCHSAVE],
           [set default behavior of R when ending a session])
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
           [special flags for compiling Fortran code to be turned into a
            shared library.])
AC_ARG_VAR([FCPICFLAGS],
           [special flags for compiling Fortran 95 code to be turned into a
            shared library.])
AC_ARG_VAR([SHLIB_LD],
           [command for linking shared libraries which contain object
            files from a C or Fortran compiler only])
AC_ARG_VAR([SHLIB_LDFLAGS],
           [special flags used by SHLIB_LD])
AC_ARG_VAR([DYLIB_LD],
           [command for linking dynamic libraries which contain object
            files from a C or Fortran compiler only])
AC_ARG_VAR([DYLIB_LDFLAGS],
           [special flags used for make a dynamic library])
AC_ARG_VAR([CXXPICFLAGS],
           [special flags for compiling C++ code to be turned into a
            shared library])
AC_ARG_VAR([SHLIB_CXXLD],
           [command for linking shared libraries which contain object
            files from a C++ compiler])
AC_ARG_VAR([SHLIB_CXXLDFLAGS],
           [special flags used by SHLIB_CXXLD])
AC_ARG_VAR([SHLIB_FCD],
           [command for linking shared libraries which contain object
            files from the Fortran 95 compiler])
AC_ARG_VAR([SHLIB_FCLDFLAGS],
           [special flags used by SHLIB_FCLD])
AC_ARG_VAR([TCLTK_LIBS],
           [flags needed for linking against the Tcl and Tk libraries])
AC_ARG_VAR([TCLTK_CPPFLAGS],
           [flags needed for finding the tcl.h and tk.h headers])
AC_ARG_VAR([MAKE], [Make command])
AC_ARG_VAR([R_BROWSER], [default browser])
AC_ARG_VAR([BLAS_LIBS],
           [flags needed for linking against external BLAS libraries])
AC_ARG_VAR([LAPACK_LIBS],
           [flags needed for linking against external LAPACK libraries])
AC_ARG_VAR([LIBnn], ['lib' or 'lib64' for dynamic libraries])
AC_ARG_VAR([SAFE_FFLAGS],
	   [Safe Fortran 77 compiler flags for e.g. dlamc.f])
AC_ARG_VAR([r_arch],
           [Use architecture-dependent subdirs with this name])
AC_ARG_VAR([DEFS], [C defines for use when compiling R])
AC_ARG_VAR([JAVA_HOME],
           [Path to the root of the Java environment])

if test -z "${r_arch}"; then
  R_ARCH=
else
  R_ARCH="/${r_arch}"
fi
AC_DEFINE_UNQUOTED(R_ARCH, "${r_arch}",
[Define this to use architecture-dependent subdirectories of this name.])
AC_SUBST([R_ARCH])


### ** Check whether we build in srcdir.

AC_PATH_PROG(GETWD, pwd, pwd)
AC_MSG_CHECKING([whether builddir is srcdir])
if test "`cd ${srcdir} && ${GETWD}`" = "`${GETWD}`"; then
  BUILDDIR_IS_SRCDIR=yes
else
  BUILDDIR_IS_SRCDIR=no
fi
AC_SUBST(BUILDDIR_IS_SRCDIR)
AC_MSG_RESULT([${BUILDDIR_IS_SRCDIR}])

### * Checks for programs.

R_MISSING_PROG(ACLOCAL, aclocal)
R_MISSING_PROG(AUTOCONF, autoconf)
R_MISSING_PROG(AUTOMAKE, automake)
R_MISSING_PROG(AUTOHEADER, autoheader)
AC_PROG_AWK
AC_PROG_EGREP
AC_PROG_LN_S
AC_PROG_RANLIB
AC_PROG_YACC
R_PROG_AR
R_PROG_INSTALL

## we would like a POSIX sed, and need one on Solaris
AC_PATH_PROGS(SED, sed, /bin/sed, [/usr/xpg4/bin:$PATH])
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
## Unzip && zip && gzip
AC_PATH_PROGS(R_UNZIPCMD, [${UNZIP} unzip], "")
AC_PATH_PROGS(R_ZIPCMD, [${ZIP} zip], "")
AC_PATH_PROGS(R_GZIPCMD, [${GZIP} gzip], true)
## Browser
R_PROG_BROWSER
## PDF viewer
R_PROG_PDFVIEWER

AC_PROG_CC

AC_PROG_GCC_TRADITIONAL
AC_PROG_CPP
R_PROG_CPP_CPPFLAGS
R_PROG_F77
AC_PROG_CXX
AC_PROG_CXXCPP
R_GCC4_VISIBILITY
AC_GNU_SOURCE ## needed to libintl

AC_PROG_OBJC
## unfortunately autoconf sets OBJC to gcc even if there is no working compiler
if test "${OBJC}" = gcc; then
  AC_LANG_PUSH([Objective C])
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM(,[id foo;])],,[OBJC=''])
  AC_LANG_POP([Objective C])
fi
R_PROG_OBJCXX

## This has to be R_DEFS as autoconf overrrides DEFS
R_DEFS=${DEFS}
AC_SUBST(R_DEFS)

## Libtool.
## (Run this after R_PROG_F77, as AC_PROG_LIBTOOL checks for a
## Fortran 77 compiler and sets F77 accordingly.)
AC_DISABLE_STATIC
AC_PROG_LIBTOOL
AC_SUBST(LIBTOOL_DEPS)

## cross-compiling
R_CROSS_COMPILING
AC_SUBST(BUILD_CC)
AC_SUBST(BUILD_R)
AM_CONDITIONAL(CROSS_COMPILING, [test "${cross_compiling}" = yes])

### * Checks for libraries.

## Set up LD_LIBRARY_PATH or equivalent.
## <FIXME>
## What is this doing *HERE*?
## Should be needed for tests using AC_RUN_IFELSE()?
## Make sure that non-standard directories specified via '-L' are really
## searched in the tests.
## Note: earlier versions used LIBS rather than LDFLAGS.
case "${host_os}" in
  darwin*)
    ## Darwin provides a full path in the ID of each library such 
    ## that the linker can add library's path to the binary at link time.
    ## This allows the dyld to find libraries even without xx_LIBRARY_PATH.
    ## No paths should be added to R_LD_LIBRARY_PATH (which in turn
    ## changes DYLD_LIBRARY_PATH), because they override the system
    ## look-up sequence. Such automatic override has proven to break things
    ## like system frameworks (e.g. ImageIO or OpenGL framework).
    ;;
  *)
    for arg in ${LDFLAGS}; do
      case "${arg}" in
        -L*)
	  lib=`echo ${arg} | sed "s/^-L//"`
	  R_SH_VAR_ADD(R_LD_LIBRARY_PATH, [${lib}], [${PATH_SEPARATOR}])
	  ;;
      esac
    done
    ;;
esac
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

## record how to strip shared/dynamic libraries.
ltout=`./libtool --config | grep '^striplib='`
eval "${ltout}"
AC_SUBST(striplib)

## <NOTE>
## This actually comes from libtool.m4.
AC_CHECK_LIBM
AC_SUBST(LIBM)
## </NOTE>
## AC_CHECK_LIBM computes LIBM but does not add to LIBS, hence we do
## the following as well.
AC_CHECK_LIB(m, sin)
case "${host_os}" in
  darwin*)
    ## MacOS <= 10.2 doesn't have dlcompat, >= 10.3 include it in libSystem
    ## since R 2.3 we don't provide a work-around for missing dl anymore
    AC_SEARCH_LIBS(dlopen, dl,,[AC_MSG_ERROR([Please install dlcompat or update to a more recent OS X.])])
    ## SI says we want '-lcc_dynamic' on Darwin, although currently
    ## http://developer.apple.com/documentation/MacOSX/ has nothing
    ## official.  Bill Northcott <w.northcott@unsw.edu.au> points out
    ## that it is only needed to GCC 3.x (and earlier) ...
    if test "${GCC}" = yes; then
      case "${CC_VERSION}" in
        2.*|3.*)
          AC_CHECK_LIB(cc_dynamic, main) ;;
      esac
    fi
    ;;
  *)
    AC_CHECK_LIB(dl, dlopen)
    ;;
esac

## Readline.
if test "${use_readline}" = yes; then
  AC_CHECK_HEADERS(readline/history.h readline/readline.h)
  r_save_LIBS="${LIBS}"
  LIBS=
  ## don't use the cached value as we need to rebuild LIBS
  unset ac_cv_lib_readline_rl_callback_read_char
  AC_CHECK_LIB(readline, rl_callback_read_char)
  use_readline="${ac_cv_lib_readline_rl_callback_read_char}"
  if test "${use_readline}" = no; then
    ## only need ncurses if libreadline is not statically linked against it
    unset ac_cv_lib_readline_rl_callback_read_char
    AC_CHECK_LIB(ncurses, main, [],
                 AC_CHECK_LIB(termcap, main, [],
                              AC_CHECK_LIB(termlib, main)))
    AC_CHECK_LIB(readline, rl_callback_read_char)
    use_readline="${ac_cv_lib_readline_rl_callback_read_char}"
  fi
  ## the NetBSD version as used in MacOS X does not have this
  AC_CHECK_FUNCS(history_truncate_file)
  READLINE_LIBS="${LIBS}"
  LIBS="${r_save_LIBS}"
  if test "${use_readline}" = no; then
    AC_MSG_ERROR([--with-readline=yes (default) and headers/libs are not available])
  else
    R_CHECK_FUNCS([rl_completion_matches], 
[#include <stdio.h>
#include <readline/readline.h>]
                 )
  fi
fi
AC_SUBST(READLINE_LIBS)

### * Checks for header files.

AC_HEADER_STDC
AC_HEADER_TIME
AC_HEADER_DIRENT
AC_HEADER_SYS_WAIT
## <NOTE>
## Some of these are also checked for when Autoconf computes the default
## includes.
AC_CHECK_HEADERS(arpa/inet.h dl.h dlfcn.h elf.h fcntl.h floatingpoint.h \
  fpu_control.h glob.h grp.h ieee754.h ieeefp.h limits.h locale.h \
  netdb.h netinet/in.h pwd.h stdbool.h strings.h \
  sys/param.h sys/select.h sys/socket.h sys/stat.h sys/resource.h \
  sys/time.h sys/times.h sys/utsname.h time.h unistd.h)
## </NOTE>
## <NOTE>
## These are ANSI C headers but some C code (written to work also
## without assuming ANSI C) may need the corresponding conditionals.
AC_CHECK_HEADERS(errno.h inttypes.h stdarg.h stdint.h string.h)
## </NOTE>
## <NOTE>
## src/main/regex.c uses HAVE_LIBINTL_H.
## But then we want full gettext support and AM_GNU_GETTEXT.
## AC_CHECK_HEADERS(libintl.h)
## </NOTE>

R_HEADER_SETJMP
R_HEADER_GLIBC2

### * Checks for types.

AC_TYPE_SIGNAL
AC_TYPE_PID_T
AC_TYPE_SIZE_T
R_SIZE_MAX
AC_CHECK_TYPE(blkcnt_t, long)
AH_TEMPLATE([blkcnt_t],
            [Define to 'long' if <sys/types.h> does not define.
             Apparently necessary to fix a GCC bug on AIX?])
R_TYPE_SOCKLEN
AC_CHECK_TYPES([stack_t], , , [#include <signal.h>])
## These are optional C99 types, which we typedef in Defn.h if absent.
## There seems some confusion as to where they should be defined:
## the standard says stdint.h but drafts and Solaris 8 have inttypes.h.
## It seems all systems having stdint.h include it in inttypes.h, and
## POSIX requires that.  But we will make sure.
AC_CHECK_TYPES([intptr_t, uintptr_t], , , [#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif])

### * Checks for compiler characteristics.

### ** Generic tests for the C, Fortran 77 and C++ compilers.

### *** C compiler.

AC_C_BIGENDIAN
AC_C_CONST
R_C_INLINE
AC_CHECK_SIZEOF(int)
if test "${ac_cv_sizeof_int}" = 4; then
  AC_DEFINE(INT_32_BITS, 1, [Define if you have 32 bit ints.])
fi
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(long long)
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(long double)
AC_CHECK_SIZEOF(size_t)

R_PROG_CC_MAKEFRAG
R_PROG_CC_LO_MAKEFRAG

### *** Fortran 77 compiler.

R_PROG_F77_FLIBS
R_PROG_F77_APPEND_UNDERSCORE
R_PROG_F77_CAN_RUN
R_PROG_F77_CC_COMPAT
R_PROG_F77_CC_COMPAT_COMPLEX

AM_CONDITIONAL(COMPILE_FORTRAN_DOUBLE_COMPLEX,
               [test "x${HAVE_FORTRAN_DOUBLE_COMPLEX}" != x])

### *** C++ compiler.

R_PROG_CXX_MAKEFRAG

### *** ObjC compiler

R_PROG_OBJC_RUNTIME
R_OBJC_FOUNDATION

### ** Platform-specific overrides for the C, Fortran 77 and C++ compilers.

case "${host_cpu}" in
  i*86|x86_64)
    R_PROG_CC_FLAG_D__NO_MATH_INLINES
    ## We used to add -mieee-fp here, but it seems it is really a
    ## linker flag for old Linuxen adding -lieee to a non-shared link.
    ;;
  alpha*)
    ## <NOTE>
    ## * IEEE math
    ## We really need to use @option{-ieee_with_inexact} (called
    ## @option{-mieee-with-inexact} for GCC) for the C compiler:
    ## @option{-ieee} (or @option{-mieee}) are not enough.
    ## According to <Albrecht.Gebhardt@uni-klu.ac.at> and Luke Tierney
    ## <luke@stat.uiowa.edu>, @option{-fpe3} is what we want for the
    ## native Fortran 77 compiler: seems that not all versions map
    ## @option{-ieee} to @option{-fpe3}.
    ## What about the C++ compilers?
    if test "${GCC}" = yes; then
      R_PROG_CC_FLAG([-mieee-with-inexact],
                     R_SH_VAR_ADD(R_XTRA_CFLAGS, [-mieee-with-inexact]))
    else
      R_PROG_CC_FLAG([-ieee_with_inexact],
                     R_SH_VAR_ADD(R_XTRA_CFLAGS, [-ieee_with_inexact]))
    fi
    if test "${G77}" = yes; then
      R_PROG_F77_FLAG([-mieee],
                      R_SH_VAR_ADD(R_XTRA_FFLAGS, [-mieee]))
    else
      R_PROG_F77_FLAG([-fpe3],
                      R_SH_VAR_ADD(R_XTRA_FFLAGS, [-fpe3]))
    fi
    if test "${GXX}" = yes; then
      R_PROG_CXX_FLAG([-mieee],
                      R_SH_VAR_ADD(R_XTRA_CXXFLAGS, [-mieee]))
    else
      R_PROG_CXX_FLAG([-ieee],
                      R_SH_VAR_ADD(R_XTRA_CXXFLAGS, [-ieee]))
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
      if test "x${OBJECT_MODE}" = "x64"; then
        R_PROG_CC_FLAG([-mminimal-toc],
                       R_SH_VAR_ADD(R_XTRA_CFLAGS, [-mminimal-toc]))
      else
        R_PROG_CC_FLAG([-mno-fp-in-toc],
                       R_SH_VAR_ADD(R_XTRA_CFLAGS, [-mno-fp-in-toc]))
      fi
    fi
    ;;
  cygwin*|mingw*|windows*|winnt)
    AC_DEFINE(HAVE_NO_SYMBOL_UNDERSCORE)
    ;;
  darwin*)
    ## This is needed for Apple's dlsym included in >= 10.3 
    ## and so we've changed dlfcn-darwin.c for 2.1.0
    AC_DEFINE(HAVE_NO_SYMBOL_UNDERSCORE)
    ## Need '-no-cpp-precomp' say J de Leeuw <deleeuw@stat.ucla.edu> and
    ## http://fink.sourceforge.net/doc/porting/basics.php.
    if test "${GCC}" = yes; then
      ## only Apple build of gcc supports (and needs?) -no-cpp-precomp
      ${CC} --version|grep Apple >/dev/null 2>/dev/null
      if test $? == 0; then
        R_SH_VAR_ADD(R_XTRA_CPPFLAGS, [-no-cpp-precomp])
      fi
    fi
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
	R_SH_VAR_ADD(R_XTRA_CPPFLAGS, [-Wp,-H16000])
	;;
    esac
    AC_CHECK_LIB(dld, shl_load, [R_XTRA_LIBS="-ldld ${R_XTRA_LIBS}"])
    ;;
  irix*)
    R_C_OPTIEEE
    ## <NOTE>
    ## We really should test explictly whether the Fortran and C++
    ## compilers *need* '-OPT:IEEE_NaN_inf=ON'.  Currently, all we do is
    ## check whether the non-GNU tools *accept* the flag.
    if test "${G77}" != yes; then
      R_PROG_F77_FLAG([-OPT:IEEE_NaN_inf=ON],
                      R_SH_VAR_ADD(R_XTRA_FFLAGS, [-OPT:IEEE_NaN_inf=ON]))
    fi
    if test "${GXX}" != yes; then
      R_PROG_CXX_FLAG([-OPT:IEEE_NaN_inf=ON],
                      R_SH_VAR_ADD(R_XTRA_CXXFLAGS, [-OPT:IEEE_NaN_inf=ON]))
    fi
    ## </NOTE>
    ;;
  linux*)
    case  "${CC}" in
      ## Intel compiler
      *icc*)
	## icc declares __GNUC__, so it picks up CFLAGS intended for gcc.
	if test "$ac_test_CFLAGS" != set; then
          if test $ac_cv_prog_cc_g = yes; then
	    case "${host_cpu}" in
	      x86_64)
	        CFLAGS="-g -O2 -std=c99"
		;;
	      *)
	        ## on ix86 optimization fails
                CFLAGS="-g -std=c99"
		;;
	    esac
          else
	    case "${host_cpu}" in
	      x86_64)
	        CFLAGS="-O2 -std=c99"
		;;
	      *)
                CFLAGS="-std=c99"
		;;
	    esac
          fi
        fi
	## -mp is the preferred form of -mieee-fp
        R_PROG_CC_FLAG([-mp],
                       R_SH_VAR_ADD(R_XTRA_CFLAGS, [-mp]))
        ;;
    esac
    case  "${F77}" in
      ## Intel compilers
      *ifc|*ifort)
	if test "$ac_test_FFLAGS" != set; then
          if test $ac_cv_prog_f77_g = yes; then
	    case "${host_cpu}" in
	      x86_64)
	        FFLAGS="-g -O2"
		;;
	      *)
                FFLAGS="-g"
		;;
	    esac
          else
	    case "${host_cpu}" in
	      x86_64)
	        FFLAGS="-O2"
		;;
	      *)
	        ## on ix86 optimization of dlamc.f fails
                FFLAGS=
		;;
	    esac
          fi
        fi
        R_PROG_CC_FLAG([-mp],
                       R_SH_VAR_ADD(R_XTRA_FFLAGS, [-mp]))
        ;;
    esac
    case  "${CXX}" in
      ## Intel compilers
      *icpc|*icc)
	if test "$ac_test_CXXFLAGS" != set; then
          if test $ac_cv_prog_cxx_g = yes; then
	    case "${host_cpu}" in
	      x86_64)
	        CXXFLAGS="-g -O2"
		;;
	      *)
                CXXFLAGS="-g"
		;;
	    esac
          else
	    case "${host_cpu}" in
	      x86_64)
	        CXXFLAGS="-O2"
		;;
	      *)
                CXXFLAGS=
		;;
	    esac
          fi
        fi
        R_PROG_CC_FLAG([-mp],
                       R_SH_VAR_ADD(R_XTRA_CXXFLAGS, [-mp]))
        ;;
    esac
    ;;
  openbsd*)
    if test -z "`echo __ELF__ | $CC -E - | grep __ELF__`"; then
      AC_DEFINE(HAVE_NO_SYMBOL_UNDERSCORE)
    fi
    ;;
  osf*)
    AC_DEFINE(HAVE_NO_SYMBOL_UNDERSCORE)
    ;;  
esac

if test "${SAFE_FFLAGS+set}" != set; then
  if test "x${ac_cv_f77_compiler_gnu}" = xyes; then
    SAFE_FFLAGS="${FFLAGS} -ffloat-store"
  else
    SAFE_FFLAGS=${FFLAGS}
  fi
fi


AC_SUBST(CFLAGS)
AC_SUBST(MAIN_CFLAGS)
AC_SUBST(SHLIB_CFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(SHLIB_CXXFLAGS)
AC_SUBST(FFLAGS)
AC_SUBST(SAFE_FFLAGS)
AC_SUBST(MAIN_FFLAGS)
AC_SUBST(SHLIB_FFLAGS)
AC_SUBST(R_XTRA_CFLAGS)
AC_SUBST(R_XTRA_CPPFLAGS)
AC_SUBST(R_XTRA_CXXFLAGS)
AC_SUBST(R_XTRA_FFLAGS)
AC_SUBST(R_XTRA_LIBS)
AC_SUBST(OBJC_LIBS)
AC_SUBST(OBJCFLAGS)
AC_SUBST(OBJCXXFLAGS)

### ** DLL stuff.

## We need to determine the following:
##
## MAIN_LD, MAINLD_FLAGS
##   command and flags for loading the main binary so that it will load
##   shared libraries (DLLs) at runtime, also for profiling.
## CPICFLAGS, CXXPICFLAGS, FPICFLAGS, FCPICFLAGS
##   flags for compiling C, C++, and Fortran library code.
## SHLIB_LD, SHLIB_LDFLAGS
##   command and flags for creating DLLs (which contain object files
##   from a C or Fortran compiler).
## DYLIB_LD, DYLIB_LDFLAGS
##   ditto for dynamic libraries (where different.)
## SHLIB_CXXLD, SHLIB_CXXLDFLAGS
##   command and flags for creating DLLs which contain object files from
##   a C++ compiler.  According to Autoconf, the C++ compiler/linker
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
## Note also that some systems (formerly AIX) do not allow for unresolved
## symbols at link time.  For such systems, we link against -lm (in case
## it exists) when building a shlib module via SHLIB_LIBADD.

main_ld="${CC}"
shlib_ld="${CC}"
shlib_cxxld="${CXX}"
SHLIB_EXT=".so"
SHLIB_LIBADD=
use_exportfiles=no

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
  ${XMKMF} > /dev/null 2>&1 || echo > Makefile
  cc=`${srcdir}/tools/GETMAKEVAL CC`
  cc=`echo ${cc} | sed "s/ .*//"`
  ## Paul Gilbert reported on R-devel 2006-04-13 a system with cc=""
  if test -n "${cc}" ; then
    r_cc_cmd=`echo ${CC} | sed "s/ .*//"`
    if test "`which ${cc}`" = "`which ${r_cc_cmd}`"; then
      shlib_ldflags=`${srcdir}/tools/GETMAKEVAL SHLIBLDFLAGS`
      cpicflags=`${srcdir}/tools/GETMAKEVAL PICFLAGS`
    fi
  fi
  cxx=`${srcdir}/tools/GETMAKEVAL CXX`
  cxx=`echo ${cxx} | sed "s/ .*//"`
  if test -n "${cxx}" ; then
    r_cxx_cmd=`echo ${CXX} | sed "s/ .*//"`
    if test "`which ${cxx}`" = "`which ${r_cxx_cmd}`"; then
      cxxpicflags=`${srcdir}/tools/GETMAKEVAL CXXPICFLAGS`
    fi
  fi
  rm -f Imakefile Makefile
fi

## Step 2.  GNU compilers.
if test "${GCC}" = yes; then
  case "${host_cpu}" in
## Sparc has only an 8k global object table, 1024 entries on 64-bit.
## PowerPC has 32k, not enough on ppc64 for the ca6200 entries in libR.so
## The only other platform where this is said to matter is m68k, which
## has 32k and so can use -fpic.
## However, although the gcc docs do not mention it, it seems s390/s390x
## also supports and needs -fPIC
    sparc*|ppc64|powerpc64|s390*)
      cpicflags="-fPIC"
      ;;
    *)
      cpicflags="-fpic"
      ;;
  esac
  shlib_ldflags="-shared"
fi
if test "${G77}" = yes; then
  case "${host_cpu}" in
    sparc*|ppc64|powerpc64|s390*)
      fpicflags="-fPIC"
      ;;
    *)
      fpicflags="-fpic"
      ;;
  esac
fi
if test "${GXX}" = yes; then
  case "${host_cpu}" in
    sparc*|ppc64|powerpc64|s390*)
      cxxpicflags="-fPIC"
      ;;
    *)
      cxxpicflags="-fpic"
      ;;
  esac
  shlib_cxxldflags="-shared"
fi

## Step 3.  Individual platform overrides.
dylib_undefined_allowed=yes
case "${host_os}" in
  aix[123]*|aix4.[01]*)
    ## These need a form of linking we no longer support
    AC_MSG_ERROR([AIX prior to 4.2 is not supported])
    ;;
  aix*)
    ## now use run-time linking
    if test "${want_R_shlib}" != yes; then
      use_exportfiles=yes
    fi
    ## All AIX code is PIC.
    cpicflags=
    cxxpicflags=
    fpicflags=
    ## not clear if this is correct for native compilers
    wl="-Wl,"
    ## libtool suggests that ia64 needs -Bexport and not -brtl
    ## but we have no confirmation.
    dylib_undefined_allowed=no
    ##ADD: A symbol of memcpy,memset is exported in libR by expall.
    ##ADD: However, for example, symbol in libc of memcpy is __memmove,__memmove64.
    ##ADD: This black magic puts lc before lR and pockets this.
    if test "x${OBJECT_MODE}" = "x64"; then
      main_ldflags="${wl}-brtl ${wl}-bexpall ${wl}-bpT:0x100000000 ${wl}-bpD:0x110000000 -lc"
    else
      main_ldflags="${wl}-brtl ${wl}-bexpall -lc"
    fi
    shlib_ldflags="${wl}-brtl ${wl}-G ${wl}-bexpall ${wl}-bnoentry -lc"
    if test "${want_R_shlib}" != yes; then
      ## symbol of an archive(RLIBS and EXTRA_LIBS) is thrown away
      ## unless export it in a main link when unused.
      R_SH_VAR_ADD(main_ldflags, [-Wl,-bE:\$(top_builddir)/etc/R.exp])
      ## Import symbol is IMPid = '.'
      ## no import symbol is IMPid = '..' (run-time link style)
      ## R_SH_VAR_ADD(LIBR, [-Wl,-bI:\$(R_HOME)/etc/R.exp])
    fi
#    main_ldflags="${wl}-bdynamic ${wl}-bE:\$(top_builddir)/etc/R.exp ${wl}-bM:SRE"
#    shlib_ldflags="${wl}-bM:SRE ${wl}-H512 ${wl}-T512 ${wl}-bnoentry ${wl}-bexpall ${wl}-bI:\$(R_HOME)/etc/R.exp"
    SHLIB_LIBADD="\$(LIBM)"
    shlib_cxxldflags="${shlib_ldflags}"
    ;;
  darwin*)
    darwin_pic="-fPIC"
    dylib_undefined_allowed=no
    darwin_dylib_ldflags="-dynamiclib"
    ## Used to need this for GUI and quartz device, but neither are in
    ## R or libR.dylib any longer (2.1.0).
    ## main_ldflags="-framework Carbon"
    ## Want '-mdynamic-no-pic' for GCC 3, says Jan de Leeuw.
    if test "${GCC}" = yes; then
      case "${CC_VERSION}" in
        3.*)
          R_SH_VAR_ADD(main_ldflags, [-mdynamic-no-pic]) ;;
      esac
    fi
    case "${host_os}" in
      ## According to Jan de Leeuw, OS X 10.1 is darwin 5, OS X 10.2 is
      ## darwin 6, and the real difference is between darwin < and >= 5.
      ## It would be nice to use R.bin (or libR.dylib) as bundle_loader
      ## together with '-undefined suppress' for darwin 5 or better, but
      ## the paths to these change when installing.
      ## The Darwin 1.[012] special casing is from libtool and could be
      ## eliminated ('-flat_namespace' is the default if available).
      ## FIXME: Rblas is entirely untested and possibly doesn't work
      ## for any darwin before darwin7!
      darwin1.[[012]]*)
        shlib_ldflags="-bundle -undefined suppress"
        darwin_pic="${darwin_pic} -fno-common"
        ;;
      ## although OS X 10.3 (Panther = Darwin 7) has some useful features,
      ## dlopen doesn't work with dylibs and RTLD_LOCAL, so we can't use them.
      darwin[[567]]*)
	shlib_ldflags="-flat_namespace -bundle -undefined suppress"
        darwin_pic="${darwin_pic} -fno-common"
        ;;
      ## * recent ld has -single_module so it doesn't need -fno-common
      ##   we have to use dylib instead of a bundle
      ## * dylib+single_module+flat_namespace=pretty much what other platforms call .so
      ##   but there can be no multiple symbols (due to flat namespace)
      ## * since 10.3 we can also use -undefined dynamic_lookup which allows us to
      ##   use two-level namespace and still have undefined symbols
      *)
        ## FIXME: strictly speaking it should be "yes" but libRblas still
	## needs -lgfortran because the sharing is a one-way street
        ## dylib_undefined_allowed=yes
	AC_MSG_CHECKING([whether linker supports dynamic lookup])
	shlib_ldflags="-dynamiclib -Wl,-macosx_version_min -Wl,10.3 -undefined dynamic_lookup -single_module -multiply_defined suppress"
	AC_CACHE_VAL([r_cv_has_dynlookup],[
[cat > conftest.c <<EOF
  void dummy() { }
EOF]
echo "${CC} ${CFLAGS} conftest.c ${shlib_ldflags} -o libconftest${DYLIB_EXT} ${LIBS}" >&AS_MESSAGE_LOG_FD
if ${CC} ${CFLAGS} conftest.c ${shlib_ldflags} -o libconftest${DYLIB_EXT} ${LIBS} 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD; then
   r_cv_has_dynlookup=yes
   AC_MSG_RESULT([yes])
else
   r_cv_has_dynlookup=no
   AC_MSG_RESULT([no])
   AC_MSG_WARN([***  Please consider updating your Xcode tools. ***])
fi
rm -f libconftest${DYLIB_EXT} conftest.c
])
	if test -n "${FORCE_FLAT_NAMESPACE}"; then
	   AC_MSG_WARN([Use of flat namespace is requested by user.])
	   r_cv_has_dynlookup=forced-no
	fi
	if test "${r_cv_has_dynlookup}" != "yes"; then
	   shlib_ldflags="-dynamiclib -flat_namespace -undefined suppress -single_module -multiply_defined suppress"
	   dylib_undefined_allowed=yes
	fi
	
	## we use the same method for shlib and dylib now
	darwin_dylib_ldflags="${shlib_ldflags}"
	## side note: we could use flat namespace instead, but there is an exception:
	## * libRblas must be 2-level, dyn lookup because of xerbla which is undefined
        ;;
    esac
    cpicflags="${darwin_pic}"
    cxxpicflags="${darwin_pic}"
    fpicflags="${darwin_pic}"
    shlib_cxxldflags="${shlib_ldflags}"
    if test "${ac_cv_lib_cc_dynamic_main}" = yes; then
      ## Could also try grepping LIBS for '-lcc_dynamic' ...
      SHLIB_LIBADD="-lcc_dynamic"
    fi
    ;;
  freebsd*)
    ## Only sure for FreeBSD 3 and above.
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
      shlib_ldflags="-shared -fPIC -Wl,-Bsymbolic"
    else
      ## <NOTE>
      ## Native cc insists on tacking on crt0.o when it calls ld, and
      ## crt0.o is not built with PIC.  As there seems to be no obvious
      ## way to tell cc not to do this, we use ld for linking shlibs.
      shlib_ld=ld
      shlib_ldflags="-b -Bsymbolic"
      ## </NOTE>
    fi
    if test "${GXX}" = yes; then
      shlib_cxxldflags="-shared -fPIC"
    fi
    ;;
  irix*)
    cpicflags=
    cxxpicflags=
    fpicflags=
    shlib_ldflags="-shared"
    shlib_cxxldflags="-shared"
    ;;
  linux*aout)			# GNU Linux/aout
    sed '/HAVE_ELF_H/d' confdefs.h > tmp.h ; mv tmp.h confdefs.h
    ;;
  linux*)			# GNU Linux/ELF
    case  "${CC}" in
      ## Intel compiler: note that -c99 may have been appended
      *icc*)
        cpicflags="-fpic"
        ;;
      ## Portland Group
      *pgcc*)
        cpicflags="-fpic"
	;;
    esac
    case  "${F77}" in
      ## Intel compilers
      *ifc|*ifort)
        fpicflags="-fpic"
        ;;
      ## Portland Group
      *pgf77|*pgf90|*pgf95)
        fpicflags="-fpic"
        ;;
    esac
    case  "${CXX}" in
      ## Intel compilers
      *icpc|*icc)
        cxxpicflags="-fpic"
        ;;
      ## Portland Group
      *pgCC)
        cxxpicflags="-fpic"
        ;;
    esac
    ## Luke Tierney says that just '-export-dynamic' does not work for
    ## Intel compilers (icc).
    main_ldflags="-Wl,--export-dynamic"
    ;;
  mingw*)
    SHLIB_EXT=".dll"
    cpicflags=
    cxxpicflags=
    fpicflags=
    ;;
  netbsd*)
    if ${CPP} - -dM < /dev/null | grep __ELF__ >/dev/null ; then
      main_ldflags="-export-dynamic"
      shlib_ldflags="-shared"
    else
      shlib_ldflags="-Bshareable"
    fi
    ;;
  openbsd*)
    ## looks like ${wl} is not defined here.  Perhaps in libtool code?
    if test -z "`echo __ELF__ | $CC -E - | grep __ELF__`"; then
      main_ldflags="${wl}-export-dynamic"
      shlib_ldflags="-shared -fPIC"
    fi
    ;;
  osf*)
    cpicflags=
    cxxpicflags=
    fpicflags=
    shlib_ldflags="-shared"
    ;;
  solaris*)
## SPARC has only an 8k global object table, 1024 entries on 64-bit,
## so need PIC not pic.  They are the same on other Solaris platforms.
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
      if test "`basename ${CXX}`" = "CC" ; then
        ## Forte version 7 needs -lCstd: Forte 6 does not.	
        ver=`${CXX} -V 2>&1 | sed 2d | grep 'Forte Developer 7 C++'`
        if test -n "${ver}" ; then
          shlib_cxxldflags="-G -lCstd"
        fi
      fi
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
##     *)
##       ;;
##   esac
## fi
## </FIXME>

## Step 5.  Overrides from the environment and error checking.
if test -z "${MAIN_LD}"; then
  MAIN_LD="${main_ld}"
  R_SH_VAR_ADD(MAIN_LDFLAGS, [${main_ldflags}])
fi

: ${CPICFLAGS="${cpicflags}"}
if test -z "${CPICFLAGS}"; then
  case "${host_os}" in
    aix*|irix*|mingw*|osf*)
      ;;
    *)
      AC_MSG_WARN([I could not determine CPICFLAGS.])
      AC_MSG_ERROR([See the file doc/html/R-admin.html for more information.])
      ;;
  esac
fi

: ${FPICFLAGS="${fpicflags}"}
if test -z "${FPICFLAGS}"; then
  case "${host_os}" in
    aix*|irix*|mingw*|osf*)
      ;;
    *)
      AC_MSG_WARN([I could not determine FPICFLAGS.])
      AC_MSG_ERROR([See the file doc/html/R-admin.html for more information.])
      ;;
  esac
fi

: ${CXXPICFLAGS="${cxxpicflags}"}
if test -z "${CXXPICFLAGS}"; then
  case "${host_os}" in
    aix*|irix*|mingw*|osf*)
      ;;
    *)
      warn_cxxpicflags="I could not determine CXXPICFLAGS."
      AC_MSG_WARN([${warn_cxxpicflags}])
      ;;
  esac
fi

if test -z "${SHLIB_LD}"; then
  SHLIB_LD="${shlib_ld}"
  R_SH_VAR_ADD(SHLIB_LDFLAGS, [${shlib_ldflags}])
fi
if test -z "${SHLIB_LDFLAGS}"; then
  AC_MSG_WARN([I could not determine SHLIB_LDFLAGS.])
  AC_MSG_ERROR([See the file doc/html/R-admin.html for more information.])
fi

if test -z "${SHLIB_CXXLD}"; then
  SHLIB_CXXLD="${shlib_cxxld}"
  R_SH_VAR_ADD(SHLIB_CXXLDFLAGS, [${shlib_cxxldflags}])
fi
if test -z "${SHLIB_CXXLDFLAGS}"; then
  warn_shlib_cxxldflags="I could not determine SHLIB_CXXLDFLAGS"
  AC_MSG_WARN([${warn_shlib_cxxldflags}])
fi

## Step 6.  We may need flags different from SHLIB_LDFLAGS and SHLIB_EXT
## for building R as a shared library to link against (the SHLIB_* vars
## just determined are really for loadable modules).  On ELF there is no
## difference, but e.g. on Mach-O for Darwin there is.
##
## Also need flags to build the Rlapack shared library on some platforms.
DYLIB_EXT="${SHLIB_EXT}"
dylib_ldflags="${SHLIB_LDFLAGS}"
LIBR_LDFLAGS=""
RLAPACK_LDFLAGS=""
RBLAS_LDFLAGS=""
case "${host_os}" in
  aix*)
    ## Not needed for -brtl linking
    # RLAPACK_LDFLAGS="${wl}-bE:\$(top_builddir)/etc/Rlapack.exp"
    # LAPACK_LDFLAGS="${wl}-bI:\$(R_HOME)/etc/Rlapack.exp"
    ;;
  darwin*)
    DYLIB_EXT=".dylib"
    dylib_ldflags="${darwin_dylib_ldflags}"
    MAJR_VERSION=`echo "${PACKAGE_VERSION}" | sed  -e "s/[[\.]][[1-9]]$/.0/"` 
    LIBR_LDFLAGS="-install_name libR.dylib -compatibility_version ${MAJR_VERSION}  -current_version ${PACKAGE_VERSION}  -headerpad_max_install_names"
    RLAPACK_LDFLAGS="-install_name libRlapack.dylib -compatibility_version ${MAJR_VERSION} -current_version ${PACKAGE_VERSION}  -headerpad_max_install_names"
    ## don't use version in libRblas so we can replace it with any BLAS implementation
    RBLAS_LDFLAGS="-install_name libRblas.dylib -headerpad_max_install_names"
    ;;
  hpux*)
    ## Needs to avoid embedding a relative path ../../../bin.
    ## See the above code for shlib_ldflags for reasons why we currently
    ## cannot always use '-Wl,+s'.
    if test "${GCC}" = yes; then
      LAPACK_LDFLAGS="-Wl,+s"
    else
      LAPACK_LDFLAGS="+s"
    fi
    ;;
  openbsd*)
    PACKAGE_VERSION_MAJOR=`echo "${PACKAGE_VERSION}" | \
      sed -e "s/\.//" -e "s/\..*$//"`
    PACKAGE_VERSION_MINOR=`echo "${PACKAGE_VERSION}" | \
      sed -e "s/.*\.\([[^.]][[^.]]*$\)/\1/"`
    DYLIB_EXT=".so.${PACKAGE_VERSION_MAJOR}.${PACKAGE_VERSION_MINOR}"
    ;;
esac

if test -z "${DYLIB_LD}"; then
  DYLIB_LD="${SHLIB_LD}"
  R_SH_VAR_ADD(DYLIB_LDFLAGS, [${dylib_ldflags}])
else
  if test -z "${DYLIB_LDFLAGS}"; then
    DYLIB_LDFLAGS="${dylib_ldflags}"
  fi
fi

AM_CONDITIONAL(DYLIB_UNDEFINED_ALLOWED, [test "x${dylib_undefined_allowed}" = xyes])

AC_SUBST(MAIN_LD)
AC_SUBST(MAIN_LDFLAGS)
AC_SUBST(CPICFLAGS)
AC_SUBST(CXXPICFLAGS)
AC_SUBST(DYLIB_LD)
AC_SUBST(DYLIB_LDFLAGS)
AC_SUBST(FCPICFLAGS)
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
AM_CONDITIONAL(USE_EXPORTFILES, [test "x${use_exportfiles}" = xyes])
AC_SUBST(DYLIB_EXT)
AC_SUBST(LIBR_LDFLAGS)
AC_SUBST(RBLAS_LDFLAGS)
AC_SUBST(RLAPACK_LDFLAGS)
AC_SUBST(LAPACK_LDFLAGS)
AC_SUBST(FW_VERSION)

### Now we have found all the flags, we need to use them to test appropriately.
### We don't currently have any C++ tests, but future-proof.
### In principle we should do this before testing for C-Fortran compatibility.

CPPFLAGS_KEEP=${CPPFLAGS}
CFLAGS_KEEP=${CFLAGS}
FFLAGS_KEEP=${FFLAGS}
CXXFLAGS_KEEP=${CXXFLAGS}
CPPFLAGS="${CPPFLAGS} ${R_XTRA_CPPFLAGS}"
if test "${want_R_shlib}" = yes; then
  CFLAGS="${CFLAGS} ${CPICFLAGS} ${R_XTRA_CFLAGS}"
  FFLAGS="${FFLAGS} ${FPICFLAGS} ${R_XTRA_FFLAGS}"
  CXXFLAGS="${CXXFLAGS} ${CXXPICFLAGS} ${R_XTRA_CXXFLAGS}"
else
  CFLAGS="${CFLAGS} ${R_XTRA_CFLAGS}"
  FFLAGS="${FFLAGS} ${R_XTRA_FFLAGS}"
  CXXFLAGS="${CXXFLAGS} ${R_XTRA_CXXFLAGS}"
fi

### * Checks for library functions.

AC_CHECK_TYPES([off_t])
AC_FUNC_ALLOCA
AC_CHECK_DECLS([alloca], , , 
[#ifdef HAVE_ALLOCA_H
# include <alloca.h>
#endif])
## fseeko/ftello may be macros, matherr is redefined
AC_CHECK_FUNCS(fseeko ftello isblank matherr)
R_CHECK_FUNCS([fcntl], [#include <fcntl.h>])
R_CHECK_FUNCS([getgrgid], [#include <grp.h>])
R_CHECK_FUNCS([expm1 hypot log1p log2 log10 rint], [#include <math.h>])
R_CHECK_FUNCS([getpwuid], [#include <pwd.h>])
R_CHECK_FUNCS([sigaction sigaltstack sigemptyset], [#include <signal.h>])
R_CHECK_FUNCS([va_copy __va_copy], [#include <stdarg.h>])
R_CHECK_FUNCS([fdopen popen], [#include <stdio.h>])
R_CHECK_FUNCS([setenv system unsetenv], [#include <stdlib.h>])
R_CHECK_FUNCS([strcoll strerror], [#include <string.h>])
R_CHECK_FUNCS([getrlimit getrusage], [#include <sys/resource.h>])
R_CHECK_FUNCS([mkfifo stat], [#include <sys/stat.h>])
R_CHECK_FUNCS([gettimeofday], [#include <sys/time.h>])
R_CHECK_FUNCS([times], [#include <sys/times.h>])
R_CHECK_FUNCS([time], [#include <time.h>])
R_CHECK_FUNCS([access chdir execv ftruncate getcwd getuid symlink sysconf],
[#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif])
## We really need putenv, and make check fails without it so it seems
## everyone has it, possibly not declared.
AC_CHECK_FUNCS(putenv)
AC_CHECK_DECLS([putenv], , , [#include <stdlib.h>])
## this is a GNU extension so usually hidden
AC_CHECK_FUNCS(vasprintf)
AC_CHECK_DECLS([vasprintf], , , [#include <stdio.h>])
## this is a BSD extension. Some systems have it in unistd.h.
AC_CHECK_FUNCS(realpath)
AC_CHECK_DECLS([realpath], , , [#include <stdlib.h>
#include <unistd.h>])

R_CHECK_FUNCS([glob], [#ifdef HAVE_GLOB_H
# include <glob.h>
#endif])

## we can't use dir() or unlink(recursive=TRUE) without these
if test "${ac_cv_have_decl_stat}" = "no"; then
   AC_MSG_ERROR([Building R requires the 'stat' system call])
fi
if test "${ac_cv_search_opendir}" = "no"; then
   AC_MSG_ERROR([Building R requires the 'opendir' system call])
fi


## <NOTE>
## No need checking for bcopy bzero memcpy mempcpy even though ifnames
## might report corresponding HAVE_FOO conditionals.
## </NOTE>


if test $ac_cv_type_off_t=yes -a $ac_cv_func_fseeko=yes -a $ac_cv_func_ftello=yes; then
   AC_DEFINE(HAVE_OFF_T, 1,
            [Define if you have off_t, fseeko and ftello.]) 
fi

## IEEE 754. We rely on this in e.g. the working log test.
R_IEEE_754

## check if putenv can substitute for unsetenv
R_PUTENV_AS_UNSETENV


## check whether nl_langinfo(CODESET) is in langinfo.h
## defines HAVE_LANGINFO_CODESET if it's there
AM_LANGINFO_CODESET

## Used to build src/include/Rmath.h.
## <NOTE>
## we don't use AC_CONFIG_HEADERS on Rmath.h.in because
## a) that would comment out #undef statements in Rmath.h.in and
## b) Rmath.h should be a self-contained file for standalone Rmath use.
## </NOTE>
if test "${ac_cv_have_decl_expm1}" = yes; then
  RMATH_HAVE_EXPM1="# define HAVE_EXPM1 1"
else
  RMATH_HAVE_EXPM1="# undef HAVE_EXPM1"
fi
AC_SUBST(RMATH_HAVE_EXPM1)
if test "${ac_cv_have_decl_log1p}" = yes; then
  RMATH_HAVE_LOG1P="# define HAVE_LOG1P 1"
else
  RMATH_HAVE_LOG1P="# undef HAVE_LOG1P"
fi
AC_SUBST(RMATH_HAVE_LOG1P)

## Do we need substitutes?
AC_REPLACE_FUNCS(acosh asinh atanh mkdtemp snprintf strdup strncasecmp vsnprintf)
## Enable declarations in Defn.h?
AC_CHECK_DECLS([acosh, asinh, atanh], , , [#include <math.h>])
AC_CHECK_DECLS([mkdtemp, snprintf, strdup, strncasecmp, vsnprintf])

AC_SEARCH_LIBS(connect, [socket])
AC_SEARCH_LIBS(gethostbyname, [nsl socket])
AC_SEARCH_LIBS(xdr_string, [nsl])
R_FUNC___SETFPUCW
R_FUNC_CALLOC
if test "${ac_cv_func_finite}" = "yes"; then
R_FUNC_FINITE
fi
if test "${ac_cv_have_decl_isfinite}" = "yes"; then
R_FUNC_ISFINITE
fi
## check value of log(0) and log(-1)
## Solaris defines log(0) to be -HUGE_VAL, but we want -Inf
R_FUNC_LOG
## check accuracy of log1p
R_FUNC_LOG1P
R_FUNC_FTELL
R_FUNC_SIGACTION

R_C99_COMPLEX

## check for vecLib framework (potentially to be used for BLAS)
## in theory vecLib is platform-independent, in practice only
## Apple's Mac OS X is known to provide it
R_CHECK_FRAMEWORK(cblas_cdotu_sub, vecLib)

## BLAS.
## <NOTE>
## This has to come *after* checking for Fortran 77 compiler/converter
## characteristics (notably name mangling and FLIBS).
## </NOTE>

if test "${use_blas}" = yes; then
  ## may acx_blas_ok to yes
  R_BLAS_LIBS
fi

if test "${acx_blas_ok}" != "yes"; then
  case "${host_os}" in
    darwin[[1-7]]*|aix*)
      ;;
    *)
      if test "${use_blas_shlib}" = "unset"; then
        use_blas_shlib="yes"
      fi
      ;;
  esac
fi

AM_CONDITIONAL(BLAS_SHLIB, [test "x${use_blas_shlib}" = xyes])

case "${host_os}" in
  darwin*)
    ## In order to allow the R build to be relocatable, we strip paths
    ## from all shlibs and rely on DYLD_LIBRARY_PATH. Unfortunately
    ## Darwin linker ignores it at build-time and doesn't use -L to
    ## resolve dylib dependencies, so libRblas will not be found unless
    ## we tell ld where it lives. I don't know of any more elegant solution :/
    if test "x${use_blas_shlib}" = xyes; then
      LIBR="${LIBR} -dylib_file libRblas.dylib:\$(R_HOME)/lib\$(R_ARCH)/libRblas.dylib"
    fi
  ;;
esac
AC_SUBST(LIBR)

## This version is used to build a shared BLAS lib
BLAS_LIBS0=${BLAS_LIBS}

## external BLAS + shared BLAS lib = we need to pass symbols through
## this may require some magic
if test "${acx_blas_ok}" = yes -a "${use_blas_shlib}" = yes; then
  case "${host_os}" in
    darwin*)
    ## test whether we can see symbols through the proxy BLAS library
    ## this test could be modified to not be Darwin-specific,
    ## however the fix is darwin-specific
    if test "${r_cv_prog_f77_append_underscore}" = yes; then
      dgemm=dgemm_
      xerbla=xerbla_
    else
      dgemm=dgemm
      xerbla=xerbla
    fi
    AC_MSG_CHECKING([whether external BLAS is visible through libRblas])
    AC_CACHE_VAL([r_cv_blas0_passthrough],[
[cat > conftestl.c <<EOF
  void ${dgemm}();
  void dummy() { ${dgemm}(); }
EOF]
echo "${CC} ${CFLAGS} conftestl.c ${SHLIB_LDFLAGS} -o libconftest${DYLIB_EXT} ${LIBS} ${BLAS_LIBS}" >&AS_MESSAGE_LOG_FD
${CC} ${CFLAGS} conftestl.c ${SHLIB_LDFLAGS} -o libconftest${DYLIB_EXT} ${LIBS} ${BLAS_LIBS} 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD
[cat > conftest.c <<EOF
void ${dgemm}();
void ${xerbla}(char *srname, int *info){};
int main(int argc, char **argv) { if (argc<0) ${dgemm}(); return 0; }
EOF]
if ${CC} ${CFLAGS} -c conftest.c 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD; then
  if ${CC} ${LDFLAGS} -o conftest${ac_exeext} \
       conftest.${ac_objext} -L. -lconftest \
       1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD;
  then
    ## redirect error messages to config.log
    output=`./conftest${ac_exeext} 2>&AS_MESSAGE_LOG_FD`
    if test ${?} = 0; then
	r_cv_blas0_passthrough=yes
    fi
  fi
fi
])
if test -n "${r_cv_blas0_passthrough}"; then
r_cv_blas0_passthrough=yes
AC_MSG_RESULT([yes])
else
AC_MSG_RESULT([no])
AC_MSG_CHECKING([can it be fixed by using -sub_umbrella])
ac_test_BLAS_LIBS=`echo "${BLAS_LIBS}"|sed -e s/-framework/-sub_umbrella/`
rm -f libconftest.dylib
echo "${CC} ${CFLAGS} conftestl.c ${SHLIB_LDFLAGS} -o libconftest${DYLIB_EXT} ${LIBS} ${BLAS_LIBS} ${ac_test_BLAS_LIBS}" >&AS_MESSAGE_LOG_FD
${CC} ${CFLAGS} conftestl.c ${SHLIB_LDFLAGS} -o libconftest${DYLIB_EXT} ${LIBS} ${BLAS_LIBS} ${ac_test_BLAS_LIBS} 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD
if ${CC} ${CFLAGS} -c conftest.c 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD; then
  if ${CC} ${LDFLAGS} -o conftest${ac_exeext} \
       conftest.${ac_objext} -L. -lconftest \
       1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD;
  then
    ## redirect error messages to config.log
    output=`./conftest${ac_exeext} 2>&AS_MESSAGE_LOG_FD`
    if test ${?} = 0; then
        r_cv_blas0_passthrough=yes
    fi
  fi
fi
if test -n "${r_cv_blas0_passthrough}"; then
r_cv_blas0_passthrough=yes
AC_MSG_RESULT([yes])
BLAS_LIBS0="${BLAS_LIBS} ${ac_test_BLAS_LIBS}"
else
AC_MSG_RESULT([no])
AC_MSG_ERROR([Cannot build Rblas shared library such that it makes external BLAS visible.
An alternative is to use internal BLAS instead and replace
libRblas.dylib with the external BLAS library after R is built.])
fi
fi
    rm -f conftest.c conftest.o conftestl.c libconftest.dylib
    ;;
  esac
fi
AC_SUBST(BLAS_LIBS0)

if test "${use_blas_shlib}" = yes; then
  ## set BLAS_LIBS to point at local version
  BLAS_LIBS="-L\$(R_HOME)/lib\$(R_ARCH) -lRblas"
fi

AM_CONDITIONAL(USE_VECLIB_G95FIX, [test "x${use_veclib_g95fix}" = xyes])
AM_CONDITIONAL(USE_EXTERNAL_BLAS, [test "${acx_blas_ok}" = "yes"])

## LAPACK.
## The default has already been set on MacOS X: otherwise it is "no"
## and so this test fails.
if test "${use_lapack}" = "yes"; then
  R_LAPACK_LIBS
fi
if test "${acx_lapack_ok}" != "yes"; then
  LAPACK_LIBS="-L\$(R_HOME)/lib\$(R_ARCH) -lRlapack"
fi
AC_SUBST(LAPACK_LIBS)
AM_CONDITIONAL(USE_EXTERNAL_LAPACK, [test "${acx_lapack_ok}" = "yes"])

### * Checks for system services.

AC_SUBST(R_MODULES)

## iconv headers and function.
if test "${use_iconv}" = yes; then
  R_ICONV
  if test "$r_cv_iconv_latin1" != yes; then
    AC_MSG_ERROR([--with-iconv=yes (default) and a suitable iconv is not available])
  fi
else
    AC_MSG_WARN([--with-iconv=no is deprecated and will be withdrawn shortly])
fi

## support for MBCS
case "${host_os}" in
  mingw*)
    r_cv_iconv_latin1=yes
    ;;
esac
R_MBCS

## X11.
R_X11
if test "${use_X11}" = yes; then
  R_MODULES=X11
else
  R_MODULES=
fi
## check if X11 typedefs KeySym
R_TYPE_KEYSYM
## check if Xmu is supported
R_X11_Xmu

## check for CoreFoundation framework (chances are much higher
## that we can build AQUA if this one is present)
R_CHECK_FRAMEWORK(CFStringGetSystemEncoding, CoreFoundation)

## Aqua
R_AQUA
AM_CONDITIONAL(BUILD_AQUA, [test "x${use_aqua}" = xyes])
AC_SUBST(use_aqua)

## Tcl/Tk.
R_TCLTK

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

## PCRE headers and libraries.
R_PCRE
 
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

## R profiling.
if test "${want_memory_profiling}" = yes; then
  AC_DEFINE(R_MEMORY_PROFILING, 1, [Define this to enable memory profiling.])
fi

## Large-file-support
AC_SYS_LARGEFILE
AC_FUNC_FSEEKO

## Valgrind instrumentation
if test ${valgrind_level} -eq 0; then
  AC_DEFINE(NVALGRIND, 1, [Define to disable Valgrind instrumentation])
fi
AC_DEFINE_UNQUOTED(VALGRIND_LEVEL, ${valgrind_level}, [Define as 1 or 2 to specify levels of Valgrind instrumentation])

## KERN_USRSTACK support (BSD, Darwin, ...)
R_KERN_USRSTACK

## check for visible __libc_stack_end on Linux
case "${host_os}" in
  linux*)
    AC_CACHE_CHECK([for visible __lib_stack_end], 
    [r_cv_libc_stack_end],
    [AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include "confdefs.h"
#include <stdlib.h>
extern void * __libc_stack_end;

int main () {
    exit(0);
}
  ]])], [r_cv_libc_stack_end=yes], [r_cv_libc_stack_end=no], 
    [r_cv_libc_stack_end=no])])

  if test "${r_cv_libc_stack_end}" = yes; then
    AC_DEFINE(HAVE_LIBC_STACK_END, 1, [Define if __libc_stack_end is visible.])
  fi
esac

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
: ${PAPERSIZE=a4}
papersize=`${PAPERCONF}`
test -z "${papersize}" && papersize="${PAPERSIZE}"
: ${R_PAPERSIZE="${papersize}"}
AC_SUBST(R_PAPERSIZE)

## Saving.
AC_SUBST(R_BATCHSAVE)

## Java support
R_JAVA

## F90/F95 support
AC_PROG_FC()
AC_LANG_PUSH(Fortran)
AC_FC_SRCEXT(f90, [dummy=1], [dummy=0])
AC_FC_SRCEXT(f95, [dummy=1], [dummy=0])
AC_LANG_POP()
: ${SHLIB_FCLD="${FC}"}
: ${SHLIB_FCLDFLAGS="${SHLIB_LDFLAGS}"}
AC_SUBST(SHLIB_FCLD)
AC_SUBST(SHLIB_FCLDFLAGS)

## Look for FCPICFLAGS
## Debian in their wisdom have f95 as a link to gfortran, so
## use this to pick out gfortran (even though it is unreliable).
if test "${ac_cv_fc_compiler_gnu}" = yes; then
  case "${host_cpu}" in
    sparc*|ppc64|powerpc64|s390*)
      fcpicflags="-fPIC"
      ;;
    *)
      fcpicflags="-fpic"
      ;;
  esac
fi
case "${host_os}" in
  darwin*)
    fcpicflags="-fno-common"
    ;;
  hpux*)
    case "${FC}" in
      f90)
        fcpicflags="+Z"
        ;;
    esac
    ;;
  linux*)
    case  "${FC}" in
      ## Intel compilers: probably get identified as GNU, but make sure.
      *ifc|*ifort)
        fcpicflags="-fpic"
        ;;
      ## Portland Group
      *pgf95|*pgf90)
        fcpicflags="-fpic"
        ;;
    esac
    ;;
  solaris*)
    if test "${ac_cv_fc_compiler_gnu}" = yes; then
      fcpicflags="-fPIC"
    else
      fcpicflags="-PIC"
    fi
    ;;
esac
: ${FCPICFLAGS="${fcpicflags}"}
if test -z "${FCPICFLAGS}"; then
  case "${host_os}" in
    aix*|irix*|mingw*|osf*)
      ;;
    *)
      AC_MSG_WARN([I could not determine FCPICFLAGS.])
      ;;
  esac
fi


## Make sure -L terms come first in LIBS.
LIBS1=""
LIBS2=""
for arg in ${LIBS}; do
  case "${arg}" in
    -L*)
      R_SH_VAR_ADD(LIBS1, [${arg}])
      ;;
    *)
      R_SH_VAR_ADD(LIBS2, [${arg}])
      ;;
  esac
done
LIBS="${LIBS1} ${LIBS2}"

## R_LD_LIBRARY_PATH.
## <FIXME>
## This should not be necessary when using libtool for linking.
## In any case, is this really good enough?
## On Linux, do not add the ld.so system directories '/lib' and
## '/usr/lib', so that the entries from '/etc/ld.so.conf' are not
## shadowed (otherwise, e.g. optimized ATLAS libs would not be used).
## On OS X (Darwin) /usr/X11R6/lib shadows OpenGL framework and is not
## needed in DYLD_LIBRARY_PATH.
case "${host_os}" in
  linux*)
    r_ld_library_defaults="/usr/lib64:/lib64:/usr/lib:/lib"
    ;;
  solaris*)
    r_ld_library_defaults="/usr/lib:/lib"
    ;;
  darwin*)
    r_ld_library_defaults="/usr/X11R6/lib"
    ;;
  *)
    r_ld_library_defaults=
    ;;
esac
for arg in ${LDFLAGS} ${FLIBS} ${BLAS_LIBS} ${LAPACK_LIBS} ${X_LIBS} \
           ${TCLTK_LIBS}; do
  case "${arg}" in
    -L*)
      lib=`echo ${arg} | sed "s/^-L//"`
      r_want_lib=true
      ## don't add anything for Darwin
      case "${host_os}" in darwin*) r_want_lib=false ;; esac
      ## Do not add non-existent directories.
      test -d "${lib}" || r_want_lib=false
      if test x"${r_want_lib}" = xtrue; then
        ## Canonicalize (/usr/lib/gcc-lib/i486-linux/3.3.4/../../..).
        lib=`cd "${lib}" && ${GETWD}`
        ## Do not add something twice, or default paths.
        r_save_IFS="${IFS}"; IFS="${PATH_SEPARATOR}"
        for dir in ${R_LD_LIBRARY_PATH}${IFS}${r_ld_library_defaults}; do
          if test x"${dir}" = x"${lib}"; then
            r_want_lib=false
            break
          fi
        done
        IFS="${r_save_IFS}"
        if test x"${r_want_lib}" = xtrue; then
          R_SH_VAR_ADD(R_LD_LIBRARY_PATH, [${lib}], [${PATH_SEPARATOR}])
        fi
      fi
      ;;
  esac
done
## Handle JAVA_LD_LIBRARY_PATH specially so that users can conveniently set just
## R_JAVA_LD_LIBRARY_PATH in their environment.
if test -n "${JAVA_LD_LIBRARY_PATH}"; then
   args=`echo ${JAVA_LD_LIBRARY_PATH} | sed -e s:\$\(JAVA_HOME\):${JAVA_HOME}:g`
   r_save_IFS="${IFS}"; IFS="${PATH_SEPARATOR}"
   for lib in ${args}; do
      r_want_lib=true
      ## Do not add non-existent directories.
      test -d "${lib}" || r_want_lib=false
      if test x"${r_want_lib}" = xtrue; then
        ## do NOT Canonicalize (/usr/lib/j2sdk1.5-sun/jre/../lib/i386).
	## because we may lose the reference to ${JAVA_HOME} if it is a symlink (it often is)
        ##lib=`cd "${lib}" && ${GETWD}`
        ## Do not add something twice, or something already in
        ## R_LD_LIBRARY_PATH or the defaults.
        for dir in ${R_JAVA_LD_LIBRARY_PATH}${IFS}${R_LD_LIBRARY_PATH}${IFS}${r_ld_library_defaults}; do
          if test x"${dir}" = x"${lib}"; then
             r_want_lib=false
             break
          fi
        done
        if test x"${r_want_lib}" = xtrue; then
	  lib=`echo ${lib}|sed -e s:${JAVA_HOME}:\$\{JAVA_HOME\}:g`
          R_SH_VAR_ADD(R_JAVA_LD_LIBRARY_PATH, [${lib}], [${PATH_SEPARATOR}])
        fi
      fi
   done
   IFS="${r_save_IFS}"
fi
## </FIXME>

AC_SUBST(R_LD_LIBRARY_PATH)
AC_SUBST(R_JAVA_LD_LIBRARY_PATH)

## Recommended packages.
if test "${use_recommended_packages}" = yes; then
  R_RECOMMENDED_PACKAGES
fi
AM_CONDITIONAL(USE_RECOMMENDED_PACKAGES,
               [test "x${use_recommended_packages}" = xyes])

# i18n support.  To update to a new version of gettext, run (untested):
# gettextize -f -c --intl
AM_NLS
if test "${USE_NLS}" = "yes"; then
  AM_GNU_GETTEXT_VERSION(0.14.5)
  AM_GNU_GETTEXT(, [need-ngettext], [../extra/intl])
  if test -n "$INTL_MACOSX_LIBS"; then
    XTRA_INTL_CPPFLAGS=-I/System/Library/Frameworks/CoreFoundation.framework/Headers
  fi
else
  USE_INCLUDED_LIBINTL=no
fi
AC_SUBST(XTRA_INTL_CPPFLAGS)

AM_CONDITIONAL(USE_NLS, [test "x${USE_NLS}" = xyes])
AM_CONDITIONAL(BUILD_LIBINTL, [test "x${USE_INCLUDED_LIBINTL}" = xyes])

### * bug fixes
# PR#9375 claims that sh on OSF1 is not POSIX-compliant and that is 
# not a bug in the OS: we add a workaround for such benighted OSes
case "${host_os}" in
  osf*)
    OSF_SH_BUG='"${1+@}"'
  ;;
  *)
    OSF_SH_BUG='"${@}"'
  ;;
esac
AC_SUBST(OSF_SH_BUG)

### * Win32 overrides
case "${host_os}" in
  mingw*)
    AC_DEFINE(HAVE_ICONV, 1)
    AC_DEFINE(HAVE_ICONVLIST, 1)
    AC_DEFINE(HAVE_ICONV_H, 1)
    AC_DEFINE(HAVE_INTERNET, 1)
    AC_DEFINE(HAVE_JPEG, 1)
    AC_DEFINE(HAVE_PNG, 1)
    AC_DEFINE(HAVE_POSIX_SETJMP, 1)
    AC_DEFINE(HAVE_SOCKETS, 1)
    AC_DEFINE(HAVE_TCLTK, 1)
    AC_DEFINE(HAVE_TIMES, 1)
    AC_DEFINE(ICONV_LATIN1, 1)
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
  doc/Makefile
  doc/html/Makefile
  doc/html/search/Makefile
  doc/manual/Makefile
  etc/Makefile
  etc/Makeconf
  etc/Renviron
  etc/ldpaths
  m4/Makefile
  po/Makefile.in 
  share/Makefile
  src/Makefile
  src/appl/Makefile
  src/extra/Makefile
  src/extra/blas/Makefile
  src/extra/bzip2/Makefile
  src/extra/intl/Makefile
  src/extra/pcre/Makefile
  src/extra/xdr/Makefile
  src/extra/zlib/Makefile
  src/include/Makefile
  src/include/Rmath.h0
  src/include/R_ext/Makefile
  src/library/Recommended/Makefile
  src/library/Makefile
  src/library/base/DESCRIPTION
  src/library/base/Makefile
  src/library/datasets/DESCRIPTION
  src/library/datasets/Makefile
  src/library/graphics/DESCRIPTION
  src/library/graphics/Makefile
  src/library/grDevices/DESCRIPTION
  src/library/grDevices/Makefile
  src/library/grDevices/src/Makefile
  src/library/grid/DESCRIPTION
  src/library/grid/Makefile
  src/library/grid/src/Makefile
  src/library/methods/DESCRIPTION
  src/library/methods/Makefile
  src/library/methods/src/Makefile
  src/library/profile/Makefile
  src/library/stats/DESCRIPTION
  src/library/stats/Makefile
  src/library/stats/src/Makefile
  src/library/stats4/DESCRIPTION
  src/library/stats4/Makefile
  src/library/splines/DESCRIPTION
  src/library/splines/Makefile
  src/library/splines/src/Makefile
  src/library/tcltk/DESCRIPTION
  src/library/tcltk/Makefile
  src/library/tcltk/src/Makefile
  src/library/tools/DESCRIPTION
  src/library/tools/Makefile
  src/library/tools/src/Makefile
  src/library/utils/DESCRIPTION
  src/library/utils/Makefile
  src/main/Makefile
  src/modules/Makefile
  src/modules/X11/Makefile
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
  src/scripts/Rprof
  src/scripts/SHLIB
  src/scripts/Sd2Rd
  src/scripts/build
  src/scripts/check
  src/unix/Makefile
  tests/Makefile
  tests/Embedding/Makefile
  tests/Examples/Makefile
  tests/Native/Makefile
  tools/Makefile
])

AC_CONFIG_COMMANDS([stamp-h],
[test -f src/include/stamp-h || echo timestamp > src/include/stamp-h])

### now reset flags
CPPFLAGS=${CPPFLAGS_KEEP}
CFLAGS=${CFLAGS_KEEP}
FFLAGS=${FFLAGS_KEEP}
CXXFLAGS=${CXXFLAGS_KEEP}

AC_OUTPUT

## Summarize configure results.
## <NOTE>
## Doing this via AC_CONFIG_COMMANDS would require explicitly passing all
## configure variables to config.status.
## </NOTE>
r_c_compiler="${CC} ${R_XTRA_CFLAGS} ${CFLAGS}"
r_cxx_compiler="${CXX} ${R_XTRA_CXXFLAGS} ${CXXFLAGS}"
r_f77_compiler="${F77} ${R_XTRA_FFLAGS} ${FFLAGS}"
r_f95_compiler="${FC} ${FCFLAGS}"
r_objc_compiler="${OBJC} ${OBJCFLAGS}"
r_interfaces=
for item in X11 aqua tcltk; do
  if eval "test \${use_${item}} = yes"; then
    R_SH_VAR_ADD(r_interfaces, [${item}], [, ])
  fi
done
r_external_libs=
if test "${use_readline}" = yes; then
  r_external_libs=readline
fi
if test "${acx_blas_ok}" = "yes"; then
  ## Try to figure out which BLAS was used.
  case "${BLAS_LIBS0}" in
    *-latlas*) r_blas=ATLAS ;;
    *-lsgemm*) r_blas=PhiPack ;;
    *sunperf*) r_blas=SunPerf ;;
    *-lessl*)  r_blas=ESSL ;;
    *vecLib*)  r_blas=vecLib ;;
    "")        r_blas=none ;;
    *)         r_blas=generic ;;
  esac
  R_SH_VAR_ADD(r_external_libs, [BLAS(${r_blas})], [, ])
fi
if test "${acx_lapack_ok}" = "yes"; then
  ## Try to figure out which LAPACK was used.
  case "${LAPACK_LIBS}" in
    *sunperf*) r_lapack=SunPerf ;;
    "")        r_lapack="in blas" ;;
    *)         r_lapack=generic ;;
  esac
  R_SH_VAR_ADD(r_external_libs, [LAPACK(${r_lapack})], [, ])
fi
r_capabilities=
if test "${have_png}" = yes; then
  R_SH_VAR_ADD(r_capabilities, [PNG], [, ])
fi
if test "${have_jpeg}" = yes; then
  R_SH_VAR_ADD(r_capabilities, [JPEG], [, ])
fi
if test "${r_cv_iconv_latin1}" = yes; then
  R_SH_VAR_ADD(r_capabilities, [iconv], [, ])
fi
if test "${want_mbcs_support}" = yes; then
  R_SH_VAR_ADD(r_capabilities, [MBCS], [, ])
fi
if test "${USE_NLS}" = yes; then
  R_SH_VAR_ADD(r_capabilities, [NLS], [, ])
fi
r_options=
if test "${want_R_framework}" = yes; then
  R_SH_VAR_ADD(r_options, [framework], [, ])
elif test "${want_R_shlib}" = yes; then
  R_SH_VAR_ADD(r_options, [shared R library], [, ])
fi
if test "${use_blas_shlib}" = yes; then
  R_SH_VAR_ADD(r_options, [shared BLAS], [, ])
fi
if test "${want_R_profiling}" = yes; then
  R_SH_VAR_ADD(r_options, [R profiling], [, ])
fi
if test "${want_memory_profiling}" = yes; then
  R_SH_VAR_ADD(r_options, [memory profiling], [, ])
fi
if test "${use_maintainer_mode}" = yes; then
  R_SH_VAR_ADD(r_options, [maintainer mode], [, ])
fi
if test "${use_strict_barrier}" = yes; then
  R_SH_VAR_ADD(r_options, [strict barrier], [, ])
fi
if test "${want_linux_lfs}" = yes; then
  R_SH_VAR_ADD(r_options, [Linux LFS], [, ])
fi
if test -n "${JAVA}"; then
  R_SH_VAR_ADD(r_options, [Java], [, ])
fi

AC_MSG_RESULT(
[
R is now configured for ${host}

  Source directory:          ${srcdir}
  Installation directory:    ${prefix}

  C compiler:                ${r_c_compiler}
  Fortran 77 compiler:       ${r_f77_compiler}

  C++ compiler:              ${r_cxx_compiler}
  Fortran 90/95 compiler:    ${r_f95_compiler}
  Obj-C compiler:	     ${r_objc_compiler}

  Interfaces supported:      ${r_interfaces}
  External libraries:        ${r_external_libs}
  Additional capabilities:   ${r_capabilities}
  Options enabled:           ${r_options}

  Recommended packages:      ${use_recommended_packages} 
])
if test -n "${warn_f77_cc_double_complex}"; then
  AC_MSG_WARN([${warn_f77_cc_double_complex}])
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
if test -n "${warn_cxxpicflags}"; then
  AC_MSG_WARN([${warn_cxxpicflags}])
fi
if test -n "${warn_fcpicflags}"; then
  AC_MSG_WARN([${warn_fcpicflags}])
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
if test -n "${warn_browser}"; then
  AC_MSG_WARN([${warn_browser}])
fi
if test -n "${warn_pdfviewer}"; then
  AC_MSG_WARN([${warn_pdfviewer}])
fi

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
