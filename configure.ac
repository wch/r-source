### configure.ac					-*- Autoconf -*-
###
### Process this file with autoconf to produce a configure script.
###
### Copyright (C) 1998-2015 R Core Team
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
### along with R; if not, a copy is available at
### http://www.r-project.org/Licenses/

AC_PREREQ(2.62)

## We want to get the version number from file 'VERSION' (rather than
## specifying the version info in 'configure.ac'.  Hence, we need a bit
## of M4 magic.  Note that M4 esyscmd has a trailing newline because the
## shell command output has one, hence the patsubst() trick.
m4_define([R_VERSION],
          m4_bpatsubst(m4_esyscmd([cat VERSION]),
                       [\([0-9.]*\)\(\w\|\W\)*],
                       [\1]))
AC_INIT([R],[R_VERSION],[https://bugs.r-project.org],[R],[https://www.r-project.org])
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

## The gettext macros call AC_GNU_SOURCE early, so all the
## C compiling makes use of that.  If it is ever removed, we
## would need it or something similar here.


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

## exclude some unsupported OSes
case "${host_os}" in
   ## Darwin 1.3.1 was OS X 10.0, 1.4.1 was 10.1, 5 is 10.2 etc
   ## with 13 being 10.9.  We no longer support < 10.4 (Tiger)
   ## http://en.wikipedia.org/wiki/Darwin_OS
   darwin1.*)
     AC_MSG_ERROR([The earliest supported OS X is 10.4.]
     ;; 
  darwin[[567]]*)
     AC_MSG_ERROR([The earliest supported OS X is 10.4.]
     ;; 
  aix[123]*|aix4.[01]*)
    ## These need a form of linking we no longer support
    AC_MSG_ERROR([AIX prior to 4.2 is not supported])
    ;;
esac

R_PLATFORM="${host}"
AC_SUBST(R_PLATFORM)
R_OS="${host_os}"
AC_SUBST(R_OS)

case "${host_os}" in
  mingw*|windows*|winnt)
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

R_CONFIG_ARGS="${ac_configure_args}"
AC_SUBST(R_CONFIG_ARGS)

### ** Defaults.

## NB: autoconf loads such files too
cfile="${srcdir}/config.site"
if test -r "${cfile}"; then
  echo "loading site script '${cfile}'"
  . "${cfile}"
fi
cfile="${HOME}/.R/config"
if test -r "${cfile}"; then
  echo "loading user script '${cfile}'"
  . "${cfile}"
fi
cfile="./config.site"
if test -r "${cfile}"; then
  echo "loading build-specific script '${cfile}'"
  . "${cfile}"
fi

## We need to establish suitable defaults for a 64-bit OS
libnn=lib
case "${host_os}" in
  linux*)
    ## Not all distros use this: some choose to march out of step  
    case "${host_cpu}" in
      x86_64|mips64|ppc64|powerpc64|sparc64|s390x)
        if test -d /usr/lib64; then
          libnn=lib64
	fi
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
[AS_HELP_STRING([--enable-R-profiling],[attempt to compile support for Rprof() @<:@yes@:>@])],
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
[AS_HELP_STRING([--enable-memory-profiling],[attempt to compile support for Rprofmem(), tracemem() @<:@no@:>@])],
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
[AS_HELP_STRING([--enable-R-framework@<:@=DIR@:>@],[OS X only: build R framework (if possible), and specify
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

## Allow the user to specify building R as a shared library.
## (but a 'dynamic library' in the terminology of OS X).
## <NOTE>
## Building a framework implies building R shared libraries, hence the
## strange default.
## We might want to warn about the case where '--disable-R-shlib' was
## given explicitly ...
## </NOTE>
AC_ARG_ENABLE([R-shlib],
[AS_HELP_STRING([--enable-R-shlib],[build the shared/dynamic library 'libR' @<:@no@:>@])],
[want_R_shlib="${enableval}"],
[want_R_shlib="${want_R_framework}"])
AM_CONDITIONAL(WANT_R_SHLIB, [test "x${want_R_shlib}" = xyes])

AC_ARG_ENABLE([R-static-lib],
[AS_HELP_STRING([--enable-R-static-lib],[build the static library 'libR.a' @<:@no@:>@])],
[want_R_static="${enableval}"],
[want_R_static="no"])
if test "x${want_R_static}" = xyes; then
  if test "x${want_R_shlib}" = xyes; then
    AC_MSG_WARN([--enable-R-static-lib conflicts with --enable-R-shlib and will be ignored])
    want_R_static=no
  fi
fi
AM_CONDITIONAL(WANT_R_STATIC, [test "x${want_R_static}" = xyes])

## Build separate shared/dynamic library containing R's BLAS if desired 
AC_ARG_ENABLE([BLAS-shlib],
[AS_HELP_STRING([--enable-BLAS-shlib],[build BLAS into a shared/dynamic library @<:@perhaps@:>@])],
[use_blas_shlib="${enableval}"],
[use_blas_shlib="unset"])

## As from R 3.2.0 split up -L... and -lR
if test "${want_R_shlib}" = yes; then
  LIBR0="-L\$(R_HOME)/lib\$(R_ARCH)"
  LIBR1=-lR
else
  LIBR0=
  LIBR1=
fi

## Enable maintainer-specific portions of Makefiles.
AC_ARG_ENABLE([maintainer-mode],
[AS_HELP_STRING([--enable-maintainer-mode],[enable make rules and dependencies not useful (and
                 maybe confusing) to the casual installer @<:@no@:>@])],
[use_maintainer_mode="${enableval}"],
[use_maintainer_mode=no])
AM_CONDITIONAL(MAINTAINER_MODE, [test "x${use_maintainer_mode}" = xyes])

## Enable testing the write barrier.
AC_ARG_ENABLE([strict-barrier],
[AS_HELP_STRING([--enable-strict-barrier],[provoke compile error on write barrier violation
                 @<:@no@:>@])],
[use_strict_barrier="${enableval}"],
[use_strict_barrier=no])
if test x"${use_strict_barrier}" = xyes; then
  AC_DEFINE(TESTING_WRITE_BARRIER, 1,
            [Define to enable provoking compile errors on write barrier
             violation.])
fi

AC_ARG_ENABLE([prebuilt-html],
[AS_HELP_STRING([--enable-prebuilt-html],[build static HTML help pages @<:@no@:>@])],
[want_prebuilt_html="${enableval}"],
[want_prebuilt_html=no])
AM_CONDITIONAL(BUILD_HTML, [test "x${want_prebuilt_html}" = xyes])

AC_ARG_ENABLE([lto],
[AS_HELP_STRING([--enable-lto],[enable link-time optimization @<:@no@:>@])],
[want_lto="${enableval}"], [want_lto=no])
## FIXME: add a test for gcc >= 4.5.0
if test "x${want_lto}" != xno; then
  LTO=-flto
fi
if test "x${want_lto}" == xyes; then
  LTOALL=-flto
fi
AC_SUBST(LTO)
AC_SUBST(LTOALL)
AM_CONDITIONAL(BUILD_LTO, [test "x${want_lto}" != xno])

AC_ARG_ENABLE([java],
[AS_HELP_STRING([--enable-java],[enable Java @<:@yes@:>@])],
[want_java="${enableval}"],
[want_java=yes])
AM_CONDITIONAL(WANT_JAVA, [test "x${want_java}" = xyes])

### ** Optional packages.

## BLAS.
AC_ARG_WITH([blas],
[AS_HELP_STRING([--with-blas],[use system BLAS library (if available), or specify it @<:@no@:>@])],
[R_ARG_USE(blas)],
[use_blas=unset])
# default is "no" except on OS X

## LAPACK.
AC_ARG_WITH([lapack],
[AS_HELP_STRING([--with-lapack],[use system LAPACK library (if available), or specify it @<:@no@:>@])],
[R_ARG_USE(lapack)],
[use_lapack=unset])
# default is "no" except on OS X

## Readline.
AC_ARG_WITH([readline],
[AS_HELP_STRING([--with-readline],[use readline library @<:@yes@:>@])],
[R_ARG_USE(readline)],
[use_readline=yes])

## Aqua.
AC_ARG_WITH([aqua],
[AS_HELP_STRING([--with-aqua],[OS X only: use Aqua (if available) @<:@yes@:>@])],
[if test "${withval}" = no; then
  want_aqua=no
else
  want_aqua=yes
fi],
[want_aqua=yes])

## Tcl/Tk.
AC_ARG_WITH([tcltk],
[AS_HELP_STRING([--with-tcltk],[use Tcl/Tk (if available), or specify its library dir @<:@yes@:>@])],
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
[AS_HELP_STRING([--with-tcl-config=TCL_CONFIG],[specify location of tclConfig.sh @<:@@:>@])],
[TCL_CONFIG="${withval}"],
[TCL_CONFIG=""])
AC_ARG_WITH([tk-config],
[AS_HELP_STRING([--with-tk-config=TK_CONFIG],[specify location of tkConfig.sh @<:@@:>@])],
[TK_CONFIG="${withval}"],
[TK_CONFIG=""])

## cairographics etc
AC_ARG_WITH([cairo],
[AS_HELP_STRING([--with-cairo],[use cairo (and pango) if available @<:@yes@:>@])],
[if test "${withval}" = no; then
  want_cairo=no
else
  want_cairo=yes
fi], [want_cairo=yes])

## other libraries
AC_ARG_WITH([libpng],
[AS_HELP_STRING([--with-libpng],[use libpng library (if available) @<:@yes@:>@])],
[R_ARG_USE(libpng)],
[use_libpng=yes])
AC_ARG_WITH([jpeglib],
[AS_HELP_STRING([--with-jpeglib],[use jpeglib library (if available) @<:@yes@:>@])],
[R_ARG_USE(jpeglib)],
[use_jpeglib=yes])
AC_ARG_WITH([libtiff],
[AS_HELP_STRING([--with-libtiff],[use libtiff library (if available) @<:@yes@:>@])],
[R_ARG_USE(libtiff)],
[use_libtiff=yes])
AC_ARG_WITH([system-tre],
[AS_HELP_STRING([--with-system-tre],[use system tre library (if available) @<:@no@:>@])],
[R_ARG_USE_SYSTEM(tre)],
[use_system_tre=no])

## Valgrind instrumentation
AC_ARG_WITH([valgrind-instrumentation],
[AS_HELP_STRING([--with-valgrind-instrumentation],[Level of additional instrumentation for Valgrind (0/1/2) @<:@0@:>@])],
[valgrind_level=${withval}],
[valgrind_level=0])

AC_ARG_WITH([system-valgrind-headers],
[AS_HELP_STRING([--with-system-valgrind-headers],[use system valgrind headers (if available) @<:@no@:>@])],
[R_ARG_USE_SYSTEM(valgrind)],
[use_system_valgrind=no])

AC_ARG_WITH([internal-tzcode],
[AS_HELP_STRING([--with-internal-tzcode],[use internal time-zone code @<:@no@:>@])],
[use_internal_tzcode=${withval}],
[use_internal_tzcode=default])


## <FIXME>
## Completely disable using libtool for building shlibs until libtool
## fully supports Fortran and C++.
## AC_ARG_WITH([libtool],
## [AS_HELP_STRING([--with-libtool],[use libtool for building shared libraries @<:@yes@:>@])],
## [use_libtool="${withval}"],
## [use_libtool=yes])
## AM_CONDITIONAL(USE_LIBTOOL, [test "x${use_libtool}" = xyes])
## </FIXME>

## Recommended R packages.
AC_ARG_WITH([recommended-packages],
[AS_HELP_STRING([--with-recommended-packages],[use/install recommended R packages @<:@yes@:>@])],
[R_ARG_USE(recommended_packages)],
[use_recommended_packages=yes])

## ICU
AC_ARG_WITH([ICU],
[AS_HELP_STRING([--with-ICU],[use ICU library (if available) @<:@yes@:>@])],
[R_ARG_USE(ICU)],
[use_ICU=yes])

## Byte-compilation of packages.
AC_ARG_ENABLE([byte-compiled-packages],
[AS_HELP_STRING([--enable-byte-compiled-packages],
                [byte-compile base and recommended packages @<:@yes@:>@])],
[want_byte_compiled_packages="${enableval}"],
[want_byte_compiled_packages=yes])
AM_CONDITIONAL(BYTE_COMPILE_PACKAGES,
               [test "x${want_byte_compiled_packages}" = xyes])

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
           [additional CFLAGS used when building shared objects])
AC_ARG_VAR([MAIN_FFLAGS],
           [additional FFLAGS used when compiling the main binary])
AC_ARG_VAR([SHLIB_FFLAGS],
           [additional FFLAGS used when building shared objects])
AC_ARG_VAR([MAIN_LD],
           [command used to link the main binary])
AC_ARG_VAR([MAIN_LDFLAGS],
           [flags which are necessary for loading a main program which
            will load shared objects (DLLs) at runtime])
AC_ARG_VAR([CPICFLAGS],
           [special flags for compiling C code to be turned into a
            shared object.])
AC_ARG_VAR([FPICFLAGS],
           [special flags for compiling Fortran code to be turned into a
            shared object.])
AC_ARG_VAR([FCPICFLAGS],
           [special flags for compiling Fortran 95 code to be turned into a
            shared object.])
AC_ARG_VAR([SHLIB_LD],
           [command for linking shared objects which contain object
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
            shared object])
AC_ARG_VAR([SHLIB_CXXLD],
           [command for linking shared objects which contain object
            files from the C++ compiler])
AC_ARG_VAR([SHLIB_CXXLDFLAGS],
           [special flags used by SHLIB_CXXLD])
AC_ARG_VAR([SHLIB_FCLD],
           [command for linking shared objects which contain object
            files from the Fortran 95 compiler])
AC_ARG_VAR([SHLIB_FCLDFLAGS],
           [special flags used by SHLIB_FCLD])
AC_ARG_VAR([TCLTK_LIBS],
           [flags needed for linking against the Tcl and Tk libraries])
AC_ARG_VAR([TCLTK_CPPFLAGS],
           [flags needed for finding the tcl.h and tk.h headers])
AC_ARG_VAR([MAKE], [make command])
AC_ARG_VAR([TAR], [tar command])
AC_ARG_VAR([R_BROWSER], [default browser])
AC_ARG_VAR([R_PDFVIEWER], [default PDF viewer])
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
AC_ARG_VAR([R_SHELL],
           [shell to be used for shell scripts, including 'R'])

if test -z "${r_arch}"; then
  R_ARCH=
  R_XTRA_CPPFLAGS2="-I\$(R_INCLUDE_DIR)"
else
  R_ARCH="/${r_arch}"
  R_XTRA_CPPFLAGS2="-I\$(R_INCLUDE_DIR) -I\$(R_INCLUDE_DIR)/${r_arch}"
fi
AC_DEFINE_UNQUOTED(R_ARCH, "${r_arch}",
[Define this to use architecture-dependent subdirectories of this name.])
AC_SUBST([R_ARCH])
AC_SUBST([R_XTRA_CPPFLAGS2])

### ** Check whether we build in srcdir.

AC_PATH_PROG(GETWD, pwd, pwd)
AC_MSG_CHECKING([whether builddir is srcdir])
if test "`cd \"${srcdir}\" && ${GETWD}`" = "`${GETWD}`"; then
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
AC_PROG_LN_S
AC_PROG_YACC
R_PROG_AR
R_PROG_INSTALL

## we would like a POSIX sed, and need one on Solaris
AC_PATH_PROGS(SED, sed, /bin/sed, [/usr/xpg4/bin:$PATH])
## 'which' is not POSIX, and might be a shell builtin or alias
##  (but should not be in 'sh')
AC_PATH_PROGS(WHICH, which, which)
## Make
: ${MAKE=make}
AC_SUBST(MAKE)
## Pager
R_PROG_PAGER
## Tar -- we prefer a GNU version
AC_PATH_PROGS(TAR, [${TAR} gtar gnutar tar], "")
## TeXMF stuff
R_PROG_TEXMF
## Unzip && zip && gzip && bip2
AC_PATH_PROGS(R_UNZIPCMD, [${UNZIP} unzip], "")
AC_PATH_PROGS(R_ZIPCMD, [${ZIP} zip], "")
AC_PATH_PROGS(R_GZIPCMD, [${GZIP} gzip], true)
AC_PATH_PROGS(R_BZIPCMD, [${BZIP} bzip2], "")
## Browser
R_PROG_BROWSER
## PDF viewer
R_PROG_PDFVIEWER
## Noweb
AC_PATH_PROG(NOTANGLE, notangle, false)
## javareconf needs this
AC_PATH_PROG(REALPATH, realpath, false)


## cairographics needs pkg-config
AC_PATH_PROG(PKGCONF, pkg-config , [], 
	     [$PATH:/usr/local/bin:/ext/bin:/ext:/sw/bin:/opt/bin])

AC_PROG_CC

AC_PROG_GCC_TRADITIONAL
AC_PROG_CPP
R_PROG_CPP_CPPFLAGS
## see if the user set FFLAGS: used for Intel compilers
userFFLAGS=${FFLAGS}
R_PROG_F77
AC_PROG_CXX
AC_PROG_CXXCPP
if test "${CXXCPP}" = "${CXX} -E"; then
  CXXCPP0="\$(CXX) -E"
else
  CXXCPP0=${CXXCPP}
fi
AC_SUBST(CXXCPP0)
R_GCC4_VISIBILITY
AC_GNU_SOURCE ## needed for libintl

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
LT_INIT
AC_SUBST(LIBTOOL_DEPS)

## cross-compiling: added May 2007, not actually used
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
## OTOH, R_LD_LIBRARY_PATH in the environment is meant to be the final version.
R_LD_LIBRARY_PATH_save=${R_LD_LIBRARY_PATH}
R_LD_LIBRARY_PATH=
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

## Record name of environment variable which tells the dynamic linker
## where to find shlibs (typically, 'LD_LIBRARY_PATH').
## Used in etc/ldpaths: override what libtool thinks on OS X
## http://hublog.hubmed.org/archives/001192.html suggests this was in 10.4
case "${host_os}" in
  darwin*)
    Rshlibpath_var=DYLD_FALLBACK_LIBRARY_PATH
    ;;
  *)
    Rshlibpath_var=${shlibpath_var}
esac
AC_SUBST(shlibpath_var)
## Export LD_LIBRARY_PATH or equivalent.
if eval "test -z \"\${${Rshlibpath_var}}\""; then
  eval "${Rshlibpath_var}=\"${R_LD_LIBRARY_PATH}\""
else
  eval "${Rshlibpath_var}=\"${R_LD_LIBRARY_PATH}${PATH_SEPARATOR}\${${Rshlibpath_var}}\""
fi
eval "export ${Rshlibpath_var}"
AC_SUBST(Rshlibpath_var)

## record how to strip shared/dynamic libraries.
AC_SUBST(striplib)
## record how to strip static libraries.
stripstaticlib=${old_striplib}
AC_SUBST(stripstaticlib)

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
    ## OS X <= 10.2 dlcompat, >= 10.3 included dlcompat in libSystem
    AC_SEARCH_LIBS(dlopen, dl,,[AC_MSG_ERROR([Your OS X is too old.])])
    ## SI says we want '-lcc_dynamic' on Darwin, although currently
    ## http://developer.apple.com/documentation/MacOSX/ has nothing
    ## official.  Bill Northcott <w.northcott@unsw.edu.au> points out
    ## that it is only needed for GCC 3.x (and earlier) ...
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
  ## the NetBSD version as used in OS X does not have this
  AC_CHECK_FUNCS(history_truncate_file)
  if test "${use_readline}" = no; then
    AC_MSG_ERROR([--with-readline=yes (default) and headers/libs are not available])
  else
    R_CHECK_FUNCS([rl_completion_matches], 
[#include <stdio.h>
#include <readline/readline.h>]
                 )
  fi
  READLINE_LIBS="${LIBS}"
  LIBS="${r_save_LIBS}"
fi
AC_SUBST(READLINE_LIBS)

### * Checks for header files.

AC_HEADER_STDC
AC_HEADER_DIRENT
## we also assume readdir and closedir
if test "${ac_cv_search_opendir}" = "no"; then
   AC_MSG_ERROR([Building R requires the 'opendir' system call])
fi
AC_HEADER_SYS_WAIT
## <NOTE>
## Some of these are also checked for when Autoconf computes the default
## includes.
##
## The following headers are POSIX: dlfcn.h fcntl.h glob.h grp.h
## pwd.h strings.h sys/resource.h sys/select.h sys/socket.h 
## sys/stat.h sys/time.h sys/times.h sys/utsname.h unistd.h utime.h
## dl.h seems to be for HP-UX
## floatingpoint.h is on FreeBSD and Solaris, originating in SysV.  
## (It is probably only used for fpsetmask on FreeBSD).
## fpu_control.h is only used on Linux, for the obsolete __setfpucw.
## sys/param.h is one way to get PATH_MAX.
## sched.h is POSIX, but we use it for Linux-specific features (affinity)
## stdalign.h is C11.
AC_CHECK_HEADERS(arpa/inet.h dl.h dlfcn.h elf.h features.h fcntl.h \
  floatingpoint.h fpu_control.h glob.h grp.h langinfo.h \
  netdb.h netinet/in.h pwd.h sched.h strings.h \
  sys/param.h sys/resource.h sys/select.h sys/socket.h \
  sys/stat.h sys/time.h sys/times.h sys/utsname.h unistd.h utime.h)
## </NOTE>
## stdalign.h is C11.
AC_CHECK_HEADERS(stdalign.h)
## <NOTE>
## These are C99 headers but some C code (written to work also
## without assuming C99) may need the corresponding conditionals.
AC_CHECK_HEADERS(errno.h inttypes.h limits.h locale.h stdarg.h stdbool.h \
                 stdint.h string.h)
## only vsnprintf.c requires stdarg.h
## </NOTE>

R_HEADER_SETJMP
R_HEADER_GLIBC2

### * Checks for types.

AC_TYPE_SIGNAL
## xz needs uint64_t
AC_TYPE_UINT64_T
AC_CHECK_TYPES([int64_t, int_fast64_t])
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

R_BIGENDIAN
AC_C_CONST
R_C_INLINE
AC_CHECK_SIZEOF(int)
## on some platforms this gives a trailing lf, so
case "${ac_cv_sizeof_int}" in
    4*)
    AC_DEFINE(INT_32_BITS, 1, [Define if you have 32 bit ints.])
    ;;
esac
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(long long)
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(size_t)
AC_ARG_ENABLE([long-double],
[AS_HELP_STRING([--enable-long-double],[use long double type @<:@yes@:>@])],
[if test "${enableval}" = no; then
  use_long_double=no
else
  use_long_double=yes
fi],
[use_long_double=yes])

dnl also used in intl.
if test "x${use_long_double}" = xyes; then 
  AC_DEFINE(HAVE_LONG_DOUBLE, 1, [Define if you wish to use the 'long double' type.])
  AC_CHECK_SIZEOF(long double)
else
  AC_MSG_WARN([Not using the 'long double' type])
fi

R_PROG_CC_MAKEFRAG
R_PROG_CC_LO_MAKEFRAG

AC_OPENMP

### *** Fortran 77 compiler.

R_PROG_F77_FLIBS
if test -z "$FLIBS_IN_SO"; then
  FLIBS_IN_SO=${FLIBS}
fi
AC_SUBST(FLIBS_IN_SO)
R_PROG_F77_APPEND_UNDERSCORE
R_PROG_F77_CAN_RUN
R_PROG_F77_CC_COMPAT
R_PROG_F77_CC_COMPAT_COMPLEX

AM_CONDITIONAL(COMPILE_FORTRAN_DOUBLE_COMPLEX,
               [test "x${HAVE_FORTRAN_DOUBLE_COMPLEX}" != x])

AC_LANG_PUSH(Fortran 77)
AC_OPENMP
AC_LANG_POP(Fortran 77)


### *** C++ compiler.

R_PROG_CXX_MAKEFRAG

AC_LANG_PUSH(C++)
AC_OPENMP
AC_LANG_POP(C++)

### *** ObjC compiler

R_PROG_OBJC_MAKEFRAG
R_PROG_OBJC_RUNTIME
## FSF builds of gcc (and maybe others?) need -fobjc-exceptions otherwise
## @try and friends don't work
R_PROG_OBJC_FLAG([-fobjc-exceptions],
		 R_SH_VAR_ADD(OBJCFLAGS, [-fobjc-exceptions]))
## FIXME: checks for Foundation are not darwin-specifc at all. In fact the whole
##        point of R_OBJC_FOUNDATION is to detect foundation classes on other
##        platforms (on Darwin we already *know* that is it -framework Foundation
##        but not so on Linux!), so the following was not intended to be conditonal.
case "${host_os}" in
  darwin*)
    R_OBJC_FOUNDATION
  ;;
esac

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
    AC_DEFINE(HAVE_NO_SYMBOL_UNDERSCORE)
    ;;
  hpux*)
    AC_DEFINE(HAVE_NO_SYMBOL_UNDERSCORE)
    case "${CC}" in
      cc|c89)
	## Luke Tierney says we also need '-Wp,-H16000' which tells the
	## pre-processor to increase the size of an internal table.  It
	## seems that src/main/vfonts/g_her_glyph.c contains a line
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
	## used to set IEEE flag, but this is version-dependent.
        ;;
    esac
    case  "${F77}" in
      ## Intel compilers
      *ifc|*ifort)
	if test "x$userFFLAGS" = x; then
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

R_ABI

### ** DLL stuff.

## We need to determine the following:
##
## MAIN_LD, MAIN_LDFLAGS
##   command and flags for loading the main binary so that it will load
##   shared objects (DLLs) at runtime, also for profiling.
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
  cc=`"${srcdir}/tools/GETMAKEVAL" CC`
  cc=`echo ${cc} | sed "s/ .*//"`
  ## Paul Gilbert reported on R-devel 2006-04-13 a system with cc=""
  if test -n "${cc}" ; then
    r_cc_cmd=`echo ${CC} | sed "s/ .*//"`
    if test "`which ${cc}`" = "`which ${r_cc_cmd}`"; then
      shlib_ldflags=`"${srcdir}/tools/GETMAKEVAL" SHLIBLDFLAGS`
      cpicflags=`"${srcdir}/tools/GETMAKEVAL" PICFLAGS`
    fi
  fi
  cxx=`"${srcdir}/tools/GETMAKEVAL" CXX`
  cxx=`echo ${cxx} | sed "s/ .*//"`
  if test -n "${cxx}" ; then
    r_cxx_cmd=`echo ${CXX} | sed "s/ .*//"`
    if test "`which ${cxx}`" = "`which ${r_cxx_cmd}`"; then
      cxxpicflags=`"${srcdir}/tools/GETMAKEVAL" CXXPICFLAGS`
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
is_cygwin=no
case "${host_os}" in
  aix*)
    use_exportfiles=yes
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
    SHLIB_LIBADD="\$(LIBM)"
    shlib_cxxldflags="${shlib_ldflags}"
    if test "${GCC}" = yes; then
      shlib_ldflags="-shared ${shlib_ldflags}"
    fi
    if test "${GXX}" = yes; then
      shlib_cxxldflags="-shared ${shlib_cxxldflags}"
    fi 
    ;;
  cygwin*)
    ## All Windows code is PIC
    cpicflags=
    cxxpicflags=
    fpicflags=
    fcpicflags=
    SHLIB_EXT=".dll"
    dylib_undefined_allowed=no
    is_cygwin=yes
    main_ldflags="${wl}--large-address-aware ${wl}--stack=0xA00000"
    ;;
  darwin*)
    darwin_pic="-fPIC"
    dylib_undefined_allowed=no
    darwin_dylib_ldflags="-dynamiclib"
    ## Want '-mdynamic-no-pic' for GCC 3, says Jan de Leeuw.
    if test "${GCC}" = yes; then
      case "${CC_VERSION}" in
        3.*)
          R_SH_VAR_ADD(main_ldflags, [-mdynamic-no-pic]) ;;
      esac
    fi
    ## some linkers are broken and need to be set to a specific
    ## OS X version to work better
    case "${host_os}" in
       darwin8*)
          # this is the 'old' way and it doesn't work with recent gcc as the driver
	  # prepends its own version
	  ### darwin_min_flag='Wl,-macosx_version_min -Wl,10.4'
	  # this is the 'new' way, but it's not clear which driver version
	  # started to support it (Xcode 2.5 is known to work and 2.4 should)
	  # A work-around for older Xcode is to set MACOSX_DEPLOYMENT_TARGET
	  # environment variable. It has its own problems, but we may as well
	  # honor it, assuming that it fixes things.
	  if test -z "${MACOSX_DEPLOYMENT_TARGET}"; then
	      darwin_min_flag='-mmacosx-version-min=10.4'
	  fi
	  ;;
       *)
	  darwin_min_flag=''
	  ;;
    esac
    case "${host_os}" in
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

	## we have to test this in case an outdated linker or non-Apple compiler is used
	AC_MSG_CHECKING([whether linker supports dynamic lookup])
	shlib_ldflags="-dynamiclib -Wl,-headerpad_max_install_names ${darwin_min_flag} -undefined dynamic_lookup -single_module -multiply_defined suppress"
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
    ## Could also use -rdynamic
    main_ldflags="-Wl,--export-dynamic"
    STATICR1="-Wl,--whole-archive"
    STATICR2="-Wl,--no-whole-archive"
    ;;
  mingw*)
    SHLIB_EXT=".dll"
    cpicflags=
    cxxpicflags=
    fpicflags=
    fcpicflags=
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
      cpicflags="-fPIC"
      ld=`${CC} -print-prog-name=ld`
      ldoutput=`${ld} -v 2>&1 | grep GNU`
      if test -n "${ldoutput}"; then
	main_ldflags="-Wl,-export-dynamic"
	shlib_ldflags="-shared"
        shlib_cxxldflags="-shared"
      else
        ## it seems gcc c 4.6.2 needs this with Solaris linker
	shlib_ldflags="-shared"
        shlib_cxxldflags="-shared"
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
    ## G77 include gfortran
    if test "${G77}" != yes; then
      fpicflags="-PIC"
    else
      fpicflags="-fPIC"
    fi
    if test "${GXX}" = yes; then
      cxxpicflags="-fPIC"
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
   main_ld_was_given=no
  if test "${main_ld}" = "${CC}"; then
     MAIN_LD="\$(CC)"
  else
     MAIN_LD="${main_ld}"
  fi
  R_SH_VAR_ADD(MAIN_LDFLAGS, [${main_ldflags}])
fi

: ${CPICFLAGS="${cpicflags}"}
if test -z "${CPICFLAGS}"; then
  case "${host_os}" in
    aix*|cygwin*|irix*|mingw*|osf*)
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
    aix*|cygwin*|irix*|mingw*|osf*)
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
    aix*|cygwin*|irix*|mingw*|osf*)
      ;;
    *)
      warn_cxxpicflags="I could not determine CXXPICFLAGS."
      AC_MSG_WARN([${warn_cxxpicflags}])
      ;;
  esac
fi

if test -z "${SHLIB_LD}"; then
  shlib_ld_was_given=no
  if test "${shlib_ld}" = "${CC}"; then
    SHLIB_LD="\$(CC)"
  else
    SHLIB_LD="${shlib_ld}"
  fi
  R_SH_VAR_ADD(SHLIB_LDFLAGS, [${shlib_ldflags}])
fi
if test -z "${SHLIB_LDFLAGS}"; then
  AC_MSG_WARN([I could not determine SHLIB_LDFLAGS.])
  AC_MSG_ERROR([See the file doc/html/R-admin.html for more information.])
fi

if test -z "${SHLIB_CXXLD}"; then
  shlib_cxxld_was_given=no
  if test "${shlib_cxxld}" = "${CXX}"; then
    SHLIB_CXXLD="\$(CXX)"
  else
    SHLIB_CXXLD="${shlib_cxxld}"
  fi
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
  dylib_ld_was_given=no
  DYLIB_LD="${SHLIB_LD}"
  R_SH_VAR_ADD(DYLIB_LDFLAGS, [${dylib_ldflags}])
else
  if test -z "${DYLIB_LDFLAGS}"; then
    DYLIB_LDFLAGS="${dylib_ldflags}"
  fi
fi

## some claim Solaris needs -lsocket -lnsl (PR#15815)
: ${INTERNET_LIBS="${internet_libs}"}
AC_SUBST(INTERNET_LIBS)

AM_CONDITIONAL(DYLIB_UNDEFINED_ALLOWED, [test "x${dylib_undefined_allowed}" = xyes])
AM_CONDITIONAL(BUILD_CYGWIN, [test "x${is_cygwin}" = xyes])

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
[Define this to be the extension used for shared objects on your system.])
AM_CONDITIONAL(USE_EXPORTFILES, [test "x${use_exportfiles}" = xyes])
AC_SUBST(DYLIB_EXT)
AC_SUBST(LIBR_LDFLAGS)
AC_SUBST(RBLAS_LDFLAGS)
AC_SUBST(RLAPACK_LDFLAGS)
AC_SUBST(LAPACK_LDFLAGS)
AC_SUBST(FW_VERSION)
AC_SUBST(STATICR1)
AC_SUBST(STATICR2)

## C++11 and later
R_CXX1X

### OpenMP.

## The basic checking is performed via AC_OPENMP added in Autoconf 2.62,
## which we already called for determining the appropriate flags for the
## C, C++, Fortran 77, Fortran compiler/linker.  Note that this gives
## variables OPENMP_CFLAGS etc., which are meant to be used for *both*
## compiling and linking.  So we can really only used them provided that
## we use the respective compilers for linking as well (or we need a
## different mechanism for determining what is needed).
##
## For compiling R itself, we use MAIN_LD and DYLIB_LD for linking, both
## defaulting to CC.  Hence:
## 
##    If both MAIN_LD and DYLIB_LD were not specified by the user and
##    equal CC and this was determined to support OpenMP, then we (try
##    to) provide OpenMP support by adding OPENMP_CFLAGS to the linker
##    flags and OPENMP_CFLAGS and OPENMP_FFLAGS to the C and Fortran 77
##    compiler flags, and defining HAVE_OPENMP.
##
## (The Fortran 77 compiler is never used for linking by default.)

if test "x${main_ld_was_given}" = xno -a "${MAIN_LD}" = "\$(CC)" -a \
        "x${dylib_ld_was_given}" = xno -a "${DYLIB_LD}" = "\$(CC)" -a \
        "x${ac_cv_prog_c_openmp}" != "xunsupported" -a \
        "x${ac_cv_prog_c_openmp}" != "x"; then
  R_OPENMP_CFLAGS="${OPENMP_CFLAGS}"
  R_OPENMP_FFLAGS="${OPENMP_FFLAGS}"
  R_SH_VAR_ADD(MAIN_LDFLAGS, [${OPENMP_CFLAGS}])
  R_SH_VAR_ADD(DYLIB_LDFLAGS, [${OPENMP_CFLAGS}])
  AC_DEFINE(HAVE_OPENMP, 1, 
            [Define if you have C OpenMP support.])
else
  R_OPENMP_CFLAGS=
  R_OPENMP_FFLAGS=
fi
AC_SUBST(R_OPENMP_CFLAGS)
AC_SUBST(R_OPENMP_FFLAGS)

## For compiling package code, we use SHLIB_FCLD, SHLIB_CXXLD or
## SHLIB_LD for linking, depending on whether the package contains
## Fortran (90/95) code, C++ (or ObjC) code, or "just" C and Fortran 77.
## However, we (currently) do not conditionalize compilation flags.  So
## the only "safe" thing we can do for now is:
##
##    If none of SHLIB_LD, SHLIB_CXXLD and SHLIB_FCLD were specified by
##    the user and equal CC, CXX and FC, respectively, and these were
##    determined to support OpenMP, the we try to provide OpenMP support
##    for packages by adding OPENMP_FCFLAGS, OPENMP_CXXFLAGS and
##    OPENMP_CFLAGS to the respective linker flags, and add the OPENMP
##    flags to all (C, C++, Fortran and Fortran 77) compiler flags.

## <FIXME>
## Need to do this after configuring Fortran 90/95 support, which comes
## way below: should this be moved up to the compiler section?
## </FIXME>

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

## C99 functions: 
## not all C99 runtimes are complete,
## but we have substitutes for expm1 hypot log1p and (internally) nearbyint[l]
## FreeBSD 10 still lacks log1pl
## FreeBSD 8.2 lacks log2
## FreeBSD 7.3 lacks nearbyintl/rintl (nearbyint appeared in 5.2)
## Apparently rint was once broken on HP-UX: undefine HAVE_RINT for such platforms
## Cygwin and FreeBSD lack powl
## Cygwin has rintl but not nearbyintl
R_CHECK_FUNCS([expm1 hypot log1p log1pl log2 log10 nearbyint nearbyintl powl rint rintl], [#include <math.h>])
## va_copy is C99: required as from R 2.13.0
R_CHECK_FUNCS([va_copy], [#include <stdarg.h>])
if test "${ac_cv_have_decl_va_copy}" = "no"; then
   AC_MSG_ERROR([Building R requires the 'va_copy' system call])
fi
## isblank should be a macro according to C99.  It was missing on Solaris 8
AC_CHECK_FUNCS(isblank)

## Solaris libsunmath
AC_CHECK_HEADERS(sunmath.h)
AC_CHECK_LIB(sunmath, cospi)

## Functions from draft C11 extensions, some of which are already in glibc
## For now, do not define _GNU_SOURCE here.
## All but pown have long been in libsunmath
AC_CHECK_FUNCS([atanpi atan2pi cospi exp10 pown sinpi tanpi], 
[#define __STDC_WANT_IEC_60559_TYPES_EXT__ 1
#include <math.h>
#ifdef HAVE_SUNMATH_H
#include <sunmath.h>
#endif])

## fseeko/ftello are POSIX, may be macros
## matherr is SVID, redefined in arithmetic.c if present
AC_CHECK_FUNCS(fseeko ftello matherr)

## POSIX functions
R_CHECK_FUNCS([fcntl], [#include <fcntl.h>])
R_CHECK_FUNCS([getgrgid], [#include <grp.h>])
R_CHECK_FUNCS([getpwuid], [#include <pwd.h>])
R_CHECK_FUNCS([kill sigaction sigaltstack sigemptyset], [#include <signal.h>])
R_CHECK_FUNCS([fdopen popen], [#include <stdio.h>])
if test "${ac_cv_have_decl_popen}" = "no"; then
   AC_MSG_ERROR([Building R requires the 'popen' system call])
fi
## Windows has neither setenv nor unsetenv
R_CHECK_FUNCS([setenv unsetenv], [#include <stdlib.h>])
R_CHECK_FUNCS([getrlimit getrusage getpriority], [#include <sys/resource.h>])
R_CHECK_FUNCS([chmod mkfifo stat umask], [#include <sys/stat.h>])
if test "${ac_cv_have_decl_stat}" = "no"; then
   AC_MSG_ERROR([Building R requires the 'stat' system call])
fi
R_CHECK_FUNCS([gettimeofday utimes], [#include <sys/time.h>])
R_CHECK_FUNCS([times], [#include <sys/times.h>])
R_CHECK_FUNCS([gmtime_r localtime_r], [#include <time.h>])
R_CHECK_FUNCS([nl_langinfo], [#include <langinfo.h>])
R_CHECK_FUNCS([access chdir execv ftruncate getcwd geteuid getuid link readlink symlink sysconf],
[#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif])
## Linux-specific feature allowing us to fix CPU affinity for parallel
R_CHECK_FUNCS([sched_setaffinity sched_getaffinity], [#include <sched.h>])
## utime was declared obsolescent in POSIX 2008 (use utimes instead)
R_CHECK_FUNCS([utime], [#include <utime.h>])
## clock_gettime is POSIX 1993, but not on OS X
## Some OSes need -lrt: Linux (for glibc versions before 2.17), Solaris,
## not FreeBSD. 
## Unsurprising, as POSIX 2008 moved it from its timers section to base.
## timespec_get is C11.
AC_CHECK_LIB(rt, clock_gettime)
R_CHECK_FUNCS([clock_gettime timespec_get], [#include <time.h>])
## We need setenv or putenv.  It seems that everyone does have
## putenv, as earlier versions of R would have failed without it.
## It is not always declared, so we do not require a declaration.
AC_CHECK_FUNCS(putenv)
AC_CHECK_DECLS([putenv], , , [#include <stdlib.h>])
## this is a GNU extension so usually hidden.  Not on Solaris
AC_CHECK_FUNCS(vasprintf)
AC_CHECK_DECLS([vasprintf], , , [#include <stdio.h>])
## mempcpy is a GNU extension used by the included gettext.  Not on Solaris
AC_CHECK_FUNCS(mempcpy)
## realpath is POSIX 2001 (and BSD)
## Some early GNU libc systems had it in unistd.h.
AC_CHECK_FUNCS(realpath)
AC_CHECK_DECLS([realpath], , , [#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif])
## glob is POSIX: we have a substitute on Windows
## assume without checking that if we have glob we also have globfree
R_CHECK_FUNCS([glob], [#ifdef HAVE_GLOB_H
# include <glob.h>
#endif])

## Lots of uses of getwd(), file.access(), Sys.glob().
## We don't need times() except as a fallback for getrusage and
## clock_gettime/gettimeofday -- but it is older and always there.
case "${host_os}" in
  mingw*)
  ;;
  *)
    if test "${ac_cv_search_glob}" = "no"; then
       AC_MSG_ERROR([Building R requires the 'glob' system call])
    fi
    if test "${ac_cv_search_access}" = "no"; then
       AC_MSG_ERROR([Building R requires the 'access' system call])
    fi
    if test "${ac_cv_search_getcwd}" = "no"; then
       AC_MSG_ERROR([Building R requires the 'getcwd' system call])
    fi
    if test "${ac_cv_search_chdir}" = "no"; then
       AC_MSG_ERROR([Building R requires the 'chdir' system call])
    fi
    if test "${ac_cv_search_times}" = "no"; then
      AC_MSG_ERROR([Building R requires the 'times' system call])
    fi
  ;;
esac
## We also use getlogin isatty rename unlink without checking.

## <NOTE>
## No need checking for bcopy bzero memcpy even though ifnames
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
if test "${ac_cv_have_decl_hypot}" = yes; then
  RMATH_HAVE_HYPOT="# define HAVE_HYPOT 1"
else
  RMATH_HAVE_HYPOT="# undef HAVE_HYPOT"
fi
AC_SUBST(RMATH_HAVE_HYPOT)
if test "${ac_cv_have_decl_log1p}" = yes; then
  RMATH_HAVE_LOG1P="# define HAVE_LOG1P 1"
else
  RMATH_HAVE_LOG1P="# undef HAVE_LOG1P"
fi
AC_SUBST(RMATH_HAVE_LOG1P)

## Do we need substitutes?
## mkdtemp is not on Solaris, added in POSIX 2008
## strdup strncasecmp were first required in POSIX 2008.
AC_REPLACE_FUNCS([mkdtemp strdup strncasecmp])
## Enable declarations in Defn.h?
AC_CHECK_DECLS([mkdtemp, strdup, strncasecmp])

AC_SEARCH_LIBS(connect, [socket])
# gethostbyname was removed in POSIX 2008 (in favour of getaddrinfo, POSIX 2004)
AC_SEARCH_LIBS(gethostbyname, [nsl socket])
AC_SEARCH_LIBS(xdr_string, [nsl tirpc])
R_FUNC___SETFPUCW
R_FUNC_CALLOC
if test "${ac_cv_have_decl_isfinite}" = "yes"; then
R_FUNC_ISFINITE
fi
## check accuracy of log1p
R_FUNC_LOG1P
R_FUNC_FTELL
R_FUNC_SIGACTION

if test x${use_internal_tzcode} = xdefault; then
case "${host_os}" in
   darwin*)
     use_internal_tzcode=yes;
     ;; 
esac
fi
if test "${use_internal_tzcode}" != yes; then
  R_MKTIME_ERRNO
  R_FUNC_MKTIME
fi

R_C99_COMPLEX

## BSD extensions
AC_CACHE_CHECK([whether 'struct tm' includes tm_zone],
                [r_cv_have_tm_zone],
[AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
// glibc needs this defined.
#define _BSD_SOURCE
#include <time.h>

int main() {
    struct tm x;
    x.tm_zone = "";
}
]])],
              [r_cv_have_tm_zone=yes],
              [r_cv_have_tm_zone=no])])
if test "x${r_cv_have_tm_zone}" = xyes; then
  AC_DEFINE(HAVE_TM_ZONE, 1, [Define to 1 if your 'struct tm' has tm_zone.])
fi

AC_CACHE_CHECK([whether 'struct tm' includes tm_gmtoff],
                [r_cv_have_tm_gmtoff],
[AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
// glibc needs this defined, second for >= 2.20
#define _BSD_SOURCE
#define _DEFAULT_SOURCE
#include <time.h>

int main() {
    struct tm x;
    x.tm_gmtoff = +3600;
}
]])],
              [r_cv_have_tm_gmtoff=yes],
              [r_cv_have_tm_gmtoff=no])])
if test "x${r_cv_have_tm_gmtoff}" = xyes; then
  AC_DEFINE(HAVE_TM_GMTOFF, 1, [Define to 1 if your 'struct tm' has tm_gmtoff.])
fi

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
    aix*)
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
## LIBR0 is for the -L part, LIBR1 for -lR (if needed)
AC_SUBST(LIBR0)
AC_SUBST(LIBR1)

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
## The default has already been set on OS X: otherwise it is "no"
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

## iconv headers and function.
R_ICONV

## check sufficient support for MBCS
R_MBCS

## support for ICU
if test "$use_ICU" = yes ; then
  R_ICU
  if test "$use_ICU" = no ; then
    case "${host_os}" in
      ## darwin has ICU 3.2 in 10.4, 3.6 in 10.5
      darwin*)
        AC_CHECK_LIB(icucore, ucol_open, [],
            [AC_MSG_ERROR([library 'icucore' is required for ICU])])
        AC_DEFINE(USE_ICU_APPLE, 1, [Define to use Apple's ICU.])
        AC_DEFINE(USE_ICU, 1, [Define to use ICU for collation.])
	use_ICU=yes
      ;;
    esac
  fi
fi
AC_SUBST(USE_ICU)
AC_SUBST(USE_ICU_APPLE)

## X11.
R_X11
AM_CONDITIONAL(BUILD_X11, [test "x${use_X11}" = "xyes"])
## check if X11 typedefs KeySym
R_TYPE_KEYSYM
## check if Xmu is supported
R_X11_Xmu

if test "x${want_cairo}" = "xyes"; then
R_PANGO_CAIRO
fi
AM_CONDITIONAL(BUILD_DEVCAIRO, [test "x${r_cv_cairo_works}" = xyes])


## Aqua
case "${host_os}" in
  darwin*)
    ## check for CoreFoundation framework (chances are much higher
    ## that we can build AQUA if this one is present)
    R_CHECK_FRAMEWORK(CFStringGetSystemEncoding, CoreFoundation)
    ## FIXME: we should verify that we can use Obj-C exceptions
    ##        such as @try and friends. The OBJC compiler tests
    ##	      above add -fobjc-exceptions where possible, but
    ##	      they don't check that the exceptions are available.
    R_AQUA
    ## That sets HAVE_AQUA, which is used to enable support of R.app
    ## and also in C headers and files related to quartz()
  ;;
  *)
    use_aqua=no
  ;;
esac
## Now used:
## - to compile src/unix/aqua.c
## - in etc/Renviron to set the personal library,
## - in grDevices to select building quartz()
AM_CONDITIONAL(BUILD_AQUA, [test "x${use_aqua}" = xyes])

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

## LZMA headers and libraries from xz-utils
R_LZMA

## PCRE headers and libraries.
R_PCRE

## tre headers and libraries.
R_TRE

## libcurl
## curl-config might not match the installed libcurl,
## so we allow the user to set CURL_CPPFLAGS, CURL_LIBS
## and check the version directly rather than by curl-config --checkfor
AC_PATH_PROG(CURL_CONFIG, curl-config)
if test -n "${CURL_CONFIG}"; then
  echo `${CURL_CONFIG} --version`
  if test -z "${CURL_CPPFLAGS}"; then
    CURL_CPPFLAGS=`${CURL_CONFIG} --cflags`
  fi
  ## This should be correct for a static-only build, user will
  ## need to override to specify static linking (see config.site)
  if test -z "${CURL_LIBS}"; then
    CURL_LIBS=`${CURL_CONFIG} --libs`
  fi
fi
r_save_CPPFLAGS="${CPPLAGS}"
CPPFLAGS="${CURL_CPPFLAGS} ${CPPFLAGS}"
r_save_LIBS="${LIBS}"
LIBS="${CURL_LIBS} ${LIBS}"
AC_CACHE_CHECK([if libcurl version >= 7.28.0], [r_cv_have_curl728],
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>
#include <curl/curl.h>
int main() 
{
#ifdef LIBCURL_VERSION_MAJOR
#if LIBCURL_VERSION_MAJOR > 7
  exit(1);
#elif LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR >= 28
  exit(0);
#else
  exit(1);
#endif
#else
  exit(1);
#endif
}
]])], [r_cv_have_curl728=yes], [r_cv_have_curl728=no], [r_cv_have_curl728=no])])
if test "x${r_cv_have_curl728}" != xyes; then
  ac_cv_have_curl_curl_h=0
  CURL_CPPFLAGS=
  CURL_LIBS=
else
  AC_CHECK_HEADERS(curl/curl.h)
fi
CPPFLAGS="${r_save_CPPFLAGS}"
LIBS="${r_save_LIBS}"
AC_SUBST(CURL_CPPFLAGS)
AC_SUBST(CURL_LIBS)

## POSIX times.
R_SYS_POSIX_LEAPSECONDS

## stat times
gl_STAT_TIME
 
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
elif test "${use_system_valgrind}" = yes; then
  AC_CHECK_HEADERS([valgrind/memcheck.h])
fi

AC_DEFINE_UNQUOTED(VALGRIND_LEVEL, ${valgrind_level}, [Define as 1 or 2 to specify levels of Valgrind instrumentation])


if test "x${use_internal_tzcode}" = xyes; then
  AC_DEFINE(USE_INTERNAL_MKTIME, 1, [Define to use internal time-zone code])
fi
AM_CONDITIONAL(BUILD_TZONE, [test "x${use_internal_tzcode}" = xyes])


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
## R_JAVA
custom_JAVA_HOME="${JAVA_HOME}"
: ${JAVA_LIBS=~autodetect~}
: ${JAVA_CPPFLAGS=~autodetect~}
: ${JAVA_LD_LIBRARY_PATH=~autodetect~}
custom_JAVA_LIBS="${JAVA_LIBS}"
custom_JAVA_CPPFLAGS="${JAVA_CPPFLAGS}"
custom_JAVA_LD_LIBRARY_PATH="${JAVA_LD_LIBRARY_PATH}"
AC_SUBST(custom_JAVA_HOME)
AC_SUBST(custom_JAVA_CPPFLAGS)
AC_SUBST(custom_JAVA_LIBS)
AC_SUBST(custom_JAVA_LD_LIBRARY_PATH)
JAVA_LD_LIBRARY_PATH=
AC_SUBST(JAVA)
AC_SUBST(JAVAC)
AC_SUBST(JAVAH)
AC_SUBST(JAR)
AC_SUBST(JAVA_LD_LIBRARY_PATH)
AC_SUBST(JAVA_LIBS0)
AC_SUBST(JAVA_CPPFLAGS0)
AC_SUBST(R_JAVA_LD_LIBRARY_PATH)


## F90/F95 support
AC_PROG_FC()
AC_LANG_PUSH(Fortran)
AC_FC_SRCEXT(f90, [dummy=1], [dummy=0])
AC_FC_SRCEXT(f95, [dummy=1], [dummy=0])
AC_OPENMP
AC_LANG_POP()
if test -z "${SHLIB_FCLD}"; then
  shlib_fcld_was_given=no
  SHLIB_FCLD="\$(FC)"
fi
: ${SHLIB_FCLDFLAGS="${SHLIB_LDFLAGS}"}
AC_SUBST(SHLIB_FCLD)
AC_SUBST(SHLIB_FCLDFLAGS)
## x86 Solaris' f95 needs additional libs when building a DSO
FCLIBS=${FCLIBS}
AC_SUBST(FCLIBS)

## OpenMP package stuff (needs to come *after* configuration of all
## compilers).
if test "x${shlib_ld_was_given}" = xno -a \
        "${SHLIB_LD}" = "\$(CC)" -a \
        "x${ac_cv_prog_c_openmp}" != "xunsupported" -a \
        "x${ac_cv_prog_c_openmp}" != "x" -a \
        "x${shlib_cxxld_was_given}" = xno -a \
        "${SHLIB_CXXLD}" = "\$(CXX)" -a \
        "x${ac_cv_prog_cxx_openmp}" != "xunsupported" -a \
        "x${shlib_fcld_was_given}" = xno -a \
        "${SHLIB_FCLD}" = "\$(FC)" -a \
        "x${ac_cv_prog_fc_openmp}" != "xunsupported"; then
  SHLIB_OPENMP_CFLAGS="${OPENMP_CFLAGS}"
  SHLIB_OPENMP_CXXFLAGS="${OPENMP_CXXFLAGS}"
  SHLIB_OPENMP_FCFLAGS="${OPENMP_FCFLAGS}"
  SHLIB_OPENMP_FFLAGS="${OPENMP_FFLAGS}"
  ## next macro is copied into Rconfig.h
  AC_DEFINE(SUPPORT_OPENMP, 1,
            [Define if you have C/C++/Fortran OpenMP support for package code.])
else
  SHLIB_OPENMP_CFLAGS=
  SHLIB_OPENMP_CXXFLAGS=
  SHLIB_OPENMP_FCFLAGS=
  SHLIB_OPENMP_FFLAGS=
fi
AC_SUBST(SHLIB_OPENMP_CFLAGS)
AC_SUBST(SHLIB_OPENMP_CXXFLAGS)
AC_SUBST(SHLIB_OPENMP_FCFLAGS)
AC_SUBST(SHLIB_OPENMP_FFLAGS)


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
    aix*|cygwin*|irix*|mingw*|osf*)
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

## On Linux, do not add the ld.so system directories such as '/lib' and
## '/usr/lib', so that the entries from '/etc/ld.so.conf' are not
## shadowed (otherwise, e.g. optimized ATLAS libs would not be used).
## On OS X (Darwin) /usr/X11R6/lib shadows the OpenGL framework and is not
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
if test -n "${R_LD_LIBRARY_PATH_save}"; then
  R_LD_LIBRARY_PATH=${R_LD_LIBRARY_PATH_save}
else
## We already added -L's from LDFLAGS (except on Darwin): 
## seem to be doing it again
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
fi

AC_SUBST(R_LD_LIBRARY_PATH)

## Recommended packages.
if test "${use_recommended_packages}" = yes; then
  R_RECOMMENDED_PACKAGES
fi
AM_CONDITIONAL(USE_RECOMMENDED_PACKAGES,
               [test "x${use_recommended_packages}" = xyes])

# i18n support.
AM_NLS
if test "${USE_NLS}" = "yes"; then
  echo
  echo "Configuring src/extra/intl directory"
  AM_GNU_GETTEXT_VERSION(0.16.1)
  AM_GNU_GETTEXT([no-libtool], [need-ngettext], [../extra/intl])
  if test -n "$INTL_MACOSX_LIBS"; then
    XTRA_INTL_CPPFLAGS=-I/System/Library/Frameworks/CoreFoundation.framework/Headers
  fi
  echo "Finished configuring src/extra/intl directory"
  echo
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

### shell for use in scripts: we allow R_SHELL to set the script,
### since some AIX systems have zsh as sh.
: ${R_SHELL=${SHELL}}
AC_SUBST(R_SHELL)
AC_MSG_RESULT([using as R_SHELL for scripts ... ${R_SHELL}])


### * Win32 overrides
case "${host_os}" in
  mingw*)
    AC_DEFINE(HAVE_ICONV, 1, [Define if you have the iconv() function.])
    AC_DEFINE(HAVE_ICONVLIST, 1, [Define if you have the `iconvlist' function.])
    AC_DEFINE(HAVE_ICONV_H, 1, [Define to 1 if you have the <iconv.h> header file.])
    AC_DEFINE(HAVE_INTERNET, 1, [Define if you have support for ftp/http
                                 access.])
    AC_DEFINE(HAVE_JPEG, 1,[Define if you have the JPEG headers and libraries.])
    AC_DEFINE(HAVE_PNG, 1, [Define if you have the PNG headers and libraries.])
    AC_DEFINE(HAVE_POSIX_SETJMP, 1, [Define if you have POSIX.1 compatible
                                     sigsetjmp/siglongjmp.])
    AC_DEFINE(HAVE_SOCKETS, 1, [Define if you have support for sockets.])
    AC_DEFINE(HAVE_TCLTK, 1, [Define if you have the Tcl/Tk headers and 
                              libraries and want Tcl/Tk support to be built.] )
    AC_DEFINE(HAVE_TIFF, 1, [Define this if libtiff is available.])
    AC_DEFINE(HAVE_TIMES, 1, [Define to 1 if you have the `times' function.])
    want_R_profiling=yes
    AC_DEFINE(R_PROFILING, 1, [Define this to enable R-level profiling.])
    AC_DEFINE(SUPPORT_LIBXML, 1, [Define if you provide support for the libxml
                                  ftp/http functions.])
    ;;
esac

### * Output.

AC_CONFIG_HEADERS([src/include/config.h])
AC_CONFIG_FILES(
[Makeconf
  Makefile
  doc/Makefile
  doc/html/Makefile
  doc/manual/Makefile
  etc/Makefile
  etc/Makeconf
  etc/Renviron
  etc/javaconf
  etc/ldpaths
  m4/Makefile
  po/Makefile 
  share/Makefile
  src/Makefile
  src/appl/Makefile
  src/extra/Makefile
  src/extra/blas/Makefile
  src/extra/intl/Makefile
  src/extra/tre/Makefile
  src/extra/tzone/Makefile
  src/extra/xdr/Makefile
  src/include/Makefile
  src/include/Rmath.h0
  src/include/R_ext/Makefile
  src/library/Recommended/Makefile
  src/library/Makefile
  src/library/base/DESCRIPTION
  src/library/base/Makefile
  src/library/compiler/DESCRIPTION
  src/library/compiler/Makefile
  src/library/datasets/DESCRIPTION
  src/library/datasets/Makefile
  src/library/graphics/DESCRIPTION
  src/library/graphics/Makefile
  src/library/graphics/src/Makefile
  src/library/grDevices/DESCRIPTION
  src/library/grDevices/Makefile
  src/library/grDevices/src/Makefile
  src/library/grDevices/src/cairo/Makefile
  src/library/grid/DESCRIPTION
  src/library/grid/Makefile
  src/library/grid/src/Makefile
  src/library/methods/DESCRIPTION
  src/library/methods/Makefile
  src/library/methods/src/Makefile
  src/library/parallel/DESCRIPTION
  src/library/parallel/Makefile
  src/library/parallel/src/Makefile
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
  src/library/translations/DESCRIPTION
  src/library/translations/Makefile
  src/library/utils/DESCRIPTION
  src/library/utils/Makefile
  src/library/utils/src/Makefile
  src/main/Makefile
  src/modules/Makefile
  src/modules/X11/Makefile
  src/modules/internet/Makefile
  src/modules/lapack/Makefile
  src/nmath/Makefile
  src/nmath/standalone/Makefile
  src/scripts/Makefile
  src/scripts/R.sh
  src/scripts/Rcmd
  src/scripts/f77_f2c
  src/scripts/javareconf
  src/scripts/mkinstalldirs
  src/scripts/pager
  src/scripts/rtags
  src/unix/Makefile
  tests/Makefile
  tests/Embedding/Makefile
  tests/Examples/Makefile
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
r_cxx1x_compiler="${CXX1X} ${CXX1XSTD} ${CXX1XFLAGS}"
r_f77_compiler="${F77} ${R_XTRA_FFLAGS} ${FFLAGS}"
r_f95_compiler="${FC} ${FCFLAGS}"
r_objc_compiler="${OBJC} ${OBJCFLAGS}"
r_interfaces=
## we will not have tested for X11 under some configure options, so
## need to test protect the test.
for item in X11 aqua tcltk; do
  if eval "test x\${use_${item}} = xyes"; then
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
    *-lgoto*) r_blas=Goto ;;
    *-lopenblas*) r_blas=OpenBLAS ;;
    *-lacml*) r_blas=ACML ;;
    *-lmkl*) r_blas=MKL ;;
    *-lsgemm*) r_blas=PhiPack ;;
    *sunperf*) r_blas=SunPerf ;;
    *-lessl*)  r_blas=ESSL ;;
    *Accelerate*)  r_blas=Accelerate ;;
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
if test "${have_tre}" = yes; then
  R_SH_VAR_ADD(r_external_libs, [tre], [, ])
fi
if test "x$ac_cv_header_curl_curl_h" = xyes; then
  R_SH_VAR_ADD(r_external_libs, [curl], [, ])
fi

r_capabilities=
r_no_capabilities=
if test "${have_png}" = yes; then
  R_SH_VAR_ADD(r_capabilities, [PNG], [, ])
else
  R_SH_VAR_ADD(r_no_capabilities, [PNG], [, ])
fi
if test "${have_jpeg}" = yes; then
  R_SH_VAR_ADD(r_capabilities, [JPEG], [, ])
else
  R_SH_VAR_ADD(r_no_capabilities, [JPEG], [, ])
fi
if test "${have_tiff}" = yes; then
  R_SH_VAR_ADD(r_capabilities, [TIFF], [, ])
else
  R_SH_VAR_ADD(r_no_capabilities, [TIFF], [, ])
fi
if test "${USE_NLS}" = yes; then
  R_SH_VAR_ADD(r_capabilities, [NLS], [, ])
else
  R_SH_VAR_ADD(r_no_capabilities, [NLS], [, ])
fi
if test "${r_cv_cairo_works}" = yes; then
  R_SH_VAR_ADD(r_capabilities, [cairo], [, ])
else
  R_SH_VAR_ADD(r_no_capabilities, [cairo], [, ])
fi
if test "${use_ICU}" = yes; then
  R_SH_VAR_ADD(r_capabilities, [ICU], [, ])
else
  R_SH_VAR_ADD(r_no_capabilities, [ICU], [, ])
fi

r_options=
r_no_options=
if test "${want_R_framework}" = yes; then
  R_SH_VAR_ADD(r_options, [framework], [, ])
elif test "${want_R_shlib}" = yes; then
  R_SH_VAR_ADD(r_options, [shared R library], [, ])
elif test "${want_R_static}" = yes; then
  R_SH_VAR_ADD(r_options, [static R library], [, ])
fi
if test "${use_blas_shlib}" = yes; then
  R_SH_VAR_ADD(r_options, [shared BLAS], [, ])
else
  R_SH_VAR_ADD(r_no_options, [shared BLAS], [, ])
fi
if test "${want_R_profiling}" = yes; then
  R_SH_VAR_ADD(r_options, [R profiling], [, ])
else
  R_SH_VAR_ADD(r_no_options, [R profiling], [, ])
fi
if test "${want_memory_profiling}" = yes; then
  R_SH_VAR_ADD(r_options, [memory profiling], [, ])
else
  R_SH_VAR_ADD(r_no_options, [memory profiling], [, ])
fi
if test "${use_maintainer_mode}" = yes; then
  R_SH_VAR_ADD(r_options, [maintainer mode], [, ])
fi
if test "${use_strict_barrier}" = yes; then
  R_SH_VAR_ADD(r_options, [strict barrier], [, ])
fi
if test "${want_prebuilt_html}" = yes; then
  R_SH_VAR_ADD(r_options, [static HTML], [, ])
fi

AC_MSG_RESULT(
[
R is now configured for ${host}

  Source directory:          ${srcdir}
  Installation directory:    ${prefix}

  C compiler:                ${r_c_compiler}
  Fortran 77 compiler:       ${r_f77_compiler}

  C++ compiler:              ${r_cxx_compiler}
  C++11 compiler:            ${r_cxx1x_compiler}
  Fortran 90/95 compiler:    ${r_f95_compiler}
  Obj-C compiler:	     ${r_objc_compiler}

  Interfaces supported:      ${r_interfaces}
  External libraries:        ${r_external_libs}
  Additional capabilities:   ${r_capabilities}
  Options enabled:           ${r_options}

  Capabilities skipped:      ${r_no_capabilities}
  Options not enabled:       ${r_no_options}

  Recommended packages:      ${use_recommended_packages} 
])
if test -n "${warn_f77_cc_double_complex}"; then
  AC_MSG_WARN([${warn_f77_cc_double_complex}])
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
if test -n "${warn_tcltk_version}"; then
  AC_MSG_WARN([${warn_tcltk_version}])
fi
if test -n "${warn_info}"; then
  AC_MSG_WARN([${warn_info}])
fi
if test -n "${warn_pdf1}"; then
  AC_MSG_WARN([${warn_pdf1}])
fi
if test -n "${warn_pdf2}"; then
  AC_MSG_WARN([${warn_pdf2}])
fi
if test -n "${warn_pdf3}"; then
  AC_MSG_WARN([${warn_pdf3}])
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
