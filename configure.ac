### configure.ac					-*- Autoconf -*-
###
### Process this file with autoconf to produce a configure script.
###
### Copyright (C) 1998-2025 R Core Team
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
### https://www.r-project.org/Licenses/

AC_PREREQ([2.72])

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

## We call AC_GNU_SOURCE early (it is a prerequisite for the gettext
## macros), so all the C compiling makes use of that.  Nowadays it calls
## AC_USE_SYSTEM_EXTENSIONS ....
## This sets _GNU_SOURCE, so glibc defines all its extensions
## (_POSIX_C_SOURCE, _XOPEN_SOURCE, _BSD_SOURCE, __USE_MISC) and these
## unlock declarations of non-C99 functions and constants.  
## Ditto for __EXTENSIONS__ on Solaris and _ALL_SOURCE on AIX.

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
   ## Darwin 1.3.1 was macOS 10.0, 1.4.1 was 10.1, 5 is 10.2 etc
   ## with 19 being 10.15 (Catalina) and 20 being 11 (Big Sur)
   ## We no longer support < 10.6 (Snow Leopard, Darwin 10)
   ## https://en.wikipedia.org/wiki/Darwin_OS
   darwin1.*)
     AC_MSG_ERROR([The earliest supported macOS is 10.6.]
     ;; 
  darwin[[56789]]*)
     AC_MSG_ERROR([The earliest supported macOS is 10.6.]
     ;; 
  aix[[123]]*|aix4.[[01]]*)
    ## These need a form of linking we no longer support
    AC_MSG_ERROR([AIX prior to 4.2 is not supported])
    ;;
esac

R_PLATFORM="${host}"
AC_SUBST(R_PLATFORM)

dnl config.guess deliberately overrides the cpu/arch as repoorted by
dnl uname -m (or -p) to use one almost unknown to Mac users. So
dnl we need a macro for a more familiar name.
MAC_CPU=
case "${host_os}" in
  darwin*)
    case "${host_cpu}" in
       aarch64)
       MAC_CPU="arm64"
       ;;
       *)
       MAC_CPU="${host_cpu}"
       ;;
    esac
    AC_SUBST(MAC_CPU)
    ;;
esac

R_OS="${host_os}"
AC_SUBST(R_OS)

case "${host_os}" in
  mingw*|windows*|winnt|msys)
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

## Shortcut for musl which refuses to identify itself at C level
if test "${host_os}" = "linux-musl"; then
  AC_DEFINE(OS_MUSL, 1, [Define if running on Linux-musl])
fi

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
    ## Allow for ppc64le (Debian calls ppc64el), powerpc64le ...
    case "${host_cpu}" in
      x86_64|mips64|ppc64*|powerpc64*|sparc64|s390x)
        ## Debian/Ubuntu has "lib64", but the primary location is "lib"
        if test -d /usr/lib64 && test ! -f /etc/debian_version; then
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
[AS_HELP_STRING([--enable-R-framework@<:@=DIR@:>@],[macOS only: build R framework (if possible), and specify
                 its installation prefix @<:@no, /Library/Frameworks@:>@])],
[want_R_framework="${enableval}"],
[want_R_framework=no])
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
## (but a 'dynamic library' in the terminology of macOS).
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

AC_ARG_WITH([C23],
[AS_HELP_STRING([--with-C23],[use C23 if available @<:@yes@:>@])],
[R_ARG_USE(C23)],
[use_C23=yes])

## Build separate shared/dynamic library containing R's BLAS if desired 
AC_ARG_ENABLE([BLAS-shlib],
[AS_HELP_STRING([--enable-BLAS-shlib],[build BLAS into a shared/dynamic library @<:@perhaps@:>@])],
[use_blas_shlib="${enableval}"],
[use_blas_shlib="unset"])

## As from R 3.2.0 split up -L... and -lR
if test "${want_R_shlib}" = yes; then
  LIBR0="-L\"\$(R_HOME)/lib\$(R_ARCH)\""
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
ltoall=no
dnl Allow to be set to empty, although what would be the point?
if test "${LTO+set}" = set; then
  uLTO="${LTO}"
else
  uLTO=-flto
fi
dnl Might well want to set to empty
if test "${LTO_FC+set}" = set; then
  uLTO_FC="${LTO_FC}"
else
  uLTO_FC="${uLTO}"
fi
if test "x${want_lto}" = xyes; then
  ltoall=yes
  LTO="${uLTO}"
  LTOALL="${uLTO}"
  LTO_FC="${uLTO_FC}"
  LTOALL_FC="${uLTO_FC}"
elif  test "x${want_lto}" = xR; then
  LTO="${uLTO}"
  LTO_FC="${uLTO_FC}"
elif  test "x${want_lto}" = xcheck; then
  AC_MSG_WARN([--enable-lto=check has been withdrawn and will be ignored])
  want_lto=no
elif  test "x${want_lto}" != xno; then
  AC_MSG_WARN([Unknown value --enable-lto=${enableval}])
  want_lto=no
fi
if test "x${want_lto}" = xno; then
  dnl unset as inadvertent settings will at best slow down R build.
  LTO=
  LTOALL=
  LTO_FC=
  LTOALL_FC=
  LTO_LD=
fi
AC_SUBST(LTO)
AC_SUBST(LTOALL)
AC_SUBST(LTO_FC)
AC_SUBST(LTOALL_FC)
AC_SUBST(LTO_LD)
## used in src/main/Makefile
AM_CONDITIONAL(BUILD_LTO, [test "x${want_lto}" != xno])
AM_CONDITIONAL(BUILD_LTO_ALL, [test "x${ltoall}" = xyes])

## needed for some versions of icx, but we do not know which
AC_SUBST(INTEL_ICX_FIX)

AC_ARG_ENABLE([java],
[AS_HELP_STRING([--enable-java],[enable Java @<:@yes@:>@])],
[want_java="${enableval}"],
[want_java=yes])
AM_CONDITIONAL(WANT_JAVA, [test "x${want_java}" = xyes])

### ** Optional packages.

## BLAS.
AC_ARG_WITH([blas],
[AS_HELP_STRING([--with-blas],[use system BLAS library (if available), or specify it @<:@unset@:>@])],
[R_ARG_USE(blas)],
[use_blas=unset])

## LAPACK.
AC_ARG_WITH([lapack],
[AS_HELP_STRING([--with-lapack],[use system LAPACK library (if available), or specify it @<:@unset@:>@])],
[R_ARG_USE(lapack)],
[use_lapack=unset])

AC_ARG_WITH([2025blas],
[AS_HELP_STRING([--with-2025blas],[compile 2025 BLAS additions into Rlapack @<:@no@:>@])],
[R_ARG_USE(2025blas)],
[use_2025blas=no])

## Readline.
AC_ARG_WITH([readline],
[AS_HELP_STRING([--with-readline],[use readline library @<:@yes@:>@])],
[R_ARG_USE(readline)],
[use_readline=yes])

## PCRE2.
AC_ARG_WITH([pcre2],
[AS_HELP_STRING([--with-pcre2],[use PCRE2 library (if available) @<:@yes@:>@])],
[R_ARG_USE(pcre2)],
[use_pcre2=yes])

## PCRE1.
AC_ARG_WITH([pcre1],
[AS_HELP_STRING([--with-pcre1],[use PCRE1 library (if available and PCRE2 is not) @<:@yes@:>@])],
[R_ARG_USE(pcre1)],
[use_pcre1=no])

## Aqua.
AC_ARG_WITH([aqua],
[AS_HELP_STRING([--with-aqua],[macOS only: use Aqua (if available) @<:@yes@:>@])],
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
[AS_HELP_STRING([--with-system-valgrind-headers],[defunct])],
[AC_MSG_WARN([--with-system-valgrind-headers is defunct])])

AC_ARG_WITH([internal-tzcode],
[AS_HELP_STRING([--with-internal-tzcode],[use internal time-zone code @<:@no, yes on macOS@:>@])],
[use_internal_tzcode=${withval}],
[use_internal_tzcode=default])

AC_ARG_WITH([internal-towlower],
[AS_HELP_STRING([--with-internal-towlower],[use internal code for towlower/upper @<:@no, yes on macOS@:>@])],
[use_internal_towlower=${withval}],
[use_internal_towlower=default])

AC_ARG_WITH([internal-iswxxxxx],
[AS_HELP_STRING([--with-internal-iswxxxxx],[use internal iswprint etc. @<:@no, yes on macOS@:>@])],
[use_internal_iswxxxxx=${withval}],
[use_internal_iswxxxxx=default])

AC_ARG_WITH([internal-wcwidth],
[AS_HELP_STRING([--with-internal-wcwidth],[use internal wcwidth @<:@yes@:>@])],
[use_internal_wcwidth=${withval}],
[use_internal_wcwidth=yes])

AC_ARG_WITH([internal-tzcode],
[AS_HELP_STRING([--with-internal-tzcode],[use internal time-zone code @<:@no, yes on macOS@:>@])],
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

AC_ARG_WITH([static-cairo],
[AS_HELP_STRING([--with-static-cairo],[allow for the use of static cairo libraries @<:@no, yes on macOS@:>@])],
[use_static_cairo=${withval}],
[use_static_cairo=default])
if test "x$use_static_cairo" = xdefault; then
  case "${host_os}" in
    darwin*)
      use_static_cairo=yes
      ;;
    *)
      use_static_cairo=no
      ;;
  esac
fi

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
           [special flags for compiling Fortran code to be 
	    turned into a shared object.])
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
	   [Safe Fortran fixed-form compiler flags for e.g. dlamc.f])
AC_ARG_VAR([r_arch],
           [Use architecture-dependent subdirs with this name])
AC_ARG_VAR([DEFS], [C defines for use when compiling R])
AC_ARG_VAR([JAVA_HOME],
           [Path to the root of the Java environment])
AC_ARG_VAR([R_SHELL],
           [shell to be used for shell scripts, including 'R'])

if test -z "${r_arch}"; then
  R_ARCH=
  R_INCLUDES="-I\"\$(R_INCLUDE_DIR)\""
else
  R_ARCH="/${r_arch}"
  R_INCLUDES="-I\"\$(R_INCLUDE_DIR)\" -I\"\$(R_INCLUDE_DIR)/${r_arch}\""
fi
AC_DEFINE_UNQUOTED(R_ARCH, "${r_arch}",
[Define this to use architecture-dependent subdirectories of this name.])
AC_SUBST([R_ARCH])
AC_SUBST([R_INCLUDES])

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

AC_PROG_LN_S
R_PROG_AR
R_PROG_INSTALL

## we would like a POSIX sed, and need one on Solaris
AC_PATH_PROGS(SED, sed, /bin/sed, [/usr/xpg4/bin:$PATH])

## 'which' is not POSIX, and might be a shell builtin or alias
##  (but should not be in 'sh')
AC_PATH_PROGS(WHICH, which, which)
if test "${WHICH}" = which ; then
  ## needed to build and run R
  ## ends up hard-coded in the utils package
  AC_MSG_ERROR([[which is required but missing]])
fi

## Make
: ${MAKE=make}
AC_SUBST(MAKE)
## Pager
R_PROG_PAGER
## Tar -- we prefer a GNU version
AC_PATH_PROGS(TAR, [${TAR} gtar gnutar tar], "")
## TeXMF stuff
R_PROG_TEXMF
## Unzip & zip & gzip & bip2
AC_PATH_PROGS(R_UNZIPCMD, [${UNZIP} unzip], "")
AC_PATH_PROGS(R_ZIPCMD, [${ZIP} zip], "")
AC_PATH_PROGS(R_GZIPCMD, [${GZIP} gzip], true)
AC_PATH_PROGS(R_BZIPCMD, [${BZIP} bzip2], "")
## Browser
R_PROG_BROWSER
## PDF viewer
R_PROG_PDFVIEWER
## Noweb (notangle), aclocal, autoconf, autoheader - maintainer mode only

R_MISSING_PROG(ACLOCAL, aclocal,
  R_MNT_WARN([[aclocal is missing - aclocal.m4 cannot be updated!]])
)
R_MISSING_PROG(AUTOCONF, autoconf,
  R_MNT_WARN([[autoconf is missing - configure cannot be updated!]])
)
R_MISSING_PROG(AUTOHEADER, autoheader,
  R_MNT_WARN([[autoheader is missing - header file templates like config.h.in cannot be updated!]])
)
AC_PROG_YACC
if test "${YACC}" = yacc ; then
  ## yacc is the default value even when yacc is not found
  AC_CHECK_PROG(JUSTYACC, yacc, yes)
  if test "x${JUSTYACC}" != xyes ; then
    R_MNT_WARN([[yacc is missing - parsers cannot be updated!]])
  fi
fi
AC_PATH_PROG(NOTANGLE, notangle, false)
if test "${NOTANGLE}" = false ; then
  R_MNT_WARN([[notangle is missing - byte-code compiler source cannot be updated!]])
  NOTANGLE=notangle    
fi
## javareconf needs this
AC_PATH_PROG(REALPATH, realpath, false)


## Search for cairographics needs pkg-config, 
## helps find jpeg, libpng and libtiff.
AC_PATH_PROGS(PKG_CONFIG, [ pkg-config pkgconf ], [])
AC_ARG_VAR([PKG_CONFIG], [path to pkg-config (or pkgconf) utility])
AC_ARG_VAR([PKG_CONFIG_PATH], [directories to add to pkg-config's search path])
AC_ARG_VAR([PKG_CONFIG_LIBDIR], [path overriding pkg-config's default search path])

AC_PROG_CC
dnl AC_PROG_GCC_TRADITIONAL
AC_USE_SYSTEM_EXTENSIONS ## see note above

dnl echo "---- initial CC = ${CC}"
CC_save=${CC}
CFLAGS_save=${CFLAGS}
CC_SUB=`echo "${CC}" | sed 's/-std=[[^ ]]*//'`
CFLAGS_SUB=`echo "${CFLAGS}" | sed -e 's/-std=[[^ ]]*//'`
## try first without any additional -std
: ${CC23="${CC_SUB}"}
: ${C23FLAGS="${CFLAGS_SUB}"}
CC=${CC23}
CFLAGS=${C23FLAGS}
unset r_cv_C23
R_C23

if test x${r_cv_C23} = xno; then
CC23="${CC_SUB} -std=gnu23"
C23FLAGS="${CFLAGS_SUB}"
CC=${CC23}
CFLAGS=${C23FLAGS}
unset r_cv_C23
R_C23
fi

dnl this is a hack until -std=gnu23 is widely accepted.
if test x${r_cv_C23} = xno; then
unset r_cv_C23
CC23="${CC_SUB} -std=gnu2x"
CC=${CC23}
R_C23
fi

if test x${r_cv_C23} = xno; then
CC23=
C23FLAGS=
fi
AC_SUBST(CC23)
AC_SUBST(C23FLAGS)

if test x$use_C23 = xyes; then
  if test x${r_cv_C23} = xyes; then
    CC=${CC23}
    CFLAGS=${C23FLAGS}
  else
    CC=${CC_save}
    CFLAGS=${CFLAGS_save}
    AC_MSG_WARN([C23 support is unavailable])
  fi
else
CC=${CC_save}
CFLAGS=${CFLAGS_save}
fi
dnl echo "---- final CC = ${CC}"


AC_PROG_CPP
R_PROG_CPP_CPPFLAGS

## As from R 3.6.0, use the modern Fortran compiler for .f files
echo "looking for a modern Fortran compiler"
## if user set FFLAGS but not FCFLAGS, default to former
if test "${FFLAGS+set}" = set -a "${FCFLAGS+set}" != set; then
  FCFLAGS=${FFLAGS}
fi
AC_PROG_FC
if test -z "${FC}"; then
  AC_MSG_ERROR([No Fortran compiler found])
fi
F77=${FC}
## see if the user set FFLAGS, otherwise use what AC_PROG_FC chose
if test "${FFLAGS+set}" != set; then
  FFLAGS=${FCFLAGS}
fi

## This looks for a C++11 compiler: Apple clang++ is not by default
AC_PROG_CXX
## check this actually compiles
R_PROG_CXX

## AC_PROG_CXXCPP looks for limits.h and does not include CPPFLAGS
## CXXCPP is used in other places (not in R), so leave alone CXXCPP itself
AC_PROG_CXXCPP
if test "${CXXCPP}" = "${CXX} -E"; then
  CXXCPP0="\$(CXX) -E"
else
  CXXCPP0=${CXXCPP}
fi
AC_SUBST(CXXCPP0)

R_GCC4_VISIBILITY

AC_PROG_OBJC
## unfortunately autoconf sets OBJC to gcc even if there is no working compiler
if test "${OBJC}" = gcc; then
  AC_LANG_PUSH([Objective C])
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM(,[id foo;])],,[OBJC=''])
  AC_LANG_POP([Objective C])
fi
## deferred until C++ standard is chosen.
dnl R_PROG_OBJCXX

## This has to be R_DEFS as autoconf overrrides DEFS
R_DEFS=${DEFS}
AC_SUBST(R_DEFS)

## Other C standards -- C23 done higher up
CC_save=${CC}
CFLAGS_save=${CFLAGS}
: ${CC17="${CC_SUB} -std=gnu17"}
: ${C17FLAGS="${CFLAGS_SUB}"}
CC=${CC17}
CFLAGS=${C17FLAGS}
R_C17
if test x${r_cv_C17} = xno; then
unset r_cv_C17
CC17="${CC_SUB} -std=gnu11"
CC=${CC17}
R_C17
fi
if test x${r_cv_C17} = xno; then
CC17=
C17FLAGS=
fi
AC_SUBST(CC17)
AC_SUBST(C17FLAGS)

: ${CC90="${CC_SUB} -std=gnu90"}
: ${C90FLAGS="${CFLAGS_SUB}"}
CC=${CC90}
CFLAGS=${C90FLAGS}
R_C90
if test x${r_cv_C90} = xno; then
CC90=
C90FLAGS=
fi
AC_SUBST(CC90)
AC_SUBST(C90FLAGS)

: ${CC99="${CC_SUB} -std=gnu99"}
: ${C99FLAGS="${CFLAGS_SUB}"}
CC=${CC99}
CFLAGS=${C99FLAGS}
R_C99
if test x${r_cv_C99} = xno; then
CC99=
C99FLAGS=
fi
AC_SUBST(CC99)
AC_SUBST(C99FLAGS)
CC=${CC_save}
CFLAGS=${CFLAGS_save}

## Libtool.
## (Run this after selecting the Fortan compiler as AC_PROG_LIBTOOL
## checks for a Fortran 77 compiler and sets F77 accordingly.)
AC_DISABLE_STATIC
LT_INIT
AC_SUBST(LIBTOOL_DEPS)

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
    ## Not so bad in later versions of Darwin, 
    ## where DYLD_FALLBACK_LIBRARY_PATH is used (see below).
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
## Used in etc/ldpaths: As from R 3.0.0 override what libtool thinks on macOS
## http://hublog.hubmed.org/archives/001192.html suggests this was in 10.4
## However, as from 10.11 this variable is not passed down to shells.
case "${host_os}" in
  darwin*)
    Rshlibpath_var=DYLD_FALLBACK_LIBRARY_PATH
    ;;
  cygwin*|mingw*|windows*|winnt|msys)
    Rshlibpath_var=PATH
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
: ${STRIP_SHARED_LIB="${striplib}"}
## record how to strip static libraries.
: ${STRIP_STATIC_LIB="${old_striplib}"}
AC_SUBST(STRIP_SHARED_LIB)
AC_SUBST(STRIP_STATIC_LIB)

## <NOTE>
## This actually comes from libtool.m4.
LT_LIB_M
AC_SUBST(LIBM)
## </NOTE>
## LT_LIB_M computes LIBM but does not add to LIBS, hence we do
## the following as well.
## This gives: 'sin' is a builtin with type 'double (double)'
AC_CHECK_LIB(m, sin)
AC_CHECK_LIB(dl, dlopen)

## Readline.
if test "${use_readline}" = yes; then
  dnl in libedit both headers exist as links to editline/readline.h
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
    ## FIXME replace main, maybe by tputs
    AC_CHECK_LIB(ncurses, main, [],
                 AC_CHECK_LIB(tinfo, main, [],
                              AC_CHECK_LIB(termcap, main, [],
                                           AC_CHECK_LIB(termlib, main))))
    AC_CHECK_LIB(readline, rl_callback_read_char)
    use_readline="${ac_cv_lib_readline_rl_callback_read_char}"
    if test "${use_readline}" = yes; then
      use_readline="${ac_cv_header_readline_readline_h}"
    fi
  fi
  if test "${use_readline}" = no; then
    AC_MSG_ERROR([--with-readline=yes (default) and headers/libs are not available])
  else
    ## the NetBSD emulation (libedit) supplied by macOS did not have these,
    ## but it does now (10.13) have history_truncate_file
    ## it has tilde_expand, which is documented to be like tilde_expand_word
    ## 'does tilde expansion of strings of type ``~user/foo''
    AC_CHECK_FUNCS(history_truncate_file tilde_expand_word)
    ## rl_completion_matches is >= 4.2 (emulation has it)
    ## rl_resize_terminal is >= 4.0 but we use it only for >= 6.3.
    ## rl_sort_completion_matches is >= 6.0 (not in emulation)
    R_CHECK_FUNCS([rl_completion_matches rl_resize_terminal rl_sort_completion_matches], 
[#include <stdio.h>
#include <readline/readline.h>]
                 )
  fi
  READLINE_LIBS="${LIBS}"
  LIBS="${r_save_LIBS}"
fi
AC_SUBST(READLINE_LIBS)

### * Checks for header files.

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
## The following headers are POSIX,
## We use sched.h for Linux-specific features (affinity)
AC_CHECK_HEADERS(dlfcn.h fcntl.h glob.h grp.h pwd.h sched.h strings.h \
  sys/resource.h sys/select.h sys/socket.h sys/stat.h sys/time.h \
  sys/times.h sys/utsname.h unistd.h utime.h)
## features.h is used by date-time code on Linux and in extra/tzone
## floatingpoint.h is used for fpsetmask on FreeBSD.
## sys/param.h is one way to get PATH_MAX.
## elf.h is used for fallback for HAVE_NO_SYMBOL_UNDERSCORE
AC_CHECK_HEADERS(arpa/inet.h elf.h features.h floatingpoint.h \
  langinfo.h netdb.h netinet/in.h sys/param.h)
## stdalign.h is C11.
AC_CHECK_HEADERS(stdalign.h)
## These are C99 headers but some C code (written to work also
## without assuming C99) may need the corresponding conditionals.
AC_CHECK_HEADERS(errno.h inttypes.h limits.h locale.h stdarg.h stdbool.h \
                 stdint.h string.h)
## only vsnprintf.c requires stdarg.h

## We also use without checking sys/sysctl.h, but only on *BSD and macOS
## The default includes check for sys/types.h (POSIX), which we use unconditionally
## </NOTE>

R_HEADER_SETJMP
R_HEADER_GLIBC2

### * Checks for types.

## liblzma uses uint64_t: used unconditionally in src/main/util.c
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
## These are optional C99 types, which we used to typedef in Defn.h if absent.
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

### ** Generic tests for the C, fixed-form Fortran and C++ compilers.

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
## This is tested in src/main/arithmetic.c and gives a compile error if false
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(long long)
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(size_t)

R_CSTACK_DIRECTION

if test "${r_cv_cstack_direction}" = "up"; then
  cstack_dir=-1
else
  cstack_dir=1
fi
AC_DEFINE_UNQUOTED([C_STACK_DIRECTION], ${cstack_dir}, 
                    [C stack direction: 1 (down) or -1 (up)])



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
  AC_MSG_NOTICE([Not using the 'long double' type])
fi

R_PROG_CC_MAKEFRAG
R_PROG_CC_LO_MAKEFRAG

## for C compiler
R_OPENMP

### *** Fortran compiler.

R_PROG_FC_FLIBS
if test -z "$FLIBS_IN_SO"; then
  FLIBS_IN_SO=${FLIBS}
fi
AC_SUBST(FLIBS_IN_SO)
R_PROG_FC_APPEND_UNDERSCORE
R_PROG_FC_CAN_RUN
R_PROG_FC_CC_COMPAT
R_PROG_FC_CC_COMPAT_COMPLEX

AM_CONDITIONAL(COMPILE_FORTRAN_DOUBLE_COMPLEX,
               [test "x${HAVE_FORTRAN_DOUBLE_COMPLEX}" != x])

AC_LANG_PUSH(Fortran)
R_OPENMP
AC_LANG_POP(Fortran)


### *** C++ compiler

R_PROG_CXX_MAKEFRAG

## OpenMP selection has to come after dialect is chosen.

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

### ** Platform-specific overrides for the C, fixed-form Fortran and C++ compilers.

## Ensure we do not pick up values from outside configure
R_XTRA_CFLAGS=
R_XTRA_CPPFLAGS=
R_XTRA_CXXFLAGS=
R_XTRA_FFLAGS=
R_XTRA_LIBS=

dnl This was for glibc 2.1
dnl case "${host_cpu}" in
dnl   i*86|x86_64)
dnl     ## This sets R_XTRA_CFLAGS
dnl     R_PROG_CC_FLAG_D__NO_MATH_INLINES
dnl     ## We used to add -mieee-fp here, but it seems it is really a
dnl     ## linker flag for old Linuxen adding -lieee to a non-shared link.
dnl     ;;
dnl esac

dnl This is set for elf platforms in Rdynload.c
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
  darwin*)
    ## which these days mean macOS
    AC_DEFINE(HAVE_NO_SYMBOL_UNDERSCORE)
    ;;
   hpux*)
dnl leave this in as not easily overridden by users
     AC_DEFINE(HAVE_NO_SYMBOL_UNDERSCORE)
dnl     case "${CC}" in
dnl       cc|c89)
dnl 	## Luke Tierney says we also need '-Wp,-H16000' which tells the
dnl 	## pre-processor to increase the size of an internal table.  It
dnl 	## seems that src/main/vfonts/g_her_glyph.c contains a line
dnl 	## that is too long for the pre-processor without this flag.
dnl 	R_SH_VAR_ADD(R_XTRA_CPPFLAGS, [-Wp,-H16000])
dnl 	;;
dnl     esac
dnl     AC_CHECK_LIB(dld, shl_load, [R_XTRA_LIBS="-ldld ${R_XTRA_LIBS}"])
     ;;
  ## We no longer special-case Intel compilers on Linux:
  ## what we had dated from Nov 2005.
  mingw*|windows*|winnt|msys)
    AC_DEFINE(HAVE_NO_SYMBOL_UNDERSCORE)
    ;;
  openbsd*)
    if test -z "`echo __ELF__ | $CC -E - | grep __ELF__`"; then
      AC_DEFINE(HAVE_NO_SYMBOL_UNDERSCORE)
    fi
    ;;
esac

## This was -ffloat-store on all gfortran/g77 platforms prior to R 3.5.0
## Suggestion adopted from https://gcc.gnu.org/wiki/FloatingPointMath
if test "${SAFE_FFLAGS+set}" != set; then
  if test "x${ac_cv_fc_compiler_gnu}" = xyes; then
    case "${host_cpu}" in
      i*86|x86_64)
      SAFE_FFLAGS="${FFLAGS} -msse2 -mfpmath=sse"
      ;;
    *)
      SAFE_FFLAGS="${FFLAGS}"
      ;;
    esac
  else
    SAFE_FFLAGS="${FFLAGS}"
  fi
fi

### type for Fortran 'hidden' character lengths, if any
### Try setting FPIFLAGS if not already set.
AC_MSG_CHECKING([for type of 'hidden' Fortran character lengths])
if test "x${FC_LEN_T}" != "x"; then
  fc_len_t=${FC_LEN_T}
else
  R_PROG_FC_CHAR_LEN_T
  if test "${r_cv_prog_fc_char_len_t}" != "xunknown" ; then
    fc_len_t=${r_cv_prog_fc_char_len_t}
  fi
fi
AC_MSG_RESULT([${fc_len_t}])

if test "x${fc_len_t}" = "x" ; then
  AC_MSG_ERROR([FC_LEN_T must be non-empty.])
fi

if test "x${fc_len_t}" != "xunknown" ; then
   AC_DEFINE_UNQUOTED([FC_LEN_T], ${fc_len_t}, 
                      [C type used for Fortran character lengths])
fi

dnl substitute FPIEFLAGS as needed, e.g. from config.site or above.
AC_SUBST(FPIEFLAGS)


AC_SUBST(CFLAGS)
AC_SUBST(MAIN_CFLAGS)
AC_SUBST(SHLIB_CFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(CXXSTD)
AC_SUBST(SHLIB_CXXFLAGS)
AC_SUBST(FFLAGS)
AC_SUBST(SAFE_FFLAGS)
AC_SUBST(MAIN_FFLAGS)
AC_SUBST(SHLIB_FFLAGS)
## used on AIX and formerly on glibc 2.1
AC_SUBST(R_XTRA_CFLAGS)
## formerly used on HP-UX
AC_SUBST(R_XTRA_CPPFLAGS)
## unused
AC_SUBST(R_XTRA_CXXFLAGS)
## used for gfortran >= 7
AC_SUBST(R_XTRA_FFLAGS)
## formerly used on HP-UX
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
## CPICFLAGS, CXXPICFLAGS, FPICFLAGS
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
## Allow user to override this, e.g. HP-UX uses .sl
: ${SHLIB_EXT=".so"}
SHLIB_LIBADD=
use_exportfiles=no

## Step 1.  Ask imake.
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
  if test -n "${cxx}" -a  -n "${CXX}"; then
    r_cxx_cmd=`echo ${CXX} | sed "s/ .*//"`
    if test "`which ${cxx}`" = "`which ${r_cxx_cmd}`"; then
      cxxpicflags=`"${srcdir}/tools/GETMAKEVAL" CXXPICFLAGS`
    fi
  fi
  rm -f Imakefile Makefile
fi

## Step 2.  GNU compilers.
## (And those C/C++ compilers claiming conformity.)
if test "${GCC}" = yes; then
  case "${host_cpu}" in
## Sparc has only an 8k global object table, 1024 entries on 64-bit.
## PowerPC has 32k, not enough on ppc64 for the ca6200 entries in libR.so
## The only other platform where this is said to matter is m68k, which
## has 32k and so can use -fpic.
## However, although the gcc docs do not mention it, it seems s390/s390x
## also supports and needs -fPIC
## See <https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=1004747> for alpha.
    aarch64*|alpha*|sparc*|ppc64*|powerpc64*|s390*)
      cpicflags="-fPIC"
      ;;
    *)
      cpicflags="-fpic"
      ;;
  esac
  shlib_ldflags="-shared"
fi
## autconf checks for gfortran by compiling a program with
dnl #ifndef __GNUC__
dnl       choke me
dnl #endif

## Use same flang for gfortran as for gcc.
if test "${ac_cv_fc_compiler_gnu}" = yes; then
  case "${host_cpu}" in
    aarch64*|alpha*|sparc*|ppc64*|powerpc64*|s390*)
      fpicflags="-fPIC"
      ;;
    *)
      fpicflags="-fpic"
      ;;
  esac
else
## Otherwise check for flang (including classic flang)
## and treat as for clang (you cannot really use flang without clang).
## Set fc_is_clang in case needed elsewhere.
cat > conftestf.F <<EOF
      program main
#ifndef __flang__
       choke me
#endif
      end
EOF
${FC} ${FFLAGS} -c conftestf.F 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD
if test ${?} = 0; then
   fc_is_clang=yes
   fpicflags=${cpicflags}
else
   fc_is_clang=no
fi
rm -Rf conftestf.* core
## echo "is this flang? ${fc_is_clang}"
fi

if test "${GXX}" = yes; then
  case "${host_cpu}" in
    aarch64*|alpha*|sparc*|ppc64*|powerpc64*|s390*)
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
  darwin*)
    darwin_pic="-fPIC"
    dylib_undefined_allowed=no
    darwin_dylib_ldflags="-dynamiclib"
    ##  -single_module is default and -multiply_defined is obsolete
    ## at least since macOS 11 (darwin 20)
    case "${host_os}" in
      darwin1*)
        shlib_ldflags="-dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress"
      ;;
      *)
        shlib_ldflags="-dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup"
      ;;
    esac
## * recent ld has -single_module so it doesn't need -fno-common
    ##   we have to use dylib instead of a bundle
    ## * dylib+single_module+flat_namespace=pretty much what other platforms call .so
    ##   but there can be no multiple symbols (due to flat namespace)
    ## * since 10.3 we can also use -undefined dynamic_lookup which allows us to
    ##   use two-level namespace and still have undefined symbols

    ## FIXME: strictly speaking it should be "yes" but libRblas still
    ## needs -lgfortran because the sharing is a one-way street
    ## dylib_undefined_allowed=yes

    ## we have to test this in case an outdated linker or non-Apple compiler is used
    AC_MSG_CHECKING([whether linker supports dynamic lookup])
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
      case "${host_os}" in
         darwin1*)
          shlib_ldflags="-dynamiclib -flat_namespace -undefined suppress -single_module -multiply_defined suppress"
         ;;
         *)
           shlib_ldflags="-dynamiclib -flat_namespace -undefined suppress"
         ;;
       esac
       dylib_undefined_allowed=yes
    fi
	
    ## we use the same method for shlib and dylib now
    darwin_dylib_ldflags="${shlib_ldflags}"
    ## side note: we could use flat namespace instead, but there is an exception:
    ## * libRblas must be 2-level, dyn lookup because of xerbla which is undefined
    cpicflags="${darwin_pic}"
    cxxpicflags="${darwin_pic}"
    ## macOS does not have a Fortran compiler, so this is speculative,
    ## but works for clang/gfortran combination
    fpicflags="${darwin_pic}"
    shlib_cxxldflags="${shlib_ldflags}"
    ;;
  freebsd*)
    ## maybe this needs to depend on the compiler:
    ## -export-dynamic used to work, but does not with clang.
    ## Seems FreeBSD has used the GNU linker since at least 3.0 (Oct 1998)
    ## We could also use -rdynamic, which seems to work with clang and gcc.
    main_ldflags="-Wl,--export-dynamic"
    shlib_ldflags="-shared"
    ;;
  gnu*)				# GNU Hurd, see FreeBSD comment
    main_ldflags="-Wl,--export-dynamic"
    ;;
dnl   hpux*)
dnl     SHLIB_EXT=".sl"
dnl     case "${CC}" in
dnl       cc|c89)
dnl         cpicflags="+Z"
dnl         ;;
dnl     esac
dnl     case "${FC}" in
dnl       f77|fort77|f90)
dnl         fpicflags="+Z"
dnl         ;;
dnl     esac
dnl     main_ldflags="-Wl,-E"
dnl     if test "${GCC}" = yes; then
dnl       shlib_ldflags="-shared -fPIC -Wl,-Bsymbolic"
dnl     else
dnl       ## <NOTE>
dnl       ## Native cc insists on tacking on crt0.o when it calls ld, and
dnl       ## crt0.o is not built with PIC.  As there seems to be no obvious
dnl       ## way to tell cc not to do this, we use ld for linking shlibs.
dnl       shlib_ld=ld
dnl       shlib_ldflags="-b -Bsymbolic"
dnl       ## </NOTE>
dnl     fi
dnl     if test "${GXX}" = yes; then
dnl       shlib_cxxldflags="-shared -fPIC"
dnl     fi
dnl     ;;
dnl   linux*aout)			# GNU Linux/aout
dnl     sed '/HAVE_ELF_H/d' confdefs.h > tmp.h ; mv tmp.h confdefs.h
dnl     ;;
  linux*)			# GNU Linux/ELF
    case  "${CC}" in
      ## Intel compiler: note that -c99 may have been appended
      *icc*)
        cpicflags="-fpic"
        ;;
      ## Nvidia (formerly Portland Group)
      *pgcc*)
        cpicflags="-fpic"
	;;
    esac
    case  "${FC}" in
      ## Intel compilers
      *ifc|*ifort|*ifx)
        fpicflags="-fpic"
        ;;
      ## Nvidia (formerly Portland Group)
      *pgf77|*pgf90|*pgf95)
        fpicflags="-fpic"
        ;;
    esac
    case  "${CXX}" in
      ## Intel compilers
      *icpc|*icc)
        cxxpicflags="-fpic"
        ;;
      ## Nvidia (formerly Portland Group)
      *pgCC)
        cxxpicflags="-fpic"
        ;;
    esac
    ## Luke Tierney says that just '-export-dynamic' does not work for
    ## Intel compilers (icc).  It is accepted by clang but ignored.
    ## Could also use -rdynamic, at least for gcc and clang.
    main_ldflags="-Wl,--export-dynamic"
    STATICR1="-Wl,--whole-archive"
    STATICR2="-Wl,--no-whole-archive"
    ;;
  mingw*|msys)
    SHLIB_EXT=".dll"
    cpicflags=
    cxxpicflags=
    fpicflags=
    fcpicflags=
    ;;
  netbsd*)
    ## See the comments about FreeBSD
    if ${CPP} - -dM < /dev/null | grep __ELF__ >/dev/null ; then
      main_ldflags="-Wl,--export-dynamic"
      shlib_ldflags="-shared"
    else
      shlib_ldflags="-Bshareable"
    fi
    ;;
  openbsd*)
    ## ${wl} is defined by libtool configuration code.
    ## Both -Wl,-export-dynamic and -Wl,--export-dynamic seem to
    ## work with the GNU linker, but the second is what is documented.
    ## libtool seems to use -Wl-E , a GNU ld alias of -Wl,--export-dynamic
    if test -z "`echo __ELF__ | $CC -E - | grep __ELF__`"; then
      main_ldflags="${wl}-export-dynamic"
      shlib_ldflags="-shared -fPIC"
    fi
    case "${host_cpu}" in 
      powerpc*)
	## GCC -fpic limits to 2**16 on OpenBSD powerpc.
	## Error message without -fPIC:
	##   relocation truncated to fit: R_PPC_GOT16...
        if test "${GCC}" = yes; then
          cpicflags="-fPIC"
        fi  
	if test "${ac_cv_fc_compiler_gnu}" = yes; then
          fpicflags="-fPIC"
        fi
        if test "${GXX}" = yes; then
          cxxpicflags="-fPIC"
        fi
        ;;
    esac
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
    if test "${ac_cv_fc_compiler_gnu}" != yes; then
      ## Oracle compiler
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
fi
R_SH_VAR_ADD(MAIN_LDFLAGS, [${main_ldflags}])

: ${CPICFLAGS="${cpicflags}"}
if test -z "${CPICFLAGS}"; then
  case "${host_os}" in
    aix*|mingw*|msys)
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
    aix*|mingw*|msys)
      ;;
    *)
      AC_MSG_WARN([I could not determine FPICFLAGS.])
      AC_MSG_ERROR([See the file doc/html/R-admin.html for more information.])
      ;;
  esac
fi

: ${CXXPICFLAGS="${cxxpicflags}"}
if test -n "${CXX}" -a -z "${CXXPICFLAGS}"; then
  case "${host_os}" in
    aix*|mingw*|msys)
      ;;
    *)
      warn_cxxpicflags="I could not determine CXXPICFLAGS."
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
if test  -n "${CXX}" -a -z "${SHLIB_CXXLDFLAGS}"; then
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
R_DYLIB_VERSION_SUFFIX=""
case "${host_os}" in
dnl   aix*)
dnl     ## Not needed for -brtl linking
dnl     # RLAPACK_LDFLAGS="${wl}-bE:\$(top_builddir)/etc/Rlapack.exp"
dnl     # LAPACK_LDFLAGS="${wl}-bI:\$(R_HOME)/etc/Rlapack.exp"
dnl     ;;
  darwin*)
    DYLIB_EXT=".dylib"
    dylib_ldflags="${darwin_dylib_ldflags}"
    MAJR_VERSION=`echo "${PACKAGE_VERSION}" | sed  -e "s/[[\.]][[1-9]]$/.0/"` 
    LIBR_LDFLAGS="-install_name libR.dylib -compatibility_version ${MAJR_VERSION}  -current_version ${PACKAGE_VERSION}  -headerpad_max_install_names"
    RLAPACK_LDFLAGS="-install_name libRlapack.dylib -compatibility_version ${MAJR_VERSION} -current_version ${PACKAGE_VERSION}  -headerpad_max_install_names"
    ## don't use version in libRblas so we can replace it with any BLAS implementation
    RBLAS_LDFLAGS="-install_name libRblas.dylib -headerpad_max_install_names"
    ;;
dnl   hpux*)
dnl     ## Needs to avoid embedding a relative path ../../../bin.
dnl     ## See the above code for shlib_ldflags for reasons why we currently
dnl     ## cannot always use '-Wl,+s'.
dnl     if test "${GCC}" = yes; then
dnl       LAPACK_LDFLAGS="-Wl,+s"
dnl     else
dnl       LAPACK_LDFLAGS="+s"
dnl     fi
dnl     ;;
  openbsd*)
    if test -z "${R_DYLIB_VERSION}"; then
      PACKAGE_VERSION_MAJOR=`echo "${PACKAGE_VERSION}" | \
        sed -e "s/\.//" -e "s/\..*$//"`
      PACKAGE_VERSION_MINOR=`echo "${PACKAGE_VERSION}" | \
        sed -e "s/.*\.\([[^.]][[^.]]*$\)/\1/"`
      R_DYLIB_VERSION="${PACKAGE_VERSION_MAJOR}.${PACKAGE_VERSION_MINOR}"
    fi
    R_DYLIB_VERSION_SUFFIX=".${R_DYLIB_VERSION}"
    ;;
esac

R_DYLIB_EXT="${DYLIB_EXT}${R_DYLIB_VERSION_SUFFIX}"

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

dnl Used in appl/Makefile.in; Darwin-specific setting there.
dnl Also used for support for Apple Accelerate.
AM_CONDITIONAL(DYLIB_UNDEFINED_ALLOWED, [test "x${dylib_undefined_allowed}" = xyes])

AC_SUBST(MAIN_LD)
AC_SUBST(MAIN_LDFLAGS)
AC_SUBST(CPICFLAGS)
AC_SUBST(CXXPICFLAGS)
AC_SUBST(DYLIB_LD)
AC_SUBST(DYLIB_LDFLAGS)
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
AC_SUBST(R_DYLIB_EXT)

## Test support for C++ standards
## AC_PROG_CXX might have added -std=gnu++11 or similar
save_CXX=${CXX}
CXX=`echo "${CXX}" | sed -e 's/ *-std=[[^ ]]*//'`
R_STDCXX([11], [CXX11], [CXX])
R_STDCXX([14], [CXX14], [CXX11])
R_STDCXX([17], [CXX17], [CXX14])
R_STDCXX([20], [CXX20], [CXX17])
R_STDCXX([23], [CXX23], [CXX20])
R_STDCXX([26], [CXX26], [CXX23])
CXX=${save_CXX}

## Set default C++ compiler to C++17 if available, otherwise unset
if test -n "${CXX}" && test -z "${CXXSTD}"; then
  if test -n "${CXX17}"; then
    CXX=${CXX17}
    CXXSTD=${CXX17STD}
    CXXFLAGS=${CXX17FLAGS}
    CXXPICFLAGS=${CXX17PICFLAGS}
  else
    AC_MSG_WARN([No C++17 compiler is available so no default C++ compiler is set])
  fi
fi

CXX_SAVE=${CXX}
CXX="${CXX} ${CXXSTD}"
R_PROG_OBJCXX
CXX=${CXX_SAVE}

## Only now can we look for the OpenMP flag
AC_LANG_PUSH(C++)
CXX_SAVE=${CXX}
CXX="${CXX} ${CXXSTD}"
R_OPENMP
CXX=${CXX_SAVE}
AC_LANG_POP(C++)

### OpenMP.

## The basic checking is performed via AC_OPENMP added in Autoconf 2.62,
## which we already called for determining the appropriate flags for the
## C, C++, Fortran compiler/linker.  Note that this gives
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
##    flags and OPENMP_CFLAGS and OPENMP_FFLAGS to the C and Fortran
##    compiler flags, and defining HAVE_OPENMP.
##
## (The fixed-form Fortran compiler is never used for linking by default.)

if test -n "${R_OPENMP_CFLAGS+set}"; then
  if test -n "${R_OPENMP_CFLAGS}"; then
    R_SH_VAR_ADD(MAIN_LDFLAGS, [${R_OPENMP_CFLAGS}])
    R_SH_VAR_ADD(DYLIB_LDFLAGS, [${R_OPENMP_CFLAGS}])
    AC_DEFINE(HAVE_OPENMP, 1, [Define if you have C OpenMP support.])
  fi
elif test "x${main_ld_was_given}" = xno -a "${MAIN_LD}" = "\$(CC)" -a \
        "x${dylib_ld_was_given}" = xno -a "${DYLIB_LD}" = "\$(CC)" -a \
        "x${ac_cv_prog_c_openmp}" != "xunsupported"; then
  R_OPENMP_CFLAGS="${OPENMP_CFLAGS}"
  R_SH_VAR_ADD(MAIN_LDFLAGS, [${OPENMP_CFLAGS}])
  R_SH_VAR_ADD(DYLIB_LDFLAGS, [${OPENMP_CFLAGS}])
  AC_DEFINE(HAVE_OPENMP, 1, [Define if you have C OpenMP support.])
else
  R_OPENMP_CFLAGS= 
fi
## Currently unused: see comment in Makeconf.in
if test -z "${R_OPENMP_FFLAGS+set}" -a \
        "x${ac_cv_prog_fc_openmp}" != "xunsupported"; then
  R_OPENMP_FFLAGS="${OPENMP_FCFLAGS}"
fi
AC_SUBST(R_OPENMP_CFLAGS)
AC_SUBST(R_OPENMP_FFLAGS)

## For compiling package code, we use SHLIB_FCLD, SHLIB_CXXLD or
## SHLIB_LD for linking, depending on whether the package contains
## free-form Fortran code, C++ (or ObjC) code, or "just" C and fixed-form Fortran.
## However, we (currently) do not conditionalize compilation flags.  So
## the only "safe" thing we can do for now is:
##
##    If none of SHLIB_LD, SHLIB_CXXLD and SHLIB_FCLD were specified by
##    the user and equal CC, CXX and FC, respectively, and these were
##    determined to support OpenMP, the we try to provide OpenMP support
##    for packages by adding OPENMP_FCFLAGS, OPENMP_CXXFLAGS and
##    OPENMP_CFLAGS to the respective linker flags, and add the OPENMP
##    flags to all (C, C++ and Fortran) compiler flags.

R_PTHREAD

### Now we have found all the flags, we need to use them to test appropriately.
### We don't currently have any C++ tests, but future-proof.
### In principle we should do this before testing for C-Fortran compatibility.

CPPFLAGS_KEEP=${CPPFLAGS}
CFLAGS_KEEP=${CFLAGS}
FFLAGS_KEEP=${FFLAGS}
CXXFLAGS_KEEP=${CXXFLAGS}
CPPFLAGS="${CPPFLAGS} ${R_XTRA_CPPFLAGS}"
if test "${want_R_shlib}" = yes; then
  CFLAGS="${R_XTRA_CFLAGS} ${CFLAGS} ${CPICFLAGS}"
  FFLAGS="${R_XTRA_FFLAGS} ${FFLAGS} ${FPICFLAGS}"
  CXXFLAGS="${R_XTRA_CXXFLAGS} ${CXXFLAGS} ${CXXPICFLAGS}}"
else
  CFLAGS="${R_XTRA_CFLAGS} ${CFLAGS}"
  FFLAGS="${R_XTRA_FFLAGS} ${FFLAGS}"
  CXXFLAGS="${R_XTRA_CXXFLAGS} ${CXXFLAGS}"
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
## but we had substitutes for expm1 hypot log1p and (internally) nearbyint[l]
## FreeBSD used to lack log1pl, but 6.0 had it
## FreeBSD 8.3 first had log2
## FreeBSD 8.3 first had nearbyintl/rintl (from 2012)
## https://www.freebsd.org/cgi/man.cgi?query=log&apropos=0&sektion=3&manpath=FreeBSD+11-current&format=html will give an overview.
## Cygwin and FreeBSD lacked powl (FreeBSD 10.4 (2017-10) has it).
## Cygwin had rintl but not nearbyintl
## https://cygwin.com/cygwin-api/compatibility.html says it now has powl and nearbyintl .
## powl nearbyintl and rintl are only used in format.c
R_CHECK_FUNCS([expm1 hypot log1p log1pl log2 log10 nearbyint nearbyintl powl rintl], [#include <math.h>])
## require these from R 3.5.0.
if test "${ac_cv_have_decl_expm1}" = "no"; then
   AC_MSG_ERROR([Building R requires the 'expm1' C99 function])
fi
if test "${ac_cv_have_decl_hypot}" = "no"; then
   AC_MSG_ERROR([Building R requires the 'hypot' C99 function])
fi
if test "${ac_cv_have_decl_log1p}" = "no"; then
   AC_MSG_ERROR([Building R requires the 'log1p' C99 function])
fi
if test "${ac_cv_have_decl_nearbyint}" = "no"; then
   AC_MSG_ERROR([Building R requires the 'nearbyint' C99 function])
fi
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

## Functions from ISO/IEC TS 18661-4:2015 C11 extensions.
## For now, do not define _GNU_SOURCE here.
## All but pown have long been in Solaris' libsunmath
## macOS has __cospi __sinpi __tanpi
AC_CHECK_FUNCS([atanpi atan2pi cospi exp10 pown sinpi tanpi __cospi __sinpi __tanpi], 
[#define __STDC_WANT_IEC_60559_FUNCS_EXT__ 1
#include <math.h>
#ifdef HAVE_SUNMATH_H
#include <sunmath.h>
#endif])

## fseeko/ftello are POSIX, may be macros
AC_CHECK_FUNCS(fseeko ftello)

## POSIX functions
R_CHECK_FUNCS([fcntl], [#include <fcntl.h>])
R_CHECK_FUNCS([getgrgid], [#include <grp.h>])
R_CHECK_FUNCS([getpwnam getpwuid], [#include <pwd.h>])
R_CHECK_FUNCS([kill sigaction sigaltstack sigemptyset], [#include <signal.h>])
R_CHECK_FUNCS([fdopen popen], [#include <stdio.h>])
if test "${ac_cv_have_decl_popen}" = "no"; then
   AC_MSG_ERROR([Building R requires the 'popen' system call])
fi
R_CHECK_FUNCS([getline], [#include <stdio.h>])
R_CHECK_FUNCS([select], 
[#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>  /* POSIX >= 2001 */
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>  /* Earlier POSIX, HP-UX? */
#endif])
case "${host_os}" in
  mingw*|windows*|winnt|msys)
  ;;
  *)
  if test "${ac_cv_have_decl_select}" = "no"; then
     AC_MSG_ERROR([Building R on Unix requires the 'select' system call])
  fi
  if test "${ac_cv_header_sys_select_h}" = "no" -a "${ac_cv_header_sys_times_h} = "no""; then
     AC_MSG_ERROR([Building R on Unix requires either <sys/select.h> or <sys/time.h>])
  fi
  ;;
esac
## Windows has neither setenv nor unsetenv
R_CHECK_FUNCS([setenv unsetenv], [#include <stdlib.h>])
R_CHECK_FUNCS([getrlimit setrlimit getrusage getpriority], [#include <sys/resource.h>])
R_CHECK_FUNCS([chmod mkfifo stat umask], [#include <sys/stat.h>])
if test "${ac_cv_have_decl_stat}" = "no"; then
   AC_MSG_ERROR([Building R requires the 'stat' system call])
fi
R_CHECK_FUNCS([gettimeofday utimes], [#include <sys/time.h>])
R_CHECK_FUNCS([times], [#include <sys/times.h>])
dnl These are POSIX functions included by C23
R_CHECK_FUNCS([gmtime_r localtime_r], [#include <time.h>])
dnl BSD extension available on glibc and macOS -- but fails on maOS pre-1900
dnl R_CHECK_FUNCS([timegm], [#include <time.h>])
R_CHECK_FUNCS([nl_langinfo], [#include <langinfo.h>])
R_CHECK_FUNCS([access chdir execv ftruncate getcwd geteuid getuid link readlink symlink sysconf],
[#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif])
## Linux-specific feature allowing us to fix CPU affinity for parallel
R_CHECK_FUNCS([sched_setaffinity sched_getaffinity], [#include <sched.h>])
## utime was declared obsolescent in POSIX 2008 (use utimes instead)
R_CHECK_FUNCS([utime], [#include <utime.h>])
## POSIX.1-2008 preferred form
R_CHECK_FUNCS([utimensat], [#include <sys/stat.h>])
## clock_gettime is POSIX 1993, but not on macOS prior to 10.12 (Sierra)
## Some OSes need -lrt: Linux (for glibc versions before 2.17), Solaris,
## not FreeBSD. 
## Unsurprising, as POSIX 2008 moved it from its timers section to base.
AC_CHECK_LIB(rt, clock_gettime)
R_CHECK_FUNCS([clock_gettime], [#include <time.h>])
## timespec_get is C11.
## macOS added timespec_get in 10.15 but used 10.15's SDK for 10.14 (Darwin 18)
case "${host_os}" in
   darwin1[[012345678]].*)
   ;;
   *)
     R_CHECK_FUNCS([timespec_get], [#include <time.h>])
   ;;
esac
## We need setenv or putenv.  It seems that everyone does have
## putenv, as earlier versions of R would have failed without it.
## It is not always declared, so we do not require a declaration.
AC_CHECK_FUNCS(putenv)
AC_CHECK_DECLS([putenv], , , [#include <stdlib.h>])
## this is a GNU extension so usually hidden.  Not in Solaris 10
AC_CHECK_FUNCS(vasprintf)
AC_CHECK_DECLS([vasprintf], , , [#include <stdio.h>])
## mempcpy is a GNU extension used by the included gettext.  Not in Solaris 10
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
AC_CHECK_FUNCS([dladdr dlsym])
AC_CHECK_DECLS([dladdr,dlsym], , , [#include<dlfcn.h>])
AC_CHECK_DECLS([RTLD_DEFAULT,RTLD_NEXT], , , [#include<dlfcn.h>])
AC_CHECK_HEADERS([thread.h])
R_CHECK_FUNCS([thr_stksegment], [
#include <thread.h>
#include <signal.h>
])
## stpcpy is POSIX 2008 (extension to C)
AC_CHECK_FUNCS([stpcpy])

## Lots of uses of getwd(), file.access(), Sys.glob().
## We don't need times() except as a fallback for getrusage and
## clock_gettime/gettimeofday -- but it is older and always there.
case "${host_os}" in
  mingw*|msys)
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

## Do we need substitutes?
## mkdtemp is not on Solaris 10, added in POSIX 2008
## strdup strncasecmp were first required in POSIX 2001.  strdup is C23
AC_REPLACE_FUNCS([mkdtemp strdup strncasecmp])
## Enable declarations in Defn.h?
AC_CHECK_DECLS([mkdtemp, strdup, strncasecmp])

AC_SEARCH_LIBS(connect, [socket])
# gethostbyname was removed in POSIX 2008 (in favour of getaddrinfo, POSIX 2001)
AC_SEARCH_LIBS(gethostbyname, [nsl socket])
R_FUNC_CALLOC
if test "${ac_cv_have_decl_isfinite}" = "yes"; then
R_FUNC_ISFINITE
fi
## check accuracy of log1p
## issues are probably ancient history: 
## reported for NetBSD 1.6 and OpenBSD 3.2 (both 2002).
R_FUNC_LOG1P
R_FUNC_FTELL
R_FUNC_SIGACTION

dnl setting the size of an 'enum' is a C23 addition,
dnl also supported in some versions of clang.
dnl GCC only supports it in a C23 mode.
R_ENUM_BASE_TYPE

if test x${use_internal_tzcode} = xdefault; then
case "${host_os}" in
   darwin*)
     use_internal_tzcode=yes;
     ;; 
esac
fi
if test "${use_internal_tzcode}" != yes; then
  R_WORKING_MKTIME
  if test "x${r_cv_working_mktime}" != xyes; then
    AC_MSG_ERROR([date-time conversions do not work in 2020 -- is tzdata installed?])
  fi
  R_MKTIME_ERRNO
  R_FUNC_MKTIME
  R_FUNC_MKTIME2
  R_FUNC_MKTIME3
else
  AC_DEFINE(MKTIME_SETS_ERRNO, 1, [Define if mktime sets errno.])
  AC_DEFINE(HAVE_WORKING_MKTIME_AFTER_2037, 1,
            [Define if your mktime works correctly after 2037.])
  AC_DEFINE(HAVE_WORKING_MKTIME_BEFORE_1902, 1,
            [Define if your mktime works correctly before 1902.])
  AC_DEFINE(HAVE_WORKING_MKTIME_BEFORE_1970, 1,
            [Define if your mktime works correctly before 1970.])
fi

if test x${use_internal_iswxxxxx} = xdefault; then
case "${host_os}" in
   darwin*)
     use_internal_iswxxxxx=yes;
     ;; 
   aix*)
     use_internal_iswxxxxx=yes;
     ;; 
   solaris*)
     use_internal_iswxxxxx=yes;
     ;; 
esac
fi
if test x${use_internal_iswxxxxx} = xyes; then
  AC_DEFINE(USE_RI18N_FNS, 1, [Define to 1 to use internal `iswprint' etc.])
fi

if test x${use_internal_wcwidth} = xyes; then
  AC_DEFINE(USE_RI18N_WIDTH, 1, [Define to 1 to use internal `wcwidth'])
fi

if test x${use_internal_towlower} = xdefault; then
case "${host_os}" in
   darwin*)
     use_internal_towlower=yes;
     ;; 
   solaris*)
     use_internal_towlower=yes;
     ;; 
esac
fi
if test x${use_internal_towlower} = xyes; then
  AC_DEFINE(USE_RI18N_CASE, 1, [Define to 1 to use internal code for `towlower' and `towupper'.])
fi

R_C99_COMPLEX
R_FUNC_CTANH

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
## This has to come *after* checking for (fixed-form) Fortran compiler/converter
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

if test "${use_2025blas}" = yes; then
   if test "${acx_lapack_ok}" = "yes"; then
     AC_MSG_ERROR([--with-2025blas cannot be used with --with-lapack])
  fi
fi
AM_CONDITIONAL(BUILD_2025BLAS, [test "${use_2025blas}" = yes])

AM_CONDITIONAL(BLAS_SHLIB, [test "x${use_blas_shlib}" = xyes])

case "${host_os}" in
  darwin*)
    ## In order to allow the R build to be relocatable, we strip paths
    ## from all shlibs and rely on DYLD_LIBRARY_PATH. Unfortunately
    ## Darwin linker ignores it at build-time and doesn't use -L to
    ## resolve dylib dependencies, so libRblas will not be found unless
    ## we tell ld where it lives. I don't know of any more elegant solution :/
    if test "x${use_blas_shlib}" = xyes; then
      LIBR="${LIBR} -dylib_file libRblas.dylib:\"\$(R_HOME)/lib\$(R_ARCH)/libRblas.dylib\""
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
  BLAS_LIBS="-L\"\$(R_HOME)/lib\$(R_ARCH)\" -lRblas"
fi

AM_CONDITIONAL(USE_VECLIB_G95FIX, [test "x${use_veclib_g95fix}" = xyes])
AM_CONDITIONAL(USE_EXTERNAL_BLAS, [test "${acx_blas_ok}" = "yes"])

## LAPACK.
## The default is "unset"
if test "${use_lapack}" = "unset"; then
  R_LAPACK_SYSTEM_LIB
elif test "${use_lapack}" = "yes"; then
  R_LAPACK_LIBS
fi
if test "${acx_lapack_ok}" != "yes"; then
  LAPACK_LIBS="-L\"\$(R_HOME)/lib\$(R_ARCH)\" -lRlapack"
  AC_MSG_NOTICE([using internal LAPACK sources])
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

## XDR headers and library routines.
R_XDR

## zlib headers and libraries.
R_ZLIB

## bzlib headers and libraries.
R_BZLIB

## LZMA headers and libraries from xz-utils
R_LZMA

## Zstd headers and libraries.
R_ZSTD

## libdeflate headers and libraries.
R_LIBDEFLATE

AC_ARG_WITH([libdeflate-compression],
[AS_HELP_STRING([--with-libdeflate-compression],[use libdeflate (if available) to (de)compress lazy-loaded R objects @<:@yes@:>@])],
[use_libdeflate=${withval}],
[use_libdeflate=yes])
if test "x$use_libdeflate" = xyes; then
  AC_DEFINE(USE_LIBDEFLATE, 1, [Define to use libdefault rather than libz for lazy-loaded R objects])
fi

## PCRE[12] headers and libraries.
R_PCRE

## tre headers and libraries.
R_TRE

## libcurl
R_LIBCURL


## Bitmap headers and libraries.
if test -n "${PKG_CONFIG}"; then
R_BITMAPS2
else
R_BITMAPS
fi

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
else
  AC_CHECK_HEADERS([valgrind/memcheck.h])
  if test "${ac_cv_header_valgrind_memcheck_h}" != yes; then
  AC_MSG_ERROR([system valgrind headers not found.])
  fi
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
/* This might get optimized out if not used */
extern void * __libc_stack_end;

int main () {
    if(!__libc_stack_end) exit(1);
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
## provide both (SunOS and HP-UX), and in those cases we choose lpr.
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
: ${JAVA_LIBS='~autodetect~'}
: ${JAVA_CPPFLAGS='~autodetect~'}
: ${JAVA_LD_LIBRARY_PATH='~autodetect~'}
custom_JAVA_LIBS="${JAVA_LIBS}"
custom_JAVA_CPPFLAGS="${JAVA_CPPFLAGS}"
custom_JAVA_LD_LIBRARY_PATH="${JAVA_LD_LIBRARY_PATH}"
custom_JAVA="${JAVA}"
custom_JAVAC="${JAVAC}"
custom_JAVAH="${JAVAH}"
custom_JAR="${JAR}"
AC_SUBST(custom_JAVA_HOME)
AC_SUBST(custom_JAVA_CPPFLAGS)
AC_SUBST(custom_JAVA_LIBS)
AC_SUBST(custom_JAVA_LD_LIBRARY_PATH)
AC_SUBST(custom_JAVA)
AC_SUBST(custom_JAVAC)
AC_SUBST(custom_JAVAH)
AC_SUBST(custom_JAR)
JAVA_LD_LIBRARY_PATH=
AC_SUBST(JAVA)
AC_SUBST(JAVAC)
AC_SUBST(JAVAH)
AC_SUBST(JAR)
AC_SUBST(JAVA_LD_LIBRARY_PATH)
AC_SUBST(JAVA_LIBS0)
AC_SUBST(JAVA_CPPFLAGS0)
AC_SUBST(R_JAVA_LD_LIBRARY_PATH)


## Free-form Fortran support (compiler chosen way above).
AC_LANG_PUSH(Fortran)
## The next two are needed to get IBM's xlf* and Intel's ifc
## to accept these extensions (but neither are current).
## They set FCFLAGS_f9x used in etc/Makeconf.in
AC_FC_SRCEXT(f90, [dummy=1], [dummy=0])
AC_FC_SRCEXT(f95, [dummy=1], [dummy=0])
## We probably should be calling AC_FC_FREEFORM: adds to FCFLAGS
R_OPENMP
AC_LANG_POP()

## SHLIB_FCLD and {SHLIB_FCLDFLAGS are legacy, but might be used in a Makefile
if test -z "${SHLIB_FCLD}"; then
  shlib_fcld_was_given=no
  SHLIB_FCLD="\$(FC)"
fi
: ${SHLIB_FCLDFLAGS="${SHLIB_LDFLAGS}"}
AC_SUBST(SHLIB_FCLD)
AC_SUBST(SHLIB_FCLDFLAGS)

## x86 Solaris' f95 needs additional libs when building a DSO
FCLIBS_XTRA=${FCLIBS_XTRA}
AC_SUBST(FCLIBS_XTRA)

## OpenMP package stuff (needs to come *after* configuration of all
## compilers).
## We allow the users to override this for packages only (for now)
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
  ## next macro is copied into Rconfig.h
  AC_DEFINE(SUPPORT_OPENMP, 1,
            [Define if you have C/C++/Fortran OpenMP support for package code.])
fi
## This assumes that OpenMP support in the C compiler includes compilation by FC
if test -z "${SHLIB_OPENMP_CFLAGS+set}" -a \
        "x${shlib_ld_was_given}" = xno -a \
        "${SHLIB_LD}" = "\$(CC)" -a \
        "x${ac_cv_prog_c_openmp}" != "xunsupported" -a \
        "x${ac_cv_prog_c_openmp}" != "x"; then
  SHLIB_OPENMP_CFLAGS="${OPENMP_CFLAGS}"
  if test -z "${SHLIB_OPENMP_FFLAGS+set}" -a \
          "x${ac_cv_prog_fc_openmp}" != "xunsupported"; then
    SHLIB_OPENMP_FFLAGS="${OPENMP_FCFLAGS}"
  fi
fi
if test -z "${SHLIB_OPENMP_CXXFLAGS+set}" -a \
        "x${shlib_cxxld_was_given}" = xno -a \
        "${SHLIB_CXXLD}" = "\$(CXX)" -a \
        "x${ac_cv_prog_cxx_openmp}" != "xunsupported"; then
  SHLIB_OPENMP_CXXFLAGS="${OPENMP_CXXFLAGS}"
fi

AC_SUBST(SHLIB_OPENMP_CFLAGS)
AC_SUBST(SHLIB_OPENMP_CXXFLAGS)
AC_SUBST(SHLIB_OPENMP_FFLAGS)


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

## On macOS (Darwin) this used to have /usr/X11R6/lib
## which shadows the OpenGL framework but we add nothing on macOS ....
case "${host_os}" in
  linux*)
    r_ld_library_defaults="/usr/lib64:/lib64:/usr/lib:/lib"
    ;;
  solaris*)
    r_ld_library_defaults="/usr/lib:/lib"
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
dnl This is no longer valid (if it ever was)
dnl    XTRA_INTL_CPPFLAGS=-I/System/Library/Frameworks/CoreFoundation.framework/Headers
dnl On a macOS build which uses static libraries, libintl_dgettext
dnl etc are provided by libR.dylib.
     LIBINTL_PKGS=
   else
     LIBINTL_PKGS=${LIBINTL}
  fi
  echo "Finished configuring src/extra/intl directory"
  echo
else
  USE_INCLUDED_LIBINTL=no
fi
dnl AC_SUBST(XTRA_INTL_CPPFLAGS)
AC_SUBST(LIBINTL_PKGS)

AM_CONDITIONAL(USE_NLS, [test "x${USE_NLS}" = xyes])
AM_CONDITIONAL(BUILD_LIBINTL, [test "x${USE_INCLUDED_LIBINTL}" = xyes])

R_OPENMP_SIMDRED

### shell for use in scripts: we allow R_SHELL to set the script,
### since some AIX and macOS systems have zsh as sh.
: ${R_SHELL=${SHELL}}
AC_SUBST(R_SHELL)
AC_MSG_RESULT([using as R_SHELL for scripts ... ${R_SHELL}])


### * Win32 overrides
case "${host_os}" in
  mingw*|msys)
    ## there are not needed with Rtools (detected correctly)
    AC_DEFINE(HAVE_ICONV, 1, [Define if you have the iconv() function.])
    AC_DEFINE(HAVE_ICONVLIST, 1, [Define if you have the `iconvlist' function.])
    AC_DEFINE(HAVE_ICONV_H, 1, [Define to 1 if you have the <iconv.h> header file.])
    AC_DEFINE(HAVE_JPEG, 1,[Define if you have the JPEG headers and libraries.])
    AC_DEFINE(HAVE_PNG, 1, [Define if you have the PNG headers and libraries.])
    AC_DEFINE(HAVE_TIFF, 1, [Define this if libtiff is available.])    

    ## these change what is detected with Rtools
    AC_DEFINE(HAVE_POSIX_SETJMP, 1, [Define if you have POSIX.1 compatible
                                     sigsetjmp/siglongjmp.])
    AC_DEFINE(HAVE_DECL_SIGLONGJMP, 1, [Define to 1 if you have the declaration of `siglongjmp',
                                       and to 0 if you don't.])
    AC_DEFINE(HAVE_DECL_SIGSETJMP, 1, [Define to 1 if you have the declaration of `sigsetjmp',
                                       and to 0 if you don't.])                                     
    AC_DEFINE(HAVE_TCLTK, 1, [Define if you have the Tcl/Tk headers and 
                              libraries and want Tcl/Tk support to be built.])
    want_R_profiling=yes
    AC_DEFINE(R_PROFILING, 1, [Define this to enable R-level profiling.])
    want_memory_profiling=yes
    AC_DEFINE(R_MEMORY_PROFILING, 1, [Define this to enable memory profiling.])
    ;;
esac

AC_ARG_WITH([newAccelerate],
[AS_HELP_STRING([--with-newAccelerate],[use macOS Accelerate in macOS >= 13.3 for BLAS and LAPACK. @<:@no@:>@])],
[use_accelerate=${withval}],
[use_accelerate=no])

if test "x$use_accelerate" = xlapack; then
  use_accelerate=yes
  use_accelerate_lapack=yes
else
  use_accelerate_lapack=no
fi

if test "x$use_accelerate" = xyes; then
  if test "x$use_blas" = xyes; then
    AC_MSG_ERROR([--with-newAccelerate cannot be used with --with-blas])
  fi
  if test "x$use_lapack" = xyes; then
    AC_MSG_ERROR([--with-newAccelerate cannot be used with --with-lapack])
  fi
fi
dnl We test for macOS then for an entry point in -framework Accelerate
dnl Alternatively we could #include <Accelerate/Acclereate.h> and
dnl use nm on the object.
if test "x$use_accelerate" = xyes; then
  case "${host_os}" in
      darwin*)
      ;;
      *)
       AC_MSG_ERROR([--with-newAccelerate requires macOS.])
      ;;    
  esac
  r_save_LIBS="${LIBS}"
  LIBS="${LIBS} -framework Accelerate"
  AC_LINK_IFELSE([AC_LANG_SOURCE(
[[
// avoid interpretation of $
#define ndgemm dgemm ## $ ## NEWLAPACK
// Accelerate.h declares dgemm_ but remaps to entry point dgemm$NEWLAPACK
void
ndgemm(const char *transa, const char *transb, const int *m,
       const int *n, const int *k, const double *alpha,
       const double *a, const int *lda,
       const double *b, const int *ldb,
       const double *beta, double *c, const int *ldc);

    int main(void)
    {
        int nrx, ncx, nry, ncy;
        double *x,  *y,  *z;
        double one = 1.0, zero = 0.0;
        ndgemm("N", "N", &nrx, &ncy, &ncx, &one, x,
               &nrx, y, &nry, &zero, z, &nrx);
    }
]])],
sdk_OK=yes,
sdk_OK=no)
  LIBS="${r_save_LIBS}"
  if test "x$sdk_OK" != xyes; then
    AC_MSG_ERROR([--with-newAccelerate requires macOS SDK >= 14.3.])
  fi
dnl No longer used as we only build (or not) BLAS and LAPACK shims
dnl  AC_DEFINE(USE_NEW_ACCELERATE, 1, [Define to use Apple's (new) Accelerate])
fi
AM_CONDITIONAL(BUILD_NEW_ACCELERATE, [test "x${use_accelerate}" = xyes])
AM_CONDITIONAL(BUILD_NEW_ACCELERATE_LAPACK,
              [test "x${use_accelerate_lapack}" = xyes])

dnl record compiler versions
CC_VER=`${CC} --version | ${SED} -n 1p`
FC_VER=`${FC} --version | ${SED} -n 1p`
AC_DEFINE_UNQUOTED(CC_VER, "${CC_VER}", [Version of C Compiler])
AC_SUBST(CC_VER)
AC_DEFINE_UNQUOTED(FC_VER, "${FC_VER}", [Version of Fortran Compiler])
AC_SUBST(REC_INSTALL_OPT)

dnl Allow addiitonal options for installing recommended packages
AC_SUBST(FC_VER)


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

## allow different LIBS in etc/Makeconf
   LIBS_PKGS=${LIBS}
case "${host_os}" in
   darwin*)
   ## This will usually include -lpcre2-8 -ldeflate -lzstd -llzma all
   ## of which are static and not part of a binary distribution, so we
   ## strip them.  This is macOS, so could use sed -E (which is POSIX)
   LIBS_PKGS=`echo ${LIBS} | sed s/\ -lpcre2-8// | sed s/\ -ldeflate// | sed s/\ -lzstd// | sed s/\ -llzma//`
   ;;
esac
AC_SUBST(LIBS_PKGS)

AC_OUTPUT

## Summarize configure results.
## <NOTE>
## Doing this via AC_CONFIG_COMMANDS would require explicitly passing all
## configure variables to config.status.
## </NOTE>
r_c_compiler="${CC} ${R_XTRA_CFLAGS} ${CFLAGS}"
## R_XTRA_CXXFLAGS is currently unused, but if it were applies to all dialects
r_cxx_compiler="${CXX} ${CXXSTD} ${R_XTRA_CXXFLAGS} ${CXXFLAGS}"
r_cxx11_compiler="${CXX11} ${CXX11STD} ${R_XTRA_CXXFLAGS} ${CXX11FLAGS}"
r_cxx14_compiler="${CXX14} ${CXX14STD} ${R_XTRA_CXXFLAGS} ${CXX14FLAGS}"
r_cxx17_compiler="${CXX17} ${CXX17STD} ${R_XTRA_CXXFLAGS} ${CXX17FLAGS}"
r_cxx20_compiler="${CXX20} ${CXX20STD} ${R_XTRA_CXXFLAGS} ${CXX20FLAGS}"
r_cxx23_compiler="${CXX23} ${CXX23STD} ${R_XTRA_CXXFLAGS} ${CXX23FLAGS}"
r_f77_compiler="${FC} ${R_XTRA_FFLAGS} ${FFLAGS}"
r_f95_compiler="${FC} ${R_XTRA_FFLAGS} ${FCFLAGS}"
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
if test "${r_cv_have_pcre2utf}" = yes; then
  R_SH_VAR_ADD(r_external_libs, [pcre2], [, ])
elif test "${r_cv_have_pcre832}" = yes; then
  R_SH_VAR_ADD(r_external_libs, [pcre1], [, ])
fi
if test "${use_readline}" = yes; then
  R_SH_VAR_ADD(r_external_libs, [readline], [, ])
fi
if test "${acx_blas_ok}" = "yes"; then
  ## Try to figure out which BLAS was used.
  case "${BLAS_LIBS0}" in
    *-latlas*) r_blas=ATLAS ;;
    *-lgoto*) r_blas=Goto ;;
    *-lopenblas*) r_blas=OpenBLAS ;;
    *-lmkl*) r_blas=MKL ;;
    *-lsgemm*) r_blas=PhiPack ;;
    *sunperf*) r_blas=SunPerf ;;
    *-lessl*)  r_blas=ESSL ;;
    *Accelerate*)  r_blas=Accelerate ;;
    *-lflexiblas*)  r_blas=FlexiBlas ;;
    *-lblis*)  r_blas=BLIS ;;
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
if test "x$have_libdeflate" = xyes; then
  R_SH_VAR_ADD(r_external_libs, [libdeflate], [, ])
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
if test "x$have_libdeflate" = xyes && test "x$use_libdeflate" = xyes; then
  R_SH_VAR_ADD(r_options, [libdeflate for lazyload], [, ])
fi

if test "x$have_libdeflate" != xyes && test "x$use_libdeflate" = xyes; then
  AC_MSG_WARN([--with-libdeflate-compression requires libdeflate])
fi
if test "${use_accelerate}" = yes; then
  R_SH_VAR_ADD(r_options, [new Accelerate], [, ])
fi

AC_MSG_RESULT(
[
R is now configured for ${host}

  Source directory:            ${srcdir}
  Installation directory:      ${prefix}

  C compiler:                  ${r_c_compiler}
  Fortran fixed-form compiler: ${r_f77_compiler}

  Default C++ compiler:        ${r_cxx_compiler}
dnl  C++11 compiler:              ${r_cxx11_compiler}
dnl  C++14 compiler:              ${r_cxx14_compiler}
dnl  C++17 compiler:              ${r_cxx17_compiler}
dnl  C++20 compiler:              ${r_cxx20_compiler}
dnl  C++23 compiler:              ${r_cxx23_compiler}
dnl We never reported the C++26 compiler (nor C17, C99 ...)
  Fortran free-form compiler:  ${r_f95_compiler}
  Obj-C compiler:	       ${r_objc_compiler}

  Interfaces supported:        ${r_interfaces}
  External libraries:          ${r_external_libs}
  Additional capabilities:     ${r_capabilities}
  Options enabled:             ${r_options}

  Capabilities skipped:        ${r_no_capabilities}
  Options not enabled:         ${r_no_options}

  Recommended packages:        ${use_recommended_packages} 
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
if test "${ac_cv_sizeof_size_t}" = 4; then
  AC_MSG_WARN([32-bit build -- this is little tested])
fi
if test "${USE_INCLUDED_LIBINTL}" = "yes"; then
  AC_MSG_WARN([Using the bundled libintl is deprecated.])
fi


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
