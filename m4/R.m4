### R.m4 -- extra macros for configuring R		-*- Autoconf -*-
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
### `http://www.gnu.org/copyleft/gpl.html', or by writing to the Free
### Software Foundation, 59 Temple Place -- Suite 330, Boston, MA
### 02111-3307, USA.

### * General support macros

## R_ARG_USE
## ---------
AC_DEFUN([R_ARG_USE],
[if test "${withval}" = no; then
  use_$1=no
else
  use_$1=yes
fi
])# R_ARG_USE

### * Programs

## R_PROG_AR
## ---------
AC_DEFUN([R_PROG_AR],
[AC_CHECK_PROGS(AR, [${AR} ar])
: ${ARFLAGS="rc"}
AC_SUBST(ARFLAGS)
])# R_PROG_AR

## R_PROG_INSTALL
## --------------
## Find a good install program.
## We like to provide INSTALL in the top-level Makeconf.
## Hence, if AC_PROG_INSTALL ends up with the 'tools/install-sh' script,
## we need to ensure a relative path (i.e., in case we build in srcdir)
## gets prefixed by top_srcdir.  (This would not be necessary if we had
## '@INSTALL@' in each Makefile.in, but we prefer to share ...)
AC_DEFUN([R_PROG_INSTALL],
[AC_REQUIRE([AC_PROG_INSTALL])
case "${INSTALL}" in
  [[\\/]]* | ?:[[\\/]]* ) # absolute
    ;;
  *)
    INSTALL="\$\(top_srcdir\)/tools/install-sh -c"
    ;;
esac
])# R_PROG_INSTALL

## R_PROG_PAGER
## ------------
AC_DEFUN([R_PROG_PAGER],
[AC_PATH_PROGS(PAGER, [${PAGER} less more page pg], false)
if test "${PAGER}" = false; then
  warn_pager="I could not determine a pager"
  AC_MSG_WARN([${warn_pager}])
fi
])# R_PROG_PAGER

## R_PROG_PERL
## -----------
AC_DEFUN([R_PROG_PERL],
[AC_PATH_PROGS(PERL, [${PERL} perl])
if test -n "${PERL}"; then
  _R_PROG_PERL_VERSION
else
  ## <NOTE>
  ## Need a full path for '@PERL@' substitutions when starting Perl
  ## scripts with a line of the form '#! FOO'.
  AC_PATH_PROGS(FALSE, false)
  PERL="${FALSE}"
  ## </NOTE>
fi
if test "${r_cv_prog_perl_v5}" = yes; then
  NO_PERL5=false
else
  warn_perl5="you cannot build the object documentation system"
  AC_MSG_WARN([${warn_perl5}])
  NO_PERL5=true
fi
AC_SUBST(NO_PERL5)
])# R_PROG_PERL

## _R_PROG_PERL_VERSION
## --------------------
## Building the R documentation system (Rdconv and friends) requires
## Perl version 5.005 or better.
## Set shell variable r_cv_prog_perl_v5 to 'yes' if a recent enough
## Perl is found, and to 'no' otherwise.
AC_DEFUN([_R_PROG_PERL_VERSION],
[AC_CACHE_CHECK([whether perl version is at least 5.005],
                [r_cv_prog_perl_v5],
[if ${PERL} -e 'require 5.005 or exit 1'; then
  r_cv_prog_perl_v5=yes
else
  r_cv_prog_perl_v5=no
fi])
])# _R_PROG_PERL_VERSION

## R_PROG_TEXMF
## ------------
AC_DEFUN([R_PROG_TEXMF],
[AC_REQUIRE([R_PROG_PERL])
AC_PATH_PROGS(DVIPS, [${DVIPS} dvips], false)
AC_PATH_PROGS(TEX, [${TEX} tex], false)
AC_PATH_PROGS(LATEX, [${LATEX} latex], false)
if test -z "${ac_cv_path_TEX}" ; then
  warn_dvi="you cannot build DVI versions of the R manuals"
elif test -z "${ac_cv_path_LATEX}"; then
  warn_dvi="you cannot build DVI versions of all the help pages"
fi
if test -n "${warn_dvi}"; then
  AC_MSG_WARN([${warn_dvi}])
fi
AC_PATH_PROGS(MAKEINDEX, [${MAKEINDEX} makeindex], false)
AC_PATH_PROGS(PDFTEX, [${PDFTEX} pdftex], false)
AC_PATH_PROGS(PDFLATEX, [${PDFLATEX} pdflatex], false)
if test -z "${ac_cv_path_PDFTEX}" ; then
  warn_pdf="you cannot build PDF versions of the R manuals"
elif test -z "${ac_cv_path_PDFLATEX}" ; then
  warn_pdf="you cannot build PDF versions of all the help pages"
fi
if test -n "${warn_pdf}"; then
  AC_MSG_WARN([${warn_pdf}])
fi
AC_PATH_PROGS(MAKEINFO_CMD, [${MAKEINFO} makeinfo])
## This test admittedly looks a bit strange ... see R_PROG_PERL.
if test "${PERL}" = "${FALSE}"; then
  AC_PATH_PROGS(INSTALL_INFO, [${INSTALL_INFO} install-info], false)
else
  INSTALL_INFO="\$(PERL) \$(top_srcdir)/tools/install-info.pl"
  AC_SUBST(INSTALL_INFO)
fi
: ${R_RD4DVI="ae"}
AC_SUBST(R_RD4DVI)
: ${R_RD4PDF="ae,hyper"}
AC_SUBST(R_RD4PDF)
])# R_PROG_TEXMF

## R_PROG_MAKEINFO
## ---------------
AC_DEFUN([R_PROG_MAKEINFO],
## This used to be part of R_PROG_TEXMF, where it really belongs.
## Unfortunately, AM_PROG_LIBTOOL unconditionally overwrites MAKEINFO
## by makeinfo or missing.  To allow users to pass a MAKEINFO setting to
## configure, we thus have to run R_PROG_TEXMF before AM_PROG_LIBTOOL,
## save the result to something not overwritten (hence MAKEINFO_CMD),
## and finally set MAKEINFO according to our needs.
[AC_REQUIRE([R_PROG_TEXMF])
AC_REQUIRE([AM_PROG_LIBTOOL])
if test -n "${MAKEINFO_CMD}"; then
  _R_PROG_MAKEINFO_VERSION
fi
if test "${r_cv_prog_makeinfo_v4}" != yes; then
  warn_info="you cannot build info versions of the R manuals"
  AC_MSG_WARN([${warn_info}])
  MAKEINFO=false
else
  MAKEINFO="${MAKEINFO_CMD}"
fi
])# R_PROG_MAKEINFO

## _R_PROG_MAKEINFO_VERSION
## ------------------------
## Building the R Texinfo manuals requires Makeinfo v4 or better.
## Set shell variable r_cv_prog_makeinfo_v4 to 'yes' if a recent
## enough Makeinfo is found, and to 'no' otherwise.
AC_DEFUN([_R_PROG_MAKEINFO_VERSION],
[AC_CACHE_CHECK([whether makeinfo version is at least 4],
                [r_cv_prog_makeinfo_v4],
[makeinfo_version=`${MAKEINFO_CMD} --version | \
  grep "^makeinfo" | sed 's/[[^)]]*) \(.\).*/\1/'`
if test -z "${makeinfo_version}"; then
  r_cv_prog_makeinfo_v4=no
elif test ${makeinfo_version} -lt 4; then
  r_cv_prog_makeinfo_v4=no
else
  r_cv_prog_makeinfo_v4=yes
fi])
])# _R_PROG_MAKEINFO_VERSION

## R_PROG_BROWSER
## --------------
AC_DEFUN([R_PROG_BROWSER],
[AC_PATH_PROGS(R_BROWSER, [netscape mozilla galeon kfmclient opera gnome-moz-remote open])
if test -z "${R_BROWSER}"; then
  warn_browser="I could not determine a browser"
  AC_MSG_WARN([${warn_browser}])
fi
AC_SUBST(R_BROWSER)
])# R_BROWSER

### * C compiler and its characteristics.

## R_PROG_CC_M
## -----------
## Check whether the C compiler accepts '-M' for generating dependencies.
AC_DEFUN([R_PROG_CC_M],
[AC_CACHE_CHECK([whether ${CC} accepts -M for generating dependencies],
                [r_cv_prog_cc_m],
[echo "#include <math.h>" > conftest.c
## No real point in using AC_LANG_* and ${ac_ext}, as we need to
## create hard-wired suffix rules.
if test -n "`${CC} -M conftest.c 2>/dev/null | grep conftest`"; then
  r_cv_prog_cc_m=yes
else
  r_cv_prog_cc_m=no
fi])
])# R_PROG_CC_M

## R_PROG_CC_C_O_LO
## ----------------
## Check whether the C compiler supports '-c -o FILE.lo'.
AC_DEFUN([R_PROG_CC_C_O_LO],
[AC_CACHE_CHECK([whether ${CC} supports -c -o FILE.lo],
                [r_cv_prog_cc_c_o_lo],
[test -d TMP || mkdir TMP
echo "int some_variable = 0;" > conftest.c
## No real point in using AC_LANG_* and ${ac_ext}, as we need to create
## hard-wired suffix rules. 
ac_try='${CC} ${CFLAGS} -c conftest.c -o TMP/conftest.lo 1>&AS_MESSAGE_LOG_FD'
if AC_TRY_EVAL(ac_try) \
    && test -f TMP/conftest.lo \
    && AC_TRY_EVAL(ac_try); then
  r_cv_prog_cc_c_o_lo=yes
else
  r_cv_prog_cc_c_o_lo=no
fi
rm -rf conftest* TMP])
])# R_PROG_CC_C_O_LO

## R_PROG_CC_MAKEFRAG
## ------------------
## Generate a Make fragment with suffix rules for the C compiler.
## Used for both building R (Makeconf) and add-ons (etc/Makeconf).
AC_DEFUN([R_PROG_CC_MAKEFRAG],
[cc_rules_frag=Makefrag.cc
AC_REQUIRE([R_PROG_CC_M])
AC_REQUIRE([R_PROG_CC_C_O_LO])
cat << \EOF > ${cc_rules_frag}
.c.o:
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -c $< -o $[@]
EOF
if test "${r_cv_prog_cc_m}" = yes; then
  cat << \EOF >> ${cc_rules_frag}
.c.d:
	@echo "making $[@] from $<"
	@$(CC) -M $(ALL_CPPFLAGS) $< | \
	  sed -e 's/^\([[^:]]*\)\.o\([[ 	]]\)*:/\1.o \1.lo\2:/' > $[@]
EOF
else
  cat << \EOF >> ${cc_rules_frag}
.c.d:
	@echo > $[@]
EOF
fi
if test "${r_cv_prog_cc_c_o_lo}" = yes; then
  cat << \EOF >> ${cc_rules_frag}
.c.lo:
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS_LO) -c $< -o $[@]
EOF
else
  cat << \EOF >> ${cc_rules_frag}
.c.lo:
	@test -d .libs || mkdir .libs
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS_LO) -c $< -o .libs/$[*].o
	mv .libs/$[*].o $[*].lo
EOF
fi
AC_SUBST_FILE(cc_rules_frag)
])# R_PROG_CC_MAKEFRAG

## R_PROG_CC_FLAG(FLAG, [ACTION-IF-TRUE])
## ---------------------------------------
## Check whether the C compiler handles command line option FLAG,
## and set shell variable r_cv_prog_cc_flag_SFLAG accordingly (where
## SFLAG is a shell-safe transliteration of FLAG).
## In addition, execute ACTION-IF-TRUE in case of success.
AC_DEFUN([R_PROG_CC_FLAG],
[ac_safe=`echo "$1" | sed 'y%./+-:=%__p___%'`
AC_MSG_CHECKING([whether ${CC} accepts $1])
AC_CACHE_VAL([r_cv_prog_cc_flag_${ac_safe}],
[AC_LANG_PUSH(C)
r_save_CFLAGS="${CFLAGS}"
CFLAGS="${CFLAGS} $1"
AC_TRY_LINK([], [],
            [eval "r_cv_prog_cc_flag_${ac_safe}=yes"],
            [eval "r_cv_prog_cc_flag_${ac_safe}=no"])
CFLAGS="${r_save_CFLAGS}"
AC_LANG_POP(C)
])
if eval "test \"`echo '$r_cv_prog_cc_flag_'$ac_safe`\" = yes"; then
  AC_MSG_RESULT([yes])
  [$2]
else
  AC_MSG_RESULT([no])
fi
])# R_PROG_CC_FLAG

## R_PROG_CC_FLAG_D__NO_MATH_INLINES
## ---------------------------
## In current glibc, inline version [x86] of exp is broken.
## We fix this by adding '-D__NO_MATH_INLINES' to R_XTRA_CFLAGS rather
## than AC_DEFINE(__NO_MATH_INLINES) as the former also takes care of
## compiling C code for add-on packages.
AC_DEFUN([R_PROG_CC_FLAG_D__NO_MATH_INLINES],
[AC_EGREP_CPP([yes],
[#include <math.h>
#if defined(__GLIBC__)
  yes
#endif
],
              [R_XTRA_CFLAGS="${R_XTRA_CFLAGS} -D__NO_MATH_INLINES"])
])# R_PROG_CC_FLAG_D__NO_MATH_INLINES

## R_C_OPTIEEE
## -----------
## Check whethter the C compiler needs '-OPT:IEEE_NaN_inf=ON' to 
## correctly deal with IEEE NaN/Inf.
## This flag is needed for the native SGI C compiler.
## If needed, add the flag to R_XTRA_CFLAGS.
AC_DEFUN([R_C_OPTIEEE],
[AC_CACHE_CHECK([whether C compiler needs -OPT:IEEE_NaN_inf=ON],
                [r_cv_c_optieee],
AC_TRY_RUN(
[#include <math.h>
#include <ieeefp.h>
int main () {
  double x = 0;
  fpsetmask(0); x = x / x; exit (x != x);
}],
	   [r_cv_c_optieee=yes],
	   [r_cv_c_optieee=no],
	   [r_cv_c_optieee=no]))
if test "${r_cv_c_optieee}" = yes; then
  R_XTRA_CFLAGS="${R_XTRA_CFLAGS} -OPT:IEEE_NaN_inf=ON"
fi
])# R_C_OPTIEEE

### * C++ compiler and its characteristics.

## R_PROG_CXX_M
## ------------
## Check whether the C++ compiler accepts '-M' for generating
## dependencies.
AC_DEFUN([R_PROG_CXX_M],
[AC_REQUIRE([R_PROG_CC_M])
AC_CACHE_CHECK([whether ${CXX} accepts -M for generating dependencies],
               [r_cv_prog_cxx_m],
[echo "#include <math.h>" > conftest.cc
## No real point in using AC_LANG_* and ${ac_ext}, as we need to create
## hard-wired suffix rules.  We could be a bit more careful as we
## actually only test suffix '.cc'. 
if test -n "`${CXX} -M conftest.cc 2>/dev/null | grep conftest`"; then
  r_cv_prog_cxx_m=yes
else
  r_cv_prog_cxx_m=no
fi])
])# R_PROG_CXX_M

## R_PROG_CXX_C_O_LO
## -----------------
## Check whether the C++ compiler supports '-c -o FILE.lo'.
AC_DEFUN([R_PROG_CXX_C_O_LO],
[cxx_o_lo_rules_frag=Makefrag.cxx
AC_CACHE_CHECK([whether ${CXX} supports -c -o FILE.lo],
               [r_cv_prog_cxx_c_o_lo],
[test -d TMP || mkdir TMP
echo "int some_variable = 0;" > conftest.cc
## No real point in using AC_LANG_* and ${ac_ext}, as we need to create
## hard-wired suffix rules.  We could be a bit more careful as we
## actually only test suffix '.cc'.
ac_try='${CXX} ${CXXFLAGS} -c conftest.cc -o TMP/conftest.lo 1>&AS_MESSAGE_LOG_FD'
if AC_TRY_EVAL(ac_try) \
    && test -f TMP/conftest.lo \
    && AC_TRY_EVAL(ac_try); then
  r_cv_prog_cxx_c_o_lo=yes
else
  r_cv_prog_cxx_c_o_lo=no
fi
rm -rf conftest* TMP])
])# R_PROG_CXX_C_O_LO

## R_PROG_CXX_MAKEFRAG
## -------------------
## Generate a Make fragment with suffix rules for the C++ compiler.
## Used for both building R (Makeconf) and add-ons (etc/Makeconf).
AC_DEFUN([R_PROG_CXX_MAKEFRAG],
[cxx_rules_frag=Makefrag.cxx
AC_REQUIRE([R_PROG_CXX_M])
AC_REQUIRE([R_PROG_CXX_C_O_LO])
cat << \EOF > ${cxx_rules_frag}
.cc.o:
	$(CXX) $(ALL_CPPFLAGS) $(ALL_CXXFLAGS) -c $< -o $[@]
.cpp.o:
	$(CXX) $(ALL_CPPFLAGS) $(ALL_CXXFLAGS) -c $< -o $[@]
.C.o:
	$(CXX) $(ALL_CPPFLAGS) $(ALL_CXXFLAGS) -c $< -o $[@]
EOF
if test "${r_cv_prog_cxx_m}" = yes; then
  cat << \EOF >> ${cxx_rules_frag}
.cc.d:
	@echo "making $[@] from $<"
	@$(CXX) -M $(ALL_CPPFLAGS) $< | \
	  sed -e 's/^\([[^:]]*\)\.o\([[ 	]]\)*:/\1.o \1.lo\2:/' > $[@]
.cpp.d:
	@echo "making $[@] from $<"
	@$(CXX) -M $(ALL_CPPFLAGS) $< | \
	  sed -e 's/^\([[^:]]*\)\.o\([[ 	]]\)*:/\1.o \1.lo\2:/' > $[@]
.C.d:
	@echo "making $[@] from $<"
	@$(CXX) -M $(ALL_CPPFLAGS) $< | \
	  sed -e 's/^\([[^:]]*\)\.o\([[ 	]]\)*:/\1.o \1.lo\2:/' > $[@]
EOF
else
  cat << \EOF >> ${cxx_rules_frag}
.cc.d:
	@echo > $[@]
.cpp.d:
	@echo > $[@]
.C.d:
	@echo > $[@]
EOF
fi
if test "${r_cv_prog_cxx_c_o_lo}" = yes; then
  cat << \EOF >> ${cxx_rules_frag}
.cc.lo:
	$(CXX) $(ALL_CPPFLAGS) $(ALL_CXXFLAGS_LO) -c $< -o $[@]
.cpp.lo:
	$(CXX) $(ALL_CPPFLAGS) $(ALL_CXXFLAGS_LO) -c $< -o $[@]
.C.lo:
	$(CXX) $(ALL_CPPFLAGS) $(ALL_CXXFLAGS_LO) -c $< -o $[@]
EOF
else
  cat << \EOF >> ${cxx_rules_frag}
.cc.lo:
	@test -d .libs || mkdir .libs
	$(CXX) $(ALL_CPPFLAGS) $(ALL_CXXFLAGS_LO) -c $< -o .libs/$[*].o
	mv .libs/$[*].o $[*].lo
.cpp.lo:
	@test -d .libs || mkdir .libs
	$(CXX) $(ALL_CPPFLAGS) $(ALL_CXXFLAGS_LO) -c $< -o .libs/$[*].o
	mv .libs/$[*].o $[*].lo
.C.lo:
	@test -d .libs || mkdir .libs
	$(CXX) $(ALL_CPPFLAGS) $(ALL_CXXFLAGS_LO) -c $< -o .libs/$[*].o
	mv .libs/$[*].o $[*].lo
EOF
fi
AC_SUBST_FILE(cxx_rules_frag)
])# R_PROG_CXX_MAKEFRAG

## R_PROG_CXX_FLAG
## ---------------
## Check whether the C++ compiler handles command line option FLAG,
## and set shell variable r_cv_prog_cc_flag_SFLAG accordingly (where
## SFLAG is a shell-safe transliteration of FLAG).
## In addition, execute ACTION-IF-TRUE in case of success.
AC_DEFUN([R_PROG_CXX_FLAG],
[ac_safe=`echo "$1" | sed 'y%./+-:=%__p___%'`
AC_MSG_CHECKING([whether ${CXX-c++} accepts $1])
AC_CACHE_VAL([r_cv_prog_cxx_flag_${ac_safe}],
[AC_LANG_PUSH(C++)
r_save_CXXFLAGS="${CXXFLAGS}"
CXXFLAGS="${CXXFLAGS} $1"
AC_TRY_LINK([], [],
	    [eval "r_cv_prog_cxx_flag_${ac_safe}=yes"],
	    [eval "r_cv_prog_cxx_flag_${ac_safe}=no"])
CXXFLAGS="${r_save_CXXFLAGS}"
AC_LANG_POP(C++)
])
if eval "test \"`echo '$r_cv_prog_cxx_flag_'$ac_safe`\" = yes"; then
  AC_MSG_RESULT([yes])
  [$2]
else
  AC_MSG_RESULT([no])
fi
])# R_PROG_CXX_FLAG

### * Fortran 77 compiler/converter and its characteristics.

## R_PROG_F77_OR_F2C
## -----------------
## Find a Fortran 77 compiler, or f2c.
##
## If we have not been forced to use a particular Fortran compiler, try
## to find one using one of the several common names.  The list is based
## on what the current autoconf CVS contains.  This says,
##
## <Quote>
## Compilers are ordered by
##  1. F77, F90, F95
##  2. Good/tested native compilers, bad/untested native compilers
##  3. Wrappers around f2c go last.
##
## 'fort77' and fc' are wrappers around 'f2c', 'fort77' being better.
## It is believed that under HP-UX 'fort77' is the name of the native
## compiler.  On some Cray systems, fort77 is a native compiler.
## cf77 and cft77 are (older) Cray F77 compilers.
## pgf77 and pgf90 are the Portland Group F77 and F90 compilers.
## xlf/xlf90/xlf95 are IBM (AIX) F77/F90/F95 compilers.
## lf95 is the Lahey-Fujitsu compiler.
## fl32 is the Microsoft Fortran "PowerStation" compiler.
## af77 is the Apogee F77 compiler for Intergraph hardware running CLIX.
## epcf90 is the "Edinburgh Portable Compiler" F90.
## </Quote>
##
## In fact, on HP-UX fort77 is the POSIX-compatible native compiler and
## f77 is not: hence we need look for fort77 first!
AC_DEFUN([R_PROG_F77_OR_F2C],
[if test -n "${F77}" && test -n "${F2C}"; then
  warn_F77_and_F2C="both 'F77' and 'F2C' given.
Using the given Fortran 77 compiler ..."
  AC_MSG_WARN([${warn_F77_and_F2C}])
  F2C=
fi
if test -n "${F77}"; then
  AC_MSG_RESULT([defining F77 to be ${F77}])
elif test -z "${F2C}"; then
  F77=
  case "${host_os}" in
    hpux*)
      AC_CHECK_PROGS(F77, [g77 fort77 f77 xlf cf77 cft77 pgf77 fl32 af77 \
                           f90 xlf90 pgf90 epcf90 f95 xlf95 lf95 g95 fc])
      ;;
    *)
      AC_CHECK_PROGS(F77, [g77 f77 xlf cf77 cft77 pgf77 fl32 af77 fort77 \
                           f90 xlf90 pgf90 epcf90 f95 xlf95 lf95 g95 fc])
      ;;
  esac
  if test -z "${F77}"; then
    AC_CHECK_PROG(F2C, f2c, f2c, [])
  fi
else
  AC_MSG_RESULT([defining F2C to be ${F2C}])
fi
if test -n "${F77}"; then
  ## If the above 'found' a Fortran 77 compiler, we run AC_PROG_F77 as
  ## this does additional testing (GNU, '-g', ...).
  AC_PROG_F77
elif test -z "${F2C}"; then
  AC_MSG_ERROR([Neither an F77 compiler nor f2c found])
fi
])# R_PROG_F77_OR_F2C

## R_PROG_F77_FLIBS
## ----------------
## Run AC_F77_LIBRARY_LDFLAGS, and fix some known problems with FLIBS.
AC_DEFUN([R_PROG_F77_FLIBS],
[AC_REQUIRE([AC_F77_LIBRARY_LDFLAGS])
## Currently g77 on Darwin links against '-lcrt1.o', which (unlike
## '-lcrt0.o') is not stripped by AC_F77_LIBRARY_LDFLAGS.  This in 
## particular causes R_PROG_F77_CC_COMPAT to fail.  Hence, we make
## sure all -lcrt?.o are removed.
## Native f90 on HPUX 11 comes up with '-l:libF90.a' causing trouble
## when using gcc for linking.  The '-l:' construction is similar to
## plain '-l' except that search order (archive/shared) given by '-a'
## is not important.  We escape such flags via '-Wl,' in case of gcc.
## Note that the current Autoconf CVS uses _AC_LINKER_OPTION for a
## similar purpose when computing FLIBS: this uses '-Xlinker' escapes
## for gcc and does nothing otherwise.  Note also that we cannot simply
## unconditionally escape with '${wl}' from libtool as on HPUX we need
## SHLIB_LD=ld for native C compilers (problem with non-PIC 'crt0.o',
## see 'Individual platform overrides' in section 'DLL stuff' in file
## 'configure.ac'.
flibs=
if test "${GCC}" = yes; then
  linker_option="-Wl,"
else
  linker_option=
fi
for arg in ${FLIBS}; do
  case "${arg}" in
    -lcrt?.o)
      ;;
    -l:*)
      flibs="${flibs} ${linker_option}${arg}"
      ;;
    *)
      flibs="${flibs} ${arg}"
      ;;
  esac
done
## Also need to deal with offending '-lcrtbegin.o' on MacOS X (Jaguar)
## with GCC 3.1 or better ...
case "${host_os}" in
  darwin*)
    flibs=`echo "${flibs}" | sed 's/\(.*\)-lcrtbegin.o\(.*\)/\1\2/'`
    ;;
esac
FLIBS="${flibs}"
## Versions of g77 up to 3.0.x only have a non-PIC (static) -lg2c which
## on some platforms means one cannot build dynamically loadable modules
## containing Fortran code.  (g77 3.1 will have a shared -lg2c too.)  As
## a workaround, Debian provides -lg2c-pic which holds pic objects only,
## and we should use in case we can find it ...
## <FIXME>
## Use FLIBS instead of ac_cv_flibs ...
if test "${G77}" = yes; then
  r_save_LIBS="${LIBS}"
  flibs=`echo "${ac_cv_flibs}" | sed 's/-lg2c/-lg2c-pic/'`
  LIBS="${LIBS} ${flibs}"
  AC_LANG_PUSH(C)
  AC_TRY_LINK([], [], [FLIBS="${flibs}"])
  AC_LANG_POP(C)
  LIBS="${r_save_LIBS}"
fi
## </FIXME>
])# R_PROG_F77_FLIBS

## R_PROG_F77_APPEND_UNDERSCORE
## ----------------------------
## See if the Fortran 77 compiler appends underscores.
## What we really should do is determine how to properly mangle the 
## names of C/C++ identifiers (potentially containing underscores) so
## that they match the name-mangling scheme used by the Fortran 77
## compiler.  Autoconf 2.50 or better has macros F77_FUNC(name, NAME)
## and F77_FUNC_(name, NAME) for this.  However, the F77_* macros in
## the R API have one argument only and therefore cannot deal with 
## Fortran 77 compilers which convert to upper case or add an extra
## underscore for identifiers containing underscores.  We give an error
## in the former case; as ISO Fortran 77 does not allow underscores in
## function names, we do nothing about the latter.
AC_DEFUN([R_PROG_F77_APPEND_UNDERSCORE],
[AC_REQUIRE([AC_F77_WRAPPERS])
## DANGER!  We really needs the results of _AC_F77_NAME_MANGLING as
## stored in the cache var ac_cv_f77_mangling which is not documented
## and hence may change ...
case "${ac_cv_f77_mangling}" in
  "upper "*)
    AC_MSG_WARN([Fortran compiler uses uppercase external names])
    AC_MSG_ERROR([cannot use Fortran])
    ;;
esac
AC_CACHE_VAL([r_cv_prog_f77_append_underscore],
[case "${ac_cv_f77_mangling}" in
  *", underscore, "*)
    r_cv_prog_f77_append_underscore=yes
    ;;
  *", no underscore, "*)
    r_cv_prog_f77_append_underscore=no
    ;;
esac])
if test -z "${r_cv_prog_f77_append_underscore}"; then
  AC_MSG_ERROR([cannot use Fortran])
fi
if test "${r_cv_prog_f77_append_underscore}" = yes; then
  AC_DEFINE(HAVE_F77_UNDERSCORE, 1,
            [Define if your Fortran compiler appends an underscore to
             external names.])
fi
])# R_PROG_F77_APPEND_UNDERSCORE

## R_PROG_F77_CC_COMPAT
## --------------------
## Check whether the Fortran 77 and C compilers agree on int and double.
AC_DEFUN([R_PROG_F77_CC_COMPAT],
[AC_REQUIRE([AC_CHECK_LIBM])
AC_MSG_CHECKING([whether ${F77} and ${CC} agree on int and double])
AC_CACHE_VAL([r_cv_prog_f77_cc_compat],
[cat > conftestf.f <<EOF
      subroutine cftest(a, b, x, y)
      integer a(3), b(2)
      double precision x(3), y(3)

      b(1) = a(3)/a(2)
      b(2) = a(3) - a(1)*a(2)
      y(1) = dble(a(3))/x(2)
      y(2) = x(3)*x(1)
      y(3) = (x(2)/x(1)) ** a(1)
      end
EOF
${F77} ${FFLAGS} -c conftestf.f 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD
## Yes we need to double quote this ...
[cat > conftest.c <<EOF
#include <math.h>
#include "confdefs.h"
#ifdef HAVE_F77_UNDERSCORE
# define F77_SYMBOL(x)   x ## _
#else
# define F77_SYMBOL(x)   x
#endif

extern void F77_SYMBOL(cftest)(int *a, int *b, double *x, double *y);

int main () {
  int a[3] = {17, 237, 2000000000}, b[2], res = 0;
  double x[3] = {3.14159265, 123.456789, 2.3e34}, z[3];
  double eps = 1e-6;
  double zres[3];
  int i, bres[2];

  zres[0] = (double) a[2]/x[1];
  zres[1] = x[2]*x[0];
  zres[2] = pow(x[1]/x[0], 17.0);
  bres[0] = a[2]/a[1];
  bres[1] = a[2] - a[0]*a[1];
  F77_SYMBOL(cftest)(a, b, x, z);
  if(b[0] != bres[0]) res++;
  if(b[1] != bres[1]) res++;
  for(i = 0; i < 3; i++)
    if(fabs(z[i]/zres[i] - 1) > eps) res++;
  printf("number of errors %d\n", res);
  exit(res);
}
EOF]
if ${CC} ${CFLAGS} -c conftest.c 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD; then
  ## <NOTE>
  ## This should really use MAIN_LD, and hence come after this is
  ## determined (and necessary additions to MAIN_LDFLAGS were made).
  ## But it seems that we currently can always use the C compiler.
  ## Also, to be defensive there should be a similar test with SHLIB_LD
  ## and SHLIB_LDFLAGS (and note that on HPUX with native cc we have to
  ## use ld for SHLIB_LD) ...
  if ${CC} ${LDFLAGS} ${MAIN_LDFLAGS} -o conftest${ac_exeext} \
       conftest.${ac_objext} conftestf.${ac_objext} ${FLIBS} \
       ${LIBM} 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD;
  ## </NOTE>
  then
    output=`./conftest${ac_exeext} 2>&1`
    if test ${?} = 0; then
      r_cv_prog_f77_cc_compat=yes
    fi
  fi
fi
])
rm -rf conftest conftest.* conftestf.* core
if test -n "${r_cv_prog_f77_cc_compat}"; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_WARN([${F77} and ${CC} disagree on int and double])
  AC_MSG_ERROR([Maybe change CFLAGS or FFLAGS?])
fi
])# R_PROG_F77_CC_COMPAT

## R_PROG_F77_CC_COMPAT_COMPLEX
## ----------------------------
## Check whether the Fortran 77 and C compilers agree on double complex.
AC_DEFUN([R_PROG_F77_CC_COMPAT_COMPLEX],
[AC_REQUIRE([AC_CHECK_LIBM])
AC_MSG_CHECKING([whether ${F77} and ${CC} agree on double complex])
AC_CACHE_VAL([r_cv_prog_f77_cc_compat_complex],
[cat > conftestf.f <<EOF
      subroutine cftest(x)
      complex*16 x(3)
      integer i

c a few tests of constructs that are sometimes missing
      if(x(1) .eq. x(1)) i = 0
      x(1) = x(1)*x(2) + x(3)
      end
EOF
${F77} ${FFLAGS} -c conftestf.f 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD
## Yes we need to double quote this ...
[cat > conftest.c <<EOF
#include <math.h>
#include "confdefs.h"
#ifdef HAVE_F77_UNDERSCORE
# define F77_SYMBOL(x)   x ## _
#else
# define F77_SYMBOL(x)   x
#endif

typedef struct {
        double r;
        double i;
} Rcomplex;

extern void F77_SYMBOL(cftest)(Rcomplex *x);

int main () {
    Rcomplex z[3];
    
    z[0].r = 3.14159265;
    z[0].i = 2.172;
    z[1].i = 3.14159265;
    z[1].r = 2.172;
    z[2].r = 123.456;
    z[2].i = 0.123456;
    F77_SYMBOL(cftest)(z);
    printf("%f %f\n", z[0].r, z[0].i);
    if(fabs(z[0].r - 123.456) < 1e-4 && fabs(z[0].i - 14.71065) < 1e-4)
	exit(0);
    else exit(1);
}
EOF]
if ${CC} ${CFLAGS} -c conftest.c 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD; then
  ## <NOTE>
  ## This should really use MAIN_LD, and hence come after this is
  ## determined (and necessary additions to MAIN_LDFLAGS were made).
  ## But it seems that we currently can always use the C compiler.
  ## Also, to be defensive there should be a similar test with SHLIB_LD
  ## and SHLIB_LDFLAGS (and note that on HPUX with native cc we have to
  ## use ld for SHLIB_LD) ...
  if ${CC} ${LDFLAGS} ${MAIN_LDFLAGS} -o conftest${ac_exeext} \
       conftest.${ac_objext} conftestf.${ac_objext} ${FLIBS} \
       ${LIBM} 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD;
  ## </NOTE>
  then
    output=`./conftest${ac_exeext} 2>&1`
    if test ${?} = 0; then
      r_cv_prog_f77_cc_compat_complex=yes
    fi
  fi
fi
])
rm -rf conftest conftest.* conftestf.* core
if test -n "${r_cv_prog_f77_cc_compat_complex}"; then
  AC_MSG_RESULT([yes])
  AC_DEFINE(HAVE_DOUBLE_COMPLEX, 1,
            [Define if C's Rcomplex and Fortran's COMPLEX*16 can be
             interchanged, and can do arithmetic on the latter.])
else
  warn_f77_cc_double_complex="${F77} and ${CC} disagree on double complex"
  AC_MSG_WARN([${warn_f77_cc_double_complex}])
fi
AC_SUBST(HAVE_DOUBLE_COMPLEX)
])# R_PROG_F77_CC_COMPAT_COMPLEX

## R_PROG_F77_C_O_LO
## -----------------
## Check whether the Fortran compiler supports '-c -o FILE.lo'.
AC_DEFUN([R_PROG_F77_C_O_LO],
[AC_CACHE_CHECK([whether ${F77} supports -c -o FILE.lo],
                [r_cv_prog_f77_c_o_lo],
[test -d TMP || mkdir TMP
cat > conftest.f <<EOF
      program conftest
      end
EOF
ac_try='${F77} ${FFLAGS} -c conftest.f -o TMP/conftest.lo 1>&AS_MESSAGE_LOG_FD'
if AC_TRY_EVAL(ac_try) \
    && test -f TMP/conftest.lo \
    && AC_TRY_EVAL(ac_try); then
  r_cv_prog_f77_c_o_lo=yes
else
  r_cv_prog_f77_c_o_lo=no
fi
rm -rf conftest* TMP])
])# R_PROG_F77_C_O_LO

## R_PROG_F77_MAKEFRAG
## -------------------
## Generate a Make fragment with suffix rules for Fortran 77 source
## files when using a Fortran 77 compiler.
## Used for both building R (Makeconf) and add-ons (etc/Makeconf).
AC_DEFUN([R_PROG_F77_MAKEFRAG],
[AC_REQUIRE([R_PROG_F77_C_O_LO])
f77_rules_frag=Makefrag.f77
cat << \EOF > ${f77_rules_frag}
.f.c:
.f.o:
	$(F77) $(ALL_FFLAGS) -c $< -o $[@]
EOF
if test "${r_cv_prog_f77_c_o_lo}" = yes; then
  cat << \EOF >> ${f77_rules_frag}
.f.lo:
	$(F77) $(ALL_FFLAGS_LO) -c $< -o $[@]
EOF
else
  cat << \EOF >> ${f77_rules_frag}
.f.lo:
	@test -d .libs || mkdir .libs
	$(F77) $(ALL_FFLAGS_LO) -c $< -o .libs/$[*].o
	mv .libs/$[*].o $[*].lo
EOF
fi
AC_SUBST_FILE(f77_rules_frag)
])# R_PROG_F77_MAKEFRAG

## R_PROG_F77_FLAG(FLAG, [ACTION-IF-TRUE])
## ---------------------------------------
## Check whether the Fortran 77 compiler handles command line option
## FLAG, and set shell variable r_cv_prog_f77_flag_SFLAG accordingly
## (where SFLAG is a shell-safe transliteration of FLAG).
## In addition, execute ACTION-IF-TRUE in case of success.
AC_DEFUN([R_PROG_F77_FLAG],
[ac_safe=`echo "$1" | sed 'y%./+-:=%__p___%'`
AC_MSG_CHECKING([whether ${F77} accepts $1])
AC_CACHE_VAL([r_cv_prog_f77_flag_${ac_safe}],
[AC_LANG_PUSH(Fortran 77)
r_save_FFLAGS="${FFLAGS}"
FFLAGS="${FFLAGS} $1"
AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
               [eval "r_cv_prog_f77_flag_${ac_safe}=yes"],
               [eval "r_cv_prog_f77_flag_${ac_safe}=no"])
FFLAGS="${r_save_FFLAGS}"
AC_LANG_POP(Fortran 77)
])
if eval "test \"`echo '$r_cv_prog_f77_flag_'$ac_safe`\" = yes"; then
  AC_MSG_RESULT([yes])
  [$2]
else
  AC_MSG_RESULT([no])
fi
])# R_PROG_F77_FLAG

## R_PROG_F2C_FLIBS
## ----------------
AC_DEFUN([R_PROG_F2C_FLIBS],
[AC_REQUIRE([AC_PROG_RANLIB])
AC_REQUIRE([AC_CHECK_LIBM])
AC_CACHE_VAL([r_cv_f2c_flibs],
[
## <FIXME>
## Why do we need this?  What about AC_F77_DUMMY_MAIN?
## This seems to be necessary on some Linux system. -- you bet! -pd
AC_LANG_PUSH(C)
cat > conftest.${ac_ext} << EOF
int MAIN_ () { exit(0); }
int MAIN__ () { exit(0); }
EOF
if AC_TRY_EVAL(ac_compile); then
  ${AR} ${ARFLAGS} libconftest.a conftest.${ac_objext} 1>&AS_MESSAGE_LOG_FD
  if test -n "${RANLIB}"; then
    ${RANLIB} libconftest.a 1>&AS_MESSAGE_LOG_FD
  fi
fi
AC_LANG_POP(C)
## </FIXME>
AC_CHECK_LIB(f2c, f_open, 
             [flibs=-lf2c],
             [flibs=],
             [-L. -lconftest ${LIBM}])
rm -f libconftest*
if test -z "${flibs}"; then
  AC_CHECK_LIB(F77, d_sin, [flibs=-lF77], [flibs=], [${LIBM}])
  if test -n "${flibs}"; then
    AC_CHECK_LIB(I77, f_rew, [flibs="${flibs} -lI77"], [flibs=], [-lF77])
  fi
fi
r_cv_f2c_flibs="${flibs}"])
FLIBS="${r_cv_f2c_flibs}"
if test -z "${FLIBS}"; then
  warn_f2c_flibs="I found f2c but not libf2c, or libF77 and libI77"
  AC_MSG_WARN([${warn_f2c_flibs}])
else
  FLIBS="${FLIBS} ${LIBM}"
fi
])# R_PROG_F2C_FLIBS

## R_PROG_F2C_MAKEFRAG
## -------------------
## Generate a Make fragment with suffix rules for Fortran 77 source
## files when using f2c, the Fortran-to-C converter.
## Used for both building R (Makeconf) and add-ons (etc/Makeconf).
AC_DEFUN([R_PROG_F2C_MAKEFRAG],
[AC_REQUIRE([R_PROG_CC_C_O_LO])
f77_rules_frag=Makefrag.f77
cat << \EOF > ${f77_rules_frag}
.f.o:
	$(F2C) $(F2CFLAGS) < $< > $[*].c
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -c $[*].c -o $[@]
	@rm -f $[*].c
.f.lo:
	$(F2C) $(F2CFLAGS) < $< > $[*].c
EOF
if test "${r_cv_prog_cc_c_o_lo}" = yes; then
  cat << \EOF >> ${f77_rules_frag}
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS_LO) -c $[*].c -o $[@]
EOF
else
  cat << \EOF >> ${f77_rules_frag}
	@test -d .libs || mkdir .libs
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS_LO) -c $[*].c -o .libs/$[*].o
	mv .libs/$[*].o $[*].lo
EOF
fi
cat << \EOF >> ${f77_rules_frag}
	@rm -f $[*].c
EOF
AC_SUBST_FILE(f77_rules_frag)
])# R_PROG_F2C_MAKEFRAG

### * Library functions

## R_FUNC___SETFPUCW
## -----------------
AC_DEFUN([R_FUNC___SETFPUCW],
[AC_CHECK_FUNC(__setfpucw, 
[AC_CACHE_CHECK([whether __setfpucw is needed],
	        [r_cv_func___setfpucw_needed],
AC_TRY_RUN(
[int main () {
#include <fpu_control.h>
#if defined(_FPU_DEFAULT) && defined(_FPU_IEEE)
  exit(_FPU_DEFAULT != _FPU_IEEE);
#endif
  exit(0);
}],
	   [r_cv_func___setfpucw_needed=no],
	   [r_cv_func___setfpucw_needed=yes],
	   [r_cv_func___setfpucw_needed=no]))
if test "x${r_cv_func___setfpucw_needed}" = xyes; then
  AC_DEFINE(NEED___SETFPUCW, 1,
	    [Define if your system needs __setfpucw() to control
             FPU rounding. 
             This was used to control floating point precision,
             rounding and floating point exceptions on older Linux
             systems. 
             As of GLIBC 2.1 this function is not used anymore.])
fi])
])# R_FUNC___SETFPUCW

## R_FUNC_CALLOC
## -------------
AC_DEFUN([R_FUNC_CALLOC],
[AC_CACHE_CHECK([for working calloc], [r_cv_func_calloc_works],
[AC_TRY_RUN(
[#include <stdlib.h>
int main () {
  int *p = calloc(0, sizeof(int));
  exit(p == 0);
}],
	    [r_cv_func_calloc_works=yes],
	    [r_cv_func_calloc_works=no],
	    [r_cv_func_calloc_works=no])])
if test "x${r_cv_func_calloc_works}" = xyes; then
  AC_DEFINE(HAVE_WORKING_CALLOC, 1,
            [Define if calloc(0) returns a null pointer.])
fi
])# R_FUNC_CALLOC

## R_FUNC_FINITE
## -------------
AC_DEFUN([R_FUNC_FINITE],
[AC_CACHE_CHECK([for working finite], [r_cv_func_finite_works],
[AC_TRY_RUN(
[#include <math.h>
#include "confdefs.h"
int main () {
#ifdef HAVE_FINITE
  exit(finite(1./0.) | finite(0./0.) | finite(-1./0.));
#else
  exit(1);
#endif
}],
	    [r_cv_func_finite_works=yes],
	    [r_cv_func_finite_works=no],
	    [r_cv_func_finite_works=no])])
if test "x${r_cv_func_finite_works}" = xyes; then
  AC_DEFINE(HAVE_WORKING_FINITE, 1,
            [Define if finite() is correct for -Inf/NaN/Inf.])
fi
])# R_FUNC_FINITE

## R_FUNC_LOG
## ----------
AC_DEFUN([R_FUNC_LOG],
[AC_CACHE_CHECK([for working log], [r_cv_func_log_works],
[AC_TRY_RUN(
[#include <math.h>
#include "confdefs.h"
int main () {
#ifdef HAVE_ISNAN
  exit(!(log(0.) == -1. / 0. && isnan(log(-1.))));
#else
  exit(log(0.) != -1. / 0);
#endif
}],
	    [r_cv_func_log_works=yes],
	    [r_cv_func_log_works=no],
	    [r_cv_func_log_works=no])])
if test "x${r_cv_func_log_works}" = xyes; then
  AC_DEFINE(HAVE_WORKING_LOG, 1,
            [Define if log() is correct for 0/-1.])
fi
])# R_FUNC_LOG

## R_FUNC_STRPTIME
## ---------------
AC_DEFUN([R_FUNC_STRPTIME],
[AC_CACHE_CHECK([for working strptime], [r_cv_func_strptime_works],
[AC_TRY_RUN(
[#include <time.h>
int main () {
#ifdef HAVE_STRPTIME
  struct tm tm;
  char *p;

  p = strptime("1960-01-01", "%Y-%m-%d", &tm);
  exit(p == 0);
#else
  exit(1);
#endif
}],
	    [r_cv_func_strptime_works=yes],
	    [r_cv_func_strptime_works=no],
	    [r_cv_func_strptime_works=no])])
if test "x${r_cv_func_strptime_works}" = xyes; then
  AC_DEFINE(HAVE_WORKING_STRPTIME, 1,
            [Define if strptime() exists and does not fail pre-1970.])
fi
])# R_FUNC_STRPTIME

### * Headers

## R_HEADER_SETJMP
## ---------------
AC_DEFUN([R_HEADER_SETJMP],
[AC_CACHE_CHECK([whether setjmp.h is POSIX.1 compatible], 
                [r_cv_header_setjmp_posix],
[AC_TRY_COMPILE([#include <setjmp.h>],
[sigjmp_buf b;
sigsetjmp(b, 0);
siglongjmp(b, 1);],
[r_cv_header_setjmp_posix=yes],
[r_cv_header_setjmp_posix=no])])
if test "${r_cv_header_setjmp_posix}" = yes; then
  AC_DEFINE(HAVE_POSIX_SETJMP, 1,
            [Define if you have POSIX.1 compatible sigsetjmp/siglongjmp.])
fi
])# R_HEADER_SETJMP

## R_HEADER_GLIBC2
## ---------------
AC_DEFUN([R_HEADER_GLIBC2],
[AC_CACHE_CHECK([for GNU C library with version >= 2],
                [r_cv_header_glibc2],
[AC_EGREP_CPP([yes],
[#include <stdio.h>
#if defined __GLIBC__ && __GLIBC__ >= 2
  yes
#endif
],
              [r_cv_header_glibc2=yes],
              [r_cv_header_glibc2=no],
              [r_cv_header_glibc2=no])
])
if test "${r_cv_header_glibc2}" = yes; then
  AC_DEFINE(HAVE_GLIBC2, 1,
            [Define if you have the GNU C library version >= 2.
             This is needed to fix a problem with getting the prototype
             of strptime().])
fi
])# R_HEADER_GLIBC2

## R_TYPE_SOCKLEN
## --------------
AC_DEFUN([R_TYPE_SOCKLEN],
[AC_MSG_CHECKING([for type of socket length])
AC_CACHE_VAL([r_cv_type_socklen],
[for t in socklen_t size_t int; do
  AC_TRY_COMPILE(
[#include <stddef.h>
#include <sys/types.h>
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef Win32
#include <winsock.h>
#endif
], 
                 [(void)getsockopt (1, 1, 1, NULL, (${t} *)NULL)],
                 [r_cv_type_socklen=${t}; break],
                 [r_cv_type_socklen=])
done])
if test "x${r_cv_type_socklen}" = x; then
  warn_type_socklen="could not determine type of socket length"
  AC_MSG_WARN([${warn_type_socklen}])
else
  AC_MSG_RESULT([${r_cv_type_socklen} *])
fi
AC_DEFINE_UNQUOTED(SOCKLEN_T, ${r_cv_type_socklen},
                   [Type for socket lengths: socklen_t, sock_t, int?])
])# R_TYPE_SOCKLEN

### * System services

## R_GNOME
## -------
AC_DEFUN([R_GNOME], 
[if test ${want_gnome} = yes; then
  GNOME_INIT_HOOK([], [cont])
  if test "${GNOMEUI_LIBS}"; then
    AM_PATH_LIBGLADE([use_gnome="yes"
                      GNOME_IF_FILES="gnome-interface.glade"],
                     [warn_libglade_version="GNOME support requires libglade version >= 0.3"
                      AC_MSG_WARN([${warn_libglade_version}])],
                     [gnome])
  fi
fi
if test "${use_gnome}" != yes; then
  use_gnome="no"
  GNOME_IF_FILES=
else
  AC_DEFINE(HAVE_GNOME, 1,
            [Define if you have the GNOME headers and libraries,
             and want the GNOME GUI to be built.])
fi
AC_SUBST(HAVE_GNOME)
AC_SUBST(GNOME_IF_FILES)
])# R_GNOME

AC_DEFUN([R_AQUA],
[use_aqua=no
if test "${want_aqua}" = yes; then
  case "${host_os}" in
    darwin*)
      use_aqua=yes
      ;;
  esac
fi
if test "${use_aqua}" = yes; then
  AC_DEFINE(HAVE_AQUA, 1,
            [Define if you have the Aqua headers and libraries,
             and want the Aqua GUI to be built.])
fi
])# R_AQUA

## R_IEEE_754
## ----------
AC_DEFUN([R_IEEE_754],
[AC_CHECK_FUNCS([finite isnan])
AC_CHECK_DECLS([isfinite, isnan], , , [#include <math.h>])
AC_CACHE_CHECK([whether you have IEEE 754 floating-point arithmetic],
               [r_cv_ieee_754],
[if (test "${ac_cv_func_finite}" = yes \
      || test "${ac_cv_have_decl_isfinite}" = yes) \
    && (test "${ac_cv_func_isnan}" = yes \
      || test "${ac_cv_have_decl_isnan}" = yes); then
  r_cv_ieee_754=yes
else
  r_cv_ieee_754=no
fi])
if test "${r_cv_ieee_754}" = yes; then
  AC_DEFINE(IEEE_754, 1,
            [Define if you have IEEE 754 floating point arithmetic.])
fi
])# R_IEEE_754

## R_BSD_NETWORKING
## ----------------
AC_DEFUN([R_BSD_NETWORKING],
[AC_CACHE_CHECK([for BSD networking],
                [r_cv_bsd_networking],
[if test "${ac_cv_header_netdb_h}" = yes \
     && test "${ac_cv_header_netinet_in_h}" = yes \
     && test "${ac_cv_header_sys_socket_h}" = yes \
     && test "${ac_cv_search_connect}" != no \
     && test "${ac_cv_search_gethostbyname}" !=  no; then
  r_cv_bsd_networking=yes
else
  r_cv_bsd_networking=no
fi])
if test "${r_cv_bsd_networking}" = yes; then
  AC_DEFINE(HAVE_BSD_NETWORKING, 1,
            [Define if you have BSD networking headers and libraries.])
  AC_DEFINE(HAVE_SOCKETS, 1,
            [Define if you have support for sockets.])
  AC_DEFINE(HAVE_INTERNET, 1,
            [Define if you have support for ftp/http access.])
  AC_DEFINE(SUPPORT_LIBXML, 1,
            [Define if you provide support for the libxml ftp/http
	     functions.])
fi
])# R_BSD_NETWORKING

## R_BITMAPS
## ---------
## Here we only need any old -lz, and don't need zlib.h.
## However, we do need recent enough libpng and jpeg, and so check both
## the header versions and for key routines in the library.
## The png code will do a run-time check of the consistency of libpng
## versions.
AC_DEFUN([R_BITMAPS],
[BITMAP_LIBS=
_R_HEADER_JPEGLIB
have_jpeg=${r_cv_header_jpeglib_h}
if test "${have_jpeg}" = yes; then
  AC_CHECK_LIB(jpeg, jpeg_destroy_compress, 
               [have_jpeg=yes],
               [have_jpeg=no],
               [${LIBS}])
fi
if test "${have_jpeg}" = yes; then
  BITMAP_LIBS="-ljpeg"
  AC_DEFINE(HAVE_JPEG, 1,
            [Define if you have the JPEG headers and libraries.])
fi
AC_CHECK_LIB(z, main, [have_png=yes], [have_png=no])
if test "${have_png}" = yes; then
  _R_HEADER_PNG
  have_png=${r_cv_header_png_h}
fi
if test "${have_png}" = yes; then
  AC_CHECK_LIB(png, png_create_write_struct,
               [have_png=yes],
               [have_png=no],
               [-lz ${LIBS}])
fi
if test "${have_png}" = yes; then
  BITMAP_LIBS="${BITMAP_LIBS} -lpng -lz"
  AC_DEFINE(HAVE_PNG, 1,
            [Define if you have the PNG headers and libraries.])
fi
AC_SUBST(BITMAP_LIBS)
])# R_BITMAPS

## _R_HEADER_JPEGLIB
## -----------------
## Set shell variable r_cv_header_jpeglib_h to 'yes' if a recent enough
## jpeglib.h is found, and to 'no' otherwise.
AC_DEFUN([_R_HEADER_JPEGLIB],
[AC_CACHE_CHECK([if jpeglib version >= 6b],
                [r_cv_header_jpeglib_h],
AC_EGREP_CPP([yes],
[#include <jpeglib.h>
#if (JPEG_LIB_VERSION >= 62)
  yes
#endif
],
             [r_cv_header_jpeglib_h=yes],
             [r_cv_header_jpeglib_h=no]))
])# _R_HEADER_JPEGLIB

## _R_HEADER_PNG
## -------------
## Set shell variable r_cv_header_png_h to 'yes' if a recent enough
## 'png.h' is found, and to 'no' otherwise.
AC_DEFUN([_R_HEADER_PNG],
[AC_CACHE_CHECK([if libpng version >= 1.0.5], 
                [r_cv_header_png_h],
AC_EGREP_CPP([yes],
[#include <png.h>
#if (PNG_LIBPNG_VER >= 10005)
  yes
#endif
],
             [r_cv_header_png_h=yes],
             [r_cv_header_png_h=no]))
])# _R_HEADER_PNG

## _R_PATH_TCL_CONFIG
## ------------------
## Try finding tclConfig.sh in common library directories and their
## tcl$x.$y subdirectories.  Set shell variable r_cv_path_TCL_CONFIG
## to the entire path of the script if found, and leave it empty
## otherwise.
AC_DEFUN([_R_PATH_TCL_CONFIG],
[AC_MSG_CHECKING([for tclConfig.sh in library (sub)directories])
AC_CACHE_VAL([r_cv_path_TCL_CONFIG],
[for ldir in /opt/lib /sw/lib /usr/local/lib /usr/lib /lib; do
  for dir in \
      ${ldir} \
      `ls -d ${ldir}/tcl[[8-9]].[[0-9]]* 2>/dev/null | sort -r`; do
    if test -f ${dir}/tclConfig.sh; then
      r_cv_path_TCL_CONFIG="${dir}/tclConfig.sh"
      break 2
    fi
  done
done])
if test -n "${r_cv_path_TCL_CONFIG}"; then
  AC_MSG_RESULT([${r_cv_path_TCL_CONFIG}])
else
  AC_MSG_RESULT([no])
fi
])# _R_PATH_TCL_CONFIG

## _R_PATH_TK_CONFIG
## ------------------
## Try finding tkConfig.sh in common library directories and their
## tk$x.$y subdirectories.  Set shell variable r_cv_path_TK_CONFIG
## to the entire path of the script if found, and leave it empty
## otherwise.
AC_DEFUN([_R_PATH_TK_CONFIG],
[AC_MSG_CHECKING([for tkConfig.sh in library (sub)directories])
AC_CACHE_VAL([r_cv_path_TK_CONFIG],
[for ldir in /opt/lib /sw/lib /usr/local/lib /usr/lib /lib; do
  for dir in \
      ${ldir} \
      `ls -d ${ldir}/tk[[8-9]].[[0-9]]* 2>/dev/null | sort -r`; do
    if test -f ${dir}/tkConfig.sh; then
      r_cv_path_TK_CONFIG="${dir}/tkConfig.sh"
      break 2
    fi
  done
done])
if test -n "${r_cv_path_TK_CONFIG}"; then
  AC_MSG_RESULT([${r_cv_path_TK_CONFIG}])
else
  AC_MSG_RESULT([no])
fi
])# _R_PATH_TK_CONFIG

## _R_TCLTK_CONFIG
## ---------------
## Try finding the tclConfig.sh and tkConfig.sh scripts in PATH as well
## as in common library directories and their tcl/tk subdirectories.
## Set shell variables TCL_CONFIG and TK_CONFIG to the entire paths to
## the scripts if found and check that the corresponding Tcl/Tk versions
## are at least 8; if not, set shell variable have_tcltk to 'no'.
AC_DEFUN([_R_TCLTK_CONFIG],
[AC_PATH_PROGS(TCL_CONFIG, [${TCL_CONFIG} tclConfig.sh])
if test -z "${TCL_CONFIG}"; then
  _R_PATH_TCL_CONFIG
  if test -n "${r_cv_path_TCL_CONFIG}"; then
    TCL_CONFIG="${r_cv_path_TCL_CONFIG}"
  fi
fi
AC_PATH_PROGS(TK_CONFIG, [${TK_CONFIG} tkConfig.sh])
if test -z "${TK_CONFIG}"; then
  _R_PATH_TK_CONFIG
  if test -n "${r_cv_path_TK_CONFIG}"; then
    TK_CONFIG="${r_cv_path_TK_CONFIG}"
  fi
fi
if test -z "${TCLTK_CPPFLAGS}" \
    || test -z "${TCLTK_LIBS}"; then
  ## Check whether the versions found via the *Config.sh files are at
  ## least 8; otherwise, issue a warning and turn off Tcl/Tk support.
  ## Note that in theory a system could have outdated versions of the
  ## *Config.sh scripts and yet up-to-date installations of Tcl/Tk in
  ## standard places ...
  if test -n "${TCL_CONFIG}"; then
    . ${TCL_CONFIG}
    if test ${TCL_MAJOR_VERSION} -lt 8; then
      warn_tcltk_version="Tcl/Tk support requires Tcl version >= 8"
      AC_MSG_WARN([${warn_tcltk_version}])
      have_tcltk=no
    fi
  fi
  if test -n "${TK_CONFIG}" \
      && test -z "${warn_tcltk_version}"; then
    . ${TK_CONFIG}
    if test ${TK_MAJOR_VERSION} -lt 8; then
      warn_tcltk_version="Tcl/Tk support requires Tk version >= 8"
      AC_MSG_WARN([${warn_tcltk_version}])
      have_tcltk=no
    fi
  fi
  if test -n "${TCL_CONFIG}" \
      && test -n "${TK_CONFIG}" \
      && test -z "${warn_tcltk_version}"; then
    if test ${TCL_MAJOR_VERSION} -ne ${TK_MAJOR_VERSION} \
      || test ${TCL_MINOR_VERSION} -ne ${TK_MINOR_VERSION}; then
     warn_tcltk_version="Tcl and Tk major or minor versions disagree"
      AC_MSG_WARN([${warn_tcltk_version}])
      have_tcltk=no
    fi
  fi
fi
])# _R_TCLTK_CONFIG

## _R_HEADER_TCL
## -------------
## Set shell variable 'r_cv_header_tcl_h' to 'yes' if a recent enough
## 'tcl.h' is found, and to 'no' otherwise.
AC_DEFUN([_R_HEADER_TCL],
[AC_CACHE_CHECK([for tcl.h], [r_cv_header_tcl_h],
[AC_EGREP_CPP([yes],
[#include <tcl.h>
#if (TCL_MAJOR_VERSION >= 8)
  yes
#endif
],
             [r_cv_header_tcl_h=yes],
             [r_cv_header_tcl_h=no])])
])# _R_HEADER_TCL

## _R_HEADER_TK
## -------------
## Set shell variable 'r_cv_header_tk_h' to 'yes' if a recent enough
## 'tk.h' is found, and to 'no' otherwise.
AC_DEFUN([_R_HEADER_TK],
[AC_CACHE_CHECK([for tk.h], [r_cv_header_tk_h],
[AC_EGREP_CPP([yes],
[#include <tk.h>
#if (TK_MAJOR_VERSION >= 8)
  yes
#endif
],
             [r_cv_header_tk_h=yes],
             [r_cv_header_tk_h=no])])
])# _R_HEADER_TK

## _R_TCLTK_CPPFLAGS
## -----------------
## Need to ensure that we can find the tcl.h and tk.h headers, which
## may be in non-standard and/or version-dependent directories, such as
## on FreeBSD systems.
##
## The logic is as follows.  If TCLTK_CPPFLAGS was specified, then we
## do not investigate any further.  Otherwise, if we still think we
## have Tcl/Tk, then first try via the corresponding *Config.sh file,
## or else try the obvious.
AC_DEFUN([_R_TCLTK_CPPFLAGS],
[AC_REQUIRE([_R_TCLTK_CONFIG])
if test -z "${TCLTK_CPPFLAGS}"; then
  ## We have to do the work.
  if test "${have_tcltk}" = yes; then
    ## Part 1.  Check for tcl.h.
    found_tcl_h=no
    if test -n "${TCL_CONFIG}"; then
      . ${TCL_CONFIG}
      ## Look for tcl.h in
      ##   ${TCL_PREFIX}/include/tcl${TCL_VERSION}
      ##   ${TCL_PREFIX}/include
      ## Also look in
      ##   ${TCL_PREFIX}/include/tcl${TCL_VERSION}/generic
      ## to deal with current FreeBSD layouts.  These also link the real
      ## thing to the version subdir, but the link cannot be used as it
      ## fails to include 'tclDecls.h' which is not linked.  Hence we
      ## must look for the real thing first.  Argh ...
      for dir in \
          ${TCL_PREFIX}/include/tcl${TCL_VERSION}/generic \
          ${TCL_PREFIX}/include/tcl${TCL_VERSION} \
          ${TCL_PREFIX}/include; do 
        AC_CHECK_HEADER([${dir}/tcl.h],
                        [TCLTK_CPPFLAGS="-I${dir}"
                         found_tcl_h=yes
                         break])
      done
    fi
    if test "${found_tcl_h}" = no; then
      _R_HEADER_TCL
      if test "${r_cv_header_tcl_h}" = yes; then
        found_tcl_h=yes
      else
        have_tcltk=no
      fi
    fi
  fi
  if test "${have_tcltk}" = yes; then
    ## Part 2.  Check for tk.h.
    found_tk_h=no
    if test -n "${TK_CONFIG}"; then
      . ${TK_CONFIG}
      ## Look for tk.h in
      ##   ${TK_PREFIX}/include/tk${TK_VERSION}
      ##   ${TK_PREFIX}/include
      ## Also look in
      ##   ${TK_PREFIX}/include/tcl${TK_VERSION}
      ## to compensate for Debian madness ...
      ## Also look in
      ##   ${TK_PREFIX}/include/tk${TK_VERSION}/generic
      ## to deal with current FreeBSD layouts.  See above for details.
      ##
      ## As the AC_CHECK_HEADER test tries including the header file and
      ## tk.h includes tcl.h and X11/Xlib.h, we need to change CPPFLAGS
      ## for the check.
      r_save_CPPFLAGS="${CPPFLAGS}"
      CPPFLAGS="${CPPFLAGS} ${TK_XINCLUDES} ${TCLTK_CPPFLAGS}"
      for dir in \
          ${TK_PREFIX}/include/tk${TK_VERSION}/generic \
          ${TK_PREFIX}/include/tk${TK_VERSION} \
          ${TK_PREFIX}/include/tcl${TK_VERSION} \
          ${TK_PREFIX}/include; do 
        AC_CHECK_HEADER([${dir}/tk.h],
                        [TCLTK_CPPFLAGS="${TCLTK_CPPFLAGS} -I${dir}"
                         found_tk_h=yes
                         break])
      done
      CPPFLAGS="${r_save_CPPFLAGS}"
    fi
    if test "${found_tk_h}" = no; then
      _R_HEADER_TK
      if test "{r_cv_header_tk_h}" = yes; then
        found_tk_h=yes
      else
        have_tcltk=no
      fi
    fi
  fi
fi
])# _R_TCLTK_CPPFLAGS

## _R_TCLTK_LIBS
## -------------
## Find the tcl and tk libraries.
AC_DEFUN([_R_TCLTK_LIBS],
[AC_REQUIRE([AC_PATH_XTRA])
AC_REQUIRE([_R_TCLTK_CONFIG])
if test -z "${TCLTK_LIBS}"; then
  ## We have to do the work.
  if test "${have_tcltk}" = yes; then
    ## Part 1.  Try finding the tcl library.
    if test -n "${TCL_CONFIG}"; then
      . ${TCL_CONFIG}
      TCLTK_LIBS="${TCL_LIB_SPEC}"
    else
      AC_CHECK_LIB(tcl, Tcl_CreateInterp,
                   [TCLTK_LIBS=-ltcl],
                   [have_tcltk=no])
    fi
  fi
  if test "${have_tcltk}" = yes; then
    ## Part 2.  Try finding the tk library.
    if test -n "${TK_CONFIG}"; then
      . ${TK_CONFIG}
      TCLTK_LIBS="${TCLTK_LIBS} ${TK_LIB_SPEC} ${TK_LIBS}"
    else
      AC_CHECK_LIB(tk, Tk_Init, , , [${TCLTK_LIBS}])
      if test "${ac_cv_lib_tk_Tk_Init}" = no; then
	## Grr, simple -ltk does not work.
	## But maybe we simply need to add X11 libs.
        ## Note that we cannot simply repeat the above test with extra
        ## libs, because AC_CHECK_LIB uses the corresponding cache var
        ## (ac_cv_lib_tk_Tk_Init in our case) if set.  As using unset
        ## is not portable shell programming according to the Autoconf
        ## docs, we use Tk_SafeInit in the test with X11 libs added.
	AC_CHECK_LIB(tk, Tk_SafeInit,
                     [TCLTK_LIBS="${TCLTK_LIBS} -ltk ${X_LIBS}"],
	             [have_tcltk=no],
                     [${TCLTK_LIBS} ${X_LIBS}])
      fi
    fi
  fi
  ## Postprocessing for AIX.
  ## On AIX, the *_LIB_SPEC variables need to contain '-bI:' flags for
  ## the Tcl export file.  These are really flags for ld rather than the
  ## C/C++ compilers, and hence may need protection via '-Wl,'.
  ## We have two ways of doing that:
  ## * Recording whether '-Wl,' is needed for the C or C++ compilers,
  ##   and getting this info into the TCLTK_LIBS make variable ... mess!
  ## * Protecting all entries in TCLTK_LIBS that do not start with '-l'
  ##   or '-L' with '-Wl,' (hoping that all compilers understand this).
  ##   Easy, hence ...
  case "${host_os}" in
    aix*)
      orig_TCLTK_LIBS="${TCLTK_LIBS}"
      TCLTK_LIBS=
      for flag in ${orig_TCLTK_LIBS}; do
        case "${flag}" in
	  -l*|-L*|-Wl,*) ;;
	  *) flag="-Wl,${flag}" ;;
	esac
	TCLTK_LIBS="${TCLTK_LIBS} ${flag}"
      done
      ;;
  esac
fi
])# _R_TCLTK_LIBS

## R_TCLTK
## -------
AC_DEFUN([R_TCLTK],
[if test "${want_tcltk}" = yes; then
  have_tcltk=yes
  _R_TCLTK_CONFIG
  _R_TCLTK_CPPFLAGS  
  _R_TCLTK_LIBS
else
  have_tcltk=no
  ## Just making sure.
  TCLTK_CPPFLAGS=
  TCLTK_LIBS=
fi
if test "${have_tcltk}" = yes; then
  AC_DEFINE(HAVE_TCLTK, 1,
            [Define if you have the Tcl/Tk headers and libraries and
	     want Tcl/Tk support to be built.])
  use_tcltk=yes
  if test -n "${TK_XINCLUDES}"; then
    TCLTK_CPPFLAGS="${TCLTK_CPPFLAGS} ${TK_XINCLUDES}"
  else
    TCLTK_CPPFLAGS="${TCLTK_CPPFLAGS} ${X_CFLAGS}"
  fi
else
  use_tcltk=no
fi
AC_SUBST(TCLTK_CPPFLAGS)
AC_SUBST(TCLTK_LIBS)
AC_SUBST(use_tcltk)
])# R_TCLTK

## R_BLAS_LIBS
## -----------
## Look for a library that implements the BLAS linear-algebra interface
## (see http://www.netlib.org/blas/).  On success, sets BLAS_LIBS to the
## requisite library linkages.
##
## This is based on ACX_BLAS by Steven G. Johnson <stevenj@alum.mit.edu>
## from the Official Autoconf Macro Archive
## (http://www.gnu.org/software/ac-archive/Installed_Packages/acx_blas.m4),
## with the following changes:
## * We also handle HPUX .sl command line specifications.
## * We explictly deal with the case of f2c.  Most likely pointless.
## * We only care about the Fortran 77 interface to Atlas, hence do not
##   test for -lcblas.
## * We do not use BLAS libs that caused problems in the past: Alpha
##   CXML and DXML, and SGI SCSL and SGIMATH (marked with COMMENT tags).
## * We do not use ACTION-IF-FOUND and ACTION-IF-NOT-FOUND.
## Based on acx_blas.m4 version 1.2 (2001-12-13)
AC_DEFUN([R_BLAS_LIBS],
[AC_REQUIRE([R_PROG_F77_FLIBS])
AC_REQUIRE([R_PROG_F77_APPEND_UNDERSCORE])
AC_REQUIRE([R_PROG_F2C_FLIBS])

acx_blas_ok=no
case "${with_blas}" in
  yes | "") ;;
  no) acx_blas_ok=disable ;;
  -* | */* | *.a | *.so | *.so.* | *.sl | *.sl.* | *.o)
    BLAS_LIBS="${with_blas}" 
    ;;
  *) BLAS_LIBS="-l${with_blas}" ;;
esac

if test "${r_cv_prog_f77_append_underscore}" = yes \
  || test -n "${F2C}"; then
  dgemm=dgemm_
  sgemm=sgemm_
else
  dgemm=dgemm
  sgemm=sgemm
fi

acx_blas_save_LIBS="${LIBS}"
LIBS="${LIBS} ${FLIBS}"

## First, check BLAS_LIBS environment variable
if test "${acx_blas_ok}" = no; then
  if test "x${BLAS_LIBS}" != x; then
    save_LIBS="${LIBS}"; LIBS="${BLAS_LIBS} ${LIBS}"
    AC_MSG_CHECKING([for ${sgemm} in ${BLAS_LIBS}])
    AC_TRY_LINK_FUNC(${sgemm}, [acx_blas_ok=yes], [BLAS_LIBS=""])
    AC_MSG_RESULT(${acx_blas_ok})
    LIBS="$save_LIBS"
  fi
fi

## BLAS linked to by default?  (happens on some supercomputers)
if test "${acx_blas_ok}" = no; then
  AC_CHECK_FUNC(${sgemm}, [acx_blas_ok=yes])
fi

## BLAS in ATLAS library? (http://math-atlas.sourceforge.net/)
if test "${acx_blas_ok}" = no; then
  AC_CHECK_LIB(atlas, ATL_xerbla,
               [AC_CHECK_LIB(f77blas, ${sgemm},
                             [acx_blas_ok=yes
                              BLAS_LIBS="-lf77blas -latlas"],
			     [], [-latlas])])
fi

## BLAS in PhiPACK libraries? (requires generic BLAS lib, too)
if test "${acx_blas_ok}" = no; then
  AC_CHECK_LIB(blas, ${sgemm},
	       [AC_CHECK_LIB(dgemm, $dgemm,
		             [AC_CHECK_LIB(sgemm, ${sgemm},
			                   [acx_blas_ok=yes
                                            BLAS_LIBS="-lsgemm -ldgemm -lblas"],
			                   [], [-lblas])],
			     [], [-lblas])])
fi

## <COMMENT>
## ## BLAS in Alpha CXML library?
## if test "${acx_blas_ok}" = no; then
##   AC_CHECK_LIB(cxml, ${sgemm},
##                [acx_blas_ok=yes; BLAS_LIBS="-lcxml"])
## fi
## </COMMENT>
  
## <COMMENT>
## # BLAS in Alpha DXML library? (now called CXML, see above)
## if test "${acx_blas_ok}" = no; then
##   AC_CHECK_LIB(dxml, ${sgemm},
##                [acx_blas_ok=yes; BLAS_LIBS="-ldxml"])
## fi
## </COMMENT>

## BLAS in Sun Performance library?
if test "${acx_blas_ok}" = no; then
  if test "x$GCC" != xyes; then # only works with Sun CC
    AC_CHECK_LIB(sunmath, acosp,
                 [AC_CHECK_LIB(sunperf, ${sgemm},
                               [BLAS_LIBS="-xlic_lib=sunperf -lsunmath"
                                acx_blas_ok=yes],
                               [], [-lsunmath])])
  fi
fi

## <COMMENT>
## ## BLAS in SCSL library?  (SGI/Cray Scientific Library)
## if test "${acx_blas_ok}" = no; then
##   AC_CHECK_LIB(scs, ${sgemm},
##                [acx_blas_ok=yes; BLAS_LIBS="-lscs"])
## fi
## </COMMENT>
 
## <COMMENT>
## ## BLAS in SGIMATH library?
## if test "${acx_blas_ok}" = no; then
##   AC_CHECK_LIB(complib.sgimath, ${sgemm},
##                [acx_blas_ok=yes; BLAS_LIBS="-lcomplib.sgimath"])
## fi
## </COMMENT>

## BLAS in IBM ESSL library? (requires generic BLAS lib, too)
if test "${acx_blas_ok}" = no; then
  AC_CHECK_LIB(blas, ${sgemm},
	       [AC_CHECK_LIB(essl, ${sgemm},
			     [acx_blas_ok=yes
                              BLAS_LIBS="-lessl -lblas"],
			     [], [-lblas ${FLIBS}])])
fi

## Generic BLAS library?
if test "${acx_blas_ok}" = no; then
  AC_CHECK_LIB(blas, ${sgemm},
               [acx_blas_ok=yes; BLAS_LIBS="-lblas"])
fi

LIBS="${acx_blas_save_LIBS}"

AC_SUBST(BLAS_LIBS)
])# R_BLAS_LIBS

## R_XDR
## -----
## Try finding XDR library functions and headers.
## FreeBSD in particular needs rpc/types.h before rpc/xdr.h.
AC_DEFUN([R_XDR],
[AC_CHECK_HEADER(rpc/types.h)
if test "${ac_cv_header_rpc_types_h}" = yes ; then
  AC_CHECK_HEADER(rpc/xdr.h, , , [#include <rpc/types.h>])
fi
AC_CACHE_CHECK([for XDR support],
                [r_cv_xdr],
[if test "${ac_cv_header_rpc_types_h}" = yes \
     && test "${ac_cv_header_rpc_xdr_h}" = yes \
     && test "${ac_cv_search_xdr_string}" != no ; then
  r_cv_xdr=yes
else
  r_cv_xdr=no
fi
])
AM_CONDITIONAL(BUILD_XDR, [test "x${r_cv_xdr}" = xno])
])# R_XDR

## R_ZLIB
## ------
## Try finding zlib library and headers.
## We check that both are installed, and that the header >= 1.1.3
## and that gzeof is in the library (which suggests the library
## is also recent enough).
AC_DEFUN([R_ZLIB],
[AC_CHECK_LIB(z, gzeof, [have_zlib=yes], [have_zlib=no])
if test "${have_zlib}" = yes; then
  AC_CHECK_HEADER(zlib.h, [have_zlib=yes], [have_zlib=no])
fi
if test "${have_zlib}" = yes; then
  _R_HEADER_ZLIB
  have_zlib=${r_cv_header_zlib_h}
fi
AC_MSG_CHECKING([whether zlib support needs to be compiled])
if test "${have_zlib}" = yes; then
  AC_MSG_RESULT([no])
  AC_DEFINE(HAVE_ZLIB, 1,
            [Define if you have the zlib headers and libraries.])
  LIBS="-lz ${LIBS}"
else
  AC_MSG_RESULT([yes])
  _R_ZLIB_MMAP
fi
AM_CONDITIONAL(BUILD_ZLIB, [test "x${have_zlib}" = xno])
AM_CONDITIONAL(USE_MMAP_ZLIB,
[test "x${have_zlib}" = xno && test "x${r_cv_zlib_mmap}" = xyes])
])# R_ZLIB

## _R_HEADER_ZLIB
## --------------
## Set shell variable r_cv_header_zlib_h to 'yes' if a recent enough
## zlib.h is found, and to 'no' otherwise.
AC_DEFUN([_R_HEADER_ZLIB],
[AC_CACHE_CHECK([if zlib version >= 1.1.3],
                [r_cv_header_zlib_h],
AC_TRY_RUN(
[#include <string.h>
#include <zlib.h>
int main() {
#ifdef ZLIB_VERSION
  exit(strcmp(ZLIB_VERSION, "1.1.3") < 0);
#else
  exit(1);
#endif
}],
           [r_cv_header_zlib_h=yes],
           [r_cv_header_zlib_h=no],
           [r_cv_header_zlib_h=no]))
])# _R_HEADER_ZLIB

## _R_ZLIB_MMAP
## ------------
AC_DEFUN([_R_ZLIB_MMAP],
[AC_CACHE_CHECK([mmap support for zlib],
                [r_cv_zlib_mmap],
AC_TRY_RUN(
[#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
caddr_t hello() {
  exit(mmap((caddr_t)0, (off_t)0, PROT_READ, MAP_SHARED, 0, (off_t)0)); 
}],
	       [r_cv_zlib_mmap=no],
	       [r_cv_zlib_mmap=yes],
	       [r_cv_zlib_mmap=yes]))
])# _R_ZLIB_MMAP

## R_PCRE
## ------
## Try finding pcre library and headers.
## RedHat puts the headers in /usr/include/pcre.
AC_DEFUN([R_PCRE],
[AC_CHECK_LIB(pcre, pcre_fullinfo, [have_pcre=yes], [have_pcre=no])
if test "${have_pcre}" = yes; then
  AC_CHECK_HEADERS(pcre.h pcre/pcre.h)
  if test "${ac_cv_header_pcre_h}" = no \
      && test "${ac_cv_header_pcre_pcre_h}" = no; then
    have_pcre=no
  fi
fi
if test "${have_pcre}" = yes; then
  AC_DEFINE(HAVE_PCRE, 1,
            [Define if you have the PCRE headers and libraries.])
  LIBS="-lpcre ${LIBS}"
fi
])# R_PCRE

## R_BZLIB
## -------
## Try finding bzlib library and headers.
## We check that both are installed,
## and that BZ2_bzlibVersion is in the library.
AC_DEFUN([R_BZLIB],
[AC_CHECK_LIB(bz2, BZ2_bzlibVersion, [have_bzlib=yes], [have_bzlib=no])
if test "${have_bzlib}" = yes; then
  AC_CHECK_HEADER(bzlib.h, [have_bzlib=yes], [have_bzlib=no])
fi
if test "${have_bzlib}" = yes; then
  AC_DEFINE(HAVE_BZLIB, 1,
            [Define if you have the bzip2 headers and libraries.])
  LIBS="-lbz2 ${LIBS}"
fi
])# R_BZLIB

## R_SYS_POSIX_LEAPSECONDS
## -----------------------
## See if your system time functions do not count leap seconds, as
## required by POSIX.
AC_DEFUN([R_SYS_POSIX_LEAPSECONDS],
[AC_CACHE_CHECK([whether leap seconds are treated according to POSIX],
                [r_cv_sys_posix_leapseconds],
AC_TRY_RUN([
#include <stdlib.h>
#include <time.h>
#include <stdio.h>
#include "confdefs.h"

int main () {
  struct tm *tm;
  time_t ct;

  ctime(&ct);
  ct = ct - (ct % 60);
  tm = gmtime(&ct);
  if(tm->tm_sec == 0) exit(1); else exit(0);
}],
	   [r_cv_sys_posix_leapseconds=no],
	   [r_cv_sys_posix_leapseconds=yes],
	   [r_cv_sys_posix_leapseconds=yes]))
if test "x${r_cv_sys_posix_leapseconds}" = xyes; then
  AC_DEFINE(HAVE_POSIX_LEAPSECONDS, 1,
            [Define if your system time functions do not count leap
	     seconds, as required by POSIX.])
fi
])# R_SYS_POSIX_LEAPSECONDS

### * Miscellaneous

## R_RECOMMENDED_PACKAGES
## ----------------------
## See if the sources for the recommended packages are available.
AC_DEFUN([R_RECOMMENDED_PACKAGES],
[AC_CACHE_CHECK([for recommended packages],
                [r_cv_misc_recommended_packages],
[r_cv_misc_recommended_packages=yes
recommended_pkgs=`grep '^R_PKGS_RECOMMENDED *=' ${srcdir}/Makeconf.in | \
  sed 's/.*=//'`
for pkg in ${recommended_pkgs}; do
  n_pkg=`ls ${srcdir}/src/library/Recommended/${pkg}_*.tar.gz | wc -l`
  if test ${n_pkg} -ne 1; then
    r_cv_misc_recommended_packages=no
    break
  fi
done])
use_recommended_packages=${r_cv_misc_recommended_packages}
])# R_RECOMMENDED_PACKAGES

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
