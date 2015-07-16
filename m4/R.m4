### R.m4 -- extra macros for configuring R		-*- Autoconf -*-
###
### Copyright (C) 1998-2014 R Core Team
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

## R_ARG_USE_SYSTEM
## ----------------
AC_DEFUN([R_ARG_USE_SYSTEM],
[if test "${withval}" = no; then
  use_system_$1=no
else
  use_system_$1=yes
fi
])# R_ARG_USE_SYSTEM

## R_SH_VAR_ADD(VARIABLE, VALUE, [SEPARATOR = " "])
## ------------------------------------------------
## Set sh variable VARIABLE to VALUE if empty (or undefined), or append
## VALUE to the value of VARIABLE, separated by SEPARATOR.
## Currently, safe only if all arguments are literals.
## Useful mostly when we do not know whether VARIABLE is empty or not.
## Should maybe also have a fourth argument to control whether adding
## happens by appending (default) or prepending ...
AC_DEFUN([R_SH_VAR_ADD],
[separator="$3"
test -z "${separator}" && separator=" "
if test -z "${[$1]}"; then
  $1="$2"
else
  $1="${[$1]}${separator}$2"
fi])# R_SH_VAR_ADD

## R_MISSING_PROG(NAME, PROGRAM)
## -----------------------------
## Simplified variant of AM_MISSING_PROG.
## Set NAME to PROGRAM if this is found and works (in the sense of
## properly implementing --version, or to an appropriate invocation
## if the missing script otherwise.
AC_DEFUN([R_MISSING_PROG],
[AC_MSG_CHECKING([for working $2])
if ($2 --version) < /dev/null > /dev/null 2>&1; then
  $1=$2
  AC_MSG_RESULT([found])
else
  $1="\$(SHELL) \$(top_srcdir)/tools/missing $2"
  AC_MSG_RESULT([missing])
fi
AC_SUBST($1)
])# R_MISSING_PROG


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
case "${host_os}" in
  hpux*)
    ## On some versions of HP-UX (seen on both 10.20 and 11.0) we end up
    ## a broken install (seen in /opt/imake/bin) which has the default
    ## permissions wrong (PR#2091).  Let's just always use install-sh on
    ## HP-UX.
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


## R_PROG_TEXMF
## ------------
AC_DEFUN([R_PROG_TEXMF],
[
## PDFTEX PDFLATEX MAKEINDEX TEXI2DVI are used to make manuals
## PDFLATEX and MAKEINDEX in the emulation mode of tools::texi2dvi
## TEXI2DVICMD sets default for R_TEXI2DVICMD, used for options('texi2dvi')
## TEX AND LATEX are no longer used
AC_PATH_PROGS(TEX, [${TEX} tex], )
AC_PATH_PROGS(PDFTEX, [${PDFTEX} pdftex], )
if test -z "${ac_cv_path_PDFTEX}" ; then
  warn_pdf1="you cannot build PDF versions of the R manuals"
  AC_MSG_WARN([${warn_pdf1}])
fi
AC_PATH_PROGS(PDFLATEX, [${PDFLATEX} pdflatex], )
if test -z "${ac_cv_path_PDFLATEX}" ; then
  warn_pdf2="you cannot build PDF versions of vignettes and help pages"
  AC_MSG_WARN([${warn_pdf2}])
fi
AC_PATH_PROGS(MAKEINDEX, [${MAKEINDEX} makeindex], )
R_PROG_MAKEINFO
AC_PATH_PROGS(TEXI2DVI, [${TEXI2DVI} texi2dvi], )
TEXI2DVICMD=${ac_cv_path_TEXI2DVI}
if test -z "${TEXI2DVICMD}"; then
  TEXI2DVICMD=texi2dvi
fi
AC_SUBST(TEXI2DVICMD)
AC_PATH_PROGS(KPSEWHICH, [${KPSEWHICH} kpsewhich], "")
dnl this is deliberately not cached: LaTeX packages change.
AC_MSG_CHECKING([for latex inconsolata package])
r_rd4pdf="times,inconsolata,hyper"
if test -n "${KPSEWHICH}"; then
  ${KPSEWHICH} zi4.sty > /dev/null
  if test $? -eq 0; then
     r_rd4pdf="times,inconsolata,hyper"
     AC_MSG_RESULT([found zi4.sty])
  else
    ${KPSEWHICH} inconsolata.sty > /dev/null
    if test $? -ne 0; then
       r_rd4pdf="times,hyper"
       if test -z "${R_RD4PDF}" ;  then
         AC_MSG_RESULT([missing])
         warn_pdf3="neither inconsolata.sty nor zi4.sty found: PDF vignettes and package manuals will not be rendered optimally"
         AC_MSG_WARN([${warn_pdf3}])
       fi
    else
      AC_MSG_RESULT([found insonsolata.sty])
    fi
  fi
fi
: ${R_RD4PDF=${r_rd4pdf}}
AC_SUBST(R_RD4PDF)
])# R_PROG_TEXMF

## R_PROG_MAKEINFO
## ---------------
AC_DEFUN([R_PROG_MAKEINFO],
[AC_PATH_PROGS(MAKEINFO, [${MAKEINFO} makeinfo])
if test -n "${MAKEINFO}"; then
  _R_PROG_MAKEINFO_VERSION
  AC_PATH_PROGS(INSTALL_INFO,
                [${INSTALL_INFO} ginstall-info install-info],
                false)
  if test "ac_cv_path_INSTALL_INFO" = "false"; then
    if test "${r_cv_prog_perl_v5}" = yes; then
      INSTALL_INFO="perl \$(top_srcdir)/tools/install-info.pl"
    fi
  fi
  AC_SUBST(INSTALL_INFO)
fi
if test "${r_cv_prog_makeinfo_v4}" != yes; then
  warn_info="you cannot build info or HTML versions of the R manuals"
  AC_MSG_WARN([${warn_info}])
  MAKEINFO=""
else
  MAKEINFO="${MAKEINFO}"
fi
])# R_PROG_MAKEINFO

## _R_PROG_MAKEINFO_VERSION
## ------------------------
## Building the R Texinfo manuals requires Makeinfo v4.7 or better.
## Set shell variable r_cv_prog_makeinfo_v4 to 'yes' if a recent
## enough Makeinfo is found, and to 'no' otherwise.
## If you change the minimum version here, also change it in
## doc/manual/Makefile.in and doc/manual/R-admin.texi.
AC_DEFUN([_R_PROG_MAKEINFO_VERSION],
[AC_CACHE_CHECK([whether makeinfo version is at least 4.7],
                [r_cv_prog_makeinfo_v4],
[makeinfo_version=`${MAKEINFO} --version | \
  grep "^makeinfo" | sed 's/[[^)]]*) \(.*\)/\1/'`
makeinfo_version_maj=`echo ${makeinfo_version} | cut -f1 -d.`
makeinfo_version_min=`echo ${makeinfo_version} | \
  cut -f2 -d. | tr -dc '0123456789.' `
if test -z "${makeinfo_version_maj}" \
     || test -z "${makeinfo_version_min}"; then
  r_cv_prog_makeinfo_v4=no
elif test ${makeinfo_version_maj} -ge 5; then
  r_cv_prog_makeinfo_v4=yes
elif test ${makeinfo_version_maj} -lt 4 \
     || test ${makeinfo_version_min} -lt 7; then
  r_cv_prog_makeinfo_v4=no
else
  r_cv_prog_makeinfo_v4=yes
fi])
])# _R_PROG_MAKEINFO_VERSION

## R_PROG_BROWSER
## --------------
## xdg-open is the freedesktop.org interface to kfmclient/gnome-open
AC_DEFUN([R_PROG_BROWSER],
[if test -z "${R_BROWSER}"; then
  AC_PATH_PROGS(R_BROWSER, [firefox mozilla galeon opera xdg-open kfmclient gnome-moz-remote open])
fi
if test -z "${R_BROWSER}"; then
  warn_browser="I could not determine a browser"
  AC_MSG_WARN([${warn_browser}])
else
  AC_MSG_RESULT([using default browser ... ${R_BROWSER}])
fi
AC_SUBST(R_BROWSER)
])# R_BROWSER

## R_PROG_PDFVIEWER
## ----------------
## Try to determine a PDF viewer.
## According to Jeff Gentry <jgentry@jimmy.harvard.edu>, 'acroread4' is
## the FreeBSD acroread port.
AC_DEFUN([R_PROG_PDFVIEWER],
[AC_PATH_PROGS(R_PDFVIEWER,
               [${R_PDFVIEWER} acroread acroread4 xdg-open evince xpdf gv gnome-gv ggv okular kpdf open gpdf kghostview])
if test -z "${R_PDFVIEWER}"; then
  warn_pdfviewer="I could not determine a PDF viewer"
  AC_MSG_WARN([${warn_pdfviewer}])
fi
AC_SUBST(R_PDFVIEWER)
])# R_PDFVIEWER

### * C compiler and its characteristics.

## R_PROG_CPP_CPPFLAGS
## -------------------
## In case of gcc, check whether the C preprocessor complains about
## having '/usr/local/include' in its header search path (no matter how
## it came there).  GCC 3.1 and 3.2 (at least) give warnings about
## 'changing search order for system directory "/usr/local/include" as
## it has already been specified as a non-system directory' which are at
## least quite annoying.
## <NOTE>
## We currently do not distinguish whether '/usr/local/include' was
## added as the R default, or by an explicit CPPFLAGS arg to configure.
## If we wanted to, we should change
##     : ${CPPFLAGS="-I/usr/local/include"}
## in 'configure.ac' by something like
##     : ${CPPFLAGS=${r_default_CPPFLAGS="-I/usr/local/include"}}
## and test whether r_default_CPPFLAGS is non-empty.
## </NOTE>
AC_DEFUN([R_PROG_CPP_CPPFLAGS],
[AC_REQUIRE([AC_PROG_CC])
AC_REQUIRE([AC_PROG_CPP])
if test "${GCC}" = yes; then
  AC_LANG_PUSH(C)
  AC_LANG_CONFTEST([AC_LANG_PROGRAM()])
  if ${CPP} ${CPPFLAGS} conftest.${ac_ext} 2>&1 1>/dev/null | \
      grep 'warning:.*system directory.*/usr/local/include' >/dev/null; then
    CPPFLAGS=`echo ${CPPFLAGS} | \
      sed 's|\(.*\)-I/usr/local/include *\(.*\)|\1\2|'`
  fi
  rm -f conftest.${ac_ext}
  AC_LANG_POP(C)
fi])# R_PROG_CPP_CPPFLAGS

## R_PROG_CC_VERSION
## -----------------
## Determine the version of the C compiler (currently only for gcc).
AC_DEFUN([R_PROG_CC_VERSION],
[AC_REQUIRE([AC_PROG_CC])
CC_VERSION=
if test "${GCC}" = yes; then
  CC_VERSION=`${CC} -v 2>&1 | grep "^.*g.. version" | \
    sed -e 's/^.*g.. version *//'`
fi])# R_PROG_CC_VERSION

## R_PROG_CC_M
## -----------
## Check whether we can figure out C Make dependencies.
AC_DEFUN([R_PROG_CC_M],
[AC_REQUIRE([R_PROG_CC_VERSION])
AC_MSG_CHECKING([whether we can compute C Make dependencies])
AC_CACHE_VAL([r_cv_prog_cc_m],
[echo "#include <math.h>" > conftest.c
## No real point in using AC_LANG_* and ${ac_ext}, as we need to create
## hard-wired suffix rules.
## Another obvious candidate to try is '${MAKEDEPEND-makedepend} -f-'.
## However, this does not work out of the box when srcdir and builddir
## are different, as it creates dependencies of the form
##   ${srcdir}/foo.o: /path/to/bar.h
## Could be made to work, of course ...
## Note also that it does not create a 'conftest.o: conftest.c' line.
## For gcc 3.2 or better, we want to use '-MM' in case this works.
cc_minus_MM=false
if test "${GCC}" = yes; then
  case "${CC_VERSION}" in
    1.*|2.*|3.[[01]]*) ;;
    *) cc_minus_MM="${CC} -MM" ;;
  esac
fi
for prog in "${cc_minus_MM}" "${CC} -M" "${CPP} -M" "cpp -M"; do
  if ${prog} conftest.c 2>/dev/null | \
      grep 'conftest.o: conftest.c' >/dev/null; then
    r_cv_prog_cc_m="${prog}"
    break
  fi
done])
if test "${r_cv_prog_cc_m}" = "${cc_minus_MM}"; then
  r_cv_prog_cc_m="\$(CC) -MM"
fi
if test -z "${r_cv_prog_cc_m}"; then
  AC_MSG_RESULT([no])
else
  AC_MSG_RESULT([yes, using ${r_cv_prog_cc_m}])
fi
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
[r_cc_rules_frag=Makefrag.cc
AC_REQUIRE([R_PROG_CC_M])
cat << \EOF > ${r_cc_rules_frag}
.c.o:
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -c $< -o $[@]
EOF
if test -n "${r_cv_prog_cc_m}"; then
  cat << EOF >> ${r_cc_rules_frag}
.c.d:
	@echo "making \$[@] from \$<"
	@${r_cv_prog_cc_m} \$(ALL_CPPFLAGS) $< > \$[@]
EOF
else
  cat << \EOF >> ${r_cc_rules_frag}
.c.d:
	@echo > $[@]
EOF
fi
AC_SUBST_FILE(r_cc_rules_frag)
])# R_PROG_CC_MAKEFRAG

## R_PROG_CC_LO_MAKEFRAG
## ---------------------
## Generate a Make fragment with suffix rules for the C compiler.
## Used for both building R (Makeconf) and add-ons (etc/Makeconf).
## Need to make .lo files in src/nmath/standalone only
## NB test -d .libs || mkdir .libs can be run more than once
##    and hence race when a parallel make is used
AC_DEFUN([R_PROG_CC_LO_MAKEFRAG],
[r_cc_lo_rules_frag=Makefrag.cc_lo
AC_REQUIRE([R_PROG_CC_C_O_LO])
if test "${r_cv_prog_cc_c_o_lo}" = yes; then
  cat << \EOF > ${r_cc_lo_rules_frag}
.c.lo:
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS_LO) -c $< -o $[@]
EOF
else
  cat << \EOF > ${r_cc_lo_rules_frag}
.c.lo:
	@-test -d .libs || mkdir .libs
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS_LO) -c $< -o .libs/$[*].o
	mv .libs/$[*].o $[*].lo
EOF
fi
AC_SUBST_FILE(r_cc_lo_rules_frag)
])# R_PROG_CC_LO_MAKEFRAG


## R_PROG_CC_FLAG(FLAG, [ACTION-IF-TRUE])
## ---------------------------------------
## Check whether the C compiler handles command line option FLAG,
## and set shell variable r_cv_prog_cc_flag_SFLAG accordingly (where
## SFLAG is a shell-safe transliteration of FLAG).
## In addition, execute ACTION-IF-TRUE in case of success.
AC_DEFUN([R_PROG_CC_FLAG],
[ac_safe=AS_TR_SH($1)
AC_MSG_CHECKING([whether ${CC} accepts $1])
AC_CACHE_VAL([r_cv_prog_cc_flag_${ac_safe}],
[AC_LANG_PUSH(C)
r_save_CFLAGS="${CFLAGS}"
CFLAGS="${CFLAGS} $1"
AC_LINK_IFELSE([AC_LANG_PROGRAM()],
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
## In glibc 2.1, inline version [x86] of exp was broken (exp(-Inf) = NaN).
## We fix this by adding '-D__NO_MATH_INLINES' to R_XTRA_CFLAGS rather
## than AC_DEFINE(__NO_MATH_INLINES) as the former also takes care of
## compiling C code for add-on packages.
AC_DEFUN([R_PROG_CC_FLAG_D__NO_MATH_INLINES],
[AC_CACHE_CHECK([whether C runtime needs -D__NO_MATH_INLINES],
                [r_cv_c_no_math_inlines],
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <math.h>
#if defined(__GLIBC__)
#include <math.h>
int main () {
  double x, y;
  x = -1./0.;
  y = exp(x);
  exit (y != 0.);
}
#else
int main () {
  exit(0);
}
#endif
]])],
              [r_cv_c_no_math_inlines=no],
              [r_cv_c_no_math_inlines=yes],
              [r_cv_c_no_math_inlines=no])])
if test "${r_cv_c_no_math_inlines}" = yes; then
  R_SH_VAR_ADD(R_XTRA_CFLAGS, [-D__NO_MATH_INLINES])
fi
])# R_PROG_CC_FLAG_D__NO_MATH_INLINES

## R_C_OPTIEEE
## -----------
## Check whether the C compiler needs '-OPT:IEEE_NaN_inf=ON' to
## correctly deal with IEEE NaN/Inf.
## This flag is needed for the native SGI C compiler.
## If needed, add the flag to R_XTRA_CFLAGS.
AC_DEFUN([R_C_OPTIEEE],
[AC_CACHE_CHECK([whether C compiler needs -OPT:IEEE_NaN_inf=ON],
                [r_cv_c_optieee],
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <math.h>
#include <stdlib.h>
#include <ieeefp.h>
int main () {
  double x = 0;
  fpsetmask(0); x = x / x; exit (x != x);
}
]])],
              [r_cv_c_optieee=yes],
              [r_cv_c_optieee=no],
              [r_cv_c_optieee=no])])
if test "${r_cv_c_optieee}" = yes; then
  R_SH_VAR_ADD(R_XTRA_CFLAGS, [-OPT:IEEE_NaN_inf=ON])
fi
])# R_C_OPTIEEE

## R_C_INLINE
## ----------
## modified version of AC_C_INLINE to use R_INLINE not inline
AC_DEFUN([R_C_INLINE],
[AC_REQUIRE([AC_PROG_CC_STDC])dnl
AC_CACHE_CHECK([for inline], r_cv_c_inline,
[r_cv_c_inline=""
for ac_kw in inline __inline__ __inline; do
  AC_COMPILE_IFELSE([AC_LANG_SOURCE(
[#ifndef __cplusplus
static $ac_kw int static_foo () {return 0; }
$ac_kw int foo () {return 0; }
#endif
])],
                    [r_cv_c_inline=$ac_kw; break])
done
])
case $r_cv_c_inline in
  no) AC_DEFINE(R_INLINE,,
                [Define as `inline', or `__inline__' or `__inline'
                 if that's what the C compiler calls it,
                 or to nothing if it is not supported.]) ;;
  *)  AC_DEFINE_UNQUOTED(R_INLINE, $r_cv_c_inline) ;;
esac
])# R_C_INLINE

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

## R_PROG_CXX_MAKEFRAG
## -------------------
## Generate a Make fragment with suffix rules for the C++ compiler.
## Used for both building R (Makeconf) and add-ons (etc/Makeconf).
AC_DEFUN([R_PROG_CXX_MAKEFRAG],
[r_cxx_rules_frag=Makefrag.cxx
AC_REQUIRE([R_PROG_CXX_M])
cat << \EOF > ${r_cxx_rules_frag}
.cc.o:
	$(CXX) $(ALL_CPPFLAGS) $(ALL_CXXFLAGS) -c $< -o $[@]
.cpp.o:
	$(CXX) $(ALL_CPPFLAGS) $(ALL_CXXFLAGS) -c $< -o $[@]
EOF
if test "${r_cv_prog_cxx_m}" = yes; then
  cat << \EOF >> ${r_cxx_rules_frag}
.cc.d:
	@echo "making $[@] from $<"
	@$(CXX) -M $(ALL_CPPFLAGS) $< > $[@]
.cpp.d:
	@echo "making $[@] from $<"
	@$(CXX) -M $(ALL_CPPFLAGS) $< > $[@]
EOF
else
  cat << \EOF >> ${r_cxx_rules_frag}
.cc.d:
	@echo > $[@]
.cpp.d:
	@echo > $[@]
EOF
fi
AC_SUBST_FILE(r_cxx_rules_frag)
])# R_PROG_CXX_MAKEFRAG

## R_PROG_CXX_FLAG
## ---------------
## Check whether the C++ compiler handles command line option FLAG,
## and set shell variable r_cv_prog_cc_flag_SFLAG accordingly (where
## SFLAG is a shell-safe transliteration of FLAG).
## In addition, execute ACTION-IF-TRUE in case of success.
AC_DEFUN([R_PROG_CXX_FLAG],
[ac_safe=AS_TR_SH($1)
AC_MSG_CHECKING([whether ${CXX-c++} accepts $1])
AC_CACHE_VAL([r_cv_prog_cxx_flag_${ac_safe}],
[AC_LANG_PUSH(C++)
r_save_CXXFLAGS="${CXXFLAGS}"
CXXFLAGS="${CXXFLAGS} $1"
AC_LINK_IFELSE([AC_LANG_PROGRAM()],
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

## R_PROG_F77
## ----------
## Find a Fortran 77 compiler
##
## If we have not been forced to use a particular Fortran compiler, try
## to find one using one of the several common names.  The list is based
## on what the current autoconf CVS contains (2005-05-21).  This says,
##
## <QUOTE>
## Known compilers:
##  f77/f90/f95: generic compiler names
##  g77: GNU Fortran 77 compiler
##  gfortran: putative GNU Fortran 95+ compiler (in progress)
##  ftn: native Fortran 95 compiler on Cray X1
##  cf77: native F77 compiler under older Crays (prefer over fort77)
##  fort77: native F77 compiler under HP-UX (and some older Crays)
##  frt: Fujitsu F77 compiler
##  pgf77/pgf90/pghpf/pgf95: Portland Group F77/F90/F95 compilers
##  xlf/xlf90/xlf95: IBM (AIX) F77/F90/F95 compilers
##  lf95: Lahey-Fujitsu F95 compiler
##  fl32: Microsoft Fortran 77 "PowerStation" compiler
##  af77: Apogee F77 compiler for Intergraph hardware running CLIX
##  epcf90: "Edinburgh Portable Compiler" F90
##  fort: Compaq (now HP) Fortran 90/95 compiler for Tru64 and Linux/Alpha
##  ifort, previously ifc: Intel Fortran 95 compiler for Linux/x86
##  efc: Intel Fortran 95 compiler for IA64
## </QUOTE>
##
## and uses the following lists:
##   F95: f95 fort xlf95 ifort ifc efc pgf95 lf95 gfortran ftn
##   F90: f90 xlf90 pgf90 pghpf epcf90
##   F77: g77 f77 xlf frt pgf77 cf77 fort77 fl32 af77
##
## We use basically the same, with the following exceptions:
## * On HP-UX fort77 is the POSIX-compatible native compiler and
##   f77 is not: hence we need look for fort77 first!
## <FIXME>
##   Is this still true?
## </FIXME>
## * It seems that g95 has been resurrected, see www.g95.org, hence we
##   add this to the list of F95 compilers.
## * Older versions of the Autoconf code used to have 'fc' as a wrapper
##   around f2c (last in the list).  It no longer has, but we still do,
##   in src/scripts, but it must be specified manually.
## <FIXME>
##   Is this still needed?
## </FIXME>
## * If the C compiler is gcc, we try looking for a matching GCC Fortran
##   compiler (gfortran for 4.x, g77 for 3.x) first.  This should handle
##   problems if GCC 4.x and 3.x suites are installed and, depending on
##   the gcc default, the "wrong" GCC Fortran compiler is picked up (as
##   reported by Bill Northcott <w.northcott@unsw.edu.au> for OSX with
##   4.0 as default and g77 around and the "old" search order F77 F95
##   F90 in use).
AC_DEFUN([R_PROG_F77],
[AC_BEFORE([$0], [AC_PROG_LIBTOOL])
AC_REQUIRE([R_PROG_CC_VERSION])
if test -n "${F77}"; then
  AC_MSG_RESULT([defining F77 to be ${F77}])
else
  F77=
  F95_compilers="f95 fort xlf95 ifort ifc efc pgf95 lf95 gfortran ftn g95"
  F90_compilers="f90 xlf90 pgf90 pghpf epcf90"
  case "${host_os}" in
    hpux*)
      F77_compilers="g77 fort77 f77 xlf frt pgf77 cf77 fl32 af77" ;;
    *)
      F77_compilers="g77 f77 xlf frt pgf77 cf77 fort77 fl32 af77" ;;
  esac
  GCC_Fortran_compiler=
  if test "${GCC}" = yes; then
    case "${CC_VERSION}" in
      3.*) GCC_Fortran_compiler=g77 ;;
      4.*) GCC_Fortran_compiler=gfortran ;;
    esac
  fi
  AC_CHECK_PROGS(F77, [ ${GCC_Fortran_compiler} ${F95_compilers} \
                        ${F90_compilers} ${F77_compilers} fc ])
fi
if test -n "${F77}"; then
  ## If the above 'found' a Fortran 77 compiler, we run AC_PROG_F77 as
  ## this does additional testing (GNU, '-g', ...).
  AC_PROG_F77
else
  AC_MSG_ERROR([No F77 compiler found])
fi
])# R_PROG_F77

## R_PROG_F77_FLIBS
## ----------------
## Run AC_F77_LIBRARY_LDFLAGS, and fix some known problems with FLIBS.
## Only do this if the user has not already set FLIBS.
AC_DEFUN([R_PROG_F77_FLIBS],
[AC_BEFORE([$0], [AC_F77_LIBRARY_LDFLAGS])
if test -z "${FLIBS}"; then
##
## Currently (Autoconf 2.50 or better, it seems) FLIBS also contains all
## elements of LIBS when AC_F77_LIBRARY_LDFLAGS is run.  This is because
## _AC_PROG_F77_V_OUTPUT() uses 'eval $ac_link' for obtaining verbose
## linker output, and AC_LANG(Fortran 77) sets up ac_link to contain
## LIBS.  Most likely a bug, and a nuisance in any case ...
## But we cannot simply eliminate the elements in FLIBS duplicated from
## LIBS (e.g. '-lm' should be preserved).  Hence, we try to call
## AC_F77_LIBRARY_LDFLAGS() with LIBS temporarily set to empty.
r_save_LIBS="${LIBS}"
LIBS=
AC_F77_LIBRARY_LDFLAGS
if test -z "${MAIN_LD}" ; then
  LIBS=
  R_C_LIBRARY_LDFLAGS
else
  CLIBS=
fi
LIBS="${r_save_LIBS}"
## Currently g77 on Darwin links against '-lcrt1.o' (and for GCC 3.1 or
## better also against '-lcrtbegin.o'), which (unlike '-lcrt0.o') are
## not stripped by AC_F77_LIBRARY_LDFLAGS.  This in particular causes
## R_PROG_F77_CC_COMPAT to fail.  Hence, we make sure all -lcrt*.o are
## removed. In Addition, -lmx and -lSystem are implicit and their
## manual inclusion leads to ordering problems (remove when autoconf
## is fixed - supposedly the CVS version is, but 2.6.0 is not).
##
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
##
## Using the Intel Fortran compiler (ifc) one typically gets incorrect
## flags, as the output from _AC_PROG_F77_V_OUTPUT() contains double
## quoted options, e.g. "-mGLOB_options_string=......", see also e.g.
## http://www.octave.org/octave-lists/archive/octave-maintainers.2002/msg00038.html.
## One possible solution is to change AC_F77_LIBRARY_LDFLAGS() to remove
## double quotes for ifc, as it already does for the Cray cft90.  As we
## prefer not to overload Autoconf code, we try to fix things here ...
##
## As of 2.1.0 we try to tidy this up a bit.
## 1) -lfrtbegin and -lgfortranbegin are used by g77/gfortran only for a
## Fortran main program, which we do not have.
## 2) g77 also tends to duplicate paths via ../../.., so we canonicalize
## paths and remove duplicates.
## 3) We do not need -L/lib etc, nor those in LDFLAGS
## 4) We exclude path with CC will include when linking.
##
## First try to fathom out what -Lfoo commands are unnecessary.
case "${host_os}" in
  linux*)
    r_libpath_default="/usr/lib64 /lib64 /usr/lib /lib"
    ;;
  solaris*)
    r_libpath_default="/usr/lib /lib"
    ;;
  *)
    r_libpath_default=
    ;;
esac
r_extra_libs=
for arg in ${LDFLAGS} ${CLIBS}; do
  case "${arg}" in
    -L*)
      lib=`echo ${arg} | sed "s/^-L//"`
      test -d "${lib}" || continue
      ## Canonicalize (/usr/lib/gcc-lib/i686-linux/3.4.3/../../..).
      lib=`cd "${lib}" && ${GETWD}`
      r_extra_libs="${r_extra_libs} $lib"
      ;;
  esac
done

flibs=
if test "${GCC}" = yes; then
  linker_option="-Wl,"
else
  linker_option=
fi
r_save_flibs=""
for arg in ${FLIBS}; do
  case "${arg}" in
    ## this is not for a Fortran main program
    -lcrt*.o | -lfrtbegin | -lgfortranbegin | -lmx | -lSystem)
      ;;
    -[[a-zA-Z]]/*\" | -[[a-zA-Z]]*\\) # ifc
      ;;
    -l:*)
      flibs="${flibs} ${linker_option}${arg}"
      ;;
    -L*)
      lib=`echo ${arg} | sed "s/^-L//"`
      ## Do not add non-existent directories.
      test -d "${lib}" || continue
      ## Canonicalize (/usr/lib/gcc-lib/i686-linux/3.4.3/../../..).
      lib=`cd "${lib}" && ${GETWD}`
      r_want_lib=true
      ## Do not add something twice nor default paths nor those in LDFLAGS
      for dir in ${r_save_flibs} ${r_libpath_default} ${r_extra_libs}; do
        if test "${dir}" = "${lib}"; then
           r_want_lib=false
           break
        fi
      done
      if test x"${r_want_lib}" = xtrue; then
        flibs="${flibs} -L${lib}"
	r_save_flibs="${r_save_flibs} ${lib}"
      fi
      ;;
    *)
      flibs="${flibs} ${arg}"
      ;;
  esac
done
FLIBS="${flibs}"
fi
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
AC_MSG_CHECKING([whether ${F77} appends underscores to external names])
AC_CACHE_VAL([r_cv_prog_f77_append_underscore],
[case "${ac_cv_f77_mangling}" in
  *", underscore, "*)
    r_cv_prog_f77_append_underscore=yes
    ;;
  *", no underscore, "*)
    r_cv_prog_f77_append_underscore=no
    ;;
esac])
if test -n "${r_cv_prog_f77_append_underscore}"; then
  AC_MSG_RESULT([${r_cv_prog_f77_append_underscore}])
else
  AC_MSG_RESULT([unknown])
  AC_MSG_ERROR([cannot use Fortran])
fi
if test "${r_cv_prog_f77_append_underscore}" = yes; then
  AC_DEFINE(HAVE_F77_UNDERSCORE, 1,
            [Define if your Fortran compiler appends an underscore to
             external names.])
fi
AC_MSG_CHECKING([whether ${F77} appends extra underscores to external names])
AC_CACHE_VAL([r_cv_prog_f77_append_second_underscore],
[case "${ac_cv_f77_mangling}" in
  *", extra underscore")
    r_cv_prog_f77_append_second_underscore=yes
    ;;
  *", no extra underscore")
    r_cv_prog_f77_append_second_underscore=no
    ;;
esac])
if test -n "${r_cv_prog_f77_append_second_underscore}"; then
  AC_MSG_RESULT([${r_cv_prog_f77_append_second_underscore}])
else
  AC_MSG_RESULT([unknown])
  AC_MSG_ERROR([cannot use Fortran])
fi
if test "${r_cv_prog_f77_append_second_underscore}" = yes; then
  AC_DEFINE(HAVE_F77_EXTRA_UNDERSCORE, 1,
            [Define if your Fortran compiler appends an extra_underscore to
             external names containing an underscore.])
fi
])# R_PROG_F77_APPEND_UNDERSCORE

## R_PROG_F77_CAN_RUN
## --------------------
## Check whether the C/Fortran set up produces runnable code, as
## a preliminary to the compatibility tests.
## May fail if Fortran shared libraries are not in the library path.
## As from 2.4.0 use the same code as the compatibility test, as
## on at least one system the latter actually used -lgfortran
## (which was broken) and the previous test here did not.
AC_DEFUN([R_PROG_F77_CAN_RUN],
[AC_REQUIRE([AC_CHECK_LIBM])
AC_MSG_CHECKING([whether mixed C/Fortran code can be run])
AC_CACHE_VAL([r_cv_prog_f77_can_run],
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
#include <stdlib.h>
#include "confdefs.h"
#ifdef HAVE_F77_UNDERSCORE
# define F77_SYMBOL(x)   x ## _
#else
# define F77_SYMBOL(x)   x
#endif
int main () {
  exit(0);
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
  ## Be nice to people who put compiler architecture opts in CFLAGS
  if ${CC} ${CFLAGS} ${LDFLAGS} ${MAIN_LDFLAGS} -o conftest${ac_exeext} \
       conftest.${ac_objext} conftestf.${ac_objext} ${FLIBS} \
       ${LIBM} 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD;
  ## </NOTE>
  then
    ## redirect error messages to config.log
    output=`./conftest${ac_exeext} 2>&AS_MESSAGE_LOG_FD`
    if test ${?} = 0; then
      r_cv_prog_f77_can_run=yes
    fi
  fi
fi
])
rm -rf conftest conftest.* conftestf.* core
if test -n "${r_cv_prog_f77_can_run}"; then
  AC_MSG_RESULT([yes])
else
  if test "${cross_compiling}" = yes; then
    AC_MSG_RESULT([don't know (cross-compiling)])
  else
    AC_MSG_WARN([cannot run mixed C/Fortran code])
    AC_MSG_ERROR([Maybe check LDFLAGS for paths to Fortran libraries?])
  fi
fi
])# R_PROG_F77_CAN_RUN

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
#include <stdlib.h>
#include <stdio.h>
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
  if ${CC} ${CFLAGS} ${LDFLAGS} ${MAIN_LDFLAGS} -o conftest${ac_exeext} \
       conftest.${ac_objext} conftestf.${ac_objext} ${FLIBS} \
       ${LIBM} 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD;
  ## </NOTE>
  then
    ## redirect error messages to config.log
    output=`./conftest${ac_exeext} 2>&AS_MESSAGE_LOG_FD`
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
  if test "${cross_compiling}" = yes; then
    AC_MSG_RESULT([don't know (cross-compiling)])
  else
    AC_MSG_WARN([${F77} and ${CC} disagree on int and double])
    AC_MSG_ERROR([Maybe change CFLAGS or FFLAGS?])
  fi
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
#include <stdlib.h>
#include "confdefs.h"
#include <stdio.h>
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
  if ${CC} ${CFLAGS} ${LDFLAGS} ${MAIN_LDFLAGS} -o conftest${ac_exeext} \
       conftest.${ac_objext} conftestf.${ac_objext} ${FLIBS} \
       ${LIBM} 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD;
  ## </NOTE>
  then
    ## redirect error messages to config.log
    output=`./conftest${ac_exeext} 2>&AS_MESSAGE_LOG_FD`
    if test ${?} = 0; then
      r_cv_prog_f77_cc_compat_complex=yes
    fi
  fi
fi
])
rm -rf conftest conftest.* conftestf.* core
if test -n "${r_cv_prog_f77_cc_compat_complex}"; then
  AC_MSG_RESULT([yes])
  AC_DEFINE(HAVE_FORTRAN_DOUBLE_COMPLEX, 1,
            [Define if C's Rcomplex and Fortran's COMPLEX*16 can be
             interchanged, and can do arithmetic on the latter.])
else
  warn_f77_cc_double_complex="${F77} and ${CC} disagree on double complex"
  AC_MSG_WARN([${warn_f77_cc_double_complex}])
fi
AC_SUBST(HAVE_FORTRAN_DOUBLE_COMPLEX)
])# R_PROG_F77_CC_COMPAT_COMPLEX

## R_PROG_F77_FLAG(FLAG, [ACTION-IF-TRUE])
## ---------------------------------------
## Check whether the Fortran 77 compiler handles command line option
## FLAG, and set shell variable r_cv_prog_f77_flag_SFLAG accordingly
## (where SFLAG is a shell-safe transliteration of FLAG).
## In addition, execute ACTION-IF-TRUE in case of success.
AC_DEFUN([R_PROG_F77_FLAG],
[ac_safe=AS_TR_SH($1)
AC_MSG_CHECKING([whether ${F77} accepts $1])
AC_CACHE_VAL([r_cv_prog_f77_flag_${ac_safe}],
[AC_LANG_PUSH(Fortran 77)
r_save_FFLAGS="${FFLAGS}"
FFLAGS="${FFLAGS} $1"
AC_LINK_IFELSE([AC_LANG_PROGRAM()],
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

## R_PROG_OBJC_M
## -------------
## Check whether we can figure out ObjC Make dependencies.
AC_DEFUN([R_PROG_OBJC_M],
[AC_MSG_CHECKING([whether we can compute ObjC Make dependencies])
AC_CACHE_VAL([r_cv_prog_objc_m],
[echo "#include <math.h>" > conftest.m
for prog in "${OBJC} -MM" "${OBJC} -M" "${CPP} -M" "cpp -M"; do
  if ${prog} conftest.m 2>/dev/null | \
      grep 'conftest.o: conftest.m' >/dev/null; then
    r_cv_prog_objc_m="${prog}"
    break
  fi
done])
if test -z "${r_cv_prog_objc_m}"; then
  AC_MSG_RESULT([no])
else
  AC_MSG_RESULT([yes, using ${r_cv_prog_objc_m}])
fi
])# R_PROG_OBJC_M

## R_PROG_OBJC_MAKEFRAG
## --------------------
## Generate a Make fragment with suffix rules for the Obj-C compiler.
AC_DEFUN([R_PROG_OBJC_MAKEFRAG],
[r_objc_rules_frag=Makefrag.m
AC_REQUIRE([R_PROG_OBJC_M])
cat << \EOF > ${r_objc_rules_frag}
.m.o:
	$(OBJC) $(ALL_CPPFLAGS) $(ALL_OBJCFLAGS) -c $< -o $[@]
EOF
if test -n "${r_cv_prog_objc_m}"; then
  cat << EOF >> ${r_objc_rules_frag}
.m.d:
	@echo "making \$[@] from \$<"
	@${r_cv_prog_objc_m} \$(ALL_CPPFLAGS) $< > \$[@]
EOF
else
  cat << \EOF >> ${r_cc_rules_frag}
.m.d:
	@echo > $[@]
EOF
fi
AC_SUBST_FILE(r_objc_rules_frag)
])# R_PROG_OBJC_MAKEFRAG

## R_PROG_OBJC_FLAG(FLAG, [ACTION-IF-TRUE])
## ---------------------------------------
## Check whether the Obj-C compiler handles command line option FLAG,
## and set shell variable r_cv_prog_objc_flag_SFLAG accordingly (where
## SFLAG is a shell-safe transliteration of FLAG).
## In addition, execute ACTION-IF-TRUE in case of success.
AC_DEFUN([R_PROG_OBJC_FLAG],
[ac_safe=AS_TR_SH($1)

  if test -z "${OBJC}"; then
    eval r_cv_prog_objc_flag_${ac_safe}=no
  else
    AC_MSG_CHECKING([whether ${OBJC} accepts $1])
    AC_CACHE_VAL([r_cv_prog_objc_flag_${ac_safe}],
    [AC_LANG_PUSH([Objective C])
    r_save_OBJCFLAGS="${OBJCFLAGS}"
    OBJCFLAGS="${OBJCFLAGS} $1"
    AC_LINK_IFELSE([AC_LANG_PROGRAM()],
               [eval "r_cv_prog_objc_flag_${ac_safe}=yes"],
               [eval "r_cv_prog_objc_flag_${ac_safe}=no"])
	       OBJCFLAGS="${r_save_OBJCFLAGS}"
	       AC_LANG_POP([Objective C])
	       ])
    if eval "test \"`echo '$r_cv_prog_objc_flag_'$ac_safe`\" = yes"; then
      AC_MSG_RESULT([yes])
      [$2]
    else
      AC_MSG_RESULT([no])
    fi
  fi
])# R_PROG_OBJC_FLAG


## R_PROG_OBJC_RUNTIME
## -------------------
## Check for ObjC runtime and style.
## Effects:
##  * r_cv_objc_runtime
##    either "none" or flags necessary to link ObjC runtime
##    in the latter case they are also appended to OBJC_LIBS
##  * r_cv_objc_runtime_style
##    one of: unknown, gnu, next
##  * conditionals OBJC_GNU_RUNTIME and OBJC_NEXT_RUNTIME
AC_DEFUN([R_PROG_OBJC_RUNTIME],
[
  if test -z "${OBJC}"; then
    r_cv_objc_runtime=none
  else

  AC_LANG_PUSH([Objective C])

  # Don't check for headers, becasue that will require Obj-C preprocessor unconditionally (autoconf bug?)
  #AC_MSG_CHECKING([for ObjC headers])
  # Check for common headers
  #AC_CHECK_HEADERS_ONCE([objc/objc.h objc/objc-api.h objc/Object.h], [ ac_has_objc_headers=yes ], [
  #  AC_MSG_FAILURE([Objective C runtime headers were not found])
  #])

  # FIXME: we don't check whether the runtime needs -lpthread which is possible
  #        (empirically Linux GNU and Apple runtime don't)
  AC_CACHE_CHECK([for ObjC runtime library], [r_cv_objc_runtime], [
    save_OBJCFLAGS="$OBJCFLAGS"
    save_LIBS="$LIBS"
    r_cv_objc_runtime=
    for libobjc in objc objc-gnu objc-lf objc-lf2; do
      LIBS="${save_LIBS} -l${libobjc}"
      #OBJCFLAGS="$OBJCFLAGS $PTHREAD_CFLAGS -fgnu-runtime"
      AC_LINK_IFELSE([
	AC_LANG_PROGRAM([
#undef __OBJC2__
#include <objc/Object.h>
			], [
  @<:@Object class@:>@;
			])
		      ], [
		        r_cv_objc_runtime="-l${libobjc}"
			break
		      ])
    done
    LIBS="$save_LIBS"
    OBJCFLAGS="$save_OBJCFLAGS"
  ])

  OBJC_LIBS="${r_cv_objc_runtime} ${OBJC_LIBS}"

  if test "z${r_cv_objc_runtime}" != z; then
  AC_CACHE_CHECK([for ObjC runtime style], [r_cv_objc_runtime_style], [
    save_OBJCFLAGS="$OBJCFLAGS"
    save_LIBS="$LIBS"
    r_cv_objc_runtime_style=unknown
    LIBS="${OBJC_LIBS} $LIBS"
    for objc_lookup_class in objc_lookup_class objc_lookUpClass; do
      AC_LINK_IFELSE([
        AC_LANG_PROGRAM([
/* see PR#15107 */
#undef __OBJC2__
#include <objc/objc.h>
#include <objc/objc-api.h>
			], [
  id class = ${objc_lookup_class} ("Object");
			])
		      ], [
		        if test ${objc_lookup_class} = objc_lookup_class; then
			  r_cv_objc_runtime_style=gnu
			else
			  r_cv_objc_runtime_style=next
			fi
			break
		      ])
    done
    LIBS="$save_LIBS"
    OBJCFLAGS="$save_OBJCFLAGS"
  ])
  fi

  if test "${r_cv_objc_runtime_style}" = gnu; then
    AC_DEFINE([OBJC_GNU_RUNTIME], 1, [Define if using GNU-style Objective C runtime.])
  fi
  if test "${r_cv_objc_runtime_style}" = next; then
    AC_DEFINE([OBJC_NEXT_RUNTIME], 1, [Define if using NeXT/Apple-style Objective C runtime.])
  fi

  AC_LANG_POP([Objective C])
  fi # -n ${OBJC}
]
)

## R_PROG_OBJCXX_WORKS(compiler, [action on success], [action on failure])
## -------------------
##
## Check whether $1 compiles ObjC++ code successfully.
## The default action on success is to set OBJCXX to $1
AC_DEFUN([R_PROG_OBJCXX_WORKS],
[AC_MSG_CHECKING([whether $1 can compile ObjC++])
## we don't use AC_LANG_xx because ObjC++ is not defined as a language (yet)
## (the test program is from the gcc test suite)
## but it needed an #undef (PR#15107)
cat << \EOF > conftest.mm
#undef __OBJC2__
#include <objc/Object.h>
#include <iostream>

@interface Greeter : Object
- (void) greet: (const char *)msg;
@end

@implementation Greeter
- (void) greet: (const char *)msg { std::cout << msg; }
@end

int
main ()
{
  std::cout << "Hello from C++\n";
  Greeter *obj = @<:@Greeter new@:>@;
  @<:@obj greet: "Hello from Objective-C\n"@:>@;
}
EOF
echo "running: $1 -c conftest.mm ${CPPFLAGS} ${OBJCXXFLAGS}" >&AS_MESSAGE_LOG_FD
if $1 -c conftest.mm ${CPPFLAGS} ${OBJCXXFLAGS} >&AS_MESSAGE_LOG_FD 2>&1; then
   AC_MSG_RESULT([yes])
   rm -rf conftest conftest.* core
   m4_default([$2], OBJCXX=$1)
else
   AC_MSG_RESULT([no])
   rm -f conftest.mm
   [$3]
fi
]) # R_PROG_OBJCXX_WORKS

## R_PROG_OBJCXX
## -------------
## Check for ObjC++ compiler and set+subst OBJCXX correspondingly.
##
## We could add Objective-C++ language definition, but we still hope
## that autoconf will do that at some point, so we'll confine ourselves
## to finding a working compiler.
AC_DEFUN([R_PROG_OBJCXX],
[AC_BEFORE([AC_PROG_CXX], [$0])
AC_BEFORE([AC_PROG_OBJC], [$0])

AC_MSG_CHECKING([for Objective C++ compiler])
AC_CACHE_VAL([r_cv_OBJCXX],[
AC_MSG_RESULT([trying some possibilities])
if test -n "${OBJCXX}"; then
  R_PROG_OBJCXX_WORKS(${OBJCXX},,OBJCXX='')
fi
# try the sequence $OBJCXX, $CXX, $OBJC
if test -z "${OBJCXX}"; then
  R_PROG_OBJCXX_WORKS(${CXX},,
    if test -z "${OBJC}"; then
      R_PROG_OBJCXX_WORKS(${OBJC})
    fi
  )
fi
r_cv_OBJCXX="${OBJCXX}"
])
OBJCXX="${r_cv_OBJCXX}"
if test -z "${OBJCXX}"; then
  AC_MSG_RESULT([no working ObjC++ compiler found])
else
  AC_MSG_RESULT([${OBJCXX}])
fi
AC_SUBST(OBJCXX)
])# R_PROG_OBJCXX


### * Library functions

## R_FUNC___SETFPUCW
## -----------------
AC_DEFUN([R_FUNC___SETFPUCW],
[AC_CHECK_FUNC(__setfpucw,
[AC_CACHE_CHECK([whether __setfpucw is needed],
	        [r_cv_func___setfpucw_needed],
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
int main () {
#include <fpu_control.h>
#include <stdlib.h>
#if defined(_FPU_DEFAULT) && defined(_FPU_IEEE)
  exit(_FPU_DEFAULT != _FPU_IEEE);
#endif
  exit(0);
}
]])],
              [r_cv_func___setfpucw_needed=no],
              [r_cv_func___setfpucw_needed=yes],
              [r_cv_func___setfpucw_needed=no])])
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
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>
int main () {
  int *p = calloc(0, sizeof(int));
  exit(p == 0);
}
]])],
               [r_cv_func_calloc_works=yes],
               [r_cv_func_calloc_works=no],
               [r_cv_func_calloc_works=no])])
if test "x${r_cv_func_calloc_works}" = xyes; then
  AC_DEFINE(HAVE_WORKING_CALLOC, 1,
            [Define if calloc(0) returns a null pointer.])
fi
])# R_FUNC_CALLOC


## R_FUNC_ISFINITE
## ---------------
AC_DEFUN([R_FUNC_ISFINITE],
[AC_CACHE_CHECK([for working isfinite], [r_cv_func_isfinite_works],
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <math.h>
#include <stdlib.h>
#include "confdefs.h"
int main () {
#ifdef HAVE_DECL_ISFINITE
  exit(isfinite(1./0.) | isfinite(0./0.) | isfinite(-1./0.));
#else
  exit(1);
#endif
}
]])],
               [r_cv_func_isfinite_works=yes],
               [r_cv_func_isfinite_works=no],
               [r_cv_func_isfinite_works=no])])
if test "x${r_cv_func_isfinite_works}" = xyes; then
  AC_DEFINE(HAVE_WORKING_ISFINITE, 1,
            [Define if isfinite() is correct for -Inf/NaN/Inf.])
fi
])# R_FUNC_ISFINITE

## R_FUNC_LOG1P
## ------------
## Suggested by Nelson H. F. Beebe <beebe@math.utah.edu> to deal with
## inaccuracies on at least NetBSD 1.6 and OpenBSD 3.2.
## However, don't test all the way into denormalized x (he had k > -1074)
## and at x = 2^-54 (d - x)/x is around 3e-17.
AC_DEFUN([R_FUNC_LOG1P],
[AC_CACHE_CHECK([for working log1p], [r_cv_func_log1p_works],
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <math.h>
#include <stdlib.h>
#include "confdefs.h"
int main () {
#ifdef HAVE_LOG1P
  int k;
  double d;
  double x = 1.0;
  for(k = 0; k < 53; k++) x /= 2.0;

  /* log(1+x) = x - (1/2)x^2 + (1/3)x^3 - (1/4)x^4 ... */
  /*          = x for x sufficiently small */
  for(k = -54; k > -1022; --k) {
    x /= 2.0;
    if(x == 0.0)
      exit(0);			/* OK: reached underflow limit */
    d = log1p(x);
    if(d == 0.0)
      exit(1);			/* ERROR: inaccurate log1p() */
    /* for large k, ((1/2)x^2)/x might appear in the guard digits */
    if(k < -80 && d != x)
      exit(1);			/* ERROR: inaccurate log1p() */
  }
  exit(0);
#else
  exit(1);
#endif
}
]])],
               [r_cv_func_log1p_works=yes],
               [r_cv_func_log1p_works=no],
               [r_cv_func_log1p_works=no])])
if test "x${r_cv_func_log1p_works}" = xyes; then
  AC_DEFINE(HAVE_WORKING_LOG1P, 1,
            [Define if log1p() exists and is accurate enough.])
  RMATH_HAVE_WORKING_LOG1P="# define HAVE_WORKING_LOG1P 1"
else
  RMATH_HAVE_WORKING_LOG1P="# undef HAVE_WORKING_LOG1P"
fi
AC_SUBST(RMATH_HAVE_WORKING_LOG1P)
])# R_FUNC_LOG1P


## R_FUNC_FTELL
## ------------
AC_DEFUN([R_FUNC_FTELL],
[AC_CACHE_CHECK([whether ftell works correctly on files opened for append],
                [r_cv_working_ftell],
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>
#include <stdio.h>

main() {
    FILE *fp;
    int pos;

    fp = fopen("testit", "wb");
    fwrite("0123456789\n", 11, 1, fp);
    fclose(fp);
    fp = fopen("testit", "ab");
    pos = ftell(fp);
    fclose(fp);
    unlink("testit");
    exit(pos != 11);
}
]])],
              [r_cv_working_ftell=yes],
              [r_cv_working_ftell=no],
              [r_cv_working_ftell=no])])
if test "x${r_cv_working_ftell}" = xyes; then
  AC_DEFINE(HAVE_WORKING_FTELL, 1,
            [Define if your ftell works correctly on files opened for append.])
fi
])# R_FUNC_FTELL

### * Headers

## R_HEADER_SETJMP
## ---------------
AC_DEFUN([R_HEADER_SETJMP],
[AC_CACHE_CHECK([whether setjmp.h is POSIX.1 compatible],
                [r_cv_header_setjmp_posix],
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
[[#include <setjmp.h>]],
[[sigjmp_buf b;
sigsetjmp(b, 0);
siglongjmp(b, 1);]])],
                   [r_cv_header_setjmp_posix=yes],
                   [r_cv_header_setjmp_posix=no])])
AC_CHECK_DECLS([sigsetjmp, siglongjmp], , , [#include <setjmp.h>])
if test "$ac_cv_have_decl_sigsetjmp" = no; then
  r_cv_header_setjmp_posix=no
fi
if test "$ac_cv_have_decl_siglongjmp" = no; then
  r_cv_header_setjmp_posix=no
fi
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

### * Types

## R_TYPE_SOCKLEN
## --------------
AC_DEFUN([R_TYPE_SOCKLEN],
[AC_MSG_CHECKING([for type of socket length])
AC_CACHE_VAL([r_cv_type_socklen],
[for t in socklen_t size_t int; do
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <stddef.h>
#include <sys/types.h>
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#ifdef Win32
# include <winsock.h>
#endif
]],
[[(void)getsockopt (1, 1, 1, NULL, (${t} *)NULL)]])],
                    [r_cv_type_socklen=${t}; break],
                    [r_cv_type_socklen=])
done])
## size_t works on Windows but is unsigned and int is correct
case "${host_os}" in
  cygwin*|mingw*|windows*|winnt)
    r_cv_type_socklen=int
    ;;
esac
if test "x${r_cv_type_socklen}" = x; then
  warn_type_socklen="could not determine type of socket length"
  AC_MSG_WARN([${warn_type_socklen}])
else
  AC_MSG_RESULT([${r_cv_type_socklen} *])
fi
AC_DEFINE_UNQUOTED(R_SOCKLEN_T, ${r_cv_type_socklen},
                   [Type for socket lengths: socklen_t, sock_t, int?])
])# R_TYPE_SOCKLEN

## R_HAVE_KEYSYM
## -------------
## Check whether X11/X.h has KeySym typedef-ed.
AC_DEFUN([R_TYPE_KEYSYM],
[AC_REQUIRE([R_X11])
if test "${use_X11}" = yes; then
  r_save_CFLAGS="${CFLAGS}"
  CFLAGS="${CFLAGS} ${X_CFLAGS}"
  AC_CHECK_TYPE([KeySym],
                r_cv_type_keysym=yes,
                r_cv_type_keysym=no,
		[#include <X11/X.h>])
  CFLAGS="${r_save_CFLAGS}"
  if test "${r_cv_type_keysym}" = yes; then
    AC_DEFINE(HAVE_KEYSYM, 1,
              [Define if you have KeySym defined in X11.])
  fi
fi])# R_TYPE_KEYSYM

### * System services

## R_X11
## -----
## Updated for R 2.5.0.  We need -lXt, and nowadays that is unbundled.
AC_DEFUN([R_X11],
[AC_PATH_XTRA			# standard X11 search macro
use_X11="no"
if test -z "${no_x}"; then
  ## now we look for Xt and its header: it seems Intrinsic.h is key.
  r_save_CFLAGS="${CFLAGS}"
  CFLAGS="${CFLAGS} ${X_CFLAGS}"
  AC_CHECK_HEADER(X11/Intrinsic.h)
  CFLAGS="${r_save_CFLAGS}"
  if test "${ac_cv_header_X11_Intrinsic_h}" = yes ; then
    AC_CHECK_LIB(Xt, XtToolkitInitialize, [have_Xt=yes], [have_Xt=no],
                 [${X_LIBS} -lX11])
    if test "${have_Xt}" = yes; then
      use_X11="yes"
    fi
  fi
fi
if test "x${use_X11}" = "xyes"; then
  AC_DEFINE(HAVE_X11, 1,
            [Define if you have the X11 headers and libraries, and want
             the X11 GUI to be built.])
  X_LIBS="${X_LIBS} -lX11 -lXt"
else
  if test "x${with_x}" != "xno"; then
    AC_MSG_ERROR(
      [--with-x=yes (default) and X11 headers/libs are not available])
  fi
fi
  AC_MSG_RESULT([using X11 ... ${use_X11}])
])# R_X11

## R_X11_Xmu
## ---------
## test for -lXmu and for X11/Xmu/Xatom.h header (for XA_CLIPBOARD).
AC_DEFUN([R_X11_Xmu],
[if test "${use_X11}" = yes; then
  r_save_CFLAGS="${CFLAGS}"
  CFLAGS="${CFLAGS} ${X_CFLAGS}"
  AC_CHECK_HEADER(X11/Xmu/Atoms.h)
  CFLAGS="${r_save_CFLAGS}"
  if test "${ac_cv_header_X11_Xmu_Atoms_h}" = yes ; then
    AC_CHECK_LIB(Xmu, XmuInternAtom, [use_Xmu=yes], [use_Xmu=no], ${X_LIBS})
    if test "${use_Xmu}" = yes; then
      AC_DEFINE(HAVE_X11_Xmu, 1,
                [Define if you have the X11/Xmu headers and libraries.])
      X_LIBS="${X_LIBS} -lXmu"
    fi
  fi
fi])# R_X11_XMu


# R_CHECK_FRAMEWORK(function, framework,
#                   [action-if-found], [action-if-not-found],
#                   [other-libs])
# generic check for a framework, a function should be supplied to
# make sure the proper framework is found.
# default action is to set have_..._fw to yes/no and to define
# HAVE_..._FW if present
# NB: the does NOT cache have_..._fw, so use with care

AC_DEFUN([R_CHECK_FRAMEWORK],
[ AC_CACHE_CHECK([for $1 in $2 framework], [r_cv_check_fw_$2],
  r_cv_check_fw_save_LIBS=$LIBS
  r_cv_check_fw_$2=no
  LIBS="-framework $2 $5 $LIBS"
  AC_LINK_IFELSE([AC_LANG_CALL([],[$1])],
                 [r_cv_check_fw_$2="-framework $2"],[])
  LIBS=$r_cv_check_fw_save_LIBS
  AS_IF([test "$r_cv_check_fw_$2" != no],
        [m4_default([$3], [AC_DEFINE_UNQUOTED(AS_TR_CPP(HAVE_$2_FW), 1, [Defined if framework $2 is present])
	AS_TR_SH(have_$2_fw)=yes])],
	[m4_default([$4], AS_TR_SH(have_$2_fw)=no)])
)
])# R_CHECK_FRAMEWORK

## R_AQUA
## ------
AC_DEFUN([R_AQUA],
[use_aqua=no
if test "${want_aqua}" = yes; then
  case "${host_os}" in
    darwin*)
      ## we can build AQUA only with CoreFoundation, otherwise
      ## Quartz device won't build
      if test -n "${r_cv_check_fw_CoreFoundation}" ; then
        use_aqua=yes
      else
        AC_MSG_WARN([requested 'aqua' but CoreFoundation was not found])
      fi
      ;;
  esac
fi
if test "${use_aqua}" = yes; then
  AC_DEFINE(HAVE_AQUA, 1,
            [Define if you have the Aqua headers and libraries,
             and want to include support for R.app 
	     and for the quartz() device to be built.])
fi
])# R_AQUA

## R_OBJC_FOUNDATION_TEST
## ---------------------
## Checks whether ObjC code using Foundation classes can be compiled and sets
## ac_objc_foundation_works accordingly (yes/no)
AC_DEFUN([R_OBJC_FOUNDATION_TEST],
[
  if test -n "$1"; then AC_MSG_CHECKING([$1]); fi
  ac_objc_foundation_works=no
  AC_LINK_IFELSE([AC_LANG_PROGRAM([
#import <Foundation/Foundation.h>
], [[
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  NSString *s = @"hello, world";

  [pool release];
]])], [ ac_objc_foundation_works=yes ])
  if test "${ac_objc_foundation_works}" = yes; then
    if test -n "$1"; then AC_MSG_RESULT(yes); fi
    [$2]
  else
    if test -n "$1"; then AC_MSG_RESULT(no); fi
    [$3]
  fi
])

## R_OBJC_FOUNDATION
## -----------------
## Checks whether a Foundation framework implementation is available.
## * ac_objc_foundation: yes|no
## * FOUNDATION_CPPFLAGS, FOUNDATION_LIBS (subst)
##
## Currently supports (in order of precedence):
## - native (or custom FOUNDATION_LIBS/CPPFLAGS)
## - Apple Foundation (via -framework Foundation)
## - libFoundation
## - GNUstep
AC_DEFUN([R_OBJC_FOUNDATION],
[
  ac_objc_foundation=no
  if test -n "${OBJC}"; then

  r_foundation_cached=yes
  AC_MSG_CHECKING([for cached Foundation settings])
  AC_CACHE_VAL([r_cv_cache_foundation_flags], [
      r_cv_cache_foundation_flags=yes
      r_foundation_cached=no])
  AC_MSG_RESULT([${r_foundation_cached}])
  # if so, fetch them from the cache
  if test "${r_foundation_cached}" = yes; then
    AC_CACHE_CHECK([FOUNDATION_LIBS], [r_cv_FOUNDATION_LIBS])
    FOUNDATION_LIBS="${r_cv_FOUNDATION_LIBS}"
    AC_CACHE_CHECK([FOUNDATION_CPPFLAGS], [r_cv_FOUNDATION_CPPFLAGS])
    FOUNDATION_CPPFLAGS="${r_cv_FOUNDATION_CPPFLAGS}"
  else

  AC_LANG_PUSH([Objective C])
  rof_save_LIBS="${LIBS}"
  rof_save_CPPFLAGS="${CPPFLAGS}"
  LIBS="${LIBS} ${FOUNDATION_LIBS}"
  CPPFLAGS="${CPPFLAGS} ${FOUNDATION_CPPFLAGS}"
  R_OBJC_FOUNDATION_TEST([whether default Foundation framework works])
  if test "${ac_objc_foundation_works}" != yes; then
    LIBS="${rof_save_LIBS} -framework Foundation"
    CPPFLAGS="${rof_save_CPPFLAGS}"
    R_OBJC_FOUNDATION_TEST([whether -framework Foundation works],
      [FOUNDATION_LIBS='-framework Foundation'])
  fi
  if test "${ac_objc_foundation_works}" != yes; then
    LIBS="${rof_save_LIBS} -lFoundation ${OBJC_LIBS}"
    R_OBJC_FOUNDATION_TEST([whether libFoundation works],
      [FOUNDATION_LIBS='-lFoundation'])
  fi
  if test "${ac_objc_foundation_works}" != yes; then
    LIBS="${rof_save_LIBS}"
    ac_working_gnustep=no
    AC_MSG_CHECKING([for GNUstep])
    if test -z "${GNUSTEP_SYSTEM_ROOT}"; then
      for dir in /usr/lib/GNUstep /usr/local/lib/GNUstep; do
	if test -e "${dir}/System/Makefiles"; then GNUSTEP_SYSTEM_ROOT="${dir}/System"; break; fi
      done
    fi
    if test -z "${GNUSTEP_SYSTEM_ROOT}"; then
      AC_MSG_RESULT([no])
    else
      AC_MSG_RESULT([in ${GNUSTEP_SYSTEM_ROOT}])
      # this is a hack - we extract the relevant flags from GNUstep's makefiles.
      # in order to do that, we must setup the entire GNUstep environment which we do
      # in a separate script as to not pollute configure's environment
      cat << EOF > gnusteptest.sh
#!/bin/sh
. ${GNUSTEP_SYSTEM_ROOT}/Library/Makefiles/GNUstep.sh
${MAKE} -s -f gnustepmake -f \${GNUSTEP_MAKEFILES}/common.make -f \${GNUSTEP_MAKEFILES}/rules.make \${1}
EOF
	   cat << \EOF > gnustepmake
printcppflags: FORCE
	@echo $(ALL_CPPFLAGS) $(ADDITIONAL_OBJCFLAGS) $(AUXILIARY_OBJCFLAGS) $(GNUSTEP_HEADERS_FLAGS)
printlibs: FORCE
	@echo $(ALL_LIB_DIRS) $(FND_LIBS) $(ADDITIONAL_OBJC_LIBS) $(AUXILIARY_OBJC_LIBS) $(OBJC_LIBS) $(SYSTEM_LIBS) $(TARGET_SYSTEM_LIBS)
FORCE:
EOF
	GNUSTEP_CPPFLAGS=`sh gnusteptest.sh printcppflags`
	GNUSTEP_LIBS=`sh gnusteptest.sh printlibs`
	#echo "  GNUstep CPPFLAGS: ${GNUSTEP_CPPFLAGS}"
	#echo "  GNUstep LIBS: ${GNUSTEP_LIBS}"
	LIBS="${rof_save_LIBS} ${GNUSTEP_LIBS}"
	CPPFLAGS="${rof_save_CPPFLAGS} ${GNUSTEP_CPPFLAGS}"
	rm -f gnusteptest.sh gnustepmake
	R_OBJC_FOUNDATION_TEST([whether GNUstep works],[
	  FOUNDATION_CPPFLAGS="${GNUSTEP_CPPFLAGS}"
	  FOUNDATION_LIBS="${GNUSTEP_LIBS}"])
    fi # -n GNUSTEP_SYSTEM_ROOT
  fi
  LIBS="${rof_save_LIBS}"
  CPPFLAGS="${rof_save_CPPFLAGS}"
  AC_SUBST(FOUNDATION_CPPFLAGS)
  AC_SUBST(FOUNDATION_LIBS)
  AC_CACHE_VAL([r_cv_FOUNDATION_CPPFLAGS],[r_cv_FOUNDATION_CPPFLAGS="${FOUNDATION_CPPFLAGS}"])
  AC_CACHE_VAL([r_cv_FOUNDATION_LIBS],[r_cv_FOUNDATION_LIBS="${FOUNDATION_LIBS}"])
  AC_LANG_POP([Objective C])
  ac_objc_foundation=${ac_objc_foundation_works}

  fi # not cached flags

  fi # -n ${OBJC}

  AC_CACHE_CHECK([for working Foundation implementation], [r_cv_objc_foundation], [r_cv_objc_foundation="${ac_objc_foundation}"])
])

## R_IEEE_754
## ----------
## According to C99, isnan and isfinite are macros in math.h,
## but some older systems have isnan as a function (possibly as well).
AC_DEFUN([R_IEEE_754],
[AC_CHECK_FUNCS([isnan])
AC_CHECK_DECLS([isfinite, isnan], , , [#include <math.h>])
AC_CACHE_CHECK([whether you have IEEE 754 floating-point arithmetic],
               [r_cv_ieee_754],
[if (test "${ac_cv_func_isnan}" = yes \
      || test "${ac_cv_have_decl_isnan}" = yes); then
  r_cv_ieee_754=yes
else
  r_cv_ieee_754=no
fi])
if test "${r_cv_ieee_754}" = yes; then
  AC_DEFINE(IEEE_754, 1,
            [Define if you have IEEE 754 floating point arithmetic.])
else
  AC_MSG_ERROR([IEEE 754 floating-point arithmetic is required])
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
if test "${use_jpeglib}" = yes; then
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
fi
if test "${use_libpng}" = yes; then
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
fi
if test "${use_libtiff}" = yes; then
  AC_CHECK_HEADERS(tiffio.h)
  if test "x${ac_cv_header_tiffio_h}" = xyes ; then
    # may need to resolve jpeg routines
    AC_CHECK_LIB(tiff, TIFFOpen, [have_tiff=yes], [have_tiff=no], [${BITMAP_LIBS}])
    if test "x${have_tiff}" = xyes; then
      AC_DEFINE(HAVE_TIFF, 1, [Define this if libtiff is available.])
      BITMAP_LIBS="-ltiff ${BITMAP_LIBS}"
    else
      # tiff 4.0.x may need lzma too: SU's static build does
      unset ac_cv_lib_tiff_TIFFOpen
      AC_CHECK_LIB(tiff, TIFFOpen, [have_tiff=yes], [have_tiff=no], [-llzma ${BITMAP_LIBS} -llzma])
      if test "x${have_tiff}" = xyes; then
        AC_DEFINE(HAVE_TIFF, 1, [Define this if libtiff is available.])
        BITMAP_LIBS="-ltiff -llzma ${BITMAP_LIBS}"
      else
        have_tiff=no
      fi
    fi
  fi
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
[AC_CACHE_CHECK([if libpng version >= 1.2.7],
                [r_cv_header_png_h],
AC_EGREP_CPP([yes],
[#include <png.h>
#if (PNG_LIBPNG_VER >= 10207)
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
## /opt/csw/lib and /usr/sfw/lib are for Solaris (blastwave and sunfreeware
## respectively).
## /opt/freeware/lib is for 'IBM AIX Toolbox for Linux Applications'
## We want to look in LIBnn only here.
AC_DEFUN([_R_PATH_TCL_CONFIG],
[AC_MSG_CHECKING([for tclConfig.sh in library (sub)directories])
AC_CACHE_VAL([r_cv_path_TCL_CONFIG],
[for ldir in /usr/local/${LIBnn} /usr/${LIBnn} /${LIBnn} /opt/lib /sw/lib /opt/csw/lib /usr/sfw/lib /opt/freeware/lib; do
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
[for ldir in /usr/local/${LIBnn} /usr/${LIBnn} /${LIBnn} /opt/lib /sw/lib /opt/csw/lib /usr/sfw/lib /opt/freeware/lib; do
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
  ## This doesn't make a great deal of sense: on past form
  ## we don't even expect future versions of 8.x to work, let alone 9.0
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
/* Revise if 9.x ever appears (and 8.x seems to increment only
   every few years). */
#if (TCL_MAJOR_VERSION >= 8) && (TCL_MINOR_VERSION >= 4)
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
/* Revise if 9.x ever appears (and 8.x seems to increment only
   every few years). */
#if (TK_MAJOR_VERSION >= 8) && (TK_MINOR_VERSION >= 4)
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
      ## TCL_INCLUDE_SPEC (if set) is what we want.
      if test -n ${TCL_INCLUDE_SPEC} ; then
        r_save_CPPFLAGS="${CPPFLAGS}"
	CPPFLAGS="${CPPFLAGS} ${TCL_INCLUDE_SPEC}"
	AC_CHECK_HEADER([tcl.h],
			[TCLTK_CPPFLAGS="${TCL_INCLUDE_SPEC}"
			 found_tcl_h=yes])
	CPPFLAGS="${r_save_CPPFLAGS}"
      fi
      if test "${found_tcl_h}" = no; then
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
      ## TK_INCLUDE_SPEC (if set) is what we want.
      if test -n ${TK_INCLUDE_SPEC} ; then
        r_save_CPPFLAGS="${CPPFLAGS}"
	CPPFLAGS="${CPPFLAGS} ${TCLTK_CPPFLAGS} ${TK_XINCLUDES} ${TK_INCLUDE_SPEC}"
	AC_CHECK_HEADER([tk.h],
		        [TCLTK_CPPFLAGS="${TCLTK_CPPFLAGS} ${TK_INCLUDE_SPEC}"
			 found_tk_h=yes])
	CPPFLAGS="${r_save_CPPFLAGS}"
      fi
      if test "${found_tk_h}" = no; then
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
if test "${have_tcltk}" = yes; then
  if test -n "${TK_XINCLUDES}"; then
    TCLTK_CPPFLAGS="${TCLTK_CPPFLAGS} ${TK_XINCLUDES}"
  else
    TCLTK_CPPFLAGS="${TCLTK_CPPFLAGS} ${X_CFLAGS}"
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
      TCLTK_LIBS="${TCLTK_LIBS} ${TK_LIB_SPEC} ${TK_XLIBSW}"
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
  ## Force evaluation ('-ltcl8.3${TCL_DBGX}' and friends ...).
  eval "TCLTK_LIBS=\"${TCLTK_LIBS}\""
fi
])# _R_TCLTK_LIBS

## _R_TCLTK_WORKS
## --------------
## Check whether compiling and linking code using Tcl/Tk works.
## Set shell variable r_cv_tcltk_works to 'yes' or 'no' accordingly.
AC_DEFUN([_R_TCLTK_WORKS],
[AC_CACHE_CHECK([whether compiling/linking Tcl/Tk code works],
                [r_cv_tcltk_works],
[AC_LANG_PUSH(C)
r_save_CPPFLAGS="${CPPFLAGS}"
r_save_LIBS="${LIBS}"
CPPFLAGS="${CPPFLAGS} ${TCLTK_CPPFLAGS}"
LIBS="${LIBS} ${TCLTK_LIBS}"
AC_LINK_IFELSE([AC_LANG_PROGRAM(
[[#include <tcl.h>
#include <tk.h>
]],
[[static char * p1 = (char *) Tcl_Init;
static char * p2 = (char *) Tk_Init;
]])],
r_cv_tcltk_works=yes,
r_cv_tcltk_works=no)
CPPFLAGS="${r_save_CPPFLAGS}"
LIBS="${r_save_LIBS}"
AC_LANG_POP(C)])
])# _R_TCLTK_WORKS

## R_TCLTK
## -------
AC_DEFUN([R_TCLTK],
[if test "${want_tcltk}" = yes; then
  have_tcltk=yes
  ## (Note that the subsequent 3 macros assume that have_tcltk has been
  ## set appropriately.)
  _R_TCLTK_CONFIG
  _R_TCLTK_CPPFLAGS
  _R_TCLTK_LIBS
  if test "${have_tcltk}" = yes; then
    _R_TCLTK_WORKS
    have_tcltk=${r_cv_tcltk_works}
  fi
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
## (http://www.gnu.org/software/ac-archive/htmldoc/acx_blas.m4),
## with the following changes:
## * We also handle HPUX .sl command line specifications.
## * We explictly deal with the case of f2c.  Most likely pointless.
## * We only care about the Fortran 77 interface to Atlas, hence do not
##   test for -lcblas.
## * We do not use BLAS libs that caused problems in the past: Alpha
##   CXML and DXML, and SGI SCSL and SGIMATH (marked with COMMENT tags).
## * As we link with $BLAS_LIBS $FLIBS $LIBS (in that order), we use the
##   same order in the tests.
## * We do not use ACTION-IF-FOUND and ACTION-IF-NOT-FOUND.
## The sunperf test calls the library as now required.
## Based on acx_blas.m4 version 1.2 (2001-12-13)
AC_DEFUN([R_BLAS_LIBS],
[AC_REQUIRE([R_PROG_F77_FLIBS])
AC_REQUIRE([R_PROG_F77_APPEND_UNDERSCORE])

acx_blas_ok=no
case "${with_blas}" in
  yes | "") ;;
  no) acx_blas_ok=disable ;;
  -* | */* | *.a | *.so | *.so.* | *.sl | *.sl.* | *.o)
    BLAS_LIBS="${with_blas}"
    ;;
  *) BLAS_LIBS="-l${with_blas}" ;;
esac

if test "${r_cv_prog_f77_append_underscore}" = yes; then
  dgemm=dgemm_
  sgemm=sgemm_
  xerbla=xerbla_
else
  dgemm=dgemm
  sgemm=sgemm
  xerbla=xerbla
fi

acx_blas_save_LIBS="${LIBS}"
LIBS="${FLIBS} ${LIBS}"

## First, check BLAS_LIBS environment variable
if test "${acx_blas_ok}" = no; then
  if test "x${BLAS_LIBS}" != x; then
    r_save_LIBS="${LIBS}"; LIBS="${BLAS_LIBS} ${LIBS}"
    AC_MSG_CHECKING([for ${dgemm} in ${BLAS_LIBS}])
    AC_TRY_LINK([void ${xerbla}(char *srname, int *info){}], ${dgemm}(),
      [acx_blas_ok=yes], [BLAS_LIBS=""])
    AC_MSG_RESULT([${acx_blas_ok}])
    LIBS="${r_save_LIBS}"
  fi
fi

## BLAS linked to by default?  (happens on some supercomputers)
if test "${acx_blas_ok}" = no; then
  AC_CHECK_FUNC(${dgemm}, [acx_blas_ok=yes])
fi

## BLAS in ATLAS library?  (http://math-atlas.sourceforge.net/)
if test "${acx_blas_ok}" = no; then
  AC_CHECK_LIB(atlas, ATL_xerbla,
               [AC_CHECK_LIB(f77blas, ${dgemm},
                             [acx_blas_ok=yes
                              BLAS_LIBS="-lf77blas -latlas"],
			     [], [-latlas])])
fi

## BLAS in PhiPACK libraries?  (requires generic BLAS lib, too)
if test "${acx_blas_ok}" = no; then
  AC_CHECK_LIB(blas, ${dgemm},
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
## Some versions require -xlic_lib=sunperf: -lsunperf will not work
## Not sure whether -lsunmath is required, but it helps anyway
if test "${acx_blas_ok}" = no; then
  if test "x$GCC" != xyes; then # only works with Sun CC
     AC_MSG_CHECKING([for ${dgemm} in -lsunperf])
     r_save_LIBS="${LIBS}"
     LIBS="-xlic_lib=sunperf -lsunmath ${LIBS}"
     AC_TRY_LINK_FUNC([${dgemm}], [R_sunperf=yes], [R_sunperf=no])
     if test "${R_sunperf}" = yes; then
        BLAS_LIBS="-xlic_lib=sunperf -lsunmath"
	acx_blas_ok=yes
     fi
     LIBS="${r_save_LIBS}"
     AC_MSG_RESULT([${acx_blas_ok}])
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
  AC_CHECK_LIB(blas, ${dgemm},
	       [AC_CHECK_LIB(essl, ${dgemm},
			     [acx_blas_ok=yes
                              BLAS_LIBS="-lessl -lblas"],
			     [], [-lblas ${FLIBS}])])
fi

## Generic BLAS library?
if test "${acx_blas_ok}" = no; then
  AC_CHECK_LIB(blas, ${dgemm},
               [acx_blas_ok=yes; BLAS_LIBS="-lblas"])
fi

## Now check if zdotu works (fails on AMD64 with the wrong compiler;
## also fails on OS X with vecLib and gfortran; but in that case we
## have a work-around using USE_VECLIB_G95FIX)
if test "${acx_blas_ok}" = yes; then
  AC_MSG_CHECKING([whether double complex BLAS can be used])
  AC_CACHE_VAL([r_cv_zdotu_is_usable],
  [cat > conftestf.f <<EOF
c Goto's BLAS at least needs a XERBLA
      subroutine xerbla(srname, info)
      character*6 srname
      integer info
      end

      subroutine test1(iflag)
      double complex zx(2), ztemp, zres, zdotu
      integer iflag
      zx(1) = (3.1d0,1.7d0)
      zx(2) = (1.6d0,-0.6d0)
      zres = zdotu(2, zx, 1, zx, 1)
      ztemp = (0.0d0,0.0d0)
      do 10 i = 1,2
 10      ztemp = ztemp + zx(i)*zx(i)
      if(abs(zres - ztemp) > 1.0d-10) then
        iflag = 1
      else
        iflag = 0
      endif
      end
EOF
${F77} ${FFLAGS} -c conftestf.f 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD
## Yes we need to double quote this ...
[cat > conftest.c <<EOF
#include <stdlib.h>
#include "confdefs.h"
#ifdef HAVE_F77_UNDERSCORE
# define F77_SYMBOL(x)   x ## _
#else
# define F77_SYMBOL(x)   x
#endif
extern void F77_SYMBOL(test1)(int *iflag);

int main () {
  int iflag;
  F77_SYMBOL(test1)(&iflag);
  exit(iflag);
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
  if ${CC} ${CFLAGS} ${LDFLAGS} ${MAIN_LDFLAGS} -o conftest${ac_exeext} \
       conftest.${ac_objext} conftestf.${ac_objext} ${FLIBS} \
       ${LIBM} ${BLAS_LIBS} 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD;
  ## </NOTE>
  then
    ## redirect error messages to config.log
    output=`./conftest${ac_exeext} 2>&AS_MESSAGE_LOG_FD`
    if test ${?} = 0; then
      r_cv_zdotu_is_usable=yes
    fi
  fi
fi
])
  rm -rf conftest conftest.* conftestf.* core
  if test -n "${r_cv_zdotu_is_usable}"; then
    AC_MSG_RESULT([yes])
  else
    ## NB: this lot is not cached
    if test "${r_cv_check_fw_vecLib}" != "no"; then
      AC_MSG_RESULT([yes])
      ## for vecLib we have a work-around by using cblas_..._sub
      use_veclib_g95fix=yes
      ## The fix may not work with internal lapack, but
      ## is more likely to in R >= 2.15.2.
    else
      AC_MSG_RESULT([no])
      BLAS_LIBS=
      acx_blas_ok="no"
    fi
  fi
fi
if test "${acx_blas_ok}" = yes; then
  AC_MSG_CHECKING([whether the BLAS is complete])
  AC_CACHE_VAL([r_cv_complete_blas],
[[cat > conftest.c <<EOF
#include <stdlib.h>
#include "confdefs.h"
#ifdef HAVE_F77_UNDERSCORE
# define F77_SYMBOL(x)   x ## _
#else
# define F77_SYMBOL(x)   x
#endif
void F77_SYMBOL(xerbla)(char *srname, int *info)
{}
void blas_set () {
  F77_SYMBOL(dasum)();
  F77_SYMBOL(daxpy)();
  F77_SYMBOL(dcopy)();
  F77_SYMBOL(ddot)();
  F77_SYMBOL(dgbmv)();
  F77_SYMBOL(dgemm)();
  F77_SYMBOL(dgemv)();
  F77_SYMBOL(dger)();
  F77_SYMBOL(dnrm2)();
  F77_SYMBOL(drot)();
  F77_SYMBOL(drotg)();
  F77_SYMBOL(drotm)();
  F77_SYMBOL(drotmg)();
  F77_SYMBOL(dsbmv)();
  F77_SYMBOL(dscal)();
  F77_SYMBOL(dsdot)();
  F77_SYMBOL(dspmv)();
  F77_SYMBOL(dspr)();
  F77_SYMBOL(dspr2)();
  F77_SYMBOL(dswap)();
  F77_SYMBOL(dsymm)();
  F77_SYMBOL(dsymv)();
  F77_SYMBOL(dsyr)();
  F77_SYMBOL(dsyr2)();
  F77_SYMBOL(dsyr2k)();
  F77_SYMBOL(dsyrk)();
  F77_SYMBOL(dtbmv)();
  F77_SYMBOL(dtbsv)();
  F77_SYMBOL(dtpmv)();
  F77_SYMBOL(dtpsv)();
  F77_SYMBOL(dtrmm)();
  F77_SYMBOL(dtrmv)();
  F77_SYMBOL(dtrsm)();
  F77_SYMBOL(dtrsv)();
  F77_SYMBOL(idamax)();
  F77_SYMBOL(lsame)();
#ifdef HAVE_FORTRAN_DOUBLE_COMPLEX
/* cmplxblas */
  F77_SYMBOL(dcabs1)();
  F77_SYMBOL(dzasum)();
  F77_SYMBOL(dznrm2)();
  F77_SYMBOL(izamax)();
  F77_SYMBOL(zaxpy)();
  F77_SYMBOL(zcopy)();
  F77_SYMBOL(zdotc)();
  F77_SYMBOL(zdotu)();
  F77_SYMBOL(zdrot)();
  F77_SYMBOL(zdscal)();
  F77_SYMBOL(zgbmv)();
  F77_SYMBOL(zgemm)();
  F77_SYMBOL(zgemv)();
  F77_SYMBOL(zgerc)();
  F77_SYMBOL(zgeru)();
  F77_SYMBOL(zhbmv)();
  F77_SYMBOL(zhemm)();
  F77_SYMBOL(zhemv)();
  F77_SYMBOL(zher)();
  F77_SYMBOL(zherk)();
  F77_SYMBOL(zher2)();
  F77_SYMBOL(zher2k)();
  F77_SYMBOL(zhpmv)();
  F77_SYMBOL(zhpr)();
  F77_SYMBOL(zhpr2)();
  F77_SYMBOL(zrotg)();
  F77_SYMBOL(zscal)();
  F77_SYMBOL(zswap)();
  F77_SYMBOL(zsymm)();
  F77_SYMBOL(zsyr2k)();
  F77_SYMBOL(zsyrk)();
  F77_SYMBOL(ztbmv)();
  F77_SYMBOL(ztbsv)();
  F77_SYMBOL(ztpmv)();
  F77_SYMBOL(ztpsv)();
  F77_SYMBOL(ztrmm)();
  F77_SYMBOL(ztrmv)();
  F77_SYMBOL(ztrsm)();
  F77_SYMBOL(ztrsv)();
#endif
}
int main ()
{
  exit(0);
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
  if ${CC} ${CFLAGS} ${LDFLAGS} ${MAIN_LDFLAGS} -o conftest${ac_exeext} \
       conftest.${ac_objext} ${FLIBS} \
       ${LIBM} ${BLAS_LIBS} 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD;
  ## </NOTE>
  then
    r_cv_complete_blas=yes
  fi
fi
])
  if test x"${r_cv_complete_blas}" != xyes; then
    acx_blas_ok="no"
    r_cv_complete_blas=no
    BLAS_LIBS=""
  fi
  AC_MSG_RESULT([${r_cv_complete_blas}])
fi

LIBS="${acx_blas_save_LIBS}"

AC_SUBST(BLAS_LIBS)
])# R_BLAS_LIBS

## R_LAPACK_LIBS
## -------------
## Look for a library that implements LAPACK (see
## http://www.netlib.org/lapack/).  On success, sets LAPACK_LIBS to the
## requisite library linkages.  Only used by the lapack module at
## present.
##
## This is roughly based on ACX_LAPACK by Steven G. Johnson
## <stevenj@alum.mit.edu> from the Official Autoconf Macro Archive
## (http://www.gnu.org/software/ac-archive/htmldoc/acx_lapack.m4),
## with the following changes:
## * We also handle HPUX .sl command line specifications.
## * We explictly deal with the case of f2c.  Most likely pointless.
## * We test for a LAPACK_LIBS environment variable after checking
##   whether LAPACK is already linked (see below).
## * We do not test for the generic lapack_rs6k library (why not?).
## * As we link with $LAPACK_LIBS $BLAS_LIBS $FLIBS $LIBS (in that
##   order), we use the same order in the tests.
## * We do not use ACTION-IF-FOUND and ACTION-IF-NOT-FOUND.
## Note that Debian ATLAS has LAPACK libs in /usr/lib/atlas (or $arch
## variants) which should be used if ATLAS is used for BLAS, and not
## found at configure time but used at run time ...
## Note also that (see R-admin) that our main intention is to allow a
## LAPACK-containing BLAS to be used ... there are too many slow or
## broken LAPACKs out there.
## Based on acx_lapack.m4 version 1.3 (2002-03-12).

## Test function was zgeev, changed to dpstrf which is LAPACK 3.2.

AC_DEFUN([R_LAPACK_LIBS],
[AC_REQUIRE([R_PROG_F77_FLIBS])
AC_REQUIRE([R_PROG_F77_APPEND_UNDERSCORE])
AC_REQUIRE([R_BLAS_LIBS])

acx_lapack_ok=no
case "${with_lapack}" in
  yes | "") ;;
  no) acx_lapack_ok=disable ;;
  -* | */* | *.a | *.so | *.so.* | *.sl | *.sl.* | *.o)
    LAPACK_LIBS="${with_lapack}"
    ;;
  *) LAPACK_LIBS="-l${with_lapack}" ;;
esac

if test "${r_cv_prog_f77_append_underscore}" = yes; then
  lapack=dpstrf_
else
  lapack=dpstrf
fi

# We cannot use LAPACK if BLAS is not found
if test "x${acx_blas_ok}" != xyes; then
  acx_lapack_ok=noblas
fi

acx_lapack_save_LIBS="${LIBS}"
LIBS="${BLAS_LIBS} ${FLIBS} ${LIBS}"

## LAPACK linked to by default?  (Could be in the BLAS libs.)
if test "${acx_lapack_ok}" = no; then
  AC_CHECK_FUNC(${lapack}, [acx_lapack_ok=yes])
fi

## Next, check LAPACK_LIBS environment variable
if test "${acx_lapack_ok}" = no; then
  if test "x${LAPACK_LIBS}" != x; then
    r_save_LIBS="${LIBS}"; LIBS="${LAPACK_LIBS} ${LIBS}"
    AC_MSG_CHECKING([for ${lapack} in ${LAPACK_LIBS}])
    AC_TRY_LINK_FUNC(${lapack}, [acx_lapack_ok=yes], [LAPACK_LIBS=""])
    AC_MSG_RESULT([${acx_lapack_ok}])
    LIBS="${r_save_LIBS}"
  fi
fi

## LAPACK in Sun Performance library?
## No longer test here as will be picked up by the default test.

## Generic LAPACK library?
if test "${acx_lapack_ok}" = no; then
  AC_CHECK_LIB(lapack, ${lapack},
               [acx_lapack_ok=yes; LAPACK_LIBS="-llapack"])
fi

LIBS="${acx_lapack_save_LIBS}"

AC_SUBST(LAPACK_LIBS)
])# R_LAPACK_LIBS

## R_XDR
## -----
## Try finding XDR library functions and headers.
## FreeBSD in particular needs rpc/types.h before rpc/xdr.h.
AC_DEFUN([R_XDR],
[AC_CHECK_HEADER(rpc/types.h)
if test "${ac_cv_header_rpc_types_h}" = yes ; then
  AC_CHECK_HEADER(rpc/xdr.h, , , [#include <rpc/types.h>])
fi
if test "${ac_cv_header_rpc_types_h}" = yes && \
   test "${ac_cv_header_rpc_xdr_h}" = yes && \
   test "${ac_cv_search_xdr_string}" != no ; then
  r_xdr=yes
else
  r_xdr=no
fi
TIRPC_CPPFLAGS=
if test "${r_xdr}" = no ; then
  ## No RPC headers, so try for TI-RPC headers: need /usr/include/tirpc
  ## on include path to find /usr/include/tirpc/netconfig.h
  save_CPPFLAGS=${CPPFLAGS}
  CPPFLAGS="${CPPFLAGS} -I/usr/include/tirpc"
  AC_CHECK_HEADER(tirpc/rpc/types.h)
  if test "${ac_cv_header_tirpc_rpc_types_h}" = yes ; then
    AC_CHECK_HEADER(tirpc/rpc/xdr.h, , , [#include <tirpc/rpc/types.h>])
  fi
  if test "${ac_cv_header_tirpc_rpc_types_h}" = yes && \
       test "${ac_cv_header_tirpc_rpc_xdr_h}" = yes &&
       test "${ac_cv_search_xdr_string}" != no ; then
    TIRPC_CPPFLAGS=-I/usr/include/tirpc
    r_xdr=yes
  fi
  CPPFLAGS="${save_CPPFLAGS}"
fi
AC_MSG_CHECKING([for XDR support])
AC_MSG_RESULT([${r_xdr}])
AM_CONDITIONAL(BUILD_XDR, [test "x${r_xdr}" = xno])
AC_SUBST(TIRPC_CPPFLAGS)
])# R_XDR

## R_ZLIB
## ------
## Try finding zlib library and headers.
## We check that both are installed, and that the header >= 1.2.3
AC_DEFUN([R_ZLIB],
[if test "x${use_system_zlib}" = xyes; then
  AC_CHECK_LIB(z, inflateInit2_, [have_zlib=yes], [have_zlib=no])
  if test "${have_zlib}" = yes; then
    AC_CHECK_HEADER(zlib.h, [have_zlib=yes], [have_zlib=no])
  fi
  if test "${have_zlib}" = yes; then
    _R_HEADER_ZLIB
    have_zlib=${r_cv_header_zlib_h}
  fi
else
  have_zlib="no"
fi
AC_MSG_CHECKING([whether zlib support needs to be compiled])
if test "${have_zlib}" = yes; then
  AC_MSG_RESULT([no])
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
[AC_CACHE_CHECK([if zlib version >= 1.2.5],
                [r_cv_header_zlib_h],
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>
#include <string.h>
#include <zlib.h>
int main() {
#ifdef ZLIB_VERSION
/* Work around Debian bug: it uses 1.2.3.4 even though there was no such
   version on the master site zlib.net */
  exit(strncmp(ZLIB_VERSION, "1.2.5", 5) < 0);
#else
  exit(1);
#endif
}
]])],
              [r_cv_header_zlib_h=yes],
              [r_cv_header_zlib_h=no],
              [r_cv_header_zlib_h=no])])
])# _R_HEADER_ZLIB

## _R_ZLIB_MMAP
## ------------
AC_DEFUN([_R_ZLIB_MMAP],
[AC_CACHE_CHECK([mmap support for zlib],
                [r_cv_zlib_mmap],
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
caddr_t hello() {
  exit(mmap((caddr_t)0, (off_t)0, PROT_READ, MAP_SHARED, 0, (off_t)0));
}
]])],
              [r_cv_zlib_mmap=no],
              [r_cv_zlib_mmap=yes],
              [r_cv_zlib_mmap=yes])])
])# _R_ZLIB_MMAP

## R_PCRE
## ------
## If selected, try finding system pcre library and headers.
## RedHat put the headers in /usr/include/pcre.
## R (2.15.3, 3.0.0) includes 8.32: there are problems < 8.10 and
## distros are often slow to update.
AC_DEFUN([R_PCRE],
[if test "x${use_system_pcre}" = xyes; then
  AC_CHECK_LIB(pcre, pcre_fullinfo, [have_pcre=yes], [have_pcre=no])
  if test "${have_pcre}" = yes; then
    AC_CHECK_HEADERS(pcre.h pcre/pcre.h)
    if test "${ac_cv_header_pcre_h}" = no \
	&& test "${ac_cv_header_pcre_pcre_h}" = no; then
      have_pcre=no
    fi
  fi
else
  have_pcre=no
fi
if test "x${have_pcre}" = xyes; then
AC_CACHE_CHECK([if PCRE version >= 8.10], [r_cv_have_pcre810],
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#ifdef HAVE_PCRE_PCRE_H
#include <pcre/pcre.h>
#else
#ifdef HAVE_PCRE_H
#include <pcre.h>
#endif
#endif
int main() {
#ifdef PCRE_MAJOR
#if PCRE_MAJOR > 8
  exit(0);
#elif PCRE_MAJOR == 8 && PCRE_MINOR >= 10
  exit(0);
#else
  exit(1);
#endif
#else
  exit(1);
#endif
}
]])], [r_cv_have_pcre810=yes], [r_cv_have_pcre810=no], [r_cv_have_pcre810=no])])
fi
if test "x${r_cv_have_pcre810}" = xyes; then
  LIBS="-lpcre ${LIBS}"
fi
AC_MSG_CHECKING([whether PCRE support needs to be compiled])
if test "x${r_cv_have_pcre810}" = xyes; then
  AC_MSG_RESULT([no])
else
  AC_MSG_RESULT([yes])
fi
AM_CONDITIONAL(BUILD_PCRE, [test "x${r_cv_have_pcre810}" != xyes])
])# R_PCRE

## R_BZLIB
## -------
## Try finding bzlib library and headers.
## We check that both are installed,
## and that BZ2_bzlibVersion is in the library.
AC_DEFUN([R_BZLIB],
[if test "x${use_system_bzlib}" = xyes; then
  AC_CHECK_LIB(bz2, BZ2_bzlibVersion, [have_bzlib=yes], [have_bzlib=no])
  if test "${have_bzlib}" = yes; then
    AC_CHECK_HEADERS(bzlib.h, [have_bzlib=yes], [have_bzlib=no])
  fi
else
  have_bzlib=no
fi
if test "x${have_bzlib}" = xyes; then
AC_CACHE_CHECK([if bzip2 version >= 1.0.6], [r_cv_have_bzlib],
[AC_LANG_PUSH(C)
r_save_LIBS="${LIBS}"
LIBS="-lbz2 ${LIBS}"
AC_RUN_IFELSE([AC_LANG_SOURCE([[
#ifdef HAVE_BZLIB_H
#include <bzlib.h>
#endif
int main() {
    char *ver = BZ2_bzlibVersion();
    exit(strcmp(ver, "1.0.6") < 0);
}
]])], [r_cv_have_bzlib=yes], [r_cv_have_bzlib=no], [r_cv_have_bzlib=no])
LIBS="${r_save_LIBS}"
AC_LANG_POP(C)])
fi
if test "x${r_cv_have_bzlib}" = xno; then
  have_bzlib=no
fi
AC_MSG_CHECKING([whether bzip2 support needs to be compiled])
if test "x${have_bzlib}" = xyes; then
  AC_MSG_RESULT([no])
  LIBS="-lbz2 ${LIBS}"
else
  AC_MSG_RESULT([yes])
fi
AM_CONDITIONAL(BUILD_BZLIB, [test "x${have_bzlib}" = xno])
])# R_BZLIB

## R_TRE
## -------
## Try finding tre library and headers.
## We check that both are installed,
AC_DEFUN([R_TRE],
[if test "x${use_system_tre}" = xyes; then
  AC_CHECK_LIB(tre, tre_regncompb, [have_tre=yes], [have_tre=no])
  if test "${have_tre}" = yes; then
    AC_CHECK_HEADERS(tre/tre.h, [have_tre=yes], [have_tre=no])
  fi
if test "x${have_tre}" = xyes; then
  AC_DEFINE(HAVE_TRE, 1, [Define if your system has tre.])
  LIBS="-ltre ${LIBS}"
fi
else
  have_tre="no"
fi
AM_CONDITIONAL(BUILD_TRE, [test x${have_tre} != xyes])
])# R_TRE

## R_LZMA
## -------
## Try finding liblzma library and headers.
## We check that both are installed,
AC_DEFUN([R_LZMA],
[if test "x${use_system_xz}" = xyes; then
  AC_CHECK_LIB(lzma, lzma_version_number, [have_lzma=yes], [have_lzma=no])
  if test "${have_lzma}" = yes; then
    AC_CHECK_HEADERS(lzma.h, [have_lzma=yes], [have_lzma=no])
  fi
if test "x${have_lzma}" = xyes; then
AC_CACHE_CHECK([if lzma version >= 5.0.3], [r_cv_have_lzma],
[AC_LANG_PUSH(C)
r_save_LIBS="${LIBS}"
LIBS="-llzma ${LIBS}"
AC_RUN_IFELSE([AC_LANG_SOURCE([[
#ifdef HAVE_LZMA_H
#include <lzma.h>
#endif
#include <stdlib.h>
int main() {
    unsigned int ver = lzma_version_number();
    // This is 10000000*major + 10000*minor + 10*revision + [012]
    // I.e. xyyyzzzs and 5.1.2 would be 50010020
    exit(ver < 50000030);
}
]])], [r_cv_have_lzma=yes], [r_cv_have_lzma=no], [r_cv_have_lzma=no])
LIBS="${r_save_LIBS}"
AC_LANG_POP(C)])
fi
if test "x${r_cv_have_lzma}" = xno; then
  have_lzma=no
fi
if test "x${have_lzma}" = xyes; then
  AC_DEFINE(HAVE_LZMA, 1, [Define if your system has lzma >= 5.0.3.])
  LIBS="-llzma ${LIBS}"
fi
else
  have_lzma="no"
fi
AM_CONDITIONAL(BUILD_XZ, [test x${have_lzma} != xyes])
])# R_LZMA


## R_SYS_POSIX_LEAPSECONDS
## -----------------------
## See if your system time functions do not count leap seconds, as
## required by POSIX.
AC_DEFUN([R_SYS_POSIX_LEAPSECONDS],
[AC_CACHE_CHECK([whether leap seconds are treated according to POSIX],
                [r_cv_sys_posix_leapseconds],
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>
#include <time.h>
#include <stdio.h>
#include "confdefs.h"

int main () {
  struct tm *tm;
  time_t ct = 0; /* required on 64bit AIX */

  ctime(&ct);
  ct = ct - (ct % 60);
  tm = gmtime(&ct);
  if(tm->tm_sec == 0) exit(1); else exit(0);
}
]])],
              [r_cv_sys_posix_leapseconds=no],
              [r_cv_sys_posix_leapseconds=yes],
              [r_cv_sys_posix_leapseconds=yes])])
if test "x${r_cv_sys_posix_leapseconds}" = xyes; then
  AC_DEFINE(HAVE_POSIX_LEAPSECONDS, 1,
            [Define if your system time functions do not count leap
	     seconds, as required by POSIX.])
fi
])# R_SYS_POSIX_LEAPSECONDS

### * Miscellaneous

## R_RECOMMENDED_PACKAGES
## ----------------------
## See if the sources for (all) the recommended packages are available.
AC_DEFUN([R_RECOMMENDED_PACKAGES],
[AC_CACHE_CHECK([for recommended packages],
                [r_cv_misc_recommended_packages],
[r_cv_misc_recommended_packages=yes
recommended_pkgs=`grep '^R_PKGS_RECOMMENDED *=' \
  ${srcdir}/share/make/vars.mk | sed 's/.*=//'`
for pkg in ${recommended_pkgs}; do
  n_pkg=`ls ${srcdir}/src/library/Recommended/${pkg}_*.tar.gz | wc -l`
  if test ${n_pkg} -ne 1; then
    r_cv_misc_recommended_packages=no
    break
  fi
done])
use_recommended_packages=${r_cv_misc_recommended_packages}
if test "x${r_cv_misc_recommended_packages}" = xno; then
  AC_MSG_ERROR([Some of the recommended packages are missing
  Use --without-recommended-packages if this was intentional])
fi
])# R_RECOMMENDED_PACKAGES

## R_SIZE_MAX
## ----------
## Look for a definition of SIZE_MAX (the maximum of size_t).
## C99 has it declared in <stdint.h>, pre-C99 POSIX in <inttypes.h>, 
## glibc in <stdint.h> and Solaris 8 in <limits.h>!
## autoconf tests for inttypes.h and stdint.h by default
AC_DEFUN([R_SIZE_MAX],
[AC_CACHE_CHECK([whether SIZE_MAX is declared],
                [r_cv_size_max],
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>
#ifdef HAVE_INTTYPES_H
#  include <inttypes.h>
#endif
#ifdef HAVE_STDINT_H
#  include <stdint.h>
#endif
#ifdef HAVE_LIMITS_H
#  include <limits.h>
#endif

int
main() {
#ifndef SIZE_MAX
  char *p = (char *) SIZE_MAX;
#endif

  ;
  return 0;
}
]])],
              [r_cv_size_max=yes],
              [r_cv_size_max=no],
              [r_cv_size_max=no])])
if test "x${r_cv_size_max}" = xyes; then
  AC_DEFINE(HAVE_DECL_SIZE_MAX, 1,
            [Define to 1 if you have the declaration of `SIZE_MAX', and to 0 if you don't.])
fi
])# R_SIZE_MAX


## R_ICONV
## -------
## Look for iconv, possibly in libiconv.
## Need to include <iconv.h> as this may define iconv as a macro.
## libiconv, e.g. on MacOS X, has iconv as a macro and needs -liconv.
AC_DEFUN([R_ICONV],
[AC_CHECK_HEADERS(iconv.h)
## need to ignore cache for this as it may set LIBS
unset ac_cv_func_iconv
AC_CACHE_CHECK(for iconv, ac_cv_func_iconv, [
  ac_cv_func_iconv="no"
  AC_TRY_LINK([#include <stdlib.h>
#ifdef HAVE_ICONV_H
#include <iconv.h>
#endif],
      [iconv_t cd = iconv_open("","");
       iconv(cd,NULL,NULL,NULL,NULL);
       iconv_close(cd);],
      ac_cv_func_iconv=yes)
  if test "$ac_cv_func_iconv" != yes; then
    r_save_LIBS="$LIBS"
    LIBS="$LIBS -liconv"
    AC_TRY_LINK([#include <stdlib.h>
#ifdef HAVE_ICONV_H
#include <iconv.h>
#endif],
        [iconv_t cd = iconv_open("","");
         iconv(cd,NULL,NULL,NULL,NULL);
         iconv_close(cd);],
        ac_cv_func_iconv="in libiconv")
      if test "$ac_cv_func_iconv" = no; then
        LIBS="$r_save_LIBS"
      fi
  fi
])
if test "$ac_cv_func_iconv" != no; then
  AC_DEFINE(HAVE_ICONV, 1, [Define if you have the `iconv' function.])

  AC_CACHE_CHECK([whether iconv accepts "UTF-8", "latin1", "ASCII" and "UCS-*"],
  [r_cv_iconv_latin1],
  [AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include "confdefs.h"
#include <stdlib.h>
#ifdef HAVE_ICONV_H
#include <iconv.h>
#endif

int main () {
  iconv_t cd;
  cd = iconv_open("latin1","UTF-8");
  if(cd == (iconv_t)(-1)) exit(1);
  iconv_close(cd);
  cd = iconv_open("UTF-8","latin1");
  if(cd == (iconv_t)(-1)) exit(1);
  iconv_close(cd);
  cd = iconv_open("","latin1");
  if(cd == (iconv_t)(-1)) exit(1);
  iconv_close(cd);
  cd = iconv_open("","UTF-8");
  if(cd == (iconv_t)(-1)) exit(1);
  iconv_close(cd);
  cd = iconv_open("latin1", "");
  if(cd == (iconv_t)(-1)) exit(1);
  iconv_close(cd);
  cd = iconv_open("UTF-8","");
  if(cd == (iconv_t)(-1)) exit(1);
  iconv_close(cd);
  cd = iconv_open("ASCII","");
  if(cd == (iconv_t)(-1)) exit(1);
  iconv_close(cd);
  cd = iconv_open("UCS-2LE","");
  if(cd == (iconv_t)(-1)) exit(1);
  iconv_close(cd);
  cd = iconv_open("", "UCS-2LE");
  if(cd == (iconv_t)(-1)) exit(1);
  iconv_close(cd);
  cd = iconv_open("UCS-2BE","");
  if(cd == (iconv_t)(-1)) exit(1);
  iconv_close(cd);
  cd = iconv_open("", "UCS-2BE");
  if(cd == (iconv_t)(-1)) exit(1);
  iconv_close(cd);
  cd = iconv_open("UCS-4LE","");
  if(cd == (iconv_t)(-1)) exit(1);
  iconv_close(cd);
  cd = iconv_open("", "UCS-4LE");
  if(cd == (iconv_t)(-1)) exit(1);
  iconv_close(cd);
  cd = iconv_open("UCS-4BE","");
  if(cd == (iconv_t)(-1)) exit(1);
  iconv_close(cd);
  cd = iconv_open("", "UCS-4BE");
  if(cd == (iconv_t)(-1)) exit(1);
  iconv_close(cd);
  exit(0);
}
  ]])], [r_cv_iconv_latin1=yes], [r_cv_iconv_latin1=no],
    [r_cv_iconv_latin1=yes])])

  ## on Windows we supply iconv ourselves
  case "${host_os}" in
    mingw*)
      r_cv_iconv_latin1=yes
      ;;
  esac
  if test "$r_cv_iconv_latin1" != yes; then
    AC_MSG_ERROR([a suitable iconv is essential])
  fi
fi
## if the iconv we are using was in libiconv we have already included -liconv
AC_CACHE_CHECK(for iconvlist, ac_cv_func_iconvlist, [
  ac_cv_func_iconvlist="no"
  AC_TRY_LINK([#include <stdlib.h>
#ifdef HAVE_ICONV_H
#include <iconv.h>
#endif
static int count_one (unsigned int namescount, char * *names, void *data)
{return 0;}],
    [iconvlist(count_one, NULL);],
      ac_cv_func_iconvlist=yes)
   ])
if test "$ac_cv_func_iconvlist" = yes; then
  AC_DEFINE(HAVE_ICONVLIST, 1, [Define if you have the `iconvlist' function.])
fi
AM_ICONV dnl from gettext.m4
])# R_ICONV


## R_MBCS
## -------------
## locales - support for MBCS and specifically UTF-8
AC_DEFUN([R_MBCS],
[
want_mbcs_support=yes
## Wide character support -- first test for headers (which are assumed in code)
AC_CHECK_HEADERS(wchar.h wctype.h)
for ac_header in wchar wctype; do
  as_ac_var=`echo "ac_cv_header_${ac_header}_h"`
  this=`eval echo '${'$as_ac_var'}'`
  if test "x$this" = xno; then
    want_mbcs_support=no
  fi
done
if test "$want_mbcs_support" = yes ; then
## Solaris 8 is missing iswblank, but we can make it from iswctype.
## These are all C99, but Cygwin lacks wcsftime & wcstod
  R_CHECK_FUNCS([mbrtowc wcrtomb wcscoll wcsftime wcstod], [#include <wchar.h>])
  R_CHECK_FUNCS([mbstowcs wcstombs], [#include <stdlib.h>])
  R_CHECK_FUNCS([wctrans iswblank wctype iswctype], 
[#include <wchar.h>
#include <wctype.h>])
  for ac_func in mbrtowc mbstowcs wcrtomb wcscoll wcstombs \
                 wctrans wctype iswctype
  do
    as_ac_var=`echo "ac_cv_have_decl_$ac_func"`
    this=`eval echo '${'$as_ac_var'}'`
    if test "x$this" = xno; then
      want_mbcs_support=no
    fi
  done
fi
## it seems IRIX once had wctrans but not wctrans_t: we check this when we
## know we have the headers and wctrans().
## Also Solaris 2.6 (very old) seems to be missing mbstate_t
if test "$want_mbcs_support" = yes ; then
  AC_CHECK_TYPES([wctrans_t, mbstate_t], , , [#include <wchar.h>
       #include <wctype.h>])
  if test $ac_cv_type_wctrans_t != yes; then
    want_mbcs_support=no
  fi
  if test $ac_cv_type_mbstate_t != yes; then
    want_mbcs_support=no
  fi
fi
if test "x${want_mbcs_support}" != xyes; then
AC_MSG_ERROR([Support for MBCS locales is required.])
fi
])# R_MBCS


## R_C99_COMPLEX
## -------------
## C99 complex
AC_DEFUN([R_C99_COMPLEX],
[AC_CHECK_HEADER(complex.h,
                 [r_c99_complex=yes],
                 [r_c99_complex=no])
if test "${r_c99_complex}" = "yes"; then
  AC_CHECK_TYPE([double complex], , [r_c99_complex=no],
                [#include <complex.h>])
fi
AC_MSG_CHECKING([whether C99 double complex is supported])
AC_MSG_RESULT([${r_c99_complex}])
dnl we are supposed to have a C99 compiler, so fail at this point.
if test "${r_c99_complex}" = "no"; then
  AC_MSG_ERROR([Support for C99 double complex type is required.])
fi
R_CHECK_FUNCS([cabs carg cexp clog csqrt cpow ccos csin ctan \
	       cacos casin catan ccosh csinh ctanh],
              [#include <complex.h>])
])# R_COMPLEX

## R_CHECK_DECL(SYMBOL,
##              [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND],
##              [INCLUDES = DEFAULT-INCLUDES])
## -------------------------------------------------------
## Check if SYMBOL (a variable or a function) is declared.
AC_DEFUN([R_CHECK_DECL],
[AS_VAR_PUSHDEF([ac_Symbol], [ac_cv_have_decl_$1])dnl
AC_CACHE_CHECK([whether $1 exists and is declared], ac_Symbol,
[AC_LINK_IFELSE([AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT([$4])],
[#ifndef $1
  char *p = (char *) $1;
#endif
])],
                   [AS_VAR_SET(ac_Symbol, yes)],
                   [AS_VAR_SET(ac_Symbol, no)])])
AS_IF([test AS_VAR_GET(ac_Symbol) = yes], [$2], [$3])[]dnl
AS_VAR_POPDEF([ac_Symbol])dnl
])# R_CHECK_DECL

## R_CHECK_FUNCS(SYMBOLS,
##              [INCLUDES = DEFAULT-INCLUDES])
## --------------------------------------------------------
## Defines HAVE_SYMBOL if declared.  SYMBOLS is an m4 list.
AC_DEFUN([R_CHECK_FUNCS],
[AC_FOREACH([AC_Func], [$1],
  [AH_TEMPLATE(AS_TR_CPP(HAVE_[]AC_Func),
               [Define to 1 if you have the `]AC_Func[' function.])])dnl
for ac_func in $1
do
R_CHECK_DECL($ac_func,
             [AC_DEFINE_UNQUOTED(AS_TR_CPP([HAVE_$ac_func]), 1)], , [$2])
done
])# R_CHECK_FUNCS

## R_GCC4_VISIBILITY
## Sets up suitable macros for visibility attributes in gcc4/gfortran
AC_DEFUN([R_GCC4_VISIBILITY],
[AC_CACHE_CHECK([whether __attribute__((visibility())) is supported],
                [r_cv_visibility_attribute],
[cat > conftest.c <<EOF
int foo __attribute__ ((visibility ("hidden"))) = 1;
int bar __attribute__ ((visibility ("default"))) = 1;
EOF
r_cv_visibility_attribute=no
if AC_TRY_COMMAND(${CC-cc} -Werror -S conftest.c -o conftest.s 1>&AS_MESSAGE_LOG_FD); then
 if grep '\.hidden.*foo' conftest.s >/dev/null; then
    r_cv_visibility_attribute=yes
 fi
fi
rm -f conftest.[cs]
])
if test $r_cv_visibility_attribute = yes; then
  AC_DEFINE(HAVE_VISIBILITY_ATTRIBUTE, 1,
           [Define to 1 if __attribute__((visibility())) is supported])
fi
## test if visibility flag is accepted: NB Solaris compilers do and ignore,
## so only make use of this if HAVE_VISIBILITY_ATTRIBUTE is true.
r_save_CFLAGS=$CFLAGS
CFLAGS="$CFLAGS -fvisibility=hidden"
AC_CACHE_CHECK(whether $CC accepts -fvisibility, r_cv_prog_cc_vis,
               [_AC_COMPILE_IFELSE([AC_LANG_PROGRAM()],
	       [r_cv_prog_cc_vis=yes], [r_cv_prog_cc_vis=no])])
CFLAGS=$r_save_CFLAGS
if test "${r_cv_prog_cc_vis}" = yes; then
  if test "${r_cv_visibility_attribute}" = yes; then
    C_VISIBILITY="-fvisibility=hidden"
  fi
fi
## Need to exclude Intel compilers, where this does not work.
## The flag is documented, and is effective but also hides
## unsatisfied references. We cannot test for GCC, as icc passes that test.
case  "${CC}" in
  ## Intel compiler: note that -c99 may have been appended
  *icc*)
    C_VISIBILITY=
    ;;
esac
AC_SUBST(C_VISIBILITY)
AC_LANG_PUSH(Fortran 77)
r_save_FFLAGS=$FFLAGS
FFLAGS="$FFLAGS -fvisibility=hidden"
AC_CACHE_CHECK(whether $F77 accepts -fvisibility, r_cv_prog_f77_vis,
               [_AC_COMPILE_IFELSE([AC_LANG_PROGRAM()],
               [r_cv_prog_f77_vis=yes], [r_cv_prog_f77_vis=no])])
FFLAGS=$r_save_FFLAGS
AC_LANG_POP(Fortran 77)
if test "${r_cv_prog_f77_vis}" = yes; then
  if test "${r_cv_visibility_attribute}" = yes; then
    F77_VISIBILITY="-fvisibility=hidden"
  fi
fi
## need to exclude Intel compilers.
case  "${F77}" in
  ## Intel compiler
  *ifc|*ifort)
    F77_VISIBILITY=
    ;;
esac
AC_SUBST(F77_VISIBILITY)
])# R_GCC4_VISIBILITY


## R_KERN_USRSTACK
## -------------
## Checks whether we can use KERN_USRSTACK sysctl to
## get the bottom of the stack (*BSD, Darwin, ...)
AC_DEFUN([R_KERN_USRSTACK],
[
  AC_CACHE_CHECK([whether KERN_USRSTACK sysctl is supported],
  [r_cv_kern_usrstack],
  [AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include "confdefs.h"
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/sysctl.h>

int main () {
  int nm[2] = {CTL_KERN, KERN_USRSTACK};
  void * base;
  size_t len = sizeof(void *);
  int r = sysctl(nm, 2, &base, &len, NULL, 0);

  exit((r==0)?0:1);
}
  ]])], [r_cv_kern_usrstack=yes], [r_cv_kern_usrstack=no],
    [r_cv_kern_usrstack=no])])

  if test $r_cv_kern_usrstack = yes; then
    AC_DEFINE(HAVE_KERN_USRSTACK, 1, [Define if KERN_USRSTACK sysctl is supported.])
  fi
])


## R_PUTENV_AS_UNSETENV
## --------------------
## On some OSes putenv can unset an environment variable via
## putenv(FOO) (glibc >= 2.2) or putenv(FOO=) (Windows)
AC_DEFUN([R_PUTENV_AS_UNSETENV],
[
  AC_CACHE_CHECK([whether putenv("FOO") can unset an environment variable],
  [r_cv_putenv_unset],
  [AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include "confdefs.h"
#include <stdlib.h>
#include <string.h>
int main()
{
    char *p;
#ifdef HAVE_PUTENV
    putenv("R_TEST=testit");
    p = getenv("R_TEST");
    if(!p) exit(10);
    if(strcmp(p, "testit")) exit(11);
    putenv("R_TEST");
    p = getenv("R_TEST");
    if(!p) exit(0);
#endif
    exit(1);
}
  ]])], [r_cv_putenv_unset=yes], [r_cv_putenv_unset=no],
    [r_cv_putenv_unset=no])])

  if test $r_cv_putenv_unset = yes; then
    AC_DEFINE(HAVE_PUTENV_UNSET, 1, [Define if putenv("FOO") can unset an environment variable])
  fi
]
[
  AC_CACHE_CHECK([whether putenv("FOO=") can unset an environment variable],
  [r_cv_putenv_unset2],
  [AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include "confdefs.h"
#include <stdlib.h>
#include <string.h>
int main()
{
    char *p;
#ifdef HAVE_PUTENV
    putenv("R_TEST=testit");
    p = getenv("R_TEST");
    if(!p) exit(10);
    if(strcmp(p, "testit")) exit(11);
    putenv("R_TEST=");
    p = getenv("R_TEST");
    if(!p) exit(0);
#endif
    exit(1);
}
  ]])], [r_cv_putenv_unset2=yes], [r_cv_putenv_unset2=no],
    [r_cv_putenv_unset2=no])])

  if test $r_cv_putenv_unset2 = yes; then
    AC_DEFINE(HAVE_PUTENV_UNSET2, 1, [Define if putenv("FOO=") can unset an environment variable])
  fi
]
)

## R_FUNC_SIGACTION
## ----------------
## Some people claim that the SA_SIGINFO flag is an extension,
## despite the clarity of POSIX markup.  One such case is Hurd.
AC_DEFUN([R_FUNC_SIGACTION],
[
  AC_CACHE_CHECK([for working sigaction],
                 [r_cv_func_sigaction_works],
                 [AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include "confdefs.h"
#include <stdlib.h>
#include <signal.h>
int main ()
{
    struct sigaction sa;
    siginfo_t si, *ip;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_ONSTACK | SA_SIGINFO;
    ip = &si;
    {
	void *addr = ip->si_addr;
	int code = ip->si_code;
    }
    exit(0);
}
]])],
               [r_cv_func_sigaction_works=yes],
               [r_cv_func_sigaction_works=no],
               [r_cv_func_sigaction_works=no])])
  if test "x${r_cv_func_sigaction_works}" = xyes; then
    AC_DEFINE(HAVE_WORKING_SIGACTION, 1,
              [Define if sigaction() is complete enough for R's usage])
  fi
])# R_FUNC_SIGACTION

## R_CROSS_COMPILING
## ---------
## check for tools necessary for cross-compiling,
## namely BUILD_CC and BUILD_R
## This macro does nothing for native builds
AC_DEFUN([R_CROSS_COMPILING],
[
if test "${cross_compiling}" = yes; then
  AC_MSG_CHECKING([for build C compiler])
  build_cc_works=no
  echo "int main(void) { return 0; }" > conftest.c
  if test -n "${BUILD_CC}" && "${BUILD_CC}" conftest.c -o conftest && ./conftest; then
      build_cc_works=yes;
  fi
  if test "${build_cc_works}" = no; then
    for prog in gcc cc; do
      if "${prog}" conftest.c -o conftest >/dev/null 2>&1 && ./conftest; then
        BUILD_CC="${prog}"; build_cc_works=yes; break
      fi
    done
  fi
  rm -rf conftest conftest.* core
  if test "${build_cc_works}" = no; then
    AC_MSG_RESULT(none)
    AC_MSG_ERROR([Build C compiler doesn't work. Set BUILD_CC to a compiler capable of creating a binary native to the build machine.])
  fi
  AC_MSG_RESULT([${BUILD_CC}])
  AC_MSG_CHECKING([for build R])
  : ${BUILD_R=R}
  if echo 'cat(R.home())'|"${BUILD_R}" --vanilla --slave >/dev/null 2>&1; then
    AC_MSG_RESULT([${BUILD_R}])
  else
    AC_MSG_RESULT(none)
    AC_MSG_ERROR([Build R doesn't work. Set BUILD_R to a native build of the same R version that you want to cross-compile.])
  fi
fi
])

## R_MKTIME_ERRNO
## --------------
## Check whether mktime sets errno
AC_DEFUN([R_MKTIME_ERRNO],
[AC_CACHE_CHECK([whether mktime sets errno], [r_cv_mktime_errno],
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>
#include <time.h>
#include <errno.h>

int main()
{
    struct tm tm;
    /* It's hard to know what is an error, since mktime is allowed to
       fix up times and there are 64-bit time_t about.
       But this works for now (yes on Solaris, no on glibc). */
    tm.tm_year = 3000; tm.tm_mon = 0; tm.tm_mday = 0;
    tm.tm_hour = 0; tm.tm_min = 0; tm.tm_sec = 0; tm.tm_isdst = -1;
    errno = 0;
    mktime(&tm);
    exit(errno == 0);
}
]])],
              [r_cv_mktime_errno=yes],
              [r_cv_mktime_errno=no],
              [r_cv_mktime_errno=no])])
if test "${r_cv_mktime_errno}" = yes; then
  AC_DEFINE(MKTIME_SETS_ERRNO,, [Define if mktime sets errno.])
fi
])# R_MKTIME_ERRNO

## R_ICU
## -----
AC_DEFUN([R_ICU],
[AC_CACHE_CHECK([for ICU], [r_cv_icu],
[r_save_LIBS="${LIBS}"
LIBS="${LIBS} -licuuc -licui18n"
AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <unicode/utypes.h>
#include <unicode/ucol.h>
#include <unicode/uloc.h>
#include <unicode/uiter.h>

#include <stdlib.h>

int main () {
    UErrorCode  status = U_ZERO_ERROR;
    UCollator *collator;
    UCharIterator aIter;

    collator = ucol_open(NULL, &status);
    if (U_FAILURE(status))  exit(1);
    /* check if ICU is complete enough */
    uiter_setUTF8(&aIter, "abc", 3);
    int result = ucol_strcollIter(collator, &aIter, &aIter, &status);
    if (U_FAILURE(status))  exit(1);
    exit(0);
}
]])],
[r_cv_icu=yes], [r_cv_icu=no], [r_cv_icu=no])
LIBS="${r_save_LIBS}"
])
if test "x${r_cv_icu}" = xyes; then
  AC_DEFINE(USE_ICU, 1, [Define to use ICU for collation.])
  LIBS="${LIBS} -licuuc -licui18n"
else
  use_ICU=no
fi
])# R_ICU

## R_ABI
## ------------
AC_DEFUN([R_ABI],
[## System type.
case "${host_os}" in
  linux*)
    R_SYSTEM_ABI="linux"
    ;;
  solaris*)    
    R_SYSTEM_ABI="solaris"
    ;;
  darwin*)
    R_SYSTEM_ABI="osx"
    ;;
  freebsd*)
    R_SYSTEM_ABI="freebsd"
    ;;
  *)
    R_SYSTEM_ABI="?"
    ;;
esac
## Compiler types
## C: AC_PROG_CC does
##   If using the GNU C compiler, set shell variable `GCC' to `yes'.
##   Alternatively, could use ac_cv_c_compiler_gnu (undocumented).
if test "${GCC}" = yes; then
  R_SYSTEM_ABI="${R_SYSTEM_ABI},gcc"
else
case "${host_os}" in
  solaris*)
  ## we assume native compilers elsewhere (e.g. for -KPIC), so do so here too.
  R_SYSTEM_ABI="${R_SYSTEM_ABI},solcc"
  ;;
  *)
  R_SYSTEM_ABI="${R_SYSTEM_ABI},?"
esac
fi
## C++: AC_PROG_CXX does
##   If using the GNU C++ compiler, set shell variable `GXX' to `yes'.
##   Alternatively, could use ac_cv_cxx_compiler_gnu (undocumented).
if test "${GXX}" = yes; then
  R_SYSTEM_ABI="${R_SYSTEM_ABI},gxx"
else
case "${host_os}" in
  solaris*)
  R_SYSTEM_ABI="${R_SYSTEM_ABI},solCC"
  ;;
  *)
  R_SYSTEM_ABI="${R_SYSTEM_ABI},?"
esac
fi
## Fortran 77: AC_PROG_F77 does
##   If using `g77' (the GNU Fortran 77 compiler), then set the shell
##   variable `G77' to `yes' (and also seems to do so for gfortran, which
##   is what we really need).
##   Alternatively, could use ac_cv_f77_compiler_gnu (undocumented).
if test "${G77}" = yes; then
  R_SYSTEM_ABI="${R_SYSTEM_ABI},gfortran"
else
case "${host_os}" in
  solaris*)
  R_SYSTEM_ABI="${R_SYSTEM_ABI},solf95"
  ;;
  *)
  R_SYSTEM_ABI="${R_SYSTEM_ABI},?"
esac
fi
## Fortran 90/95: AC_PROG_FC does not seem to set a shell variable
##   indicating the GNU Fortran 90/95 compiler.
##   Hence, need to use ac_cv_fc_compiler_gnu (undocumented).
if test "${ac_cv_fc_compiler_gnu}" = yes; then
  R_SYSTEM_ABI="${R_SYSTEM_ABI},gfortran"
else
case "${host_os}" in
  solaris*)
  R_SYSTEM_ABI="${R_SYSTEM_ABI},solf95"
  ;;
  *)
  R_SYSTEM_ABI="${R_SYSTEM_ABI},?"
esac
fi
AC_SUBST(R_SYSTEM_ABI)
]) # R_ABI

## R_FUNC_MKTIME
## ------------
AC_DEFUN([R_FUNC_MKTIME],
[AC_CACHE_CHECK([whether mktime works correctly outside 1902-2037],
                [r_cv_working_mktime],
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>
#include <time.h>

main() {
    if(sizeof(time_t) < 8) exit(1);

    struct tm tm;
    time_t res;
    putenv("TZ=Europe/London");
    tm.tm_sec = tm.tm_min = 0; tm.tm_hour = 12;
    tm.tm_mday = 1; tm.tm_mon = 0; tm.tm_year = 80; tm.tm_isdst = 0;
    res = mktime(&tm);
    if(res == (time_t)-1) exit(2);
    tm.tm_mday = 1; tm.tm_year = 01; tm.tm_isdst = 0;
    res = mktime(&tm);
    if(res == (time_t)-1) exit(3);
    tm.tm_year = 140;
    res = mktime(&tm);
    if(res != 2209032000L) exit(4);
    tm.tm_mon = 6; tm.tm_isdst = 1;
    res = mktime(&tm);
    if(res != 2224753200L) exit(5);

    exit(0);
}
]])],
              [r_cv_working_mktime=yes],
              [r_cv_working_mktime=no],
              [r_cv_working_mktime=no])])
if test "x${r_cv_working_mktime}" = xyes; then
  AC_DEFINE(HAVE_WORKING_64BIT_MKTIME, 1,
            [Define if your mktime works correctly outside 1902-2037.])
fi
])# R_FUNC_MKTIME

## R_CXX1X
## -------
## Support for C++11 and later, for use in packages.
AC_DEFUN([R_CXX1X],
[r_save_CXX="${CXX}"
r_save_CXXFLAGS="${CXXFLAGS}"

: ${CXX1X=${CXX}}
: ${CXX1XFLAGS=${CXXFLAGS}}
: ${CXX1XPICFLAGS=${CXXPICFLAGS}}

CXX="${CXX1X} ${CXX1XSTD}"
CXXFLAGS="${CXX1XFLAGS} ${CXX1XPICFLAGS}"
AC_LANG_PUSH([C++])dnl
AX_CXX_COMPILE_STDCXX_11([noext], [optional])
AC_LANG_POP([C++])dnl Seems the macro does not always get this right
CXX="${r_save_CXX}"
CXXFLAGS="${r_save_CXXFLAGS}"
if test "${HAVE_CXX11}" = "1"; then
  CXX1XSTD="${CXX1XSTD} ${switch}"
else
  CXX1X=""
  CXX1XSTD=""
  CXX1XFLAGS=""
  CXX1XPICFLAGS=""
fi

AC_SUBST(CXX1X)
AC_SUBST(CXX1XSTD)
AC_SUBST(CXX1XFLAGS)
AC_SUBST(CXX1XPICFLAGS)
if test -z "${SHLIB_CXX1XLD}"; then
  SHLIB_CXX1XLD="\$(CXX1X) \$(CXX1XSTD)"
fi
AC_SUBST(SHLIB_CXX1XLD)
: ${SHLIB_CXX1XLDFLAGS=${SHLIB_CXXLDFLAGS}}
AC_SUBST(SHLIB_CXX1XLDFLAGS)

AC_ARG_VAR([CXX1X], [C++11 compiler command])
AC_ARG_VAR([CXX1XSTD],
           [special flag for compiling and for linking C++11 code, e.g. -std=c++11])
AC_ARG_VAR([CXX1XFLAGS], [C++11 compiler flags])
AC_ARG_VAR([CXX1XPICFLAGS],
           [special flags for compiling C++11 code to be turned into a
            shared object])
AC_ARG_VAR([SHLIB_CXX1XLD],
           [command for linking shared objects which contain object
            files from the C++11 compiler])
AC_ARG_VAR([SHLIB_CXX1XLDFLAGS], [special flags used by SHLIB_CXX1XLD])
])# R_CXX1X


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
