### R.m4 -- extra macros for configuring R
###
### Copyright (C) 1998, 1999, 2000, 2001 R Core Team
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

##
## R_ARG_WITH_EXCLUSIVE
##
AC_DEFUN([R_ARG_WITH_EXCLUSIVE],
 [if test "${with_$1+set}" = set; then
    if test "${with_$2+set}" = set; then
      if test "$with_$2" = no; then
	true
      else
	$3
      fi
    fi
  fi])
##
## R_ARG_USE
##
AC_DEFUN([R_ARG_USE],
 [if test "${withval}" = no; then
    use_$1=false
  else
    use_$1=true
  fi])
##
## R_PROG_AR
##
AC_DEFUN([R_PROG_AR],
 [AC_CHECK_PROGS(AR, [${AR} ar])
  : ${ARFLAGS="rc"}
  AC_SUBST(ARFLAGS)])
##
## R_PROG_ECHO_N
##
AC_DEFUN([R_PROG_ECHO_N],
 [AC_MSG_CHECKING([whether echo can suppress newlines])
  if echo "testing\c" | grep c >/dev/null; then
    if echo -n "testing" | sed s/-n/xn/ | grep xn >/dev/null; then
      ECHO_N= ECHO_C= ECHO_T='	'
    else
      ECHO_N=-n ECHO_C= ECHO_T=
    fi
  else
    ECHO_N= ECHO_C='\c' ECHO_T=
  fi
  if test -z "${ECHO_T}"; then
    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no)
  fi
  AC_SUBST(ECHO_C)
  AC_SUBST(ECHO_N)
  AC_SUBST(ECHO_T)
 ])
##
## R_PROG_INSTALL
##
AC_DEFUN([R_PROG_INSTALL],
 [AC_REQUIRE([AC_PROG_INSTALL])
  warn_install="redefining INSTALL to be `pwd`/tools/install-sh -c"
  case "${INSTALL}" in
    [[!/]]*install-sh*)
      ## Fix a bug in older versions of autoconf---the path of the
      ## install shell script is not cached.  Could also use an absolute
      ## path in AC_CONFIG_AUX_DIR().
      INSTALL="\$\(top_srcdir\)/tools/install-sh -c"
      AC_MSG_WARN([${warn_install}])
      ;;
  esac
  case "${host}" in
    *aix*|*hpux*)
      ## installbsd on AIX does not seem to work?
      INSTALL="\$\(top_srcdir\)/tools/install-sh -c"
      AC_MSG_WARN([${warn_install}])
      ;;
  esac
 ])
##
## R_PROG_PERL
##
changequote(<<, >>)dnl
define(PERL5_CHECK,
<<
  if ${PERL} -e 'exit 1 if $]<5'
  then
    r_cv_prog_perl_v5=yes
  else
    r_cv_prog_perl_v5=no
  fi
>>)
changequote([, ]) dnl
AC_DEFUN([R_PROG_PERL],
 [AC_PATH_PROGS(PERL, [${PERL} perl])
  if test -n "${PERL}"; then
    AC_CACHE_CHECK([whether perl version is at least 5],
      r_cv_prog_perl_v5, [PERL5_CHECK()] )
  else
    PERL=false
  fi
  if test "${r_cv_prog_perl_v5}" = yes; then
    NO_PERL5=false
  else
    warn_perl5="you cannot build the object documentation system"
    AC_MSG_WARN(${warn_perl5})
    NO_PERL5=true
  fi
  AC_SUBST(NO_PERL5)
 ])
##
## R_PROG_TEXMF
##
AC_DEFUN([R_PROG_TEXMF],
 [AC_REQUIRE([R_PROG_PERL])
  AC_PATH_PROGS(DVIPS, [${DVIPS} dvips], false)
  AC_PATH_PROGS(TEX, [${TEX} tex], false)
  AC_PATH_PROGS(LATEX, [${LATEX} latex], false)
  if test -z "${ac_cv_path_LATEX}" ; then
    warn_dvi="you cannot build DVI versions of the R manuals"
    AC_MSG_WARN(${warn_dvi})
  fi
  AC_PATH_PROGS(MAKEINDEX, [${MAKEINDEX} makeindex], false)
  AC_PATH_PROGS(PDFTEX, [${PDFTEX} pdftex], false)
  AC_PATH_PROGS(PDFLATEX, [${PDFLATEX} pdflatex], false)
  if test -z "${ac_cv_path_PDFLATEX}" ; then
    warn_pdf="you cannot build PDF versions of the R manuals"
    AC_MSG_WARN(${warn_pdf})
  fi
  AC_PATH_PROGS(MAKEINFO, [${MAKEINFO} makeinfo])
  if test -n "${MAKEINFO}"; then
    AC_CACHE_CHECK([whether makeinfo version is at least 4],
      r_cv_prog_makeinfo_v4,
      [ makeinfo_version=`${MAKEINFO} --version | grep "^makeinfo" | \
          sed 's/[[^)]]*) \(.\).*/\1/'`
	if test -z "${makeinfo_version}"; then
	  r_cv_prog_makeinfo_v4=no
	elif test ${makeinfo_version} -lt 4; then
	  r_cv_prog_makeinfo_v4=no
	else
	  r_cv_prog_makeinfo_v4=yes
	fi
      ])
  fi
  if test "${r_cv_prog_makeinfo_v4}" != yes; then
    warn_info="you cannot build info versions of the R manuals"
    AC_MSG_WARN(${warn_info})
    MAKEINFO=false
  fi
  if test "${PERL}" != false; then
    INSTALL_INFO="\$(PERL) \$(top_srcdir)/tools/install-info.pl"
    AC_SUBST(INSTALL_INFO)
  else
    AC_PATH_PROGS(INSTALL_INFO, [${INSTALL_INFO} install-info], false)
  fi
  : ${R_RD4DVI="ae"}
  AC_SUBST(R_RD4DVI)
  : ${R_RD4PDF="ae,hyper"}
  AC_SUBST(R_RD4PDF)
  ])
##
## R_PROG_CC_M
##
## Test whether the C compiler accepts -M for generating dependencies
##
AC_DEFUN([R_PROG_CC_M],
  [ depend_rules_frag=Makefrag.dep
    AC_CACHE_CHECK(
      [whether ${CC} accepts -M for generating dependencies],
      r_cv_prog_cc_m,
      [ AC_LANG_SAVE
	AC_LANG_C
        echo "#include <math.h>" > conftest.${ac_ext}
	if test -n "`${CC} -M conftest.${ac_ext} 2>/dev/null \
		    | grep conftest`"; then
	  r_cv_prog_cc_m=yes
	else
	  r_cv_prog_cc_m=no
	fi
	AC_LANG_RESTORE
      ])
    if test "${r_cv_prog_cc_m}" = yes; then
      cat << \EOF > ${depend_rules_frag}
.c.d:
	@echo "making $[@] from $<"
	@$(CC) -M $(ALL_CPPFLAGS) $< | \
	  sed -e 's/^\([[^:]]*\)\.o\([[ 	]]\)*:/\1.o \1.lo\2:/' > $[@]
EOF
    else
      cat << \EOF > ${depend_rules_frag}
.c.d:
	@touch $[@]
EOF
    fi
    AC_SUBST_FILE(depend_rules_frag)
  ])
##
## R_PROG_CC_C_O_LO
##
## See whether C compiler supports -c -o FILE.lo
##
AC_DEFUN([R_PROG_CC_C_O_LO],
[ cc_o_lo_rules_frag=Makefrag.cc
  AC_CACHE_CHECK([whether ${CC} supports -c -o FILE.lo],
    r_cv_prog_cc_c_o_lo,
    [ test -d TMP || mkdir TMP
      echo "foo(){}" > conftest.c
      ac_try='${CC} ${CFLAGS} -c conftest.c -o TMP/conftest.lo 1>&AC_FD_CC'
      if AC_TRY_EVAL(ac_try) \
          && test -f TMP/conftest.lo \
          && AC_TRY_EVAL(ac_try); then
        r_cv_prog_cc_c_o_lo=yes
      else
        r_cv_prog_cc_c_o_lo=no
      fi
      rm -rf conftest* TMP
    ])
  if test "${r_cv_prog_cc_c_o_lo}" = yes; then
    cat << \EOF > ${cc_o_lo_rules_frag}
.c.lo:
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS_LO) -c $< -o $[@]
EOF
  else
    cat << \EOF > ${cc_o_lo_rules_frag}
.c.lo:
	@test -d .libs || mkdir .libs
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS_LO) -c $< -o .libs/$[*].o
	mv .libs/$[*].o $[*].lo
EOF
  fi
  AC_SUBST_FILE(cc_o_lo_rules_frag)
])
##
## R_PROG_CC_FLAG
##
## Test whether the C compiler handles a command line option
##
AC_DEFUN([R_PROG_CC_FLAG],
  [ ac_safe=`echo "$1" | sed 'y%./+-%__p_%'`
    AC_MSG_CHECKING([whether ${CC-cc} accepts $1])
    AC_CACHE_VAL(r_cv_prog_cc_flag_${ac_safe},
      [ AC_LANG_SAVE
        AC_LANG_C
	XCFLAGS="${CFLAGS}"
	CFLAGS="${CFLAGS} $1"
	AC_TRY_LINK([], [],
	  eval "r_cv_prog_cc_flag_${ac_safe}=yes",
	  eval "r_cv_prog_cc_flag_${ac_safe}=no")
	CFLAGS="${XCFLAGS}"
	AC_LANG_RESTORE
      ])
    if eval "test \"`echo '$r_cv_prog_cc_flag_'$ac_safe`\" = yes"; then
      AC_MSG_RESULT(yes)
      [$2]
    else
      AC_MSG_RESULT(no)
    fi
  ])
##
## R_PROG_CXX_FLAG
##
## Test whether the C++ compiler handles a command line option
##
AC_DEFUN([R_PROG_CXX_FLAG],
  [ ac_safe=`echo "$1" | sed 'y%./+-%__p_%'`
    AC_MSG_CHECKING([whether ${CXX-c++} accepts $1])
    AC_CACHE_VAL(r_cv_prog_cxx_flag_${ac_safe},
      [ AC_LANG_SAVE
        AC_LANG_CPLUSPLUS
	XCXXFLAGS="${CXXFLAGS}"
	CXXFLAGS="${CXXFLAGS} $1"
	AC_TRY_LINK([], [],
	  eval "r_cv_prog_cxx_flag_${ac_safe}=yes",
	  eval "r_cv_prog_cxx_flag_${ac_safe}=no")
	CXXFLAGS="${XCXXFLAGS}"
	AC_LANG_RESTORE
      ])
    if eval "test \"`echo '$r_cv_prog_cxx_flag_'$ac_safe`\" = yes"; then
      AC_MSG_RESULT(yes)
      [$2]
    else
      AC_MSG_RESULT(no)
    fi
  ])
##
## R_PROG_F77_OR_F2C
##
## Find a Fortran 77 compiler, or f2c.
##
## If we have not been forced to use a particular FORTRAN compiler, try
## to find one using one of the several common names.  The list is based
## on what the current autoconf CVS contains, and ordered by
##  1. F77, F90, F95
##  2. Good/tested native compilers, bad/untested native compilers
##  3. Wrappers around f2c go last.
##
## `fort77' and `fc' are wrappers around `f2c', `fort77' being better.
## It is believed that under HP-UX `fort77' is the name of the native
## compiler.  On some Cray systems, fort77 is a native compiler.
## cf77 and cft77 are (older) Cray F77 compilers.
## pgf77 and pgf90 are the Portland Group F77 and F90 compilers.
## xlf/xlf90/xlf95 are IBM (AIX) F77/F90/F95 compilers.
## lf95 is the Lahey-Fujitsu compiler.
## fl32 is the Microsoft Fortran "PowerStation" compiler.
## af77 is the Apogee F77 compiler for Intergraph hardware running CLIX.
## epcf90 is the "Edinburgh Portable Compiler" F90.
##
## The configure options `--with-g77', `--with-f77', or `--with-f2c'
## force g77, f77, or f2c to be used (under *exactly* these names).  It
## is also possible to use these options to specify the full path name
## of the compiler.
AC_DEFUN([R_PROG_F77_OR_F2C], [
if test -n "${FC}"; then
  F77=${FC}
  AC_MSG_RESULT([defining F77 to be ${F77}])
elif ${use_f77}; then
  if test "${with_f77}" = yes; then
    F77=f77
  else
    F77="${with_f77}"
  fi
  AC_MSG_RESULT([defining F77 to be ${F77}])
elif ${use_g77}; then
  if test "${with_g77}" = yes; then
    F77=g77
  else
    F77="${with_g77}"
  fi
  AC_MSG_RESULT([defining F77 to be ${F77}])
elif ${use_f2c}; then
  F77=
  if test "${with_f2c}" = yes; then
    F2C=f2c
  else
    F2C="${with_f2c}"
  fi
  AC_MSG_RESULT([defining F2C to be ${F2C}])
else
  F77=
  AC_CHECK_PROGS(F77, [g77 f77 xlf cf77 cft77 pgf77 fl32 af77 fort77 f90 xlf90 pgf90 epcf90 f95 xlf95 lf95 g95 fc])
  if test -z "${F77}"; then
    AC_CHECK_PROG(F2C, f2c, f2c, [])
  fi
fi
])
##
## R_PROG_F77_GNU
##
## See if ${F77-f77} is the GNU Fortran compiler
##
AC_DEFUN([R_PROG_F77_GNU],
  [ AC_CACHE_CHECK([whether ${F77-f77} is the GNU Fortran compiler],
      r_cv_prog_f77_is_g77,
      [ if ${use_g77}; then
	  r_cv_prog_f77_is_g77=yes
	else
	  foutput=`${F77-f77} -v 2>&1 | egrep "GNU F77|egcs|g77"`
	  if test -n "${foutput}"; then
	    r_cv_prog_f77_is_g77=yes
	  else
	    r_cv_prog_f77_is_g77=no
	  fi
	fi
      ])
    if test "${r_cv_prog_f77_is_g77}" = yes; then
      G77=yes
      : ${FFLAGS="-g -O2"}
    else
      G77=
    fi
  ])
##
## See if the Fortran compiler appends underscores
##
AC_DEFUN([R_PROG_F77_APPEND_UNDERSCORE],
 [AC_CACHE_CHECK([whether ${F77-f77} appends underscores],
    r_cv_prog_f77_append_underscore,
    [ cat > conftestf.f <<EOF
      subroutine try
      end
EOF
      ${F77} -c ${FFLAGS} conftestf.f 1>&AC_FD_CC 2>&AC_FD_CC
      cat > conftest.c <<EOF
main() { try_(); }
EOF
      ${CC} ${CFLAGS} ${CPPFLAGS} ${LDFLAGS} -o conftest${ac_exeext} \
	conftest.c conftestf.${ac_objext} 1>&AC_FD_CC 2>&AC_FD_CC
      if test ${?} = 0; then
	r_cv_prog_f77_append_underscore=yes
      else
	cat > conftest.c <<EOF
main() { try(); }
EOF
	${CC} ${CFLAGS} ${CPPFLAGS} ${LDFLAGS} -o conftest${ac_exeext} \
	  conftest.c conftestf.${ac_objext} 1>&AC_FD_CC 2>&AC_FD_CC
	if test ${?} = 0; then
	  r_cv_prog_f77_append_underscore=no
	fi
      fi
      rm -rf conftest conftest.* conftestf.*
      if test -z "${r_cv_prog_f77_append_underscore}"; then
	AC_MSG_ERROR([Nothing worked - cannot use FORTRAN])
      fi
    ])
  if test "${r_cv_prog_f77_append_underscore}" = yes; then
    AC_DEFINE(HAVE_F77_UNDERSCORE, 1)
  fi
])
##
## See whether Fortran and C compilers agree on int and double
##
AC_DEFUN([R_PROG_F77_CC_COMPAT],
 [AC_REQUIRE([AC_CHECK_LIBM])
  AC_MSG_CHECKING([whether ${F77-f77} and ${CC-cc} agree on int and double])
  AC_CACHE_VAL(r_cv_prog_f77_cc_compat,
    [ cat > conftestf.f <<EOF
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
      ${F77} ${FFLAGS} -c conftestf.f 1>&AC_FD_CC 2>&AC_FD_CC
      changequote(, )
      cat > conftest.c <<EOF
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
  return(res);
}
EOF
      changequote([, ])
      if ${CC-cc} ${CFLAGS} -c conftest.c 1>&AC_FD_CC 2>&AC_FD_CC; then
	## FIXME
	## This should really use MAIN_LD, and hence come after this is
	## determined.  Or maybe we can always use ${CC} eventually?
	## Also, this used to have `-lm' hardwired ...
	if ${CC-cc} ${LDFLAGS} ${MAIN_LDFLAGS} -o conftest${ac_exeext} \
	    conftest.${ac_objext} conftestf.${ac_objext} ${FLIBS} \
	    ${LIBM} 1>&AC_FD_CC 2>&AC_FD_CC; then
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
    AC_MSG_WARN([${F77-f77} and ${CC-cc} disagree on int and double])
    AC_MSG_ERROR([Maybe change CFLAGS or FFLAGS?])
  fi
])
##
## See whether Fortran and C compilers agree on double complex
##
AC_DEFUN([R_PROG_F77_CC_COMPAT_COMPLEX],
 [AC_REQUIRE([AC_CHECK_LIBM])
  AC_MSG_CHECKING([whether ${F77-f77} and ${CC-cc} agree on double complex])
  AC_CACHE_VAL(r_cv_prog_complex_compat,
    [ cat > conftestf.f <<EOF
      subroutine cftest(x)
      complex*16 x(3)
      integer i

c a few tests of constructs that are sometimes missing
      if(x(1) .eq. x(1)) i = 0
      x(1) = x(1)*x(2) + x(3)
      end
EOF
      ${F77} ${FFLAGS} -c conftestf.f 1>&AC_FD_CC 2>&AC_FD_CC
      changequote(, )
      cat > conftest.c <<EOF
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
	return 0;
    else return 1;
}
EOF
      changequote([, ])
      if ${CC-cc} ${CFLAGS} -c conftest.c 1>&AC_FD_CC 2>&AC_FD_CC; then
	## FIXME
	## This should really use MAIN_LD, and hence come after this is
	## determined.  Or maybe we can always use ${CC} eventually?
	## Also, this used to have `-lm' hardwired ...
	if ${CC-cc} ${LDFLAGS} ${MAIN_LDFLAGS} -o conftest${ac_exeext} \
	    conftest.${ac_objext} conftestf.${ac_objext} ${FLIBS} \
	    ${LIBM} 1>&AC_FD_CC 2>&AC_FD_CC; then
          output=`./conftest${ac_exeext} 2>&1`
	  if test ${?} = 0; then
	    r_cv_prog_complex_compat=yes
	  fi
	fi
      fi
    ])
  rm -rf conftest conftest.* conftestf.* core
  if test -n "${r_cv_prog_complex_compat}"; then
    AC_MSG_RESULT([yes])
    AC_DEFINE(HAVE_DOUBLE_COMPLEX)
  else
    AC_MSG_WARN([${F77-f77} and ${CC-cc} disagree on double complex])
  fi
    AC_SUBST(HAVE_DOUBLE_COMPLEX)
])
##
## See whether Fortran compiler supports -c -o FILE.lo
##
AC_DEFUN([R_PROG_F77_C_O_LO],
[AC_CACHE_CHECK([whether ${F77} supports -c -o FILE.lo],
  r_cv_prog_f77_c_o_lo,
  [ test -d TMP || mkdir TMP
    cat > conftest.f <<EOF
      program conftest
      end
EOF
    ac_try='${F77} ${FFLAGS} -c conftest.f -o TMP/conftest.lo 1>&AC_FD_CC'
    if AC_TRY_EVAL(ac_try) \
        && test -f TMP/conftest.lo \
        && AC_TRY_EVAL(ac_try); then
      r_cv_prog_f77_c_o_lo=yes
    else
      r_cv_prog_f77_c_o_lo=no
    fi
    rm -rf conftest* TMP
  ])
])
##
## R_PROG_F2C_FLIBS
##
AC_DEFUN([R_PROG_F2C_FLIBS],
 [AC_REQUIRE([AC_PROG_RANLIB])
  AC_REQUIRE([AC_CHECK_LIBM])
  AC_CACHE_VAL(r_cv_f2c_flibs,
    [## This seems to be necessary on some Linux system. -- you bet! -pd
      AC_LANG_SAVE
      AC_LANG_C
      cat > conftest.${ac_ext} << EOF
int MAIN_ () { return 0; }
int MAIN__ () { return 0; }
EOF
      if AC_TRY_EVAL(ac_compile); then
	${AR} ${ARFLAGS} libconftest.a conftest.${ac_objext} 1>&AC_FD_CC
	if test -n "${RANLIB}"; then
	  ${RANLIB} libconftest.a 1>&AC_FD_CC
	fi
      fi
      AC_DEFINE(HAVE_F77_UNDERSCORE)
      AC_LANG_RESTORE
      AC_CHECK_LIB(f2c, f_open, flibs=-lf2c, flibs=,
	[-L. -lconftest ${LIBM}])
      rm -f libconftest*
      if test -z "${flibs}"; then
	AC_CHECK_LIB(F77, d_sin, flibs=-lF77, flibs=, ${LIBM})
	if test -n "${flibs}"; then
	  AC_CHECK_LIB(I77, f_rew, flibs="${flibs} -lI77", flibs=, -lF77)
	fi
      fi
      r_cv_f2c_flibs="${flibs}"])
  FLIBS="${r_cv_f2c_flibs}"
  if test -z "${FLIBS}"; then
    warn_f2c_flibs="I found f2c but not libf2c, or libF77 and libI77"
    AC_MSG_WARN(${warn_f2c_flibs})
  else
    FLIBS="${FLIBS} ${LIBM}"
  fi])
##
## R_FUNC___SETFPUCW
##
AC_DEFUN([R_FUNC___SETFPUCW],
  [ AC_CHECK_FUNC(__setfpucw,
    [ AC_CACHE_CHECK([whether __setfpucw is needed],
	r_cv_func___setfpucw_needed,
	AC_TRY_RUN(
	  changequote(<<, >>)dnl
<<
int main () {
#include <fpu_control.h>
#if defined(_FPU_DEFAULT) && defined(_FPU_IEEE)
  return(_FPU_DEFAULT != _FPU_IEEE);
#endif
  return(0);
}
>>,
	  changequote([, ])dnl
	  r_cv_func___setfpucw_needed=no,
	  r_cv_func___setfpucw_needed=yes,
	  r_cv_func___setfpucw_needed=no))
      if test "${r_cv_func___setfpucw_needed}" = yes; then
	AC_DEFINE(NEED___SETFPUCW)
      fi
    ])
  ])
##
## R_FUNC_CALLOC
##
AC_DEFUN([R_FUNC_CALLOC],
  [ AC_CACHE_CHECK([whether calloc is broken],
      r_cv_func_calloc_broken,
      AC_TRY_RUN(
	changequote(<<, >>)dnl
	<<
#include <stdlib.h>
int main () {
  int *p = calloc(0, sizeof(int));
  return(p == 0);
}
	>>,
	changequote([, ])dnl
	r_cv_func_calloc_broken=no,
	r_cv_func_calloc_broken=yes,
	r_cv_func_calloc_broken=yes))
    if test "${r_cv_func_calloc_broken}" = yes; then
      AC_DEFINE(CALLOC_BROKEN)
    fi
  ])
##
## R_FUNC_FINITE
##
AC_DEFUN([R_FUNC_FINITE],
  [ AC_CACHE_CHECK([whether finite is broken],
      r_cv_func_finite_broken,
      AC_TRY_RUN(
	changequote(<<, >>)dnl
	<<
#include <math.h>
#include "confdefs.h"
int main () {
#ifdef HAVE_FINITE
  return(finite(1./0.) | finite(0./0.) | finite(-1./0.));
#else
  return(0);
#endif
}
	>>,
	changequote([, ])dnl
	r_cv_func_finite_broken=no,
	r_cv_func_finite_broken=yes,
	r_cv_func_finite_broken=yes))
    if test "${r_cv_func_finite_broken}" = yes; then
      AC_DEFINE(FINITE_BROKEN)
    fi
  ])
##
## R_FUNC_LOG
##
AC_DEFUN([R_FUNC_LOG],
  [ AC_CACHE_CHECK([whether log is broken],
      r_cv_func_log_broken,
      AC_TRY_RUN(
	changequote(<<, >>)dnl
	<<
#include <math.h>
#include "confdefs.h"
int main () {
#ifdef HAVE_ISNAN
  return(!(log(0.) == -1. / 0. && isnan(log(-1.))));
#else
  return(log(0.) != -1. / 0);
#endif
}
	>>,
	changequote([, ])dnl
	r_cv_func_log_broken=no,
	r_cv_func_log_broken=yes,
	r_cv_func_log_broken=yes))
    if test "${r_cv_func_log_broken}" = yes; then
      AC_DEFINE(LOG_BROKEN)
    fi
  ])
##
## R_HEADER_SETJMP
##
AC_DEFUN([R_HEADER_SETJMP],
 [AC_CACHE_CHECK([whether setjmp.h is POSIX.1 compatible], 
    r_cv_header_setjmp_posix,
    [AC_EGREP_HEADER(sigjmp_buf, setjmp.h, 
       r_cv_header_setjmp_posix=yes,
       r_cv_header_setjmp_posix=no)
     if test "${r_cv_header_setjmp_posix}" = yes; then
       AC_EGREP_HEADER(siglongjmp, setjmp.h, , r_cv_header_setjmp_posix=no)
     fi
     if test "${r_cv_header_setjmp_posix}" = yes; then
       AC_EGREP_HEADER(sigsetjmp, setjmp.h, , r_cv_header_setjmp_posix=no)
     fi])
  if test "${r_cv_header_setjmp_posix}" = yes; then
    AC_DEFINE(HAVE_POSIX_SETJMP)
  fi])
##
## R_HEADER_GLIBC2
##
AC_DEFUN([R_HEADER_GLIBC2],
 [AC_CACHE_CHECK([for GNU C library with version >= 2],
    r_cv_header_glibc2,
    AC_EGREP_CPP(yes,
      changequote(<<, >>)dnl
      <<
#include <stdio.h>
#if defined __GLIBC__ && __GLIBC__ >= 2
  yes
#endif
      >>,
      changequote([, ])dnl
      r_cv_header_glibc2=yes,
      r_cv_header_glibc2=no,
      r_cv_header_glibc2=no))
  if test "${r_cv_header_glibc2}" = yes; then
    AC_DEFINE(HAVE_GLIBC2)
  fi])
##
## R_TYPE_SOCKLEN
##
AC_DEFUN([R_TYPE_SOCKLEN], [
AC_CHECK_HEADER(sys/socket.h)
AC_MSG_CHECKING([for type of socket length])
if test "${ac_cv_header_sys_socket_h}" = yes; then
  AC_CACHE_VAL(r_cv_type_socklen, [
    for t in socklen_t size_t int; do
      AC_TRY_COMPILE([
#include <stddef.h>
#include <sys/types.h>
#include <sys/socket.h>], [
          (void)getsockopt (1, 1, 1, NULL, (${t} *)NULL)],
	[ r_cv_type_socklen=${t}; break ],
	[ r_cv_type_socklen= ])
    done])
fi
if test "x${r_cv_type_socklen}" = x; then
  AC_MSG_WARN(could not determine)
else
  AC_MSG_RESULT([${r_cv_type_socklen} *])
fi
AC_DEFINE_UNQUOTED(SOCKLEN_T, ${r_cv_type_socklen})
])
##
## R_C_IEEE_754
##
AC_DEFUN([R_C_IEEE_754],
 [AC_CHECK_FUNCS(finite isnan)
  AC_CACHE_CHECK([whether you have IEEE 754 floating-point arithmetic],
    r_cv_c_ieee_754,
    dnl FIXME: This fails is finite() or isnan() are defined as macros
    dnl rather than exist as library functions ...
    AC_EGREP_CPP(yes,
      changequote(<<, >>)dnl
      <<
#include "confdefs.h"
#if defined(HAVE_FINITE) && defined(HAVE_ISNAN)
  yes
#endif
      >>,
      changequote([, ])dnl
      r_cv_c_ieee_754=yes,
      r_cv_c_ieee_754=no,
      r_cv_c_ieee_754=no))
  if test "${r_cv_c_ieee_754}" = yes; then
    AC_DEFINE(IEEE_754)
  fi])
##
## R_C_OPTIEEE
##
AC_DEFUN([R_C_OPTIEEE],
  [ AC_CACHE_CHECK([whether C compiler needs -OPT:IEEE_NaN_inf=ON],
      r_cv_c_optieee,
      AC_TRY_RUN(
	changequote(<<, >>)dnl
	<<
#include <math.h>
#include <ieeefp.h>
int main () {
  double x = 0;
  fpsetmask(0); x = x / x; return (x != x);
}
	>>,
	changequote([, ])dnl
	r_cv_c_optieee=yes,
	r_cv_c_optieee=no,
	r_cv_c_optieee=no))
    if test "${r_cv_c_optieee}" = yes; then
      R_XTRA_CFLAGS="${R_XTRA_CFLAGS} -OPT:IEEE_NaN_inf=ON"
    fi
  ])
##
## R_GNOME
##
AC_DEFUN([R_GNOME], [ 
  if test ${want_gnome} = yes; then
    GNOME_INIT_HOOK([], cont)
    if test "${GNOMEUI_LIBS}"; then
      AM_PATH_LIBGLADE(
        [use_gnome="yes"
	  GNOME_IF_FILES="gnome-interface.glade"],
        [ warn_libglade_version="GNOME support requires libglade version >= 0.3"
	  AC_MSG_WARN(${warn_libglade_version})],
        gnome)
    fi
  fi
  if test "${use_gnome}" != yes; then
    use_gnome="no"
    GNOME_IF_FILES=
  else
    AC_DEFINE(HAVE_GNOME, 1)
  fi
  AC_SUBST(HAVE_GNOME)
  AC_SUBST(GNOME_IF_FILES)])
##
## R_BSD_NETWORKING
##
AC_DEFUN([R_BSD_NETWORKING], [
AC_CACHE_CHECK([for BSD networking],
  r_cv_bsd_networking,
  [ if test "${ac_cv_header_netdb_h}" = yes \
        && test "${ac_cv_header_netinet_in_h}" = yes \
        && test "${ac_cv_header_netinet_tcp_h}" = yes \
        && test "${ac_cv_header_sys_socket_h}" = yes \
        && (test "${ac_cv_func_connect}" = yes \
          || test "${ac_cv_lib_socket_connect}" = yes) \
        && (test "${ac_cv_func_gethostbyname}" = yes \
          || test "${ac_cv_lib_nsl_gethostbyname}" = yes); then
      r_cv_bsd_networking=yes
    else
      r_cv_bsd_networking=no
    fi])
if test "${r_cv_bsd_networking}" = yes; then
  AC_DEFINE(HAVE_BSD_NETWORKING)
  AC_DEFINE(HAVE_SOCKETS)
  AC_DEFINE(HAVE_INTERNET)
  AC_DEFINE(SUPPORT_LIBXML)
fi
])
##
## R_BITMAPS
##
AC_DEFUN([R_BITMAPS], [
  BITMAP_LIBS=
  AC_CHECK_HEADER(jpeglib.h, [
    AC_MSG_CHECKING([if jpeglib version >= 6b])
    AC_EGREP_CPP(yes, 
      [
#include "confdefs.h"
#include <jpeglib.h>
#if (JPEG_LIB_VERSION >= 62)
  yes
#endif], [
      AC_MSG_RESULT([yes])
      AC_CHECK_LIB(jpeg, jpeg_destroy_compress, [
        BITMAP_LIBS=-ljpeg
        AC_DEFINE(HAVE_JPEG)
      ], , ${LIBS})
    ], AC_MSG_RESULT([no]))
  ])
  AC_CHECK_LIB(z, main, [
    AC_CHECK_HEADER(png.h, [
      AC_MSG_CHECKING([if libpng version >= 1.0.5])
      AC_EGREP_CPP(yes, 
        [
#include "confdefs.h"
#include <png.h>
#if (PNG_LIBPNG_VER >= 10005)
  yes
#endif], [
        AC_MSG_RESULT([yes])
        AC_CHECK_LIB(png, png_create_write_struct, [
          BITMAP_LIBS="${BITMAP_LIBS} -lpng -lz"
	  AC_DEFINE(HAVE_PNG)
        ], , ${LIBS})
      ], AC_MSG_RESULT([no]))
    ])
  ])
  AC_SUBST(BITMAP_LIBS)])
##
## Try finding {tcl,tk}Config.sh
##
## R_TCLTK_CONFIG()
##
AC_DEFUN([R_TCLTK_CONFIG],
[libpath="${tcltk_prefix}:${LD_LIBRARY_PATH}"
libpath="${libpath}:/opt/lib:/usr/local/lib:/usr/lib:/lib"
AC_PATH_PROG(TCL_CONFIG, [${TCL_CONFIG} tclConfig.sh], , ${libpath})
AC_PATH_PROG(TK_CONFIG, [${TK_CONFIG} tkConfig.sh], , ${libpath})
if test -z "${TCLTK_CPPFLAGS}" -o -z "${TCLTK_LIBS}"; then
  ## Check whether the versions found via the *Config.sh files are at
  ## least 8; otherwise, issue a warning and turn off Tcl/Tk support.
  ## Note that in theory a system could have outdated versions of the
  ## *Config.sh scripts and yet up-to-date installations of Tcl/Tk in
  ## standard places ...
  if test -n "${TCL_CONFIG}"; then
    . ${TCL_CONFIG}
    if test ${TCL_MAJOR_VERSION} -lt 8; then
      warn_tcltk_version="Tcl/Tk support requires Tcl version >= 8"
      AC_MSG_WARN(${warn_tcltk_version})
      have_tcltk=no
    fi
  fi
  if test -n "${TK_CONFIG}" -a -z "${warn_tcltk_version}"; then
    . ${TK_CONFIG}
    if test ${TK_MAJOR_VERSION} -lt 8; then
      warn_tcltk_version="Tcl/Tk support requires Tk version >= 8"
      AC_MSG_WARN(${warn_tcltk_version})
      have_tcltk=no
    fi
  fi
fi
])
##
## Need to ensure that we can find the tcl.h and tk.h headers, which
## may be in non-standard and/or version-dependent directories, such as
## on FreeBSD systems.
##
## The logic is as follows.  If TCLTK_CPPFLAGS was specified, then we
## do not investigate any further.  Otherwise, if we still think we
## have Tcl/Tk, then first try via the corresponding *Config.sh file,
## or else try the obvious.
##
## R_TCLTK_CPPFLAGS()
##
AC_DEFUN([R_TCLTK_CPPFLAGS],
[AC_REQUIRE([R_TCLTK_CONFIG])
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
      AC_CHECK_HEADER(${TCL_PREFIX}/include/tcl${TCL_VERSION}/tcl.h,
        [TCLTK_CPPFLAGS="-I${TCL_PREFIX}/include/tcl${TCL_VERSION}"
	  found_tcl_h=yes])
      if test "${found_tcl_h}" = no; then
	AC_CHECK_HEADER(${TCL_PREFIX}/include/tcl.h,
	  [TCLTK_CPPFLAGS="-I${TCL_PREFIX}/include"
            found_tcl_h=yes])
      fi
    fi
    if test "${found_tcl_h}" = no; then
      AC_MSG_CHECKING([for tcl.h])
      AC_EGREP_CPP(yes, [
#include <tcl.h>
#if (TCL_MAJOR_VERSION >= 8)
  yes
#endif], found_tcl_h=yes, have_tcltk=no)
      AC_MSG_RESULT([${found_tcl_h}])
    fi
    unset found_tcl_h
  fi
  if test "${have_tcltk}" = yes; then
    ## Part 2.  Check for tk.h.
    found_tk_h=no
    if test -n "${TK_CONFIG}"; then
      . ${TK_CONFIG}
      ## Look for tk.h in
      ##   ${TK_PREFIX}/include/tk${TK_VERSION}
      ##   ${TK_PREFIX}/include
      ## As the AC_CHECK_HEADER test tries including the header file and
      ## tk.h includes tcl.h and X11/Xlib.h, we need to change CPPFLAGS
      ## for the check.
      save_CPPFLAGS="${CPPFLAGS}"
      CPPFLAGS="${CPPFLAGS} ${TK_XINCLUDES} ${TCLTK_CPPFLAGS}"
      AC_CHECK_HEADER(${TK_PREFIX}/include/tk${TK_VERSION}/tk.h,
        [TCLTK_CPPFLAGS="${TCLTK_CPPFLAGS} -I${TK_PREFIX}/include/tk${TK_VERSION}"
	  found_tk_h=yes])
      if test "${found_tk_h}" = no; then
	AC_CHECK_HEADER(${TK_PREFIX}/include/tk.h,
          [TCLTK_CPPFLAGS="${TCLTK_CPPFLAGS} -I${TK_PREFIX}/include"
            found_tk_h=yes])
      fi
      CPPFLAGS="${save_CPPFLAGS}"
    fi
    if test "${found_tk_h}" = no; then
      AC_MSG_CHECKING([for tk.h])
      AC_EGREP_CPP(yes, [
#include <tk.h>
#if (TK_MAJOR_VERSION >= 8)
  yes
#endif], found_tk_h=yes, have_tcltk=no)
      AC_MSG_RESULT([${found_tk_h}])
    fi
    unset found_tk_h
  fi
fi])
##
## Find the tcl and tk libraries.
##
## R_TCLTK_LIBS()
##
AC_DEFUN([R_TCLTK_LIBS],
[AC_REQUIRE([AC_PATH_XTRA])
AC_REQUIRE([R_TCLTK_CONFIG])
if test -z "${TCLTK_LIBS}"; then
  ## We have to do the work.
  if test "${have_tcltk}" = yes; then
    ## Part 1.  Try finding the tcl library.
    if test -n "${TCL_CONFIG}"; then
      . ${TCL_CONFIG}
      TCLTK_LIBS="${TCL_LIB_SPEC}"
    else
      AC_CHECK_LIB(tcl, Tcl_CreateInterp,
        TCLTK_LIBS=-ltcl,
        have_tcltk=no)
    fi
  fi
  if test "${have_tcltk}" = yes; then
    ## Part 2.  Try finding the tk library.
    if test -n "${TK_CONFIG}"; then
      . ${TK_CONFIG}
      TCLTK_LIBS="${TCLTK_LIBS} ${TK_LIB_SPEC} ${TK_LIBS}"
    else
      AC_CHECK_LIB(tk, Tk_Init, , , ${TCLTK_LIBS})
      if test "${ac_cv_lib_tk_Tk_Init}" = no; then
	## Grr, simple -ltk does not work.
	## But maybe we simply need to add X11 libs.
	unset ac_cv_lib_tk_Tk_Init
	AC_CHECK_LIB(tk, Tk_Init,
          [TCLTK_LIBS="${TCLTK_LIBS} -ltk ${X_LIBS}"],
	  have_tcltk=no,
          [${TCLTK_LIBS} ${X_LIBS}])
      fi
    fi
  fi
fi
])
##
## R_TCLTK()
##
AC_DEFUN([R_TCLTK],
[if test "${want_tcltk}" = yes; then
  have_tcltk=yes
  R_TCLTK_CONFIG
  R_TCLTK_CPPFLAGS  
  R_TCLTK_LIBS
else
  have_tcltk=no
  ## Just making sure.
  TCLTK_CPPFLAGS=
  TCLTK_LIBS=
fi
if test "${have_tcltk}" = yes; then
  AC_DEFINE(HAVE_TCLTK)
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
])


AC_DEFUN([R_BLAS_LIBS], [
if test "${r_cv_prog_f77_append_underscore}" = yes \
  || test -n "${F2C}"; then
  dgemm_func=dgemm_
else
  dgemm_func=dgemm
fi
if test -z "${with_blas}"; then
  with_blas=yes
fi

if test "$with_blas" = "no"; then
  BLAS_LIBS=" "
elif test "$with_blas" != "yes"; then
  # user specified a BLAS library to try on the command line
  # Safeguard against users giving the location of the lib.
  blas_lib_dir=`dirname ${with_blas}`
  if test "x${blas_lib_dir}" = x; then
    AC_CHECK_LIB($with_blas, $dgemm_func, 
                 BLAS_LIBS="-l$with_blas", , $FLIBS)
  else
    blas_lib_name=`basename ${with_blas} | sed 's/^lib\([[^.]]*\).*$/\1/'`
    AC_CHECK_LIB($blas_lib_name, $dgemm_func,
      BLAS_LIBS="-L${blas_lib_dir} -l${blas_lib_name}", ,
      [-L${blas_lib_dir} ${FLIBS}])
  fi
fi

if test "x$BLAS_LIBS" = x; then
  # Checks for ATLAS BLAS library:
  AC_CHECK_LIB(atlas, ATL_xerbla, BLAS_LIBS="-latlas")
  if test "x$BLAS_LIBS" != x; then
    # check for other atlas libs:
    AC_CHECK_LIB(cblas, cblas_dgemm,BLAS_LIBS="-lcblas $BLAS_LIBS",,$BLAS_LIBS)
    AC_CHECK_LIB(f77blas, $dgemm_func, 
		 BLAS_LIBS="-lf77blas $BLAS_LIBS", , $BLAS_LIBS $FLIBS)
  fi
fi

# if test "x$BLAS_LIBS" = x; then
#   # BLAS in Alpha CXML library?
#   AC_CHECK_LIB(cxml, $dgemm_func, BLAS_LIBS="-lcxml", , $FLIBS)
# fi

# if test "x$BLAS_LIBS" = x; then
#   # BLAS in Alpha DXML library? (now called CXML, see above)
#   AC_CHECK_LIB(dxml, $dgemm_func, BLAS_LIBS="-ldxml", , $FLIBS)
# fi

if test "x$BLAS_LIBS" = x; then
  if test "x$GCC" != xyes; then
    # Check for BLAS in Sun Performance library:
    AC_CHECK_LIB(sunmath, acosp,
                 AC_CHECK_LIB(sunperf, $dgemm_func,
			      BLAS_LIBS="-xlic_lib=sunperf -lsunmath", ,
			      [-lsunmath $FLIBS]))
  fi
fi

if test "x$BLAS_LIBS" = x; then
  # Check for BLAS in SCSL and SGIMATH libraries (prefer SCSL):
  AC_CHECK_LIB(scs, $dgemm_func,
               BLAS_LIBS="-lscs", 
	       AC_CHECK_LIB(complib.sgimath, $dgemm_func,
			    BLAS_LIBS="-lcomplib.sgimath", , $FLIBS), $FLIBS)
fi

if test "x$BLAS_LIBS" = x; then
  # Checks for BLAS in IBM ESSL library.  We must also link
  # with -lblas in this case (ESSL does not include the full BLAS):
  AC_CHECK_LIB(blas, zherk, 
	       AC_CHECK_LIB(essl, $dgemm_func, 
			    BLAS_LIBS="-lessl -lblas", , $FLIBS), , $FLIBS)
fi

if test "x$BLAS_LIBS" = x; then
  # Finally, check for the generic BLAS library:
  AC_CHECK_LIB(blas, $dgemm_func, BLAS_LIBS="-lblas", , $FLIBS)
fi

if test "$with_blas" = "no"; then
  # Unset BLAS_LIBS so that we know below that nothing was found.
  BLAS_LIBS=""
fi

AC_SUBST(BLAS_LIBS)
])

##
## Try finding zlib library and headers
##
## R_ZLIB()
##
AC_DEFUN([R_ZLIB], [
  have_zlib=no
  AC_CHECK_LIB(z, main, [
    AC_CHECK_HEADER(zlib.h, [
      AC_MSG_CHECKING([if zlib version >= 1.1.3])
      AC_TRY_RUN([
#include "confdefs.h"
#include <string.h>
#include <zlib.h>
int main() {
#ifdef ZLIB_VERSION
  return(strcmp(ZLIB_VERSION, "1.1.3") < 0);
#else
  return(1);
#endif
}],     [AC_MSG_RESULT([yes])
          have_zlib=yes],
        AC_MSG_RESULT([no]),
        AC_MSG_RESULT([no]))
    ])
  ])
  if test "${have_zlib}" = yes; then
    AC_DEFINE(HAVE_ZLIB)
  fi
])
##
## See if leap seconds are used.
##
AC_DEFUN([R_USES_LEAPSECONDS],
 [AC_MSG_CHECKING([whether leap seconds are counted])
  AC_CACHE_VAL(r_cv_uses_leapseconds,
    [ cat > conftest.c <<EOF
#include <time.h>
#include <stdio.h>
#include "confdefs.h"

int main () {
  struct tm *tm;
  time_t ct;

  ctime(&ct);
  ct = ct - (ct % 60);
  tm = gmtime(&ct);
  printf("%d", tm->tm_sec);
  exit(0);
}
EOF
      if ${CC-cc} ${CFLAGS} -o conftest${ac_exeext} conftest.c; then
          output=`./conftest${ac_exeext}`
	  if test ${?} -eq 0; then
            if test ${output} -gt 0; then
	      r_cv_uses_leapseconds=yes
            fi
	  fi
      fi
    ])
  rm -rf conftest conftest.* core
  if test -n "${r_cv_uses_leapseconds}"; then
    AC_MSG_RESULT([yes])
    AC_DEFINE(USING_LEAPSECONDS, 1)
  else
    AC_MSG_RESULT([no])
  fi
])
