dnl aclocal.m4 generated automatically by aclocal 1.4-p6

dnl Copyright (C) 1994, 1995-8, 1999, 2001 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY, to the extent permitted by law; without
dnl even the implied warranty of MERCHANTABILITY or FITNESS FOR A
dnl PARTICULAR PURPOSE.



AC_DEFUN([R_ARG_USE],
[if test "${withval}" = no; then
  use_$1=no
else
  use_$1=yes
fi
])# R_ARG_USE


AC_DEFUN([R_PROG_AR],
[AC_CHECK_PROGS(AR, [${AR} ar])
: ${ARFLAGS="rc"}
AC_SUBST(ARFLAGS)
])# R_PROG_AR

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

AC_DEFUN([R_PROG_PAGER],
[AC_PATH_PROGS(PAGER, [${PAGER} less more page pg], false)
if test "${PAGER}" = false; then
  warn_pager="I could not determine a pager"
  AC_MSG_WARN([${warn_pager}])
fi
])# R_PROG_PAGER

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

AC_DEFUN([_R_PROG_PERL_VERSION],
[AC_CACHE_CHECK([whether perl version is at least 5.005],
                [r_cv_prog_perl_v5],
[if ${PERL} -e 'require 5.005 or exit 1'; then
  r_cv_prog_perl_v5=yes
else
  r_cv_prog_perl_v5=no
fi])
])# _R_PROG_PERL_VERSION

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

AC_DEFUN([R_PROG_MAKEINFO],
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

AC_DEFUN([R_PROG_BROWSER],
[AC_PATH_PROGS(R_BROWSER, [netscape mozilla galeon kfmclient opera gnome-moz-remote open])
if test -z "${R_BROWSER}"; then
  warn_browser="I could not determine a browser"
  AC_MSG_WARN([${warn_browser}])
fi
AC_SUBST(R_BROWSER)
])# R_BROWSER


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

AC_DEFUN([R_PROG_CC_M],
[AC_CACHE_CHECK([whether ${CC} accepts -M for generating dependencies],
                [r_cv_prog_cc_m],
[echo "#include <math.h>" > conftest.c
if test -n "`${CC} -M conftest.c 2>/dev/null | grep conftest`"; then
  r_cv_prog_cc_m=yes
else
  r_cv_prog_cc_m=no
fi])
])# R_PROG_CC_M

AC_DEFUN([R_PROG_CC_C_O_LO],
[AC_CACHE_CHECK([whether ${CC} supports -c -o FILE.lo],
                [r_cv_prog_cc_c_o_lo],
[test -d TMP || mkdir TMP
echo "int some_variable = 0;" > conftest.c
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

AC_DEFUN([R_PROG_CC_FLAG_D__NO_MATH_INLINES],
[AC_EGREP_CPP([yes],
[#include <math.h>
#if defined(__GLIBC__)
  yes
#endif
],
              [R_XTRA_CFLAGS="${R_XTRA_CFLAGS} -D__NO_MATH_INLINES"])
])# R_PROG_CC_FLAG_D__NO_MATH_INLINES

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


AC_DEFUN([R_PROG_CXX_M],
[AC_REQUIRE([R_PROG_CC_M])
AC_CACHE_CHECK([whether ${CXX} accepts -M for generating dependencies],
               [r_cv_prog_cxx_m],
[echo "#include <math.h>" > conftest.cc
if test -n "`${CXX} -M conftest.cc 2>/dev/null | grep conftest`"; then
  r_cv_prog_cxx_m=yes
else
  r_cv_prog_cxx_m=no
fi])
])# R_PROG_CXX_M

AC_DEFUN([R_PROG_CXX_C_O_LO],
[cxx_o_lo_rules_frag=Makefrag.cxx
AC_CACHE_CHECK([whether ${CXX} supports -c -o FILE.lo],
               [r_cv_prog_cxx_c_o_lo],
[test -d TMP || mkdir TMP
echo "int some_variable = 0;" > conftest.cc
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

AC_DEFUN([R_PROG_F77_FLIBS],
[AC_REQUIRE([AC_F77_LIBRARY_LDFLAGS])
flibs=
if test "${GCC}" = yes; then
  linker_option="-Wl,"
else
  linker_option=
fi
for arg in ${FLIBS}; do
  case "${arg}" in
    -lcrt*.o)
      ;;
    -l:*)
      flibs="${flibs} ${linker_option}${arg}"
      ;;
    *)
      flibs="${flibs} ${arg}"
      ;;
  esac
done
FLIBS="${flibs}"
if test "${G77}" = yes; then
  r_save_LIBS="${LIBS}"
  flibs=`echo "${FLIBS}" | sed 's/-lg2c/-lg2c-pic/'`
  LIBS="${LIBS} ${flibs}"
  AC_LANG_PUSH(C)
  AC_TRY_LINK([], [], [FLIBS="${flibs}"])
  AC_LANG_POP(C)
  LIBS="${r_save_LIBS}"
fi
])# R_PROG_F77_FLIBS

AC_DEFUN([R_PROG_F77_APPEND_UNDERSCORE],
[AC_REQUIRE([AC_F77_WRAPPERS])
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

AC_DEFUN([R_PROG_F2C_FLIBS],
[AC_REQUIRE([AC_PROG_RANLIB])
AC_REQUIRE([AC_CHECK_LIBM])
AC_CACHE_VAL([r_cv_f2c_flibs],
[
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

if test "${acx_blas_ok}" = no; then
  if test "x${BLAS_LIBS}" != x; then
    save_LIBS="${LIBS}"; LIBS="${BLAS_LIBS} ${LIBS}"
    AC_MSG_CHECKING([for ${sgemm} in ${BLAS_LIBS}])
    AC_TRY_LINK_FUNC(${sgemm}, [acx_blas_ok=yes], [BLAS_LIBS=""])
    AC_MSG_RESULT(${acx_blas_ok})
    LIBS="$save_LIBS"
  fi
fi

if test "${acx_blas_ok}" = no; then
  AC_CHECK_FUNC(${sgemm}, [acx_blas_ok=yes])
fi

if test "${acx_blas_ok}" = no; then
  AC_CHECK_LIB(atlas, ATL_xerbla,
               [AC_CHECK_LIB(f77blas, ${sgemm},
                             [acx_blas_ok=yes
                              BLAS_LIBS="-lf77blas -latlas"],
			     [], [-latlas])])
fi

if test "${acx_blas_ok}" = no; then
  AC_CHECK_LIB(blas, ${sgemm},
	       [AC_CHECK_LIB(dgemm, $dgemm,
		             [AC_CHECK_LIB(sgemm, ${sgemm},
			                   [acx_blas_ok=yes
                                            BLAS_LIBS="-lsgemm -ldgemm -lblas"],
			                   [], [-lblas])],
			     [], [-lblas])])
fi

  

if test "${acx_blas_ok}" = no; then
  if test "x$GCC" != xyes; then # only works with Sun CC
    AC_CHECK_LIB(sunmath, acosp,
                 [AC_CHECK_LIB(sunperf, ${sgemm},
                               [BLAS_LIBS="-xlic_lib=sunperf -lsunmath"
                                acx_blas_ok=yes],
                               [], [-lsunmath])])
  fi
fi

 

if test "${acx_blas_ok}" = no; then
  AC_CHECK_LIB(blas, ${sgemm},
	       [AC_CHECK_LIB(essl, ${sgemm},
			     [acx_blas_ok=yes
                              BLAS_LIBS="-lessl -lblas"],
			     [], [-lblas ${FLIBS}])])
fi

if test "${acx_blas_ok}" = no; then
  AC_CHECK_LIB(blas, ${sgemm},
               [acx_blas_ok=yes; BLAS_LIBS="-lblas"])
fi

LIBS="${acx_blas_save_LIBS}"

AC_SUBST(BLAS_LIBS)
])# R_BLAS_LIBS

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

dnl
dnl GNOME_GNORBA_HOOK (script-if-gnorba-found, failflag)
dnl
dnl if failflag is "failure" it aborts if gnorba is not found.
dnl

AC_DEFUN([GNOME_GNORBA_HOOK],[
	GNOME_ORBIT_HOOK([],$2)
	AC_CACHE_CHECK([for gnorba libraries],gnome_cv_gnorba_found,[
		gnome_cv_gnorba_found=no
		if test x$gnome_cv_orbit_found = xyes; then
			GNORBA_CFLAGS="`gnome-config --cflags gnorba gnomeui`"
			GNORBA_LIBS="`gnome-config --libs gnorba gnomeui`"
			if test -n "$GNORBA_LIBS"; then
				gnome_cv_gnorba_found=yes
			fi
		fi
	])
	AM_CONDITIONAL(HAVE_GNORBA, test x$gnome_cv_gnorba_found = xyes)
	if test x$gnome_cv_orbit_found = xyes; then
		$1
		GNORBA_CFLAGS="`gnome-config --cflags gnorba gnomeui`"
		GNORBA_LIBS="`gnome-config --libs gnorba gnomeui`"
		AC_SUBST(GNORBA_CFLAGS)
		AC_SUBST(GNORBA_LIBS)
	else
	    	if test x$2 = xfailure; then
			AC_MSG_ERROR(gnorba library not installed or installation problem)
	    	fi
	fi
])

AC_DEFUN([GNOME_GNORBA_CHECK], [
	GNOME_GNORBA_HOOK([],failure)
])
dnl
dnl GNOME_ORBIT_HOOK (script-if-orbit-found, failflag)
dnl
dnl if failflag is "failure" it aborts if orbit is not found.
dnl

AC_DEFUN([GNOME_ORBIT_HOOK],[
	AC_PATH_PROG(ORBIT_CONFIG,orbit-config,no)
	AC_PATH_PROG(ORBIT_IDL,orbit-idl,no)
	AC_CACHE_CHECK([for working ORBit environment],gnome_cv_orbit_found,[
		if test x$ORBIT_CONFIG = xno -o x$ORBIT_IDL = xno; then
			gnome_cv_orbit_found=no
		else
			gnome_cv_orbit_found=yes
		fi
	])
	AM_CONDITIONAL(HAVE_ORBIT, test x$gnome_cv_orbit_found = xyes)
	if test x$gnome_cv_orbit_found = xyes; then
		$1
		ORBIT_CFLAGS=`orbit-config --cflags client server`
		ORBIT_LIBS=`orbit-config --use-service=name --libs client server`
		AC_SUBST(ORBIT_CFLAGS)
		AC_SUBST(ORBIT_LIBS)
	else
    		if test x$2 = xfailure; then
			AC_MSG_ERROR(ORBit not installed or installation problem)
    		fi
	fi
])

AC_DEFUN([GNOME_ORBIT_CHECK], [
	GNOME_ORBIT_HOOK([],failure)
])
dnl
dnl GNOME_INIT_HOOK (script-if-gnome-enabled, [failflag], [additional-inits])
dnl
dnl if failflag is "fail" then GNOME_INIT_HOOK will abort if gnomeConf.sh
dnl is not found. 
dnl

AC_DEFUN([GNOME_INIT_HOOK],[
	AC_SUBST(GNOME_LIBS)
	AC_SUBST(GNOMEUI_LIBS)
	AC_SUBST(GNOMEGNORBA_LIBS)
	AC_SUBST(GTKXMHTML_LIBS)
	AC_SUBST(ZVT_LIBS)
	AC_SUBST(GNOME_LIBDIR)
	AC_SUBST(GNOME_INCLUDEDIR)

	AC_ARG_WITH(gnome-includes,
        [  --with-gnome-includes=DIR
                          specify location of GNOME headers []], [
	CPPFLAGS="$CPPFLAGS -I$withval"
	])
	
	AC_ARG_WITH(gnome-libs,
        [  --with-gnome-libs=DIR   specify location of GNOME libs []], [
	LIBS="$LIBS -L$withval"
	gnome_prefix=$withval
	])

	if test "x$want_gnome" = xyes; then

	    AC_PATH_PROG(GNOME_CONFIG,gnome-config,no)
	    if test "$GNOME_CONFIG" = "no"; then
	      no_gnome_config="yes"
	    else
	      AC_MSG_CHECKING(if $GNOME_CONFIG works)
	      if $GNOME_CONFIG --libs-only-l gnome >/dev/null 2>&1; then
	        AC_MSG_RESULT(yes)
	        GNOME_GNORBA_HOOK([],$2)
	        GNOME_LIBS="`$GNOME_CONFIG --libs-only-l gnome`"
	        GNOMEUI_LIBS="`$GNOME_CONFIG --libs-only-l gnomeui`"
	        GNOMEGNORBA_LIBS="`$GNOME_CONFIG --libs-only-l gnorba gnomeui`"
	        GTKXMHTML_LIBS="`$GNOME_CONFIG --libs-only-l gtkxmhtml`"
		ZVT_LIBS="`$GNOME_CONFIG --libs-only-l zvt`"
	        GNOME_LIBDIR="`$GNOME_CONFIG --libs-only-L gnorba gnomeui`"
	        GNOME_INCLUDEDIR="`$GNOME_CONFIG --cflags gnorba gnomeui`"
                $1
	      else
	        AC_MSG_RESULT(no)
	        no_gnome_config="yes"
              fi
            fi

	    if test x$gnome_prefix = x; then
	      if test x$exec_prefix = xNONE; then
	        if test x$prefix = xNONE; then
		    gnome_prefix=$ac_default_prefix/lib
	        else
 		    gnome_prefix=$prefix/lib
	        fi
	      else
	        gnome_prefix=`eval echo \`echo $libdir\``
	      fi
	    fi
	
	    if test "$no_gnome_config" = "yes"; then
              AC_MSG_CHECKING(for gnomeConf.sh file in $gnome_prefix)
	      if test -f $gnome_prefix/gnomeConf.sh; then
	        AC_MSG_RESULT(found)
	        echo "loading gnome configuration from" \
		     "$gnome_prefix/gnomeConf.sh"
	        . $gnome_prefix/gnomeConf.sh
	        $1
	      else
	        AC_MSG_RESULT(not found)
 	        if test x$2 = xfail; then
	          AC_MSG_ERROR(Could not find the gnomeConf.sh file that is generated by gnome-libs install)
 	        fi
	      fi
            fi
	fi

	if test -n "$3"; then
	  n="$3"
	  for i in $n; do
	    AC_MSG_CHECKING(extra library \"$i\")
	    case $i in 
	      applets)
		AC_SUBST(GNOME_APPLETS_LIBS)
		GNOME_APPLETS_LIBS=`$GNOME_CONFIG --libs-only-l applets`
		AC_MSG_RESULT($GNOME_APPLETS_LIBS);;
	      capplet)
		AC_SUBST(GNOME_CAPPLET_LIBS)
		GNOME_CAPPLET_LIBS=`$GNOME_CONFIG --libs-only-l capplet`
		AC_MSG_RESULT($GNOME_CAPPLET_LIBS);;
	      *)
		AC_MSG_RESULT(unknown library)
	    esac
	  done
	fi
])

dnl
dnl GNOME_INIT ([additional-inits])
dnl

AC_DEFUN([GNOME_INIT],[
	GNOME_INIT_HOOK([],fail,$1)
])
# a macro to get the libs/cflags for libglade
# serial 1

dnl AM_PATH_LIBGLADE([ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND [, MODULES]]])
dnl Test to see if libglade is installed, and define LIBGLADE_CFLAGS, LIBS
dnl
AC_DEFUN(AM_PATH_LIBGLADE,
[dnl
dnl Get the cflags and libraries from the libglade-config script
dnl
AC_ARG_WITH(libglade-config,
[  --with-libglade-config=LIBGLADE_CONFIG
                          specify location of libglade-config []],
LIBGLADE_CONFIG="$withval")

module_args=
for module in . $3; do
  case "$module" in
    gnome)
      module_args="$module_args gnome"
      ;;
    bonobo)
      module_args="$module_args bonobo"
      ;;
    gnomedb)
      module_args="$module_args gnomedb"
      ;;
  esac
done

AC_PATH_PROG(LIBGLADE_CONFIG, libglade-config, no)
AC_MSG_CHECKING(for libglade)
if test "$LIBGLADE_CONFIG" = "no"; then
  AC_MSG_RESULT(no)
  ifelse([$2], , :, [$2])
else
  if $LIBGLADE_CONFIG --check $module_args; then
    LIBGLADE_CFLAGS=`$LIBGLADE_CONFIG --cflags $module_args`
    LIBGLADE_LIBS=`$LIBGLADE_CONFIG --libs $module_args`
    AC_MSG_RESULT(yes)
    ifelse([$1], , :, [$1])
  else
    echo "*** libglade was not compiled with support for $module_args" 1>&2
    AC_MSG_RESULT(no)
    ifelse([$2], , :, [$2])
  fi
fi
AC_SUBST(LIBGLADE_CFLAGS)
AC_SUBST(LIBGLADE_LIBS)
])
# libtool.m4 - Configure libtool for the host system. -*-Shell-script-*-

# serial 46 AC_PROG_LIBTOOL

AC_DEFUN([AC_PROG_LIBTOOL],
[AC_REQUIRE([AC_LIBTOOL_SETUP])dnl

# This can be used to rebuild libtool when needed
LIBTOOL_DEPS="$ac_aux_dir/ltmain.sh"

# Always use our own libtool.
LIBTOOL='$(SHELL) $(top_builddir)/libtool'
AC_SUBST(LIBTOOL)dnl

# Prevent multiple expansion
define([AC_PROG_LIBTOOL], [])
])

AC_DEFUN([AC_LIBTOOL_SETUP],
[AC_PREREQ(2.13)dnl
AC_REQUIRE([AC_ENABLE_SHARED])dnl
AC_REQUIRE([AC_ENABLE_STATIC])dnl
AC_REQUIRE([AC_ENABLE_FAST_INSTALL])dnl
AC_REQUIRE([AC_CANONICAL_HOST])dnl
AC_REQUIRE([AC_CANONICAL_BUILD])dnl
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_PROG_LD])dnl
AC_REQUIRE([AC_PROG_LD_RELOAD_FLAG])dnl
AC_REQUIRE([AC_PROG_NM])dnl
AC_REQUIRE([AC_PROG_LN_S])dnl
AC_REQUIRE([AC_DEPLIBS_CHECK_METHOD])dnl
AC_REQUIRE([AC_OBJEXT])dnl
AC_REQUIRE([AC_EXEEXT])dnl
dnl

_LT_AC_PROG_ECHO_BACKSLASH
# Only perform the check for file, if the check method requires it
case $deplibs_check_method in
file_magic*)
  if test "$file_magic_cmd" = '$MAGIC_CMD'; then
    AC_PATH_MAGIC
  fi
  ;;
esac

AC_CHECK_TOOL(RANLIB, ranlib, :)
AC_CHECK_TOOL(STRIP, strip, :)

ifdef([AC_PROVIDE_AC_LIBTOOL_DLOPEN], enable_dlopen=yes, enable_dlopen=no)
ifdef([AC_PROVIDE_AC_LIBTOOL_WIN32_DLL],
enable_win32_dll=yes, enable_win32_dll=no)

AC_ARG_ENABLE(libtool-lock,
  [  --disable-libtool-lock  avoid locking (might break parallel builds)])
test "x$enable_libtool_lock" != xno && enable_libtool_lock=yes

# Some flags need to be propagated to the compiler or linker for good
# libtool support.
case $host in
*-*-irix6*)
  # Find out which ABI we are using.
  echo '[#]line __oline__ "configure"' > conftest.$ac_ext
  if AC_TRY_EVAL(ac_compile); then
    case `/usr/bin/file conftest.$ac_objext` in
    *32-bit*)
      LD="${LD-ld} -32"
      ;;
    *N32*)
      LD="${LD-ld} -n32"
      ;;
    *64-bit*)
      LD="${LD-ld} -64"
      ;;
    esac
  fi
  rm -rf conftest*
  ;;

*-*-sco3.2v5*)
  # On SCO OpenServer 5, we need -belf to get full-featured binaries.
  SAVE_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -belf"
  AC_CACHE_CHECK([whether the C compiler needs -belf], lt_cv_cc_needs_belf,
    [AC_LANG_SAVE
     AC_LANG_C
     AC_TRY_LINK([],[],[lt_cv_cc_needs_belf=yes],[lt_cv_cc_needs_belf=no])
     AC_LANG_RESTORE])
  if test x"$lt_cv_cc_needs_belf" != x"yes"; then
    # this is probably gcc 2.8.0, egcs 1.0 or newer; no need for -belf
    CFLAGS="$SAVE_CFLAGS"
  fi
  ;;

ifdef([AC_PROVIDE_AC_LIBTOOL_WIN32_DLL],
[*-*-cygwin* | *-*-mingw* | *-*-pw32*)
  AC_CHECK_TOOL(DLLTOOL, dlltool, false)
  AC_CHECK_TOOL(AS, as, false)
  AC_CHECK_TOOL(OBJDUMP, objdump, false)

  # recent cygwin and mingw systems supply a stub DllMain which the user
  # can override, but on older systems we have to supply one
  AC_CACHE_CHECK([if libtool should supply DllMain function], lt_cv_need_dllmain,
    [AC_TRY_LINK([],
      [extern int __attribute__((__stdcall__)) DllMain(void*, int, void*);
      DllMain (0, 0, 0);],
      [lt_cv_need_dllmain=no],[lt_cv_need_dllmain=yes])])

  case $host/$CC in
  *-*-cygwin*/gcc*-mno-cygwin*|*-*-mingw*)
    # old mingw systems require "-dll" to link a DLL, while more recent ones
    # require "-mdll"
    SAVE_CFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS -mdll"
    AC_CACHE_CHECK([how to link DLLs], lt_cv_cc_dll_switch,
      [AC_TRY_LINK([], [], [lt_cv_cc_dll_switch=-mdll],[lt_cv_cc_dll_switch=-dll])])
    CFLAGS="$SAVE_CFLAGS" ;;
  *-*-cygwin* | *-*-pw32*)
    # cygwin systems need to pass --dll to the linker, and not link
    # crt.o which will require a WinMain@16 definition.
    lt_cv_cc_dll_switch="-Wl,--dll -nostartfiles" ;;
  esac
  ;;
  ])
esac

_LT_AC_LTCONFIG_HACK

])

# AC_LIBTOOL_HEADER_ASSERT
# ------------------------
AC_DEFUN([AC_LIBTOOL_HEADER_ASSERT],
[AC_CACHE_CHECK([whether $CC supports assert without backlinking],
    [lt_cv_func_assert_works],
    [case $host in
    *-*-solaris*)
      if test "$GCC" = yes && test "$with_gnu_ld" != yes; then
        case `$CC --version 2>/dev/null` in
        [[12]].*) lt_cv_func_assert_works=no ;;
        *)        lt_cv_func_assert_works=yes ;;
        esac
      fi
      ;;
    esac])

if test "x$lt_cv_func_assert_works" = xyes; then
  AC_CHECK_HEADERS(assert.h)
fi
])# AC_LIBTOOL_HEADER_ASSERT

# _LT_AC_CHECK_DLFCN
# --------------------
AC_DEFUN([_LT_AC_CHECK_DLFCN],
[AC_CHECK_HEADERS(dlfcn.h)
])# _LT_AC_CHECK_DLFCN

# AC_LIBTOOL_SYS_GLOBAL_SYMBOL_PIPE
# ---------------------------------
AC_DEFUN([AC_LIBTOOL_SYS_GLOBAL_SYMBOL_PIPE],
[AC_REQUIRE([AC_CANONICAL_HOST])
AC_REQUIRE([AC_PROG_NM])
AC_REQUIRE([AC_OBJEXT])
# Check for command to grab the raw symbol name followed by C symbol from nm.
AC_MSG_CHECKING([command to parse $NM output])
AC_CACHE_VAL([lt_cv_sys_global_symbol_pipe], [dnl

# These are sane defaults that work on at least a few old systems.
# [They come from Ultrix.  What could be older than Ultrix?!! ;)]

# Character class describing NM global symbol codes.
symcode='[[BCDEGRST]]'

# Regexp to match symbols that can be accessed directly from C.
sympat='\([[_A-Za-z]][[_A-Za-z0-9]]*\)'

# Transform the above into a raw symbol and a C symbol.
symxfrm='\1 \2\3 \3'

# Transform an extracted symbol line into a proper C declaration
lt_cv_global_symbol_to_cdecl="sed -n -e 's/^. .* \(.*\)$/extern char \1;/p'"

# Transform an extracted symbol line into symbol name and symbol address
lt_cv_global_symbol_to_c_name_address="sed -n -e 's/^: \([[^ ]]*\) $/  {\\\"\1\\\", (lt_ptr) 0},/p' -e 's/^$symcode \([[^ ]]*\) \([[^ ]]*\)$/  {\"\2\", (lt_ptr) \&\2},/p'"

# Define system-specific variables.
case $host_os in
aix*)
  symcode='[[BCDT]]'
  ;;
cygwin* | mingw* | pw32*)
  symcode='[[ABCDGISTW]]'
  ;;
hpux*) # Its linker distinguishes data from code symbols
  lt_cv_global_symbol_to_cdecl="sed -n -e 's/^T .* \(.*\)$/extern char \1();/p' -e 's/^$symcode* .* \(.*\)$/extern char \1;/p'"
  lt_cv_global_symbol_to_c_name_address="sed -n -e 's/^: \([[^ ]]*\) $/  {\\\"\1\\\", (lt_ptr) 0},/p' -e 's/^$symcode* \([[^ ]]*\) \([[^ ]]*\)$/  {\"\2\", (lt_ptr) \&\2},/p'"
  ;;
irix*)
  symcode='[[BCDEGRST]]'
  ;;
solaris* | sysv5*)
  symcode='[[BDT]]'
  ;;
sysv4)
  symcode='[[DFNSTU]]'
  ;;
esac

# Handle CRLF in mingw tool chain
opt_cr=
case $host_os in
mingw*)
  opt_cr=`echo 'x\{0,1\}' | tr x '\015'` # option cr in regexp
  ;;
esac

# If we're using GNU nm, then use its standard symbol codes.
if $NM -V 2>&1 | egrep '(GNU|with BFD)' > /dev/null; then
  symcode='[[ABCDGISTW]]'
fi

# Try without a prefix undercore, then with it.
for ac_symprfx in "" "_"; do

  # Write the raw and C identifiers.
lt_cv_sys_global_symbol_pipe="sed -n -e 's/^.*[[ 	]]\($symcode$symcode*\)[[ 	]][[ 	]]*\($ac_symprfx\)$sympat$opt_cr$/$symxfrm/p'"

  # Check to see that the pipe works correctly.
  pipe_works=no
  rm -f conftest*
  cat > conftest.$ac_ext <<EOF
#ifdef __cplusplus
extern "C" {
#endif
char nm_test_var;
void nm_test_func(){}
#ifdef __cplusplus
}
#endif
int main(){nm_test_var='a';nm_test_func();return(0);}
EOF

  if AC_TRY_EVAL(ac_compile); then
    # Now try to grab the symbols.
    nlist=conftest.nm
    if AC_TRY_EVAL(NM conftest.$ac_objext \| $lt_cv_sys_global_symbol_pipe \> $nlist) && test -s "$nlist"; then
      # Try sorting and uniquifying the output.
      if sort "$nlist" | uniq > "$nlist"T; then
	mv -f "$nlist"T "$nlist"
      else
	rm -f "$nlist"T
      fi

      # Make sure that we snagged all the symbols we need.
      if egrep ' nm_test_var$' "$nlist" >/dev/null; then
	if egrep ' nm_test_func$' "$nlist" >/dev/null; then
	  cat <<EOF > conftest.$ac_ext
#ifdef __cplusplus
extern "C" {
#endif

EOF
	  # Now generate the symbol file.
	  eval "$lt_cv_global_symbol_to_cdecl"' < "$nlist" >> conftest.$ac_ext'

	  cat <<EOF >> conftest.$ac_ext
#if defined (__STDC__) && __STDC__
# define lt_ptr void *
#else
# define lt_ptr char *
# define const
#endif

/* The mapping between symbol names and symbols. */
const struct {
  const char *name;
  lt_ptr address;
}
lt_preloaded_symbols[[]] =
{
EOF
	  sed "s/^$symcode$symcode* \(.*\) \(.*\)$/  {\"\2\", (lt_ptr) \&\2},/" < "$nlist" >> conftest.$ac_ext
	  cat <<\EOF >> conftest.$ac_ext
  {0, (lt_ptr) 0}
};

#ifdef __cplusplus
}
#endif
EOF
	  # Now try linking the two files.
	  mv conftest.$ac_objext conftstm.$ac_objext
	  save_LIBS="$LIBS"
	  save_CFLAGS="$CFLAGS"
	  LIBS="conftstm.$ac_objext"
	  CFLAGS="$CFLAGS$no_builtin_flag"
	  if AC_TRY_EVAL(ac_link) && test -s conftest; then
	    pipe_works=yes
	  fi
	  LIBS="$save_LIBS"
	  CFLAGS="$save_CFLAGS"
	else
	  echo "cannot find nm_test_func in $nlist" >&AC_FD_CC
	fi
      else
	echo "cannot find nm_test_var in $nlist" >&AC_FD_CC
      fi
    else
      echo "cannot run $lt_cv_sys_global_symbol_pipe" >&AC_FD_CC
    fi
  else
    echo "$progname: failed program was:" >&AC_FD_CC
    cat conftest.$ac_ext >&5
  fi
  rm -f conftest* conftst*

  # Do not use the global_symbol_pipe unless it works.
  if test "$pipe_works" = yes; then
    break
  else
    lt_cv_sys_global_symbol_pipe=
  fi
done
])
global_symbol_pipe="$lt_cv_sys_global_symbol_pipe"
if test -z "$lt_cv_sys_global_symbol_pipe"; then
  global_symbol_to_cdecl=
  global_symbol_to_c_name_address=
else
  global_symbol_to_cdecl="$lt_cv_global_symbol_to_cdecl"
  global_symbol_to_c_name_address="$lt_cv_global_symbol_to_c_name_address"
fi
if test -z "$global_symbol_pipe$global_symbol_to_cdec$global_symbol_to_c_name_address";
then
  AC_MSG_RESULT(failed)
else
  AC_MSG_RESULT(ok)
fi
]) # AC_LIBTOOL_SYS_GLOBAL_SYMBOL_PIPE

# _LT_AC_LIBTOOL_SYS_PATH_SEPARATOR
# ---------------------------------
AC_DEFUN([_LT_AC_LIBTOOL_SYS_PATH_SEPARATOR],
[# Find the correct PATH separator.  Usually this is `:', but
# DJGPP uses `;' like DOS.
if test "X${PATH_SEPARATOR+set}" != Xset; then
  UNAME=${UNAME-`uname 2>/dev/null`}
  case X$UNAME in
    *-DOS) lt_cv_sys_path_separator=';' ;;
    *)     lt_cv_sys_path_separator=':' ;;
  esac
  PATH_SEPARATOR=$lt_cv_sys_path_separator
fi
])# _LT_AC_LIBTOOL_SYS_PATH_SEPARATOR

# _LT_AC_PROG_ECHO_BACKSLASH
# --------------------------
# Add some code to the start of the generated configure script which
# will find an echo command which doesn't interpret backslashes.
AC_DEFUN([_LT_AC_PROG_ECHO_BACKSLASH],
[ifdef([AC_DIVERSION_NOTICE], [AC_DIVERT_PUSH(AC_DIVERSION_NOTICE)],
			      [AC_DIVERT_PUSH(NOTICE)])
_LT_AC_LIBTOOL_SYS_PATH_SEPARATOR

# Check that we are running under the correct shell.
SHELL=${CONFIG_SHELL-/bin/sh}

case X$ECHO in
X*--fallback-echo)
  # Remove one level of quotation (which was required for Make).
  ECHO=`echo "$ECHO" | sed 's,\\\\\[$]\\[$]0,'[$]0','`
  ;;
esac

echo=${ECHO-echo}
if test "X[$]1" = X--no-reexec; then
  # Discard the --no-reexec flag, and continue.
  shift
elif test "X[$]1" = X--fallback-echo; then
  # Avoid inline document here, it may be left over
  :
elif test "X`($echo '\t') 2>/dev/null`" = 'X\t'; then
  # Yippee, $echo works!
  :
else
  # Restart under the correct shell.
  exec $SHELL "[$]0" --no-reexec ${1+"[$]@"}
fi

if test "X[$]1" = X--fallback-echo; then
  # used as fallback echo
  shift
  cat <<EOF
$*
EOF
  exit 0
fi

# The HP-UX ksh and POSIX shell print the target directory to stdout
# if CDPATH is set.
if test "X${CDPATH+set}" = Xset; then CDPATH=:; export CDPATH; fi

if test -z "$ECHO"; then
if test "X${echo_test_string+set}" != Xset; then
# find a string as large as possible, as long as the shell can cope with it
  for cmd in 'sed 50q "[$]0"' 'sed 20q "[$]0"' 'sed 10q "[$]0"' 'sed 2q "[$]0"' 'echo test'; do
    # expected sizes: less than 2Kb, 1Kb, 512 bytes, 16 bytes, ...
    if (echo_test_string="`eval $cmd`") 2>/dev/null &&
       echo_test_string="`eval $cmd`" &&
       (test "X$echo_test_string" = "X$echo_test_string") 2>/dev/null
    then
      break
    fi
  done
fi

if test "X`($echo '\t') 2>/dev/null`" = 'X\t' &&
   echo_testing_string=`($echo "$echo_test_string") 2>/dev/null` &&
   test "X$echo_testing_string" = "X$echo_test_string"; then
  :
else
  # The Solaris, AIX, and Digital Unix default echo programs unquote
  # backslashes.  This makes it impossible to quote backslashes using
  #   echo "$something" | sed 's/\\/\\\\/g'
  #
  # So, first we look for a working echo in the user's PATH.

  IFS="${IFS= 	}"; save_ifs="$IFS"; IFS=$PATH_SEPARATOR
  for dir in $PATH /usr/ucb; do
    if (test -f $dir/echo || test -f $dir/echo$ac_exeext) &&
       test "X`($dir/echo '\t') 2>/dev/null`" = 'X\t' &&
       echo_testing_string=`($dir/echo "$echo_test_string") 2>/dev/null` &&
       test "X$echo_testing_string" = "X$echo_test_string"; then
      echo="$dir/echo"
      break
    fi
  done
  IFS="$save_ifs"

  if test "X$echo" = Xecho; then
    # We didn't find a better echo, so look for alternatives.
    if test "X`(print -r '\t') 2>/dev/null`" = 'X\t' &&
       echo_testing_string=`(print -r "$echo_test_string") 2>/dev/null` &&
       test "X$echo_testing_string" = "X$echo_test_string"; then
      # This shell has a builtin print -r that does the trick.
      echo='print -r'
    elif (test -f /bin/ksh || test -f /bin/ksh$ac_exeext) &&
	 test "X$CONFIG_SHELL" != X/bin/ksh; then
      # If we have ksh, try running configure again with it.
      ORIGINAL_CONFIG_SHELL=${CONFIG_SHELL-/bin/sh}
      export ORIGINAL_CONFIG_SHELL
      CONFIG_SHELL=/bin/ksh
      export CONFIG_SHELL
      exec $CONFIG_SHELL "[$]0" --no-reexec ${1+"[$]@"}
    else
      # Try using printf.
      echo='printf %s\n'
      if test "X`($echo '\t') 2>/dev/null`" = 'X\t' &&
	 echo_testing_string=`($echo "$echo_test_string") 2>/dev/null` &&
	 test "X$echo_testing_string" = "X$echo_test_string"; then
	# Cool, printf works
	:
      elif echo_testing_string=`($ORIGINAL_CONFIG_SHELL "[$]0" --fallback-echo '\t') 2>/dev/null` &&
	   test "X$echo_testing_string" = 'X\t' &&
	   echo_testing_string=`($ORIGINAL_CONFIG_SHELL "[$]0" --fallback-echo "$echo_test_string") 2>/dev/null` &&
	   test "X$echo_testing_string" = "X$echo_test_string"; then
	CONFIG_SHELL=$ORIGINAL_CONFIG_SHELL
	export CONFIG_SHELL
	SHELL="$CONFIG_SHELL"
	export SHELL
	echo="$CONFIG_SHELL [$]0 --fallback-echo"
      elif echo_testing_string=`($CONFIG_SHELL "[$]0" --fallback-echo '\t') 2>/dev/null` &&
	   test "X$echo_testing_string" = 'X\t' &&
	   echo_testing_string=`($CONFIG_SHELL "[$]0" --fallback-echo "$echo_test_string") 2>/dev/null` &&
	   test "X$echo_testing_string" = "X$echo_test_string"; then
	echo="$CONFIG_SHELL [$]0 --fallback-echo"
      else
	# maybe with a smaller string...
	prev=:

	for cmd in 'echo test' 'sed 2q "[$]0"' 'sed 10q "[$]0"' 'sed 20q "[$]0"' 'sed 50q "[$]0"'; do
	  if (test "X$echo_test_string" = "X`eval $cmd`") 2>/dev/null
	  then
	    break
	  fi
	  prev="$cmd"
	done

	if test "$prev" != 'sed 50q "[$]0"'; then
	  echo_test_string=`eval $prev`
	  export echo_test_string
	  exec ${ORIGINAL_CONFIG_SHELL-${CONFIG_SHELL-/bin/sh}} "[$]0" ${1+"[$]@"}
	else
	  # Oops.  We lost completely, so just stick with echo.
	  echo=echo
	fi
      fi
    fi
  fi
fi
fi

# Copy echo and quote the copy suitably for passing to libtool from
# the Makefile, instead of quoting the original, which is used later.
ECHO=$echo
if test "X$ECHO" = "X$CONFIG_SHELL [$]0 --fallback-echo"; then
   ECHO="$CONFIG_SHELL \\\$\[$]0 --fallback-echo"
fi

AC_SUBST(ECHO)
AC_DIVERT_POP
])# _LT_AC_PROG_ECHO_BACKSLASH

# _LT_AC_TRY_DLOPEN_SELF (ACTION-IF-TRUE, ACTION-IF-TRUE-W-USCORE,
#                           ACTION-IF-FALSE, ACTION-IF-CROSS-COMPILING)
# ------------------------------------------------------------------
AC_DEFUN([_LT_AC_TRY_DLOPEN_SELF],
[if test "$cross_compiling" = yes; then :
  [$4]
else
  AC_REQUIRE([_LT_AC_CHECK_DLFCN])dnl
  lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
  lt_status=$lt_dlunknown
  cat > conftest.$ac_ext <<EOF
[#line __oline__ "configure"
#include "confdefs.h"

#if HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#include <stdio.h>

#ifdef RTLD_GLOBAL
#  define LT_DLGLOBAL		RTLD_GLOBAL
#else
#  ifdef DL_GLOBAL
#    define LT_DLGLOBAL		DL_GLOBAL
#  else
#    define LT_DLGLOBAL		0
#  endif
#endif

/* We may have to define LT_DLLAZY_OR_NOW in the command line if we
   find out it does not work in some platform. */
#ifndef LT_DLLAZY_OR_NOW
#  ifdef RTLD_LAZY
#    define LT_DLLAZY_OR_NOW		RTLD_LAZY
#  else
#    ifdef DL_LAZY
#      define LT_DLLAZY_OR_NOW		DL_LAZY
#    else
#      ifdef RTLD_NOW
#        define LT_DLLAZY_OR_NOW	RTLD_NOW
#      else
#        ifdef DL_NOW
#          define LT_DLLAZY_OR_NOW	DL_NOW
#        else
#          define LT_DLLAZY_OR_NOW	0
#        endif
#      endif
#    endif
#  endif
#endif

#ifdef __cplusplus
extern "C" void exit (int);
#endif

void fnord() { int i=42;}
int main ()
{
  void *self = dlopen (0, LT_DLGLOBAL|LT_DLLAZY_OR_NOW);
  int status = $lt_dlunknown;

  if (self)
    {
      if (dlsym (self,"fnord"))       status = $lt_dlno_uscore;
      else if (dlsym( self,"_fnord")) status = $lt_dlneed_uscore;
      /* dlclose (self); */
    }

    exit (status);
}]
EOF
  if AC_TRY_EVAL(ac_link) && test -s conftest${ac_exeext} 2>/dev/null; then
    (./conftest; exit; ) 2>/dev/null
    lt_status=$?
    case x$lt_status in
      x$lt_dlno_uscore) $1 ;;
      x$lt_dlneed_uscore) $2 ;;
      x$lt_unknown|x*) $3 ;;
    esac
  else :
    # compilation failed
    $3
  fi
fi
rm -fr conftest*
])# _LT_AC_TRY_DLOPEN_SELF

# AC_LIBTOOL_DLOPEN_SELF
# -------------------
AC_DEFUN([AC_LIBTOOL_DLOPEN_SELF],
[if test "x$enable_dlopen" != xyes; then
  enable_dlopen=unknown
  enable_dlopen_self=unknown
  enable_dlopen_self_static=unknown
else
  lt_cv_dlopen=no
  lt_cv_dlopen_libs=

  case $host_os in
  beos*)
    lt_cv_dlopen="load_add_on"
    lt_cv_dlopen_libs=
    lt_cv_dlopen_self=yes
    ;;

  cygwin* | mingw* | pw32*)
    lt_cv_dlopen="LoadLibrary"
    lt_cv_dlopen_libs=
   ;;

  *)
    AC_CHECK_FUNC([shl_load],
          [lt_cv_dlopen="shl_load"],
      [AC_CHECK_LIB([dld], [shl_load],
            [lt_cv_dlopen="shl_load" lt_cv_dlopen_libs="-dld"],
	[AC_CHECK_FUNC([dlopen],
	      [lt_cv_dlopen="dlopen"],
	  [AC_CHECK_LIB([dl], [dlopen],
	        [lt_cv_dlopen="dlopen" lt_cv_dlopen_libs="-ldl"],
	    [AC_CHECK_LIB([svld], [dlopen],
	          [lt_cv_dlopen="dlopen" lt_cv_dlopen_libs="-lsvld"],
	      [AC_CHECK_LIB([dld], [dld_link],
	            [lt_cv_dlopen="dld_link" lt_cv_dlopen_libs="-dld"])
	      ])
	    ])
	  ])
	])
      ])
    ;;
  esac

  if test "x$lt_cv_dlopen" != xno; then
    enable_dlopen=yes
  else
    enable_dlopen=no
  fi

  case $lt_cv_dlopen in
  dlopen)
    save_CPPFLAGS="$CPPFLAGS"
    AC_REQUIRE([_LT_AC_CHECK_DLFCN])dnl
    test "x$ac_cv_header_dlfcn_h" = xyes && CPPFLAGS="$CPPFLAGS -DHAVE_DLFCN_H"

    save_LDFLAGS="$LDFLAGS"
    eval LDFLAGS=\"\$LDFLAGS $export_dynamic_flag_spec\"

    save_LIBS="$LIBS"
    LIBS="$lt_cv_dlopen_libs $LIBS"

    AC_CACHE_CHECK([whether a program can dlopen itself],
	  lt_cv_dlopen_self, [dnl
	  _LT_AC_TRY_DLOPEN_SELF(
	    lt_cv_dlopen_self=yes, lt_cv_dlopen_self=yes,
	    lt_cv_dlopen_self=no, lt_cv_dlopen_self=cross)
    ])

    if test "x$lt_cv_dlopen_self" = xyes; then
      LDFLAGS="$LDFLAGS $link_static_flag"
      AC_CACHE_CHECK([whether a statically linked program can dlopen itself],
    	  lt_cv_dlopen_self_static, [dnl
	  _LT_AC_TRY_DLOPEN_SELF(
	    lt_cv_dlopen_self_static=yes, lt_cv_dlopen_self_static=yes,
	    lt_cv_dlopen_self_static=no,  lt_cv_dlopen_self_static=cross)
      ])
    fi

    CPPFLAGS="$save_CPPFLAGS"
    LDFLAGS="$save_LDFLAGS"
    LIBS="$save_LIBS"
    ;;
  esac

  case $lt_cv_dlopen_self in
  yes|no) enable_dlopen_self=$lt_cv_dlopen_self ;;
  *) enable_dlopen_self=unknown ;;
  esac

  case $lt_cv_dlopen_self_static in
  yes|no) enable_dlopen_self_static=$lt_cv_dlopen_self_static ;;
  *) enable_dlopen_self_static=unknown ;;
  esac
fi
])# AC_LIBTOOL_DLOPEN_SELF

AC_DEFUN([_LT_AC_LTCONFIG_HACK],
[AC_REQUIRE([AC_LIBTOOL_SYS_GLOBAL_SYMBOL_PIPE])dnl
# Sed substitution that helps us do robust quoting.  It backslashifies
# metacharacters that are still active within double-quoted strings.
Xsed='sed -e s/^X//'
sed_quote_subst='s/\([[\\"\\`$\\\\]]\)/\\\1/g'

# Same as above, but do not quote variable references.
double_quote_subst='s/\([[\\"\\`\\\\]]\)/\\\1/g'

# Sed substitution to delay expansion of an escaped shell variable in a
# double_quote_subst'ed string.
delay_variable_subst='s/\\\\\\\\\\\$/\\\\\\$/g'

# Constants:
rm="rm -f"

# Global variables:
default_ofile=libtool
can_build_shared=yes

# All known linkers require a `.a' archive for static linking (except M$VC,
# which needs '.lib').
libext=a
ltmain="$ac_aux_dir/ltmain.sh"
ofile="$default_ofile"
with_gnu_ld="$lt_cv_prog_gnu_ld"
need_locks="$enable_libtool_lock"

old_CC="$CC"
old_CFLAGS="$CFLAGS"

# Set sane defaults for various variables
test -z "$AR" && AR=ar
test -z "$AR_FLAGS" && AR_FLAGS=cru
test -z "$AS" && AS=as
test -z "$CC" && CC=cc
test -z "$DLLTOOL" && DLLTOOL=dlltool
test -z "$LD" && LD=ld
test -z "$LN_S" && LN_S="ln -s"
test -z "$MAGIC_CMD" && MAGIC_CMD=file
test -z "$NM" && NM=nm
test -z "$OBJDUMP" && OBJDUMP=objdump
test -z "$RANLIB" && RANLIB=:
test -z "$STRIP" && STRIP=:
test -z "$ac_objext" && ac_objext=o

if test x"$host" != x"$build"; then
  ac_tool_prefix=${host_alias}-
else
  ac_tool_prefix=
fi

# Transform linux* to *-*-linux-gnu*, to support old configure scripts.
case $host_os in
linux-gnu*) ;;
linux*) host=`echo $host | sed 's/^\(.*-.*-linux\)\(.*\)$/\1-gnu\2/'`
esac

case $host_os in
aix3*)
  # AIX sometimes has problems with the GCC collect2 program.  For some
  # reason, if we set the COLLECT_NAMES environment variable, the problems
  # vanish in a puff of smoke.
  if test "X${COLLECT_NAMES+set}" != Xset; then
    COLLECT_NAMES=
    export COLLECT_NAMES
  fi
  ;;
esac

# Determine commands to create old-style static archives.
old_archive_cmds='$AR $AR_FLAGS $oldlib$oldobjs$old_deplibs'
old_postinstall_cmds='chmod 644 $oldlib'
old_postuninstall_cmds=

if test -n "$RANLIB"; then
  case $host_os in
  openbsd*)
    old_postinstall_cmds="\$RANLIB -t \$oldlib~$old_postinstall_cmds"
    ;;
  *)
    old_postinstall_cmds="\$RANLIB \$oldlib~$old_postinstall_cmds"
    ;;
  esac
  old_archive_cmds="$old_archive_cmds~\$RANLIB \$oldlib"
fi

# Allow CC to be a program name with arguments.
set dummy $CC
compiler="[$]2"

AC_MSG_CHECKING([for objdir])
rm -f .libs 2>/dev/null
mkdir .libs 2>/dev/null
if test -d .libs; then
  objdir=.libs
else
  # MS-DOS does not allow filenames that begin with a dot.
  objdir=_libs
fi
rmdir .libs 2>/dev/null
AC_MSG_RESULT($objdir)


AC_ARG_WITH(pic,
[  --with-pic              try to use only PIC/non-PIC objects [default=use both]],
pic_mode="$withval", pic_mode=default)
test -z "$pic_mode" && pic_mode=default

# We assume here that the value for lt_cv_prog_cc_pic will not be cached
# in isolation, and that seeing it set (from the cache) indicates that
# the associated values are set (in the cache) correctly too.
AC_MSG_CHECKING([for $compiler option to produce PIC])
AC_CACHE_VAL(lt_cv_prog_cc_pic,
[ lt_cv_prog_cc_pic=
  lt_cv_prog_cc_shlib=
  lt_cv_prog_cc_wl=
  lt_cv_prog_cc_static=
  lt_cv_prog_cc_no_builtin=
  lt_cv_prog_cc_can_build_shared=$can_build_shared

  if test "$GCC" = yes; then
    lt_cv_prog_cc_wl='-Wl,'
    lt_cv_prog_cc_static='-static'

    case $host_os in
    aix*)
      # Below there is a dirty hack to force normal static linking with -ldl
      # The problem is because libdl dynamically linked with both libc and
      # libC (AIX C++ library), which obviously doesn't included in libraries
      # list by gcc. This cause undefined symbols with -static flags.
      # This hack allows C programs to be linked with "-static -ldl", but
      # not sure about C++ programs.
      lt_cv_prog_cc_static="$lt_cv_prog_cc_static ${lt_cv_prog_cc_wl}-lC"
      ;;
    amigaos*)
      # FIXME: we need at least 68020 code to build shared libraries, but
      # adding the `-m68020' flag to GCC prevents building anything better,
      # like `-m68040'.
      lt_cv_prog_cc_pic='-m68020 -resident32 -malways-restore-a4'
      ;;
    beos* | irix5* | irix6* | osf3* | osf4* | osf5*)
      # PIC is the default for these OSes.
      ;;
    darwin* | rhapsody*)
      # PIC is the default on this platform
      # Common symbols not allowed in MH_DYLIB files
      lt_cv_prog_cc_pic='-fno-common'
      ;;
    cygwin* | mingw* | pw32* | os2*)
      # This hack is so that the source file can tell whether it is being
      # built for inclusion in a dll (and should export symbols for example).
      lt_cv_prog_cc_pic='-DDLL_EXPORT'
      ;;
    sysv4*MP*)
      if test -d /usr/nec; then
	 lt_cv_prog_cc_pic=-Kconform_pic
      fi
      ;;
    *)
      lt_cv_prog_cc_pic='-fPIC'
      ;;
    esac
  else
    # PORTME Check for PIC flags for the system compiler.
    case $host_os in
    aix3* | aix4* | aix5*)
      lt_cv_prog_cc_wl='-Wl,'
      # All AIX code is PIC.
      if test "$host_cpu" = ia64; then
	# AIX 5 now supports IA64 processor
	lt_cv_prog_cc_static='-Bstatic'
      else
	lt_cv_prog_cc_static='-bnso -bI:/lib/syscalls.exp'
      fi
      ;;

    hpux9* | hpux10* | hpux11*)
      # Is there a better lt_cv_prog_cc_static that works with the bundled CC?
      lt_cv_prog_cc_wl='-Wl,'
      lt_cv_prog_cc_static="${lt_cv_prog_cc_wl}-a ${lt_cv_prog_cc_wl}archive"
      lt_cv_prog_cc_pic='+Z'
      ;;

    irix5* | irix6*)
      lt_cv_prog_cc_wl='-Wl,'
      lt_cv_prog_cc_static='-non_shared'
      # PIC (with -KPIC) is the default.
      ;;

    cygwin* | mingw* | pw32* | os2*)
      # This hack is so that the source file can tell whether it is being
      # built for inclusion in a dll (and should export symbols for example).
      lt_cv_prog_cc_pic='-DDLL_EXPORT'
      ;;

    newsos6)
      lt_cv_prog_cc_pic='-KPIC'
      lt_cv_prog_cc_static='-Bstatic'
      ;;

    osf3* | osf4* | osf5*)
      # All OSF/1 code is PIC.
      lt_cv_prog_cc_wl='-Wl,'
      lt_cv_prog_cc_static='-non_shared'
      ;;

    sco3.2v5*)
      lt_cv_prog_cc_pic='-Kpic'
      lt_cv_prog_cc_static='-dn'
      lt_cv_prog_cc_shlib='-belf'
      ;;

    solaris*)
      lt_cv_prog_cc_pic='-KPIC'
      lt_cv_prog_cc_static='-Bstatic'
      lt_cv_prog_cc_wl='-Wl,'
      ;;

    sunos4*)
      lt_cv_prog_cc_pic='-PIC'
      lt_cv_prog_cc_static='-Bstatic'
      lt_cv_prog_cc_wl='-Qoption ld '
      ;;

    sysv4 | sysv4.2uw2* | sysv4.3* | sysv5*)
      lt_cv_prog_cc_pic='-KPIC'
      lt_cv_prog_cc_static='-Bstatic'
      if test "x$host_vendor" = xsni; then
	lt_cv_prog_cc_wl='-LD'
      else
	lt_cv_prog_cc_wl='-Wl,'
      fi
      ;;

    uts4*)
      lt_cv_prog_cc_pic='-pic'
      lt_cv_prog_cc_static='-Bstatic'
      ;;

    sysv4*MP*)
      if test -d /usr/nec ;then
	lt_cv_prog_cc_pic='-Kconform_pic'
	lt_cv_prog_cc_static='-Bstatic'
      fi
      ;;

    *)
      lt_cv_prog_cc_can_build_shared=no
      ;;
    esac
  fi
])
if test -z "$lt_cv_prog_cc_pic"; then
  AC_MSG_RESULT([none])
else
  AC_MSG_RESULT([$lt_cv_prog_cc_pic])

  # Check to make sure the pic_flag actually works.
  AC_MSG_CHECKING([if $compiler PIC flag $lt_cv_prog_cc_pic works])
  AC_CACHE_VAL(lt_cv_prog_cc_pic_works, [dnl
    save_CFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS $lt_cv_prog_cc_pic -DPIC"
    AC_TRY_COMPILE([], [], [dnl
      case $host_os in
      hpux9* | hpux10* | hpux11*)
	# On HP-UX, both CC and GCC only warn that PIC is supported... then
	# they create non-PIC objects.  So, if there were any warnings, we
	# assume that PIC is not supported.
	if test -s conftest.err; then
	  lt_cv_prog_cc_pic_works=no
	else
	  lt_cv_prog_cc_pic_works=yes
	fi
	;;
      *)
	lt_cv_prog_cc_pic_works=yes
	;;
      esac
    ], [dnl
      lt_cv_prog_cc_pic_works=no
    ])
    CFLAGS="$save_CFLAGS"
  ])

  if test "X$lt_cv_prog_cc_pic_works" = Xno; then
    lt_cv_prog_cc_pic=
    lt_cv_prog_cc_can_build_shared=no
  else
    lt_cv_prog_cc_pic=" $lt_cv_prog_cc_pic"
  fi

  AC_MSG_RESULT([$lt_cv_prog_cc_pic_works])
fi

# Check for any special shared library compilation flags.
if test -n "$lt_cv_prog_cc_shlib"; then
  AC_MSG_WARN([\`$CC' requires \`$lt_cv_prog_cc_shlib' to build shared libraries])
  if echo "$old_CC $old_CFLAGS " | egrep -e "[[ 	]]$lt_cv_prog_cc_shlib[[ 	]]" >/dev/null; then :
  else
   AC_MSG_WARN([add \`$lt_cv_prog_cc_shlib' to the CC or CFLAGS env variable and reconfigure])
    lt_cv_prog_cc_can_build_shared=no
  fi
fi

AC_MSG_CHECKING([if $compiler static flag $lt_cv_prog_cc_static works])
AC_CACHE_VAL([lt_cv_prog_cc_static_works], [dnl
  lt_cv_prog_cc_static_works=no
  save_LDFLAGS="$LDFLAGS"
  LDFLAGS="$LDFLAGS $lt_cv_prog_cc_static"
  AC_TRY_LINK([], [], [lt_cv_prog_cc_static_works=yes])
  LDFLAGS="$save_LDFLAGS"
])

# Belt *and* braces to stop my trousers falling down:
test "X$lt_cv_prog_cc_static_works" = Xno && lt_cv_prog_cc_static=
AC_MSG_RESULT([$lt_cv_prog_cc_static_works])

pic_flag="$lt_cv_prog_cc_pic"
special_shlib_compile_flags="$lt_cv_prog_cc_shlib"
wl="$lt_cv_prog_cc_wl"
link_static_flag="$lt_cv_prog_cc_static"
no_builtin_flag="$lt_cv_prog_cc_no_builtin"
can_build_shared="$lt_cv_prog_cc_can_build_shared"


# Check to see if options -o and -c are simultaneously supported by compiler
AC_MSG_CHECKING([if $compiler supports -c -o file.$ac_objext])
AC_CACHE_VAL([lt_cv_compiler_c_o], [
$rm -r conftest 2>/dev/null
mkdir conftest
cd conftest
echo "int some_variable = 0;" > conftest.$ac_ext
mkdir out
# According to Tom Tromey, Ian Lance Taylor reported there are C compilers
# that will create temporary files in the current directory regardless of
# the output directory.  Thus, making CWD read-only will cause this test
# to fail, enabling locking or at least warning the user not to do parallel
# builds.
chmod -w .
save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -o out/conftest2.$ac_objext"
compiler_c_o=no
if { (eval echo configure:__oline__: \"$ac_compile\") 1>&5; (eval $ac_compile) 2>out/conftest.err; } && test -s out/conftest2.$ac_objext; then
  # The compiler can only warn and ignore the option if not recognized
  # So say no if there are warnings
  if test -s out/conftest.err; then
    lt_cv_compiler_c_o=no
  else
    lt_cv_compiler_c_o=yes
  fi
else
  # Append any errors to the config.log.
  cat out/conftest.err 1>&AC_FD_CC
  lt_cv_compiler_c_o=no
fi
CFLAGS="$save_CFLAGS"
chmod u+w .
$rm conftest* out/*
rmdir out
cd ..
rmdir conftest
$rm -r conftest 2>/dev/null
])
compiler_c_o=$lt_cv_compiler_c_o
AC_MSG_RESULT([$compiler_c_o])

if test x"$compiler_c_o" = x"yes"; then
  # Check to see if we can write to a .lo
  AC_MSG_CHECKING([if $compiler supports -c -o file.lo])
  AC_CACHE_VAL([lt_cv_compiler_o_lo], [
  lt_cv_compiler_o_lo=no
  save_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -c -o conftest.lo"
  save_objext="$ac_objext"
  ac_objext=lo
  AC_TRY_COMPILE([], [int some_variable = 0;], [dnl
    # The compiler can only warn and ignore the option if not recognized
    # So say no if there are warnings
    if test -s conftest.err; then
      lt_cv_compiler_o_lo=no
    else
      lt_cv_compiler_o_lo=yes
    fi
  ])
  ac_objext="$save_objext"
  CFLAGS="$save_CFLAGS"
  ])
  compiler_o_lo=$lt_cv_compiler_o_lo
  AC_MSG_RESULT([$compiler_o_lo])
else
  compiler_o_lo=no
fi

# Check to see if we can do hard links to lock some files if needed
hard_links="nottested"
if test "$compiler_c_o" = no && test "$need_locks" != no; then
  # do not overwrite the value of need_locks provided by the user
  AC_MSG_CHECKING([if we can lock with hard links])
  hard_links=yes
  $rm conftest*
  ln conftest.a conftest.b 2>/dev/null && hard_links=no
  touch conftest.a
  ln conftest.a conftest.b 2>&5 || hard_links=no
  ln conftest.a conftest.b 2>/dev/null && hard_links=no
  AC_MSG_RESULT([$hard_links])
  if test "$hard_links" = no; then
    AC_MSG_WARN([\`$CC' does not support \`-c -o', so \`make -j' may be unsafe])
    need_locks=warn
  fi
else
  need_locks=no
fi

if test "$GCC" = yes; then
  # Check to see if options -fno-rtti -fno-exceptions are supported by compiler
  AC_MSG_CHECKING([if $compiler supports -fno-rtti -fno-exceptions])
  echo "int some_variable = 0;" > conftest.$ac_ext
  save_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -fno-rtti -fno-exceptions -c conftest.$ac_ext"
  compiler_rtti_exceptions=no
  AC_TRY_COMPILE([], [int some_variable = 0;], [dnl
    # The compiler can only warn and ignore the option if not recognized
    # So say no if there are warnings
    if test -s conftest.err; then
      compiler_rtti_exceptions=no
    else
      compiler_rtti_exceptions=yes
    fi
  ])
  CFLAGS="$save_CFLAGS"
  AC_MSG_RESULT([$compiler_rtti_exceptions])

  if test "$compiler_rtti_exceptions" = "yes"; then
    no_builtin_flag=' -fno-builtin -fno-rtti -fno-exceptions'
  else
    no_builtin_flag=' -fno-builtin'
  fi
fi

# See if the linker supports building shared libraries.
AC_MSG_CHECKING([whether the linker ($LD) supports shared libraries])

allow_undefined_flag=
no_undefined_flag=
need_lib_prefix=unknown
need_version=unknown
# when you set need_version to no, make sure it does not cause -set_version
# flags to be left without arguments
archive_cmds=
archive_expsym_cmds=
old_archive_from_new_cmds=
old_archive_from_expsyms_cmds=
export_dynamic_flag_spec=
whole_archive_flag_spec=
thread_safe_flag_spec=
hardcode_into_libs=no
hardcode_libdir_flag_spec=
hardcode_libdir_separator=
hardcode_direct=no
hardcode_minus_L=no
hardcode_shlibpath_var=unsupported
runpath_var=
link_all_deplibs=unknown
always_export_symbols=no
export_symbols_cmds='$NM $libobjs $convenience | $global_symbol_pipe | sed '\''s/.* //'\'' | sort | uniq > $export_symbols'
# include_expsyms should be a list of space-separated symbols to be *always*
# included in the symbol list
include_expsyms=
# exclude_expsyms can be an egrep regular expression of symbols to exclude
# it will be wrapped by ` (' and `)$', so one must not match beginning or
# end of line.  Example: `a|bc|.*d.*' will exclude the symbols `a' and `bc',
# as well as any symbol that contains `d'.
exclude_expsyms="_GLOBAL_OFFSET_TABLE_"
# Although _GLOBAL_OFFSET_TABLE_ is a valid symbol C name, most a.out
# platforms (ab)use it in PIC code, but their linkers get confused if
# the symbol is explicitly referenced.  Since portable code cannot
# rely on this symbol name, it's probably fine to never include it in
# preloaded symbol tables.
extract_expsyms_cmds=

case $host_os in
cygwin* | mingw* | pw32*)
  # FIXME: the MSVC++ port hasn't been tested in a loooong time
  # When not using gcc, we currently assume that we are using
  # Microsoft Visual C++.
  if test "$GCC" != yes; then
    with_gnu_ld=no
  fi
  ;;
openbsd*)
  with_gnu_ld=no
  ;;
esac

ld_shlibs=yes
if test "$with_gnu_ld" = yes; then
  # If archive_cmds runs LD, not CC, wlarc should be empty
  wlarc='${wl}'

  # See if GNU ld supports shared libraries.
  case $host_os in
  aix3* | aix4* | aix5*)
    # On AIX, the GNU linker is very broken
    # Note:Check GNU linker on AIX 5-IA64 when/if it becomes available.
    ld_shlibs=no
    cat <<EOF 1>&2

*** Warning: the GNU linker, at least up to release 2.9.1, is reported
*** to be unable to reliably create shared libraries on AIX.
*** Therefore, libtool is disabling shared libraries support.  If you
*** really care for shared libraries, you may want to modify your PATH
*** so that a non-GNU linker is found, and then restart.

EOF
    ;;

  amigaos*)
    archive_cmds='$rm $output_objdir/a2ixlibrary.data~$echo "#define NAME $libname" > $output_objdir/a2ixlibrary.data~$echo "#define LIBRARY_ID 1" >> $output_objdir/a2ixlibrary.data~$echo "#define VERSION $major" >> $output_objdir/a2ixlibrary.data~$echo "#define REVISION $revision" >> $output_objdir/a2ixlibrary.data~$AR $AR_FLAGS $lib $libobjs~$RANLIB $lib~(cd $output_objdir && a2ixlibrary -32)'
    hardcode_libdir_flag_spec='-L$libdir'
    hardcode_minus_L=yes

    # Samuel A. Falvo II <kc5tja@dolphin.openprojects.net> reports
    # that the semantics of dynamic libraries on AmigaOS, at least up
    # to version 4, is to share data among multiple programs linked
    # with the same dynamic library.  Since this doesn't match the
    # behavior of shared libraries on other platforms, we can use
    # them.
    ld_shlibs=no
    ;;

  beos*)
    if $LD --help 2>&1 | egrep ': supported targets:.* elf' > /dev/null; then
      allow_undefined_flag=unsupported
      # Joseph Beckenbach <jrb3@best.com> says some releases of gcc
      # support --undefined.  This deserves some investigation.  FIXME
      archive_cmds='$CC -nostart $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
    else
      ld_shlibs=no
    fi
    ;;

  cygwin* | mingw* | pw32*)
    # hardcode_libdir_flag_spec is actually meaningless, as there is
    # no search path for DLLs.
    hardcode_libdir_flag_spec='-L$libdir'
    allow_undefined_flag=unsupported
    always_export_symbols=yes

    extract_expsyms_cmds='test -f $output_objdir/impgen.c || \
      sed -e "/^# \/\* impgen\.c starts here \*\//,/^# \/\* impgen.c ends here \*\// { s/^# //;s/^# *$//; p; }" -e d < $''0 > $output_objdir/impgen.c~
      test -f $output_objdir/impgen.exe || (cd $output_objdir && \
      if test "x$HOST_CC" != "x" ; then $HOST_CC -o impgen impgen.c ; \
      else $CC -o impgen impgen.c ; fi)~
      $output_objdir/impgen $dir/$soroot > $output_objdir/$soname-def'

    old_archive_from_expsyms_cmds='$DLLTOOL --as=$AS --dllname $soname --def $output_objdir/$soname-def --output-lib $output_objdir/$newlib'

    # cygwin and mingw dlls have different entry points and sets of symbols
    # to exclude.
    # FIXME: what about values for MSVC?
    dll_entry=__cygwin_dll_entry@12
    dll_exclude_symbols=DllMain@12,_cygwin_dll_entry@12,_cygwin_noncygwin_dll_entry@12~
    case $host_os in
    mingw*)
      # mingw values
      dll_entry=_DllMainCRTStartup@12
      dll_exclude_symbols=DllMain@12,DllMainCRTStartup@12,DllEntryPoint@12~
      ;;
    esac

    # mingw and cygwin differ, and it's simplest to just exclude the union
    # of the two symbol sets.
    dll_exclude_symbols=DllMain@12,_cygwin_dll_entry@12,_cygwin_noncygwin_dll_entry@12,DllMainCRTStartup@12,DllEntryPoint@12

    # recent cygwin and mingw systems supply a stub DllMain which the user
    # can override, but on older systems we have to supply one (in ltdll.c)
    if test "x$lt_cv_need_dllmain" = "xyes"; then
      ltdll_obj='$output_objdir/$soname-ltdll.'"$ac_objext "
      ltdll_cmds='test -f $output_objdir/$soname-ltdll.c || sed -e "/^# \/\* ltdll\.c starts here \*\//,/^# \/\* ltdll.c ends here \*\// { s/^# //; p; }" -e d < $''0 > $output_objdir/$soname-ltdll.c~
	test -f $output_objdir/$soname-ltdll.$ac_objext || (cd $output_objdir && $CC -c $soname-ltdll.c)~'
    else
      ltdll_obj=
      ltdll_cmds=
    fi

    # Extract the symbol export list from an `--export-all' def file,
    # then regenerate the def file from the symbol export list, so that
    # the compiled dll only exports the symbol export list.
    # Be careful not to strip the DATA tag left be newer dlltools.
    export_symbols_cmds="$ltdll_cmds"'
      $DLLTOOL --export-all --exclude-symbols '$dll_exclude_symbols' --output-def $output_objdir/$soname-def '$ltdll_obj'$libobjs $convenience~
      sed -e "1,/EXPORTS/d" -e "s/ @ [[0-9]]*//" -e "s/ *;.*$//" < $output_objdir/$soname-def > $export_symbols'

    # If the export-symbols file already is a .def file (1st line
    # is EXPORTS), use it as is.
    # If DATA tags from a recent dlltool are present, honour them!
    archive_expsym_cmds='if test "x`head -1 $export_symbols`" = xEXPORTS; then
	cp $export_symbols $output_objdir/$soname-def;
      else
	echo EXPORTS > $output_objdir/$soname-def;
	_lt_hint=1;
	cat $export_symbols | while read symbol; do
	 set dummy \$symbol;
	 case \[$]# in
	   2) echo "   \[$]2 @ \$_lt_hint ; " >> $output_objdir/$soname-def;;
	   *) echo "     \[$]2 @ \$_lt_hint \[$]3 ; " >> $output_objdir/$soname-def;;
	 esac;
	 _lt_hint=`expr 1 + \$_lt_hint`;
	done;
      fi~
      '"$ltdll_cmds"'
      $CC -Wl,--base-file,$output_objdir/$soname-base '$lt_cv_cc_dll_switch' -Wl,-e,'$dll_entry' -o $output_objdir/$soname '$ltdll_obj'$libobjs $deplibs $compiler_flags~
      $DLLTOOL --as=$AS --dllname $soname --exclude-symbols '$dll_exclude_symbols' --def $output_objdir/$soname-def --base-file $output_objdir/$soname-base --output-exp $output_objdir/$soname-exp~
      $CC -Wl,--base-file,$output_objdir/$soname-base $output_objdir/$soname-exp '$lt_cv_cc_dll_switch' -Wl,-e,'$dll_entry' -o $output_objdir/$soname '$ltdll_obj'$libobjs $deplibs $compiler_flags~
      $DLLTOOL --as=$AS --dllname $soname --exclude-symbols '$dll_exclude_symbols' --def $output_objdir/$soname-def --base-file $output_objdir/$soname-base --output-exp $output_objdir/$soname-exp --output-lib $output_objdir/$libname.dll.a~
      $CC $output_objdir/$soname-exp '$lt_cv_cc_dll_switch' -Wl,-e,'$dll_entry' -o $output_objdir/$soname '$ltdll_obj'$libobjs $deplibs $compiler_flags'
    ;;

  netbsd*)
    if echo __ELF__ | $CC -E - | grep __ELF__ >/dev/null; then
      archive_cmds='$LD -Bshareable $libobjs $deplibs $linker_flags -o $lib'
      wlarc=
    else
      archive_cmds='$CC -shared -nodefaultlibs $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
      archive_expsym_cmds='$CC -shared -nodefaultlibs $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname ${wl}-retain-symbols-file $wl$export_symbols -o $lib'
    fi
    ;;

  solaris* | sysv5*)
    if $LD -v 2>&1 | egrep 'BFD 2\.8' > /dev/null; then
      ld_shlibs=no
      cat <<EOF 1>&2

*** Warning: The releases 2.8.* of the GNU linker cannot reliably
*** create shared libraries on Solaris systems.  Therefore, libtool
*** is disabling shared libraries support.  We urge you to upgrade GNU
*** binutils to release 2.9.1 or newer.  Another option is to modify
*** your PATH or compiler configuration so that the native linker is
*** used, and then restart.

EOF
    elif $LD --help 2>&1 | egrep ': supported targets:.* elf' > /dev/null; then
      archive_cmds='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
      archive_expsym_cmds='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname ${wl}-retain-symbols-file $wl$export_symbols -o $lib'
    else
      ld_shlibs=no
    fi
    ;;

  sunos4*)
    archive_cmds='$LD -assert pure-text -Bshareable -o $lib $libobjs $deplibs $linker_flags'
    wlarc=
    hardcode_direct=yes
    hardcode_shlibpath_var=no
    ;;

  *)
    if $LD --help 2>&1 | egrep ': supported targets:.* elf' > /dev/null; then
      archive_cmds='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
      archive_expsym_cmds='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname ${wl}-retain-symbols-file $wl$export_symbols -o $lib'
    else
      ld_shlibs=no
    fi
    ;;
  esac

  if test "$ld_shlibs" = yes; then
    runpath_var=LD_RUN_PATH
    hardcode_libdir_flag_spec='${wl}--rpath ${wl}$libdir'
    export_dynamic_flag_spec='${wl}--export-dynamic'
    case $host_os in
    cygwin* | mingw* | pw32*)
      # dlltool doesn't understand --whole-archive et. al.
      whole_archive_flag_spec=
      ;;
    *)
      # ancient GNU ld didn't support --whole-archive et. al.
      if $LD --help 2>&1 | egrep 'no-whole-archive' > /dev/null; then
	whole_archive_flag_spec="$wlarc"'--whole-archive$convenience '"$wlarc"'--no-whole-archive'
      else
	whole_archive_flag_spec=
      fi
      ;;
    esac
  fi
else
  # PORTME fill in a description of your system's linker (not GNU ld)
  case $host_os in
  aix3*)
    allow_undefined_flag=unsupported
    always_export_symbols=yes
    archive_expsym_cmds='$LD -o $output_objdir/$soname $libobjs $deplibs $linker_flags -bE:$export_symbols -T512 -H512 -bM:SRE~$AR $AR_FLAGS $lib $output_objdir/$soname'
    # Note: this linker hardcodes the directories in LIBPATH if there
    # are no directories specified by -L.
    hardcode_minus_L=yes
    if test "$GCC" = yes && test -z "$link_static_flag"; then
      # Neither direct hardcoding nor static linking is supported with a
      # broken collect2.
      hardcode_direct=unsupported
    fi
    ;;

  aix4* | aix5*)
    if test "$host_cpu" = ia64; then
      # On IA64, the linker does run time linking by default, so we don't
      # have to do anything special.
      aix_use_runtimelinking=no
      exp_sym_flag='-Bexport'
      no_entry_flag=""
    else
      aix_use_runtimelinking=no

      # Test if we are trying to use run time linking or normal
      # AIX style linking. If -brtl is somewhere in LDFLAGS, we
      # need to do runtime linking.
      case $host_os in aix4.[[23]]|aix4.[[23]].*|aix5*)
	for ld_flag in $LDFLAGS; do
	  if (test $ld_flag = "-brtl" || test $ld_flag = "-Wl,-brtl"); then
	    aix_use_runtimelinking=yes
	    break
	  fi
	done
      esac

      exp_sym_flag='-bexport'
      no_entry_flag='-bnoentry'
    fi

    # When large executables or shared objects are built, AIX ld can
    # have problems creating the table of contents.  If linking a library
    # or program results in "error TOC overflow" add -mminimal-toc to
    # CXXFLAGS/CFLAGS for g++/gcc.  In the cases where that is not
    # enough to fix the problem, add -Wl,-bbigtoc to LDFLAGS.

    hardcode_direct=yes
    archive_cmds=''
    hardcode_libdir_separator=':'
    if test "$GCC" = yes; then
      case $host_os in aix4.[[012]]|aix4.[[012]].*)
	collect2name=`${CC} -print-prog-name=collect2`
	if test -f "$collect2name" && \
	  strings "$collect2name" | grep resolve_lib_name >/dev/null
	then
	  # We have reworked collect2
	  hardcode_direct=yes
	else
	  # We have old collect2
	  hardcode_direct=unsupported
	  # It fails to find uninstalled libraries when the uninstalled
	  # path is not listed in the libpath.  Setting hardcode_minus_L
	  # to unsupported forces relinking
	  hardcode_minus_L=yes
	  hardcode_libdir_flag_spec='-L$libdir'
	  hardcode_libdir_separator=
	fi
      esac

      shared_flag='-shared'
    else
      # not using gcc
      if test "$host_cpu" = ia64; then
	shared_flag='${wl}-G'
      else
	if test "$aix_use_runtimelinking" = yes; then
	  shared_flag='${wl}-G'
	else
	  shared_flag='${wl}-bM:SRE'
	fi
      fi
    fi

    # It seems that -bexpall can do strange things, so it is better to
    # generate a list of symbols to export.
    always_export_symbols=yes
    if test "$aix_use_runtimelinking" = yes; then
      # Warning - without using the other runtime loading flags (-brtl),
      # -berok will link without error, but may produce a broken library.
      allow_undefined_flag='-berok'
      hardcode_libdir_flag_spec='${wl}-blibpath:$libdir:/usr/lib:/lib'
      archive_expsym_cmds="\$CC"' -o $output_objdir/$soname $libobjs $deplibs $compiler_flags `if test "x${allow_undefined_flag}" != "x"; then echo "${wl}${allow_undefined_flag}"; else :; fi` '"\${wl}$no_entry_flag \${wl}$exp_sym_flag:\$export_symbols $shared_flag"
    else
      if test "$host_cpu" = ia64; then
	hardcode_libdir_flag_spec='${wl}-R $libdir:/usr/lib:/lib'
	allow_undefined_flag="-z nodefs"
	archive_expsym_cmds="\$CC $shared_flag"' -o $output_objdir/$soname ${wl}-h$soname $libobjs $deplibs $compiler_flags ${wl}${allow_undefined_flag} '"\${wl}$no_entry_flag \${wl}$exp_sym_flag:\$export_symbols"
      else
	hardcode_libdir_flag_spec='${wl}-bnolibpath ${wl}-blibpath:$libdir:/usr/lib:/lib'
	# Warning - without using the other run time loading flags,
	# -berok will link without error, but may produce a broken library.
	allow_undefined_flag='${wl}-berok'
	# This is a bit strange, but is similar to how AIX traditionally builds
	# it's shared libraries.
	archive_expsym_cmds="\$CC $shared_flag"' -o $output_objdir/$soname $libobjs $deplibs $compiler_flags ${allow_undefined_flag} '"\${wl}$no_entry_flag \${wl}$exp_sym_flag:\$export_symbols"' ~$AR -crlo $objdir/$libname$release.a $objdir/$soname'
      fi
    fi
    ;;

  amigaos*)
    archive_cmds='$rm $output_objdir/a2ixlibrary.data~$echo "#define NAME $libname" > $output_objdir/a2ixlibrary.data~$echo "#define LIBRARY_ID 1" >> $output_objdir/a2ixlibrary.data~$echo "#define VERSION $major" >> $output_objdir/a2ixlibrary.data~$echo "#define REVISION $revision" >> $output_objdir/a2ixlibrary.data~$AR $AR_FLAGS $lib $libobjs~$RANLIB $lib~(cd $output_objdir && a2ixlibrary -32)'
    hardcode_libdir_flag_spec='-L$libdir'
    hardcode_minus_L=yes
    # see comment about different semantics on the GNU ld section
    ld_shlibs=no
    ;;

  cygwin* | mingw* | pw32*)
    # When not using gcc, we currently assume that we are using
    # Microsoft Visual C++.
    # hardcode_libdir_flag_spec is actually meaningless, as there is
    # no search path for DLLs.
    hardcode_libdir_flag_spec=' '
    allow_undefined_flag=unsupported
    # Tell ltmain to make .lib files, not .a files.
    libext=lib
    # FIXME: Setting linknames here is a bad hack.
    archive_cmds='$CC -o $lib $libobjs $compiler_flags `echo "$deplibs" | sed -e '\''s/ -lc$//'\''` -link -dll~linknames='
    # The linker will automatically build a .lib file if we build a DLL.
    old_archive_from_new_cmds='true'
    # FIXME: Should let the user specify the lib program.
    old_archive_cmds='lib /OUT:$oldlib$oldobjs$old_deplibs'
    fix_srcfile_path='`cygpath -w "$srcfile"`'
    ;;

  darwin* | rhapsody*)
    case "$host_os" in
    rhapsody* | darwin1.[[012]])
      allow_undefined_flag='-undefined suppress'
      ;;
    *) # Darwin 1.3 on
      allow_undefined_flag='-flat_namespace -undefined suppress'
      ;;
    esac
    # FIXME: Relying on posixy $() will cause problems for
    #        cross-compilation, but unfortunately the echo tests do not
    #        yet detect zsh echo's removal of \ escapes.
    archive_cmds='$nonopt $(test "x$module" = xyes && echo -bundle || echo -dynamiclib) $allow_undefined_flag -o $lib $libobjs $deplibs$linker_flags -install_name $rpath/$soname $verstring'
    # We need to add '_' to the symbols in $export_symbols first
    #archive_expsym_cmds="$archive_cmds"' && strip -s $export_symbols'
    hardcode_direct=yes
    hardcode_shlibpath_var=no
    whole_archive_flag_spec='-all_load $convenience'
    ;;

  freebsd1*)
    ld_shlibs=no
    ;;

  # FreeBSD 2.2.[012] allows us to include c++rt0.o to get C++ constructor
  # support.  Future versions do this automatically, but an explicit c++rt0.o
  # does not break anything, and helps significantly (at the cost of a little
  # extra space).
  freebsd2.2*)
    archive_cmds='$LD -Bshareable -o $lib $libobjs $deplibs $linker_flags /usr/lib/c++rt0.o'
    hardcode_libdir_flag_spec='-R$libdir'
    hardcode_direct=yes
    hardcode_shlibpath_var=no
    ;;

  # Unfortunately, older versions of FreeBSD 2 do not have this feature.
  freebsd2*)
    archive_cmds='$LD -Bshareable -o $lib $libobjs $deplibs $linker_flags'
    hardcode_direct=yes
    hardcode_minus_L=yes
    hardcode_shlibpath_var=no
    ;;

  # FreeBSD 3 and greater uses gcc -shared to do shared libraries.
  freebsd*)
    archive_cmds='$CC -shared -o $lib $libobjs $deplibs $compiler_flags'
    hardcode_libdir_flag_spec='-R$libdir'
    hardcode_direct=yes
    hardcode_shlibpath_var=no
    ;;

  hpux9* | hpux10* | hpux11*)
    case $host_os in
    hpux9*) archive_cmds='$rm $output_objdir/$soname~$LD -b +b $install_libdir -o $output_objdir/$soname $libobjs $deplibs $linker_flags~test $output_objdir/$soname = $lib || mv $output_objdir/$soname $lib' ;;
    *) archive_cmds='$LD -b +h $soname +b $install_libdir -o $lib $libobjs $deplibs $linker_flags' ;;
    esac
    hardcode_libdir_flag_spec='${wl}+b ${wl}$libdir'
    hardcode_libdir_separator=:
    hardcode_direct=yes
    hardcode_minus_L=yes # Not in the search PATH, but as the default
			 # location of the library.
    export_dynamic_flag_spec='${wl}-E'
    ;;

  irix5* | irix6*)
    if test "$GCC" = yes; then
      archive_cmds='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname ${wl}$soname `test -n "$verstring" && echo ${wl}-set_version ${wl}$verstring` ${wl}-update_registry ${wl}${output_objdir}/so_locations -o $lib'
    else
      archive_cmds='$LD -shared $libobjs $deplibs $linker_flags -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${output_objdir}/so_locations -o $lib'
    fi
    hardcode_libdir_flag_spec='${wl}-rpath ${wl}$libdir'
    hardcode_libdir_separator=:
    link_all_deplibs=yes
    ;;

  netbsd*)
    if echo __ELF__ | $CC -E - | grep __ELF__ >/dev/null; then
      archive_cmds='$LD -Bshareable -o $lib $libobjs $deplibs $linker_flags'  # a.out
    else
      archive_cmds='$LD -shared -o $lib $libobjs $deplibs $linker_flags'      # ELF
    fi
    hardcode_libdir_flag_spec='-R$libdir'
    hardcode_direct=yes
    hardcode_shlibpath_var=no
    ;;

  newsos6)
    archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
    hardcode_direct=yes
    hardcode_libdir_flag_spec='${wl}-rpath ${wl}$libdir'
    hardcode_libdir_separator=:
    hardcode_shlibpath_var=no
    ;;

  openbsd*)
    hardcode_direct=yes
    hardcode_shlibpath_var=no
    if test -z "`echo __ELF__ | $CC -E - | grep __ELF__`" || test "$host_os-$host_cpu" = "openbsd2.8-powerpc"; then
      archive_cmds='$CC -shared $pic_flag -o $lib $libobjs $deplibs $linker_flags'
      hardcode_libdir_flag_spec='${wl}-rpath,$libdir'
      export_dynamic_flag_spec='${wl}-E'
    else
      case "$host_os" in
      openbsd[[01]].* | openbsd2.[[0-7]] | openbsd2.[[0-7]].*)
	archive_cmds='$LD -Bshareable -o $lib $libobjs $deplibs $linker_flags'
	hardcode_libdir_flag_spec='-R$libdir'
        ;;
      *)
        archive_cmds='$CC -shared $pic_flag -o $lib $libobjs $deplibs $linker_flags'
        hardcode_libdir_flag_spec='${wl}-rpath,$libdir'
        ;;
      esac
    fi
    ;;

  os2*)
    hardcode_libdir_flag_spec='-L$libdir'
    hardcode_minus_L=yes
    allow_undefined_flag=unsupported
    archive_cmds='$echo "LIBRARY $libname INITINSTANCE" > $output_objdir/$libname.def~$echo "DESCRIPTION \"$libname\"" >> $output_objdir/$libname.def~$echo DATA >> $output_objdir/$libname.def~$echo " SINGLE NONSHARED" >> $output_objdir/$libname.def~$echo EXPORTS >> $output_objdir/$libname.def~emxexp $libobjs >> $output_objdir/$libname.def~$CC -Zdll -Zcrtdll -o $lib $libobjs $deplibs $compiler_flags $output_objdir/$libname.def'
    old_archive_from_new_cmds='emximp -o $output_objdir/$libname.a $output_objdir/$libname.def'
    ;;

  osf3*)
    if test "$GCC" = yes; then
      allow_undefined_flag=' ${wl}-expect_unresolved ${wl}\*'
      archive_cmds='$CC -shared${allow_undefined_flag} $libobjs $deplibs $compiler_flags ${wl}-soname ${wl}$soname `test -n "$verstring" && echo ${wl}-set_version ${wl}$verstring` ${wl}-update_registry ${wl}${output_objdir}/so_locations -o $lib'
    else
      allow_undefined_flag=' -expect_unresolved \*'
      archive_cmds='$LD -shared${allow_undefined_flag} $libobjs $deplibs $linker_flags -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${output_objdir}/so_locations -o $lib'
    fi
    hardcode_libdir_flag_spec='${wl}-rpath ${wl}$libdir'
    hardcode_libdir_separator=:
    ;;

  osf4* | osf5*)	# as osf3* with the addition of -msym flag
    if test "$GCC" = yes; then
      allow_undefined_flag=' ${wl}-expect_unresolved ${wl}\*'
      archive_cmds='$CC -shared${allow_undefined_flag} $libobjs $deplibs $compiler_flags ${wl}-msym ${wl}-soname ${wl}$soname `test -n "$verstring" && echo ${wl}-set_version ${wl}$verstring` ${wl}-update_registry ${wl}${output_objdir}/so_locations -o $lib'
      hardcode_libdir_flag_spec='${wl}-rpath ${wl}$libdir'
    else
      allow_undefined_flag=' -expect_unresolved \*'
      archive_cmds='$LD -shared${allow_undefined_flag} $libobjs $deplibs $linker_flags -msym -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${output_objdir}/so_locations -o $lib'
      archive_expsym_cmds='for i in `cat $export_symbols`; do printf "-exported_symbol " >> $lib.exp; echo "\$i" >> $lib.exp; done; echo "-hidden">> $lib.exp~
      $LD -shared${allow_undefined_flag} -input $lib.exp $linker_flags $libobjs $deplibs -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${objdir}/so_locations -o $lib~$rm $lib.exp'

      #Both c and cxx compiler support -rpath directly
      hardcode_libdir_flag_spec='-rpath $libdir'
    fi
    hardcode_libdir_separator=:
    ;;

  sco3.2v5*)
    archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
    hardcode_shlibpath_var=no
    runpath_var=LD_RUN_PATH
    hardcode_runpath_var=yes
    export_dynamic_flag_spec='${wl}-Bexport'
    ;;

  solaris*)
    # gcc --version < 3.0 without binutils cannot create self contained
    # shared libraries reliably, requiring libgcc.a to resolve some of
    # the object symbols generated in some cases.  Libraries that use
    # assert need libgcc.a to resolve __eprintf, for example.  Linking
    # a copy of libgcc.a into every shared library to guarantee resolving
    # such symbols causes other problems:  According to Tim Van Holder
    # <tim.van.holder@pandora.be>, C++ libraries end up with a separate
    # (to the application) exception stack for one thing.
    no_undefined_flag=' -z defs'
    if test "$GCC" = yes; then
      case `$CC --version 2>/dev/null` in
      [[12]].*)
	cat <<EOF 1>&2

*** Warning: Releases of GCC earlier than version 3.0 cannot reliably
*** create self contained shared libraries on Solaris systems, without
*** introducing a dependency on libgcc.a.  Therefore, libtool is disabling
*** -no-undefined support, which will at least allow you to build shared
*** libraries.  However, you may find that when you link such libraries
*** into an application without using GCC, you have to manually add
*** \`gcc --print-libgcc-file-name\` to the link command.  We urge you to
*** upgrade to a newer version of GCC.  Another option is to rebuild your
*** current GCC to use the GNU linker from GNU binutils 2.9.1 or newer.

EOF
        no_undefined_flag=
	;;
      esac
    fi
    # $CC -shared without GNU ld will not create a library from C++
    # object files and a static libstdc++, better avoid it by now
    archive_cmds='$LD -G${allow_undefined_flag} -h $soname -o $lib $libobjs $deplibs $linker_flags'
    archive_expsym_cmds='$echo "{ global:" > $lib.exp~cat $export_symbols | sed -e "s/\(.*\)/\1;/" >> $lib.exp~$echo "local: *; };" >> $lib.exp~
		$LD -G${allow_undefined_flag} -M $lib.exp -h $soname -o $lib $libobjs $deplibs $linker_flags~$rm $lib.exp'
    hardcode_libdir_flag_spec='-R$libdir'
    hardcode_shlibpath_var=no
    case $host_os in
    solaris2.[[0-5]] | solaris2.[[0-5]].*) ;;
    *) # Supported since Solaris 2.6 (maybe 2.5.1?)
      whole_archive_flag_spec='-z allextract$convenience -z defaultextract' ;;
    esac
    link_all_deplibs=yes
    ;;

  sunos4*)
    if test "x$host_vendor" = xsequent; then
      # Use $CC to link under sequent, because it throws in some extra .o
      # files that make .init and .fini sections work.
      archive_cmds='$CC -G ${wl}-h $soname -o $lib $libobjs $deplibs $compiler_flags'
    else
      archive_cmds='$LD -assert pure-text -Bstatic -o $lib $libobjs $deplibs $linker_flags'
    fi
    hardcode_libdir_flag_spec='-L$libdir'
    hardcode_direct=yes
    hardcode_minus_L=yes
    hardcode_shlibpath_var=no
    ;;

  sysv4)
    if test "x$host_vendor" = xsno; then
      archive_cmds='$LD -G -Bsymbolic -h $soname -o $lib $libobjs $deplibs $linker_flags'
      hardcode_direct=yes # is this really true???
    else
      archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
      hardcode_direct=no #Motorola manual says yes, but my tests say they lie
    fi
    runpath_var='LD_RUN_PATH'
    hardcode_shlibpath_var=no
    ;;

  sysv4.3*)
    archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
    hardcode_shlibpath_var=no
    export_dynamic_flag_spec='-Bexport'
    ;;

  sysv5*)
    no_undefined_flag=' -z text'
    # $CC -shared without GNU ld will not create a library from C++
    # object files and a static libstdc++, better avoid it by now
    archive_cmds='$LD -G${allow_undefined_flag} -h $soname -o $lib $libobjs $deplibs $linker_flags'
    archive_expsym_cmds='$echo "{ global:" > $lib.exp~cat $export_symbols | sed -e "s/\(.*\)/\1;/" >> $lib.exp~$echo "local: *; };" >> $lib.exp~
		$LD -G${allow_undefined_flag} -M $lib.exp -h $soname -o $lib $libobjs $deplibs $linker_flags~$rm $lib.exp'
    hardcode_libdir_flag_spec=
    hardcode_shlibpath_var=no
    runpath_var='LD_RUN_PATH'
    ;;

  uts4*)
    archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
    hardcode_libdir_flag_spec='-L$libdir'
    hardcode_shlibpath_var=no
    ;;

  dgux*)
    archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
    hardcode_libdir_flag_spec='-L$libdir'
    hardcode_shlibpath_var=no
    ;;

  sysv4*MP*)
    if test -d /usr/nec; then
      archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
      hardcode_shlibpath_var=no
      runpath_var=LD_RUN_PATH
      hardcode_runpath_var=yes
      ld_shlibs=yes
    fi
    ;;

  sysv4.2uw2*)
    archive_cmds='$LD -G -o $lib $libobjs $deplibs $linker_flags'
    hardcode_direct=yes
    hardcode_minus_L=no
    hardcode_shlibpath_var=no
    hardcode_runpath_var=yes
    runpath_var=LD_RUN_PATH
    ;;

  sysv5uw7* | unixware7*)
    no_undefined_flag='${wl}-z ${wl}text'
    if test "$GCC" = yes; then
      archive_cmds='$CC -shared ${wl}-h ${wl}$soname -o $lib $libobjs $deplibs $compiler_flags'
    else
      archive_cmds='$CC -G ${wl}-h ${wl}$soname -o $lib $libobjs $deplibs $compiler_flags'
    fi
    runpath_var='LD_RUN_PATH'
    hardcode_shlibpath_var=no
    ;;

  *)
    ld_shlibs=no
    ;;
  esac
fi
AC_MSG_RESULT([$ld_shlibs])
test "$ld_shlibs" = no && can_build_shared=no

# Check hardcoding attributes.
AC_MSG_CHECKING([how to hardcode library paths into programs])
hardcode_action=
if test -n "$hardcode_libdir_flag_spec" || \
   test -n "$runpath_var"; then

  # We can hardcode non-existant directories.
  if test "$hardcode_direct" != no &&
     # If the only mechanism to avoid hardcoding is shlibpath_var, we
     # have to relink, otherwise we might link with an installed library
     # when we should be linking with a yet-to-be-installed one
     ## test "$hardcode_shlibpath_var" != no &&
     test "$hardcode_minus_L" != no; then
    # Linking always hardcodes the temporary library directory.
    hardcode_action=relink
  else
    # We can link without hardcoding, and we can hardcode nonexisting dirs.
    hardcode_action=immediate
  fi
else
  # We cannot hardcode anything, or else we can only hardcode existing
  # directories.
  hardcode_action=unsupported
fi
AC_MSG_RESULT([$hardcode_action])

striplib=
old_striplib=
AC_MSG_CHECKING([whether stripping libraries is possible])
if test -n "$STRIP" && $STRIP -V 2>&1 | grep "GNU strip" >/dev/null; then
  test -z "$old_striplib" && old_striplib="$STRIP --strip-debug"
  test -z "$striplib" && striplib="$STRIP --strip-unneeded"
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
fi

reload_cmds='$LD$reload_flag -o $output$reload_objs'
test -z "$deplibs_check_method" && deplibs_check_method=unknown

# PORTME Fill in your ld.so characteristics
AC_MSG_CHECKING([dynamic linker characteristics])
library_names_spec=
libname_spec='lib$name'
soname_spec=
postinstall_cmds=
postuninstall_cmds=
finish_cmds=
finish_eval=
shlibpath_var=
shlibpath_overrides_runpath=unknown
version_type=none
dynamic_linker="$host_os ld.so"
sys_lib_dlsearch_path_spec="/lib /usr/lib"
sys_lib_search_path_spec="/lib /usr/lib /usr/local/lib"

case $host_os in
aix3*)
  version_type=linux
  library_names_spec='${libname}${release}.so$versuffix $libname.a'
  shlibpath_var=LIBPATH

  # AIX has no versioning support, so we append a major version to the name.
  soname_spec='${libname}${release}.so$major'
  ;;

aix4* | aix5*)
  version_type=linux
  if test "$host_cpu" = ia64; then
    # AIX 5 supports IA64
    library_names_spec='${libname}${release}.so$major ${libname}${release}.so$versuffix $libname.so'
    shlibpath_var=LD_LIBRARY_PATH
  else
    # With GCC up to 2.95.x, collect2 would create an import file
    # for dependence libraries.  The import file would start with
    # the line `#! .'.  This would cause the generated library to
    # depend on `.', always an invalid library.  This was fixed in
    # development snapshots of GCC prior to 3.0.
    case $host_os in
      aix4 | aix4.[[01]] | aix4.[[01]].*)
	if { echo '#if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 97)'
	     echo ' yes '
	     echo '#endif'; } | ${CC} -E - | grep yes > /dev/null; then
	  :
	else
	  can_build_shared=no
	fi
	;;
    esac
    # AIX (on Power*) has no versioning support, so currently we can
    # not hardcode correct soname into executable. Probably we can
    # add versioning support to collect2, so additional links can
    # be useful in future.
    if test "$aix_use_runtimelinking" = yes; then
      # If using run time linking (on AIX 4.2 or later) use lib<name>.so
      # instead of lib<name>.a to let people know that these are not
      # typical AIX shared libraries.
      library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
    else
      # We preserve .a as extension for shared libraries through AIX4.2
      # and later when we are not doing run time linking.
      library_names_spec='${libname}${release}.a $libname.a'
      soname_spec='${libname}${release}.so$major'
    fi
    shlibpath_var=LIBPATH
  fi
  ;;

amigaos*)
  library_names_spec='$libname.ixlibrary $libname.a'
  # Create ${libname}_ixlibrary.a entries in /sys/libs.
  finish_eval='for lib in `ls $libdir/*.ixlibrary 2>/dev/null`; do libname=`$echo "X$lib" | $Xsed -e '\''s%^.*/\([[^/]]*\)\.ixlibrary$%\1%'\''`; test $rm /sys/libs/${libname}_ixlibrary.a; $show "(cd /sys/libs && $LN_S $lib ${libname}_ixlibrary.a)"; (cd /sys/libs && $LN_S $lib ${libname}_ixlibrary.a) || exit 1; done'
  ;;

beos*)
  library_names_spec='${libname}.so'
  dynamic_linker="$host_os ld.so"
  shlibpath_var=LIBRARY_PATH
  ;;

bsdi4*)
  version_type=linux
  need_version=no
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
  soname_spec='${libname}${release}.so$major'
  finish_cmds='PATH="\$PATH:/sbin" ldconfig $libdir'
  shlibpath_var=LD_LIBRARY_PATH
  sys_lib_search_path_spec="/shlib /usr/lib /usr/X11/lib /usr/contrib/lib /lib /usr/local/lib"
  sys_lib_dlsearch_path_spec="/shlib /usr/lib /usr/local/lib"
  export_dynamic_flag_spec=-rdynamic
  # the default ld.so.conf also contains /usr/contrib/lib and
  # /usr/X11R6/lib (/usr/X11 is a link to /usr/X11R6), but let us allow
  # libtool to hard-code these into programs
  ;;

cygwin* | mingw* | pw32*)
  version_type=windows
  need_version=no
  need_lib_prefix=no
  case $GCC,$host_os in
  yes,cygwin*)
    library_names_spec='$libname.dll.a'
    soname_spec='`echo ${libname} | sed -e 's/^lib/cyg/'``echo ${release} | sed -e 's/[[.]]/-/g'`${versuffix}.dll'
    postinstall_cmds='dlpath=`bash 2>&1 -c '\''. $dir/${file}i;echo \$dlname'\''`~
      dldir=$destdir/`dirname \$dlpath`~
      test -d \$dldir || mkdir -p \$dldir~
      $install_prog .libs/$dlname \$dldir/$dlname'
    postuninstall_cmds='dldll=`bash 2>&1 -c '\''. $file; echo \$dlname'\''`~
      dlpath=$dir/\$dldll~
       $rm \$dlpath'
    ;;
  yes,mingw*)
    library_names_spec='${libname}`echo ${release} | sed -e 's/[[.]]/-/g'`${versuffix}.dll'
    sys_lib_search_path_spec=`$CC -print-search-dirs | grep "^libraries:" | sed -e "s/^libraries://" -e "s/;/ /g"`
    ;;
  yes,pw32*)
    library_names_spec='`echo ${libname} | sed -e 's/^lib/pw/'``echo ${release} | sed -e 's/[.]/-/g'`${versuffix}.dll'
    ;;
  *)
    library_names_spec='${libname}`echo ${release} | sed -e 's/[[.]]/-/g'`${versuffix}.dll $libname.lib'
    ;;
  esac
  dynamic_linker='Win32 ld.exe'
  # FIXME: first we should search . and the directory the executable is in
  shlibpath_var=PATH
  ;;

darwin* | rhapsody*)
  dynamic_linker="$host_os dyld"
  version_type=darwin
  need_lib_prefix=no
  need_version=no
  # FIXME: Relying on posixy $() will cause problems for
  #        cross-compilation, but unfortunately the echo tests do not
  #        yet detect zsh echo's removal of \ escapes.
  library_names_spec='${libname}${release}${versuffix}.$(test .$module = .yes && echo so || echo dylib) ${libname}${release}${major}.$(test .$module = .yes && echo so || echo dylib) ${libname}.$(test .$module = .yes && echo so || echo dylib)'
  soname_spec='${libname}${release}${major}.$(test .$module = .yes && echo so || echo dylib)'
  shlibpath_overrides_runpath=yes
  shlibpath_var=DYLD_LIBRARY_PATH
  ;;

freebsd1*)
  dynamic_linker=no
  ;;

freebsd*)
  objformat=`test -x /usr/bin/objformat && /usr/bin/objformat || echo aout`
  version_type=freebsd-$objformat
  case $version_type in
    freebsd-elf*)
      library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so $libname.so'
      need_version=no
      need_lib_prefix=no
      ;;
    freebsd-*)
      library_names_spec='${libname}${release}.so$versuffix $libname.so$versuffix'
      need_version=yes
      ;;
  esac
  shlibpath_var=LD_LIBRARY_PATH
  case $host_os in
  freebsd2*)
    shlibpath_overrides_runpath=yes
    ;;
  *)
    shlibpath_overrides_runpath=no
    hardcode_into_libs=yes
    ;;
  esac
  ;;

gnu*)
  version_type=linux
  need_lib_prefix=no
  need_version=no
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so${major} ${libname}.so'
  soname_spec='${libname}${release}.so$major'
  shlibpath_var=LD_LIBRARY_PATH
  hardcode_into_libs=yes
  ;;

hpux9* | hpux10* | hpux11*)
  # Give a soname corresponding to the major version so that dld.sl refuses to
  # link against other versions.
  dynamic_linker="$host_os dld.sl"
  version_type=sunos
  need_lib_prefix=no
  need_version=no
  shlibpath_var=SHLIB_PATH
  shlibpath_overrides_runpath=no # +s is required to enable SHLIB_PATH
  library_names_spec='${libname}${release}.sl$versuffix ${libname}${release}.sl$major $libname.sl'
  soname_spec='${libname}${release}.sl$major'
  # HP-UX runs *really* slowly unless shared libraries are mode 555.
  postinstall_cmds='chmod 555 $lib'
  ;;

irix5* | irix6*)
  version_type=irix
  need_lib_prefix=no
  need_version=no
  soname_spec='${libname}${release}.so$major'
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major ${libname}${release}.so $libname.so'
  case $host_os in
  irix5*)
    libsuff= shlibsuff=
    ;;
  *)
    case $LD in # libtool.m4 will add one of these switches to LD
    *-32|*"-32 ") libsuff= shlibsuff= libmagic=32-bit;;
    *-n32|*"-n32 ") libsuff=32 shlibsuff=N32 libmagic=N32;;
    *-64|*"-64 ") libsuff=64 shlibsuff=64 libmagic=64-bit;;
    *) libsuff= shlibsuff= libmagic=never-match;;
    esac
    ;;
  esac
  shlibpath_var=LD_LIBRARY${shlibsuff}_PATH
  shlibpath_overrides_runpath=no
  sys_lib_search_path_spec="/usr/lib${libsuff} /lib${libsuff} /usr/local/lib${libsuff}"
  sys_lib_dlsearch_path_spec="/usr/lib${libsuff} /lib${libsuff}"
  ;;

# No shared lib support for Linux oldld, aout, or coff.
linux-gnuoldld* | linux-gnuaout* | linux-gnucoff*)
  dynamic_linker=no
  ;;

# This must be Linux ELF.
linux-gnu*)
  version_type=linux
  need_lib_prefix=no
  need_version=no
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
  soname_spec='${libname}${release}.so$major'
  finish_cmds='PATH="\$PATH:/sbin" ldconfig -n $libdir'
  shlibpath_var=LD_LIBRARY_PATH
  shlibpath_overrides_runpath=no
  # This implies no fast_install, which is unacceptable.
  # Some rework will be needed to allow for fast_install
  # before this can be enabled.
  hardcode_into_libs=yes

  # We used to test for /lib/ld.so.1 and disable shared libraries on
  # powerpc, because MkLinux only supported shared libraries with the
  # GNU dynamic linker.  Since this was broken with cross compilers,
  # most powerpc-linux boxes support dynamic linking these days and
  # people can always --disable-shared, the test was removed, and we
  # assume the GNU/Linux dynamic linker is in use.
  dynamic_linker='GNU/Linux ld.so'
  ;;

netbsd*)
  version_type=sunos
  need_lib_prefix=no
  need_version=no
  if echo __ELF__ | $CC -E - | grep __ELF__ >/dev/null; then
    library_names_spec='${libname}${release}.so$versuffix ${libname}.so$versuffix'
    finish_cmds='PATH="\$PATH:/sbin" ldconfig -m $libdir'
    dynamic_linker='NetBSD (a.out) ld.so'
  else
    library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major ${libname}${release}.so ${libname}.so'
    soname_spec='${libname}${release}.so$major'
    dynamic_linker='NetBSD ld.elf_so'
  fi
  shlibpath_var=LD_LIBRARY_PATH
  shlibpath_overrides_runpath=yes
  hardcode_into_libs=yes
  ;;

newsos6)
  version_type=linux
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
  shlibpath_var=LD_LIBRARY_PATH
  shlibpath_overrides_runpath=yes
  ;;

openbsd*)
  version_type=sunos
  need_lib_prefix=no
  need_version=no
  if test -z "`echo __ELF__ | $CC -E - | grep __ELF__`" || test "$host_os-$host_cpu" = "openbsd2.8-powerpc"; then
    case "$host_os" in
    openbsd2.[[89]] | openbsd2.[[89]].*)
      shlibpath_overrides_runpath=no
      ;;
    *)
      shlibpath_overrides_runpath=yes
      ;;
    esac
  else
    shlibpath_overrides_runpath=yes
  fi
  library_names_spec='${libname}${release}.so$versuffix ${libname}.so$versuffix'
  finish_cmds='PATH="\$PATH:/sbin" ldconfig -m $libdir'
  shlibpath_var=LD_LIBRARY_PATH
  ;;

os2*)
  libname_spec='$name'
  need_lib_prefix=no
  library_names_spec='$libname.dll $libname.a'
  dynamic_linker='OS/2 ld.exe'
  shlibpath_var=LIBPATH
  ;;

osf3* | osf4* | osf5*)
  version_type=osf
  need_version=no
  soname_spec='${libname}${release}.so'
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so $libname.so'
  shlibpath_var=LD_LIBRARY_PATH
  sys_lib_search_path_spec="/usr/shlib /usr/ccs/lib /usr/lib/cmplrs/cc /usr/lib /usr/local/lib /var/shlib"
  sys_lib_dlsearch_path_spec="$sys_lib_search_path_spec"
  ;;

sco3.2v5*)
  version_type=osf
  soname_spec='${libname}${release}.so$major'
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
  shlibpath_var=LD_LIBRARY_PATH
  ;;

solaris*)
  version_type=linux
  need_lib_prefix=no
  need_version=no
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
  soname_spec='${libname}${release}.so$major'
  shlibpath_var=LD_LIBRARY_PATH
  shlibpath_overrides_runpath=yes
  hardcode_into_libs=yes
  # ldd complains unless libraries are executable
  postinstall_cmds='chmod +x $lib'
  ;;

sunos4*)
  version_type=sunos
  library_names_spec='${libname}${release}.so$versuffix ${libname}.so$versuffix'
  finish_cmds='PATH="\$PATH:/usr/etc" ldconfig $libdir'
  shlibpath_var=LD_LIBRARY_PATH
  shlibpath_overrides_runpath=yes
  if test "$with_gnu_ld" = yes; then
    need_lib_prefix=no
  fi
  need_version=yes
  ;;

sysv4 | sysv4.2uw2* | sysv4.3* | sysv5*)
  version_type=linux
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
  soname_spec='${libname}${release}.so$major'
  shlibpath_var=LD_LIBRARY_PATH
  case $host_vendor in
    sni)
      shlibpath_overrides_runpath=no
      ;;
    motorola)
      need_lib_prefix=no
      need_version=no
      shlibpath_overrides_runpath=no
      sys_lib_search_path_spec='/lib /usr/lib /usr/ccs/lib'
      ;;
  esac
  ;;

uts4*)
  version_type=linux
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
  soname_spec='${libname}${release}.so$major'
  shlibpath_var=LD_LIBRARY_PATH
  ;;

dgux*)
  version_type=linux
  need_lib_prefix=no
  need_version=no
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
  soname_spec='${libname}${release}.so$major'
  shlibpath_var=LD_LIBRARY_PATH
  ;;

sysv4*MP*)
  if test -d /usr/nec ;then
    version_type=linux
    library_names_spec='$libname.so.$versuffix $libname.so.$major $libname.so'
    soname_spec='$libname.so.$major'
    shlibpath_var=LD_LIBRARY_PATH
  fi
  ;;

*)
  dynamic_linker=no
  ;;
esac
AC_MSG_RESULT([$dynamic_linker])
test "$dynamic_linker" = no && can_build_shared=no

# Report the final consequences.
AC_MSG_CHECKING([if libtool supports shared libraries])
AC_MSG_RESULT([$can_build_shared])

AC_MSG_CHECKING([whether to build shared libraries])
test "$can_build_shared" = "no" && enable_shared=no

# On AIX, shared libraries and static libraries use the same namespace, and
# are all built from PIC.
case "$host_os" in
aix3*)
  test "$enable_shared" = yes && enable_static=no
  if test -n "$RANLIB"; then
    archive_cmds="$archive_cmds~\$RANLIB \$lib"
    postinstall_cmds='$RANLIB $lib'
  fi
  ;;

aix4*)
  if test "$host_cpu" != ia64 && test "$aix_use_runtimelinking" = no ; then
    test "$enable_shared" = yes && enable_static=no
  fi
  ;;
esac
AC_MSG_RESULT([$enable_shared])

AC_MSG_CHECKING([whether to build static libraries])
# Make sure either enable_shared or enable_static is yes.
test "$enable_shared" = yes || enable_static=yes
AC_MSG_RESULT([$enable_static])

if test "$hardcode_action" = relink; then
  # Fast installation is not supported
  enable_fast_install=no
elif test "$shlibpath_overrides_runpath" = yes ||
     test "$enable_shared" = no; then
  # Fast installation is not necessary
  enable_fast_install=needless
fi

variables_saved_for_relink="PATH $shlibpath_var $runpath_var"
if test "$GCC" = yes; then
  variables_saved_for_relink="$variables_saved_for_relink GCC_EXEC_PREFIX COMPILER_PATH LIBRARY_PATH"
fi

AC_LIBTOOL_DLOPEN_SELF

if test "$enable_shared" = yes && test "$GCC" = yes; then
  case $archive_cmds in
  *'~'*)
    # FIXME: we may have to deal with multi-command sequences.
    ;;
  '$CC '*)
    # Test whether the compiler implicitly links with -lc since on some
    # systems, -lgcc has to come before -lc. If gcc already passes -lc
    # to ld, don't add -lc before -lgcc.
    AC_MSG_CHECKING([whether -lc should be explicitly linked in])
    AC_CACHE_VAL([lt_cv_archive_cmds_need_lc],
    [$rm conftest*
    echo 'static int dummy;' > conftest.$ac_ext

    if AC_TRY_EVAL(ac_compile); then
      soname=conftest
      lib=conftest
      libobjs=conftest.$ac_objext
      deplibs=
      wl=$lt_cv_prog_cc_wl
      compiler_flags=-v
      linker_flags=-v
      verstring=
      output_objdir=.
      libname=conftest
      save_allow_undefined_flag=$allow_undefined_flag
      allow_undefined_flag=
      if AC_TRY_EVAL(archive_cmds 2\>\&1 \| grep \" -lc \" \>/dev/null 2\>\&1)
      then
	lt_cv_archive_cmds_need_lc=no
      else
	lt_cv_archive_cmds_need_lc=yes
      fi
      allow_undefined_flag=$save_allow_undefined_flag
    else
      cat conftest.err 1>&5
    fi])
    AC_MSG_RESULT([$lt_cv_archive_cmds_need_lc])
    ;;
  esac
fi
need_lc=${lt_cv_archive_cmds_need_lc-yes}

# The second clause should only fire when bootstrapping the
# libtool distribution, otherwise you forgot to ship ltmain.sh
# with your package, and you will get complaints that there are
# no rules to generate ltmain.sh.
if test -f "$ltmain"; then
  :
else
  # If there is no Makefile yet, we rely on a make rule to execute
  # `config.status --recheck' to rerun these tests and create the
  # libtool script then.
  test -f Makefile && make "$ltmain"
fi

if test -f "$ltmain"; then
  trap "$rm \"${ofile}T\"; exit 1" 1 2 15
  $rm -f "${ofile}T"

  echo creating $ofile

  # Now quote all the things that may contain metacharacters while being
  # careful not to overquote the AC_SUBSTed values.  We take copies of the
  # variables and quote the copies for generation of the libtool script.
  for var in echo old_CC old_CFLAGS \
    AR AR_FLAGS CC LD LN_S NM SHELL \
    reload_flag reload_cmds wl \
    pic_flag link_static_flag no_builtin_flag export_dynamic_flag_spec \
    thread_safe_flag_spec whole_archive_flag_spec libname_spec \
    library_names_spec soname_spec \
    RANLIB old_archive_cmds old_archive_from_new_cmds old_postinstall_cmds \
    old_postuninstall_cmds archive_cmds archive_expsym_cmds postinstall_cmds \
    postuninstall_cmds extract_expsyms_cmds old_archive_from_expsyms_cmds \
    old_striplib striplib file_magic_cmd export_symbols_cmds \
    deplibs_check_method allow_undefined_flag no_undefined_flag \
    finish_cmds finish_eval global_symbol_pipe global_symbol_to_cdecl \
    global_symbol_to_c_name_address \
    hardcode_libdir_flag_spec hardcode_libdir_separator  \
    sys_lib_search_path_spec sys_lib_dlsearch_path_spec \
    compiler_c_o compiler_o_lo need_locks exclude_expsyms include_expsyms; do

    case $var in
    reload_cmds | old_archive_cmds | old_archive_from_new_cmds | \
    old_postinstall_cmds | old_postuninstall_cmds | \
    export_symbols_cmds | archive_cmds | archive_expsym_cmds | \
    extract_expsyms_cmds | old_archive_from_expsyms_cmds | \
    postinstall_cmds | postuninstall_cmds | \
    finish_cmds | sys_lib_search_path_spec | sys_lib_dlsearch_path_spec)
      # Double-quote double-evaled strings.
      eval "lt_$var=\\\"\`\$echo \"X\$$var\" | \$Xsed -e \"\$double_quote_subst\" -e \"\$sed_quote_subst\" -e \"\$delay_variable_subst\"\`\\\""
      ;;
    *)
      eval "lt_$var=\\\"\`\$echo \"X\$$var\" | \$Xsed -e \"\$sed_quote_subst\"\`\\\""
      ;;
    esac
  done

  cat <<__EOF__ > "${ofile}T"
#! $SHELL

# `$echo "$ofile" | sed 's%^.*/%%'` - Provide generalized library-building support services.
# Generated automatically by $PROGRAM (GNU $PACKAGE $VERSION$TIMESTAMP)
# NOTE: Changes made to this file will be lost: look at ltmain.sh.
#
# Copyright (C) 1996-2000 Free Software Foundation, Inc.
# Originally by Gordon Matzigkeit <gord@gnu.ai.mit.edu>, 1996
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#
# As a special exception to the GNU General Public License, if you
# distribute this file as part of a program that contains a
# configuration script generated by Autoconf, you may include it under
# the same distribution terms that you use for the rest of that program.

# Sed that helps us avoid accidentally triggering echo(1) options like -n.
Xsed="sed -e s/^X//"

# The HP-UX ksh and POSIX shell print the target directory to stdout
# if CDPATH is set.
if test "X\${CDPATH+set}" = Xset; then CDPATH=:; export CDPATH; fi

# ### BEGIN LIBTOOL CONFIG

# Libtool was configured on host `(hostname || uname -n) 2>/dev/null | sed 1q`:

# Shell to use when invoking shell scripts.
SHELL=$lt_SHELL

# Whether or not to build shared libraries.
build_libtool_libs=$enable_shared

# Whether or not to build static libraries.
build_old_libs=$enable_static

# Whether or not to add -lc for building shared libraries.
build_libtool_need_lc=$need_lc

# Whether or not to optimize for fast installation.
fast_install=$enable_fast_install

# The host system.
host_alias=$host_alias
host=$host

# An echo program that does not interpret backslashes.
echo=$lt_echo

# The archiver.
AR=$lt_AR
AR_FLAGS=$lt_AR_FLAGS

# The default C compiler.
CC=$lt_CC

# Is the compiler the GNU C compiler?
with_gcc=$GCC

# The linker used to build libraries.
LD=$lt_LD

# Whether we need hard or soft links.
LN_S=$lt_LN_S

# A BSD-compatible nm program.
NM=$lt_NM

# A symbol stripping program
STRIP=$STRIP

# Used to examine libraries when file_magic_cmd begins "file"
MAGIC_CMD=$MAGIC_CMD

# Used on cygwin: DLL creation program.
DLLTOOL="$DLLTOOL"

# Used on cygwin: object dumper.
OBJDUMP="$OBJDUMP"

# Used on cygwin: assembler.
AS="$AS"

# The name of the directory that contains temporary libtool files.
objdir=$objdir

# How to create reloadable object files.
reload_flag=$lt_reload_flag
reload_cmds=$lt_reload_cmds

# How to pass a linker flag through the compiler.
wl=$lt_wl

# Object file suffix (normally "o").
objext="$ac_objext"

# Old archive suffix (normally "a").
libext="$libext"

# Executable file suffix (normally "").
exeext="$exeext"

# Additional compiler flags for building library objects.
pic_flag=$lt_pic_flag
pic_mode=$pic_mode

# Does compiler simultaneously support -c and -o options?
compiler_c_o=$lt_compiler_c_o

# Can we write directly to a .lo ?
compiler_o_lo=$lt_compiler_o_lo

# Must we lock files when doing compilation ?
need_locks=$lt_need_locks

# Do we need the lib prefix for modules?
need_lib_prefix=$need_lib_prefix

# Do we need a version for libraries?
need_version=$need_version

# Whether dlopen is supported.
dlopen_support=$enable_dlopen

# Whether dlopen of programs is supported.
dlopen_self=$enable_dlopen_self

# Whether dlopen of statically linked programs is supported.
dlopen_self_static=$enable_dlopen_self_static

# Compiler flag to prevent dynamic linking.
link_static_flag=$lt_link_static_flag

# Compiler flag to turn off builtin functions.
no_builtin_flag=$lt_no_builtin_flag

# Compiler flag to allow reflexive dlopens.
export_dynamic_flag_spec=$lt_export_dynamic_flag_spec

# Compiler flag to generate shared objects directly from archives.
whole_archive_flag_spec=$lt_whole_archive_flag_spec

# Compiler flag to generate thread-safe objects.
thread_safe_flag_spec=$lt_thread_safe_flag_spec

# Library versioning type.
version_type=$version_type

# Format of library name prefix.
libname_spec=$lt_libname_spec

# List of archive names.  First name is the real one, the rest are links.
# The last name is the one that the linker finds with -lNAME.
library_names_spec=$lt_library_names_spec

# The coded name of the library, if different from the real name.
soname_spec=$lt_soname_spec

# Commands used to build and install an old-style archive.
RANLIB=$lt_RANLIB
old_archive_cmds=$lt_old_archive_cmds
old_postinstall_cmds=$lt_old_postinstall_cmds
old_postuninstall_cmds=$lt_old_postuninstall_cmds

# Create an old-style archive from a shared archive.
old_archive_from_new_cmds=$lt_old_archive_from_new_cmds

# Create a temporary old-style archive to link instead of a shared archive.
old_archive_from_expsyms_cmds=$lt_old_archive_from_expsyms_cmds

# Commands used to build and install a shared archive.
archive_cmds=$lt_archive_cmds
archive_expsym_cmds=$lt_archive_expsym_cmds
postinstall_cmds=$lt_postinstall_cmds
postuninstall_cmds=$lt_postuninstall_cmds

# Commands to strip libraries.
old_striplib=$lt_old_striplib
striplib=$lt_striplib

# Method to check whether dependent libraries are shared objects.
deplibs_check_method=$lt_deplibs_check_method

# Command to use when deplibs_check_method == file_magic.
file_magic_cmd=$lt_file_magic_cmd

# Flag that allows shared libraries with undefined symbols to be built.
allow_undefined_flag=$lt_allow_undefined_flag

# Flag that forces no undefined symbols.
no_undefined_flag=$lt_no_undefined_flag

# Commands used to finish a libtool library installation in a directory.
finish_cmds=$lt_finish_cmds

# Same as above, but a single script fragment to be evaled but not shown.
finish_eval=$lt_finish_eval

# Take the output of nm and produce a listing of raw symbols and C names.
global_symbol_pipe=$lt_global_symbol_pipe

# Transform the output of nm in a proper C declaration
global_symbol_to_cdecl=$lt_global_symbol_to_cdecl

# Transform the output of nm in a C name address pair
global_symbol_to_c_name_address=$lt_global_symbol_to_c_name_address

# This is the shared library runtime path variable.
runpath_var=$runpath_var

# This is the shared library path variable.
shlibpath_var=$shlibpath_var

# Is shlibpath searched before the hard-coded library search path?
shlibpath_overrides_runpath=$shlibpath_overrides_runpath

# How to hardcode a shared library path into an executable.
hardcode_action=$hardcode_action

# Whether we should hardcode library paths into libraries.
hardcode_into_libs=$hardcode_into_libs

# Flag to hardcode \$libdir into a binary during linking.
# This must work even if \$libdir does not exist.
hardcode_libdir_flag_spec=$lt_hardcode_libdir_flag_spec

# Whether we need a single -rpath flag with a separated argument.
hardcode_libdir_separator=$lt_hardcode_libdir_separator

# Set to yes if using DIR/libNAME.so during linking hardcodes DIR into the
# resulting binary.
hardcode_direct=$hardcode_direct

# Set to yes if using the -LDIR flag during linking hardcodes DIR into the
# resulting binary.
hardcode_minus_L=$hardcode_minus_L

# Set to yes if using SHLIBPATH_VAR=DIR during linking hardcodes DIR into
# the resulting binary.
hardcode_shlibpath_var=$hardcode_shlibpath_var

# Variables whose values should be saved in libtool wrapper scripts and
# restored at relink time.
variables_saved_for_relink="$variables_saved_for_relink"

# Whether libtool must link a program against all its dependency libraries.
link_all_deplibs=$link_all_deplibs

# Compile-time system search path for libraries
sys_lib_search_path_spec=$lt_sys_lib_search_path_spec

# Run-time system search path for libraries
sys_lib_dlsearch_path_spec=$lt_sys_lib_dlsearch_path_spec

# Fix the shell variable \$srcfile for the compiler.
fix_srcfile_path="$fix_srcfile_path"

# Set to yes if exported symbols are required.
always_export_symbols=$always_export_symbols

# The commands to list exported symbols.
export_symbols_cmds=$lt_export_symbols_cmds

# The commands to extract the exported symbol list from a shared archive.
extract_expsyms_cmds=$lt_extract_expsyms_cmds

# Symbols that should not be listed in the preloaded symbols.
exclude_expsyms=$lt_exclude_expsyms

# Symbols that must always be exported.
include_expsyms=$lt_include_expsyms

# ### END LIBTOOL CONFIG

__EOF__

  case $host_os in
  aix3*)
    cat <<\EOF >> "${ofile}T"

# AIX sometimes has problems with the GCC collect2 program.  For some
# reason, if we set the COLLECT_NAMES environment variable, the problems
# vanish in a puff of smoke.
if test "X${COLLECT_NAMES+set}" != Xset; then
  COLLECT_NAMES=
  export COLLECT_NAMES
fi
EOF
    ;;
  esac

  case $host_os in
  cygwin* | mingw* | pw32* | os2*)
    cat <<'EOF' >> "${ofile}T"
      # This is a source program that is used to create dlls on Windows
      # Don't remove nor modify the starting and closing comments
# /* ltdll.c starts here */
# #define WIN32_LEAN_AND_MEAN
# #include <windows.h>
# #undef WIN32_LEAN_AND_MEAN
# #include <stdio.h>
#
# #ifndef __CYGWIN__
# #  ifdef __CYGWIN32__
# #    define __CYGWIN__ __CYGWIN32__
# #  endif
# #endif
#
# #ifdef __cplusplus
# extern "C" {
# #endif
# BOOL APIENTRY DllMain (HINSTANCE hInst, DWORD reason, LPVOID reserved);
# #ifdef __cplusplus
# }
# #endif
#
# #ifdef __CYGWIN__
# #include <cygwin/cygwin_dll.h>
# DECLARE_CYGWIN_DLL( DllMain );
# #endif
# HINSTANCE __hDllInstance_base;
#
# BOOL APIENTRY
# DllMain (HINSTANCE hInst, DWORD reason, LPVOID reserved)
# {
#   __hDllInstance_base = hInst;
#   return TRUE;
# }
# /* ltdll.c ends here */
	# This is a source program that is used to create import libraries
	# on Windows for dlls which lack them. Don't remove nor modify the
	# starting and closing comments
# /* impgen.c starts here */
# /*   Copyright (C) 1999-2000 Free Software Foundation, Inc.
#
#  This file is part of GNU libtool.
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#  */
#
# #include <stdio.h>		/* for printf() */
# #include <unistd.h>		/* for open(), lseek(), read() */
# #include <fcntl.h>		/* for O_RDONLY, O_BINARY */
# #include <string.h>		/* for strdup() */
#
# /* O_BINARY isn't required (or even defined sometimes) under Unix */
# #ifndef O_BINARY
# #define O_BINARY 0
# #endif
#
# static unsigned int
# pe_get16 (fd, offset)
#      int fd;
#      int offset;
# {
#   unsigned char b[2];
#   lseek (fd, offset, SEEK_SET);
#   read (fd, b, 2);
#   return b[0] + (b[1]<<8);
# }
#
# static unsigned int
# pe_get32 (fd, offset)
#     int fd;
#     int offset;
# {
#   unsigned char b[4];
#   lseek (fd, offset, SEEK_SET);
#   read (fd, b, 4);
#   return b[0] + (b[1]<<8) + (b[2]<<16) + (b[3]<<24);
# }
#
# static unsigned int
# pe_as32 (ptr)
#      void *ptr;
# {
#   unsigned char *b = ptr;
#   return b[0] + (b[1]<<8) + (b[2]<<16) + (b[3]<<24);
# }
#
# int
# main (argc, argv)
#     int argc;
#     char *argv[];
# {
#     int dll;
#     unsigned long pe_header_offset, opthdr_ofs, num_entries, i;
#     unsigned long export_rva, export_size, nsections, secptr, expptr;
#     unsigned long name_rvas, nexp;
#     unsigned char *expdata, *erva;
#     char *filename, *dll_name;
#
#     filename = argv[1];
#
#     dll = open(filename, O_RDONLY|O_BINARY);
#     if (dll < 1)
# 	return 1;
#
#     dll_name = filename;
#
#     for (i=0; filename[i]; i++)
# 	if (filename[i] == '/' || filename[i] == '\\'  || filename[i] == ':')
# 	    dll_name = filename + i +1;
#
#     pe_header_offset = pe_get32 (dll, 0x3c);
#     opthdr_ofs = pe_header_offset + 4 + 20;
#     num_entries = pe_get32 (dll, opthdr_ofs + 92);
#
#     if (num_entries < 1) /* no exports */
# 	return 1;
#
#     export_rva = pe_get32 (dll, opthdr_ofs + 96);
#     export_size = pe_get32 (dll, opthdr_ofs + 100);
#     nsections = pe_get16 (dll, pe_header_offset + 4 +2);
#     secptr = (pe_header_offset + 4 + 20 +
# 	      pe_get16 (dll, pe_header_offset + 4 + 16));
#
#     expptr = 0;
#     for (i = 0; i < nsections; i++)
#     {
# 	char sname[8];
# 	unsigned long secptr1 = secptr + 40 * i;
# 	unsigned long vaddr = pe_get32 (dll, secptr1 + 12);
# 	unsigned long vsize = pe_get32 (dll, secptr1 + 16);
# 	unsigned long fptr = pe_get32 (dll, secptr1 + 20);
# 	lseek(dll, secptr1, SEEK_SET);
# 	read(dll, sname, 8);
# 	if (vaddr <= export_rva && vaddr+vsize > export_rva)
# 	{
# 	    expptr = fptr + (export_rva - vaddr);
# 	    if (export_rva + export_size > vaddr + vsize)
# 		export_size = vsize - (export_rva - vaddr);
# 	    break;
# 	}
#     }
#
#     expdata = (unsigned char*)malloc(export_size);
#     lseek (dll, expptr, SEEK_SET);
#     read (dll, expdata, export_size);
#     erva = expdata - export_rva;
#
#     nexp = pe_as32 (expdata+24);
#     name_rvas = pe_as32 (expdata+32);
#
#     printf ("EXPORTS\n");
#     for (i = 0; i<nexp; i++)
#     {
# 	unsigned long name_rva = pe_as32 (erva+name_rvas+i*4);
# 	printf ("\t%s @ %ld ;\n", erva+name_rva, 1+ i);
#     }
#
#     return 0;
# }
# /* impgen.c ends here */

EOF
    ;;
  esac

  # We use sed instead of cat because bash on DJGPP gets confused if
  # if finds mixed CR/LF and LF-only lines.  Since sed operates in
  # text mode, it properly converts lines to CR/LF.  This bash problem
  # is reportedly fixed, but why not run on old versions too?
  sed '$q' "$ltmain" >> "${ofile}T" || (rm -f "${ofile}T"; exit 1)

  mv -f "${ofile}T" "$ofile" || \
    (rm -f "$ofile" && cp "${ofile}T" "$ofile" && rm -f "${ofile}T")
  chmod +x "$ofile"
fi

])# _LT_AC_LTCONFIG_HACK

# AC_LIBTOOL_DLOPEN - enable checks for dlopen support
AC_DEFUN([AC_LIBTOOL_DLOPEN], [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])])

# AC_LIBTOOL_WIN32_DLL - declare package support for building win32 dll's
AC_DEFUN([AC_LIBTOOL_WIN32_DLL], [AC_BEFORE([$0], [AC_LIBTOOL_SETUP])])

# AC_ENABLE_SHARED - implement the --enable-shared flag
# Usage: AC_ENABLE_SHARED[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN([AC_ENABLE_SHARED],
[define([AC_ENABLE_SHARED_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(shared,
changequote(<<, >>)dnl
<<  --enable-shared[=PKGS]  build shared libraries [default=>>AC_ENABLE_SHARED_DEFAULT],
changequote([, ])dnl
[p=${PACKAGE-default}
case $enableval in
yes) enable_shared=yes ;;
no) enable_shared=no ;;
*)
  enable_shared=no
  # Look at the argument we got.  We use all the common list separators.
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
  for pkg in $enableval; do
    if test "X$pkg" = "X$p"; then
      enable_shared=yes
    fi
  done
  IFS="$ac_save_ifs"
  ;;
esac],
enable_shared=AC_ENABLE_SHARED_DEFAULT)dnl
])

# AC_DISABLE_SHARED - set the default shared flag to --disable-shared
AC_DEFUN([AC_DISABLE_SHARED],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_SHARED(no)])

# AC_ENABLE_STATIC - implement the --enable-static flag
# Usage: AC_ENABLE_STATIC[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN([AC_ENABLE_STATIC],
[define([AC_ENABLE_STATIC_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(static,
changequote(<<, >>)dnl
<<  --enable-static[=PKGS]  build static libraries [default=>>AC_ENABLE_STATIC_DEFAULT],
changequote([, ])dnl
[p=${PACKAGE-default}
case $enableval in
yes) enable_static=yes ;;
no) enable_static=no ;;
*)
  enable_static=no
  # Look at the argument we got.  We use all the common list separators.
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
  for pkg in $enableval; do
    if test "X$pkg" = "X$p"; then
      enable_static=yes
    fi
  done
  IFS="$ac_save_ifs"
  ;;
esac],
enable_static=AC_ENABLE_STATIC_DEFAULT)dnl
])

# AC_DISABLE_STATIC - set the default static flag to --disable-static
AC_DEFUN([AC_DISABLE_STATIC],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_STATIC(no)])


# AC_ENABLE_FAST_INSTALL - implement the --enable-fast-install flag
# Usage: AC_ENABLE_FAST_INSTALL[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN([AC_ENABLE_FAST_INSTALL],
[define([AC_ENABLE_FAST_INSTALL_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(fast-install,
changequote(<<, >>)dnl
<<  --enable-fast-install[=PKGS]  optimize for fast installation [default=>>AC_ENABLE_FAST_INSTALL_DEFAULT],
changequote([, ])dnl
[p=${PACKAGE-default}
case $enableval in
yes) enable_fast_install=yes ;;
no) enable_fast_install=no ;;
*)
  enable_fast_install=no
  # Look at the argument we got.  We use all the common list separators.
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
  for pkg in $enableval; do
    if test "X$pkg" = "X$p"; then
      enable_fast_install=yes
    fi
  done
  IFS="$ac_save_ifs"
  ;;
esac],
enable_fast_install=AC_ENABLE_FAST_INSTALL_DEFAULT)dnl
])

# AC_DISABLE_FAST_INSTALL - set the default to --disable-fast-install
AC_DEFUN([AC_DISABLE_FAST_INSTALL],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_FAST_INSTALL(no)])

# AC_LIBTOOL_PICMODE - implement the --with-pic flag
# Usage: AC_LIBTOOL_PICMODE[(MODE)]
#   Where MODE is either `yes' or `no'.  If omitted, it defaults to
#   `both'.
AC_DEFUN([AC_LIBTOOL_PICMODE],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
pic_mode=ifelse($#,1,$1,default)])


# AC_PATH_TOOL_PREFIX - find a file program which can recognise shared library
AC_DEFUN([AC_PATH_TOOL_PREFIX],
[AC_MSG_CHECKING([for $1])
AC_CACHE_VAL(lt_cv_path_MAGIC_CMD,
[case $MAGIC_CMD in
  /*)
  lt_cv_path_MAGIC_CMD="$MAGIC_CMD" # Let the user override the test with a path.
  ;;
  ?:/*)
  lt_cv_path_MAGIC_CMD="$MAGIC_CMD" # Let the user override the test with a dos path.
  ;;
  *)
  ac_save_MAGIC_CMD="$MAGIC_CMD"
  IFS="${IFS=   }"; ac_save_ifs="$IFS"; IFS=":"
dnl $ac_dummy forces splitting on constant user-supplied paths.
dnl POSIX.2 word splitting is done only on the output of word expansions,
dnl not every word.  This closes a longstanding sh security hole.
  ac_dummy="ifelse([$2], , $PATH, [$2])"
  for ac_dir in $ac_dummy; do
    test -z "$ac_dir" && ac_dir=.
    if test -f $ac_dir/$1; then
      lt_cv_path_MAGIC_CMD="$ac_dir/$1"
      if test -n "$file_magic_test_file"; then
	case $deplibs_check_method in
	"file_magic "*)
	  file_magic_regex="`expr \"$deplibs_check_method\" : \"file_magic \(.*\)\"`"
	  MAGIC_CMD="$lt_cv_path_MAGIC_CMD"
	  if eval $file_magic_cmd \$file_magic_test_file 2> /dev/null |
	    egrep "$file_magic_regex" > /dev/null; then
	    :
	  else
	    cat <<EOF 1>&2

*** Warning: the command libtool uses to detect shared libraries,
*** $file_magic_cmd, produces output that libtool cannot recognize.
*** The result is that libtool may fail to recognize shared libraries
*** as such.  This will affect the creation of libtool libraries that
*** depend on shared libraries, but programs linked with such libtool
*** libraries will work regardless of this problem.  Nevertheless, you
*** may want to report the problem to your system manager and/or to
*** bug-libtool@gnu.org

EOF
	  fi ;;
	esac
      fi
      break
    fi
  done
  IFS="$ac_save_ifs"
  MAGIC_CMD="$ac_save_MAGIC_CMD"
  ;;
esac])
MAGIC_CMD="$lt_cv_path_MAGIC_CMD"
if test -n "$MAGIC_CMD"; then
  AC_MSG_RESULT($MAGIC_CMD)
else
  AC_MSG_RESULT(no)
fi
])


# AC_PATH_MAGIC - find a file program which can recognise a shared library
AC_DEFUN([AC_PATH_MAGIC],
[AC_REQUIRE([AC_CHECK_TOOL_PREFIX])dnl
AC_PATH_TOOL_PREFIX(${ac_tool_prefix}file, /usr/bin:$PATH)
if test -z "$lt_cv_path_MAGIC_CMD"; then
  if test -n "$ac_tool_prefix"; then
    AC_PATH_TOOL_PREFIX(file, /usr/bin:$PATH)
  else
    MAGIC_CMD=:
  fi
fi
])


# AC_PROG_LD - find the path to the GNU or non-GNU linker
AC_DEFUN([AC_PROG_LD],
[AC_ARG_WITH(gnu-ld,
[  --with-gnu-ld           assume the C compiler uses GNU ld [default=no]],
test "$withval" = no || with_gnu_ld=yes, with_gnu_ld=no)
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_CANONICAL_HOST])dnl
AC_REQUIRE([AC_CANONICAL_BUILD])dnl
AC_REQUIRE([_LT_AC_LIBTOOL_SYS_PATH_SEPARATOR])dnl
ac_prog=ld
if test "$GCC" = yes; then
  # Check if gcc -print-prog-name=ld gives a path.
  AC_MSG_CHECKING([for ld used by GCC])
  case $host in
  *-*-mingw*)
    # gcc leaves a trailing carriage return which upsets mingw
    ac_prog=`($CC -print-prog-name=ld) 2>&5 | tr -d '\015'` ;;
  *)
    ac_prog=`($CC -print-prog-name=ld) 2>&5` ;;
  esac
  case $ac_prog in
    # Accept absolute paths.
    [[\\/]]* | [[A-Za-z]]:[[\\/]]*)
      re_direlt='/[[^/]][[^/]]*/\.\./'
      # Canonicalize the path of ld
      ac_prog=`echo $ac_prog| sed 's%\\\\%/%g'`
      while echo $ac_prog | grep "$re_direlt" > /dev/null 2>&1; do
	ac_prog=`echo $ac_prog| sed "s%$re_direlt%/%"`
      done
      test -z "$LD" && LD="$ac_prog"
      ;;
  "")
    # If it fails, then pretend we aren't using GCC.
    ac_prog=ld
    ;;
  *)
    # If it is relative, then search for the first ld in PATH.
    with_gnu_ld=unknown
    ;;
  esac
elif test "$with_gnu_ld" = yes; then
  AC_MSG_CHECKING([for GNU ld])
else
  AC_MSG_CHECKING([for non-GNU ld])
fi
AC_CACHE_VAL(lt_cv_path_LD,
[if test -z "$LD"; then
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS=$PATH_SEPARATOR
  for ac_dir in $PATH; do
    test -z "$ac_dir" && ac_dir=.
    if test -f "$ac_dir/$ac_prog" || test -f "$ac_dir/$ac_prog$ac_exeext"; then
      lt_cv_path_LD="$ac_dir/$ac_prog"
      # Check to see if the program is GNU ld.  I'd rather use --version,
      # but apparently some GNU ld's only accept -v.
      # Break only if it was the GNU/non-GNU ld that we prefer.
      if "$lt_cv_path_LD" -v 2>&1 < /dev/null | egrep '(GNU|with BFD)' > /dev/null; then
	test "$with_gnu_ld" != no && break
      else
	test "$with_gnu_ld" != yes && break
      fi
    fi
  done
  IFS="$ac_save_ifs"
else
  lt_cv_path_LD="$LD" # Let the user override the test with a path.
fi])
LD="$lt_cv_path_LD"
if test -n "$LD"; then
  AC_MSG_RESULT($LD)
else
  AC_MSG_RESULT(no)
fi
test -z "$LD" && AC_MSG_ERROR([no acceptable ld found in \$PATH])
AC_PROG_LD_GNU
])

# AC_PROG_LD_GNU -
AC_DEFUN([AC_PROG_LD_GNU],
[AC_CACHE_CHECK([if the linker ($LD) is GNU ld], lt_cv_prog_gnu_ld,
[# I'd rather use --version here, but apparently some GNU ld's only accept -v.
if $LD -v 2>&1 </dev/null | egrep '(GNU|with BFD)' 1>&5; then
  lt_cv_prog_gnu_ld=yes
else
  lt_cv_prog_gnu_ld=no
fi])
with_gnu_ld=$lt_cv_prog_gnu_ld
])

# AC_PROG_LD_RELOAD_FLAG - find reload flag for linker
#   -- PORTME Some linkers may need a different reload flag.
AC_DEFUN([AC_PROG_LD_RELOAD_FLAG],
[AC_CACHE_CHECK([for $LD option to reload object files], lt_cv_ld_reload_flag,
[lt_cv_ld_reload_flag='-r'])
reload_flag=$lt_cv_ld_reload_flag
test -n "$reload_flag" && reload_flag=" $reload_flag"
])

# AC_DEPLIBS_CHECK_METHOD - how to check for library dependencies
#  -- PORTME fill in with the dynamic library characteristics
AC_DEFUN([AC_DEPLIBS_CHECK_METHOD],
[AC_CACHE_CHECK([how to recognise dependant libraries],
lt_cv_deplibs_check_method,
[lt_cv_file_magic_cmd='$MAGIC_CMD'
lt_cv_file_magic_test_file=
lt_cv_deplibs_check_method='unknown'
# Need to set the preceding variable on all platforms that support
# interlibrary dependencies.
# 'none' -- dependencies not supported.
# `unknown' -- same as none, but documents that we really don't know.
# 'pass_all' -- all dependencies passed with no checks.
# 'test_compile' -- check by making test program.
# 'file_magic [[regex]]' -- check by looking for files in library path
# which responds to the $file_magic_cmd with a given egrep regex.
# If you have `file' or equivalent on your system and you're not sure
# whether `pass_all' will *always* work, you probably want this one.

case $host_os in
aix4* | aix5*)
  lt_cv_deplibs_check_method=pass_all
  ;;

beos*)
  lt_cv_deplibs_check_method=pass_all
  ;;

bsdi4*)
  lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[ML]]SB (shared object|dynamic lib)'
  lt_cv_file_magic_cmd='/usr/bin/file -L'
  lt_cv_file_magic_test_file=/shlib/libc.so
  ;;

cygwin* | mingw* | pw32*)
  lt_cv_deplibs_check_method='file_magic file format pei*-i386(.*architecture: i386)?'
  lt_cv_file_magic_cmd='$OBJDUMP -f'
  ;;

darwin* | rhapsody*)
  lt_cv_deplibs_check_method='file_magic Mach-O dynamically linked shared library'
  lt_cv_file_magic_cmd='/usr/bin/file -L'
  case "$host_os" in
  rhapsody* | darwin1.[[012]])
    lt_cv_file_magic_test_file=`echo /System/Library/Frameworks/System.framework/Versions/*/System | head -1`
    ;;
  *) # Darwin 1.3 on
    lt_cv_file_magic_test_file='/usr/lib/libSystem.dylib'
    ;;
  esac
  ;;

freebsd*)
  if echo __ELF__ | $CC -E - | grep __ELF__ > /dev/null; then
    case $host_cpu in
    i*86 )
      # Not sure whether the presence of OpenBSD here was a mistake.
      # Let's accept both of them until this is cleared up.
      lt_cv_deplibs_check_method='file_magic (FreeBSD|OpenBSD)/i[[3-9]]86 (compact )?demand paged shared library'
      lt_cv_file_magic_cmd=/usr/bin/file
      lt_cv_file_magic_test_file=`echo /usr/lib/libc.so.*`
      ;;
    esac
  else
    lt_cv_deplibs_check_method=pass_all
  fi
  ;;

gnu*)
  lt_cv_deplibs_check_method=pass_all
  ;;

hpux10.20*|hpux11*)
  lt_cv_deplibs_check_method='file_magic (s[[0-9]][[0-9]][[0-9]]|PA-RISC[[0-9]].[[0-9]]) shared library'
  lt_cv_file_magic_cmd=/usr/bin/file
  lt_cv_file_magic_test_file=/usr/lib/libc.sl
  ;;

irix5* | irix6*)
  case $host_os in
  irix5*)
    # this will be overridden with pass_all, but let us keep it just in case
    lt_cv_deplibs_check_method="file_magic ELF 32-bit MSB dynamic lib MIPS - version 1"
    ;;
  *)
    case $LD in
    *-32|*"-32 ") libmagic=32-bit;;
    *-n32|*"-n32 ") libmagic=N32;;
    *-64|*"-64 ") libmagic=64-bit;;
    *) libmagic=never-match;;
    esac
    # this will be overridden with pass_all, but let us keep it just in case
    lt_cv_deplibs_check_method="file_magic ELF ${libmagic} MSB mips-[[1234]] dynamic lib MIPS - version 1"
    ;;
  esac
  lt_cv_file_magic_test_file=`echo /lib${libsuff}/libc.so*`
  lt_cv_deplibs_check_method=pass_all
  ;;

# This must be Linux ELF.
linux-gnu*)
  case $host_cpu in
  alpha* | hppa* | i*86 | powerpc* | sparc* | ia64* )
    lt_cv_deplibs_check_method=pass_all ;;
  *)
    # glibc up to 2.1.1 does not perform some relocations on ARM
    lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[LM]]SB (shared object|dynamic lib )' ;;
  esac
  lt_cv_file_magic_test_file=`echo /lib/libc.so* /lib/libc-*.so`
  ;;

netbsd*)
  if echo __ELF__ | $CC -E - | grep __ELF__ > /dev/null; then
    lt_cv_deplibs_check_method='match_pattern /lib[[^/\.]]+\.so\.[[0-9]]+\.[[0-9]]+$'
  else
    lt_cv_deplibs_check_method='match_pattern /lib[[^/\.]]+\.so$'
  fi
  ;;

newos6*)
  lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[ML]]SB (executable|dynamic lib)'
  lt_cv_file_magic_cmd=/usr/bin/file
  lt_cv_file_magic_test_file=/usr/lib/libnls.so
  ;;

openbsd*)
  lt_cv_file_magic_cmd=/usr/bin/file
  lt_cv_file_magic_test_file=`echo /usr/lib/libc.so.*`
  if test -z "`echo __ELF__ | $CC -E - | grep __ELF__`" || test "$host_os-$host_cpu" = "openbsd2.8-powerpc"; then
    lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[LM]]SB shared object'
  else
    lt_cv_deplibs_check_method='file_magic OpenBSD.* shared library'
  fi
  ;;

osf3* | osf4* | osf5*)
  # this will be overridden with pass_all, but let us keep it just in case
  lt_cv_deplibs_check_method='file_magic COFF format alpha shared library'
  lt_cv_file_magic_test_file=/shlib/libc.so
  lt_cv_deplibs_check_method=pass_all
  ;;

sco3.2v5*)
  lt_cv_deplibs_check_method=pass_all
  ;;

solaris*)
  lt_cv_deplibs_check_method=pass_all
  lt_cv_file_magic_test_file=/lib/libc.so
  ;;

sysv5uw[[78]]* | sysv4*uw2*)
  lt_cv_deplibs_check_method=pass_all
  ;;

sysv4 | sysv4.2uw2* | sysv4.3* | sysv5*)
  case $host_vendor in
  motorola)
    lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[ML]]SB (shared object|dynamic lib) M[[0-9]][[0-9]]* Version [[0-9]]'
    lt_cv_file_magic_test_file=`echo /usr/lib/libc.so*`
    ;;
  ncr)
    lt_cv_deplibs_check_method=pass_all
    ;;
  sequent)
    lt_cv_file_magic_cmd='/bin/file'
    lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[LM]]SB (shared object|dynamic lib )'
    ;;
  sni)
    lt_cv_file_magic_cmd='/bin/file'
    lt_cv_deplibs_check_method="file_magic ELF [[0-9]][[0-9]]*-bit [[LM]]SB dynamic lib"
    lt_cv_file_magic_test_file=/lib/libc.so
    ;;
  esac
  ;;
esac
])
file_magic_cmd=$lt_cv_file_magic_cmd
deplibs_check_method=$lt_cv_deplibs_check_method
])


# AC_PROG_NM - find the path to a BSD-compatible name lister
AC_DEFUN([AC_PROG_NM],
[AC_REQUIRE([_LT_AC_LIBTOOL_SYS_PATH_SEPARATOR])dnl
AC_MSG_CHECKING([for BSD-compatible nm])
AC_CACHE_VAL(lt_cv_path_NM,
[if test -n "$NM"; then
  # Let the user override the test.
  lt_cv_path_NM="$NM"
else
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS=$PATH_SEPARATOR
  for ac_dir in $PATH /usr/ccs/bin /usr/ucb /bin; do
    test -z "$ac_dir" && ac_dir=.
    tmp_nm=$ac_dir/${ac_tool_prefix}nm
    if test -f $tmp_nm || test -f $tmp_nm$ac_exeext ; then
      # Check to see if the nm accepts a BSD-compat flag.
      # Adding the `sed 1q' prevents false positives on HP-UX, which says:
      #   nm: unknown option "B" ignored
      # Tru64's nm complains that /dev/null is an invalid object file
      if ($tmp_nm -B /dev/null 2>&1 | sed '1q'; exit 0) | egrep '(/dev/null|Invalid file or object type)' >/dev/null; then
	lt_cv_path_NM="$tmp_nm -B"
	break
      elif ($tmp_nm -p /dev/null 2>&1 | sed '1q'; exit 0) | egrep /dev/null >/dev/null; then
	lt_cv_path_NM="$tmp_nm -p"
	break
      else
	lt_cv_path_NM=${lt_cv_path_NM="$tmp_nm"} # keep the first match, but
	continue # so that we can try to find one that supports BSD flags
      fi
    fi
  done
  IFS="$ac_save_ifs"
  test -z "$lt_cv_path_NM" && lt_cv_path_NM=nm
fi])
NM="$lt_cv_path_NM"
AC_MSG_RESULT([$NM])
])

# AC_CHECK_LIBM - check for math library
AC_DEFUN([AC_CHECK_LIBM],
[AC_REQUIRE([AC_CANONICAL_HOST])dnl
LIBM=
case $host in
*-*-beos* | *-*-cygwin* | *-*-pw32*)
  # These system don't have libm
  ;;
*-ncr-sysv4.3*)
  AC_CHECK_LIB(mw, _mwvalidcheckl, LIBM="-lmw")
  AC_CHECK_LIB(m, main, LIBM="$LIBM -lm")
  ;;
*)
  AC_CHECK_LIB(m, main, LIBM="-lm")
  ;;
esac
])

# AC_LIBLTDL_CONVENIENCE[(dir)] - sets LIBLTDL to the link flags for
# the libltdl convenience library and INCLTDL to the include flags for
# the libltdl header and adds --enable-ltdl-convenience to the
# configure arguments.  Note that LIBLTDL and INCLTDL are not
# AC_SUBSTed, nor is AC_CONFIG_SUBDIRS called.  If DIR is not
# provided, it is assumed to be `libltdl'.  LIBLTDL will be prefixed
# with '${top_builddir}/' and INCLTDL will be prefixed with
# '${top_srcdir}/' (note the single quotes!).  If your package is not
# flat and you're not using automake, define top_builddir and
# top_srcdir appropriately in the Makefiles.
AC_DEFUN([AC_LIBLTDL_CONVENIENCE],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
  case $enable_ltdl_convenience in
  no) AC_MSG_ERROR([this package needs a convenience libltdl]) ;;
  "") enable_ltdl_convenience=yes
      ac_configure_args="$ac_configure_args --enable-ltdl-convenience" ;;
  esac
  LIBLTDL='${top_builddir}/'ifelse($#,1,[$1],['libltdl'])/libltdlc.la
  INCLTDL='-I${top_srcdir}/'ifelse($#,1,[$1],['libltdl'])
])

# AC_LIBLTDL_INSTALLABLE[(dir)] - sets LIBLTDL to the link flags for
# the libltdl installable library and INCLTDL to the include flags for
# the libltdl header and adds --enable-ltdl-install to the configure
# arguments.  Note that LIBLTDL and INCLTDL are not AC_SUBSTed, nor is
# AC_CONFIG_SUBDIRS called.  If DIR is not provided and an installed
# libltdl is not found, it is assumed to be `libltdl'.  LIBLTDL will
# be prefixed with '${top_builddir}/' and INCLTDL will be prefixed
# with '${top_srcdir}/' (note the single quotes!).  If your package is
# not flat and you're not using automake, define top_builddir and
# top_srcdir appropriately in the Makefiles.
# In the future, this macro may have to be called after AC_PROG_LIBTOOL.
AC_DEFUN([AC_LIBLTDL_INSTALLABLE],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
  AC_CHECK_LIB(ltdl, main,
  [test x"$enable_ltdl_install" != xyes && enable_ltdl_install=no],
  [if test x"$enable_ltdl_install" = xno; then
     AC_MSG_WARN([libltdl not installed, but installation disabled])
   else
     enable_ltdl_install=yes
   fi
  ])
  if test x"$enable_ltdl_install" = x"yes"; then
    ac_configure_args="$ac_configure_args --enable-ltdl-install"
    LIBLTDL='${top_builddir}/'ifelse($#,1,[$1],['libltdl'])/libltdl.la
    INCLTDL='-I${top_srcdir}/'ifelse($#,1,[$1],['libltdl'])
  else
    ac_configure_args="$ac_configure_args --enable-ltdl-install=no"
    LIBLTDL="-lltdl"
    INCLTDL=
  fi
])

# old names
AC_DEFUN([AM_PROG_LIBTOOL],   [AC_PROG_LIBTOOL])
AC_DEFUN([AM_ENABLE_SHARED],  [AC_ENABLE_SHARED($@)])
AC_DEFUN([AM_ENABLE_STATIC],  [AC_ENABLE_STATIC($@)])
AC_DEFUN([AM_DISABLE_SHARED], [AC_DISABLE_SHARED($@)])
AC_DEFUN([AM_DISABLE_STATIC], [AC_DISABLE_STATIC($@)])
AC_DEFUN([AM_PROG_LD],        [AC_PROG_LD])
AC_DEFUN([AM_PROG_NM],        [AC_PROG_NM])

# This is just to silence aclocal about the macro not being used
ifelse([AC_DISABLE_FAST_INSTALL])

# Define a conditional.

AC_DEFUN([AM_CONDITIONAL],
[AC_SUBST($1_TRUE)
AC_SUBST($1_FALSE)
if $2; then
  $1_TRUE=
  $1_FALSE='#'
else
  $1_TRUE='#'
  $1_FALSE=
fi])

# Do all the work for Automake.  This macro actually does too much --
# some checks are only needed if your package does certain things.
# But this isn't really a big deal.

# serial 1

dnl Usage:
dnl AM_INIT_AUTOMAKE(package,version, [no-define])

AC_DEFUN([AM_INIT_AUTOMAKE],
[AC_REQUIRE([AM_SET_CURRENT_AUTOMAKE_VERSION])dnl
AC_REQUIRE([AC_PROG_INSTALL])
PACKAGE=[$1]
AC_SUBST(PACKAGE)
VERSION=[$2]
AC_SUBST(VERSION)
dnl test to see if srcdir already configured
if test "`cd $srcdir && pwd`" != "`pwd`" && test -f $srcdir/config.status; then
  AC_MSG_ERROR([source directory already configured; run "make distclean" there first])
fi
ifelse([$3],,
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of package])
AC_DEFINE_UNQUOTED(VERSION, "$VERSION", [Version number of package]))
AC_REQUIRE([AM_SANITY_CHECK])
AC_REQUIRE([AC_ARG_PROGRAM])
dnl FIXME This is truly gross.
missing_dir=`cd $ac_aux_dir && pwd`
AM_MISSING_PROG(ACLOCAL, aclocal-${am__api_version}, $missing_dir)
AM_MISSING_PROG(AUTOCONF, autoconf, $missing_dir)
AM_MISSING_PROG(AUTOMAKE, automake-${am__api_version}, $missing_dir)
AM_MISSING_PROG(AUTOHEADER, autoheader, $missing_dir)
AM_MISSING_PROG(MAKEINFO, makeinfo, $missing_dir)
AC_REQUIRE([AC_PROG_MAKE_SET])])

# Copyright 2002  Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA

# AM_AUTOMAKE_VERSION(VERSION)
# ----------------------------
# Automake X.Y traces this macro to ensure aclocal.m4 has been
# generated from the m4 files accompanying Automake X.Y.
AC_DEFUN([AM_AUTOMAKE_VERSION],[am__api_version="1.4"])

# AM_SET_CURRENT_AUTOMAKE_VERSION
# -------------------------------
# Call AM_AUTOMAKE_VERSION so it can be traced.
# This function is AC_REQUIREd by AC_INIT_AUTOMAKE.
AC_DEFUN([AM_SET_CURRENT_AUTOMAKE_VERSION],
	 [AM_AUTOMAKE_VERSION([1.4-p6])])

#
# Check to make sure that the build environment is sane.
#

AC_DEFUN([AM_SANITY_CHECK],
[AC_MSG_CHECKING([whether build environment is sane])
# Just in case
sleep 1
echo timestamp > conftestfile
# Do `set' in a subshell so we don't clobber the current shell's
# arguments.  Must try -L first in case configure is actually a
# symlink; some systems play weird games with the mod time of symlinks
# (eg FreeBSD returns the mod time of the symlink's containing
# directory).
if (
   set X `ls -Lt $srcdir/configure conftestfile 2> /dev/null`
   if test "[$]*" = "X"; then
      # -L didn't work.
      set X `ls -t $srcdir/configure conftestfile`
   fi
   if test "[$]*" != "X $srcdir/configure conftestfile" \
      && test "[$]*" != "X conftestfile $srcdir/configure"; then

      # If neither matched, then we have a broken ls.  This can happen
      # if, for instance, CONFIG_SHELL is bash and it inherits a
      # broken ls alias from the environment.  This has actually
      # happened.  Such a system could not be considered "sane".
      AC_MSG_ERROR([ls -t appears to fail.  Make sure there is not a broken
alias in your environment])
   fi

   test "[$]2" = conftestfile
   )
then
   # Ok.
   :
else
   AC_MSG_ERROR([newly created file is older than distributed files!
Check your system clock])
fi
rm -f conftest*
AC_MSG_RESULT(yes)])

dnl AM_MISSING_PROG(NAME, PROGRAM, DIRECTORY)
dnl The program must properly implement --version.
AC_DEFUN([AM_MISSING_PROG],
[AC_MSG_CHECKING(for working $2)
# Run test in a subshell; some versions of sh will print an error if
# an executable is not found, even if stderr is redirected.
# Redirect stdin to placate older versions of autoconf.  Sigh.
if ($2 --version) < /dev/null > /dev/null 2>&1; then
   $1=$2
   AC_MSG_RESULT(found)
else
   $1="$3/missing $2"
   AC_MSG_RESULT(missing)
fi
AC_SUBST($1)])

