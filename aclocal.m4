dnl aclocal.m4 generated automatically by aclocal 1.4

dnl Copyright (C) 1994, 1995-8, 1999 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY, to the extent permitted by law; without
dnl even the implied warranty of MERCHANTABILITY or FITNESS FOR A
dnl PARTICULAR PURPOSE.

dnl aclocal.m4 -- extra macros for configuring R
dnl
dnl Copyright (C) 1998, 1999, 2000 R Core Team
dnl
dnl
dnl
dnl R_ARG_WITH_EXCLUSIVE
dnl
AC_DEFUN(R_ARG_WITH_EXCLUSIVE,
 [if test "${with_$1+set}" = set; then
    if test "${with_$2+set}" = set; then
      if test "$with_$2" = no; then
	true
      else
	$3
      fi
    fi
  fi])
dnl
dnl R_ARG_USE
dnl
AC_DEFUN(R_ARG_USE,
 [if test "${withval}" = no; then
    use_$1=false
  else
    use_$1=true
  fi])
dnl
dnl R_PROG_AR
dnl
AC_DEFUN(R_PROG_AR,
 [AC_CHECK_PROGS(AR, [${AR} ar])
  : ${ARFLAGS="rc"}
  AC_SUBST(ARFLAGS)])
dnl
dnl R_PROG_ECHO_N
dnl
AC_DEFUN(R_PROG_ECHO_N,
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
dnl
dnl R_PROG_INSTALL
dnl
AC_DEFUN(R_PROG_INSTALL,
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
dnl
dnl R_PROG_PERL
dnl
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
AC_DEFUN(R_PROG_PERL,
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
dnl
dnl R_PROG_TEXMF
dnl
AC_DEFUN(R_PROG_TEXMF,
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
    INSTALL_INFO="\$(top_builddir)/tools/install-info"
    AC_SUBST(INSTALL_INFO)
  else
    AC_PATH_PROGS(INSTALL_INFO, [${INSTALL_INFO} install-info], false)
  fi
  : ${R_RD4DVI="ae"}
  AC_SUBST(R_RD4DVI)
  : ${R_RD4PDF="ae,hyper"}
  AC_SUBST(R_RD4PDF)
  ])
dnl
dnl R_PROG_CC_M
dnl
dnl Test whether the C compiler accepts -M for generating dependencies
dnl
AC_DEFUN(R_PROG_CC_M,
  [ depend_rules_frag=Makefrag.dep
    AC_CACHE_CHECK(
      [whether ${CC} accepts -M for generating dependencies],
      r_cv_prog_cc_m,
      [ echo "#include <math.h>" > conftest.${ac_ext}
	if test -n "`${CC} -M conftest.${ac_ext} 2>/dev/null \
		    | grep conftest`"; then
	  r_cv_prog_cc_m=yes
	else
	  r_cv_prog_cc_m=no
	fi
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
dnl
dnl R_PROG_CC_FLAG
dnl
dnl Test whether the C compiler handles a command line option
dnl
AC_DEFUN(R_PROG_CC_FLAG,
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
dnl
dnl R_PROG_CXX_FLAG
dnl
dnl Test whether the C++ compiler handles a command line option
dnl
AC_DEFUN(R_PROG_CXX_FLAG,
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
dnl
dnl R_PROG_F77_WORKS
dnl
dnl Determine whether the Fortran 77 compiler works (in the sense that
dnl we can create executables, but not necessarily run them).  This
dnl tests in particular whether all Fortran libraries are available.
dnl
AC_DEFUN(R_PROG_F77_WORKS, [
    AC_CACHE_CHECK([whether the Fortran 77 compiler (${FC} ${FFLAGS} ${LDFLAGS}) works],
    r_cv_prog_f77_works, [
      cat > conftest.f <<EOF    
      program conftest
      end
EOF
      ${FC} -o conftest ${FFLAGS} ${LDFLAGS} conftest.f ${LIBS} \
        1>&AC_FD_CC 2>&AC_FD_CC
      if test ${?} = 0; then
        r_cv_prog_f77_works=yes
      else
        r_cv_prog_f77_works=no
      fi])
  rm -rf conftest conftest.* core
  if test ${r_cv_prog_f77_works} = no; then
    AC_MSG_WARN([Maybe your Fortran installation is incomplete])
    AC_MSG_ERROR([Fortran 77 compiler does not work])
  fi])
dnl
dnl R_PROG_F77_GNU
dnl
dnl See if ${F77-f77} is the GNU Fortran compiler
dnl
AC_DEFUN(R_PROG_F77_GNU,
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
dnl
dnl See if the Fortran compiler appends underscores
dnl
AC_DEFUN(R_PROG_F77_APPEND_UNDERSCORE,
 [AC_CACHE_CHECK([whether ${F77-f77} appends underscores],
    r_cv_prog_f77_append_underscore,
    [ cat > conftestf.f <<EOF
      subroutine try
      end
EOF
      ${FC} -c ${FFLAGS} conftestf.f 1>&AC_FD_CC 2>&AC_FD_CC
      cat > conftest.c <<EOF
main() { try_(); }
EOF
      ${CC} ${CFLAGS} ${CPPFLAGS} ${LDFLAGS} -o conftest \
	conftest.c conftestf.o 1>&AC_FD_CC 2>&AC_FD_CC
      if test ${?} = 0; then
	r_cv_prog_f77_append_underscore=yes
      else
	cat > conftest.c <<EOF
main() { try(); }
EOF
	${CC} ${CFLAGS} ${CPPFLAGS} ${LDFLAGS} -o conftest \
	  conftest.c conftestf.o 1>&AC_FD_CC 2>&AC_FD_CC
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
dnl
dnl See whether Fortran and C compilers agree on int and double
dnl
AC_DEFUN(R_PROG_F77_CC_COMPAT,
 [AC_MSG_CHECKING([whether ${F77-f77} and ${CC-cc} agree on int and double])
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
      ${FC} ${FFLAGS} -c conftestf.f 1>&AC_FD_CC 2>&AC_FD_CC
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
	if ${CC-cc} ${LDFLAGS} ${MAIN_LDFLAGS} -o conftest conftest.o conftestf.o \
            ${FLIBS} -lm 1>&AC_FD_CC 2>&AC_FD_CC; then
          output=`./conftest 2>&1`
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
dnl
dnl R_PROG_F2C_FLIBS
dnl
AC_DEFUN(R_PROG_F2C_FLIBS,
 [AC_CACHE_VAL(r_cv_f2c_flibs,
    [## This seems to be necessary on some Linux system. -- you bet! -pd
      cat > conftest.${ac_ext} << EOF
int MAIN_ () { return 0; }
int MAIN__ () { return 0; }
EOF
      if AC_TRY_EVAL(ac_compile); then
	${AR} ${ARFLAGS} libconftest.a conftest.o 1>&AC_FD_CC
	if test -n "${RANLIB}"; then
	  ${RANLIB} libconftest.a 1>&AC_FD_CC
	fi
      fi
      AC_DEFINE(HAVE_F77_UNDERSCORE)
      AC_CHECK_LIB(f2c, f_open, flibs=-lf2c, flibs=, -L. -lconftest -lm)
      rm -f libconftest*
      if test -z "${flibs}"; then
	AC_CHECK_LIB(F77, d_sin, flibs=-lF77, flibs=, -lm)
	if test -n "${flibs}"; then
	  AC_CHECK_LIB(I77, f_rew, flibs="${flibs} -lI77", flibs=, -lF77)
	fi
      fi
      r_cv_f2c_flibs="${flibs}"])
  FLIBS="${r_cv_f2c_flibs}"
  if test -z "${FLIBS}"; then
    warn_f2c_flibs="I found f2c but not libf2c, or libF77 and libI77"
    AC_MSG_WARN(${warn_f2c_flibs})
  fi])
dnl
dnl R_FUNC___SETFPUCW
dnl
AC_DEFUN(R_FUNC___SETFPUCW,
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
dnl
dnl R_FUNC_CALLOC
dnl
AC_DEFUN(R_FUNC_CALLOC,
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
dnl
dnl R_FUNC_FINITE
dnl
AC_DEFUN(R_FUNC_FINITE,
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
dnl
dnl R_FUNC_LOG
dnl
AC_DEFUN(R_FUNC_LOG,
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
dnl
dnl R_HEADER_SETJMP
dnl
AC_DEFUN(R_HEADER_SETJMP,
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
dnl
dnl R_HEADER_GLIBC2
dnl
AC_DEFUN(R_HEADER_GLIBC2,
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
dnl
dnl
dnl R_C_IEEE_754
dnl
AC_DEFUN(R_C_IEEE_754,
 [AC_CHECK_FUNCS(finite isnan)
  AC_CACHE_CHECK([whether you have IEEE 754 floating-point arithmetic],
    r_cv_c_ieee_754,
    dnl FIXME: This fails is finite() or isnan() are defined as macros
    dnl rather than exist as library functions ...
    AC_EGREP_CPP(yes,
      changequote(<<, >>)dnl
      <<
#include "conftest.h"
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
dnl
dnl R_C_OPTIEEE
dnl
AC_DEFUN(R_C_OPTIEEE,
  [ AC_CACHE_CHECK([whether compilers need -OPT:IEEE_NaN_inf=ON],
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
      R_XTRA_FFLAGS="${R_XTRA_FFLAGS} -OPT:IEEE_NaN_inf=ON"
    fi
  ])
dnl
dnl R_GNOME
dnl
AC_DEFUN(R_GNOME, [ 
  if test ${want_gnome} = yes; then
    GNOME_INIT_HOOK([], cont)
    if test "${GNOMEUI_LIBS}"; then
      AM_PATH_LIBGLADE(
        [use_gnome="yes"
	  GNOME_IF_FILES="gnome-interface.glade"],
        [ warn_libglade="GNOME support requires libglade version >= 0.3"
	  AC_MSG_WARN(${warn_libglade})],
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
dnl
dnl GNOME_INIT_HOOK (script-if-gnome-enabled, failflag)
dnl
dnl if failflag is "fail" then GNOME_INIT_HOOK will abort if gnomeConf.sh
dnl is not found. 
dnl
AC_DEFUN([GNOME_INIT_HOOK], [	
  AC_SUBST(GNOME_LIBS)
  AC_SUBST(GNOMEUI_LIBS)
  AC_SUBST(GNOMEGNORBA_LIBS)
  AC_SUBST(GTKXMHTML_LIBS)
  AC_SUBST(GNOME_APPLET_LIBS)
  AC_SUBST(GNOME_LIBDIR)
  AC_SUBST(GNOME_INCLUDEDIR)

  AC_ARG_WITH(gnome-includes,
    [  --with-gnome-includes   specify location of GNOME headers], [
    CFLAGS="$CFLAGS -I$withval"
  ])
	
  AC_ARG_WITH(gnome-libs,
    [  --with-gnome-libs       specify location of GNOME libs], [
    LDFLAGS="$LDFLAGS -L$withval"
    gnome_prefix=$withval
  ])

  if test "${want_gnome}" = yes; then
    AC_PATH_PROG(GNOME_CONFIG, gnome-config, no)
    if test "${GNOME_CONFIG}" = no; then
      no_gnome_config="yes"
    else
      AC_MSG_CHECKING(if ${GNOME_CONFIG} works)
      if ${GNOME_CONFIG} --libs-only-l gnome >/dev/null 2>&1; then
	AC_MSG_RESULT(yes)
	GNOME_GNORBA_HOOK([], $2)
	GNOME_LIBS="`${GNOME_CONFIG} --libs-only-l gnome`"
	GNOMEUI_LIBS="`${GNOME_CONFIG} --libs-only-l gnomeui`"
	GNOMEGNORBA_LIBS="`${GNOME_CONFIG} --libs-only-l gnorba gnomeui`"
	GTKXMHTML_LIBS="`${GNOME_CONFIG} --libs-only-l gtkxmhtml`"
	GNOME_APPLET_LIBS="`${GNOME_CONFIG} --libs-only-l applets`"
	GNOME_LIBDIR="`${GNOME_CONFIG} --libs-only-L gnorba gnomeui`"
	GNOME_INCLUDEDIR="`${GNOME_CONFIG} --cflags gnorba gnomeui`"
	$1
      else
	AC_MSG_RESULT(no)
	no_gnome_config="yes"
      fi
    fi

  if test ${exec_prefix} = NONE; then
    if test ${prefix} = NONE; then
      gnome_prefix=${ac_default_prefix}/lib
    else
      gnome_prefix=${prefix}/lib
    fi
  else
    gnome_prefix=`eval echo \`echo ${libdir}\``
  fi
	
  if test "${no_gnome_config}" = "yes"; then
    AC_MSG_CHECKING(for gnomeConf.sh file in ${gnome_prefix})
    if test -f $gnome_prefix/gnomeConf.sh; then
      AC_MSG_RESULT(found)
      echo "loading gnome configuration from ${gnome_prefix}/gnomeConf.sh"
      . ${gnome_prefix}/gnomeConf.sh
      $1
    else
      AC_MSG_RESULT(not found)
      if test "$2" = fail; then
	AC_MSG_ERROR(Could not find the gnomeConf.sh file that is generated by gnome-libs install)
      fi
    fi
  fi
fi
])

AC_DEFUN([GNOME_INIT], [
  GNOME_INIT_HOOK([], fail)
])

dnl
dnl GNOME_GNORBA_HOOK (script-if-gnorba-found, failflag)
dnl
dnl if failflag is "failure" it aborts if gnorba is not found.
dnl

AC_DEFUN([GNOME_GNORBA_HOOK], [
  GNOME_ORBIT_HOOK([], $2)
  AC_CACHE_CHECK([for gnorba libraries],
    gnome_cv_gnorba_found,
    [ gnome_cv_gnorba_found=no
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

AC_DEFUN([GNOME_ORBIT_HOOK], [
  AC_PATH_PROG(ORBIT_CONFIG, orbit-config, no)
  AC_PATH_PROG(ORBIT_IDL, orbit-idl, no)
  AC_CACHE_CHECK([for working ORBit environment],
    gnome_cv_orbit_found,
    [ if test x$ORBIT_CONFIG = xno -o x$ORBIT_IDL = xno; then
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
  GNOME_ORBIT_HOOK([], failure)
])

dnl AM_PATH_LIBGLADE([ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND [, MODULES]]])
dnl Test to see if libglade is installed, and define LIBGLADE_CFLAGS, LIBS
dnl
AC_DEFUN(AM_PATH_LIBGLADE,
[dnl
dnl Get the cflags and libraries from the libglade-config script
dnl
AC_ARG_WITH(libglade-config,
[  --with-libglade-config=LIBGLADE_CONFIG
                          specify location of libglade-config],
LIBGLADE_CONFIG="$withval")

module_args=
for module in . $3; do
  case "$module" in
    gnome)
      module_args="$module_args gnome"
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

dnl
dnl R_BITMAPS
dnl
AC_DEFUN(R_BITMAPS, [
  BITMAP_LIBS=
  AC_EGREP_HEADER(jpeg_error_mgr, jpeglib.h, [
    AC_CHECK_LIB(jpeg, jpeg_destroy_compress, [
      BITMAP_LIBS=-ljpeg
      AC_DEFINE(HAVE_JPEG)
    ], , ${LIBS})
  ])
  AC_CHECK_LIB(z, main, [
    AC_CHECK_HEADER(png.h, [
      AC_CHECK_LIB(png, png_create_write_struct, [
        BITMAP_LIBS="${BITMAP_LIBS} -lpng -lz"
	AC_DEFINE(HAVE_PNG)
      ], , ${LIBS})
    ])
  ])
  AC_SUBST(BITMAP_LIBS)])
dnl
dnl Try finding {tcl,tk}Config.sh
dnl
dnl R_TCLTK_CONFIG()
dnl
AC_DEFUN(R_TCLTK_CONFIG,
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
dnl
dnl Need to ensure that we can find the tcl.h and tk.h headers, which
dnl may be in non-standard and/or version-dependent directories, such as
dnl on FreeBSD systems.
dnl
dnl The logic is as follows.  If TCLTK_CPPFLAGS was specified, then we
dnl do not investigate any further.  Otherwise, if we still think we
dnl have Tcl/Tk, then first try via the corresponding *Config.sh file,
dnl or else try the obvious.
dnl
dnl R_TCLTK_CPPFLAGS()
dnl
AC_DEFUN(R_TCLTK_CPPFLAGS,
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
      AC_CHECK_HEADER(tcl.h, , have_tcltk=no)
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
      AC_CHECK_HEADER(tk.h, , have_tcltk=no)
    fi
    unset found_tk_h
  fi
fi])
dnl
dnl Find the tcl and tk libraries.
dnl
dnl R_TCLTK_LIBS()
dnl
AC_DEFUN(R_TCLTK_LIBS,
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
dnl
dnl R_TCLTK()
dnl
AC_DEFUN(R_TCLTK,
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


AC_DEFUN(R_BLAS_LIBS, [
if test "${r_cv_prog_f77_append_underscore}" = yes; then
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

if test "x$BLAS_LIBS" = x; then
  # BLAS in Alpha CXML library?
  AC_CHECK_LIB(cxml, $dgemm_func, BLAS_LIBS="-lcxml", , $FLIBS)
fi

if test "x$BLAS_LIBS" = x; then
  # BLAS in Alpha DXML library? (now called CXML, see above)
  AC_CHECK_LIB(dxml, $dgemm_func, BLAS_LIBS="-ldxml", , $FLIBS)
fi

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

dnl
dnl See if leap seconds are used.
dnl
AC_DEFUN(R_USES_LEAPSECONDS,
 [AC_MSG_CHECKING([whether leap seconds are counted])
  AC_CACHE_VAL(uses_leapseconds,
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
      if ${CC-cc} ${CFLAGS} -o conftest conftest.c; then
          output=`./conftest`
	  if test ${?} -eq 0; then
            if test ${output} -gt 0; then
	      uses_leapseconds=yes
            fi
	  fi
      fi
    ])
  rm -rf conftest conftest.* core
  if test -n "${uses_leapseconds}"; then
    AC_MSG_RESULT([yes])
    AC_DEFINE(USING_LEAPSECONDS, 1)
  else
    AC_MSG_RESULT([no])
  fi
])
dnl
dnl configure related programs
dnl
missing_dir=`cd ${ac_aux_dir} && pwd`
AM_MISSING_PROG(ACLOCAL, aclocal, ${missing_dir})
AM_MISSING_PROG(AUTOCONF, autoconf, ${missing_dir})
AM_MISSING_PROG(AUTOMAKE, automake, ${missing_dir})
AM_MISSING_PROG(AUTOHEADER, autoheader, ${missing_dir})

dnl Local Variables: ***
dnl mode: sh ***
dnl sh-indentation: 2 ***
dnl End: ***

# Define a conditional.

AC_DEFUN(AM_CONDITIONAL,
[AC_SUBST($1_TRUE)
AC_SUBST($1_FALSE)
if $2; then
  $1_TRUE=
  $1_FALSE='#'
else
  $1_TRUE='#'
  $1_FALSE=
fi])

dnl AM_MISSING_PROG(NAME, PROGRAM, DIRECTORY)
dnl The program must properly implement --version.
AC_DEFUN(AM_MISSING_PROG,
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

# Do all the work for Automake.  This macro actually does too much --
# some checks are only needed if your package does certain things.
# But this isn't really a big deal.

# serial 1

dnl Usage:
dnl AM_INIT_AUTOMAKE(package,version, [no-define])

AC_DEFUN(AM_INIT_AUTOMAKE,
[AC_REQUIRE([AC_PROG_INSTALL])
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
AM_MISSING_PROG(ACLOCAL, aclocal, $missing_dir)
AM_MISSING_PROG(AUTOCONF, autoconf, $missing_dir)
AM_MISSING_PROG(AUTOMAKE, automake, $missing_dir)
AM_MISSING_PROG(AUTOHEADER, autoheader, $missing_dir)
AM_MISSING_PROG(MAKEINFO, makeinfo, $missing_dir)
AC_REQUIRE([AC_PROG_MAKE_SET])])

#
# Check to make sure that the build environment is sane.
#

AC_DEFUN(AM_SANITY_CHECK,
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

# Like AC_CONFIG_HEADER, but automatically create stamp file.

AC_DEFUN(AM_CONFIG_HEADER,
[AC_PREREQ([2.12])
AC_CONFIG_HEADER([$1])
dnl When config.status generates a header, we must update the stamp-h file.
dnl This file resides in the same directory as the config header
dnl that is generated.  We must strip everything past the first ":",
dnl and everything past the last "/".
AC_OUTPUT_COMMANDS(changequote(<<,>>)dnl
ifelse(patsubst(<<$1>>, <<[^ ]>>, <<>>), <<>>,
<<test -z "<<$>>CONFIG_HEADERS" || echo timestamp > patsubst(<<$1>>, <<^\([^:]*/\)?.*>>, <<\1>>)stamp-h<<>>dnl>>,
<<am_indx=1
for am_file in <<$1>>; do
  case " <<$>>CONFIG_HEADERS " in
  *" <<$>>am_file "*<<)>>
    echo timestamp > `echo <<$>>am_file | sed -e 's%:.*%%' -e 's%[^/]*$%%'`stamp-h$am_indx
    ;;
  esac
  am_indx=`expr "<<$>>am_indx" + 1`
done<<>>dnl>>)
changequote([,]))])


# serial 40 AC_PROG_LIBTOOL
AC_DEFUN(AC_PROG_LIBTOOL,
[AC_REQUIRE([AC_LIBTOOL_SETUP])dnl

# Save cache, so that ltconfig can load it
AC_CACHE_SAVE

# Actually configure libtool.  ac_aux_dir is where install-sh is found.
CC="$CC" CFLAGS="$CFLAGS" CPPFLAGS="$CPPFLAGS" \
LD="$LD" LDFLAGS="$LDFLAGS" LIBS="$LIBS" \
LN_S="$LN_S" NM="$NM" RANLIB="$RANLIB" \
DLLTOOL="$DLLTOOL" AS="$AS" OBJDUMP="$OBJDUMP" \
${CONFIG_SHELL-/bin/sh} $ac_aux_dir/ltconfig --no-reexec \
$libtool_flags --no-verify $ac_aux_dir/ltmain.sh $host \
|| AC_MSG_ERROR([libtool configure failed])

# Reload cache, that may have been modified by ltconfig
AC_CACHE_LOAD

# This can be used to rebuild libtool when needed
LIBTOOL_DEPS="$ac_aux_dir/ltconfig $ac_aux_dir/ltmain.sh"

# Always use our own libtool.
LIBTOOL='$(SHELL) $(top_builddir)/libtool'
AC_SUBST(LIBTOOL)dnl

# Redirect the config.log output again, so that the ltconfig log is not
# clobbered by the next message.
exec 5>>./config.log
])

AC_DEFUN(AC_LIBTOOL_SETUP,
[AC_PREREQ(2.13)dnl
AC_REQUIRE([AC_ENABLE_SHARED])dnl
AC_REQUIRE([AC_ENABLE_STATIC])dnl
AC_REQUIRE([AC_ENABLE_FAST_INSTALL])dnl
AC_REQUIRE([AC_CANONICAL_HOST])dnl
AC_REQUIRE([AC_CANONICAL_BUILD])dnl
AC_REQUIRE([AC_PROG_RANLIB])dnl
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_PROG_LD])dnl
AC_REQUIRE([AC_PROG_NM])dnl
AC_REQUIRE([AC_PROG_LN_S])dnl
dnl

# Check for any special flags to pass to ltconfig.
libtool_flags="--cache-file=$cache_file"
test "$enable_shared" = no && libtool_flags="$libtool_flags --disable-shared"
test "$enable_static" = no && libtool_flags="$libtool_flags --disable-static"
test "$enable_fast_install" = no && libtool_flags="$libtool_flags --disable-fast-install"
test "$ac_cv_prog_gcc" = yes && libtool_flags="$libtool_flags --with-gcc"
test "$ac_cv_prog_gnu_ld" = yes && libtool_flags="$libtool_flags --with-gnu-ld"
ifdef([AC_PROVIDE_AC_LIBTOOL_DLOPEN],
[libtool_flags="$libtool_flags --enable-dlopen"])
ifdef([AC_PROVIDE_AC_LIBTOOL_WIN32_DLL],
[libtool_flags="$libtool_flags --enable-win32-dll"])
AC_ARG_ENABLE(libtool-lock,
  [  --disable-libtool-lock  avoid locking (might break parallel builds)])
test "x$enable_libtool_lock" = xno && libtool_flags="$libtool_flags --disable-lock"
test x"$silent" = xyes && libtool_flags="$libtool_flags --silent"

# Some flags need to be propagated to the compiler or linker for good
# libtool support.
case "$host" in
*-*-irix6*)
  # Find out which ABI we are using.
  echo '[#]line __oline__ "configure"' > conftest.$ac_ext
  if AC_TRY_EVAL(ac_compile); then
    case "`/usr/bin/file conftest.o`" in
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
[*-*-cygwin* | *-*-mingw*)
  AC_CHECK_TOOL(DLLTOOL, dlltool, false)
  AC_CHECK_TOOL(AS, as, false)
  AC_CHECK_TOOL(OBJDUMP, objdump, false)
  ;;
])
esac
])

# AC_LIBTOOL_DLOPEN - enable checks for dlopen support
AC_DEFUN(AC_LIBTOOL_DLOPEN, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])])

# AC_LIBTOOL_WIN32_DLL - declare package support for building win32 dll's
AC_DEFUN(AC_LIBTOOL_WIN32_DLL, [AC_BEFORE([$0], [AC_LIBTOOL_SETUP])])

# AC_ENABLE_SHARED - implement the --enable-shared flag
# Usage: AC_ENABLE_SHARED[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN(AC_ENABLE_SHARED, [dnl
define([AC_ENABLE_SHARED_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(shared,
changequote(<<, >>)dnl
<<  --enable-shared[=PKGS]  build shared libraries [default=>>AC_ENABLE_SHARED_DEFAULT],
changequote([, ])dnl
[p=${PACKAGE-default}
case "$enableval" in
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
AC_DEFUN(AC_DISABLE_SHARED, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_SHARED(no)])

# AC_ENABLE_STATIC - implement the --enable-static flag
# Usage: AC_ENABLE_STATIC[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN(AC_ENABLE_STATIC, [dnl
define([AC_ENABLE_STATIC_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(static,
changequote(<<, >>)dnl
<<  --enable-static[=PKGS]  build static libraries [default=>>AC_ENABLE_STATIC_DEFAULT],
changequote([, ])dnl
[p=${PACKAGE-default}
case "$enableval" in
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
AC_DEFUN(AC_DISABLE_STATIC, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_STATIC(no)])


# AC_ENABLE_FAST_INSTALL - implement the --enable-fast-install flag
# Usage: AC_ENABLE_FAST_INSTALL[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN(AC_ENABLE_FAST_INSTALL, [dnl
define([AC_ENABLE_FAST_INSTALL_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(fast-install,
changequote(<<, >>)dnl
<<  --enable-fast-install[=PKGS]  optimize for fast installation [default=>>AC_ENABLE_FAST_INSTALL_DEFAULT],
changequote([, ])dnl
[p=${PACKAGE-default}
case "$enableval" in
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

# AC_ENABLE_FAST_INSTALL - set the default to --disable-fast-install
AC_DEFUN(AC_DISABLE_FAST_INSTALL, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_FAST_INSTALL(no)])

# AC_PROG_LD - find the path to the GNU or non-GNU linker
AC_DEFUN(AC_PROG_LD,
[AC_ARG_WITH(gnu-ld,
[  --with-gnu-ld           assume the C compiler uses GNU ld [default=no]],
test "$withval" = no || with_gnu_ld=yes, with_gnu_ld=no)
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_CANONICAL_HOST])dnl
AC_REQUIRE([AC_CANONICAL_BUILD])dnl
ac_prog=ld
if test "$ac_cv_prog_gcc" = yes; then
  # Check if gcc -print-prog-name=ld gives a path.
  AC_MSG_CHECKING([for ld used by GCC])
  ac_prog=`($CC -print-prog-name=ld) 2>&5`
  case "$ac_prog" in
    # Accept absolute paths.
changequote(,)dnl
    [\\/]* | [A-Za-z]:[\\/]*)
      re_direlt='/[^/][^/]*/\.\./'
changequote([,])dnl
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
AC_CACHE_VAL(ac_cv_path_LD,
[if test -z "$LD"; then
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}${PATH_SEPARATOR-:}"
  for ac_dir in $PATH; do
    test -z "$ac_dir" && ac_dir=.
    if test -f "$ac_dir/$ac_prog" || test -f "$ac_dir/$ac_prog$ac_exeext"; then
      ac_cv_path_LD="$ac_dir/$ac_prog"
      # Check to see if the program is GNU ld.  I'd rather use --version,
      # but apparently some GNU ld's only accept -v.
      # Break only if it was the GNU/non-GNU ld that we prefer.
      if "$ac_cv_path_LD" -v 2>&1 < /dev/null | egrep '(GNU|with BFD)' > /dev/null; then
	test "$with_gnu_ld" != no && break
      else
	test "$with_gnu_ld" != yes && break
      fi
    fi
  done
  IFS="$ac_save_ifs"
else
  ac_cv_path_LD="$LD" # Let the user override the test with a path.
fi])
LD="$ac_cv_path_LD"
if test -n "$LD"; then
  AC_MSG_RESULT($LD)
else
  AC_MSG_RESULT(no)
fi
test -z "$LD" && AC_MSG_ERROR([no acceptable ld found in \$PATH])
AC_SUBST(LD)
AC_PROG_LD_GNU
])

AC_DEFUN(AC_PROG_LD_GNU,
[AC_CACHE_CHECK([if the linker ($LD) is GNU ld], ac_cv_prog_gnu_ld,
[# I'd rather use --version here, but apparently some GNU ld's only accept -v.
if $LD -v 2>&1 </dev/null | egrep '(GNU|with BFD)' 1>&5; then
  ac_cv_prog_gnu_ld=yes
else
  ac_cv_prog_gnu_ld=no
fi])
])

# AC_PROG_NM - find the path to a BSD-compatible name lister
AC_DEFUN(AC_PROG_NM,
[AC_MSG_CHECKING([for BSD-compatible nm])
AC_CACHE_VAL(ac_cv_path_NM,
[if test -n "$NM"; then
  # Let the user override the test.
  ac_cv_path_NM="$NM"
else
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}${PATH_SEPARATOR-:}"
  for ac_dir in $PATH /usr/ccs/bin /usr/ucb /bin; do
    test -z "$ac_dir" && ac_dir=.
    if test -f $ac_dir/nm || test -f $ac_dir/nm$ac_exeext ; then
      # Check to see if the nm accepts a BSD-compat flag.
      # Adding the `sed 1q' prevents false positives on HP-UX, which says:
      #   nm: unknown option "B" ignored
      if ($ac_dir/nm -B /dev/null 2>&1 | sed '1q'; exit 0) | egrep /dev/null >/dev/null; then
	ac_cv_path_NM="$ac_dir/nm -B"
	break
      elif ($ac_dir/nm -p /dev/null 2>&1 | sed '1q'; exit 0) | egrep /dev/null >/dev/null; then
	ac_cv_path_NM="$ac_dir/nm -p"
	break
      else
	ac_cv_path_NM=${ac_cv_path_NM="$ac_dir/nm"} # keep the first match, but
	continue # so that we can try to find one that supports BSD flags
      fi
    fi
  done
  IFS="$ac_save_ifs"
  test -z "$ac_cv_path_NM" && ac_cv_path_NM=nm
fi])
NM="$ac_cv_path_NM"
AC_MSG_RESULT([$NM])
AC_SUBST(NM)
])

# AC_CHECK_LIBM - check for math library
AC_DEFUN(AC_CHECK_LIBM,
[AC_REQUIRE([AC_CANONICAL_HOST])dnl
LIBM=
case "$host" in
*-*-beos* | *-*-cygwin*)
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
# the libltdl convenience library, adds --enable-ltdl-convenience to
# the configure arguments.  Note that LIBLTDL is not AC_SUBSTed, nor
# is AC_CONFIG_SUBDIRS called.  If DIR is not provided, it is assumed
# to be `${top_builddir}/libltdl'.  Make sure you start DIR with
# '${top_builddir}/' (note the single quotes!) if your package is not
# flat, and, if you're not using automake, define top_builddir as
# appropriate in the Makefiles.
AC_DEFUN(AC_LIBLTDL_CONVENIENCE, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
  case "$enable_ltdl_convenience" in
  no) AC_MSG_ERROR([this package needs a convenience libltdl]) ;;
  "") enable_ltdl_convenience=yes
      ac_configure_args="$ac_configure_args --enable-ltdl-convenience" ;;
  esac
  LIBLTDL=ifelse($#,1,$1,['${top_builddir}/libltdl'])/libltdlc.la
  INCLTDL=ifelse($#,1,-I$1,['-I${top_builddir}/libltdl'])
])

# AC_LIBLTDL_INSTALLABLE[(dir)] - sets LIBLTDL to the link flags for
# the libltdl installable library, and adds --enable-ltdl-install to
# the configure arguments.  Note that LIBLTDL is not AC_SUBSTed, nor
# is AC_CONFIG_SUBDIRS called.  If DIR is not provided, it is assumed
# to be `${top_builddir}/libltdl'.  Make sure you start DIR with
# '${top_builddir}/' (note the single quotes!) if your package is not
# flat, and, if you're not using automake, define top_builddir as
# appropriate in the Makefiles.
# In the future, this macro may have to be called after AC_PROG_LIBTOOL.
AC_DEFUN(AC_LIBLTDL_INSTALLABLE, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
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
    LIBLTDL=ifelse($#,1,$1,['${top_builddir}/libltdl'])/libltdl.la
    INCLTDL=ifelse($#,1,-I$1,['-I${top_builddir}/libltdl'])
  else
    ac_configure_args="$ac_configure_args --enable-ltdl-install=no"
    LIBLTDL="-lltdl"
    INCLTDL=
  fi
])

dnl old names
AC_DEFUN(AM_PROG_LIBTOOL, [indir([AC_PROG_LIBTOOL])])dnl
AC_DEFUN(AM_ENABLE_SHARED, [indir([AC_ENABLE_SHARED], $@)])dnl
AC_DEFUN(AM_ENABLE_STATIC, [indir([AC_ENABLE_STATIC], $@)])dnl
AC_DEFUN(AM_DISABLE_SHARED, [indir([AC_DISABLE_SHARED], $@)])dnl
AC_DEFUN(AM_DISABLE_STATIC, [indir([AC_DISABLE_STATIC], $@)])dnl
AC_DEFUN(AM_PROG_LD, [indir([AC_PROG_LD])])dnl
AC_DEFUN(AM_PROG_NM, [indir([AC_PROG_NM])])dnl

dnl This is just to silence aclocal about the macro not being used
ifelse([AC_DISABLE_FAST_INSTALL])dnl

