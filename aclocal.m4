dnl aclocal.m4 -- extra macros for configuring R
dnl
dnl Copyright (C) 1998, 1999, 2000 R Core Team
dnl
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
	@$(CC) -M $(ALL_CPPFLAGS) $< > $[@]
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
	## This should really use MAINLD, and hence come after this is
	## determined.  Or maybe we can always use ${CC} eventually?
	if ${CC-cc} ${LDFLAGS} ${MAINLDFLAGS} -o conftest conftest.o conftestf.o \
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
    [  --with-gnome-includes   Specify location of GNOME headers], [
    CFLAGS="$CFLAGS -I$withval"
  ])
	
  AC_ARG_WITH(gnome-libs,
    [  --with-gnome-libs       Specify location of GNOME libs], [
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

AC_DEFUN(AM_CONDITIONAL, [
  AC_SUBST($1_TRUE)
  AC_SUBST($1_FALSE)
  if $2; then
    $1_TRUE=
    $1_FALSE='#'
  else
    $1_TRUE='#'
    $1_FALSE=
  fi
])

dnl AM_PATH_LIBGLADE([ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND [, MODULES]]])
dnl Test to see if libglade is installed, and define LIBGLADE_CFLAGS, LIBS
dnl
AC_DEFUN(AM_PATH_LIBGLADE,
[dnl
dnl Get the cflags and libraries from the libglade-config script
dnl
AC_ARG_WITH(libglade-config,
[  --with-libglade-config=LIBGLADE_CONFIG  Location of libglade-config],
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

AC_DEFUN(AM_CONDITIONAL, [
  AC_SUBST($1_TRUE)
  AC_SUBST($1_FALSE)
  if $2; then
    $1_TRUE=
    $1_FALSE='#'
  else
    $1_TRUE='#'
    $1_FALSE=
  fi
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
      AC_CHECK_HEADER(${TK_PREFIX}/include/tk${TK_VERSION}/tk.h,
        [TCLTK_CPPFLAGS="${TCLTK_CPPFLAGS} -I${TK_PREFIX}/include/tk${TK_VERSION}"
	  found_tk_h=yes])
      if test "${found_tk_h}" = no; then
	AC_CHECK_HEADER(${TK_PREFIX}/include/tk.h,
          [TCLTK_CPPFLAGS="${TCLTK_CPPFLAGS} -I${TK_PREFIX}/include"
            found_tk_h=yes])
      fi
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

dnl Local Variables: ***
dnl mode: sh ***
dnl sh-indentation: 2 ***
dnl End: ***
