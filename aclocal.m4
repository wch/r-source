dnl aclocal.m4 -- extra macros for configuring R
dnl
dnl Copyright (C) 1998, 1999 R Core Team
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
dnl R_PROG_PERL
dnl
AC_DEFUN(R_PROG_PERL,
 [AC_PATH_PROG(PERL, perl)
  if test -n "${PERL}"; then
    AC_CACHE_CHECK([whether perl version is at least 5],
      r_cv_prog_perl_v5,
      [ perl_version=`${PERL} -v | \
	  sed -n 's/^.*perl.*version \(.\).*/\1/p'`
	if test ${perl_version} -ge 5; then
	  r_cv_prog_perl_v5=yes
	else
	  r_cv_prog_perl_v5=no
	fi
      ])
  else
    PERL=false
  fi
  if test "${r_cv_prog_perl_v5}" = yes; then
    NO_PERL5=false
  else
    AC_MSG_WARN([you cannot build the object documentation system])
    NO_PERL5=true
  fi
  AC_SUBST(NO_PERL5)
 ])
dnl
dnl R_PROG_TEXMF
dnl
AC_DEFUN(R_PROG_TEXMF,
 [AC_REQUIRE([R_PROG_PERL])
  AC_PATH_PROG(DVIPS, [${DVIPS} dvips], false)
  AC_PATH_PROG(LATEX, [${LATEX} latex], false)
  if test "{ac_cv_path_LATEX}" = false; then
    AC_MSG_WARN([you cannot build DVI versions of the R manuals])
  fi
  AC_PATH_PROG(MAKEINDEX, [${MAKEINDEX} makeindex], false)
  AC_PATH_PROG(PDFLATEX, [${PDFLATEX} pdflatex], false)
  if test "{ac_cv_path_PDFLATEX}" = false; then
    AC_MSG_WARN([you cannot build PDF versions of the R manuals])
  fi
  AC_PATH_PROG(MAKEINFO, [${MAKEINFO} makeinfo])
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
    AC_MSG_WARN([you cannot build info versions of the R manuals])
    MAKEINFO=false
  fi
  if test "${PERL}" != false; then
    INSTALL_INFO="\$(top_builddir)/tools/install-info"
    AC_SUBST(INSTALL_INFO)
  else
    AC_PATH_PROG(INSTALL_INFO, [${INSTALL_INFO} install-info], false)
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
      [ AC_LANG_C
	XCFLAGS="${CFLAGS}"
	CFLAGS="${CFLAGS} $1"
	AC_TRY_LINK([], [],
	  eval "r_cv_prog_cc_flag_${ac_safe}=yes",
	  eval "r_cv_prog_cc_flag_${ac_safe}=no")
	CFLAGS="${XCFLAGS}"
      ])
    if eval "test \"`echo '$r_cv_prog_cc_flag_'$ac_safe`\" = yes"; then
      AC_MSG_RESULT(yes)
      [$2]
    else
      AC_MSG_RESULT(no)
    fi
  ])
dnl
dnl OCTAVE_FLIBS
dnl
dnl See what libraries are used by the Fortran compiler.
dnl
dnl Write a minimal program and compile it with -v.  I don't know what
dnl to do if your compiler doesn't have -v...
dnl
AC_DEFUN(OCTAVE_FLIBS,
[AC_MSG_CHECKING([for Fortran libraries])
AC_CACHE_VAL(octave_cv_flibs,
[changequote(, )dnl
echo "      END" > conftest.f
foutput=`${F77-f77} -v -o conftest conftest.f 2>&1 | grep -v "^Driving"`
dnl
dnl The easiest thing to do for xlf output is to replace all the commas
dnl with spaces.  Try to only do that if the output is really from xlf,
dnl since doing that causes problems on other systems.
dnl
xlf_p=`echo $foutput | grep xlfentry`
if test -n "$xlf_p"; then
  foutput=`echo $foutput | sed 's/,/ /g'`
fi
dnl
ld_run_path=`echo $foutput | \
  sed -n -e 's/^.*LD_RUN_PATH *= *\([^ ]*\).*/\1/p'`
dnl
dnl We are only supposed to find this on Solaris systems...
dnl Uh, the run path should be absolute, shouldn't it?
dnl
case "$ld_run_path" in
  /*)
    if test "$ac_cv_prog_gcc" = yes; then
      ld_run_path="-Xlinker -R -Xlinker $ld_run_path"
    else
      ld_run_path="-R $ld_run_path"
    fi
  ;;
  *)
    ld_run_path=
  ;;
esac
dnl
flibs=
lflags=
dnl
dnl If want_arg is set, we know we want the arg to be added to the list,
dnl so we don't have to examine it.
dnl
want_arg=
dnl
for arg in $foutput; do
  old_want_arg=$want_arg
  want_arg=
dnl
dnl None of the options that take arguments expect the argument to
dnl start with a -, so pretend we didn't see anything special.
dnl
  if test -n "$old_want_arg"; then
    case "$arg" in
      -*)
	old_want_arg=
      ;;
    esac
  fi
  case "$old_want_arg" in
    '')
      case $arg in
	/*.a)
	  exists=false
	  for f in $lflags; do
	    if test x$arg = x$f; then
	      exists=true
	    fi
	  done
	  if $exists; then
	    arg=
	  else
	    lflags="$lflags $arg"
	  fi
	;;
	-bI:*)
	  exists=false
	  for f in $lflags; do
	    if test x$arg = x$f; then
	      exists=true
	    fi
	  done
	  if $exists; then
	    arg=
	  else
	    if test "$ac_cv_prog_gcc" = yes; then
	      lflags="$lflags -Xlinker $arg"
	    else
	      lflags="$lflags $arg"
	    fi
	  fi
	;;
	-lang* | -lcrt0.o | -lc | -lgcc)
	  arg=
	;;
	-[lLR])
	  want_arg=$arg
	  arg=
	;;
	-[lLR]*)
	  exists=false
	  for f in $lflags; do
	    if test x$arg = x$f; then
	      exists=true
	    fi
	  done
	  if $exists; then
	    arg=
	  else
	    case "$arg" in
	      -lkernel32)
		case "$canonical_host_type" in
		  *-*-cygwin32)
		    arg=
		  ;;
		  *)
		    lflags="$lflags $arg"
		  ;;
		esac
	      ;;
	      -lm)
	      ;;
	      *)
		lflags="$lflags $arg"
	      ;;
	    esac
	  fi
	;;
	-u)
	  want_arg=$arg
	  arg=
	;;
	-Y)
	  want_arg=$arg
	  arg=
	;;
	*)
	  arg=
	;;
      esac
    ;;
    -[lLR])
      arg="$old_want_arg $arg"
    ;;
    -u)
      arg="-u $arg"
    ;;
    -Y)
dnl
dnl Should probably try to ensure unique directory options here too.
dnl This probably only applies to Solaris systems, and then will only
dnl work with gcc...
dnl
      arg=`echo $arg | sed -e 's%^P,%%'`
      SAVE_IFS=$IFS
      IFS=:
      list=
      for elt in $arg; do
	list="$list -L$elt"
      done
      IFS=$SAVE_IFS
      arg="$list"
    ;;
  esac
dnl
  if test -n "$arg"; then
    flibs="$flibs $arg"
  fi
done
if test -n "$ld_run_path"; then
  flibs_result="$ld_run_path $flibs"
else
  flibs_result="$flibs"
fi
changequote([, ])dnl
octave_cv_flibs="$flibs_result"])
FLIBS="$octave_cv_flibs"
AC_MSG_RESULT([$FLIBS])])
dnl
dnl R_PROG_F77_FLIBS
dnl
dnl Determine the libraries wanted by the Fortran compiler, and whether
dnl they have all been installed ...
AC_DEFUN(R_PROG_F77_FLIBS, [
  AC_REQUIRE([OCTAVE_FLIBS])
  AC_CACHE_CHECK([whether Fortran libraries work correctly],
    r_cv_prog_f77_flibs_ok, [
      echo "      END" > conftest.f
      ${FC} -c ${FFLAGS} conftest.f 1>&AC_FD_CC 2>&AC_FD_CC
      ${CC} ${LDFLAGS} -o conftest conftest.o ${FLIBS} \
        1>&AC_FD_CC 2>&AC_FD_CC
      if test ${?} = 0; then
        r_cv_prog_f77_flibs_ok=yes
      else
        r_cv_prog_f77_flibs_ok=no
      fi])
  rm -rf conftest conftest.* core
  if test ${r_cv_prog_f77_flibs_ok} = no; then
    AC_MSG_WARN([Fortran libraries do not work correctly])
    AC_MSG_ERROR([Maybe your Fortran installation is incomplete])
  fi])
dnl
dnl R_PROG_F77_G77
dnl
dnl See if ${F77-f77} is the GNU Fortran compiler
dnl
AC_DEFUN(R_PROG_F77_G77,
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
	if ${CC-cc} ${LDFLAGS} -o conftest conftest.o conftestf.o \
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
    AC_MSG_WARN([I found f2c but not libf2c, or libF77 and libI77])
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
	  RGNOMEDIR="gnome"
	  RGNOMEBIN="\$(top_builddir)/bin/R.GNOME"
	  GNOME_IF_FILES="gnome-interface.glade"],
        [AC_MSG_WARN([GNOME support requires libglade version >= 0.3])],
        gnome)
    fi
  fi
  if test "${use_gnome}" != yes; then
    use_gnome="no"
    RGNOMEDIR=
    RGNOMEBIN=
    GNOME_IF_FILES=
  fi
  AC_SUBST(RGNOMEDIR)
  AC_SUBST(RGNOMEBIN)
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

dnl Local Variables: ***
dnl mode: sh ***
dnl sh-indentation: 2 ***
dnl End: ***
