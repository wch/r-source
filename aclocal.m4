dnl aclocal.m4 -- extra macros for configuring R
dnl
dnl Copyright (C) 1998, 1999 R Core Team
dnl
dnl This file is part of R.
dnl
dnl R is free software; you can redistribute it and/or modify it under
dnl the terms of the GNU General Public License as published by the Free
dnl Software Foundation; either version 2 of the License, or (at your
dnl option) any later version.
dnl
dnl R is distributed in the hope that it will be useful, but WITHOUT ANY
dnl WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
dnl License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with R; if not, you can obtain it via the World Wide Web at
dnl `http://www.gnu.org/copyleft/gpl.html', or by writing to the Free
dnl Software Foundation, 59 Temple Place -- Suite 330, Boston, MA
dnl 02111-3307, USA.

dnl OCTAVE_CHECK_EXCLUSIVE_WITH_OPTIONS
AC_DEFUN(OCTAVE_CHECK_EXCLUSIVE_WITH_OPTIONS,
[if test "${with_$1+set}" = set; then
  if test "${with_$2+set}" = set; then
    if test "$with_$2" = no; then
      true
    else
      $3
    fi
  fi
fi])

dnl See if $F77 is the GNU Fortran compiler
dnl
AC_DEFUN(R_PROG_G77,
[AC_CACHE_CHECK([whether ${F77-f77} is the GNU Fortran compiler],
r_cv_f77_is_g77,
[if ${use_g77}; then
  r_cv_f77_is_g77=yes
else
  foutput=`${F77-f77} -v 2>&1 | egrep "GNU F77|egcs|g77"`
  if test -n "${foutput}"; then
    r_cv_f77_is_g77=yes
  else
    r_cv_f77_is_g77=no
  fi
fi])
])
dnl
dnl See what libraries are used by the Fortran compiler.
dnl
dnl Write a minimal program and compile it with -v.  I don't know what
dnl to do if your compiler doesn't have -v...
dnl
dnl OCTAVE_FLIBS()
AC_DEFUN(OCTAVE_FLIBS,
[AC_MSG_CHECKING([for Fortran libraries])
AC_CACHE_VAL(octave_cv_flibs,
[changequote(, )dnl
echo "      END" > conftest.f
foutput=`${F77-f77} -v -o conftest conftest.f 2>&1`
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
dnl See if the Fortran compiler appends underscores
dnl
AC_DEFUN(R_F77_APPEND_UNDERSCORE,
 [AC_MSG_CHECKING([whether ${F77-f77} appends underscores])
  AC_CACHE_VAL(r_cv_f77_append_underscore,
   [r_cv_f77_append_underscore=no,
    cat > conftestf.f <<EOF
      subroutine try
      end
EOF
    ${FC} -c ${FFLAGS} conftestf.f 2>/dev/null 1>/dev/null
    cat > conftest.c <<EOF
main() { try_(); }
EOF
    ${CC} ${CFLAGS} ${CPPFLAGS} ${LDFLAGS} -o conftest \
      conftest.c conftestf.o 1>/dev/null 2>/dev/null
    if test ${?} = 0; then
      r_cv_f77_append_underscore=yes
    else
      cat > conftest.c <<EOF
main() { try(); }
EOF
      ${CC} ${CFLAGS} ${CPPFLAGS} ${LDFLAGS} -o conftest \
        conftest.c conftestf.o 1>/dev/null 2>/dev/null
      if test ${?} = 0; then
        r_cv_f77_append_underscore=no
      fi
    fi
    rm -rf conftest conftest.* conftestf.*
    if test -z "${r_cv_f77_append_underscore}"; then
      AC_MSG_ERROR([Nothing worked - cannot use FORTRAN])
    fi
  ])
  AC_MSG_RESULT([${r_cv_f77_append_underscore}])
  if test "${r_cv_f77_append_underscore}" = yes; then
    AC_DEFINE(HAVE_F77_UNDERSCORE, 1)
  fi
])

AC_DEFUN(OCTAVE_CC_FLAG, [
  ac_safe=`echo "$1" | sed 'y%./+-%__p_%'`
  AC_MSG_CHECKING(whether ${CC-cc} accepts $1)
  AC_CACHE_VAL(octave_cv_cc_flag_$ac_safe, [
    AC_LANG_SAVE
    AC_LANG_C
    XCFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS $1"
    AC_TRY_LINK([], [],
      eval "octave_cv_cc_flag_$ac_safe=yes",
      eval "octave_cv_cc_flag_$ac_safe=no")
    CFLAGS="$XCFLAGS"
    AC_LANG_RESTORE
  ])
  if eval "test \"`echo '$octave_cv_cc_flag_'$ac_safe`\" = yes"; then
    AC_MSG_RESULT(yes)
    ifelse([$2], , [
      CFLAGS="$CFLAGS $1"
      AC_MSG_RESULT([adding $1 to CFLAGS])], [$2])
  else
    AC_MSG_RESULT(no)
    ifelse([$3], , , [$3])
  fi
])

dnl
dnl GNOME_INIT_HOOK (script-if-gnome-enabled, failflag)
dnl
dnl if failflag is "fail" then GNOME_INIT_HOOK will abort if gnomeConf.sh
dnl is not found. 
dnl
AC_DEFUN([GNOME_INIT_HOOK],
[	
  AC_SUBST(GNOME_LIBS)
  AC_SUBST(GNOMEUI_LIBS)
  AC_SUBST(GNOMEGNORBA_LIBS)
  AC_SUBST(GTKXMHTML_LIBS)
  AC_SUBST(GNOME_APPLET_LIBS)
  AC_SUBST(GNOME_LIBDIR)
  AC_SUBST(GNOME_INCLUDEDIR)

  AC_ARG_WITH(gnome-includes,
  [  --with-gnome-includes   Specify location of GNOME headers],[
    CFLAGS="$CFLAGS -I$withval"
  ])
	
  AC_ARG_WITH(gnome-libs,
  [  --with-gnome-libs       Specify location of GNOME libs],[
    LDFLAGS="$LDFLAGS -L$withval"
    gnome_prefix=$withval
  ])

  AC_ARG_WITH(gnome,
  [  --with-gnome            Specify prefix for GNOME files],
    if test "${withval}" = yes; then
      want_gnome=yes
      dnl Note that an empty true branch is not valid sh syntax.
      ifelse([$1], [], :, [$1])
    else
      if test "${withval}" = no; then
	want_gnome=no
      else
	want_gnome=yes
	LDFLAGS="$LDFLAGS -L${withval}/lib"
	CFLAGS="$CFLAGS -I${withval}/include"
	gnome_prefix=${withval}/lib
      fi
    fi,
    want_gnome=yes)

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
      if test "${2}" = fail; then
	AC_MSG_ERROR(Could not find the gnomeConf.sh file that is generated by gnome-libs install)
      fi
    fi
  fi
fi
])

AC_DEFUN([GNOME_INIT],[
	GNOME_INIT_HOOK([],fail)
])

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
