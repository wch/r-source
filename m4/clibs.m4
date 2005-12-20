### clibs.m4 -- extra macros for configuring R		-*- Autoconf -*-
###
### Copyright (C) 2004 R Core Team
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
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
### Boston, MA 02110-1301, USA

## modified version of AC_F77_LIBRARY_LDFLAGS for C libraries


# _R_PROG_C_V_OUTPUT([FLAG = $r_cv_prog_c_v])
# -------------------------------------------------
# Link a trivial Fortran program, compiling with a verbose output FLAG
# (whose default value, $r_cv_prog_c_v, is computed by
# _R_PROG_C_V), and return the output in $r_c_v_output.  This
# output is processed in the way expected by _AC_FC_LIBRARY_LDFLAGS,
# so that any link flags that are echoed by the compiler appear as
# space-separated items.
AC_DEFUN([_R_PROG_C_V_OUTPUT],
[AC_LANG_CONFTEST([AC_LANG_PROGRAM([])])

# Compile and link our simple test program by passing a flag (argument
# 1 to this macro) to the Fortran compiler in order to get
# "verbose" output that we can then parse for the Fortran linker
# flags.
r_save_CFLAGS=$CFLAGS
CFLAGS="$CFLAGS m4_default([$1], [$r_cv_prog_c_v])"
(eval echo $as_me:__oline__: \"$ac_link\") >&AS_MESSAGE_LOG_FD
r_c_v_output=`eval $ac_link AS_MESSAGE_LOG_FD>&1 2>&1 | grep -v 'Driving:'`
echo "$r_c_v_output" >&AS_MESSAGE_LOG_FD
CFLAGS=$r_save_CFLAGS

rm -f conftest*

# On HP/UX there is a line like: "LPATH is: /foo:/bar:/baz" where
# /foo, /bar, and /baz are search directories for the Fortran linker.
# Here, we change these into -L/foo -L/bar -L/baz (and put it first):
r_c_v_output="`echo $r_c_v_output |
	grep 'LPATH is:' |
	sed 's,.*LPATH is\(: *[[^ ]]*\).*,\1,;s,: */, -L/,g'` $r_c_v_output"

case $r_c_v_output in
  # If we are using xlc then replace all the commas with spaces.
  *xlcentry*)
    r_c_v_output=`echo $r_c_v_output | sed 's/,/ /g'` ;;

  # With Intel ifc, ignore the quoted -mGLOB_options_string stuff (quoted
  # $LIBS confuse us, and the libraries appear later in the output anyway).
  *mGLOB_options_string*)
    r_c_v_output=`echo $r_c_v_output | sed 's/\"-mGLOB[[^\"]]*\"/ /g'` ;;
esac

])# _R_PROG_C_V_OUTPUT


# _R_PROG_C_V
# --------------
#
# Determine the flag that causes the Fortran compiler to print
# information of library and object files (normally -v)
# Needed for _AC_FC_LIBRARY_FLAGS
# Some compilers don't accept -v (Lahey: -verbose, xlf: -V, Fujitsu: -###)
AC_DEFUN([_R_PROG_C_V],
[AC_CACHE_CHECK([how to get verbose linking output from ${CC}],
                [r_cv_prog_c_v],
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM()],
[r_cv_prog_c_v=
# Try some options frequently used verbose output
for r_verb in -v -verbose --verbose -V -\#\#\#; do
  _R_PROG_C_V_OUTPUT($r_verb)
  # look for -l* and *.a constructs in the output
  for r_arg in $r_c_v_output; do
     case $r_arg in
        [[\\/]]*.a | ?:[[\\/]]*.a | -[[lLRu]]*)
          r_cv_prog_c_v=$r_verb
          break 2 ;;
     esac
  done
done
if test -z "$r_cv_prog_c_v"; then
   AC_MSG_WARN([cannot determine how to obtain linking information from ${CC}])
fi],
                  [AC_MSG_WARN([compilation failed])])
])])# _R_PROG_C_V


# _R_C_LIBRARY_LDFLAGS
# ----------------------
#
# Determine the linker flags (e.g. "-L" and "-l") for the C
# intrinsic and run-time libraries that are required to successfully
# link a C program or shared library.  The output variable
# CLIBS is set to these flags.
#
# This macro is intended to be used in those situations when it is
# necessary to mix, e.g. C++ and Fortran, source code into a single
# program or shared library.
#
# For example, if object files from a C++ and Fortran compiler must
# be linked together, then the C++ compiler/linker must be used for
# linking (since special C++-ish things need to happen at link time
# like calling global constructors, instantiating templates, enabling
# exception support, etc.).
#
# This macro was packaged in its current form by Matthew D. Langston.
# However, nearly all of this macro came from the "OCTAVE_FLIBS" macro
# in "octave-2.0.13/aclocal.m4", and full credit should go to John
# W. Eaton for writing this extremely useful macro.  Thank you John.
AC_DEFUN([_R_C_LIBRARY_LDFLAGS],
[_R_PROG_C_V
AC_CACHE_CHECK([for C libraries of ${CC}], r_cv_c_libs,
[if test "x$CLIBS" != "x"; then
  r_cv_c_libs="$CLIBS" # Let the user override the test.
else

_R_PROG_C_V_OUTPUT

r_cv_c_libs=

# Save positional arguments (if any)
r_save_positional="$[@]"

set X $r_c_v_output
while test $[@%:@] != 1; do
  shift
  r_arg=$[1]
  case $r_arg in
        [[\\/]]*.a | ?:[[\\/]]*.a)
          _AC_LIST_MEMBER_IF($r_arg, $r_cv_c_libs, ,
              r_cv_c_libs="$r_cv_c_libs $r_arg")
          ;;
        -bI:*)
          _AC_LIST_MEMBER_IF($r_arg, $r_cv_c_libs, ,
             [_AC_LINKER_OPTION([$r_arg], r_cv_c_libs)])
          ;;
          # Ignore these flags.
        -lang* | -lcrt[[01]].o | -lcrtbegin.o | -lc | -lgcc | -libmil | -LANG:=*)
          ;;
        -lkernel32)
          test x"$CYGWIN" != xyes && r_cv_c_libs="$r_cv_c_libs $r_arg"
          ;;
        -[[LRuY]])
          # These flags, when seen by themselves, take an argument.
          # We remove the space between option and argument and re-iterate
          # unless we find an empty arg or a new option (starting with -)
	  case $[2] in
	     "" | -*);;
	     *)
		r_arg="$r_arg$[2]"
		shift; shift
		set X $r_arg "$[@]"
		;;
	  esac
          ;;
        -YP,*)
          for r_j in `echo $r_arg | sed -e 's/-YP,/-L/;s/:/ -L/g'`; do
            _AC_LIST_MEMBER_IF($r_j, $r_cv_c_libs, ,
                               [r_arg="$r_arg $r_j"
                               r_cv_c_libs="$r_cv_c_libs $r_j"])
          done
          ;;
        -[[lLR]]*)
          _AC_LIST_MEMBER_IF($r_arg, $r_cv_c_libs, ,
                             r_cv_c_libs="$r_cv_c_libs $r_arg")
          ;;
          # Ignore everything else.
  esac
done
# restore positional arguments
set X $r_save_positional; shift

# We only consider "LD_RUN_PATH" on Solaris systems.  If this is seen,
# then we insist that the "run path" must be an absolute path (i.e. it
# must begin with a "/").
case `(uname -sr) 2>/dev/null` in
   "SunOS 5"*)
      r_ld_run_path=`echo $r_c_v_output |
                        sed -n 's,^.*LD_RUN_PATH *= *\(/[[^ ]]*\).*$,-R\1,p'`
      test "x$r_ld_run_path" != x &&
        _AC_LINKER_OPTION([$r_ld_run_path], r_cv_c_libs)
      ;;
esac
fi # test "x$CLIBS" = "x"
])
CLIBS=
for arg in $r_cv_c_libs; do
  case "${arg}" in
    -L*)
      CLIBS="${CLIBS} $arg"
      ;;
  esac
done
])# _R_C_LIBRARY_LDFLAGS


# R_C_LIBRARY_LDFLAGS
# -------------------
AC_DEFUN([R_C_LIBRARY_LDFLAGS],
[AC_REQUIRE([AC_PROG_CC])dnl
AC_LANG_PUSH(C)dnl
_R_C_LIBRARY_LDFLAGS
AC_LANG_POP(C)dnl
])# R_C_LIBRARY_LDFLAGS

