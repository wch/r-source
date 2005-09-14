### java.m4 -- macros for Java environment detection    -*- Autoconf -*-
###
### Copyright (C) 2005 R Core Team
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


## R_RUN_JAVA(variable for the result, parameters)
## ----------
## runs the java interpreter ${JAVA_PROG} with specified parameters and
## saves the output to the supplied variable. The exit value is ignored.
AC_DEFUN([R_RUN_JAVA],
[
  acx_java_result=
  if test -z "${JAVA_PROG}"; then
    echo "$as_me:$LINENO: JAVA_PROG is not set, cannot run java $2" >&AS_MESSAGE_LOG_FD
  else
    echo "$as_me:$LINENO: running ${JAVA_PROG} $2" >&AS_MESSAGE_LOG_FD
    acx_java_result=`${JAVA_PROG} $2 2>&AS_MESSAGE_LOG_FD`
    echo "$as_me:$LINENO: output: '$acx_java_result'" >&AS_MESSAGE_LOG_FD
  fi
  AC_SUBST([$1], [$acx_java_result])
])


## R_JAVA
## -----------
## Looks for Java JRE/JDK and sets:
## have_java to yes/no; if it is yes then also sets:
## JAVA_PROG to Java interpreter path
## JAVA_HOME to the home directory of the Java runtime/jdk
## JAVA_LD_PATH to the path necessary for Java runtime
## JAVA_LIBS to flags necessary to link JNI programs (*)
##
## JAVA_HOME env var is honored during the search and the search
## will fail if it is set incorrectly.
AC_DEFUN([R_JAVA],
[
have_java=no

## find java compiler binaries
if test -z "${JAVA_HOME}" ; then
  JAVA_PATH=${PATH}
else
  JAVA_PATH=${JAVA_HOME}:${JAVA_HOME}/bin:${PATH}
fi
## if 'java' is not on the PATH or JAVA_HOME, add some guesses as of
## where java could live
JAVA_PATH=${JAVA_PATH}:/usr/java/bin:/usr/jdk/bin:/usr/lib/java/bin:/usr/lib/jdk/bin:/usr/local/java/bin:/usr/local/jdk/bin:/usr/local/lib/java/bin:/usr/local/lib/jdk/bin
AC_PATH_PROGS(JAVA_PROG,java,,${JAVA_PATH})
## FIXME: we may want to check for jikes, kaffe and others...
AC_PATH_PROGS(JAVAC,javac,,${JAVA_PATH})

## this is where our test-class lives (in tools directory)
getsp_cp=${ac_aux_dir}

AC_MSG_CHECKING([whether Java interpreter works])
acx_java_works=no
if test -n "${JAVA_PROG}" ; then
  R_RUN_JAVA(jc_result,[-classpath ${getsp_cp} getsp -test])
  if test "${jc_result}" = "Test1234OK"; then
    acx_java_works=yes
  fi
fi

if test ${acx_java_works} = yes; then
  AC_MSG_RESULT([yes])

  AC_MSG_CHECKING([for Java environment])


  ## retrieve JAVA_HOME from Java itself if not set 
  if test -z "${JAVA_HOME}" ; then
    R_RUN_JAVA(JAVA_HOME,[-classpath ${getsp_cp} getsp java.home])
  fi

  ## the availability of JAVA_HOME will tell us whether it's supported
  if test -z "${JAVA_HOME}" ; then
    if test x$acx_java_env_msg != xyes; then
      AC_MSG_RESULT([not found])
    fi
  else
    AC_MSG_RESULT([in ${JAVA_HOME}])

    case "${host_os}" in
      darwin*)
        JAVA_LIBS="-framework JavaVM"
        JAVA_LD_PATH=
        ;;
      *)
        R_RUN_JAVA(JAVA_LIBS, [-classpath ${getsp_cp} getsp -libs])
        JAVA_LIBS="${JAVA_LIBS} -ljvm"
        R_RUN_JAVA(JAVA_LD_PATH, [-classpath ${getsp_cp} getsp java.library.path])
        ;;
    esac
  
    ## note that we actually don't test JAVA_LIBS - we hope that the detection
    ## was correct. We should also test the functionality for javac.

    have_java=yes
  fi
else
  AC_MSG_RESULT([no])
  JAVA_PROG=
  JAVAC=
  JAVA_HOME=
fi

AC_SUBST(JAVA_HOME)
AC_SUBST(JAVA_PROG)
AC_SUBST(JAVA_LD_PATH)
AC_SUBST(JAVA_LIBS)
])
