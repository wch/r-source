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
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
### Boston, MA 02110-1301, USA


## R_RUN_JAVA(variable for the result, parameters)
## ----------
## runs the java interpreter ${JAVA} with specified parameters and
## saves the output to the supplied variable. The exit value is ignored.
AC_DEFUN([R_RUN_JAVA],
[
  acx_java_result=
  if test -z "${JAVA}"; then
    echo "$as_me:$LINENO: JAVA is not set, cannot run java $2" >&AS_MESSAGE_LOG_FD
  else
    echo "$as_me:$LINENO: running ${JAVA} $2" >&AS_MESSAGE_LOG_FD
    acx_java_result=`${JAVA} $2 2>&AS_MESSAGE_LOG_FD`
    echo "$as_me:$LINENO: output: '$acx_java_result'" >&AS_MESSAGE_LOG_FD
  fi
  $1=$acx_java_result
])


## R_JAVA
## -----------
## Looks for Java JRE/JDK and sets:
## have_java to yes/no; if it is yes then also sets:
## JAVA to Java interpreter path
## JAVA_HOME to the home directory of the Java runtime/jdk
## JAVA_LD_LIBRARY_PATH to the path necessary for Java runtime
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
  ## try jre/bin first just in case we don't have full JDK
  JAVA_PATH=${JAVA_HOME}:${JAVA_HOME}/jre/bin:${JAVA_HOME}/bin:${PATH}
fi
## if 'java' is not on the PATH or JAVA_HOME, add some guesses as of
## where java could live
JAVA_PATH=${JAVA_PATH}:/usr/java/bin:/usr/jdk/bin:/usr/lib/java/bin:/usr/lib/jdk/bin:/usr/local/java/bin:/usr/local/jdk/bin:/usr/local/lib/java/bin:/usr/local/lib/jdk/bin
AC_PATH_PROGS(JAVA,java,,${JAVA_PATH})
## FIXME: we may want to check for jikes, kaffe and others...
## (however, most of them have compatibility wrappers by now)
AC_PATH_PROGS(JAVAC,javac,,${JAVA_PATH})
AC_PATH_PROGS(JAVAH,javah,,${JAVA_PATH})
AC_PATH_PROGS(JAR,jar,,${JAVA_PATH})

## we don't require a compiler, but it would be useful
AC_MSG_CHECKING([whether Java compiler works])
acx_javac_works=no
if test -n "${JAVAC}"; then
  rm -f A.java A.class
  echo "public class A { }" > A.java
  if "${JAVAC}" A.java 2>&AS_MESSAGE_LOG_FD; then
    if test -e A.class; then
      acx_javac_works=yes
    fi
  fi
  rm -rf A.java A.class
fi
if test "${acx_javac_works}" = yes; then
  AC_MSG_RESULT([yes])
else
  JAVAC=
  AC_MSG_RESULT([no])
fi

## this is where our test-class lives (in tools directory)
getsp_cp=${ac_aux_dir}

AC_MSG_CHECKING([whether Java interpreter works])
acx_java_works=no
if test -n "${JAVA}" ; then
  R_RUN_JAVA(acx_jc_result,[-classpath ${getsp_cp} getsp -test])
  if test "${acx_jc_result}" = "Test1234OK"; then
    acx_java_works=yes
  fi
  acx_jc_result=
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
        JAVA_LIBS0="-framework JavaVM"
        JAVA_LD_LIBRARY_PATH=
        ;;
      *)
        R_RUN_JAVA(JAVA_LIBS, [-classpath ${getsp_cp} getsp -libs])
        JAVA_LIBS="${JAVA_LIBS} -ljvm"
        R_RUN_JAVA(JAVA_LD_LIBRARY_PATH, [-classpath ${getsp_cp} getsp java.library.path])

	JAVA_LIBS0=`echo ${JAVA_LIBS} | sed -e s:${JAVA_HOME}:\$\(JAVA_HOME\):g`
	JAVA_LD_LIBRARY_PATH=`echo ${JAVA_LD_LIBRARY_PATH} | sed -e s:${JAVA_HOME}:\$\(JAVA_HOME\):g`

	## FIXME: we may want to figure out whether JAVA_HOME is a symlink
	## to any paths in JAVA_LD_LIBARARY_PATH in which case we should use
	## the symlink instead. This would solve versioning problems across
	## Sun's JDKs.
        ;;
    esac
  
    ## note that we actually don't test JAVA_LIBS - we hope that the detection
    ## was correct.

    have_java=yes
  fi
else
  AC_MSG_RESULT([no])
  JAVA=
  JAVA_HOME=
fi

AC_SUBST(JAVA_HOME)
AC_SUBST(JAVA)
AC_SUBST(JAVAC)
AC_SUBST(JAVAH)
AC_SUBST(JAR)
AC_SUBST(JAVA_LD_LIBRARY_PATH)
AC_SUBST(JAVA_LIBS0)
])
