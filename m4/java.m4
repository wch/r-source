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

AC_MSG_CHECKING([for Java environment])

## this is where our test-class lives (in tools directory)
getsp_cp=${ac_aux_dir}

## retrieve JAVA_HOME from Java itself if not set and we found
## the `java' interpreter
if test -n "${JAVA_PROG}" ; then
  if test -z "${JAVA_HOME}" ; then
    JAVA_HOME=`${JAVA_PROG} -classpath ${getsp_cp} getsp java.home`
  fi
fi

## the availability of JAVA_HOME will tell us whether it's supported
if test -z "${JAVA_HOME}" ; then
  AC_MSG_RESULT([not found])
else
  AC_MSG_RESULT([in ${JAVA_HOME}])

  case "${host_os}" in
    darwin*)
      JAVA_LIBS="-framework JavaVM"
      JAVA_LD_PATH=
      ;;
    *)
      JAVA_LIBS=`${JAVA_PROG} -classpath ${getsp_cp} getsp -libs`
      JAVA_LIBS="${JAVA_LIBS} -ljvm"
      JAVA_LD_PATH=`${JAVA_PROG} -classpath ${getsp_cp} getsp java.library.path`
      ;;
  esac
  
  ## note that we actually don't test JAVA_LIBS - we hope that the detection
  ## was correct.

  have_java=yes
fi
AC_SUBST(JAVA_HOME)
AC_SUBST(JAVA_PROG)
AC_SUBST(JAVA_LD_PATH)
AC_SUBST(JAVA_LIBS)
])
