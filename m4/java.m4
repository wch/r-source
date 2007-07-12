### java.m4 -- macros for Java environment detection    -*- Autoconf -*-
###
### Copyright (C) 2005-7 R Core Team
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
## JAVA_CPPFLAGS to cpp flags necessary to find Java includes (*)
## JAVAC to Java compiler path (optional)
## JAVAH to Java header preprocessor path (optional)
## JAR to Java archiver (optional)
##
## (*) - those variables are modified for use in make files
##       to rely on $(JAVA_HOME) and substituted with 0 suffix
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
  JAVA_PATH=${JAVA_HOME}:${JAVA_HOME}/jre/bin:${JAVA_HOME}/bin:${JAVA_HOME}/../bin:${PATH}
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
AC_CACHE_CHECK([whether Java compiler works], [r_cv_javac_works],
[r_cv_javac_works=no
if test -n "${JAVAC}"; then
  rm -f A.java A.class
  echo "public class A { }" > A.java
  if "${JAVAC}" A.java 2>&AS_MESSAGE_LOG_FD; then
    if test -e A.class; then
      r_cv_javac_works=yes
    fi
  fi
  rm -rf A.java A.class
fi])

## this is where our test-class lives (in tools directory)
getsp_cp=${ac_aux_dir}

AC_CACHE_CHECK([whether Java interpreter works], [r_cv_java_works],
[r_cv_java_works=no
if test -n "${JAVA}" ; then
  R_RUN_JAVA(acx_jc_result,[-classpath ${getsp_cp} getsp -test])
  if test "${acx_jc_result}" = "Test1234OK"; then
    r_cv_java_works=yes
  fi
  acx_jc_result=
fi])

if test ${r_cv_java_works} = yes; then
  AC_CACHE_CHECK([Java environment], [r_cv_java_home], [
    ## find JAVA_HOME from Java itself unless specified
    if test -z "${JAVA_HOME}" ; then
      R_RUN_JAVA(JAVA_HOME,[-classpath ${getsp_cp} getsp java.home])
    fi
    r_cv_java_home="${JAVA_HOME}"
  ])
  JAVA_HOME="${r_cv_java_home}"

  # we have Java support, detect flags
  if test -n "${JAVA_HOME}"; then
    # find out whether all settings are already cached
    r_java_settings_cached=yes
    AC_MSG_CHECKING([for cached Java settings])
    AC_CACHE_VAL([r_cv_cache_java_flags], [
      r_cv_cache_java_flags=yes
      r_java_settings_cached=no])
    AC_MSG_RESULT([${r_java_settings_cached}])
    # if so, fetch them from the cache
    if test "${r_java_settings_cached}" = yes; then
      AC_CACHE_CHECK([JAVA_LIBS], [r_cv_JAVA_LIBS])
      JAVA_LIBS0="${r_cv_JAVA_LIBS}"
      AC_CACHE_CHECK([JAVA_CPPFLAGS],[r_cv_JAVA_CPPFLAGS])
      JAVA_CPPFLAGS0="${r_cv_JAVA_CPPFLAGS}"
      AC_CACHE_CHECK([JAVA_LD_LIBRARY_PATH],[r_cv_JAVA_LD_LIBRARY_PATH])
      JAVA_LD_LIBRARY_PATH="${r_cv_JAVA_LD_LIBRARY_PATH}"
    else
    # otherwise detect all Java-relevant flags

    : ${JAVA_LIBS=~autodetect~}
    : ${JAVA_CPPFLAGS=~autodetect~}
    : ${JAVA_LD_LIBRARY_PATH=~autodetect~}
    custom_JAVA_LIBS="${JAVA_LIBS}"
    custom_JAVA_CPPFLAGS="${JAVA_CPPFLAGS}"
    custom_JAVA_LD_LIBRARY_PATH="${JAVA_LD_LIBRARY_PATH}"

    case "${host_os}" in
      darwin*)
        JAVA_LIBS="-framework JavaVM"
        JAVA_LIBS0="-framework JavaVM"
	JAVA_CPPFLAGS="-I${JAVA_HOME}/include"
	JAVA_CPPFLAGS0='-I$(JAVA_HOME)/include'
        JAVA_LD_LIBRARY_PATH=
        ;;
      *)
        R_RUN_JAVA(JAVA_LIBS, [-classpath ${getsp_cp} getsp -libs])
        JAVA_LIBS="${JAVA_LIBS} -ljvm"
        R_RUN_JAVA(JAVA_LD_LIBRARY_PATH, [-classpath ${getsp_cp} getsp java.library.path])

	JAVA_LIBS0=`echo ${JAVA_LIBS} | sed -e s:${JAVA_HOME}:\$\(JAVA_HOME\):g`
	JAVA_LD_LIBRARY_PATH=`echo ${JAVA_LD_LIBRARY_PATH} | sed -e s:${JAVA_HOME}:\$\(JAVA_HOME\):g`

	## includes consist of two parts - jni.h and machine-dependent jni_md.h
	jinc=''
	for pinc in include ../include jre/include; do 
	  if test -e "${JAVA_HOME}/${pinc}/jni.h"; then jinc="${JAVA_HOME}/${pinc}"; break; fi
	done
	## only if we get jni.h we can try to find jni_md.h
	if test -n "${jinc}"; then
	   JAVA_CPPFLAGS="-I${jinc}"
	   mdinc=''
	   jmdirs=''
	   ## put the most probable locations for each system in the first place
	   case "${host_os}" in
	     linux*)   jmdirs=linux;;
	     bsdi*)    jmdirs=bsdos;;
	     osf*)     jmdirs=alpha;;
	     solaris*) jmdirs=solaris;;
	     freebsd*) jmdirs=freebsd;;
	   esac
	   ## prepend . and append less-likely ones
	   jmdirs=". ${jmdirs} genunix ppc x86 iris hp-ux aix win32 cygwin openbsd"
	   for pimd in ${jmdirs}; do
	     if test -e "${jinc}/${pimd}/jni_md.h"; then jmdinc="${jinc}/${pimd}"; break; fi
	   done
	   if test -z "${jmdinc}"; then
	     # ultima-ratio: use find and pray that it works
	     jmdinc=`find "${jinc}/" -name jni_md.h 2>dev/null |head -n 1 2>/dev/null`
	     if test -n "${jmdinc}"; then jmdinc=`dirname "${jmdinc}"`; fi
	   fi
	   if test -n "${jmdinc}"; then
	     if test "${jmdinc}" != "${jinc}/."; then
	       JAVA_CPPFLAGS="${JAVA_CPPFLAGS} -I${jmdinc}"
	     fi
	   fi
	fi
	JAVA_CPPFLAGS0=`echo ${JAVA_CPPFLAGS} | sed -e s:${JAVA_HOME}:\$\(JAVA_HOME\):g`
        ;;
    esac

    ## honor user overrides
    acx_java_uses_custom_flags=no
    if test "${custom_JAVA_LIBS}" != '~autodetect~'; then
      JAVA_LIBS="${custom_JAVA_LIBS}"
      JAVA_LIBS0=`echo ${JAVA_LIBS} | sed -e s:${JAVA_HOME}:\$\(JAVA_HOME\):g`
      acx_java_uses_custom_flags=yes
    fi
    if test "${custom_JAVA_CPPFLAGS}" != '~autodetect~'; then
      JAVA_CPPFLAGS="${custom_JAVA_CPPFLAGS}"
      JAVA_CPPFLAGS0=`echo ${JAVA_CPPFLAGS} | sed -e s:${JAVA_HOME}:\$\(JAVA_HOME\):g`
      acx_java_uses_custom_flags=yes
    fi
    if test "${custom_JAVA_LD_LIBRARY_PATH}" != '~autodetect~'; then
      JAVA_LD_LIBRARY_PATH="${custom_JAVA_LD_LIBRARY_PATH}"
    fi

    ## try to link a simple JNI program
    AC_CACHE_CHECK([whether JNI programs can be compiled], [r_cv_jni],
    [r_cv_jni=
    j_save_LIBS="${LIBS}"
    j_save_CPPF="${CPPFLAGS}"
    LIBS="${JAVA_LIBS}"
    CPPFLAGS="${JAVA_CPPFLAGS}"
    AC_LINK_IFELSE([
#include <jni.h>
int main(void) {
    JNI_CreateJavaVM(0, 0, 0);
    return 0;
}
],[r_cv_jni="yes"],[
    if test "${acx_java_uses_custom_flags}" = yes; then
      r_cv_jni=no
      AC_MSG_ERROR([Failed to compile a JNI program with custom JAVA_LIBS/JAVA_CPPFLAGS.
See config.log for details.
Do NOT set JAVA_LIBS/JAVA_CPPFLAGS unless you are sure they are correct!
Java/JNI support is optional unless you set either JAVA_LIBS or JAVA_CPPFLAGS.])
    fi
    ## some OSes/Javas need -lpthread
    LIBS="${LIBS} -lpthread"
    AC_LINK_IFELSE([
#include <jni.h>
int main(void) {
    JNI_CreateJavaVM(0, 0, 0);
    return 0;
}
],[r_cv_jni="yes (with pthreads)"],[r_cv_jni="no"])])
    LIBS="${j_save_LIBS}"
    CPPFLAGS="${j_save_CPPF}"
])
    ##AC_MSG_RESULT([$r_cv_jni])
    if test "${r_cv_jni}" = "yes (with pthreads)"; then
      JAVA_LIBS0="${JAVA_LIBS0} -lpthread"
    fi

    # cache all detected flags
      AC_CACHE_VAL([r_cv_JAVA_LIBS],[r_cv_JAVA_LIBS="${JAVA_LIBS0}"])
      AC_CACHE_VAL([r_cv_JAVA_CPPFLAGS],[r_cv_JAVA_CPPFLAGS="${JAVA_CPPFLAGS0}"])      
      AC_CACHE_VAL([r_cv_JAVA_LD_LIBRARY_PATH],[r_cv_JAVA_LD_LIBRARY_PATH="${JAVA_LD_LIBRARY_PATH}"])
    fi # cached flags

    have_java=yes
  fi
else  ## not r_cv_java_works
  JAVA=
  JAVA_HOME=
fi

## AC_SUBST(JAVA_HOME) # not needed? is precious now
AC_SUBST(JAVA)
AC_SUBST(JAVAC)
AC_SUBST(JAVAH)
AC_SUBST(JAR)
AC_SUBST(JAVA_LD_LIBRARY_PATH)
AC_SUBST(JAVA_LIBS0)
AC_SUBST(JAVA_CPPFLAGS0)
])
