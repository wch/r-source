### configure.ac                                        -*- Autoconf -*-
###
### Process this file with autoconf to produce a configure script.
###
### Copyright (C) 1998-2004 R Core Team
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
### 'http://www.gnu.org/copyleft/gpl.html', or by writing to the Free
### Software Foundation, 59 Temple Place -- Suite 330, Boston, MA
### 02111-3307, USA.

AC_PREREQ(2.59)

AC_INIT([Rgnome], [2.1.0], [r-bugs@R-project.org], [Rgnome])
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
AC_SUBST(R_HOME)

## We provide these defaults so that headers and libraries in
## '/usr/local' are found (by the native tools, mostly).
## We do it here because AC_ARG_WITH(gnome) may add to CPPFLAGS and
## LDFLAGS.
: ${CPPFLAGS="-I/usr/local/include"}
: ${LDFLAGS="-L/usr/local/lib"}

### ** Handle arguments to configure.

config_opts="${ac_configure_args}"
AC_SUBST(config_opts)

AC_ARG_WITH([gnome],
[AC_HELP_STRING([--with-gnome],
                [use GNOME, or specify its prefix @<:@yes@:>@])],
[if test "${withval}" = no; then
  want_gnome=no
elif test "${withval}" = yes; then
  want_gnome=yes
  want_R_shlib=yes
else
  want_gnome=yes
  want_R_shlib=yes
  CPPFLAGS="${CPPFLAGS} -I${withval}/include"
  LDFLAGS="${LDFLAGS} -L${withval}/lib"
  gnome_prefix="${withval}/lib"
fi],
[want_gnome=yes])

LIBR=`${R_HOME}/bin/R CMD config --ldflags`
if test "${LIBR}" = ""; then
  echo "Need a shared R library"
  exit 1
fi

echo LIBR is ${LIBR}
AC_SUBST(LIBR)

CC=`${R_HOME}/bin/R CMD config CC`
CFLAGS=`${R_HOME}/bin/R CMD config CFLAGS`
## we would like a POSIX sed, and need one on Solaris
AC_PATH_PROGS(SED, sed, /bin/sed, [/usr/xpg4/bin:$PATH])
## Make
: ${MAKE=make}
AC_SUBST(MAKE)

AC_PROG_INSTALL
AC_CHECK_HEADERS(fcntl.h locale.h string.h strings.h)

## R_GNOME
## -------
AC_DEFUN([R_GNOME],
[ GNOME_INIT_HOOK([], [cont])
  if test "${GNOMEUI_LIBS}"; then
    AM_PATH_LIBGLADE([use_gnome="yes"
                      GNOME_IF_FILES="gnome-interface.glade"],
                     [warn_libglade_version="GNOME support requires libglade ver
sion >= 0.3"
                      AC_MSG_WARN([${warn_libglade_version}])],
                     [gnome])
  fi
if test "${use_gnome}" != yes; then
  use_gnome="no"
  GNOME_IF_FILES=
fi
AC_SUBST(GNOME_IF_FILES)
])# R_GNOME

R_GNOME

AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES([Makefile])

AC_OUTPUT
