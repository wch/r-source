### cairo.m4 -- extra macros for configuring R for Cairo    -*- Autoconf -*-
###
### Copyright (C) 2008 R Core Team
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
### along with R; if not, a copy is available at
### http://www.r-project.org/Licenses/

## provisional version

AC_DEFUN([R_CAIRO], [
save_CPPFLAGS=${CPPFLAGS}
save_LIBS=${LIBS}
have_cairo=no
if test "xx$CAIRO_LIBS" == "xx"; then
   if test "xx$PKGCONF" != "xx"; then
      AC_MSG_CHECKING([whether pkg-config knows about cairo])
      if "${PKGCONF}" --exists cairo; then
         AC_MSG_RESULT([yes])
	 AC_MSG_CHECKING([for configurable backends])
	 modlist="cairo"
	 for module in cairo-ft cairo-xlib cairo-xlib-xrender; do
	    if "${PKGCONF}" --exists ${module}; then
		modlist="${modlist} ${module}"
	    fi
	 done
	 AC_MSG_RESULT(${modlist})
	 CAIRO_CFLAGS=`"${PKGCONF}" --cflags ${modlist}`
	 CAIRO_LIBS=`"${PKGCONF}" --libs ${modlist}`
      else
	AC_MSG_RESULT([no])
      fi
   fi
fi
if test -z "${CAIRO_LIBS}"; then
   AC_MSG_NOTICE([CAIRO_LIBS is unset, attempting to guess it.])
   cpre=''
   for pre in /usr /usr/local /usr/X11 /usr/X11R6 /opt /sw; do
      if test -e "${pre}/include/cairo/cairo.h"; then
         cpre=${pre}; break
      fi
   done
   if test -n "${cpre}"; then
      CAIRO_CFLAGS="-I${cpre}/include/cairo"
      if test "${cpre}" = /usr; then
         CAIRO_LIBS="-lcairo"
      else dnl FIXME use LIBnn
         CAIRO_LIBS="-L${cpre}/lib -lcairo"
      fi
   fi
fi

AC_MSG_NOTICE([CAIRO_CFLAGS=${CAIRO_CFLAGS}])

CPPFLAGS="${CPPFLAGS} ${CAIRO_CFLAGS}"
AC_CHECK_HEADERS(cairo.h)

uses_ats=no
AC_MSG_CHECKING([for ATS font support in Cairo])
AC_LINK_IFELSE([
#include "cairo.h"
int main(void) {
#ifndef CAIRO_HAS_ATSUI_FONT
#error no PNG support
#endif
    return 0;
}],[AC_MSG_RESULT([yes])
uses_ats=yes],[AC_MSG_RESULT([no])])

if test "xx${CAIRO_LIBS}" == "xx"; then
   if test $uses_ats == "yes"; then
      CAIRO_LIBS="-lcairo -framework ApplicationServices"
   else
      CAIRO_LIBS="-lcairo"
   fi
fi

 LIBS="${LIBS} ${CAIRO_LIBS}"
AC_MSG_NOTICE([CAIRO_LIBS=${CAIRO_LIBS}])

has_cairo_ft=no
AC_MSG_CHECKING([for FreeType support in cairo])
AC_COMPILE_IFELSE([
#include <cairo.h>
#ifndef CAIRO_HAS_FT_FONT
#error nope, no ft
#endif
],[has_cairo_ft=yes])
AC_MSG_RESULT(${has_cairo_ft})

need_xtra_ft_flags=no
if test "${has_cairo_ft}" = yes; then
 AC_MSG_CHECKING([whether FreeType needs additional flags])
 AC_LINK_IFELSE([
#include <cairo.h>
#include <cairo-ft.h>
int main(void) {
   cairo_ft_font_face_create_for_pattern(0);
   return 0;
}
],[],[need_xtra_ft_flags=yes])
 AC_MSG_RESULT(${need_xtra_ft_flags})
fi

if test "${need_xtra_ft_flags}" = yes; then
 if test -n "${PKGCONF}"; then
   AC_MSG_CHECKING([whether pkg-config knows about fontconfig or freetype2])
   ftmod=''
   for module in fontconfig freetype2; do
      if "${PKGCONF}" --exists ${module}; then ftmod="${ftmod} ${module}"; fi
   done
   if test -n "${ftmod}"; then
      AC_MSG_RESULT(yes)
      FT_CPPFLAGS=`"${PKGCONF}" --cflags ${ftmod}`
      FT_LIBS=`"${PKGCONF}" --libs ${ftmod}`
   else
      AC_MSG_RESULT(no)
   fi
 fi
 if test -z "${FT_LIBS}"; then
   AC_MSG_CHECKING([whether fontconfig/freetype2 location can be guessed])
   FCI=''; FTI=''; FCL=''; FTL=''
   for pre in /usr /usr/X11 /usr/X11R6 /usr/local /opt /sw; do
     if test -z "${FCI}" -a -e "${pre}/include/fontconfig/fontconfig.h"; then
        FCI="-I${pre}/include"
	FCL="-L${pre}/lib"
     fi
     if test -z "{FTI}" -a -e "${pre}/include/freetype2/freetype/freetype.h"; then
        FTI="-I${pre}/include/freetype2"
	FTL="-L${pre}/lib"
     fi
   done
   if test "${FCI}" = -I/usr/include; then FCI=''; fi
   if test "${FCL}" = "-L/usr/lib"; then FCL=''; fi
   if test "${FTL}" = "-L/usr/lib"; then FTL=''; fi
   if test "${FTL}" = "${FCL}"; then FTL=''; fi
   if test -z "${FCI}" -a -z "${FTI}"; then
      AC_MSG_RESULT(no)
      AC_MSG_ERROR([Cannot find fontconfig/freetype2 although cairo claims to support it.])
   fi
   FT_CPPFLAGS="${FCI} ${FTI}"
   FT_LIBS="${FCL} ${FTL} -lfontconfig -lfreetype -lz"
   AC_MSG_RESULT([possibly])
 fi
 LIBS="${LIBS} ${FT_LIBS}"
 CPPFLAGS="${CPPFLAGS} ${FT_CPPFLAGS}"
 AC_MSG_CHECKING([whether additional flags work])
 AC_LINK_IFELSE([
#include <cairo.h>
#include <cairo-ft.h>
int main(void) {
   cairo_ft_font_face_create_for_pattern(0);
   return 0;
}
 ],[AC_MSG_RESULT(yes)
 CAIRO_LIBS="${CAIRO_LIBS} ${FT_LIBS}"
 CAIRO_CFLAGS="${CAIRO_CFLAGS} ${FT_CPPFLAGS}"
 ],[
 AC_MSG_RESULT(no)
 AC_MSG_ERROR([Cannot use cairo-ft backend, although cairo claims it is working.])
 ])
fi
CPPFLAGS=${save_CPPFLAGS}
LIBS=${save_LIBS}
])