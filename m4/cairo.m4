### cairo.m4 -- extra macros for configuring R for cairo    -*- Autoconf -*-
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


AC_DEFUN([R_PANGO_CAIRO], [
if test "x${PKGCONF}" = "x"; then
  AC_MSG_NOTICE([not checking for cairo as pkg-config is not present])
else
  save_CPPFLAGS=${CPPFLAGS}
  save_LIBS=${LIBS}
  AC_CACHE_CHECK([whether pkg-config knows about cairo and pango], 
      [r_cv_has_cairo],
      [if "${PKGCONF}" --exists pangocairo; then
         r_cv_has_cairo="yes"
       else
         r_cv_has_cairo="no"
       fi
  ])
  if test "x${r_cv_has_cairo}" = "xyes"; then
     modlist="pangocairo"
     for module in cairo-xlib cairo-xlib-xrender; do
       if "${PKGCONF}" --exists ${module}; then
	 modlist="${modlist} ${module}"
       fi
     done
     CAIRO_CPPFLAGS=`"${PKGCONF}" --cflags ${modlist}`
     CAIRO_LIBS=`"${PKGCONF}" --libs ${modlist}`
   fi

  CPPFLAGS="${CPPFLAGS} ${CAIRO_CPPFLAGS}"
  LIBS="${LIBS} ${CAIRO_LIBS}"

  AC_CACHE_CHECK([whether cairo including pango works], 
		  [r_cv_cairo_works], [AC_LINK_IFELSE([
#include <pango/pango.h>
#include <pango/pangocairo.h>
#include <cairo-xlib.h>
int main(void) {
    cairo_t  *CC;    
    cairo_arc(CC, 0.0, 0.0, 1.0, 0.0, 6.28);
    pango_cairo_create_layout(CC);
    pango_font_description_new();
    return 0;
 }
	],[r_cv_cairo_works=yes],[r_cv_cairo_works=no
          CAIRO_LIBS=
          CAIRO_CFLAGS=
        ])])
  CPPFLAGS=${save_CPPFLAGS}
  LIBS=${save_LIBS}
fi
if test "x${r_cv_cairo_works}" = xyes; then
   AC_DEFINE(HAVE_WORKING_CAIRO, 1,
            [Define to 1 if you have cairo including pango.]) 
fi
dnl AM_CONDITIONAL(BUILD_WITH_CAIRO, [test "x${r_cv_cairo_works}" = xyes])
AC_SUBST(CAIRO_CPPFLAGS)
AC_SUBST(CAIRO_LIBS)
])
