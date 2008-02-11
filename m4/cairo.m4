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
AC_MSG_CHECKING([for Cairo])
if test "x${PKGCONF}" = "x"; then
  AC_MSG_RESULT(no)
else
  AC_MSG_RESULT(yes)
  save_CPPFLAGS=${CPPFLAGS}
  save_LIBS=${LIBS}
  AC_CACHE_CHECK([whether pkg-config knows about Cairo], [r_cv_has_cairo],
      [if "${PKGCONF}" --exists cairo; then
         r_cv_has_cairo="yes"
       else
         r_cv_has_cairo="no"
       fi
  ])
  if test "x${r_cv_has_cairo}" = "xyes"; then
  AC_CACHE_CHECK([for Cairo backends], [r_cv_cairo_backends],
     [modlist="cairo"
     for module in cairo-ft cairo-xlib cairo-xlib-xrender; do
       if "${PKGCONF}" --exists ${module}; then
	 modlist="${modlist} ${module}"
       fi
     done
     CAIRO_CFLAGS=`"${PKGCONF}" --cflags ${modlist}`
     CAIRO_LIBS=`"${PKGCONF}" --libs ${modlist}`
     r_cv_cairo_backends=${modlist}
     ])
   fi

  AC_MSG_NOTICE([CAIRO_CFLAGS=${CAIRO_CFLAGS}])
  AC_MSG_NOTICE([CAIRO_LIBS=${CAIRO_LIBS}])

  CPPFLAGS="${CPPFLAGS} ${CAIRO_CFLAGS}"
  AC_CHECK_HEADERS(cairo.h cairo-ft.h)

  LIBS="${LIBS} ${CAIRO_LIBS}"

  AC_CACHE_CHECK([whether freetype2 support in Cairo works], 
                 [r_cv_cairo_works], [AC_LINK_IFELSE([
 #include <cairo.h>
 #include <cairo-ft.h>
 int main(void) {
     cairo_ft_font_face_create_for_pattern(0);
     return 0;
 }
 ],[r_cv_cairo_works=yes],[r_cv_cairo_works=no
  CAIRO_LIBS=
  CAIRO_CFLAGS=
 ])])

  CPPFLAGS=${save_CPPFLAGS}
  LIBS=${save_LIBS}
fi
])