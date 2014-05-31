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
      [r_cv_has_pangocairo],
      [if "${PKGCONF}" --exists pangocairo; then
         r_cv_has_pangocairo="yes"
       else
         r_cv_has_pangocairo="no"
       fi
  ])
  if test "x${r_cv_has_pangocairo}" = "xyes"; then
    modlist="pangocairo"
    for module in cairo-png; do
      if "${PKGCONF}" --exists ${module}; then
	modlist="${modlist} ${module}"
      fi
    done
    if "${PKGCONF}" --exists cairo-pdf; then
       modlist="${modlist} cairo-pdf"
       r_cairo_pdf=yes
    fi
    if "${PKGCONF}" --exists cairo-ps; then
       modlist="${modlist} cairo-ps"
       r_cairo_ps=yes
    fi
    if "${PKGCONF}" --exists cairo-svg; then
       modlist="${modlist} cairo-svg"
       r_cairo_svg=yes
    fi
      if "${PKGCONF}" --exists cairo-xlib; then
         xmodlist="${modlist} cairo-xlib"
      else
         xmodlist="${modlist}"
      fi
    CAIRO_CPPFLAGS=`"${PKGCONF}" --cflags ${modlist}`
    CAIROX11_CPPFLAGS=`"${PKGCONF}" --cflags ${xmodlist}`
    CAIRO_LIBS=`"${PKGCONF}" --libs ${modlist}`
    CAIROX11_LIBS=`"${PKGCONF}" --libs ${xmodlist}`

    CPPFLAGS="${CPPFLAGS} ${CAIRO_CPPFLAGS}"
    LIBS="${LIBS} ${CAIRO_LIBS}"

     AC_CACHE_CHECK([whether cairo including pango is >= 1.2 and works], 
		    [r_cv_cairo_works],
                    [AC_LINK_IFELSE([AC_LANG_SOURCE([[
#include <pango/pango.h>
#include <pango/pangocairo.h>
#include <cairo-xlib.h>
#if CAIRO_VERSION  < 10200
#error cairo version >= 1.2 required
#endif
int main(void) {
    cairo_t  *CC;
    cairo_arc(CC, 0.0, 0.0, 1.0, 0.0, 6.28);
    pango_cairo_create_layout(CC);
    pango_font_description_new();
    return 0;
 }
	]])],[r_cv_cairo_works=yes],[r_cv_cairo_works=no
          CAIRO_LIBS=
          CAIRO_CFLAGS=
        ])])
    CPPFLAGS=${save_CPPFLAGS}
    LIBS=${save_LIBS}
  else  ## no pangocairo, check for just cairo
    AC_CACHE_CHECK([whether pkg-config knows about cairo], [r_cv_has_cairo],
	[if "${PKGCONF}" --exists cairo; then
	   r_cv_has_cairo="yes"
	 else
	   r_cv_has_cairo="no"
	 fi
    ])
    if test "x${r_cv_has_cairo}" = "xyes"; then
      modlist="cairo"
      ## on Linux, cairo-ft brings in header paths <cairo-ft.h>:
      ## the code which needs this is currently conditionalized
      for module in cairo-png cairo-ft; do
	if "${PKGCONF}" --exists ${module}; then
	  modlist="${modlist} ${module}"
	fi
      done
      if "${PKGCONF}" --exists cairo-pdf; then
         modlist="${modlist} cairo-pdf"
         r_cairo_pdf=yes
      fi
      if "${PKGCONF}" --exists cairo-ps; then
         modlist="${modlist} cairo-ps"
         r_cairo_ps=yes
      fi
      if "${PKGCONF}" --exists cairo-svg; then
         modlist="${modlist} cairo-svg"
         r_cairo_svg=yes
      fi
      if "${PKGCONF}" --exists cairo-xlib; then
         xmodlist="${modlist} cairo-xlib"
      else
         xmodlist="${modlist}"
      fi
      CAIRO_CPPFLAGS=`"${PKGCONF}" --cflags ${modlist}`
      CAIROX11_CPPFLAGS=`"${PKGCONF}" --cflags ${xmodlist}`
      case "${host_os}" in
        darwin*)
          ## This is for static OS X build
          CAIRO_LIBS=`"${PKGCONF}" --static --libs ${modlist}`
          CAIROX11_LIBS=`"${PKGCONF}" --static --libs ${xmodlist}`
          ;;
        *)
          CAIRO_LIBS=`"${PKGCONF}" --libs ${modlist}`
          CAIROX11_LIBS=`"${PKGCONF}" --libs ${xmodlist}`
          ;;
      esac

      CPPFLAGS="${CPPFLAGS} ${CAIRO_CPPFLAGS}"
      LIBS="${LIBS} ${CAIRO_LIBS}"

      AC_CACHE_CHECK([whether cairo is >= 1.2 and works], 
		     [r_cv_cairo_works], 
                     [AC_LINK_IFELSE([AC_LANG_SOURCE([[
#include <cairo.h>
#include <cairo-xlib.h>
#if CAIRO_VERSION  < 10200
#error cairo version >= 1.2 required
#endif
int main(void) {
    cairo_t  *CC;    
    cairo_arc(CC, 0.0, 0.0, 1.0, 0.0, 6.28);
    cairo_select_font_face (CC, "Helvetica", CAIRO_FONT_SLANT_NORMAL, 
                            CAIRO_FONT_WEIGHT_BOLD);
    return 0;
 }
	]])],[r_cv_cairo_works=yes],[r_cv_cairo_works=no
          CAIRO_LIBS=
          CAIRO_CFLAGS=
        ])])
      CPPFLAGS=${save_CPPFLAGS}
      LIBS=${save_LIBS}
    fi
  fi
fi

if test "x${r_cv_has_pangocairo}" = xyes; then
   AC_DEFINE(HAVE_PANGOCAIRO, 1, [Define to 1 if you have pangocairo.]) 
fi
if test "x${r_cv_cairo_works}" = xyes; then
   AC_DEFINE(HAVE_WORKING_CAIRO, 1, [Define to 1 if you have cairo.])
fi
if test "x${r_cairo_pdf}" = xyes; then
   AC_DEFINE(HAVE_CAIRO_PDF, 1, [Define to 1 if you have cairo-ps.]) 
fi
if test "x${r_cairo_ps}" = xyes; then
   AC_DEFINE(HAVE_CAIRO_PS, 1, [Define to 1 if you have cairo-pdf.]) 
fi
if test "x${r_cairo_svg}" = xyes; then
   AC_DEFINE(HAVE_CAIRO_SVG, 1, [Define to 1 if you have cairo-svg.]) 
fi
AC_SUBST(CAIRO_CPPFLAGS)
AC_SUBST(CAIROX11_CPPFLAGS)
AC_SUBST(CAIRO_LIBS)
AC_SUBST(CAIROX11_LIBS)
])
