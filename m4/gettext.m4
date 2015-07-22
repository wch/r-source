## created from gettext 0.16.1 by
## cat gettext.m4 glibc2.m4 glibc21.m4 iconv.m4 intdiv0.m4 intl.m4 
## intmax.m4 inttypes-pri.m4 inttypes_h.m4 lcmessage.m4 lock.m4 
## longdouble.m4 longlong.m4 nls.m4 printf-posix.m4 progtest.m4 
## size_max.m4 stdint_h.m4 uintmax_t.m4 ulonglong.m4 visibility.m4 
## wchar_t.m4 wint_t.m4 xsize.m4 > .../m4/gettext.m4

## Then disable testing for libiconv prefix

# gettext.m4 serial 59 (gettext-0.16.1)
dnl Copyright (C) 1995-2006 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl
dnl This file can can be used in projects which are not available under
dnl the GNU General Public License or the GNU Library General Public
dnl License but which still want to provide support for the GNU gettext
dnl functionality.
dnl Please note that the actual code of the GNU gettext library is covered
dnl by the GNU Library General Public License, and the rest of the GNU
dnl gettext package package is covered by the GNU General Public License.
dnl They are *not* in the public domain.

dnl Authors:
dnl   Ulrich Drepper <drepper@cygnus.com>, 1995-2000.
dnl   Bruno Haible <haible@clisp.cons.org>, 2000-2006.

dnl Macro to add for using GNU gettext.

dnl Usage: AM_GNU_GETTEXT([INTLSYMBOL], [NEEDSYMBOL], [INTLDIR]).
dnl INTLSYMBOL can be one of 'external', 'no-libtool', 'use-libtool'. The
dnl    default (if it is not specified or empty) is 'no-libtool'.
dnl    INTLSYMBOL should be 'external' for packages with no intl directory,
dnl    and 'no-libtool' or 'use-libtool' for packages with an intl directory.
dnl    If INTLSYMBOL is 'use-libtool', then a libtool library
dnl    $(top_builddir)/intl/libintl.la will be created (shared and/or static,
dnl    depending on --{enable,disable}-{shared,static} and on the presence of
dnl    AM-DISABLE-SHARED). If INTLSYMBOL is 'no-libtool', a static library
dnl    $(top_builddir)/intl/libintl.a will be created.
dnl If NEEDSYMBOL is specified and is 'need-ngettext', then GNU gettext
dnl    implementations (in libc or libintl) without the ngettext() function
dnl    will be ignored.  If NEEDSYMBOL is specified and is
dnl    'need-formatstring-macros', then GNU gettext implementations that don't
dnl    support the ISO C 99 <inttypes.h> formatstring macros will be ignored.
dnl INTLDIR is used to find the intl libraries.  If empty,
dnl    the value `$(top_builddir)/intl/' is used.
dnl
dnl The result of the configuration is one of three cases:
dnl 1) GNU gettext, as included in the intl subdirectory, will be compiled
dnl    and used.
dnl    Catalog format: GNU --> install in $(datadir)
dnl    Catalog extension: .mo after installation, .gmo in source tree
dnl 2) GNU gettext has been found in the system's C library.
dnl    Catalog format: GNU --> install in $(datadir)
dnl    Catalog extension: .mo after installation, .gmo in source tree
dnl 3) No internationalization, always use English msgid.
dnl    Catalog format: none
dnl    Catalog extension: none
dnl If INTLSYMBOL is 'external', only cases 2 and 3 can occur.
dnl The use of .gmo is historical (it was needed to avoid overwriting the
dnl GNU format catalogs when building on a platform with an X/Open gettext),
dnl but we keep it in order not to force irrelevant filename changes on the
dnl maintainers.
dnl
AC_DEFUN([AM_GNU_GETTEXT],
[
  dnl Argument checking.
  ifelse([$1], [], , [ifelse([$1], [external], , [ifelse([$1], [no-libtool], , [ifelse([$1], [use-libtool], ,
    [errprint([ERROR: invalid first argument to AM_GNU_GETTEXT
])])])])])
  ifelse([$2], [], , [ifelse([$2], [need-ngettext], , [ifelse([$2], [need-formatstring-macros], ,
    [errprint([ERROR: invalid second argument to AM_GNU_GETTEXT
])])])])
  define([gt_included_intl],
    ifelse([$1], [external],
      ifdef([AM_GNU_GETTEXT_][INTL_SUBDIR], [yes], [no]),
      [yes]))
  define([gt_libtool_suffix_prefix], ifelse([$1], [use-libtool], [l], []))
  gt_NEEDS_INIT
  AM_GNU_GETTEXT_NEED([$2])

dnl  AC_REQUIRE([AM_PO_SUBDIRS])dnl
  ifelse(gt_included_intl, yes, [
    AC_REQUIRE([AM_INTL_SUBDIR])dnl
  ])

  dnl Prerequisites of AC_LIB_LINKFLAGS_BODY.
  AC_REQUIRE([AC_LIB_PREPARE_PREFIX])
  AC_REQUIRE([AC_LIB_RPATH])

  dnl Sometimes libintl requires libiconv, so first search for libiconv.
  dnl Ideally we would do this search only after the
  dnl      if test "$USE_NLS" = "yes"; then
  dnl        if { eval "gt_val=\$$gt_func_gnugettext_libc"; test "$gt_val" != "yes"; }; then
  dnl tests. But if configure.in invokes AM_ICONV after AM_GNU_GETTEXT
  dnl the configure script would need to contain the same shell code
  dnl again, outside any 'if'. There are two solutions:
  dnl - Invoke AM_ICONV_LINKFLAGS_BODY here, outside any 'if'.
  dnl - Control the expansions in more detail using AC_PROVIDE_IFELSE.
  dnl Since AC_PROVIDE_IFELSE is only in autoconf >= 2.52 and not
  dnl documented, we avoid it.
  ifelse(gt_included_intl, yes, , [
    AC_REQUIRE([AM_ICONV_LINKFLAGS_BODY])
  ])

  dnl Sometimes, on OS X, libintl requires linking with CoreFoundation.
  gt_INTL_MACOSX

  dnl Set USE_NLS.
  AC_REQUIRE([AM_NLS])

  ifelse(gt_included_intl, yes, [
    BUILD_INCLUDED_LIBINTL=no
    USE_INCLUDED_LIBINTL=no
  ])
  LIBINTL=
  LTLIBINTL=
  POSUB=

  dnl Add a version number to the cache macros.
  case " $gt_needs " in
    *" need-formatstring-macros "*) gt_api_version=3 ;;
    *" need-ngettext "*) gt_api_version=2 ;;
    *) gt_api_version=1 ;;
  esac
  gt_func_gnugettext_libc="gt_cv_func_gnugettext${gt_api_version}_libc"
  gt_func_gnugettext_libintl="gt_cv_func_gnugettext${gt_api_version}_libintl"

  dnl If we use NLS figure out what method
  if test "$USE_NLS" = "yes"; then
    gt_use_preinstalled_gnugettext=no
    ifelse(gt_included_intl, yes, [
      AC_MSG_CHECKING([whether included gettext is requested])
      AC_ARG_WITH(included-gettext,
        [  --with-included-gettext use the GNU gettext library included here],
        nls_cv_force_use_gnu_gettext=$withval,
        nls_cv_force_use_gnu_gettext=no)
      AC_MSG_RESULT($nls_cv_force_use_gnu_gettext)

      nls_cv_use_gnu_gettext="$nls_cv_force_use_gnu_gettext"
      if test "$nls_cv_force_use_gnu_gettext" != "yes"; then
    ])
        dnl User does not insist on using GNU NLS library.  Figure out what
        dnl to use.  If GNU gettext is available we use this.  Else we have
        dnl to fall back to GNU NLS library.

        if test $gt_api_version -ge 3; then
          gt_revision_test_code='
#ifndef __GNU_GETTEXT_SUPPORTED_REVISION
#define __GNU_GETTEXT_SUPPORTED_REVISION(major) ((major) == 0 ? 0 : -1)
#endif
changequote(,)dnl
typedef int array [2 * (__GNU_GETTEXT_SUPPORTED_REVISION(0) >= 1) - 1];
changequote([,])dnl
'
        else
          gt_revision_test_code=
        fi
        if test $gt_api_version -ge 2; then
          gt_expression_test_code=' + * ngettext ("", "", 0)'
        else
          gt_expression_test_code=
        fi

        AC_CACHE_CHECK([for GNU gettext in libc], [$gt_func_gnugettext_libc],
         [AC_TRY_LINK([#include <libintl.h>
$gt_revision_test_code
extern int _nl_msg_cat_cntr;
extern int *_nl_domain_bindings;],
            [bindtextdomain ("", "");
return * gettext ("")$gt_expression_test_code + _nl_msg_cat_cntr + *_nl_domain_bindings],
            [eval "$gt_func_gnugettext_libc=yes"],
            [eval "$gt_func_gnugettext_libc=no"])])

        if { eval "gt_val=\$$gt_func_gnugettext_libc"; test "$gt_val" != "yes"; }; then
          dnl Sometimes libintl requires libiconv, so first search for libiconv.
          ifelse(gt_included_intl, yes, , [
            AM_ICONV_LINK
          ])
          dnl Search for libintl and define LIBINTL, LTLIBINTL and INCINTL
          dnl accordingly. Don't use AC_LIB_LINKFLAGS_BODY([intl],[iconv])
          dnl because that would add "-liconv" to LIBINTL and LTLIBINTL
          dnl even if libiconv doesn't exist.
          AC_LIB_LINKFLAGS_BODY([intl])
          AC_CACHE_CHECK([for GNU gettext in libintl],
            [$gt_func_gnugettext_libintl],
           [gt_save_CPPFLAGS="$CPPFLAGS"
            CPPFLAGS="$CPPFLAGS $INCINTL"
            gt_save_LIBS="$LIBS"
            LIBS="$LIBS $LIBINTL"
            dnl Now see whether libintl exists and does not depend on libiconv.
            AC_TRY_LINK([#include <libintl.h>
$gt_revision_test_code
extern int _nl_msg_cat_cntr;
extern
#ifdef __cplusplus
"C"
#endif
const char *_nl_expand_alias (const char *);],
              [bindtextdomain ("", "");
return * gettext ("")$gt_expression_test_code + _nl_msg_cat_cntr + *_nl_expand_alias ("")],
              [eval "$gt_func_gnugettext_libintl=yes"],
              [eval "$gt_func_gnugettext_libintl=no"])
            dnl Now see whether libintl exists and depends on libiconv.
            if { eval "gt_val=\$$gt_func_gnugettext_libintl"; test "$gt_val" != yes; } && test -n "$LIBICONV"; then
              LIBS="$LIBS $LIBICONV"
              AC_TRY_LINK([#include <libintl.h>
$gt_revision_test_code
extern int _nl_msg_cat_cntr;
extern
#ifdef __cplusplus
"C"
#endif
const char *_nl_expand_alias (const char *);],
                [bindtextdomain ("", "");
return * gettext ("")$gt_expression_test_code + _nl_msg_cat_cntr + *_nl_expand_alias ("")],
               [LIBINTL="$LIBINTL $LIBICONV"
                LTLIBINTL="$LTLIBINTL $LTLIBICONV"
                eval "$gt_func_gnugettext_libintl=yes"
               ])
            fi
            CPPFLAGS="$gt_save_CPPFLAGS"
            LIBS="$gt_save_LIBS"])
        fi

        dnl If an already present or preinstalled GNU gettext() is found,
        dnl use it.  But if this macro is used in GNU gettext, and GNU
        dnl gettext is already preinstalled in libintl, we update this
        dnl libintl.  (Cf. the install rule in intl/Makefile.in.)
        if { eval "gt_val=\$$gt_func_gnugettext_libc"; test "$gt_val" = "yes"; } \
           || { { eval "gt_val=\$$gt_func_gnugettext_libintl"; test "$gt_val" = "yes"; } \
                && test "$PACKAGE" != gettext-runtime \
                && test "$PACKAGE" != gettext-tools; }; then
          gt_use_preinstalled_gnugettext=yes
        else
          dnl Reset the values set by searching for libintl.
          LIBINTL=
          LTLIBINTL=
          INCINTL=
        fi

    ifelse(gt_included_intl, yes, [
        if test "$gt_use_preinstalled_gnugettext" != "yes"; then
          dnl GNU gettext is not found in the C library.
          dnl Fall back on included GNU gettext library.
          nls_cv_use_gnu_gettext=yes
        fi
      fi

      if test "$nls_cv_use_gnu_gettext" = "yes"; then
        dnl Mark actions used to generate GNU NLS library.
        BUILD_INCLUDED_LIBINTL=yes
        USE_INCLUDED_LIBINTL=yes
	dnl R change: don't include static libs
	LIBINTL="$LIBICONV $LIBTHREAD"
        dnl LIBINTL="ifelse([$3],[],\${top_builddir}/intl,[$3])/libintl.[]gt_libtool_suffix_prefix[]a $LIBICONV $LIBTHREAD"
        LTLIBINTL="ifelse([$3],[],\${top_builddir}/intl,[$3])/libintl.[]gt_libtool_suffix_prefix[]a $LTLIBICONV $LTLIBTHREAD"
        LIBS=`echo " $LIBS " | sed -e 's/ -lintl / /' -e 's/^ //' -e 's/ $//'`
      fi

      CATOBJEXT=
      if test "$gt_use_preinstalled_gnugettext" = "yes" \
         || test "$nls_cv_use_gnu_gettext" = "yes"; then
        dnl Mark actions to use GNU gettext tools.
        CATOBJEXT=.gmo
      fi
    ])

    if test -n "$INTL_MACOSX_LIBS"; then
      if test "$gt_use_preinstalled_gnugettext" = "yes" \
         || test "$nls_cv_use_gnu_gettext" = "yes"; then
        dnl Some extra flags are needed during linking.
        LIBINTL="$LIBINTL $INTL_MACOSX_LIBS"
        LTLIBINTL="$LTLIBINTL $INTL_MACOSX_LIBS"
      fi
    fi

    if test "$gt_use_preinstalled_gnugettext" = "yes" \
       || test "$nls_cv_use_gnu_gettext" = "yes"; then
      AC_DEFINE(ENABLE_NLS, 1,
        [Define to 1 if translation of program messages to the user's native language
   is requested.])
    else
      USE_NLS=no
    fi
  fi

  AC_MSG_CHECKING([whether to use NLS])
  AC_MSG_RESULT([$USE_NLS])
  if test "$USE_NLS" = "yes"; then
    AC_MSG_CHECKING([where the gettext function comes from])
    if test "$gt_use_preinstalled_gnugettext" = "yes"; then
      if { eval "gt_val=\$$gt_func_gnugettext_libintl"; test "$gt_val" = "yes"; }; then
        gt_source="external libintl"
      else
        gt_source="libc"
      fi
    else
      gt_source="included intl directory"
    fi
    AC_MSG_RESULT([$gt_source])
  fi

  if test "$USE_NLS" = "yes"; then

    if test "$gt_use_preinstalled_gnugettext" = "yes"; then
      if { eval "gt_val=\$$gt_func_gnugettext_libintl"; test "$gt_val" = "yes"; }; then
        AC_MSG_CHECKING([how to link with libintl])
        AC_MSG_RESULT([$LIBINTL])
        AC_LIB_APPENDTOVAR([CPPFLAGS], [$INCINTL])
      fi

      dnl For backward compatibility. Some packages may be using this.
      AC_DEFINE(HAVE_GETTEXT, 1,
       [Define if the GNU gettext() function is already present or preinstalled.])
      AC_DEFINE(HAVE_DCGETTEXT, 1,
       [Define if the GNU dcgettext() function is already present or preinstalled.])
    fi

    dnl We need to process the po/ directory.
    POSUB=po
  fi

  ifelse(gt_included_intl, yes, [
    dnl If this is used in GNU gettext we have to set BUILD_INCLUDED_LIBINTL
    dnl to 'yes' because some of the testsuite requires it.
    if test "$PACKAGE" = gettext-runtime || test "$PACKAGE" = gettext-tools; then
      BUILD_INCLUDED_LIBINTL=yes
    fi

    dnl Make all variables we use known to autoconf.
    AC_SUBST(BUILD_INCLUDED_LIBINTL)
    AC_SUBST(USE_INCLUDED_LIBINTL)
    AC_SUBST(CATOBJEXT)

    dnl For backward compatibility. Some configure.ins may be using this.
    nls_cv_header_intl=
    nls_cv_header_libgt=

    dnl For backward compatibility. Some Makefiles may be using this.
    DATADIRNAME=share
    AC_SUBST(DATADIRNAME)

    dnl For backward compatibility. Some Makefiles may be using this.
    INSTOBJEXT=.mo
    AC_SUBST(INSTOBJEXT)

    dnl For backward compatibility. Some Makefiles may be using this.
    GENCAT=gencat
    AC_SUBST(GENCAT)

    dnl For backward compatibility. Some Makefiles may be using this.
    INTLOBJS=
    if test "$USE_INCLUDED_LIBINTL" = yes; then
      INTLOBJS="\$(GETTOBJS)"
    fi
    AC_SUBST(INTLOBJS)

    dnl Enable libtool support if the surrounding package wishes it.
    INTL_LIBTOOL_SUFFIX_PREFIX=gt_libtool_suffix_prefix
    AC_SUBST(INTL_LIBTOOL_SUFFIX_PREFIX)
  ])

  dnl For backward compatibility. Some Makefiles may be using this.
  INTLLIBS="$LIBINTL"
  AC_SUBST(INTLLIBS)

  dnl Make all documented variables known to autoconf.
  AC_SUBST(LIBINTL)
  AC_SUBST(LTLIBINTL)
  AC_SUBST(POSUB)
])


dnl Checks for special options needed on OS X.
dnl Defines INTL_MACOSX_LIBS.
AC_DEFUN([gt_INTL_MACOSX],
[
  dnl Check for API introduced in OS X 10.2.
  AC_CACHE_CHECK([for CFPreferencesCopyAppValue],
    gt_cv_func_CFPreferencesCopyAppValue,
    [gt_save_LIBS="$LIBS"
     LIBS="$LIBS -Wl,-framework -Wl,CoreFoundation"
     AC_TRY_LINK([#include <CoreFoundation/CFPreferences.h>],
       [CFPreferencesCopyAppValue(NULL, NULL)],
       [gt_cv_func_CFPreferencesCopyAppValue=yes],
       [gt_cv_func_CFPreferencesCopyAppValue=no])
     LIBS="$gt_save_LIBS"])
  if test $gt_cv_func_CFPreferencesCopyAppValue = yes; then
    AC_DEFINE([HAVE_CFPREFERENCESCOPYAPPVALUE], 1,
      [Define to 1 if you have the OS X function CFPreferencesCopyAppValue in the CoreFoundation framework. (For intl)])
  fi
  dnl Check for API introduced in OS X 10.3.
  AC_CACHE_CHECK([for CFLocaleCopyCurrent], gt_cv_func_CFLocaleCopyCurrent,
    [gt_save_LIBS="$LIBS"
     LIBS="$LIBS -Wl,-framework -Wl,CoreFoundation"
     AC_TRY_LINK([#include <CoreFoundation/CFLocale.h>], [CFLocaleCopyCurrent();],
       [gt_cv_func_CFLocaleCopyCurrent=yes],
       [gt_cv_func_CFLocaleCopyCurrent=no])
     LIBS="$gt_save_LIBS"])
  if test $gt_cv_func_CFLocaleCopyCurrent = yes; then
    AC_DEFINE([HAVE_CFLOCALECOPYCURRENT], 1,
      [Define to 1 if you have the OS X function CFLocaleCopyCurrent in the CoreFoundation framework. (For intl)])
  fi
  INTL_MACOSX_LIBS=
  if test $gt_cv_func_CFPreferencesCopyAppValue = yes || test $gt_cv_func_CFLocaleCopyCurrent = yes; then
    INTL_MACOSX_LIBS="-Wl,-framework -Wl,CoreFoundation"
  fi
  AC_SUBST([INTL_MACOSX_LIBS])
])


dnl gt_NEEDS_INIT ensures that the gt_needs variable is initialized.
m4_define([gt_NEEDS_INIT],
[
  m4_divert_text([DEFAULTS], [gt_needs=])
  m4_define([gt_NEEDS_INIT], [])
])


dnl Usage: AM_GNU_GETTEXT_NEED([NEEDSYMBOL])
AC_DEFUN([AM_GNU_GETTEXT_NEED],
[
  m4_divert_text([INIT_PREPARE], [gt_needs="$gt_needs $1"])
])


dnl Usage: AM_GNU_GETTEXT_VERSION([gettext-version])
AC_DEFUN([AM_GNU_GETTEXT_VERSION], [])
# glibc2.m4 serial 1
dnl Copyright (C) 2000-2002, 2004 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

# Test for the GNU C Library, version 2.0 or newer.
# From Bruno Haible.

AC_DEFUN([gt_GLIBC2],
  [
    AC_CACHE_CHECK(whether we are using the GNU C Library 2 or newer,
      ac_cv_gnu_library_2,
      [AC_EGREP_CPP([Lucky GNU user],
	[
#include <features.h>
#ifdef __GNU_LIBRARY__
 #if (__GLIBC__ >= 2)
  Lucky GNU user
 #endif
#endif
	],
	ac_cv_gnu_library_2=yes,
	ac_cv_gnu_library_2=no)
      ]
    )
    AC_SUBST(GLIBC2)
    GLIBC2="$ac_cv_gnu_library_2"
  ]
)
# glibc21.m4 serial 3
dnl Copyright (C) 2000-2002, 2004 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

# Test for the GNU C Library, version 2.1 or newer.
# From Bruno Haible.

AC_DEFUN([gl_GLIBC21],
  [
    AC_CACHE_CHECK(whether we are using the GNU C Library 2.1 or newer,
      ac_cv_gnu_library_2_1,
      [AC_EGREP_CPP([Lucky GNU user],
	[
#include <features.h>
#ifdef __GNU_LIBRARY__
 #if (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 1) || (__GLIBC__ > 2)
  Lucky GNU user
 #endif
#endif
	],
	ac_cv_gnu_library_2_1=yes,
	ac_cv_gnu_library_2_1=no)
      ]
    )
    AC_SUBST(GLIBC21)
    GLIBC21="$ac_cv_gnu_library_2_1"
  ]
)
# iconv.m4 serial AM4 (gettext-0.11.3)
dnl Copyright (C) 2000-2002 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.

AC_DEFUN([AM_ICONV_LINKFLAGS_BODY],
[
  dnl Prerequisites of AC_LIB_LINKFLAGS_BODY.
  AC_REQUIRE([AC_LIB_PREPARE_PREFIX])
  AC_REQUIRE([AC_LIB_RPATH])

  dnl Search for libiconv and define LIBICONV, LTLIBICONV and INCICONV
  dnl accordingly.
  dnl AC_LIB_LINKFLAGS_BODY([iconv])
])

AC_DEFUN([AM_ICONV_LINK],
[
  dnl Some systems have iconv in libc, some have it in libiconv (OSF/1 and
  dnl those with the standalone portable GNU libiconv installed).

  dnl Search for libiconv and define LIBICONV, LTLIBICONV and INCICONV
  dnl accordingly.
  dnl AC_REQUIRE([AM_ICONV_LINKFLAGS_BODY])

  dnl Add $INCICONV to CPPFLAGS before performing the following checks,
  dnl because if the user has installed libiconv and not disabled its use
  dnl via --without-libiconv-prefix, he wants to use it. The first
  dnl AC_TRY_LINK will then fail, the second AC_TRY_LINK will succeed.
  am_save_CPPFLAGS="$CPPFLAGS"
  AC_LIB_APPENDTOVAR([CPPFLAGS], [$INCICONV])

  AC_CACHE_CHECK(for iconv, am_cv_func_iconv, [
    am_cv_func_iconv="no, consider installing GNU libiconv"
    am_cv_lib_iconv=no
    AC_TRY_LINK([#include <stdlib.h>
#include <iconv.h>],
      [iconv_t cd = iconv_open("","");
       iconv(cd,NULL,NULL,NULL,NULL);
       iconv_close(cd);],
      am_cv_func_iconv=yes)
    if test "$am_cv_func_iconv" != yes; then
      am_save_LIBS="$LIBS"
      LIBS="$LIBS $LIBICONV"
      AC_TRY_LINK([#include <stdlib.h>
#include <iconv.h>],
        [iconv_t cd = iconv_open("","");
         iconv(cd,NULL,NULL,NULL,NULL);
         iconv_close(cd);],
        am_cv_lib_iconv=yes
        am_cv_func_iconv=yes)
      LIBS="$am_save_LIBS"
    fi
  ])
  if test "$am_cv_func_iconv" = yes; then
    AC_DEFINE(HAVE_ICONV, 1, [Define if you have the iconv() function. (For intl)])
  fi
  if test "$am_cv_lib_iconv" = yes; then
    AC_MSG_CHECKING([how to link with libiconv])
    AC_MSG_RESULT([$LIBICONV])
  else
    dnl If $LIBICONV didn't lead to a usable library, we don't need $INCICONV
    dnl either.
    CPPFLAGS="$am_save_CPPFLAGS"
    LIBICONV=
    LTLIBICONV=
  fi
  AC_SUBST(LIBICONV)
  AC_SUBST(LTLIBICONV)
])

AC_DEFUN([AM_ICONV],
[
  AM_ICONV_LINK
  if test "$am_cv_func_iconv" = yes; then
    AC_MSG_CHECKING([for iconv declaration])
    AC_CACHE_VAL(am_cv_proto_iconv, [
      AC_TRY_COMPILE([
#include <stdlib.h>
#include <iconv.h>
extern
#ifdef __cplusplus
"C"
#endif
#if defined(__STDC__) || defined(__cplusplus)
size_t iconv (iconv_t cd, char * *inbuf, size_t *inbytesleft, char * *outbuf, size_t *outbytesleft);
#else
size_t iconv();
#endif
], [], am_cv_proto_iconv_arg1="", am_cv_proto_iconv_arg1="const")
      am_cv_proto_iconv="extern size_t iconv (iconv_t cd, $am_cv_proto_iconv_arg1 char * *inbuf, size_t *inbytesleft, char * *outbuf, size_t *outbytesleft);"])
    am_cv_proto_iconv=`echo "[$]am_cv_proto_iconv" | tr -s ' ' | sed -e 's/( /(/'`
    AC_MSG_RESULT([$]{ac_t:-
         }[$]am_cv_proto_iconv)
    AC_DEFINE_UNQUOTED(ICONV_CONST, $am_cv_proto_iconv_arg1,
      [Define as const if the declaration of iconv() needs const.])
  fi
])
# intdiv0.m4 serial 1 (gettext-0.11.3)
dnl Copyright (C) 2002 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.

AC_DEFUN([gt_INTDIV0],
[
  AC_REQUIRE([AC_PROG_CC])dnl
  AC_REQUIRE([AC_CANONICAL_HOST])dnl

  AC_CACHE_CHECK([whether integer division by zero raises SIGFPE],
    gt_cv_int_divbyzero_sigfpe,
    [
      AC_TRY_RUN([
#include <stdlib.h>
#include <signal.h>

static void
#ifdef __cplusplus
sigfpe_handler (int sig)
#else
sigfpe_handler (sig) int sig;
#endif
{
  /* Exit with code 0 if SIGFPE, with code 1 if any other signal.  */
  exit (sig != SIGFPE);
}

int x = 1;
int y = 0;
int z;
int nan;

int main ()
{
  signal (SIGFPE, sigfpe_handler);
/* IRIX and AIX (when "xlc -qcheck" is used) yield signal SIGTRAP.  */
#if (defined (__sgi) || defined (_AIX)) && defined (SIGTRAP)
  signal (SIGTRAP, sigfpe_handler);
#endif
/* Linux/SPARC yields signal SIGILL.  */
#if defined (__sparc__) && defined (__linux__)
  signal (SIGILL, sigfpe_handler);
#endif

  z = x / y;
  nan = y / y;
  exit (1);
}
], gt_cv_int_divbyzero_sigfpe=yes, gt_cv_int_divbyzero_sigfpe=no,
        [
          # Guess based on the CPU.
          case "$host_cpu" in
            alpha* | i[34567]86 | m68k | s390*)
              gt_cv_int_divbyzero_sigfpe="guessing yes";;
            *)
              gt_cv_int_divbyzero_sigfpe="guessing no";;
          esac
        ])
    ])
  case "$gt_cv_int_divbyzero_sigfpe" in
    *yes) value=1;;
    *) value=0;;
  esac
  AC_DEFINE_UNQUOTED(INTDIV0_RAISES_SIGFPE, $value,
    [Define if integer division by zero raises signal SIGFPE. (For intl)])
])
# intl.m4 serial 3 (gettext-0.16)
dnl Copyright (C) 1995-2006 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl
dnl This file can can be used in projects which are not available under
dnl the GNU General Public License or the GNU Library General Public
dnl License but which still want to provide support for the GNU gettext
dnl functionality.
dnl Please note that the actual code of the GNU gettext library is covered
dnl by the GNU Library General Public License, and the rest of the GNU
dnl gettext package package is covered by the GNU General Public License.
dnl They are *not* in the public domain.

dnl Authors:
dnl   Ulrich Drepper <drepper@cygnus.com>, 1995-2000.
dnl   Bruno Haible <haible@clisp.cons.org>, 2000-2006.

AC_PREREQ(2.52)

dnl Checks for all prerequisites of the intl subdirectory,
dnl except for INTL_LIBTOOL_SUFFIX_PREFIX (and possibly LIBTOOL), INTLOBJS,
dnl            USE_INCLUDED_LIBINTL, BUILD_INCLUDED_LIBINTL.
AC_DEFUN([AM_INTL_SUBDIR],
[
  AC_REQUIRE([AC_PROG_INSTALL])dnl
  AC_REQUIRE([AM_PROG_MKDIR_P])dnl defined by automake
  AC_REQUIRE([AC_PROG_CC])dnl
  AC_REQUIRE([AC_CANONICAL_HOST])dnl
  AC_REQUIRE([gt_GLIBC2])dnl
  AC_REQUIRE([AC_PROG_RANLIB])dnl
  AC_REQUIRE([gl_VISIBILITY])dnl
  AC_REQUIRE([gt_INTL_SUBDIR_CORE])dnl
  AC_REQUIRE([AC_TYPE_LONG_LONG_INT])dnl
  AC_REQUIRE([gt_TYPE_LONGDOUBLE])dnl
  AC_REQUIRE([gt_TYPE_WCHAR_T])dnl
  AC_REQUIRE([gt_TYPE_WINT_T])dnl
  AC_REQUIRE([gl_AC_HEADER_INTTYPES_H])
  AC_REQUIRE([gt_TYPE_INTMAX_T])
  AC_REQUIRE([gt_PRINTF_POSIX])
  AC_REQUIRE([gl_GLIBC21])dnl
  AC_REQUIRE([gl_XSIZE])dnl
  AC_REQUIRE([gt_INTL_MACOSX])dnl

  AC_CHECK_TYPE([ptrdiff_t], ,
    [AC_DEFINE([ptrdiff_t], [long],
       [Define as the type of the result of subtracting two pointers, if the system doesn't define it.  (For intl)])
    ])
  AC_CHECK_HEADERS([stddef.h stdlib.h string.h])
  AC_CHECK_FUNCS([asprintf fwprintf putenv setenv setlocale snprintf wcslen])

  dnl Use the _snprintf function only if it is declared (because on NetBSD it
  dnl is defined as a weak alias of snprintf; we prefer to use the latter).
  gt_CHECK_DECL(_snprintf, [#include <stdio.h>])
  gt_CHECK_DECL(_snwprintf, [#include <stdio.h>])

  dnl Use the *_unlocked functions only if they are declared.
  dnl (because some of them were defined without being declared in Solaris
  dnl 2.5.1 but were removed in Solaris 2.6, whereas we want binaries built
  dnl on Solaris 2.5.1 to run on Solaris 2.6).
  dnl Don't use AC_CHECK_DECLS because it isn't supported in autoconf-2.13.
  gt_CHECK_DECL(getc_unlocked, [#include <stdio.h>])

  case $gt_cv_func_printf_posix in
    *yes) HAVE_POSIX_PRINTF=1 ;;
    *) HAVE_POSIX_PRINTF=0 ;;
  esac
  AC_SUBST([HAVE_POSIX_PRINTF])
  if test "$ac_cv_func_asprintf" = yes; then
    HAVE_ASPRINTF=1
  else
    HAVE_ASPRINTF=0
  fi
  AC_SUBST([HAVE_ASPRINTF])
  if test "$ac_cv_func_snprintf" = yes; then
    HAVE_SNPRINTF=1
  else
    HAVE_SNPRINTF=0
  fi
  AC_SUBST([HAVE_SNPRINTF])
  if test "$ac_cv_func_wprintf" = yes; then
    HAVE_WPRINTF=1
  else
    HAVE_WPRINTF=0
  fi
  AC_SUBST([HAVE_WPRINTF])

  AM_LANGINFO_CODESET
  gt_LC_MESSAGES

  dnl Compilation on mingw and Cygwin needs special Makefile rules, because
  dnl 1. when we install a shared library, we must arrange to export
  dnl    auxiliary pointer variables for every exported variable,
  dnl 2. when we install a shared library and a static library simultaneously,
  dnl    the include file specifies __declspec(dllimport) and therefore we
  dnl    must arrange to define the auxiliary pointer variables for the
  dnl    exported variables _also_ in the static library.
  if test "$enable_shared" = yes; then
    case "$host_os" in
      cygwin*) is_woe32dll=yes ;;
      *) is_woe32dll=no ;;
    esac
  else
    is_woe32dll=no
  fi
  WOE32DLL=$is_woe32dll
  AC_SUBST([WOE32DLL])

  dnl Rename some macros and functions used for locking.
  AH_BOTTOM([
#define __libc_lock_t                   gl_lock_t
#define __libc_lock_define              gl_lock_define
#define __libc_lock_define_initialized  gl_lock_define_initialized
#define __libc_lock_init                gl_lock_init
#define __libc_lock_lock                gl_lock_lock
#define __libc_lock_unlock              gl_lock_unlock
#define __libc_lock_recursive_t                   gl_recursive_lock_t
#define __libc_lock_define_recursive              gl_recursive_lock_define
#define __libc_lock_define_initialized_recursive  gl_recursive_lock_define_initialized
#define __libc_lock_init_recursive                gl_recursive_lock_init
#define __libc_lock_lock_recursive                gl_recursive_lock_lock
#define __libc_lock_unlock_recursive              gl_recursive_lock_unlock
#define glthread_in_use  libintl_thread_in_use
#define glthread_lock_init     libintl_lock_init
#define glthread_lock_lock     libintl_lock_lock
#define glthread_lock_unlock   libintl_lock_unlock
#define glthread_lock_destroy  libintl_lock_destroy
#define glthread_rwlock_init     libintl_rwlock_init
#define glthread_rwlock_rdlock   libintl_rwlock_rdlock
#define glthread_rwlock_wrlock   libintl_rwlock_wrlock
#define glthread_rwlock_unlock   libintl_rwlock_unlock
#define glthread_rwlock_destroy  libintl_rwlock_destroy
#define glthread_recursive_lock_init     libintl_recursive_lock_init
#define glthread_recursive_lock_lock     libintl_recursive_lock_lock
#define glthread_recursive_lock_unlock   libintl_recursive_lock_unlock
#define glthread_recursive_lock_destroy  libintl_recursive_lock_destroy
#define glthread_once                 libintl_once
#define glthread_once_call            libintl_once_call
#define glthread_once_singlethreaded  libintl_once_singlethreaded
])
])


dnl Checks for the core files of the intl subdirectory:
dnl   dcigettext.c
dnl   eval-plural.h
dnl   explodename.c
dnl   finddomain.c
dnl   gettextP.h
dnl   gmo.h
dnl   hash-string.h hash-string.c
dnl   l10nflist.c
dnl   libgnuintl.h.in (except the *printf stuff)
dnl   loadinfo.h
dnl   loadmsgcat.c
dnl   localealias.c
dnl   log.c
dnl   plural-exp.h plural-exp.c
dnl   plural.y
dnl Used by libglocale.
AC_DEFUN([gt_INTL_SUBDIR_CORE],
[
  AC_REQUIRE([AC_C_INLINE])dnl
  AC_REQUIRE([AC_TYPE_SIZE_T])dnl
  AC_REQUIRE([gl_AC_HEADER_STDINT_H])
  AC_REQUIRE([AC_FUNC_ALLOCA])dnl
  AC_REQUIRE([AC_FUNC_MMAP])dnl
  AC_REQUIRE([gt_INTDIV0])dnl
  AC_REQUIRE([gl_AC_TYPE_UINTMAX_T])dnl
  AC_REQUIRE([gt_INTTYPES_PRI])dnl
  AC_REQUIRE([gl_LOCK])dnl

  AC_TRY_LINK(
    [int foo (int a) { a = __builtin_expect (a, 10); return a == 10 ? 0 : 1; }],
    [],
    [AC_DEFINE([HAVE_BUILTIN_EXPECT], 1,
       [Define to 1 if the compiler understands __builtin_expect.  (For intl)])])

  AC_CHECK_HEADERS([argz.h inttypes.h limits.h unistd.h sys/param.h])
  AC_CHECK_FUNCS([getcwd getegid geteuid getgid getuid mempcpy munmap \
    stpcpy strcasecmp strdup strtoul tsearch argz_count argz_stringify \
    argz_next __fsetlocking])

  dnl Use the *_unlocked functions only if they are declared.
  dnl (because some of them were defined without being declared in Solaris
  dnl 2.5.1 but were removed in Solaris 2.6, whereas we want binaries built
  dnl on Solaris 2.5.1 to run on Solaris 2.6).
  dnl Don't use AC_CHECK_DECLS because it isn't supported in autoconf-2.13.
  gt_CHECK_DECL(feof_unlocked, [#include <stdio.h>])
  gt_CHECK_DECL(fgets_unlocked, [#include <stdio.h>])

  AM_ICONV

  dnl glibc >= 2.4 has a NL_LOCALE_NAME macro when _GNU_SOURCE is defined,
  dnl and a _NL_LOCALE_NAME macro always.
  AC_CACHE_CHECK([for NL_LOCALE_NAME macro], gt_cv_nl_locale_name,
    [AC_TRY_LINK([#include <langinfo.h>
#include <locale.h>],
      [char* cs = nl_langinfo(_NL_LOCALE_NAME(LC_MESSAGES));],
      gt_cv_nl_locale_name=yes,
      gt_cv_nl_locale_name=no)
    ])
  if test $gt_cv_nl_locale_name = yes; then
    AC_DEFINE(HAVE_NL_LOCALE_NAME, 1,
      [Define if you have <langinfo.h> and it defines the NL_LOCALE_NAME macro if _GNU_SOURCE is defined.])
  fi

  dnl intl/plural.c is generated from intl/plural.y. It requires bison,
  dnl because plural.y uses bison specific features. It requires at least
  dnl bison-1.26 because earlier versions generate a plural.c that doesn't
  dnl compile.
  dnl bison is only needed for the maintainer (who touches plural.y). But in
  dnl order to avoid separate Makefiles or --enable-maintainer-mode, we put
  dnl the rule in general Makefile. Now, some people carelessly touch the
  dnl files or have a broken "make" program, hence the plural.c rule will
  dnl sometimes fire. To avoid an error, defines BISON to ":" if it is not
  dnl present or too old.
  AC_CHECK_PROGS([INTLBISON], [bison])
  if test -z "$INTLBISON"; then
    ac_verc_fail=yes
  else
    dnl Found it, now check the version.
    AC_MSG_CHECKING([version of bison])
changequote(<<,>>)dnl
    ac_prog_version=`$INTLBISON --version 2>&1 | sed -n 's/^.*GNU Bison.* \([0-9]*\.[0-9.]*\).*$/\1/p'`
    case $ac_prog_version in
      '') ac_prog_version="v. ?.??, bad"; ac_verc_fail=yes;;
      1.2[6-9]* | 1.[3-9][0-9]* | [2-9].*)
changequote([,])dnl
         ac_prog_version="$ac_prog_version, ok"; ac_verc_fail=no;;
      *) ac_prog_version="$ac_prog_version, bad"; ac_verc_fail=yes;;
    esac
    AC_MSG_RESULT([$ac_prog_version])
  fi
  if test $ac_verc_fail = yes; then
    INTLBISON=:
  fi
])


dnl gt_CHECK_DECL(FUNC, INCLUDES)
dnl Check whether a function is declared.
AC_DEFUN([gt_CHECK_DECL],
[
  AC_CACHE_CHECK([whether $1 is declared], ac_cv_have_decl_$1,
    [AC_TRY_COMPILE([$2], [
#ifndef $1
  char *p = (char *) $1;
#endif
], ac_cv_have_decl_$1=yes, ac_cv_have_decl_$1=no)])
  if test $ac_cv_have_decl_$1 = yes; then
    gt_value=1
  else
    gt_value=0
  fi
  AC_DEFINE_UNQUOTED([HAVE_DECL_]translit($1, [a-z], [A-Z]), [$gt_value],
    [Define to 1 if you have the declaration of `$1', and to 0 if you don't.  (For intl)])
])
# intmax.m4 serial 3 (gettext-0.16)
dnl Copyright (C) 2002-2005 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.
dnl Test whether the system has the 'intmax_t' type, but don't attempt to
dnl find a replacement if it is lacking.

AC_DEFUN([gt_TYPE_INTMAX_T],
[
  AC_REQUIRE([gl_AC_HEADER_INTTYPES_H])
  AC_REQUIRE([gl_AC_HEADER_STDINT_H])
  AC_CACHE_CHECK(for intmax_t, gt_cv_c_intmax_t,
    [AC_TRY_COMPILE([
#include <stddef.h>
#include <stdlib.h>
#if HAVE_STDINT_H_WITH_UINTMAX
#include <stdint.h>
#endif
#if HAVE_INTTYPES_H_WITH_UINTMAX
#include <inttypes.h>
#endif
],     [intmax_t x = -1;
        return !x;],
       gt_cv_c_intmax_t=yes,
       gt_cv_c_intmax_t=no)])
  if test $gt_cv_c_intmax_t = yes; then
    AC_DEFINE(HAVE_INTMAX_T, 1,
      [Define if you have the 'intmax_t' type in <stdint.h> or <inttypes.h>.  (For intl)])
  fi
])
# inttypes-pri.m4 serial 4 (gettext-0.16)
dnl Copyright (C) 1997-2002, 2006 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.

AC_PREREQ(2.52)

# Define PRI_MACROS_BROKEN if <inttypes.h> exists and defines the PRI*
# macros to non-string values.  This is the case on AIX 4.3.3.

AC_DEFUN([gt_INTTYPES_PRI],
[
  AC_CHECK_HEADERS([inttypes.h])
  if test $ac_cv_header_inttypes_h = yes; then
    AC_CACHE_CHECK([whether the inttypes.h PRIxNN macros are broken],
      gt_cv_inttypes_pri_broken,
      [
        AC_TRY_COMPILE([#include <inttypes.h>
#ifdef PRId32
char *p = PRId32;
#endif
], [], gt_cv_inttypes_pri_broken=no, gt_cv_inttypes_pri_broken=yes)
      ])
  fi
  if test "$gt_cv_inttypes_pri_broken" = yes; then
    AC_DEFINE_UNQUOTED(PRI_MACROS_BROKEN, 1,
      [Define if <inttypes.h> exists and defines unusable PRI* macros.  (For intl)])
    PRI_MACROS_BROKEN=1
  else
    PRI_MACROS_BROKEN=0
  fi
  AC_SUBST([PRI_MACROS_BROKEN])
])
# inttypes_h.m4 serial 7
dnl Copyright (C) 1997-2004, 2006 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Paul Eggert.

# Define HAVE_INTTYPES_H_WITH_UINTMAX if <inttypes.h> exists,
# doesn't clash with <sys/types.h>, and declares uintmax_t.

AC_DEFUN([gl_AC_HEADER_INTTYPES_H],
[
  AC_CACHE_CHECK([for inttypes.h], gl_cv_header_inttypes_h,
  [AC_TRY_COMPILE(
    [#include <sys/types.h>
#include <inttypes.h>],
    [uintmax_t i = (uintmax_t) -1; return !i;],
    gl_cv_header_inttypes_h=yes,
    gl_cv_header_inttypes_h=no)])
  if test $gl_cv_header_inttypes_h = yes; then
    AC_DEFINE_UNQUOTED(HAVE_INTTYPES_H_WITH_UINTMAX, 1,
      [Define if <inttypes.h> exists, doesn't clash with <sys/types.h>,
       and declares uintmax_t.  (For intl)])
  fi
])
# lcmessage.m4 serial 4 (gettext-0.14.2)
dnl Copyright (C) 1995-2002, 2004-2005 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl
dnl This file can can be used in projects which are not available under
dnl the GNU General Public License or the GNU Library General Public
dnl License but which still want to provide support for the GNU gettext
dnl functionality.
dnl Please note that the actual code of the GNU gettext library is covered
dnl by the GNU Library General Public License, and the rest of the GNU
dnl gettext package package is covered by the GNU General Public License.
dnl They are *not* in the public domain.

dnl Authors:
dnl   Ulrich Drepper <drepper@cygnus.com>, 1995.

# Check whether LC_MESSAGES is available in <locale.h>.

AC_DEFUN([gt_LC_MESSAGES],
[
  AC_CACHE_CHECK([for LC_MESSAGES], gt_cv_val_LC_MESSAGES,
    [AC_TRY_LINK([#include <locale.h>], [return LC_MESSAGES],
       gt_cv_val_LC_MESSAGES=yes, gt_cv_val_LC_MESSAGES=no)])
  if test $gt_cv_val_LC_MESSAGES = yes; then
    AC_DEFINE(HAVE_LC_MESSAGES, 1,
      [Define if your <locale.h> file defines LC_MESSAGES.])
  fi
])
# lock.m4 serial 6 (gettext-0.16)
dnl Copyright (C) 2005-2006 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.

dnl Tests for a multithreading library to be used.
dnl Defines at most one of the macros USE_POSIX_THREADS, USE_SOLARIS_THREADS,
dnl USE_PTH_THREADS, USE_WIN32_THREADS
dnl Sets the variables LIBTHREAD and LTLIBTHREAD to the linker options for use
dnl in a Makefile (LIBTHREAD for use without libtool, LTLIBTHREAD for use with
dnl libtool).
dnl Sets the variables LIBMULTITHREAD and LTLIBMULTITHREAD similarly, for
dnl programs that really need multithread functionality. The difference
dnl between LIBTHREAD and LIBMULTITHREAD is that on platforms supporting weak
dnl symbols, typically LIBTHREAD="" whereas LIBMULTITHREAD="-lpthread".
dnl Adds to CPPFLAGS the flag -D_REENTRANT or -D_THREAD_SAFE if needed for
dnl multithread-safe programs.

AC_DEFUN([gl_LOCK_EARLY],
[
  AC_REQUIRE([gl_LOCK_EARLY_BODY])
])

dnl The guts of gl_LOCK_EARLY. Needs to be expanded only once.

AC_DEFUN([gl_LOCK_EARLY_BODY],
[
  dnl Ordering constraints: This macro modifies CPPFLAGS in a way that
  dnl influences the result of the autoconf tests that test for *_unlocked
  dnl declarations, on AIX 5 at least. Therefore it must come early.
  AC_BEFORE([$0], [gl_FUNC_GLIBC_UNLOCKED_IO])dnl
  AC_BEFORE([$0], [gl_ARGP])dnl

  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([AC_GNU_SOURCE]) dnl needed for pthread_rwlock_t on glibc systems
  dnl Check for multithreading.
  AC_ARG_ENABLE(threads,
dnl AC_HELP_STRING([--enable-threads={posix|solaris|pth|win32}], [specify multithreading API])
dnl AC_HELP_STRING([--disable-threads], [build without multithread safety]),
dnl    [gl_use_threads=$enableval],
    ,
    [case "$host_os" in
       dnl Disable multithreading by default on OSF/1, because it interferes
       dnl with fork()/exec(): When msgexec is linked with -lpthread, its child
       dnl process gets an endless segmentation fault inside execvp().
       osf*) gl_use_threads=no ;;
       *)    gl_use_threads=yes ;;
     esac
    ])
  if test "$gl_use_threads" = yes || test "$gl_use_threads" = posix; then
    # For using <pthread.h>:
    case "$host_os" in
      osf*)
        # On OSF/1, the compiler needs the flag -D_REENTRANT so that it
        # groks <pthread.h>. cc also understands the flag -pthread, but
        # we don't use it because 1. gcc-2.95 doesn't understand -pthread,
        # 2. putting a flag into CPPFLAGS that has an effect on the linker
        # causes the AC_TRY_LINK test below to succeed unexpectedly,
        # leading to wrong values of LIBTHREAD and LTLIBTHREAD.
        CPPFLAGS="$CPPFLAGS -D_REENTRANT"
        ;;
    esac
    # Some systems optimize for single-threaded programs by default, and
    # need special flags to disable these optimizations. For example, the
    # definition of 'errno' in <errno.h>.
    case "$host_os" in
      aix* | freebsd*) CPPFLAGS="$CPPFLAGS -D_THREAD_SAFE" ;;
      solaris*) CPPFLAGS="$CPPFLAGS -D_REENTRANT" ;;
    esac
  fi
])

dnl The guts of gl_LOCK. Needs to be expanded only once.

AC_DEFUN([gl_LOCK_BODY],
[
  AC_REQUIRE([gl_LOCK_EARLY_BODY])
  gl_threads_api=none
  LIBTHREAD=
  LTLIBTHREAD=
  LIBMULTITHREAD=
  LTLIBMULTITHREAD=
  if test "$gl_use_threads" != no; then
    dnl Check whether the compiler and linker support weak declarations.
    AC_MSG_CHECKING([whether imported symbols can be declared weak])
    gl_have_weak=no
    AC_TRY_LINK([extern void xyzzy ();
#pragma weak xyzzy], [xyzzy();], [gl_have_weak=yes])
    AC_MSG_RESULT([$gl_have_weak])
    if test "$gl_use_threads" = yes || test "$gl_use_threads" = posix; then
      # On OSF/1, the compiler needs the flag -pthread or -D_REENTRANT so that
      # it groks <pthread.h>. It's added above, in gl_LOCK_EARLY_BODY.
      AC_CHECK_HEADER(pthread.h, gl_have_pthread_h=yes, gl_have_pthread_h=no)
      if test "$gl_have_pthread_h" = yes; then
        # Other possible tests:
        #   -lpthreads (FSU threads, PCthreads)
        #   -lgthreads
        gl_have_pthread=
        # Test whether both pthread_mutex_lock and pthread_mutexattr_init exist
        # in libc. IRIX 6.5 has the first one in both libc and libpthread, but
        # the second one only in libpthread, and lock.c needs it.
        AC_TRY_LINK([#include <pthread.h>],
          [pthread_mutex_lock((pthread_mutex_t*)0);
           pthread_mutexattr_init((pthread_mutexattr_t*)0);],
          [gl_have_pthread=yes])
        # Test for libpthread by looking for pthread_kill. (Not pthread_self,
        # since it is defined as a macro on OSF/1.)
        if test -n "$gl_have_pthread"; then
          # The program links fine without libpthread. But it may actually
          # need to link with libpthread in order to create multiple threads.
          AC_CHECK_LIB(pthread, pthread_kill,
            [LIBMULTITHREAD=-lpthread LTLIBMULTITHREAD=-lpthread
             # On Solaris and HP-UX, most pthread functions exist also in libc.
             # Therefore pthread_in_use() needs to actually try to create a
             # thread: pthread_create from libc will fail, whereas
             # pthread_create will actually create a thread.
             case "$host_os" in
               solaris* | hpux*)
                 AC_DEFINE([PTHREAD_IN_USE_DETECTION_HARD], 1,
                   [Define if the pthread_in_use() detection is hard.  (For intl)])
             esac
            ])
        else
          # Some library is needed. Try libpthread and libc_r.
          AC_CHECK_LIB(pthread, pthread_kill,
            [gl_have_pthread=yes
             LIBTHREAD=-lpthread LTLIBTHREAD=-lpthread
             LIBMULTITHREAD=-lpthread LTLIBMULTITHREAD=-lpthread])
          if test -z "$gl_have_pthread"; then
            # For FreeBSD 4.
            AC_CHECK_LIB(c_r, pthread_kill,
              [gl_have_pthread=yes
               LIBTHREAD=-lc_r LTLIBTHREAD=-lc_r
               LIBMULTITHREAD=-lc_r LTLIBMULTITHREAD=-lc_r])
          fi
        fi
        if test -n "$gl_have_pthread"; then
          gl_threads_api=posix
          AC_DEFINE([USE_POSIX_THREADS], 1,
            [Define if the POSIX multithreading library can be used.  (For intl)])
          if test -n "$LIBMULTITHREAD" || test -n "$LTLIBMULTITHREAD"; then
            if test $gl_have_weak = yes; then
              AC_DEFINE([USE_POSIX_THREADS_WEAK], 1,
                [Define if references to the POSIX multithreading library should be made weak.  (For intl)])
              LIBTHREAD=
              LTLIBTHREAD=
            fi
          fi
          # OSF/1 4.0 and OS X 10.1 lack the pthread_rwlock_t type and the
          # pthread_rwlock_* functions.
          AC_CHECK_TYPE([pthread_rwlock_t],
            [AC_DEFINE([HAVE_PTHREAD_RWLOCK], 1,
               [Define if the POSIX multithreading library has read/write locks.  (For intl)])],
            [],
            [#include <pthread.h>])
          # glibc defines PTHREAD_MUTEX_RECURSIVE as enum, not as a macro.
          AC_TRY_COMPILE([#include <pthread.h>],
            [#if __FreeBSD__ == 4
error "No, in FreeBSD 4.0 recursive mutexes actually don't work."
#else
int x = (int)PTHREAD_MUTEX_RECURSIVE;
return !x;
#endif],
            [AC_DEFINE([HAVE_PTHREAD_MUTEX_RECURSIVE], 1,
               [Define if the <pthread.h> defines PTHREAD_MUTEX_RECURSIVE.  (For intl)])])
        fi
      fi
    fi
    if test -z "$gl_have_pthread"; then
      if test "$gl_use_threads" = yes || test "$gl_use_threads" = solaris; then
        gl_have_solaristhread=
        gl_save_LIBS="$LIBS"
        LIBS="$LIBS -lthread"
        AC_TRY_LINK([#include <thread.h>
#include <synch.h>],
          [thr_self();],
          [gl_have_solaristhread=yes])
        LIBS="$gl_save_LIBS"
        if test -n "$gl_have_solaristhread"; then
          gl_threads_api=solaris
          LIBTHREAD=-lthread
          LTLIBTHREAD=-lthread
          LIBMULTITHREAD="$LIBTHREAD"
          LTLIBMULTITHREAD="$LTLIBTHREAD"
          AC_DEFINE([USE_SOLARIS_THREADS], 1,
            [Define if the old Solaris multithreading library can be used.  (For intl)])
          if test $gl_have_weak = yes; then
            AC_DEFINE([USE_SOLARIS_THREADS_WEAK], 1,
              [Define if references to the old Solaris multithreading library should be made weak.  (For intl)])
            LIBTHREAD=
            LTLIBTHREAD=
          fi
        fi
      fi
    fi
    if test "$gl_use_threads" = pth; then
      gl_save_CPPFLAGS="$CPPFLAGS"
      AC_LIB_LINKFLAGS(pth)
      gl_have_pth=
      gl_save_LIBS="$LIBS"
      LIBS="$LIBS -lpth"
      AC_TRY_LINK([#include <pth.h>], [pth_self();], gl_have_pth=yes)
      LIBS="$gl_save_LIBS"
      if test -n "$gl_have_pth"; then
        gl_threads_api=pth
        LIBTHREAD="$LIBPTH"
        LTLIBTHREAD="$LTLIBPTH"
        LIBMULTITHREAD="$LIBTHREAD"
        LTLIBMULTITHREAD="$LTLIBTHREAD"
        AC_DEFINE([USE_PTH_THREADS], 1,
          [Define if the GNU Pth multithreading library can be used.  (For intl)])
        if test -n "$LIBMULTITHREAD" || test -n "$LTLIBMULTITHREAD"; then
          if test $gl_have_weak = yes; then
            AC_DEFINE([USE_PTH_THREADS_WEAK], 1,
              [Define if references to the GNU Pth multithreading library should be made weak.  (For intl)])
            LIBTHREAD=
            LTLIBTHREAD=
          fi
        fi
      else
        CPPFLAGS="$gl_save_CPPFLAGS"
      fi
    fi
    if test -z "$gl_have_pthread"; then
      if test "$gl_use_threads" = yes || test "$gl_use_threads" = win32; then
        if { case "$host_os" in
               mingw*) true;;
               *) false;;
             esac
           }; then
          gl_threads_api=win32
          AC_DEFINE([USE_WIN32_THREADS], 1,
            [Define if the Win32 multithreading API can be used.  (For intl)])
        fi
      fi
    fi
  fi
  AC_MSG_CHECKING([for multithread API to use])
  AC_MSG_RESULT([$gl_threads_api])
  AC_SUBST(LIBTHREAD)
  AC_SUBST(LTLIBTHREAD)
  AC_SUBST(LIBMULTITHREAD)
  AC_SUBST(LTLIBMULTITHREAD)
])

AC_DEFUN([gl_LOCK],
[
  AC_REQUIRE([gl_LOCK_EARLY])
  AC_REQUIRE([gl_LOCK_BODY])
  gl_PREREQ_LOCK
])

# Prerequisites of lib/lock.c.
AC_DEFUN([gl_PREREQ_LOCK], [
  AC_REQUIRE([AC_C_INLINE])
])

dnl Survey of platforms:
dnl
dnl Platform          Available   Compiler    Supports   test-lock
dnl                   flavours    option      weak       result
dnl ---------------   ---------   ---------   --------   ---------
dnl Linux 2.4/glibc   posix       -lpthread       Y      OK
dnl
dnl GNU Hurd/glibc    posix
dnl
dnl FreeBSD 5.3       posix       -lc_r           Y
dnl                   posix       -lkse ?         Y
dnl                   posix       -lpthread ?     Y
dnl                   posix       -lthr           Y
dnl
dnl FreeBSD 5.2       posix       -lc_r           Y
dnl                   posix       -lkse           Y
dnl                   posix       -lthr           Y
dnl
dnl FreeBSD 4.0,4.10  posix       -lc_r           Y      OK
dnl
dnl NetBSD 1.6        --
dnl
dnl OpenBSD 3.4       posix       -lpthread       Y      OK
dnl
dnl MacOS X 10.[123]  posix       -lpthread       Y      OK
dnl
dnl Solaris 7,8,9     posix       -lpthread       Y      Sol 7,8: 0.0; Sol 9: OK
dnl                   solaris     -lthread        Y      Sol 7,8: 0.0; Sol 9: OK
dnl
dnl HP-UX 11          posix       -lpthread       N (cc) OK
dnl                                               Y (gcc)
dnl
dnl IRIX 6.5          posix       -lpthread       Y      0.5
dnl
dnl AIX 4.3,5.1       posix       -lpthread       N      AIX 4: 0.5; AIX 5: OK
dnl
dnl OSF/1 4.0,5.1     posix       -pthread (cc)   N      OK
dnl                               -lpthread (gcc) Y
dnl
dnl Cygwin            posix       -lpthread       Y      OK
dnl
dnl Any of the above  pth         -lpth                  0.0
dnl
dnl Mingw             win32                       N      OK
dnl
dnl BeOS 5            --
dnl
dnl The test-lock result shows what happens if in test-lock.c EXPLICIT_YIELD is
dnl turned off:
dnl   OK if all three tests terminate OK,
dnl   0.5 if the first test terminates OK but the second one loops endlessly,
dnl   0.0 if the first test already loops endlessly.
# longdouble.m4 serial 2 (gettext-0.15)
dnl Copyright (C) 2002-2003, 2006 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.
dnl Test whether the compiler supports the 'long double' type.
dnl Prerequisite: AC_PROG_CC

dnl This file is only needed in autoconf <= 2.59.  Newer versions of autoconf
dnl have a macro AC_TYPE_LONG_DOUBLE with identical semantics.

AC_DEFUN([gt_TYPE_LONGDOUBLE],
[
  AC_CACHE_CHECK([for long double], gt_cv_c_long_double,
    [if test "$GCC" = yes; then
       gt_cv_c_long_double=yes
     else
       AC_TRY_COMPILE([
         /* The Stardent Vistra knows sizeof(long double), but does not support it.  */
         long double foo = 0.0;
         /* On Ultrix 4.3 cc, long double is 4 and double is 8.  */
         int array [2*(sizeof(long double) >= sizeof(double)) - 1];
         ], ,
         gt_cv_c_long_double=yes, gt_cv_c_long_double=no)
     fi])
dnl if test $gt_cv_c_long_double = yes; then
dnl    AC_DEFINE(HAVE_LONG_DOUBLE, 1, [Define if you have the 'long double' type.  (For intl)])
dnl  fi
])
# longlong.m4 serial 8
dnl Copyright (C) 1999-2006 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Paul Eggert.

# Define HAVE_LONG_LONG_INT if 'long long int' works.
# This fixes a bug in Autoconf 2.60, but can be removed once we
# assume 2.61 everywhere.

# Note: If the type 'long long int' exists but is only 32 bits large
# (as on some very old compilers), AC_TYPE_LONG_LONG_INT will not be
# defined. In this case you can treat 'long long int' like 'long int'.

AC_DEFUN([AC_TYPE_LONG_LONG_INT],
[
  AC_CACHE_CHECK([for long long int], [ac_cv_type_long_long_int],
    [AC_LINK_IFELSE(
       [AC_LANG_PROGRAM(
	  [[long long int ll = 9223372036854775807ll;
	    long long int nll = -9223372036854775807LL;
	    typedef int a[((-9223372036854775807LL < 0
			    && 0 < 9223372036854775807ll)
			   ? 1 : -1)];
	    int i = 63;]],
	  [[long long int llmax = 9223372036854775807ll;
	    return ((ll << 63) | (ll >> 63) | (ll < i) | (ll > i)
		    | (llmax / ll) | (llmax % ll));]])],
       [ac_cv_type_long_long_int=yes],
       [ac_cv_type_long_long_int=no])])
  if test $ac_cv_type_long_long_int = yes; then
    AC_DEFINE([HAVE_LONG_LONG_INT], 1,
      [Define to 1 if the system has the type `long long int'.  (For intl)])
  fi
])

# This macro is obsolescent and should go away soon.
AC_DEFUN([gl_AC_TYPE_LONG_LONG],
[
  AC_REQUIRE([AC_TYPE_LONG_LONG_INT])
  ac_cv_type_long_long=$ac_cv_type_long_long_int
  if test $ac_cv_type_long_long = yes; then
    AC_DEFINE(HAVE_LONG_LONG, 1,
      [Define if you have the 'long long' type.  (For intl)])
  fi
])
# nls.m4 serial 3 (gettext-0.15)
dnl Copyright (C) 1995-2003, 2005-2006 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl
dnl This file can can be used in projects which are not available under
dnl the GNU General Public License or the GNU Library General Public
dnl License but which still want to provide support for the GNU gettext
dnl functionality.
dnl Please note that the actual code of the GNU gettext library is covered
dnl by the GNU Library General Public License, and the rest of the GNU
dnl gettext package package is covered by the GNU General Public License.
dnl They are *not* in the public domain.

dnl Authors:
dnl   Ulrich Drepper <drepper@cygnus.com>, 1995-2000.
dnl   Bruno Haible <haible@clisp.cons.org>, 2000-2003.

AC_PREREQ(2.50)

AC_DEFUN([AM_NLS],
[
  AC_MSG_CHECKING([whether NLS is requested])
  dnl Default is enabled NLS
  AC_ARG_ENABLE(nls,
    [  --disable-nls           do not use Native Language Support],
    USE_NLS=$enableval, USE_NLS=yes)
  AC_MSG_RESULT($USE_NLS)
  AC_SUBST(USE_NLS)
])
# printf-posix.m4 serial 2 (gettext-0.13.1)
dnl Copyright (C) 2003 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.
dnl Test whether the printf() function supports POSIX/XSI format strings with
dnl positions.

AC_DEFUN([gt_PRINTF_POSIX],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_CACHE_CHECK([whether printf() supports POSIX/XSI format strings],
    gt_cv_func_printf_posix,
    [
      AC_TRY_RUN([
#include <stdio.h>
#include <string.h>
/* The string "%2$d %1$d", with dollar characters protected from the shell's
   dollar expansion (possibly an autoconf bug).  */
static char format[] = { '%', '2', '$', 'd', ' ', '%', '1', '$', 'd', '\0' };
static char buf[100];
int main ()
{
  sprintf (buf, format, 33, 55);
  return (strcmp (buf, "55 33") != 0);
}], gt_cv_func_printf_posix=yes, gt_cv_func_printf_posix=no,
      [
        AC_EGREP_CPP(notposix, [
#if defined __NetBSD__ || defined _MSC_VER || defined __MINGW32__ || defined __CYGWIN__
  notposix
#endif
        ], gt_cv_func_printf_posix="guessing no",
           gt_cv_func_printf_posix="guessing yes")
      ])
    ])
  case $gt_cv_func_printf_posix in
    *yes)
      AC_DEFINE(HAVE_POSIX_PRINTF, 1,
        [Define if your printf() function supports format strings with positions.  (For intl)])
      ;;
  esac
])
# progtest.m4 serial 4 (gettext-0.14.2)
dnl Copyright (C) 1996-2003, 2005 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl
dnl This file can can be used in projects which are not available under
dnl the GNU General Public License or the GNU Library General Public
dnl License but which still want to provide support for the GNU gettext
dnl functionality.
dnl Please note that the actual code of the GNU gettext library is covered
dnl by the GNU Library General Public License, and the rest of the GNU
dnl gettext package package is covered by the GNU General Public License.
dnl They are *not* in the public domain.

dnl Authors:
dnl   Ulrich Drepper <drepper@cygnus.com>, 1996.

AC_PREREQ(2.50)

# Search path for a program which passes the given test.

dnl AM_PATH_PROG_WITH_TEST(VARIABLE, PROG-TO-CHECK-FOR,
dnl   TEST-PERFORMED-ON-FOUND_PROGRAM [, VALUE-IF-NOT-FOUND [, PATH]])
AC_DEFUN([AM_PATH_PROG_WITH_TEST],
[
# Prepare PATH_SEPARATOR.
# The user is always right.
if test "${PATH_SEPARATOR+set}" != set; then
  echo "#! /bin/sh" >conf$$.sh
  echo  "exit 0"   >>conf$$.sh
  chmod +x conf$$.sh
  if (PATH="/nonexistent;."; conf$$.sh) >/dev/null 2>&1; then
    PATH_SEPARATOR=';'
  else
    PATH_SEPARATOR=:
  fi
  rm -f conf$$.sh
fi

# Find out how to test for executable files. Don't use a zero-byte file,
# as systems may use methods other than mode bits to determine executability.
cat >conf$$.file <<_ASEOF
#! /bin/sh
exit 0
_ASEOF
chmod +x conf$$.file
if test -x conf$$.file >/dev/null 2>&1; then
  ac_executable_p="test -x"
else
  ac_executable_p="test -f"
fi
rm -f conf$$.file

# Extract the first word of "$2", so it can be a program name with args.
set dummy $2; ac_word=[$]2
AC_MSG_CHECKING([for $ac_word])
AC_CACHE_VAL(ac_cv_path_$1,
[case "[$]$1" in
  [[\\/]]* | ?:[[\\/]]*)
    ac_cv_path_$1="[$]$1" # Let the user override the test with a path.
    ;;
  *)
    ac_save_IFS="$IFS"; IFS=$PATH_SEPARATOR
    for ac_dir in ifelse([$5], , $PATH, [$5]); do
      IFS="$ac_save_IFS"
      test -z "$ac_dir" && ac_dir=.
      for ac_exec_ext in '' $ac_executable_extensions; do
        if $ac_executable_p "$ac_dir/$ac_word$ac_exec_ext"; then
          echo "$as_me: trying $ac_dir/$ac_word..." >&AS_MESSAGE_LOG_FD
          if [$3]; then
            ac_cv_path_$1="$ac_dir/$ac_word$ac_exec_ext"
            break 2
          fi
        fi
      done
    done
    IFS="$ac_save_IFS"
dnl If no 4th arg is given, leave the cache variable unset,
dnl so AC_PATH_PROGS will keep looking.
ifelse([$4], , , [  test -z "[$]ac_cv_path_$1" && ac_cv_path_$1="$4"
])dnl
    ;;
esac])dnl
$1="$ac_cv_path_$1"
if test ifelse([$4], , [-n "[$]$1"], ["[$]$1" != "$4"]); then
  AC_MSG_RESULT([$]$1)
else
  AC_MSG_RESULT(no)
fi
AC_SUBST($1)dnl
])
# size_max.m4 serial 5
dnl Copyright (C) 2003, 2005-2006 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.

AC_DEFUN([gl_SIZE_MAX],
[
  AC_CHECK_HEADERS(stdint.h)
  dnl First test whether the system already has SIZE_MAX.
  AC_MSG_CHECKING([for SIZE_MAX])
  AC_CACHE_VAL([gl_cv_size_max], [
    gl_cv_size_max=
    AC_EGREP_CPP([Found it], [
#include <limits.h>
#if HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef SIZE_MAX
Found it
#endif
], gl_cv_size_max=yes)
    if test -z "$gl_cv_size_max"; then
      dnl Define it ourselves. Here we assume that the type 'size_t' is not wider
      dnl than the type 'unsigned long'. Try hard to find a definition that can
      dnl be used in a preprocessor #if, i.e. doesn't contain a cast.
      _AC_COMPUTE_INT([sizeof (size_t) * CHAR_BIT - 1], size_t_bits_minus_1,
        [#include <stddef.h>
#include <limits.h>], size_t_bits_minus_1=)
      _AC_COMPUTE_INT([sizeof (size_t) <= sizeof (unsigned int)], fits_in_uint,
        [#include <stddef.h>], fits_in_uint=)
      if test -n "$size_t_bits_minus_1" && test -n "$fits_in_uint"; then
        if test $fits_in_uint = 1; then
          dnl Even though SIZE_MAX fits in an unsigned int, it must be of type
          dnl 'unsigned long' if the type 'size_t' is the same as 'unsigned long'.
          AC_TRY_COMPILE([#include <stddef.h>
            extern size_t foo;
            extern unsigned long foo;
            ], [], fits_in_uint=0)
        fi
        dnl We cannot use 'expr' to simplify this expression, because 'expr'
        dnl works only with 'long' integers in the host environment, while we
        dnl might be cross-compiling from a 32-bit platform to a 64-bit platform.
        if test $fits_in_uint = 1; then
          gl_cv_size_max="(((1U << $size_t_bits_minus_1) - 1) * 2 + 1)"
        else
          gl_cv_size_max="(((1UL << $size_t_bits_minus_1) - 1) * 2 + 1)"
        fi
      else
        dnl Shouldn't happen, but who knows...
        gl_cv_size_max='((size_t)~(size_t)0)'
      fi
    fi
  ])
  AC_MSG_RESULT([$gl_cv_size_max])
  if test "$gl_cv_size_max" != yes; then
    AC_DEFINE_UNQUOTED([SIZE_MAX], [$gl_cv_size_max],
      [Define as the maximum value of type 'size_t', if the system doesn't define it.  (For intl)])
  fi
])
# stdint_h.m4 serial 6
dnl Copyright (C) 1997-2004, 2006 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Paul Eggert.

# Define HAVE_STDINT_H_WITH_UINTMAX if <stdint.h> exists,
# doesn't clash with <sys/types.h>, and declares uintmax_t.

AC_DEFUN([gl_AC_HEADER_STDINT_H],
[
  AC_CACHE_CHECK([for stdint.h], gl_cv_header_stdint_h,
  [AC_TRY_COMPILE(
    [#include <sys/types.h>
#include <stdint.h>],
    [uintmax_t i = (uintmax_t) -1; return !i;],
    gl_cv_header_stdint_h=yes,
    gl_cv_header_stdint_h=no)])
  if test $gl_cv_header_stdint_h = yes; then
    AC_DEFINE_UNQUOTED(HAVE_STDINT_H_WITH_UINTMAX, 1,
      [Define if <stdint.h> exists, doesn't clash with <sys/types.h>,
       and declares uintmax_t.  (For intl)])
  fi
])
# uintmax_t.m4 serial 9
dnl Copyright (C) 1997-2004 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Paul Eggert.

AC_PREREQ(2.13)

# Define uintmax_t to 'unsigned long' or 'unsigned long long'
# if it is not already defined in <stdint.h> or <inttypes.h>.

AC_DEFUN([gl_AC_TYPE_UINTMAX_T],
[
  AC_REQUIRE([gl_AC_HEADER_INTTYPES_H])
  AC_REQUIRE([gl_AC_HEADER_STDINT_H])
  if test $gl_cv_header_inttypes_h = no && test $gl_cv_header_stdint_h = no; then
    AC_REQUIRE([gl_AC_TYPE_UNSIGNED_LONG_LONG])
    test $ac_cv_type_unsigned_long_long = yes \
      && ac_type='unsigned long long' \
      || ac_type='unsigned long'
    AC_DEFINE_UNQUOTED(uintmax_t, $ac_type,
      [Define to unsigned long or unsigned long long
       if <stdint.h> and <inttypes.h> don't define.  (For intl)])
  else
    AC_DEFINE(HAVE_UINTMAX_T, 1,
      [Define if you have the 'uintmax_t' type in <stdint.h> or <inttypes.h>.  (For intl)])
  fi
])
# ulonglong.m4 serial 6
dnl Copyright (C) 1999-2006 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Paul Eggert.

# Define HAVE_UNSIGNED_LONG_LONG_INT if 'unsigned long long int' works.
# This fixes a bug in Autoconf 2.60, but can be removed once we
# assume 2.61 everywhere.

# Note: If the type 'unsigned long long int' exists but is only 32 bits
# large (as on some very old compilers), AC_TYPE_UNSIGNED_LONG_LONG_INT
# will not be defined. In this case you can treat 'unsigned long long int'
# like 'unsigned long int'.

AC_DEFUN([AC_TYPE_UNSIGNED_LONG_LONG_INT],
[
  AC_CACHE_CHECK([for unsigned long long int],
    [ac_cv_type_unsigned_long_long_int],
    [AC_LINK_IFELSE(
       [AC_LANG_PROGRAM(
	  [[unsigned long long int ull = 18446744073709551615ULL;
	    typedef int a[(18446744073709551615ULL <= (unsigned long long int) -1
			   ? 1 : -1)];
	   int i = 63;]],
	  [[unsigned long long int ullmax = 18446744073709551615ull;
	    return (ull << 63 | ull >> 63 | ull << i | ull >> i
		    | ullmax / ull | ullmax % ull);]])],
       [ac_cv_type_unsigned_long_long_int=yes],
       [ac_cv_type_unsigned_long_long_int=no])])
  if test $ac_cv_type_unsigned_long_long_int = yes; then
    AC_DEFINE([HAVE_UNSIGNED_LONG_LONG_INT], 1,
      [Define to 1 if the system has the type `unsigned long long int'.  (For intl)])
  fi
])

# This macro is obsolescent and should go away soon.
AC_DEFUN([gl_AC_TYPE_UNSIGNED_LONG_LONG],
[
  AC_REQUIRE([AC_TYPE_UNSIGNED_LONG_LONG_INT])
  ac_cv_type_unsigned_long_long=$ac_cv_type_unsigned_long_long_int
  if test $ac_cv_type_unsigned_long_long = yes; then
    AC_DEFINE(HAVE_UNSIGNED_LONG_LONG, 1,
      [Define if you have the 'unsigned long long' type.  (For intl)])
  fi
])
# visibility.m4 serial 1 (gettext-0.15)
dnl Copyright (C) 2005 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.

dnl Tests whether the compiler supports the command-line option
dnl -fvisibility=hidden and the function and variable attributes
dnl __attribute__((__visibility__("hidden"))) and
dnl __attribute__((__visibility__("default"))).
dnl Does *not* test for __visibility__("protected") - which has tricky
dnl semantics (see the 'vismain' test in glibc) and does not exist e.g. on
dnl MacOS X.
dnl Does *not* test for __visibility__("internal") - which has processor
dnl dependent semantics.
dnl Does *not* test for #pragma GCC visibility push(hidden) - which is
dnl "really only recommended for legacy code".
dnl Set the variable CFLAG_VISIBILITY.
dnl Defines and sets the variable HAVE_VISIBILITY.

AC_DEFUN([gl_VISIBILITY],
[
  AC_REQUIRE([AC_PROG_CC])
  CFLAG_VISIBILITY=
  HAVE_VISIBILITY=0
  if test -n "$GCC"; then
    AC_MSG_CHECKING([for simple visibility declarations])
    AC_CACHE_VAL(gl_cv_cc_visibility, [
      gl_save_CFLAGS="$CFLAGS"
      CFLAGS="$CFLAGS -fvisibility=hidden"
      AC_TRY_COMPILE(
        [extern __attribute__((__visibility__("hidden"))) int hiddenvar;
         extern __attribute__((__visibility__("default"))) int exportedvar;
         extern __attribute__((__visibility__("hidden"))) int hiddenfunc (void);
         extern __attribute__((__visibility__("default"))) int exportedfunc (void);],
        [],
        gl_cv_cc_visibility=yes,
        gl_cv_cc_visibility=no)
      CFLAGS="$gl_save_CFLAGS"])
    AC_MSG_RESULT([$gl_cv_cc_visibility])
    if test $gl_cv_cc_visibility = yes; then
      CFLAG_VISIBILITY="-fvisibility=hidden"
      HAVE_VISIBILITY=1
    fi
  fi
  AC_SUBST([CFLAG_VISIBILITY])
  AC_SUBST([HAVE_VISIBILITY])
  AC_DEFINE_UNQUOTED([HAVE_VISIBILITY], [$HAVE_VISIBILITY],
    [Define to 1 or 0, depending whether the compiler supports simple visibility declarations.  (For intl)])
])
# wchar_t.m4 serial 1 (gettext-0.12)
dnl Copyright (C) 2002-2003 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.
dnl Test whether <stddef.h> has the 'wchar_t' type.
dnl Prerequisite: AC_PROG_CC

AC_DEFUN([gt_TYPE_WCHAR_T],
[
  AC_CACHE_CHECK([for wchar_t], gt_cv_c_wchar_t,
    [AC_TRY_COMPILE([#include <stddef.h>
       wchar_t foo = (wchar_t)'\0';], ,
       gt_cv_c_wchar_t=yes, gt_cv_c_wchar_t=no)])
  if test $gt_cv_c_wchar_t = yes; then
    AC_DEFINE(HAVE_WCHAR_T, 1, [Define if you have the 'wchar_t' type.  (For intl)])
  fi
])
# wint_t.m4 serial 1 (gettext-0.12)
dnl Copyright (C) 2003 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.
dnl Test whether <wchar.h> has the 'wint_t' type.
dnl Prerequisite: AC_PROG_CC

AC_DEFUN([gt_TYPE_WINT_T],
[
  AC_CACHE_CHECK([for wint_t], gt_cv_c_wint_t,
    [AC_TRY_COMPILE([#include <wchar.h>
       wint_t foo = (wchar_t)'\0';], ,
       gt_cv_c_wint_t=yes, gt_cv_c_wint_t=no)])
  if test $gt_cv_c_wint_t = yes; then
    AC_DEFINE(HAVE_WINT_T, 1, [Define if you have the 'wint_t' type.  (For intl)])
  fi
])
# xsize.m4 serial 3
dnl Copyright (C) 2003-2004 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_XSIZE],
[
  dnl Prerequisites of lib/xsize.h.
  AC_REQUIRE([gl_SIZE_MAX])
  AC_REQUIRE([AC_C_INLINE])
  AC_CHECK_HEADERS(stdint.h)
])
