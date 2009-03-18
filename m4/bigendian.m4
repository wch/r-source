# extracted form autoconf 2.61: 2.63 is different
# Programming languages support.
# Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software
# Foundation, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

# R_BIGENDIAN ([ACTION-IF-TRUE], [ACTION-IF-FALSE], [ACTION-IF-UNKNOWN])
# -------------------------------------------------------------------------
AC_DEFUN([R_BIGENDIAN],
[AC_CACHE_CHECK(whether byte ordering is bigendian, ac_cv_c_bigendian,
[# See if sys/param.h defines the BYTE_ORDER macro.
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <sys/types.h>
#include <sys/param.h>
],
[#if  ! (defined BYTE_ORDER && defined BIG_ENDIAN && defined LITTLE_ENDIAN \
	&& BYTE_ORDER && BIG_ENDIAN && LITTLE_ENDIAN)
 bogus endian macros
#endif
])],
[# It does; now see whether it defined to BIG_ENDIAN or not.
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <sys/types.h>
#include <sys/param.h>
], [#if BYTE_ORDER != BIG_ENDIAN
 not big endian
#endif
])], [ac_cv_c_bigendian=yes], [ac_cv_c_bigendian=no])],
[# It does not; compile a test program.
AC_RUN_IFELSE(
[AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT], [[
  /* Are we little or big endian?  From Harbison&Steele.  */
  union
  {
    long int l;
    char c[sizeof (long int)];
  } u;
  u.l = 1;
  return u.c[sizeof (long int) - 1] == 1;
]])],
	      [ac_cv_c_bigendian=no],
	      [ac_cv_c_bigendian=yes],
[# try to guess the endianness by grepping values into an object file
  ac_cv_c_bigendian=unknown
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
[[short int ascii_mm[] = { 0x4249, 0x4765, 0x6E44, 0x6961, 0x6E53, 0x7953, 0 };
short int ascii_ii[] = { 0x694C, 0x5454, 0x656C, 0x6E45, 0x6944, 0x6E61, 0 };
void _ascii () { char *s = (char *) ascii_mm; s = (char *) ascii_ii; }
short int ebcdic_ii[] = { 0x89D3, 0xE3E3, 0x8593, 0x95C5, 0x89C4, 0x9581, 0 };
short int ebcdic_mm[] = { 0xC2C9, 0xC785, 0x95C4, 0x8981, 0x95E2, 0xA8E2, 0 };
void _ebcdic () { char *s = (char *) ebcdic_mm; s = (char *) ebcdic_ii; }]],
[[ _ascii (); _ebcdic (); ]])],
[if grep BIGenDianSyS conftest.$ac_objext >/dev/null ; then
  ac_cv_c_bigendian=yes
fi
if grep LiTTleEnDian conftest.$ac_objext >/dev/null ; then
  if test "$ac_cv_c_bigendian" = unknown; then
    ac_cv_c_bigendian=no
  else
    # finding both strings is unlikely to happen, but who knows?
    ac_cv_c_bigendian=unknown
  fi
fi])])])])
case $ac_cv_c_bigendian in
  yes)
    m4_default([$1],
      [AC_DEFINE([WORDS_BIGENDIAN], 1,
	[Define to 1 if your processor stores words with the most significant
	 byte first (like Motorola and SPARC, unlike Intel and VAX).])]) ;;
  no)
    $2 ;;
  *)
    m4_default([$3],
      [AC_MSG_ERROR([unknown endianness
presetting ac_cv_c_bigendian=no (or yes) will help])]) ;;
esac
])# R_BIGENDIAN
