/* Copyright (C) 1999-2003 Free Software Foundation, Inc.
   This file is part of the GNU LIBICONV Library.

   The GNU LIBICONV Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   The GNU LIBICONV Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU LIBICONV Library; see the file COPYING.LIB.
   If not, write to the Free Software Foundation, Inc., 59 Temple Place -
   Suite 330, Boston, MA 02111-1307, USA.  */

#ifndef _LIBICONV_H
#define _LIBICONV_H

#define _LIBICONV_VERSION 0x0109    /* version number: (major<<8) + minor */

#undef iconv_t
#define iconv_t libiconv_t
typedef void* iconv_t;

/* Get size_t declaration. */
#include <stddef.h>

#define iconv_open libiconv_open
#define iconv libiconv
#define iconv_close libiconv_close

extern iconv_t iconv_open (const char* tocode, const char* fromcode);
extern size_t iconv (iconv_t cd, const char* * inbuf, size_t *inbytesleft, 
		     char* * outbuf, size_t *outbytesleft);
extern int iconv_close (iconv_t cd);

/* Listing of locale independent encodings. */
#define iconvlist libiconvlist
extern void iconvlist (int (*do_one) (unsigned int namescount,
                                      const char * const * names,
                                      void* data),
                       void* data);

#endif /* _LIBICONV_H */
