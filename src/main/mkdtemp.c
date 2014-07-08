/* Copyright (C) 1999, 2001-2003 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   a copy is available at Foundation, Inc., 51 Franklin Street,
   Fifth Floor, Boston, MA 02110-1301 USA.  */

/* Extracted from misc/mkdtemp.c and sysdeps/posix/tempname.c,
   with R modifications for randomness and Win32.  

   mkdtemp was required by POSIX 2008: we use this substitute on
   Windows and Solaris 10.
*/

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <errno.h>
#ifndef __set_errno
# define __set_errno(Val) errno = (Val)
#endif

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include <stdio.h>
#ifndef TMP_MAX
# define TMP_MAX 238328
#endif

/* This is a little strange: inttypes.h is supposed according to
   POSIX to include stdint.h */
#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif


#ifndef Win32
#include <sys/stat.h>
#ifdef STAT_MACROS_BROKEN
# undef S_ISDIR
#endif
#if !defined S_ISDIR && defined S_IFDIR
# define S_ISDIR(mode) (((mode) & S_IFMT) == S_IFDIR)
#endif
#if !S_IRUSR && S_IREAD
# define S_IRUSR S_IREAD
#endif
#if !S_IRUSR
# define S_IRUSR 00400
#endif
#if !S_IWUSR && S_IWRITE
# define S_IWUSR S_IWRITE
#endif
#if !S_IWUSR
# define S_IWUSR 00200
#endif
#if !S_IXUSR && S_IEXEC
# define S_IXUSR S_IEXEC
#endif
#if !S_IXUSR
# define S_IXUSR 00100
#endif
#endif

/* Use the widest available unsigned type if uint64_t is not
   available.  The algorithm below extracts a number less than 62**6
   (approximately 2**35.725) from uint64_t, so ancient hosts where
   uintmax_t is only 32 bits lose about 3.725 bits of randomness,
   which is better than not having mkstemp at all.  */
#if !defined UINT64_MAX && !defined uint64_t
# define uint64_t uintmax_t
#endif

/* These are the characters used in temporary filenames.  */
static const char letters[] =
"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

extern unsigned int Rf_TimeToSeed(void); // times.c

/* Generate a temporary file name based on TMPL.  TMPL must match the
   rules for mk[s]temp (i.e. end in "XXXXXX").  The name constructed
   does not exist at the time of the call to __gen_tempname.  TMPL is
   overwritten with the result.

   KIND is:
   __GT_DIR:            create a directory, which will be mode 0700.

   We use a clever algorithm to get hard-to-predict names. */
static int
gen_tempname (char *tmpl)
{
  int len;
  char *XXXXXX;
  static uint64_t value;
  uint64_t random_time_bits;
  int count, fd = -1;
  int save_errno = errno;

  len = (int) strlen(tmpl);
  if (len < 6 || strcmp (&tmpl[len - 6], "XXXXXX"))
    {
      __set_errno (EINVAL);
      return -1;
    }

  /* This is where the Xs start.  */
  XXXXXX = &tmpl[len - 6];

  /* Get some more or less random data.  We need 36 bits. */
  random_time_bits = Rf_TimeToSeed();
  value += (random_time_bits << 8) ^ getpid ();

  for (count = 0; count < TMP_MAX; value += 7777, ++count)
    {
      uint64_t v = value;

      /* Fill in the random bits.  */
      XXXXXX[0] = letters[v % 62];
      v /= 62;
      XXXXXX[1] = letters[v % 62];
      v /= 62;
      XXXXXX[2] = letters[v % 62];
      v /= 62;
      XXXXXX[3] = letters[v % 62];
      v /= 62;
      XXXXXX[4] = letters[v % 62];
      v /= 62;
      XXXXXX[5] = letters[v % 62];

#ifdef Win32
      fd = mkdir (tmpl);
#else
      fd = mkdir (tmpl, S_IRUSR | S_IWUSR | S_IXUSR);
#endif

      if (fd >= 0)
	{
	  __set_errno (save_errno);
	  return fd;
	}
      else if (errno != EEXIST)
	return -1;
    }

  /* We got out of the loop because we ran out of combinations to try.  */
  __set_errno (EEXIST);
  return -1;
}

/* Generate a unique temporary directory from TEMPLATE.
   The last six characters of TEMPLATE must be "XXXXXX";
   they are replaced with a string that makes the filename unique.
   The directory is created, mode 700, and its name is returned.
   (This function comes from OpenBSD.) */
char *
mkdtemp (char *Template)
#ifdef __cplusplus
	throw()
#endif
{
  if (gen_tempname (Template))
    return NULL;
  else
    return Template;
}
