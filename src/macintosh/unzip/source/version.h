/*
  Copyright (c) 1990-2000 Info-ZIP.  All rights reserved.

  See the accompanying file LICENSE, version 2000-Apr-09 or later
  (the contents of which are also included in unzip.h) for terms of use.
  If, for some reason, all these files are missing, the Info-ZIP license
  also may be found at:  ftp://ftp.info-zip.org/pub/infozip/license.html
*/
/*
   version.h (for UnZip) by Info-ZIP.
 */

#ifndef __version_h     /* don't include more than once */
#define __version_h

#ifndef BETA
#  define BETA          /* define BETA for internal beta releases */
#endif

#ifdef BETA
#  define UZ_BETALEVEL      "c BETA"
#  define UZ_VERSION_DATE   "17 Jul 00"         /* internal beta version */
#else
#  define UZ_BETALEVEL      ""
#  define UZ_VERSION_DATE   "26 July 2000"      /* official release version */
#  define RELEASE
#endif

#define UZ_MAJORVER    5   /* UnZip */
#define UZ_MINORVER    4

#define ZI_MAJORVER    2   /* ZipInfo */
#define ZI_MINORVER    3

#define UZ_PATCHLEVEL  2

#define UZ_VER_STRING  "5.42"        /* keep in sync with Version numbers! */


/* these are obsolete but remain for backward compatibility: */
#define D2_MAJORVER    UZ_MAJORVER   /* DLL for OS/2 */
#define D2_MINORVER    UZ_MINORVER
#define D2_PATCHLEVEL  UZ_PATCHLEVEL

#define DW_MAJORVER    UZ_MAJORVER   /* DLL for MS Windows */
#define DW_MINORVER    UZ_MINORVER
#define DW_PATCHLEVEL  UZ_PATCHLEVEL

#define WIN_VERSION_DATE  UZ_VERSION_DATE

#endif /* !__version_h */
