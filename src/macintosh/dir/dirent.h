/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-1999  Ross Ihaka
 *                2000-2001  Stefano M. Iacus and the R core team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
  Macintosh specific include declarations. This emulates dirent.h
*/
#ifndef _DIRENT_H
#define _DIRENT_H

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

#include <locale.h>
/*
#include <stat.h>
*/
#include <errno.h>
#include <Errors.h>

/*
  Define declarations.
*/
#define S_IREAD  00400
#define S_IWRITE  00200

/*
  Typedef declarations.
*/

#ifndef NAME_MAX
#define NAME_MAX    1024
#endif

struct dirent {
    unsigned long   d_fileno;
    short           d_reclen;
    short           d_namlen;
    char            d_name[NAME_MAX + 1];
};

#ifndef DIR
typedef struct {
    short           ioFDirIndex;
    short           ioVRefNum;
    long            ioDrDirID;
    short           flags;
    struct dirent   currEntry;
} DIR;
#endif

#define direct dirent

/*
  Macintosh utilities routines.
*/
extern DIR
  *opendir(char *);

extern int
  Exit(int),
  MACSystemCommand(const char *);

extern long
  telldir(DIR *);

extern struct dirent
  *readdir(DIR *);
 
extern void
  closedir(DIR *),
  MACErrorHandler(const unsigned int,const char *,const char *),
  MACWarningHandler(const unsigned int,const char *,const char *),
  ProcessPendingEvents(char *),
  seekdir(DIR *,long),
  SetApplicationType(const char *,const char *,OSType);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif
void closedir(DIR *entry);
DIR *opendir(char *path);
struct dirent *readdir(DIR *entry);
void seekdir(DIR *entry,long position);
void rewinddir(DIR *entry);
#endif
