/*
  Macintosh specific include declarations.
*/
#ifndef _DIRENT_H
#define _DIRENT_H

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

#include <locale.h>
#include <stat.h>
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

typedef struct {
    short           ioFDirIndex;
    short           ioVRefNum;
    long            ioDrDirID;
    short           flags;
    struct dirent   currEntry;
} DIR;


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
