#include <stddef.h>
#include <string.h>
#include <dlfcn.h>
#include <stdio.h>

#include <CodeFragments.h>
#include "macutils.h"

static char errbuf[512];

/* Minimal emulation of SysVR4-ELF dynamic loading routines for the Macintosh.
 * Based on code by Bob Stine as Modified by Steve Majewski. */
void *dlopen(const char *name, int mode)
{
  FSSpec fileSpec;
  Str255 errName, libName;
  OSErr err;
  Ptr mainAddr;
  CFragConnectionID connID;

  /* Build a file spec record for GetDiskFragment */
  if (strlen(name) < 254)
    strcpy((char *) libName, name);
  else {
    sprintf(errbuf, "library name too long");
    return NULL;
  }
  CtoPstr((char *) libName);
  err = FSMakeFSSpecFromPath((ConstStr255Param) libName, &fileSpec);
  if (err != noErr) {
    sprintf(errbuf, "error code %d creating file spec for library %s",
            err, name);
    return NULL;
  }

  /* Open the fragment (will not add another copy if loaded, though gives
     new ID) */
  err = GetDiskFragment(&fileSpec, 0, kCFragGoesToEOF, 0, kLoadCFrag,
                        &connID, &mainAddr, errName);
  if (err == noErr)
    return (void *) connID;
  else {
    PtoCstr(errName);
    sprintf(errbuf, "error code %d getting disk fragment %s for library %s",
            err, errName, name);
    return NULL;
  }
}

/* This version does not handle NULL as the library for looking in the
   executable. It also does not check the symbol class. */
void *dlsym(void *lib, const char *name)
{
  CFragConnectionID connID = (CFragConnectionID) lib;
  OSErr err;
  Ptr symAddr;
  CFragSymbolClass symClass;
  Str255 symName;
  
   
  if (strlen(name) < 254)
    strcpy((char *) symName, name);
  else {
    sprintf(errbuf, "symbol name too long");
    return NULL;
  }
  CtoPstr((char *) symName);
  err = FindSymbol(connID, symName, &symAddr, &symClass);
  if (err == noErr)
    return (void *) symAddr;
  else {
    sprintf(errbuf, "error code %d looking up symbol %s", err, name);
    return NULL;
  }
}

int dlclose(void *lib)
{
  CFragConnectionID connID = (CFragConnectionID) lib;
  OSErr err;
  err = CloseConnection(&connID);
  if (err == noErr)
    return 0;
  else {
    sprintf(errbuf, "error code %d closing library", err);
    return -1;
  }
}

char *dlerror()
{
  return errbuf;
}
