/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997  The R Core Team
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

/*  Changes made by Heiner Schwarte:
 *
 *  1. Ensure that the code will run on platforms which do not support
 *     the use of dlsym to loacate symbols in the executing program
 *     itself (AIX).
 *
 *  2. Symbols are added to a hash table as they are located.
 */

/*  Dynamic Loading Support
 *
 *  This module provides support for run-time loading of shared libraries
 *  access to symbols within such libraries via .C and .Fortran.  This is
 *  done under Unix with dlopen, dlclose and dlsym (the exception is
 *  hpux, where we use compatibility code provided by Luke Tierney.
 *  There are two cases:
 *
 *
 *  1. The dlopen interface is available.
 *
 *  In this case all symbol location is done using the dlopen routines.
 *  We maintain a list of currently loaded shared libraries in an array
 *  called "LoadedDLL" with the number of currenly loaded libraries
 *  being "CountDLL".  To locate a symbol, we probe the loaded libraries
 *  in order until the symbol is located.  If we do not find a symbol
 *  in the loaded libraries, we search the executable itself.  This
 *  search is not very efficient, but this probably pales into
 *  insignificance when compared with the inefficiencies in the R
 *  interpreter.
 *
 *  Loading and unloading of shared libraries is done via the routines
 *  AddDLL and DeleteDLL.  These routines maintain the list of currently
 *  loaded libraries.  When a library is added, any existing reference
 *  to that library are deleted and then the library is inserted at the
 *  start of the search list.  This way, symbols in more recently loaded
 *  libraries are found first.
 *
 *
 *  2. The dlopen interface is not available.
 *
 *  In this case we use the table "CFunTabEntry" to locate functions
 *  in the executable.  We do this by straight linear search through
 *  the table.  Note that the content of the table is created at
 *  system build time from the list in ../appl/ROUTINES.
 */

#include "Defn.h"
#include "Mathlib.h"
#include <string.h>
#include <stdlib.h>
#include <sys/param.h>

typedef int (*DL_FUNC)();
typedef struct {
        char *name;
        DL_FUNC func;
} CFunTabEntry;  
#include "FFDecl.h"

	/* This provides a table of built-in C and Fortran functions */

static CFunTabEntry CFunTab[] =
{
#include "FFTab.h"
        {NULL, NULL}
};      

	/* The following code loads in a compatibility module */
	/* written by Luke Tierney to support S version 4 on */
	/* Hewlett-Packard machines.  The relevant defines are */
	/* set up by autoconfigure */

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#else
#ifdef HAVE_DL_H
#include "hpdlfcn.c"
#define HAVE_DLFCN_H
#endif
#endif

#ifdef HAVE_DLFCN_H
#ifndef RTLD_LAZY
#define RTLD_LAZY 1
#endif

struct libhandlelist
{
  void *ptr;
  char *librarypath;
  struct libhandlelist *next;
};

static struct libhandlelist *list = NULL;

static void 
add_ptr (void *p, char *name)
{
  struct libhandlelist *tmp;
  if (p == NULL)
    return;
  tmp = (struct libhandlelist *) malloc (sizeof (struct libhandlelist));
  tmp->ptr = p;
  tmp->librarypath = (char *) malloc (sizeof (char) * (strlen (name) + 1));
  strcpy (tmp->librarypath, name);
  tmp->next = list;
  list = tmp;
}

/* Inserts the specified DLL at the start of the DLL list */
/* All the other entries are "moved down" by one. */
/* Returns 1 if the library was successfully added */
/* and returns 0 if there library table is full or */
/* or if dlopen fails for some reason. */

static int AddDLL(char *path)
{
  void *handle;
  handle = dlopen(path, RTLD_LAZY|RTLD_GLOBAL);
  if(handle == NULL)
    return 0;
  add_ptr(handle,path);
  return 1;
}

static void RemoveFromHashTable (void *);

/* Remove the specified DLL from the current DLL list */
/* Returns 1 if the DLL was found and removed from */
/* the list and returns 0 otherwise. */

static int DeleteDLL(char *path)
{
  struct libhandlelist *ptr, *oldptr=NULL;
  ptr = list;
  while (ptr != NULL)
    {
      if (strcmp (ptr->librarypath, path) == 0)
	{
	  RemoveFromHashTable (ptr->ptr);
	  dlclose (ptr->ptr);
	  free (ptr->librarypath);
	  if (list == ptr)
	    list = ptr->next;
	  else
	    oldptr->next = ptr->next;
	  free (ptr);
	  return 1;
	}
      oldptr = ptr;
      ptr = ptr->next;
    }
  return 0;
}

typedef struct HashTabElem
  {
    CFunTabEntry data;
    struct HashTabElem *next;
    void *libraryhandle;
  }
HashTabElem;


/* HashTable stores name - pointer pairs with chaining. Sometimes      */
/* the hashtable  will be expanded and reorganized. The implementation */
/* is entirely elementary.  Possible sizes of the table are 2^p+1      */
/* where p is a positive integer.                                      */

static HashTabElem **HashTable;
static int HASHSIZE;
static int NumberElem;


static int 
HashCode (char *symbol)
{
  unsigned int code = 0;
  char *p = symbol;

  while (*p)
    code = 8 * code + *p++;
  return code % HASHSIZE;
}


static void 
HashInstall (char *name, DL_FUNC func, void *libhandle)
{
  int key;
  HashTabElem *newptr;
  newptr = (HashTabElem *) malloc (sizeof (HashTabElem));
  (newptr->data).name = (char *) malloc (strlen (name) + 1);
  strcpy ((newptr->data).name, name);
  (newptr->data).func = func;
  newptr->libraryhandle = libhandle;
  NumberElem++;
  key = HashCode (name);
  newptr->next = HashTable[key];
  HashTable[key] = newptr;
}


static void 
HashExpand ()
{
  int oldsize;
  int i;
  HashTabElem **OldTable;
  HashTabElem *ptr, *newptr;
  oldsize = HASHSIZE;
  OldTable = HashTable;
  HASHSIZE = 2 * HASHSIZE - 1;
  NumberElem = 0;
  HashTable = (HashTabElem **) malloc (HASHSIZE *
				       sizeof (HashTabElem *));
  for (i = 0; i < HASHSIZE; i++)
    HashTable[i] = NULL;

  for (i = 0; i < oldsize; i++)
    {
      ptr = OldTable[i];
      while (ptr != NULL)
	{
	  HashInstall (ptr->data.name, ptr->data.func, ptr->libraryhandle);
	  newptr = ptr->next;
	  free (ptr->data.name);
	  free (ptr);
	  ptr = newptr;
	}
    }
  free (OldTable);
}

static DL_FUNC 
HashLookup (char *symbol)
{
  int key;
  HashTabElem *ptr;
  key = HashCode (symbol);
  ptr = HashTable[key];
  while (ptr != NULL)
    {
      if (strcmp (symbol, (ptr->data).name) == 0)
	return (ptr->data).func;
      ptr = ptr->next;
    }
  return NULL;
}


static void 
RemoveFromHashTable (void *handle)
{
  int key;
  HashTabElem *ptr, *oldptr=NULL;
  for (key = 0; key < HASHSIZE; key++)
    {
      ptr = HashTable[key];
      while (ptr != NULL)
	{
	  if (ptr->libraryhandle == handle)
	    {
	      if (HashTable[key] == ptr)
		{
		  HashTable[key] = ptr->next;
		  free ((ptr->data).name);
		  free (ptr);
		  ptr = HashTable[key];
		}
	      else
		{
		  oldptr->next = ptr->next;
		  free ((ptr->data).name);
		  free (ptr);
		  ptr = oldptr->next;
		}
	    }
	  else
	    {
	      oldptr = ptr;
	      ptr = ptr->next;
	    }
	}
    }
}



/* findDynProc checks whether one of the libraries    */
/* that have been loaded contains the symbol name and */
/* returns a pointer to that symbol and the library   */ 
/* handle upon success.                               */


DL_FUNC 
findDynProc (char const *name, void **handle)
{
  struct libhandlelist *tmp;
  DL_FUNC fcnptr;
  for (tmp = list; tmp != NULL; tmp = tmp->next)
    {
      /* The following line is not legal ANSI C. */
      /* It is only meant to be used in systems supporting */
      /* the dlopen() interface, in which systems data and  */
      /* function pointers _are_ the same size and _can_   */
      /* be cast without loss of information.              */
      fcnptr = (DL_FUNC) dlsym (tmp->ptr, name);
      if (fcnptr != NULL)
	{
	  *handle = tmp->ptr;
	  return fcnptr;
	}
    }
  return NULL;
}


DL_FUNC R_FindSymbol(char const *name)
{
  char buf[MAXIDSIZE+1];
  DL_FUNC fcnptr=NULL;
  void *libhandle;
#ifdef HAVE_NO_SYMBOL_UNDERSCORE
  sprintf(buf, "%s", name);
#else
  sprintf(buf, "_%s", name);
#endif
  if (!(fcnptr = HashLookup (buf)))
    {
      if ((fcnptr = findDynProc (buf, &libhandle)))
	{
	  if ((1.0 * NumberElem) / HASHSIZE > 0.5)
	    HashExpand ();
	  HashInstall (buf, fcnptr, libhandle);
	}
    }
  return fcnptr;
}


void 
InitFunctionHashing ()
{
  int n;
  int i, size = 3;
#ifdef OLD
  NaokSymbol = install ("NAOK");
  DupSymbol = install ("DUP");
#endif
  n = sizeof (CFunTab) / sizeof (CFunTabEntry);
  while (size < n/2 )
    size = 2 * size - 1;
  HASHSIZE = size;
  NumberElem = 0;
  HashTable = (HashTabElem **) malloc (HASHSIZE * sizeof (HashTabElem *));
  for (i = 0; i < HASHSIZE; i++)
    HashTable[i] = NULL;
  for (i = 0; CFunTab[i].name; i++)
    HashInstall (CFunTab[i].name, CFunTab[i].func, NULL);
  HashExpand ();
}


static void GetFullDLLPath(SEXP call, char *buf, char *path)
{
	if(path[0] != '/') {
		if(!getcwd(buf,2*MAXPATHLEN))
			errorcall(call, "can't get working directory!\n");
		strcat(buf, "/");
		strcat(buf, path);
	}
	else strcpy(buf, path);
}

	/* do_dynload implements the R-Interface for the */
	/* loading of shared libraries */

SEXP do_dynload(SEXP call, SEXP op, SEXP args, SEXP env)
{
	char buf[2*MAXPATHLEN];
	checkArity(op,args);
	if (!isString(CAR(args)) || length(CAR(args)) < 1)
		errorcall(call, "character argument expected\n");
	GetFullDLLPath(call, buf, CHAR(STRING(CAR(args))[0]));
	DeleteDLL(buf);
	if(!AddDLL(buf))
		errorcall(call, "unable to load shared library \"%s\"\n", buf);
	return R_NilValue;
}

SEXP do_dynunload(SEXP call, SEXP op, SEXP args, SEXP env)
{
	char buf[2*MAXPATHLEN];
	checkArity(op,args);
	if (!isString(CAR(args)) || length(CAR(args)) < 1)
		errorcall(call, "character argument expected\n");
	GetFullDLLPath(call, buf, CHAR(STRING(CAR(args))[0]));
	if(!DeleteDLL(buf))
		errorcall(call, "shared library \"%s\" was not loaded\n", buf);
	return R_NilValue;
}

#else

void InitFunctionHashing()
{
#ifdef OLD
        NaokSymbol = install("NAOK");
        DupSymbol = install("DUP");
#endif
}

DL_FUNC R_FindSymbol(char const *name)
{
	int i;
	for(i=0 ; CFunTab[i].name ; i++)
		if(!strcmp(name, CFunTab[i].name))
			return CFunTab[i].func;
	return (DL_FUNC)0;
}

SEXP do_dynload(SEXP call, SEXP op, SEXP args, SEXP env)
{
	error("no dyn.load support in this R version\n");
}

SEXP do_dynunload(SEXP call, SEXP op, SEXP args, SEXP env)
{
	error("no dyn.load support in this R version\n");
}

#endif
