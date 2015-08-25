/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2000  The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <dl.h>

#undef DEBUG

#define RTLD_LAZY (BIND_DEFERRED | BIND_NONFATAL)

/* these are added to shut up warnings that cause make check to fail. */
#define RTLD_LOCAL 0
#define RTLD_GLOBAL 0
#define RTLD_NOW 0

#include "hpdlfcn.h"

/*
 * This is a minimal implementation of the ELF dlopen, dlclose, dlsym
 * and dlerror routines based on HP's shl_load, shl_unload and
 * shl_findsym. */

/*
 * Reference Counting.
 *
 * Empirically it looks like the HP routines do not mainain a
 * reference count, so I maintain one here.
 */

typedef struct lib_entry {
  shl_t handle;
  int count;
  struct lib_entry *next;
} *LibEntry;

#define lib_entry_handle(e) ((e)->handle)
#define lib_entry_count(e) ((e)->count)
#define lib_entry_next(e) ((e)->next)
#define set_lib_entry_handle(e,v) ((e)->handle = (v))
#define set_lib_entry_count(e,v) ((e)->count = (v))
#define set_lib_entry_next(e,v) ((e)->next = (v))
#define increment_lib_entry_count(e) ((e)->count++)
#define decrement_lib_entry_count(e) ((e)->count--)

static LibEntry Entries = NULL;

static LibEntry find_lib_entry(shl_t handle)
{
  LibEntry entry;

  for (entry = Entries; entry != NULL; entry = lib_entry_next(entry))
    if (lib_entry_handle(entry) == handle)
      return entry;
  return NULL;
}

static LibEntry new_lib_entry(shl_t handle)
{
  LibEntry entry;

  if ((entry = (LibEntry) malloc(sizeof(struct lib_entry))) != NULL) {
    set_lib_entry_handle(entry, handle);
    set_lib_entry_count(entry, 1);
    set_lib_entry_next(entry, Entries);
    Entries = entry;
  }
  return entry;
}

static void free_lib_entry(LibEntry entry)
{
  if (entry == Entries)
    Entries = lib_entry_next(entry);
  else {
    LibEntry last, next;
    for (last = Entries, next = lib_entry_next(last);
	 next != NULL;
	 last = next, next = lib_entry_next(last)) {
      if (entry == next) {
	set_lib_entry_next(last, lib_entry_next(entry));
	break;
      }
    }
  }
  free(entry);
}


/*
 * Error Handling.
 */

#define ERRBUFSIZE 1000

static char errbuf[ERRBUFSIZE];
static int dlerrno = 0;

char *dlerror(void)
{
  return dlerrno ? errbuf : NULL;
}


/*
 * Opening and Closing Liraries.
 */

void *dlopen(const char *fname, int mode)
{
  shl_t handle;
  LibEntry entry = NULL;

  dlerrno = 0;
  if (fname == NULL)
    handle = PROG_HANDLE;
  else {
    handle = shl_load(fname, mode | BIND_VERBOSE, 0L);
    if (handle != NULL) {
      if ((entry = find_lib_entry(handle)) == NULL) {
	if ((entry = new_lib_entry(handle)) == NULL) {
	  shl_unload(handle);
	  handle = NULL;
	}
      }
      else
	increment_lib_entry_count(entry);
    }
    if (handle == NULL) {
      dlerrno = 1;
      sprintf(errbuf, "can't open %s", fname);
    }
  }
#ifdef DEBUG
  printf("opening library %s, handle = %x, count = %d\n",
	 fname, handle, entry ? lib_entry_count(entry) : -1);
  if (dlerrno) printf("%s\n", dlerror());
#endif
  return (void *) handle;
}

int dlclose(void *handle)
{
  LibEntry entry;
#ifdef DEBUG
  entry = find_lib_entry(handle);
  printf("closing library handle = %x, count = %d\n",
	 handle, entry ? lib_entry_count(entry) : -1);
#endif

  dlerrno = 0;
  if ((shl_t) handle == PROG_HANDLE)
    return 0; /* ignore attempts to close main program */
  else {

    if ((entry = find_lib_entry((shl_t) handle)) != NULL) {
      decrement_lib_entry_count(entry);
      if (lib_entry_count(entry) > 0)
	return 0;
      else {
	/* unload once reference count reaches zero */
	free_lib_entry(entry);
	if (shl_unload((shl_t) handle) == 0)
	  return 0;
      }
    }
    /* if you get to here, an error has occurred */
    dlerrno = 1;
    sprintf(errbuf, "attempt to close library failed");
#ifdef DEBUG
    printf("%s\n", dlerror());
#endif
    return -1;
  }
}


/*
 * Symbol Lookup.
 */

void *dlsym(void *handle, const char *name)
{
  void *f;
  shl_t myhandle;

  dlerrno = 0;
  myhandle = (handle == NULL) ? PROG_HANDLE : (shl_t) handle;

  if (shl_findsym(&myhandle, name, TYPE_PROCEDURE, &f) != 0) {
    dlerrno = 1;
    sprintf(errbuf, "symbol %s not found", name);
#ifdef DEBUG
    printf("symbol %s not found", name);
#endif

    f = NULL;
  }

  return(f);
}
