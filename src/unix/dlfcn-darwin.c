/*
Copyright (c) 2002 Jorge Acereda  <jacereda@users.sourceforge.net> &
                   Peter O'Gorman <ogorman@users.sourceforge.net>
                   
Portions may be copyright others, see the AUTHORS file included with this
distribution.                  

Maintained by Peter O'Gorman <ogorman@users.sourceforge.net>

Bug Reports and other queries should go to <ogorman@users.sourceforge.net>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/* AUTHORS file:

Original code by Jorge Acereda  <jacereda@users.sourceforge.net>
This was heavily modified by Peter O'Gorman
<ogorman@users.sourceforge.net>

With input from (in alphabetical order):

Max Horn <max@quendi.de>
Francis James Franklin <fjf@alinameridon.com>
Darin Ohashi <DOhashi@maplesoft.com>

Forgive me if I missed you, and e-mail me
<ogorman@users.sourceforge.net>
to get added to this list.

*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifdef __APPLE_CC__


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdarg.h>
#include <limits.h>
#include <mach-o/dyld.h>
#include <mach-o/nlist.h>
#include "dlfcn-darwin.h"

/* This is not available on 10.1 */
#ifndef LC_LOAD_WEAK_DYLIB
#define	LC_LOAD_WEAK_DYLIB (0x18 | LC_REQ_DYLD)
#endif

/* With this stuff here, this thing may actually compile/run on 10.0 systems
 * Not that I have a 10.0 system to test it on anylonger
 */
#ifndef LC_REQ_DYLD
#define LC_REQ_DYLD 0x80000000
#endif
#ifndef NSADDIMAGE_OPTION_RETURN_ONLY_IF_LOADED
#define NSADDIMAGE_OPTION_RETURN_ONLY_IF_LOADED 0x4
#endif
#ifndef NSADDIMAGE_OPTION_RETURN_ON_ERROR
#define NSADDIMAGE_OPTION_RETURN_ON_ERROR 0x1
#endif
#ifndef NSLOOKUPSYMBOLINIMAGE_OPTION_BIND
#define NSLOOKUPSYMBOLINIMAGE_OPTION_BIND 0x0
#endif
#ifndef NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR
#define NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR 0x4
#endif
/* These symbols will be looked for in dyld */
static const struct mach_header *(*dyld_NSAddImage) (const char *, unsigned long) = 0;
static int (*dyld_NSIsSymbolNameDefinedInImage) (const struct mach_header *, const char *) = 0;
static NSSymbol(*dyld_NSLookupSymbolInImage)
	(const struct mach_header *, const char *, unsigned long) = 0;


/* Define this to make dlcompat reuse data block. This way in theory we save
 * a little bit of overhead. However we then couldn't correctly catch excess
 * calls to dlclose(). Hence we don't use this feature
 */
#undef REUSE_STATUS

/* Size of the internal error message buffer (used by dlerror()) */
#define ERR_STR_LEN			256

/* Maximum number of search paths supported by getSearchPath */
#define MAX_SEARCH_PATHS	32


#define MAGIC_DYLIB_OFI ((NSObjectFileImage) 'DYOF')
#define MAGIC_DYLIB_MOD ((NSModule) 'DYMO')

/* internal flags */
#define DL_IN_LIST 0x01

/* This is our central data structure. Whenever a module is loaded via
 * dlopen(), we create such a struct.
 */
struct dlstatus
{
	struct dlstatus *next;		/* pointer to next element in the linked list */
	NSModule module;
	const struct mach_header *lib;
	int refs;					/* reference count */
	int mode;					/* mode in which this module was loaded */
	dev_t device;
	ino_t inode;
	int flags;					/* Any internal flags we may need */
};

/* Head node of the dlstatus list */
static struct dlstatus mainStatus = { 0, MAGIC_DYLIB_MOD, NULL, -1, RTLD_GLOBAL, 0, 0, 0 };
static struct dlstatus *stqueue = &mainStatus;


/* Storage for the last error message (used by dlerror()) */
static char err_str[ERR_STR_LEN];
static int err_filled = 0;


/* Prototypes to internal functions */
static void debug(const char *fmt, ...);
static void darwin_error(const char *str, ...);
static const char *safegetenv(const char *s);
static const char *searchList(void);
static const char *getSearchPath(int i);
static const char *getFullPath(int i, const char *file);
static const struct stat *findFile(const char *file, const char **fullPath);
static int isValidStatus(struct dlstatus *status);
static inline int isFlagSet(int mode, int flag);
static struct dlstatus *lookupStatus(const struct stat *sbuf);
static void insertStatus(struct dlstatus *dls, const struct stat *sbuf);
static int promoteLocalToGlobal(struct dlstatus *dls);
static void *reference(struct dlstatus *dls, int mode);
static void *dlsymIntern(struct dlstatus *dls, const char *symbol, int canSetError);
static struct dlstatus *allocStatus(void);
static struct dlstatus *loadModule(const char *path, const struct stat *sbuf, int mode);
static NSSymbol *search_linked_libs(const struct mach_header *mh, const char *symbol);
static char *get_lib_name(const struct mach_header *mh);
static const struct mach_header *get_mach_header_from_NSModule(NSModule * mod);
/* Two Global Functions */
void *dlsym_prepend_underscore(void *handle, const char *symbol);
void *dlsym_auto_underscore(void *handle, const char *symbol);

/* Functions */

static void debug(const char *fmt, ...)
{
#if DEBUG > 1
	va_list arg;
	va_start(arg, fmt);
	fprintf(stderr, "DLDEBUG: ");
	vfprintf(stderr, fmt, arg);
	fprintf(stderr, "\n");
	fflush(stderr);
	va_end(arg);
#endif
}

static void darwin_error(const char *str, ...)
{
	va_list arg;
	va_start(arg, str);
	strncpy(err_str, "dlcompat: ", ERR_STR_LEN);
	vsnprintf(err_str + 10, ERR_STR_LEN - 10, str, arg);
	va_end(arg);
	debug("ERROR: %s\n", err_str);
	err_filled = 1;
}

static void warning(const char *str)
{
#if DEBUG > 0
	fprintf(stderr, "WARNING: dlcompat: %s\n", str);
#endif
}

static const char *safegetenv(const char *s)
{
	const char *ss = getenv(s);
	return ss ? ss : "";
}

/* because this is only used for debugging and error reporting functions, we
 * don't really care about how elegant it is... it could use the load
 * commands to find the install name of the library, but...
 */
static char *get_lib_name(const struct mach_header *mh)
{
	unsigned long count = _dyld_image_count();
	unsigned long i;
	char *val = NULL;
	if (mh)
	{
		for (i = 0; i < count; i++)
		{
			if (mh == _dyld_get_image_header(i))
			{
				val = _dyld_get_image_name(i);
				break;
			}
		}
	}
	return val;
}

/* Returns the mach_header for the module bu going through all the loaded images
 * and finding the one with the same name as the module. There really ought to be
 * an api for doing this, would be faster, but there isn't one right now
 */
static const struct mach_header *get_mach_header_from_NSModule(NSModule * mod)
{
	const char *mod_name = NSNameOfModule(mod);
	struct mach_header *mh = NULL;
	unsigned long count = _dyld_image_count();
	unsigned long i;
	debug("Module name: %s", mod_name);
	for (i = 0; i < count; i++)
	{
		if (!strcmp(mod_name, _dyld_get_image_name(i)))
		{
			mh = _dyld_get_image_header(i);
			break;
		}
	}
	return mh;
}


/* Compute and return a list of all directories that we should search when
 * trying to locate a module. We first look at the values of LD_LIBRARY_PATH
 * and DYLD_LIBRARY_PATH, and then finally fall back to looking into
 * /usr/lib and /lib. Since both of the environments variables can contain a
 * list of colon seperated paths, we simply concat them and the two other paths
 * into one big string, which we then can easily parse.
 * Splitting this string into the actual path list is done by getSearchPath()
 */
static const char *searchList()
{
	size_t buf_size;
	char *buf;
	const char *ldlp = safegetenv("LD_LIBRARY_PATH");
	const char *dyldlp = safegetenv("DYLD_LIBRARY_PATH");
	char *stdpath = getenv("DYLD_FALLBACK_LIBRARY_PATH");
	if (!stdpath)
		stdpath = "/usr/local/lib:/lib:/usr/lib";
	buf_size = strlen(ldlp) + strlen(dyldlp) + strlen(stdpath) + 1;
	buf = malloc(buf_size);
	snprintf(buf, buf_size, "%s%s%s%s%s", dyldlp, (dyldlp[0] ? ":" : ""), ldlp, (ldlp[0] ? ":" : ""),
			 stdpath);
	return buf;
}

/* Returns the ith search path from the list as computed by searchList() */
static const char *getSearchPath(int i)
{
	static const char *list = 0;
	static const char *path[MAX_SEARCH_PATHS] = { 0 };
	static int end = 0;
	if (!list && !end)
		list = searchList();
	while (!path[i] && !end)
	{
		path[i] = strsep((char **)&list, ":");
		if (path[i][0] == 0)
			path[i] = 0;
		end = list == 0;
	}
	return path[i];
}

static const char *getFullPath(int i, const char *file)
{
	static char buf[PATH_MAX];
	const char *path = getSearchPath(i);
	if (path)
		snprintf(buf, PATH_MAX, "%s/%s", path, file);
	return path ? buf : 0;
}

/* Given a file name, try to determine the full path for that file. Starts
 * its search in the current directory, and then tries all paths in the 
 * search list in the order they are specified there.
 */
static const struct stat *findFile(const char *file, const char **fullPath)
{
	int i = 0;
	static struct stat sbuf;
	debug("finding file %s", file);
	*fullPath = file;
	do
	{
		if (0 == stat(*fullPath, &sbuf))
			return &sbuf;
	}
	while ((*fullPath = getFullPath(i++, file)));
	return 0;
}

/* Determine whether a given dlstatus is valid or not */
static int isValidStatus(struct dlstatus *status)
{
	/* Walk the list to verify status is contained in it */
	struct dlstatus *dls = stqueue;
	while (dls && status != dls)
		dls = dls->next;

	if (dls == 0)
		darwin_error("invalid handle");
	else if (dls->module == 0)
		darwin_error("handle to closed library");
	else
		return TRUE;
	return FALSE;
}

static inline int isFlagSet(int mode, int flag)
{
	return (mode & flag) == flag;
}

static struct dlstatus *lookupStatus(const struct stat *sbuf)
{
	struct dlstatus *dls = stqueue;
	debug("looking for status");
	while (dls && ( /* isFlagSet(dls->mode, RTLD_UNSHARED) */ 0
				   || sbuf->st_dev != dls->device || sbuf->st_ino != dls->inode))
		dls = dls->next;
	return dls;
}

static void insertStatus(struct dlstatus *dls, const struct stat *sbuf)
{
	debug("inserting status");
	dls->inode = sbuf->st_ino;
	dls->device = sbuf->st_dev;
	dls->refs = 0;
	dls->mode = 0;
	if ((dls->flags & DL_IN_LIST) == 0)
	{
		dls->next = stqueue;
		stqueue = dls;
		dls->flags |= DL_IN_LIST;
	}
}

static struct dlstatus *allocStatus()
{
	struct dlstatus *dls;
#ifdef REUSE_STATUS
	dls = stqueue;
	while (dls && dls->module)
		dls = dls->next;
	if (!dls)
#endif
		dls = malloc(sizeof(*dls));
	dls->flags = 0;
	return dls;
}

static int promoteLocalToGlobal(struct dlstatus *dls)
{
	static int (*p) (NSModule module) = 0;
	debug("promoting");
	if (!p)
		_dyld_func_lookup("__dyld_NSMakePrivateModulePublic", (unsigned long *)&p);
	return (dls->module == MAGIC_DYLIB_MOD) || (p && p(dls->module));
}

static void *reference(struct dlstatus *dls, int mode)
{
	if (dls)
	{
		if (dls->module == MAGIC_DYLIB_MOD && isFlagSet(mode, RTLD_LOCAL))
		{
			warning("trying to open a .dylib with RTLD_LOCAL");
			darwin_error("unable to open a .dylib with RTLD_LOCAL");
			return NULL;
		}
		if (isFlagSet(mode, RTLD_GLOBAL) &&
			!isFlagSet(dls->mode, RTLD_GLOBAL) && !promoteLocalToGlobal(dls))
		{
			darwin_error("unable to promote local module to global");
			return NULL;
		}
		dls->mode |= mode;
		dls->refs++;
	}
	else
		debug("reference called with NULL argument");

	return dls;
}

/*
dyld adds libraries by first adding the directly dependant libraries in link order, and
then adding the dependencies for those libraries, so we should do the same... but we don't
bother adding the extra dependencies, if the symbols are neither in the loaded image nor
any of it's direct dependencies, then it probably isn't there.
*/
NSSymbol *search_linked_libs(const struct mach_header * mh, const char *symbol)
{
	int n;
	struct load_command *lc = 0;
	struct mach_header *wh;
	NSSymbol *nssym = 0;
	if (dyld_NSAddImage && dyld_NSIsSymbolNameDefinedInImage && dyld_NSLookupSymbolInImage)
	{
		lc = (struct load_command *)((char *)mh + sizeof(struct mach_header));
		for (n = 0; n < mh->ncmds; n++, lc = (struct load_command *)((char *)lc + lc->cmdsize))
		{
			if ((LC_LOAD_DYLIB == lc->cmd) || (LC_LOAD_WEAK_DYLIB == lc->cmd))
			{
				if ((wh = (struct mach_header *)
					 dyld_NSAddImage((char *)(((struct dylib_command *)lc)->dylib.name.offset +
											  (char *)lc),
									 NSADDIMAGE_OPTION_RETURN_ONLY_IF_LOADED |
									 NSADDIMAGE_OPTION_RETURN_ON_ERROR)))
				{
					if (dyld_NSIsSymbolNameDefinedInImage(wh, symbol))
					{
						nssym = dyld_NSLookupSymbolInImage(wh,
														   symbol,
														   NSLOOKUPSYMBOLINIMAGE_OPTION_BIND
														   |
														   NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR);
						break;
					}
				}
			}
		}
	}
	return nssym;
}


static void *dlsymIntern(struct dlstatus *dls, const char *symbol, int canSetError)
{
	NSSymbol *nssym = 0;
	if (dls->module != MAGIC_DYLIB_MOD)
	{
		nssym = NSLookupSymbolInModule(dls->module, symbol);
		if (!nssym && NSIsSymbolNameDefined(symbol))
		{
			debug("Searching dependencies");
			nssym = search_linked_libs(get_mach_header_from_NSModule(dls->module), symbol);
		}
	}
	else if (dls->lib && dyld_NSIsSymbolNameDefinedInImage && dyld_NSLookupSymbolInImage)
	{
		if (dyld_NSIsSymbolNameDefinedInImage(dls->lib, symbol))
		{
			nssym = dyld_NSLookupSymbolInImage(dls->lib,
											   symbol,
											   NSLOOKUPSYMBOLINIMAGE_OPTION_BIND
											   | NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR);
		}
		else if (NSIsSymbolNameDefined(symbol))
		{
			debug("Searching dependencies");
			nssym = search_linked_libs(dls->lib, symbol);
		}
	}
	else if (dls->module == MAGIC_DYLIB_MOD)
	{
		/* Global context, use NSLookupAndBindSymbol */
		if (NSIsSymbolNameDefined(symbol))
		{
			/* There doesn't seem to be a return on error option for this call???
			   this is potentially broken, if binding fails, it will improperly
			   exit the application. */
			nssym = NSLookupAndBindSymbol(symbol);
		}
	}
	/* Error reporting */
	if (!nssym)
	{
		const char *name;
		NSLinkEditErrors dylder;
		int dylderno;
		const char *dylderrstr;
		const char *dyldfile;
		NSLinkEditError(&dylder, &dylderno, &dyldfile, &dylderrstr);
		if (!dylderrstr || !strlen(dylderrstr))
		{
			if (dls->module != MAGIC_DYLIB_MOD)
				name = NSNameOfModule(dls->module);
			else
			{
				if (!dls->lib)
				{
					name = "global context";
				}
				else
				{
					name = get_lib_name(dls->lib);
				}
			}
			if (canSetError)
			{
				darwin_error("unable to find symbol \"%s\" in \"%s\"", symbol, name);
			}
			else
			{
				debug("Symbol \"%s\" not in lib/module \"%s\"", symbol, name);
			}
		}
		else
		{
			if (canSetError)
			{
				darwin_error(dylderrstr);
			}
			else
			{
				debug(dylderrstr);
			}
		}
		return NULL;
	}

	return NSAddressOfSymbol(nssym);
}

static struct dlstatus *loadModule(const char *path, const struct stat *sbuf, int mode)
{
	NSObjectFileImage ofi = 0;
	NSObjectFileImageReturnCode ofirc;
	struct dlstatus *dls;
	NSLinkEditErrors ler;
	int lerno;
	const char *errstr;
	const char *file;
	void (*init) (void);

	ofirc = NSCreateObjectFileImageFromFile(path, &ofi);
	switch (ofirc)
	{
		case NSObjectFileImageSuccess:
			break;
		case NSObjectFileImageInappropriateFile:
			if (dyld_NSAddImage && dyld_NSIsSymbolNameDefinedInImage && dyld_NSLookupSymbolInImage)
			{
				if (isFlagSet(mode, RTLD_LOCAL))
				{
					warning("trying to open a .dylib with RTLD_LOCAL");
					darwin_error("unable to open this file with RTLD_LOCAL");
					return NULL;
				}
			}
			else
			{
				darwin_error("opening this file is unsupported on this system");
				return NULL;
			}
			break;
		case NSObjectFileImageFailure:
			darwin_error("object file setup failure");
			return NULL;
		case NSObjectFileImageArch:
			darwin_error("no object for this architecture");
			return NULL;
		case NSObjectFileImageFormat:
			darwin_error("bad object file format");
			return NULL;
		case NSObjectFileImageAccess:
			darwin_error("can't read object file");
			return NULL;
		default:
			darwin_error("unknown error from NSCreateObjectFileImageFromFile()");
			return NULL;
	}
	dls = lookupStatus(sbuf);
	if (!dls)
	{
		dls = allocStatus();
	}
	if (!dls)
	{
		darwin_error("unable to allocate memory");
		return NULL;
	}
	dls->lib = 0;
	if (ofirc == NSObjectFileImageInappropriateFile)
	{
		if ((dls->lib = dyld_NSAddImage(path, NSADDIMAGE_OPTION_RETURN_ON_ERROR)))
		{
			debug("Dynamic lib loaded at %ld", dls->lib);
			ofi = MAGIC_DYLIB_OFI;
			dls->module = MAGIC_DYLIB_MOD;
			ofirc = NSObjectFileImageSuccess;
			/* Although it is possible with a bit of work to modify this so it works and
			   functions with RTLD_NOW, I don't deem it necessary at the moment */
		}
	}
	else
	{
		dls->module = NSLinkModule(ofi, path,
								   NSLINKMODULE_OPTION_RETURN_ON_ERROR
								   | NSLINKMODULE_OPTION_PRIVATE
								   | (isFlagSet(mode, RTLD_NOW) ? NSLINKMODULE_OPTION_BINDNOW : 0));
		NSDestroyObjectFileImage(ofi);
	}
	if (!dls->module)
	{
		NSLinkEditError(&ler, &lerno, &file, &errstr);
		free(dls);
		darwin_error(errstr);
		return NULL;
	}
	insertStatus(dls, sbuf);
	if ((init = dlsymIntern(dls, "__init", 0)))
	{
		debug("calling _init()");
		init();
	}

	return dls;
}

void *dlopen(const char *path, int mode)
{
	const struct stat *sbuf;
	struct dlstatus *dls;
	const char *fullPath;
	static int inited = 0;
	if (!inited)
	{
		inited = 1;
		_dyld_func_lookup("__dyld_NSAddImage", (unsigned long *)&dyld_NSAddImage);
		_dyld_func_lookup("__dyld_NSIsSymbolNameDefinedInImage",
						  (unsigned long *)&dyld_NSIsSymbolNameDefinedInImage);
		_dyld_func_lookup("__dyld_NSLookupSymbolInImage", (unsigned long *)&dyld_NSLookupSymbolInImage);
	}
	if (!path)
	{
		return &mainStatus;
	}
	if (!(sbuf = findFile(path, &fullPath)))
	{
		darwin_error("file \"%s\" not found", path);
		return NULL;
	}
	/* Now checks that it hasn't been closed already */
	if ((dls = lookupStatus(sbuf)) && (dls->refs > 0))
	{
		/* debug("status found"); */
		return reference(dls, mode);
	}
	if (isFlagSet(mode, RTLD_NOLOAD))
	{
		darwin_error("no existing handle and RTLD_NOLOAD specified");
		return NULL;
	}
	if (isFlagSet(mode, RTLD_LAZY) && isFlagSet(mode, RTLD_NOW))
	{
		darwin_error("how can I load something both RTLD_LAZY and RTLD_NOW?");
		return NULL;
	}
	return reference(loadModule(fullPath, sbuf, mode), mode);
}

#define FINK_BUILD 1

#if FINK_BUILD
void *dlsym_prepend_underscore(void *handle, const char *symbol)
#else
void *dlsym(void *handle, const char *symbol)
#endif
{
/*
 	A quick and easy way for porting packages which call dlsym(handle,"sym")
	If the porter adds -Ddlsym=dlsym_prepend_underscore to the CFLAGS then
	this function will be called, and will add the required underscore.
	
	Note that I haven't figured out yet which should be "standard", prepend
	the underscore always, or not at all. These global functions need to go away
	for opendarwin.
*/
	static char undersym[257];
	int sym_len = strlen(symbol);
	void *value = NULL;
	char *malloc_sym = NULL;
	struct dlstatus *dls = handle;

	if (!isValidStatus(dls))
		return NULL;

	if (sym_len < 256)
	{
		snprintf(undersym, 256, "_%s", symbol);
		value = dlsymIntern(handle, undersym, 1);
	}
	else
	{
		malloc_sym = malloc(sym_len + 2);
		if (malloc_sym)
		{
			sprintf(malloc_sym, "_%s", symbol);
			value = dlsymIntern(handle, malloc_sym, 1);
			free(malloc_sym);
		}
		else
		{
			darwin_error("Unable to allocate memory");
		}
	}
	return value;
}

#if FINK_BUILD
void *dlsym_auto_underscore(void *handle, const char *symbol)
{
	struct dlstatus *dls = handle;
	void *addr = 0;
	if (!isValidStatus(dls))
		return NULL;
	addr = dlsymIntern(dls, symbol, 0);
	if (!addr)
		addr = dlsym_prepend_underscore(handle, symbol);
	return addr;
}


void *dlsym(void *handle, const char *symbol)
{
	struct dlstatus *dls = handle;
	void *addr = 0;

	if (!isValidStatus(dls))
		return NULL;
	addr = dlsymIntern(dls, symbol, 1);
	return addr;
}
#endif

int dlclose(void *handle)
{
	struct dlstatus *dls = handle;
	if (!isValidStatus(dls))
		return 1;
	if (dls->module == MAGIC_DYLIB_MOD)
	{
		warning("trying to close a .dylib!");
		darwin_error("dynamic libraries cannot be closed");
		return 1;
	}
	if (!dls->module)
	{
		darwin_error("module already closed");
		return 1;
	}
	dls->refs--;
	if (!dls->refs)
	{
		unsigned long options = 0;
		void (*fini) (void);
		if ((fini = dlsymIntern(dls, "__fini", 0)))
		{
			debug("calling _fini()");
			fini();
		}
		if (isFlagSet(dls->mode, RTLD_NODELETE))
			options |= NSUNLINKMODULE_OPTION_KEEP_MEMORY_MAPPED;
		if (!NSUnLinkModule(dls->module, options))
		{
			darwin_error("unable to unlink module");
			return 1;
		}
		dls->module = 0;
		/* Note: the dlstatus struct dls is neither removed from the list
		 * nor is the memory it occupies freed. This shouldn't pose a 
		 * problem in mostly all cases, though.
		 */
	}
	return 0;
}

const char *dlerror(void)
{
	const char *e = err_filled ? err_str : 0;
	err_filled = 0;
	return e;
}

int dladdr(void *p, Dl_info * info)
{
	unsigned long i;
	unsigned long j;
	unsigned long count = _dyld_image_count();
	struct mach_header *mh = 0;
	struct load_command *lc = 0;
	unsigned long addr = NULL;
	int found = 0;
	if (!info)
		return 0;
	info->dli_fname = 0;
	info->dli_fbase = 0;
	info->dli_sname = 0;
	info->dli_saddr = 0;
/* Some of this was swiped from code posted by Douglas Davidson <ddavidso AT apple DOT com>
to darwin-development AT lists DOT apple DOT com and slightly modified
*/
	for (i = 0; i < count; i++)
	{
		addr = (unsigned long)p - _dyld_get_image_vmaddr_slide(i);
		mh = _dyld_get_image_header(i);
		if (mh)
		{
			lc = (struct load_command *)((char *)mh + sizeof(struct mach_header));
			for (j = 0; j < mh->ncmds; j++, lc = (struct load_command *)((char *)lc + lc->cmdsize))
			{
				if (LC_SEGMENT == lc->cmd &&
					addr >= ((struct segment_command *)lc)->vmaddr &&
					addr <
					((struct segment_command *)lc)->vmaddr + ((struct segment_command *)lc)->vmsize)
				{
					info->dli_fname = _dyld_get_image_name(i);
					info->dli_fbase = (void *)mh;
					found = 1;
					break;
				}
			}
			if (found)
				break;
		}
	}
	if (!found)
		return 0;
/* 	Okay, we seem to have found a place for the address, so now, we have to search the symbol table
	for the nearest symbol with an address less than or equal to the passed in address */
	lc = (struct load_command *)((char *)mh + sizeof(struct mach_header));
	for (j = 0; j < mh->ncmds; j++, lc = (struct load_command *)((char *)lc + lc->cmdsize))
	{
		if (LC_SYMTAB == lc->cmd)
		{
			struct nlist *symtable =
				(struct nlist *)(((struct symtab_command *)lc)->symoff + (void *)mh);
			unsigned long numsyms = ((struct symtab_command *)lc)->nsyms;
			struct nlist *nearest = NULL;
			unsigned long diff = 0xffffffff;
			unsigned long strtable = (unsigned long)(((struct symtab_command *)lc)->stroff + (void *)mh);
			for (i = 0; i < numsyms; i++)
			{
				/* Ignore the following kinds of Symbols */
				if ((!symtable->n_value)                /* Undefined */
					|| (symtable->n_type >= N_PEXT)	    /* Debug symbol */
					|| (!(symtable->n_type & N_EXT))	/* Local Symbol */
					)
				{
					symtable++;
					continue;
				}
				if ((addr >= symtable->n_value) && (diff >= (symtable->n_value - addr)))
				{
					diff = symtable->n_value - addr;
					nearest = symtable;
				}
				symtable++;
			}
			if (nearest)
			{
				info->dli_saddr = nearest->n_value + (p - addr);
				info->dli_sname = (char *)(strtable + nearest->n_un.n_strx);
			}
		}
	}
	return 1;
}

#endif /* __APPLE_CC__ */
