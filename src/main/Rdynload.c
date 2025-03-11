/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2023 The R Core Team
 *  Copyright (C) 1995-1996 Robert Gentleman and Ross Ihaka
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


/*
  This is an effort to merge the 3 different dynload.c files in the
  distribution from the unix/, macintosh/dll/ and gnuwin32/ directories.
  The aim is to consolidate these different implementations into
      i) a generic or platform-independent common core
     ii) platform-dependent routines that are registered
	 as function pointers.
  The reason for using function pointers rather than explicit
  linking of symbols is
     a) to avoid confusion in the linking
     b) to allow for easily overriding these in embedded applications
	in which a host application needs to control how R finds
	symbols. This may be necessary for security reasons.
 */

/*  Dynamic Loading Support
 *
 *  This module provides support for run-time loading of shared objects
 *  access to symbols within such objects via .C and .Fortran.  This is
 *  done under Unix with dlopen, dlclose and dlsym.
 *  There are two cases:
 *
 *
 *  1. The dlopen interface is available.
 *
 *  In this case all symbol location in packages is done using the
 *  dlopen routines.  We maintain a list of currently loaded shared
 *  objects in an array called "LoadedDLL" with the number of currently
 *  loaded objects being "CountDLL".  To locate a symbol, we probe
 *  the loaded objects in order until the symbol is located.  If we
 *  do not find a symbol in the loaded objects, we search the
 *  executable itself.  This search is not very efficient, but this
 *  probably pales into insignificance when compared with the
 *  inefficiencies in the R interpreter.
 *
 *  Loading and unloading of shared objects is done via the routines
 *  AddDLL and DeleteDLL.  These routines maintain the list of
 *  currently loaded objects.  When an object is added, any existing
 *  reference to that object is deleted and then the object is
 *  inserted at the start of the search list.  This way, symbols in
 *  more recently loaded objects are found first.
 *
 *
 *  2. Accessing native routines in base (the R executable).
 *
 *  In this case, we use the registration mechanism and the DllInfo array
 *  in ../main/Rdynload.c to locate functions in the executable. We do this
 *  by straight linear search through the table.
 *  Note that the base routines registered are listed in
 *               ../main/registration.c
 *  and are registered during the initialization of the R engine.
 *
 *
 *  If speed is ever an issue in the lookup of registered symbols, we can
 *  store the registered routines in a hashtable or binary tree as they
 *  are being registered.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>

#include <string.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <Rmath.h>
#include <Rdynpriv.h>

#ifdef Unix
# ifdef HAVE_DLFCN_H
#  define HAVE_DYNAMIC_LOADING
# endif
# ifndef HAVE_NO_SYMBOL_UNDERSCORE
#  ifdef HAVE_ELF_H
#   define HAVE_NO_SYMBOL_UNDERSCORE
#  endif /* HAVE_ELF_H */
# endif /* HAVE_NO_SYMBOL_UNDERSCORE */
#endif

#ifdef Win32
# define HAVE_DYNAMIC_LOADING
#endif


#ifdef CACHE_DLL_SYM  /* Used on Windows */
#define MAX_CACHE	100
/* keep a record of symbols that have been found, about 70 bytes each */
R_CPFun CPFun[MAX_CACHE];
int nCPFun = 0;
#endif

static int MaxNumDLLs = 0; /* initialized in initLoadedDLL */

static int CountDLL = 0;

#include <R_ext/Rdynload.h>

/* Allocated in initLoadedDLL at R session start. Never free'd */
static DllInfo** LoadedDLL = NULL;
static SEXP DLLInfoEptrs = NULL; /* cache of external pointers to DllInfo */
static SEXP SymbolEptrs = NULL;  /* (weak) list of symbol external pointers */

static int addDLL(char *dpath, char *name, HINSTANCE handle);
static SEXP Rf_MakeDLLInfo(DllInfo *info);

static SEXP createRSymbolObject(SEXP sname, DL_FUNC f,
				R_RegisteredNativeSymbol *symbol,
				bool withRegistrationInfo);

static DllInfo *R_RegisterDLL(HINSTANCE handle, const char *path);

attribute_hidden OSDynSymbol Rf_osDynSymbol;
attribute_hidden OSDynSymbol *R_osDynSymbol = &Rf_osDynSymbol;

void R_init_base(DllInfo *); /* In Registration.c */

static void initLoadedDLL(void);

attribute_hidden void
InitDynload(void)
{
    initLoadedDLL();
    int which = addDLL(Rstrdup("base"), "base", NULL);
    DllInfo *dll = LoadedDLL[which];
    R_init_base(dll);
    InitFunctionHashing();
}

/* Allocate LoadedDLL. Errors are reported via R_Suicide, because this is
   called too early during startup to use error(.) */
static void initLoadedDLL(void)
{
    if (CountDLL != 0 || LoadedDLL != NULL)
	R_Suicide("DLL table corruption detected"); /* not translated */

    /* Note that it is likely that dlopen will use up at least one file
       descriptor for each DLL loaded (it may load further dynamically
       linked libraries), so we do not want to get close to the fd limit
       (which may be as low as 256).

       When R_MAX_NUM_DLLS environment variable is set and is in range
       [100,1000] and the fd limit is sufficient or can be increased,
       this becomes the maximum number of DLLs. Otherwise, R fails to start.

       When R_MAX_NUM_DLLS is not set, R uses a reasonable default value
       that matches the fd limit. R attempts to increase the limit if it
       is too small. The goal for maximum number of DLLs is currently 614.

       The limit receives increased attention with 'workflow'
       documents which load increasingly more packages, hitting the
       default fd limit of 256 on macOS systems.
    */

    char *req = getenv("R_MAX_NUM_DLLS");
    if (req != NULL) {
	/* set exactly the requested limit, or fail */
	int reqlimit = atoi(req);
	if (reqlimit < 100) {
	    char msg[128];
	    snprintf(msg, 128,
	      _("R_MAX_NUM_DLLS must be at least %d"), 100);
	    R_Suicide(msg);
	}
	if (reqlimit > 1000) {
	    char msg[128];
	    snprintf(msg, 128,
	      _("R_MAX_NUM_DLLS cannot be bigger than %d"), 1000);
	    R_Suicide(msg);
	}
	int needed_fds = (int)ceil(reqlimit / 0.6);
	int fdlimit = R_EnsureFDLimit(needed_fds);
	if (fdlimit < 0 && reqlimit > 100) {
	    /* this is very unlikely */
	    char msg[128];
	    snprintf(msg, 128,
	      _("R_MAX_NUM_DLLS cannot be bigger than %d when fd limit is not known"),
	      100);
	    R_Suicide(msg);
	} else if (fdlimit >= 0 && fdlimit < needed_fds) {
	    int maxdlllimit = (int) (0.6 * fdlimit);
	    if (maxdlllimit < 100)
		R_Suicide(_("the limit on the number of open files is too low"));
	    char msg[128];
	    snprintf(msg, 128,
	      _("R_MAX_NUM_DLLS bigger than %d may exhaust open files limit"),
	      maxdlllimit);
	    R_Suicide(msg);
	}
	/* when fdlimit == -1 (not known), currently only reqlimit of 100 is
	   allowed */
	MaxNumDLLs = reqlimit;
    } else {
	/* set a reasonable default limit */
	int needed_fds = 1024;
	int fdlimit = R_EnsureFDLimit(needed_fds);
	if (fdlimit < 0)
	    MaxNumDLLs = 100;
	else {
	    MaxNumDLLs = (int) (0.6 * fdlimit);
	    if (MaxNumDLLs < 100)
		R_Suicide(_("the limit on the number of open files is too low"));
	}
    }

    /* memory is set to zero */
    LoadedDLL = (DllInfo **) calloc(MaxNumDLLs, sizeof(DllInfo*));
    if (LoadedDLL == NULL)
	R_Suicide(_("could not allocate space for DLL table"));
    DLLInfoEptrs = allocVector(VECSXP, MaxNumDLLs);
    R_PreserveObject(DLLInfoEptrs);
    SymbolEptrs = CONS(R_NilValue, R_NilValue);
    R_PreserveObject(SymbolEptrs);
}

#define MAXCOUNT 10
static void
R_registerSymbolEptr(SEXP eptr, SEXP einfo)
{
    static int cleancount = MAXCOUNT;

    /* remove unneeded entries from the list */
    if(--cleancount <= 0) {
	cleancount = MAXCOUNT;
	for (SEXP last = SymbolEptrs, next = CDR(SymbolEptrs);
	     next != R_NilValue;
	     next = CDR(next))
	    if (R_WeakRefKey(CAR(next)) == R_NilValue)
		SETCDR(last, CDR(next));
	    else
		last = next;
    }

    /* Add eptr to the head of the list, with weakref value identifying the
       DllInfo. The identification is needed when deleting the DLL. */
    SETCDR(SymbolEptrs,
           CONS(R_MakeWeakRef(eptr, einfo, R_NilValue, FALSE),
                CDR(SymbolEptrs)));
}

/* returns DllInfo used by the embedding application.
   the underlying "(embedding)" entry is created if not present */
DllInfo *R_getEmbeddingDllInfo(void)
{
    DllInfo *dll = R_getDllInfo("(embedding)");
    if (dll == NULL) {
	int which = addDLL(Rstrdup("(embedding)"), "(embedding)", NULL);
	dll = LoadedDLL[which];
	/* make sure we don't attempt dynamic lookup */
	R_useDynamicSymbols(dll, FALSE);
    }
    return dll;
}

// API, in header R_ext/Rdynload.h
Rboolean R_useDynamicSymbols(DllInfo *info, Rboolean value)
{
    Rboolean old;
    old = info->useDynamicLookup;
    info->useDynamicLookup = value;
    return old;
}

// API, in header R_ext/Rdynload.h
Rboolean R_forceSymbols(DllInfo *info, Rboolean value)
{
    Rboolean old;
    old = info->forceSymbols;
    info->forceSymbols = value;
    return old;
}

static void
R_addCRoutine(DllInfo *info, const R_CMethodDef * const croutine,
	      Rf_DotCSymbol *sym);
static void
R_addCallRoutine(DllInfo *info,
		 const R_CallMethodDef * const croutine,
		 Rf_DotCallSymbol *sym);
static void
R_addFortranRoutine(DllInfo *info,
		    const R_FortranMethodDef * const croutine,
		    Rf_DotFortranSymbol *sym);
static void
R_addExternalRoutine(DllInfo *info,
		     const R_ExternalMethodDef * const croutine,
		     Rf_DotExternalSymbol *sym);


/*
 Returns a reference to the DllInfo object associated with the shared object
 with the path name `path'. This ensures uniqueness rather than having the
 undesirable situation of two objects with the same name but in different
 directories.
 This is available so that it can be called from arbitrary C routines
 that need to call R_registerRoutines(). The initialization routine
 R_init_<object name> is passed the DllInfo reference as an argument.
 Other routines must explicitly request it using this routine.
 */
attribute_hidden DllInfo *
R_getDllInfo(const char *path)
{
    int i;
    for(i = 0; i < CountDLL; i++) {
	if(strcmp(LoadedDLL[i]->path, path) == 0) return(LoadedDLL[i]);
    }
    return (DllInfo*) NULL;
}

static int
R_getDllIndex(DllInfo *info)
{
    int i;
    for(i = 0; i < CountDLL; i++) {
	if(LoadedDLL[i] == info) return i;
    }
    return -1;
}

/*
  Explicitly register the native routines for use in .Call(), .C() and
  .Fortran() functions. These registered values are used to resolve
  symbols in an object that makes a call to this routine, rather than
  the usual dynamic resolution done by dlsym() or the equivalent on
  the different platforms.
 */
int
R_registerRoutines(DllInfo *info, const R_CMethodDef * const croutines,
		   const R_CallMethodDef * const callRoutines,
		   const R_FortranMethodDef * const fortranRoutines,
		   const R_ExternalMethodDef * const externalRoutines)
{
    int i, num;

    if(info == NULL)
	error(_("R_RegisterRoutines called with invalid DllInfo object."));

    /* Default is to look in registered and then dynamic (unless
       the is no handle such as in "base" or "embedded")
       Potentially change in the future to be only registered
       if there are any registered values.
    */
    info->useDynamicLookup = (info->handle) ? TRUE : FALSE;
    info->forceSymbols = FALSE;

    if(croutines) {
	for(num = 0; croutines[num].name != NULL; num++) {;}
	info->CSymbols = (Rf_DotCSymbol*)calloc((size_t) num,
						sizeof(Rf_DotCSymbol));
	info->numCSymbols = num;
	for(i = 0; i < num; i++) {
	    R_addCRoutine(info, croutines+i, info->CSymbols + i);
	}
    }

    if(fortranRoutines) {
	for(num = 0; fortranRoutines[num].name != NULL; num++) {;}
	info->FortranSymbols =
	    (Rf_DotFortranSymbol*)calloc((size_t) num,
					 sizeof(Rf_DotFortranSymbol));
	info->numFortranSymbols = num;
	for(i = 0; i < num; i++)
	    R_addFortranRoutine(info, fortranRoutines+i,
				info->FortranSymbols + i);
    }

    if(callRoutines) {
	for(num = 0; callRoutines[num].name != NULL; num++) {;}
	info->CallSymbols =
	    (Rf_DotCallSymbol*)calloc((size_t) num, sizeof(Rf_DotCallSymbol));
	info->numCallSymbols = num;
	for(i = 0; i < num; i++)
	    R_addCallRoutine(info, callRoutines+i, info->CallSymbols + i);
    }

    if(externalRoutines) {
	for(num = 0; externalRoutines[num].name != NULL; num++) {;}
	info->ExternalSymbols =
	    (Rf_DotExternalSymbol*)calloc((size_t) num,
					  sizeof(Rf_DotExternalSymbol));
	info->numExternalSymbols = num;

	for(i = 0; i < num; i++)
	    R_addExternalRoutine(info, externalRoutines+i,
				 info->ExternalSymbols + i);
    }

    return(1);
}

static SEXP getSymbolComponent(SEXP sSym, const char *name, SEXPTYPE type, int optional) {
    SEXP sNames = 0;
    int i = 0, n;
    if (TYPEOF(sSym) != VECSXP ||
	TYPEOF(sNames = getAttrib(sSym, R_NamesSymbol)) != STRSXP)
	Rf_error(_("Invalid object."));
    n = LENGTH(sNames);
    while (i < n) {
	if (!strcmp(CHAR(STRING_ELT(sNames, i)), name)) {
	    SEXP res = R_NilValue;
	    if (i >= LENGTH(sSym) ||
		((type != ANYSXP) && (TYPEOF(res = VECTOR_ELT(sSym, i)) != type)))
		Rf_error(_("Invalid entry '%s' in native symbol object."), name);
	    return res;
	}
	i++;
    }
    if (!optional)
	Rf_error(_("Component '%s' missing in symbol object."), name);
    return R_NilValue;
}

/* This takes a list objects of any of the classes CRoutine, CallRoutine,
   FortranRoutine, ExternalRoutine containing registration info in either
   numParameters or nativeParamTypes and registers them using R_registerRoutines. */
attribute_hidden SEXP Rf_registerRoutines(SEXP sSymbolList) {
    DllInfo *dll = NULL;
    int i = 0, n;
    int n_c = 0, n_call = 0, n_f = 0, n_ext = 0;
    if (TYPEOF(sSymbolList) != VECSXP)
	Rf_error(_("Invalid symbol list."));
    n = LENGTH(sSymbolList);
    /* PASS 1: find the number of entries for each type */
    while (i < n) {
	SEXP sSym = VECTOR_ELT(sSymbolList, i++);
	if (inherits(sSym, "CRoutine"))
	    n_c++;
	else if (inherits(sSym, "CallRoutine"))
	    n_call++;
	else if (inherits(sSym, "FortranRoutine"))
	    n_f++;
	else if (inherits(sSym, "ExternalRoutine"))
	    n_ext++;
	else
	    Rf_error(_("Symbol at %d does not have registration information."), i);
    }
    /* PASS 2: allocate all necessary structures and fill them.
       R_registerRoutines() copies all contents so we can use anything
       from the symbol list as-is and any transient allocations as well */
    R_CMethodDef *cRoutines = n_c ? (R_CMethodDef*) R_alloc(n_c + 1, sizeof(R_CMethodDef)) : NULL;
    R_CallMethodDef *callRoutines = n_call ? (R_CallMethodDef*) R_alloc(n_call + 1, sizeof(R_CallMethodDef)) : NULL;
    R_FortranMethodDef *fortranRoutines = n_f ? (R_FortranMethodDef*) R_alloc(n_f + 1, sizeof(R_FortranMethodDef)) : NULL;
    R_ExternalMethodDef *externalRoutines = n_ext ? (R_ExternalMethodDef*) R_alloc(n_c + 1, sizeof(R_ExternalMethodDef)) : NULL;
    /* populate them from the symbols */
    i = n_c = n_call = n_f = n_ext = 0;
    while (i < n) {
	const char *cName = 0;
	SEXP sSym = VECTOR_ELT(sSymbolList, i++);
	SEXP sName = getSymbolComponent(sSym, "name", STRSXP, 0);
	SEXP sAddr = getSymbolComponent(sSym, "address", EXTPTRSXP, 0);
	SEXP sArgTypes = getSymbolComponent(sSym, "nativeParamTypes", INTSXP, 1);
	SEXP sArgNum = getSymbolComponent(sSym, "numParameters", INTSXP, 1);
	SEXP sDll = getSymbolComponent(sSym, "dll", VECSXP, 0);
	SEXP sDllInfo = getSymbolComponent(sDll, "info", EXTPTRSXP, 0);
#ifdef __clang__
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wpedantic"
#elif defined __GNUC__
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wpedantic"	
#endif
	DL_FUNC addr = (DL_FUNC) EXTPTR_PTR(sAddr);
#ifdef __clang__
# pragma clang diagnostic pop
#elif defined __GNUC__
# pragma GCC diagnostic pop
#endif
	R_NativePrimitiveArgType* types = NULL;
	int numArgs = -1;
	if (LENGTH(sName) != 1)
	    Rf_error(_("Invalid symbol name."));
	cName = CHAR(STRING_ELT(sName, 0));
	if (inherits(sAddr, "RegisteredNativeSymbol"))
	    Rf_error(_("Cannot register already registered native symbol '%s'."), cName);
	if (!inherits(sAddr, "NativeSymbol"))
	    Rf_error(_("Symbol '%s' does not have a valid native address."), cName);
	if (!inherits(sDllInfo, "DLLInfoReference"))
	    Rf_error(_("Symbol '%s' does not have a valid DllInfo reference."), cName);
	if (!dll)
	    dll = (DllInfo*) EXTPTR_PTR(sDllInfo);
	else /* check if it is the same Dll, we only support one Dll per call */
	    if (dll != (DllInfo*) EXTPTR_PTR(sDllInfo))
		Rf_error(_("Symbol '%s' comes from a different shared object."), cName);
	if (sArgTypes == R_NilValue) { /* no type spec - just use num args */
	    if (sArgNum == R_NilValue)
		Rf_error(_("Symbol '%s' is missing parameter specification."), cName);
	    numArgs = (INTEGER(sArgNum)[0] < 0) ? -1 : INTEGER(sArgNum)[0];
	} else {
	    /* NOTE: we do assume that we can use the native type specification as-is,
	       i.e. R_NativePrimitiveArgType has to match INTSXP */
	    numArgs = LENGTH(sArgTypes);
	    if (numArgs)
		types = (R_NativePrimitiveArgType*)(INTEGER(sArgTypes));
	}
	if (inherits(sSym, "CRoutine")) {
	    cRoutines[n_c].name    = cName;
	    cRoutines[n_c].fun     = addr;
	    cRoutines[n_c].types   = types;
	    cRoutines[n_c].numArgs = numArgs;
	    n_c++;
	} else if (inherits(sSym, "CallRoutine")) {
	    callRoutines[n_call].name    = cName;
	    callRoutines[n_call].fun     = addr;
	    /* callRoutines[n_call].types   = types; */
	    callRoutines[n_call].numArgs = numArgs;
	    n_call++;
	} else if (inherits(sSym, "FortranRoutine")) {
	    fortranRoutines[n_f].name    = cName;
	    fortranRoutines[n_f].fun     = addr;
	    fortranRoutines[n_f].types   = types;
	    fortranRoutines[n_f].numArgs = numArgs;	    
	    n_f++;
	} else if (inherits(sSym, "ExternalRoutine")) {
	    externalRoutines[n_ext].name    = cName;
	    externalRoutines[n_ext].fun     = addr;
	    /* externalRoutines[n_ext].types   = types; */
	    externalRoutines[n_ext].numArgs = numArgs;
	    n_ext++;
	}
    }

    /* terminate the lists */
    if (n_c)
	memset(cRoutines + n_c, 0, sizeof(*cRoutines));
    if (n_call)
	memset(callRoutines + n_call, 0, sizeof(*callRoutines));
    if (n_f)
	memset(fortranRoutines + n_f, 0, sizeof(*fortranRoutines));
    if (n_ext)
	memset(externalRoutines + n_ext, 0, sizeof(*externalRoutines));

    return ScalarLogical(R_registerRoutines(dll, cRoutines, callRoutines,
					    fortranRoutines, externalRoutines));
}

static void
R_setPrimitiveArgTypes(const R_FortranMethodDef * const croutine,
		       Rf_DotFortranSymbol *sym)
{
    sym->types = (R_NativePrimitiveArgType *)
	malloc(sizeof(R_NativePrimitiveArgType) * (size_t) croutine->numArgs);
    if(!sym->types)
	error("allocation failure in R_setPrimitiveArgTypes");
    if(sym->types)
	memcpy(sym->types, croutine->types,
	       sizeof(R_NativePrimitiveArgType) * (size_t) croutine->numArgs);

}

static void
R_addFortranRoutine(DllInfo *info,
		    const R_FortranMethodDef * const croutine,
		    Rf_DotFortranSymbol *sym)
{
    sym->name = Rstrdup(croutine->name);
    sym->fun = croutine->fun;
    sym->numArgs = croutine->numArgs > -1 ? croutine->numArgs : -1;
    if(croutine->types)
	R_setPrimitiveArgTypes(croutine, sym);
}

static void
R_addExternalRoutine(DllInfo *info,
		     const R_ExternalMethodDef * const croutine,
		     Rf_DotExternalSymbol *sym)
{
    sym->name = Rstrdup(croutine->name);
    sym->fun = croutine->fun;
    sym->numArgs = croutine->numArgs > -1 ? croutine->numArgs : -1;
}

static void
R_addCRoutine(DllInfo *info, const R_CMethodDef * const croutine,
	      Rf_DotCSymbol *sym)
{
    sym->name = Rstrdup(croutine->name);
    sym->fun = croutine->fun;
    sym->numArgs = croutine->numArgs > -1 ? croutine->numArgs : -1;
    if(croutine->types)
	R_setPrimitiveArgTypes(croutine, sym);
}

static void
R_addCallRoutine(DllInfo *info, const R_CallMethodDef * const croutine,
		 Rf_DotCallSymbol *sym)
{
    sym->name = Rstrdup(croutine->name);
    sym->fun = croutine->fun;
    sym->numArgs = croutine->numArgs > -1 ? croutine->numArgs : -1;
}

static void
Rf_freeCSymbol(Rf_DotCSymbol *sym)
{
    free(sym->name);
}

static void
Rf_freeCallSymbol(Rf_DotCallSymbol *sym)
{
    free(sym->name);
}

static void
Rf_freeExternalSymbol(Rf_DotCallSymbol *sym)
{
    free(sym->name);
}

static void
Rf_freeFortranSymbol(Rf_DotFortranSymbol *sym)
{
    free(sym->name);
}

static void
Rf_freeDllInfo(DllInfo *info)
{
    int i;
    if (!info)
	return;
    free(info->name);
    free(info->path);
    if(info->CSymbols) {
	for(i = 0; i < info->numCSymbols; i++)
	    Rf_freeCSymbol(info->CSymbols+i);
	free(info->CSymbols);
    }
    if(info->CallSymbols) {
	for(i = 0; i < info->numCallSymbols; i++)
	    Rf_freeCallSymbol(info->CallSymbols+i);
	free(info->CallSymbols);
    }
    if(info->ExternalSymbols) {
	for(i = 0; i < info->numExternalSymbols; i++)
	    Rf_freeExternalSymbol(info->ExternalSymbols+i);
	free(info->ExternalSymbols);
    }
    if(info->FortranSymbols) {
	for(i = 0; i < info->numFortranSymbols; i++)
	    Rf_freeFortranSymbol(info->FortranSymbols+i);
	free(info->FortranSymbols);
    }
    free(info);
}


typedef void (*DllInfoUnloadCall)(DllInfo *);
typedef DllInfoUnloadCall DllInfoInitCall;

static bool
R_callDLLUnload(DllInfo *dllInfo)
{
    char buf[1024];
    DllInfoUnloadCall f;
    R_RegisteredNativeSymbol symbol;
    symbol.type = R_ANY_SYM;

    snprintf(buf, 1024, "R_unload_%s", dllInfo->name);
    f = (DllInfoUnloadCall) R_dlsym(dllInfo, buf, &symbol);
    if(f) f(dllInfo);

    return(TRUE);
}

static void freeRegisteredNativeSymbolCopy(SEXP);

	/* Remove the specified DLL from the current DLL list */
	/* Returns 1 if the DLL was found and removed from */
	/* the list and returns 0 otherwise. */

static int DeleteDLL(const char *path)
{
    int   i, loc;

    for (i = 0; i < CountDLL; i++) {
	if (!strcmp(path, LoadedDLL[i]->path)) {
	    loc = i;
	    goto found;
	}
    }
    return 0;
found:
#ifdef CACHE_DLL_SYM
    if(R_osDynSymbol->deleteCachedSymbols)
	R_osDynSymbol->deleteCachedSymbols(LoadedDLL[loc]);
#endif
    R_reinit_altrep_classes(LoadedDLL[loc]);
    R_callDLLUnload(LoadedDLL[loc]);
    R_osDynSymbol->closeLibrary(LoadedDLL[loc]->handle);
    Rf_freeDllInfo(LoadedDLL[loc]);

    /* zero pointers to DllInfo and the handle */
    SEXP e = VECTOR_ELT(DLLInfoEptrs, loc);
    if (!isNull(e))
	R_ClearExternalPtr(e);
    SEXP sRegisteredNativeSymbol = install("registered native symbol");
    for (SEXP last = SymbolEptrs, next = CDR(SymbolEptrs);
         next != R_NilValue;
         next = CDR(next))
	if (R_WeakRefValue(CAR(next)) == e) {
	    SETCDR(last, CDR(next)); /* remove from list */
	    SEXP p = R_WeakRefKey(CAR(next));
	    if (TYPEOF(p) == EXTPTRSXP && R_ExternalPtrAddr(p)) {
		if (R_ExternalPtrTag(p) == sRegisteredNativeSymbol)
		    freeRegisteredNativeSymbolCopy(p);
		R_ClearExternalPtr(p);
	    }
	} else
	    last = next;



    for(i = loc + 1 ; i < CountDLL ; i++) {
	LoadedDLL[i - 1] = LoadedDLL[i];
	SET_VECTOR_ELT(DLLInfoEptrs, i - 1, VECTOR_ELT(DLLInfoEptrs, i));
    }
    CountDLL--;
    LoadedDLL[CountDLL] = NULL;
    SET_VECTOR_ELT(DLLInfoEptrs, CountDLL, R_NilValue);
    return 1;
}

attribute_hidden
DL_FUNC Rf_lookupCachedSymbol(const char *name, const char *pkg, int all, DllInfo **dll)
{
#ifdef CACHE_DLL_SYM
    int i;
    for (i = 0; i < nCPFun; i++)
	if (!strcmp(name, CPFun[i].name) &&
	    (all || !strcmp(pkg, CPFun[i].pkg))) {

	    if (dll)
		*dll = CPFun[i].dll;
	    return CPFun[i].func;
	}
#endif

    return((DL_FUNC) NULL);
}

 /*
   If we are caching the native level symbols, this routine
   discards the ones from the DLL identified by loc.
   This is called as the initial action of DeleteDLL().
  */
attribute_hidden
void Rf_deleteCachedSymbols(DllInfo *dll)
{
#ifdef CACHE_DLL_SYM
    int i;
    /* Wouldn't a linked list be easier here?
       Potentially ruin the contiguity of the memory.
    */
    for(i = nCPFun - 1; i >= 0; i--)
	if(!strcmp(CPFun[i].pkg, dll->name)) {
	    if(i < nCPFun - 1) {
		nCPFun--;
		strcpy(CPFun[i].name, CPFun[nCPFun].name);
		strcpy(CPFun[i].pkg, CPFun[nCPFun].pkg);
		CPFun[i].func = CPFun[nCPFun].func;
		CPFun[i].dll = CPFun[nCPFun].dll;
	    } else nCPFun--;
	}
#endif /* CACHE_DLL_SYM */
}


#ifdef Win32
#define DLLerrBUFSIZE 4000
#else  /* Not Windows */
#define DLLerrBUFSIZE 1000
#endif

static char DLLerror[DLLerrBUFSIZE] = "";

/* the error message; length taken from ERRBUFSIZE in ./hpdlfcn.c  */

	/* Inserts the specified DLL at the head of the DLL list */
	/* Returns 1 if the DLL was successfully added */
	/* and returns 0 if the DLL table is full or */
	/* or if dlopen fails for some reason. */


static DllInfo* AddDLL(const char *path, int asLocal, int now,
		       const char *DLLsearchpath)
{
    HINSTANCE handle;
    DllInfo *info = NULL;

    int i, loc;

    loc = -1;
    for (i = 0; i < CountDLL; i++)
	if (!strcmp(path, LoadedDLL[i]->path)) {
	    loc = i;
	    break;
	}

    if (loc >= 0) {
	/* The DLL is already loaded, so move it to the head of the list
	   and exit. We assume that the same path means the DLL file is
	   also the same: the user should always unload a DLL file before
	   modifying it (on Windows, it is locked, on Unix, modifying while
	   loaded may cause a crash on function call or even dlsym()). */

	/* FIXME: it might make sense to implement a reference count of how
	   many times a DLL has been loaded, and only delete it when the count
	   reaches zero, but that would break code relying on the existing
	   behavior */

	DllInfo *info = LoadedDLL[loc];
	SEXP eptrs = PROTECT(VECTOR_ELT(DLLInfoEptrs, loc));

	for(i = loc + 1 ; i < CountDLL ; i++) {
	    LoadedDLL[i - 1] = LoadedDLL[i];
	    SET_VECTOR_ELT(DLLInfoEptrs, i - 1, VECTOR_ELT(DLLInfoEptrs, i));
	}

	LoadedDLL[CountDLL - 1] = info;
	SET_VECTOR_ELT(DLLInfoEptrs, CountDLL - 1, eptrs);
	UNPROTECT(1); /* eptrs */
	return info;
    }

    if(CountDLL == MaxNumDLLs) {
	strcpy(DLLerror, _("`maximal number of DLLs reached..."));
	return NULL;
    }

    handle = R_osDynSymbol->loadLibrary(path, asLocal, now, DLLsearchpath);

    if(handle == NULL) {
	R_osDynSymbol->getError(DLLerror, DLLerrBUFSIZE);
	return NULL;
    }

    info = R_RegisterDLL(handle, path);

    /* Now look for an initializing routine named R_init_<object name>.
       If it is present, we call it. It should take a reference to the
       DllInfo object currently being initialized.
    */
    if(info) {
	const char *nm = info->name;
	size_t len = strlen(nm) + 9;
	char tmp[len]; // R_init_ + underscore + null
	DllInfoInitCall f;
#ifdef HAVE_NO_SYMBOL_UNDERSCORE
	snprintf(tmp, len,  "%s%s","R_init_", info->name);
#else
	snprintf(tmp, len, "_%s%s","R_init_", info->name);
#endif
	f = (DllInfoInitCall) R_osDynSymbol->dlsym(info, tmp);
	/* If that failed, might have used the package name with
	   . replaced by _ (as . it not valid in symbol names). */
	if(!f) {
	    /* This is potentially unsafe in MBCSs, as '.' might be
	       part of a character: but is not in UTF-8 */
	    for(char *p = tmp; *p; p++) if(*p == '.') *p = '_';
	    f = (DllInfoInitCall) R_osDynSymbol->dlsym(info, tmp);
	}
	if(f) f(info);
    }

    return info;
}


static DllInfo *R_RegisterDLL(HINSTANCE handle, const char *path)
{
    char *dpath,  DLLname[R_PATH_MAX], *p;
    DllInfo *info;

    dpath = (char *) malloc(strlen(path)+1);
    if(dpath == NULL) {
	strcpy(DLLerror, _("could not allocate space for 'path'"));
	R_osDynSymbol->closeLibrary(handle);
	return 0;
    }
    strcpy(dpath, path);

    if(R_osDynSymbol->fixPath) R_osDynSymbol->fixPath(dpath);

    /* keep only basename from path */
    p = Rf_strrchr(dpath, FILESEP[0]);
    if(!p) p = dpath; else p++;
    if(strlen(p) < R_PATH_MAX) strcpy(DLLname, p);
    else error(_("DLLname '%s' is too long"), p);

    /* remove SHLIB_EXT if present */
    p = DLLname + strlen(DLLname) - strlen(SHLIB_EXT);
#ifdef Win32  /* case-insensitive file system */
    if(p > DLLname && stricmp(p, SHLIB_EXT) == 0) *p = '\0';
#else
    if(p > DLLname && strcmp(p, SHLIB_EXT) == 0) *p = '\0';
#endif

    if (addDLL(dpath, DLLname, handle)) {
	info = LoadedDLL[CountDLL-1];
	/* default is to use old-style dynamic lookup.  The object's
	   initialization routine can limit access by setting this to FALSE.
	*/
	info->useDynamicLookup = TRUE;
	info->forceSymbols = FALSE;
	return info;
    } else
	/* dpath freed in addDLL */
	return NULL;
}

static int
addDLL(char *dpath, char *DLLname, HINSTANCE handle)
{
    int ans = CountDLL;
    char *name = (char *) malloc(strlen(DLLname)+1);
    if(name == NULL) {
	strcpy(DLLerror, _("could not allocate space for 'name'"));
	if(handle)
	    R_osDynSymbol->closeLibrary(handle);
	free(dpath);
	return 0;
    }
    strcpy(name, DLLname);

    DllInfo *info = (DllInfo *) malloc(sizeof(DllInfo));
    if(info == NULL) {
	strcpy(DLLerror, _("could not allocate space for 'DllInfo'"));
	if(handle)
	    R_osDynSymbol->closeLibrary(handle);
	free(name);
	free(dpath);
	return 0;
    }

    info->path = dpath;
    info->name = name;
    info->handle = handle;

    info->numCSymbols = 0;
    info->numCallSymbols = 0;
    info->numFortranSymbols = 0;
    info->numExternalSymbols = 0;
    info->CSymbols = NULL;
    info->CallSymbols = NULL;
    info->FortranSymbols = NULL;
    info->ExternalSymbols = NULL;
    LoadedDLL[CountDLL] = info;
    SET_VECTOR_ELT(DLLInfoEptrs, CountDLL, R_NilValue);
    CountDLL++;

    return(ans);
}

static Rf_DotCSymbol *
Rf_lookupRegisteredCSymbol(DllInfo *info, const char *name)
{
    for(int i = 0; i < info->numCSymbols; i++) {
	if(strcmp(name, info->CSymbols[i].name) == 0)
	    return(&(info->CSymbols[i]));
    }
    return NULL;
}

static Rf_DotFortranSymbol *
Rf_lookupRegisteredFortranSymbol(DllInfo *info, const char *name)
{
    for(int i = 0; i < info->numFortranSymbols; i++) {
	if(strcmp(name, info->FortranSymbols[i].name) == 0)
	    return(&(info->FortranSymbols[i]));
    }

    return (Rf_DotFortranSymbol*) NULL;
}

static Rf_DotCallSymbol *
Rf_lookupRegisteredCallSymbol(DllInfo *info, const char *name)
{

    for(int i = 0; i < info->numCallSymbols; i++) {
	if(strcmp(name, info->CallSymbols[i].name) == 0)
	    return(&(info->CallSymbols[i]));
    }
    return (Rf_DotCallSymbol*) NULL;
}

static Rf_DotExternalSymbol *
Rf_lookupRegisteredExternalSymbol(DllInfo *info, const char *name)
{
    for(int i = 0; i < info->numExternalSymbols; i++) {
	if(strcmp(name, info->ExternalSymbols[i].name) == 0)
	    return(&(info->ExternalSymbols[i]));
    }
    return (Rf_DotExternalSymbol*) NULL;
}

static DL_FUNC
R_getDLLRegisteredSymbol(DllInfo *info, const char *name,
			 R_RegisteredNativeSymbol *symbol)
{
    NativeSymbolType purpose = R_ANY_SYM;

    if(symbol)
	purpose = symbol->type;

    if((purpose == R_ANY_SYM || purpose == R_C_SYM) &&
       info->numCSymbols > 0) {
	Rf_DotCSymbol *sym;
	sym = Rf_lookupRegisteredCSymbol(info, name);
	if(sym) {
	    if(symbol) {
		symbol->type = R_C_SYM;
		symbol->symbol.c = sym;
		symbol->dll = info;
	    }

	    return((DL_FUNC) sym->fun);
	}
    }

    if((purpose == R_ANY_SYM || purpose == R_CALL_SYM) &&
       info->numCallSymbols > 0) {
	Rf_DotCallSymbol *sym;
	sym = Rf_lookupRegisteredCallSymbol(info, name);
	if(sym) {
	    if(symbol) {
		symbol->type = R_CALL_SYM;
		symbol->symbol.call = sym;
		symbol->dll = info;
	    }
	    return((DL_FUNC) sym->fun);
	}
    }

    if((purpose == R_ANY_SYM || purpose == R_FORTRAN_SYM) &&
       info->numFortranSymbols > 0) {
	Rf_DotFortranSymbol *sym;
	sym = Rf_lookupRegisteredFortranSymbol(info, name);
	if(sym) {
	    if(symbol) {
		symbol->type = R_FORTRAN_SYM;
		symbol->symbol.fortran = sym;
		symbol->dll = info;
	    }
	    return((DL_FUNC) sym->fun);
	}
    }

    if((purpose == R_ANY_SYM || purpose == R_EXTERNAL_SYM) &&
       info->numExternalSymbols > 0) {
	Rf_DotExternalSymbol *sym;
	sym = Rf_lookupRegisteredExternalSymbol(info, name);
	if(sym) {
	    if(symbol) {
		symbol->type = R_EXTERNAL_SYM;
		symbol->symbol.external = sym;
		symbol->dll = info;
	    }
	    return((DL_FUNC) sym->fun);
	}
    }

    return((DL_FUNC) NULL);
}

attribute_hidden DL_FUNC
R_dlsym(DllInfo *info, char const *name,
	R_RegisteredNativeSymbol *symbol)
{
    size_t len = strlen(name) + 4;
    char buf[len]; /* up to 3 additional underscores */
    DL_FUNC f;

    f = R_getDLLRegisteredSymbol(info, name, symbol);
    if(f) return(f);


    if(info->useDynamicLookup == FALSE) return(NULL);

#ifdef HAVE_NO_SYMBOL_UNDERSCORE
    snprintf(buf, len, "%s", name);
#else
    snprintf(buf, len, "_%s", name);
#endif

/* HAVE_F77_EXTRA_UNDERSCORE is only use here and not in F77_NAME etc.
   It seems of only historical interest */
#ifdef HAVE_F77_UNDERSCORE
    if(symbol && symbol->type == R_FORTRAN_SYM) {
	strcat(buf, "_");
# ifdef HAVE_F77_EXTRA_UNDERSCORE
	if(strchr(name, '_')) strcat(buf, "_");
# endif
    }
#endif

    f = (DL_FUNC) R_osDynSymbol->dlsym(info, buf);
#ifdef HAVE_F77_UNDERSCORE
    if (!f && symbol && symbol->type == R_ANY_SYM) {
	strcat(buf, "_");
# ifdef HAVE_F77_EXTRA_UNDERSCORE
	if(strchr(name, '_')) strcat(buf, "_");
# endif
	f = (DL_FUNC) R_osDynSymbol->dlsym(info, buf);
    }
#endif

    if (f && symbol)
	symbol->dll = info;
    return f;
}

/* R_FindSymbol checks whether one of the objects that have been
   loaded contains the symbol name and returns a pointer to that
   symbol upon success.
*/

DL_FUNC R_FindSymbol(char const *name, char const *pkg,
		     R_RegisteredNativeSymbol *symbol)
{
    DL_FUNC fcnptr = (DL_FUNC) NULL;
    int i, all = (strlen(pkg) == 0), doit;

    if(R_osDynSymbol->lookupCachedSymbol) {
	DllInfo *dll = NULL;
	fcnptr = R_osDynSymbol->lookupCachedSymbol(name, pkg, all, &dll);
	if (fcnptr && symbol && dll)
	    symbol->dll = dll;
	if(fcnptr)
	    return(fcnptr);
    }

    /* The following is not legal ANSI C. */
    /* It is only meant to be used in systems supporting */
    /* the dlopen() interface, in which systems data and  */
    /* function pointers _are_ the same size and _can_   */
    /* be cast without loss of information.	     */

    for (i = CountDLL - 1; i >= 0; i--) {
	doit = all;
	if(!doit && !strcmp(pkg, LoadedDLL[i]->name)) doit = 2;
	if(doit && LoadedDLL[i]->forceSymbols) doit = 0;
	if(doit) {
	    fcnptr = R_dlsym(LoadedDLL[i], name, symbol); /* R_osDynSymbol->dlsym */
	    if (fcnptr != (DL_FUNC) NULL) {
		if(symbol)
		    symbol->dll = LoadedDLL[i];
#ifdef CACHE_DLL_SYM
		if(strlen(pkg) <= 20 && strlen(name) <= 40 && nCPFun < MAX_CACHE
		   && (!symbol || !symbol->symbol.c)) {
		    strcpy(CPFun[nCPFun].pkg, LoadedDLL[i]->name);
		    strcpy(CPFun[nCPFun].name, name);
		    CPFun[nCPFun].func = fcnptr;
		    CPFun[nCPFun].dll = LoadedDLL[i];
		    nCPFun++;
		}
#endif
		return fcnptr;
	    }
	}
	if(doit > 1) return (DL_FUNC) NULL;  /* Only look in the first-matching DLL */
    }

    return (DL_FUNC) NULL;
}


static void
GetFullDLLPath(SEXP call, char *buf, size_t bufsize, const char *const path)
{
    size_t res = R_osDynSymbol->getFullDLLPath(call, buf, bufsize, path);
    if (res >= bufsize)
	error(_("path too long")); 
}

	/* do_dynload implements the R-Interface for the */
	/* loading of shared objects */

/*
  Extended to support 2 additional arguments (3 in total).
  First argument is the name of the DLL.
  Second argument is a logical indicating whether we
  want the symbols to be kept in their own local symbol table
  or added to the global symbol table of the application.
  Third argument is a logical indicating whether the
  dynamic loading should relocate all routine symbols
  now and signal any errors immediately or lazily relocate
  the symbols as they are invoked. This is useful for
  developers so that they can ensure that all the symbols
  are available before they release, and allows users to
  call routines from "incomplete" DLLs.
 */

attribute_hidden SEXP do_dynload(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char buf[2 * R_PATH_MAX];
    DllInfo *info;

    checkArity(op,args);
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	error(_("character argument expected"));
    GetFullDLLPath(call, buf, sizeof(buf),
                   translateCharFP(STRING_ELT(CAR(args), 0)));
    /* AddDLL does this DeleteDLL(buf); */
    info = AddDLL(buf, LOGICAL(CADR(args))[0], LOGICAL(CADDR(args))[0],
		  translateCharFP(STRING_ELT(CADDDR(args), 0)));
    if(!info)
	error(_("unable to load shared object '%s':\n  %s"), buf, DLLerror);
    return(Rf_MakeDLLInfo(info));
}

attribute_hidden SEXP do_dynunload(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char buf[2 * R_PATH_MAX];

    checkArity(op,args);
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	error(_("character argument expected"));
    GetFullDLLPath(call, buf, sizeof(buf),
                   translateCharFP(STRING_ELT(CAR(args), 0)));
    if(!DeleteDLL(buf))
	error(_("shared object '%s\' was not loaded"), buf);
    return R_NilValue;
}

int R_moduleCdynload(const char *module, int local, int now)
{
    char dllpath[R_PATH_MAX], *p = getenv("R_HOME");
    DllInfo *res;

    if(!p) return 0;
#ifdef R_ARCH
    snprintf(dllpath, R_PATH_MAX, "%s%smodules%s%s%s%s%s", p, FILESEP, FILESEP,
	     R_ARCH, FILESEP, module, SHLIB_EXT);
#else
    snprintf(dllpath, R_PATH_MAX, "%s%smodules%s%s%s", p, FILESEP, FILESEP,
	     module, SHLIB_EXT);
#endif
    res = AddDLL(dllpath, local, now, "");
    if(!res)
	warning(_("unable to load shared object '%s':\n  %s"),
		dllpath, DLLerror);
    return res != NULL ? 1 : 0;
}

int R_cairoCdynload(int local, int now)
{
    char dllpath[R_PATH_MAX], *p = getenv("R_HOME"), *module = "cairo";
    DllInfo *res;

    if(!p) return 0;
#ifdef R_ARCH
    snprintf(dllpath, R_PATH_MAX, "%s/library/grDevices/libs/%s/%s%s",
	     p, R_ARCH, module, SHLIB_EXT);
#else
    snprintf(dllpath, R_PATH_MAX, "%s/library/grDevices/libs/%s%s",
	     p, module, SHLIB_EXT);
#endif
    res = AddDLL(dllpath, local, now, "");
    if(!res)
	warning(_("unable to load shared object '%s':\n  %s"),
		dllpath, DLLerror);
    return res != NULL ? 1 : 0;
}

/**
  Creates an R object representing the value of the
  function pointer given by `f'. This object has class
  NativeSymbol and can be used to relay symbols from
  one DLL to another.
 */
static SEXP
Rf_MakeNativeSymbolRef(DL_FUNC f)
{
    SEXP ref, klass;

    PROTECT(ref = R_MakeExternalPtrFn(f, install("native symbol"),
				      R_NilValue));
    PROTECT(klass = mkString("NativeSymbol"));
    setAttrib(ref, R_ClassSymbol, klass);
    UNPROTECT(2);
    return(ref);
}

static void
freeRegisteredNativeSymbolCopy(SEXP ref)
{
   void *ptr;
   ptr = R_ExternalPtrAddr(ref);
   if (ptr)
       free(ptr);
}

static SEXP
Rf_MakeRegisteredNativeSymbol(R_RegisteredNativeSymbol *symbol)
{
    SEXP ref, klass;
    R_RegisteredNativeSymbol *copy;
    copy = (R_RegisteredNativeSymbol *) malloc(1 * sizeof(R_RegisteredNativeSymbol));
    if(!copy) {
	error(ngettext("cannot allocate memory for registered native symbol (%d byte)",
		       "cannot allocate memory for registered native symbol (%d bytes)",
		      (int) sizeof(R_RegisteredNativeSymbol)),
		      (int) sizeof(R_RegisteredNativeSymbol));
    }
    *copy = *symbol;

    PROTECT(ref = R_MakeExternalPtr(copy,
				    install("registered native symbol"),
				    R_NilValue));
    R_RegisterCFinalizer(ref, freeRegisteredNativeSymbolCopy);

    PROTECT(klass = mkString("RegisteredNativeSymbol"));
    setAttrib(ref, R_ClassSymbol, klass);

    UNPROTECT(2);
    return(ref);
}


static SEXP
Rf_makeDllObject(HINSTANCE inst)
{
    SEXP ans;

    PROTECT(ans = R_MakeExternalPtr(inst, install("DLLHandle"),
				    R_NilValue));
    setAttrib(ans, R_ClassSymbol, mkString("DLLHandle"));
    UNPROTECT(1);

    return(ans);
}

static SEXP
Rf_makeDllInfoReference(DllInfo *info)
{
    SEXP ans;

    int i = R_getDllIndex(info);
    if (i >= 0) {
	ans = VECTOR_ELT(DLLInfoEptrs, i);
	if (!isNull(ans))
	    return ans;
    }

    PROTECT(ans = R_MakeExternalPtr((HINSTANCE) info, install("DLLInfo"),
				    R_NilValue));
    setAttrib(ans, R_ClassSymbol, mkString("DLLInfoReference"));
    if (i >= 0)
	SET_VECTOR_ELT(DLLInfoEptrs, i, ans);
    UNPROTECT(1);

    return(ans);
}


/**
 Creates an R object representing the public DLL information stored in
 info. Currently this is only the short and the long, fully qualified
 name of the DLL and whether we only look for symbols that have been
 registered in this DLL or do we also use dynamic lookup.
 */
static SEXP
Rf_MakeDLLInfo(DllInfo *info)
{
    SEXP ref, elNames, tmp;
    int i, n;
    const char *const names[] = {"name", "path", "dynamicLookup",
				 "handle", "info", "forceSymbols"};

    n = sizeof(names)/sizeof(names[0]);

    PROTECT(ref = allocVector(VECSXP, n));
    SET_VECTOR_ELT(ref, 0, tmp = allocVector(STRSXP, 1));
    if(info->name)
	SET_STRING_ELT(tmp, 0, mkChar(info->name));
    SET_VECTOR_ELT(ref, 1, tmp = allocVector(STRSXP, 1));
    if(info->path)
	SET_STRING_ELT(tmp, 0, mkChar(info->path));
    SET_VECTOR_ELT(ref, 2, ScalarLogical(info->useDynamicLookup));

    SEXP ehandle = Rf_makeDllObject(info->handle);
    SET_VECTOR_ELT(ref, 3, ehandle);
    SEXP einfo = Rf_makeDllInfoReference(info);
    SET_VECTOR_ELT(ref, 4, einfo);
    R_registerSymbolEptr(ehandle, einfo);
    SET_VECTOR_ELT(ref, 5, ScalarLogical(info->forceSymbols));

    PROTECT(elNames = allocVector(STRSXP, n));
    for(i = 0; i < n; i++)
	SET_STRING_ELT(elNames, i, mkChar(names[i]));
    setAttrib(ref, R_NamesSymbol, elNames);

    setAttrib(ref, R_ClassSymbol, mkString("DLLInfo"));

    UNPROTECT(2);

    return(ref);
}

/*
  This is the routine associated with the getNativeSymbolInfo()
  function and it takes the name of a symbol and optionally an
  object identifier (package usually) in which to restrict the search
  for this symbol. It resolves the symbol and returns it to the caller
  giving the symbol address, the package information (i.e. name and
  fully qualified shared object name). If the symbol was explicitly
  registered (rather than dynamically resolved by R), then we pass
  back that information also, giving the number of arguments it
  expects and the interface by which it should be called.
  The returned object has class NativeSymbol. If the symbol was
  registered, we add a class identifying the interface type
  for which it is intended (i.e. .C(), .Call(), etc.)
 */
attribute_hidden SEXP
R_getSymbolInfo(SEXP sname, SEXP spackage, SEXP withRegistrationInfo)
{
    const void *vmax = vmaxget();
    const char *package, *name;
    R_RegisteredNativeSymbol symbol = {R_ANY_SYM, {NULL}, NULL};
    SEXP sym = R_NilValue;
    DL_FUNC f = NULL;

    package = "";

    if (!isString(sname) || LENGTH(sname) != 1)
	error(_("invalid '%s' argument"), "name");
    name = translateCharFP(STRING_ELT(sname, 0));

    if(length(spackage)) {
	if(TYPEOF(spackage) == STRSXP)
	    package = translateCharFP(STRING_ELT(spackage, 0));
	else if(TYPEOF(spackage) == EXTPTRSXP &&
		R_ExternalPtrTag(spackage) == install("DLLInfo")) {
	    DllInfo *dll = (DllInfo *) R_ExternalPtrAddr(spackage);
	    if (!dll)
		error(_("NULL value passed for DllInfo"));
	    f = R_dlsym(dll, name, &symbol);
	    package = NULL;
	} else
	    error(_("must pass package name or DllInfo reference"));
    }

    if(package)
	f = R_FindSymbol(name, package, &symbol);

    if(f)
	sym = createRSymbolObject(sname, f, &symbol,
				  asBool2(withRegistrationInfo, R_NilValue));

    vmaxset(vmax);
    return sym;
}

attribute_hidden SEXP
R_getDllTable(void)
{
    int i;
    SEXP ans, nm;

 again:
    PROTECT(ans = allocVector(VECSXP, CountDLL));
    for(i = 0; i < CountDLL; i++)
	SET_VECTOR_ELT(ans, i, Rf_MakeDLLInfo(LoadedDLL[i]));
    
    setAttrib(ans, R_ClassSymbol, mkString("DLLInfoList"));
    UNPROTECT(1);

    /* There is a problem here: The allocations can cause gc, and gc
       may result in no longer referenced DLLs being unloaded.  So
       CountDLL can be reduced during this loop.  A simple work-around
       is to just try again until CountDLL at the end is the same as
       it was at the beginning.  LT */
    if (CountDLL != LENGTH(ans))
	goto again;

    PROTECT(ans);
    PROTECT(nm = allocVector(STRSXP, CountDLL));
    setAttrib(ans, R_NamesSymbol, nm);
    for(int i = 0; i < CountDLL; i++)
	SET_STRING_ELT(nm, i,
	               /* ->name from DllInfo */
		       STRING_ELT(VECTOR_ELT(VECTOR_ELT(ans, i), 0), 0));
    UNPROTECT(2);
    return(ans);
}

static SEXP
createRSymbolObject(SEXP sname, DL_FUNC f, R_RegisteredNativeSymbol *symbol,
		    bool withRegistrationInfo)
{
    SEXP tmp, klass, sym, names;
    int n = (symbol->type != R_ANY_SYM) ? 4 : 3;
    int numProtects = 0;

    PROTECT(sym = allocVector(VECSXP, n));    numProtects++;
    PROTECT(names = allocVector(STRSXP, n));    numProtects++;

    if(!sname || sname == R_NilValue) {
	PROTECT(sname = mkString(symbol->symbol.call->name));
	numProtects++;
    }

    SET_VECTOR_ELT(sym, 0, sname);
    SET_STRING_ELT(names, 0, mkChar("name"));

    SEXP eaddress =
	withRegistrationInfo && symbol && symbol->symbol.c && symbol->dll
		   ? Rf_MakeRegisteredNativeSymbol(symbol)
		   : Rf_MakeNativeSymbolRef(f);
    SET_VECTOR_ELT(sym, 1, eaddress);
    SET_STRING_ELT(names, 1, mkChar("address"));
    if(symbol->dll) { /* now always */
	SEXP rinfo = Rf_MakeDLLInfo(symbol->dll);
	SET_VECTOR_ELT(sym, 2, rinfo);
	SEXP einfo = VECTOR_ELT(rinfo, 4 /* info */);
	R_registerSymbolEptr(eaddress, einfo);
    }
    SET_STRING_ELT(names, 2, mkChar("dll"));


    PROTECT(klass = allocVector(STRSXP, (symbol->type != R_ANY_SYM ? 2 : 1)));
    numProtects++;
    SET_STRING_ELT(klass, LENGTH(klass) - 1, mkChar("NativeSymbolInfo"));

    if(n > 3) {
	/* Add the registration information:
	   the number of arguments and the classname.
	*/
	int nargs = -1;
	char *className = "";
	switch(symbol->type) {
	case R_C_SYM:
	    nargs = symbol->symbol.c->numArgs;
	    className = "CRoutine";
	    break;
	case R_CALL_SYM:
	    nargs = symbol->symbol.call->numArgs;
	    className = "CallRoutine";
	    break;
	case R_FORTRAN_SYM:
	    nargs = symbol->symbol.fortran->numArgs;
	    className = "FortranRoutine";
	    break;
	case R_EXTERNAL_SYM:
	    nargs = symbol->symbol.external->numArgs;
	    className = "ExternalRoutine";
	    break;
	default:
	    /* Something unintended has happened if we get here. */
	    error(_("unimplemented type %d in 'createRSymbolObject'"),
		  symbol->type);
	    break;
	}
	SET_VECTOR_ELT(sym, 3, tmp = ScalarInteger(nargs));
	SET_STRING_ELT(klass, 0, mkChar(className));
	SET_STRING_ELT(names, 3, mkChar("numParameters"));
    }

    setAttrib(sym, R_ClassSymbol, klass);
    setAttrib(sym, R_NamesSymbol, names);

    UNPROTECT(numProtects);
    return(sym);
}

static SEXP
R_getRoutineSymbols(NativeSymbolType type, DllInfo *info)
{
    SEXP ans;
    int i, num;
    R_RegisteredNativeSymbol  sym;
    DL_FUNC address = NULL;

    sym.dll = info;
    sym.type =type;

    switch(type) {
    case R_CALL_SYM: num = info->numCallSymbols;
	break;
    case R_C_SYM: num = info->numCSymbols;
	break;
    case R_FORTRAN_SYM: num = info->numFortranSymbols;
	break;
    case R_EXTERNAL_SYM: num = info->numExternalSymbols;
	break;
    default:
	num = 0;
    }

    PROTECT(ans = allocVector(VECSXP, num));

    for(i = 0; i < num ; i++) {
	switch(type) {
	case R_CALL_SYM:
	    sym.symbol.call = &info->CallSymbols[i];
	    address = sym.symbol.call->fun;
	    break;
	case R_C_SYM:
	    sym.symbol.c = &info->CSymbols[i];
	    address = sym.symbol.c->fun;
	    break;
	case R_FORTRAN_SYM:
	    sym.symbol.fortran = &info->FortranSymbols[i];
	    address = sym.symbol.fortran->fun;
	    break;
	case R_EXTERNAL_SYM:
	    sym.symbol.external = &info->ExternalSymbols[i];
	    address = sym.symbol.external->fun;
	    break;
	default:
	    continue;
	}
	SET_VECTOR_ELT(ans, i, createRSymbolObject(NULL,  address, &sym, TRUE));/* XXX */
    }

    setAttrib(ans, R_ClassSymbol, mkString("NativeRoutineList"));
    UNPROTECT(1);
    return(ans);
}


attribute_hidden SEXP
R_getRegisteredRoutines(SEXP dll)
{
    DllInfo *info;
    SEXP ans, snames;
    int i;
    const char * const names[] = {".C", ".Call", ".Fortran", ".External"};

    if(TYPEOF(dll) != EXTPTRSXP &&
       R_ExternalPtrTag(dll) != install("DLLInfo"))
	error(_("R_getRegisteredRoutines() expects a DllInfo reference"));

    info = (DllInfo *) R_ExternalPtrAddr(dll);
    if(!info) error(_("NULL value passed for DllInfo"));


    PROTECT(ans = allocVector(VECSXP, 4));

    SET_VECTOR_ELT(ans, 0, R_getRoutineSymbols(R_C_SYM, info));
    SET_VECTOR_ELT(ans, 1, R_getRoutineSymbols(R_CALL_SYM, info));
    SET_VECTOR_ELT(ans, 2, R_getRoutineSymbols(R_FORTRAN_SYM, info));
    SET_VECTOR_ELT(ans, 3, R_getRoutineSymbols(R_EXTERNAL_SYM, info));

    PROTECT(snames = allocVector(STRSXP, 4));
    for(i = 0; i < 4; i++)
	SET_STRING_ELT(snames, i, mkChar(names[i]));
    setAttrib(ans, R_NamesSymbol, snames);
    UNPROTECT(2);
    return(ans);
}

attribute_hidden SEXP
do_getSymbolInfo(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return R_getSymbolInfo(CAR(args), CADR(args), CADDR(args));
}

/* .Internal(getLoadedDLLs()) */
attribute_hidden SEXP
do_getDllTable(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return R_getDllTable();
}

attribute_hidden SEXP
do_getRegisteredRoutines(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return R_getRegisteredRoutines(CAR(args));
}



/* Experimental interface for exporting and importing functions and
   data from one package for use from C code in a package.  The
   registration part probably ought to be integrated with the other
   registrations.  The naming of these routines may be less than
   ideal. */

static SEXP CEntryTable = NULL;

static SEXP get_package_CEntry_table(const char *package)
{
    SEXP penv, pname;

    if (CEntryTable == NULL) {
	CEntryTable = R_NewHashedEnv(R_NilValue, 0);
	R_PreserveObject(CEntryTable);
    }
    pname = install(package);
    penv = R_findVarInFrame(CEntryTable, pname);
    if (penv == R_UnboundValue) {
	penv = R_NewHashedEnv(R_NilValue, 0);
	defineVar(pname, penv, CEntryTable);
    }
    return penv;
}

/* FIXME: clear the callables when unloading DLLs. May require changing
   the interface or approximating. */
void R_RegisterCCallable(const char *package, const char *name, DL_FUNC fptr)
{
    SEXP penv = get_package_CEntry_table(package);
    PROTECT(penv);
    SEXP eptr = R_MakeExternalPtrFn(fptr, R_NilValue, R_NilValue);
    PROTECT(eptr);
    defineVar(install(name), eptr, penv);
    UNPROTECT(2);
}

DL_FUNC R_GetCCallable(const char *package, const char *name)
{
    SEXP penv = get_package_CEntry_table(package);
    PROTECT(penv);
    SEXP eptr = R_findVarInFrame(penv, install(name));
    UNPROTECT(1);
    if (eptr == R_UnboundValue)
	error(_("function '%s' not provided by package '%s'"), name, package);
    else if (TYPEOF(eptr) != EXTPTRSXP)
	error(_("table entry must be an external pointer"));
    return R_ExternalPtrAddrFn(eptr);
}
