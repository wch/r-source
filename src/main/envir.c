/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2012  The R Core Team.
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
 *  http://www.r-project.org/Licenses/
 *
 *
 *
 *  Environments:
 *
 *  All the action of associating values with symbols happens
 *  in this code.  An environment is (essentially) a list of
 *  environment "frames" of the form
 *
 *	FRAME(envir) = environment frame
 *	ENCLOS(envir) = parent environment
 *	HASHTAB(envir) = (optional) hash table
 *
 *  Each frame is a (tagged) list with
 *
 *	TAG(item) = symbol
 *	CAR(item) = value bound to symbol in this frame
 *	CDR(item) = next value on the list
 *
 *  When the value of a symbol is required, the environment is
 *  traversed frame-by-frame until a value is found.
 *
 *  If a value is not found during the traversal, the symbol's
 *  "value" slot is inspected for a value.  This "top-level"
 *  environment is where system functions and variables reside.
 *
 *  Environments with the NO_SPECIAL_SYMBOLS flag set are known to not
 *  contain any special symbols, as indicated by the IS_SPECIAL_SYMBOL
 *  macro.  Lookup for such a symbol can then bypass this environment
 *  without searching it.
 */

/* R 1.8.0: namespaces are no longer experimental, so the following
 *  are no longer 'experimental options':
 *
 * EXPERIMENTAL_NAMESPACES: When this is defined the variable
 *     R_BaseNamespace holds an environment that has R_GlobalEnv as
 *     its parent.  This environment does not actually contain any
 *     bindings of its own.  Instead, it redirects all fetches and
 *     assignments to the SYMVALUE fields of the base (R_BaseEnv)
 *     environment.  If evaluation occurs in R_BaseNamespace, then
 *     base is searched before R_GlobalEnv.
 *
 * ENVIRONMENT_LOCKING: Locking an environment prevents new bindings
 *     from being created and existing bindings from being removed.
 *
 * FANCY_BINDINGS: This enables binding locking and "active bindings".
 *     When a binding is locked, its value cannot be changed.  It may
 *     still be removed from the environment if the environment is not
 *     locked.
 *
 *     Active bindings contain a function in their value cell.
 *     Getting the value of an active binding calls this function with
 *     no arguments and returns the result.  Assigning to an active
 *     binding calls this function with one argument, the new value.
 *     Active bindings may be useful for mapping external variables,
 *     such as C variables or data base entries, to R variables.  They
 *     may also be useful for making some globals thread-safe.
 *
 *     Bindings are marked as locked or active using bits 14 and 15 in
 *     their gp fields.  Since the save/load code writes out this
 *     field it means the value will be preserved across save/load.
 *     But older versions of R will interpret the entire gp field as
 *     the MISSING field, which may cause confusion.  If we keep this
 *     code, then we will need to make sure that there are no
 *     locked/active bindings in workspaces written for older versions
 *     of R to read.
 *
 * LT */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include <R_ext/Callbacks.h>

#define FAST_BASE_CACHE_LOOKUP  /* Define to enable fast lookups of symbols */
                                /*    in global cache from base environment */

#define IS_USER_DATABASE(rho)  OBJECT((rho)) && inherits((rho), "UserDefinedDatabase")

/* various definitions of macros/functions in Defn.h */

#define FRAME_LOCK_MASK (1<<14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define LOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) | FRAME_LOCK_MASK)
/*#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))*/

/* use the same bits (15 and 14) in symbols and bindings */
#define BINDING_VALUE(b) ((IS_ACTIVE_BINDING(b) ? getActiveValue(CAR(b)) : CAR(b)))

#define SYMBOL_BINDING_VALUE(s) ((IS_ACTIVE_BINDING(s) ? getActiveValue(SYMVALUE(s)) : SYMVALUE(s)))
#define SYMBOL_HAS_BINDING(s) (IS_ACTIVE_BINDING(s) || (SYMVALUE(s) != R_UnboundValue))

#define SET_BINDING_VALUE(b,val) do { \
  SEXP __b__ = (b); \
  SEXP __val__ = (val); \
  if (BINDING_IS_LOCKED(__b__)) \
    error(_("cannot change value of locked binding for '%s'"), \
	  CHAR(PRINTNAME(TAG(__b__)))); \
  if (IS_ACTIVE_BINDING(__b__)) \
    setActiveValue(CAR(__b__), __val__); \
  else \
    SETCAR(__b__, __val__); \
} while (0)

#define SET_SYMBOL_BINDING_VALUE(sym, val) do { \
  SEXP __sym__ = (sym); \
  SEXP __val__ = (val); \
  if (BINDING_IS_LOCKED(__sym__)) \
    error(_("cannot change value of locked binding for '%s'"), \
	  CHAR(PRINTNAME(__sym__))); \
  if (IS_ACTIVE_BINDING(__sym__)) \
    setActiveValue(SYMVALUE(__sym__), __val__); \
  else \
    SET_SYMVALUE(__sym__, __val__); \
} while (0)

static void setActiveValue(SEXP fun, SEXP val)
{
    SEXP arg = LCONS(R_QuoteSymbol, LCONS(val, R_NilValue));
    SEXP expr = LCONS(fun, LCONS(arg, R_NilValue));
    PROTECT(expr);
    eval(expr, R_GlobalEnv);
    UNPROTECT(1);
}

static SEXP getActiveValue(SEXP fun)
{
    SEXP expr = LCONS(fun, R_NilValue);
    PROTECT(expr);
    expr = eval(expr, R_GlobalEnv);
    UNPROTECT(1);
    return expr;
}

/* Macro version of isNull for only the test against R_NilValue */
#define ISNULL(x) ((x) == R_NilValue)

/* Function to determine whethr an environment contains special symbols */
Rboolean R_envHasNoSpecialSymbols (SEXP env)
{
    SEXP frame;

    if (HASHTAB(env) != R_NilValue)
	return FALSE;

    for (frame = FRAME(env); frame != R_NilValue; frame = CDR(frame))
        if (IS_SPECIAL_SYMBOL(TAG(frame)))
            return FALSE;

    return TRUE;
}

/*----------------------------------------------------------------------

  Hash Tables

  We use a basic separate chaining algorithm.	A hash table consists
  of SEXP (vector) which contains a number of SEXPs (lists).

  The only non-static function is R_NewHashedEnv, which allows code to
  request a hashed environment.  All others are static to allow
  internal changes of implementation without affecting client code.
*/

#define HASHSIZE(x)	     LENGTH(x)
#define HASHPRI(x)	     TRUELENGTH(x)
#define HASHTABLEGROWTHRATE  1.2
#define HASHMINSIZE	     29
#define SET_HASHPRI(x,v)     SET_TRUELENGTH(x,v)

#define IS_HASHED(x)	     (HASHTAB(x) != R_NilValue)

/*----------------------------------------------------------------------

  String Hashing

  This is taken from the second edition of the "Dragon Book" by
  Aho, Ullman and Sethi.

*/

/* was extern: used in this file and names.c (for the symbol table).

   This hash function seems to work well enough for symbol tables,
   and hash tables get saved as part of environments so changing it
   is a major decision.
 */
int attribute_hidden R_Newhashpjw(const char *s)
{
    char *p;
    unsigned h = 0, g;
    for (p = (char *) s; *p; p++) {
	h = (h << 4) + (*p);
	if ((g = h & 0xf0000000) != 0) {
	    h = h ^ (g >> 24);
	    h = h ^ g;
	}
    }
    return h;
}

/*----------------------------------------------------------------------

  R_HashSet

  Hashtable set function.  Sets 'symbol' in 'table' to be 'value'.
  'hashcode' must be provided by user.	Allocates some memory for list
  entries.

*/

static void R_HashSet(int hashcode, SEXP symbol, SEXP table, SEXP value,
		      Rboolean frame_locked)
{
    SEXP chain;

    /* Grab the chain from the hashtable */
    chain = VECTOR_ELT(table, hashcode);

    /* Search for the value in the chain */
    for (; !ISNULL(chain); chain = CDR(chain))
	if (TAG(chain) == symbol) {
	    SET_BINDING_VALUE(chain, value);
	    SET_MISSING(chain, 0);	/* Over-ride for new value */
	    return;
	}
    if (frame_locked)
	error(_("cannot add bindings to a locked environment"));
    if (ISNULL(chain))
	SET_HASHPRI(table, HASHPRI(table) + 1);
    /* Add the value into the chain */
    SET_VECTOR_ELT(table, hashcode, CONS(value, VECTOR_ELT(table, hashcode)));
    SET_TAG(VECTOR_ELT(table, hashcode), symbol);
    return;
}



/*----------------------------------------------------------------------

  R_HashGet

  Hashtable get function.  Returns 'value' from 'table' indexed by
  'symbol'.  'hashcode' must be provided by user.  Returns
  'R_UnboundValue' if value is not present.

*/

static SEXP R_HashGet(int hashcode, SEXP symbol, SEXP table)
{
    SEXP chain;

    /* Grab the chain from the hashtable */
    chain = VECTOR_ELT(table, hashcode);
    /* Retrieve the value from the chain */
    for (; chain != R_NilValue ; chain = CDR(chain))
	if (TAG(chain) == symbol) return BINDING_VALUE(chain);
    /* If not found */
    return R_UnboundValue;
}

static Rboolean R_HashExists(int hashcode, SEXP symbol, SEXP table)
{
    SEXP chain;

    /* Grab the chain from the hashtable */
    chain = VECTOR_ELT(table, hashcode);
    /* Find the binding in the chain */
    for (; chain != R_NilValue ; chain = CDR(chain))
	if (TAG(chain) == symbol) return TRUE;
    /* If not found */
    return FALSE;
}



/*----------------------------------------------------------------------

  R_HashGetLoc

  Hashtable get location function. Just like R_HashGet, but returns
  location of variable, rather than its value. Returns R_NilValue
  if not found.

*/

static SEXP R_HashGetLoc(int hashcode, SEXP symbol, SEXP table)
{
    SEXP chain;

    /* Grab the chain from the hashtable */
    chain = VECTOR_ELT(table, hashcode);
    /* Retrieve the value from the chain */
    for (; !ISNULL(chain); chain = CDR(chain))
	if (TAG(chain) == symbol) return chain;
    /* If not found */
    return R_NilValue;
}



/*----------------------------------------------------------------------

  R_NewHashTable

  Hash table initialisation function.  Creates a table of size 'size'
  that increases in size by 'growth_rate' after a threshold is met.

*/

static SEXP R_NewHashTable(int size)
{
    SEXP table;

    if (size <= 0) size = HASHMINSIZE;

    /* Allocate hash table in the form of a vector */
    PROTECT(table = allocVector(VECSXP, size));
    SET_HASHPRI(table, 0);
    UNPROTECT(1);
    return(table);
}

/*----------------------------------------------------------------------

  R_NewHashedEnv

  Returns a new environment with a hash table initialized with default
  size.  The only non-static hash table function.
*/

SEXP R_NewHashedEnv(SEXP enclos, SEXP size)
{
    SEXP s;

    PROTECT(enclos);
    PROTECT(size);
    PROTECT(s = NewEnvironment(R_NilValue, R_NilValue, enclos));
    SET_HASHTAB(s, R_NewHashTable(asInteger(size)));
    UNPROTECT(3);
    return s;
}


/*----------------------------------------------------------------------

  R_HashDelete

  Hash table delete function.  Symbols are not removed from the table.
  They have their value set to 'R_UnboundValue'.

*/

static SEXP DeleteItem(SEXP symbol, SEXP lst)
{
    if (lst != R_NilValue) {
	SETCDR(lst, DeleteItem(symbol, CDR(lst)));
	if (TAG(lst) == symbol) {
	    SETCAR(lst, R_UnboundValue); /* in case binding is cached */
	    LOCK_BINDING(lst);           /* in case binding is cached */
	    lst = CDR(lst);
	}
    }
    return lst;
}

static void R_HashDelete(int hashcode, SEXP symbol, SEXP table)
{
    SEXP list = DeleteItem(symbol,
			   VECTOR_ELT(table, hashcode % HASHSIZE(table)));
    if (list == R_NilValue)
	SET_HASHPRI(table, HASHPRI(table) - 1);
    SET_VECTOR_ELT(table, hashcode % HASHSIZE(table), list);
    return;
}



/*----------------------------------------------------------------------

  R_HashResize

  Hash table resizing function Increase the size of the hash table by
  the growth_rate of the table.	 The vector is reallocated, however
  the lists with in the hash table have their pointers shuffled around
  so that they are not reallocated.

*/

static SEXP R_HashResize(SEXP table)
{
    SEXP new_table, chain, new_chain, tmp_chain;
    int counter, new_hashcode;

    /* Do some checking */
    if (TYPEOF(table) != VECSXP)
	error("first argument ('table') not of type VECSXP, from R_HashResize");

    /* This may have to change.	 The growth rate should
       be independent of the size (not implemented yet) */
    /* hash_grow = HASHSIZE(table); */

    /* Allocate the new hash table */
    new_table = R_NewHashTable((int)(HASHSIZE(table) * HASHTABLEGROWTHRATE));
    for (counter = 0; counter < length(table); counter++) {
	chain = VECTOR_ELT(table, counter);
	while (!ISNULL(chain)) {
	    new_hashcode = R_Newhashpjw(CHAR(PRINTNAME(TAG(chain)))) %
		HASHSIZE(new_table);
	    new_chain = VECTOR_ELT(new_table, new_hashcode);
	    /* If using a primary slot then increase HASHPRI */
	    if (ISNULL(new_chain))
		SET_HASHPRI(new_table, HASHPRI(new_table) + 1);
	    tmp_chain = chain;
	    chain = CDR(chain);
	    SETCDR(tmp_chain, new_chain);
	    SET_VECTOR_ELT(new_table, new_hashcode,  tmp_chain);
#ifdef MIKE_DEBUG
	    fprintf(stdout, "HASHSIZE = %d\nHASHPRI = %d\ncounter = %d\nHASHCODE = %d\n",
		    HASHSIZE(table), HASHPRI(table), counter, new_hashcode);
#endif
	}
    }
    /* Some debugging statements */
#ifdef MIKE_DEBUG
    fprintf(stdout, "Resized O.K.\n");
    fprintf(stdout, "Old size: %d, New size: %d\n",
	    HASHSIZE(table), HASHSIZE(new_table));
    fprintf(stdout, "Old pri: %d, New pri: %d\n",
	    HASHPRI(table), HASHPRI(new_table));
#endif
    return new_table;
} /* end R_HashResize */



/*----------------------------------------------------------------------

  R_HashSizeCheck

  Hash table size rechecking function.	Compares the load factor
  (size/# of primary slots used)  to a particular threshhold value.
  Returns true if the table needs to be resized.

*/

static int R_HashSizeCheck(SEXP table)
{
    int resize;
    double thresh_val;

    /* Do some checking */
    if (TYPEOF(table) != VECSXP)
	error("first argument ('table') not of type VECSXP, R_HashSizeCheck");
    resize = 0; thresh_val = 0.85;
    if ((double)HASHPRI(table) > (double)HASHSIZE(table) * thresh_val)
	resize = 1;
    return resize;
}



/*----------------------------------------------------------------------

  R_HashFrame

  Hashing for environment frames.  This function ensures that the
  first frame in the given environment has been hashed.	 Ultimately
  all enironments should be created in hashed form.  At that point
  this function will be redundant.

*/

static SEXP R_HashFrame(SEXP rho)
{
    int hashcode;
    SEXP frame, chain, tmp_chain, table;

    /* Do some checking */
    if (TYPEOF(rho) != ENVSXP)
	error("first argument ('table') not of type ENVSXP, from R_HashVector2Hash");
    table = HASHTAB(rho);
    frame = FRAME(rho);
    while (!ISNULL(frame)) {
	if( !HASHASH(PRINTNAME(TAG(frame))) ) {
	    SET_HASHVALUE(PRINTNAME(TAG(frame)),
			  R_Newhashpjw(CHAR(PRINTNAME(TAG(frame)))));
	    SET_HASHASH(PRINTNAME(TAG(frame)), 1);
	}
	hashcode = HASHVALUE(PRINTNAME(TAG(frame))) % HASHSIZE(table);
	chain = VECTOR_ELT(table, hashcode);
	/* If using a primary slot then increase HASHPRI */
	if (ISNULL(chain)) SET_HASHPRI(table, HASHPRI(table) + 1);
	tmp_chain = frame;
	frame = CDR(frame);
	SETCDR(tmp_chain, chain);
	SET_VECTOR_ELT(table, hashcode, tmp_chain);
    }
    SET_FRAME(rho, R_NilValue);
    return rho;
}


/* ---------------------------------------------------------------------

   R_HashProfile

   Profiling tool for analyzing hash table performance.  Returns a
   three element list with components:

   size: the total size of the hash table

   nchains: the number of non-null chains in the table (as reported by
	    HASHPRI())

   counts: an integer vector the same length as size giving the length of
	   each chain (or zero if no chain is present).  This allows
	   for assessing collisions in the hash table.
 */

static SEXP R_HashProfile(SEXP table)
{
    SEXP chain, ans, chain_counts, nms;
    int i, count;

    PROTECT(ans = allocVector(VECSXP, 3));
    PROTECT(nms = allocVector(STRSXP, 3));
    SET_STRING_ELT(nms, 0, mkChar("size"));    /* size of hashtable */
    SET_STRING_ELT(nms, 1, mkChar("nchains")); /* number of non-null chains */
    SET_STRING_ELT(nms, 2, mkChar("counts"));  /* length of each chain */
    setAttrib(ans, R_NamesSymbol, nms);
    UNPROTECT(1);

    SET_VECTOR_ELT(ans, 0, ScalarInteger(length(table)));
    SET_VECTOR_ELT(ans, 1, ScalarInteger(HASHPRI(table)));

    PROTECT(chain_counts = allocVector(INTSXP, length(table)));
    for (i = 0; i < length(table); i++) {
	chain = VECTOR_ELT(table, i);
	count = 0;
	for (; chain != R_NilValue ; chain = CDR(chain)) {
	    count++;
	}
	INTEGER(chain_counts)[i] = count;
    }

    SET_VECTOR_ELT(ans, 2, chain_counts);

    UNPROTECT(2);
    return ans;
}



/*----------------------------------------------------------------------

  Environments

  The following code implements variable searching for environments.

*/


/*----------------------------------------------------------------------

  InitGlobalEnv

  Create the initial global environment.  The global environment is
  no longer a linked list of environment frames.  Instead it is a
  vector of environments which is searched from beginning to end.

  Note that only the first frame of each of these environments is
  searched.  This is intended to make it possible to implement
  namespaces at some (indeterminate) point in the future.

  We hash the initial environment.  100 is a magic number discovered
  by Ross.  Change it if you feel inclined.

*/

#define USE_GLOBAL_CACHE
#ifdef USE_GLOBAL_CACHE  /* NB leave in place: see below */
/* Global variable caching.  A cache is maintained in a hash table,
   R_GlobalCache.  The entry values are either R_UnboundValue (a
   flushed cache entry), the binding LISTSXP cell from the environment
   containing the binding found in a search from R_GlobalEnv, or a
   symbol if the globally visible binding lives in the base package.
   The cache for a variable is flushed if a new binding for it is
   created in a global frame or if the variable is removed from any
   global frame.

   Symbols in the global cache with values from the base environment
   are flagged with BASE_SYM_CACHED, so that their value can be
   returned immediately without needing to look in the hash table.
   They must still have entries in the hash table, however, so that
   they can be flushed as needed.

   To make sure the cache is valid, all binding creations and removals
   from global frames must go through the interface functions in this
   file.

   Initially only the R_GlobalEnv frame is a global frame.  Additional
   global frames can only be created by attach.  All other frames are
   considered local.  Whether a frame is local or not is recorded in
   the highest order bit of the ENVFLAGS field (the gp field of
   sxpinfo).

   It is possible that the benefit of caching may be significantly
   reduced if we introduce namespace management.  Since maintaining
   cache integrity is a bit tricky and since it might complicate
   threading a bit (I'm not sure it will but it needs to be thought
   through if nothing else) it might make sense to remove caching at
   that time.  To make that easier, the ifdef's should probably be
   left in place.

   L. T. */

#define GLOBAL_FRAME_MASK (1<<15)
#define IS_GLOBAL_FRAME(e) (ENVFLAGS(e) & GLOBAL_FRAME_MASK)
#define MARK_AS_GLOBAL_FRAME(e) \
  SET_ENVFLAGS(e, ENVFLAGS(e) | GLOBAL_FRAME_MASK)
#define MARK_AS_LOCAL_FRAME(e) \
  SET_ENVFLAGS(e, ENVFLAGS(e) & (~ GLOBAL_FRAME_MASK))

#define INITIAL_CACHE_SIZE 1000

static SEXP R_GlobalCache, R_GlobalCachePreserve;
#endif
static SEXP R_BaseNamespaceName;
static SEXP R_NamespaceSymbol;

void attribute_hidden InitBaseEnv()
{
    R_EmptyEnv = NewEnvironment(R_NilValue, R_NilValue, R_NilValue);
    R_BaseEnv = NewEnvironment(R_NilValue, R_NilValue, R_EmptyEnv);
}

void attribute_hidden InitGlobalEnv()
{
    R_NamespaceSymbol = install(".__NAMESPACE__.");

    R_GlobalEnv = R_NewHashedEnv(R_BaseEnv, ScalarInteger(0));
    R_MethodsNamespace = R_GlobalEnv; // so it is initialized.
#ifdef NEW_CODE /* Not used */
    HASHTAB(R_GlobalEnv) = R_NewHashTable(100);
#endif
#ifdef USE_GLOBAL_CACHE
    MARK_AS_GLOBAL_FRAME(R_GlobalEnv);
    R_GlobalCache = R_NewHashTable(INITIAL_CACHE_SIZE);
    R_GlobalCachePreserve = CONS(R_GlobalCache, R_NilValue);
    R_PreserveObject(R_GlobalCachePreserve);
#endif
    R_BaseNamespace = NewEnvironment(R_NilValue, R_NilValue, R_GlobalEnv);
    R_PreserveObject(R_BaseNamespace);
    SET_SYMVALUE(install(".BaseNamespaceEnv"), R_BaseNamespace);
    R_BaseNamespaceName = ScalarString(mkChar("base"));
    R_PreserveObject(R_BaseNamespaceName);
    R_NamespaceRegistry = R_NewHashedEnv(R_NilValue, ScalarInteger(0));
    R_PreserveObject(R_NamespaceRegistry);
    defineVar(R_baseSymbol, R_BaseNamespace, R_NamespaceRegistry);
    /**** needed to properly initialize the base namespace */
}

#ifdef USE_GLOBAL_CACHE
static int hashIndex(SEXP symbol, SEXP table)
{
    SEXP c = PRINTNAME(symbol);
    if( !HASHASH(c) ) {
	SET_HASHVALUE(c, R_Newhashpjw(CHAR(c)));
	SET_HASHASH(c, 1);
    }
    return HASHVALUE(c) % HASHSIZE(table);
}

static void R_FlushGlobalCache(SEXP sym)
{
    SEXP entry = R_HashGetLoc(hashIndex(sym, R_GlobalCache), sym,
			      R_GlobalCache);
    if (entry != R_NilValue) {
	SETCAR(entry, R_UnboundValue);
#ifdef FAST_BASE_CACHE_LOOKUP
        UNSET_BASE_SYM_CACHED(sym);
#endif
    }
}

static void R_FlushGlobalCacheFromTable(SEXP table)
{
    int i, size;
    SEXP chain;
    size = HASHSIZE(table);
    for (i = 0; i < size; i++) {
	for (chain = VECTOR_ELT(table, i); chain != R_NilValue; chain = CDR(chain))
	    R_FlushGlobalCache(TAG(chain));
    }
}

/**
 Flush the cache based on the names provided by the user defined
 table, specifically returned from calling objects() for that
 table.
 */
static void R_FlushGlobalCacheFromUserTable(SEXP udb)
{
    int n, i;
    R_ObjectTable *tb;
    SEXP names;
    tb = (R_ObjectTable*) R_ExternalPtrAddr(udb);
    names = tb->objects(tb);
    n = length(names);
    for(i = 0; i < n ; i++)
	R_FlushGlobalCache(Rf_installChar(STRING_ELT(names,i)));
}

static void R_AddGlobalCache(SEXP symbol, SEXP place)
{
    int oldpri = HASHPRI(R_GlobalCache);
    R_HashSet(hashIndex(symbol, R_GlobalCache), symbol, R_GlobalCache, place,
	      FALSE);
#ifdef FAST_BASE_CACHE_LOOKUP
    if (symbol == place)
	SET_BASE_SYM_CACHED(symbol);
    else
	UNSET_BASE_SYM_CACHED(symbol);
#endif
    if (oldpri != HASHPRI(R_GlobalCache) &&
	HASHPRI(R_GlobalCache) > 0.85 * HASHSIZE(R_GlobalCache)) {
	R_GlobalCache = R_HashResize(R_GlobalCache);
	SETCAR(R_GlobalCachePreserve, R_GlobalCache);
    }
}

static SEXP R_GetGlobalCache(SEXP symbol)
{
    SEXP vl;

#ifdef FAST_BASE_CACHE_LOOKUP
    if (BASE_SYM_CACHED(symbol))
        return SYMBOL_BINDING_VALUE(symbol);
#endif

    vl = R_HashGet(hashIndex(symbol, R_GlobalCache), symbol,
			R_GlobalCache);
    switch(TYPEOF(vl)) {
    case SYMSXP:
	if (vl == R_UnboundValue) /* avoid test?? */
	    return R_UnboundValue;
	else return SYMBOL_BINDING_VALUE(vl);
    case LISTSXP:
	return BINDING_VALUE(vl);
    default:
	error(_("invalid cached value in R_GetGlobalCache"));
	return R_NilValue;
    }
}
#endif /* USE_GLOBAL_CACHE */

/*----------------------------------------------------------------------

  unbindVar

  Remove a value from an environment. This happens only in the frame
  of the specified environment.

  FIXME ? should this also unbind the symbol value slot when rho is
  R_BaseEnv.
  This is only called from eval.c in applydefine and bcEval
  (and applydefine only works for unhashed environments, so not base).
*/

static SEXP RemoveFromList(SEXP thing, SEXP list, int *found)
{
    if (list == R_NilValue) {
	*found = 0;
	return R_NilValue;
    }
    else if (TAG(list) == thing) {
	*found = 1;
	SETCAR(list, R_UnboundValue); /* in case binding is cached */
	LOCK_BINDING(list);           /* in case binding is cached */
	SEXP rest = CDR(list);
	SETCDR(list, R_NilValue);     /* to fix refcnt on 'rest' */
	return rest;
    }
    else {
	SEXP last = list;
	SEXP next = CDR(list);
	while (next != R_NilValue) {
	    if (TAG(next) == thing) {
		*found = 1;
		SETCAR(next, R_UnboundValue); /* in case binding is cached */
		LOCK_BINDING(next);           /* in case binding is cached */
		SETCDR(last, CDR(next));
		SETCDR(next, R_NilValue);     /* to fix refcnt on 'list' */
		return list;
	    }
	    else {
		last = next;
		next = CDR(next);
	    }
	}
	*found = 0;
	return list;
    }
}

void attribute_hidden unbindVar(SEXP symbol, SEXP rho)
{
    int hashcode;
    SEXP c;

    if (rho == R_BaseNamespace)
	error(_("cannot unbind in the base namespace"));
    if (rho == R_BaseEnv)
	error(_("unbind in the base environment is unimplemented"));
    if (FRAME_IS_LOCKED(rho))
	error(_("cannot remove bindings from a locked environment"));
#ifdef USE_GLOBAL_CACHE
    if (IS_GLOBAL_FRAME(rho))
	R_FlushGlobalCache(symbol);
#endif
    if (HASHTAB(rho) == R_NilValue) {
	int found;
	SEXP list;
	list = RemoveFromList(symbol, FRAME(rho), &found);
	if (found) {
	    if (rho == R_GlobalEnv) R_DirtyImage = 1;
	    SET_FRAME(rho, list);
	}
    }
    else {
	/* This case is currently unused */
	c = PRINTNAME(symbol);
	if( !HASHASH(c) ) {
	    SET_HASHVALUE(c, R_Newhashpjw(CHAR(c)));
	    SET_HASHASH(c, 1);
	}
	hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	R_HashDelete(hashcode, symbol, HASHTAB(rho));
	/* we have no record here if deletion worked */
	if (rho == R_GlobalEnv) R_DirtyImage = 1;
    }
}



/*----------------------------------------------------------------------

  findVarLocInFrame

  Look up the location of the value of a symbol in a
  single environment frame.  Almost like findVarInFrame, but
  does not return the value. R_NilValue if not found.

  Callers set *canCache = TRUE or NULL
*/

static SEXP findVarLocInFrame(SEXP rho, SEXP symbol, Rboolean *canCache)
{
    int hashcode;
    SEXP frame, c;

    if (rho == R_BaseEnv || rho == R_BaseNamespace) {
	error("'findVarLocInFrame' cannot be used on the base environment");
	/* the code below doesn't really make sense as it returns the
	   value, not the binding.  We _could_ return the symbol as
	   the binding object in that case, but it isn't clear that
	   would be useful. LT */
	c = SYMBOL_BINDING_VALUE(symbol);
	return (c == R_UnboundValue) ? R_NilValue : c;
    }

    if (rho == R_EmptyEnv)
	return(R_NilValue);

    if(IS_USER_DATABASE(rho)) {
	R_ObjectTable *table;
	SEXP val, tmp = R_NilValue;
	table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(rho));
	/* Better to use exists() here if we don't actually need the value! */
	val = table->get(CHAR(PRINTNAME(symbol)), canCache, table);
	if(val != R_UnboundValue) {
	    /* The result should probably be identified as being from
	       a user database, or maybe use an active binding
	       mechanism to allow setting a new value to get back to
	       the data base. */
	    tmp = allocSExp(LISTSXP);
	    SETCAR(tmp, val);
	    SET_TAG(tmp, symbol);
	    /* If the database has a canCache method, then call that.
	       Otherwise, we believe the setting for canCache. */
	    if(canCache && table->canCache)
		*canCache = table->canCache(CHAR(PRINTNAME(symbol)), table);
	}
	return(tmp);
    }

    if (HASHTAB(rho) == R_NilValue) {
	frame = FRAME(rho);
	while (frame != R_NilValue && TAG(frame) != symbol)
	    frame = CDR(frame);
	return frame;
    }
    else {
	c = PRINTNAME(symbol);
	if( !HASHASH(c) ) {
	    SET_HASHVALUE(c, R_Newhashpjw(CHAR(c)));
	    SET_HASHASH(c,  1);
	}
	hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	/* Will return 'R_NilValue' if not found */
	return R_HashGetLoc(hashcode, symbol, HASHTAB(rho));
    }
}


/*
  External version and accessor functions. Returned value is cast as
  an opaque pointer to insure it is only used by routines in this
  group.  This allows the implementation to be changed without needing
  to change other files.
*/

R_varloc_t R_findVarLocInFrame(SEXP rho, SEXP symbol)
{
    SEXP binding = findVarLocInFrame(rho, symbol, NULL);
    return binding == R_NilValue ? NULL : (R_varloc_t) binding;
}

attribute_hidden
SEXP R_GetVarLocValue(R_varloc_t vl)
{
    return BINDING_VALUE((SEXP) vl);
}

attribute_hidden
SEXP R_GetVarLocSymbol(R_varloc_t vl)
{
    return TAG((SEXP) vl);
}

/* used in methods */
Rboolean R_GetVarLocMISSING(R_varloc_t vl)
{
    return MISSING((SEXP) vl);
}

attribute_hidden
void R_SetVarLocValue(R_varloc_t vl, SEXP value)
{
    SET_BINDING_VALUE((SEXP) vl, value);
}


/*----------------------------------------------------------------------

  findVarInFrame

  Look up the value of a symbol in a single environment frame.	This
  is the basic building block of all variable lookups.

  It is important that this be as efficient as possible.

  The final argument is usually TRUE and indicates whether the
  lookup is being done in order to get the value (TRUE) or
  simply to check whether there is a value bound to the specified
  symbol in this frame (FALSE).  This is used for get() and exists().
*/

SEXP findVarInFrame3(SEXP rho, SEXP symbol, Rboolean doGet)
{
    int hashcode;
    SEXP frame, c;

    if (TYPEOF(rho) == NILSXP)
	error(_("use of NULL environment is defunct"));

    if (rho == R_BaseNamespace || rho == R_BaseEnv)
	return SYMBOL_BINDING_VALUE(symbol);

    if (rho == R_EmptyEnv)
	return R_UnboundValue;

    if(IS_USER_DATABASE(rho)) {
	/* Use the objects function pointer for this symbol. */
	R_ObjectTable *table;
	SEXP val = R_UnboundValue;
	table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(rho));
	if(table->active) {
	    if(doGet)
		val = table->get(CHAR(PRINTNAME(symbol)), NULL, table);
	    else {
		if(table->exists(CHAR(PRINTNAME(symbol)), NULL, table))
		    val = table->get(CHAR(PRINTNAME(symbol)), NULL, table);
		else
		    val = R_UnboundValue;
	    }
	}
	return(val);
    } else if (HASHTAB(rho) == R_NilValue) {
	frame = FRAME(rho);
	while (frame != R_NilValue) {
	    if (TAG(frame) == symbol)
		return BINDING_VALUE(frame);
	    frame = CDR(frame);
	}
    }
    else {
	c = PRINTNAME(symbol);
	if( !HASHASH(c) ) {
	    SET_HASHVALUE(c, R_Newhashpjw(CHAR(c)));
	    SET_HASHASH(c, 1);
	}
	hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	/* Will return 'R_UnboundValue' if not found */
	return(R_HashGet(hashcode, symbol, HASHTAB(rho)));
    }
    return R_UnboundValue;
}

/* This variant of findVarinFrame3 is needed to avoid running active
   binding functions in calls to exists() with mode = "any" */
static Rboolean existsVarInFrame(SEXP rho, SEXP symbol)
{
    int hashcode;
    SEXP frame, c;

    if (TYPEOF(rho) == NILSXP)
	error(_("use of NULL environment is defunct"));

    if (rho == R_BaseNamespace || rho == R_BaseEnv)
	return SYMBOL_HAS_BINDING(symbol);

    if (rho == R_EmptyEnv)
	return FALSE;

    if(IS_USER_DATABASE(rho)) {
	/* Use the objects function pointer for this symbol. */
	R_ObjectTable *table;
	Rboolean val = FALSE;
	table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(rho));
	if(table->active) {
	    if(table->exists(CHAR(PRINTNAME(symbol)), NULL, table))
		val = TRUE;
	    else
		val = FALSE;
	}
	return(val);
    } else if (HASHTAB(rho) == R_NilValue) {
	frame = FRAME(rho);
	while (frame != R_NilValue) {
	    if (TAG(frame) == symbol)
		return TRUE;
	    frame = CDR(frame);
	}
    }
    else {
	c = PRINTNAME(symbol);
	if( !HASHASH(c) ) {
	    SET_HASHVALUE(c, R_Newhashpjw(CHAR(c)));
	    SET_HASHASH(c, 1);
	}
	hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	/* Will return 'R_UnboundValue' if not found */
	return R_HashExists(hashcode, symbol, HASHTAB(rho));
    }
    return FALSE;
}

SEXP findVarInFrame(SEXP rho, SEXP symbol)
{
    return findVarInFrame3(rho, symbol, TRUE);
}


/*----------------------------------------------------------------------

  findVar

  Look up a symbol in an environment.

*/

#ifdef USE_GLOBAL_CACHE
/* findGlobalVar searches for a symbol value starting at R_GlobalEnv,
   so the cache can be used. */
static SEXP findGlobalVar(SEXP symbol)
{
    SEXP vl, rho;
    Rboolean canCache = TRUE;
    vl = R_GetGlobalCache(symbol);
    if (vl != R_UnboundValue)
	return vl;
    for (rho = R_GlobalEnv; rho != R_EmptyEnv; rho = ENCLOS(rho)) {
	if (rho != R_BaseEnv) { /* we won't have R_BaseNamespace */
	    vl = findVarLocInFrame(rho, symbol, &canCache);
	    if (vl != R_NilValue) {
		if(canCache)
		    R_AddGlobalCache(symbol, vl);
		return BINDING_VALUE(vl);
	    }
	} else {
	    vl = SYMBOL_BINDING_VALUE(symbol);
	    if (vl != R_UnboundValue)
		R_AddGlobalCache(symbol, symbol);
	    return vl;
	}

    }
    return R_UnboundValue;
}
#endif

SEXP findVar(SEXP symbol, SEXP rho)
{
    SEXP vl;

    if (TYPEOF(rho) == NILSXP)
	error(_("use of NULL environment is defunct"));

    if (!isEnvironment(rho))
	error(_("argument to '%s' is not an environment"), "findVar");

#ifdef USE_GLOBAL_CACHE
    /* This first loop handles local frames, if there are any.  It
       will also handle all frames if rho is a global frame other than
       R_GlobalEnv */
    while (rho != R_GlobalEnv && rho != R_EmptyEnv) {
	vl = findVarInFrame3(rho, symbol, TRUE /* get rather than exists */);
	if (vl != R_UnboundValue) return (vl);
	rho = ENCLOS(rho);
    }
    if (rho == R_GlobalEnv)
	return findGlobalVar(symbol);
    else
	return R_UnboundValue;
#else
    while (rho != R_EmptyEnv) {
	vl = findVarInFrame3(rho, symbol, TRUE);
	if (vl != R_UnboundValue) return (vl);
	rho = ENCLOS(rho);
    }
    return R_UnboundValue;
#endif
}



/*----------------------------------------------------------------------

  findVar1

  Look up a symbol in an environment.  Ignore any values which are
  not of the specified type.

*/

SEXP attribute_hidden
findVar1(SEXP symbol, SEXP rho, SEXPTYPE mode, int inherits)
{
    SEXP vl;
    while (rho != R_EmptyEnv) {
	vl = findVarInFrame3(rho, symbol, TRUE);
	if (vl != R_UnboundValue) {
	    if (mode == ANYSXP) return vl;
	    if (TYPEOF(vl) == PROMSXP) {
		PROTECT(vl);
		vl = eval(vl, rho);
		UNPROTECT(1);
	    }
	    if (TYPEOF(vl) == mode) return vl;
	    if (mode == FUNSXP && (TYPEOF(vl) == CLOSXP ||
				   TYPEOF(vl) == BUILTINSXP ||
				   TYPEOF(vl) == SPECIALSXP))
		return (vl);
	}
	if (inherits)
	    rho = ENCLOS(rho);
	else
	    return (R_UnboundValue);
    }
    return (R_UnboundValue);
}

/*
 *  ditto, but check *mode* not *type*
 */

static SEXP
findVar1mode(SEXP symbol, SEXP rho, SEXPTYPE mode, int inherits,
	     Rboolean doGet)
{
    SEXP vl;
    int tl;
    if (mode == INTSXP) mode = REALSXP;
    if (mode == FUNSXP || mode ==  BUILTINSXP || mode == SPECIALSXP)
	mode = CLOSXP;
    while (rho != R_EmptyEnv) {
	if (! doGet && mode == ANYSXP)
	    vl = existsVarInFrame(rho, symbol) ? R_NilValue : R_UnboundValue;
	else
	    vl = findVarInFrame3(rho, symbol, doGet);

	if (vl != R_UnboundValue) {
	    if (mode == ANYSXP) return vl;
	    if (TYPEOF(vl) == PROMSXP) {
		PROTECT(vl);
		vl = eval(vl, rho);
		UNPROTECT(1);
	    }
	    tl = TYPEOF(vl);
	    if (tl == INTSXP) tl = REALSXP;
	    if (tl == FUNSXP || tl ==  BUILTINSXP || tl == SPECIALSXP)
		tl = CLOSXP;
	    if (tl == mode) return vl;
	}
	if (inherits)
	    rho = ENCLOS(rho);
	else
	    return (R_UnboundValue);
    }
    return (R_UnboundValue);
}


/*
   ddVal:
   a function to take a name and determine if it is of the form
   ..x where x is an integer; if so x is returned otherwise 0 is returned
*/
static int ddVal(SEXP symbol)
{
    const char *buf;
    char *endp;
    int rval;

    buf = CHAR(PRINTNAME(symbol));
    if( !strncmp(buf,"..",2) && strlen(buf) > 2 ) {
	buf += 2;
	rval = (int) strtol(buf, &endp, 10);
	if( *endp != '\0')
	    return 0;
	else
	    return rval;
    }
    return 0;
}

/*----------------------------------------------------------------------
  ddfindVar

  This function fetches the variables ..1, ..2, etc from the first
  frame of the environment passed as the second argument to ddfindVar.
  These variables are implicitly defined whenever a ... object is
  created.

  To determine values for the variables we first search for an
  explicit definition of the symbol, them we look for a ... object in
  the frame and then walk through it to find the appropriate values.

  If no value is obtained we return R_UnboundValue.

  It is an error to specify a .. index longer than the length of the
  ... object the value is sought in.

*/

attribute_hidden
SEXP ddfindVar(SEXP symbol, SEXP rho)
{
    int i;
    SEXP vl;

    /* first look for ... symbol  */
    vl = findVar(R_DotsSymbol, rho);
    i = ddVal(symbol);
    if (vl != R_UnboundValue) {
	if (length(vl) >= i) {
	    vl = nthcdr(vl, i - 1);
	    return(CAR(vl));
	}
	else
	    error(_("the ... list does not contain %d elements"), i);
    }
    else error(_("..%d used in an incorrect context, no ... to look in"), i);

    return R_NilValue;
}



/*----------------------------------------------------------------------

  dynamicfindVar

  This function does a variable lookup, but uses dynamic scoping rules
  rather than the lexical scoping rules used in findVar.

  Return R_UnboundValue if the symbol isn't located and the calling
  function needs to handle the errors.

*/

#ifdef UNUSED
SEXP dynamicfindVar(SEXP symbol, RCNTXT *cptr)
{
    SEXP vl;
    while (cptr != R_ToplevelContext) {
	if (cptr->callflag & CTXT_FUNCTION) {
	    vl = findVarInFrame3(cptr->cloenv, symbol, TRUE);
	    if (vl != R_UnboundValue) return vl;
	}
	cptr = cptr->nextcontext;
    }
    return R_UnboundValue;
}
#endif



/*----------------------------------------------------------------------

  findFun

  Search for a function in an environment This is a specially modified
  version of findVar which ignores values its finds if they are not
  functions.

 [ NEEDED: This needs to be modified so that a search for an arbitrary mode can
  be made.  Then findVar and findFun could become same function.]

  This could call findVar1.  NB: they behave differently on failure.
*/

SEXP findFun(SEXP symbol, SEXP rho)
{
    SEXP vl;

    /* If the symbol is marked as special, skip to the first
       environment that might contain such a symbol. */
    if (IS_SPECIAL_SYMBOL(symbol)) {
        while (rho != R_EmptyEnv && NO_SPECIAL_SYMBOLS(rho))
            rho = ENCLOS(rho);
    }

    while (rho != R_EmptyEnv) {
	/* This is not really right.  Any variable can mask a function */
#ifdef USE_GLOBAL_CACHE
	if (rho == R_GlobalEnv)
#ifdef FAST_BASE_CACHE_LOOKUP
            if (BASE_SYM_CACHED(symbol))
                vl = SYMBOL_BINDING_VALUE(symbol);
            else
                vl = findGlobalVar(symbol);
#else
	    vl = findGlobalVar(symbol);
#endif
	else
	    vl = findVarInFrame3(rho, symbol, TRUE);
#else
	vl = findVarInFrame3(rho, symbol, TRUE);
#endif
	if (vl != R_UnboundValue) {
	    if (TYPEOF(vl) == PROMSXP) {
		PROTECT(vl);
		vl = eval(vl, rho);
		UNPROTECT(1);
	    }
	    if (TYPEOF(vl) == CLOSXP || TYPEOF(vl) == BUILTINSXP ||
		TYPEOF(vl) == SPECIALSXP)
		return (vl);
	    if (vl == R_MissingArg)
		error(_("argument \"%s\" is missing, with no default"),
		      CHAR(PRINTNAME(symbol)));
	}
	rho = ENCLOS(rho);
    }
    error(_("could not find function \"%s\""), EncodeChar(PRINTNAME(symbol)));
    /* NOT REACHED */
    return R_UnboundValue;
}


/*----------------------------------------------------------------------

  defineVar

  Assign a value in a specific environment frame.

*/

void defineVar(SEXP symbol, SEXP value, SEXP rho)
{
    int hashcode;
    SEXP frame, c;

    /* R_DirtyImage should only be set if assigning to R_GlobalEnv. */
    if (rho == R_GlobalEnv) R_DirtyImage = 1;

    if (rho == R_EmptyEnv)
	error(_("cannot assign values in the empty environment"));

    if(IS_USER_DATABASE(rho)) {
	R_ObjectTable *table;
	table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(rho));
	if(table->assign == NULL)
	    error(_("cannot assign variables to this database"));
	PROTECT(value);
	table->assign(CHAR(PRINTNAME(symbol)), value, table);
	UNPROTECT(1);
#ifdef USE_GLOBAL_CACHE
	if (IS_GLOBAL_FRAME(rho)) R_FlushGlobalCache(symbol);
#endif
	return;
    }

    if (rho == R_BaseNamespace || rho == R_BaseEnv) {
	gsetVar(symbol, value, rho);
    } else {
#ifdef USE_GLOBAL_CACHE
	if (IS_GLOBAL_FRAME(rho)) R_FlushGlobalCache(symbol);
#endif

        if (IS_SPECIAL_SYMBOL(symbol))
            UNSET_NO_SPECIAL_SYMBOLS(rho);

	if (HASHTAB(rho) == R_NilValue) {
	    /* First check for an existing binding */
	    frame = FRAME(rho);
	    while (frame != R_NilValue) {
		if (TAG(frame) == symbol) {
		    SET_BINDING_VALUE(frame, value);
		    SET_MISSING(frame, 0);	/* Over-ride */
		    return;
		}
		frame = CDR(frame);
	    }
	    if (FRAME_IS_LOCKED(rho))
		error(_("cannot add bindings to a locked environment"));
	    SET_FRAME(rho, CONS(value, FRAME(rho)));
	    SET_TAG(FRAME(rho), symbol);
	}
	else {
	    c = PRINTNAME(symbol);
	    if( !HASHASH(c) ) {
		SET_HASHVALUE(c, R_Newhashpjw(CHAR(c)));
		SET_HASHASH(c, 1);
	    }
	    hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	    R_HashSet(hashcode, symbol, HASHTAB(rho), value,
		      FRAME_IS_LOCKED(rho));
	    if (R_HashSizeCheck(HASHTAB(rho)))
		SET_HASHTAB(rho, R_HashResize(HASHTAB(rho)));
	}
    }
}

/*----------------------------------------------------------------------

  setVarInFrame

  Assign a new value to an existing symbol in a frame.
  Return the symbol if successful and R_NilValue if not.

  [ Taken static in 2.4.0: not called for emptyenv or baseenv. ]
*/

static SEXP setVarInFrame(SEXP rho, SEXP symbol, SEXP value)
{
    int hashcode;
    SEXP frame, c;

    /* R_DirtyImage should only be set if assigning to R_GlobalEnv. */
    if (rho == R_GlobalEnv) R_DirtyImage = 1;
    if (rho == R_EmptyEnv) return R_NilValue;

    if(IS_USER_DATABASE(rho)) {
	/* FIXME: This does not behave as described */
	R_ObjectTable *table;
	table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(rho));
	if(table->assign == NULL)
	    error(_("cannot assign variables to this database"));
	PROTECT(value);
	SEXP result = table->assign(CHAR(PRINTNAME(symbol)), value, table);
	UNPROTECT(1);
	return(result);
    }

    if (rho == R_BaseNamespace || rho == R_BaseEnv) {
	if (SYMVALUE(symbol) == R_UnboundValue) return R_NilValue;
	SET_SYMBOL_BINDING_VALUE(symbol, value);
	return symbol;
    }

    if (HASHTAB(rho) == R_NilValue) {
	frame = FRAME(rho);
	while (frame != R_NilValue) {
	    if (TAG(frame) == symbol) {
		if (rho == R_GlobalEnv) R_DirtyImage = 1;
		SET_BINDING_VALUE(frame, value);
		SET_MISSING(frame, 0);	/* same as defineVar */
		return symbol;
	    }
	    frame = CDR(frame);
	}
    } else {
	/* Do the hash table thing */
	c = PRINTNAME(symbol);
	if( !HASHASH(c) ) {
	    SET_HASHVALUE(c, R_Newhashpjw(CHAR(c)));
	    SET_HASHASH(c, 1);
	}
	hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	frame = R_HashGetLoc(hashcode, symbol, HASHTAB(rho));
	if (frame != R_NilValue) {
	    if (rho == R_GlobalEnv) R_DirtyImage = 1;
	    SET_BINDING_VALUE(frame, value);
	    SET_MISSING(frame, 0);	/* same as defineVar */
	    return symbol;
	}
    }
    return R_NilValue; /* -Wall */
}


/*----------------------------------------------------------------------

    setVar

    Assign a new value to bound symbol.	 Note this does the "inherits"
    case.  I.e. it searches frame-by-frame for a symbol and binds the
    given value to the first symbol encountered.  If no symbol is
    found then a binding is created in the global environment.

    Changed in R 2.4.0 to look in the base environment (previously the
    search stopped befor the base environment, but would (and still
    does) assign into the base namespace if that is on the search and
    the symbol existed there).

*/

void setVar(SEXP symbol, SEXP value, SEXP rho)
{
    SEXP vl;
    while (rho != R_EmptyEnv) {
	vl = setVarInFrame(rho, symbol, value);
	if (vl != R_NilValue) return;
	rho = ENCLOS(rho);
    }
    defineVar(symbol, value, R_GlobalEnv);
}



/*----------------------------------------------------------------------

  gsetVar

  Assignment in the base environment. Here we assign directly into
  the base environment.

*/

void gsetVar(SEXP symbol, SEXP value, SEXP rho)
{
    if (FRAME_IS_LOCKED(rho)) {
	if(SYMVALUE(symbol) == R_UnboundValue)
	    error(_("cannot add binding of '%s' to the base environment"),
		  CHAR(PRINTNAME(symbol)));
    }
#ifdef USE_GLOBAL_CACHE
    R_FlushGlobalCache(symbol);
#endif
    SET_SYMBOL_BINDING_VALUE(symbol, value);
}

/* get environment from a subclass if possible; else return NULL */
#define simple_as_environment(arg) (IS_S4_OBJECT(arg) && (TYPEOF(arg) == S4SXP) ? R_getS4DataSlot(arg, ENVSXP) : R_NilValue)



/*----------------------------------------------------------------------

  do_assign : .Internal(assign(x, value, envir, inherits))

*/
SEXP attribute_hidden do_assign(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP name=R_NilValue, val, aenv;
    int ginherits = 0;
    checkArity(op, args);

    if (!isString(CAR(args)) || length(CAR(args)) == 0)
	error(_("invalid first argument"));
    else {
	if (length(CAR(args)) > 1)
	    warning(_("only the first element is used as variable name"));
	name = installTrChar(STRING_ELT(CAR(args), 0));
    }
    PROTECT(val = CADR(args));
    aenv = CADDR(args);
    if (TYPEOF(aenv) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(aenv) != ENVSXP &&
	TYPEOF((aenv = simple_as_environment(aenv))) != ENVSXP)
	error(_("invalid '%s' argument"), "envir");
    ginherits = asLogical(CADDDR(args));
    if (ginherits == NA_LOGICAL)
	error(_("invalid '%s' argument"), "inherits");
    if (ginherits)
	setVar(name, val, aenv);
    else
	defineVar(name, val, aenv);
    UNPROTECT(1);
    return val;
}


/**
 * do_list2env : .Internal(list2env(x, envir))
  */
SEXP attribute_hidden do_list2env(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, xnms, envir;
    int n;
    checkArity(op, args);

    if (TYPEOF(CAR(args)) != VECSXP)
	error(_("first argument must be a named list"));
    x = CAR(args);
    n = LENGTH(x);
    xnms = getAttrib(x, R_NamesSymbol);
    if (n && (TYPEOF(xnms) != STRSXP || LENGTH(xnms) != n))
	error(_("names(x) must be a character vector of the same length as x"));
    envir = CADR(args);
    if (TYPEOF(envir) != ENVSXP)
	error(_("'envir' argument must be an environment"));

    for(int i = 0; i < n; i++) {
	SEXP name = installTrChar(STRING_ELT(xnms, i));
	defineVar(name, VECTOR_ELT(x, i), envir);
    }

    return envir;
}


/*----------------------------------------------------------------------

  do_remove

  There are three arguments to do_remove; a list of names to remove,
  an optional environment (if missing set it to R_GlobalEnv) and
  inherits, a logical indicating whether to look in the parent env if
  a symbol is not found in the supplied env.  This is ignored if
  environment is not specified.

*/

static int RemoveVariable(SEXP name, int hashcode, SEXP env)
{
    int found;
    SEXP list;

    if (env == R_BaseNamespace)
	error(_("cannot remove variables from base namespace"));
    if (env == R_BaseEnv)
	error(_("cannot remove variables from the base environment"));
    if (env == R_EmptyEnv)
	error(_("cannot remove variables from the empty environment"));
    if (FRAME_IS_LOCKED(env))
	error(_("cannot remove bindings from a locked environment"));

    if(IS_USER_DATABASE(env)) {
	R_ObjectTable *table;
	table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(env));
	if(table->remove == NULL)
	    error(_("cannot remove variables from this database"));
	return(table->remove(CHAR(PRINTNAME(name)), table));
    }

    if (IS_HASHED(env)) {
	SEXP hashtab = HASHTAB(env);
	int idx = hashcode % HASHSIZE(hashtab);
	list = RemoveFromList(name, VECTOR_ELT(hashtab, idx), &found);
	if (found) {
	    if(env == R_GlobalEnv) R_DirtyImage = 1;
	    if (list == R_NilValue)
		SET_HASHPRI(hashtab, HASHPRI(hashtab) - 1);
	    SET_VECTOR_ELT(hashtab, idx, list);
#ifdef USE_GLOBAL_CACHE
	    if (IS_GLOBAL_FRAME(env))
		R_FlushGlobalCache(name);
#endif
	}
    }
    else {
	list = RemoveFromList(name, FRAME(env), &found);
	if (found) {
	    if(env == R_GlobalEnv) R_DirtyImage = 1;
	    SET_FRAME(env, list);
#ifdef USE_GLOBAL_CACHE
	    if (IS_GLOBAL_FRAME(env))
		R_FlushGlobalCache(name);
#endif
	}
    }
    return found;
}

SEXP attribute_hidden do_remove(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* .Internal(remove(list, envir, inherits)) */

    SEXP name, envarg, tsym, tenv;
    int ginherits = 0;
    int done, i, hashcode;
    checkArity(op, args);

    name = CAR(args);
    if (!isString(name))
	error(_("invalid first argument"));
    args = CDR(args);

    envarg = CAR(args);
    if (TYPEOF(envarg) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(envarg) != ENVSXP &&
	TYPEOF((envarg = simple_as_environment(envarg))) != ENVSXP)
	error(_("invalid '%s' argument"), "envir");
    args = CDR(args);

    ginherits = asLogical(CAR(args));
    if (ginherits == NA_LOGICAL)
	error(_("invalid '%s' argument"), "inherits");

    for (i = 0; i < LENGTH(name); i++) {
	done = 0;
	tsym = installTrChar(STRING_ELT(name, i));
	if( !HASHASH(PRINTNAME(tsym)) )
	    hashcode = R_Newhashpjw(CHAR(PRINTNAME(tsym)));
	else
	    hashcode = HASHVALUE(PRINTNAME(tsym));
	tenv = envarg;
	while (tenv != R_EmptyEnv) {
	    done = RemoveVariable(tsym, hashcode, tenv);
	    if (done || !ginherits)
		break;
	    tenv = CDR(tenv);
	}
	if (!done)
	    warning(_("object '%s' not found"), EncodeChar(PRINTNAME(tsym)));
    }
    return R_NilValue;
}


/*----------------------------------------------------------------------

  do_get

  This function returns the SEXP associated with the character
  argument.  It needs the environment of the calling function as a
  default.

      get(x, envir, mode, inherits)
      exists(x, envir, mode, inherits)

*/


SEXP attribute_hidden do_get(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval, genv, t1 = R_NilValue;
    SEXPTYPE gmode;
    int ginherits = 0, where;
    checkArity(op, args);

    /* The first arg is the object name */
    /* It must be present and a non-empty string */

    if (!isValidStringF(CAR(args)))
	error(_("invalid first argument"));
    else
	t1 = installTrChar(STRING_ELT(CAR(args), 0));

    /* envir :	originally, the "where=" argument */

    if (TYPEOF(CADR(args)) == REALSXP || TYPEOF(CADR(args)) == INTSXP) {
	where = asInteger(CADR(args));
	genv = R_sysframe(where, R_GlobalContext);
    }
    else if (TYPEOF(CADR(args)) == NILSXP) {
	error(_("use of NULL environment is defunct"));
	genv = R_NilValue;  /* -Wall */
    }
    else if (TYPEOF(CADR(args)) == ENVSXP)
	genv = CADR(args);
    else if(TYPEOF((genv = simple_as_environment(CADR(args)))) != ENVSXP) {
	error(_("invalid '%s' argument"), "envir");
	genv = R_NilValue;  /* -Wall */
    }

    /* mode :  The mode of the object being sought */

    /* as from R 1.2.0, this is the *mode*, not the *typeof* aka
       storage.mode.
    */

    if (isString(CADDR(args))) {
	if (!strcmp(CHAR(STRING_ELT(CAR(CDDR(args)), 0)), "function")) /* ASCII */
	    gmode = FUNSXP;
	else
	    gmode = str2type(CHAR(STRING_ELT(CAR(CDDR(args)), 0))); /* ASCII */
    } else {
	error(_("invalid '%s' argument"), "mode");
	gmode = FUNSXP;/* -Wall */
    }

    ginherits = asLogical(CADDDR(args));
    if (ginherits == NA_LOGICAL)
	error(_("invalid '%s' argument"), "inherits");

    /* Search for the object */
    rval = findVar1mode(t1, genv, gmode, ginherits, PRIMVAL(op));

    if (PRIMVAL(op)) { /* have get(.) */
	if (rval == R_MissingArg)
	    error(_("argument \"%s\" is missing, with no default"),
		  CHAR(PRINTNAME(t1)));
	if (rval == R_UnboundValue) {
	    if (gmode == ANYSXP)
		error(_("object '%s' not found"), EncodeChar(PRINTNAME(t1)));
	    else
		error(_("object '%s' of mode '%s' was not found"),
		      CHAR(PRINTNAME(t1)),
		      CHAR(STRING_ELT(CAR(CDDR(args)), 0))); /* ASCII */
	}

	/* We need to evaluate if it is a promise */
	if (TYPEOF(rval) == PROMSXP)
	    rval = eval(rval, genv);

	if (!ISNULL(rval) && NAMED(rval) == 0)
	    SET_NAMED(rval, 1);
	return rval;
    }
    else { /* exists(.) */
	if (rval == R_UnboundValue)
	    ginherits = 0;
	else
	    ginherits = 1;
	return ScalarLogical(ginherits);
    }
}

static SEXP gfind(const char *name, SEXP env, SEXPTYPE mode,
		  SEXP ifnotfound, int inherits, SEXP enclos)
{
    SEXP rval, t1, R_fcall, var;

    t1 = install(name);

    /* Search for the object - last arg is 1 to 'get' */
    rval = findVar1mode(t1, env, mode, inherits, 1);

    if (rval == R_UnboundValue) {
	if( isFunction(ifnotfound) ) {
	    PROTECT(var = mkString(name));
	    PROTECT(R_fcall = LCONS(ifnotfound, LCONS(var, R_NilValue)));
	    rval = eval(R_fcall, enclos);
	    UNPROTECT(2);
	} else
	    rval = ifnotfound;
    }

    /* We need to evaluate if it is a promise */
    if (TYPEOF(rval) == PROMSXP) rval = eval(rval, env);
    if (!ISNULL(rval) && NAMED(rval) == 0) SET_NAMED(rval, 1);
    return rval;
}


/** mget(): get multiple values from an environment
 *
 * .Internal(mget(x, envir, mode, ifnotfound, inherits))
 *
 * @return  a list of the same length as x, a character vector (of names).
 */
SEXP attribute_hidden do_mget(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, env, x, mode, ifnotfound;
    int ginherits = 0, nvals, nmode, nifnfnd;

    checkArity(op, args);

    x = CAR(args);

    nvals = length(x);

    /* The first arg is the object name */
    /* It must be present and a string */
    if (!isString(x) )
	error(_("invalid first argument"));
    for(int i = 0; i < nvals; i++)
	if( isNull(STRING_ELT(x, i)) || !CHAR(STRING_ELT(x, 0))[0] )
	    error(_("invalid name in position %d"), i+1);

    env = CADR(args);
    if (ISNULL(env)) {
	error(_("use of NULL environment is defunct"));
    } else if( !isEnvironment(env) )
	error(_("second argument must be an environment"));

    mode = CADDR(args);
    nmode = length(mode);
    if( !isString(mode) )
	error(_("invalid '%s' argument"), "mode");

    if( nmode != nvals && nmode != 1 )
	error(_("wrong length for '%s' argument"), "mode");

    PROTECT(ifnotfound = coerceVector(CADDDR(args), VECSXP));
    nifnfnd = length(ifnotfound);
    if( !isVector(ifnotfound) )
	error(_("invalid '%s' argument"), "ifnotfound");

    if( nifnfnd != nvals && nifnfnd != 1 )
	error(_("wrong length for '%s' argument"), "ifnotfound");

    ginherits = asLogical(CAD4R(args));
    if (ginherits == NA_LOGICAL)
	error(_("invalid '%s' argument"), "inherits");

    PROTECT(ans = allocVector(VECSXP, nvals));

    for(int i = 0; i < nvals; i++) {
	SEXPTYPE gmode;
	if (!strcmp(CHAR(STRING_ELT(CAR(CDDR(args)), i % nmode)), "function"))
	    gmode = FUNSXP;
	else {
	    gmode = str2type(CHAR(STRING_ELT(CAR(CDDR(args)), i % nmode)));
	    if(gmode == (SEXPTYPE) (-1))
		error(_("invalid '%s' argument"), "mode");
	}
        SEXP ans_i = gfind(translateChar(STRING_ELT(x, i % nvals)), env,
                           gmode, VECTOR_ELT(ifnotfound, i % nifnfnd),
                           ginherits, rho);
	SET_VECTOR_ELT(ans, i, lazy_duplicate(ans_i));
    }

    setAttrib(ans, R_NamesSymbol, lazy_duplicate(x));
    UNPROTECT(2);
    return(ans);
}

/*----------------------------------------------------------------------

  do_missing

  This function tests whether the symbol passed as its first argument
  is a missing argument to the current closure.  rho is the
  environment that missing was called from.

  R_isMissing is called on the not-yet-evaluated value of an argument,
  if this is a symbol, as it could be a missing argument that has been
  passed down.  So 'symbol' is the promise value, and 'rho' its
  evaluation argument.

  It is also called in arithmetic.c. for e.g. do_log
*/

int attribute_hidden
R_isMissing(SEXP symbol, SEXP rho)
{
    int ddv=0;
    SEXP vl, s;

    if (symbol == R_MissingArg) /* Yes, this can happen */
	return 1;

    /* check for infinite recursion */
    R_CheckStack();

    if (DDVAL(symbol)) {
	s = R_DotsSymbol;
	ddv = ddVal(symbol);
    }
    else
	s = symbol;

    if (rho == R_BaseEnv || rho == R_BaseNamespace)
	return 0;  /* is this really the right thing to do? LT */

    vl = findVarLocInFrame(rho, s, NULL);
    if (vl != R_NilValue) {
	if (DDVAL(symbol)) {
	    if (length(CAR(vl)) < ddv || CAR(vl) == R_MissingArg)
		return 1;
	    /* defineVar(symbol, value, R_GlobalEnv); */
	    else
		vl = nthcdr(CAR(vl), ddv-1);
	}
	if (MISSING(vl) == 1 || CAR(vl) == R_MissingArg)
	    return 1;
	if (IS_ACTIVE_BINDING(vl))
	    return 0;
	if (TYPEOF(CAR(vl)) == PROMSXP &&
	    PRVALUE(CAR(vl)) == R_UnboundValue &&
	    TYPEOF(PREXPR(CAR(vl))) == SYMSXP) {
	    /* This code uses the PRSEEN bit to detect cycles.  If a
	       cycle occurs then a missing argument was encountered,
	       so the return value is TRUE.  It would be a little
	       safer to use the promise stack to ensure unsetting of
	       the bits in the event of a longjump, but doing so would
	       require distinguishing between evaluating promises and
	       checking for missingness.  Because of the test above
	       for an active binding a longjmp should only happen if
	       the stack check fails.  LT */
	    if (PRSEEN(CAR(vl)))
		return 1;
	    else {
		int val;
		SET_PRSEEN(CAR(vl), 1);
		val = R_isMissing(PREXPR(CAR(vl)), PRENV(CAR(vl)));
		SET_PRSEEN(CAR(vl), 0);
		return val;
	    }
	}
	else
	    return 0;
    }
    return 0;
}

/* this is primitive and a SPECIALSXP */
SEXP attribute_hidden do_missing(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int ddv=0;
    SEXP rval, t, sym, s;

    checkArity(op, args);
    check1arg(args, call, "x");
    s = sym = CAR(args);
    if( isString(sym) && length(sym)==1 )
	s = sym = installTrChar(STRING_ELT(CAR(args), 0));
    if (!isSymbol(sym))
	errorcall(call, _("invalid use of 'missing'"));

    if (DDVAL(sym)) {
	ddv = ddVal(sym);
	sym = R_DotsSymbol;
    }
    rval = allocVector(LGLSXP,1);

    t = findVarLocInFrame(rho, sym, NULL);
    if (t != R_NilValue) {
	if (DDVAL(s)) {
	    if (length(CAR(t)) < ddv  || CAR(t) == R_MissingArg) {
		LOGICAL(rval)[0] = 1;
		return rval;
	    }
	    else
		t = nthcdr(CAR(t), ddv-1);
	}
	if (MISSING(t) || CAR(t) == R_MissingArg) {
	    LOGICAL(rval)[0] = 1;
	    return rval;
	}
	else goto havebinding;
    }
    else  /* it wasn't an argument to the function */
	errorcall(call, _("'missing' can only be used for arguments"));

 havebinding:

    t = CAR(t);
    if (TYPEOF(t) != PROMSXP) {
	LOGICAL(rval)[0] = 0;
	return rval;
    }

    if (!isSymbol(PREXPR(t))) LOGICAL(rval)[0] = 0;
    else LOGICAL(rval)[0] = R_isMissing(PREXPR(t), PRENV(t));
    return rval;
}

/*----------------------------------------------------------------------

  do_globalenv

  Returns the current global environment.

*/


SEXP attribute_hidden do_globalenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_GlobalEnv;
}

/*----------------------------------------------------------------------

  do_baseenv

  Returns the current base environment.

*/


SEXP attribute_hidden do_baseenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_BaseEnv;
}

/*----------------------------------------------------------------------

  do_emptyenv

  Returns the current empty environment.

*/


SEXP attribute_hidden do_emptyenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_EmptyEnv;
}


/*----------------------------------------------------------------------

  do_attach

  To attach a list we make up an environment and insert components
  of the list in as the values of this env and install the tags from
  the list as the names.

*/

SEXP attribute_hidden do_attach(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP name, s, t, x;
    int pos, hsize;
    Rboolean isSpecial;

    checkArity(op, args);

    pos = asInteger(CADR(args));
    if (pos == NA_INTEGER)
	error(_("'pos' must be an integer"));

    name = CADDR(args);
    if (!isValidStringF(name))
	error(_("invalid '%s' argument"), "name");

    isSpecial = IS_USER_DATABASE(CAR(args));

    if(!isSpecial) {
	if (isNewList(CAR(args))) {
	    SETCAR(args, VectorToPairList(CAR(args)));

	    for (x = CAR(args); x != R_NilValue; x = CDR(x))
		if (TAG(x) == R_NilValue)
		    error(_("all elements of a list must be named"));
	    PROTECT(s = allocSExp(ENVSXP));
	    SET_FRAME(s, shallow_duplicate(CAR(args)));
	} else if (isEnvironment(CAR(args))) {
	    SEXP p, loadenv = CAR(args);

	    PROTECT(s = allocSExp(ENVSXP));
	    if (HASHTAB(loadenv) != R_NilValue) {
		int i, n;
		n = length(HASHTAB(loadenv));
		for (i = 0; i < n; i++) {
		    p = VECTOR_ELT(HASHTAB(loadenv), i);
		    while (p != R_NilValue) {
			defineVar(TAG(p), lazy_duplicate(CAR(p)), s);
			p = CDR(p);
		    }
		}
		/* FIXME: duplicate the hash table and assign here */
	    } else {
		for(p = FRAME(loadenv); p != R_NilValue; p = CDR(p))
                    defineVar(TAG(p), lazy_duplicate(CAR(p)), s);
	    }
	} else {
	    error(_("'attach' only works for lists, data frames and environments"));
	    s = R_NilValue; /* -Wall */
	}

	/* Connect FRAME(s) into HASHTAB(s) */
	if (length(s) < HASHMINSIZE)
	    hsize = HASHMINSIZE;
	else
	    hsize = length(s);

	SET_HASHTAB(s, R_NewHashTable(hsize));
	s = R_HashFrame(s);

	/* FIXME: A little inefficient */
	while (R_HashSizeCheck(HASHTAB(s)))
	    SET_HASHTAB(s, R_HashResize(HASHTAB(s)));

    } else { /* is a user object */
	/* Having this here (rather than below) means that the onAttach routine
	   is called before the table is attached. This may not be necessary or
	   desirable. */
	R_ObjectTable *tb = (R_ObjectTable*) R_ExternalPtrAddr(CAR(args));
	if(tb->onAttach)
	    tb->onAttach(tb);
	s = allocSExp(ENVSXP);
	SET_HASHTAB(s, CAR(args));
	setAttrib(s, R_ClassSymbol, getAttrib(HASHTAB(s), R_ClassSymbol));
    }

    setAttrib(s, R_NameSymbol, name);
    for (t = R_GlobalEnv; ENCLOS(t) != R_BaseEnv && pos > 2; t = ENCLOS(t))
	pos--;

    if (ENCLOS(t) == R_BaseEnv) {
	SET_ENCLOS(t, s);
	SET_ENCLOS(s, R_BaseEnv);
    }
    else {
	x = ENCLOS(t);
	SET_ENCLOS(t, s);
	SET_ENCLOS(s, x);
    }

    if(!isSpecial) { /* Temporary: need to remove the elements identified by objects(CAR(args)) */
#ifdef USE_GLOBAL_CACHE
	R_FlushGlobalCacheFromTable(HASHTAB(s));
	MARK_AS_GLOBAL_FRAME(s);
#endif
	UNPROTECT(1);
    } else {
#ifdef USE_GLOBAL_CACHE
	R_FlushGlobalCacheFromUserTable(HASHTAB(s));
	MARK_AS_GLOBAL_FRAME(s);
#endif
    }

    return s;
}



/*----------------------------------------------------------------------

  do_detach

  detach the specified environment.  Detachment only takes place by
  position.

*/

SEXP attribute_hidden do_detach(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, t, x;
    int pos, n;
    Rboolean isSpecial = FALSE;

    checkArity(op, args);
    pos = asInteger(CAR(args));

    for (n = 2, t = ENCLOS(R_GlobalEnv); t != R_BaseEnv; t = ENCLOS(t))
	n++;

    if (pos == n) /* n is the length of the search list */
	error(_("detaching \"package:base\" is not allowed"));

    for (t = R_GlobalEnv ; ENCLOS(t) != R_BaseEnv && pos > 2 ; t = ENCLOS(t))
	pos--;
    if (pos != 2) {
	error(_("invalid '%s' argument"), "pos");
	s = t;	/* for -Wall */
    }
    else {
	PROTECT(s = ENCLOS(t));
	x = ENCLOS(s);
	SET_ENCLOS(t, x);
	isSpecial = IS_USER_DATABASE(s);
	if(isSpecial) {
	    R_ObjectTable *tb = (R_ObjectTable*) R_ExternalPtrAddr(HASHTAB(s));
	    if(tb->onDetach) tb->onDetach(tb);
	}

	SET_ENCLOS(s, R_BaseEnv);
    }
#ifdef USE_GLOBAL_CACHE
    if(!isSpecial) {
	R_FlushGlobalCacheFromTable(HASHTAB(s));
	MARK_AS_LOCAL_FRAME(s);
    } else {
	R_FlushGlobalCacheFromUserTable(HASHTAB(s));
	MARK_AS_LOCAL_FRAME(s); /* was _GLOBAL_ prior to 2.4.0 */
    }
#endif
    UNPROTECT(1);
    return s;
}



/*----------------------------------------------------------------------

  do_search

  Print out the current search path.

*/

SEXP attribute_hidden do_search(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, name, t;
    int i, n;

    checkArity(op, args);
    n = 2;
    for (t = ENCLOS(R_GlobalEnv); t != R_BaseEnv ; t = ENCLOS(t))
	n++;
    PROTECT(ans = allocVector(STRSXP, n));
    /* TODO - what should the name of this be? */
    SET_STRING_ELT(ans, 0, mkChar(".GlobalEnv"));
    SET_STRING_ELT(ans, n-1, mkChar("package:base"));
    i = 1;
    for (t = ENCLOS(R_GlobalEnv); t != R_BaseEnv ; t = ENCLOS(t)) {
	name = getAttrib(t, R_NameSymbol);
	if (!isString(name) || length(name) < 1)
	    SET_STRING_ELT(ans, i, mkChar("(unknown)"));
	else
	    SET_STRING_ELT(ans, i, STRING_ELT(name, 0));
	i++;
    }
    UNPROTECT(1);
    return ans;
}


/*----------------------------------------------------------------------

  do_ls

  This code implements the functionality of the "ls" and "objects"
  functions.  [ ls(envir, all.names, sorted) ]

*/

static int FrameSize(SEXP frame, int all)
{
    int count = 0;
    while (frame != R_NilValue) {
	if ((all || CHAR(PRINTNAME(TAG(frame)))[0] != '.') &&
				      CAR(frame) != R_UnboundValue)
	    count += 1;
	frame = CDR(frame);
    }
    return count;
}

static void FrameNames(SEXP frame, int all, SEXP names, int *indx)
{
    while (frame != R_NilValue) {
	if ((all || CHAR(PRINTNAME(TAG(frame)))[0] != '.') &&
				      CAR(frame) != R_UnboundValue) {
	    SET_STRING_ELT(names, *indx, PRINTNAME(TAG(frame)));
	    (*indx)++;
	}
	frame = CDR(frame);
    }
}

static void FrameValues(SEXP frame, int all, SEXP values, int *indx)
{
    while (frame != R_NilValue) {
	if ((all || CHAR(PRINTNAME(TAG(frame)))[0] != '.') &&
				      CAR(frame) != R_UnboundValue) {
	    SEXP value = CAR(frame);
	    if (TYPEOF(value) == PROMSXP) {
		PROTECT(value);
		value = eval(value, R_GlobalEnv);
		UNPROTECT(1);
	    }
	    SET_VECTOR_ELT(values, *indx, lazy_duplicate(value));
	    (*indx)++;
	}
	frame = CDR(frame);
    }
}

static int HashTableSize(SEXP table, int all)
{
    int count = 0;
    int n = length(table);
    int i;
    for (i = 0; i < n; i++)
	count += FrameSize(VECTOR_ELT(table, i), all);
    return count;
}

static void HashTableNames(SEXP table, int all, SEXP names, int *indx)
{
    int n = length(table);
    int i;
    for (i = 0; i < n; i++)
	FrameNames(VECTOR_ELT(table, i), all, names, indx);
}

static void HashTableValues(SEXP table, int all, SEXP values, int *indx)
{
    int n = length(table);
    int i;
    for (i = 0; i < n; i++)
	FrameValues(VECTOR_ELT(table, i), all, values, indx);
}

static int BuiltinSize(int all, int intern)
{
    int count = 0;
    SEXP s;
    int j;
    for (j = 0; j < HSIZE; j++) {
	for (s = R_SymbolTable[j]; s != R_NilValue; s = CDR(s)) {
	    if (intern) {
		if (INTERNAL(CAR(s)) != R_NilValue)
		    count++;
	    }
	    else {
		if ((all || CHAR(PRINTNAME(CAR(s)))[0] != '.')
		    && SYMVALUE(CAR(s)) != R_UnboundValue)
		    count++;
	    }
	}
    }
    return count;
}

static void
BuiltinNames(int all, int intern, SEXP names, int *indx)
{
    SEXP s;
    int j;
    for (j = 0; j < HSIZE; j++) {
	for (s = R_SymbolTable[j]; s != R_NilValue; s = CDR(s)) {
	    if (intern) {
		if (INTERNAL(CAR(s)) != R_NilValue)
		    SET_STRING_ELT(names, (*indx)++, PRINTNAME(CAR(s)));
	    }
	    else {
		if ((all || CHAR(PRINTNAME(CAR(s)))[0] != '.')
		    && SYMVALUE(CAR(s)) != R_UnboundValue)
		    SET_STRING_ELT(names, (*indx)++, PRINTNAME(CAR(s)));
	    }
	}
    }
}

static void
BuiltinValues(int all, int intern, SEXP values, int *indx)
{
    SEXP s, vl;
    int j;
    for (j = 0; j < HSIZE; j++) {
	for (s = R_SymbolTable[j]; s != R_NilValue; s = CDR(s)) {
	    if (intern) {
		if (INTERNAL(CAR(s)) != R_NilValue) {
		    vl = SYMVALUE(CAR(s));
		    if (TYPEOF(vl) == PROMSXP) {
			PROTECT(vl);
			vl = eval(vl, R_BaseEnv);
			UNPROTECT(1);
		    }
		    SET_VECTOR_ELT(values, (*indx)++, lazy_duplicate(vl));
		}
	    }
	    else {
		if ((all || CHAR(PRINTNAME(CAR(s)))[0] != '.')
		    && SYMVALUE(CAR(s)) != R_UnboundValue) {
		    vl = SYMVALUE(CAR(s));
		    if (TYPEOF(vl) == PROMSXP) {
			PROTECT(vl);
			vl = eval(vl, R_BaseEnv);
			UNPROTECT(1);
		    }
		    SET_VECTOR_ELT(values, (*indx)++, lazy_duplicate(vl));
		}
	    }
	}
    }
}

SEXP attribute_hidden do_ls(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    if(IS_USER_DATABASE(CAR(args))) {
	R_ObjectTable *tb = (R_ObjectTable*)
	    R_ExternalPtrAddr(HASHTAB(CAR(args)));
	return(tb->objects(tb));
    }

    SEXP env = CAR(args);

    /* if (env == R_BaseNamespace) env = R_BaseEnv; */

    int all = asLogical(CADR(args));
    if (all == NA_LOGICAL) all = 0;

    int sort_nms = asLogical(CADDR(args)); /* sorted = TRUE/FALSE */
    if (sort_nms == NA_LOGICAL) sort_nms = 0;

    return R_lsInternal3(env, all, sort_nms);
}

/* takes a *list* of environments, a boolean indicating whether to get all
   names and a boolean if sorted is desired */
SEXP R_lsInternal3(SEXP env, Rboolean all, Rboolean sorted)
{
    /* Step 1 : Compute the Vector Size */
    int k = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	k += BuiltinSize(all, 0);
    else if (isEnvironment(env) ||
	isEnvironment(env = simple_as_environment(env))) {
	if (HASHTAB(env) != R_NilValue)
	    k += HashTableSize(HASHTAB(env), all);
	else
	    k += FrameSize(FRAME(env), all);
    }
    else
	error(_("invalid '%s' argument"), "envir");

    /* Step 2 : Allocate and Fill the Result */
    SEXP ans = PROTECT(allocVector(STRSXP, k));
    k = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	BuiltinNames(all, 0, ans, &k);
    else if (isEnvironment(env)) {
	if (HASHTAB(env) != R_NilValue)
	    HashTableNames(HASHTAB(env), all, ans, &k);
	else
	    FrameNames(FRAME(env), all, ans, &k);
    }

    UNPROTECT(1);
    if(sorted) sortVector(ans, FALSE);
    return ans;
}

/* non-API version used in several packages */
SEXP R_lsInternal(SEXP env, Rboolean all)
{
    return R_lsInternal3(env, all, TRUE);
}

/* transform an environment into a named list: as.list.environment(.) */

SEXP attribute_hidden do_env2list(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, ans, names;
    int k, all;

    checkArity(op, args);

    env = CAR(args);
    if (ISNULL(env))
	error(_("use of NULL environment is defunct"));
    if( !isEnvironment(env) ) {
        SEXP xdata;
	if( IS_S4_OBJECT(env) && TYPEOF(env) == S4SXP &&
	    (xdata = R_getS4DataSlot(env, ENVSXP)) != R_NilValue)
	    env = xdata;
	else
	    error(_("argument must be an environment"));
    }

    all = asLogical(CADR(args)); /* all.names = TRUE/FALSE */
    if (all == NA_LOGICAL) all = 0;

    int sort_nms = asLogical(CADDR(args)); /* sorted = TRUE/FALSE */
    if (sort_nms == NA_LOGICAL) sort_nms = 0;

    if (env == R_BaseEnv || env == R_BaseNamespace)
	k = BuiltinSize(all, 0);
    else if (HASHTAB(env) != R_NilValue)
	k = HashTableSize(HASHTAB(env), all);
    else
	k = FrameSize(FRAME(env), all);

    PROTECT(names = allocVector(STRSXP, k));
    PROTECT(ans = allocVector(VECSXP, k));

    k = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	BuiltinValues(all, 0, ans, &k);
    else if (HASHTAB(env) != R_NilValue)
	HashTableValues(HASHTAB(env), all, ans, &k);
    else
	FrameValues(FRAME(env), all, ans, &k);

    k = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	BuiltinNames(all, 0, names, &k);
    else if (HASHTAB(env) != R_NilValue)
	HashTableNames(HASHTAB(env), all, names, &k);
    else
	FrameNames(FRAME(env), all, names, &k);

    if(sort_nms) {
	// return list with *sorted* names
	SEXP sind = PROTECT(allocVector(INTSXP, k));
	int *indx = INTEGER(sind);
	for (int i = 0; i < k; i++) indx[i] = i;
	orderVector1(indx, k, names, /* nalast */ TRUE, /* decreasing */ FALSE,
		     R_NilValue);
	SEXP ans2   = PROTECT(allocVector(VECSXP, k));
	SEXP names2 = PROTECT(allocVector(STRSXP, k));
	for(int i = 0; i < k; i++) {
	    SET_STRING_ELT(names2, i, STRING_ELT(names, indx[i]));
	    SET_VECTOR_ELT(ans2,   i, VECTOR_ELT(ans,   indx[i]));
	}
	setAttrib(ans2, R_NamesSymbol, names2);
	UNPROTECT(5);
	return(ans2);
    }
    else {
	setAttrib(ans, R_NamesSymbol, names);
	UNPROTECT(2);
	return(ans);
    }
}

/*
 * apply a function to all objects in an environment and return the
 * results in a list.
 * Equivalent to lapply(as.list(env, all.names=all.names), FUN, ...)
 */
/* This is a special .Internal */
SEXP attribute_hidden do_eapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, ans, R_fcall, FUN, tmp, tmp2, ind;
    int i, k, k2;
    int /* boolean */ all, useNms;

    checkArity(op, args);

    env = eval(CAR(args), rho);
    if (ISNULL(env))
	error(_("use of NULL environment is defunct"));
    if( !isEnvironment(env) )
	error(_("argument must be an environment"));

    FUN = CADR(args);
    if (!isSymbol(FUN))
	error(_("arguments must be symbolic"));

    /* 'all.names' : */
    all = asLogical(eval(CADDR(args), rho));
    if (all == NA_LOGICAL) all = 0;

    /* 'USE.NAMES' : */
    useNms = asLogical(eval(CADDDR(args), rho));
    if (useNms == NA_LOGICAL) useNms = 0;

    if (env == R_BaseEnv || env == R_BaseNamespace)
	k = BuiltinSize(all, 0);
    else if (HASHTAB(env) != R_NilValue)
	k = HashTableSize(HASHTAB(env), all);
    else
	k = FrameSize(FRAME(env), all);

    PROTECT(ans  = allocVector(VECSXP, k));
    PROTECT(tmp2 = allocVector(VECSXP, k));

    k2 = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	BuiltinValues(all, 0, tmp2, &k2);
    else if (HASHTAB(env) != R_NilValue)
	HashTableValues(HASHTAB(env), all, tmp2, &k2);
    else
	FrameValues(FRAME(env), all, tmp2, &k2);

    PROTECT(ind = allocVector(INTSXP, 1));
    /* tmp :=  `[`(<elist>, i) */
    PROTECT(tmp = LCONS(R_Bracket2Symbol,
			LCONS(tmp2, LCONS(ind, R_NilValue))));
    /* fcall :=  <FUN>( tmp, ... ) */
    PROTECT(R_fcall = LCONS(FUN, LCONS(tmp, LCONS(R_DotsSymbol, R_NilValue))));

    for(i = 0; i < k2; i++) {
	INTEGER(ind)[0] = i+1;
	SEXP tmp = eval(R_fcall, rho);
	if (MAYBE_REFERENCED(tmp))
	    tmp = lazy_duplicate(tmp);
	SET_VECTOR_ELT(ans, i, tmp);
    }

    if (useNms) {
	SEXP names;
	PROTECT(names = allocVector(STRSXP, k));
	k = 0;
	if (env == R_BaseEnv || env == R_BaseNamespace)
	    BuiltinNames(all, 0, names, &k);
	else if(HASHTAB(env) != R_NilValue)
	    HashTableNames(HASHTAB(env), all, names, &k);
	else
	    FrameNames(FRAME(env), all, names, &k);

	setAttrib(ans, R_NamesSymbol, names);
	UNPROTECT(1);
    }
    UNPROTECT(5);
    return(ans);
}

int envlength(SEXP rho)
{
    if( HASHTAB(rho) != R_NilValue)
	return HashTableSize(HASHTAB(rho), 1);
    else
	return FrameSize(FRAME(rho), 1);
}

/*----------------------------------------------------------------------

  do_builtins

  Return the names of all the built in functions.  These are fetched
  directly from the symbol table.

*/

SEXP attribute_hidden do_builtins(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    int intern, nelts;
    checkArity(op, args);
    intern = asLogical(CAR(args));
    if (intern == NA_INTEGER) intern = 0;
    nelts = BuiltinSize(1, intern);
    ans = allocVector(STRSXP, nelts);
    nelts = 0;
    BuiltinNames(1, intern, ans, &nelts);
    sortVector(ans, TRUE);
    return ans;
}


/*----------------------------------------------------------------------

  do_pos2env

  This function returns the environment at a specified position in the
  search path or the environment of the caller of
  pos.to.env (? but pos.to.env is usually used in arg lists and hence
  is evaluated in the calling environment so this is one higher).

  When pos = -1 the environment of the closure that pos2env is
  evaluated in is obtained. Note: this relies on pos.to.env being
  a primitive.

 */
static SEXP pos2env(int pos, SEXP call)
{
    SEXP env;
    RCNTXT *cptr;

    if (pos == NA_INTEGER || pos < -1 || pos == 0) {
	errorcall(call, _("invalid '%s' argument"), "pos");
	env = call;/* just for -Wall */
    }
    else if (pos == -1) {
	/* make sure the context is a funcall */
	cptr = R_GlobalContext;
	while( !(cptr->callflag & CTXT_FUNCTION) && cptr->nextcontext
	       != NULL )
	    cptr = cptr->nextcontext;
	if( !(cptr->callflag & CTXT_FUNCTION) )
	    errorcall(call, _("no enclosing environment"));

	env = cptr->sysparent;
	if (R_GlobalEnv != R_NilValue && env == R_NilValue)
	    errorcall(call, _("invalid '%s' argument"), "pos");
    }
    else {
	for (env = R_GlobalEnv; env != R_EmptyEnv && pos > 1;
	     env = ENCLOS(env))
	    pos--;
	if (pos != 1)
	    errorcall(call, _("invalid '%s' argument"), "pos");
    }
    return env;
}

/* this is primitive */
SEXP attribute_hidden do_pos2env(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, pos;
    int i, npos;
    checkArity(op, args);
    check1arg(args, call, "x");

    PROTECT(pos = coerceVector(CAR(args), INTSXP));
    npos = length(pos);
    if (npos <= 0)
	errorcall(call, _("invalid '%s' argument"), "pos");
    PROTECT(env = allocVector(VECSXP, npos));
    for (i = 0; i < npos; i++) {
	SET_VECTOR_ELT(env, i, pos2env(INTEGER(pos)[i], call));
    }
    if (npos == 1) env = VECTOR_ELT(env, 0);
    UNPROTECT(2);
    return env;
}

static SEXP matchEnvir(SEXP call, const char *what)
{
    SEXP t, name;
    const void *vmax = vmaxget();
    if(!strcmp(".GlobalEnv", what))
	return R_GlobalEnv;
    if(!strcmp("package:base", what))
	return R_BaseEnv;
    for (t = ENCLOS(R_GlobalEnv); t != R_EmptyEnv ; t = ENCLOS(t)) {
	name = getAttrib(t, R_NameSymbol);
	if(isString(name) && length(name) > 0 &&
	   !strcmp(translateChar(STRING_ELT(name, 0)), what)) {
	    vmaxset(vmax);
	    return t;
	}
    }
    errorcall(call, _("no item called \"%s\" on the search list"), what);
    /* not reached */
    vmaxset(vmax);
    return R_NilValue;
}

/* This is primitive */
SEXP attribute_hidden
do_as_environment(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP arg = CAR(args), ans;
    checkArity(op, args);
    check1arg(args, call, "object");
    if(isEnvironment(arg))
	return arg;
    if(isObject(arg) &&
       DispatchOrEval(call, op, "as.environment", args, rho, &ans, 0, 1))
	return ans;
    switch(TYPEOF(arg)) {
    case STRSXP:
	return matchEnvir(call, translateChar(asChar(arg)));
    case REALSXP:
    case INTSXP:
	return do_pos2env(call, op, args, rho);
    case NILSXP:
	errorcall(call,_("using 'as.environment(NULL)' is defunct"));
	return R_BaseEnv;	/* -Wall */
    case S4SXP: {
	/* dispatch was tried above already */
	SEXP dot_xData = R_getS4DataSlot(arg, ENVSXP);
	if(!isEnvironment(dot_xData))
	    errorcall(call, _("S4 object does not extend class \"environment\""));
	else
	    return(dot_xData);
    }
    case VECSXP: {
	/* implement as.environment.list() {isObject(.) is false for a list} */
	SEXP call, val;
	PROTECT(call = lang4(install("list2env"), arg,
			     /* envir = */R_NilValue,
			     /* parent = */R_EmptyEnv));
	val = eval(call, rho);
	UNPROTECT(1);
	return val;
    }
    default:
	errorcall(call, _("invalid object for 'as.environment'"));
	return R_NilValue;	/* -Wall */
    }
}

void R_LockEnvironment(SEXP env, Rboolean bindings)
{
    if(IS_S4_OBJECT(env) && (TYPEOF(env) == S4SXP))
	env = R_getS4DataSlot(env, ANYSXP); /* better be an ENVSXP */
    if (env == R_BaseEnv || env == R_BaseNamespace) {
	if (bindings) {
	    SEXP s;
	    int j;
	    for (j = 0; j < HSIZE; j++)
		for (s = R_SymbolTable[j]; s != R_NilValue; s = CDR(s))
		    if(SYMVALUE(CAR(s)) != R_UnboundValue)
			LOCK_BINDING(CAR(s));
	}
#ifdef NOT_YET
	/* causes problems with Matrix */
	LOCK_FRAME(env);
#endif
	return;
    }

    if (TYPEOF(env) != ENVSXP)
	error(_("not an environment"));
    if (bindings) {
	if (IS_HASHED(env)) {
	    SEXP table, chain;
	    int i, size;
	    table = HASHTAB(env);
	    size = HASHSIZE(table);
	    for (i = 0; i < size; i++)
		for (chain = VECTOR_ELT(table, i);
		     chain != R_NilValue;
		     chain = CDR(chain))
		    LOCK_BINDING(chain);
	}
	else {
	    SEXP frame;
	    for (frame = FRAME(env); frame != R_NilValue; frame = CDR(frame))
		LOCK_BINDING(frame);
	}
    }
    LOCK_FRAME(env);
}

Rboolean R_EnvironmentIsLocked(SEXP env)
{
    if (TYPEOF(env) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP &&
	TYPEOF((env = simple_as_environment(env))) != ENVSXP)
	error(_("not an environment"));
    return FRAME_IS_LOCKED(env) != 0;
}

SEXP attribute_hidden do_lockEnv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP frame;
    Rboolean bindings;
    checkArity(op, args);
    frame = CAR(args);
    bindings = asLogical(CADR(args));
    R_LockEnvironment(frame, bindings);
    return R_NilValue;
}

SEXP attribute_hidden do_envIsLocked(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return ScalarLogical(R_EnvironmentIsLocked(CAR(args)));
}

void R_LockBinding(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error(_("not a symbol"));
    if (TYPEOF(env) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP &&
	TYPEOF((env = simple_as_environment(env))) != ENVSXP)
	error(_("not an environment"));
    if (env == R_BaseEnv || env == R_BaseNamespace)
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	LOCK_BINDING(sym);
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue)
	    error(_("no binding for \"%s\""), EncodeChar(PRINTNAME(sym)));
	LOCK_BINDING(binding);
    }
}

void R_unLockBinding(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error(_("not a symbol"));
    if (TYPEOF(env) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP &&
	TYPEOF((env = simple_as_environment(env))) != ENVSXP)
	error(_("not an environment"));
    if (env == R_BaseEnv || env == R_BaseNamespace)
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	UNLOCK_BINDING(sym);
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue)
	    error(_("no binding for \"%s\""), EncodeChar(PRINTNAME(sym)));
	UNLOCK_BINDING(binding);
    }
}

void R_MakeActiveBinding(SEXP sym, SEXP fun, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error(_("not a symbol"));
    if (! isFunction(fun))
	error(_("not a function"));
    if (TYPEOF(env) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP &&
	TYPEOF((env = simple_as_environment(env))) != ENVSXP)
	error(_("not an environment"));
    if (env == R_BaseEnv || env == R_BaseNamespace) {
	if (SYMVALUE(sym) != R_UnboundValue && ! IS_ACTIVE_BINDING(sym))
	    error(_("symbol already has a regular binding"));
	else if (BINDING_IS_LOCKED(sym))
	    error(_("cannot change active binding if binding is locked"));
	SET_SYMVALUE(sym, fun);
	SET_ACTIVE_BINDING_BIT(sym);
	/* we don't need to worry about the global cache here as
	   a regular binding cannot be changed */
    }
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue) {
	    defineVar(sym, fun, env); /* fails if env is locked */
	    binding = findVarLocInFrame(env, sym, NULL);
	    SET_ACTIVE_BINDING_BIT(binding);
	}
	else if (! IS_ACTIVE_BINDING(binding))
	    error(_("symbol already has a regular binding"));
	else if (BINDING_IS_LOCKED(binding))
	    error(_("cannot change active binding if binding is locked"));
	else
	    SETCAR(binding, fun);
    }
}

Rboolean R_BindingIsLocked(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error(_("not a symbol"));
    if (TYPEOF(env) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP &&
	TYPEOF((env = simple_as_environment(env))) != ENVSXP)
	error(_("not an environment"));
    if (env == R_BaseEnv || env == R_BaseNamespace)
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	return BINDING_IS_LOCKED(sym) != 0;
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue)
	    error(_("no binding for \"%s\""), EncodeChar(PRINTNAME(sym)));
	return BINDING_IS_LOCKED(binding) != 0;
    }
}

Rboolean R_BindingIsActive(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error(_("not a symbol"));
    if (TYPEOF(env) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP &&
	TYPEOF((env = simple_as_environment(env))) != ENVSXP)
	error(_("not an environment"));
    if (env == R_BaseEnv || env == R_BaseNamespace)
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	return IS_ACTIVE_BINDING(sym) != 0;
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue)
	    error(_("no binding for \"%s\""), EncodeChar(PRINTNAME(sym)));
	return IS_ACTIVE_BINDING(binding) != 0;
    }
}

Rboolean R_HasFancyBindings(SEXP rho)
{
    if (IS_HASHED(rho)) {
	SEXP table, chain;
	int i, size;

	table = HASHTAB(rho);
	size = HASHSIZE(table);
	for (i = 0; i < size; i++)
	    for (chain = VECTOR_ELT(table, i);
		 chain != R_NilValue;
		 chain = CDR(chain))
		if (IS_ACTIVE_BINDING(chain) || BINDING_IS_LOCKED(chain))
		    return TRUE;
	return FALSE;
    }
    else {
	SEXP frame;

	for (frame = FRAME(rho); frame != R_NilValue; frame = CDR(frame))
	    if (IS_ACTIVE_BINDING(frame) || BINDING_IS_LOCKED(frame))
		return TRUE;
	return FALSE;
    }
}

SEXP attribute_hidden do_lockBnd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, env;
    checkArity(op, args);
    sym = CAR(args);
    env = CADR(args);
    switch(PRIMVAL(op)) {
    case 0:
	R_LockBinding(sym, env);
	break;
    case 1:
	R_unLockBinding(sym, env);
	break;
    default:
	error(_("unknown op"));
    }
    return R_NilValue;
}

SEXP attribute_hidden do_bndIsLocked(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, env;
    checkArity(op, args);
    sym = CAR(args);
    env = CADR(args);
    return ScalarLogical(R_BindingIsLocked(sym, env));
}

SEXP attribute_hidden do_mkActiveBnd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, fun, env;
    checkArity(op, args);
    sym = CAR(args);
    fun = CADR(args);
    env = CADDR(args);
    R_MakeActiveBinding(sym, fun, env);
    return R_NilValue;
}

SEXP attribute_hidden do_bndIsActive(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, env;
    checkArity(op, args);
    sym = CAR(args);
    env = CADR(args);
    return ScalarLogical(R_BindingIsActive(sym, env));
}

/* This is a .Internal with no wrapper, currently unused in base R */
SEXP attribute_hidden do_mkUnbound(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym;
    checkArity(op, args);
    sym = CAR(args);

    if (TYPEOF(sym) != SYMSXP) error(_("not a symbol"));
    /* This is not quite the same as SET_SYMBOL_BINDING_VALUE as it
       does not allow active bindings to be unbound */
    if (R_BindingIsLocked(sym, R_BaseEnv))
	error(_("cannot unbind a locked binding"));
    if (R_BindingIsActive(sym, R_BaseEnv))
	error(_("cannot unbind an active binding"));
    SET_SYMVALUE(sym, R_UnboundValue);
#ifdef USE_GLOBAL_CACHE
    R_FlushGlobalCache(sym);
#endif
    return R_NilValue;
}

void R_RestoreHashCount(SEXP rho)
{
    if (IS_HASHED(rho)) {
	SEXP table;
	int i, count, size;

	table = HASHTAB(rho);
	size = HASHSIZE(table);
	for (i = 0, count = 0; i < size; i++)
	    if (VECTOR_ELT(table, i) != R_NilValue)
		count++;
	SET_HASHPRI(table, count);
    }
}

Rboolean R_IsPackageEnv(SEXP rho)
{
    if (TYPEOF(rho) == ENVSXP) {
	SEXP name = getAttrib(rho, R_NameSymbol);
	char *packprefix = "package:";
	size_t pplen = strlen(packprefix);
	if(isString(name) && length(name) > 0 &&
	   ! strncmp(packprefix, CHAR(STRING_ELT(name, 0)), pplen)) /* ASCII */
	    return TRUE;
	else
	    return FALSE;
    }
    else
	return FALSE;
}

SEXP R_PackageEnvName(SEXP rho)
{
    if (TYPEOF(rho) == ENVSXP) {
	SEXP name = getAttrib(rho, R_NameSymbol);
	char *packprefix = "package:";
	size_t pplen = strlen(packprefix);
	if(isString(name) && length(name) > 0 &&
	   ! strncmp(packprefix, CHAR(STRING_ELT(name, 0)), pplen)) /* ASCII */
	    return name;
	else
	    return R_NilValue;
    }
    else
	return R_NilValue;
}

SEXP R_FindPackageEnv(SEXP info)
{
    SEXP expr, val;
    PROTECT(info);
    PROTECT(expr = LCONS(install("findPackageEnv"), LCONS(info, R_NilValue)));
    val = eval(expr, R_GlobalEnv);
    UNPROTECT(2);
    return val;
}

Rboolean R_IsNamespaceEnv(SEXP rho)
{
    if (rho == R_BaseNamespace)
	return TRUE;
    else if (TYPEOF(rho) == ENVSXP) {
	SEXP info = findVarInFrame3(rho, R_NamespaceSymbol, TRUE);
	if (info != R_UnboundValue && TYPEOF(info) == ENVSXP) {
	    SEXP spec = findVarInFrame3(info, install("spec"), TRUE);
	    if (spec != R_UnboundValue &&
		TYPEOF(spec) == STRSXP && LENGTH(spec) > 0)
		return TRUE;
	    else
		return FALSE;
	}
	else return FALSE;
    }
    else return FALSE;
}

SEXP attribute_hidden do_isNSEnv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_IsNamespaceEnv(CAR(args)) ? mkTrue() : mkFalse();
}

SEXP R_NamespaceEnvSpec(SEXP rho)
{
    /* The namespace spec is a character vector that specifies the
       namespace.  The first element is the namespace name.  The
       second element, if present, is the namespace version.  Further
       elements may be added later. */
    if (rho == R_BaseNamespace)
	return R_BaseNamespaceName;
    else if (TYPEOF(rho) == ENVSXP) {
	SEXP info = findVarInFrame3(rho, R_NamespaceSymbol, TRUE);
	if (info != R_UnboundValue && TYPEOF(info) == ENVSXP) {
	    SEXP spec = findVarInFrame3(info, install("spec"), TRUE);
	    if (spec != R_UnboundValue &&
		TYPEOF(spec) == STRSXP && LENGTH(spec) > 0)
		return spec;
	    else
		return R_NilValue;
	}
	else return R_NilValue;
    }
    else return R_NilValue;
}

SEXP R_FindNamespace(SEXP info)
{
    SEXP expr, val;
    PROTECT(info);
    PROTECT(expr = LCONS(install("getNamespace"), LCONS(info, R_NilValue)));
    val = eval(expr, R_GlobalEnv);
    UNPROTECT(2);
    return val;
}

static SEXP checkNSname(SEXP call, SEXP name)
{
    switch (TYPEOF(name)) {
    case SYMSXP:
	break;
    case STRSXP:
	if (LENGTH(name) >= 1) {
	    name = installTrChar(STRING_ELT(name, 0));
	    break;
	}
	/* else fall through */
    default:
	errorcall(call, _("bad namespace name"));
    }
    return name;
}

SEXP attribute_hidden do_regNS(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP name, val;
    checkArity(op, args);
    name = checkNSname(call, CAR(args));
    val = CADR(args);
    if (findVarInFrame(R_NamespaceRegistry, name) != R_UnboundValue)
	errorcall(call, _("namespace already registered"));
    defineVar(name, val, R_NamespaceRegistry);
    return R_NilValue;
}

SEXP attribute_hidden do_unregNS(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP name;
    int hashcode;
    checkArity(op, args);
    name = checkNSname(call, CAR(args));
    if (findVarInFrame(R_NamespaceRegistry, name) == R_UnboundValue)
	errorcall(call, _("namespace not registered"));
    if( !HASHASH(PRINTNAME(name)))
	hashcode = R_Newhashpjw(CHAR(PRINTNAME(name)));
    else
	hashcode = HASHVALUE(PRINTNAME(name));
    RemoveVariable(name, hashcode, R_NamespaceRegistry);
    return R_NilValue;
}

SEXP attribute_hidden do_getRegNS(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP name, val;
    checkArity(op, args);
    name = checkNSname(call, CAR(args));
    val = findVarInFrame(R_NamespaceRegistry, name);
    if (val == R_UnboundValue)
	return R_NilValue;
    else
	return val;
}

SEXP attribute_hidden do_getNSRegistry(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_NamespaceRegistry;
}

SEXP attribute_hidden do_importIntoEnv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* This function copies values of variables from one environment
       to another environment, possibly with different names.
       Promises are not forced and active bindings are preserved. */
    SEXP impenv, impnames, expenv, expnames;
    SEXP impsym, expsym, val;
    int i, n;

    checkArity(op, args);

    impenv = CAR(args); args = CDR(args);
    impnames = CAR(args); args = CDR(args);
    expenv = CAR(args); args = CDR(args);
    expnames = CAR(args); args = CDR(args);

    if (TYPEOF(impenv) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(impenv) != ENVSXP &&
	TYPEOF((impenv = simple_as_environment(impenv))) != ENVSXP)
	error(_("bad import environment argument"));
    if (TYPEOF(expenv) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(expenv) != ENVSXP &&
	TYPEOF((expenv = simple_as_environment(expenv))) != ENVSXP)
	error(_("bad export environment argument"));
    if (TYPEOF(impnames) != STRSXP || TYPEOF(expnames) != STRSXP)
	error(_("invalid '%s' argument"), "names");
    if (LENGTH(impnames) != LENGTH(expnames))
	error(_("length of import and export names must match"));

    n = LENGTH(impnames);
    for (i = 0; i < n; i++) {
	impsym = installTrChar(STRING_ELT(impnames, i));
	expsym = installTrChar(STRING_ELT(expnames, i));

	/* find the binding--may be a CONS cell or a symbol */
	SEXP binding = R_NilValue;
	for (SEXP env = expenv;
	     env != R_EmptyEnv && binding == R_NilValue;
	     env = ENCLOS(env))
	    if (env == R_BaseNamespace) {
		if (SYMVALUE(expsym) != R_UnboundValue)
		    binding = expsym;
	    } else
		binding = findVarLocInFrame(env, expsym, NULL);
	if (binding == R_NilValue)
	    binding = expsym;

	/* get value of the binding; do not force promises */
	if (TYPEOF(binding) == SYMSXP) {
	    if (SYMVALUE(expsym) == R_UnboundValue)
		error(_("exported symbol '%s' has no value"),
		      CHAR(PRINTNAME(expsym)));
	    val = SYMVALUE(expsym);
	}
	else val = CAR(binding);

	/* import the binding */
	if (IS_ACTIVE_BINDING(binding))
	    R_MakeActiveBinding(impsym, val, impenv);
	/* This is just a tiny optimization */
	else if (impenv == R_BaseNamespace || impenv == R_BaseEnv)
	    gsetVar(impsym, val, impenv);
	else
	    defineVar(impsym, val, impenv);
    }
    return R_NilValue;
}


SEXP attribute_hidden do_envprofile(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* Return a list containing profiling information given a hashed
       environment.  For non-hashed environments, this function
       returns R_NilValue.  This seems appropriate since there is no
       way to test whether an environment is hashed at the R level.
    */
    SEXP env, ans = R_NilValue /* -Wall */;
    env = CAR(args);
    if (isEnvironment(env)) {
	if (IS_HASHED(env))
	    ans = R_HashProfile(HASHTAB(env));
    } else
	error("argument must be a hashed environment");
    return ans;
}

SEXP mkCharCE(const char *name, cetype_t enc)
{
    size_t len =  strlen(name);
    if (len > INT_MAX)
	error("R character strings are limited to 2^31-1 bytes");
   return mkCharLenCE(name, (int) len, enc);
}

/* no longer used in R but docuented in 2.7.x */
SEXP mkCharLen(const char *name, int len)
{
    return mkCharLenCE(name, len, CE_NATIVE);
}

SEXP mkChar(const char *name)
{
    size_t len =  strlen(name);
    if (len > INT_MAX)
	error("R character strings are limited to 2^31-1 bytes");
    return mkCharLenCE(name, (int) len, CE_NATIVE);
}

/* Global CHARSXP cache and code for char-based hash tables */

/* We can reuse the hash structure, but need separate code for get/set
   of values since our keys are char* and not SEXP symbol types.

   Experience has shown that it is better to use a different hash function,
   and a power of 2 for the hash size.
*/

/* char_hash_size MUST be a power of 2 and char_hash_mask ==
   char_hash_size - 1 for x & char_hash_mask to be equivalent to x %
   char_hash_size.
*/
static unsigned int char_hash_size = 65536;
static unsigned int char_hash_mask = 65535;

static unsigned int char_hash(const char *s, int len)
{
    /* djb2 as from http://www.cse.yorku.ca/~oz/hash.html */
    char *p;
    int i;
    unsigned int h = 5381;
    for (p = (char *) s, i = 0; i < len; p++, i++)
	h = ((h << 5) + h) + (*p);
    return h;
}

void attribute_hidden InitStringHash()
{
    R_StringHash = R_NewHashTable(char_hash_size);
}

/* #define DEBUG_GLOBAL_STRING_HASH 1 */

/* Resize the global R_StringHash CHARSXP cache */
static void R_StringHash_resize(unsigned int newsize)
{
    SEXP old_table = R_StringHash;
    SEXP new_table, chain, new_chain, val, next;
    unsigned int counter, new_hashcode, newmask;
#ifdef DEBUG_GLOBAL_STRING_HASH
    unsigned int oldsize = HASHSIZE(R_StringHash);
    unsigned int oldpri = HASHPRI(R_StringHash);
    unsigned int newsize, newpri;
#endif

    /* Allocate the new hash table.  This could fail to allocate
       enough memory, and ideally we would recover from that and
       carry over with a table that was getting full.
     */
    /* When using the ATTRIB fields to maintain the chains the chain
       moving is destructive and does not involve allocation.  This is
       therefore the only point where GC can occur. */
    new_table = R_NewHashTable(newsize);
    newmask = newsize - 1;

    /* transfer chains from old table to new table */
    for (counter = 0; counter < LENGTH(old_table); counter++) {
	chain = VECTOR_ELT(old_table, counter);
	while (!ISNULL(chain)) {
	    val = CXHEAD(chain);
	    next = CXTAIL(chain);
	    new_hashcode = char_hash(CHAR(val), LENGTH(val)) & newmask;
	    new_chain = VECTOR_ELT(new_table, new_hashcode);
	    /* If using a primary slot then increase HASHPRI */
	    if (ISNULL(new_chain))
		SET_HASHPRI(new_table, HASHPRI(new_table) + 1);
	    /* move the current chain link to the new chain */
	    /* this is a destrictive modification */
	    new_chain = SET_CXTAIL(val, new_chain);
	    SET_VECTOR_ELT(new_table, new_hashcode, new_chain);
	    chain = next;
	}
    }
    R_StringHash = new_table;
    char_hash_size = newsize;
    char_hash_mask = newmask;
#ifdef DEBUG_GLOBAL_STRING_HASH
    newsize = HASHSIZE(new_table);
    newpri = HASHPRI(new_table);
    Rprintf("Resized: size %d => %d\tpri %d => %d\n",
	    oldsize, newsize, oldpri, newpri);
#endif
}

/* mkCharCE - make a character (CHARSXP) variable and set its
   encoding bit.  If a CHARSXP with the same string already exists in
   the global CHARSXP cache, R_StringHash, it is returned.  Otherwise,
   a new CHARSXP is created, added to the cache and then returned. */


SEXP mkCharLenCE(const char *name, int len, cetype_t enc)
{
    SEXP cval, chain;
    unsigned int hashcode;
    int need_enc;
    Rboolean embedNul = FALSE, is_ascii = TRUE;

    switch(enc){
    case CE_NATIVE:
    case CE_UTF8:
    case CE_LATIN1:
    case CE_BYTES:
    case CE_SYMBOL:
    case CE_ANY:
	break;
    default:
	error(_("unknown encoding: %d"), enc);
    }
    for (int slen = 0; slen < len; slen++) {
	if ((unsigned int) name[slen] > 127) is_ascii = FALSE;
	if (!name[slen]) embedNul = TRUE;
    }
    if (embedNul) {
	SEXP c;
	/* This is tricky: we want to make a reasonable job of
	   representing this string, and EncodeString() is the most
	   comprehensive */
	c = allocCharsxp(len);
	memcpy(CHAR_RW(c), name, len);
	switch(enc) {
	case CE_UTF8: SET_UTF8(c); break;
	case CE_LATIN1: SET_LATIN1(c); break;
	case CE_BYTES: SET_BYTES(c); break;
	default: break;
	}
	if (is_ascii) SET_ASCII(c);
	error(_("embedded nul in string: '%s'"),
	      EncodeString(c, 0, 0, Rprt_adj_none));
    }

    if (enc && is_ascii) enc = CE_NATIVE;
    switch(enc) {
    case CE_UTF8: need_enc = UTF8_MASK; break;
    case CE_LATIN1: need_enc = LATIN1_MASK; break;
    case CE_BYTES: need_enc = BYTES_MASK; break;
    default: need_enc = 0;
    }

    hashcode = char_hash(name, len) & char_hash_mask;

    /* Search for a cached value */
    cval = R_NilValue;
    chain = VECTOR_ELT(R_StringHash, hashcode);
    for (; !ISNULL(chain) ; chain = CXTAIL(chain)) {
	SEXP val = CXHEAD(chain);
	if (TYPEOF(val) != CHARSXP) break; /* sanity check */
	if (need_enc == (ENC_KNOWN(val) | IS_BYTES(val)) &&
	    LENGTH(val) == len &&  /* quick pretest */
	    memcmp(CHAR(val), name, len) == 0) {
	    cval = val;
	    break;
	}
    }
    if (cval == R_NilValue) {
	/* no cached value; need to allocate one and add to the cache */
	PROTECT(cval = allocCharsxp(len));
	memcpy(CHAR_RW(cval), name, len);
	switch(enc) {
	case CE_NATIVE:
	    break;          /* don't set encoding */
	case CE_UTF8:
	    SET_UTF8(cval);
	    break;
	case CE_LATIN1:
	    SET_LATIN1(cval);
	    break;
	case CE_BYTES:
	    SET_BYTES(cval);
	    break;
	default:
	    error("unknown encoding mask: %d", enc);
	}
	if (is_ascii) SET_ASCII(cval);
	SET_CACHED(cval);  /* Mark it */
	/* add the new value to the cache */
	chain = VECTOR_ELT(R_StringHash, hashcode);
	if (ISNULL(chain))
	    SET_HASHPRI(R_StringHash, HASHPRI(R_StringHash) + 1);
	/* this is a destrictive modification */
	chain = SET_CXTAIL(cval, chain);
	SET_VECTOR_ELT(R_StringHash, hashcode, chain);

	/* resize the hash table if necessary with the new entry still
	   protected.
	   Maximum possible power of two is 2^30 for a VECSXP.
	   FIXME: this has changed with long vectors.
	*/
	if (R_HashSizeCheck(R_StringHash)
	    && char_hash_size < 1073741824 /* 2^30 */)
	    R_StringHash_resize(char_hash_size * 2);

	UNPROTECT(1);
    }
    return cval;
}


#ifdef DEBUG_SHOW_CHARSXP_CACHE
/* Call this from gdb with

       call do_show_cache(10)

   for the first 10 cache chains in use. */
void do_show_cache(int n)
{
    int i, j;
    Rprintf("Cache size: %d\n", LENGTH(R_StringHash));
    Rprintf("Cache pri:  %d\n", HASHPRI(R_StringHash));
    for (i = 0, j = 0; j < n && i < LENGTH(R_StringHash); i++) {
	SEXP chain = VECTOR_ELT(R_StringHash, i);
	if (! ISNULL(chain)) {
	    Rprintf("Line %d: ", i);
	    do {
		if (IS_UTF8(CXHEAD(chain)))
		    Rprintf("U");
		else if (IS_LATIN1(CXHEAD(chain)))
		    Rprintf("L");
		else if (IS_BYTES(CXHEAD(chain)))
		    Rprintf("B");
		Rprintf("|%s| ", CHAR(CXHEAD(chain)));
		chain = CXTAIL(chain);
	    } while(! ISNULL(chain));
	    Rprintf("\n");
	    j++;
	}
    }
}

void do_write_cache()
{
    int i;
    FILE *f = fopen("/tmp/CACHE", "w");
    if (f != NULL) {
	fprintf(f, "Cache size: %d\n", LENGTH(R_StringHash));
	fprintf(f, "Cache pri:  %d\n", HASHPRI(R_StringHash));
	for (i = 0; i < LENGTH(R_StringHash); i++) {
	    SEXP chain = VECTOR_ELT(R_StringHash, i);
	    if (! ISNULL(chain)) {
		fprintf(f, "Line %d: ", i);
		do {
		    if (IS_UTF8(CXHEAD(chain)))
			fprintf(f, "U");
		    else if (IS_LATIN1(CXHEAD(chain)))
			fprintf(f, "L");
		    else if (IS_BYTES(CXHEAD(chain)))
			fprintf(f, "B");
		    fprintf(f, "|%s| ", CHAR(CXHEAD(chain)));
		    chain = CXTAIL(chain);
		} while(! ISNULL(chain));
		fprintf(f, "\n");
	    }
	}
	fclose(f);
    }
}
#endif /* DEBUG_SHOW_CHARSXP_CACHE */
