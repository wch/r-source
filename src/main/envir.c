/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2007 the R Development Core Group.
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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
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

/* <UTF8> char here is either ASCII or handled as a whole */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "Defn.h"
#include <R_ext/Callbacks.h>

#define IS_USER_DATABASE(rho)  OBJECT((rho)) && inherits((rho), "UserDefinedDatabase")

/* various definitions of macros/functions in Defn.h */

#define FRAME_LOCK_MASK (1<<14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define LOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) | FRAME_LOCK_MASK)
/*#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))*/

/* use the same bits (15 and 14) in symbols and bindings */
#define BINDING_VALUE(b) ((IS_ACTIVE_BINDING(b) ? getActiveValue(CAR(b)) : CAR(b)))

#define SYMBOL_BINDING_VALUE(s) ((IS_ACTIVE_BINDING(s) ? getActiveValue(SYMVALUE(s)) : SYMVALUE(s)))

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
    SEXP s_quote = install("quote");
    SEXP arg = LCONS(s_quote, LCONS(val, R_NilValue));
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
#define SET_HASHSIZE(x,v)    SETLENGTH(x,v)
#define SET_HASHPRI(x,v)     SET_TRUELENGTH(x,v)

#define IS_HASHED(x)	     (HASHTAB(x) != R_NilValue)

/*----------------------------------------------------------------------

  String Hashing

  This is taken from the second edition of the "Dragon Book" by
  Aho, Ullman and Sethi.

*/

/* was extern: used in this file and names.c */
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
    for (; !isNull(chain); chain = CDR(chain))
	if (TAG(chain) == symbol) {
	    SET_BINDING_VALUE(chain, value);
	    SET_MISSING(chain, 0);	/* Over-ride for new value */
	    return;
	}
    if (frame_locked)
	error(_("cannot add bindings to a locked environment"));
    if (isNull(chain))
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
    for (; !isNull(chain); chain = CDR(chain))
	if (TAG(chain) == symbol) return chain;
    /* If not found */
    return R_NilValue;
}



/*----------------------------------------------------------------------

  R_NewHashTable

  Hash table initialisation function.  Creates a table of size 'size'
  that increases in size by 'growth_rate' after a threshold is met.

*/

static SEXP R_NewHashTable(int size, int growth_rate)
{
    SEXP table;

    /* Some checking */
    if (growth_rate <= 0) growth_rate =  HASHTABLEGROWTHRATE;
    if (size <= 0) size = HASHMINSIZE;

    /* Allocate hash table in the form of a vector */
    PROTECT(table = allocVector(VECSXP, size));
    SET_HASHSIZE(table, size);
    SET_HASHPRI(table, 0);
    UNPROTECT(1);
    return(table);
}

/*----------------------------------------------------------------------

  R_NewHashedEnv

  Returns a new environment with a hash table initialized with default
  size/growth settings.  The only non-static hash table function.
*/

SEXP R_NewHashedEnv(SEXP enclos, SEXP size)
{
    SEXP s;

    PROTECT(s = NewEnvironment(R_NilValue, R_NilValue, enclos));
    SET_HASHTAB(s, R_NewHashTable(asInteger(size), 0));
    UNPROTECT(1);
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
	if (TAG(lst) == symbol) lst = CDR(lst);
    }
    return lst;
}

static void R_HashDelete(int hashcode, SEXP symbol, SEXP table)
{
    SET_VECTOR_ELT(table, hashcode % HASHSIZE(table),
	DeleteItem(symbol, VECTOR_ELT(table, hashcode % HASHSIZE(table))));
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
    int /*hash_grow,*/ counter, new_hashcode;

    /* Do some checking */
    if (TYPEOF(table) != VECSXP)
	error("first argument ('table') not of type VECSXP, from R_HashResize");

    /* This may have to change.	 The growth rate should
       be independent of the size (not implemented yet) */
    /* hash_grow = HASHSIZE(table); */

    /* Allocate the new hash table */
    new_table = R_NewHashTable(HASHSIZE(table) * HASHTABLEGROWTHRATE,
			       HASHTABLEGROWTHRATE);
    for (counter = 0; counter < length(table); counter++) {
	chain = VECTOR_ELT(table, counter);
	while (!isNull(chain)) {
	    new_hashcode = R_Newhashpjw(CHAR(PRINTNAME(TAG(chain)))) %
		HASHSIZE(new_table);
	    new_chain = VECTOR_ELT(new_table, new_hashcode);
	    /* If using a primary slot then increase HASHPRI */
	    if (isNull(new_chain))
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
    while (!isNull(frame)) {
	if( !HASHASH(PRINTNAME(TAG(frame))) ) {
	    SET_HASHVALUE(PRINTNAME(TAG(frame)),
			  R_Newhashpjw(CHAR(PRINTNAME(TAG(frame)))));
	    SET_HASHASH(PRINTNAME(TAG(frame)), 1);
	}
	hashcode = HASHVALUE(PRINTNAME(TAG(frame))) % HASHSIZE(table);
	chain = VECTOR_ELT(table, hashcode);
	/* If using a primary slot then increase HASHPRI */
	if (isNull(chain)) SET_HASHPRI(table, HASHPRI(table) + 1);
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
#ifdef USE_GLOBAL_CACHE
/* Global variable caching.  A cache is maintained in a hash table,
   R_GlobalCache.  The entry values are either R_UnboundValue (a
   flushed cache entry), the binding LISTSXP cell from the environment
   containing the binding found in a search from R_GlobalEnv, or a
   symbol if the globally visible binding lives in the base package.
   The cache for a variable is flushed if a new binding for it is
   created in a global frame or if the variable is removed from any
   global frame.

   To make sure the cache is valid, all binding creations and removals
   from global frames must go through the interface functions in this
   file.

   Initially only the R_GlobalEnv frame is a global frame.  Additional
   global frames can only be created by attach.  All other frames are
   considered local.  Whether a frame is local or not is recorded in
   the highest order bit of the ENVFLAGS field (the gp field of
   sxpinfo).

   It is possible that the benefit of caching may be significantly
   reduced if we introduce name space management.  Since maintaining
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

void attribute_hidden InitBaseEnv()
{
    R_EmptyEnv = NewEnvironment(R_NilValue, R_NilValue, R_NilValue);
    R_BaseEnv = NewEnvironment(R_NilValue, R_NilValue, R_EmptyEnv);
}

void attribute_hidden InitGlobalEnv()
{
    R_GlobalEnv = NewEnvironment(R_NilValue, R_NilValue, R_BaseEnv);
#ifdef NEW_CODE
    HASHTAB(R_GlobalEnv) = R_NewHashTable(100, HASHTABLEGROWTHRATE);
#endif
#ifdef USE_GLOBAL_CACHE
    MARK_AS_GLOBAL_FRAME(R_GlobalEnv);
    R_GlobalCache = R_NewHashTable(INITIAL_CACHE_SIZE, HASHTABLEGROWTHRATE);
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
    defineVar(install("base"), R_BaseNamespace, R_NamespaceRegistry);
    /**** needed to properly initialize the base name space */
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
    if (entry != R_NilValue)
	SETCAR(entry, R_UnboundValue);
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
	R_FlushGlobalCache(Rf_install(CHAR(STRING_ELT(names,i))));
}

static void R_AddGlobalCache(SEXP symbol, SEXP place)
{
    int oldpri = HASHPRI(R_GlobalCache);
    R_HashSet(hashIndex(symbol, R_GlobalCache), symbol, R_GlobalCache, place,
	      FALSE);
    if (oldpri != HASHPRI(R_GlobalCache) &&
	HASHPRI(R_GlobalCache) > 0.85 * HASHSIZE(R_GlobalCache)) {
	R_GlobalCache = R_HashResize(R_GlobalCache);
	SETCAR(R_GlobalCachePreserve, R_GlobalCache);
    }
}

static SEXP R_GetGlobalCache(SEXP symbol)
{
    SEXP vl = R_HashGet(hashIndex(symbol, R_GlobalCache), symbol,
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
	return CDR(list);
    }
    else {
	SEXP last = list;
	SEXP next = CDR(list);
	while (next != R_NilValue) {
	    if (TAG(next) == thing) {
		*found = 1;
		SETCDR(last, CDR(next));
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

SEXP R_GetVarLocValue(R_varloc_t vl)
{
    return BINDING_VALUE((SEXP) vl);
}

SEXP R_GetVarLocSymbol(R_varloc_t vl)
{
    return TAG((SEXP) vl);
}

Rboolean R_GetVarLocMISSING(R_varloc_t vl)
{
    return MISSING((SEXP) vl);
}

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
        rval = strtol(buf, &endp, 10);
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

SEXP ddfindVar(SEXP symbol, SEXP rho)
{
    int i;
    SEXP vl;

    /* first look for the .. symbol itself */
    vl = findVarInFrame3(rho, symbol, TRUE);
    if (vl != R_UnboundValue)
	return(vl);

    i = ddVal(symbol);
    vl = findVarInFrame3(rho, R_DotsSymbol, TRUE);
    if (vl != R_UnboundValue) {
	if (length(vl) >= i) {
	    vl = nthcdr(vl, i - 1);
	    return(CAR(vl));
	}
	else
	    error(_("The ... list does not contain %d elements"), i);
    }
    else {
	vl = findVar(symbol, rho);
	if( vl != R_UnboundValue )
	    return(vl);
	error(_("..%d used in an incorrect context, no ... to look in"), i);
    }
    return R_NilValue;
}



/*----------------------------------------------------------------------

  dynamicFindVar

  This function does a variable lookup, but uses dynamic scoping rules
  rather than the lexical scoping rules used in findVar.

  Return R_UnboundValue if the symbol isn't located and the calling
  function needs to handle the errors.

*/

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
    while (rho != R_EmptyEnv) {
	/* This is not really right.  Any variable can mask a function */
#ifdef USE_GLOBAL_CACHE
	if (rho == R_GlobalEnv)
	    vl = findGlobalVar(symbol);
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
    error(_("could not find function \"%s\""), CHAR(PRINTNAME(symbol)));
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
	table->assign(CHAR(PRINTNAME(symbol)), value, table);
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
        return(table->assign(CHAR(PRINTNAME(symbol)), value, table));
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
    else
	name = install(translateChar(STRING_ELT(CAR(args), 0)));
    PROTECT(val = CADR(args));
    aenv = CADDR(args);
    if (TYPEOF(aenv) == NILSXP)
    	error(_("use of NULL environment is defunct"));
    if (TYPEOF(aenv) != ENVSXP)
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
    if (TYPEOF(envarg) != ENVSXP)
	error(_("invalid '%s' argument"), "envir");
    args = CDR(args);

    ginherits = asLogical(CAR(args));
    if (ginherits == NA_LOGICAL)
	error(_("invalid '%s' argument"), "inherits");

    for (i = 0; i < LENGTH(name); i++) {
	done = 0;
	tsym = install(translateChar(STRING_ELT(name, i)));
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
	    warning(_("variable \"%s\" was not found"),
		    CHAR(PRINTNAME(tsym)));
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
	t1 = install(translateChar(STRING_ELT(CAR(args), 0)));

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
    else {
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
	if (rval == R_UnboundValue) {
	    if (gmode == ANYSXP)
		error(_("variable \"%s\" was not found"),
		      CHAR(PRINTNAME(t1)));
	    else
		error(_("variable \"%s\" of mode \"%s\" was not found"),
		      CHAR(PRINTNAME(t1)),
		      CHAR(STRING_ELT(CAR(CDDR(args)), 0))); /* ASCII */
	}

	/* We need to evaluate if it is a promise */
	if (TYPEOF(rval) == PROMSXP)
	    rval = eval(rval, genv);

	if (!isNull(rval) && NAMED(rval) == 0)
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
    if (!isNull(rval) && NAMED(rval) == 0) SET_NAMED(rval, 1);
    return rval;
}

/* get multiple values from an environment */
SEXP attribute_hidden do_mget(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, env, x, mode, ifnotfound, ifnfnd;
    SEXPTYPE gmode; /* is unsigned int */
    int ginherits = 0, nvals, nmode, nifnfnd, i;

    checkArity(op, args);

    x = CAR(args);

    nvals = length(x);

    /* The first arg is the object name */
    /* It must be present and a string */
    if (!isString(x) )
	error(_("invalid first argument"));
    for(i = 0; i < nvals; i++)
	if( isNull(STRING_ELT(x, i)) || !CHAR(STRING_ELT(x, 0))[0] )
	    error(_("invalid name in position %d"), i+1);

    /* FIXME: should we install them all?) */

    env = CADR(args);
    if (isNull(env)) {
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

    /* now for each element of x, we look for it, using the inherits,
       etc */

    for(i = 0; i < nvals; i++) {
	if (isString(mode)) { /* ASCII */
	    if (!strcmp(CHAR(STRING_ELT(CAR(CDDR(args)), i % nmode )), "function"))
		gmode = FUNSXP;
	    else
		gmode = str2type(CHAR(STRING_ELT(CAR(CDDR(args)), i % nmode )));
	} else {
	    error(_("invalid '%s' argument"), "mode");
	    gmode = FUNSXP; /* -Wall */
	}

	/* is the mode provided one of the real modes? */
	if( gmode == (SEXPTYPE) (-1))
	    error(_("invalid '%s' argument"), "mode");


	if( TYPEOF(ifnotfound) != VECSXP )
	    error(_("invalid '%s' argument"), "ifnotfound");
	if( nifnfnd == 1 ) /* length has been checked to be 1 or nvals. */
	    ifnfnd = VECTOR_ELT(ifnotfound, 0);
	else
	    ifnfnd = VECTOR_ELT(ifnotfound, i);

	SET_VECTOR_ELT(ans, i, 
		       gfind(translateChar(STRING_ELT(x,i % nvals)), env,
			     gmode, ifnfnd, ginherits, rho));
    }

    setAttrib(ans, R_NamesSymbol, duplicate(x));
    UNPROTECT(2);
    return(ans);
}

/*----------------------------------------------------------------------

  do_missing

  This function tests whether the symbol passed as its first argument
  is a missing argument to the current closure.  rho is the
  environment that missing was called from.

  isMissing is called on the not-yet-evaluated value of an argument,
  if this is a symbol, as it could be a missing argument that has been
  passed down.  So 'symbol' is the promise value, and 'rho' its
  evaluation argument.

*/

static int isMissing(SEXP symbol, SEXP rho)
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
	if (TYPEOF(CAR(vl)) == PROMSXP &&
	    PRVALUE(CAR(vl)) == R_UnboundValue &&
	    TYPEOF(PREXPR(CAR(vl))) == SYMSXP)
	    return isMissing(PREXPR(CAR(vl)), PRENV(CAR(vl)));
	else
	    return 0;
    }
    return 0;
}

/* this is primitive */
SEXP attribute_hidden do_missing(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int ddv=0;
    SEXP rval, t, sym, s;

    checkArity(op, args);
    s = sym = CAR(args);
    if( isString(sym) && length(sym)==1 )
        s = sym = install(translateChar(STRING_ELT(CAR(args), 0)));
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
    else LOGICAL(rval)[0] = isMissing(PREXPR(t), PRENV(t));
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
	error(_("invalid value for '%s'"), "name");

    isSpecial = IS_USER_DATABASE(CAR(args));

    if(!isSpecial) {
	if (isNewList(CAR(args))) {
	    SETCAR(args, VectorToPairList(CAR(args)));

	    for (x = CAR(args); x != R_NilValue; x = CDR(x))
		if (TAG(x) == R_NilValue)
		    error(_("all elements of a list must be named"));
	    PROTECT(s = allocSExp(ENVSXP));
	    SET_FRAME(s, duplicate(CAR(args)));
	} else if (isEnvironment(CAR(args))) {
	    SEXP p, loadenv = CAR(args);

	    PROTECT(s = allocSExp(ENVSXP));
	    if (HASHTAB(loadenv) != R_NilValue) {
		int i, n;
		n = length(HASHTAB(loadenv));
		for (i = 0; i < n; i++) {
		    p = VECTOR_ELT(HASHTAB(loadenv), i);
		    while (p != R_NilValue) {
			defineVar(TAG(p), duplicate(CAR(p)), s);
			p = CDR(p);
		    }
		}
		/* FIXME: duplicate the hash table and assign here */
	    } else {
		for(p = FRAME(loadenv); p != R_NilValue; p = CDR(p))
		    defineVar(TAG(p), duplicate(CAR(p)), s);
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

	SET_HASHTAB(s, R_NewHashTable(hsize, HASHTABLEGROWTHRATE));
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

    setAttrib(s, install("name"), name);
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
    return FRAME(s);
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
	name = getAttrib(t, install("name"));
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
  functions.  [ ls(envir, all.names) ]

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
	    SET_VECTOR_ELT(values, *indx, duplicate(CAR(frame)));
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
		    SET_VECTOR_ELT(values, (*indx)++, duplicate(vl));
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
		    SET_VECTOR_ELT(values, (*indx)++, duplicate(vl));
		}
	    }
	}
    }
}

SEXP attribute_hidden do_ls(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env;
    int all;
    checkArity(op, args);

    if(IS_USER_DATABASE(CAR(args))) {
	R_ObjectTable *tb = (R_ObjectTable*)
	    R_ExternalPtrAddr(HASHTAB(CAR(args)));
        return(tb->objects(tb));
    }

    env = CAR(args);

    /* if (env == R_BaseNamespace) env = R_BaseEnv; */

    all = asLogical(CADR(args));
    if (all == NA_LOGICAL) all = 0;

    return R_lsInternal(env, all);
}

/* takes a *list* of environments and a boolean indicating whether to get all
   names */
SEXP R_lsInternal(SEXP env, Rboolean all)
{
    int  k;
    SEXP ans;


    /* Step 1 : Compute the Vector Size */
    k = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
        k += BuiltinSize(all, 0);
    else if (isEnvironment(env)) {
	if (HASHTAB(env) != R_NilValue)
	    k += HashTableSize(HASHTAB(env), all);
	else
	    k += FrameSize(FRAME(env), all);
    }
    else
        error(_("invalid '%s' argument"), "envir");

    /* Step 2 : Allocate and Fill the Result */
    PROTECT(ans = allocVector(STRSXP, k));
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
    sortVector(ans, FALSE);
    return ans;
}

/* transform an environment into a named list */

SEXP attribute_hidden do_env2list(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, ans, names;
    int k, all;

    checkArity(op, args);

    env = CAR(args);
    if (isNull(env))
    	error(_("use of NULL environment is defunct"));
    if( !isEnvironment(env) )
        error(_("argument must be an environment"));

    all = asLogical(CADR(args));
    if (all == NA_LOGICAL) all = 0;

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

    setAttrib(ans, R_NamesSymbol, names);
    UNPROTECT(2);
    return(ans);
}

/*
 * apply a function to all objects in an environment and return the
 * results in a list.
 * Equivalent to lapply(as.list(env, all.names=all.names), FUN, ...)
 */

SEXP attribute_hidden do_eapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, ans, names, R_fcall, FUN, tmp, tmp2, ind;
    int i, k, all;

    checkArity(op, args);

    env = eval(CAR(args), rho);
    if (isNull(env))
    	error(_("use of NULL environment is defunct"));
    if( !isEnvironment(env) )
        error(_("argument must be an environment"));

    FUN = CADR(args);
    if (!isSymbol(FUN))
        error(_("arguments must be symbolic"));

    all = asLogical(eval(CADDR(args), rho));
    if (all == NA_LOGICAL) all = 0;

    if (env == R_BaseEnv || env == R_BaseNamespace)
        k = BuiltinSize(all, 0);
    else if (HASHTAB(env) != R_NilValue)
        k = HashTableSize(HASHTAB(env), all);
    else
        k = FrameSize(FRAME(env), all);

    PROTECT(names = allocVector(STRSXP, k));
    PROTECT(ans = allocVector(VECSXP, k));
    PROTECT(tmp2 = allocVector(VECSXP, k));

    k = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	BuiltinValues(all, 0, tmp2, &k);
    else if (HASHTAB(env) != R_NilValue)
	HashTableValues(HASHTAB(env), all, tmp2, &k);
    else
	FrameValues(FRAME(env), all, tmp2, &k);

    PROTECT(ind = allocVector(INTSXP, 1));
    PROTECT(tmp = LCONS(R_Bracket2Symbol,
			LCONS(tmp2, LCONS(ind, R_NilValue))));
    PROTECT(R_fcall = LCONS(FUN, LCONS(tmp, LCONS(R_DotsSymbol, R_NilValue))));

    for(i = 0; i < k; i++) {
      INTEGER(ind)[0] = i+1;
      SET_VECTOR_ELT(ans, i, eval(R_fcall, rho));
    }

    k = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	BuiltinNames(all, 0, names, &k);
    else if(HASHTAB(env) != R_NilValue)
        HashTableNames(HASHTAB(env), all, names, &k);
    else
        FrameNames(FRAME(env), all, names, &k);

    setAttrib(ans, R_NamesSymbol, names);
    UNPROTECT(6);
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

  do_libfixup

  This function copies the bindings in the loading environment to the
  library environment frame (the one that gets put in the search path)
  and removes the bindings from the loading environment.  Values that
  contain promises (created by delayedAssign, for example) are not forced.
  Values that are closures with environments equal to the loading
  environment are reparented to .GlobalEnv.  Finally, all bindings are
  removed from the loading environment.

  This routine can die if we automatically create a name space when
  loading a package.
*/

SEXP attribute_hidden do_libfixup(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP libenv, loadenv, p;
    checkArity(op, args);
    loadenv = CAR(args);
    libenv = CADR(args);
    if (TYPEOF(libenv) != ENVSXP || !isEnvironment(loadenv))
	errorcall(call, _("invalid arguments"));
    if (HASHTAB(loadenv) != R_NilValue) {
	int i, n;
	n = length(HASHTAB(loadenv));
	for (i = 0; i < n; i++) {
	    p = VECTOR_ELT(HASHTAB(loadenv), i);
	    while (p != R_NilValue) {
		if (TYPEOF(CAR(p)) == CLOSXP && CLOENV(CAR(p)) == loadenv)
		    SET_CLOENV(CAR(p), R_GlobalEnv);
		defineVar(TAG(p), CAR(p), libenv);
		p = CDR(p);
	    }
	}
    }
    else {
	p = FRAME(loadenv);
	while (p != R_NilValue) {
	    if (TYPEOF(CAR(p)) == CLOSXP && CLOENV(CAR(p)) == loadenv)
		SET_CLOENV(CAR(p), R_GlobalEnv);
	    defineVar(TAG(p), CAR(p), libenv);
	    p = CDR(p);
	}
    }
    SET_HASHTAB(loadenv, R_NilValue);
    SET_FRAME(loadenv, R_NilValue);
    return libenv;
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
	errorcall(call, R_MSG_IA);
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
	    errorcall(call, R_MSG_IA);
    }
    else {
	for (env = R_GlobalEnv; env != R_EmptyEnv && pos > 1;
	     env = ENCLOS(env))
	    pos--;
	if (pos != 1)
	    errorcall(call, R_MSG_IA);
    }
    return env;
}

/* this is primitive */
SEXP attribute_hidden do_pos2env(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, pos;
    int i, npos;
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
    SEXP t, name, nameSymbol;
    if(!strcmp(".GlobalEnv", what))
	return R_GlobalEnv;
    if(!strcmp("package:base", what))
	return R_BaseEnv;
    nameSymbol = install("name");
    for (t = ENCLOS(R_GlobalEnv); t != R_EmptyEnv ; t = ENCLOS(t)) {
	name = getAttrib(t, nameSymbol);
	if(isString(name) && length(name) > 0 &&
	   !strcmp(translateChar(STRING_ELT(name, 0)), what))
	    return t;
    }
    errorcall(call, _("no item called \"%s\" on the search list"), what);
    return R_NilValue;
}

/* This is primitive */
SEXP attribute_hidden 
do_as_environment(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP arg = CAR(args);
    checkArity(op, args);
    if(isEnvironment(arg))
	return arg;
    switch(TYPEOF(arg)) {
    case STRSXP:
	return matchEnvir(call, translateChar(asChar(arg)));
    case REALSXP:
    case INTSXP:
	return do_pos2env(call, op, args, rho);
    case NILSXP:
	errorcall(call,_("using 'as.environment(NULL)' is defunct"));
	return R_BaseEnv;	/* -Wall */
    default:
	errorcall(call, _("invalid object for 'as.environment'"));
	return R_NilValue;	/* -Wall */
    }
}

void R_LockEnvironment(SEXP env, Rboolean bindings)
{
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
    if (TYPEOF(env) != ENVSXP)
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
    if (TYPEOF(env) != ENVSXP)
	error(_("not an environment"));
    if (env == R_BaseEnv || env == R_BaseNamespace)
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	LOCK_BINDING(sym);
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue)
	    error(_("no binding for \"%s\""), CHAR(PRINTNAME(sym)));
	LOCK_BINDING(binding);
    }
}

void R_unLockBinding(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error(_("not a symbol"));
    if (TYPEOF(env) == NILSXP)
    	error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP)
	error(_("not an environment"));
    if (env == R_BaseEnv || env == R_BaseNamespace)
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	UNLOCK_BINDING(sym);
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue)
	    error(_("no binding for \"%s\""), CHAR(PRINTNAME(sym)));
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
    if (TYPEOF(env) != ENVSXP)
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
    if (TYPEOF(env) != ENVSXP)
	error(_("not an environment"));
    if (env == R_BaseEnv || env == R_BaseNamespace)
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	return BINDING_IS_LOCKED(sym) != 0;
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue)
	    error(_("no binding for \"%s\""), CHAR(PRINTNAME(sym)));
	return BINDING_IS_LOCKED(binding) != 0;
    }
}

Rboolean R_BindingIsActive(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error(_("not a symbol"));
    if (TYPEOF(env) == NILSXP)
    	error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP)
	error(_("not an environment"));
    if (env == R_BaseEnv || env == R_BaseNamespace)
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	return IS_ACTIVE_BINDING(sym) != 0;
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue)
	    error(_("no binding for \"%s\""), CHAR(PRINTNAME(sym)));
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

/* This is a .Internal with no wrapper */
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
    SEXP nameSymbol = install("name");
    if (TYPEOF(rho) == ENVSXP) {
	SEXP name = getAttrib(rho, nameSymbol);
	char *packprefix = "package:";
	int pplen = strlen(packprefix);
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
    SEXP nameSymbol = install("name");
    if (TYPEOF(rho) == ENVSXP) {
	SEXP name = getAttrib(rho, nameSymbol);
	char *packprefix = "package:";
	int pplen = strlen(packprefix);
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
	SEXP info = findVarInFrame3(rho, install(".__NAMESPACE__."), TRUE);
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
    /* The name space spec is a character vector that specifies the
       name space.  The first element is the name space name.  The
       second element, if present, is the name space version.  Further
       elements may be added later. */
    if (rho == R_BaseNamespace)
	return R_BaseNamespaceName;
    else if (TYPEOF(rho) == ENVSXP) {
	SEXP info = findVarInFrame3(rho, install(".__NAMESPACE__."), TRUE);
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
	    name = install(translateChar(STRING_ELT(name, 0)));
	    break;
	}
	/* else fall through */
    default:
	errorcall(call, _("bad name space name"));
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
	errorcall(call, _("name space already registered"));
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
	errorcall(call, _("name space not registered"));
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
    SEXP impsym, expsym, binding, env, val;
    int i, n;

    checkArity(op, args);

    impenv = CAR(args); args = CDR(args);
    impnames = CAR(args); args = CDR(args);
    expenv = CAR(args); args = CDR(args);
    expnames = CAR(args); args = CDR(args);

    if (TYPEOF(impenv) == NILSXP)
    	error(_("use of NULL environment is defunct"));
    if (TYPEOF(impenv) != ENVSXP)
	error(_("bad import environment argument"));
    if (TYPEOF(expenv) == NILSXP)
    	error(_("use of NULL environment is defunct"));
    if (TYPEOF(expenv) != ENVSXP)
	error(_("bad export environment argument"));
    if (TYPEOF(impnames) != STRSXP || TYPEOF(expnames) != STRSXP)
	error(_("invalid '%s' argument"), "names");
    if (LENGTH(impnames) != LENGTH(expnames))
	error(_("length of import and export names must match"));

    n = LENGTH(impnames);
    for (i = 0; i < n; i++) {
	impsym = install(translateChar(STRING_ELT(impnames, i)));
	expsym = install(translateChar(STRING_ELT(expnames, i)));

	/* find the binding--may be a CONS cell or a symbol */
	for (env = expenv, binding = R_NilValue;
	     env != R_EmptyEnv && binding == R_NilValue;
	     env = ENCLOS(env))
	    if (env == R_BaseNamespace) {
		if (SYMVALUE(expsym) != R_UnboundValue)
		    binding = expsym;
	    }
	    else
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


/* Global CHARSXP cache and code for char-based hash tables */

/* We can reuse the hash structure, but need separate code for get/set
   of values since our keys are char* and not SEXP symbol types. */

void attribute_hidden InitStringHash()
{
    const int STRING_HASH_INIT_SIZE = 54979;
    R_StringHash = R_NewHashTable(STRING_HASH_INIT_SIZE, 0);
}

#define NEXT_CHAIN_EL(e) (CDR(e))
#define SET_NEXT_CHAIN_EL(e1, e2) (SETCDR(e1, e2))

#define INT_IS_LATIN1(x) (x & LATIN1_MASK)
#define INT_IS_UTF8(x)   (x & UTF8_MASK)
#define STRINGHASHCMP(s, k, e) ( \
                                ((INT_IS_UTF8(e) == IS_UTF8(s)) &&    \
                                 (INT_IS_LATIN1(e) == IS_LATIN1(s)))  && \
                                strcmp(CHAR(s), k) == 0)

#define CHARENCCMP(a, b) ( \
                         ((IS_UTF8(a) == IS_UTF8(b)) && \
                          (IS_UTF8(a) == IS_UTF8(b))))

/* Resize a hash table with char* based keys.  The new table will be
   sized according to HASHTABLEGROWTHRATE. */
static SEXP R_CharHashResize(SEXP table)
{
    SEXP new_table, chain, new_chain, val;
    int /*hash_grow,*/ counter, new_hashcode;

    /* Do some checking */
    if (TYPEOF(table) != VECSXP)
	error("first argument ('table') not of type VECSXP, from R_StringHashResize");

    /* Allocate the new hash table */
    new_table = R_NewHashTable(HASHSIZE(table) * HASHTABLEGROWTHRATE,
			       HASHTABLEGROWTHRATE);
    PROTECT(new_table);
    for (counter = 0; counter < length(table); counter++) {
	chain = VECTOR_ELT(table, counter);
	while (!isNull(chain)) {
            val = CAR(chain);
            if (*CHAR(val) != '\0')
                new_hashcode = R_Newhashpjw(CHAR(val)) % HASHSIZE(new_table);
            else
                new_hashcode = 0;
	    new_chain = VECTOR_ELT(new_table, new_hashcode);
	    /* If using a primary slot then increase HASHPRI */
	    if (isNull(new_chain))
		SET_HASHPRI(new_table, HASHPRI(new_table) + 1);
	    SET_VECTOR_ELT(new_table, new_hashcode,  CONS(val, new_chain));
	    chain = NEXT_CHAIN_EL(chain);
	}
    }
    UNPROTECT(1);
    return new_table;
}

/* Resize the global R_StringHash CHARSXP cache */
static void R_StringHash_resize()
{
    SEXP new_table;

    /* Normally, GC can modify R_StringHash by null-ifying unmarked CHARSXPs.
       Since GC can occur during the resize, we temporarily preserve all CHARSXPs
       in the cache to avoid any changes during the resize op.
     */
    R_PreserveObject(R_StringHash);

    PROTECT(new_table = R_CharHashResize(R_StringHash));

#ifdef DEBUG_GLOBAL_STRING_HASH
    char *status = HASHPRI(new_table) > HASHPRI(R_StringHash) ? " OK" : "BAD";
    Rprintf("Resized: size %d => %d\tpri %d => %d\t%s\n",
            HASHSIZE(R_StringHash), HASHSIZE(new_table),
            HASHPRI(table), HASHPRI(new_table), status);
#endif
    UNPROTECT(1);
    R_ReleaseObject(R_StringHash);
    R_StringHash = new_table;
}


/* Get CHARSXP for string s in the hash table.
   Return R_NilValue, if not found.
*/
static SEXP R_CharHashGet(int hashcode, const char *s, int enc, SEXP table)
{
    SEXP chain, val;

    /* Grab the chain from the hashtable */
    chain = VECTOR_ELT(table, hashcode);
    /* Retrieve the value from the chain */
    for (; chain != R_NilValue ; chain = CDR(chain)) {
        val = CAR(chain);
	if (STRINGHASHCMP(val, s, enc))
            return val;
    }
    /* If not found */
    return R_NilValue;
}

/* Get a CHARSXP from the global cache, R_NilValue if not found */
#define R_StringHash_get(h, s, e) (R_CharHashGet(h, s, e, R_StringHash))


/* Add CHARSXP, schar to the table using key CHAR(schar).  If a
   CHARSXP with that key already exists, it is replaced.
*/
static void R_CharHashSet(int hashcode, SEXP schar, SEXP table)
{
    SEXP chain, val;

    /* Grab the chain from the hashtable */
    chain = VECTOR_ELT(table, hashcode);

    /* Search for the value in the chain */
    for (; !isNull(chain); chain = CDR(chain)) {
        val = CAR(chain);
	if (CHARENCCMP(val, schar) &&
            strcmp(CHAR(val), CHAR(schar)) == 0) {
            SETCAR(chain, schar); /* set to new value */
	    return;
	}
    }
    chain = VECTOR_ELT(table, hashcode);
    if (isNull(chain))
	SET_HASHPRI(table, HASHPRI(table) + 1);
    /* Add the value into the chain */
    SET_VECTOR_ELT(table, hashcode, CONS(schar, chain));
    return;
}

/* Add a CHARSXP to the global cache */
#define R_StringHash_set(h, s) (R_CharHashSet(h, s, R_StringHash))

/* mkCharEnc - make a character (CHARSXP) variable and set its
   encoding bit.  If a CHARSXP with the same string already exists in
   the global CHARSXP cache, R_StringHash, it is returned.  Otherwise,
   a new CHARSXP is created, added to the cache and then returned. */
SEXP mkCharEnc(const char *name, int enc)
{
    SEXP cval;
    int h = 0;

    if (enc != 0 && enc != UTF8_MASK &&
        enc != LATIN1_MASK)
        error("unknown encoding mask: %d", enc);

    if (R_HashSizeCheck(R_StringHash)) {
        R_StringHash_resize();
    }

    /* have to handle "" specially since it doesn't hash */
    if (*name != '\0')
        h = R_Newhashpjw(name) % HASHSIZE(R_StringHash);
    cval = R_StringHash_get(h, name, enc);
    if (cval == R_NilValue) {
        PROTECT(cval = allocString(strlen(name)));
        strcpy(CHAR_RW(cval), name);
        switch(enc) {
        case 0:
            break;          /* don't set encoding */
        case UTF8_MASK:
            SET_UTF8(cval);
            break;
        case LATIN1_MASK:
            SET_LATIN1(cval);
            break;
        default:
            error("unknown encoding mask: %d", enc);
        }
        R_StringHash_set(h, cval);
        UNPROTECT(1);
    }
    return cval;
}

/* mkChar - make a character (CHARSXP) variable.  If a CHARSXP with
   the same string already exists in the global CHARSXP cache,
   R_StringHash, it is returned.  Otherwise, a new CHARSXP is created,
   added to the cache and then returned. */
SEXP mkChar(const char *name)
{
    return mkCharEnc(name, 0);
}
