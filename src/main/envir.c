/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2002 the R Development Core Group.
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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

/* Some experimental options:
 *
 * EXPERIMENTAL_NAMESPACES: When this is defined the variable
 *     R_BaseNamespace holds an environment that has R_GlobalEnv as
 *     its parent.  This environment does not actually contain any
 *     bindings of its own.  Instead, it redirects all fetches and
 *     assignments to the SYMVALUE fields of the base (NULL)
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
 *     This code is experimental; if it doesn't seem useful it can be
 *     removed.
 *
 * LT */

/* This is needed for now for the write barrier test to work.  But
   since it disables testing of this file it should either be removed
   or at least limited to code that really needs it. LT */
#define USE_RINTERNALS

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include <R_ext/Callbacks.h>

#define IS_USER_DATABASE(rho)  OBJECT((rho)) && inherits((rho), "UserDefinedDatabase")


#ifdef ENVIRONMENT_LOCKING
#define FRAME_LOCK_MASK (1<<14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define LOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) | FRAME_LOCK_MASK)
/*#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))*/
#endif

#ifdef FANCY_BINDINGS
#ifndef NEW_BINDING_FLAGS
#error need NEW_BINDING_FLAGS to be defined
#endif
#ifndef NEW_SYMBOL_FLAGS
#error need NEW_SYMBOL_FLAGS to be defined
#endif

/* use the same bits (15 and 14) in symbols and bindings */
#define ACTIVE_BINDING_MASK (1<<15)
#define BINDING_LOCK_MASK (1<<14) 
#define SPECIAL_BINDING_MASK (ACTIVE_BINDING_MASK | BINDING_LOCK_MASK)
#define IS_ACTIVE_BINDING(b) ((b)->sxpinfo.gp & ACTIVE_BINDING_MASK)
#define BINDING_IS_LOCKED(b) ((b)->sxpinfo.gp & BINDING_LOCK_MASK)
#define SET_ACTIVE_BINDING_BIT(b) ((b)->sxpinfo.gp |= ACTIVE_BINDING_MASK)
#define LOCK_BINDING(b) ((b)->sxpinfo.gp |= BINDING_LOCK_MASK)

#define BINDING_VALUE(b) ((IS_ACTIVE_BINDING(b) ? getActiveValue(CAR(b)) : CAR(b)))

#define SYMBOL_BINDING_VALUE(s) ((IS_ACTIVE_BINDING(s) ? getActiveValue(SYMVALUE(s)) : SYMVALUE(s)))

#define SET_BINDING_VALUE(b,val) do { \
  SEXP __b__ = (b); \
  SEXP __val__ = (val); \
  if (BINDING_IS_LOCKED(__b__)) \
    error("can't change value of a locked binding"); \
  if (IS_ACTIVE_BINDING(__b__)) \
    setActiveValue(CAR(__b__), __val__); \
  else \
    SETCAR(__b__, __val__); \
} while (0)

#define SET_SYMBOL_BINDING_VALUE(sym, val) do { \
  SEXP __sym__ = (sym); \
  SEXP __val__ = (val); \
  if (BINDING_IS_LOCKED(__sym__)) \
    error("can't change value of a locked binding"); \
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
#else /* no FANCY_BINDINGS */
#define BINDING_VALUE(b) CAR(b)
#define SYMBOL_BINDING_VALUE(s) SYMVALUE(s)
#define SET_BINDING_VALUE(b,val) SETCAR(b,val)
#define SET_SYMBOL_BINDING_VALUE(sym, val) SET_SYMVALUE(sym,val)
#endif /* FANCY_BINDINGS */

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

extern int R_Newhashpjw(char *s)
{
    char *p;
    unsigned h = 0, g;
    for (p = s; *p; p = p + 1) {
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

#ifdef ENVIRONMENT_LOCKING
static void R_HashSet(int hashcode, SEXP symbol, SEXP table, SEXP value,
		      Rboolean frame_locked)
#else
static void R_HashSet(int hashcode, SEXP symbol, SEXP table, SEXP value)
#endif
{
    SEXP chain;

    /* Grab the chain from the hashtable */
    chain = VECTOR_ELT(table, hashcode);
#ifndef ENVIRONMENT_LOCKING
    if (isNull(chain)) {
	SET_HASHPRI(table, HASHPRI(table) + 1);
    }
#endif
    /* Add the value into the chain */
    for (; !isNull(chain); chain = CDR(chain)) {
	if (TAG(chain) == symbol) {
	    SET_BINDING_VALUE(chain, value);
	    return;
	}
    }
#ifdef ENVIRONMENT_LOCKING
    if (frame_locked)
	error("can't add bindings to a locked environment");
    if (isNull(chain)) {
	SET_HASHPRI(table, HASHPRI(table) + 1);
    }
#endif
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
    for (; chain != R_NilValue ; chain = CDR(chain)) {
	if (TAG(chain) == symbol) {
	    return BINDING_VALUE(chain);
	}
    }
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
    for (; !isNull(chain); chain = CDR(chain)) {
	if (TAG(chain) == symbol) {
	    return chain;
	}
    }
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
    if (growth_rate <= 0) 
	growth_rate =  HASHTABLEGROWTHRATE;

    if (size <= 0) {
	size = HASHMINSIZE;
    }
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

SEXP R_NewHashedEnv(SEXP enclos)
{
    SEXP s;

    PROTECT(s = NewEnvironment(R_NilValue, R_NilValue, enclos));
    SET_HASHTAB(s, R_NewHashTable(0,0)); /* 0, 0 gets the recomended minima */
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
	if (TAG(lst) == symbol)
	    lst = CDR(lst);
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
    if (TYPEOF(table) != VECSXP) {
	error("1st arg (table) not of type VECSXP,  from R_HashResize");
    }

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
  (size/# of primary slots used).  to a praticular threshhold value.
  Returns true if the table needs to be resized.

*/

static int R_HashSizeCheck(SEXP table)
{
    int resize;
    double thresh_val;

    /* Do some checking */
    if (TYPEOF(table) != VECSXP){
	error("1st arg (table) not of type VECSXP, R_HashSizeCheck");
    }
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
    if (TYPEOF(rho) != ENVSXP) {
	error("1st arg (table) not of type ENVSXP, from R_HashVector2Hash");
    }
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
   that time.  To make that easier, the idfef's should probably be
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
#ifdef EXPERIMENTAL_NAMESPACES
static SEXP R_BaseNamespaceName;
#endif

void InitGlobalEnv()
{
    R_GlobalEnv = NewEnvironment(R_NilValue, R_NilValue, R_NilValue);
#ifdef NEW_CODE
    HASHTAB(R_GlobalEnv) = R_NewHashTable(100, HASHTABLEGROWTHRATE);
#endif
#ifdef USE_GLOBAL_CACHE
    MARK_AS_GLOBAL_FRAME(R_GlobalEnv);
    R_GlobalCache = R_NewHashTable(INITIAL_CACHE_SIZE, HASHTABLEGROWTHRATE);
    R_GlobalCachePreserve = CONS(R_GlobalCache, R_NilValue);
    R_PreserveObject(R_GlobalCachePreserve);
#endif
#ifdef EXPERIMENTAL_NAMESPACES
    R_BaseNamespace = NewEnvironment(R_NilValue, R_NilValue, R_GlobalEnv);
    R_PreserveObject(R_BaseNamespace);
    SET_SYMVALUE(install(".BaseNamespaceEnv"), R_BaseNamespace);
    R_BaseNamespaceName = ScalarString(mkChar("base"));
    R_PreserveObject(R_BaseNamespaceName);
    R_NamespaceRegistry = R_NewHashedEnv(R_NilValue);
    R_PreserveObject(R_NamespaceRegistry);
    /**** need to properly initialize the base name space */
    /**** need to enter base namespace in registry */
#endif
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
  SEXP entry = R_HashGetLoc(hashIndex(sym, R_GlobalCache), sym, R_GlobalCache);
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
    for(i = 0; i < n ; i++) {
	R_FlushGlobalCache(Rf_install(CHAR(STRING_ELT(names,i))));
    }
}

static void R_AddGlobalCache(SEXP symbol, SEXP place)
{
  int oldpri = HASHPRI(R_GlobalCache);
#ifdef ENVIRONMENT_LOCKING
  R_HashSet(hashIndex(symbol, R_GlobalCache), symbol, R_GlobalCache, place,
	    FALSE);
#else
  R_HashSet(hashIndex(symbol, R_GlobalCache), symbol, R_GlobalCache, place);
#endif
  if (oldpri != HASHPRI(R_GlobalCache) && 
      HASHPRI(R_GlobalCache) > 0.85 * HASHSIZE(R_GlobalCache)) {
    R_GlobalCache = R_HashResize(R_GlobalCache);
    SETCAR(R_GlobalCachePreserve, R_GlobalCache);
  }
}

static SEXP R_GetGlobalCache(SEXP symbol)
{
  SEXP vl = R_HashGet(hashIndex(symbol, R_GlobalCache), symbol, R_GlobalCache);
  switch(TYPEOF(vl)) {
  case SYMSXP:
    if (vl == R_UnboundValue) /* avoid test?? */
      return R_UnboundValue;
    else return SYMBOL_BINDING_VALUE(vl);
  case LISTSXP:
    return BINDING_VALUE(vl);
  default:
    error("illegal cached value");
    return R_NilValue;
  }
}
#endif /* USE_GLOBAL_CACHE */
  
/*----------------------------------------------------------------------

  unbindVar

  Remove a value from an environment. This happens only in the frame
  of the specified frame.

  FIXME ? should this also unbind the symbol value slot when rho is
  R_NilValue.

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
  
void unbindVar(SEXP symbol, SEXP rho)
{
    int hashcode;
    SEXP c;
#ifdef EXPERIMENTAL_NAMESPACES
    if (rho == R_BaseNamespace)
	error("can't unbind in the base environment");
#endif
#ifdef ENVIRONMENT_LOCKING
    if (rho == R_NilValue)
	error("can't unbind in the NULL environment");
    if (FRAME_IS_LOCKED(rho))
	error("can't remove bindings from a locked environment");
#endif
#ifdef USE_GLOBAL_CACHE
    if (IS_GLOBAL_FRAME(rho))
	R_FlushGlobalCache(symbol);
#endif
    if (HASHTAB(rho) == R_NilValue) {
	int found;
	SEXP list;
	list = RemoveFromList(symbol, FRAME(rho), &found);
	if (found) {
	    R_DirtyImage = 1;
	    SET_FRAME(rho, list);
	}
    }
    else {
	c = PRINTNAME(symbol);
	if( !HASHASH(c) ) {
	    SET_HASHVALUE(c, R_Newhashpjw(CHAR(c)));
	    SET_HASHASH(c, 1);
	}
	hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	R_HashDelete(hashcode, symbol, HASHTAB(rho));
    }
}



/*----------------------------------------------------------------------

  findVarLocInFrame

  Look up the location of the value of a symbol in a
  single environment frame.  Almost like findVarInFrame, but
  does not return the value. R_NilValue if not found.

*/

static SEXP findVarLocInFrame(SEXP rho, SEXP symbol, Rboolean *canCache)
{
    int hashcode;
    SEXP frame, c;

#ifdef EXPERIMENTAL_NAMESPACES
    if (rho == R_NilValue)
        error("can't get binding from NULL environment");
    if (rho == R_BaseNamespace)
        error("can't get binding from base namespace");
#endif

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

#ifdef EXPERIMENTAL_NAMESPACES
    if (rho == R_BaseNamespace)
	return SYMBOL_BINDING_VALUE(symbol);
#endif

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

  This needs to be changed so that the environment chain is searched
  and then the searchpath is traversed.

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
    for (rho = R_GlobalEnv; rho != R_NilValue; rho = ENCLOS(rho)) {
	vl = findVarLocInFrame(rho, symbol, &canCache);
	if (vl != R_NilValue) {
            if(canCache)
		R_AddGlobalCache(symbol, vl);
	    return BINDING_VALUE(vl);
	}
    }
    vl = SYMBOL_BINDING_VALUE(symbol);
    if (vl != R_UnboundValue)
	R_AddGlobalCache(symbol, symbol);
    return vl;
}
#endif

SEXP findVar(SEXP symbol, SEXP rho)
{
    SEXP vl;
#ifdef USE_GLOBAL_CACHE
    /* This first loop handles local frames, if there are any.  It
       will also handle all frames if rho is a global frame other than
       R_GlobalEnv */
    while (rho != R_GlobalEnv && rho != R_NilValue) {
	vl = findVarInFrame3(rho, symbol, TRUE /* get rather than exists */);
	if (vl != R_UnboundValue)
	    return (vl);
	rho = ENCLOS(rho);
    }
    if (rho == R_GlobalEnv)
	return findGlobalVar(symbol);
    else
	return SYMBOL_BINDING_VALUE(symbol);
#else
    while (rho != R_NilValue) {
	vl = findVarInFrame3(rho, symbol, TRUE);
	if (vl != R_UnboundValue)
	    return (vl);
	rho = ENCLOS(rho);
    }
    return SYMBOL_BINDING_VALUE(symbol);
#endif
}



/*----------------------------------------------------------------------

  findVar1

  Look up a symbol in an environment.  Ignore any values which are
  not of the specified type.

  This needs to be changed so that the environment chain is searched
  and then the searchpath is traversed.

*/

SEXP findVar1(SEXP symbol, SEXP rho, SEXPTYPE mode, int inherits)
{
    SEXP vl;
    while (rho != R_NilValue) {
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
    return SYMBOL_BINDING_VALUE(symbol);
}

/*
 *  ditto, but check *mode* not *type*
 */

SEXP findVar1mode(SEXP symbol, SEXP rho, SEXPTYPE mode, int inherits, Rboolean doGet)
{
    SEXP vl;
    int tl;
    if (mode == INTSXP) mode = REALSXP;
    if (mode == FUNSXP || mode ==  BUILTINSXP || mode == SPECIALSXP) 
	mode = CLOSXP;
    while (rho != R_NilValue) {
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
    return SYMBOL_BINDING_VALUE(symbol);
}


/* 
   ddVal:
   a function to take a name and determine if it is of the form
   ..x where x is an integer; if so x is returned otherwise 0 is returned
*/
static int ddVal(SEXP symbol)
{
    char *buf, *endp;
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
	    error("The ... list does not contain %d elements",i);
    }
    else {
	vl = findVar(symbol, rho);
	if( vl != R_UnboundValue )
	    return(vl);
	error("..%d used in an incorrect context, no ... to look in",i);
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
	    if (vl != R_UnboundValue)
		return vl;
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

  NEEDED: This needs to be modified so that an object of arbitrary
  mode is searmodify this so that a search for an arbitrary mode can
  be made.  Then findVar and findFun could become same function

*/

SEXP findFun(SEXP symbol, SEXP rho)
{
    SEXP vl;
    while (rho != R_NilValue) {
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
		error("Argument \"%s\" is missing, with no default",
		      CHAR(PRINTNAME(symbol)));
#ifdef Warn_on_non_function
	    warning("ignored non function \"%s\"",
		    CHAR(PRINTNAME(symbol)));
#endif
	}
	rho = ENCLOS(rho);
    }
    if (SYMVALUE(symbol) == R_UnboundValue)
	error("couldn't find function \"%s\"", CHAR(PRINTNAME(symbol)));
    return SYMBOL_BINDING_VALUE(symbol);
}


/*----------------------------------------------------------------------

  defineVar

  Assign a value in a specific environment frame.  This needs to be
  rethought when it comes time to add a search path.

*/

void defineVar(SEXP symbol, SEXP value, SEXP rho)
{
    int hashcode;
    SEXP frame, c;
    R_DirtyImage = 1;
#ifdef EXPERIMENTAL_NAMESPACES
    if (rho != R_BaseNamespace && rho != R_NilValue) {
#else
    if (rho != R_NilValue) {
#endif
#ifdef USE_GLOBAL_CACHE
	if (IS_GLOBAL_FRAME(rho))
	    R_FlushGlobalCache(symbol);
#endif

	if(IS_USER_DATABASE(rho)) {
	    R_ObjectTable *table;
	    table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(rho));
	    if(table->assign == NULL)
		error("can't assign variables to this database");
	    table->assign(CHAR(PRINTNAME(symbol)), value, table);      
            return;
	}

	if (HASHTAB(rho) == R_NilValue) {
	    frame = FRAME(rho);
	    while (frame != R_NilValue) {
		if (TAG(frame) == symbol) {
		    SET_BINDING_VALUE(frame, value);
		    SET_MISSING(frame, 0);	/* Over-ride */
		    return;
		}
		frame = CDR(frame);
	    }
#ifdef ENVIRONMENT_LOCKING
	    if (FRAME_IS_LOCKED(rho))
		error("can't add bindings to a locked environment");
#endif
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
#ifdef ENVIRONMENT_LOCKING
	    R_HashSet(hashcode, symbol, HASHTAB(rho), value,
		      FRAME_IS_LOCKED(rho));
#else
	    R_HashSet(hashcode, symbol, HASHTAB(rho), value);
#endif
	    if (R_HashSizeCheck(HASHTAB(rho)))
		SET_HASHTAB(rho, R_HashResize(HASHTAB(rho)));
	}
    }
    else {
#ifdef USE_GLOBAL_CACHE
	R_FlushGlobalCache(symbol);
#endif
	SET_SYMBOL_BINDING_VALUE(symbol, value);
    }
}


/*----------------------------------------------------------------------

  setVarInFrame

  Assign a new value to a symbol in a frame.  Return the symbol if
  successful and R_NilValue if not.

*/

SEXP setVarInFrame(SEXP rho, SEXP symbol, SEXP value)
{
    int hashcode;
    SEXP frame, c;

    if(IS_USER_DATABASE(rho)) {
	R_ObjectTable *table;
        table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(rho));
        if(table->assign == NULL)
	    error("can't remove variables from this database");
        return(table->assign(CHAR(PRINTNAME(symbol)), value, table));      
    }

#ifdef EXPERIMENTAL_NAMESPACES
    if (rho == R_BaseNamespace) {
#ifdef USE_GLOBAL_CACHE
	R_FlushGlobalCache(symbol);
#endif
	SET_SYMBOL_BINDING_VALUE(symbol, value);
	return symbol;
    }
    else
#endif
    if (HASHTAB(rho) == R_NilValue) {
	frame = FRAME(rho);
	while (frame != R_NilValue) {
	    if (TAG(frame) == symbol) {
		SET_BINDING_VALUE(frame, value);
		return symbol;
	    }
	    frame = CDR(frame);
	}
	return R_NilValue;
    }
    else {
	/* Do the hash table thing */
	c = PRINTNAME(symbol);
	if( !HASHASH(c) ) {
	    SET_HASHVALUE(c, R_Newhashpjw(CHAR(c)));
	    SET_HASHASH(c, 1);
	}
	hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	frame = R_HashGetLoc(hashcode, symbol, HASHTAB(rho));
	if (frame != R_NilValue) {
	  SET_BINDING_VALUE(frame, value);
	  return symbol;
	}
	else return R_NilValue;
    }
}



/*----------------------------------------------------------------------

    setVar

    Assign a new value to bound symbol.	 Note this does the "inherits"
    case.  I.e. it searches frame-by-frame for an symbol and binds the
    given value to the first symbol encountered.  If no symbol is
    found then a binding is created in the global environment.

*/

void setVar(SEXP symbol, SEXP value, SEXP rho)
{
    SEXP vl;
    while (rho != R_NilValue) {
	R_DirtyImage = 1;
	vl = setVarInFrame(rho, symbol, value);
	if (vl != R_NilValue) {
	    return;
	}
	rho = ENCLOS(rho);
    }
    defineVar(symbol, value, R_GlobalEnv);
}



/*----------------------------------------------------------------------

  gsetVar

  Assignment in the system environment.	 Here we assign directly into
  the system environment.

*/

void gsetVar(SEXP symbol, SEXP value, SEXP rho)
{
    R_DirtyImage = 1;
#ifdef USE_GLOBAL_CACHE
    R_FlushGlobalCache(symbol);
#endif
    SET_SYMBOL_BINDING_VALUE(symbol, value);
}



/*----------------------------------------------------------------------

  do_assign : .Internal(assign(x, value, envir, inherits))

*/
SEXP do_assign(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP name=R_NilValue, val, aenv;
    int ginherits = 0;
    checkArity(op, args);

    if (!isString(CAR(args)) || length(CAR(args)) == 0)
	error("invalid first argument");
    else
	name = install(CHAR(STRING_ELT(CAR(args), 0)));
    PROTECT(val = CADR(args));
    R_Visible = 0;
    aenv = CAR(CDDR(args));
    if (TYPEOF(aenv) != ENVSXP && aenv != R_NilValue)
	errorcall(call, "invalid `envir' argument");
    if (isLogical(CAR(nthcdr(args, 3))))
	ginherits = LOGICAL(CAR(nthcdr(args, 3)))[0];
    else
	errorcall(call, "invalid `inherits' argument");
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

#ifdef EXPERIMENTAL_NAMESPACES
    if (env == R_BaseNamespace)
	error("can't remove variables from base namespace");
#endif

#ifdef ENVIRONMENT_LOCKING
    if (FRAME_IS_LOCKED(env))
	error("can't remove bindings from a locked environment");
#endif	

    if(IS_USER_DATABASE(env)) {
	R_ObjectTable *table;
        table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(env));
        if(table->remove == NULL)
	    error("can't remove variables from this database");
        return(table->remove(CHAR(PRINTNAME(name)), table));      
    }

    if (IS_HASHED(env)) {
	SEXP hashtab = HASHTAB(env);
	int idx = hashcode % HASHSIZE(hashtab);
	list = RemoveFromList(name, VECTOR_ELT(hashtab, idx), &found);
	if (found) {
	    R_DirtyImage = 1;
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
	    R_DirtyImage = 1;
	    SET_FRAME(env, list);
#ifdef USE_GLOBAL_CACHE
	    if (IS_GLOBAL_FRAME(env))
		R_FlushGlobalCache(name);
#endif
	}
    }
    return found;
}

SEXP do_remove(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* .Internal(remove(list, envir, inherits)) */

    SEXP name, envarg, tsym, tenv;
    int ginherits = 0;
    int done, i, hashcode;
    checkArity(op, args);

    name = CAR(args);
    if (!isString(name))
	errorcall(call, "invalid first argument to remove.");
    args = CDR(args);

    envarg = CAR(args);
    if (envarg != R_NilValue) {
	if (TYPEOF(envarg) != ENVSXP)
	    errorcall(call, "invalid `envir' argument");
    }
    else envarg = R_GlobalContext->sysparent;
    args = CDR(args);

    if (isLogical(CAR(args)))
	ginherits = asLogical(CAR(args));
    else
	errorcall(call, "invalid `inherits' argument");

    for (i = 0; i < LENGTH(name); i++) {
	done = 0;
	tsym = install(CHAR(STRING_ELT(name, i)));
	if( !HASHASH(PRINTNAME(tsym)) )
	    hashcode = R_Newhashpjw(CHAR(PRINTNAME(tsym)));
	else
	    hashcode = HASHVALUE(PRINTNAME(tsym));
	tenv = envarg;
	while (tenv != R_NilValue) {
	    done = RemoveVariable(tsym, hashcode, tenv);
	    if (done || !ginherits)
		break;
	    tenv = CDR(tenv);
	}
	if (!done)
	    warning("remove: variable \"%s\" was not found",
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


SEXP do_get(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval, genv, t1;
    SEXPTYPE gmode;
    int ginherits = 0, where;
    checkArity(op, args);

    /* The first arg is the object name */
    /* It must be present and a string */

    if (!isValidStringF(CAR(args))) {
	errorcall(call, "invalid first argument");
	t1 = R_NilValue;
    }
    else
	t1 = install(CHAR(STRING_ELT(CAR(args), 0)));

    /* envir :	originally, the "where=" argument */

    if (TYPEOF(CADR(args)) == REALSXP || TYPEOF(CADR(args)) == INTSXP) {
	where = asInteger(CADR(args));
	genv = R_sysframe(where,R_GlobalContext);
    }
    else if (TYPEOF(CADR(args)) == ENVSXP || CADR(args) == R_NilValue)
	genv = CADR(args);
    else {
	errorcall(call,"invalid envir argument");
	genv = R_NilValue;  /* -Wall */
    }

    /* mode :  The mode of the object being sought */

    /* as from R 1.2.0, this is the *mode*, not the *typeof* aka
       storage.mode.
    */

    if (isString(CAR(CDDR(args)))) {
	if (!strcmp(CHAR(STRING_ELT(CAR(CDDR(args)), 0)),"function"))
	    gmode = FUNSXP;
	else
	    gmode = str2type(CHAR(STRING_ELT(CAR(CDDR(args)), 0)));
    } else {
	errorcall(call,"invalid mode argument");
	gmode = FUNSXP;/* -Wall */
    }

    if (isLogical(CAR(nthcdr(args, 3))))
	ginherits = LOGICAL(CAR(nthcdr(args, 3)))[0];
    else
	errorcall(call,"invalid inherits argument");

    /* Search for the object */
    rval = findVar1mode(t1, genv, gmode, ginherits, PRIMVAL(op));

    if (PRIMVAL(op)) { /* have get(.) */
	if (rval == R_UnboundValue)
	    errorcall(call,"variable \"%s\" was not found",
		      CHAR(PRINTNAME(t1)));
	/* We need to evaluate if it is a promise */
	if (TYPEOF(rval) == PROMSXP)
	    rval = eval(rval, genv);
	SET_NAMED(rval, 1);
	return rval;
    }
    else { /* exists(.) */
	if (rval == R_UnboundValue)
	    ginherits = 0;
	else
	    ginherits = 1;
	rval = allocVector(LGLSXP, 1);
	LOGICAL(rval)[0] = ginherits;
	return rval;
    }
}


/*----------------------------------------------------------------------

  do_missing

  This function tests whether the symbol passed as its first argument
  is ia "missing argument to the current closure.  rho is the
  environment that missing was called from.

*/

static int isMissing(SEXP symbol, SEXP rho)
{
    int ddv=0;
    SEXP vl, s;

    if (symbol == R_MissingArg) /* Yes, this can happen */
        return 1;

    if (DDVAL(symbol)) {
	s = R_DotsSymbol;
	ddv = ddVal(symbol);
    }
    else
	s = symbol;

#ifdef EXPERIMENTAL_NAMESPACES
    if (rho == R_NilValue || rho == R_BaseNamespace)
	return 0;  /* is this really the right thing to do? LT */
#else
    if (rho == R_NilValue)
	return 0;  /* is this really the right thing to do? LT */
#endif

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

SEXP do_missing(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int ddv=0;
    SEXP rval, t, sym, s;

    checkArity(op, args);
    s = sym = CAR(args);
    if( isString(sym) && length(sym)==1 )
        s = sym = install(CHAR(STRING_ELT(CAR(args), 0)));
    if (!isSymbol(sym))
	error("\"missing\" illegal use of missing");

    if (DDVAL(sym)) {
        ddv = ddVal(sym);
	sym = R_DotsSymbol;
    }
    rval=allocVector(LGLSXP,1);

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
	error("\"missing\" illegal use of missing");

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


SEXP do_globalenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_GlobalEnv;
}


/*----------------------------------------------------------------------

  do_attach

  To attach a list we make up an environment and insert components
  of the list in as the values of this env and install the tags from
  the list as the names.

*/

SEXP do_attach(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP name, s, t, x;
    int pos, hsize;
    Rboolean isSpecial;
    
    checkArity(op, args);

    pos = asInteger(CADR(args));
    if (pos == NA_INTEGER)
	error("attach: pos must be an integer");

    name = CADDR(args);
    if (!isValidStringF(name))
	error("attach: invalid object name");

    isSpecial = IS_USER_DATABASE(CAR(args));

    if(!isSpecial) {
      if (!isNewList(CAR(args)))
   	   error("attach only works for lists and data frames");
      SETCAR(args, VectorToPairList(CAR(args)));
   
   
      for (x = CAR(args); x != R_NilValue; x = CDR(x))
   	   if (TAG(x) == R_NilValue)
   	       error("attach: all elements must be named");
      PROTECT(s = allocSExp(ENVSXP));
      setAttrib(s, install("name"), name);
   
      SET_FRAME(s, duplicate(CAR(args)));
   
      /* Connect FRAME(s) into HASHTAB(s) */
      if (length(s) < HASHMINSIZE)
   	   hsize = HASHMINSIZE;
      else
   	   hsize = length(s);
   
      SET_HASHTAB(s, R_NewHashTable(hsize, HASHTABLEGROWTHRATE));
      s = R_HashFrame(s);
   
      /* FIXME: A little inefficient */
      while (R_HashSizeCheck(HASHTAB(s))) {
   	   SET_HASHTAB(s, R_HashResize(HASHTAB(s)));
      }
    } else {
        /* Having this here (rather than below) means that the onAttach routine
           is called before the table is attached. This may not be necessary or
           desirable. */
       	R_ObjectTable *tb = (R_ObjectTable*) R_ExternalPtrAddr(CAR(args));      
        if(tb->onAttach)
	    tb->onAttach(tb);
        s = allocSExp(ENVSXP);
        SET_HASHTAB(s, CAR(args));
    }

    for (t = R_GlobalEnv; ENCLOS(t) != R_NilValue && pos > 2; t = ENCLOS(t))
	pos--;

    if (ENCLOS(t) == R_NilValue) {
	SET_ENCLOS(t, s);
	SET_ENCLOS(s, R_NilValue);
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
	setAttrib(s, R_ClassSymbol, getAttrib(HASHTAB(s), R_ClassSymbol));
	setAttrib(s, install("name"), name);
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

SEXP do_detach(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, t, x;
    int pos, n;
    Rboolean isSpecial = FALSE;

    checkArity(op, args);
    pos = asInteger(CAR(args));

    for (n = 2, t = ENCLOS(R_GlobalEnv); t != R_NilValue ; t = ENCLOS(t))
	n++;

    if (pos == n) /* n is the length of the search list */
	errorcall(call, "detaching \"package:base\" is not allowed");

    for (t = R_GlobalEnv ; ENCLOS(t) != R_NilValue && pos > 2 ; t = ENCLOS(t))
	pos--;
    if (pos != 2) {
	error("detach: invalid pos= given");
	s = t;	/* for -Wall */
    }
    else {
	PROTECT(s = ENCLOS(t));
	x = ENCLOS(s);
	SET_ENCLOS(t, x);
        isSpecial = IS_USER_DATABASE(s);
	if(isSpecial) {
	    R_ObjectTable *tb = (R_ObjectTable*) R_ExternalPtrAddr(HASHTAB(s));      
	    if(tb->onDetach)
		tb->onDetach(tb);
	}

	SET_ENCLOS(s, R_NilValue);
    }
#ifdef USE_GLOBAL_CACHE
    if(!isSpecial) {
	R_FlushGlobalCacheFromTable(HASHTAB(s));
	MARK_AS_LOCAL_FRAME(s);
    } else {
        R_FlushGlobalCacheFromUserTable(HASHTAB(s));
	MARK_AS_GLOBAL_FRAME(s);
    }
#endif
    R_Visible = 0;
    UNPROTECT(1);
    return FRAME(s);
}



/*----------------------------------------------------------------------

  do_search

  Print out the current search path.

*/

SEXP do_search(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, name, t;
    int i, n;

    checkArity(op, args);
    n = 2;
    for (t = ENCLOS(R_GlobalEnv); t != R_NilValue ; t = ENCLOS(t))
	n++;
    PROTECT(ans = allocVector(STRSXP, n));
    /* TODO - what should the name of this be? */
    SET_STRING_ELT(ans, 0, mkChar(".GlobalEnv"));
    SET_STRING_ELT(ans, n-1, mkChar("package:base"));
    i = 1;
    for (t = ENCLOS(R_GlobalEnv); t != R_NilValue ; t = ENCLOS(t)) {
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
		if (SYMVALUE(CAR(s)) != R_UnboundValue)
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
		if (SYMVALUE(CAR(s)) != R_UnboundValue)
		    SET_STRING_ELT(names, (*indx)++, PRINTNAME(CAR(s)));
	    }
	}
    }
}

SEXP do_ls(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, env, envp;
    int all, i, k, n;
    checkArity(op, args);

    if(IS_USER_DATABASE(CAR(args))) {
	R_ObjectTable *tb = (R_ObjectTable*) R_ExternalPtrAddr(HASHTAB(CAR(args)));      
        return(tb->objects(tb));
    }

    envp = CAR(args);

#ifdef EXPERIMENTAL_NAMESPACES
    if (envp == R_BaseNamespace)
	envp = R_NilValue;
#endif

    if (isNull(envp) || !isNewList(envp)) {
	PROTECT(env = allocVector(VECSXP, 1));
	SET_VECTOR_ELT(env, 0, envp);
    }
    else
	PROTECT(env = envp);

    all = asLogical(CADR(args));
    if (all == NA_LOGICAL)
	all = 0;
    /* Step 1 : Compute the Vector Size */
    k = 0;
    n = length(env);
    for (i = 0; i < n; i++) {
	if (VECTOR_ELT(env, i) == R_NilValue)
	    k += BuiltinSize(all, 0);
	else if (isEnvironment(VECTOR_ELT(env, i))) {
	    if (HASHTAB(VECTOR_ELT(env, i)) != R_NilValue)
		k += HashTableSize(HASHTAB(VECTOR_ELT(env, i)), all);
	    else
		k += FrameSize(FRAME(VECTOR_ELT(env, i)), all);
	}
	else error("invalid envir= argument");
    }
    /* Step 2 : Allocate and Fill the Result */
    ans = allocVector(STRSXP, k);
    k = 0;
    for (i = 0; i < n; i++) {
	if (VECTOR_ELT(env, i) == R_NilValue)
	    BuiltinNames(all, 0, ans, &k);
	else if (isEnvironment(VECTOR_ELT(env, i))) {
	    if (HASHTAB(VECTOR_ELT(env, i)) != R_NilValue)
		HashTableNames(HASHTAB(VECTOR_ELT(env, i)), all, ans, &k);
	    else
		FrameNames(FRAME(VECTOR_ELT(env, i)), all, ans, &k);
	}
    }
    UNPROTECT(1);
    sortVector(ans, FALSE);
    return ans;
}

/*----------------------------------------------------------------------

  do_builtins

  Return the names of all the built in functions.  These are fetched
  directly from the symbol table.

*/

SEXP do_builtins(SEXP call, SEXP op, SEXP args, SEXP rho)
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
  contain promises (created by delay, for example) are not forced.
  Values that are closures with environments equal to the loading
  environment are reparented to .GlobalEnv.  Finally, all bindings are
  removed from the loading environment.

  This routine can die if we automatically create a name space when
  loading a package.
*/

SEXP do_libfixup(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP libenv, loadenv, p;
    checkArity(op, args);
    loadenv = CAR(args);
    libenv = CADR(args);
    if (TYPEOF(libenv) != ENVSXP || !isEnvironment(loadenv))
	errorcall(call, "invalid arguments");
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
	    errorcall(call, "no enclosing environment");

	env = cptr->sysparent;
	if (R_GlobalEnv != R_NilValue && env == R_NilValue)
	    errorcall(call, R_MSG_IA);
    }
    else {
	for (env = R_GlobalEnv; env != R_NilValue && pos > 1;
	     env = ENCLOS(env))
	    pos--;
	if (pos != 1)
	    error(R_MSG_IA);
    }
    return env;
}

SEXP do_pos2env(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, pos;
    int i, npos;
    PROTECT(pos = coerceVector(CAR(args), INTSXP));
    npos = length(pos);
    if (npos <= 0)
	errorcall(call, "invalid \"pos\" argument");
    PROTECT(env = allocVector(VECSXP, npos));
    for (i = 0; i < npos; i++) {
	SET_VECTOR_ELT(env, i, pos2env(INTEGER(pos)[i], call));
    }
    if (npos == 1) env = VECTOR_ELT(env, 0);
    UNPROTECT(2);
    return env;
}

static SEXP matchEnvir(SEXP call, char *what)
{
  SEXP t, name, nameSymbol;
  if(!strcmp(".GlobalEnv", what))
    return R_GlobalEnv;
  if(!strcmp("package:base", what))
    return R_NilValue;
  nameSymbol = install("name");
  for (t = ENCLOS(R_GlobalEnv); t != R_NilValue ; t = ENCLOS(t)) {
    name = getAttrib(t, nameSymbol);
    if(isString(name) && length(name) > 0 &&
       !strcmp(CHAR(STRING_ELT(name, 0)), what))
      return t;
  }
  errorcall(call, "Package named \"%s\" not found in search list",
	    what);
  return R_NilValue;
}

SEXP do_as_environment(SEXP call, SEXP op, SEXP args, SEXP rho)
{
  SEXP arg = CAR(args);
  checkArity(op, args);
  if(isEnvironment(arg))
    return arg;
  switch(TYPEOF(arg)) {
  case STRSXP:
    return matchEnvir(call, CHAR(asChar(arg)));
  case REALSXP: case INTSXP:
    return do_pos2env(call, op, args, rho);
  default:
    errorcall(call, "Invalid object for as.environment");
    return R_NilValue; /* -Wall */
  }
}

#ifdef ENVIRONMENT_LOCKING
void R_LockEnvironment(SEXP env, Rboolean bindings)
{
    if (env == R_NilValue)
	error("locking the NULL (base) environment is not supported yet");
    if (TYPEOF(env) != ENVSXP)
	error("not an environment");
    if (bindings) {
#ifdef FANCY_BINDINGS
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
#else
	error("locking bindings is not supported");
#endif
    }
    LOCK_FRAME(env);
}

Rboolean R_EnvironmentIsLocked(SEXP env)
{
    if (env != R_NilValue && TYPEOF(env) != ENVSXP)
	error("not an environment");
    if (env == R_NilValue)
	return FALSE;
    else
	return FRAME_IS_LOCKED(env);
}

SEXP do_lockEnv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP frame;
    Rboolean bindings;
    checkArity(op, args);
    frame = CAR(args);
    bindings = asLogical(CADR(args));
    R_LockEnvironment(frame, bindings);
    return R_NilValue;
}

SEXP do_envIsLocked(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return ScalarLogical(R_EnvironmentIsLocked(CAR(args)));
}
#endif
#ifdef FANCY_BINDINGS
void R_LockBinding(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error("not a symbol");
    if (env != R_NilValue && TYPEOF(env) != ENVSXP)
	error("not an environment");
#ifdef EXPERIMENTAL_NAMESPACES
    if (env == R_NilValue || env == R_BaseNamespace)
#else
    if (env == R_NilValue)
#endif
	LOCK_BINDING(sym);
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue)
	    error("no binding for \"%s\"", CHAR(PRINTNAME(sym)));
	warning("saved workspaces with locked bindings may not work"
		" properly when loaded into older versions of R");
	LOCK_BINDING(binding);
    }
}

void R_MakeActiveBinding(SEXP sym, SEXP fun, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error("not a symbol");
    if (! isFunction(fun))
	error("not a function");
    if (env != R_NilValue && TYPEOF(env) != ENVSXP)
	error("not an environment");
#ifdef EXPERIMENTAL_NAMESPACES
    if (env == R_NilValue || env == R_BaseNamespace) {
#else
    if (env == R_NilValue) {
#endif
	if (SYMVALUE(sym) != R_UnboundValue && ! IS_ACTIVE_BINDING(sym))
	    error("symbol already has a regular binding");
	else if (BINDING_IS_LOCKED(sym))
	    error("can't change active binding if binding is locked");
	SET_SYMVALUE(sym, fun);
	SET_ACTIVE_BINDING_BIT(sym);
    }
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue) {
	    warning("saved workspaces with active bindings may not work"
		    " properly when loaded into older versions of R");
	    defineVar(sym, fun, env); /* fails if env is locked */
	    binding = findVarLocInFrame(env, sym, NULL);
	    SET_ACTIVE_BINDING_BIT(binding);
	}
	else if (! IS_ACTIVE_BINDING(binding))
	    error("symbol already has a regular binding");
	else if (BINDING_IS_LOCKED(binding))
	    error("can't change active binding if binding is locked");
	else
	    SETCAR(binding, fun);
    }
}    

Rboolean R_BindingIsLocked(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error("not a symbol");
    if (env != R_NilValue && TYPEOF(env) != ENVSXP)
	error("not an environment");
#ifdef EXPERIMENTAL_NAMESPACES
    if (env == R_NilValue || env == R_BaseNamespace)
#else
    if (env == R_NilValue)
#endif
	return BINDING_IS_LOCKED(sym);
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue)
	    error("no binding for \"%s\"", CHAR(PRINTNAME(sym)));
	return BINDING_IS_LOCKED(binding);
    }
}

Rboolean R_BindingIsActive(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error("not a symbol");
    if (env != R_NilValue && TYPEOF(env) != ENVSXP)
	error("not an environment");
#ifdef EXPERIMENTAL_NAMESPACES
    if (env == R_NilValue || env == R_BaseNamespace)
#else
    if (env == R_NilValue)
#endif
	return IS_ACTIVE_BINDING(sym);
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue)
	    error("no binding for \"%s\"", CHAR(PRINTNAME(sym)));
	return IS_ACTIVE_BINDING(binding);
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

SEXP do_lockBnd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, env;
    checkArity(op, args);
    sym = CAR(args);
    env = CADR(args);
    R_LockBinding(sym, env);
    return R_NilValue;
}

SEXP do_bndIsLocked(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, env;
    checkArity(op, args);
    sym = CAR(args);
    env = CADR(args);
    return ScalarLogical(R_BindingIsLocked(sym, env));
}

SEXP do_mkActiveBnd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, fun, env;
    checkArity(op, args);
    sym = CAR(args);
    fun = CADR(args);
    env = CADDR(args);
    R_MakeActiveBinding(sym, fun, env);
    return R_NilValue;
}

SEXP do_bndIsActive(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, env;
    checkArity(op, args);
    sym = CAR(args);
    env = CADR(args);
    return ScalarLogical(R_BindingIsActive(sym, env));
}

SEXP do_mkUnbound(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym;
    checkArity(op, args);
    sym = CAR(args);
    if (TYPEOF(sym) != SYMSXP) error("not a symbol");
    if (R_BindingIsLocked(sym, R_NilValue))
        error("can't unbind a locked binding");
    if (R_BindingIsActive(sym, R_NilValue))
        error("can't unbind and active binding");
    SET_SYMVALUE(sym, R_UnboundValue);
    return R_NilValue;
}
#endif

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
	   ! strncmp(packprefix, CHAR(STRING_ELT(name, 0)), pplen))
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
	   ! strncmp(packprefix, CHAR(STRING_ELT(name, 0)), pplen))
	    return name;
	else
	    return R_NilValue;
    }
    else
	return R_NilValue;
}

SEXP R_FindPackageEnv(SEXP info)
{
    SEXP fun, expr, val;
    PROTECT(info);
    fun = install("findPackageEnv");
    if (findVar(fun, R_GlobalEnv) == R_UnboundValue) { /* not a perfect test */
	warning("using .GlobalEnv instead of %s", CHAR(STRING_ELT(info, 0)));
	UNPROTECT(1);
	return R_GlobalEnv;
    }
    else {
	PROTECT(expr = LCONS(fun, LCONS(info, R_NilValue)));
	val = eval(expr, R_GlobalEnv);
	UNPROTECT(2);
	return val;
    }
}

#ifdef EXPERIMENTAL_NAMESPACES
Rboolean R_IsNamespaceEnv(SEXP rho)
{
    if (rho == R_BaseNamespace)
	return TRUE;
    else if (TYPEOF(rho) == ENVSXP) {
	SEXP name = findVarInFrame3(rho, install(".__NAMESPACE__."), TRUE);
	if (name != R_UnboundValue &&
	    TYPEOF(name) == STRSXP && LENGTH(name) > 0)
	    return TRUE;
	else
	    return FALSE;
    }
    else return FALSE;
}
  
SEXP R_NamespaceEnvName(SEXP rho)
{
    if (rho == R_BaseNamespace)
	return R_BaseNamespaceName;
    else if (TYPEOF(rho) == ENVSXP) {
	SEXP name = findVarInFrame3(rho, install(".__NAMESPACE__."), TRUE);
	if (name != R_UnboundValue &&
	    TYPEOF(name) == STRSXP && LENGTH(name) > 0)
	    return name;
	else
	    return R_NilValue;
    }
    else return R_NilValue;
}

SEXP R_FindNamespace(SEXP info)
{
    SEXP fun, expr, val;
    PROTECT(info);
    fun = install("getNamespace");
    if (findVar(fun, R_GlobalEnv) == R_UnboundValue) { /* not a perfect test */
	warning("namespaces not abailable; using .GlobalEnv");
	UNPROTECT(1);
	return R_GlobalEnv;
    }
    else {
	PROTECT(expr = LCONS(fun, LCONS(info, R_NilValue)));
	val = eval(expr, R_GlobalEnv);
	UNPROTECT(2);
	return val;
    }
}

static SEXP checkNSname(SEXP call, SEXP name)
{
    switch (TYPEOF(name)) {
    case STRSXP:
	if (LENGTH(name) == 1)
	    name = install(CHAR(STRING_ELT(name, 0)));
	/* fall through */
    case SYMSXP: break;
    default: errorcall(call, "bad name space name");
    }
    return name;
}

SEXP do_regNS(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP name, val;
    checkArity(op, args);
    name = checkNSname(call, CAR(args));
    val = CADR(args);
    if (findVarInFrame(R_NamespaceRegistry, name) != R_UnboundValue)
	errorcall(call, "name space already registered");
    defineVar(name, val, R_NamespaceRegistry);
    return R_NilValue;
}

SEXP do_unregNS(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP name;
    int hashcode;
    checkArity(op, args);
    name = checkNSname(call, CAR(args));
    if (findVarInFrame(R_NamespaceRegistry, name) == R_UnboundValue)
	errorcall(call, "name space not registered");
    if( !HASHASH(PRINTNAME(name)))
	hashcode = R_Newhashpjw(CHAR(PRINTNAME(name)));
    else
	hashcode = HASHVALUE(PRINTNAME(name));
    RemoveVariable(name, hashcode, R_NamespaceRegistry);
    return R_NilValue;
}

SEXP do_getRegNS(SEXP call, SEXP op, SEXP args, SEXP rho)
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

SEXP do_getNSRegistry(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_NamespaceRegistry;
}
#endif
