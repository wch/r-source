/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2016  The R Core Team
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

/* This is currently restricted to vectors of length < 2^30 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>

#define NIL -1
#define ARGUSED(x) LEVELS(x)
#define SET_ARGUSED(x,v) SETLEVELS(x,v)

/* interval at which to check interrupts */
#define NINTERRUPT 1000000

typedef size_t hlen;

/* Hash function and equality test for keys */
typedef struct _HashData HashData;

struct _HashData {
    int K;
    hlen M;
    R_xlen_t nmax;
#ifdef LONG_VECTOR_SUPPORT
    Rboolean isLong;
#endif
    hlen (*hash)(SEXP, R_xlen_t, HashData *);
    int (*equal)(SEXP, R_xlen_t, SEXP, R_xlen_t);
    SEXP HashTable;

    int nomatch;
    Rboolean useUTF8;
    Rboolean useCache;
};


/*
   Integer keys are hashed via a random number generator
   based on Knuth's recommendations.  The high order K bits
   are used as the hash code.

   NB: lots of this code relies on M being a power of two and
   on silent integer overflow mod 2^32.

   <FIXME> Integer keys are wasteful for logical and raw vectors, but
   the tables are small in that case.  It would be much easier to
   implement long vectors, though.
*/

/*  Currently the hash table is implemented as a (signed) integer
    array.  So there are two 31-bit restrictions, the length of the
    array and the values.  The values are initially NIL (-1).  O-based
    indices are inserted by isDuplicated, and invalidated by setting
    to NA_INTEGER.
*/

static hlen scatter(unsigned int key, HashData *d)
{
    return 3141592653U * key >> (32 - d->K);
}

static hlen lhash(SEXP x, R_xlen_t indx, HashData *d)
{
    if (LOGICAL(x)[indx] == NA_LOGICAL) return 2U;
    return (hlen) LOGICAL(x)[indx];
}

static hlen ihash(SEXP x, R_xlen_t indx, HashData *d)
{
    if (INTEGER(x)[indx] == NA_INTEGER) return 0;
    return scatter((unsigned int) (INTEGER(x)[indx]), d);
}

/* We use unions here because Solaris gcc -O2 has trouble with
   casting + incrementing pointers.  We use tests here, but R currently
   assumes int is 4 bytes and double is 8 bytes.
 */
union foo {
    double d;
    unsigned int u[2];
};

static hlen rhash(SEXP x, R_xlen_t indx, HashData *d)
{
    /* There is a problem with signed 0s under IEC60559 */
    double tmp = (REAL(x)[indx] == 0.0) ? 0.0 : REAL(x)[indx];
    /* need to use both 32-byte chunks or endianness is an issue */
    /* we want all NaNs except NA equal, and all NAs equal */
    if (R_IsNA(tmp)) tmp = NA_REAL;
    else if (R_IsNaN(tmp)) tmp = R_NaN;
#if 2*SIZEOF_INT == SIZEOF_DOUBLE
    {
	union foo tmpu;
	tmpu.d = tmp;
	return scatter(tmpu.u[0] + tmpu.u[1], d);
    }
#else
    return scatter(*((unsigned int *) (&tmp)), d);
#endif
}

static Rcomplex unify_complex_na(Rcomplex z) {
    Rcomplex ans;
    ans.r = (z.r == 0.0) ? 0.0 : z.r;
    ans.i = (z.i == 0.0) ? 0.0 : z.i;
    /* we want all NaNs except NA equal, and all NAs equal */
    if (R_IsNA(ans.r)) ans.r = NA_REAL;
    else if (R_IsNaN(ans.r)) ans.r = R_NaN;
    if (R_IsNA(ans.i)) ans.i = NA_REAL;
    else if (R_IsNaN(ans.i)) ans.i = R_NaN;
    return ans;
}

static hlen chash(SEXP x, R_xlen_t indx, HashData *d)
{
    Rcomplex tmp = unify_complex_na(COMPLEX(x)[indx]);

#if 2*SIZEOF_INT == SIZEOF_DOUBLE
    {
	unsigned int u;
	union foo tmpu;
	tmpu.d = tmp.r;
	u = tmpu.u[0] ^ tmpu.u[1];
	tmpu.d = tmp.i;
	u ^= tmpu.u[0] ^ tmpu.u[1];
	return scatter(u, d);
    }
#else
	return scatter((*((unsigned int *)(&tmp.r)) ^
			(*((unsigned int *)(&tmp.i)))), d);
#endif
}

/* Hash CHARSXP by address.  Hash values are int, For 64bit pointers,
 * we do (upper ^ lower) */
static hlen cshash(SEXP x, R_xlen_t indx, HashData *d)
{
    intptr_t z = (intptr_t) STRING_ELT(x, indx);
    unsigned int z1 = (unsigned int)(z & 0xffffffff), z2 = 0;
#if SIZEOF_LONG == 8
    z2 = (unsigned int)(z/0x100000000L);
#endif
    return scatter(z1 ^ z2, d);
}

static hlen shash(SEXP x, R_xlen_t indx, HashData *d)
{
    unsigned int k;
    const char *p;
    const void *vmax = vmaxget();
    if(!d->useUTF8 && d->useCache) return cshash(x, indx, d);
    /* Not having d->useCache really should not happen anymore. */
    p = translateCharUTF8(STRING_ELT(x, indx));
    k = 0;
    while (*p++)
	k = 11 * k + (unsigned int) *p; /* was 8 but 11 isn't a power of 2 */
    vmaxset(vmax); /* discard any memory used by translateChar */
    return scatter(k, d);
}

static int lequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return 0;
    return (LOGICAL(x)[i] == LOGICAL(y)[j]);
}


static int iequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return 0;
    return (INTEGER(x)[i] == INTEGER(y)[j]);
}

/* BDR 2002-1-17  We don't want NA and other NaNs to be equal */
static int requal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return 0;
    if (!ISNAN(REAL(x)[i]) && !ISNAN(REAL(y)[j]))
	return (REAL(x)[i] == REAL(y)[j]);
    else if (R_IsNA(REAL(x)[i]) && R_IsNA(REAL(y)[j])) return 1;
    else if (R_IsNaN(REAL(x)[i]) && R_IsNaN(REAL(y)[j])) return 1;
    else return 0;
}

/* This is differentiating {NA,1}, {NA,0}, {NA, NaN}, {NA, NA},
 * but R's print() and format()  render all as "NA" */
static int cplx_eq(Rcomplex x, Rcomplex y)
{
    if (!ISNAN(x.r) && !ISNAN(x.i) &&
	!ISNAN(y.r) && !ISNAN(y.i))
	return x.r == y.r && x.i == y.i;
    else if ((R_IsNA(x.r) || R_IsNA(x.i)) &&
	     (R_IsNA(y.r) || R_IsNA(y.i)))
	return 1;
    else if ((R_IsNaN(x.r) || R_IsNaN(x.i)) &&
	     (R_IsNaN(y.r) || R_IsNaN(y.i)))
	return 1;
    else
	return 0;
}

static int cequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return 0;
    return cplx_eq(COMPLEX(x)[i], COMPLEX(y)[j]);
}

static int sequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return 0;
    /* Two strings which have the same address must be the same,
       so avoid looking at the contents */
    if (STRING_ELT(x, i) == STRING_ELT(y, j)) return 1;
    /* Then if either is NA the other cannot be */
    /* Once all CHARSXPs are cached, Seql will handle this */
    if (STRING_ELT(x, i) == NA_STRING || STRING_ELT(y, j) == NA_STRING)
	return 0;
    return Seql(STRING_ELT(x, i), STRING_ELT(y, j));
}

static hlen rawhash(SEXP x, R_xlen_t indx, HashData *d)
{
    return (hlen) RAW(x)[indx];
}

static int rawequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return 0;
    return (RAW(x)[i] == RAW(y)[j]);
}

static hlen vhash(SEXP x, R_xlen_t indx, HashData *d)
{
    int i;
    unsigned int key;
    SEXP _this = VECTOR_ELT(x, indx);

    key = OBJECT(_this) + 2*TYPEOF(_this) + 100U*(unsigned int) length(_this);
    /* maybe we should also look at attributes, but that slows us down */
    switch (TYPEOF(_this)) {
    case LGLSXP:
	/* This is not too clever: pack into 32-bits and then scatter? */
	for(i = 0; i < LENGTH(_this); i++) {
	    key ^= lhash(_this, i, d);
	    key *= 97;
	}
	break;
    case INTSXP:
	for(i = 0; i < LENGTH(_this); i++) {
	    key ^= ihash(_this, i, d);
	    key *= 97;
	}
	break;
    case REALSXP:
	for(i = 0; i < LENGTH(_this); i++) {
	    key ^= rhash(_this, i, d);
	    key *= 97;
	}
	break;
    case CPLXSXP:
	for(i = 0; i < LENGTH(_this); i++) {
	    key ^= chash(_this, i, d);
	    key *= 97;
	}
	break;
    case STRSXP:
	for(i = 0; i < LENGTH(_this); i++) {
	    key ^= shash(_this, i, d);
	    key *= 97;
	}
	break;
    case RAWSXP:
	for(i = 0; i < LENGTH(_this); i++) {
	    key ^= scatter((unsigned int)rawhash(_this, i, d), d);
	    key *= 97;
	}
	break;
    case VECSXP:
	for(i = 0; i < LENGTH(_this); i++) {
	    key ^= vhash(_this, i, d);
	    key *= 97;
	}
	break;
    default:
	break;
    }
    return scatter(key, d);
}

static int vequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return 0;
    return R_compute_identical(VECTOR_ELT(x, i), VECTOR_ELT(y, j), 0);
}

/*
  Choose M to be the smallest power of 2
  not less than 2*n and set K = log2(M).
  Need K >= 1 and hence M >= 2, and 2^M < 2^31-1, hence n <= 2^29.

  Dec 2004: modified from 4*n to 2*n, since in the worst case we have
  a 50% full table, and that is still rather efficient -- see
  R. Sedgewick (1998) Algorithms in C++ 3rd edition p.606.
*/
static void MKsetup(R_xlen_t n, HashData *d, R_xlen_t nmax)
{
#ifdef LONG_VECTOR_SUPPORT
    /* M = 2^32 is safe, hence n <= 2^31 -1 */
    if(n < 0) /* protect against overflow to -ve */
	error(_("length %d is too large for hashing"), n);
#else
    if(n < 0 || n >= 1073741824) /* protect against overflow to -ve */
	error(_("length %d is too large for hashing"), n);
#endif

    if (nmax != NA_INTEGER && nmax != 1) n = nmax;
    size_t n2 = 2U * (size_t) n;
    d->M = 2;
    d->K = 1;
    while (d->M < n2) {
	d->M *= 2;
	d->K++;
    }
    d->nmax = n;
}

#define IMAX 4294967296L
static void HashTableSetup(SEXP x, HashData *d, R_xlen_t nmax)
{
    d->useUTF8 = FALSE;
    d->useCache = TRUE;
    switch (TYPEOF(x)) {
    case LGLSXP:
	d->hash = lhash;
	d->equal = lequal;
	d->nmax = d->M = 4;
	d->K = 2; /* unused */
	break;
    case INTSXP:
    {
	d->hash = ihash;
	d->equal = iequal;
#ifdef LONG_VECTOR_SUPPORT
	R_xlen_t nn = XLENGTH(x);
	if (nn > IMAX) nn = IMAX;
	MKsetup(nn, d, nmax);
#else
	MKsetup(LENGTH(x), d, nmax);
#endif
    }
	break;
    case REALSXP:
	d->hash = rhash;
	d->equal = requal;
	MKsetup(XLENGTH(x), d, nmax);
	break;
    case CPLXSXP:
	d->hash = chash;
	d->equal = cequal;
	MKsetup(XLENGTH(x), d, nmax);
	break;
    case STRSXP:
	d->hash = shash;
	d->equal = sequal;
	MKsetup(XLENGTH(x), d, nmax);
	break;
    case RAWSXP:
	d->hash = rawhash;
	d->equal = rawequal;
	d->nmax = d->M = 256;
	d->K = 8; /* unused */
	break;
    case VECSXP:
	d->hash = vhash;
	d->equal = vequal;
	MKsetup(XLENGTH(x), d, nmax);
	break;
    default:
	UNIMPLEMENTED_TYPE("HashTableSetup", x);
    }
#ifdef LONG_VECTOR_SUPPORT
    d->isLong = IS_LONG_VEC(x);
    if (d->isLong) {
	d->HashTable = allocVector(REALSXP, (R_xlen_t) d->M);
	for (R_xlen_t i = 0; i < d->M; i++) REAL(d->HashTable)[i] = NIL;
    } else
#endif
    {
	d->HashTable = allocVector(INTSXP, (R_xlen_t) d->M);
	for (R_xlen_t i = 0; i < d->M; i++) INTEGER(d->HashTable)[i] = NIL;
    }
}

/* Open address hashing */
/* Collision resolution is by linear probing */
/* The table is guaranteed large so this is sufficient */

static int isDuplicated(SEXP x, R_xlen_t indx, HashData *d)
{
#ifdef LONG_VECTOR_SUPPORT
    if (d->isLong) {
	double *h = REAL(d->HashTable);
	hlen i = d->hash(x, indx, d);
	while (h[i] != NIL) {
	    if (d->equal(x, (R_xlen_t) h[i], x, indx))
		return h[i] >= 0 ? 1 : 0;
	    i = (i + 1) % d->M;
	}
	if (d->nmax-- < 0) error("hash table is full");
	h[i] = (double) indx;
    } else
#endif
    {
	int *h = INTEGER(d->HashTable);
	hlen i = d->hash(x, indx, d);
	while (h[i] != NIL) {
	    if (d->equal(x, h[i], x, indx))
		return h[i] >= 0 ? 1 : 0;
	    i = (i + 1) % d->M;
	}
	if (d->nmax-- < 0) error("hash table is full");
	h[i] = (int) indx;
    }
    return 0;
}

static void removeEntry(SEXP table, SEXP x, R_xlen_t indx, HashData *d)
{
#ifdef LONG_VECTOR_SUPPORT
    if (d->isLong) {
	double *h = REAL(d->HashTable);
	hlen i = d->hash(x, indx, d);
	while (h[i] >= 0) {
	    if (d->equal(table, (R_xlen_t) h[i], x, indx)) {
		h[i] = NA_INTEGER;  /* < 0, only index values are inserted */
		return;
	    }
	    i = (i + 1) % d->M;
	}
    } else
#endif
    {
	int *h = INTEGER(d->HashTable);
	hlen i = d->hash(x, indx, d);
	while (h[i] >= 0) {
	    if (d->equal(table, h[i], x, indx)) {
		h[i] = NA_INTEGER;  /* < 0, only index values are inserted */
		return;
	    }
	    i = (i + 1) % d->M;
	}
    }
}

#define DUPLICATED_INIT						\
    HashData data;						\
    HashTableSetup(x, &data, nmax);				\
    if(TYPEOF(x) == STRSXP) {					\
	data.useUTF8 = FALSE; data.useCache = TRUE;		\
	for(i = 0; i < n; i++) {				\
	    if(IS_BYTES(STRING_ELT(x, i))) {			\
		data.useUTF8 = FALSE; break;			\
	    }							\
	    if(ENC_KNOWN(STRING_ELT(x, i))) {			\
		data.useUTF8 = TRUE;				\
	    }							\
	    if(!IS_CACHED(STRING_ELT(x, i))) {			\
		data.useCache = FALSE; break;			\
	    }							\
	}							\
    }

/* used in scan() */
SEXP duplicated(SEXP x, Rboolean from_last)
{
    SEXP ans;
    int *v, nmax = NA_INTEGER;

    if (!isVector(x)) error(_("'duplicated' applies only to vectors"));
    R_xlen_t i, n = XLENGTH(x);
    DUPLICATED_INIT;

    PROTECT(data.HashTable);
    PROTECT(ans = allocVector(LGLSXP, n));

    v = LOGICAL(ans);

    if(from_last)
	for (i = n-1; i >= 0; i--) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}
    else
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}

    UNPROTECT(2);
    return ans;
}

static SEXP Duplicated(SEXP x, Rboolean from_last, int nmax)
{
    SEXP ans;
    int *v;

    if (!isVector(x)) error(_("'duplicated' applies only to vectors"));
    R_xlen_t i, n = XLENGTH(x);
    DUPLICATED_INIT;

    PROTECT(data.HashTable);
    PROTECT(ans = allocVector(LGLSXP, n));

    v = LOGICAL(ans);

    if(from_last)
	for (i = n-1; i >= 0; i--) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}
    else
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}

    UNPROTECT(2);
    return ans;
}

/* simpler version of the above : return 1-based index of first, or 0 : */
R_xlen_t any_duplicated(SEXP x, Rboolean from_last)
{
    R_xlen_t result = 0;
    int nmax = NA_INTEGER;

    if (!isVector(x)) error(_("'duplicated' applies only to vectors"));
    R_xlen_t i, n = XLENGTH(x);

    DUPLICATED_INIT;
    PROTECT(data.HashTable);

    if(from_last) {
	for (i = n-1; i >= 0; i--) {
	    if(isDuplicated(x, i, &data)) { result = ++i; break; }
	}
    } else {
	for (i = 0; i < n; i++) {
	    if(isDuplicated(x, i, &data)) { result = ++i; break; }
	}
    }
    UNPROTECT(1);
    return result;
}

static SEXP duplicated3(SEXP x, SEXP incomp, Rboolean from_last, int nmax)
{
    SEXP ans;
    int *v, j, m;

    if (!isVector(x)) error(_("'duplicated' applies only to vectors"));
    R_xlen_t i, n = XLENGTH(x);
    DUPLICATED_INIT;

    PROTECT(data.HashTable);
    PROTECT(ans = allocVector(LGLSXP, n));

    v = LOGICAL(ans);

    if(from_last)
	for (i = n-1; i >= 0; i--) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}
    else
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}

    if(length(incomp)) {
	PROTECT(incomp = coerceVector(incomp, TYPEOF(x)));
	m = length(incomp);
	for (i = 0; i < n; i++)
	    if(v[i]) {
		for(j = 0; j < m; j++)
		    if(data.equal(x, i, incomp, j)) {v[i] = 0; break;}
	    }
	UNPROTECT(1);
    }
    UNPROTECT(2);
    return ans;
}

/* return (1-based) index of first duplication, or 0 : */
R_xlen_t any_duplicated3(SEXP x, SEXP incomp, Rboolean from_last)
{
    int j, m = length(incomp), nmax = NA_INTEGER;

    if (!isVector(x)) error(_("'duplicated' applies only to vectors"));
    R_xlen_t i, n = XLENGTH(x);
    DUPLICATED_INIT;
    PROTECT(data.HashTable);

    if(!m) error(_("any_duplicated3(., <0-length incomp>)"));

    PROTECT(incomp = coerceVector(incomp, TYPEOF(x)));
    m = length(incomp);

    if(from_last)
	for (i = n-1; i >= 0; i--) {
#define IS_DUPLICATED_CHECK				\
	    if(isDuplicated(x, i, &data)) {		\
		Rboolean isDup = TRUE;			\
		for(j = 0; j < m; j++)			\
		    if(data.equal(x, i, incomp, j)) {	\
			isDup = FALSE; break;		\
		    }					\
		if(isDup) {				\
		    UNPROTECT(2);			\
		    return ++i;				\
		}					\
		/* else continue */			\
	    }
	    IS_DUPLICATED_CHECK;
	}
    else {
	for (i = 0; i < n; i++) {
	    IS_DUPLICATED_CHECK;
	}
    }

    UNPROTECT(2);
    return 0;
}

#undef IS_DUPLICATED_CHECK
#undef DUPLICATED_INIT


/* .Internal(duplicated(x))	  [op=0]
  .Internal(unique(x))		  [op=1]
   .Internal(anyDuplicated(x))	  [op=2]
*/
SEXP attribute_hidden do_duplicated(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, incomp, dup, ans;
    int fromLast, nmax = NA_INTEGER;
    R_xlen_t i, k, n;

    checkArity(op, args);
    x = CAR(args);
    incomp = CADR(args);
    if (length(CADDR(args)) < 1)
	error(_("'fromLast' must be length 1"));
    fromLast = asLogical(CADDR(args));
    if (fromLast == NA_LOGICAL)
	error(_("'fromLast' must be TRUE or FALSE"));

    Rboolean fL = (Rboolean) fromLast;

    /* handle zero length vectors, and NULL */
    if ((n = xlength(x)) == 0)
	return(PRIMVAL(op) <= 1
	       ? allocVector(PRIMVAL(op) != 1 ? LGLSXP : TYPEOF(x), 0)
	       : ScalarInteger(0));

    if (!isVector(x)) {
	error(_("%s() applies only to vectors"),
	      (PRIMVAL(op) == 0 ? "duplicated" :
	       (PRIMVAL(op) == 1 ? "unique" : /* 2 */ "anyDuplicated")));
    }
    if (PRIMVAL(op) <= 1) {
	nmax = asInteger(CADDDR(args));
	if (nmax != NA_INTEGER && nmax <= 0)
	    error(_("'nmax' must be positive"));
    }

    if(length(incomp) && /* S has FALSE to mean empty */
       !(isLogical(incomp) && length(incomp) == 1 && LOGICAL(incomp)[0] == 0)) {
	if(PRIMVAL(op) == 2) {
	    /* return R's 1-based index :*/
	    R_xlen_t ind  = any_duplicated3(x, incomp, fL);
	    if(ind > INT_MAX) return ScalarReal((double) ind);
	    else return ScalarInteger((int)ind);
	} else
	    dup = duplicated3(x, incomp, fL, nmax);
    }
    else {
	if(PRIMVAL(op) == 2) {
	    R_xlen_t ind  = any_duplicated(x, fL);
	    if(ind > INT_MAX) return ScalarReal((double) ind);
	    else return ScalarInteger((int)ind);
	} else
	    dup = Duplicated(x, fL, nmax);
    }
    if (PRIMVAL(op) == 0) /* "duplicated()" */
	return dup;
    /*	ELSE
	use the results of "duplicated" to get "unique" */

    /* count unique entries */
    k = 0;
    for (i = 0; i < n; i++)
	if (LOGICAL(dup)[i] == 0)
	    k++;

    PROTECT(dup);
    PROTECT(ans = allocVector(TYPEOF(x), k));

    k = 0;
    switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL(dup)[i] == 0)
		INTEGER(ans)[k++] = INTEGER(x)[i];
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL(dup)[i] == 0)
		REAL(ans)[k++] = REAL(x)[i];
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL(dup)[i] == 0) {
		COMPLEX(ans)[k].r = COMPLEX(x)[i].r;
		COMPLEX(ans)[k].i = COMPLEX(x)[i].i;
		k++;
	    }
	break;
    case STRSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL(dup)[i] == 0)
		SET_STRING_ELT(ans, k++, STRING_ELT(x, i));
	break;
    case VECSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL(dup)[i] == 0)
		SET_VECTOR_ELT(ans, k++, VECTOR_ELT(x, i));
	break;
    case RAWSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL(dup)[i] == 0)
		RAW(ans)[k++] = RAW(x)[i];
	break;
    default:
	UNIMPLEMENTED_TYPE("duplicated", x);
    }
    UNPROTECT(2);
    return ans;
}

/* Build a hash table, ignoring information on duplication */
static void DoHashing(SEXP table, HashData *d)
{
    R_xlen_t i, n = XLENGTH(table);
    for (i = 0; i < n; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	(void) isDuplicated(table, i, d);
    }
}

/* invalidate entries: normally few */
static void UndoHashing(SEXP x, SEXP table, HashData *d)
{
    for (R_xlen_t i = 0; i < XLENGTH(x); i++) removeEntry(table, x, i, d);
}

static int Lookup(SEXP table, SEXP x, R_xlen_t indx, HashData *d)
{
    int *h = INTEGER(d->HashTable);
    hlen i = d->hash(x, indx, d);
    while (h[i] != NIL) {
	if (d->equal(table, h[i], x, indx))
	    return h[i] >= 0 ? h[i] + 1 : d->nomatch;
	i = (i + 1) % d->M;
    }
    return d->nomatch;
}

/* Now do the table lookup */
static SEXP HashLookup(SEXP table, SEXP x, HashData *d)
{
    SEXP ans;
    R_xlen_t i, n;

    n = XLENGTH(x);
    PROTECT(ans = allocVector(INTSXP, n));
    for (i = 0; i < n; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	INTEGER(ans)[i] = Lookup(table, x, i, d);
    }
    UNPROTECT(1);
    return ans;
}

static SEXP match_transform(SEXP s, SEXP env)
{
    if(OBJECT(s)) {
	if(inherits(s, "factor")) return asCharacterFactor(s);
	else if(inherits(s, "POSIXlt")) { /* and maybe more classes in the future:
					   * Call R's (generic)	 as.character(s) : */
	    SEXP call, r;
	    PROTECT(call = lang2(install("as.character"), s));
	    r = eval(call, env);
	    UNPROTECT(1);
	    return r;
	}
    }
    /* else */
    return duplicate(s);
}

// workhorse of R's match() and hence also  " ix %in% itable "
SEXP match5(SEXP itable, SEXP ix, int nmatch, SEXP incomp, SEXP env)
{
    SEXP ans, x, table;
    SEXPTYPE type;
    HashData data;

    R_xlen_t n = xlength(ix);

    /* handle zero length arguments */
    if (n == 0) return allocVector(INTSXP, 0);
    if (length(itable) == 0) {
	ans = allocVector(INTSXP, n);
	for (R_xlen_t i = 0; i < n; i++) INTEGER(ans)[i] = nmatch;
	return ans;
    }

    int nprot = 0;
    PROTECT(x	  = match_transform(ix,	    env)); nprot++;
    PROTECT(table = match_transform(itable, env)); nprot++;
    /* or should we use PROTECT_WITH_INDEX and REPROTECT below ? */

    /* Coerce to a common type; type == NILSXP is ok here.
     * Note that above we coerce factors and "POSIXlt", only to character.
     * Hence, coerce to character or to `higher' type
     * (given that we have "Vector" or NULL) */
    if(TYPEOF(x) >= STRSXP || TYPEOF(table) >= STRSXP) type = STRSXP;
    else type = TYPEOF(x) < TYPEOF(table) ? TYPEOF(table) : TYPEOF(x);
    PROTECT(x	  = coerceVector(x,	type)); nprot++;
    PROTECT(table = coerceVector(table, type)); nprot++;

    // special case scalar x -- for speed only :
    if(LENGTH(x) == 1 && !incomp) {
      PROTECT(ans = ScalarInteger(nmatch)); nprot++;
      switch (type) {
      case STRSXP: {
	  SEXP x_val = STRING_ELT(x,0);
	  for (int i=0; i < LENGTH(itable); i++) if (Seql(STRING_ELT(table,i), x_val)) {
		  INTEGER(ans)[0] = i + 1; break;
	      }
	  break; }
      case LGLSXP:
      case INTSXP: {
	  int x_val = INTEGER(x)[0],
	      *table_p = INTEGER(table);
	  for (int i=0; i < LENGTH(itable); i++) if (table_p[i] == x_val) {
		  INTEGER(ans)[0] = i + 1; break;
	      }
	  break; }
      case REALSXP: {
	  double x_val = (REAL(x)[0] == 0.) ? 0. : REAL(x)[0],// pblm with signed 0s under IEC60559
	      *table_p = REAL(table);
	  /* we want all NaNs except NA equal, and all NAs equal */
	  if (R_IsNA(x_val)) {
	      for (int i=0; i < LENGTH(itable); i++) if (R_IsNA(table_p[i])) {
		      INTEGER(ans)[0] = i + 1; break;
		  }
	  }
	  else if (R_IsNaN(x_val)) {
	      for (int i=0; i < LENGTH(itable); i++) if (R_IsNaN(table_p[i])) {
		      INTEGER(ans)[0] = i + 1; break;
		  }
	  }
	  else {
	      for (int i=0; i < LENGTH(itable); i++) if (table_p[i] == x_val) {
		      INTEGER(ans)[0] = i + 1; break;
	      }
	  }
	  break; }
      case CPLXSXP: {
	  Rcomplex x_val = COMPLEX(x)[0],
	      *table_p = COMPLEX(table);
	  for (int i=0; i < LENGTH(itable); i++)
	      if (cplx_eq(table_p[i], x_val)) {
		  INTEGER(ans)[0] = i + 1; break;
	      }
	  break; }
      case RAWSXP: {
	  Rbyte x_val = RAW(x)[0],
	      *table_p = RAW(table);
	  for (int i=0; i < LENGTH(itable); i++) if (table_p[i] == x_val) {
		  INTEGER(ans)[0] = i + 1; break;
	      }
	  break; }
      }
    }
    else { // regular case

    if (incomp) { PROTECT(incomp = coerceVector(incomp, type)); nprot++; }
    data.nomatch = nmatch;
    HashTableSetup(table, &data, NA_INTEGER);
    if(type == STRSXP) {
	Rboolean useBytes = FALSE;
	Rboolean useUTF8 = FALSE;
	Rboolean useCache = TRUE;
	for(R_xlen_t i = 0; i < length(x); i++) {
	    SEXP s = STRING_ELT(x, i);
	    if(IS_BYTES(s)) {
		useBytes = TRUE;
		useUTF8 = FALSE;
		break;
	    }
	    if(ENC_KNOWN(s)) {
		useUTF8 = TRUE;
	    }
	    if(!IS_CACHED(s)) {
		useCache = FALSE;
		break;
	    }
	}
	if(!useBytes || useCache) {
	    for(int i = 0; i < length(table); i++) {
		SEXP s = STRING_ELT(table, i);
		if(IS_BYTES(s)) {
		    useBytes = TRUE;
		    useUTF8 = FALSE;
		    break;
		}
		if(ENC_KNOWN(s)) {
		    useUTF8 = TRUE;
		}
		if(!IS_CACHED(s)) {
		    useCache = FALSE;
		    break;
		}
	    }
	}
	data.useUTF8 = useUTF8;
	data.useCache = useCache;
    }
    PROTECT(data.HashTable); nprot++;
    DoHashing(table, &data);
    if (incomp) UndoHashing(incomp, table, &data);
    ans = HashLookup(table, x, &data);
  }
    UNPROTECT(nprot);
    return ans;
}

SEXP matchE(SEXP itable, SEXP ix, int nmatch, SEXP env)
{
    return match5(itable, ix, nmatch, NULL, env);
}

/* used from other code, not here: */
SEXP match(SEXP itable, SEXP ix, int nmatch)
{
    return match5(itable, ix, nmatch, NULL, R_BaseEnv);
}


// .Internal(match(x, table, nomatch, incomparables)) :
SEXP attribute_hidden do_match(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    if ((!isVector(CAR(args)) && !isNull(CAR(args)))
	|| (!isVector(CADR(args)) && !isNull(CADR(args))))
	error(_("'match' requires vector arguments"));

    int nomatch = asInteger(CADDR(args));
    SEXP incomp = CADDDR(args);

    if (isNull(incomp) || /* S has FALSE to mean empty */
	(length(incomp) == 1 && isLogical(incomp) && LOGICAL(incomp)[0] == 0))
	return match5(CADR(args), CAR(args), nomatch, NULL, env);
    else
	return match5(CADR(args), CAR(args), nomatch, incomp, env);
}

/* pmatch and charmatch return integer positions, so cannot be used
   for long vector tables */

/* Partial Matching of Strings */
/* Fully S Compatible version. */

/* Hmm, this was not all S compatible!	The desired behaviour is:
 * First do exact matches, and mark elements as used as they are matched
 *   unless dup_ok is true.
 * Then do partial matching, from left to right, using up the table
 *   unless dup_ok is true.  Multiple partial matches are ignored.
 * Empty strings are unmatched			      BDR 2000/2/16
 */

SEXP attribute_hidden do_pmatch(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, input, target;
    int mtch, n_target, mtch_count, dups_ok, no_match;
    size_t temp;
    int *used = NULL, *ians;
    const char **in, **tar;
    Rboolean no_dups;
    Rboolean useBytes = FALSE, useUTF8 = FALSE;

    checkArity(op, args);
    input = CAR(args);
    R_xlen_t n_input = XLENGTH(input);
    target = CADR(args);
    n_target = LENGTH(target); // not allowed to be long
    no_match = asInteger(CADDR(args));
    dups_ok = asLogical(CADDDR(args));
    if (dups_ok == NA_LOGICAL)
	error(_("invalid '%s' argument"), "duplicates.ok");
    no_dups = !dups_ok;

    if (!isString(input) || !isString(target))
	error(_("argument is not of mode character"));

    if(no_dups) {
	used = (int *) R_alloc((size_t) n_target, sizeof(int));
	for (int j = 0; j < n_target; j++) used[j] = 0;
    }

    for(R_xlen_t i = 0; i < n_input; i++) {
	if(IS_BYTES(STRING_ELT(input, i))) {
	    useBytes = TRUE;
	    useUTF8 = FALSE;
	    break;
	} else if(ENC_KNOWN(STRING_ELT(input, i))) {
	    useUTF8 = TRUE;
	}
    }
    if(!useBytes) {
	for(R_xlen_t i = 0; i < n_target; i++) {
	    if(IS_BYTES(STRING_ELT(target, i))) {
		useBytes = TRUE;
		useUTF8 = FALSE;
		break;
	    } else if(ENC_KNOWN(STRING_ELT(target, i))) {
		useUTF8 = TRUE;
	    }
	}
    }

    in = (const char **) R_alloc((size_t) n_input, sizeof(char *));
    tar = (const char **) R_alloc((size_t) n_target, sizeof(char *));
    PROTECT(ans = allocVector(INTSXP, n_input));
    ians = INTEGER(ans);
    if(useBytes) {
	for(R_xlen_t i = 0; i < n_input; i++) {
	    in[i] = CHAR(STRING_ELT(input, i));
	    ians[i] = 0;
	}
	for(int j = 0; j < n_target; j++)
	    tar[j] = CHAR(STRING_ELT(target, j));
    }
    else if(useUTF8) {
	for(R_xlen_t i = 0; i < n_input; i++) {
	    in[i] = translateCharUTF8(STRING_ELT(input, i));
	    ians[i] = 0;
	}
	for(int j = 0; j < n_target; j++)
	    tar[j] = translateCharUTF8(STRING_ELT(target, j));
    } else {
	for(R_xlen_t i = 0; i < n_input; i++) {
	    in[i] = translateChar(STRING_ELT(input, i));
	    ians[i] = 0;
	}
	for(int j = 0; j < n_target; j++)
	    tar[j] = translateChar(STRING_ELT(target, j));
    }
    /* First pass, exact matching */
    R_xlen_t nexact = 0;
    /* Compromise when hashing used changed in 3.2.0 (PR#15697) */
    if (n_input <= 100 || n_target <= 100) {
	for (R_xlen_t i = 0; i < n_input; i++) {
	    const char *ss = in[i];
	    if (strlen(ss) == 0) continue;
	    for (int j = 0; j < n_target; j++) {
		if (no_dups && used[j]) continue;
		if (strcmp(ss, tar[j]) == 0) {
		    ians[i] = j + 1;
		    if (no_dups) used[j] = 1;
		    nexact++;
		    break;
		}
	    }
	}
    } else {
	HashData data;
	HashTableSetup(target, &data, NA_INTEGER);
	data.useUTF8 = useUTF8;
	data.nomatch = 0;
	DoHashing(target, &data);
	for (R_xlen_t i = 0; i < n_input; i++) {
	    if (strlen(in[i]) == 0) /* don't look up "" */
		continue;
	    int j = Lookup(target, input, i, &data);
	    if ((j == 0) || (no_dups && used[j - 1])) continue;
	    if (no_dups) used[j - 1] = 1;
	    ians[i] = j;
	    nexact++;
	}
    }

    if(nexact < n_input) {
	/* Second pass, partial matching */
	for (R_xlen_t i = 0; i < n_input; i++) {
	    const char *ss;
	    if (ians[i]) continue;
	    ss = in[i];
	    temp = strlen(ss);
	    if (temp == 0) continue;
	    mtch = 0;
	    mtch_count = 0;
	    for (int j = 0; j < n_target; j++) {
		if (no_dups && used[j]) continue;
		if (strncmp(ss, tar[j], temp) == 0) {
		    mtch = j + 1;
		    mtch_count++;
		}
	    }
	    if (mtch > 0 && mtch_count == 1) {
		if(no_dups) used[mtch - 1] = 1;
		ians[i] = mtch;
	    }
	}
	/* Third pass, set no matches */
	for (R_xlen_t i = 0; i < n_input; i++)
	    if(ians[i] == 0) ians[i] = no_match;

    }
    UNPROTECT(1);
    return ans;
}


/* Partial Matching of Strings */
/* Based on Therneau's charmatch. */

SEXP attribute_hidden do_charmatch(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, input, target;
    const char *ss, *st;
    Rboolean useBytes = FALSE, useUTF8 = FALSE;

    checkArity(op, args);

    input = CAR(args);
    R_xlen_t n_input = LENGTH(input);
    target = CADR(args);
    int n_target = LENGTH(target);

    if (!isString(input) || !isString(target))
	error(_("argument is not of mode character"));
    int no_match = asInteger(CADDR(args));

    for(R_xlen_t i = 0; i < n_input; i++) {
	if(IS_BYTES(STRING_ELT(input, i))) {
	    useBytes = TRUE;
	    useUTF8 = FALSE;
	    break;
	} else if(ENC_KNOWN(STRING_ELT(input, i))) {
	    useUTF8 = TRUE;
	}
    }
    if(!useBytes) {
	for(int i = 0; i < n_target; i++) {
	    if(IS_BYTES(STRING_ELT(target, i))) {
		useBytes = TRUE;
		useUTF8 = FALSE;
		break;
	    } else if(ENC_KNOWN(STRING_ELT(target, i))) {
		useUTF8 = TRUE;
	    }
	}
    }

    PROTECT(ans = allocVector(INTSXP, n_input));
    int *ians = INTEGER(ans);

    const void *vmax = vmaxget();  // prudence: .Internal does this too.
    for(R_xlen_t i = 0; i < n_input; i++) {
	if(useBytes)
	    ss = CHAR(STRING_ELT(input, i));
	else if(useUTF8)
	    ss = translateCharUTF8(STRING_ELT(input, i));
	else
	    ss = translateChar(STRING_ELT(input, i));
	size_t temp = strlen(ss);
	int imatch = NA_INTEGER;
	Rboolean perfect = FALSE;
	/* we could reset vmax here too: worth it? */
	for(int j = 0; j < n_target; j++) {
	    if(useBytes)
		st = CHAR(STRING_ELT(target, j));
	    else if(useUTF8)
		st = translateCharUTF8(STRING_ELT(target, j));
	    else
		st = translateChar(STRING_ELT(target, j));
	    int k = strncmp(ss, st, temp);
	    if (k == 0) {
		if (strlen(st) == temp) {
		    if (perfect)
			imatch = 0;
		    else {
			perfect = TRUE;
			imatch = j + 1;
		    }
		}
		else if (!perfect) {
		    if (imatch == NA_INTEGER)
			imatch = j + 1;
		    else
			imatch = 0;
		}
	    }
	}
	ians[i] = (imatch == NA_INTEGER) ? no_match : imatch;
	vmaxset(vmax);
    }
    UNPROTECT(1);
    return ans;
}


/* Functions for matching the supplied arguments to the */
/* formal arguments of functions.  The returned value */
/* is a list with all components named. */

#define ARGUSED(x) LEVELS(x)

static SEXP StripUnmatched(SEXP s)
{
    if (s == R_NilValue) return s;

    if (CAR(s) == R_MissingArg && !ARGUSED(s) ) {
	return StripUnmatched(CDR(s));
    }
    else if (CAR(s) == R_DotsSymbol ) {
	return StripUnmatched(CDR(s));
    }
    else {
	SETCDR(s, StripUnmatched(CDR(s)));
	return s;
    }
}

static SEXP ExpandDots(SEXP s, int expdots)
{
    SEXP r;
    if (s == R_NilValue)
	return s;
    if (TYPEOF(CAR(s)) == DOTSXP ) {
	SET_TYPEOF(CAR(s), LISTSXP);	/* a safe mutation */
	if (expdots) {
	    r = CAR(s);
	    while (CDR(r) != R_NilValue ) {
		SET_ARGUSED(r, 1);
		r = CDR(r);
	    }
	    SET_ARGUSED(r, 1);
	    SETCDR(r, ExpandDots(CDR(s), expdots));
	    return CAR(s);
	}
    }
    else
	SET_ARGUSED(s, 0);
    SETCDR(s, ExpandDots(CDR(s), expdots));
    return s;
}
static SEXP subDots(SEXP rho)
{
    SEXP rval, dots, a, b, t;
    int len,i;

    dots = findVar(R_DotsSymbol, rho);

    if (dots == R_UnboundValue)
	error(_("... used in a situation where it does not exist"));

    if (dots == R_MissingArg)
	return dots;

    if (!isPairList(dots))
	error(_("... is not a pairlist"));

    len = length(dots);
    PROTECT(dots);
    PROTECT(rval=allocList(len));
    for(a = dots, b = rval, i = 1; i <= len; a = CDR(a), b = CDR(b), i++) {
	SET_TAG(b, TAG(a));
	t = CAR(a);
	while (TYPEOF(t) == PROMSXP)
	    t = PREXPR(t);
	if( isSymbol(t) || isLanguage(t) )
	    SETCAR(b, installDDVAL(i));
	else
	    SETCAR(b, t);
    }
    UNPROTECT(2);
    return rval;
}


SEXP attribute_hidden do_matchcall(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP formals, actuals, rlist;
    SEXP funcall, f, b, rval, sysp, t1, t2, tail;
//    RCNTXT *cptr;
    int expdots;

    checkArity(op,args);

    funcall = CADR(args);

    if (TYPEOF(funcall) == EXPRSXP)
	funcall = VECTOR_ELT(funcall, 0);

    if (TYPEOF(funcall) != LANGSXP)
	error(_("invalid '%s' argument"), "call");

    b = CAR(args);
    if (TYPEOF(b) != CLOSXP)
	error(_("invalid '%s' argument"), "definition");

    sysp = CAR(CDDDR(args));
    if (!isEnvironment(sysp))
	error(_("'envir' must be an environment"));

    /* Do we expand ... ? */

    expdots = asLogical(CAR(CDDR(args)));
    if (expdots == NA_LOGICAL)
	error(_("invalid '%s' argument"), "expand.dots");

    /* Get the formals and match the actual args */

    formals = FORMALS(b);
    PROTECT(actuals = duplicate(CDR(funcall)));

    /* If there is a ... symbol then expand it out in the sysp env
       We need to take some care since the ... might be in the middle
       of the actuals  */

    t2 = R_MissingArg;
    for (t1=actuals ; t1!=R_NilValue ; t1 = CDR(t1) ) {
	if (CAR(t1) == R_DotsSymbol) {
	    t2 = subDots(sysp);
	    break;
	}
    }
    /* now to splice t2 into the correct spot in actuals */
    if (t2 != R_MissingArg ) {	/* so we did something above */
	if( CAR(actuals) == R_DotsSymbol ) {
	    UNPROTECT(1);
	    actuals = listAppend(t2, CDR(actuals));
	    PROTECT(actuals);
	}
	else {
	    for(t1=actuals; t1!=R_NilValue; t1=CDR(t1)) {
		if( CADR(t1) == R_DotsSymbol ) {
		    tail = CDDR(t1);
		    SETCDR(t1, t2);
		    listAppend(actuals,tail);
		    break;
		}
	    }
	}
    } else { /* get rid of it */
	if( CAR(actuals) == R_DotsSymbol ) {
	    UNPROTECT(1);
	    actuals = CDR(actuals);
	    PROTECT(actuals);
	}
	else {
	    for(t1=actuals; t1!=R_NilValue; t1=CDR(t1)) {
		if( CADR(t1) == R_DotsSymbol ) {
		    tail = CDDR(t1);
		    SETCDR(t1, tail);
		    break;
		}
	    }
	}
    }
    rlist = matchArgs(formals, actuals, call);

    /* Attach the argument names as tags */

    for (f = formals, b = rlist; b != R_NilValue; b = CDR(b), f = CDR(f)) {
	SET_TAG(b, TAG(f));
    }


    /* Handle the dots */

    PROTECT(rlist = ExpandDots(rlist, expdots));

    /* Eliminate any unmatched formals and any that match R_DotSymbol */
    /* This needs to be after ExpandDots as the DOTSXP might match ... */

    rlist = StripUnmatched(rlist);

    PROTECT(rval = allocSExp(LANGSXP));
    SETCAR(rval, duplicate(CAR(funcall)));
    SETCDR(rval, rlist);
    UNPROTECT(3);
    return rval;
}


#include <R_ext/RS.h> /* for Memzero */

#ifdef _AIX  /*some people just have to be different: is this still needed? */
#    include <memory.h>
#endif


static SEXP
rowsum(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP rn)
{
    SEXP matches,ans;
    int n, p, ng, narm;
    R_xlen_t offset = 0, offsetg = 0;
    HashData data;
    data.nomatch = 0;

    n = LENGTH(g);
    ng = length(uniqueg);
    narm = asLogical(snarm);
    if(narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");
    if(isMatrix(x)) p = ncols(x); else p = 1;

    HashTableSetup(uniqueg, &data, NA_INTEGER);
    PROTECT(data.HashTable);
    DoHashing(uniqueg, &data);
    PROTECT(matches = HashLookup(uniqueg, g, &data));

    PROTECT(ans = allocMatrix(TYPEOF(x), ng, p));

    switch(TYPEOF(x)){
    case REALSXP:
	Memzero(REAL(ans), ng*p);
	for(int i = 0; i < p; i++) {
	    for(int j = 0; j < n; j++)
		if(!narm || !ISNAN(REAL(x)[j + offset]))
		    REAL(ans)[INTEGER(matches)[j] - 1 + offsetg]
			+= REAL(x)[j + offset];
	    offset += n;
	    offsetg += ng;
	}
	break;
    case INTSXP:
	Memzero(INTEGER(ans), ng*p);
	for(int i = 0; i < p; i++) {
	    for(int j = 0; j < n; j++) {
		if (INTEGER(x)[j + offset] == NA_INTEGER) {
		    if(!narm)
			INTEGER(ans)[INTEGER(matches)[j] - 1 + offsetg]
			    = NA_INTEGER;
		} else if (INTEGER(ans)[INTEGER(matches)[j] - 1 + offsetg]
			   != NA_INTEGER) {
		    /* check for integer overflows */
		    int itmp = INTEGER(ans)[INTEGER(matches)[j] - 1 + offsetg];
		    double dtmp = itmp;
		    dtmp += INTEGER(x)[j + offset];
		    if (dtmp < INT_MIN || dtmp > INT_MAX) itmp = NA_INTEGER;
		    else itmp += INTEGER(x)[j + offset];
		    INTEGER(ans)[INTEGER(matches)[j] - 1 + offsetg] = itmp;
		}
	    }
	    offset += n;
	    offsetg += ng;
	}
	break;
    default:
	error("non-numeric matrix in rowsum(): this should not happen");
    }
    if (TYPEOF(rn) != STRSXP) error("row names are not character");
    SEXP dn = allocVector(VECSXP, 2), dn2, dn3;
    setAttrib(ans, R_DimNamesSymbol, dn);
    SET_VECTOR_ELT(dn, 0, rn);
    dn2 = getAttrib(x, R_DimNamesSymbol);
    if(length(dn2) >= 2 &&
       !isNull(dn3 = VECTOR_ELT(dn2, 1))) SET_VECTOR_ELT(dn, 1, dn3);

    UNPROTECT(3); /* HashTable, matches, ans */
    return ans;
}

static SEXP
rowsum_df(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP rn)
{
    SEXP matches,ans,col,xcol;
    int p, narm;
    HashData data;
    data.nomatch = 0;

    R_xlen_t n = XLENGTH(g);
    p = LENGTH(x);
    R_xlen_t ng = XLENGTH(uniqueg);
    narm = asLogical(snarm);
    if(narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");

    HashTableSetup(uniqueg, &data, NA_INTEGER);
    PROTECT(data.HashTable);
    DoHashing(uniqueg, &data);
    PROTECT(matches = HashLookup(uniqueg, g, &data));

    PROTECT(ans = allocVector(VECSXP, p));

    for(int i = 0; i < p; i++) {
	xcol = VECTOR_ELT(x,i);
	if (!isNumeric(xcol))
	    error(_("non-numeric data frame in rowsum"));
	switch(TYPEOF(xcol)){
	case REALSXP:
	    PROTECT(col = allocVector(REALSXP,ng));
	    Memzero(REAL(col), ng);
	    for(R_xlen_t j = 0; j < n; j++)
		if(!narm || !ISNAN(REAL(xcol)[j]))
		    REAL(col)[INTEGER(matches)[j] - 1] += REAL(xcol)[j];
	    SET_VECTOR_ELT(ans,i,col);
	    UNPROTECT(1);
	    break;
	case INTSXP:
	    PROTECT(col = allocVector(INTSXP, ng));
	    Memzero(INTEGER(col), ng);
	    for(R_xlen_t j = 0; j < n; j++) {
		if (INTEGER(xcol)[j] == NA_INTEGER) {
		    if(!narm)
			INTEGER(col)[INTEGER(matches)[j] - 1] = NA_INTEGER;
		} else if (INTEGER(col)[INTEGER(matches)[j] - 1] != NA_INTEGER) {
		    int itmp = INTEGER(col)[INTEGER(matches)[j] - 1];
		    double dtmp = itmp;
		    dtmp += INTEGER(xcol)[j];
		    if (dtmp < INT_MIN || dtmp > INT_MAX) itmp = NA_INTEGER;
		    else itmp += INTEGER(xcol)[j];
		    INTEGER(col)[INTEGER(matches)[j] - 1] = itmp;
		}
	    }
	    SET_VECTOR_ELT(ans, i, col);
	    UNPROTECT(1);
	    break;

	default:
	    error(_("this cannot happen"));
	}
    }
    namesgets(ans, getAttrib(x, R_NamesSymbol));
    if (TYPEOF(rn) != STRSXP) error("row names are not character");
    setAttrib(ans, R_RowNamesSymbol, rn);
    classgets(ans, mkString("data.frame"));

    UNPROTECT(3); /* HashTable, matches, ans */
    return ans;
}

SEXP attribute_hidden do_rowsum(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    if(PRIMVAL(op) == 1)
	return rowsum_df(CAR(args), CADR(args), CADDR(args), CADDDR(args),
			 CAD4R(args));
    else
	return rowsum(CAR(args), CADR(args), CADDR(args), CADDDR(args),
		      CAD4R(args));
}


/* returns 1-based duplicate no */
static int isDuplicated2(SEXP x, int indx, HashData *d)
{
    int *h = INTEGER(d->HashTable);
    hlen i = d->hash(x, indx, d);
    while (h[i] != NIL) {
	if (d->equal(x, h[i], x, indx))
	    return h[i] + 1;
	i = (i + 1) % d->M;
    }
    h[i] = indx;
    return 0;
}

static SEXP duplicated2(SEXP x, HashData *d)
{
    SEXP ans;
    int i, n;

    n = LENGTH(x);
    HashTableSetup(x, d, NA_INTEGER);
    PROTECT(d->HashTable);
    PROTECT(ans = allocVector(INTSXP, n));

    int *h = INTEGER(d->HashTable);
    int *v = INTEGER(ans);
    for (i = 0; i < d->M; i++) h[i] = NIL;
    for (i = 0; i < n; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	v[i] = isDuplicated2(x, i, d);
    }
    UNPROTECT(2);
    return ans;
}

SEXP attribute_hidden do_makeunique(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP names, sep, ans, dup, newx;
    int i, cnt, *cnts, dp;
    int n, len, maxlen = 0;
    HashData data;
    const char *csep, *ss;
    const void *vmax;

    checkArity(op, args);
    names = CAR(args);
    if(!isString(names))
	error(_("'names' must be a character vector"));
    n = LENGTH(names);
    sep = CADR(args);
    if(!isString(sep) || LENGTH(sep) != 1)
	error(_("'%s' must be a character string"), "sep");
    csep = translateChar(STRING_ELT(sep, 0));
    PROTECT(ans = allocVector(STRSXP, n));
    vmax = vmaxget();
    for(i = 0; i < n; i++) {
	SET_STRING_ELT(ans, i, STRING_ELT(names, i));
	len = (int) strlen(translateChar(STRING_ELT(names, i)));
	if(len > maxlen) maxlen = len;
	vmaxset(vmax);
    }
    if(n > 1) {
	/* +2 for terminator and rounding error */
	char buf[maxlen + (int) strlen(csep)
		 + (int) (log((double)n)/log(10.0)) + 2];
	if(n < 10000) {
	    R_CheckStack2((size_t)n * sizeof(int));
	    cnts = (int *) alloca(((size_t) n) * sizeof(int));
	} else {
	    /* This is going to be slow so use expensive allocation
	       that will be recovered if interrupted. */
	    cnts = (int *) R_alloc((size_t) n,	sizeof(int));
	}
	for(i = 0; i < n; i++) cnts[i] = 1;
	data.nomatch = 0;
	PROTECT(newx = allocVector(STRSXP, 1));
	PROTECT(dup = duplicated2(names, &data));
	PROTECT(data.HashTable);
	vmax = vmaxget();
	for(i = 1; i < n; i++) { /* first cannot be a duplicate */
	    dp = INTEGER(dup)[i]; /* 1-based number of first occurrence */
	    if(dp == 0) continue;
	    ss = translateChar(STRING_ELT(names, i));
	    /* Try appending 1,2,3, ..., n-1 until it is not already in use */
	    for(cnt = cnts[dp - 1]; cnt < n; cnt++) {
		sprintf(buf, "%s%s%d", ss, csep, cnt);
		SET_STRING_ELT(newx, 0, mkChar(buf));
		if(Lookup(ans, newx, 0, &data) == data.nomatch) break;
	    }
	    SET_STRING_ELT(ans, i, STRING_ELT(newx, 0));
	    /* insert it */ (void) isDuplicated(ans, i, &data);
	    cnts[dp - 1] = cnt+1; /* cache the first unused cnt value */
	    vmaxset(vmax);
	}
	UNPROTECT(3);
    }
    UNPROTECT(1);
    return ans;
}

/* Use hashing to improve object.size. Here we want equal CHARSXPs,
   not equal contents. */

static int csequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    return STRING_ELT(x, i) == STRING_ELT(y, j);
}

static void HashTableSetup1(SEXP x, HashData *d)
{
    d->hash = cshash;
    d->equal = csequal;
#ifdef LONG_VECTOR_SUPPORT
    d->isLong = FALSE;
#endif
    MKsetup(LENGTH(x), d, NA_INTEGER);
    d->HashTable = allocVector(INTSXP, (R_xlen_t) d->M);
    for (R_xlen_t i = 0; i < d->M; i++) INTEGER(d->HashTable)[i] = NIL;
}

/* used in utils */
SEXP Rf_csduplicated(SEXP x)
{
    SEXP ans;
    int n;
    HashData data;

    if(TYPEOF(x) != STRSXP)
	error("C function 'csduplicated' not called on a STRSXP");
    n = LENGTH(x);
    HashTableSetup1(x, &data);
    PROTECT(data.HashTable);
    PROTECT(ans = allocVector(LGLSXP, n));

    int *v = LOGICAL(ans);

    for (int i = 0; i < n; i++) v[i] = isDuplicated(x, i, &data);

    UNPROTECT(2);
    return ans;
}

#include <R_ext/Random.h>

// more fine-grained  unif_rand() for n > INT_MAX
static R_INLINE double ru()
{
    double U = 33554432.0;
    return (floor(U*unif_rand()) + unif_rand())/U;
}

// sample.int(.) --> .Internal(sample2(n, size)) :
SEXP attribute_hidden do_sample2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP ans;
    double dn = asReal(CAR(args));
    int k = asInteger(CADR(args));
    if (!R_FINITE(dn) || dn < 0 || dn > 4.5e15 || (k > 0 && dn == 0))
	error(_("invalid first argument"));
    if (k < 0) error(_("invalid '%s' argument"), "size");
    if (k > dn/2) error("This algorithm is for size <= n/2");
    HashData data;
    GetRNGstate();
    if (dn > INT_MAX) {
	ans = PROTECT(allocVector(REALSXP, k));
	double *ry = REAL(ans);
	HashTableSetup(ans, &data, NA_INTEGER);
	PROTECT(data.HashTable);
	for (int i = 0; i < k; i++)
	    for(int j = 0; j < 100; j++) { // average < 2
		ry[i] = floor(dn * ru() + 1);
		if(!isDuplicated(ans, i, &data)) break;
	    }
   } else {
	ans = PROTECT(allocVector(INTSXP, k));
	int *iy = INTEGER(ans);
	HashTableSetup(ans, &data, NA_INTEGER);
	PROTECT(data.HashTable);
	for (int i = 0; i < k; i++)
	    for(int j = 0; j < 100; j++) { // average < 2
		iy[i] = (int)(dn * unif_rand() + 1);
		if(!isDuplicated(ans, i, &data)) break;
	    }
    }
    PutRNGstate();
    UNPROTECT(2);
    return ans;
}
