/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2006  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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
 */

/* <UTF8> char here is either ASCII or handled as a whole or as 
   a leading portion for partial matching 
*/


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined(HAVE_GLIBC2)
/* for isnan in Rinlinedfuns.h */
# define _SVID_SOURCE 1
#endif

#include <Defn.h>

#define NIL -1
#define ARGUSED(x) LEVELS(x)
#define SET_ARGUSED(x,v) SETLEVELS(x,v)

/* Hash function and equality test for keys */
typedef struct _HashData HashData;

struct _HashData {
  int K, M;
  int(*hash) (SEXP, int, HashData *);
  int(*equal) (SEXP, int, SEXP, int);
  SEXP HashTable;

  int nomatch;
};


/* 
   Integer keys are hashed via a random number generator
   based on Knuth's recommendations.  The high order K bits
   are used as the hash code.
   
   NB: lots of this code relies on M being a power of two and
   on silent integer overflow mod 2^32.  It also relies on M < 31.

   <FIXME>  Integer keys are wasteful for logical and raw vectors, 
   but the tables are small in that case.
*/

static int scatter(unsigned int key, HashData *d)
{
    return 3141592653U * key >> (32 - d->K);
}

static int lhash(SEXP x, int indx, HashData *d)
{
    if (LOGICAL(x)[indx] == NA_LOGICAL)
	return 2;
    return LOGICAL(x)[indx];
}

static int ihash(SEXP x, int indx, HashData *d)
{
    if (INTEGER(x)[indx] == NA_INTEGER)
	return 0;
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

static int rhash(SEXP x, int indx, HashData *d)
{
    /* There is a problem with signed 0s under IEEE */
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

static int chash(SEXP x, int indx, HashData *d)
{
    Rcomplex tmp;
    unsigned int u;
    tmp.r = (COMPLEX(x)[indx].r == 0.0) ? 0.0 : COMPLEX(x)[indx].r;
    tmp.i = (COMPLEX(x)[indx].i == 0.0) ? 0.0 : COMPLEX(x)[indx].i;
    /* we want all NaNs except NA equal, and all NAs equal */
    if (R_IsNA(tmp.r)) tmp.r = NA_REAL;
    else if (R_IsNaN(tmp.r)) tmp.r = R_NaN;
    if (R_IsNA(tmp.i)) tmp.i = NA_REAL;
    else if (R_IsNaN(tmp.i)) tmp.i = R_NaN;
#if 2*SIZEOF_INT == SIZEOF_DOUBLE
    {
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

static int shash(SEXP x, int indx, HashData *d)
{
    unsigned int k;
    char *p = CHAR(STRING_ELT(x, indx));
    k = 0;
    while (*p++)
	    k = 11 * k + *p; /* was 8 but 11 isn't a power of 2 */
    return scatter(k, d);
}

static int lequal(SEXP x, int i, SEXP y, int j)
{
    return (LOGICAL(x)[i] == LOGICAL(y)[j]);
}


static int iequal(SEXP x, int i, SEXP y, int j)
{
    return (INTEGER(x)[i] == INTEGER(y)[j]);
}

/* BDR 2002-1-17  We don't want NA and other NaNs to be equal */
static int requal(SEXP x, int i, SEXP y, int j)
{
    if (!ISNAN(REAL(x)[i]) && !ISNAN(REAL(y)[j]))
	return (REAL(x)[i] == REAL(y)[j]);
    else if (R_IsNA(REAL(x)[i]) && R_IsNA(REAL(y)[j])) return 1;
    else if (R_IsNaN(REAL(x)[i]) && R_IsNaN(REAL(y)[j])) return 1;
    else return 0;
}

static int cequal(SEXP x, int i, SEXP y, int j)
{
    if (!ISNAN(COMPLEX(x)[i].r) && !ISNAN(COMPLEX(x)[i].i)
       && !ISNAN(COMPLEX(y)[j].r) && !ISNAN(COMPLEX(y)[j].i))
	return COMPLEX(x)[i].r == COMPLEX(y)[j].r &&
	    COMPLEX(x)[i].i == COMPLEX(y)[j].i;
    else if ((R_IsNA(COMPLEX(x)[i].r) || R_IsNA(COMPLEX(x)[i].i))
	    && (R_IsNA(COMPLEX(y)[j].r) || R_IsNA(COMPLEX(y)[j].i)))
	return 1;
    else if ((R_IsNaN(COMPLEX(x)[i].r) || R_IsNaN(COMPLEX(x)[i].i))
	    && (R_IsNaN(COMPLEX(y)[j].r) || R_IsNaN(COMPLEX(y)[j].i)))
	return 1;
    else
	return 0;
}

static int sequal(SEXP x, int i, SEXP y, int j)
{
    /* Two strings which have the same address must be the same, 
       so avoid looking at the contents */
    if (STRING_ELT(x, i) == STRING_ELT(y, j)) return 1;
    /* Then if either is NA the other cannot be */
    if (STRING_ELT(x, i) == NA_STRING || STRING_ELT(y, j) == NA_STRING) 
	return 0;
    /* Finally look at the contents if necessary */
    return !strcmp(CHAR(STRING_ELT(x, i)), CHAR(STRING_ELT(y, j)));
}

static int rawhash(SEXP x, int indx, HashData *d)
{
    return RAW(x)[indx];
}

static int rawequal(SEXP x, int i, SEXP y, int j)
{
    return (RAW(x)[i] == RAW(y)[j]);
}

static int vhash(SEXP x, int indx, HashData *d)
{
    int i;
    unsigned int key;
    SEXP _this = VECTOR_ELT(x, indx);
    
    key = OBJECT(_this) + 2*TYPEOF(_this) + 100*length(_this);
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
	    key ^= scatter(rawhash(_this, i, d), d);
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

static int vequal(SEXP x, int i, SEXP y, int j)
{
    return compute_identical(VECTOR_ELT(x, i), VECTOR_ELT(y, j));
}

/*
  Choose M to be the smallest power of 2
  not less than 2*n and set K = log2(M).
  Need K >= 1 and hence M >= 2, and 2^M <= 2^31 -1, hence n <= 2^29.

  Dec 2004: modified from 4*n to 2*n, since in the worst case we have
  a 50% full table, and that is still rather efficient -- see
  R. Sedgewick (1998) Algorithms in C++ 3rd edition p.606.
*/
static void MKsetup(int n, HashData *d)
{
    int n4 = 2 * n;

    if(n < 0 || n > 536870912) /* protect against overflow to -ve */
	error(_("length %d is too large for hashing"), n);
    d->M = 2;
    d->K = 1;
    while (d->M < n4) {
	d->M *= 2;
	d->K += 1;
    }
}

static void HashTableSetup(SEXP x, HashData *d)
{
    switch (TYPEOF(x)) {
    case LGLSXP:
	d->hash = lhash;
	d->equal = lequal;
	MKsetup(3, d);
	break;
    case INTSXP:
	d->hash = ihash;
	d->equal = iequal;
	MKsetup(LENGTH(x), d);
	break;
    case REALSXP:
	d->hash = rhash;
	d->equal = requal;
	MKsetup(LENGTH(x), d);
	break;
    case CPLXSXP:
	d->hash = chash;
	d->equal = cequal;
	MKsetup(LENGTH(x), d);
	break;
    case STRSXP:
	d->hash = shash;
	d->equal = sequal;
	MKsetup(LENGTH(x), d);
	break;
    case RAWSXP:
	d->hash = rawhash;
	d->equal = rawequal;
	d->M = 256;
	d->K = 8; /* unused */
	break;
    case VECSXP:
	d->hash = vhash;
	d->equal = vequal;
	MKsetup(LENGTH(x), d);
	break;
    default:
	UNIMPLEMENTED_TYPE("HashTableSetup", x);
    }
    d->HashTable = allocVector(INTSXP, d->M);
}

/* Open address hashing */
/* Collision resolution is by linear probing */
/* The table is guaranteed large so this is sufficient */

static int isDuplicated(SEXP x, int indx, HashData *d)
{
    int i, *h;

    h = INTEGER(d->HashTable);
    i = d->hash(x, indx, d);
    while (h[i] != NIL) {
	if (d->equal(x, h[i], x, indx))
	    return 1;
	i = (i + 1) % d->M;
    }
    h[i] = indx;
    return 0;
}

SEXP duplicated(SEXP x)
{
    SEXP ans;
    int *h, *v;
    int i, n;
    HashData data;

    if (!isVector(x)) 
	error(_("'duplicated' applies only to vectors"));

    n = LENGTH(x);
    HashTableSetup(x, &data);
    PROTECT(data.HashTable);
    ans = allocVector(LGLSXP, n);
    UNPROTECT(1);
    h = INTEGER(data.HashTable);
    v = LOGICAL(ans);

    for (i = 0; i < data.M; i++)
	h[i] = NIL;

    for (i = 0; i < n; i++)
	v[i] = isDuplicated(x, i, &data);

    return ans;
}

/* .Internal(duplicated(x))       [op=0]
   .Internal(unique(x))	          [op=1]
*/
SEXP attribute_hidden do_duplicated(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, dup, ans;
    int i, k, n;

    checkArity(op, args);
    x = CAR(args);
    /* handle zero length vectors, and NULL */
    if ((n = length(x)) == 0)
	return(allocVector(PRIMVAL(op) != 1 ? LGLSXP : TYPEOF(x), 0));

    if (!isVector(x)) {
	PrintValue(x);
	error(_("%s() applies only to vectors"),
	      (PRIMVAL(op) == 0 ? "duplicated" : "unique"));
    }

    dup = duplicated(x);
    if (PRIMVAL(op) == 0) /* "duplicated()" : */
	return dup;
    /*	ELSE
	use the results of "duplicated" to get "unique" */

    /* count unique entries */
    k = 0;
    for (i = 0; i < n; i++)
	if (LOGICAL(dup)[i] == 0)
	    k++;

    PROTECT(dup);
    ans = allocVector(TYPEOF(x), k);
    UNPROTECT(1);

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
    return ans;
}

/* Build a hash table, ignoring information on duplication */
static void DoHashing(SEXP table, HashData *d)
{
    int *h, i, n;

    n = LENGTH(table);
    h = INTEGER(d->HashTable);

    for (i = 0; i < d->M; i++)
	h[i] = NIL;

    for (i = 0; i < n; i++)
	(void) isDuplicated(table, i, d);
}

static int Lookup(SEXP table, SEXP x, int indx, HashData *d)
{
    int i, *h;

    h = INTEGER(d->HashTable);
    i = d->hash(x, indx, d);
    while (h[i] != NIL) {
	if (d->equal(table, h[i], x, indx))
	    return h[i] + 1;
	i = (i + 1) % d->M;
    }
    return d->nomatch;
}

/* Now do the table lookup */
static SEXP HashLookup(SEXP table, SEXP x, HashData *d)
{
    SEXP ans;
    int i, n;

    n = LENGTH(x);
    ans = allocVector(INTSXP, n);
    for (i = 0; i < n; i++) {
	INTEGER(ans)[i] = Lookup(table, x, i, d);
    }
    return ans;
}

SEXP attribute_hidden do_match(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int nomatch;

    checkArity(op, args);

    if ((!isVector(CAR(args)) && !isNull(CAR(args)))
	|| (!isVector(CADR(args)) && !isNull(CADR(args))))
	error(_("'match' requires vector arguments"));

    nomatch = asInteger(CAR(CDDR(args)));
    return match(CADR(args), CAR(args), nomatch);
}

SEXP match(SEXP itable, SEXP ix, int nmatch)
{
    SEXP ans, x, table;
    SEXPTYPE type;
    HashData data;
    int i, n = length(ix);

    /* Coerce to a common type; type == NILSXP is ok here.
     * Note that R's match() does only coerce factors (to character).
     * Hence, coerce to character or to `higher' type
     * (given that we have "Vector" or NULL) */
    if(TYPEOF(ix)  >= STRSXP || TYPEOF(itable) >= STRSXP) type = STRSXP;
    else type = TYPEOF(ix) < TYPEOF(itable) ? TYPEOF(itable) : TYPEOF(ix);
    PROTECT(x = coerceVector(ix, type));
    PROTECT(table = coerceVector(itable, type));

    /* handle zero length arguments */
    if (n == 0) {
	UNPROTECT(2);
	return allocVector(INTSXP, 0);
    }
    if (length(table) == 0) {
	ans = allocVector(INTSXP, n);
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] = nmatch;
	UNPROTECT(2);
	return ans;
    }
    data.nomatch = nmatch;
    HashTableSetup(table, &data);
    PROTECT(data.HashTable);
    DoHashing(table, &data);
    ans = HashLookup(table, x, &data);
    UNPROTECT(3);
    return ans;
}

/* Partial Matching of Strings */
/* Fully S Compatible version. */

/* Hmm, this was not all S compatible!  The desired behaviour is:
 * First do exact matches, and mark elements as used as they are matched
 *   unless dup_ok is true.
 * Then do partial matching, from left to right, using up the table
 *   unless dup_ok is true.  Multiple partial matches are ignored.
 * Empty strings are unmatched                        BDR 2000/2/16
 */

SEXP attribute_hidden do_pmatch(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, input, target;
    int i, j, k, mtch, n_input, n_target, mtch_count, temp, dups_ok;
    int * used;
    char *vmax;

    checkArity(op, args);
    vmax = vmaxget();
    input = CAR(args);
    n_input = LENGTH(input);
    target = CADR(args);
    n_target = LENGTH(target);
    dups_ok = asLogical(CADDR(args));
    if (dups_ok == NA_LOGICAL)
	errorcall(call, _("invalid '%s' argument"), "duplicates.ok");

    if (!isString(input) || !isString(target))
	errorcall(call, _("argument is not of mode character"));

    used = (int *) R_alloc(n_target, sizeof(int));
    for (j = 0; j < n_target; j++) used[j] = 0;
    ans = allocVector(INTSXP, n_input);
    for (i = 0; i < n_input; i++) INTEGER(ans)[i] = 0;

    /* First pass, exact matching */
    for (i = 0; i < n_input; i++) {
	temp = strlen(CHAR(STRING_ELT(input, i)));
	if (temp == 0) continue;
	for (j = 0; j < n_target; j++) {
	    if (!dups_ok && used[j]) continue;
	    k = strcmp(CHAR(STRING_ELT(input, i)),
		       CHAR(STRING_ELT(target, j)));
	    if (k == 0) {
		used[j] = 1;
		INTEGER(ans)[i] = j + 1;
		break;
	    }
	}
    }
    /* Second pass, partial matching */
    for (i = 0; i < n_input; i++) {
	if (INTEGER(ans)[i]) continue;
	temp = strlen(CHAR(STRING_ELT(input, i)));
	if (temp == 0) continue;
	mtch = 0;
	mtch_count = 0;
	for (j = 0; j < n_target; j++) {
	    if (!dups_ok && used[j]) continue;
	    k = strncmp(CHAR(STRING_ELT(input, i)),
			CHAR(STRING_ELT(target, j)), temp);
	    if (k == 0) {
		mtch = j + 1;
		mtch_count++;
	    }
	}
	if (mtch > 0 && mtch_count == 1) {
	    used[mtch - 1] = 1;
	    INTEGER(ans)[i] = mtch;
	}
    }

    vmaxset(vmax);
    return ans;
}


/* Partial Matching of Strings */
/* Based on Therneau's charmatch. */

SEXP attribute_hidden do_charmatch(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, input, target;
    Rboolean perfect;
    int i, j, k, imatch, n_input, n_target, temp;

    checkArity(op, args);

    input = CAR(args);
    n_input = LENGTH(input);
    target = CADR(args);
    n_target = LENGTH(target);

    if (!isString(input) || !isString(target))
	errorcall(call, _("argument is not of mode character"));

    ans = allocVector(INTSXP, n_input);

    for (i = 0; i < n_input; i++) {
	temp = strlen(CHAR(STRING_ELT(input, i)));
	imatch = NA_INTEGER;
	perfect = FALSE;
	for (j = 0; j < n_target; j++) {
	    k = strncmp(CHAR(STRING_ELT(input, i)),
			CHAR(STRING_ELT(target, j)), temp);
	    if (k == 0) {
		if (strlen(CHAR(STRING_ELT(target, j))) == temp) {
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
	INTEGER(ans)[i] = imatch;
    }
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
    char tbuf[10];

    dots = findVar(R_DotsSymbol, rho);

    if (dots == R_UnboundValue)
	error(_("... used in a situation where it doesn't exist"));

    if (dots == R_MissingArg)
	return dots;

    len = length(dots);
    PROTECT(rval=allocList(len));
    for(a=dots, b=rval, i=1; i<=len; a=CDR(a), b=CDR(b), i++) {
	sprintf(tbuf,"..%d",i);
	SET_TAG(b, TAG(a));
	t = CAR(a);
	while (TYPEOF(t) == PROMSXP)
	    t = PREXPR(t);
	if( isSymbol(t) || isLanguage(t) )
	    SETCAR(b, mkSYMSXP(mkChar(tbuf), R_UnboundValue));
	else
	    SETCAR(b, t);
    }
    UNPROTECT(1);
    return rval;
}


SEXP attribute_hidden do_matchcall(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP formals, actuals, rlist;
    SEXP funcall, f, b, rval, sysp, t1, t2, tail;
    RCNTXT *cptr;
    int expdots;

    checkArity(op,args);

    funcall = CADR(args);

    if (TYPEOF(funcall) == EXPRSXP)
	funcall = VECTOR_ELT(funcall, 0);

    if (TYPEOF(funcall) != LANGSXP) {
	b = deparse1(funcall, 1, SIMPLEDEPARSE);
	errorcall(call, _("'%s' is not a valid call"), CHAR(STRING_ELT(b, 0)));
    }

    /* Get the function definition */
    sysp = R_GlobalContext->sysparent;

    if (TYPEOF(CAR(args)) == NILSXP) {
	/* Get the env that the function containing */
	/* matchcall was called from. */
	cptr = R_GlobalContext;
	while (cptr != NULL) {
	    if (cptr->callflag & CTXT_FUNCTION && cptr->cloenv == sysp)
		break;
	    cptr = cptr->nextcontext;
	}
	if ( cptr == NULL )
	    sysp = R_GlobalEnv;
	else
	    sysp = cptr->sysparent;
	if (cptr != NULL)
	    /* Changed to use the function from which match.call was
	       called as recorded in the context.  This change is
	       needed in case the current function is computed in a
	       way that cannot be reproduced by a second computation,
	       or if it is a registered S3 method that is not
	       lexically visible at the call site.

	       There is one particular case where this represents a
	       change from previous semantics: The definition is NULL,
	       the call is supplied explicitly, and the function in
	       the call is NOT the current function.  The new behavior
	       is to ignore the function in the call and use the
	       current function.  This is consistent with (my reading
	       of) the documentation in both R and Splus.  However,
	       the old behavior of R was consistent with the behavior
	       of Splus (and inconsistent with the documentation in
	       both cases).

	       The previous semantics for this case can be restored by
	       having the .Internal receive an additional argument
	       that indicates whether the call was supplied explicitly
	       or missing, and using the function recorded in the
	       context only if the call was not supplied explicitly.
	       The documentation should also be changed to be
	       consistent with this behavior.  LT */
	    PROTECT(b = duplicate(cptr->callfun));
	else if ( TYPEOF(CAR(funcall)) == SYMSXP )
	    PROTECT(b = findFun(CAR(funcall), sysp));
	else
	    PROTECT(b = eval(CAR(funcall), sysp));
    }
    else PROTECT(b = CAR(args));

    /* It must be a closure! */

    if (TYPEOF(b) != CLOSXP) {
	b = deparse1(b, 1, SIMPLEDEPARSE);
	errorcall(call, _("'%s' is not a function"), CHAR(STRING_ELT(b, 0)));
    }

    /* Do we expand ... ? */

    expdots = asLogical(CAR(CDDR(args)));
    if (expdots == NA_LOGICAL) {
	b = deparse1(CADDR(args), 1, SIMPLEDEPARSE);
	errorcall(call, _("'%s' is not a logical"), CHAR(STRING_ELT(b, 0)));
    }

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
    rlist = matchArgs(formals, actuals);

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
    UNPROTECT(4);
    return rval;
}


#if defined(HAVE_STRING_H)
#  include <string.h>
#  ifdef _AIX  /*some people just have to be different */
#    include <memory.h>
#  endif
/* int and double zeros are all bits off */
#  define ZEROINT(X,N,I) do{memset(INTEGER(X),0,N*sizeof(int));}while(0)
#  define ZERODBL(X,N,I) do{memset(REAL(X),0,N*sizeof(double));}while(0)
#else
#  define ZEROINT(X,N,I) for(I=0;I<N;I++) INTEGER(X)[I]=0
#  define ZERODBL(X,N,I) for(I=0;I<N;I++) REAL(X)[I]=0
#endif

SEXP attribute_hidden 
Rrowsum_matrix(SEXP x, SEXP ncol, SEXP g, SEXP uniqueg, SEXP snarm)
{
    SEXP matches,ans;
    int i, j, n, p, ng = 0, offset, offsetg, narm;
    HashData data;
    data.nomatch = 0;

    n = LENGTH(g);
    p = INTEGER(ncol)[0];
    ng = length(uniqueg);
    narm = asLogical(snarm);
    if(narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");

    HashTableSetup(uniqueg, &data);
    PROTECT(data.HashTable);
    DoHashing(uniqueg, &data);
    PROTECT(matches = HashLookup(uniqueg, g, &data));

    PROTECT(ans=allocMatrix(TYPEOF(x), ng, p));

    offset = 0; offsetg = 0;

    switch(TYPEOF(x)){
    case REALSXP:
	ZERODBL(ans, ng*p, i);
	for(i = 0; i < p; i++) {
	    for(j = 0; j < n; j++)
		if(!narm || !ISNAN(REAL(x)[j+offset]))
		    REAL(ans)[INTEGER(matches)[j]-1+offsetg] 
			+= REAL(x)[j+offset];
	    offset += n;
	    offsetg += ng;
	}
	break;
    case INTSXP:
	ZEROINT(ans, ng*p, i);
	for(i = 0; i < p; i++) {
	    for(j = 0; j < n; j++) {
		if (INTEGER(x)[j+offset] == NA_INTEGER) {
		    if(!narm)
			INTEGER(ans)[INTEGER(matches)[j]-1+offsetg] 
			    = NA_INTEGER;
		} else if (INTEGER(ans)[INTEGER(matches)[j]-1+offsetg] 
			 != NA_INTEGER)
		    INTEGER(ans)[INTEGER(matches)[j]-1+offsetg] 
			+= INTEGER(x)[j+offset];
	    }
	    offset += n;
	    offsetg += ng;
	}
	break;
    default:
	error(_("non-numeric matrix in rowsum(): this cannot happen"));
    }

    UNPROTECT(2); /*HashTable, matches*/
    UNPROTECT(1); /*ans*/
    return ans;
}

SEXP attribute_hidden
Rrowsum_df(SEXP x, SEXP ncol, SEXP g, SEXP uniqueg, SEXP snarm)
{
    SEXP matches,ans,col,xcol;
    int i, j, n, p, ng = 0, offset, offsetg, narm;
    HashData data;
    data.nomatch = 0;

    n = LENGTH(g);
    p = INTEGER(ncol)[0];
    ng = length(uniqueg);
    narm = asLogical(snarm);
    if(narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");

    HashTableSetup(uniqueg, &data);
    PROTECT(data.HashTable);
    DoHashing(uniqueg, &data);
    PROTECT(matches = HashLookup(uniqueg, g, &data));

    PROTECT(ans = allocVector(VECSXP, p));

    offset = 0; offsetg = 0;

    for(i = 0; i < p; i++) {
	xcol = VECTOR_ELT(x,i);
	if (!isNumeric(xcol))
	    error(_("non-numeric data frame in rowsum"));
	switch(TYPEOF(xcol)){
	case REALSXP:
	    PROTECT(col = allocVector(REALSXP,ng));
	    ZERODBL(col, ng, i);
	    for(j = 0; j < n; j++)
		if(!narm || !ISNAN(REAL(xcol)[j]))
		    REAL(col)[INTEGER(matches)[j]-1] += REAL(xcol)[j];
	    SET_VECTOR_ELT(ans,i,col);
	    UNPROTECT(1);
	    break;
	case INTSXP:
	    PROTECT(col = allocVector(INTSXP, ng));
	    ZEROINT(col, ng, i);
	    for(j = 0; j < n; j++) {
		if (INTEGER(xcol)[j] == NA_INTEGER) {
		    if(!narm)
			INTEGER(col)[INTEGER(matches)[j]-1] = NA_INTEGER;
		} else if (INTEGER(col)[INTEGER(matches)[j]-1] != NA_INTEGER)
		    INTEGER(col)[INTEGER(matches)[j]-1] += INTEGER(xcol)[j];
	    }
	    SET_VECTOR_ELT(ans, i, col);
	    UNPROTECT(1);
	    break;

	default:
	    error(_("this cannot happen"));
	}
    }
    namesgets(ans, getAttrib(x, R_NamesSymbol));

    UNPROTECT(2); /*HashTable, matches*/
    UNPROTECT(1); /*ans*/
    return ans;
}

/* returns 1-based duplicate no */
static int isDuplicated2(SEXP x, int indx, HashData *d)
{
    int i, *h;

    h = INTEGER(d->HashTable);
    i = d->hash(x, indx, d);
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
    int *h, *v;
    int i, n;

    n = LENGTH(x);
    HashTableSetup(x, d);
    PROTECT(d->HashTable);
    ans = allocVector(INTSXP, n);
    UNPROTECT(1);
    h = INTEGER(d->HashTable);
    v = INTEGER(ans);
    for (i = 0; i < d->M; i++) h[i] = NIL;
    for (i = 0; i < n; i++) v[i] = isDuplicated2(x, i, d);
    return ans;
}

SEXP attribute_hidden do_makeunique(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP names, sep, ans, dup, newx;
    int i, n, cnt, len, maxlen = 0, *cnts, dp;
    HashData data;
    char *csep, *buf;
    char *vmax;

    checkArity(op, args);
    vmax = vmaxget();
    names = CAR(args);
    if(!isString(names))
	errorcall(call, _("'names' must be a character vector"));
    n = LENGTH(names);
    sep = CADR(args);
    if(!isString(sep) || LENGTH(sep) != 1)
	errorcall(call, _("'sep' must be a character string"));
    csep = CHAR(STRING_ELT(sep, 0));
    PROTECT(ans = allocVector(STRSXP, n));
    for(i = 0; i < n; i++) {
	SET_STRING_ELT(ans, i, STRING_ELT(names, i));
	len = strlen(CHAR(STRING_ELT(names, i)));
	if(len > maxlen) maxlen = len;
    }
    if(n > 1) {
	/* +2 for terminator and rounding error */
	buf = (char *) alloca(maxlen + strlen(csep) + (int) (log((double)n)/log(10.0)) + 2);
	if(n < 10000) {
	    cnts = (int *) alloca(n * sizeof(int));
	} else {
	    /* This is going to be slow so use expensive allocation
	       that will be recovered if interrupted. */
	    cnts = (int *) R_alloc(n,  sizeof(int));
	}
	R_CheckStack();
	for(i = 0; i < n; i++) cnts[i] = 1;
	data.nomatch = 0;
	PROTECT(newx = allocVector(STRSXP, 1));
	PROTECT(dup = duplicated2(names, &data));
	PROTECT(data.HashTable);
	for(i = 1; i < n; i++) { /* first cannot be a duplicate */
	    dp = INTEGER(dup)[i]; /* 1-based number of first occurrence */
	    if(dp == 0) continue;
	    /* Try appending 1,2,3, ..., n-1 until it is not already in use */
	    for(cnt = cnts[dp-1]; cnt < n; cnt++) {
		sprintf(buf, "%s%s%d", CHAR(STRING_ELT(names, i)), csep, cnt);
		SET_STRING_ELT(newx, 0, mkChar(buf));
		if(Lookup(ans, newx, 0, &data) == data.nomatch) break;
	    }
	    SET_STRING_ELT(ans, i, STRING_ELT(newx, 0));
	    /* insert it */ (void) isDuplicated(ans, i, &data);
	    cnts[dp-1] = cnt+1; /* cache the first unused cnt value */
	}
	UNPROTECT(3);
    }
    UNPROTECT(1);
    vmaxset(vmax);
    return ans;
}

/* Use hashing to improve object.size. Here we want equal CHARSXPs, 
   not equal contents.  This only uses the bottom 32 bits of the pointer, 
   but for now that's almost certainly OK */

static int cshash(SEXP x, int indx, HashData *d)
{
    intptr_t z = (intptr_t) STRING_ELT(x, indx);
    unsigned int z1 = z & 0xffffffff, z2 = 0;
#if SIZEOF_LONG == 8
    z2 = z/0x100000000L;
#endif
    return scatter(z1 ^ z2, d);
}

static int csequal(SEXP x, int i, SEXP y, int j)
{
    return STRING_ELT(x, i) == STRING_ELT(y, j);
}

static void HashTableSetup1(SEXP x, HashData *d)
{
    d->hash = cshash;
    d->equal = csequal;
    MKsetup(LENGTH(x), d);
    d->HashTable = allocVector(INTSXP, d->M);
}

SEXP attribute_hidden csduplicated(SEXP x)
{
    SEXP ans;
    int *h, *v;
    int i, n;
    HashData data;

    if(TYPEOF(x) != STRSXP)
	error(_("csduplicated not called on a STRSXP"));
    n = LENGTH(x);
    HashTableSetup1(x, &data);
    PROTECT(data.HashTable);
    ans = allocVector(LGLSXP, n);
    UNPROTECT(1);
    h = INTEGER(data.HashTable);
    v = LOGICAL(ans);

    for (i = 0; i < data.M; i++)
	h[i] = NIL;

    for (i = 0; i < n; i++)
	v[i] = isDuplicated(x, i, &data);

    return ans;
}
