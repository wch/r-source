/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2019   The R Core Team.
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


 *  Matching and Partial Matching for Strings
 *
 *  In theory all string matching code should be placed in this file
 *  At present there are still a couple of rogue matchers about.
 *
 *
 *  psmatch(char *, char *, int);
 *
 *  This code will perform partial matching for list tags.  When
 *  exact is 1, and exact match is required (typically after ...)
 *  otherwise partial matching is performed.
 *
 *  Examples:
 *
 *	psmatch("aaa", "aaa", 0) -> 1
 *	psmatch("aaa", "aa", 0) -> 1
 *	psmatch("aa", "aaa", 0) -> 0
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"


/* used in subscript.c and subassign.c */
Rboolean NonNullStringMatch(SEXP s, SEXP t)
{
    /* "" or NA string matches nothing */
    if (s == NA_STRING || t == NA_STRING) return FALSE;
    if (CHAR(s)[0] && CHAR(t)[0] && Seql(s, t))
	return TRUE;
    else
	return FALSE;
}

/* currently unused outside this file */
Rboolean psmatch(const char *f, const char *t, Rboolean exact)
{
    if (exact)
	return (Rboolean)!strcmp(f, t);
    /* else */
    while (*t) {
	if (*t != *f)   return FALSE;
	t++;
	f++;
    }
    return TRUE;
}


/* Matching formals and arguments */

static R_INLINE SEXP charFromSexp(SEXP s)
{
    switch (TYPEOF(s)) {
    case SYMSXP:
	return PRINTNAME(s);
    case CHARSXP:
	return s;
    case STRSXP:
	if (LENGTH(s) == 1)
	    return STRING_ELT(s, 0);
	/* fall back to error */
    default:
	error(_("invalid partial string match"));
    }
}

Rboolean pmatch(SEXP formal, SEXP tag, Rboolean exact)
{
    SEXP f = charFromSexp(formal);
    SEXP t = charFromSexp(tag);
    cetype_t fenc = getCharCE(f);
    cetype_t tenc = getCharCE(t);

    if (fenc == tenc)
	return psmatch(CHAR(f), CHAR(t), exact);
    else {
	const void *vmax = vmaxget();
	Rboolean res = psmatch(translateCharUTF8(f), translateCharUTF8(t),
	                       exact);
	vmaxset(vmax);
	return res;
    }
}


/* Destructively Extract A Named List Element. */
/* Returns the first partially matching tag found. */
/* Pattern is a C string. */

static SEXP matchPar_int(const char *tag, SEXP *list, Rboolean exact)
{
    if (*list == R_NilValue)
	return R_MissingArg;
    else if (TAG(*list) != R_NilValue &&
	     psmatch(tag, CHAR(PRINTNAME(TAG(*list))), exact)) {
	SEXP s = *list;
	*list = CDR(*list);
	return CAR(s);
    }
    else {
	SEXP last = *list;
	SEXP next = CDR(*list);
	while (next != R_NilValue) {
	    if (TAG(next) != R_NilValue &&
		psmatch(tag, CHAR(PRINTNAME(TAG(next))), exact)) {
		SETCDR(last, CDR(next));
		return CAR(next);
	    }
	    else {
		last = next;
		next = CDR(next);
	    }
	}
	return R_MissingArg;
    }
}

/* unused outside this file */
SEXP attribute_hidden matchPar(const char *tag, SEXP * list)
{
    return matchPar_int(tag, list, FALSE);
}



/* Destructively Extract A Named List Element. */
/* Returns the first partially matching tag found. */
/* Pattern is a symbol. */

SEXP attribute_hidden matchArg(SEXP tag, SEXP * list)
{
    return matchPar(CHAR(PRINTNAME(tag)), list);
}


/* Destructively Extract A Named List Element. */
/* Returns the first exactly matching tag found. */
/* Pattern is a symbol. */

SEXP attribute_hidden matchArgExact(SEXP tag, SEXP * list)
{
      return matchPar_int(CHAR(PRINTNAME(tag)), list, TRUE);
}


/* Match the supplied arguments with the formals and */
/* return the matched arguments in actuals. */

#define ARGUSED(x) LEVELS(x)
#define SET_ARGUSED(x,v) SETLEVELS(x,v)


/* We need to leave 'supplied' unchanged in case we call UseMethod */
/* MULTIPLE_MATCHES was added by RI in Jan 2005 but never activated:
   code in R-2-8-branch */
/* Renamed to matchArgs_NR to reflect that it returns a
   non-reference-tracking list */

SEXP attribute_hidden matchArgs_NR(SEXP formals, SEXP supplied, SEXP call)
{
    Rboolean seendots;
    int i, arg_i = 0;
    SEXP f, a, b, dots, actuals;

    actuals = R_NilValue;
    for (f = formals ; f != R_NilValue ; f = CDR(f), arg_i++) {
	/* CONS_NR is used since argument lists created here are only
	   used internally and so should not increment reference
	   counts */
	actuals = CONS_NR(R_MissingArg, actuals);
	SET_MISSING(actuals, 1);
    }
    /* We use fargused instead of ARGUSED/SET_ARGUSED on elements of
       formals to avoid modification of the formals SEXPs.  A gc can
       cause matchArgs_NR to be called from finalizer code, resulting in
       another matchArgs_NR call with the same formals.  In R-2.10.x, this
       corrupted the ARGUSED data of the formals and resulted in an
       incorrect "formal argument 'foo' matched by multiple actual
       arguments" error.
     */
    int fargused[arg_i ? arg_i : 1]; // avoid undefined behaviour
    memset(fargused, 0, sizeof(fargused));

    for(b = supplied; b != R_NilValue; b = CDR(b)) SET_ARGUSED(b, 0);

    PROTECT(actuals);

    /* First pass: exact matches by tag */
    /* Grab matched arguments and check */
    /* for multiple exact matches. */

    f = formals;
    a = actuals;
    arg_i = 0;
    while (f != R_NilValue) {
      SEXP ftag = TAG(f);
      const char *ftag_name = CHAR(PRINTNAME(ftag));
      if (ftag != R_DotsSymbol && ftag != R_NilValue) {
	    for (b = supplied, i = 1; b != R_NilValue; b = CDR(b), i++) {
	      SEXP btag = TAG(b);
	      if (btag != R_NilValue) {
		  const char *btag_name = CHAR(PRINTNAME(btag));
		  if (streql( ftag_name, btag_name )) {
		      if (fargused[arg_i] == 2)
			  errorcall(call,
	                      _("formal argument \"%s\" matched by multiple actual arguments"),
	                      CHAR(PRINTNAME(TAG(f))));
		      if (ARGUSED(b) == 2)
			  errorcall(call,
	                      _("argument %d matches multiple formal arguments"),
                              i);
		      SETCAR(a, CAR(b));
		      if(CAR(b) != R_MissingArg) SET_MISSING(a, 0);
		      SET_ARGUSED(b, 2);
		      fargused[arg_i] = 2;
		  }
	      }
	    }
	}
	f = CDR(f);
	a = CDR(a);
        arg_i++;
    }

    /* Second pass: partial matches based on tags */
    /* An exact match is required after first ... */
    /* The location of the first ... is saved in "dots" */

    dots = R_NilValue;
    seendots = FALSE;
    f = formals;
    a = actuals;
    arg_i = 0;
    while (f != R_NilValue) {
	if (fargused[arg_i] == 0) {
	    if (TAG(f) == R_DotsSymbol && !seendots) {
		/* Record where ... value goes */
		dots = a;
		seendots = TRUE;
	    } else {
		for (b = supplied, i = 1; b != R_NilValue; b = CDR(b), i++) {
		    if (ARGUSED(b) != 2 && TAG(b) != R_NilValue &&
			pmatch(TAG(f), TAG(b), seendots)) {
			if (ARGUSED(b))
			    errorcall(call,
				_("argument %d matches multiple formal arguments"), i);
			if (fargused[arg_i] == 1)
			    errorcall(call,
				_("formal argument \"%s\" matched by multiple actual arguments"),
				CHAR(PRINTNAME(TAG(f))));
			if (R_warn_partial_match_args) {
			    warningcall(call,
					_("partial argument match of '%s' to '%s'"),
					CHAR(PRINTNAME(TAG(b))),
					CHAR(PRINTNAME(TAG(f))) );
			}
			SETCAR(a, CAR(b));
			if (CAR(b) != R_MissingArg) SET_MISSING(a, 0);
			SET_ARGUSED(b, 1);
			fargused[arg_i] = 1;
		    }
		}
	    }
	}
	f = CDR(f);
	a = CDR(a);
        arg_i++;
    }

    /* Third pass: matches based on order */
    /* All args specified in tag=value form */
    /* have now been matched.  If we find ... */
    /* we gobble up all the remaining args. */
    /* Otherwise we bind untagged values in */
    /* order to any unmatched formals. */

    f = formals;
    a = actuals;
    b = supplied;
    seendots = FALSE;

    while (f != R_NilValue && b != R_NilValue && !seendots) {
	if (TAG(f) == R_DotsSymbol) {
	    /* Skip ... matching until all tags done */
	    seendots = TRUE;
	    f = CDR(f);
	    a = CDR(a);
	} else if (CAR(a) != R_MissingArg) {
	    /* Already matched by tag */
	    /* skip to next formal */
	    f = CDR(f);
	    a = CDR(a);
	} else if (ARGUSED(b) || TAG(b) != R_NilValue) {
	    /* This value used or tagged , skip to next value */
	    /* The second test above is needed because we */
	    /* shouldn't consider tagged values for positional */
	    /* matches. */
	    /* The formal being considered remains the same */
	    b = CDR(b);
	} else {
	    /* We have a positional match */
	    SETCAR(a, CAR(b));
	    if(CAR(b) != R_MissingArg) SET_MISSING(a, 0);
	    SET_ARGUSED(b, 1);
	    b = CDR(b);
	    f = CDR(f);
	    a = CDR(a);
	}
    }

    if (dots != R_NilValue) {
	/* Gobble up all unused actuals */
	SET_MISSING(dots, 0);
	i = 0;
	for(a = supplied; a != R_NilValue ; a = CDR(a)) if(!ARGUSED(a)) i++;

	if (i) {
	    a = allocList(i);
	    SET_TYPEOF(a, DOTSXP);
	    f = a;
	    for(b = supplied; b != R_NilValue; b = CDR(b))
		if(!ARGUSED(b)) {
		    SETCAR(f, CAR(b));
		    SET_TAG(f, TAG(b));
		    f = CDR(f);
		}
	    SETCAR(dots, a);
	}
    } else {
	/* Check that all arguments are used */
	SEXP unused = R_NilValue, last = R_NilValue;
	for (b = supplied; b != R_NilValue; b = CDR(b))
	    if (!ARGUSED(b)) {
		if(last == R_NilValue) {
		    PROTECT(unused = CONS(CAR(b), R_NilValue));
		    SET_TAG(unused, TAG(b));
		    last = unused;
		} else {
		    SETCDR(last, CONS(CAR(b), R_NilValue));
		    last = CDR(last);
		    SET_TAG(last, TAG(b));
		}
	    }

	if(last != R_NilValue) {
            /* show bad arguments in call without evaluating them */
            SEXP unusedForError = R_NilValue, last = R_NilValue;

            for(b = unused ; b != R_NilValue ; b = CDR(b)) {
                SEXP tagB = TAG(b), carB = CAR(b) ;
                if (TYPEOF(carB) == PROMSXP) carB = PREXPR(carB) ;
                if (last == R_NilValue) {
                    PROTECT(last = CONS(carB, R_NilValue));
                    SET_TAG(last, tagB);
                    unusedForError = last;
                } else {
                    SETCDR(last, CONS(carB, R_NilValue));
                    last = CDR(last);
                    SET_TAG(last, tagB);
                }
            }
	    errorcall(call /* R_GlobalContext->call */,
		      ngettext("unused argument %s",
			       "unused arguments %s",
			       (unsigned long) length(unusedForError)),
		      strchr(CHAR(asChar(deparse1line(unusedForError, 0))), '('));
	}
    }
    UNPROTECT(1);
    return(actuals);
}

/* Use matchArgs_RC if the result might escape into R. */
SEXP attribute_hidden matchArgs_RC(SEXP formals, SEXP supplied, SEXP call)
{
    SEXP args = matchArgs_NR(formals, supplied, call);
    /* it would be better not to build this arglist with CONS_NR in
       the first place */
    for (SEXP a = args; a  != R_NilValue; a = CDR(a)) {
	if (! TRACKREFS(a)) {
	    ENABLE_REFCNT(a);
	    INCREMENT_REFCNT(CAR(a));
	    INCREMENT_REFCNT(CDR(a));
	}
    }
    return args;
}


/* patchArgsByActuals - patch promargs (given as 'supplied') to be promises
   for the respective actuals in the given environment 'cloenv'.  This is
   used by NextMethod to allow patching of arguments to the current closure
   before dispatching to the next method.  The implementation is based on
   matchArgs_NR, but there is no error/warning checking, assuming that it has
   already been done by a call to matchArgs_NR when the current closure was
   invoked.
*/

typedef enum {
    FS_UNMATCHED       = 0, /* the formal was not matched by any supplied arg */
    FS_MATCHED_PRESENT = 1, /* the formal was matched by a non-missing arg */
    FS_MATCHED_MISSING = 2, /* the formal was matched, but by a missing arg */
    FS_MATCHED_LOCAL   = 3, /* the formal was matched by a missing arg, but
                               a local variable of the same name as the formal
                               has been used */
} fstype_t;

static R_INLINE
void patchArgument(SEXP suppliedSlot, SEXP name, fstype_t *farg, SEXP cloenv) {
    SEXP value = CAR(suppliedSlot);
    if (value == R_MissingArg) {
        value = findVarInFrame3(cloenv, name, TRUE);
        if (value == R_MissingArg) {
            if (farg) *farg = FS_MATCHED_MISSING;
            return;
        }
        if (farg) *farg = FS_MATCHED_LOCAL;
    } else
        if (farg) *farg = FS_MATCHED_PRESENT;

    SETCAR(suppliedSlot, mkPROMISE(name, cloenv));
}

SEXP attribute_hidden
patchArgsByActuals(SEXP formals, SEXP supplied, SEXP cloenv)
{
    int i, seendots, farg_i;
    SEXP f, a, b, prsupplied;

    int nfarg = length(formals);
    if (!nfarg) nfarg = 1; // avoid undefined behaviour
    fstype_t farg[nfarg];
    for(i = 0; i < nfarg; i++) farg[i] = FS_UNMATCHED;

    /* Shallow-duplicate supplied arguments */

    PROTECT(prsupplied = allocList(length(supplied)));
    for(b = supplied, a = prsupplied; b != R_NilValue; b = CDR(b), a = CDR(a)) {
        SETCAR(a, CAR(b));
        SET_ARGUSED(a, 0);
        SET_TAG(a, TAG(b));
    }

    /* First pass: exact matches by tag */

    f = formals;
    farg_i = 0;
    while (f != R_NilValue) {
	if (TAG(f) != R_DotsSymbol) {
	    for (b = prsupplied; b != R_NilValue; b = CDR(b)) {
		if (TAG(b) != R_NilValue && pmatch(TAG(f), TAG(b), 1)) {
		    patchArgument(b, TAG(f), &farg[farg_i], cloenv);
		    SET_ARGUSED(b, 2);
		    break; /* Previous invocation of matchArgs_NR */
		           /* ensured unique matches */
		}
	    }
	}
	f = CDR(f);
	farg_i++;
    }

    /* Second pass: partial matches based on tags */
    /* An exact match is required after first ... */
    /* The location of the first ... is saved in "dots" */

    seendots = 0;
    f = formals;
    farg_i = 0;
    while (f != R_NilValue) {
	if (farg[farg_i] == FS_UNMATCHED) {
	    if (TAG(f) == R_DotsSymbol && !seendots) {
		seendots = 1;
	    } else {
		for (b = prsupplied; b != R_NilValue; b = CDR(b)) {
		    if (!ARGUSED(b) && TAG(b) != R_NilValue &&
			pmatch(TAG(f), TAG(b), seendots)) {

			patchArgument(b, TAG(f), &farg[farg_i], cloenv);
			SET_ARGUSED(b, 1);
			break; /* Previous invocation of matchArgs_NR */
			       /* ensured unique matches */
		    }
		}
	    }
	}
	f = CDR(f);
	farg_i++;
    }

    /* Third pass: matches based on order */
    /* All args specified in tag=value form */
    /* have now been matched.  If we find ... */
    /* we gobble up all the remaining args. */
    /* Otherwise we bind untagged values in */
    /* order to any unmatched formals. */

    f = formals;
    b = prsupplied;
    farg_i = 0;
    while (f != R_NilValue && b != R_NilValue) {
	if (TAG(f) == R_DotsSymbol) {
	    /* Done, ... and following args cannot be patched */
	    break;
	} else if (farg[farg_i] == FS_MATCHED_PRESENT) {
	    /* Note that this check corresponds to CAR(b) == R_MissingArg */
	    /* in matchArgs_NR */

	    /* Already matched by tag */
	    /* skip to next formal */
	    f = CDR(f);
	    farg_i++;
	} else if (ARGUSED(b) || TAG(b) != R_NilValue) {
	    /* This value is used or tagged, skip to next value */
	    /* The second test above is needed because we */
	    /* shouldn't consider tagged values for positional */
	    /* matches. */
	    /* The formal being considered remains the same */
	    b = CDR(b);
	} else {
	    /* We have a positional match */
	    if (farg[farg_i] == FS_MATCHED_LOCAL)
	        /* Another supplied argument, a missing with a tag, has */
	        /* been patched to a promise reading this formal, because */
	        /* there was a local variable of that name. Hence, we have */
	        /* to turn this supplied argument into a missing. */
	        /* Otherwise, we would supply a value twice, confusing */
	        /* argument matching in subsequently called functions. */
	        SETCAR(b, R_MissingArg);
	    else
	        patchArgument(b, TAG(f), NULL, cloenv);

	    SET_ARGUSED(b, 1);
	    b = CDR(b);
	    f = CDR(f);
	    farg_i++;
	}
    }

    /* Previous invocation of matchArgs_NR ensured all args are used */
    UNPROTECT(1);
    return(prsupplied);
}
