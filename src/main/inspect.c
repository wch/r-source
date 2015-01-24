/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2009-2014 The R Core Team.
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
 */

/* This is an experimental facility for printing low-level information
   about R objects. It is not intended to be exposed at the top level
   but rather used as a debugging/inspection facility. It is not
   necessarily complete - feel free to add missing pieces. */

#define USE_RINTERNALS

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <R_ext/Print.h>

/* FIXME: envir.c keeps this private - it should probably go to Defn.h */
#define FRAME_LOCK_MASK (1<<14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define GLOBAL_FRAME_MASK (1<<15)
#define IS_GLOBAL_FRAME(e) (ENVFLAGS(e) & GLOBAL_FRAME_MASK)

/* based on EncodeEnvironment in  printutils.c */
static void PrintEnvironment(SEXP x)
{
    const void *vmax = vmaxget();
    if (x == R_GlobalEnv)
	Rprintf("<R_GlobalEnv>");
    else if (x == R_BaseEnv)
	Rprintf("<base>");
    else if (x == R_EmptyEnv)
	Rprintf("<R_EmptyEnv>");
    else if (R_IsPackageEnv(x))
	Rprintf("<%s>",
		translateChar(STRING_ELT(R_PackageEnvName(x), 0)));
    else if (R_IsNamespaceEnv(x))
	Rprintf("<namespace:%s>",
		translateChar(STRING_ELT(R_NamespaceEnvSpec(x), 0)));
    else Rprintf("<%p>", (void *)x);
    vmaxset(vmax);
}

/* print prefix */
static void pp(int pre) {
    /* this is sort of silly, I know, but it saves at least some output
       calls (and we can replace \t by spaces if desired) ... */
    while (pre >= 8) { Rprintf("\t"); pre -= 8; }
    while (pre-- > 0) Rprintf(" ");
}

static const char *typename(SEXP v) {
    return sexptype2char(TYPEOF(v)); // -> memory.c
}

/* pre is the prefix, v is the object to inspect, deep specifies
   the recursion behavior (0 = no recursion, -1 = [sort of] unlimited
   recursion, positive numbers define the maximum recursion depth)
   and pvec is the max. number of vector elements to show  */
static void inspect_tree(int pre, SEXP v, int deep, int pvec) {
    int a = 0;
    pp(pre);
    /* the use of %lx is deliberate because I hate the output of %p,
       but if this causes portability issues, it could be changed.
       SU

       It is invalid on 64-bit Windows.
    */
#ifdef _WIN64
    Rprintf("@%p %02d %s g%dc%d [", v, TYPEOF(v), typename(v), 
	    v->sxpinfo.gcgen, v->sxpinfo.gccls);
#else
    Rprintf("@%lx %02d %s g%dc%d [", (long) v, TYPEOF(v), typename(v), 
	    v->sxpinfo.gcgen, v->sxpinfo.gccls);
#endif
    if (OBJECT(v)) { a = 1; Rprintf("OBJ"); }
    if (MARK(v)) { if (a) Rprintf(","); Rprintf("MARK"); a = 1; }
#ifndef SWITCH_TO_REFCNT
    if (NAMED(v)) { if (a) Rprintf(","); Rprintf("NAM(%d)",NAMED(v)); a = 1; }
#endif
    if (REFCNT(v)) { if (a) Rprintf(","); Rprintf("REF(%d)",REFCNT(v)); a = 1; }
    if (RDEBUG(v)) { if (a) Rprintf(","); Rprintf("DBG"); a = 1; }
    if (RTRACE(v)) { if (a) Rprintf(","); Rprintf("TR"); a = 1; }
    if (RSTEP(v)) { if (a) Rprintf(","); Rprintf("STP"); a = 1; }
    if (IS_S4_OBJECT(v)) { if (a) Rprintf(","); Rprintf("S4"); a = 1; }
    if (TYPEOF(v) == SYMSXP || TYPEOF(v) == LISTSXP) {
	if (IS_ACTIVE_BINDING(v)) { if (a) Rprintf(","); Rprintf("AB"); a = 1; }
	if (BINDING_IS_LOCKED(v)) { if (a) Rprintf(","); Rprintf("LCK"); a = 1; }
    }    
    if (TYPEOF(v) == ENVSXP) {
        if (FRAME_IS_LOCKED(v)) { if (a) Rprintf(","); Rprintf("LCK"); a = 1; }
	if (IS_GLOBAL_FRAME(v)) { if (a) Rprintf(","); Rprintf("GL"); a = 1; }
    }
    if (LEVELS(v)) { if (a) Rprintf(","); Rprintf("gp=0x%x", LEVELS(v)); a = 1; }
    if (ATTRIB(v) && ATTRIB(v) != R_NilValue) { if (a) Rprintf(","); Rprintf("ATT"); a = 1; }
    Rprintf("] ");
    switch (TYPEOF(v)) {
    case VECSXP: case STRSXP: case LGLSXP: case INTSXP: case RAWSXP:
    case REALSXP: case CPLXSXP: case EXPRSXP:
	Rprintf("(len=%ld, tl=%ld)", XLENGTH(v), XTRUELENGTH(v));
    }
    if (TYPEOF(v) == ENVSXP) /* NOTE: this is not a trivial OP since it involves looking up things
				in the environment, so for a low-level debugging we may want to
				avoid it .. */
        PrintEnvironment(v);
    if (TYPEOF(v) == CHARSXP) {
	if (IS_BYTES(v)) Rprintf("[bytes] ");
	if (IS_LATIN1(v)) Rprintf("[latin1] ");
	if (IS_UTF8(v)) Rprintf("[UTF8] ");
	if (IS_ASCII(v)) Rprintf("[ASCII] ");
	if (IS_CACHED(v)) Rprintf("[cached] ");
	Rprintf("\"%s\"", CHAR(v));
    }
    if (TYPEOF(v) == SYMSXP)
	Rprintf("\"%s\"%s", EncodeChar(PRINTNAME(v)), (SYMVALUE(v) == R_UnboundValue) ? "" : " (has value)");
    switch (TYPEOF(v)) { /* for native vectors print the first elements in-line */
    case LGLSXP:
	if (XLENGTH(v) > 0) {
		unsigned int i = 0;
		while (i < XLENGTH(v) && i < pvec) {
		    Rprintf("%s%d", (i > 0) ? "," : " ", (int) LOGICAL(v)[i]);
		    i++;
		}
		if (i < XLENGTH(v)) Rprintf(",...");
	}
	break;
    case INTSXP:
	if (XLENGTH(v) > 0) {
	    unsigned int i = 0;
	    while (i < XLENGTH(v) && i < pvec) {
		Rprintf("%s%d", (i > 0) ? "," : " ", INTEGER(v)[i]);
		i++;
	    }
	    if (i < XLENGTH(v)) Rprintf(",...");
	}
	break;
    case RAWSXP:
	if (XLENGTH(v) > 0) {
	    unsigned int i = 0;
	    while (i < XLENGTH(v) && i < pvec) {
		Rprintf("%s%02x", (i > 0) ? "," : " ", (int) ((unsigned char) RAW(v)[i]));
		i++;
	    }
	    if (i < XLENGTH(v)) Rprintf(",...");
	}
	break;
    case REALSXP:
	if (XLENGTH(v) > 0) {
	    unsigned int i = 0;
	    while (i < XLENGTH(v) && i < pvec) {
		Rprintf("%s%g", (i > 0) ? "," : " ", REAL(v)[i]);
		i++;
	    }
	    if (i < XLENGTH(v)) Rprintf(",...");
	}
	break;
    }
    Rprintf("\n");
    if (deep) switch (TYPEOF(v)) {
	case VECSXP: case EXPRSXP:
	    {
		unsigned int i = 0;
		while (i < XLENGTH(v) && i < pvec) {
		    inspect_tree(pre+2, VECTOR_ELT(v, i), deep - 1, pvec);
		    i++;
		}
		if (i < XLENGTH(v)) { pp(pre+2); Rprintf("...\n"); }
	    }
	    break;
	case STRSXP:
	    {
		unsigned int i = 0;
		while (i < XLENGTH(v) && i < pvec) {
		    inspect_tree(pre+2, STRING_ELT(v, i), deep - 1, pvec);
		    i++;
		}
		if (i < XLENGTH(v)) { pp(pre+2); Rprintf("...\n"); }
	    }
	    break;
	case LISTSXP: case LANGSXP:
	    {
		SEXP lc = v;
		while (lc != R_NilValue) {
		    if (TAG(lc) && TAG(lc) != R_NilValue) {
			pp(pre + 2);
			Rprintf("TAG: "); /* TAG should be a one-liner since it's a symbol so we don't put it on an extra line*/
			inspect_tree(0, TAG(lc), deep - 1, pvec);
		    }		  
		    inspect_tree(pre + 2, CAR(lc), deep - 1, pvec);
		    lc = CDR(lc);
		}
	    }
	    break;
	case ENVSXP:
	    if (FRAME(v) != R_NilValue) {
		pp(pre); Rprintf("FRAME:\n");
		inspect_tree(pre+2, FRAME(v), deep - 1, pvec);
	    }
	    pp(pre); Rprintf("ENCLOS:\n");
	    inspect_tree(pre+2, ENCLOS(v), 0, pvec);
	    if (HASHTAB(v) != R_NilValue) {
		pp(pre); Rprintf("HASHTAB:\n");
		inspect_tree(pre+2, HASHTAB(v), deep - 1, pvec);
	    }
	    break;
	    
	case CLOSXP:
	    pp(pre); Rprintf("FORMALS:\n");
	    inspect_tree(pre+2, FORMALS(v), deep - 1, pvec);
	    pp(pre); Rprintf("BODY:\n");
	    inspect_tree(pre+2, BODY(v), deep - 1, pvec);
	    pp(pre); Rprintf("CLOENV:\n");
	    inspect_tree(pre+2, CLOENV(v), 0, pvec);
	    break;
	}
    
    if (ATTRIB(v) && ATTRIB(v) != R_NilValue && TYPEOF(v) != CHARSXP) {
	pp(pre); Rprintf("ATTRIB:\n"); inspect_tree(pre+2, ATTRIB(v), deep, pvec);
    }
}

/* internal API - takes one mandatory argument (object to inspect) and
   two optional arguments (deep and pvec - see above), positional argument
   matching only */
SEXP attribute_hidden do_inspect(SEXP call, SEXP op, SEXP args, SEXP env) {
    SEXP obj = CAR(args);
    int deep = -1;
    int pvec = 5;
    if (CDR(args) != R_NilValue) {
	deep = asInteger(CADR(args));
	if (CDDR(args) != R_NilValue)
	    pvec = asInteger(CADDR(args));
    }
	
    inspect_tree(0, CAR(args), deep, pvec);
    return obj;
}

SEXP attribute_hidden do_address(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_MakeExternalPtr((void *) CAR(args), R_NilValue, R_NilValue);
}

SEXP attribute_hidden do_refcnt(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return ScalarInteger(REFCNT(CAR(args)));
}

/* the following functions can be use internally and for debugging purposes -
   so far they are not used in any actual code */
SEXP attribute_hidden R_inspect(SEXP x) {
    inspect_tree(0, x, -1, 5);
    return x;
}

SEXP attribute_hidden R_inspect3(SEXP x, int deep, int pvec) {
    inspect_tree(0, x, deep, pvec);
    return x;
}
