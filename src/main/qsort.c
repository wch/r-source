
/* **********************************************************************
 * === This was 'sort()' in  gamfit's  mysort.f  [or sortdi() in sortdi.f ] :
 * was at end of  modreg/src/ppr.f
 * Translated by f2c (version 20010821) and f2c-clean,v 1.9 2000/01/13 13:46:53
 * then manually by Martin Maechler
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h> /* => Utils.h with the C protos from here */
#include <R_ext/Applic.h> /* F77_.. protos from here */
#include <Rmath.h>

#include <R_ext/RS.h>


/* R function  qsort(x, index.return) */
SEXP do_qsort(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, sx;
    int indx_ret, n;
    double *vx = NULL;
    int *ivx = NULL;
    Rboolean x_real, x_int;

    checkArity(op, args);
    x = CAR(args);
    if (!isNumeric(x))
	errorcall(call, "Argument is not a numeric vector");
    x_real= TYPEOF(x) == REALSXP;
    x_int = !x_real && (TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP);
    PROTECT(sx = (x_real || x_int) ? duplicate(x) : coerceVector(x, REALSXP));
    /* if x has names, drop them, since they won't be ordered : */
    if (!isNull(getAttrib(sx, R_NamesSymbol)))
	setAttrib(sx, R_NamesSymbol, R_NilValue);
    indx_ret = asLogical(CADR(args));
    n = LENGTH(x);
    if(x_int) ivx = INTEGER(sx); else vx = REAL(sx);
    if(indx_ret) {
	SEXP ans, ansnames, indx;
	int i, *ix;
	/* answer will have x = sorted x , ix = index :*/
	PROTECT(ans      = allocVector(VECSXP, 2));
	PROTECT(ansnames = allocVector(STRSXP, 2));
	PROTECT(indx = allocVector(INTSXP, n));
	ix = INTEGER(indx);
	for(i = 0; i < n; i++)
	    ix[i] = i+1;

	if(x_int)
	    R_qsort_int_I(ivx, ix, 1, n);
	else
	    R_qsort_I(vx, ix, 1, n);

	SET_VECTOR_ELT(ans, 0, sx);
	SET_VECTOR_ELT(ans, 1, indx);
	SET_VECTOR_ELT(ansnames, 0, mkChar("x"));
	SET_VECTOR_ELT(ansnames, 1, mkChar("ix"));
	setAttrib(ans, R_NamesSymbol, ansnames);
	UNPROTECT(4);
	return ans;
    }
    else {
	if(x_int)
	    R_qsort_int(ivx, 1, n);
	else
	    R_qsort(vx, 1, n);

	UNPROTECT(1);
	return sx;
    }
}


/* ORIGINALLY: Fortran a() was double precision -- for scratch space convenience
 * ---> change and change in modreg's calling function!!
 */
int F77_NAME(qsort4)(double *v, int *indx, int *ii, int *jj)
{
    R_qsort_I(v, indx, *ii, *jj);
    return 0;
}
int F77_NAME(qsort3)(double *v, int *ii, int *jj)
{
    R_qsort(v, *ii, *jj);
    return 0;
}


#define qsort_Index
#define NUMERIC double
void R_qsort_I(double *v, int *I, int i, int j)
#include "qsort-body.c"
#undef NUMERIC

#define NUMERIC int
void R_qsort_int_I(int *v, int *I, int i, int j)
#include "qsort-body.c"
#undef NUMERIC

#undef qsort_Index

#define NUMERIC double
void R_qsort(double *v, int i, int j)
#include "qsort-body.c"
#undef NUMERIC

#define NUMERIC int
void R_qsort_int(int *v, int i, int j)
#include "qsort-body.c"
#undef NUMERIC

/* Local variables:
 * mode: c
 * kept-old-versions: 12
 * kept-new-versions: 20
 * End:
 */
