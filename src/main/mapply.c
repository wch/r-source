
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "R.h"
#include "Rinternals.h"


SEXP do_mapply(SEXP f, SEXP varyingArgs, SEXP constantArgs, SEXP rho)
{

    int i, j, m,nc, *lengths, *counters, named, longest=0;
    SEXP vnames, fcall,  mindex, nindex, tmp1, tmp2, ans;

    m = length(varyingArgs);
    nc = length(constantArgs);
    vnames = PROTECT(getAttrib(varyingArgs, R_NamesSymbol));

    named = vnames!=R_NilValue;

    lengths = (int *)  R_alloc(m, sizeof(int));
    for(i = 0; i < m; i++){
	lengths[i] = length(VECTOR_ELT(varyingArgs,i));
	if (lengths[i] > longest) longest=lengths[i];
    }


    counters = (int *) R_alloc(m, sizeof(int));
    for(i = 0; i < m; counters[i++]=0);

    mindex=PROTECT(allocVector(VECSXP, m));
    nindex=PROTECT(allocVector(VECSXP, m));

    /* build a call
       f(dots[[1]][[4]],dots[[2]][[4]],dots[[3]][[4]],d=7)
    */

    if (constantArgs == R_NilValue)
	PROTECT(fcall=R_NilValue);
    else
	PROTECT(fcall=VectorToPairList(constantArgs));

    for(j = m-1; j >= 0;j--) {
	SET_VECTOR_ELT(mindex,j, allocVector(INTSXP,1));
	SET_VECTOR_ELT(nindex,j, allocVector(INTSXP,1));
	INTEGER(VECTOR_ELT(mindex,j))[0] = j+1;

	PROTECT(tmp1=lang3(R_Bracket2Symbol,
			   install("dots"),
			   VECTOR_ELT(mindex,j)));

	PROTECT(tmp2=lang3(R_Bracket2Symbol,
			   tmp1,
			   VECTOR_ELT(nindex,j)));


	UNPROTECT(3);
	PROTECT(fcall=LCONS(tmp2, fcall));

	if (named && CHAR(STRING_ELT(vnames,j))[0]!='\0')
	    SET_TAG(fcall, install(CHAR(STRING_ELT(vnames,j))));

    }
    UNPROTECT(1);
    PROTECT(fcall=LCONS(f, fcall));

    PROTECT(ans=allocVector(VECSXP, longest));

    for(i = 0; i < longest; i++) {
	for(j = 0; j < m; j++) {
	    counters[j] = (++counters[j]>lengths[j]) ? 1 : counters[j];
	    INTEGER(VECTOR_ELT(nindex,j))[0] = counters[j];
	}
	SET_VECTOR_ELT(ans, i, eval(fcall, rho));
    }

    for(j = 0; j < m; j++) {
	if (counters[j] != lengths[j])
	    warning("longer argument not a multiple of length of shorter");
    }

    UNPROTECT(5);

    return(ans);
}
