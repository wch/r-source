#include <Rmath.h>
#include <Rinternals.h>

SEXP R_cutree(SEXP merge, SEXP which)
{
    SEXP ans;
    int n, k, l, nclust, m1, m2, ok, anscol;
    int *x, *y, *y1;

    x = (int *) R_alloc(nrows(merge)+2, sizeof(int));
    y = (int *) R_alloc(nrows(merge)+2, sizeof(int));
    y1 = (int *) R_alloc(nrows(merge)+2, sizeof(int));

    merge = coerceVector(merge, INTSXP);
    which = coerceVector(which, INTSXP);

    n=nrows(merge)+1;
    PROTECT(ans = allocMatrix(INTSXP, n, LENGTH(which)));
	    
    for(k=1; k<=n; k++){
	x[k] = k;
	y[k] = 0;
    }

    for(k=1; k<=n-2; k++){
	m1 = INTEGER(merge)[k-1];
	m2 = INTEGER(merge)[n-1+k-1];

	if((m1 < 0) && (m2 < 0)){
	    y[-m1] = y[-m2] = k;
	    x[-m1] = x[-m2] = 0;
	}
	else if((m1 < 0) || (m2 < 0)){
	    for(l=1; l<=n; l++){
		if(y[l]==imax2(m1,m2)){
		    y[l]=k;
		}
	    }
	    y[-imin2(m1,m2)] = k;
	    x[-imin2(m1,m2)] = 0;
	}
	else{
	    for(l=1; l<=n; l++){
		if( (y[l]==m1) || (y[l]==m2) ){
		    y[l] = k;
		}
	    }
	}

	nclust=0;
	for(l=1; l<=n; l++){
	    y1[l] = 0;
	}

	ok = anscol = 0;
	while(!ok && (anscol < LENGTH(which))){
	    ok = ( k == n-INTEGER(which)[anscol++]);
	}
	
	if(ok){
	    for(l=1; l<=n; l++){
		if(x[l] > 0){
		    INTEGER(ans)[(anscol-1)*n + l-1] = ++nclust;
		}
		else{
		    if(y1[y[l]] == 0){
			y1[y[l]] = ++nclust;
		    }
		    INTEGER(ans)[(anscol-1)*n + l-1] = y1[y[l]];
		}
	    }
	}
    }


    UNPROTECT(1);
    return(ans);
}
