#include "S.h"

/* cor is the autocorrelations starting from 0 lag*/
void uni_pacf(double *cor, double *p, int *pn, int *pnlag)
{
    int n = *pn, nlag = *pnlag;
    int i, ll;
    double a, b, c, *v, *w;
    
    v = (double*) R_alloc(nlag, sizeof(double));
    w = (double*) R_alloc(nlag, sizeof(double));
    w[0] = p[0] = cor[1];
    for(ll = 1; ll < nlag; ll++) {
	a = cor[ll+1];
	b = 1.0;
	for(i = 0; i < ll; i++) {
	    a -= w[i] * cor[ll - i];
	    b -= w[i] * cor[i + 1];
	}
	p[ll] = c = a/b;
	if(ll+1 == nlag) break;
	w[ll] = c;
	for(i = 0; i < ll; i++)
	    v[ll-i-1] = w[i];
	for(i = 0; i < ll; i++)
	    w[i] -= c*v[i];
    }
}
