#include "R_ext/Utils.h"/* R_rsort() */
#include <math.h>

static int il(int n, double x)
{
    return (int)floor((n - 1) * x);
}

static int iu(int n, double x)
{
    return (int)ceil((n - 1) * x);
}

static void line(double *x, double *y, double *z, double *w, int n, 
		 double *coef)
{
    int i, j, k;
    double xb, x1, x2, xt, yt, yb, tmp1, tmp2;
    double slope, yint;

    for(i = 0 ;  i< n ; i++) {
	z[i] = x[i];
	w[i] = y[i];
    }
    R_rsort(z, n);

    tmp1 = z[il(n, 1.0/6.0)];
    tmp2 = z[iu(n, 1.0/6.0)];
    xb = 0.5*(tmp1+tmp2);

    tmp1 = z[il(n, 2.0/6.0)];
    tmp2 = z[iu(n, 2.0/6.0)];
    x1 = 0.5*(tmp1+tmp2);

    tmp1 = z[il(n, 4.0/6.0)];
    tmp2 = z[iu(n, 4.0/6.0)];
    x2 = 0.5*(tmp1+tmp2);

    tmp1 = z[il(n, 5.0/6.0)];
    tmp2 = z[iu(n, 5.0/6.0)];
    xt = 0.5*(tmp1+tmp2);

    slope = 0.0;

    for(j = 1 ; j <= 1 ; j++) {
	k = 0;
	for( i = 0 ; i < n ; i++ )
	    if( x[i] <= x1 )
		z[k++] = w[i];
	R_rsort(z, k);
	yb = 0.5 * (z[il(k, 0.5)] + z[iu(k, 0.5)]);
	k = 0;
	for(i = 0 ; i < n ; i++)
	    if( x[i] >= x2 )
		z[k++] = w[i];
	R_rsort(z,k);
	yt = 0.5 * (z[il(k, 0.5)] + z[iu(k, 0.5)]);
	slope += (yt - yb)/(xt - xb);
	for(i = 0 ; i < n ; i++) {
	    z[i] = y[i]-slope*x[i];
	    w[i] = z[i];
	}
	R_rsort(z,n);
	yint = 0.5 * (z[il(n, 0.5)] + z[iu(n, 0.5)]);
    }
    for( i = 0 ; i < n ; i++ ) {
	w[i] = yint + slope*x[i];
	z[i] = y[i] - w[i];
    }
    coef[0] = yint;
    coef[1] = slope;
}

void tukeyline(double *x, double *y, double *z, double *w, int *n, 
	       double *coef)
{
    line(x, y, z, w, *n, coef);
}
