#include <math.h>

static line(double *x, double *y, double *z, double *w, int n, double *coef)
{
	int i, j, k;
	double xb, x1, x2, xt, yt, yb, tmp1, tmp2;
	double slope, yint;

	for(i=0 ; i<n ; i++) {
		z[i] = x[i];
		w[i] = y[i];
	}
	rsort(z, n);

	tmp1 = z[(int)(1*n/6.-0.01)];
	tmp2 = z[(int)(1*n/6.+0.01)];
	xb = 0.5*(tmp1+tmp2);

	tmp1 = z[(int)(2*n/6.-0.01)];
	tmp2 = z[(int)(2*n/6.+0.01)];
	x1 = 0.5*(tmp1+tmp2);

	tmp1 = z[(int)(4*n/6.-0.01)];
	tmp2 = z[(int)(4*n/6.+0.01)];
	x2 = 0.5*(tmp1+tmp2);

	tmp1 = z[(int)(5*n/6.-0.01)];
	tmp2 = z[(int)(5*n/6.+0.01)];
	xt = 0.5*(tmp1+tmp2);

	slope = 0.0;
	j = 0;
l2:     j++;
	k = 0;
	for( i=0 ; i<n ; i++ )
		if( x[i]<=x1 )
			z[k++] = w[i];
	rsort(z,k);
	yb = 0.5*(z[(int)(n/6.-0.01)]+z[(int)(n/6.+0.01)]);
	k = 0;
	for(i=0 ; i<n ; i++)
		if( x[i]>=x2 )
			z[k++] = w[i];
	rsort(z,k);
	yt = 0.5*(z[(int)(k-n/6.-0.01)]+z[(int)(k-n/6.+0.01)]);
	slope += (yt-yb)/(xt-xb);
	for(i=0 ; i<n ; i++) {
		z[i] = y[i]-slope*x[i];
		w[i] = z[i];
	}
	rsort(z,n);
	yint = 0.5*(z[(int)(n/2.-0.01)]+z[(int)(n/2.+0.01)]);
	if( j<2 ) {
		goto l2;
	}
	for( i=0 ; i<n ; i++ ) {
		w[i] = yint+slope*x[i];
		z[i] = y[i]-w[i];
	}
	coef[0] = yint;
	coef[1] = slope;
}

int tukeyline(double *x, double *y, double *z, double *w, int *n, double *coef)
{
	line(x, y, z, w, *n, coef);
}
