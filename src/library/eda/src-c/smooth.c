/* Tukey Median Smoothing */

#include <math.h>

static double amed3(double u, double v, double w)
{
	if(u <= v && v <= w) return v;
	if(u >= v && v >= w) return v;
	if(u <= w && w <= v) return w;
	if(u >= w && w >= v) return w;
	return u;
}

static double absdiff(double *x, double *y, int n)
{
	int i;
	double sum = 0;
	for(i=0 ; i<n ; i++)
		sum += fabs(x[i]-y[i]);
	return sum;
}


static void a3(double *x, double *y, int n, double *d)
{
	int i;

	y[0] = amed3(x[0], x[1], 3.0*x[1]-2.0*x[2]);
	for(i=1 ; i<n-1 ; i++)
		y[i] = amed3(x[i-1], x[i], x[i+1]);
	y[n-1] = amed3(x[n-1], x[n-2], 3.0*x[n-2]-2.0*x[n-3]);
	*d = absdiff(x,y,n);
}


static void a3r(double *x, double *y, double *z, int n, double *d)
{
	int i;

	a3(x, y, n, d);
	while(*d > 0.0) {
		a3(y, z, n, d);
		for(i=0 ; i<n ; i++)
			y[i] = z[i];
	}
	*d = absdiff(x, y, n);
}


static int sptest(double *x, int i, int j)
{
	if(x[i] != x[i+1]) return 0;
	if(x[j-1] <= x[j] && x[j+1] <= x[j+2]) return 0;
	if(x[j-1] >= x[j] && x[j+1] >= x[j+2]) return 0;
	return 1;
}


static void split(double *x, double *y, int n, double *d)
{
	int i;

	for(i=0 ; i<n ; i++)
		y[i] = x[i];

	if( sptest(x, 1, 1) ) {
		y[1] = x[0];
		y[2] = amed3(x[2], x[3], 3.0*x[3]-2*x[4]);
	}
	for(i=2 ; i<n-3 ; i++)
		if(sptest(x, i, i)) {
			y[i] = amed3(x[i], x[i-1], 3*x[i-1]-2*x[i-2]);
			y[i+1] = amed3(x[i+1], x[i+2], 3*x[i+2]-2*x[i+3]);
		}
	if(sptest(x, n-3, n-3)) {
		y[n-2] = x[n-1];
		y[n-3] = amed3(x[n-3], x[n-4], 3*x[n-4]-2*x[n-5]);
	}
	*d = absdiff(x, y, n);
}

static void a3rs(double *x, double *y, double *z, int n, double *d)
{
	a3r(x, z, y, n, d);
	split(z, y, n, d);
}


static void a3rsr(double *x, double *y, double *z, double *w, int n, double *d)
{
	int i;

	a3r(x, y, z, n, d);
	do {
		split(y, z, n, d);
		a3r(z, y, w, n, d);
		for(i=0 ; i<n ; i++)
			z[i] = x[i] - y[i];
	}
	while (*d > 0.0);
}

tukeysmooth(double *x, double *y, double *z, double *w, int *n, double *d)
{
	a3rsr(x, y, z, w, *n, d);
}
