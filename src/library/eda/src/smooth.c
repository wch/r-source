/* Tukey Median Smoothing */

#include <math.h>

static double amed3(double u, double v, double w)
{
    /* Median(u,v,w): */
    if(u <= v && v <= w) return v;
    if(u >= v && v >= w) return v;
    if(u <= w && w <= v) return w;
    if(u >= w && w >= v) return w;
    return u;
}
/* Note: Velleman & Hoaglin use a smarter version,  which returns "change"
   ----
   and change = TRUE, when  med3(u,v,w) != v   ==> makes "R" much faster
*/

static double absdiff(double *x, double *y, int n)
{
    /* || x - y ||_{L1} */
    int i;
    double sum = 0;
    for(i = 0; i < n; i++)
	sum += fabs(x[i]-y[i]);
    return sum;
}


static void a3(double *x, double *y, int n, double *d)
{
    /* y[] := Running Median of three (x) with Tukey's end rule */
    int i;

/* MM: Two errors :
   ---
   1) The end rule should only be applied once; typically after "3R"
      but not after every "3" !!
   2) The end rule is done wrongly here:
      it should use y[1] and y[n-2] (smoothed) instead of  x[1] and x[n-2]
*/
    y[0] = amed3(x[0], x[1], 3*x[1] - 2*x[2]);
    for(i = 1; i < n-1; i++)
	y[i] = amed3(x[i-1], x[i], x[i+1]);
    y[n-1] = amed3(x[n-1], x[n-2], 3*x[n-2] - 2*x[n-3]);
    *d = absdiff(x,y,n);
}


static void a3r(double *x, double *y, double *z, int n, double *d)
{
    /* y[] := "3R"(x) ; 3R = Median of three, repeated until convergence */
    int i;
    a3(x, y, n, d);
    while(*d > 0.0) {
	a3(y, z, n, d);
	for(i=0 ; i<n ; i++)
	    y[i] = z[i];
    }
    *d = absdiff(x, y, n);
}


static int sptest(double *x, int i)
{
    /* Split test:
       Are we at a /-\ or \_/ location => split should be made ?
     */
    if(x[i] != x[i+1]) return 0;
    if(x[i-1] <= x[i] && x[i+1] <= x[i+2]) return 0;
    if(x[i-1] >= x[i] && x[i+1] >= x[i+2]) return 0;
    return 1;
}


static void split(double *x, double *y, int n, double *d)
{
    /* y[] := S(x[])  where S() = "split"  */
    int i;

    for(i=0 ; i<n ; i++)
	y[i] = x[i];

    if( sptest(x, 1) ) {
	y[1] = x[0];
	y[2] = amed3(x[2], x[3], 3.0*x[3]-2*x[4]);
    }
    for(i=2 ; i<n-3 ; i++)
	if(sptest(x, i)) {
	    y[i]   = amed3(x[i  ], x[i-1], 3*x[i-1] - 2*x[i-2]);
	    y[i+1] = amed3(x[i+1], x[i+2], 3*x[i+2] - 2*x[i+3]);
	}
    if(sptest(x, n-3)) {
	y[n-2] = x[n-1];
	y[n-3] = amed3(x[n-3], x[n-4], 3*x[n-4]-2*x[n-5]);
    }
    *d = absdiff(x, y, n);
}

static void a3rsr(double *x, double *y, double *z, double *w, int n)
{
    /* y[1:n] := "3RSR"(x[1:n]);  z := residuals; w = "work"; */
    int i;
    double d;

    a3r(x, y, z, n, &d);
    do {
	split(y, z, n, &d);
	a3r(z, y, w, n, &d);
	/* BUG:  d = || z - y ||, but should be  || y.orig - y ||  !! */

	/* shortcut:  if(d == 0.) return; */
	for(i=0 ; i<n ; i++)
	    z[i] = x[i] - y[i];
    }
    while (d > 0.);
}

void tukeysmooth(double *x, double *y, double *z, double *w, int *n)
{
    a3rsr(x, y, z, w, *n);
    return;
}
