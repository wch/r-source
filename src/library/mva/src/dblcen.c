#include <math.h>
#include "mva.h"

/* Double Centering for Classical Multidimensional Scaling */

void dblcen(double *a, int *na)
{
    double sum;
    int n, i, j;

    n = *na;
    for(i=0 ; i<n ; i++) {
	sum = 0;
	for(j=0 ; j<n ; j++)
	    sum += a[i+j*n];
	sum /= n;
	for(j=0 ; j<n ; j++)
	    a[i+j*n] -= sum;
    }
    for(j=0 ; j<n ; j++) {
	sum = 0;
	for(i=0 ; i<n ; i++)
	    sum += a[i+j*n];
	sum /= n;
	for(i=0 ; i<n ; i++)
	    a[i+j*n] -= sum;
    }
}
