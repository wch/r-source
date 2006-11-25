/*
 *  R : A Computer Language for Statistical Data Analysis
 *
 *  Modified version of code from the Cephes Math Library
 *  Modifications Copyright (C) 2002 The R Development Core Team.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <math.h>
#include <R_ext/Arith.h>

/* based on
Cephes Math Library Release 2.8:  June, 2000
Copyright (C) 1987, 1995, 2000 by Stephen L. Moshier
*/

double polevl(double x, double *coef, int N)
{
    double *p = coef, ans;
    int i = N;

    ans = *p++;
    do
	ans = ans * x  +  *p++;
    while( --i );
    return( ans );
}

double p1evl(double x, double *coef, int N)
{
    double *p = coef, ans;
    int i = N - 1;

    ans = x + *p++;
    do
	ans = ans * x  + *p++;
    while( --i );
    return( ans );
}

static double P[] = {
-8.54074331929669305196E-1,
 1.20426861384072379242E1,
-4.61252884198732692637E1,
 6.54566728676544377376E1,
-3.09092539379866942570E1
};
static double Q[] = {
/* 1.00000000000000000000E0,*/
-1.95638849376911654834E1,
 1.08938092147140262656E2,
-2.49839401325893582852E2,
 2.52006675691344555838E2,
-9.27277618139601130017E1
};

double atanh(double x)
#ifdef __cplusplus
	throw ()
#endif
{
    double s, z;

    if(ISNAN(x)) return x;
    if( x == 0.0 ) return x;
    z = fabs(x);
    if( z >= 1.0 ) {
	if( x == 1.0 ) return R_PosInf;
	if( x == -1.0 ) return R_NegInf;
	return NA_REAL;
    }

    if( z < 1.0e-7 ) return x;

    if( z < 0.5 ) {
	z = x * x;
	s = x + x * z * (polevl(z, P, 4) / p1evl(z, Q, 5));
	return s;
    }

    return 0.5 * log((1.0+x)/(1.0-x));
}
