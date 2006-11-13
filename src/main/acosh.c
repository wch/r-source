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
Copyright 1984, 1995, 2000 by Stephen L. Moshier
*/

static double polevl(double x, double *coef, int N)
{
    double *p = coef, ans;
    int i = N;

    ans = *p++;
    do
	ans = ans * x + *p++;
    while( --i );
    return( ans );
}

static double p1evl(double x, double *coef, int N)
{
    double *p = coef, ans;
    int i = N - 1;

    ans = x + *p++;
    do
	ans = ans * x + *p++;
    while( --i );
    return( ans );
}

static double P[] = {
 1.18801130533544501356E2,
 3.94726656571334401102E3,
 3.43989375926195455866E4,
 1.08102874834699867335E5,
 1.10855947270161294369E5
};
static double Q[] = {
/* 1.00000000000000000000E0,*/
 1.86145380837903397292E2,
 4.15352677227719831579E3,
 2.97683430363289370382E4,
 8.29725251988426222434E4,
 7.83869920495893927727E4
};

#define LOGE2 1.4426950408889634073599

double acosh(double x)
#ifdef __cplusplus
	throw ()
#endif
{
    double a, z;

    if(ISNAN(x)) return x;
    if( x < 1.0 ) return NA_REAL;
    if( x > 1.0e8 ) {
	if( x == R_PosInf ) return R_PosInf;
	return log(x) + LOGE2;
    }

    z = x - 1.0;

    if( z < 0.5 ) {
	a = sqrt(z) * (polevl(z, P, 4) / p1evl(z, Q, 5) );
	return a;
    }

    a = sqrt( z*(x+1.0) );
    return log(x + a);
}
