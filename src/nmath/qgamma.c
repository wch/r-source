/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  based on AS91 (C) 1979 Royal Statistical Society
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  SYNOPSIS
 *
 *    #include "Mathlib.h"
 *    double qgamma(double p, double shape, double scale);
 *
 *  DESCRIPTION
 *
 *    Compute the quantile function of the gamma distribution.
 *
 *  NOTES
 *
 *    This function is based on the Applied Statistics
 *    Algorithm AS 91 ("ppchi2") and via pgamma(.) AS 239.
 *
 *  REFERENCES
 *
 *    Best, D. J. and D. E. Roberts (1975).
 *    Percentage Points of the Chi-Squared Disribution.
 *    Applied Statistics 24, page 385.
 */

#include "Mathlib.h"

#define C7	4.67
#define C8	6.66
#define C9	6.73
#define C10	13.32

#define EPS1 1e-2
#define EPS2 5e-7/* final precision */
#define MAXIT 1000/* was 20 */

#define pMIN 1e-100    /* was 0.000002 = 2e-6 */
#define pMAX (1-1e-12)/* was 0.999998 = 1 - 2e-6 */

double qgamma(double p, double alpha, double scale)
{
    static const double
	i420  = 1./ 420.,
	i2520 = 1./ 2520.,
	i5040 = 1./ 5040;

    double a, b, c, ch, g, p1, v;
    double p2, q, s1, s2, s3, s4, s5, s6, t, x;
    int i;

    /* test arguments and initialise */

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(alpha) || ISNAN(scale))
	return p + alpha + scale;
#endif

    if (p < 0 || p > 1 || alpha <= 0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    if (/* 0 <= */ p < pMIN) return 0;
    if (/* 1 >= */ p > pMAX) return ML_POSINF;

    v = 2*alpha;

    c = alpha-1;
    g = lgammafn(alpha);/* log Gamma(v/2) */


/*----- Phase I : Starting Approximation */


    if(v < (-1.24)*log(p)) {	/* for small chi-squared */

	ch = pow(p*alpha*exp(g+alpha*M_LN2), 1/alpha);
	if(ch < EPS2) {/* Corrected according to AS 91; MM, May 25, 1999 */
	    goto END;
	}

    } else if(v > 0.32) {	/*  using Wilson and Hilferty estimate */

	x = qnorm(p, 0, 1);
	p1 = 0.222222/v;
	ch = v*pow(x*sqrt(p1)+1-p1, 3);

	/* starting approximation for p tending to 1 */

	if( ch > 2.2*v + 6 )
	    ch = -2*(log(1-p) - c*log(0.5*ch) + g);

    } else { /* for v <= 0.32 */

	ch = 0.4;
	a = log(1-p) + g + c*M_LN2;
	do {
	    q = ch;
	    p1 = 1. / (1+ch*(C7+ch));
	    p2 = ch*(C9+ch*(C8+ch));
	    t = -0.5 +(C7+2*ch)*p1 - (C9+ch*(C10+3*ch))/p2;
	    ch -= (1- exp(a+0.5*ch)*p2*p1)/t;
	} while(fabs(q - ch) > EPS1*fabs(ch));
    }

/*----- Phase II: Iteration
 *	Call pgamma() [AS 239]  and calculate seven term taylor series
 */
    for( i=1 ; i <= MAXIT ; i++ ) {
	q = ch;
	p1 = 0.5*ch;
	p2 = p - pgamma(p1, alpha, 1);
#ifdef IEEE_754
	if(!R_FINITE(p2))
#else
	if(errno != 0)
#endif
		return ML_NAN;

	t = p2*exp(alpha*M_LN2+g+p1-c*log(ch));
	b = t/ch;
	a = 0.5*t - b*c;
	s1 = (210+a*(140+a*(105+a*(84+a*(70+60*a))))) * i420;
	s2 = (420+a*(735+a*(966+a*(1141+1278*a)))) * i2520;
	s3 = (210+a*(462+a*(707+932*a))) * i2520;
	s4 = (252+a*(672+1182*a)+c*(294+a*(889+1740*a))) * i5040;
	s5 = (84+2264*a+c*(1175+606*a)) * i2520;
	s6 = (120+c*(346+127*c)) * i5040;
	ch += t*(1+0.5*t*s1-b*c*(s1-b*(s2-b*(s3-b*(s4-b*(s5-b*s6))))));
	if(fabs(q - ch) < EPS2*ch)
	    goto END;
    }
    ML_ERROR(ME_PRECISION);
 END:
    return 0.5*scale*ch;
}
