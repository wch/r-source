/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
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
 *    double rpois(double lambda)
 *
 *  DESCRIPTION
 *
 *    Random variates from the Poisson distribution.
 *
 *  REFERENCE
 *
 *    Ahrens, J.H. and Dieter, U. (1982).
 *    Computer generation of Poisson deviates
 *    from modified normal distributions.
 *    ACM Trans. Math. Software 8, 163-179.
 */

/* Factorial Table */
static double fact[10] =
{
    1.0,
    1.0,
    2.0,
    6.0,
    24.0,
    120.0,
    720.0,
    5040.0,
    40320.0,
    362880.0
};

#define a0	-0.5
#define a1	 0.3333333
#define a2	-0.2500068
#define a3	 0.2000118
#define a4	-0.1661269
#define a5	 0.1421878
#define a6	-0.1384794
#define a7	 0.1250060

#define repeat for(;;)

#include "Mathlib.h"

double rpois(double mu)
{
    static double /* a0, a1, a2, a3, a4, a5, a6, a7, */ b1, b2;
    static double c, c0, c1, c2, c3, d, del, difmuk, e;
    static double fk, fx, fy, g, omega;
    static double p, p0, px, py, q, s, t, u, v, x, xx;
    static double pp[36];
    static int j, k, kflag, l, m;
    static int ipois;
    static double muprev = 0.0;
    static double muold = 0.0;

    if (mu != muprev) {
	if (mu >= 10.0) {
	    /* case a. (recalculation of s,d,l */
	    /* if mu has changed)  */
	    /* the poisson probabilities pk */
	    /* exceed the discrete normal */
	    /* probabilities fk whenever k >= m(mu). */
	    /* l=ifix(mu-1.1484) is an upper bound */
	    /* to m(mu) for all mu >= 10. */
	    muprev = mu;
	    s = sqrt(mu);
	    d = 6.0 * mu * mu;
	    l = mu - 1.1484;
	} else {
	    /* Case B. (start new table and */
	    /* calculate p0 if necessary) */
	    muprev = 0.0;
	    if (mu != muold) {
		muold = mu;
		m = imax2(1, (int) mu);
		l = 0;
		p = exp(-mu);
		q = p;
		p0 = p;
	    }
	    repeat {
				/* Step U. uniform sample */
				/* for inversion method */
		u = sunif();
		ipois = 0;
		if (u <= p0)
		    return (double)ipois;
				/* Step T. table comparison until */
				/* the end pp(l) of the pp-table of */
				/* cumulative poisson probabilities */
				/* (0.458=pp(9) for mu=10) */
		if (l != 0) {
		    j = 1;
		    if (u > 0.458)
			j = imin2(l, m);
		    for (k = j; k <= l; k++)
			if (u <= pp[k])
			    return (double)k;
		    if (l == 35)
			continue;
		}
				/* Step C. creation of new poisson */
				/* probabilities p and their cumulatives */
				/* q=pp[k] */
		l = l + 1;
		for (k = l; k <= 35; k++) {
		    p = p * mu / k;
		    q = q + p;
		    pp[k] = q;
		    if (u <= q) {
			l = k;
			return (double)k;
		    }
		}
		l = 35;
	    }
	}
    }
    /* Step N. normal sample */
    /* snorm() for standard normal deviate */
    g = mu + s * snorm();
    if (g >= 0.0) {
	ipois = g;
	/* Step I. immediate acceptance */
	/* if ipois is large enough */
	if (ipois >= l)
	    return (double)ipois;
	/* Step S. squeeze acceptance */
	/* sunif() for (0,1)-sample u */
	fk = ipois;
	difmuk = mu - fk;
	u = sunif();
	if (d * u >= difmuk * difmuk * difmuk)
	    return (double)ipois;
    }
    /* Step P. preparations for steps Q and H. */
    /* (recalculations of parameters if necessary) */
    /* 0.3989423=(2*pi)**(-0.5) */
    /* 0.416667e-1=1./24. */
    /* 0.1428571=1./7. */
    /* The quantities b1, b2, c3, c2, c1, c0 are for the Hermite */
    /* approximations to the discrete normal probabilities fk. */
    /* c=.1069/mu guarantees majorization by the 'hat'-function. */
    if (mu != muold) {
	muold = mu;
	omega = 0.3989423 / s;
	b1 = 0.4166667e-1 / mu;
	b2 = 0.3 * b1 * b1;
	c3 = 0.1428571 * b1 * b2;
	c2 = b2 - 15. * c3;
	c1 = b1 - 6. * b2 + 45. * c3;
	c0 = 1. - b1 + 3. * b2 - 15. * c3;
	c = 0.1069 / mu;
    }
    if (g >= 0.0) {
	/* 'Subroutine' F is called (kflag=0 for correct return) */
	kflag = 0;
	goto L20;
    }
    repeat {
	/* Step E. Exponential Sample */
	/* sexp() for standard exponential deviate */
	/* e and sample t from the laplace 'hat' */
	/* (if t <= -0.6744 then pk < fk for all mu >= 10.) */
	e = sexp();
	u = sunif();
	u = u + u - 1.0;
	t = 1.8 + fsign(e, u);
	if (t > -0.6744) {
	    ipois = mu + s * t;
	    fk = ipois;
	    difmuk = mu - fk;
	    /* 'subroutine' f is called */
	    /* (kflag=1 for correct return) */
	    kflag = 1;
	    /* Step f. 'subroutine' f. */
	    /* calculation of px,py,fx,fy. */
	    /* case ignpoi < 10 uses */
	    /* factorials from table fact */
	  L20:if (ipois < 10) {
	      px = -mu;
	      py = pow(mu, (double) ipois) / fact[ipois];
	  } else {
				/* Case ipois >= 10 uses polynomial */
				/* approximation a0-a7 for accuracy */
				/* when advisable */
				/* 0.8333333e-1=1./12.0 */
				/* 0.3989423=(2*pi)**(-0.5) */
	      del = 0.8333333e-1 / fk;
	      del = del - 4.8 * del * del * del;
	      v = difmuk / fk;
	      if (fabs(v) <= 0.25)
		  px = fk * v * v * (((((((a7 * v + a6) * v + a5) * v + a4) * v + a3) * v + a2) * v + a1) * v + a0) - del;
	      else
		  px = fk * log(1.0 + v) - difmuk - del;
	      py = 0.3989423 / sqrt(fk);
	  }
	    x = (0.5 - difmuk) / s;
	    xx = x * x;
	    fx = -0.5 * xx;
	    fy = omega * (((c3 * xx + c2) * xx + c1) * xx + c0);
	    if (kflag > 0) {
				/* Step H. hat acceptance */
				/* (e is repeated on rejection) */
		if (c * fabs(u) <= py * exp(px + e) - fy * exp(fx + e))
		    break;
	    } else
				/* step q. quotient acceptance (rare case) */
		if (fy - u * fy <= py * exp(px - fx))
		    break;
	}
    }
    return (double)ipois;
}
