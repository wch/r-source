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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  SYNOPSIS
 *
 *    #include "Mathlib.h"
 *    double snorm(void);
 *
 *  DESCRIPTION
 *
 *    Random variates from the STANDARD normal distribution  N(0,1).
 *
 * Is called from  rnorm(..), but also rt(), rf(), rgamma(), ...
 */

#include "Mathlib.h"

#define KINDERMAN_RAMAGE

#ifdef AHRENS_DIETER

/*
 *  REFERENCE
 *
 *    Ahrens, J.H. and Dieter, U.
 *    Extensions of Forsythe's method for random sampling from
 *    the normal distribution.
 *    Math. Comput. 27, 927-937.
 *
 *    The definitions of the constants a[k], d[k], t[k] and
 *    h[k] are according to the abovementioned article
 */

static double a[32] =
{
	0.0000000, 0.03917609, 0.07841241, 0.1177699,
	0.1573107, 0.19709910, 0.23720210, 0.2776904,
	0.3186394, 0.36012990, 0.40225010, 0.4450965,
	0.4887764, 0.53340970, 0.57913220, 0.6260990,
	0.6744898, 0.72451440, 0.77642180, 0.8305109,
	0.8871466, 0.94678180, 1.00999000, 1.0775160,
	1.1503490, 1.22985900, 1.31801100, 1.4177970,
	1.5341210, 1.67594000, 1.86273200, 2.1538750
};

static double d[31] =
{
	0.0000000, 0.0000000, 0.0000000, 0.0000000,
	0.0000000, 0.2636843, 0.2425085, 0.2255674,
	0.2116342, 0.1999243, 0.1899108, 0.1812252,
	0.1736014, 0.1668419, 0.1607967, 0.1553497,
	0.1504094, 0.1459026, 0.1417700, 0.1379632,
	0.1344418, 0.1311722, 0.1281260, 0.1252791,
	0.1226109, 0.1201036, 0.1177417, 0.1155119,
	0.1134023, 0.1114027, 0.1095039
};

static double t[31] =
{
	7.673828e-4, 0.002306870, 0.003860618, 0.005438454,
	0.007050699, 0.008708396, 0.010423570, 0.012209530,
	0.014081250, 0.016055790, 0.018152900, 0.020395730,
	0.022811770, 0.025434070, 0.028302960, 0.031468220,
	0.034992330, 0.038954830, 0.043458780, 0.048640350,
	0.054683340, 0.061842220, 0.070479830, 0.081131950,
	0.094624440, 0.112300100, 0.136498000, 0.171688600,
	0.227624100, 0.330498000, 0.584703100
};

static double h[31] =
{
	0.03920617, 0.03932705, 0.03950999, 0.03975703,
	0.04007093, 0.04045533, 0.04091481, 0.04145507,
	0.04208311, 0.04280748, 0.04363863, 0.04458932,
	0.04567523, 0.04691571, 0.04833487, 0.04996298,
	0.05183859, 0.05401138, 0.05654656, 0.05953130,
	0.06308489, 0.06737503, 0.07264544, 0.07926471,
	0.08781922, 0.09930398, 0.11555990, 0.14043440,
	0.18361420, 0.27900160, 0.70104740
};

#define repeat for(;;)

double snorm(void)
{
	double s, u, w, y, ustar, aa, tt;
	int i;

	u = sunif();
	s = 0.0;
	if (u > 0.5)
		s = 1.0;
	u = u + u - s;
	u *= 32.0;
	i = (int) u;
	if (i == 32)
		i = 31;
	if (i != 0) {
		ustar = u - i;
		aa = a[i - 1];
		while (ustar <= t[i - 1]) {
			u = sunif();
			w = u * (a[i] - aa);
			tt = (w * 0.5 + aa) * w;
			repeat {
				if (ustar > tt)
					goto deliver;
				u = sunif();
				if (ustar < u)
					break;
				tt = u;
				ustar = sunif();
			}
			ustar = sunif();
		}
		w = (ustar - t[i - 1]) * h[i - 1];
	}
	else {
		i = 6;
		aa = a[31];
		repeat {
			u = u + u;
			if (u >= 1.0)
				break;
			aa = aa + d[i - 1];
			i = i + 1;
		}
		u = u - 1.0;
		repeat {
			w = u * d[i - 1];
			tt = (w * 0.5 + aa) * w;
			repeat {
				ustar = sunif();
				if (ustar > tt)
					goto jump;
				u = sunif();
				if (ustar < u)
					break;
				tt = u;
			}
			u = sunif();
		}
	      jump:;
	}

      deliver:
	y = aa + w;
	return (s == 1.0) ? -y : y;

}

#endif

#ifdef KINDERMAN_RAMAGE

/*
 *  REFERENCE
 *
 *    Kinderman A. J. and Ramage J. G. (1976).
 *    Computer generation of normal random variables.
 *    JASA 71, 893-896.
 */

#define C1		0.398942280401433
#define C2		0.180025191068563
#define g(x)		(C1*exp(-x*x/2.0)-C2*(a-fabs(x)))

#ifndef Win32
#define max(a,b)	(((a)>(b))?(a):(b))
#define min(a,b)	(((a)<(b))?(a):(b))
#endif

static double a =  2.216035867166471;

double snorm()
{
	double t, u1, u2, u3;
	double sunif();

	u1 = sunif();
	if( u1<0.884070402298758 ) {
		u2 = sunif();
		return a*(1.13113163544180*u1+u2-1);
	}

	if( u1>=0.973310954173898 ) {
	   tail:
		u2 = sunif();
		u3 = sunif();
		t = (a*a-2*log(u3));
		if( u2*u2<(a*a)/t )
			return (u1<0.986655477086949) ? sqrt(t) : -sqrt(t) ;
		goto tail;
	}

	if( u1>=0.958720824790463 ) {
	region3:
		u2 = sunif();
		u3 = sunif();
		t = a-0.630834801921960*min(u2,u3);
		if( max(u2,u3)<=0.755591531667601 )
			return (u2<u3) ? t : -t ;
		if( 0.034240503750111*fabs(u2-u3)<=g(t) )
			return (u2<u3) ? t : -t ;
		goto region3;
	}

	if( u1>=0.911312780288703 ) {
	region2:
		u2 = sunif();
		u3 = sunif();
		t = 0.479727404222441+1.105473661022070*min(u2,u3);
		if( max(u2,u3)<=0.872834976671790 )
			return (u2<u3) ? t : -t ;
		if( 0.049264496373128*fabs(u2-u3)<=g(t) )
			return (u2<u3) ? t : -t ;
		goto region2;
	}

region1:
	u2 = sunif();
	u3 = sunif();
	t = 0.479727404222441-0.595507138015940*min(u2,u3);
	if( max(u2,u3)<=0.805577924423817 )
		return (u2<u3) ? t : -t ;
	goto region1;
}

#endif
