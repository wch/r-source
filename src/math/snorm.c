/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 */

#include "Mathlib.h"

#ifdef NOTGOOD

/* Reference:
 * Ahrens, J.H. and Dieter, U.
 * Extensions of Forsythe's method for random sampling from
 * the normal distribution.
 * Math. Comput. 27, 927-937.
 *
 * The definitions of the constants a[k], d[k], t[k] and
 * H[k] are according to the abovementioned article
 */

static double a[32] =
{
	0.0,
	0.03917609,
	0.07841241,
	0.1177699,
	0.1573107,
	0.1970991,
	0.2372021,
	0.2776904,
	0.3186394,
	0.3601299,
	0.4022501,
	0.4450965,
	0.4887764,
	0.5334097,
	0.5791322,
	0.626099,
	0.6744898,
	0.7245144,
	0.7764218,
	0.8305109,
	0.8871466,
	0.9467818,
	1.00999,
	1.077516,
	1.150349,
	1.229859,
	1.318011,
	1.417797,
	1.534121,
	1.67594,
	1.862732,
	2.153875
};

static double d[31] =
{
	0.0,
	0.0,
	0.0,
	0.0,
	0.0,
	0.2636843,
	0.2425085,
	0.2255674,
	0.2116342,
	0.1999243,
	0.1899108,
	0.1812252,
	0.1736014,
	0.1668419,
	0.1607967,
	0.1553497,
	0.1504094,
	0.1459026,
	0.14177,
	0.1379632,
	0.1344418,
	0.1311722,
	0.128126,
	0.1252791,
	0.1226109,
	0.1201036,
	0.1177417,
	0.1155119,
	0.1134023,
	0.1114027,
	0.1095039
};

static double t[31] =
{
	7.673828e-4,
	0.00230687,
	0.003860618,
	0.005438454,
	0.007050699,
	0.008708396,
	0.01042357,
	0.01220953,
	0.01408125,
	0.01605579,
	0.0181529,
	0.02039573,
	0.02281177,
	0.02543407,
	0.02830296,
	0.03146822,
	0.03499233,
	0.03895483,
	0.04345878,
	0.04864035,
	0.05468334,
	0.06184222,
	0.07047983,
	0.08113195,
	0.09462444,
	0.1123001,
	0.136498,
	0.1716886,
	0.2276241,
	0.330498,
	0.5847031
};

static double H[31] =
{
	0.03920617,
	0.03932705,
	0.03950999,
	0.03975703,
	0.04007093,
	0.04045533,
	0.04091481,
	0.04145507,
	0.04208311,
	0.04280748,
	0.04363863,
	0.04458932,
	0.04567523,
	0.04691571,
	0.04833487,
	0.04996298,
	0.05183859,
	0.05401138,
	0.05654656,
	0.0595313,
	0.06308489,
	0.06737503,
	0.07264544,
	0.07926471,
	0.08781922,
	0.09930398,
	0.1155599,
	0.1404344,
	0.1836142,
	0.2790016,
	0.7010474
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
		w = (ustar - t[i - 1]) * H[i - 1];
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

#else

/*  Kinderman A. J. and Ramage J. G. (1976).
 *  Computer generation of normal random variables.
 *  JASA 71, 893-896.
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
