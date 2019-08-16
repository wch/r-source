/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2019  The R Core Team
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <limits.h> /* for INT_MAX */
#include <stddef.h> /* for size_t */
#include <stdlib.h> /* for abs */
#include <math.h>
#include <Rmath.h> /* for imax2(.),..*/
//#include <R_ext/Applic.h>

/*  Fast Fourier Transform
 *
 *  These routines are based on code by Richard Singleton in the
 *  book "Programs for Digital Signal Processing" put out by IEEE.
 *
 *  I have translated them to C and moved the memory allocation
 *  so that it takes place under the control of the algorithm
 *  which calls these; for R, see ./fourier.c
                                  ~~~~~~~~~~~
 *
 *  void fft_factor(int n, int *maxf, int *maxp)
 *
 *	This factorizes the series length and computes the values of
 *	maxf and maxp which determine the amount of scratch storage
 *	required by the algorithm.
 *
 *	If maxf is zero on return, an error occured during factorization.
 *	The nature of the error can be determined from the value of maxp.
 *	If maxp is zero, an invalid (zero) parameter was passed and
 *	if maxp is one,	 the internal nfac array was too small.	 This can only
 *	happen for series lengths which exceed 12,754,584.
 *
 * PROBLEM (see fftmx  below):	nfac[] is overwritten by fftmx() in fft_work()
 * -------  Consequence:  fft_factor() must be called way too often,
 * at least from  do_mvfft() [ ../main/fourier.c ]
 *
 *	The following arrays need to be allocated following the call to
 *	fft_factor and preceding the call to fft_work.
 *
 *		work	double[4*maxf]
 *		iwork	int[maxp]
 *
 *  int fft_work(double *a, double *b, int nseg, int n, int nspn,
 *		 int isn, double *work, int *iwork)
 *
 *	The routine returns 1 if the transform was completed successfully and
 *			    0 if invalid values of the parameters were supplied.
 *
 *  Ross Ihaka
 *  University of Auckland
 *  February 1997
 *  ==========================================================================
 *
 *  Header from the original Singleton algorithm:
 *
 * --------------------------------------------------------------
 * subroutine:	 fft
 * multivariate complex fourier transform, computed in place
 * using mixed-radix fast fourier transform algorithm.
 * --------------------------------------------------------------
 *
 * arrays a and b originally hold the real and imaginary
 *	components of the data, and return the real and
 *	imaginary components of the resulting fourier coefficients.
 * multivariate data is indexed according to the fortran
 *	array element successor function, without limit
 *	on the number of implied multiple subscripts.
 *	the subroutine is called once for each variate.
 *	the calls for a multivariate transform may be in any order.
 *
 * n	is the dimension of the current variable.
 * nspn is the spacing of consecutive data values
 *	while indexing the current variable.
 * nseg nseg*n*nspn is the total number of complex data values.
 * isn	the sign of isn determines the sign of the complex
 *	exponential, and the magnitude of isn is normally one.
 *	the magnitude of isn determines the indexing increment for a&b.
 *
 * if fft is called twice, with opposite signs on isn, an
 *	identity transformation is done...calls can be in either order.
 *	the results are scaled by 1/n when the sign of isn is positive.
 *
 * a tri-variate transform with a(n1,n2,n3), b(n1,n2,n3)
 * is computed by
 *	call fft(a,b,n2*n3,n1, 1,   -1)
 *	call fft(a,b,n3	  ,n2,n1,   -1)
 *	call fft(a,b,1,	   n3,n1*n2,-1)
 *
 * a single-variate transform of n complex data values is computed by
 *	call fft(a,b,1,n,1,-1)
 *
 * the data may alternatively be stored in a single complex
 *	array a, then the magnitude of isn changed to two to
 *	give the correct indexing increment and a(2) used to
 *	pass the initial address for the sequence of imaginary
 *	values, e.g.
 *	   call fft(a,a(2),nseg,n,nspn,-2)
 *
 * nfac[15] (array) is working storage for factoring n.	 the smallest
 *	number exceeding the 15 locations provided is 12,754,584.
 *
 * Update in R 3.1.0: nfac[20], increased array size. It is now possible to
 * factor any positive int n, up to 2^31 - 1.
 */

static void fftmx(double *a, double *b, int ntot, int n, int nspan, int isn,
		  int m, int kt, double *at, double *ck, double *bt, double *sk,
		  int *np, int *nfac)
{
/* called from	fft_work() */

/* Design BUG:	One purpose of fft_factor() would be to compute
 * ----------	nfac[] once and for all; and fft_work() [i.e. fftmx ]
 *		could reuse the factorization.
 * However: nfac[] is `destroyed' currently in the code below
 */
    double aa, aj, ajm, ajp, ak, akm, akp;
    double bb, bj, bjm, bjp, bk, bkm, bkp;
    double c1, c2=0, c3=0, c72, cd;
    double dr, rad;
    double s1, s120, s2=0, s3=0, s72, sd;
    int i, inc, j, jc, jf, jj;
    int k, k1, k2, k3=0, k4, kk, klim, ks, kspan, kspnn;
    int lim, maxf, mm, nn, nt;

    a--; b--; at--; ck--; bt--; sk--;
    np--;
    nfac--;/*the global one!*/

    inc = abs(isn);
    nt = inc*ntot;
    ks = inc*nspan;
    rad = M_PI_4;/* = pi/4 =^= 45 degrees */
    s72 = rad/0.625;/* 72 = 45 / .625  degrees */
    c72 = cos(s72);
    s72 = sin(s72);
    s120 = 0.5*M_SQRT_3;/* sin(120) = sqrt(3)/2 */
    if(isn <= 0) {
	s72 = -s72;
	s120 = -s120;
	rad = -rad;
    } else {
#ifdef SCALING
	/* scale by 1/n for isn > 0 */
	ak = 1.0/n;
	for(j=1 ; j<=nt ; j+=inc) {
	    a[j] *= ak;
	    b[j] *= ak;
	}
#endif
    }

    kspan = ks;
    nn = nt - inc;
    jc = ks/n;

	/* sin, cos values are re-initialized each lim steps */

    lim = 32;
    klim = lim*jc;
    i = 0;
    jf = 0;
    maxf = nfac[m - kt];
    if(kt > 0) maxf = imax2(nfac[kt],maxf);

	/* compute fourier transform */

L_start:
    dr = (8.0*jc)/kspan;
    cd = sin(0.5*dr*rad);
    cd = 2.0*cd*cd;
    sd = sin(dr*rad);
    kk = 1;
    i++;
    if( nfac[i] != 2) goto L110;

/* transform for factor of 2 (including rotation factor) */

    kspan /= 2;
    k1 = kspan + 2;
    do {
	do {
	    k2 = kk + kspan;
	    ak = a[k2];
	    bk = b[k2];
	    a[k2] = a[kk] - ak;
	    b[k2] = b[kk] - bk;
	    a[kk] += ak;
	    b[kk] += bk;
	    kk = k2 + kspan;
	} while(kk <= nn);
	kk -= nn;
    } while(kk <= jc);

    if(kk > kspan) goto L_fin;
L60:
    c1 = 1.0 - cd;
    s1 = sd;
    mm = imin2(k1/2,klim);
    goto L80;

L70:
    ak = c1 - (cd*c1+sd*s1);
    s1 = (sd*c1-cd*s1) + s1;

/* the following three statements compensate for truncation error. */
/* if rounded arithmetic is used (nowadays always ?!), substitute  c1=ak */
#ifdef TRUNCATED_ARITHMETIC
    c1 = 0.5/(ak*ak+s1*s1) + 0.5;
    s1 = c1*s1;
    c1 = c1*ak;
#else
    c1 = ak;
#endif

L80:
    do {
	k2 = kk + kspan;
	ak = a[kk] - a[k2];
	bk = b[kk] - b[k2];
	a[kk] += a[k2];
	b[kk] += b[k2];
	a[k2] = c1*ak - s1*bk;
	b[k2] = s1*ak + c1*bk;
	kk = k2 + kspan;
    } while(kk < nt);
    k2 = kk - nt;
    c1 = -c1;
    kk = k1 - k2;
    if( kk > k2) goto L80;
    kk += jc;
    if(kk <= mm) goto L70;
    if(kk >= k2) {
	k1 = k1 + inc + inc;
	kk = (k1-kspan)/2 + jc;
	if( kk <= jc+jc) goto L60;
	goto L_start;
    }

    s1 = ((kk-1)/jc)*dr*rad;
    c1 = cos(s1);
    s1 = sin(s1);
    mm = imin2(k1/2,mm+klim);
    goto L80;

/* transform for factor of 3 (optional code) */

L100:
    k1 = kk + kspan;
    k2 = k1 + kspan;
    ak = a[kk];
    bk = b[kk];
    aj = a[k1] + a[k2];
    bj = b[k1] + b[k2];
    a[kk] = ak + aj;
    b[kk] = bk + bj;
    ak = -0.5*aj + ak;
    bk = -0.5*bj + bk;
    aj = (a[k1]-a[k2])*s120;
    bj = (b[k1]-b[k2])*s120;
    a[k1] = ak - bj;
    b[k1] = bk + aj;
    a[k2] = ak + bj;
    b[k2] = bk - aj;
    kk = k2 + kspan;
    if( kk < nn) goto L100;
    kk = kk - nn;
    if( kk <= kspan) goto L100;
    goto L290;

/* transform for factor of 4 */

L110:
    if( nfac[i] != 4) goto L_f_odd;
    kspnn = kspan;
    kspan /= 4;
L120:
    c1 = 1.0;
    s1 = 0;
    mm = imin2(kspan,klim);
    goto L150;
L130:
    c2 = c1 - (cd*c1+sd*s1);
    s1 = (sd*c1-cd*s1) + s1;

/* the following three statements compensate for truncation error. */
/* if rounded arithmetic is used (nowadays always ?!), substitute  c1=c2 */
#ifdef TRUNCATED_ARITHMETIC
    c1 = 0.5/(c2*c2+s1*s1) + 0.5;
    s1 = c1*s1;
    c1 = c1*c2;
#else
    c1 = c2;
#endif

L140:
    c2 = c1*c1 - s1*s1;
    s2 = c1*s1*2.0;
    c3 = c2*c1 - s2*s1;
    s3 = c2*s1 + s2*c1;

L150:
    k1 = kk + kspan;
    k2 = k1 + kspan;
    k3 = k2 + kspan;
    akp = a[kk] + a[k2];
    akm = a[kk] - a[k2];
    ajp = a[k1] + a[k3];
    ajm = a[k1] - a[k3];
    a[kk] = akp + ajp;
    ajp = akp - ajp;
    bkp = b[kk] + b[k2];
    bkm = b[kk] - b[k2];
    bjp = b[k1] + b[k3];
    bjm = b[k1] - b[k3];
    b[kk] = bkp + bjp;
    bjp = bkp - bjp;
    if( isn < 0) goto L180;
    akp = akm - bjm;
    akm = akm + bjm;
    bkp = bkm + ajm;
    bkm = bkm - ajm;
    if( s1 == 0.0) goto L190;
L160:
    a[k1] = akp*c1 - bkp*s1;
    b[k1] = akp*s1 + bkp*c1;
    a[k2] = ajp*c2 - bjp*s2;
    b[k2] = ajp*s2 + bjp*c2;
    a[k3] = akm*c3 - bkm*s3;
    b[k3] = akm*s3 + bkm*c3;
    kk = k3 + kspan;
    if( kk <= nt) goto L150;
L170:
    kk = kk - nt + jc;
    if( kk <= mm) goto L130;
    if( kk < kspan) goto L200;
    kk = kk - kspan + inc;
    if(kk <= jc) goto L120;
    if(kspan == jc) goto L_fin;
    goto L_start;
L180:
    akp = akm + bjm;
    akm = akm - bjm;
    bkp = bkm - ajm;
    bkm = bkm + ajm;
    if( s1 != 0.0) goto L160;
L190:
    a[k1] = akp;
    b[k1] = bkp;
    a[k2] = ajp;
    b[k2] = bjp;
    a[k3] = akm;
    b[k3] = bkm;
    kk = k3 + kspan;
    if( kk <= nt) goto L150;
    goto L170;
L200:
    s1 = ((kk-1)/jc)*dr*rad;
    c1 = cos(s1);
    s1 = sin(s1);
    mm = imin2(kspan,mm+klim);
    goto L140;

/* transform for factor of 5 (optional code) */

L_f5:
    c2 = c72*c72 - s72*s72;
    s2 = 2.0*c72*s72;
L220:
    k1 = kk + kspan;
    k2 = k1 + kspan;
    k3 = k2 + kspan;
    k4 = k3 + kspan;
    akp = a[k1] + a[k4];
    akm = a[k1] - a[k4];
    bkp = b[k1] + b[k4];
    bkm = b[k1] - b[k4];
    ajp = a[k2] + a[k3];
    ajm = a[k2] - a[k3];
    bjp = b[k2] + b[k3];
    bjm = b[k2] - b[k3];
    aa = a[kk];
    bb = b[kk];
    a[kk] = aa + akp + ajp;
    b[kk] = bb + bkp + bjp;
    ak = akp*c72 + ajp*c2 + aa;
    bk = bkp*c72 + bjp*c2 + bb;
    aj = akm*s72 + ajm*s2;
    bj = bkm*s72 + bjm*s2;
    a[k1] = ak - bj;
    a[k4] = ak + bj;
    b[k1] = bk + aj;
    b[k4] = bk - aj;
    ak = akp*c2 + ajp*c72 + aa;
    bk = bkp*c2 + bjp*c72 + bb;
    aj = akm*s2 - ajm*s72;
    bj = bkm*s2 - bjm*s72;
    a[k2] = ak - bj;
    a[k3] = ak + bj;
    b[k2] = bk + aj;
    b[k3] = bk - aj;
    kk = k4 + kspan;
    if( kk < nn) goto L220;
    kk = kk - nn;
    if( kk <= kspan) goto L220;
    goto L290;

/* transform for odd factors */

L_f_odd:
    k = nfac[i];
    kspnn = kspan;
    kspan /= k;
    if(k == 3) goto L100;
    if(k == 5) goto L_f5;
    if(k == jf) goto L250;
    jf = k;
    s1 = rad/(k/8.0);
    c1 = cos(s1);
    s1 = sin(s1);
    ck[jf] = 1.0;
    sk[jf] = 0.0;

    for(j = 1; j < k; j++) { /* k is changing as well */
	ck[j] = ck[k]*c1 + sk[k]*s1;
	sk[j] = ck[k]*s1 - sk[k]*c1;
	k--;
	ck[k] = ck[j];
	sk[k] = -sk[j];
    }

L250:
    k1 = kk;
    k2 = kk + kspnn;
    aa = a[kk];
    bb = b[kk];
    ak = aa;
    bk = bb;
    j = 1;
    k1 = k1 + kspan;
L260:
    k2 = k2 - kspan;
    j++;
    at[j] = a[k1] + a[k2];
    ak = at[j] + ak;
    bt[j] = b[k1] + b[k2];
    bk = bt[j] + bk;
    j++;
    at[j] = a[k1] - a[k2];
    bt[j] = b[k1] - b[k2];
    k1 = k1 + kspan;
    if( k1 < k2) goto L260;
    a[kk] = ak;
    b[kk] = bk;
    k1 = kk;
    k2 = kk + kspnn;
    j = 1;
L270:
    k1 += kspan;
    k2 -= kspan;
    jj = j;
    ak = aa;
    bk = bb;
    aj = 0.0;
    bj = 0.0;
    k = 1;
    for(k=2; k < jf; k++) {
	ak += at[k]*ck[jj];
	bk += bt[k]*ck[jj];
	k++;
	aj += at[k]*sk[jj];
	bj += bt[k]*sk[jj];
	jj += j;
	if(jj > jf) jj -= jf;
    }
    k = jf - j;
    a[k1] = ak - bj;
    b[k1] = bk + aj;
    a[k2] = ak + bj;
    b[k2] = bk - aj;
    j++;
    if( j < k) goto L270;
    kk = kk + kspnn;
    if( kk <= nn) goto L250;
    kk = kk - nn;
    if( kk <= kspan) goto L250;

/* multiply by rotation factor (except for factors of 2 and 4) */

L290:
    if(i == m) goto L_fin;
    kk = jc + 1;
L300:
    c2 = 1.0 - cd;
    s1 = sd;
    mm = imin2(kspan,klim);

    do { /* L320: */
	c1 = c2;
	s2 = s1;
	kk += kspan;
	do { /* L330: */
	    do {
		ak = a[kk];
		a[kk] = c2*ak - s2*b[kk];
		b[kk] = s2*ak + c2*b[kk];
		kk += kspnn;
	    } while(kk <= nt);
	    ak = s1*s2;
	    s2 = s1*c2 + c1*s2;
	    c2 = c1*c2 - ak;
	    kk += -nt + kspan;
	} while(kk <= kspnn);
	kk += -kspnn + jc;
	if(kk <= mm) { /* L310: */
	    c2 = c1 - (cd*c1+sd*s1);
	    s1 = s1 + (sd*c1-cd*s1);
/* the following three statements compensate for truncation error.*/
/* if rounded arithmetic is used (nowadays always ?!), they may be deleted. */
#ifdef TRUNCATED_ARITHMETIC
	    c1 = 0.5/(c2*c2+s1*s1) + 0.5;
	    s1 = c1*s1;
	    c2 = c1*c2;
#endif
	    continue/* goto L320*/;
	}
	if(kk >= kspan) {
	    kk = kk - kspan + jc + inc;
	    if( kk <= jc+jc) goto L300;
	    goto L_start;
	}
	s1 = ((kk-1)/jc)*dr*rad;
	c2 = cos(s1);
	s1 = sin(s1);
	mm = imin2(kspan,mm+klim);
    } while(1);

/*------------------------------------------------------------*/


/* permute the results to normal order---done in two stages */
/* permutation for square factors of n */

L_fin:
    np[1] = ks;
    if( kt == 0) goto L440;
    k = kt + kt + 1;
    if( m < k) k--;
    np[k+1] = jc;
    for(j = 1; j < k; j++, k--) {
	np[j+1] = np[j]/nfac[j];
	np[k] = np[k+1]*nfac[j];
    }
    k3 = np[k+1];
    kspan = np[2];
    kk = jc + 1;
    k2 = kspan + 1;
    j = 1;

    if(n == ntot) {

	/* permutation for single-variate transform (optional code) */

      L370:
	do {
	    ak = a[kk];	   a[kk] = a[k2];    a[k2] = ak;
	    bk = b[kk];	   b[kk] = b[k2];    b[k2] = bk;
	    kk += inc;
	    k2 += kspan;
	} while(k2 < ks);
      L380:
	do { k2 -= np[j]; j++; k2 += np[j+1]; } while(k2 > np[j]);
	j = 1;
	do {
	    if(kk < k2) goto L370;
	    kk += inc;
	    k2 += kspan;
	} while(k2 < ks);
	if( kk < ks) goto L380;
	jc = k3;

    } else {

	/* permutation for multivariate transform */

      L400:
	k = kk + jc;
	do {
	    ak = a[kk]; a[kk] = a[k2]; a[k2] = ak;
	    bk = b[kk]; b[kk] = b[k2]; b[k2] = bk;
	    kk += inc;
	    k2 += inc;
	} while( kk < k);
	kk += ks - jc;
	k2 += ks - jc;
	if(kk < nt) goto L400;
	k2 += - nt + kspan;
	kk += - nt + jc;
	if( k2 < ks) goto L400;

	do {
	    do { k2 -= np[j]; j++; k2 += np[j+1]; } while(k2 > np[j]);
	    j = 1;
	    do {
		if(kk < k2) goto L400;
		kk += jc;
		k2 += kspan;
	    } while(k2 < ks);
	} while(kk < ks);
	jc = k3;
    }

L440:
    if( 2*kt+1 >= m) return;
    kspnn = np[kt+1];

/* permutation for square-free factors of n */

    /* Here, nfac[] is overwritten... -- now CUMULATIVE ("cumprod") factors */
    nn = m - kt;
    nfac[nn+1] = 1;
    for(j = nn; j > kt; j--)
	nfac[j] *= nfac[j+1];
    kt++;
    nn = nfac[kt] - 1;
    jj = 0;
    j = 0;
    goto L480;
L460:
    jj -= k2;
    k2 = kk;
    k++;
    kk = nfac[k];
L470:
    jj += kk;
    if( jj >= k2) goto L460;
    np[j] = jj;
L480:
    k2 = nfac[kt];
    k = kt + 1;
    kk = nfac[k];
    j++;
    if( j <= nn) goto L470;

/* determine the permutation cycles of length greater than 1 */

    j = 0;
    goto L500;

    do {
	do { k = kk; kk = np[k]; np[k] = -kk; } while(kk != j);
	k3 = kk;
      L500:
	do { j++; kk = np[j]; } while(kk < 0);
    } while(kk != j);
    np[j] = -j;
    if( j != nn) goto L500;
    maxf *= inc;
    goto L570;

/* reorder a and b, following the permutation cycles */

L_ord:
    do j--; while(np[j] < 0);
    jj = jc;

L520:
    kspan = imin2(jj,maxf);
    jj -= kspan;
    k = np[j];
    kk = jc*k + i + jj;

    for(k1= kk + kspan, k2= 1; k1 != kk;
	k1 -= inc, k2++) {
	at[k2] = a[k1];
	bt[k2] = b[k1];
    }

    do {
	k1 = kk + kspan;
	k2 = k1 - jc*(k+np[k]);
	k = -np[k];
	do {
	    a[k1] = a[k2];
	    b[k1] = b[k2];
	    k1 -= inc;
	    k2 -= inc;
	} while( k1 != kk);
	kk = k2;
    } while(k != j);

    for(k1= kk + kspan, k2= 1; k1 > kk;
	k1 -= inc, k2++) {
	a[k1] = at[k2];
	b[k1] = bt[k2];
    }

    if(jj != 0) goto L520;
    if( j != 1) goto L_ord;

L570:
    j = k3 + 1;
    nt = nt - kspnn;
    i = nt - inc + 1;
    if( nt >= 0) goto L_ord;
} /* fftmx */

static int old_n = 0;

static int nfac[20];
static int m_fac;
static int kt;
static int maxf;
static int maxp;

/* At the end of factorization,
 *	nfac[]	contains the factors,
 *	m_fac	contains the number of factors and
 *	kt	contains the number of square factors  */

/* non-API, but used by package RandomFields */
void fft_factor(int n, int *pmaxf, int *pmaxp)
{
/* fft_factor - factorization check and determination of memory
 *		requirements for the fft.
 *
 * On return,	*pmaxf will give the maximum factor size
 * and		*pmaxp will give the amount of integer scratch storage required.
 *
 * If *pmaxf == 0, there was an error, the error type is indicated by *pmaxp:
 *
 *  If *pmaxp == 0  There was an illegal zero parameter among nseg, n, and nspn.
 *  If *pmaxp == 1  There were more than 20 factors to ntot.  */

    int j, jj, k, sqrtk, kchanged;

	/* check series length */

    if (n <= 0) {
	old_n = 0; *pmaxf = 0; *pmaxp = 0;
	return;
    }
    else old_n = n;

	/* determine the factors of n */

    m_fac = 0;
    k = n;/* k := remaining unfactored factor of n */
    if (k == 1)
	return;

	/* extract square factors first ------------------ */

    /* extract 4^2 = 16 separately
     * ==> at most one remaining factor 2^2 = 4, done below */
    while(k % 16 == 0) {
	nfac[m_fac++] = 4;
	k /= 16;
    }

    /* extract 3^2, 5^2, ... */
    kchanged = 0;
    sqrtk = (int)sqrt(k);
    for(j = 3; j <= sqrtk; j += 2) {
	jj = j * j;
	while(k % jj == 0) {
	    nfac[m_fac++] = j;
	    k /= jj;
	    kchanged = 1;
	}
	if (kchanged) {
	    kchanged = 0;
	    sqrtk = (int)sqrt(k);
	}
    }

    if(k <= 4) {
	kt = m_fac;
	nfac[m_fac] = k;
	if(k != 1) m_fac++;
    }
    else {
	if(k % 4 == 0) {
	    nfac[m_fac++] = 2;
	    k /= 4;
	}

	/* all square factors out now, but k >= 5 still */

	kt = m_fac;
	maxp = imax2(kt+kt+2, k-1);
	j = 2;
	do {
	    if (k % j == 0) {
		nfac[m_fac++] = j;
		k /= j;
	    }
	    if (j > INT_MAX - 2)
		break;
	    j = ((j+1)/2)*2 + 1;
	}
	while(j <= k);
    }

    if (m_fac <= kt+1)
	maxp = m_fac+kt+1;
    if (m_fac+kt > 20) {		/* error - too many factors */
	old_n = 0; *pmaxf = 0; *pmaxp = 0;
	return;
    }
    else {
	if (kt != 0) {
	    j = kt;
	    while(j != 0)
		nfac[m_fac++] = nfac[--j];
	}
	maxf = nfac[m_fac-kt-1];
/* The last squared factor is not necessarily the largest PR#1429 */
	if (kt > 0) maxf = imax2(nfac[kt-1], maxf);
	if (kt > 1) maxf = imax2(nfac[kt-2], maxf);
	if (kt > 2) maxf = imax2(nfac[kt-3], maxf);
    }
    *pmaxf = maxf;
    *pmaxp = maxp;
}


Rboolean fft_work(double *a, double *b, int nseg, int n, int nspn, int isn,
		  double *work, int *iwork)
{
    int nf, nspan, ntot;

	/* check that factorization was successful */

    if(old_n == 0) return FALSE;

	/* check that the parameters match those of the factorization call */

    if(n != old_n || nseg <= 0 || nspn <= 0 || isn == 0)
	return FALSE;

	/* perform the transform */

    nf = n;
    nspan = nf * nspn;
    ntot = nspan * nseg;

    fftmx(a, b, ntot, nf, nspan, isn, m_fac, kt,
	  &work[0], &work[maxf], &work[2*(size_t)maxf], &work[3*(size_t)maxf],
	  iwork, nfac);

    return TRUE;
}
