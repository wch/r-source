/*
 *  R : A Computer Langage for Statistical Data Analysis
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <math.h>

/*  Fast Fourier Transform
 *
 *  These routines are based on code by Richard Singleton in the
 *  book "Programs for Digital Signal Processing" put out by IEEE.
 *
 *  I have translated them to C and moved the memory allocation
 *  so that it takes place under the control of the algorithm
 *  which calls these.
 *
 *  void fft_factor(int n, int *maxf, int *maxp)
 *
 *  This factorizes the series length and computes the values of
 *  maxf and maxp which determine the amount of scratch storage
 *  required by the algorithm.
 *
 *  If maxf is zero on return, an error occured during factorization.
 *  The nature of the error can be determined from the value of maxp.
 *  If maxp is zero, an invalid (zero) parameter was passed and if
 *  maxp is one the internal nfac array was too small.  This can only
 *  happen for series lengths which exceed 12,754,584.
 *
 *  The following arrays need to be allocated following the call to
 *  fft_factor and preceding the call to fft_work.
 *
 *	work	double[4*maxf]
 *	iwork	int[maxp]
 *
 *  int fft_work(double *a, double *b, int nseg, int n, int nspn,
 *               int isn, double *work, int *iwork)
 *
 *  The routine returns 1 if the transform was completed successfully and
 *  0 if invalid values of the parameters were supplied.
 *
 *  Ross Ihaka
 *  University of Auckland
 *  February 1997
 *
 *
 *  Header from the original Singleton algorithm:
 *
 *  ----------------------------------------------------------------------
 *  subroutine:  fft
 *  multivariate complex fourier transform, computed in place
 *  using mixed-radix fast fourier transform algorithm.
 *  ----------------------------------------------------------------------
 * 
 *  arrays a and b originally hold the real and imaginary
 *       components of the data, and return the real and
 *       imaginary components of the resulting fourier coefficients.
 *  multivariate data is indexed according to the fortran
 *       array element successor function, without limit
 *       on the number of implied multiple subscripts.
 *       the subroutine is called once for each variate.
 *       the calls for a multivariate transform may be in any order.
 * 
 *  n is the dimension of the current variable.
 *  nspn is the spacing of consecutive data values
 *       while indexing the current variable.
 *  nseg*n*nspn is the total number of complex data values.
 *  the sign of isn determines the sign of the complex
 *       exponential, and the magnitude of isn is normally one.
 *       the magnitude of isn determines the indexing increment for a&b.
 * 
 *  if fft is called twice, with opposite signs on isn, an
 *       identity transformation is done...calls can be in either order.
 *       the results are scaled by 1/n when the sign of isn is positive.
 * 
 *  a tri-variate transform with a(n1,n2,n3), b(n1,n2,n3)
 *  is computed by
 *         call fft(a,b,n2*n3,n1,1,-1)
 *         call fft(a,b,n3,n2,n1,-1)
 *         call fft(a,b,1,n3,n1*n2,-1)
 * 
 *  a single-variate transform of n complex data values is computed by
 *         call fft(a,b,1,n,1,-1)
 * 
 *  the data may alternatively be stored in a single complex
 *       array a, then the magnitude of isn changed to two to
 *       give the correct indexing increment and a(2) used to
 *       pass the initial address for the sequence of imaginary
 *       values, e.g.
 *         call fft(a,a(2),nseg,n,nspn,-2)
 * 
 *  array nfac is working storage for factoring n.  the smallest
 *       number exceeding the 15 locations provided is 12,754,584.
 * 
 */

static int old_n = 0;

static int nfac[15];
static int m;
static int kt;
static int maxf;
static int maxp;

	/*  at the end of factorization,  nfac[] contains the factors,
	 *  m contains the number of factors and kt contains the number
	 *  of square factors  */

/* a whole bunch of stuff to keep watcom's C compiler happy */
#ifdef min
#undef min
#endif
#ifdef max
#undef max
#endif

static int max(int i, int j)
{
	return (i > j) ? i : j;
}

static int min(int i, int j)
{
	return (i < j) ? i : j;
}

/*  fft_factor - factorization check and determination of memory
 *  requirements for the fft.  On return *pmaxf will give the
 *  maximum factor size and *pmaxp will give the amount of integer
 *  scratch storage required.
 *
 *  If on return *pmaxf == 0, there was an error, the error type
 *  is indicated by *pmaxp.
 *
 *  If *pmaxp == 0  There was an illegal zero parameter among
 *                  nseg, n, and nspn.
 *
 *  If *pmaxp == 1  There we more than 15 factors to ntot.  */

void fft_factor(int n, int *pmaxf, int *pmaxp)
{
	int j, jj, k;

		/* check series length */

	if (n <= 0) {
		old_n = 0;
		*pmaxf = 0;
		*pmaxp = 0;
		return;
	}
	else old_n = n;

		/* determine the factors of n */

	m = 0;
	k = n;
	if (k == 1) return;

		/* extract square factors first */

	while(k % 16 == 0) {
		nfac[m++] = 4;
		k = k/16;
	}

	j = 3;
	jj = 9;

	while(jj <= k) {
		while(k % jj == 0) {
			nfac[m++] = j;
			k = k/jj;
		}
		j = j+2;
		jj = j*j;
	}

	if(k <= 4) {
		kt = m;
		nfac[m] = k;
		if(k != 1)
			m = m+1;
	}
	else {
		if(k % 4 == 0) {
			nfac[m++] = 2;
			k = k/4;
		}

			/* all square factors out now, but k >= 5 still */

		kt = m;
		maxp = max(kt+kt+2, k-1);
		j = 2;
		do {
			if (k % j == 0) {
				nfac[m++] = j;
				k = k/j;
			}
			j = ((j+1)/2)*2+1;
		}
                while(j <= k);
	}

	if (m <= kt+1)
		maxp = m+kt+1;
	if (m+kt > 15) {		/* error - too many factors */
		old_n = 0;
		*pmaxf = 0;
		*pmaxp = 0;
		return;
	}
	else {
		if (kt != 0) {
			j = kt;
			while(j != 0)
				nfac[m++] = nfac[--j];
		}
		maxf = m-kt;
		maxf = nfac[maxf-1];
		if (kt > 0)
			maxf = max(nfac[kt-1], maxf);
	}
	*pmaxf = maxf;
	*pmaxp = maxp;
}

static void fftmx(double *a, double *b, int ntot, int n, int nspan, int isn,
	int m, int kt, double *at, double *ck, double *bt, double *sk,
	int *np, int *nfac)
/*
double *a, *b, *at, *ck, *bt, *sk;
int ntot, n, nspan, isn, m, kt;
int *np, *nfac;
*/
{
      double aa, aj, ajm, ajp, ak, akm, akp;
      double bb, bj, bjm, bjp, bk, bkm, bkp;
      double c1, c2, c3, c72, cd;
      double dr, rad;
      double s1, s120, s2, s3, s72, sd;
      int i, inc, j, jc, jf, jj;
      int k, k1, k2, k3, k4, kk, klim, ks, kspan, kspnn;
      int lim, maxf, mm, nn, nt;

      a--; b--; at--; ck--; bt--; sk--;
      np--; nfac--;

      inc = abs(isn);
      nt = inc*ntot;
      ks = inc*nspan;
      rad = atan(1.0);
      s72 = rad/0.625;
      c72 = cos(s72);
      s72 = sin(s72);
      s120 = sqrt(0.75);
      if(isn > 0) goto L10;
      s72 = -s72;
      s120 = -s120;
      rad = -rad;
      goto L30;

	/* scale by 1/n for isn > 0 */

#ifdef SCALING
 L10: ak = 1.0/n;
      for(j=1 ; j<=nt ; j+=inc) {;
        a[j] = a[j]*ak;
        b[j] = b[j]*ak;
      };
#else
 L10:
	;
#endif

 L30: kspan = ks;
      nn = nt - inc;
      jc = ks/n;

	/* sin, cos values are re-initialized each lim steps */

      lim = 32;
      klim = lim*jc;
      i = 0;
      jf = 0;
      maxf = m - kt;
      maxf = nfac[maxf];
      if( kt > 0) maxf = max(nfac[kt],maxf);

	/* compute fourier transform */

 L40: dr = (8.0*jc)/kspan;
      cd = sin(0.5*dr*rad);
      cd = 2.0*cd*cd;
      sd = sin(dr*rad);
      kk = 1;
      i = i + 1;
      if( nfac[i] != 2) goto L110;

	/* transform for factor of 2 (including rotation factor) */

      kspan = kspan/2;
      k1 = kspan + 2;
 L50: k2 = kk + kspan;
      ak = a[k2];
      bk = b[k2];
      a[k2] = a[kk] - ak;
      b[k2] = b[kk] - bk;
      a[kk] = a[kk] + ak;
      b[kk] = b[kk] + bk;
      kk = k2 + kspan;
      if( kk <= nn) goto L50;
      kk = kk - nn;
      if( kk <= jc) goto L50;
      if( kk > kspan) goto L350;
 L60: c1 = 1.0 - cd;
      s1 = sd;
      mm = min(k1/2,klim);
      goto L80;
 L70: ak = c1 - (cd*c1+sd*s1);
      s1 = (sd*c1-cd*s1) + s1;

	/* the following three statements compensate for truncation */
	/* error.  if rounded arithmetic is used, substitute */
	/*	c1=ak */

#ifdef TRUNCATED_ARITHMETIC
      c1 = 0.5/(ak*ak+s1*s1) + 0.5;
      s1 = c1*s1;
      c1 = c1*ak;
#else
      c1 = ak;
#endif

 L80: k2 = kk + kspan;
      ak = a[kk] - a[k2];
      bk = b[kk] - b[k2];
      a[kk] = a[kk] + a[k2];
      b[kk] = b[kk] + b[k2];
      a[k2] = c1*ak - s1*bk;
      b[k2] = s1*ak + c1*bk;
      kk = k2 + kspan;
      if( kk < nt) goto L80;
      k2 = kk - nt;
      c1 = -c1;
      kk = k1 - k2;
      if( kk > k2) goto L80;
      kk = kk + jc;
      if( kk <= mm) goto L70;
      if( kk < k2) goto L90;
      k1 = k1 + inc + inc;
      kk = (k1-kspan)/2 + jc;
      if( kk <= jc+jc) goto L60;
      goto L40;
 L90: s1 = ((kk-1)/jc)*dr*rad;
      c1 = cos(s1);
      s1 = sin(s1);
      mm = min(k1/2,mm+klim);
      goto L80;

	/* transform for factor of 3 (optional code) */

L100: k1 = kk + kspan;
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

L110: if( nfac[i] != 4) goto L230;
      kspnn = kspan;
      kspan = kspan/4;
L120: c1 = 1.0;
      s1 = 0;
      mm = min(kspan,klim);
      goto L150;
L130: c2 = c1 - (cd*c1+sd*s1);
      s1 = (sd*c1-cd*s1) + s1;

	/* the following three statements compensate for truncation */
	/* error.  if rounded arithmetic is used, substitute */
	/* c1=c2 */

#ifdef TRUNCATED_ARITHMETIC
      c1 = 0.5/(c2*c2+s1*s1) + 0.5;
      s1 = c1*s1;
      c1 = c1*c2;
#else
      c1 = c2;
#endif

L140: c2 = c1*c1 - s1*s1;
      s2 = c1*s1*2.0;
      c3 = c2*c1 - s2*s1;
      s3 = c2*s1 + s2*c1;
L150: k1 = kk + kspan;
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
L160: a[k1] = akp*c1 - bkp*s1;
      b[k1] = akp*s1 + bkp*c1;
      a[k2] = ajp*c2 - bjp*s2;
      b[k2] = ajp*s2 + bjp*c2;
      a[k3] = akm*c3 - bkm*s3;
      b[k3] = akm*s3 + bkm*c3;
      kk = k3 + kspan;
      if( kk <= nt) goto L150;
L170: kk = kk - nt + jc;
      if( kk <= mm) goto L130;
      if( kk < kspan) goto L200;
      kk = kk - kspan + inc;
      if( kk <= jc) goto L120;
      if( kspan == jc) goto L350;
      goto L40;
L180: akp = akm + bjm;
      akm = akm - bjm;
      bkp = bkm - ajm;
      bkm = bkm + ajm;
      if( s1 != 0.0) goto L160;
L190: a[k1] = akp;
      b[k1] = bkp;
      a[k2] = ajp;
      b[k2] = bjp;
      a[k3] = akm;
      b[k3] = bkm;
      kk = k3 + kspan;
      if( kk <= nt) goto L150;
      goto L170;
L200: s1 = ((kk-1)/jc)*dr*rad;
      c1 = cos(s1);
      s1 = sin(s1);
      mm = min(kspan,mm+klim);
      goto L140;

	/* transform for factor of 5 (optional code) */

L210: c2 = c72*c72 - s72*s72;
      s2 = 2.0*c72*s72;
L220: k1 = kk + kspan;
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

L230: k = nfac[i];
      kspnn = kspan;
      kspan = kspan/k;
      if( k == 3) goto L100;
      if( k == 5) goto L210;
      if( k == jf) goto L250;
      jf = k;
      s1 = rad/(k/8.0);
      c1 = cos(s1);
      s1 = sin(s1);
      ck[jf] = 1.0;
      sk[jf] = 0.0;
      j = 1;
L240: ck[j] = ck[k]*c1 + sk[k]*s1;
      sk[j] = ck[k]*s1 - sk[k]*c1;
      k = k - 1;
      ck[k] = ck[j];
      sk[k] = -sk[j];
      j = j + 1;
      if( j < k) goto L240;
L250: k1 = kk;
      k2 = kk + kspnn;
      aa = a[kk];
      bb = b[kk];
      ak = aa;
      bk = bb;
      j = 1;
      k1 = k1 + kspan;
L260: k2 = k2 - kspan;
      j = j + 1;
      at[j] = a[k1] + a[k2];
      ak = at[j] + ak;
      bt[j] = b[k1] + b[k2];
      bk = bt[j] + bk;
      j = j + 1;
      at[j] = a[k1] - a[k2];
      bt[j] = b[k1] - b[k2];
      k1 = k1 + kspan;
      if( k1 < k2) goto L260;
      a[kk] = ak;
      b[kk] = bk;
      k1 = kk;
      k2 = kk + kspnn;
      j = 1;
L270: k1 = k1 + kspan;
      k2 = k2 - kspan;
      jj = j;
      ak = aa;
      bk = bb;
      aj = 0.0;
      bj = 0.0;
      k = 1;
L280: k = k + 1;
      ak = at[k]*ck[jj] + ak;
      bk = bt[k]*ck[jj] + bk;
      k = k + 1;
      aj = at[k]*sk[jj] + aj;
      bj = bt[k]*sk[jj] + bj;
      jj = jj + j;
      if( jj > jf) jj = jj - jf;
      if( k < jf) goto L280;
      k = jf - j;
      a[k1] = ak - bj;
      b[k1] = bk + aj;
      a[k2] = ak + bj;
      b[k2] = bk - aj;
      j = j + 1;
      if( j < k) goto L270;
      kk = kk + kspnn;
      if( kk <= nn) goto L250;
      kk = kk - nn;
      if( kk <= kspan) goto L250;

	/* multiply by rotation factor (except for factors of 2 and 4) */

L290: if( i == m) goto L350;
      kk = jc + 1;
L300: c2 = 1.0 - cd;
      s1 = sd;
      mm = min(kspan,klim);
      goto L320;
L310: c2 = c1 - (cd*c1+sd*s1);
      s1 = s1 + (sd*c1-cd*s1);

	/* the following three statements compensate for truncation */
	/* error.  if rounded arithmetic is used, they may */
	/* be deleted. */

#ifdef TRUNCATED_ARITHMETIC
      c1 = 0.5/(c2*c2+s1*s1) + 0.5;
      s1 = c1*s1;
      c2 = c1*c2;
#endif

L320: c1 = c2;
      s2 = s1;
      kk = kk + kspan;
L330: ak = a[kk];
      a[kk] = c2*ak - s2*b[kk];
      b[kk] = s2*ak + c2*b[kk];
      kk = kk + kspnn;
      if( kk <= nt) goto L330;
      ak = s1*s2;
      s2 = s1*c2 + c1*s2;
      c2 = c1*c2 - ak;
      kk = kk - nt + kspan;
      if( kk <= kspnn) goto L330;
      kk = kk - kspnn + jc;
      if( kk <= mm) goto L310;
      if( kk < kspan) goto L340;
      kk = kk - kspan + jc + inc;
      if( kk <= jc+jc) goto L300;
      goto L40;
L340: s1 = ((kk-1)/jc)*dr*rad;
      c2 = cos(s1);
      s1 = sin(s1);
      mm = min(kspan,mm+klim);
      goto L320;

	/* permute the results to normal order---done in two stages */
	/* permutation for square factors of n */

L350: np[1] = ks;
      if( kt == 0) goto L440;
      k = kt + kt + 1;
      if( m < k) k = k - 1;
      j = 1;
      np[k+1] = jc;
L360: np[j+1] = np[j]/nfac[j];
      np[k] = np[k+1]*nfac[j];
      j = j + 1;
      k = k - 1;
      if( j < k) goto L360;
      k3 = np[k+1];
      kspan = np[2];
      kk = jc + 1;
      k2 = kspan + 1;
      j = 1;
      if( n != ntot) goto L400;

	/* permutation for single-variate transform (optional code) */

L370: ak = a[kk];
      a[kk] = a[k2];
      a[k2] = ak;
      bk = b[kk];
      b[kk] = b[k2];
      b[k2] = bk;
      kk = kk + inc;
      k2 = kspan + k2;
      if( k2 < ks) goto L370;
L380: k2 = k2 - np[j];
      j = j + 1;
      k2 = np[j+1] + k2;
      if( k2 > np[j]) goto L380;
      j = 1;
L390: if( kk < k2) goto L370;
      kk = kk + inc;
      k2 = kspan + k2;
      if( k2 < ks) goto L390;
      if( kk < ks) goto L380;
      jc = k3;
      goto L440;

	/* permutation for multivariate transform */

L400: k = kk + jc;
L410: ak = a[kk];
      a[kk] = a[k2];
      a[k2] = ak;
      bk = b[kk];
      b[kk] = b[k2];
      b[k2] = bk;
      kk = kk + inc;
      k2 = k2 + inc;
      if( kk < k) goto L410;
      kk = kk + ks - jc;
      k2 = k2 + ks - jc;
      if( kk < nt) goto L400;
      k2 = k2 - nt + kspan;
      kk = kk - nt + jc;
      if( k2 < ks) goto L400;
L420: k2 = k2 - np[j];
      j = j + 1;
      k2 = np[j+1] + k2;
      if( k2 > np[j]) goto L420;
      j = 1;
L430: if( kk < k2) goto L400;
      kk = kk + jc;
      k2 = kspan + k2;
      if( k2 < ks) goto L430;
      if( kk < ks) goto L420;
      jc = k3;
L440: if( 2*kt+1 >= m) return;
      kspnn = np[kt+1];

	/* permutation for square-free factors of n */

      j = m - kt;
      nfac[j+1] = 1;
L450: nfac[j] = nfac[j]*nfac[j+1];
      j = j - 1;
      if( j != kt) goto L450;
      kt = kt + 1;
      nn = nfac[kt] - 1;
      jj = 0;
      j = 0;
      goto L480;
L460: jj = jj - k2;
      k2 = kk;
      k = k + 1;
      kk = nfac[k];
L470: jj = kk + jj;
      if( jj >= k2) goto L460;
      np[j] = jj;
L480: k2 = nfac[kt];
      k = kt + 1;
      kk = nfac[k];
      j = j + 1;
      if( j <= nn) goto L470;

	/* determine the permutation cycles of length greater than 1 */

      j = 0;
      goto L500;
L490: k = kk;
      kk = np[k];
      np[k] = -kk;
      if( kk != j) goto L490;
      k3 = kk;
L500: j = j + 1;
      kk = np[j];
      if( kk < 0) goto L500;
      if( kk != j) goto L490;
      np[j] = -j;
      if( j != nn) goto L500;
      maxf = inc*maxf;

	/* reorder a and b, following the permutation cycles */

      goto L570;
L510: j = j - 1;
      if( np[j] < 0) goto L510;
      jj = jc;
L520: kspan = jj;
      if( jj > maxf) kspan = maxf;
      jj = jj - kspan;
      k = np[j];
      kk = jc*k + i + jj;
      k1 = kk + kspan;
      k2 = 0;
L530: k2 = k2 + 1;
      at[k2] = a[k1];
      bt[k2] = b[k1];
      k1 = k1 - inc;
      if( k1 != kk) goto L530;
L540: k1 = kk + kspan;
      k2 = k1 - jc*(k+np[k]);
      k = -np[k];
L550: a[k1] = a[k2];
      b[k1] = b[k2];
      k1 = k1 - inc;
      k2 = k2 - inc;
      if( k1 != kk) goto L550;
      kk = k2;
      if( k != j) goto L540;
      k1 = kk + kspan;
      k2 = 0;
L560: k2 = k2 + 1;
      a[k1] = at[k2];
      b[k1] = bt[k2];
      k1 = k1 - inc;
      if( k1 != kk) goto L560;
      if( jj != 0) goto L520;
      if( j != 1) goto L510;
L570: j = k3 + 1;
      nt = nt - kspnn;
      i = nt - inc + 1;
      if( nt >= 0) goto L510;
      return;
}

int fft_work(double *a, double *b, int nseg, int n, int nspn, int isn,
	double *work, int *iwork)
{
	int nf, nspan, ntot;

		/* check that factorization was successful */

	if(old_n == 0) return 0;

		/* check that the parameters match those */
		/* of the factorization call */

	if(n != old_n || nseg <= 0 || nspn <= 0 || isn == 0)
		return 0;

		/* perform the transform */

	nf = n;
	nspan = nf * nspn;
	ntot = nspan * nseg;

	fftmx(a, b, ntot, nf, nspan, isn, m, kt, &work[0],
		&work[maxf], &work[2*maxf], &work[3*maxf], iwork, nfac);

	return 1;
}
