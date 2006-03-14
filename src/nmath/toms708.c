/* Based on C translation of ACM TOMS 708
   Please do not change this, e.g. to use R's versions of the 
   ancillary routines, without investigating the error analysis as we
   do need very high relative accuracy.  This version has about
   14 digits accuracy.
*/

#include "nmath.h"
#include <math.h>
#define min(a,b) ((a < b)?a:b)
#define max(a,b) ((a > b)?a:b)

static double bfrac(double, double, double, double, double, double);
static void bgrat(double, double, double, double, double *, double, int *);
static void grat1(double, double, double, double *, double *, double);
static double apser(double, double, double, double);
static double bpser(double, double, double, double);
static double basym(double, double, double, double);
static double fpser(double, double, double, double);
static double bup(double, double, double, double, int, double);
static double exparg(int);
static double psi(double);
static double gam1(double);
static double gamln1(double);
static double betaln(double, double);
static double algdiv(double, double);
static double brcmp1(int, double, double, double, double);
static double brcomp(double, double, double, double);
static double rlog1(double);
static double bcorr(double, double);
static double gamln(double);
static double alnrel(double);
static double esum(int, double);
static double erf__(double);
static double rexpm1(double);
static double erfc1(int, double);
static double gsumln(double, double);
extern double Rf_d1mach(int i);
extern int Rf_i1mach(int i);


/*      ALGORITHM 708, COLLECTED ALGORITHMS FROM ACM. */
/*      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE, */
/*      VOL. 18, NO. 3, SEPTEMBER, 1992, PP. 360-373z. */
void attribute_hidden
bratio(double a, double b, double x, double y, double *w, double *w1, 
       int *ierr, int log_p)
{
    /* System generated locals */
    double d1;

    /* Local variables */
    int n, ind, ierr1;
    double t, z, a0, b0, x0, y0, eps, lambda;

/* ----------------------------------------------------------------------- */

/*            EVALUATION OF THE INCOMPLETE BETA FUNCTION IX(A,B) */

/*                     -------------------- */

/*     IT IS ASSUMED THAT A AND B ARE NONNEGATIVE, AND THAT X <= 1 */
/*     AND Y = 1 - X.  BRATIO ASSIGNS W AND W1 THE VALUES */

/*                      W  = IX(A,B) */
/*                      W1 = 1 - IX(A,B) */

/*     IERR IS A VARIABLE THAT REPORTS THE STATUS OF THE RESULTS. */
/*     IF NO INPUT ERRORS ARE DETECTED THEN IERR IS SET TO 0 AND */
/*     W AND W1 ARE COMPUTED. OTHERWISE, IF AN ERROR IS DETECTED, */
/*     THEN W AND W1 ARE ASSIGNED THE VALUE 0 AND IERR IS SET TO */
/*     ONE OF THE FOLLOWING VALUES ... */

/*        IERR = 1  IF A OR B IS NEGATIVE */
/*        IERR = 2  IF A = B = 0 */
/*        IERR = 3  IF X < 0 OR X > 1 */
/*        IERR = 4  IF Y < 0 OR Y > 1 */
/*        IERR = 5  IF X + Y != 1 */
/*        IERR = 6  IF X = A = 0 */
/*        IERR = 7  IF Y = B = 0 */

/* -------------------- */
/*     WRITTEN BY ALFRED H. MORRIS, JR. */
/*        NAVAL SURFACE WARFARE CENTER */
/*        DAHLGREN, VIRGINIA */
/*     REVISED ... NOV 1991 */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */

/*     ****** EPS IS A MACHINE DEPENDENT CONSTANT. EPS IS THE SMALLEST */
/*            FLOATING POINT NUMBER FOR WHICH 1.0 + EPS > 1.0 */

    eps = 2.0 * Rf_d1mach(3);

/* ----------------------------------------------------------------------- */
    *w = 0.0;
    *w1 = 0.0;
    if (a < 0.0 || b < 0.0) {
	goto L300;
    }
    if (a == 0.0 && b == 0.0) {
	goto L310;
    }
    if (x < 0.0 || x > 1.0) {
	goto L320;
    }
    if (y < 0.0 || y > 1.0) {
	goto L330;
    }
    z = x + y - 0.5 - 0.5;
    if (fabs(z) > eps * 3.0) {
	goto L340;
    }

    *ierr = 0;
    if (x == 0.0) {
	goto L200;
    }
    if (y == 0.0) {
	goto L210;
    }
    if (a == 0.0) {
	goto L211;
    }
    if (b == 0.0) {
	goto L201;
    }

    eps = max(eps,1e-15);
    if (max(a,b) < eps * .001) {
	goto L230;
    }

    ind = 0;
    a0 = a;
    b0 = b;
    x0 = x;
    y0 = y;
    if (min(a0,b0) > 1.0) {
	goto L30;
    }

/*             PROCEDURE FOR a0 <= 1 OR b0 <= 1 */

    if (x <= 0.5) {
	goto L10;
    }
    ind = 1;
    a0 = b;
    b0 = a;
    x0 = y;
    y0 = x;

L10:
/* Computing MIN */
    if (b0 < min(eps, eps * a0)) {
	goto L80;
    }
/* Computing MIN */
    if (a0 < min(eps, eps * b0) && b0 * x0 <= 1.0) {
	goto L90;
    }
    if (max(a0,b0) > 1.0) {
	goto L20;
    }
    if (a0 >= min(0.2, b0)) {
	goto L100;
    }
    if (pow(x0, a0) <= 0.9) {
	goto L100;
    }
    if (x0 >= 0.3) {
	goto L110;
    }
    n = 20;
    goto L130;

L20:
    if (b0 <= 1.0) {
	goto L100;
    }
    if (x0 >= 0.3) {
	goto L110;
    }
    if (x0 >= 0.1) {
	goto L21;
    }
    if (pow(x0*b0, a0) <= 0.7) {
	goto L100;
    }
L21:
    if (b0 > 15.0) {
	goto L131;
    }
    n = 20;
    goto L130;

/*             PROCEDURE FOR a0 > 1 AND b0 > 1 */

L30:
    if (a > b) {
	goto L31;
    }
    lambda = a - (a + b) * x;
    goto L32;
L31:
    lambda = (a + b) * y - b;
L32:
    if (lambda >= 0.0) {
	goto L40;
    }
    ind = 1;
    a0 = b;
    b0 = a;
    x0 = y;
    y0 = x;
    lambda = fabs(lambda);

L40:
    if (b0 < 40.0 && b0 * x0 <= 0.7) {
	goto L100;
    }
    if (b0 < 40.0) {
	goto L140;
    }
    if (a0 > b0) {
	goto L50;
    }
    if (a0 <= 100.0) {
	goto L120;
    }
    if (lambda > a0 * 0.03) {
	goto L120;
    }
    goto L180;
L50:
    if (b0 <= 100.0) {
	goto L120;
    }
    if (lambda > b0 * 0.03) {
	goto L120;
    }
    goto L180;

/*            EVALUATION OF THE APPROPRIATE ALGORITHM */

L80:  /* b0 < min(eps, eps * a0) */
    *w = fpser(a0, b0, x0, eps);
    *w1 = 0.5 - *w + 0.5;
    goto L220;

L90: /* a0 < min(eps, eps * b0) && b0 * x0 <= 1.0 */
    *w1 = apser(a0, b0, x0, eps);
    *w = 0.5 - *w1 + 0.5;
    goto L220;

L100:
    *w = bpser(a0, b0, x0, eps);
    *w1 = 0.5 - *w + 0.5;
    goto L220;

L110:
    *w1 = bpser(b0, a0, y0, eps);
    *w = 0.5 - *w1 + 0.5;
    goto L220;

L120:
    d1 = eps * 15.0;
    *w = bfrac(a0, b0, x0, y0, lambda, d1);
    *w1 = 0.5 - *w + 0.5;
    goto L220;

L130:
    *w1 = bup(b0, a0, y0, x0, n, eps);
    b0 += n;
L131:
    bgrat(b0, a0, y0, x0, w1, 15*eps, &ierr1);
    *w = 0.5 - *w1 + 0.5;
    goto L220;

L140:
    n = (int) b0;
    b0 -= n;
    if (b0 != 0.0) {
	goto L141;
    }
    --n;
    b0 = 1.0;
L141:
    *w = bup(b0, a0, y0, x0, n, eps);
    if (x0 > 0.7) {
	goto L150;
    }
    *w += bpser(a0, b0, x0, eps);
    *w1 = 0.5 - *w + 0.5;
    goto L220;

L150:
    if (a0 > 15.0) {
	goto L151;
    }
    n = 20;
    *w += bup(a0, b0, x0, y0, n, eps);
    a0 += n;
L151:
    bgrat(a0, b0, x0, y0, w, 15*eps, &ierr1);
    *w1 = 0.5 - *w + 0.5;
    goto L220;

L180:
    d1 = eps * 100.0;
    *w = basym(a0, b0, lambda, d1);
    *w1 = 0.5 - *w + 0.5;
    goto L220;

/*               TERMINATION OF THE PROCEDURE */

L200:
    if (a == 0.0) {
	goto L350;
    }
L201:
    *w = 0.0;
    *w1 = 1.0;
    return;

L210:
    if (b == 0.0) {
	goto L360;
    }
L211:
    *w = 1.0;
    *w1 = 0.0;
    return;

L220:
    if (ind == 0) {
	return;
    }
    t = *w;
    *w = *w1;
    *w1 = t;
    return;

/*           PROCEDURE FOR A AND B < 1.e-3*eps */

L230:
    *w = b / (a + b);
    *w1 = a / (a + b);
    return;

/*                       ERROR RETURN */

L300:
    *ierr = 1;
    return;
L310:
    *ierr = 2;
    return;
L320:
    *ierr = 3;
    return;
L330:
    *ierr = 4;
    return;
L340:
    *ierr = 5;
    return;
L350:
    *ierr = 6;
    return;
L360:
    *ierr = 7;
    return;
} /* bratio */

double fpser(double a, double b, double x, double eps)
{
    /* System generated locals */
    double ret_val;

    /* Local variables */
    double c, s, t, an, tol;

/* ----------------------------------------------------------------------- */

/*                 EVALUATION OF I (A,B) */
/*                                X */

/*          FOR B < MIN(EPS, EPS*A) AND X <= 0.5. */

/* ----------------------------------------------------------------------- */

/*                  SET  FPSER = X**A */

    ret_val = 1.0;
    if (a <= eps * 0.001) {
	goto L10;
    }
    ret_val = 0.0;
    t = a * log(x);
    if (t < exparg(1)) {
	return ret_val;
    }
    ret_val = exp(t);

/*                NOTE THAT 1/B(A,B) = B */

L10:
    ret_val = b / a * ret_val;
    tol = eps / a;
    an = a + 1.0;
    t = x;
    s = t / an;
L20:
    an += 1.0;
    t = x * t;
    c = t / an;
    s += c;
    if (fabs(c) > tol) {
	goto L20;
    }

    ret_val *= a * s + 1.0;
    return ret_val;
} /* fpser */

static double apser(double a, double b, double x, double eps)
{
    /* Initialized data */

    static double g = .577215664901533;

    /* Local variables */
    double c, j, s, t, aj, bx;
    double tol;

/* ----------------------------------------------------------------------- */
/*     APSER YIELDS THE INCOMPLETE BETA RATIO I(SUB(1-X))(B,A) FOR */
/*     A <= MIN(EPS,EPS*B), B*X <= 1, AND X <= 0.5. USED WHEN */
/*     A IS VERY SMALL. USE ONLY IF ABOVE INEQUALITIES ARE SATISFIED. */
/* ----------------------------------------------------------------------- */
/* -------------------- */
/* -------------------- */
    bx = b * x;
    t = x - bx;
    if (b * eps > 0.02) {
	goto L10;
    }
    c = log(x) + psi(b) + g + t;
    goto L20;
L10:
    c = log(bx) + g + t;

L20:
    tol = eps * 5.0 * fabs(c);
    j = 1.0;
    s = 0.0;
L30:
    j += 1.0;
    t *= x - bx / j;
    aj = t / j;
    s += aj;
    if (fabs(aj) > tol) {
	goto L30;
    }

    return -a * (c + s);
} /* apser */

static double bpser(double a, double b, double x, double eps)
{
    /* System generated locals */
    int i1;
    double ret_val;

    /* Local variables */
    int i, m;
    double c, n, t, u, w, z, a0, b0, apb, tol, sum;

/* ----------------------------------------------------------------------- */
/*     POWER SERIES EXPANSION FOR EVALUATING IX(A,B) WHEN B <= 1 */
/*     OR B*X <= 0.7.  EPS IS THE TOLERANCE USED. */
/* ----------------------------------------------------------------------- */

    ret_val = 0.0;
    if (x == 0.0) {
	return ret_val;
    }
/* ----------------------------------------------------------------------- */
/*            COMPUTE THE FACTOR X**A/(A*BETA(A,B)) */
/* ----------------------------------------------------------------------- */
    a0 = min(a,b);
    if (a0 < 1.0) {
	goto L10;
    }
    z = a * log(x) - betaln(a, b);
    ret_val = exp(z) / a;
    goto L70;
L10:
    b0 = max(a,b);
    if (b0 >= 8.0) {
	goto L60;
    }
    if (b0 > 1.0) {
	goto L40;
    }

/*            PROCEDURE FOR a0 < 1 AND b0 <= 1 */

    ret_val = pow(x, a);
    if (ret_val == 0.0) {
	return ret_val;
    }

    apb = a + b;
    if (apb > 1.0) {
	goto L20;
    }
    z = gam1(apb) + 1.0;
    goto L30;
L20:
    u = a + b - 1.;
    z = (gam1(u) + 1.0) / apb;

L30:
    c = (gam1(a) + 1.0) * (gam1(b) + 1.0) / z;
    ret_val = ret_val * c * (b / apb);
    goto L70;

/*         PROCEDURE FOR a0 < 1 AND 1 < b0 < 8 */

L40:
    u = gamln1(a0);
    m = b0 - 1.0;
    if (m < 1) {
	goto L50;
    }
    c = 1.0;
    i1 = m;
    for (i = 1; i <= i1; ++i) {
	b0 += -1.0;
/* L41: */
	c *= b0 / (a0 + b0);
    }
    u = log(c) + u;

L50:
    z = a * log(x) - u;
    b0 += -1.0;
    apb = a0 + b0;
    if (apb > 1.0) {
	goto L51;
    }
    t = gam1(apb) + 1.0;
    goto L52;
L51:
    u = a0 + b0 - 1.;
    t = (gam1(u) + 1.0) / apb;
L52:
    ret_val = exp(z) * (a0 / a) * (gam1(b0) + 1.0) / t;
    goto L70;

/*            PROCEDURE FOR a0 < 1 AND b0 >= 8 */

L60:
    u = gamln1(a0) + algdiv(a0, b0);
    z = a * log(x) - u;
    ret_val = a0 / a * exp(z);
L70:
    if (ret_val == 0.0 || a <= eps * 0.1) {
	return ret_val;
    }
/* ----------------------------------------------------------------------- */
/*                     COMPUTE THE SERIES */
/* ----------------------------------------------------------------------- */
    sum = 0.0;
    n = 0.0;
    c = 1.0;
    tol = eps / a;
L100:
    n += 1.0;
    c = c * (0.5 - b / n + 0.5) * x;
    w = c / (a + n);
    sum += w;
    if (fabs(w) > tol) {
	goto L100;
    }
    ret_val *= a * sum + 1.0;
    return ret_val;
} /* bpser */

static double bup(double a, double b, double x, double y, int n, double eps)
{
    /* System generated locals */
    int i1;
    double ret_val, d1;

    /* Local variables */
    double d;
    int i, k;
    double l, r, t, w;
    int mu;
    double ap1;
    int nm1, kp1;
    double apb;

/* ----------------------------------------------------------------------- */
/*     EVALUATION OF IX(A,B) - IX(A+N,B) WHERE N IS A POSITIVE INT. */
/*     EPS IS THE TOLERANCE USED. */
/* ----------------------------------------------------------------------- */

/*          OBTAIN THE SCALING FACTOR EXP(-MU) AND */
/*             EXP(MU)*(X**A*Y**B/BETA(A,B))/A */

    apb = a + b;
    ap1 = a + 1.0;
    mu = 0;
    d = 1.0;
    if (n == 1 || a < 1.0) {
	goto L10;
    }
    if (apb < ap1 * 1.1) {
	goto L10;
    }
    mu = (d1 = exparg(1), (int) fabs(d1));
    k = (int) exparg(0);
    if (k < mu) {
	mu = k;
    }
    t = (double) mu;
    d = exp(-t);

L10:
    ret_val = brcmp1(mu, a, b, x, y) / a;
    if (n == 1 || ret_val == 0.0) {
	return ret_val;
    }
    nm1 = n - 1;
    w = d;

/*          LET K BE THE INDEX OF THE MAXIMUM TERM */

    k = 0;
    if (b <= 1.0) {
	goto L40;
    }
    if (y > 1e-4) {
	goto L20;
    }
    k = nm1;
    goto L30;
L20:
    r = (b - 1.0) * x / y - a;
    if (r < 1.0) {
	goto L40;
    }
    k = nm1;
    t = (double) nm1;
    if (r < t) {
	k = (int) r;
    }

/*          ADD THE INCREASING TERMS OF THE SERIES */

L30:
    i1 = k;
    for (i = 1; i <= i1; ++i) {
	l = (double) (i - 1);
	d = (apb + l) / (ap1 + l) * x * d;
	w += d;
/* L31: */
    }
    if (k == nm1) {
	goto L50;
    }

/*          ADD THE REMAINING TERMS OF THE SERIES */

L40:
    kp1 = k + 1;
    i1 = nm1;
    for (i = kp1; i <= i1; ++i) {
	l = (double) (i - 1);
	d = (apb + l) / (ap1 + l) * x * d;
	w += d;
	if (d <= eps * w) {
	    goto L50;
	}
/* L41: */
    }

/*               TERMINATE THE PROCEDURE */

L50:
    ret_val *= w;
    return ret_val;
} /* bup */

static double bfrac(double a, double b, double x, double y, double lambda, 
		    double eps)
{
    /* System generated locals */
    double ret_val, d1;

    /* Local variables */
    double c, e, n, p, r, s, t, w, c0, c1, r0, an, bn, yp1, anp1, bnp1,
	    beta, alpha;

/* ----------------------------------------------------------------------- */
/*     CONTINUED FRACTION EXPANSION FOR IX(A,B) WHEN A,B > 1. */
/*     IT IS ASSUMED THAT  LAMBDA = (A + B)*Y - B. */
/* ----------------------------------------------------------------------- */
/* -------------------- */
    ret_val = brcomp(a, b, x, y);
    if (ret_val == 0.0) {
	return ret_val;
    }

    c = lambda + 1.0;
    c0 = b / a;
    c1 = 1.0 / a + 1.0;
    yp1 = y + 1.0;

    n = 0.0;
    p = 1.0;
    s = a + 1.0;
    an = 0.0;
    bn = 1.0;
    anp1 = 1.0;
    bnp1 = c / c1;
    r = c1 / c;

/*        CONTINUED FRACTION CALCULATION */

L10:
    n += 1.0;
    t = n / a;
    w = n * (b - n) * x;
    e = a / s;
    alpha = p * (p + c0) * e * e * (w * x);
    e = (t + 1.0) / (c1 + t + t);
    beta = n + w / s + e * (c + n * yp1);
    p = t + 1.0;
    s += 2.0;

/*        UPDATE AN, BN, ANP1, AND BNP1 */

    t = alpha * an + beta * anp1;
    an = anp1;
    anp1 = t;
    t = alpha * bn + beta * bnp1;
    bn = bnp1;
    bnp1 = t;

    r0 = r;
    r = anp1 / bnp1;
    if ((d1 = r - r0, fabs(d1)) <= eps * r) {
	goto L20;
    }

/*        RESCALE AN, BN, ANP1, AND BNP1 */

    an /= bnp1;
    bn /= bnp1;
    anp1 = r;
    bnp1 = 1.0;
    goto L10;

/*                 TERMINATION */

L20:
    ret_val *= r;
    return ret_val;
} /* bfrac */

static double brcomp(double a, double b, double x, double y)
{
    /* Initialized data */

    static double const__ = .398942280401433;

    /* System generated locals */
    int i1;
    double ret_val;

    /* Local variables */
    double c, e, h;
    int i, n;
    double t, u, v, z, a0, b0, x0, y0, apb, lnx, lny;
    double lambda;

/* ----------------------------------------------------------------------- */
/*               EVALUATION OF X**A*Y**B/BETA(A,B) */
/* ----------------------------------------------------------------------- */
/* ----------------- */
/*     CONST = 1/SQRT(2*PI) */
/* ----------------- */

    ret_val = 0.0;
    if (x == 0.0 || y == 0.0) {
	return ret_val;
    }
    a0 = min(a, b);
    if (a0 >= 8.0) {
	goto L100;
    }

    if (x > .375) {
	goto L10;
    }
    lnx = log(x);
    lny = alnrel(-x);
    goto L20;
L10:
    if (y > .375) {
	goto L11;
    }
    lnx = alnrel(-y);
    lny = log(y);
    goto L20;
L11:
    lnx = log(x);
    lny = log(y);

L20:
    z = a * lnx + b * lny;
    if (a0 < 1.0) {
	goto L30;
    }
    z -= betaln(a, b);
    ret_val = exp(z);
    return ret_val;
/* ----------------------------------------------------------------------- */
/*              PROCEDURE FOR a < 1 OR b < 1 */
/* ----------------------------------------------------------------------- */
L30:
    b0 = max(a, b);
    if (b0 >= 8.0) {
	goto L80;
    }
    if (b0 > 1.0) {
	goto L60;
    }

/*                   ALGORITHM FOR b0 <= 1 */

    ret_val = exp(z);
    if (ret_val == 0.0) {
	return ret_val;
    }

    apb = a + b;
    if (apb > 1.0) {
	goto L40;
    }
    z = gam1(apb) + 1.0;
    goto L50;
L40:
    u = a + b - 1.;
    z = (gam1(u) + 1.0) / apb;

L50:
    c = (gam1(a) + 1.0) * (gam1(b) + 1.0) / z;
    ret_val = ret_val * (a0 * c) / (a0 / b0 + 1.0);
    return ret_val;

/*                ALGORITHM FOR 1 < b0 < 8 */

L60:
    u = gamln1(a0);
    n = b0 - 1.0;
    if (n < 1) {
	goto L70;
    }
    c = 1.0;
    i1 = n;
    for (i = 1; i <= i1; ++i) {
	b0 += -1.0;
	c *= b0 / (a0 + b0);
/* L61: */
    }
    u = log(c) + u;

L70:
    z -= u;
    b0 += -1.0;
    apb = a0 + b0;
    if (apb > 1.0) {
	goto L71;
    }
    t = gam1(apb) + 1.0;
    goto L72;
L71:
    u = a0 + b0 - 1.;
    t = (gam1(u) + 1.0) / apb;
L72:
    ret_val = a0 * exp(z) * (gam1(b0) + 1.0) / t;
    return ret_val;

/*                   ALGORITHM FOR b0 >= 8 */

L80:
    u = gamln1(a0) + algdiv(a0, b0);
    ret_val = a0 * exp(z - u);
    return ret_val;
/* ----------------------------------------------------------------------- */
/*              PROCEDURE FOR A >= 8 AND B >= 8 */
/* ----------------------------------------------------------------------- */
L100:
    if (a > b) {
	goto L101;
    }
    h = a / b;
    x0 = h / (h + 1.0);
    y0 = 1.0 / (h + 1.0);
    lambda = a - (a + b) * x;
    goto L110;
L101:
    h = b / a;
    x0 = 1.0 / (h + 1.0);
    y0 = h / (h + 1.0);
    lambda = (a + b) * y - b;

L110:
    e = -lambda / a;
    if (fabs(e) > .6) {
	goto L111;
    }
    u = rlog1(e);
    goto L120;
L111:
    u = e - log(x / x0);

L120:
    e = lambda / b;
    if (fabs(e) > .6) {
	goto L121;
    }
    v = rlog1(e);
    goto L130;
L121:
    v = e - log(y / y0);

L130:
    z = exp(-(a * u + b * v));
    ret_val = const__ * sqrt(b * x0) * z * exp(-bcorr(a, b));
    return ret_val;
} /* brcomp */

static double brcmp1(int mu, double a, double b, double x, double y)
{
    /* Initialized data */

    static double const__ = .398942280401433;

    /* System generated locals */
    int i1;
    double ret_val, r1;

    /* Local variables */
    double c, e, h;
    int i, n;
    double t, u, v, z, a0, b0, x0, y0, apb, lnx, lny;
    double lambda;

/* ----------------------------------------------------------------------- */
/*          EVALUATION OF  EXP(MU) * (X**A*Y**B/BETA(A,B)) */
/* ----------------------------------------------------------------------- */
/* ----------------- */
/*     CONST = 1/SQRT(2*PI) */
/* ----------------- */

    a0 = min(a,b);
    if (a0 >= 8.0) {
	goto L100;
    }

    if (x > .375) {
	goto L10;
    }
    lnx = log(x);
    lny = alnrel(-x);
    goto L20;
L10:
    if (y > .375) {
	goto L11;
    }
    lnx = alnrel(-y);
    lny = log(y);
    goto L20;
L11:
    lnx = log(x);
    lny = log(y);

L20:
    z = a * lnx + b * lny;
    if (a0 < 1.0) {
	goto L30;
    }
    z -= betaln(a, b);
    ret_val = esum(mu, z);
    return ret_val;
/* ----------------------------------------------------------------------- */
/*              PROCEDURE FOR A < 1 OR B < 1 */
/* ----------------------------------------------------------------------- */
L30:
    b0 = max(a,b);
    if (b0 >= 8.0) {
	goto L80;
    }
    if (b0 > 1.0) {
	goto L60;
    }

/*                   ALGORITHM FOR b0 <= 1 */

    ret_val = esum(mu, z);
    if (ret_val == 0.0) {
	return ret_val;
    }

    apb = a + b;
    if (apb > 1.0) {
	goto L40;
    }
    z = gam1(apb) + 1.0;
    goto L50;
L40:
    u = a + b - 1.;
    z = (gam1(u) + 1.0) / apb;

L50:
    c = (gam1(a) + 1.0) * (gam1(b) + 1.0) / z;
    ret_val = ret_val * (a0 * c) / (a0 / b0 + 1.0);
    return ret_val;

/*                ALGORITHM FOR 1 < b0 < 8 */

L60:
    u = gamln1(a0);
    n = b0 - 1.0;
    if (n < 1) {
	goto L70;
    }
    c = 1.0;
    i1 = n;
    for (i = 1; i <= i1; ++i) {
	b0 += -1.0;
	c *= b0 / (a0 + b0);
/* L61: */
    }
    u = log(c) + u;

L70:
    z -= u;
    b0 += -1.0;
    apb = a0 + b0;
    if (apb > 1.0) {
	goto L71;
    }
    t = gam1(apb) + 1.0;
    goto L72;
L71:
    u = a0 + b0 - 1.;
    t = (gam1(u) + 1.0) / apb;
L72:
    ret_val = a0 * esum(mu, z) * (gam1(b0) + 1.0) / t;
    return ret_val;

/*                   ALGORITHM FOR b0 >= 8 */

L80:
    u = gamln1(a0) + algdiv(a0, b0);
    ret_val = a0 * esum(mu, z - u);
    return ret_val;
/* ----------------------------------------------------------------------- */
/*              PROCEDURE FOR A >= 8 AND B >= 8 */
/* ----------------------------------------------------------------------- */
L100:
    if (a > b) {
	goto L101;
    }
    h = a / b;
    x0 = h / (h + 1.0);
    y0 = 1.0 / (h + 1.0);
    lambda = a - (a + b) * x;
    goto L110;
L101:
    h = b / a;
    x0 = 1.0 / (h + 1.0);
    y0 = h / (h + 1.0);
    lambda = (a + b) * y - b;

L110:
    e = -lambda / a;
    if (fabs(e) > 0.6) {
	goto L111;
    }
    u = rlog1(e);
    goto L120;
L111:
    u = e - log(x / x0);

L120:
    e = lambda / b;
    if (fabs(e) > 0.6) {
	goto L121;
    }
    v = rlog1(e);
    goto L130;
L121:
    v = e - log(y / y0);

L130:
    r1 = -(a * u + b * v);
    z = esum(mu, r1);
    ret_val = const__ * sqrt(b * x0) * z * exp(-bcorr(a, b));
    return ret_val;
} /* brcmp1 */

static void bgrat(double a, double b, double x, double y, double *w,
		  double eps, int *ierr)
{
    /* System generated locals */
    int i1;
    double r1;

    /* Local variables */
    double c[30], d[30];
    int i;
    double j, l;
    int n;
    double p, q, r, s, t, u, v, z, n2, t2, dj, cn, nu, bm1;
    int nm1;
    double lnx, sum;
    double bp2n, coef;

/* ----------------------------------------------------------------------- */
/*     ASYMPTOTIC EXPANSION FOR IX(A,B) WHEN A IS LARGER THAN B. */
/*     THE RESULT OF THE EXPANSION IS ADDED TO W. IT IS ASSUMED */
/*     THAT A >= 15 AND B <= 1.  EPS IS THE TOLERANCE USED. */
/*     IERR IS A VARIABLE THAT REPORTS THE STATUS OF THE RESULTS. */
/* ----------------------------------------------------------------------- */

    bm1 = b - 0.5 - 0.5;
    nu = a + bm1 * 0.5;
    if (y > 0.375) {
	goto L10;
    }
    lnx = alnrel(-y);
    goto L11;
L10:
    lnx = log(x);
L11:
    z = -nu * lnx;
    if (b * z == 0.0) {
	goto L100;
    }

/*                 COMPUTATION OF THE EXPANSION */
/*                 SET R = EXP(-Z)*Z**B/GAMMA(B) */

    r = b * (gam1(b) + 1.0) * exp(b * log(z));
    r = r * exp(a * lnx) * exp(bm1 * 0.5 * lnx);
    u = algdiv(b, a) + b * log(nu);
    u = r * exp(-u);
    if (u == 0.0) {
	goto L100;
    }
    grat1(b, z, r, &p, &q, eps);

/* Computing 2nd power */
    r1 = 1.0 / nu;
    v = r1 * r1 * 0.25;
    t2 = lnx * 0.25 * lnx;
    l = *w / u;
    j = q / r;
    sum = j;
    t = 1.0;
    cn = 1.0;
    n2 = 0.0;
    for (n = 1; n <= 30; ++n) {
	bp2n = b + n2;
	j = (bp2n * (bp2n + 1.0) * j + (z + bp2n + 1.0) * t) * v;
	n2 += 2.0;
	t *= t2;
	cn /= n2 * (n2 + 1.0);
	c[n - 1] = cn;
	s = 0.0;
	if (n == 1) {
	    goto L21;
	}
	nm1 = n - 1;
	coef = b - n;
	i1 = nm1;
	for (i = 1; i <= i1; ++i) {
	    s += coef * c[i - 1] * d[n - i - 1];
/* L20: */
	    coef += b;
	}
L21:
	d[n - 1] = bm1 * cn + s / n;
	dj = d[n - 1] * j;
	sum += dj;
	if (sum <= 0.0) {
	    goto L100;
	}
	if (fabs(dj) <= eps * (sum + l)) {
	    goto L30;
	}
/* L22: */
    }

/*                    ADD THE RESULTS TO W */

L30:
    *ierr = 0;
    *w += u * sum;
    return;

/*               THE EXPANSION CANNOT BE COMPUTED */

L100:
    *ierr = 1;
    return;
} /* bgrat */

static void grat1(double a, double x, double r, double *p, double *q,
		  double eps)
{
    /* Local variables */
    double c, g, h, j, l, t, w, z, an, am0, an0, a2n, b2n, cma;
    double tol, sum;
    double a2nm1, b2nm1;

/* ----------------------------------------------------------------------- */
/*        EVALUATION OF THE INCOMPLETE GAMMA RATIO FUNCTIONS */
/*                      P(A,X) AND Q(A,X) */

/*     IT IS ASSUMED THAT A <= 1.  EPS IS THE TOLERANCE TO BE USED. */
/*     THE INPUT ARGUMENT R HAS THE VALUE E**(-X)*X**A/GAMMA(A). */
/* ----------------------------------------------------------------------- */
    if (a * x == 0.0) {
	goto L130;
    }
    if (a == 0.5) {
	goto L120;
    }
    if (x < 1.1) {
	goto L10;
    }
    goto L50;

/*             TAYLOR SERIES FOR P(A,X)/X**A */

L10:
    an = 3.0;
    c = x;
    sum = x / (a + 3.0);
    tol = eps * 0.1 / (a + 1.0);
L11:
    an += 1.0;
    c = -c * (x / an);
    t = c / (a + an);
    sum += t;
    if (fabs(t) > tol) {
	goto L11;
    }
    j = a * x * ((sum / 6.0 - 0.5 / (a + 2.0)) * x + 1.0 / (a + 1.0));

    z = a * log(x);
    h = gam1(a);
    g = h + 1.0;
    if (x < 0.25) {
	goto L20;
    }
    if (a < x / 2.59) {
	goto L40;
    }
    goto L30;
L20:
    if (z > -0.13394) {
	goto L40;
    }

L30:
    w = exp(z);
    *p = w * g * (0.5 - j + 0.5);
    *q = 0.5 - *p + 0.5;
    return;

L40:
    l = rexpm1(z);
    w = l + 0.5 + 0.5;
    *q = (w * j - l) * g - h;
    if (*q < 0.0) {
	goto L110;
    }
    *p = 0.5 - *q + 0.5;
    return;

/*              CONTINUED FRACTION EXPANSION */

L50:
    a2nm1 = 1.0;
    a2n = 1.0;
    b2nm1 = x;
    b2n = x + (1.0 - a);
    c = 1.0;
L51:
    a2nm1 = x * a2n + c * a2nm1;
    b2nm1 = x * b2n + c * b2nm1;
    am0 = a2nm1 / b2nm1;
    c += 1.0;
    cma = c - a;
    a2n = a2nm1 + cma * a2n;
    b2n = b2nm1 + cma * b2n;
    an0 = a2n / b2n;
    if (fabs(an0 - am0) >= eps * an0) {
	goto L51;
    }
    *q = r * an0;
    *p = 0.5 - *q + 0.5;
    return;

/*                SPECIAL CASES */

L100:
    *p = 0.0;
    *q = 1.0;
    return;

L110:
    *p = 1.0;
    *q = 0.0;
    return;

L120:
    if (x >= 0.25) {
	goto L121;
    }
    *p = erf__(sqrt(x));
    *q = 0.5 - *p + 0.5;
    return;
L121:
    *q = erfc1(0, sqrt(x));
    *p = 0.5 - *q + 0.5;
    return;

L130:
    if (x <= a) {
	goto L100;
    }
    goto L110;
} /* grat1 */

static double basym(double a, double b, double lambda, double eps)
{
    /* Initialized data */

    static int num = 20;
    static double e0 = 1.12837916709551;
    static double e1 = .353553390593274;

    /* System generated locals */
    int i1, i2, i3, i4;
    double ret_val;

    /* Local variables */
    double c[21], d[21], f, h;
    int i, j, m, n;
    double r, s, t, u, w, z, a0[21], b0[21], j0, j1, h2, r0, r1, t0, t1, w0,
	     z0, z2, hn, zn;
    int im1, mm1, np1, imj, mmj;
    double sum, znm1, bsum, dsum;

/* ----------------------------------------------------------------------- */
/*     ASYMPTOTIC EXPANSION FOR IX(A,B) FOR LARGE A AND B. */
/*     LAMBDA = (A + B)*Y - B  AND EPS IS THE TOLERANCE USED. */
/*     IT IS ASSUMED THAT LAMBDA IS NONNEGATIVE AND THAT */
/*     A AND B ARE GREATER THAN OR EQUAL TO 15. */
/* ----------------------------------------------------------------------- */
/* ------------------------ */
/*     ****** NUM IS THE MAXIMUM VALUE THAT N CAN TAKE IN THE DO LOOP */
/*            ENDING AT STATEMENT 50. IT IS REQUIRED THAT NUM BE EVEN. */
/*            THE ARRAYS A0, B0, C, D HAVE DIMENSION NUM + 1. */

/* ------------------------ */
/*     E0 = 2/SQRT(PI) */
/*     E1 = 2**(-3/2) */
/* ------------------------ */
/* ------------------------ */
    ret_val = 0.0;
    if (a >= b) {
	goto L10;
    }
    h = a / b;
    r0 = 1.0 / (h + 1.0);
    r1 = (b - a) / b;
    w0 = 1.0 / sqrt(a * (h + 1.0));
    goto L20;
L10:
    h = b / a;
    r0 = 1.0 / (h + 1.0);
    r1 = (b - a) / a;
    w0 = 1.0 / sqrt(b * (h + 1.0));

L20:
    f = a * rlog1(-lambda/a) + b * rlog1(lambda/b);
    t = exp(-f);
    if (t == 0.0) {
	return ret_val;
    }
    z0 = sqrt(f);
    z = z0 / e1 * 0.5;
    z2 = f + f;

    a0[0] = r1 * .66666666666666663;
    c[0] = a0[0] * -0.5;
    d[0] = -c[0];
    j0 = 0.5 / e0 * erfc1(1, z0);
    j1 = e1;
    sum = j0 + d[0] * w0 * j1;

    s = 1.0;
    h2 = h * h;
    hn = 1.0;
    w = w0;
    znm1 = z;
    zn = z2;
    i1 = num;
    for (n = 2; n <= i1; n += 2) {
	hn = h2 * hn;
	a0[n - 1] = r0 * 2.0 * (h * hn + 1.0) / (n + 2.0);
	np1 = n + 1;
	s += hn;
	a0[np1 - 1] = r1 * 2.0 * s / (n + 3.0);

	i2 = np1;
	for (i = n; i <= i2; ++i) {
	    r = (i + 1.0) * -0.5;
	    b0[0] = r * a0[0];
	    i3 = i;
	    for (m = 2; m <= i3; ++m) {
		bsum = 0.0;
		mm1 = m - 1;
		i4 = mm1;
		for (j = 1; j <= i4; ++j) {
		    mmj = m - j;
/* L30: */
		    bsum += (j * r - mmj) * a0[j - 1] * b0[mmj - 1];
		}
/* L31: */
		b0[m - 1] = r * a0[m - 1] + bsum / m;
	    }
	    c[i - 1] = b0[i - 1] / (i + 1.0);

	    dsum = 0.0;
	    im1 = i - 1;
	    i3 = im1;
	    for (j = 1; j <= i3; ++j) {
		imj = i - j;
/* L40: */
		dsum += d[imj - 1] * c[j - 1];
	    }
/* L41: */
	    d[i - 1] = -(dsum + c[i - 1]);
	}

	j0 = e1 * znm1 + (n - 1.0) * j0;
	j1 = e1 * zn + n * j1;
	znm1 = z2 * znm1;
	zn = z2 * zn;
	w = w0 * w;
	t0 = d[n - 1] * w * j0;
	w = w0 * w;
	t1 = d[np1 - 1] * w * j1;
	sum += t0 + t1;
	if (fabs(t0) + fabs(t1) <= eps * sum) {
	    goto L60;
	}
/* L50: */
    }

L60:
    u = exp(-bcorr(a, b));
    ret_val = e0 * t * u * sum;
    return ret_val;
} /* basym_ */


static double exparg(int l)
{
    /* Local variables */
    int m;
    double lnb;

/* -------------------------------------------------------------------- */
/*     IF L = 0 THEN  EXPARG(L) = THE LARGEST POSITIVE W FOR WHICH */
/*     EXP(W) CAN BE COMPUTED. */

/*     IF L IS NONZERO THEN  EXPARG(L) = THE LARGEST NEGATIVE W FOR */
/*     WHICH THE COMPUTED VALUE OF EXP(W) IS NONZERO. */

/*     NOTE... ONLY AN APPROXIMATE VALUE FOR EXPARG(L) IS NEEDED. */
/* -------------------------------------------------------------------- */

    lnb = .69314718055995;
    if (l == 0) {
	m = Rf_i1mach(16);
	return m * lnb * .99999;
    }
    m = Rf_i1mach(15) - 1;
    return m * lnb * .99999;
} /* exparg */

static double esum(int mu, double x)
{
    /* System generated locals */
    double ret_val;

    /* Local variables */
    double w;

/* ----------------------------------------------------------------------- */
/*                    EVALUATION OF EXP(MU + X) */
/* ----------------------------------------------------------------------- */
    if (x > 0.0) {
	goto L10;
    }

    if (mu < 0) {
	goto L20;
    }
    w = mu + x;
    if (w > 0.0) {
	goto L20;
    }
    ret_val = exp(w);
    return ret_val;

L10:
    if (mu > 0) {
	goto L20;
    }
    w = mu + x;
    if (w < 0.0) {
	goto L20;
    }
    ret_val = exp(w);
    return ret_val;

L20:
    w = (double) (mu);
    ret_val = exp(w) * exp(x);
    return ret_val;
} /* esum */

double rexpm1(double x)
{
    /* Initialized data */

    static double p1 = 9.14041914819518e-10;
    static double p2 = .0238082361044469;
    static double q1 = -.499999999085958;
    static double q2 = .107141568980644;
    static double q3 = -.0119041179760821;
    static double q4 = 5.95130811860248e-4;

    /* System generated locals */
    double ret_val;

    /* Local variables */
    double w;

/* ----------------------------------------------------------------------- */
/*            EVALUATION OF THE FUNCTION EXP(X) - 1 */
/* ----------------------------------------------------------------------- */
/* ----------------------- */
    if (fabs(x) > 0.15) {
	goto L10;
    }
    ret_val = x * (((p2 * x + p1) * x + 1.0) / ((((q4 * x + q3) * x + q2)
	     * x + q1) * x + 1.0));
    return ret_val;

L10:
    w = exp(x);
    if (x > 0.0) {
	goto L20;
    }
    ret_val = w - 0.5 - 0.5;
    return ret_val;
L20:
    ret_val = w * (0.5 - 1.0 / w + 0.5);
    return ret_val;
} /* rexpm1 */

static double alnrel(double a)
{
    /* Initialized data */

    static double p1 = -1.29418923021993;
    static double p2 = .405303492862024;
    static double p3 = -.0178874546012214;
    static double q1 = -1.62752256355323;
    static double q2 = .747811014037616;
    static double q3 = -.0845104217945565;

    /* System generated locals */
    double ret_val;

    /* Local variables */
    double t, w, x, t2;

/* ----------------------------------------------------------------------- */
/*            EVALUATION OF THE FUNCTION LN(1 + A) */
/* ----------------------------------------------------------------------- */
/* -------------------------- */
    if (fabs(a) > 0.375) {
	goto L10;
    }
    t = a / (a + 2.0);
    t2 = t * t;
    w = (((p3 * t2 + p2) * t2 + p1) * t2 + 1.0) / (((q3 * t2 + q2) * t2 + q1)
	    * t2 + 1.0);
    ret_val = t * 2.0 * w;
    return ret_val;

L10:
    x = a + 1.;
    ret_val = log(x);
    return ret_val;
} /* alnrel */

static double rlog1(double x)
{
    /* Initialized data */

    static double a = .0566749439387324;
    static double b = .0456512608815524;
    static double p0 = .333333333333333;
    static double p1 = -.224696413112536;
    static double p2 = .00620886815375787;
    static double q1 = -1.27408923933623;
    static double q2 = .354508718369557;

    /* System generated locals */
    double ret_val;

    /* Local variables */
    double h, r, t, w, w1;

/* ----------------------------------------------------------------------- */
/*             EVALUATION OF THE FUNCTION X - LN(1 + X) */
/* ----------------------------------------------------------------------- */
/* ------------------------ */
/* ------------------------ */
    if (x < -0.39 || x > 0.57) {
	goto L100;
    }
    if (x < -0.18) {
	goto L10;
    }
    if (x > 0.18) {
	goto L20;
    }

/*              ARGUMENT REDUCTION */

    h = x;
    w1 = 0.0;
    goto L30;

L10:
    h = x + .3;
    h /= .7;
    w1 = a - h * .3;
    goto L30;

L20:
    h = x * .75 - .25;
    w1 = b + h / 3.0;

/*               SERIES EXPANSION */

L30:
    r = h / (h + 2.0);
    t = r * r;
    w = ((p2 * t + p1) * t + p0) / ((q2 * t + q1) * t + 1.0);
    ret_val = t * 2.0 * (1.0 / (1.0 - r) - r * w) + w1;
    return ret_val;


L100:
    w = x + 0.5 + 0.5;
    ret_val = x - log(w);
    return ret_val;
} /* rlog1 */

static double erf__(double x)
{
    /* Initialized data */

    static double c = .564189583547756;
    static double a[5] = { 7.7105849500132e-5,-.00133733772997339,
	    .0323076579225834,.0479137145607681,.128379167095513 };
    static double b[3] = { .00301048631703895,.0538971687740286,
	    .375795757275549 };
    static double p[8] = { -1.36864857382717e-7,.564195517478974,
	    7.21175825088309,43.1622272220567,152.98928504694,
	    339.320816734344,451.918953711873,300.459261020162 };
    static double q[8] = { 1.,12.7827273196294,77.0001529352295,
	    277.585444743988,638.980264465631,931.35409485061,
	    790.950925327898,300.459260956983 };
    static double r[5] = { 2.10144126479064,26.2370141675169,
	    21.3688200555087,4.6580782871847,.282094791773523 };
    static double s[4] = { 94.153775055546,187.11481179959,
	    99.0191814623914,18.0124575948747 };

    /* System generated locals */
    double ret_val;

    /* Local variables */
    double t, x2, ax, bot, top;

/* ----------------------------------------------------------------------- */
/*             EVALUATION OF THE REAL ERROR FUNCTION */
/* ----------------------------------------------------------------------- */
/* ------------------------- */
/* ------------------------- */
/* ------------------------- */
/* ------------------------- */
/* ------------------------- */
    ax = fabs(x);
    if (ax > 0.5) {
	goto L10;
    }
    t = x * x;
    top = (((a[0] * t + a[1]) * t + a[2]) * t + a[3]) * t + a[4] + 1.0;
    bot = ((b[0] * t + b[1]) * t + b[2]) * t + 1.0;
    ret_val = x * (top / bot);
    return ret_val;

L10:
    if (ax > 4.0) {
	goto L20;
    }
    top = ((((((p[0] * ax + p[1]) * ax + p[2]) * ax + p[3]) * ax + p[4]) * ax
	    + p[5]) * ax + p[6]) * ax + p[7];
    bot = ((((((q[0] * ax + q[1]) * ax + q[2]) * ax + q[3]) * ax + q[4]) * ax
	    + q[5]) * ax + q[6]) * ax + q[7];
    ret_val = 0.5 - exp(-x * x) * top / bot + 0.5;
    if (x < 0.0) {
	ret_val = -ret_val;
    }
    return ret_val;

L20:
    if (ax >= 5.8) {
	goto L30;
    }
    x2 = x * x;
    t = 1.0 / x2;
    top = (((r[0] * t + r[1]) * t + r[2]) * t + r[3]) * t + r[4];
    bot = (((s[0] * t + s[1]) * t + s[2]) * t + s[3]) * t + 1.0;
    ret_val = (c - top / (x2 * bot)) / ax;
    ret_val = 0.5 - exp(-x2) * ret_val + 0.5;
    if (x < 0.0) {
	ret_val = -ret_val;
    }
    return ret_val;

L30:
    ret_val = x > 0 ? 1 : -1;
    return ret_val;
} /* erf */

static double erfc1(int ind, double x)
{
    /* Initialized data */

    static double c = .564189583547756;
    static double a[5] = { 7.7105849500132e-5,-.00133733772997339,
	    .0323076579225834,.0479137145607681,.128379167095513 };
    static double b[3] = { .00301048631703895,.0538971687740286,
	    .375795757275549 };
    static double p[8] = { -1.36864857382717e-7,.564195517478974,
	    7.21175825088309,43.1622272220567,152.98928504694,
	    339.320816734344,451.918953711873,300.459261020162 };
    static double q[8] = { 1.,12.7827273196294,77.0001529352295,
	    277.585444743988,638.980264465631,931.35409485061,
	    790.950925327898,300.459260956983 };
    static double r[5] = { 2.10144126479064,26.2370141675169,
	    21.3688200555087,4.6580782871847,.282094791773523 };
    static double s[4] = { 94.153775055546,187.11481179959,
	    99.0191814623914,18.0124575948747 };

    /* System generated locals */
    double ret_val, d1;

    /* Local variables */
    double e, t, w, ax, bot, top;

/* ----------------------------------------------------------------------- */
/*         EVALUATION OF THE COMPLEMENTARY ERROR FUNCTION */

/*          ERFC1(IND,X) = ERFC(X)            IF IND = 0 */
/*          ERFC1(IND,X) = EXP(X*X)*ERFC(X)   OTHERWISE */
/* ----------------------------------------------------------------------- */
/* ------------------------- */
/* ------------------------- */
/* ------------------------- */
/* ------------------------- */
/* ------------------------- */

/*                     ABS(X) <= 0.5 */

    ax = fabs(x);
    if (ax > 0.5) {
	goto L10;
    }
    t = x * x;
    top = (((a[0] * t + a[1]) * t + a[2]) * t + a[3]) * t + a[4] + 1.0;
    bot = ((b[0] * t + b[1]) * t + b[2]) * t + 1.0;
    ret_val = 0.5 - x * (top / bot) + 0.5;
    if (ind != 0) {
	ret_val = exp(t) * ret_val;
    }
    return ret_val;

/*                  0.5 < ABS(X) <= 4 */

L10:
    if (ax > 4.0) {
	goto L20;
    }
    top = ((((((p[0] * ax + p[1]) * ax + p[2]) * ax + p[3]) * ax + p[4]) * ax
	    + p[5]) * ax + p[6]) * ax + p[7];
    bot = ((((((q[0] * ax + q[1]) * ax + q[2]) * ax + q[3]) * ax + q[4]) * ax
	    + q[5]) * ax + q[6]) * ax + q[7];
    ret_val = top / bot;
    goto L40;

/*                      ABS(X) > 4 */

L20:
    if (x <= -5.6) {
	goto L50;
    }
    if (ind != 0) {
	goto L30;
    }
    if (x > 100.0) {
	goto L60;
    }
    if (x * x > -exparg(1)) {
	goto L60;
    }

L30:
/* Computing 2nd power */
    d1 = 1.0 / x;
    t = d1 * d1;
    top = (((r[0] * t + r[1]) * t + r[2]) * t + r[3]) * t + r[4];
    bot = (((s[0] * t + s[1]) * t + s[2]) * t + s[3]) * t + 1.0;
    ret_val = (c - t * top / bot) / ax;

/*                      FINAL ASSEMBLY */

L40:
    if (ind == 0) {
	goto L41;
    }
    if (x < 0.0) {
	ret_val = exp(x * x) * 2.0 - ret_val;
    }
    return ret_val;
L41:
    w = x * x;
    t = w;
    e = w - t;
    ret_val = (0.5 - e + 0.5) * exp(-t) * ret_val;
    if (x < 0.0) {
	ret_val = 2.0 - ret_val;
    }
    return ret_val;

/*             LIMIT VALUE FOR LARGE NEGATIVE X */

L50:
    ret_val = 2.0;
    if (ind != 0) {
	ret_val = exp(x * x) * 2.0;
    }
    return ret_val;

/*             LIMIT VALUE FOR LARGE POSITIVE X */
/*                       WHEN IND = 0 */

L60:
    ret_val = 0.0;
    return ret_val;
} /* erfc1 */

static double gam1(double a)
{
    /* Initialized data */

    static double p[7] = { .577215664901533,-.409078193005776,
	    -.230975380857675,.0597275330452234,.0076696818164949,
	    -.00514889771323592,5.89597428611429e-4 };
    static double q[5] = { 1.,.427569613095214,.158451672430138,
	    .0261132021441447,.00423244297896961 };
    static double r[9] = { -.422784335098468,-.771330383816272,
	    -.244757765222226,.118378989872749,9.30357293360349e-4,
	    -.0118290993445146,.00223047661158249,2.66505979058923e-4,
	    -1.32674909766242e-4 };
    static double s1 = .273076135303957;
    static double s2 = .0559398236957378;

    /* System generated locals */
    double ret_val;

    /* Local variables */
    double d, t, w, bot, top;

/*     ------------------------------------------------------------------ */
/*     COMPUTATION OF 1/GAMMA(A+1) - 1  FOR -0.5 <= A <= 1.5 */
/*     ------------------------------------------------------------------ */
/*     ------------------- */
/*     ------------------- */
/*     ------------------- */
/*     ------------------- */
/*     ------------------- */
    t = a;
    d = a - 0.5;
    if (d > 0.0) {
	t = d - 0.5;
    }
    if (t < 0.0) {
	goto L30;
    } else if (t == 0) {
	goto L10;
    } else {
	goto L20;
    }

L10:
    ret_val = 0.0;
    return ret_val;

L20:
    top = (((((p[6] * t + p[5]) * t + p[4]) * t + p[3]) * t + p[2]) * t + p[1]
	    ) * t + p[0];
    bot = (((q[4] * t + q[3]) * t + q[2]) * t + q[1]) * t + 1.0;
    w = top / bot;
    if (d > 0.0) {
	goto L21;
    }
    ret_val = a * w;
    return ret_val;
L21:
    ret_val = t / a * (w - 0.5 - 0.5);
    return ret_val;

L30:
    top = (((((((r[8] * t + r[7]) * t + r[6]) * t + r[5]) * t + r[4]
	    ) * t + r[3]) * t + r[2]) * t + r[1]) * t + r[0];
    bot = (s2 * t + s1) * t + 1.0;
    w = top / bot;
    if (d > 0.0) {
	goto L31;
    }
    ret_val = a * (w + 0.5 + 0.5);
    return ret_val;
L31:
    ret_val = t * w / a;
    return ret_val;
} /* gam1 */

static double gamln1(double a)
{
    /* Initialized data */

    static double p0 = .577215664901533;
    static double p1 = .844203922187225;
    static double p2 = -.168860593646662;
    static double p3 = -.780427615533591;
    static double p4 = -.402055799310489;
    static double p5 = -.0673562214325671;
    static double p6 = -.00271935708322958;
    static double q1 = 2.88743195473681;
    static double q2 = 3.12755088914843;
    static double q3 = 1.56875193295039;
    static double q4 = .361951990101499;
    static double q5 = .0325038868253937;
    static double q6 = 6.67465618796164e-4;
    static double r0 = .422784335098467;
    static double r1 = .848044614534529;
    static double r2 = .565221050691933;
    static double r3 = .156513060486551;
    static double r4 = .017050248402265;
    static double r5 = 4.97958207639485e-4;
    static double s1 = 1.24313399877507;
    static double s2 = .548042109832463;
    static double s3 = .10155218743983;
    static double s4 = .00713309612391;
    static double s5 = 1.16165475989616e-4;

    /* System generated locals */
    double ret_val;

    /* Local variables */
    double w, x;

/* ----------------------------------------------------------------------- */
/*     EVALUATION OF LN(GAMMA(1 + A)) FOR -0.2 <= A <= 1.25 */
/* ----------------------------------------------------------------------- */
/* ---------------------- */
/* ---------------------- */
    if (a >= 0.6) {
	goto L10;
    }
    w = ((((((p6 * a + p5) * a + p4) * a + p3) * a + p2) * a + p1) * a
	    + p0) / ((((((q6 * a + q5) * a + q4) * a + q3) * a + q2) * a
	    + q1) * a + 1.0);
    ret_val = -(a) * w;
    return ret_val;

L10:
    x = a - 0.5 - 0.5;
    w = (((((r5 * x + r4) * x + r3) * x + r2) * x + r1) * x + r0) / (((((s5 *
	    x + s4) * x + s3) * x + s2) * x + s1) * x + 1.0);
    ret_val = x * w;
    return ret_val;
} /* gamln1 */

static double psi(double x)
{
    /* Initialized data */

    static double piov4 = .785398163397448;
    static double dx0 = 1.461632144968362341262659542325721325;
    static double p1[7] = { .0089538502298197,4.77762828042627,
	    142.441585084029,1186.45200713425,3633.51846806499,
	    4138.10161269013,1305.60269827897 };
    static double q1[6] = { 44.8452573429826,520.752771467162,
	    2210.0079924783,3641.27349079381,1908.310765963,
	    6.91091682714533e-6 };
    static double p2[4] = { -2.12940445131011,-7.01677227766759,
	    -4.48616543918019,-.648157123766197 };
    static double q2[4] = { 32.2703493791143,89.2920700481861,
	    54.6117738103215,7.77788548522962 };

    /* System generated locals */
    double ret_val, d1, d2;

    /* Local variables */
    int i, m, n;
    double w, z;
    int nq;
    double den, aug, sgn, xmx0, xmax1, upper;
    double xsmall;

/* --------------------------------------------------------------------- */

/*                 EVALUATION OF THE DIGAMMA FUNCTION */

/*                           ----------- */

/*     PSI(XX) IS ASSIGNED THE VALUE 0 WHEN THE DIGAMMA FUNCTION CANNOT */
/*     BE COMPUTED. */

/*     THE MAIN COMPUTATION INVOLVES EVALUATION OF RATIONAL CHEBYSHEV */
/*     APPROXIMATIONS PUBLISHED IN MATH. COMP. 27, 123-127(1973) BY */
/*     CODY, STRECOK AND THACHER. */

/* --------------------------------------------------------------------- */
/*     PSI WAS WRITTEN AT ARGONNE NATIONAL LABORATORY FOR THE FUNPACK */
/*     PACKAGE OF SPECIAL FUNCTION SUBROUTINES. PSI WAS MODIFIED BY */
/*     A.H. MORRIS (NSWC). */
/* --------------------------------------------------------------------- */
/* --------------------------------------------------------------------- */

/*     PIOV4 = PI/4 */
/*     DX0 = ZERO OF PSI TO EXTENDED PRECISION */

/* --------------------------------------------------------------------- */
/* --------------------------------------------------------------------- */

/*     COEFFICIENTS FOR RATIONAL APPROXIMATION OF */
/*     PSI(X) / (X - X0),  0.5 <= X <= 3.0 */

/* --------------------------------------------------------------------- */
/* --------------------------------------------------------------------- */

/*     COEFFICIENTS FOR RATIONAL APPROXIMATION OF */
/*     PSI(X) - LN(X) + 1 / (2*X),  X > 3.0 */

/* --------------------------------------------------------------------- */
/* --------------------------------------------------------------------- */

/*     MACHINE DEPENDENT CONSTANTS ... */

/*        XMAX1  = THE SMALLEST POSITIVE FLOATING POINT CONSTANT */
/*                 WITH ENTIRELY INT REPRESENTATION.  ALSO USED */
/*                 AS NEGATIVE OF LOWER BOUND ON ACCEPTABLE NEGATIVE */
/*                 ARGUMENTS AND AS THE POSITIVE ARGUMENT BEYOND WHICH */
/*                 PSI MAY BE REPRESENTED AS LOG(X). */

/*        XSMALL = ABSOLUTE ARGUMENT BELOW WHICH PI*COTAN(PI*X) */
/*                 MAY BE REPRESENTED BY 1/X. */

/* --------------------------------------------------------------------- */
    xmax1 = Rf_d1mach(4) - 1.0;
/* Computing MIN */
    d1 = xmax1, d2 = 0.5 / Rf_d1mach(3);;
    xmax1 = min(d1,d2);
    xsmall = 1e-9;
/* --------------------------------------------------------------------- */
    aug = 0.0;
    if (x >= 0.5) {
	goto L200;
    }
/* --------------------------------------------------------------------- */
/*     X < 0.5,  USE REFLECTION FORMULA */
/*     PSI(1-X) = PSI(X) + PI * COTAN(PI*X) */
/* --------------------------------------------------------------------- */
    if (fabs(x) > xsmall) {
	goto L100;
    }
    if (x == 0.0) {
	goto L400;
    }
/* --------------------------------------------------------------------- */
/*     0 < ABS(X) <= XSMALL.  USE 1/X AS A SUBSTITUTE */
/*     FOR  PI*COTAN(PI*X) */
/* --------------------------------------------------------------------- */
    aug = -1.0 / x;
    goto L150;
/* --------------------------------------------------------------------- */
/*     REDUCTION OF ARGUMENT FOR COTAN */
/* --------------------------------------------------------------------- */
L100:
    w = -x;
    sgn = piov4;
    if (w > 0.0) {
	goto L120;
    }
    w = -w;
    sgn = -sgn;
/* --------------------------------------------------------------------- */
/*     MAKE AN ERROR EXIT IF X <= -XMAX1 */
/* --------------------------------------------------------------------- */
L120:
    if (w >= xmax1) {
	goto L400;
    }
    nq = (int) w;
    w -= (double) nq;
    nq = (int) (w * 4.0);
    w = (w - (double) nq * 0.25) * 4.0;
/* --------------------------------------------------------------------- */
/*     W IS NOW RELATED TO THE FRACTIONAL PART OF  4.0 * X. */
/*     ADJUST ARGUMENT TO CORRESPOND TO VALUES IN FIRST */
/*     QUADRANT AND DETERMINE SIGN */
/* --------------------------------------------------------------------- */
    n = nq / 2;
    if (n + n != nq) {
	w = 1.0 - w;
    }
    z = piov4 * w;
    m = n / 2;
    if (m + m != n) {
	sgn = -sgn;
    }
/* --------------------------------------------------------------------- */
/*     DETERMINE FINAL VALUE FOR  -PI*COTAN(PI*X) */
/* --------------------------------------------------------------------- */
    n = (nq + 1) / 2;
    m = n / 2;
    m += m;
    if (m != n) {
	goto L140;
    }
/* --------------------------------------------------------------------- */
/*     CHECK FOR SINGULARITY */
/* --------------------------------------------------------------------- */
    if (z == 0.0) {
	goto L400;
    }
/* --------------------------------------------------------------------- */
/*     USE COS/SIN AS A SUBSTITUTE FOR COTAN, AND */
/*     SIN/COS AS A SUBSTITUTE FOR TAN */
/* --------------------------------------------------------------------- */
    aug = sgn * (cos(z) / sin(z) * 4.0);
    goto L150;
L140:
    aug = sgn * (sin(z) / cos(z) * 4.0);
L150:
    x = 1.0 - x;
L200:
    if (x > 3.0) {
	goto L300;
    }
/* --------------------------------------------------------------------- */
/*     0.5 <= X <= 3.0 */
/* --------------------------------------------------------------------- */
    den = x;
    upper = p1[0] * x;

    for (i = 1; i <= 5; ++i) {
	den = (den + q1[i - 1]) * x;
	upper = (upper + p1[i]) * x;
/* L210: */
    }

    den = (upper + p1[6]) / (den + q1[5]);
    xmx0 = x - dx0;
    ret_val = den * xmx0 + aug;
    return ret_val;
/* --------------------------------------------------------------------- */
/*     IF X >= XMAX1, PSI = LN(X) */
/* --------------------------------------------------------------------- */
L300:
    if (x >= xmax1) {
	goto L350;
    }
/* --------------------------------------------------------------------- */
/*     3.0 < X < XMAX1 */
/* --------------------------------------------------------------------- */
    w = 1.0 / (x * x);
    den = w;
    upper = p2[0] * w;

    for (i = 1; i <= 3; ++i) {
	den = (den + q2[i - 1]) * w;
	upper = (upper + p2[i]) * w;
/* L310: */
    }

    aug = upper / (den + q2[3]) - 0.5 / x + aug;
L350:
    ret_val = aug + log(x);
    return ret_val;
/* --------------------------------------------------------------------- */
/*     ERROR RETURN */
/* --------------------------------------------------------------------- */
L400:
    ret_val = 0.0;
    return ret_val;
} /* psi */

static double betaln(double a0, double b0)
{
    /* Initialized data */

    static double e = .918938533204673;

    /* System generated locals */
    int i1;
    double ret_val;

    /* Local variables */
    double a, b, c, h;
    int i, n;
    double u, v, w, z;

/* ----------------------------------------------------------------------- */
/*     EVALUATION OF THE LOGARITHM OF THE BETA FUNCTION */
/* ----------------------------------------------------------------------- */
/*     E = 0.5*LN(2*PI) */
/* -------------------------- */
/* -------------------------- */
    a = min(a0 ,b0);
    b = max(a0, b0);
    if (a >= 8.0) {
	goto L60;
    }
    if (a >= 1.0) {
	goto L20;
    }
/* ----------------------------------------------------------------------- */
/*                   PROCEDURE WHEN A < 1 */
/* ----------------------------------------------------------------------- */
    if (b >= 8.0) {
	goto L10;
    }
    ret_val = gamln(a) + (gamln(b) - gamln(a+b));
    return ret_val;
L10:
    ret_val = gamln(a) + algdiv(a, b);
    return ret_val;
/* ----------------------------------------------------------------------- */
/*                PROCEDURE WHEN 1 <= A < 8 */
/* ----------------------------------------------------------------------- */
L20:
    if (a > 2.0) {
	goto L30;
    }
    if (b > 2.0) {
	goto L21;
    }
    ret_val = gamln(a) + gamln(b) - gsumln(a, b);
    return ret_val;
L21:
    w = 0.0;
    if (b < 8.0) {
	goto L40;
    }
    ret_val = gamln(a) + algdiv(a, b);
    return ret_val;

/*                REDUCTION OF A WHEN B <= 1000 */

L30:
    if (b > 1e3) {
	goto L50;
    }
    n = a - 1.0;
    w = 1.0;
    i1 = n;
    for (i = 1; i <= i1; ++i) {
	a += -1.0;
	h = a / b;
	w *= h / (h + 1.0);
/* L31: */
    }
    w = log(w);
    if (b < 8.0) {
	goto L40;
    }
    ret_val = w + gamln(a) + algdiv(a, b);
    return ret_val;

/*                 REDUCTION OF B WHEN B < 8 */

L40:
    n = b - 1.0;
    z = 1.0;
    i1 = n;
    for (i = 1; i <= i1; ++i) {
	b += -1.0;
	z *= b / (a + b);
/* L41: */
    }
    ret_val = w + log(z) + (gamln(a) + (gamln(b) - gsumln(a, b)));
    return ret_val;

/*                REDUCTION OF A WHEN B > 1000 */

L50:
    n = a - 1.0;
    w = 1.0;
    i1 = n;
    for (i = 1; i <= i1; ++i) {
	a += -1.0;
	w *= a / (a / b + 1.0);
/* L51: */
    }
    ret_val = log(w) - n * log(b) + (gamln(a) + algdiv(a, b));
    return ret_val;
/* ----------------------------------------------------------------------- */
/*                   PROCEDURE WHEN A >= 8 */
/* ----------------------------------------------------------------------- */
L60:
    w = bcorr(a, b);
    h = a / b;
    c = h / (h + 1.0);
    u = -(a - 0.5) * log(c);
    v = b * alnrel(h);
    if (u <= v) {
	goto L61;
    }
    ret_val = log(b) * -0.5 + e + w - v - u;
    return ret_val;
L61:
    ret_val = log(b) * -0.5 + e + w - u - v;
    return ret_val;
} /* betaln */

static double gsumln(double a, double b)
{
    /* System generated locals */
    double ret_val;

    /* Local variables */
    double x;

/* ----------------------------------------------------------------------- */
/*          EVALUATION OF THE FUNCTION LN(GAMMA(A + B)) */
/*          FOR 1 <= A <= 2  AND  1 <= B <= 2 */
/* ----------------------------------------------------------------------- */
    x = a + b - 2.;
    if (x > 0.25) {
	goto L10;
    }
    ret_val = gamln1(x + 1.0);
    return ret_val;
L10:
    if (x > 1.25) {
	goto L20;
    }
    ret_val = gamln1(x) + alnrel(x);
    return ret_val;
L20:
    ret_val = gamln1(x - 1.0) + log(x * (x + 1.0));
    return ret_val;
} /* gsumln */

static double bcorr(double a0, double b0)
{
    /* Initialized data */

    static double c0 = .0833333333333333;
    static double c1 = -.00277777777760991;
    static double c2 = 7.9365066682539e-4;
    static double c3 = -5.9520293135187e-4;
    static double c4 = 8.37308034031215e-4;
    static double c5 = -.00165322962780713;

    /* System generated locals */
    double ret_val, r1;

    /* Local variables */
    double a, b, c, h, t, w, x, s3, s5, x2, s7, s9, s11;

/* ----------------------------------------------------------------------- */

/*     EVALUATION OF  DEL(A0) + DEL(B0) - DEL(A0 + B0)  WHERE */
/*     LN(GAMMA(A)) = (A - 0.5)*LN(A) - A + 0.5*LN(2*PI) + DEL(A). */
/*     IT IS ASSUMED THAT A0 >= 8 AND B0 >= 8. */

/* ----------------------------------------------------------------------- */
/* ------------------------ */
    a = min(a0, b0);
    b = max(a0, b0);

    h = a / b;
    c = h / (h + 1.0);
    x = 1.0 / (h + 1.0);
    x2 = x * x;

/*                SET SN = (1 - X**N)/(1 - X) */

    s3 = x + x2 + 1.0;
    s5 = x + x2 * s3 + 1.0;
    s7 = x + x2 * s5 + 1.0;
    s9 = x + x2 * s7 + 1.0;
    s11 = x + x2 * s9 + 1.0;

/*                SET W = DEL(B) - DEL(A + B) */

/* Computing 2nd power */
    r1 = 1.0 / b;
    t = r1 * r1;
    w = ((((c5 * s11 * t + c4 * s9) * t + c3 * s7) * t + c2 * s5) * t + c1 *
	    s3) * t + c0;
    w *= c / b;

/*                   COMPUTE  DEL(A) + W */

/* Computing 2nd power */
    r1 = 1.0 / a;
    t = r1 * r1;
    ret_val = (((((c5 * t + c4) * t + c3) * t + c2) * t + c1) * t + c0) / a +
	    w;
    return ret_val;
} /* bcorr */

static double algdiv(double a, double b)
{
    /* Initialized data */

    static double c0 = .0833333333333333;
    static double c1 = -.00277777777760991;
    static double c2 = 7.9365066682539e-4;
    static double c3 = -5.9520293135187e-4;
    static double c4 = 8.37308034031215e-4;
    static double c5 = -.00165322962780713;

    /* System generated locals */
    double ret_val, r1;

    /* Local variables */
    double c, d, h, t, u, v, w, x, s3, s5, x2, s7, s9, s11;

/* ----------------------------------------------------------------------- */

/*     COMPUTATION OF LN(GAMMA(B)/GAMMA(A+B)) WHEN B >= 8 */

/*                         -------- */

/*     IN THIS ALGORITHM, DEL(X) IS THE FUNCTION DEFINED BY */
/*     LN(GAMMA(X)) = (X - 0.5)*LN(X) - X + 0.5*LN(2*PI) + DEL(X). */

/* ----------------------------------------------------------------------- */
/* ------------------------ */
    if (a <= b) {
	goto L10;
    }
    h = b / a;
    c = 1.0 / (h + 1.0);
    x = h / (h + 1.0);
    d = a + (b - 0.5);
    goto L20;
L10:
    h = a / b;
    c = h / (h + 1.0);
    x = 1.0 / (h + 1.0);
    d = b + (a - 0.5);

/*                SET SN = (1 - X**N)/(1 - X) */

L20:
    x2 = x * x;
    s3 = x + x2 + 1.0;
    s5 = x + x2 * s3 + 1.0;
    s7 = x + x2 * s5 + 1.0;
    s9 = x + x2 * s7 + 1.0;
    s11 = x + x2 * s9 + 1.0;

/*                SET W = DEL(B) - DEL(A + B) */

/* Computing 2nd power */
    r1 = 1.0 / b;
    t = r1 * r1;
    w = ((((c5 * s11 * t + c4 * s9) * t + c3 * s7) * t + c2 * s5) * t + c1 *
	    s3) * t + c0;
    w *= c / b;

/*                    COMBINE THE RESULTS */

    r1 = a / b;
    u = d * alnrel(r1);
    v = a * (log(b) - 1.0);
    if (u <= v) {
	goto L30;
    }
    ret_val = w - v - u;
    return ret_val;
L30:
    ret_val = w - u - v;
    return ret_val;
} /* algdiv */

static double gamln(double a)
{
    /* Initialized data */

    static double d = .418938533204673;
    static double c0 = .0833333333333333;
    static double c1 = -.00277777777760991;
    static double c2 = 7.9365066682539e-4;
    static double c3 = -5.9520293135187e-4;
    static double c4 = 8.37308034031215e-4;
    static double c5 = -.00165322962780713;

    /* System generated locals */
    int i1;
    double ret_val, r1;

    /* Local variables */
    int i, n;
    double t, w;

/* ----------------------------------------------------------------------- */
/*            EVALUATION OF LN(GAMMA(A)) FOR POSITIVE A */
/* ----------------------------------------------------------------------- */
/*     WRITTEN BY ALFRED H. MORRIS */
/*          NAVAL SURFACE WARFARE CENTER */
/*          DAHLGREN, VIRGINIA */
/* -------------------------- */
/*     D = 0.5*(LN(2*PI) - 1) */
/* -------------------------- */
/* -------------------------- */
/* ----------------------------------------------------------------------- */
    if (a > 0.8) {
	goto L10;
    }
    ret_val = gamln1(a) - log(a);
    return ret_val;
L10:
    if (a > 2.25) {
	goto L20;
    }
    t = a - 0.5 - 0.5;
    ret_val = gamln1(t);
    return ret_val;

L20:
    if (a >= 10.0) {
	goto L30;
    }
    n = a - 1.25;
    t = a;
    w = 1.0;
    i1 = n;
    for (i = 1; i <= i1; ++i) {
	t += -1.0;
/* L21: */
	w = t * w;
    }
    r1 = t - 1.0;
    ret_val = gamln1(r1) + log(w);
    return ret_val;

L30:
/* Computing 2nd power */
    r1 = 1.0 / a;
    t = r1 * r1;
    w = (((((c5 * t + c4) * t + c3) * t + c2) * t + c1) * t + c0) / a;
    ret_val = d + w + (a - 0.5) * (log(a) - 1.0);
    return ret_val;
} /* gamln */
