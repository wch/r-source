/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2010   The R Core Team.
 *
 *  Based on ACM TOMS643 (1993)
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

/* -*- mode: c; kept-new-versions: 25; kept-old-versions: 20 -*-

   Fisher's exact test for contingency tables -- usage see below

   fexact.f -- translated by f2c (version 19971204).
   Run through a slightly modified version of MM's f2c-clean.
   Heavily hand-edited by KH and MM.
 */

/* <UTF8> chars are handled as whole strings */

#include <R.h>
#include <stdio.h>
#include <limits.h>

static void f2xact(int nrow, int ncol, int *table, int ldtabl,
		   double *expect, double *percnt, double *emin,
		   double *prt, double *pre, double *fact, int *ico, int *iro,
		   int *kyy, int *idif, int *irn, int *key,
		   int *ldkey, int *ipoin, double *stp, int *ldstp,
		   int *ifrq, double *LP, double *SP, double *tm,
		   int *key2, int *iwk, double *rwk);
static double f3xact(int nrow, int *irow, int ncol, int *icol, int ntot,
		     double *fact, int *ico, int *iro,
		     int *it, int *lb, int *nr, int *nt, int *nu,
		     int *itc, int *ist, double *stv, double *alen,
		     const double *tol);
static double f4xact(int nrow, int *irow, int ncol, int *icol, double dspt,
		     double *fact, int *icstk, int *ncstk,
		     int *lstk, int *mstk, int *nstk, int *nrstk, int *irstk,
		     double *ystk, const double *tol);
static void f5xact(double *pastp, const double *tol, int *kval, int *key,
		   int *ldkey, int *ipoin, double *stp, int *ldstp,
		   int *ifrq, int *npoin, int *nr, int *nl, int *ifreq,
		   int *itop, Rboolean psh);
static Rboolean f6xact(int nrow, int *irow, int *kyy,
		       int *key, int *ldkey, int *last, int *ipn);
static void f7xact(int nrow, int *imax, int *idif, int *k, int *ks,
		   int *iflag);
static void f8xact(int *irow, int is, int i1, int izero, int *new);
static double f9xact(int n, int ntot, int *ir, double *fact);
static Rboolean f10act(int nrow, int *irow, int ncol, int *icol, double *val,
		       double *fact, int *nd, int *ne, int *m);
static void f11act(int *irow, int i1, int i2, int *new);
static void NORET prterr(int icode, const char *mes);
static int iwork(int iwkmax, int *iwkpt, int number, int itype);

#ifdef USING_R
# define isort(n, ix)		R_isort(ix, *n)
# include <Rmath.h>	/* -> pgamma() */
#else
 static void isort(int *n, int *ix);
 static double gammds(double *y, double *p, int *ifault);
 static double alogam(double *x, int *ifault);
#endif

/* The only public function : */
void
fexact(int *nrow, int *ncol, int *table, int *ldtabl,
       double *expect, double *percnt, double *emin, double *prt,
       double *pre, /* new in C : */ int *workspace,
       /* new arg, was const = 30*/int *mult)
{

/*
  ALGORITHM 643, COLLECTED ALGORITHMS FROM ACM.
  THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
  VOL. 19, NO. 4, DECEMBER, 1993, PP. 484-488.
  -----------------------------------------------------------------------
  Name:	      FEXACT
  Purpose:    Computes Fisher's exact test probabilities and a hybrid
	      approximation to Fisher exact test probabilities for a
	      contingency table using the network algorithm.

  Arguments:
    NROW    - The number of rows in the table.			(Input)
    NCOL    - The number of columns in the table.		(Input)
    TABLE   - NROW by NCOL matrix containing the contingency
	      table.						(Input)
    LDTABL  - Leading dimension of TABLE exactly as specified
	      in the dimension statement in the calling
	      program.						(Input)
    EXPECT  - Expected value used in the hybrid algorithm for
	      deciding when to use asymptotic theory
	      probabilities.					(Input)
	      If EXPECT <= 0.0 then asymptotic theory probabilities
	      are not used and Fisher exact test probabilities are
	      computed.	 Otherwise, if PERCNT or more of the cells in
	      the remaining table have estimated expected values of
	      EXPECT or more, with no remaining cell having expected
	      value less than EMIN, then asymptotic chi-squared
	      probabilities are used.  See the algorithm section of the
	      manual document for details.
	      Use EXPECT = 5.0 to obtain the 'Cochran' condition.
    PERCNT  - Percentage of remaining cells that must have
	      estimated expected values greater than EXPECT
	      before asymptotic probabilities can be used.	(Input)
	      See argument EXPECT for details.
	      Use PERCNT = 80.0 to obtain the 'Cochran' condition.
    EMIN    - Minimum cell estimated expected value allowed for
	      asymptotic chi-squared probabilities to be used.	(Input)
	      See argument EXPECT for details.
	      Use EMIN = 1.0 to obtain the 'Cochran' condition.
    PRT     - Probability of the observed table for fixed
	      marginal totals.					(Output)
    PRE     - Table p-value.					(Output)
	      PRE is the probability of a more extreme table,
	      where `extreme' is in a probabilistic sense.
	      If EXPECT < 0 then the Fisher exact probability
	      is returned.  Otherwise, an approximation to the
	      Fisher exact probability is computed based upon
	      asymptotic chi-squared probabilities for ``large''
	      table expected values.  The user defines ``large''
	      through the arguments EXPECT, PERCNT, and EMIN.

  Remarks:
  1. For many problems one megabyte or more of workspace can be
     required.	If the environment supports it, the user should begin
     by increasing the workspace used to 200,000 units.
  2. In FEXACT, LDSTP = MULT*LDKEY.  The proportion of table space used
     by STP may be changed by changing the line MULT = 30 below to
     another value. --> MULT is now an __argument__ of the function
  3. FEXACT may be converted to single precision by setting IREAL = 3,
     and converting all DOUBLE PRECISION specifications (except the
     specifications for RWRK, IWRK, and DWRK) to REAL.	This will
     require changing the names and specifications of the intrinsic
     functions ALOG, AMAX1, AMIN1, EXP, and REAL.  In addition, the
     machine specific constants will need to be changed, and the name
     DWRK will need to be changed to RWRK in the call to F2XACT.
  4. Machine specific constants are specified and documented in F2XACT.
     A missing value code is specified in both FEXACT and F2XACT.
  5. Although not a restriction, is is not generally practical to call
     this routine with large tables which are not sparse and in
     which the 'hybrid' algorithm has little effect.  For example,
     although it is feasible to compute exact probabilities for the
     table
	    1 8 5 4 4 2 2
	    5 3 3 4 3 1 0
	   10 1 4 0 0 0 0,
     computing exact probabilities for a similar table which has been
     enlarged by the addition of an extra row (or column) may not be
     feasible.
  -----------------------------------------------------------------------
  */

    /* CONSTANT Parameters : */

    /* To increase the length of the table of past path lengths relative
       to the length of the hash table, increase MULT.
    */

    /* AMISS is a missing value indicator which is returned when the
       probability is not defined.
    */
    const double amiss = -12345.;
    /*
      Set IREAL = 4 for DOUBLE PRECISION
      Set IREAL = 3 for SINGLE PRECISION
    */
#define i_real 4
#define i_int  2

    /* System generated locals */
    int ikh;
    /* Local variables */
    int nco, nro, ntot, numb, iiwk, irwk;
    int i, j, k, kk, ldkey, ldstp, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10;
    int i3a, i3b, i3c, i9a, iwkmax, iwkpt;

    /* Workspace Allocation (freed when returning to R) */
    double *equiv;
    iwkmax = 2 * (int) (*workspace / 2);
    equiv = (double *) R_alloc(iwkmax / 2, sizeof(double));

#define dwrk (equiv)
#define iwrk ((int *)equiv)
#define rwrk ((float *)equiv)

    /* Function Body */
    iwkpt = 0;

    if (*nrow > *ldtabl)
	prterr(1, "NROW must be less than or equal to LDTABL.");

    ntot = 0;
    for (i = 0; i < *nrow; ++i) {
	for (j = 0; j < *ncol; ++j) {
	    if (table[i + j * *ldtabl] < 0)
		prterr(2, "All elements of TABLE may not be negative.");
	    ntot += table[i + j * *ldtabl];
	}
    }
    if (ntot == 0) {
	prterr(3, "All elements of TABLE are zero.\n"
	       "PRT and PRE are set to missing values.");
	*pre = *prt = amiss;
	return;
    }

    /* nco := max(*nrow, *ncol)
     * nro := min(*nrow, *ncol) */
    if(*ncol > *nrow) {
	nco = *ncol;
	nro = *nrow;
    } else {
	nco = *nrow;
	nro = *ncol;
    }
    k = *nrow + *ncol + 1;
    kk = k * nco;

    ikh = ntot + 1;
    i1  = iwork(iwkmax, &iwkpt, ikh, i_real);
    i2  = iwork(iwkmax, &iwkpt, nco, i_int);
    i3  = iwork(iwkmax, &iwkpt, nco, i_int);
    i3a = iwork(iwkmax, &iwkpt, nco, i_int);
    i3b = iwork(iwkmax, &iwkpt, nro, i_int);
    i3c = iwork(iwkmax, &iwkpt, nro, i_int);
    ikh = imax2(k * 5 + (kk << 1), nco * 7 + 800);
    iiwk= iwork(iwkmax, &iwkpt, ikh, i_int);
    ikh = imax2(nco + 401, k);
    irwk= iwork(iwkmax, &iwkpt, ikh, i_real);

    /* NOTE:
       What follows below splits the remaining amount iwkmax - iwkpt of
       (int) workspace into hash tables as follows.
	   type  size       index
	   INT   2 * ldkey  i4 i5 i11
	   REAL  2 * ldkey  i8 i9 i10
	   REAL  2 * ldstp  i6
	   INT   6 * ldstp  i7
       Hence, we need ldkey times
	   3 * 2 + 3 * 2 * s + 2 * mult * s + 6 * mult
       chunks of integer memory, where s = sizeof(REAL) / sizeof(INT).
       If doubles are used and are twice as long as ints, this gives
	   18 + 10 * mult
       so that the value of ldkey can be obtained by dividing available
       (int) workspace by this number.

       In fact, because iwork() can actually s * n + s - 1 int chunks
       when allocating a REAL, we use ldkey = available / numb - 1.

       FIXME:
       Can we always assume that sizeof(double) / sizeof(int) is 2?
       */

    if (i_real == 4) {		/* Double precision reals */
	numb = 18 + 10 * *mult;
    } else {			/* Single precision reals */
	numb = (*mult << 3) + 12;
    }
    ldkey = (iwkmax - iwkpt) / numb - 1;
    ldstp = *mult * ldkey;
    ikh = ldkey << 1;	i4  = iwork(iwkmax, &iwkpt, ikh, i_int);
    ikh = ldkey << 1;	i5  = iwork(iwkmax, &iwkpt, ikh, i_int);
    ikh = ldstp << 1;	i6  = iwork(iwkmax, &iwkpt, ikh, i_real);
    ikh = ldstp * 6;	i7  = iwork(iwkmax, &iwkpt, ikh, i_int);
    ikh = ldkey << 1;	i8  = iwork(iwkmax, &iwkpt, ikh, i_real);
    ikh = ldkey << 1;	i9  = iwork(iwkmax, &iwkpt, ikh, i_real);
    ikh = ldkey << 1;	i9a = iwork(iwkmax, &iwkpt, ikh, i_real);
    ikh = ldkey << 1;	i10 = iwork(iwkmax, &iwkpt, ikh, i_int);


    /* To convert to double precision, change RWRK to DWRK in the next CALL.
     */
    f2xact(*nrow,
	   *ncol,
	   table,
	   *ldtabl,
	   expect,
	   percnt,
	   emin,
	   prt,
	   pre,
	   dwrk + i1,
	   iwrk + i2,
	   iwrk + i3,
	   iwrk + i3a,
	   iwrk + i3b,
	   iwrk + i3c,
	   iwrk + i4,
	   &ldkey,
	   iwrk + i5,
	   dwrk + i6,
	   &ldstp,
	   iwrk + i7,
	   dwrk + i8,
	   dwrk + i9,
	   dwrk + i9a,
	   iwrk + i10,
	   iwrk + iiwk,
	   dwrk + irwk);

    return;
}

#undef rwrk
#undef iwrk
#undef dwrk


void
f2xact(int nrow, int ncol, int *table, int ldtabl,
       double *expect, double *percnt, double *emin, double *prt,
       double *pre, double *fact, int *ico, int *iro, int *kyy,
       int *idif, int *irn, int *key, int *ldkey, int *ipoin,
       double *stp, int *ldstp, int *ifrq, double *LP, double *SP,
       double *tm, int *key2, int *iwk, double *rwk)
{
/*
  -----------------------------------------------------------------------
  Name:		F2XACT
  Purpose:	Computes Fisher's exact test for a contingency table,
		routine with workspace variables specified.
  -----------------------------------------------------------------------
  */
    const int imax = INT_MAX;/* the largest representable int on the machine.*/

    /* AMISS is a missing value indicator which is returned when the
       probability is not defined. */
    const double amiss = -12345.;

    /* TOL is chosen as the square root of the smallest relative spacing. */
    const static double tol = 3.45254e-7;

    const char* ch_err_5 =
	"The hash table key cannot be computed because the largest key\n"
	"is larger than the largest representable int.\n"
	"The algorithm cannot proceed.\n"
	"Reduce the workspace size or use another algorithm.";

    /* Local variables -- changed from "static"
     *  (*does* change results very slightly on i386 linux) */
    int i, ii, j, k, n,
	iflag,ifreq, ikkey, ikstp, ikstp2, ipn, ipo, itop, itp = 0,
	jkey, jstp, jstp2, jstp3, jstp4, k1, kb, kd, ks, kval = 0, kmax, last,
	ncell, ntot, nco, nro, nro2, nrb,
	i31, i32, i33, i34, i35, i36, i37, i38, i39,
	i41, i42, i43, i44, i45, i46, i47, i48, i310, i311;

    double dspt, d1,dd,df,ddf, drn,dro, obs, obs2, obs3, pastp,pv, tmp=0.;

#ifndef USING_R
    double d2;
    int ifault;
#endif
    Rboolean nr_gt_nc, maybe_chisq, chisq = FALSE/* -Wall */, psh;

    /* Parameter adjustments */
    table -= ldtabl + 1;
    --ico;
    --iro;
    --kyy;
    --idif;
    --irn;

    --key;
    --ipoin;
    --stp;
    --ifrq;
    --LP;
    --SP;
    --tm;
    --key2;
    --iwk;
    --rwk;


    /* Check table dimensions */
    if (nrow > ldtabl)
	prterr(1, "NROW must be less than or equal to LDTABL.");
    if (ncol <= 1)
	prterr(4, "NCOL must be at least 2");

    /* Initialize KEY array */
    for (i = 1; i <= *ldkey << 1; ++i) {
	key[i] = -9999;
	key2[i] = -9999;
    }

    nr_gt_nc =  nrow > ncol;
    /* nco := max(nrow, ncol) : */
    if(nr_gt_nc)
	nco = nrow;
    else
	nco = ncol;
    /* Compute row marginals and total */
    ntot = 0;
    for (i = 1; i <= nrow; ++i) {
	iro[i] = 0;
	for (j = 1; j <= ncol; ++j) {
	    if (table[i + j * ldtabl] < 0.)
		prterr(2, "All elements of TABLE may not be negative.");
	    iro[i] += table[i + j * ldtabl];
	}
	ntot += iro[i];
    }

    if (ntot == 0) {
	prterr(3, "All elements of TABLE are zero.\n"
	       "PRT and PRE are set to missing values.");
	*pre = *prt = amiss;
	return;
    }

    /* Column marginals */
    for (i = 1; i <= ncol; ++i) {
	ico[i] = 0;
	for (j = 1; j <= nrow; ++j)
	    ico[i] += table[j + i * ldtabl];
    }

    /* sort marginals */
    isort(&nrow, &iro[1]);
    isort(&ncol, &ico[1]);

    /*	Determine row and column marginals.
	Define max(nrow,ncol) =: nco >= nro := min(nrow,ncol)
	nco is defined above

	Swap marginals if necessary to	ico[1:nco] & iro[1:nro]
     */
    if (nr_gt_nc) {
	nro = ncol;
	/* Swap marginals */
	for (i = 1; i <= nco; ++i) {
	    ii = iro[i];
	    if (i <= nro)
		iro[i] = ico[i];
	    ico[i] = ii;
	}
    } else
	nro = nrow;

    /* Get multiplers for stack */
    kyy[1] = 1;
    for (i = 1; i < nro; ++i) {
	/* Hash table multipliers */
	if (iro[i] + 1 <= imax / kyy[i]) {
	    kyy[i + 1] = kyy[i] * (iro[i] + 1);
	    j /= kyy[i];
	}
	else {
	    prterr(5, ch_err_5);
	    return;
	}
    }

    /* Check for Maximum product : */
    /* original code: if (iro[nro - 1] + 1 > imax / kyy[nro - 1]) */
    if (iro[nro] + 1 > imax / kyy[nro]) {
	/* L_ERR_5: */
	prterr(501, ch_err_5);
	return;
    }

    /* Compute log factorials */
    fact[0] = 0.;
    fact[1] = 0.;
    if(ntot >= 2) fact[2] = log(2.);
    /* MM: old code assuming log() to be SLOW */
    for (i = 3; i <= ntot; i += 2) {
	fact[i] = fact[i - 1] + log((double) i);
	j = i + 1;
	if (j <= ntot)
	    fact[j] = fact[i] + fact[2] + fact[j / 2] - fact[j / 2 - 1];
    }
    /* Compute obs := observed path length */
    obs = tol;
    ntot = 0;
    for (j = 1; j <= nco; ++j) {
	dd = 0.;
	if (nr_gt_nc) {
	    for (i = 1; i <= nro; ++i) {
		dd += fact[table[j + i * ldtabl]];
		ntot +=    table[j + i * ldtabl];
	    }
	} else {
	    for (i = 1, ii = j * ldtabl + 1; i <= nro; i++, ii++) {
		dd += fact[table[ii]];
		ntot +=    table[ii];
	    }
	}
	obs += fact[ico[j]] - dd;
    }

    /* Denominator of observed table: DRO */
    dro = f9xact(nro, ntot, &iro[1], fact);
    /* improve: the following "easily" underflows to zero -- return "log()" */
    *prt = exp(obs - dro);
    *pre = 0.;
    itop = 0;
    maybe_chisq = (*expect > 0.);

    /* Initialize pointers for workspace */
    /* f3xact */
    i31 = 1;
    i32 = i31 + nco;
    i33 = i32 + nco;
    i34 = i33 + nco;
    i35 = i34 + nco;
    i36 = i35 + nco;
    i37 = i36 + nco;
    i38 = i37 + nco;
    i39 = i38 + 400;
    i310 = 1;
    i311 = 1 + 400;
    /* f4xact */
    i = nrow + ncol + 1;
    i41 = 1;
    i42 = i41 + i;
    i43 = i42 + i;
    i44 = i43 + i;
    i45 = i44 + i;
    i46 = i45 + i;
    i47 = i46 + i * nco;
    i48 = 1;

    /* Initialize pointers */
    k = nco;
    last = *ldkey + 1;
    jkey = *ldkey + 1;
    jstp = *ldstp + 1;
    jstp2 = *ldstp * 3 + 1;
    jstp3 = (*ldstp << 2) + 1;
    jstp4 = *ldstp * 5 + 1;
    ikkey = 0;
    ikstp = 0;
    ikstp2 = *ldstp << 1;
    ipo = 1;
    ipoin[1] = 1;
    stp[1] = 0.;
    ifrq[1] = 1;
    ifrq[ikstp2 + 1] = -1;

Outer_Loop:
    kb = nco - k + 1;
    ks = 0;
    n = ico[kb];
    kd = nro + 1;
    kmax = nro;
    /* IDIF is the difference in going to the daughter */
    for (i = 1; i <= nro; ++i)
	idif[i] = 0;

    /* Generate the first daughter */
    do {
	--kd;
	ntot = imin2(n, iro[kd]);
	idif[kd] = ntot;
	if (idif[kmax] == 0)
	    --kmax;
	n -= ntot;

    } while (n > 0 && kd != 1);

    if (n != 0) /* i.e. kd == 1 */
	goto L310;


    k1 = k - 1;
    n = ico[kb];
    ntot = 0;
    for (i = kb + 1; i <= nco; ++i)
	ntot += ico[i];


L150:
    /* Arc to daughter length=ICO[KB] */
    for (i = 1; i <= nro; ++i)
	irn[i] = iro[i] - idif[i];

    if (k1 > 1) {
	/* Sort irn */
	if (nro == 2) {
	    if (irn[1] > irn[2]) {
		ii = irn[1]; irn[1] = irn[2]; irn[2] = ii;
	    }
	} else
	    isort(&nro, &irn[1]);

	/* Adjust start for zero */
	for (i = 1; i <= nro; ++i) {
	    if (irn[i] != 0)
		break;
	}
	nrb = i;
    }
    else {
	nrb = 1;
    }
    nro2 = nro - nrb + 1;

    /* Some table values */
    ddf = f9xact(nro,  n,    &idif[1],  fact);
    drn = f9xact(nro2, ntot, &irn[nrb], fact) - dro + ddf;
    /* Get hash value */
    if (k1 > 1) {
	kval = irn[1];
	/* Note that with the corrected check at error "502",
	 * we won't have overflow in  kval  below : */
	for (i = 2; i <= nro; ++i)
	    kval += irn[i] * kyy[i];

	/* Get hash table entry */
	i = kval % (*ldkey << 1) + 1;
	/* Search for unused location */
	for (itp = i; itp <= *ldkey << 1; ++itp) {
	    ii = key2[itp];
	    if (ii == kval) {
		goto L240;
	    } else if (ii < 0) {
		key2[itp] = kval;
		LP[itp] = 1.;
		SP[itp] = 1.;
		goto L240;
	    }
	}

	for (itp = 1; itp <= i - 1; ++itp) {
	    ii = key2[itp];
	    if (ii == kval) {
		goto L240;
	    } else if (ii < 0) {
		key2[itp] = kval;
		LP[itp] = 1.;
		goto L240;
	    }
	}

	/* KH
	   prterr(6, "LDKEY is too small.\n"
	   "It is not possible to give the value of LDKEY required,\n"
	   "but you could try doubling LDKEY (and possibly LDSTP).");
	   */
	prterr(6, "LDKEY is too small for this problem.\n"
	       "Try increasing the size of the workspace.");
    }

L240:
    psh = TRUE;
    /* Recover pastp */
    ipn = ipoin[ipo + ikkey];
    pastp = stp[ipn + ikstp];
    ifreq = ifrq[ipn + ikstp];
    /* Compute shortest and longest path */
    if (k1 > 1) {
	obs2 = obs - fact[ico[kb + 1]] - fact[ico[kb + 2]] - ddf;
	for (i = 3; i <= k1; ++i)
	    obs2 -= fact[ico[kb + i]];

	if (LP[itp] > 0.) {
	    dspt = obs - obs2 - ddf;
	    /* Compute longest path */
	    LP[itp] = f3xact(nro2, &irn[nrb], k1, &ico[kb + 1], ntot, fact,
			      &iwk[i31], &iwk[i32], &iwk[i33], &iwk[i34],
			      &iwk[i35], &iwk[i36], &iwk[i37], &iwk[i38],
			      &iwk[i39], &rwk[i310], &rwk[i311], &tol);
	    if(LP[itp] > 0.) {/* can this happen? */
		REprintf("___ LP[itp=%d] = %g > 0\n", itp, LP[itp]);
		LP[itp] = 0.;
	    }

	    /* Compute shortest path -- using  dspt  as offset */
	    SP[itp] = f4xact(nro2, &irn[nrb], k1, &ico[kb + 1], dspt, fact,
			      &iwk[i47], &iwk[i41], &iwk[i42], &iwk[i43],
			      &iwk[i44], &iwk[i45], &iwk[i46], &rwk[i48], &tol);
	    /* SP[itp] = fmin2(0., SP[itp] - dspt);*/
	    if(SP[itp] > 0.) { /* can this happen? */
		REprintf("___ SP[itp=%d] = %g > 0\n", itp, SP[itp]);
		SP[itp] = 0.;
	    }

	    /* Use chi-squared approximation? */
	    if (maybe_chisq && (irn[nrb] * ico[kb + 1]) > ntot * *emin) {
		ncell = 0.;
		for (i = 0; i < nro2; ++i)
		    for (j = 1; j <= k1; ++j)
			if (irn[nrb + i] * ico[kb + j] >= ntot * *expect)
			    ncell++;

		if (ncell * 100 >= k1 * nro2 * *percnt) {
		    tmp = 0.;
		    for (i = 0; i < nro2; ++i)
			tmp += (fact[irn[nrb + i]] -
				fact[irn[nrb + i] - 1]);
		    tmp *= k1 - 1;
		    for (j = 1; j <= k1; ++j)
			tmp += (nro2 - 1) * (fact[ico[kb + j]] -
					     fact[ico[kb + j] - 1]);
		    df = (double) ((nro2 - 1) * (k1 - 1));
		    tmp += df * 1.83787706640934548356065947281;
		    tmp -= (nro2 * k1 - 1) * (fact[ntot] - fact[ntot - 1]);
		    tm[itp] = (obs - dro) * -2. - tmp;
		} else {
		    /* tm[itp] set to a flag value */
		    tm[itp] = -9876.;
		}
	    } else {
		tm[itp] = -9876.;
	    }
	}
	obs3 = obs2 - LP[itp];
	obs2 -= SP[itp];
	if (tm[itp] == -9876.) {
	    chisq = FALSE;
	} else {
	    chisq = TRUE;
	    tmp = tm[itp];
	}
    } else {
	obs2 = obs - drn - dro;
	obs3 = obs2;
    }

L300:
    /* Process node with new PASTP */
    if (pastp <= obs3) {
	/* Update pre */
	*pre += (double) ifreq * exp(pastp + drn);
    } else if (pastp < obs2) {
	if (chisq) {
	    df = (double) ((nro2 - 1) * (k1 - 1));
#ifdef USING_R
	    pv = pgamma(fmax2(0., tmp + (pastp + drn) * 2.) / 2.,
			df / 2., /*scale = */ 1.,
			/*lower_tail = */FALSE, /*log_p = */ FALSE);
#else
	    d1 = fmax2(0., tmp + (pastp + drn) * 2.) / 2.;
	    d2 = df / 2.;
	    pv = 1. - gammds(&d1, &d2, &ifault);
#endif
	    *pre += (double) ifreq * exp(pastp + drn) * pv;
	} else {
	    /* Put daughter on queue */
	    d1 = pastp + ddf;
	    f5xact(&d1, &tol, &kval, &key[jkey], ldkey, &ipoin[jkey],
		   &stp[jstp], ldstp, &ifrq[jstp], &ifrq[jstp2],
		   &ifrq[jstp3], &ifrq[jstp4], &ifreq, &itop, psh);
	    psh = FALSE;
	}
    }
    /* Get next PASTP on chain */
    ipn = ifrq[ipn + ikstp2];
    if (ipn > 0) {
	pastp = stp[ipn + ikstp];
	ifreq = ifrq[ipn + ikstp];
	goto L300;
    }
    /* Generate a new daughter node */
    f7xact(kmax, &iro[1], &idif[1], &kd, &ks, &iflag);
    if (iflag != 1)
	goto L150;


L310:
    /* Go get a new mother from stage K */
    do {
	if(!f6xact(nro, &iro[1], &kyy[1], &key[ikkey + 1], ldkey, &last, &ipo))
	    /* Update pointers */
	    goto Outer_Loop;

	/* else : no additional nodes to process */
	--k;
	itop = 0;
	ikkey = jkey - 1;
	ikstp = jstp - 1;
	ikstp2 = jstp2 - 1;
	jkey = *ldkey - jkey + 2;
	jstp = *ldstp - jstp + 2;
	jstp2 = (*ldstp << 1) + jstp;
	for (i = 1; i <= *ldkey << 1; ++i)
	    key2[i] = -9999;

    } while (k >= 2);

}/* f2xact() */


double
f3xact(int nrow, int *irow, int ncol, int *icol,
       int ntot, double *fact, int *ico, int *iro, int *it,
       int *lb, int *nr, int *nt, int *nu, int *itc, int *ist,
       double *stv, double *alen, const double *tol)
{
/*
 -----------------------------------------------------------------------
  Name:	      F3XACT
  Purpose:    Computes the longest path length for a given table.

  Arguments:
    NROW    - The number of rows in the table.			(Input)
    IROW    - Vector of length NROW containing the row sums
	      for the table.					(Input)
    NCOL    - The number of columns in the table.		(Input)
    ICOL    - Vector of length K containing the column sums
	      for the table.					(Input)
    NTOT    - The total count in the table.			(Input)
    FACT    - Vector containing the logarithms of factorials.	(Input)
    ICO     - Work vector of length MAX(NROW,NCOL).
    IRO     - Work vector of length MAX(NROW,NCOL).
    IT	    - Work vector of length MAX(NROW,NCOL).
    LB	    - Work vector of length MAX(NROW,NCOL).
    NR	    - Work vector of length MAX(NROW,NCOL).
    NT	    - Work vector of length MAX(NROW,NCOL).
    NU	    - Work vector of length MAX(NROW,NCOL).
    ITC     - Work vector of length 400.
    IST     - Work vector of length 400.
    STV     - Work vector of length 400.
    ALEN    - Work vector of length MAX(NROW,NCOL).
    TOL     - Tolerance.					(Input)

  Return Value :
    LP     - The longest path for the table.			(Output)

  -----------------------------------------------------------------------
  */

    const int ldst = 200;/* half stack size */
    /* Initialized data */
    static int nst = 0;
    static int nitc = 0;

    /* Local variables */
    int i, k;
    int n11, n12, ii, nn, ks, ic1, ic2, nc1, nn1;
    int nr1, nco, nct, ipn, irl, key, lev, itp, nro, nrt, kyy, nc1s;
    double LP, v, val, vmn;
    Rboolean xmin;

    /* Parameter adjustments */
    --stv;
    --ist;
    --itc;
    --nu;
    --nt;
    --nr;
    --lb;
    --it;
    --iro;
    --ico;
    --icol;
    --irow;

    if (nrow <= 1) {	/* nrow is 1 */
	LP = 0.;
	if (nrow > 0) {
	    for (i = 1; i <= ncol; ++i)
		LP -= fact[icol[i]];
	}
	return LP;
    }

    if (ncol <= 1) {	/* ncol is 1 */
	LP = 0.;
	if (ncol > 0) {
	    for (i = 1; i <= nrow; ++i)
		LP -= fact[irow[i]];
	}
	return LP;
    }

    /* 2 by 2 table */
    if (nrow * ncol == 4) {
	n11 = (irow[1] + 1) * (icol[1] + 1) / (ntot + 2);
	n12 = irow[1] - n11;
	return -(fact[n11] + fact[n12] +
		 fact[icol[1] - n11] + fact[icol[2] - n12]);
    }

    /* ELSE:  larger than 2 x 2 : */

    /* Test for optimal table */
    val = 0.;
    if (irow[nrow] <= irow[1] + ncol) {
	xmin = f10act(nrow, &irow[1], ncol, &icol[1], &val, fact,
		      &lb[1], &nu[1], &nr[1]);
    } else xmin = FALSE;
    if (! xmin &&  icol[ncol] <= icol[1] + nrow) {
	xmin = f10act(ncol, &icol[1], nrow, &irow[1], &val, fact,
		      &lb[1], &nu[1], &nr[1]);
    }
    if (xmin)
	return  - val;


    /* Setup for dynamic programming */

    for (i = 0; i <= ncol; ++i)
	alen[i] = 0.;
    for (i = 1; i <= 2*ldst; ++i)
	ist[i] = -1;

    nn = ntot;
    /* Minimize ncol */
    if (nrow >= ncol) {
	nro = nrow;
	nco = ncol;
	ico[1] = icol[1];
	nt[1] = nn - ico[1];
	for (i = 2; i <= ncol; ++i) {
	    ico[i] = icol[i];
	    nt[i] = nt[i - 1] - ico[i];
	}
	for (i = 1; i <= nrow; ++i)
	    iro[i] = irow[i];

    } else {
	nro = ncol;
	nco = nrow;
	ico[1] = irow[1];
	nt[1] = nn - ico[1];
	for (i = 2; i <= nrow; ++i) {
	    ico[i] = irow[i];
	    nt[i] = nt[i - 1] - ico[i];
	}
	for (i = 1; i <= ncol; ++i)
	    iro[i] = icol[i];
    }

    nc1s = nco - 1;
    kyy = ico[nco] + 1;
    /* Initialize pointers */
    vmn = 1e100;/* to contain min(v..) */
    irl = 1;
    ks = 0;
    k = ldst;


LnewNode: /* Setup to generate new node */

    lev = 1;
    nr1 = nro - 1;
    nrt = iro[irl];
    nct = ico[1];
    lb[1] = (int) ((((double) nrt + 1) * (nct + 1)) /
		    (double) (nn + nr1 * nc1s + 1) - *tol) - 1;
    nu[1] = (int) ((((double) nrt + nc1s) * (nct + nr1)) /
		    (double) (nn + nr1 + nc1s)) - lb[1] + 1;
    nr[1] = nrt - lb[1];

LoopNode: /* Generate a node */
    --nu[lev];
    if (nu[lev] == 0) {
	if (lev == 1)
	    goto L200;

	--lev;
	goto LoopNode;
    }
    ++lb[lev];
    --nr[lev];

    while(1) {
	alen[lev] = alen[lev - 1] + fact[lb[lev]];
	if (lev >= nc1s)
	    break;

	nn1 = nt[lev];
	nrt = nr[lev];
	++lev;
	nc1 = nco - lev;
	nct = ico[lev];
	lb[lev] = (int) ((((double) nrt + 1) * (nct + 1)) /
			  (double) (nn1 + nr1 * nc1 + 1) - *tol);
	nu[lev] = (int) ((((double) nrt + nc1) * (nct + nr1)) /
			  (double) (nn1 + nr1 + nc1) - lb[lev] + 1);
	nr[lev] = nrt - lb[lev];
    }
    alen[nco] = alen[lev] + fact[nr[lev]];
    lb[nco] = nr[lev];

    v = val + alen[nco];

    if (nro == 2) { /* Only 1 row left */
	v += fact[ico[1] - lb[1]] + fact[ico[2] - lb[2]];
	for (i = 3; i <= nco; ++i)
	    v += fact[ico[i] - lb[i]];

	if (v < vmn)
	    vmn = v;

    } else if (nro == 3 && nco == 2) { /* 3 rows and 2 columns */
	nn1 = nn - iro[irl] + 2;
	ic1 = ico[1] - lb[1];
	ic2 = ico[2] - lb[2];
	n11 = (iro[irl + 1] + 1) * (ic1 + 1) / nn1;
	n12 = iro[irl + 1] - n11;
	v += fact[n11] + fact[n12] + fact[ic1 - n11] + fact[ic2 - n12];
	if (v < vmn)
	    vmn = v;

    } else { /* Column marginals are new node */

	for (i = 1; i <= nco; ++i)
	    it[i] = imax2(ico[i] - lb[i], 0);

	/* Sort column marginals it[] : */
	if (nco == 2) {
	    if (it[1] > it[2]) { /* swap */
		ii = it[1]; it[1] = it[2]; it[2] = ii;
	    }
	} else
	    isort(&nco, &it[1]);

	/* Compute hash value */
	key = it[1] * kyy + it[2];
	for (i = 3; i <= nco; ++i) {
	    key = it[i] + key * kyy;
	}
	if (key < -1)
	    PROBLEM "Bug in FEXACT: gave negative key" RECOVER(NULL_ENTRY);
	/* Table index */
	ipn = key % ldst + 1;
	/* Find empty position */
	for (itp = ipn, ii = ks + ipn; itp <= ldst; ++itp, ++ii) {
	    if (ist[ii] < 0) {
		goto L180;
	    } else if (ist[ii] == key) {
		goto L190;
	    }
	}

	for (itp = 1, ii = ks + 1; itp <= ipn - 1; ++itp, ++ii) {
	    if (ist[ii] < 0) {
		goto L180;
	    } else if (ist[ii] == key) {
		goto L190;
	    }
	}

	/* this happens less, now that we check for negative key above: */
	prterr(30, "Stack length exceeded in f3xact.\n"
	       "This problem should not occur.");

L180: /* Push onto stack */
	ist[ii] = key;
	stv[ii] = v;
	++nst;
	ii = nst + ks;
	itc[ii] = itp;
	goto LoopNode;

L190: /* Marginals already on stack */
	stv[ii] = fmin2(v, stv[ii]);
    }
    goto LoopNode;


L200: /* Pop item from stack */
    if (nitc > 0) {
	/* Stack index */
	itp = itc[nitc + k] + k;
	--nitc;
	val = stv[itp];
	key = ist[itp];
	ist[itp] = -1;
	/* Compute marginals */
	for (i = nco; i >= 2; --i) {
	    ico[i] = key % kyy;
	    key /= kyy;
	}
	ico[1] = key;
	/* Set up nt array */
	nt[1] = nn - ico[1];
	for (i = 2; i <= nco; ++i)
	    nt[i] = nt[i - 1] - ico[i];

	/* Test for optimality (L90) */
	if (iro[nro] <= iro[irl] + nco) {
	    xmin = f10act(nro, &iro[irl], nco, &ico[1], &val, fact,
			  &lb[1], &nu[1], &nr[1]);
	} else xmin = FALSE;

	if (!xmin && ico[nco] <= ico[1] + nro)
	    xmin = f10act(nco, &ico[1], nro, &iro[irl], &val, fact,
			  &lb[1], &nu[1], &nr[1]);
	if (xmin) {
	    if (vmn > val)
		vmn = val;
	    goto L200;
	}
	else goto LnewNode;

    } else if (nro > 2 && nst > 0) {
	/* Go to next level */
	nitc = nst;
	nst = 0;
	k = ks;
	ks = ldst - ks;
	nn -= iro[irl];
	++irl;
	--nro;
	goto L200;
    }

    return  - vmn;
}

double
f4xact(int nrow, int *irow, int ncol, int *icol, double dspt,
       double *fact, int *icstk, int *ncstk, int *lstk, int *mstk,
       int *nstk, int *nrstk, int *irstk, double *ystk, const double *tol)
{
/*
  -----------------------------------------------------------------------
  Name:	      F4XACT
  Purpose:    Computes the shortest path length for a given table.

  Arguments:
     NROW   - The number of rows in the table.	(Input)
     IROW   - Vector of length NROW containing the row sums for the
	      table.  (Input)
     NCOL   - The number of columns in the table.  (Input)
     ICOL   - Vector of length K containing the column sums for the
	      table.  (Input)
     DSPT   - "offset"  for SP computation
     FACT   - Vector containing the logarithms of factorials.  (Input)
     ICSTK  - NCOL by NROW+NCOL+1 work array.
     NCSTK  - Work vector of length NROW+NCOL+1.
     LSTK   - Work vector of length NROW+NCOL+1.
     MSTK   - Work vector of length NROW+NCOL+1.
     NSTK   - Work vector of length NROW+NCOL+1.
     NRSTK  - Work vector of length NROW+NCOL+1.
     IRSTK  - NROW by MAX(NROW,NCOL) work array.
     YSTK   - Work vector of length NROW+NCOL+1.
     TOL    - Tolerance.					(Input)

  Return Value :

    SP	    - The shortest path for the table.			(Output)
  -----------------------------------------------------------------------
  */

    /* Local variables */
    int i, j, k, l, m, n, ic1, ir1, ict, irt, istk, nco, nro;
    double y, amx, SP;

    /* Take care of the easy cases first */
    if (nrow == 1) {
	SP = 0.;
	for (i = 0; i < ncol; ++i)
	    SP -= fact[icol[i]];
	return SP;
    }
    if (ncol == 1) {
	SP = 0.;
	for (i = 0; i < nrow; ++i)
	    SP -= fact[irow[i]];
	return SP;
    }
    if (nrow * ncol == 4) {
	if (irow[1] <= icol[1])
	    return -(fact[irow[1]] + fact[icol[1]] + fact[icol[1] - irow[1]]);
	else
	    return -(fact[icol[1]] + fact[irow[1]] + fact[irow[1] - icol[1]]);
    }

    /* Parameter adjustments */
    irstk -= nrow + 1;
    icstk -= ncol + 1;

    --nrstk;
    --ncstk;
    --lstk;
    --mstk;
    --nstk;
    --ystk;

    /* initialization before loop */
    for (i = 1; i <= nrow; ++i)
	irstk[i + nrow] = irow[nrow - i];

    for (j = 1; j <= ncol; ++j)
	icstk[j + ncol] = icol[ncol - j];

    nro = nrow;
    nco = ncol;
    nrstk[1] = nro;
    ncstk[1] = nco;
    ystk[1] = 0.;
    y = 0.;
    istk = 1;
    l = 1;
    amx = 0.;
    SP = dspt;

    /* First LOOP */
    do {
	ir1 = irstk[istk * nrow + 1];
	ic1 = icstk[istk * ncol + 1];
	if (ir1 > ic1) {
	    if (nro >= nco) {
		m = nco - 1;	n = 2;
	    } else {
		m = nro;	n = 1;
	    }
	} else if (ir1 < ic1) {
	    if (nro <= nco) {
		m = nro - 1;	n = 1;
	    } else {
		m = nco;	n = 2;
	    }
	} else {
	    if (nro <= nco) {
		m = nro - 1;	n = 1;
	    } else {
		m = nco - 1;	n = 2;
	    }
	}

    L60:
	if (n == 1) {
	    i = l; j = 1;
	} else {
	    i = 1; j = l;
	}

	irt = irstk[i + istk * nrow];
	ict = icstk[j + istk * ncol];
	y += fact[imin2(irt, ict)];
	if (irt == ict) {
	    --nro;
	    --nco;
	    f11act(&irstk[istk * nrow + 1], i, nro,
		   &irstk[(istk + 1) * nrow + 1]);
	    f11act(&icstk[istk * ncol + 1], j, nco,
		   &icstk[(istk + 1) * ncol + 1]);
	} else if (irt > ict) {
	    --nco;
	    f11act(&icstk[istk * ncol + 1], j, nco,
		   &icstk[(istk + 1) * ncol + 1]);
	    f8xact(&irstk[istk * nrow + 1], irt - ict, i, nro,
		   &irstk[(istk + 1) * nrow + 1]);
	} else {
	    --nro;
	    f11act(&irstk[istk * nrow + 1], i, nro,
		   &irstk[(istk + 1) * nrow + 1]);
	    f8xact(&icstk[istk * ncol + 1], ict - irt, j, nco,
		   &icstk[(istk + 1) * ncol + 1]);
	}

	if (nro == 1) {
	    for (k = 1; k <= nco; ++k)
		y += fact[icstk[k + (istk + 1) * ncol]];
	    break;
	}
	if (nco == 1) {
	    for (k = 1; k <= nro; ++k)
		y += fact[irstk[k + (istk + 1) * nrow]];
	    break;
	}

	lstk[istk] = l;
	mstk[istk] = m;
	nstk[istk] = n;
	++istk;
	nrstk[istk] = nro;
	ncstk[istk] = nco;
	ystk[istk] = y;
	l = 1;
    } while(1);/* end do */

/* L90:*/
    if (y > amx) {
	amx = y;
	if (SP - amx <= *tol)
	    return -dspt;
    }

/* L100: */
    do {
	--istk;
	if (istk == 0) {
	    SP -= amx;
	    if (SP - amx <= *tol)
		return -dspt;
	    else
		return SP - dspt;
	}
	l = lstk[istk] + 1;

	/* L110: */
	for(;; ++l) {
	    if (l > mstk[istk])	break;

	    n = nstk[istk];
	    nro = nrstk[istk];
	    nco = ncstk[istk];
	    y = ystk[istk];
	    if (n == 1) {
		if (irstk[l	+ istk * nrow] <
		    irstk[l - 1 + istk * nrow])	goto L60;
	    }
	    else if (n == 2) {
		if (icstk[l	+ istk * ncol] <
		    icstk[l - 1 + istk * ncol])	goto L60;
	    }
	}
    } while(1);
}


void
f5xact(double *pastp, const double *tol, int *kval, int *key, int *ldkey,
       int *ipoin, double *stp, int *ldstp, int *ifrq, int *npoin,
       int *nr, int *nl, int *ifreq, int *itop, Rboolean psh)
{
/*
  -----------------------------------------------------------------------
  Name:	      F5XACT aka "PUT"
  Purpose:    Put node on stack in network algorithm.

  Arguments:
     PASTP  - The past path length.				(Input)
     TOL    - Tolerance for equivalence of past path lengths.	(Input)
     KVAL   - Key value.					(Input)
     KEY    - Vector of length LDKEY containing the key values.	(in/out)
     LDKEY  - Length of vector KEY.				(Input)
     IPOIN  - Vector of length LDKEY pointing to the
	      linked list of past path lengths.		(in/out)
     STP    - Vector of length LSDTP containing the
	      linked lists of past path lengths.		(in/out)
     LDSTP  - Length of vector STP.				(Input)
     IFRQ   - Vector of length LDSTP containing the past path
	      frequencies.					(in/out)
     NPOIN  - Vector of length LDSTP containing the pointers to
	      the next past path length.			(in/out)
     NR	    - Vector of length LDSTP containing the right object
	      pointers in the tree of past path lengths.        (in/out)
     NL	    - Vector of length LDSTP containing the left object
	      pointers in the tree of past path lengths.        (in/out)
     IFREQ  - Frequency of the current path length.             (Input)
     ITOP   - Pointer to the top of STP.			(Input)
     PSH    - Logical.						(Input)
	      If PSH is true, the past path length is found in the
	      table KEY.  Otherwise the location of the past path
	      length is assumed known and to have been found in
	      a previous call. ==>>>>> USING "static" variables
  -----------------------------------------------------------------------
  */

    /* Local variables */
    static int itmp, ird, ipn, itp; /* << *need* static, see PSH above */
    double test1, test2;

    /* Parameter adjustments */
    --nl;
    --nr;
    --npoin;
    --ifrq;
    --stp;

    /* Function Body */
    if (psh) {
	/* Convert KVAL to int in range 1, ..., LDKEY. */
	ird = *kval % *ldkey;
	/* Search for an unused location */
	for (itp = ird; itp < *ldkey; ++itp) {
	    if (key[itp] == *kval)
		goto L40;

	    if (key[itp] < 0)
		goto L30;
	}
	for (itp = 0; itp < ird; ++itp) {
	    if (key[itp] == *kval)
		goto L40;

	    if (key[itp] < 0)
		goto L30;
	}
	/* Return if KEY array is full */
	/* KH
	  prterr(6, "LDKEY is too small for this problem.\n"
	  "It is not possible to estimate the value of LDKEY "
	  "required,\n"
	  "but twice the current value may be sufficient.");
	  */
	prterr(6, "LDKEY is too small for this problem.\n"
	       "Try increasing the size of the workspace.");


L30: /* Update KEY */

	key[itp] = *kval;
	++(*itop);
	ipoin[itp] = *itop;
	/* Return if STP array full */
	if (*itop > *ldstp) {
	    /* KH
	       prterr(7, "LDSTP is too small for this problem.\n"
	       "It is not possible to estimate the value of LDSTP "
	       "required,\n"
	       "but twice the current value may be sufficient.");
	       */
	    prterr(7, "LDSTP is too small for this problem.\n"
		   "Try increasing the size of the workspace.");
	}
	/* Update STP, etc. */
	npoin[*itop] = -1;
	nr   [*itop] = -1;
	nl   [*itop] = -1;
	stp  [*itop] = *pastp;
	ifrq [*itop] = *ifreq;
	return;
    }

L40: /* Find location, if any, of pastp */

    ipn = ipoin[itp];
    test1 = *pastp - *tol;
    test2 = *pastp + *tol;

    do {
	if (stp[ipn] < test1)
	    ipn = nl[ipn];
	else if (stp[ipn] > test2)
	    ipn = nr[ipn];
	else {
	    ifrq[ipn] += *ifreq;
	    return;
	}
    } while (ipn > 0);

    /* Return if STP array full */
    ++(*itop);
    if (*itop > *ldstp) {
	/*
	  prterr(7, "LDSTP is too small for this problem.\n"
	  "It is not possible to estimate the value of LDSTP "
	  "required,\n"
	  "but twice the current value may be sufficient.");
	  */
	prterr(7, "LDSTP is too small for this problem.\n"
	       "Try increasing the size of the workspace.");
	return;
    }

    /* Find location to add value */
    ipn = ipoin[itp];
    itmp = ipn;

L60:
    if (stp[ipn] < test1) {
	itmp = ipn;
	ipn = nl[ipn];
	if (ipn > 0)
	    goto L60;
	/* else */
	nl[itmp] = *itop;
    }
    else if (stp[ipn] > test2) {
	itmp = ipn;
	ipn = nr[ipn];
	if (ipn > 0)
	    goto L60;
	/* else */
	nr[itmp] = *itop;
    }
    /* Update STP, etc. */
    npoin[*itop] = npoin[itmp];
    npoin[itmp] = *itop;
    stp	 [*itop] = *pastp;
    ifrq [*itop] = *ifreq;
    nl	 [*itop] = -1;
    nr	 [*itop] = -1;
}


Rboolean
f6xact(int nrow, int *irow, int *kyy, int *key, int *ldkey, int *last, int *ipn)
{
/*
  -----------------------------------------------------------------------
  Name:	      F6XACT  aka "GET"
  Purpose:    Pop a node off the stack.

  Arguments:
    NROW    - The number of rows in the table.			(Input)
    IROW    - Vector of length nrow containing the row sums on
	      output.						(Output)
    KYY     - Constant mutlipliers used in forming the hash
	      table key.					(Input)
    KEY     - Vector of length LDKEY containing the hash table
	      keys.						(In/out)
    LDKEY   - Length of vector KEY.				(Input)
    LAST    - Index of the last key popped off the stack.	(In/out)
    IPN     - Pointer to the linked list of past path lengths.	(Output)

  Return value :
    TRUE if there are no additional nodes to process.           (Output)
  -----------------------------------------------------------------------
  */
    int kval, j;

    --key;

L10:
    ++(*last);
    if (*last <= *ldkey) {
	if (key[*last] < 0)
	    goto L10;

	/* Get KVAL from the stack */
	kval = key[*last];
	key[*last] = -9999;
	for (j = nrow-1; j > 0; j--) {
	    irow[j] = kval / kyy[j];
	    kval -= irow[j] * kyy[j];
	}
	irow[0] = kval;
	*ipn = *last;
	return FALSE;
    } else {
	*last = 0;
	return TRUE;
    }
}


void
f7xact(int nrow, int *imax, int *idif, int *k, int *ks, int *iflag)
{
/*
  -----------------------------------------------------------------------
  Name:	      F7XACT
  Purpose:    Generate the new nodes for given marginal totals.

  Arguments:
    NROW    - The number of rows in the table.			(Input)
    IMAX    - The row marginal totals.				(Input)
    IDIF    - The column counts for the new column.		(in/out)
    K	    - Indicator for the row to decrement.		(in/out)
    KS	    - Indicator for the row to increment.		(in/out)
    IFLAG   - Status indicator.					(Output)
	      If IFLAG is zero, a new table was generated.  For
	      IFLAG = 1, no additional tables could be generated.
  -----------------------------------------------------------------------
  */
    int i, m, kk, mm;

    /* Parameter adjustments */
    --idif;
    --imax;

    /* Function Body */
    *iflag = 0;
    /* Find node which can be incremented, ks */
    if (*ks == 0)
	do {
	    ++(*ks);
	} while (idif[*ks] == imax[*ks]);

    /* Find node to decrement (>ks) */
    if (idif[*k] > 0 && *k > *ks) {
	--idif[*k];
	do {
	    --(*k);
	} while(imax[*k] == 0);

	m = *k;

	/* Find node to increment (>=ks) */
	while (idif[m] >= imax[m]) {
	    --m;
	}
	++idif[m];
	/* Change ks */
	if (m == *ks && idif[m] == imax[m])
	    *ks = *k;
    }
    else {
 Loop:
	/* Check for finish */
	for (kk = *k + 1; kk <= nrow; ++kk) {
	    if (idif[kk] > 0) {
		goto L70;
	    }
	}
	*iflag = 1;
	return;

 L70:
	/* Reallocate counts */
	mm = 1;
	for (i = 1; i <= *k; ++i) {
	    mm += idif[i];
	    idif[i] = 0;
	}
	*k = kk;

	do {
	    --(*k);
	    m = imin2(mm, imax[*k]);
	    idif[*k] = m;
	    mm -= m;
	} while (mm > 0 && *k != 1);

	/* Check that all counts reallocated */
	if (mm > 0) {
	    if (kk != nrow) {
		*k = kk;
		goto Loop;
	    }
	    *iflag = 1;
	    return;
	}
	/* Get ks */
	--idif[kk];
	*ks = 0;
	do {
	    ++(*ks);
	    if (*ks > *k) {
		return;
	    }
	} while (idif[*ks] >= imax[*ks]);
    }
}


void f8xact(int *irow, int is, int i1, int izero, int *new)
{
/*
  -----------------------------------------------------------------------
  Name:	      F8XACT
  Purpose:    Routine for reducing a vector when there is a zero
	      element.
  Arguments:
     IROW   - Vector containing the row counts.			(Input)
     IS	    - Indicator.					(Input)
     I1	    - Indicator.					(Input)
     IZERO  - Position of the zero.				(Input)
     NEW    - Vector of new row counts.				(Output)
  -----------------------------------------------------------------------
  */

    int i;

    /* Parameter adjustments */
    --new;
    --irow;

    /* Function Body */
    for (i = 1; i < i1; ++i)
	new[i] = irow[i];

    for (i = i1; i <= izero - 1; ++i) {
	if (is >= irow[i + 1])
	    break;
	new[i] = irow[i + 1];
    }

    new[i] = is;

    for(;;) {
	++i;
	if (i > izero)
	    return;
	new[i] = irow[i];
    }
}

double f9xact(int n, int ntot, int *ir, double *fact)
{
/*
  -----------------------------------------------------------------------
  Name:	      F9XACT
  Purpose:    Computes the log of a multinomial coefficient.

  Arguments:
     N	    - Length of IR.					(Input)
     NTOT   - Number for factorial in numerator.		(Input)
     IR	    - Vector of length N containing the numbers for
	      the denominator of the factorial.			(Input)
     FACT   - Table of log factorials.				(Input)
  Returns:
	    - The log of the multinomal coefficient.		(Output)
  -----------------------------------------------------------------------
  */
    double d;
    int k;

    d = fact[ntot];
    for (k = 0; k < n; k++)
	d -= fact[ir[k]];
    return d;
}


Rboolean
f10act(int nrow, int *irow, int ncol, int *icol, double *val,
       double *fact, int *nd, int *ne, int *m)
{
/*
  -----------------------------------------------------------------------
  Name:	    F10ACT
  Purpose:  Computes the shortest path length for special tables.

  Arguments:
     NROW   - The number of rows in the table.			(Input)
     IROW   - Vector of length NROW containing the row totals.	(Input)
     NCOL   - The number of columns in the table.		(Input)
     ICO    - Vector of length NCOL containing the column totals.(Input)
     VAL    - The shortest path.				(Input/Output)
     FACT   - Vector containing the logarithms of factorials.   (Input)
     ND	    - Workspace vector of length NROW.			(Input)
     NE	    - Workspace vector of length NCOL.			(Input)
     M	    - Workspace vector of length NCOL.			(Input)

  Returns (VAL and):
     XMIN   - Set to true if shortest path obtained.		(Output)
  -----------------------------------------------------------------------
  */
    /* Local variables */
    int i, is, ix;

    /* Function Body */
    for (i = 0; i < nrow - 1; ++i)
	nd[i] = 0;

    is = icol[0] / nrow;
    ix = icol[0] - nrow * is;
    ne[0] = is;
    m[0] = ix;
    if (ix != 0)
	++nd[ix-1];

    for (i = 1; i < ncol; ++i) {
	ix = icol[i] / nrow;
	ne[i] = ix;
	is += ix;
	ix = icol[i] - nrow * ix;
	m[i] = ix;
	if (ix != 0)
	    ++nd[ix-1];
    }

    for (i = nrow - 3; i >= 0; --i)
	nd[i] += nd[i + 1];

    ix = 0;
    for (i = nrow; i >= 2; --i) {
	ix += is + nd[nrow - i] - irow[i-1];
	if (ix < 0)
	    return FALSE;
    }

    for (i = 0; i < ncol; ++i) {
	ix = ne[i];
	is = m[i];
	*val +=  is * fact[ix + 1] + (nrow - is) * fact[ix];
    }
    return TRUE;
}


void f11act(int *irow, int i1, int i2, int *new)
{
/*
  -----------------------------------------------------------------------
  Name:	      F11ACT
  Purpose:    Routine for revising row totals.

  Arguments:
     IROW   - Vector containing the row totals.	(Input)
     I1	    - Indicator.			(Input)
     I2	    - Indicator.			(Input)
     NEW    - Vector containing the row totals.	(Output)
  -----------------------------------------------------------------------
  */
    int i;

    for (i = 0;  i < (i1 - 1); ++i)	new[i] = irow[i];
    for (i = i1; i <= i2; ++i)	      new[i-1] = irow[i];

    return;
}


void NORET prterr(int icode, const char *mes)
{
/*
  -----------------------------------------------------------------------
  Name:	      prterr
  Purpose:    Print an error message and stop.

  Arguments:
     icode  - Integer code for the error message.		(Input)
     mes    - Character string containing the error message.	(Input)
  -----------------------------------------------------------------------
  */
    PROBLEM "FEXACT error %d.\n%s", icode, mes RECOVER(NULL_ENTRY);
}

int iwork(int iwkmax, int *iwkpt, int number, int itype)
{
/*
  -----------------------------------------------------------------------
  Name:	      iwork
  Purpose:    Routine for allocating workspace.

  Arguments:
     iwkmax - Maximum (int) amount of workspace.		(Input)
     iwkpt  - Amount of (int) workspace currently allocated.	(in/out)
     number - Number of elements of workspace desired.		(Input)
     itype  - Workspace type.					(Input)
	      ITYPE  TYPE
		2    integer
		3    float
		4    double
     iwork(): Index in rwrk, dwrk, or iwrk of the beginning of
	      the first free element in the workspace array.	(Output)
  -----------------------------------------------------------------------
  */
    int i;

    i = *iwkpt;
    if (itype == 2 || itype == 3)
	*iwkpt += number;
    else { /* double */
	if (i % 2 != 0)
	    ++i;
	*iwkpt += (number << 1);
	i /= 2;
    }
    if (*iwkpt > iwkmax)
	prterr(40, "Out of workspace.");

    return i;
}



#ifndef USING_R

void isort(int *n, int *ix)
{
/*
  -----------------------------------------------------------------------
  Name:	      ISORT
  Purpose:    Shell sort for an int vector.

  Arguments:
     N	    - Lenth of vector IX.	(Input)
     IX	    - Vector to be sorted.	(in/out)
  -----------------------------------------------------------------------
  */
    static int ikey, i, j, m, il[10], kl, it, iu[10], ku;

    /* Parameter adjustments */
    --ix;

    /* Function Body */
    m = 1;
    i = 1;
    j = *n;

L10:
    if (i >= j) {
	goto L40;
    }
    kl = i;
    ku = j;
    ikey = i;
    ++j;
    /* Find element in first half */
L20:
    ++i;
    if (i < j) {
	if (ix[ikey] > ix[i]) {
	    goto L20;
	}
    }
    /* Find element in second half */
L30:
    --j;
    if (ix[j] > ix[ikey]) {
	goto L30;
    }
    /* Interchange */
    if (i < j) {
	it = ix[i];
	ix[i] = ix[j];
	ix[j] = it;
	goto L20;
    }
    it = ix[ikey];
    ix[ikey] = ix[j];
    ix[j] = it;
    /* Save upper and lower subscripts of the array yet to be sorted */
    if (m < 11) {
	if (j - kl < ku - j) {
	    il[m - 1] = j + 1;
	    iu[m - 1] = ku;
	    i = kl;
	    --j;
	} else {
	    il[m - 1] = kl;
	    iu[m - 1] = j - 1;
	    i = j + 1;
	    j = ku;
	}
	++m;
	goto L10;
    } else {
	prterr(20, "This should never occur.");
    }
    /* Use another segment */
L40:
    --m;
    if (m == 0) {
	return;
    }
    i = il[m - 1];
    j = iu[m - 1];
    goto L10;
}

double gammds(double *y, double *p, int *ifault)
{
/*
  -----------------------------------------------------------------------
  Name:	      GAMMDS
  Purpose:    Cumulative distribution for the gamma distribution.
  Usage:      PGAMMA (Q, ALPHA,IFAULT)
  Arguments:
     Q	    - Value at which the distribution is desired.  (Input)
     ALPHA  - Parameter in the gamma distribution.  (Input)
     IFAULT - Error indicator.	(Output)
	       IFAULT  DEFINITION
		 0     No error
		 1     An argument is misspecified.
		 2     A numerical error has occurred.
     PGAMMA - The cdf for the gamma distribution with parameter alpha
	      evaluated at Q.  (Output)
  -----------------------------------------------------------------------

  Algorithm AS 147 APPL. Statist. (1980) VOL. 29, P. 113

  Computes the incomplete gamma integral for positive parameters Y, P
  using and infinite series.
  */

    static double a, c, f, g;
    static int ifail;

    /* Checks for the admissibility of arguments and value of F */
    *ifault = 1;
    g = 0.;
    if (*y <= 0. || *p <= 0.) {
	return g;
    }
    *ifault = 2;

    /*
      ALOGAM is natural log of gamma function no need to test ifail as
      an error is impossible
      */

    a = *p + 1.;
    f = exp(*p * log(*y) - alogam(&a, &ifail) - *y);
    if (f == 0.) {
	return g;
    }
    *ifault = 0;

    /* Series begins */
    c = 1.;
    g = 1.;
    a = *p;
L10:
    do {
	a += 1.;
	c *= (*y / a);
	g += c;
    } while (c > 1e-6 * g);

    g *= f;
    return g;
}

/*
  -----------------------------------------------------------------------
  Name:	      ALOGAM
  Purpose:    Value of the log-gamma function.
  Usage:      ALOGAM (X, IFAULT)
  Arguments:
     X	    - Value at which the log-gamma function is to be evaluated.
	      (Input)
     IFAULT  - Error indicator.	 (Output)
	       IFAULT  DEFINITION
		 0     No error
		 1     X < 0
     ALGAMA - The value of the log-gamma function at XX.  (Output)
  -----------------------------------------------------------------------

  Algorithm ACM 291, Comm. ACM. (1966) Vol. 9, P. 684

  Evaluates natural logarithm of gamma(x) for X greater than zero.
  */

double alogam(double *x, int *ifault)
{
    /* Initialized data */

    static double a1 = .918938533204673;
    static double a2 = 5.95238095238e-4;
    static double a3 = 7.93650793651e-4;
    static double a4 = .002777777777778;
    static double a5 = .083333333333333;

    /* Local variables */
    static double f, y, z;

    *ifault = 1;
    if (*x < 0.) {
	return(0.);
    }
    *ifault = 0;
    y = *x;
    f = 0.;
    if (y >= 7.) {
	goto L30;
    }
    f = y;
L10:
    y += 1.;
    if (y >= 7.) {
	goto L20;
    }
    f *= y;
    goto L10;
L20:
    f = -log(f);
L30:
    z = 1. / (y * y);
    return(f + (y - .5) * log(y) - y + a1 +
	   (((-a2 * z + a3) * z - a4) * z + a5) / y);
}

#endif /* not USING_R */

#include <Rinternals.h>

SEXP Fexact(SEXP x, SEXP pars, SEXP work, SEXP smult)
{
    int nr = nrows(x), nc = ncols(x), ws = asInteger(work),
	mult = asInteger(smult);
    pars = PROTECT(coerceVector(pars, REALSXP));
    double p, prt, *rp =  REAL(pars);
    fexact(&nr, &nc, INTEGER(x), &nr, rp, rp+1, rp+2, &prt, &p, &ws, &mult);
    UNPROTECT(1);
    return ScalarReal(p);
}
