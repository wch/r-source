/*
   Fisher's exact test for contingency tables -- usage see below

   fexact.f -- translated by f2c (version 19971204).
   Run through a slightly modified version of MM's f2c-clean.
   Heavily hand-edited by KH and MM.
 */

#include <R.h>

#include "ctest.h"

#undef min
#undef max
#define	max(a, b)		((a) < (b) ? (b) : (a))
#define	min(a, b)		((a) > (b) ? (b) : (a))

static void f2xact(Sint *nrow, Sint *ncol, double *table, Sint *ldtabl,
		  double *expect, double *percnt, double *emin, double
		  *prt, double *pre, double *fact, Sint *ico, Sint
		  *iro, Sint *kyy, Sint *idif, Sint *irn, Sint *key,
		  Sint *ldkey, Sint *ipoin, double *stp, Sint *ldstp,
		  Sint *ifrq, double *dlp, double *dsp, double *tm,
		  Sint *key2, Sint *iwk, double *rwk);
static void f3xact(Sint *nrow, Sint *irow, Sint *ncol,	Sint *icol,
		  double *dlp, Sint *mm, double *fact, Sint *ico, Sint
		  *iro, Sint *it, Sint *lb, Sint *nr, Sint *nt, Sint
		  *nu, Sint *itc, Sint *ist, double *stv, double *alen,
		  const double *tol);
static void f4xact(Sint *nrow, Sint *irow, Sint *ncol, Sint *icol,
		  double *dsp, double *fact, Sint *icstk, Sint *ncstk,
		  Sint *lstk, Sint *mstk, Sint *nstk, Sint *nrstk, Sint
		  *irstk, double *ystk, const double *tol);
static void f5xact(double *pastp, const double *tol, Sint *kval, Sint *key,
		  Sint *ldkey, Sint *ipoin, double *stp, Sint *ldstp,
		  Sint *ifrq, Sint *npoin, Sint *nr, Sint *nl, Sint
		  *ifreq, Sint *itop, Sint *ipsh);
static void f6xact(Sint *nrow, Sint *irow, Sint *iflag, Sint *kyy,
		   Sint *key, Sint *ldkey, Sint *last, Sint *ipn);
static void f7xact(Sint *nrow, Sint *imax, Sint *idif, Sint *k, Sint *ks,
		   Sint *iflag);
static void f8xact(Sint *irow, Sint *is, Sint *i1, Sint *izero, Sint *new);
static double f9xact(Sint *n, Sint *mm, Sint *ir, double *fact);
static void f10act(Sint *nrow, Sint *irow, Sint *ncol, Sint *icol,
		  double *val, Sint *xmin, double *fact, Sint *nd,
		  Sint *ne, Sint *m);
static void f11act(Sint *irow, Sint *i1, Sint *i2, Sint *new);
static void prterr(int icode, char *mes);
static Sint iwork(Sint iwkmax, Sint *iwkpt, Sint number, Sint itype);

#ifdef USING_R
# define isort(n, ix)		R_isort(ix, *n)
# include <Rmath.h>	/* -> pgamma() */
#else
 static void isort(Sint *n, Sint *ix);
 static double gammds(double *y, double *p, Sint *ifault);
 static double alogam(double *x, Sint *ifault);
#endif

/* The only public function : */
void
fexact(Sint *nrow, Sint *ncol, double *table, Sint *ldtabl,
       double *expect, double *percnt, double *emin, double *prt,
       double *pre, /* new in C : */ Sint *workspace)
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
  Usage:      CALL FEXACT (NROW, NCOL, TABLE, LDTABL, EXPECT, PERCNT,
                           EMIN, PRT, PRE)
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
  2. In FEXACT, LDSTP = 30*LDKEY.  The proportion of table space used
     by STP may be changed by changing the line MULT = 30 below to
     another value.
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

    /* To increase the length of the table of paste path lengths relative
       to the length of the hash table, increase MULT.
    */
    const Sint mult = 30;
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
    Sint ikh;
    /* Local variables */
    Sint nco, nro, ntot, numb, iiwk, irwk;
    Sint i, j, k, kk, ldkey, ldstp, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10;
    Sint i3a, i3b, i3c, i9a, iwkmax, iwkpt;

    /* Workspace Allocation (freed at end) */
    double *equiv;
    iwkmax = 2 * (Sint) (*workspace / 2);
    equiv = Calloc(iwkmax / 2, double);
    if (!equiv) {
	prterr(0, "Can not allocate specified workspace");
    }
#define dwrk (equiv)
#define iwrk ((Sint *)equiv)
#define rwrk ((float *)equiv)

    /* Parameter adjustments */
    table -= *ldtabl + 1;

    /* Function Body */
    iwkpt = 0;

    if (*nrow > *ldtabl)
	prterr(1, "NROW must be less than or equal to LDTABL.");

    ntot = 0;
    for (i = 1; i <= *nrow; ++i) {
	for (j = 1; j <= *ncol; ++j) {
	    if (table[i + j * *ldtabl] < 0.)
		prterr(2, "All elements of TABLE must be positive.");
	    ntot = (Sint) (ntot + table[i + j * *ldtabl]);
	}
    }
    if (ntot == 0) {
	prterr(3, "All elements of TABLE are zero.\n"
	       "PRT and PRE are set to missing values.");
	*prt = amiss;
	*pre = amiss;
	goto L_End;
    }

    nco = max(*nrow, *ncol);
    nro = *nrow + *ncol - nco;/* = min(*nrow, *ncol) */
    k = *nrow + *ncol + 1;
    kk = k * nco;

    ikh = ntot + 1;
    i1  = iwork(iwkmax, &iwkpt, ikh, i_real);
    i2  = iwork(iwkmax, &iwkpt, nco, i_int);
    i3  = iwork(iwkmax, &iwkpt, nco, i_int);
    i3a = iwork(iwkmax, &iwkpt, nco, i_int);
    i3b = iwork(iwkmax, &iwkpt, nro, i_int);
    i3c = iwork(iwkmax, &iwkpt, nro, i_int);
    ikh = max(k * 5 + (kk << 1), nco * 7 + 800);
    iiwk= iwork(iwkmax, &iwkpt, ikh, i_int);
    ikh = max(nco + 401, k);
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
	numb = 18 + 10 * mult;
    } else {			/* Single precision reals */
	numb = (mult << 3) + 12;
    }
    ldkey = (iwkmax - iwkpt) / numb - 1;
    ldstp = mult * ldkey;
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
    f2xact(nrow,
	   ncol,
	   &table[*ldtabl + 1],
	   ldtabl,
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

L_End:
    Free(equiv);
    return;
}

#undef rwrk
#undef iwrk
#undef dwrk


/*
  -----------------------------------------------------------------------
  Name:		F2XACT
  Purpose:	Computes Fisher's exact test for a contingency table,
		routine with workspace variables specified.
  Usage:	F2XACT (NROW, NCOL, TABLE, LDTABL, EXPECT, PERCNT,
			EMIN, PRT, PRE, FACT, ICO, IRO, KYY, IDIF,
			IRN, KEY, LDKEY, IPOIN, STP, LDSTP, IFRQ,
			DLP, DSP, TM, KEY2, IWK, RWK)
  -----------------------------------------------------------------------
  */
void
f2xact(Sint *nrow, Sint *ncol, double *table, Sint *ldtabl,
       double *expect, double *percnt, double *emin, double *prt,
       double *pre, double *fact, Sint *ico, Sint *iro, Sint *kyy,
       Sint *idif, Sint *irn, Sint *key, Sint *ldkey, Sint *ipoin,
       double *stp, Sint *ldstp, Sint *ifrq, double *dlp, double *dsp,
       double *tm, Sint *key2, Sint *iwk, double *rwk)
{
    /* IMAX is the largest representable Sint on the machine. */
    const Sint imax = SINT_MAX;

    /* AMISS is a missing value indicator which is returned when the
       probability is not defined. */
    const double amiss = -12345.;

    /* TOL is chosen as the square root of the smallest relative spacing. */
#ifndef Macintosh
    const  static double tol = 3.45254e-7;
#else
    static double tol = 3.45254e-7;
#endif    
    /* EMX is a large positive value used in comparing expected values. */
    const static double emx = 1e30;

    /* Local variables {{any really need to be static ???}} */
    static Sint kval, kmax, jkey, last, ipsh, itmp, itop, jstp, ntot,
	jstp2, jstp3, jstp4, i, ii, j, k, n, iflag, ncell, ifreq, chisq,
	ikkey, ikstp, ikstp2, k1, kb, kd, ks,
	i31, i32, i33, i34, i35, i36, i37, i38, i39,
	i41, i42, i43, i44, i45, i46, i47, i48, i310, i311,
	nco, nrb, ipn, ipo, itp, nro, nro2;
    static double dspt, dd, df,ddf, drn,dro, emn, obs, obs2, obs3,
	pastp, pv, tmp;
    double d1;
#ifndef USING_R
    double d2;
    static Sint ifault;
#endif
    Rboolean nr_gt_nc;

    /* Parameter adjustments */
    table -= *ldtabl + 1;
    --ico;
    --iro;
    --kyy;
    --idif;
    --irn;
    --key;
    --ipoin;
    --stp;
    --ifrq;
    --dlp;
    --dsp;
    --tm;
    --key2;
    --iwk;
    --rwk;


    /* Check table dimensions */
    if (*nrow > *ldtabl)
	prterr(1, "NROW must be less than or equal to LDTABL.");
    if (*ncol <= 1)
	prterr(4, "NCOL must be at least 2");

    /* Initialize KEY array */
    for (i = 1; i <= *ldkey << 1; ++i) {
	key[i] = -9999;
	key2[i] = -9999;
    }
    /* Initialize parameters */
    *pre = 0.;
    itop = 0;
    if (*expect > 0.)
	emn = *emin;
    else
	emn = emx;

    nr_gt_nc =  *nrow > *ncol;
    /* nco := max(nrow, ncol) : */
    if(nr_gt_nc)
	nco = *nrow;
    else
	nco = *ncol;
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
    i311 = 401;
    /* f4xact */
    k = *nrow + *ncol + 1;
    i41 = 1;
    i42 = i41 + k;
    i43 = i42 + k;
    i44 = i43 + k;
    i45 = i44 + k;
    i46 = i45 + k;
    i47 = i46 + k * nco;
    i48 = 1;

    /* Compute row marginals and total */
    ntot = 0;
    for (i = 1; i <= *nrow; ++i) {
	iro[i] = 0;
	for (j = 1; j <= *ncol; ++j) {
	    if (table[i + j * *ldtabl] < -1e-4)
		prterr(2, "All elements of TABLE must be positive.");
	    iro[i] += (Sint) table[i + j * *ldtabl];
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
    for (i = 1; i <= *ncol; ++i) {
	ico[i] = 0;
	for (j = 1; j <= *nrow; ++j)
	    ico[i] += (Sint) table[j + i * *ldtabl];
    }

    /* sort marginals */
    isort(nrow, &iro[1]);
    isort(ncol, &ico[1]);

    /*	Determine row and column marginals.
	Define max(nrow,ncol) =: nco >= nro := min(nrow,ncol)
	nco is defined above

	Swap marginals if necessary to	ico[1:nco] & iro[1:nro]
     */
    if (nr_gt_nc) {
	nro = *ncol;
	/* Swap marginals */
	for (i = 1; i <= nco; ++i) {
	    itmp = iro[i];
	    if (i <= nro)
		iro[i] = ico[i];
	    ico[i] = itmp;
	}
    } else
	nro = *nrow;


    /* Get multiplers for stack */
    kyy[1] = 1;
    for (i = 2; i <= nro; ++i) {
	/* Hash table multipliers */
	if (iro[i - 1] + 1 <= imax / kyy[i - 1]) {
	    kyy[i] = kyy[i - 1] * (iro[i - 1] + 1);
	    j /= kyy[i - 1];
	} else
	    goto L_ERR_5;
    }
    /* Maximum product */
    if (iro[nro - 1] + 1 <= imax / kyy[nro - 1]) {
	kmax = (iro[nro] + 1) * kyy[nro - 1];
    } else {
    L_ERR_5:
	prterr(5, "The hash table key cannot be computed because "
	       "the largest key\n"
	       "is larger than the largest representable Sint.\n"
	       "The algorithm cannot proceed.\n"
	       "Reduce the workspace size, or use `exact = FALSE'.");
	return;
    }

    /* Compute log factorials */
    fact[0] = 0.;
    fact[1] = 0.;
    fact[2] = log(2.);/* MM: old code assuming log() to be SLOW */
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
	for (i = 1; i <= nro; ++i) {
	    if (nr_gt_nc) {
		dd += fact[(Sint) table[j + i * *ldtabl]];
		ntot +=    (Sint) table[j + i * *ldtabl];
	    } else {
		dd += fact[(Sint) table[i + j * *ldtabl]];
		ntot +=    (Sint) table[i + j * *ldtabl];
	    }
	}
	obs += fact[ico[j]] - dd;
    }
    /* Denominator of observed table: DRO */
    dro = f9xact(&nro, &ntot, &iro[1], fact);
    *prt = exp(obs - dro);
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
	ntot = min(n, iro[kd]);
	idif[kd] = ntot;
	if (idif[kmax] == 0)
	    --kmax;
	n -= ntot;
    }
    while (n > 0 && kd != 1);

    if (n != 0) {
	goto L310;
    }

    k1 = k - 1;
    n = ico[kb];
    ntot = 0;
    for (i = kb + 1; i <= nco; ++i)
	ntot += ico[i];


L150:
    /* Arc to daughter length=ICO(KB) */
    for (i = 1; i <= nro; ++i)
	irn[i] = iro[i] - idif[i];

    /* Sort irn */
    if (k1 > 1) {
	if (nro == 2) {
	    if (irn[1] > irn[2]) {
		ii = irn[1];
		irn[1] = irn[2];
		irn[2] = ii;
	    }
	} else if (nro == 3) {
	    ii = irn[1];
	    if (ii > irn[3]) {
		if (ii > irn[2]) {
		    if (irn[2] > irn[3]) {
			irn[1] = irn[3];
			irn[3] = ii;
		    } else {
			irn[1] = irn[2];
			irn[2] = irn[3];
			irn[3] = ii;
		    }
		} else {
		    irn[1] = irn[3];
		    irn[3] = irn[2];
		    irn[2] = ii;
		}
	    } else if (ii > irn[2]) {
		irn[1] = irn[2];
		irn[2] = ii;
	    } else if (irn[2] > irn[3]) {
		ii = irn[2];
		irn[2] = irn[3];
		irn[3] = ii;
	    }
	} else {
	    for (j = 2; j <= nro; ++j) {
		i = j - 1;
		ii = irn[j];

		while (ii < irn[i]) {
		    irn[i + 1] = irn[i];
		    --i;
		    if (i == 0)
			break;
		}
		irn[i + 1] = ii;
	    }
	}
	/* Adjust start for zero */
	for (i = 1; i <= nro; ++i) {
	    if (irn[i] != 0)
		break;
	}

	nrb = i;
	nro2 = nro - i + 1;
    } else {
	nrb = 1;
	nro2 = nro;
    }
    /* Some table values */
    ddf = f9xact(&nro, &n, &idif[1], fact);
    drn = f9xact(&nro2, &ntot, &irn[nrb], fact) - dro + ddf;
    /* Get hash value */
    if (k1 > 1) {
	kval = irn[1] + irn[2] * kyy[2];
	for (i = 3; i <= nro; ++i) {
	    kval += irn[i] * kyy[i];
	}
	/* Get hash table entry */
	i = kval % (*ldkey << 1) + 1;
	/* Search for unused location */
	for (itp = i; itp <= *ldkey << 1; ++itp) {
	    ii = key2[itp];
	    if (ii == kval) {
		goto L240;
	    } else if (ii < 0) {
		key2[itp] = kval;
		dlp[itp] = 1.;
		dsp[itp] = 1.;
		goto L240;
	    }
	}

	for (itp = 1; itp <= i - 1; ++itp) {
	    ii = key2[itp];
	    if (ii == kval) {
		goto L240;
	    } else if (ii < 0) {
		key2[itp] = kval;
		dlp[itp] = 1.;
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
    ipsh = (1);
    /* Recover pastp */
    ipn = ipoin[ipo + ikkey];
    pastp = stp[ipn + ikstp];
    ifreq = ifrq[ipn + ikstp];
    /* Compute shortest and longest path */
    if (k1 > 1) {
	obs2 = obs - fact[ico[kb + 1]] - fact[ico[kb + 2]] - ddf;
	for (i = 3; i <= k1; ++i) {
	    obs2 -= fact[ico[kb + i]];
	}
	if (dlp[itp] > 0.) {
	    dspt = obs - obs2 - ddf;
	    /* Compute longest path */
	    dlp[itp] = 0.;
	    f3xact(&nro2, &irn[nrb], &k1, &ico[kb + 1], &dlp[itp],
		   &ntot, fact, &iwk[i31], &iwk[i32], &iwk[i33],
		   &iwk[i34], &iwk[i35], &iwk[i36], &iwk[i37],
		   &iwk[i38], &iwk[i39], &rwk[i310], &rwk[i311], &tol);
	    dlp[itp] = min(0., dlp[itp]);
	    /* Compute shortest path */
	    dsp[itp] = dspt;
	    f4xact(&nro2, &irn[nrb], &k1, &ico[kb + 1], &dsp[itp], fact,
		   &iwk[i47], &iwk[i41], &iwk[i42], &iwk[i43],
		   &iwk[i44], &iwk[i45], &iwk[i46], &rwk[i48], &tol);
	    dsp[itp] = min(0., dsp[itp] - dspt);
	    /* Use chi-squared approximation? */
	    if ((irn[nrb] * ico[kb + 1]) > ntot * emn) {
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
		    /* tm(itp) set to a flag value */
		    tm[itp] = -9876.;
		}
	    } else {
		tm[itp] = -9876.;
	    }
	}
	obs3 = obs2 - dlp[itp];
	obs2 -= dsp[itp];
	if (tm[itp] == -9876.) {
	    chisq = (0);
	} else {
	    chisq = (1);
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
	    d1 = max(0., tmp + (pastp + drn) * 2.) / 2.;
	    d2 = df / 2.;
	    pv = 1. - gammds(&d1, &d2, &ifault);
#endif
	    *pre += (double) ifreq * exp(pastp + drn) * pv;
	} else {
	    /* Put daughter on queue */
	    d1 = pastp + ddf;
	    f5xact(&d1, &tol, &kval, &key[jkey], ldkey, &ipoin[jkey],
		   &stp[jstp], ldstp, &ifrq[jstp], &ifrq[jstp2],
		   &ifrq[jstp3], &ifrq[jstp4], &ifreq, &itop, &ipsh);
	    ipsh = (0);
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
    f7xact(&kmax, &iro[1], &idif[1], &kd, &ks, &iflag);
    if (iflag != 1) {
	goto L150;
    }

L310:
    /* Go get a new mother from stage K */
    do {
	iflag = 1;
	f6xact(&nro, &iro[1], &iflag, &kyy[1], &key[ikkey + 1], ldkey,
	       &last, &ipo);
	/* Update pointers */
	if (iflag != 3)
	    goto Outer_Loop;
	/* else  iflag == 3 : no additional nodes to process */
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
}

/*
  -----------------------------------------------------------------------
  Name:	      F3XACT
  Purpose:    Computes the shortest path length for a given table.
  Usage:      F3XACT (NROW, IROW, NCOL, ICOL, DLP, MM, FACT, ICO, IRO,
		      IT, LB, NR, NT, NU, ITC, IST, STV, ALEN, TOL)
  Arguments:
    NROW    - The number of rows in the table.			(Input)
    IROW    - Vector of length NROW containing the row sums
              for the table.					(Input)
    NCOL    - The number of columns in the table.		(Input)
    ICOL    - Vector of length K containing the column sums
              for the table.					(Input)
    DLP     - The longest path for the table.			(Output)
    MM	    - The total count in the table.			(Output)
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
  -----------------------------------------------------------------------
  */

void
f3xact(Sint *nrow, Sint *irow, Sint *ncol, Sint *icol, double *dlp,
       Sint *mm, double *fact, Sint *ico, Sint *iro, Sint *it,
       Sint *lb, Sint *nr, Sint *nt, Sint *nu, Sint *itc, Sint *ist,
       double *stv, double *alen, const double *tol)
{
    /* Initialized data */
    static Sint ldst = 200;
    static Sint nst = 0;
    static Sint nitc = 0;

    /* Local variables */
    static Sint xmin;
    static Sint i, k;
    static double v;
    static Sint n11, n12, ii, nn, ks, ic1, ic2, nc1, nn1;
    static Sint nr1, nco;
    static double val;
    static Sint nct, ipn, irl, key, lev, itp, nro;
    static double vmn;
    static Sint nrt, kyy, nc1s;

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

    /* Function Body */
    for (i = 0; i <= *ncol; ++i) {
	alen[i] = 0.;
    }
    for (i = 1; i <= 400; ++i) {
	ist[i] = -1;
    }
    /* nrow is 1 */
    if (*nrow <= 1) {
	if (*nrow > 0) {
	    *dlp -= fact[icol[1]];
	    for (i = 2; i <= *ncol; ++i) {
		*dlp -= fact[icol[i]];
	    }
	}
	return;
    }
    /* ncol is 1 */
    if (*ncol <= 1) {
	if (*ncol > 0) {
	    *dlp = *dlp - fact[irow[1]] - fact[irow[2]];
	    for (i = 3; i <= *nrow; ++i) {
		*dlp -= fact[irow[i]];
	    }
	}
	return;
    }
    /* 2 by 2 table */
    if (*nrow * *ncol == 4) {
	n11 = (irow[1] + 1) * (icol[1] + 1) / (*mm + 2);
	n12 = irow[1] - n11;
	*dlp = *dlp - fact[n11] - fact[n12] - fact[icol[1] - n11]
	    - fact[icol[2] - n12];
	return;
    }
    /* Test for optimal table */
    val = 0.;
    xmin = (0);
    if (irow[*nrow] <= irow[1] + *ncol) {
	f10act(nrow, &irow[1], ncol, &icol[1], &val, &xmin, fact,
	       &lb[1], &nu[1], &nr[1]);
    }
    if (! xmin) {
	if (icol[*ncol] <= icol[1] + *nrow) {
	    f10act(ncol, &icol[1], nrow, &irow[1], &val, &xmin, fact,
		   &lb[1], &nu[1], &nr[1]);
	}
    }

    if (xmin) {
	*dlp -= val;
	return;
    }
    /* Setup for dynamic programming */
    nn = *mm;
    /* Minimize ncol */
    if (*nrow >= *ncol) {
	nro = *nrow;
	nco = *ncol;
	for (i = 1; i <= *nrow; ++i) {
	    iro[i] = irow[i];
	}
	ico[1] = icol[1];
	nt[1] = nn - ico[1];
	for (i = 2; i <= *ncol; ++i) {
	    ico[i] = icol[i];
	    nt[i] = nt[i - 1] - ico[i];
	}
    } else {
	nro = *ncol;
	nco = *nrow;
	ico[1] = irow[1];
	nt[1] = nn - ico[1];
	for (i = 2; i <= *nrow; ++i) {
	    ico[i] = irow[i];
	    nt[i] = nt[i - 1] - ico[i];
	}
	for (i = 1; i <= *ncol; ++i)
	    iro[i] = icol[i];
    }
    /* Initialize pointers */
    vmn = 1e10;
    nc1s = nco - 1;
    irl = 1;
    ks = 0;
    k = ldst;
    kyy = ico[nco] + 1;

LnewNode: /* Setup to generate new node */

    lev = 1;
    nr1 = nro - 1;
    nrt = iro[irl];
    nct = ico[1];
    lb[1] = (Sint) ((double) ((nrt + 1) * (nct + 1)) /
		    (double) (nn + nr1 * nc1s + 1) - *tol) - 1;
    nu[1] = (Sint) ((double) ((nrt + nc1s) * (nct + nr1)) /
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
L120:
    alen[lev] = alen[lev - 1] + fact[lb[lev]];
    if (lev < nc1s) {
	nn1 = nt[lev];
	nrt = nr[lev];
	++lev;
	nc1 = nco - lev;
	nct = ico[lev];
	lb[lev] = (Sint) ((double) ((nrt + 1) * (nct + 1)) /
			  (double) (nn1 + nr1 * nc1 + 1) - *tol);
	nu[lev] = (Sint) ((double) ((nrt + nc1) * (nct + nr1)) /
			  (double) (nn1 + nr1 + nc1) - lb[lev] + 1);
	nr[lev] = nrt - lb[lev];
	goto L120;
    }
    alen[nco] = alen[lev] + fact[nr[lev]];
    lb[nco] = nr[lev];

    v = val + alen[nco];
    if (nro == 2) {
	/* Only 1 row left */
	v = v + fact[ico[1] - lb[1]] + fact[ico[2] - lb[2]];
	for (i = 3; i <= nco; ++i) {
	    v += fact[ico[i] - lb[i]];
	}
	if (v < vmn) {
	    vmn = v;
	}
    } else if (nro == 3 && nco == 2) {
	/* 3 rows and 2 columns */
	nn1 = nn - iro[irl] + 2;
	ic1 = ico[1] - lb[1];
	ic2 = ico[2] - lb[2];
	n11 = (iro[irl + 1] + 1) * (ic1 + 1) / nn1;
	n12 = iro[irl + 1] - n11;
	v = v + fact[n11] + fact[n12] + fact[ic1 - n11]
	    + fact[ic2 - n12];
	if (v < vmn) {
	    vmn = v;
	}
    } else {
	/* Column marginals are new node */
	for (i = 1; i <= nco; ++i) {
	    it[i] = ico[i] - lb[i];
	}
	/* Sort column marginals */
	if (nco == 2) {
	    if (it[1] > it[2]) {
		ii = it[1];
		it[1] = it[2];
		it[2] = ii;
	    }
	} else if (nco == 3) {
	    ii = it[1];
	    if (ii > it[3]) {
		if (ii > it[2]) {
		    if (it[2] > it[3]) {
			it[1] = it[3];
			it[3] = ii;
		    } else {
			it[1] = it[2];
			it[2] = it[3];
			it[3] = ii;
		    }
		} else {
		    it[1] = it[3];
		    it[3] = it[2];
		    it[2] = ii;
		}
	    } else if (ii > it[2]) {
		it[1] = it[2];
		it[2] = ii;
	    } else if (it[2] > it[3]) {
		ii = it[2];
		it[2] = it[3];
		it[3] = ii;
	    }
	} else {
	    isort(&nco, &it[1]);
	}
	/* Compute hash value */
	key = it[1] * kyy + it[2];
	for (i = 3; i <= nco; ++i) {
	    key = it[i] + key * kyy;
	}
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
	stv[ii] = min(v, stv[ii]);
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
	xmin = (0);
	if (iro[nro] <= iro[irl] + nco) {
	    f10act(&nro, &iro[irl], &nco, &ico[1], &val, &xmin, fact,
		   &lb[1], &nu[1], &nr[1]);
	}
	if (!xmin && ico[nco] <= ico[1] + nro)
	    f10act(&nco, &ico[1], &nro, &iro[irl], &val, &xmin, fact,
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

    *dlp -= vmn;
}

/*
  -----------------------------------------------------------------------
  Name:	      F4XACT
  Purpose:    Computes the longest path length for a given table.
  Usage:      CALL F4XACT (NROW, IROW, NCOL, ICOL, DSP, FACT, ICSTK,
			  NCSTK, LSTK, MSTK, NSTK, NRSTK, IRSTK, YSTK,
			  TOL)
  Arguments:
     NROW   - The number of rows in the table.	(Input)
     IROW   - Vector of length NROW containing the row sums for the
	      table.  (Input)
     NCOL   - The number of columns in the table.  (Input)
     ICOL   - Vector of length K containing the column sums for the
	      table.  (Input)
     DSP    - The shortest path for the table.	(Output)
     FACT   - Vector containing the logarithms of factorials.  (Input)
     ICSTK  - NCOL by NROW+NCOL+1 work array.
     NCSTK  - Work vector of length NROW+NCOL+1.
     LSTK   - Work vector of length NROW+NCOL+1.
     MSTK   - Work vector of length NROW+NCOL+1.
     NSTK   - Work vector of length NROW+NCOL+1.
     NRSTK  - Work vector of length NROW+NCOL+1.
     IRSTK  - NROW by MAX(NROW,NCOL) work array.
     YSTK   - Work vector of length NROW+NCOL+1.
     TOL    - Tolerance.  (Input)
  -----------------------------------------------------------------------
  */

void
f4xact(Sint *nrow, Sint *irow, Sint *ncol, Sint *icol, double *dsp,
       double *fact, Sint *icstk, Sint *ncstk, Sint *lstk, Sint *mstk,
       Sint *nstk, Sint *nrstk, Sint *irstk, double *ystk, const double *tol)
{
    /* System generated locals */
    Sint ikh;

    /* Local variables */
    Sint i, j, k, l, m, n, mn, ic1, ir1, ict, irt, istk, nco, nro;
    double y, amx;

    /* Parameter adjustments */
    irstk -= *nrow + 1;
    --irow;
    icstk -= *ncol + 1;
    --icol;
    --ncstk;
    --lstk;
    --mstk;
    --nstk;
    --nrstk;
    --ystk;

    /* Function Body */
    /* Take care of the easy cases first */
    if (*nrow == 1) {
	for (i = 1; i <= *ncol; ++i) {
	    *dsp -= fact[icol[i]];
	}
	return;
    }
    if (*ncol == 1) {
	for (i = 1; i <= *nrow; ++i) {
	    *dsp -= fact[irow[i]];
	}
	return;
    }
    if (*nrow * *ncol == 4) {
	if (irow[2] <= icol[2]) {
	    *dsp = *dsp - fact[irow[2]] - fact[icol[1]]
		- fact[icol[2] - irow[2]];
	} else {
	    *dsp = *dsp - fact[icol[2]] - fact[irow[1]]
		- fact[irow[2] - icol[2]];
	}
	return;
    }
    /* initialization before loop */
    for (i = 1; i <= *nrow; ++i) {
	irstk[i + *nrow] = irow[*nrow - i + 1];
    }
    for (j = 1; j <= *ncol; ++j) {
	icstk[j + *ncol] = icol[*ncol - j + 1];
    }

    nro = *nrow;
    nco = *ncol;
    nrstk[1] = nro;
    ncstk[1] = nco;
    ystk[1] = 0.;
    y = 0.;
    istk = 1;
    l = 1;
    amx = 0.;

    /* First LOOP */
    do {
	ir1 = irstk[istk * *nrow + 1];
	ic1 = icstk[istk * *ncol + 1];
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

	irt = irstk[i + istk * *nrow];
	ict = icstk[j + istk * *ncol];
	mn = irt;
	if (mn > ict) {
	    mn = ict;
	}
	y += fact[mn];
	if (irt == ict) {
	    --nro;
	    --nco;
	    f11act(&irstk[istk * *nrow + 1], &i, &nro,
		   &irstk[(istk + 1) * *nrow + 1]);
	    f11act(&icstk[istk * *ncol + 1], &j, &nco,
		   &icstk[(istk + 1) * *ncol + 1]);
	} else if (irt > ict) {
	    --nco;
	    f11act(&icstk[istk * *ncol + 1], &j, &nco,
		   &icstk[(istk + 1) * *ncol + 1]);
	    ikh = irt - ict;
	    f8xact(&irstk[istk * *nrow + 1], &ikh, &i,
		   &nro, &irstk[(istk + 1) * *nrow + 1]);
	} else {
	    --nro;
	    f11act(&irstk[istk * *nrow + 1], &i, &nro,
		   &irstk[(istk + 1) * *nrow + 1]);
	    ikh = ict - irt;
	    f8xact(&icstk[istk * *ncol + 1], &ikh, &j,
		   &nco, &icstk[(istk + 1) * *ncol + 1]);
	}

	if (nro == 1) {
	    for (k = 1; k <= nco; ++k) {
		y += fact[icstk[k + (istk + 1) * *ncol]];
	    }
	    break;
	}
	if (nco == 1) {
	    for (k = 1; k <= nro; ++k) {
		y += fact[irstk[k + (istk + 1) * *nrow]];
	    }
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
	if (*dsp - amx <= *tol) {
	    *dsp = 0.;
	    return;
	}
    }

L100:
    --istk;
    if (istk == 0) {
	*dsp -= amx;
	if (*dsp - amx <= *tol) {
	    *dsp = 0.;
	}
	return;
    }
    l = lstk[istk] + 1;

/* L110: */
    for(;; ++l) {
	if (l > mstk[istk])	goto L100;

	n = nstk[istk];
	nro = nrstk[istk];
	nco = ncstk[istk];
	y = ystk[istk];
	if (n == 1) {
	    if (irstk[l	    + istk * *nrow] <
		irstk[l - 1 + istk * *nrow])	goto L60;
	}
	else if (n == 2) {
	    if (icstk[l	    + istk * *ncol] <
		icstk[l - 1 + istk * *ncol])	goto L60;
	}
    }
}

/*
  -----------------------------------------------------------------------
  Name:	      F5XACT
  Purpose:    Put node on stack in network algorithm.
  Usage:      CALL F5XACT (PASTP, TOL, KVAL, KEY, LDKEY, IPOIN, STP,
			  LDSTP, IFRQ, NPOIN, NR, NL, IFREQ, ITOP,
			  IPSH)
  Arguments:
     PASTP  - The past path length.				(Input)
     TOL    - Tolerance for equivalence of past path lengths.  	(Input)
     KVAL   - Key value.  					(Input)
     KEY    - Vector of length LDKEY containing the key values.	(in/out)
     LDKEY  - Length of vector KEY.  				(Input)
     IPOIN  - Vector of length LDKEY pointing to the
	      linked list of past path lengths.  		(in/out)
     STP    - Vector of length LSDTP containing the
	      linked lists of past path lengths.  		(in/out)
     LDSTP  - Length of vector STP.  				(Input)
     IFRQ   - Vector of length LDSTP containing the past path
	      frequencies.  					(in/out)
     NPOIN  - Vector of length LDSTP containing the pointers to
	      the next past path length.  			(in/out)
     NR	    - Vector of length LDSTP containing the right object
	      pointers in the tree of past path lengths.        (in/out)
     NL	    - Vector of length LDSTP containing the left object
	      pointers in the tree of past path lengths.        (in/out)
     IFREQ  - Frequency of the current path length.             (Input)
     ITOP   - Pointer to the top of STP.  			(Input)
     IPSH   - Option parameter.	 				(Input)
	      If IPSH is true, the past path length is found in the
	      table KEY.  Otherwise the location of the past path
	      length is assumed known and to have been found in
	      a previous call. ==>>>>> USING "static" variables
  -----------------------------------------------------------------------
  */

void
f5xact(double *pastp, const double *tol, Sint *kval, Sint *key, Sint *ldkey,
       Sint *ipoin, double *stp, Sint *ldstp, Sint *ifrq, Sint *npoin,
       Sint *nr, Sint *nl, Sint *ifreq, Sint *itop, Sint *ipsh)
{
    /* Local variables */
    static Sint itmp, ird, ipn, itp;
    double test1, test2;

    /* Parameter adjustments */
    --nl;
    --nr;
    --npoin;
    --ifrq;
    --stp;
    --ipoin;
    --key;

    /* Function Body */
    if (*ipsh) {
	/* Convert KVAL to Sint in range 1, ..., LDKEY. */
	ird = *kval % *ldkey + 1;
	/* Search for an unused location */
	for (itp = ird; itp <= *ldkey; ++itp) {
	    if (key[itp] == *kval) {
		goto L40;
	    }
	    if (key[itp] < 0) {
		goto L30;
	    }
	}
	for (itp = 1; itp <= ird - 1; ++itp) {
	    if (key[itp] == *kval) {
		goto L40;
	    }
	    if (key[itp] < 0) {
		goto L30;
	    }
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

	/* Update KEY */
L30:
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
	nr[*itop] = -1;
	nl[*itop] = -1;
	stp[*itop] = *pastp;
	ifrq[*itop] = *ifreq;
	return;
    }

    /* Find location, if any, of pastp */
L40:
    ipn = ipoin[itp];
    test1 = *pastp - *tol;
    test2 = *pastp + *tol;

L50:
    if (stp[ipn] < test1) {
	ipn = nl[ipn];
	if (ipn > 0) {
	    goto L50;
	}
    } else if (stp[ipn] > test2) {
	ipn = nr[ipn];
	if (ipn > 0) {
	    goto L50;
	}
    } else {
	ifrq[ipn] += *ifreq;
	return;
    }
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
	if (ipn > 0) {
	    goto L60;
	} else {
	    nl[itmp] = *itop;
	}
    } else if (stp[ipn] > test2) {
	itmp = ipn;
	ipn = nr[ipn];
	if (ipn > 0) {
	    goto L60;
	} else {
	    nr[itmp] = *itop;
	}
    }
    /* Update STP, etc. */
    npoin[*itop] = npoin[itmp];
    npoin[itmp] = *itop;
    stp[*itop] = *pastp;
    ifrq[*itop] = *ifreq;
    nl[*itop] = -1;
    nr[*itop] = -1;
}

/*
  -----------------------------------------------------------------------
  Name:	      F6XACT
  Purpose:    Pop a node off the stack.
  Usage:      CALL F6XACT (NROW, IROW, IFLAG, KYY, KEY, LDKEY, LAST, IPN)
  Arguments:
    NROW    - The number of rows in the table.			(Input)
    IROW    - Vector of length nrow containing the row sums on
              output.						(Output)
    IFLAG   - Set to 3 if there are no additional nodes to process.
								(Output)
    KYY     - Constant mutlipliers used in forming the hash
              table key.					(Input)
    KEY     - Vector of length LDKEY containing the hash table
              keys.						(In/out)
    LDKEY   - Length of vector KEY.				(Input)
    LAST    - Index of the last key popped off the stack.	(In/out)
    IPN     - Pointer to the linked list of past path lengths.	(Output)
  -----------------------------------------------------------------------
  */
void
f6xact(Sint *nrow, Sint *irow, Sint *iflag, Sint *kyy, Sint *key, Sint
       *ldkey, Sint *last, Sint *ipn)
{
    Sint kval, j;

    /* Parameter adjustments */
    --key;
    --kyy;
    --irow;

    /* Function Body */
L10:
    ++(*last);
    if (*last <= *ldkey) {
	if (key[*last] < 0) {
	    goto L10;
	}
	/* Get KVAL from the stack */
	kval = key[*last];
	key[*last] = -9999;
	for (j = *nrow; j >= 2; --j) {
	    irow[j] = kval / kyy[j];
	    kval -= irow[j] * kyy[j];
	}
	irow[1] = kval;
	*ipn = *last;
    } else {
	*last = 0;
	*iflag = 3;
    }
    return;
}

/*
  -----------------------------------------------------------------------
  Name:	      F7XACT
  Purpose:    Generate the new nodes for given marinal totals.
  Usage:      CALL F7XACT (NROW, IMAX, IDIF, K, KS, IFLAG)
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

void
f7xact(Sint *nrow, Sint *imax, Sint *idif, Sint *k, Sint *ks,
       Sint *iflag)
    
{
    Sint i, m, k1, mm;

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
	if (m == *ks) {
	    if (idif[m] == imax[m]) {
		*ks = *k;
	    }
	}
    }
    else {
 Loop:
	/* Check for finish */
	for (k1 = *k + 1; k1 <= *nrow; ++k1) {
	    if (idif[k1] > 0) {
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
	*k = k1;

	do {
	    --(*k);
	    m = min(mm, imax[*k]);
	    idif[*k] = m;
	    mm -= m;
	} while (mm > 0 && *k != 1);

	/* Check that all counts reallocated */
	if (mm > 0) {
	    if (k1 != *nrow) {
		*k = k1;
		goto Loop;
	    }
	    *iflag = 1;
	    return;
	}
	/* Get ks */
	--idif[k1];
	*ks = 0;
	do {
	    ++(*ks);
	    if (*ks > *k) {
		return;
	    }
	} while (idif[*ks] >= imax[*ks]);
    }
}

/*
  -----------------------------------------------------------------------
  Name:	      F8XACT
  Purpose:    Routine for reducing a vector when there is a zero
	      element.
  Usage:      CALL F8XACT (IROW, IS, I1, IZERO, NEW)
  Arguments:
     IROW   - Vector containing the row counts.			(Input)
     IS	    - Indicator.					(Input)
     I1	    - Indicator.					(Input)
     IZERO  - Position of the zero.				(Input)
     NEW    - Vector of new row counts.				(Output)
  -----------------------------------------------------------------------
  */

void
f8xact(Sint *irow, Sint *is, Sint *i1, Sint *izero, Sint *new)
{
    int i;

    /* Parameter adjustments */
    --new;
    --irow;

    /* Function Body */
    for (i = 1; i < *i1; ++i)
	new[i] = irow[i];

    for (i = *i1; i <= *izero - 1; ++i) {
	if (*is >= irow[i + 1])
	    break;
	new[i] = irow[i + 1];
    }

    new[i] = *is;

    for(;;) {
	++i;
	if (i > *izero)
	    return;
	new[i] = irow[i];
    }
}

/*
  -----------------------------------------------------------------------
  Name:	      F9XACT
  Purpose:    Computes the log of a multinomial coefficient.
  Usage:      F9XACT(N, MM, IR, FACT)
  Arguments:
     N	    - Length of IR.					(Input)
     MM	    - Number for factorial in numerator.		(Input)
     IR	    - Vector of length N containing the numbers for
              the denominator of the factorial.			(Input)
     FACT   - Table of log factorials.				(Input)
     F9XACT - The log of the multinomal coefficient.		(Output)
  -----------------------------------------------------------------------
  */

double
f9xact(Sint *n, Sint *mm, Sint *ir, double *fact)
{
    double d;
    int k;

    d = fact[*mm];
    for (k = 0; k < *n; k++)
	d -= fact[ir[k]];
    return d;
}

/*
  -----------------------------------------------------------------------
  Name:	    F10ACT
  Purpose:  Computes the shortest path length for special tables.
  Usage:    F10ACT (NROW, IROW, NCOL, ICOL, VAL, XMIN, FACT, ND, NE, M)
  Arguments:
     NROW   - The number of rows in the table.			(Input)
     IROW   - Vector of length NROW containing the row totals.	(Input)
     NCOL   - The number of columns in the table.  		(Input)
     ICO    - Vector of length NCOL containing the column totals.(Input)
     VAL    - The shortest path.  				(Output)
     XMIN   - Set to true if shortest path obtained.  		(Output)
     FACT   - Vector containing the logarithms of factorials.   (Input)
     ND	    - Workspace vector of length NROW.			(Input)
     NE	    - Workspace vector of length NCOL.			(Input)
     M	    - Workspace vector of length NCOL.			(Input)

  Chapter:    STAT/LIBRARY Categorical and Discrete Data Analysis
  -----------------------------------------------------------------------
  */

void
f10act(Sint *nrow, Sint *irow, Sint *ncol, Sint *icol, double *val,
       Sint *xmin, double *fact, Sint *nd, Sint *ne, Sint *m)
{
    /* Local variables */
    Sint i, is, ix, nrw1;

    /* Parameter adjustments */
    --m;
    --ne;
    --nd;
    --icol;
    --irow;

    /* Function Body */
    for (i = 1; i <= *nrow - 1; ++i)
	nd[i] = 0;

    is = icol[1] / *nrow;
    ix = icol[1] - *nrow * is;
    ne[1] = is;
    m[1] = ix;
    if (ix != 0)
	++nd[ix];

    for (i = 2; i <= *ncol; ++i) {
	ix = icol[i] / *nrow;
	ne[i] = ix;
	is += ix;
	ix = icol[i] - *nrow * ix;
	m[i] = ix;
	if (ix != 0)
	    ++nd[ix];
    }

    for (i = *nrow - 2; i >= 1; --i)
	nd[i] += nd[i + 1];

    ix = 0;
    nrw1 = *nrow + 1;
    for (i = *nrow; i >= 2; --i) {
	ix = ix + is + nd[nrw1 - i] - irow[i];
	if (ix < 0)
	    return;
    }

    for (i = 1; i <= *ncol; ++i) {
	ix = ne[i];
	is = m[i];
	*val = *val + is * fact[ix + 1] + (*nrow - is) * fact[ix];
    }
    *xmin = (1);

    return;
}

/*
  -----------------------------------------------------------------------
  Name:	      F11ACT
  Purpose:    Routine for revising row totals.
  Usage:      CALL F11ACT (IROW, I1, I2, NEW)
  Arguments:
     IROW   - Vector containing the row totals.	(Input)
     I1	    - Indicator.			(Input)
     I2	    - Indicator.  			(Input)
     NEW    - Vector containing the row totals.	(Output)
  -----------------------------------------------------------------------
  */
void
f11act(Sint *irow, Sint *i1, Sint *i2, Sint *new)
{
    Sint i;

    /* Parameter adjustments */
    --new;
    --irow;

    for (i = 1; i <= (*i1 - 1); ++i)	new[i] = irow[i];
    for (i = *i1; i <= *i2; ++i)	new[i] = irow[i + 1];

    return;
}

/*
  -----------------------------------------------------------------------
  Name:	      prterr
  Purpose:    Print an error message and stop.
  Usage:      prterr(icode, mes)
  Arguments:
     icode  - Integer code for the error message.		(Input)
     mes    - Character string containing the error message.	(Input)
  -----------------------------------------------------------------------
  */
void
prterr(int icode, char *mes)
{
    PROBLEM "FEXACT error %d.\n%s", icode, mes RECOVER(NULL_ENTRY);
    return;
}

/*
  -----------------------------------------------------------------------
  Name:	      iwork
  Purpose:    Routine for allocating workspace.
  Usage:      iwork (iwkmax, iwkpt, number, itype)
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
Sint
iwork(Sint iwkmax, Sint *iwkpt, Sint number, Sint itype)
{
    Sint i;

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

void isort(Sint *n, Sint *ix)
{
/*
  -----------------------------------------------------------------------
  Name:	      ISORT
  Purpose:    Shell sort for an Sint vector.
  Usage:      CALL ISORT (N, IX)
  Arguments:
     N	    - Lenth of vector IX.	(Input)
     IX	    - Vector to be sorted.	(in/out)
  -----------------------------------------------------------------------
  */
    static Sint ikey, i, j, m, il[10], kl, it, iu[10], ku;

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

double gammds(double *y, double *p, Sint *ifault)
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
    static Sint ifail;

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
    a += 1.;
    c = c * *y / a;
    g += c;
    if (c / g > 1e-6) {
	goto L10;
    }
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

double alogam(double *x, Sint *ifault)
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
