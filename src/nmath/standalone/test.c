/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 2000--2026  The R Core Team
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
 *
 */

// Checking rbinom() --  wrt RNGkind(binom.kind = *)
#include <R_ext/Random.h>

#define MATHLIB_STANDALONE 1
#include <Rmath.h>

#include <stdio.h>

// defined in ../rbinom.c , and used in its (..STANDALONE..) R_binom_kind()
extern Binomtype ML_Binom_kind;

int
main(int argc, char** argv)
{
/* something to force the library to be included */
    printf("*** loaded '%s'\n", argv[0]);
    double
	dn = dnorm(0.7, 0., 1., 0),
	qn = qnorm(0.7, 0., 1., 0, 0);
    printf("dnorm(0.7) = %.12g\n", dn);
    printf("qnorm(0.7) = %.12g\n", qn);
    printf("pnorm(qnorm(..)) = %.12g\n", pnorm(qn, 0., 1., 0, 0));
    // several  RNGkind(normal.kind = *) :
    N01_kind = AHRENS_DIETER; set_seed(12, 34); printf("one normal       %9.6f\n", norm_rand());
    N01_kind = BOX_MULLER;    set_seed(12, 34); printf("normal via Box_M %9.6f\n", norm_rand());
    N01_kind = INVERSION;     set_seed(12, 34); printf("normal via Inv.  %9.6f\n", norm_rand());
    //  RNGkind(binom.kind = *) -- rbinom()
    set_seed(428, 1234);
#define MAX_N 7
    int N = MAX_N, i, max_i = 20000;
    for(i = 1; i <= max_i; i++) {
	unsigned int si_1, si_2;
	get_seed(&si_1, &si_2); ML_Binom_kind = BTPE;       double B1 = rbinom(320., 0.25);
	set_seed( si_1,  si_2); ML_Binom_kind = BUGGY_BTPE; double B2 = rbinom(320., 0.25);
	if(B1 != B2) {
	    printf("rbinom(320, 0.25) difference at i=%5d, w/ seeds (%u, %u)\n", i, si_1, si_2);
	    printf(" B{ BTPE } =%4.0f,\n B{Buggy..}=%4.0f\n", B1, B2);
	    N--; if(!N) break;
	}
    }
    if(N == MAX_N) printf("*** Did _not_ find one difference in %d rbinom() calls!\n", max_i);
    return 0;
}
