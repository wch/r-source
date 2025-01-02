/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 2000--2025  The R Core Team
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

#define MATHLIB_STANDALONE 1
#include <Rmath.h>

#include <stdio.h>
typedef enum {
    BUGGY_KINDERMAN_RAMAGE,
    AHRENS_DIETER,
    BOX_MULLER,
    USER_NORM,
    INVERSION,
    KINDERMAN_RAMAGE
} N01type;

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
    N01_kind = AHRENS_DIETER;
    set_seed(123, 456); printf("one normal %f\n", norm_rand());
    N01_kind = BOX_MULLER;
    set_seed(123, 456); printf("normal via Box_M %f\n", norm_rand());
    N01_kind = INVERSION;
    set_seed(123, 456); printf("normal via Inv.  %f\n", norm_rand());

    return 0;
}
