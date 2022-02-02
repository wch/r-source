/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 2000, 2003  The R Core Team
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

/* A version of Marsaglia-MultiCarry */

static unsigned int I1=1234, I2=5678;

void set_seed(unsigned int i1, unsigned int i2)
{
    I1 = i1; I2 = i2;
}

void get_seed(unsigned int *i1, unsigned int *i2)
{
    *i1 = I1; *i2 = I2;
}


double unif_rand(void)
{
    I1= 36969*(I1 & 0177777) + (I1>>16);
    I2= 18000*(I2 & 0177777) + (I2>>16);
    return ((I1 << 16)^(I2 & 0177777)) * 2.328306437080797e-10; /* in [0,1) */
}

#include <math.h>
#include <stdint.h>
//copied from src/main/RNG.c:
//generate a random non-negative integer < 2 ^ bits in 16 bit chunks
static double rbits(int bits)
{
    int_least64_t v = 0;
    for (int n = 0; n <= bits; n += 16) {
	int v1 = (int) floor(unif_rand() * 65536);
	v = 65536 * v + v1;
    }
    // mask out the bits in the result that are not needed
    return (double) (v & ((1L << bits) - 1));
}

double R_unif_index(double dn)
{
    // rejection sampling from integers below the next larger power of two
    if (dn <= 0)
	return 0.0;
    int bits = (int) ceil(log2(dn));
    double dv;
    do { dv = rbits(bits); } while (dn <= dv);
    return dv;
}
