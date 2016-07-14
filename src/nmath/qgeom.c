/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 	     Ross Ihaka
 *  Copyright (C) 2000--2016 The R Core Team
 *  Copyright (C) 2004--2016 The R Foundation
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
 *  DESCRIPTION
 *
 *    The quantile function of the geometric distribution.
 */

#include "nmath.h"
#include "dpq.h"

double qgeom(double p, double prob, int lower_tail, int log_p)
{
#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(prob))
	return p + prob;
#endif
    if (prob <= 0 || prob > 1) ML_ERR_return_NAN;

    R_Q_P01_check(p);
    if (prob == 1) return(0);
    R_Q_P01_boundaries(p, 0, ML_POSINF);

/* add a fuzz to ensure left continuity, but value must be >= 0 */
    return fmax2(0, ceil(R_DT_Clog(p) / log1p(- prob) - 1 - 1e-12));
}
