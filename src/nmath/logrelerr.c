/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  SYNOPSIS
 *
 *	#include "Mathlib.h"
 *	double logrelerr(double x);
 *
 *  DESCRIPTION
 *
 *    Compute the relative error logarithm.
 *
 *			log(1 + x)
 *
 *  NOTES
 *
 *	This code is a translation of the Fortran subroutine `dlnrel'
 *	written by W. Fullerton of Los Alamos Scientific Laboratory.
 */

#include "Mathlib.h"

double logrelerr(double x)
{
    /* series for alnr on the interval -3.75000e-01 to	3.75000e-01 */
    /*				     with weighted error   6.35e-32 */
    /*				      log weighted error  31.20	    */
    /*			    significant figures required  30.93	    */
    /*				 decimal places required  32.01	    */
    static double alnrcs[43] = {
	+.10378693562743769800686267719098e+1,
	-.13364301504908918098766041553133e+0,
	+.19408249135520563357926199374750e-1,
	-.30107551127535777690376537776592e-2,
	+.48694614797154850090456366509137e-3,
	-.81054881893175356066809943008622e-4,
	+.13778847799559524782938251496059e-4,
	-.23802210894358970251369992914935e-5,
	+.41640416213865183476391859901989e-6,
	-.73595828378075994984266837031998e-7,
	+.13117611876241674949152294345011e-7,
	-.23546709317742425136696092330175e-8,
	+.42522773276034997775638052962567e-9,
	-.77190894134840796826108107493300e-10,
	+.14075746481359069909215356472191e-10,
	-.25769072058024680627537078627584e-11,
	+.47342406666294421849154395005938e-12,
	-.87249012674742641745301263292675e-13,
	+.16124614902740551465739833119115e-13,
	-.29875652015665773006710792416815e-14,
	+.55480701209082887983041321697279e-15,
	-.10324619158271569595141333961932e-15,
	+.19250239203049851177878503244868e-16,
	-.35955073465265150011189707844266e-17,
	+.67264542537876857892194574226773e-18,
	-.12602624168735219252082425637546e-18,
	+.23644884408606210044916158955519e-19,
	-.44419377050807936898878389179733e-20,
	+.83546594464034259016241293994666e-21,
	-.15731559416479562574899253521066e-21,
	+.29653128740247422686154369706666e-22,
	-.55949583481815947292156013226666e-23,
	+.10566354268835681048187284138666e-23,
	-.19972483680670204548314999466666e-24,
	+.37782977818839361421049855999999e-25,
	-.71531586889081740345038165333333e-26,
	+.13552488463674213646502024533333e-26,
	-.25694673048487567430079829333333e-27,
	+.48747756066216949076459519999999e-28,
	-.92542112530849715321132373333333e-29,
	+.17578597841760239233269760000000e-29,
	-.33410026677731010351377066666666e-30,
	+.63533936180236187354180266666666e-31,
    };
    static int nlnrel = 0;
    static double xmin = 0.;

    if (nlnrel == 0) {
	nlnrel = chebyshev_init(alnrcs, 43, 0.1 * d1mach(3));
	xmin = -1.0 + sqrt(d1mach(4));
    }

    if (x <= -1) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }

    if (x < xmin) {
	/* answer less than half precision because x too near -1 */
	ML_ERROR(ME_PRECISION);
    }

    if (fabs(x) <= .375)
	return x * (1 - x * chebyshev_eval(x / .375, alnrcs, nlnrel));
    else
	return log(x + 1);
}
