/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2021  The R Core Team
 *  Copyright (C) 2002--2011  The R Foundation
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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


 *  This is an extensive reworking by Paul Murrell of an original
 *  quick hack by Ross Ihaka designed to give a superset of the
 *  functionality in the AT&T Bell Laboratories GRZ library.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <float.h> /* for DBL_EPSILON etc */
#include <Graphics.h>
// --> R_ext/GraphicsEngine.h + Rgraphics.h
#include <Rmath.h>		/* eg. fmax2() */

static void GLPretty(double *ul, double *uh, int *n);

/* used in GScale() (../library/graphics/src/graphics.c), but also in
                     ../library/grDevices/src/axis_scales.c : */
// (usr = (min,max), n_inp, log) |--> (axp = (min, max), n_out) :
void GAxisPars(double *min, double *max, int *n, Rboolean log,
	       int axis) // <- needed for warning() only
{
#define EPS_FAC_2 16
    //            -- was 100 (till R 4.1.0); 16 == EPS_FAC in ../library/graphics/src/graphics.c
    Rboolean swap = *min > *max;
    /* Feature: in R, something like  xlim = c(100,0)  just works */
#define MAYBE_SWAP(_U,_V) do		\
    if(swap) {				\
	double t = _U; _U = _V; _V = t;	\
    } while(0)

    MAYBE_SWAP(*min, *max);
    /* save only for the extreme case (EPS_FAC_2): */
    double min_o = *min, max_o = *max;

#ifdef DEBUG_axis
    REprintf("GAxisPars(%s): maybe_swap => (min=%g, max=%g); ",
	     log ? "log=TRUE" : "", min_o, max_o);
#endif
    if(log) {
	/* Avoid infinities */
	if(*max >  308) { *max =  308; if(*min > *max) *min = *max; }
	if(*min < -307) { *min = -307; if(*max < *min) *max = *min; }
	*min = Rexp10(*min);
	*max = Rexp10(*max);
#ifdef DEBUG_axis
	REprintf("before GLPretty(min=%g, max=%g, n=%d):\n", *min, *max, *n);
#endif
	GLPretty(min, max, n);
    }
    else {
#ifdef DEBUG_axis
	REprintf("before GEPretty(..):\n");
#endif
	GEPretty(min, max, n);
    }

#ifdef DEBUG_axis
    REprintf(" [GAP]: then (min=%g, max=%g, n=%d)\n", *min, *max, *n);
#endif

    double t_ = fmax2(fabs(*max), fabs(*min)),
	tf = // careful to avoid overflow (and underflow) here:
	    (t_ > 1)
	    ? (t_ * DBL_EPSILON) * EPS_FAC_2
	    : (t_ * EPS_FAC_2  ) * DBL_EPSILON;
	if(tf == 0) tf = DBL_MIN;

    if(fabs(*max - *min) <= tf) {
	/* Treat this case somewhat similar to the (min ~= max) case above */
	/* Too much accuracy here just shows machine differences */
	if(axis) // no warning with (axis = 0)
	warning(_("axis(%d, *): range of values (%5.2g) is small wrt |M| = %7.2g --> not pretty()")
		/*"to compute accurately"*/,
		axis, fabs(*max - *min), t_);

	/* No pretty()ing anymore */
	*min = min_o;
	*max = max_o;
	double eps = .005 * (*max - *min);/* .005: not to go to DBL_MIN/MAX */
	*min += eps;
	*max -= eps;
	if(log) {
	    *min = Rexp10(*min);
	    *max = Rexp10(*max);
	}
	*n = 1;
#ifdef DEBUG_axis
	REprintf(" small range() --> axp[1:3]=(min,max, n=1) = (%g, %g, 1)\n", *min, *max);
#endif
    }
    MAYBE_SWAP(*min, *max);
}

#define LPR_SMALL  2
#define LPR_MEDIUM 3

static void GLPretty(double *ul, double *uh, int *n)
{
/* Generate pretty tick values --	LOGARITHMIC scale
 * __ ul < uh __
 * This only does a very simple setup.
 * The real work happens when the axis is drawn. */
    double dl = *ul, dh = *uh;
    int p1 = (int) ceil (log10(dl)),
	p2 = (int) floor(log10(dh));
    if(p2 <= p1 &&  dh/dl > 10.0) {
	p1 = (int) ceil (log10(dl) - 0.5);
	p2 = (int) floor(log10(dh) + 0.5);
    }

    if (p2 <= p1) { /* floor(log10(uh)) <= ceil(log10(ul))
			 * <==>	 log10(uh) - log10(ul) < 2
			 * <==>		uh / ul	       < 100 */
	/* Very small range : Use tickmarks from a LINEAR scale
	 *		      Splus uses n = 9 here, but that is dumb */
	GPretty(ul, uh, n);
	*n = -*n;
    }
    else { /* extra tickmarks --> CreateAtVector() in ./plot.c */
	/* round to nice "1e<N>" */
	*ul = Rexp10((double)p1);
	*uh = Rexp10((double)p2);
	// have p2-p1 >= 1
	if (p2 - p1 <= LPR_SMALL)
	    *n = 3; /* Small range :	Use 1,2,5,10 times 10^k tickmarks */
	else if (p2 - p1 <= LPR_MEDIUM)
	    *n = 2; /* Medium range :	Use 1,5 times 10^k tickmarks */
	else
	    *n = 1; /* Large range :	Use 10^k tickmarks
		     *			But decimate, when there are too many*/
    }
}

void GPretty(double *lo, double *up, int *ndiv)
{
    GEPretty(lo, up, ndiv); // --> in ./engine.c , --> calling R_pretty()
}
