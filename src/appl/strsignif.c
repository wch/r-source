/*
 *  Copyright (C) Martin Maechler, 1994, 1998
 *  Copyright (C) 2001-2007 the R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 *
 *  I want you to preserve the copyright of the original author(s),
 *  and encourage you to send me any improvements by e-mail. (MM).
 *
 *  Originally from Bill Dunlap
 *  bill@stat.washington.edu
 *  Wed Feb 21, 1990
 *
 *  Much improved by Martin Maechler, including the "fg" format.
 *
 *  Patched by Friedrich.Leisch@ci.tuwien.ac.at
 *  Fri Nov 22, 1996
 *
 *  Some fixes by Ross Ihaka
 *  ihaka@stat.auckland.ac.nz
 *  Sat Dec 21, 1996
 *  Integer arguments changed from "long" to "int"
 *  Bus error due to non-writable strings fixed
 *
 *  BDR 2001-10-30 use R_alloc not Calloc as memory was not
 *  reclaimed on error (and there are many error exits).
 *
 *	type	"double" or "integer" (R - numeric 'mode').
 *
 *	width	The total field width; width < 0 means to left justify
 *		the number in this field (equivalent to flag = "-").
 *		It is possible that the result will be longer than this,
 *		but that should only happen in reasonable cases.
 *
 *	digits	The desired number of digits after the decimal point.
 *		digits < 0 uses the default for C, namely 6 digits.
 *
 *	format	"d" (for integers) or "f", "e","E", "g", "G" (for 'real')
 *		"f" gives numbers in the usual "xxx.xxx" format;
 *		"e" and "E" give n.ddde<nn> or n.dddE<nn> (scientific format);
 *		"g" and "G" puts them into scientific format if it saves
 *		space to do so.
 *	    NEW: "fg" gives numbers in "xxx.xxx" format as "f",
 *		  ~~  however, digits are *significant* digits and no
 *		      trailing zeros are produced, as in "g".
 *
 *	flag	Format modifier as in K&R "C", 2nd ed., p.243;
 *		e.g., "0" pads leading zeros; "-" does left adjustment
 *		the other possible flags are  "+", " ", and "#".
 *	  New (Feb.98): if flag has more than one character, all are passed..
 */

/* <UTF8> char here is either ASCII or handled as a whole */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <stdio.h>
#include <string.h>
#include <math.h>

#include <R_ext/Error.h>	/* error */
#include <R_ext/Memory.h>	/* R_alloc */
#include <R_ext/Applic.h>
#include <Rmath.h>		/* fround */

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) gettext (String)
#else
#define _(String) (String)
#endif

/*
   The declaration for x is unusual for a .C() but is managed by
   casting in the code itself.  However, it does mean that we cannot
   use the argument type matching
 */
void str_signif(char *x, int *n, const char **type, int *width, int *digits,
		const char **format, const char **flag, char **result)
{
    int wid = *width;
    int dig = *digits;
    int i, nn = *n;
    int short do_fg = !strcmp("fg",*format);/* == 1  iff  format == "fg" */
    double xx;
    int iex, j, jL, len_flag = strlen(*flag);

    char *f0  =	 R_alloc(do_fg ? 1+len_flag+3 : 1, sizeof(char));
    char *form = R_alloc(len_flag+4 + strlen(*format), sizeof(char));

    if (wid == 0)
	error(_(".C(..): Width cannot be zero"));

    if (strcmp("d", *format) == 0) {
	if (len_flag == 0)
	    strcpy(form, "%*d");
	else {
	    strcpy(form, "%");
	    strcat(form, *flag);
	    strcat(form, "*d");
	}
	if (strcmp("integer", *type) == 0)
	    for (i=0; i < nn; i++)
		sprintf(result[i], form, wid, ((int *)x)[i]);
	else
	    error(_(".C(..): 'type' must be \"integer\" for  \"d\"-format"));
    }
    else { /* --- floating point --- */
	if (len_flag == 0)
	    strcpy(form, "%*.*");
	else {
	    strcpy(form, "%");
	    strcat(form, *flag);
	    strcat(form, "*.*");
	}

	if(do_fg) {
	    strcpy(f0, "%");
	    strcat(f0, *flag);
	    strcat(f0, ".*f");
	    strcat(form, "g");
	}
	else
	    strcat(form, *format);
#ifdef DEBUG
	fprintf(stderr, "strsignif.c: form='%s', wid=%d, dig=%d\n",
		form, wid, dig);
	if(do_fg) fprintf(stderr, "\t\"fg\": f0='%s'.", f0);
#endif
	if (strcmp("double", *type) == 0) {
	    if(do_fg) /* do smart "f" : */
		for (i=0; i < nn; i++) {
		    xx = ((double *)x)[i];
		    if(xx == 0.)
			strcpy(result[i], "0");
		    else {
			/* This was iex= (int)floor(log10(fabs(xx)))
			   That's wrong, as xx might get rounded up,
			   and we do need some fuzz or 99.5 is correct.
			*/
			double xxx = fabs(xx), X;
			iex= (int)floor(log10(xxx) + 1e-12);
			X = fround(xxx/pow(10.0, (double)iex)+ 1e-12, 
				   (double)(dig-1));
			if(iex > 0 &&  X >= 10) {
			    xx = X * pow(10.0, (double)iex);
			    iex++;
			}
			if(iex == -4 && fabs(xx)< 1e-4) {/* VERY rare case */
			    iex = -5;
			}
			if(iex < -4) {
				/* "g" would result in 'e-' representation:*/
			    sprintf(result[i], f0, dig-1 + -iex, xx);
#ifdef DEBUG
			    fprintf(stderr, " x[%d]=%g, iex%d\n", i, xx, iex);
			    fprintf(stderr, "\tres. = '%s'; ", result[i]);
#endif
				/* Remove trailing  "0"s : */
			    jL = j = strlen(result[i])-1;
			    while(result[i][j] == '0') j--;
			    result[i][j+1] = '\0';
#ifdef DEBUG
			    fprintf(stderr, "\t>>> jL=%d, j=%d; new res= '%s'\n",
				    jL, j, result[i]);
#endif
			} else { /* iex >= -4:	NOT "e-" */
				/* if iex >= dig, would have "e+" representation */
#ifdef DEBUG
			    fprintf(stderr, "\t  iex >= -4; using %d for 'dig'\n",
				    (iex >= dig) ? (iex+1) : dig);
#endif
			    sprintf(result[i], form, wid,
				    (iex >= dig) ? (iex+1) : dig, xx);
			}
		    } /* xx != 0 */
		} /* if(do_fg) for(i..) */
	    else
		for (i=0; i < nn; i++) {
		    sprintf(result[i], form, wid, dig, ((double *)x)[i]);
		}
	} else
	    error(_(".C(..): 'type' must be \"real\" for this format"));
    }
}
