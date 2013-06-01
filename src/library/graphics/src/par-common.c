/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2012  The R Core Team
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
 *  http://www.r-project.org/Licenses/
 */

/* Graphical parameters which are treated identically by
 * par( <nam> = <value> )  and	highlevel  plotfun (..., <nam> = <value> ).
 *
 * This is #included both from Specify() and Specify2() into ./par.c
*/
    if (streql(what, "adj")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	BoundsCheck(x, 0.0, 1.0, what);
	R_DEV__(adj) = x;
    }
    else if (streql(what, "ann")) {
	lengthCheck(what, value, 1);	ix = asLogical(value);
	R_DEV__(ann) = (ix != 0);/* NA |-> TRUE */
    }
    else if (streql(what, "bg")) {
	/* in par() this means the plot region, inline it means filled points */
#ifdef FOR_PAR
	lengthCheck(what, value, 1);
#else
	if (!isVector(value) || LENGTH(value) < 1) par_error(what);
#endif
	R_DEV__(bg) = RGBpar3(value, 0, dpptr(dd)->bg);
#ifdef FOR_PAR
	R_DEV__(new) = FALSE;
#endif
    }
    else if (streql(what, "bty")) {
	lengthCheck(what, value, 1);
	if (!isString(value))
	    par_error(what);
	cx = CHAR(STRING_ELT(value, 0))[0];
	switch (cx) {
	case 'o': case 'O':
	case 'l': case 'L':
	case '7':
	case 'c': case 'C': case '[':
	case ']':
	case 'u': case 'U':
	case 'n':
	    R_DEV__(bty) = cx;
	    break;
	default:
	    par_error(what);
	}
    }

    else if (streql(what, "cex")) {
#ifdef FOR_PAR
	lengthCheck(what, value, 1);
/* else: cex can be a vector of length > 1, so pick off first value
   (as e.g. pch always did) */
#endif
	x = asReal(value);
	posRealCheck(x, what);
#ifdef FOR_PAR
	R_DEV__(cex) = 1.0;
	R_DEV__(cexbase) = x;
#else
	R_DEV__(cex) = x; // not setting cexbase here
#endif
    }
    else if (streql(what, "cex.main")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(cexmain) = x;
    }
    else if (streql(what, "cex.lab")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(cexlab) = x;
    }
    else if (streql(what, "cex.sub")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(cexsub) = x;
    }
    else if (streql(what, "cex.axis")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(cexaxis) = x;
    }
    else if (streql(what, "col")) {
#ifdef FOR_PAR
	lengthCheck(what, value, 1);
#else
	if (!isVector(value) || LENGTH(value) < 1) par_error(what);
#endif
	R_DEV__(col) = RGBpar3(value, 0, dpptr(dd)->bg);
    }
    else if (streql(what, "col.main")) {
	lengthCheck(what, value, 1);
	R_DEV__(colmain) = RGBpar3(value, 0, dpptr(dd)->bg);
    }
    else if (streql(what, "col.lab")) {
	lengthCheck(what, value, 1);
	R_DEV__(collab) = RGBpar3(value, 0, dpptr(dd)->bg);
    }
    else if (streql(what, "col.sub")) {
	lengthCheck(what, value, 1);
	R_DEV__(colsub) = RGBpar3(value, 0, dpptr(dd)->bg);
    }
    else if (streql(what, "col.axis")) {
	lengthCheck(what, value, 1);
	R_DEV__(colaxis) = RGBpar3(value, 0, dpptr(dd)->bg);
    }
    else if (streql(what, "crt")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	naRealCheck(x, what);
	R_DEV__(crt) = x;
    }
    else if (streql(what, "err")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	if (ix == 0 || ix == -1)
	    R_DEV__(err) = ix;
	else par_error(what);
    }
     else if (streql(what, "family")) {
	const char *ss;
	value = coerceVector(value, STRSXP);
	lengthCheck(what, value, 1);
	ss = translateChar(STRING_ELT(value, 0));
	if(strlen(ss) > 200)
	    error(_("graphical parameter 'family' has a maximum length of 200 bytes"));
#ifdef FOR_PAR
	strncpy(dpptr(dd)->family, ss, 201);
#endif
	strncpy(gpptr(dd)->family, ss, 201);
    }
    else if (streql(what, "fg")) {
	lengthCheck(what, value, 1);
	ix = RGBpar3(value, 0, dpptr(dd)->bg);
#ifdef FOR_PAR
	/* par(fg=) sets BOTH "fg" and "col" */
	R_DEV__(col) = ix;
#endif
	R_DEV__(fg) = ix;
     }
     else if (streql(what, "font")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	posIntCheck(ix, what);
	R_DEV__(font) = ix;
    }
    else if (streql(what, "font.main")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	posIntCheck(ix, what);
	R_DEV__(fontmain) = ix;
    }
    else if (streql(what, "font.lab")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	posIntCheck(ix, what);
	R_DEV__(fontlab) = ix;
    }
    else if (streql(what, "font.sub")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	posIntCheck(ix, what);
	R_DEV__(fontsub) = ix;
    }
    else if (streql(what, "font.axis")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	posIntCheck(ix, what);
	R_DEV__(fontaxis) = ix;
    }
    else if (streql(what, "lab")) {
	value = coerceVector(value, INTSXP);
	lengthCheck(what, value, 3);
	posIntCheck   (INTEGER(value)[0], what);
	posIntCheck   (INTEGER(value)[1], what);
	nonnegIntCheck(INTEGER(value)[2], what);
	R_DEV__(lab[0]) = INTEGER(value)[0];
	R_DEV__(lab[1]) = INTEGER(value)[1];
	R_DEV__(lab[2]) = INTEGER(value)[2];
    }
    else if (streql(what, "las")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	if (0 <= ix && ix <= 3)
	    R_DEV__(las) = ix;
	else par_error(what);
    }
    else if (streql(what, "lend")) {
	lengthCheck(what, value, 1);
	R_DEV__(lend) = GE_LENDpar(value, 0);
    }
    else if (streql(what, "ljoin")) {
	lengthCheck(what, value, 1);
	R_DEV__(ljoin) = GE_LJOINpar(value, 0);
    }
    else if (streql(what, "lmitre")) {
	lengthCheck(what, value, 1);
	x = asReal(value);
	posRealCheck(x, what);
	if (x < 1)
	    par_error(what);
	R_DEV__(lmitre) = x;
    }
    else if (streql(what, "lty")) {
#ifdef FOR_PAR
	lengthCheck(what, value, 1);
#else
	if (!isVector(value) || LENGTH(value) < 1) par_error(what);
#endif
	R_DEV__(lty) = GE_LTYpar(value, 0);
    }
    else if (streql(what, "lwd")) {
#ifdef FOR_PAR
	lengthCheck(what, value, 1);
#else
	if (!isVector(value) || LENGTH(value) < 1) par_error(what);
#endif
	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(lwd) = x;
    }
    else if (streql(what, "mgp")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 3);
	/* Since 1.6.x: Allow negative (S-compatibly): */
	naRealCheck(REAL(value)[0], what);
	naRealCheck(REAL(value)[1], what);
	naRealCheck(REAL(value)[2], what);
	if(REAL(value)[0] * REAL(value)[1] < 0 ||
	   REAL(value)[0] * REAL(value)[2] < 0)
	    warning("`mgp[1:3]' are of differing sign");
	R_DEV__(mgp[0]) = REAL(value)[0];
	R_DEV__(mgp[1]) = REAL(value)[1];
	R_DEV__(mgp[2]) = REAL(value)[2];
    }
    else if (streql(what, "mkh")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(mkh) = x;
    }
    else if (streql(what, "pch")) {
#ifdef FOR_PAR
	lengthCheck(what, value, 1);
#else
	if (!isVector(value) || LENGTH(value) < 1) par_error(what);
#endif
	if (isString(value)) {
	    ix = GEstring_to_pch(STRING_ELT(value, 0));
	} else if (isNumeric(value)) {
	    ix = asInteger(value);
	} else par_error(what);
	if(ix == NA_INTEGER) par_error(what);
	R_DEV__(pch) = ix;
    }
    else if (streql(what, "smo")) {
	/* FIXME: not real */
	lengthCheck(what, value, 1);	x = asReal(value);
	nonnegRealCheck(x, what);
	R_DEV__(smo) = (int) x;
    }
    else if (streql(what, "srt")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	naRealCheck(x, what);
	R_DEV__(srt) = x;
    }

    /* NOTE: tck and tcl must be treated in parallel; if one is NA,
     *	the other must be non-NA.  If tcl is NA, then setting tck to NA
     *	will reset tck to its initial default value.  See also graphics.c. */
    else if (streql(what, "tck")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	R_DEV__(tck) = x;
	if (R_FINITE(x))
	    R_DEV__(tcl) = NA_REAL;
	else if(!R_FINITE(dpptr(dd)->tcl))
	    R_DEV__(tcl) = -0.5;
    }
    else if (streql(what, "tcl")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	R_DEV__(tcl) = x;
	if (R_FINITE(x))
	    R_DEV__(tck) = NA_REAL;
	else if (!R_FINITE(dpptr(dd)->tck))
	    R_DEV__(tck) = -0.01; /* S Default -- was 0.02 till R 1.5.x */
    }
    else if (streql(what, "xaxp")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 3);
	naRealCheck(REAL(value)[0], what);
	naRealCheck(REAL(value)[1], what);
	if ((R_DEV__(xlog)))
	    logAxpCheck((int) (REAL(value)[2]), what);
	else
	    posIntCheck((int) (REAL(value)[2]), what);
	R_DEV__(xaxp[0]) = REAL(value)[0];
	R_DEV__(xaxp[1]) = REAL(value)[1];
	R_DEV__(xaxp[2]) = (int)(REAL(value)[2]);
    }
    else if (streql(what, "xaxs")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	cx = CHAR(STRING_ELT(value, 0))[0];
	if (cx == 's' || cx == 'e' || cx == 'i' || cx == 'r' || cx == 'd')
	    R_DEV__(xaxs) = cx;
	else par_error(what);
    }
    else if (streql(what, "xaxt")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	cx = CHAR(STRING_ELT(value, 0))[0];
	if (cx == 's' || cx == 'l' || cx == 't' || cx == 'n')
	    R_DEV__(xaxt) = cx;
	else par_error(what);
    }
    else if (streql(what, "xpd")) {
	lengthCheck(what, value, 1);
	ix = asInteger(value);
	if (ix == NA_INTEGER)
	    R_DEV__(xpd) = 2;
	else
	    R_DEV__(xpd) = (ix != 0);
    }
    else if (streql(what, "yaxp")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 3);
	naRealCheck(REAL(value)[0], what);
	naRealCheck(REAL(value)[1], what);
	if ((R_DEV__(ylog)))
	    logAxpCheck((int) (REAL(value)[2]), what);
	else
	    posIntCheck((int) (REAL(value)[2]), what);
	R_DEV__(yaxp[0]) = REAL(value)[0];
	R_DEV__(yaxp[1]) = REAL(value)[1];
	R_DEV__(yaxp[2]) = (int) (REAL(value)[2]);
    }
    else if (streql(what, "yaxs")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	cx = CHAR(STRING_ELT(value, 0))[0];
	if (cx == 's' || cx == 'e' || cx == 'i' || cx == 'r' || cx == 'd')
	    R_DEV__(yaxs) = cx;
	else par_error(what);
    }
    else if (streql(what, "yaxt")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	cx = CHAR(STRING_ELT(value, 0))[0];
	if (cx == 's' || cx == 'l' || cx == 't' || cx == 'n')
	    R_DEV__(yaxt) = cx;
	else par_error(what);
    }
