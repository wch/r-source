/* <UTF8> char here is either ASCII or handled as a whole */

/* Graphical parameters which are treated identically by
 * par( <nam> = <value> )  and	highlevel  plotfun (..., <nam> = <value> ).
 *
 * This is #included both from Specify() and Specify2() into ./par.c
*/
    if (streql(what, "adj")) {
	lengthCheck(what, value, 1, call);	x = asReal(value);
	BoundsCheck(x, 0.0, 1.0, what);
	R_DEV__(adj) = x;
    }
    else if (streql(what, "ann")) {
	lengthCheck(what, value, 1, call);	ix = asLogical(value);
	R_DEV__(ann) = (ix != 0);/* NA |-> TRUE */
    }
    else if (streql(what, "bty")) {
	lengthCheck(what, value, 1, call);
	if (!isString(value))
	    par_error(what);
	ix = CHAR(STRING_ELT(value, 0))[0];
	switch (ix) {
	case 'o': case 'O':
	case 'l': case 'L':
	case '7':
	case 'c': case 'C': case '[':
	case ']':
	case 'u': case 'U':
	case 'n':
	    R_DEV__(bty) = ix;
	    break;
	default:
	    par_error(what);
	}
    }

    else if (streql(what, "cex.main")) {
	lengthCheck(what, value, 1, call);	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(cexmain) = x;
    }
    else if (streql(what, "cex.lab")) {
	lengthCheck(what, value, 1, call);	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(cexlab) = x;
    }
    else if (streql(what, "cex.sub")) {
	lengthCheck(what, value, 1, call);	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(cexsub) = x;
    }
    else if (streql(what, "cex.axis")) {
	lengthCheck(what, value, 1, call);	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(cexaxis) = x;
    }
    else if (streql(what, "col")) {
	/* col can be a vector of length > 1, so pick off first value
	   (as e.g. pch always did) */
	if (!isVector(value) || LENGTH(value) < 1) par_error(what);
	R_DEV__(col) = RGBpar(value, 0);
    }
    else if (streql(what, "col.main")) {
	lengthCheck(what, value, 1, call);	ix = RGBpar(value, 0);
	/*	naIntCheck(ix, what); */
	R_DEV__(colmain) = ix;
	R_DEV__(col) = RGBpar(value, 0);
    }
    else if (streql(what, "col.lab")) {
	lengthCheck(what, value, 1, call);
	R_DEV__(collab) = RGBpar(value, 0);
    }
    else if (streql(what, "col.sub")) {
	lengthCheck(what, value, 1, call);
	R_DEV__(colsub) = RGBpar(value, 0);
    }
    else if (streql(what, "col.axis")) {
	lengthCheck(what, value, 1, call);
	R_DEV__(colaxis) = RGBpar(value, 0);
    }
    else if (streql(what, "crt")) {
	lengthCheck(what, value, 1, call);	x = asReal(value);
	naRealCheck(x, what);
	R_DEV__(crt) = x;
    }
    else if (streql(what, "err")) {
	lengthCheck(what, value, 1, call);	ix = asInteger(value);
	if (ix == 0 || ix == -1)
	    R_DEV__(err) = ix;
	else par_error(what);
    }

    else if (streql(what, "font")) {
	lengthCheck(what, value, 1, call);	ix = asInteger(value);
	posIntCheck(ix, what);
	R_DEV__(font) = ix;
    }
    else if (streql(what, "font.main")) {
	lengthCheck(what, value, 1, call);	ix = asInteger(value);
	posIntCheck(ix, what);
	R_DEV__(fontmain) = ix;
    }
    else if (streql(what, "font.lab")) {
	lengthCheck(what, value, 1, call);	ix = asInteger(value);
	posIntCheck(ix, what);
	R_DEV__(fontlab) = ix;
    }
    else if (streql(what, "font.sub")) {
	lengthCheck(what, value, 1, call);	ix = asInteger(value);
	posIntCheck(ix, what);
	R_DEV__(fontsub) = ix;
    }
    else if (streql(what, "font.axis")) {
	lengthCheck(what, value, 1, call);	ix = asInteger(value);
	posIntCheck(ix, what);
	R_DEV__(fontaxis) = ix;
    }
    else if(streql(what, "gamma")) {
	lengthCheck(what, value, 1, call);	x = asReal(value);
	posRealCheck(x, what);
	if (((GEDevDesc*) dd)->dev->canChangeGamma)
	    R_DEV__(gamma) = x;
	else
	    warningcall(call, _("'gamma' cannot be modified on this device"));
    }
    else if (streql(what, "lab")) {
	value = coerceVector(value, INTSXP);
	lengthCheck(what, value, 3, call);
	posIntCheck   (INTEGER(value)[0], what);
	posIntCheck   (INTEGER(value)[1], what);
	nonnegIntCheck(INTEGER(value)[2], what);
	R_DEV__(lab[0]) = INTEGER(value)[0];
	R_DEV__(lab[1]) = INTEGER(value)[1];
	R_DEV__(lab[2]) = INTEGER(value)[2];
    }
    else if (streql(what, "las")) {
	lengthCheck(what, value, 1, call);	ix = asInteger(value);
	if (0 <= ix && ix <= 3)
	    R_DEV__(las) = ix;
	else par_error(what);
    }
    else if (streql(what, "lend")) {
	lengthCheck(what, value, 1, call);
	R_DEV__(lend) = LENDpar(value, 0);
    }
    else if (streql(what, "ljoin")) {
	lengthCheck(what, value, 1, call);
	R_DEV__(ljoin) = LJOINpar(value, 0);
    }
    else if (streql(what, "lmitre")) {
	lengthCheck(what, value, 1, call);
	x = asReal(value);
	posRealCheck(x, what);
	if (x < 1)
	    par_error(what);
	R_DEV__(lmitre) = x;
    }
    else if (streql(what, "lty")) {
	/* lty can be a vector of length > 1, so pick off first value
	   (as e.g. pch always did) */
	if (!isVector(value) || LENGTH(value) < 1)
	    par_error(what);
	R_DEV__(lty) = LTYpar(value, 0);
    }
    else if (streql(what, "lwd")) {
	/* lwd can be a vector of length > 1, so pick off first value
	   (as e.g. pch always did) */
	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(lwd) = x;
    }
    else if (streql(what, "mgp")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 3, call);
	/* Since 1.6.x: Allow negative (S-compatibly): */
	naRealCheck(REAL(value)[0], what);
	naRealCheck(REAL(value)[1], what);
	naRealCheck(REAL(value)[2], what);
	if(REAL(value)[0] * REAL(value)[1] < 0 ||
	   REAL(value)[0] * REAL(value)[2] < 0)
	    warningcall(call, "`mgp[1:3]' are of differing sign");
	R_DEV__(mgp[0]) = REAL(value)[0];
	R_DEV__(mgp[1]) = REAL(value)[1];
	R_DEV__(mgp[2]) = REAL(value)[2];
    }
    else if (streql(what, "mkh")) {
	lengthCheck(what, value, 1, call);	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(mkh) = x;
    }
    else if (streql(what, "pch")) {
	if (!isVector(value) || LENGTH(value) < 1)
	    par_error(what);
	if (isString(value)) {
	    ix = CHAR(STRING_ELT(value, 0))[0];
	}
	else if (isNumeric(value)) {
	    ix = asInteger(value);
	    nonnegIntCheck(ix, what);
	}
	else par_error(what);
	R_DEV__(pch) = ix;
    }
    else if (streql(what, "smo")) {
	lengthCheck(what, value, 1, call);	x = asReal(value);
	nonnegRealCheck(x, what);
	R_DEV__(smo) = x;
    }
    else if (streql(what, "srt")) {
	lengthCheck(what, value, 1, call);	x = asReal(value);
	naRealCheck(x, what);
	R_DEV__(srt) = x;
    }

    /* NOTE: tck and tcl must be treated in parallel; if one is NA,
     *	the other must be non-NA.  If tcl is NA, then setting tck to NA
     *	will reset tck to its initial default value.  See also graphics.c. */
    else if (streql(what, "tck")) {
	lengthCheck(what, value, 1, call);	x = asReal(value);
	R_DEV__(tck) = x;
	if (R_FINITE(x))
	    R_DEV__(tcl) = NA_REAL;
	else if(!R_FINITE(Rf_dpptr(dd)->tcl))
	    R_DEV__(tcl) = -0.5;
    }
    else if (streql(what, "tcl")) {
	lengthCheck(what, value, 1, call);	x = asReal(value);
	R_DEV__(tcl) = x;
	if (R_FINITE(x))
	    R_DEV__(tck) = NA_REAL;
	else if (!R_FINITE(Rf_dpptr(dd)->tck))
	    R_DEV__(tck) = -0.01; /* S Default -- was 0.02 till R 1.5.x */
    }
    else if (streql(what, "xaxp")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 3, call);
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
	ix = CHAR(STRING_ELT(value, 0))[0];
	if (ix == 's' || ix == 'e' || ix == 'i' || ix == 'r' || ix == 'd')
	    R_DEV__(xaxs) = ix;
	else par_error(what);
    }
    else if (streql(what, "xaxt")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING_ELT(value, 0))[0];
	if (ix == 's' || ix == 'l' || ix == 't' || ix == 'n')
	    R_DEV__(xaxt) = ix;
	else par_error(what);
    }
    else if (streql(what, "xpd")) {
	lengthCheck(what, value, 1, call);
	ix = asInteger(value);
	if (ix == NA_INTEGER)
	    R_DEV__(xpd) = 2;
	else
	    R_DEV__(xpd) = (ix != 0);
    }
    else if (streql(what, "yaxp")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 3, call);
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
	ix = CHAR(STRING_ELT(value, 0))[0];
	if (ix == 's' || ix == 'e' || ix == 'i' || ix == 'r' || ix == 'd')
	    R_DEV__(yaxs) = ix;
	else par_error(what);
    }
    else if (streql(what, "yaxt")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING_ELT(value, 0))[0];
	if (ix == 's' || ix == 'l' || ix == 't' || ix == 'n')
	    R_DEV__(yaxt) = ix;
	else par_error(what);
    }

