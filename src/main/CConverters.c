/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2007  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

/* <UTF8> char here is handled as a whole string */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <R_ext/RConverters.h>

/* head of the linked list of converters. Typically, NULL.*/
static R_toCConverter   *StoCConverters = NULL;

/* Convert an R object to a non-moveable C/Fortran object and return
   a pointer to it.  This leaves pointers for anything other
   than vectors and lists unaltered.
   This is is accessed in do_dotCode() in dotcode.c. It is not for public use.
*/
attribute_hidden void *
Rf_convertToC(SEXP s, R_CConvertInfo *info, int *success,
              R_toCConverter **converter)
{
    void *ans;
    R_toCConverter *tmp = StoCConverters;

    while(tmp) {
	if(tmp->active) {
	    if(tmp->matcher(s, info, tmp)) {
		*success = 1;
		ans =tmp->converter(s, info, tmp);
		if(converter)
		    *converter = tmp;
		return(ans);
	    }
	}
	tmp = tmp->next;
    }

    *success = 0;
    return((void*) NULL);
}


/**************** R-to-C converter management routines ******************/

/*
  Create a new element for the R-to-C converters and add it to the
  end of the converter list.
*/
R_toCConverter
*R_addToCConverter(R_ToCPredicate matcher, R_ToCConverter converter,
		   R_FromCConverter reverse, void *userData, char *desc)
{
    R_toCConverter *tmp = StoCConverters;
    R_toCConverter *el;
    /* Create and populate the new entry. */
    el = (R_toCConverter *) malloc(sizeof(R_toCConverter));
    el->matcher = matcher;
    el->converter = converter;
    el->userData = userData;
    el->reverse = reverse;
    el->active = TRUE;
    if(desc)
	el->description = strdup(desc);
    el->next = (R_toCConverter*) NULL;

    /* Add the entry to the end of the list. */
    if(StoCConverters == NULL)
	StoCConverters = el;
    else {
	while(tmp->next) {
	    tmp = tmp->next;
	}
	tmp->next = el;
    }

    return(el);
}

/*
  Get the which'th element of the S-to-C converter list.
*/
R_toCConverter *R_getToCConverterByIndex(int which)
{
    R_toCConverter *tmp = StoCConverters;
    int n = 0;
    while(tmp) {
	if(n == which)
	    return(tmp);
	n++;
	tmp = tmp->next;
    }

    return((R_toCConverter*) NULL);
}


/*
   Find the first element in the S-to-C converter list
   that has the specified description string.
 */
R_toCConverter *R_getToCConverterByDescription(const char *desc)
{
    R_toCConverter *tmp = StoCConverters;
    while(tmp) {
	if(tmp->description && strcmp(tmp->description, desc) == 0)
	    return(tmp);
	tmp = tmp->next;
    }

    return((R_toCConverter*) NULL);
}

/*
 Remove the specfied element of the S-to-C converter list.
*/
void R_removeToCConverter(R_toCConverter *el)
{
    R_toCConverter *tmp = StoCConverters;
    if(el == StoCConverters)
	StoCConverters = el->next;
    else {
	while(tmp) {
	    if(tmp->next == el) {
		tmp->next = el->next;
		return;
	    }
	    tmp = tmp->next;
	}
    }
}

/*
 This does not free the user-level data.
*/
void freeCConverter(R_toCConverter *el)
{
    if(el) {
	if(el->description)
	    free(el->description);
    }
    free(el);
}


/************************************************/

/*
 converter predicate used to check whether the object is an "instance"
 of the class whose name is stored in as a string in the userData field of the
 converter element (`el'). Called as
  R_addToCConverter(R_converterMatchClass, converter, reverse, "class-name", "description...")

 This could use the R_isInstanceOf() in envir.c
 or more appropriately inherits() in util.c
*/
Rboolean
R_converterMatchClass(SEXP obj, R_CConvertInfo *inf, R_toCConverter *el)
{
    SEXP klasses = getAttrib(obj, R_ClassSymbol);
    int i, n;
    n = length(klasses);
    for(i = 0; i < n ; i++) {
	if(strcmp(translateChar(STRING_ELT(klasses,i)),
		  (char *)el->userData) == 0) {
	    return(TRUE);
	}
    }

    return(FALSE);
}

/**********  S-level entry points. ***************/

/*
  These should become .Internal() functions.
*/


/*
  Determine the number of R-to-C converters currently registered,
  including the inactive ones.
*/

static int Rf_getNumRtoCConverters()
{
    int n = 0;
    R_toCConverter *tmp = StoCConverters;

    while(tmp) {
	n++;
	tmp = tmp->next;
    }
    return(n);
}

/*
 Returns the number of registered converters to S.
*/

SEXP attribute_hidden
do_getNumRtoCConverters(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    checkArity(op, args);
    ans = allocVector(INTSXP, 1);
    INTEGER(ans)[0] = Rf_getNumRtoCConverters();
    return(ans);
}

/*
  Return a character vector describing each of the converter elements.
 */
SEXP attribute_hidden
do_getRtoCConverterDescriptions(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int n = 0, i;
    R_toCConverter *tmp = StoCConverters;
    SEXP ans;

    checkArity(op, args);

    n = Rf_getNumRtoCConverters();

    PROTECT(ans = allocVector(STRSXP, n));
    tmp = StoCConverters;
    for(i = 0; i < n ; i++) {
	if(tmp->description)
	    SET_STRING_ELT(ans, i, mkChar(tmp->description));
	tmp = tmp->next;
    }
    UNPROTECT(1);
    return(ans);
}

/*
 Return a logical vector indicating whether each converter element
 is active or inactive
*/
SEXP attribute_hidden
do_getRtoCConverterStatus(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int n = 0, i;
    R_toCConverter *tmp = StoCConverters;
    SEXP ans;

    checkArity(op, args);
    n = Rf_getNumRtoCConverters();

    PROTECT(ans = allocVector(LGLSXP, n));
    tmp = StoCConverters;
    for(i = 0; i < n ; i++) {
	LOGICAL(ans)[i] = tmp->active;
	tmp = tmp->next;
    }
    UNPROTECT(1);
    return(ans);
}



/*
 Set a particular element of the converter list to be active or inactive.
 The element is identified by index (starting at 1) or by the description
 string. This returns the original setting.

 This is also used to remove an element. The op contains a different value
 to indicate this.
*/
SEXP attribute_hidden
do_setToCConverterActiveStatus(SEXP call, SEXP op, SEXP args, SEXP env)
{
    R_toCConverter *el;
    SEXP id, status;

    checkArity(op, args);

    id = CAR(args);
    if(isString(id)) {
	el = R_getToCConverterByDescription(translateChar(STRING_ELT(id, 0)));
    } else {
	el = R_getToCConverterByIndex(asInteger(id) - 1);
    }
    if(el == NULL) {
	error(_("no R-to-C converter found corresponding to identifier"));
    }

    PROTECT(status = allocVector(LGLSXP, 1));
    if(PRIMVAL(op) == 0) {
	LOGICAL(status)[0] = el->active;
	el->active = LOGICAL(CADR(args))[0];
    } else {
	R_removeToCConverter(el);
	LOGICAL(status)[0] = TRUE;
    }
    UNPROTECT(1);
    return(status);
}


