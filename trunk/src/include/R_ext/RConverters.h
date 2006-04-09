/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2001   Robert Gentleman, Ross Ihaka
 *                             and the R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 *
 *
 * Application Routines, typically implemented in  ../appl/
 * ----------------------------------------------  ========
 */

#ifndef R_CCONVERTERS_H
#define R_CCONVERTERS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <Rinternals.h>

  /* Context information controlling how the conversion is performed, passed 
     to RObjToCPtr in dotcode.c and the different user level converters. */
  typedef struct {
    int  naok;
    int  narg;
    int  dup;
    int  Fort;
 
    char const * name ;

    SEXP classes;
  } R_CConvertInfo;


  /* Typedefs for structs defined below with future/cross-referencing. */
  typedef struct RtoCConverter R_toCConverter;
  typedef struct RFromCConvertInfo R_FromCConvertInfo;


  /* The matching routine which determines whether the converter can process the given SEXP. */
  typedef Rboolean (*R_ToCPredicate)(SEXP obj, R_CConvertInfo *info, R_toCConverter *el);

  /* The converter routine that returns the value to be passed to the C routine.
     (We may have to make the return type a union to handle the different types.) */
  typedef void*    (*R_ToCConverter)(SEXP obj, R_CConvertInfo *info, R_toCConverter *el);

  /* The reverse converter from the C argument to the R object that is returned via the .C() call. */ 
  typedef SEXP     (*R_FromCConverter)(void *value, SEXP arg, R_FromCConvertInfo *info, 
				       R_toCConverter *el);


  /* The definition of the converter element which are stored as a linked list. */
  struct RtoCConverter {
    R_ToCPredicate    matcher; /* check if converter applies to R object */
    R_ToCConverter    converter; /* convert the R object to C value */
    R_FromCConverter  reverse;   /* convert the C value back to an R object. */
    char             *description; /* user-readable string describing the converter. */
    void             *userData;    /* additional information used in (any of) the matcher,
                                      converter, and reverse routines to parameterize them. */
    Rboolean          active;  /* allows the converter to be in the list but ignored temporarily. */

    R_toCConverter   *next;    /* next element in the linked list. */
  };


  /*   Information used to convert C values to R objects at the end of do_dotCode() */
  struct RFromCConvertInfo {
    const char *functionName; /* the name of the routine being called (S's name for it). */
   
    int         argIndex; /* the pariticular argument being processed. */

    /* We provide all of the arguments and the corresponding C values. 
       This gives the full context of the call to the reverse converter */
    SEXP        allArgs;
    void      **cargs;
    int         nargs;
  };



  /* Internal mechanism for employing the converter mechanism, used in do_dotCode() in dotcode.c */
  void *Rf_convertToC(SEXP s, R_CConvertInfo *info, int *success, R_toCConverter **converter);

  /* Converter management facilities. */
  R_toCConverter *R_addToCConverter(R_ToCPredicate match, R_ToCConverter converter,
				    R_FromCConverter reverse, 
				    void *userData, char *desc);
  R_toCConverter *R_getToCConverterByIndex(int which);
  R_toCConverter *R_getToCConverterByDescription(const char *desc);
  void R_removeToCConverter(R_toCConverter *el);

  Rboolean R_converterMatchClass(SEXP obj, R_CConvertInfo *inf, R_toCConverter *el);
  void freeCConverter(R_toCConverter *el);

  /* The routines correpsonding to the .Internal() providing access to the
     management facilities of the converter list.
  */
  SEXP do_getNumRtoCConverters(SEXP call, SEXP op, SEXP args, SEXP env);
  SEXP do_getRtoCConverterDescriptions(SEXP call, SEXP op, SEXP args, SEXP env);
  SEXP do_getRtoCConverterStatus(SEXP call, SEXP op, SEXP args, SEXP env);
  SEXP do_setToCConverterActiveStatus(SEXP call, SEXP op, SEXP args, SEXP env);


#ifdef __cplusplus
}
#endif

#endif /* R_CCONVERTERS_H */
