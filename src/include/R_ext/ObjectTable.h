/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2025 The R Core Team.
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This file is part of R. R is distributed under the terms of the
 *  GNU General Public License, either Version 2, June 1991 or Version 3,
 *  June 2007. See doc/COPYRIGHTS for details of the copyright status of R.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/* 
   Not part of the API, subject to change at any time.
*/

#ifndef R_OBJECTTABLE_H
#define R_OBJECTTABLE_H

#include <Rinternals.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
  The following definitions are for callbacks to R functions and
  methods related to user-level tables.  This was implemented in a
  separate package formerly available from Omegahat and these
  declarations allow the package to interface to the internal R code.
  
  See https://developer.r-project.org/RObjectTables.pdf.
*/

typedef struct  _R_ObjectTable R_ObjectTable;

/* Do we actually need the exists() since it is never called but R
   uses get to see if the symbol is bound to anything? */
typedef Rboolean (*Rdb_exists)(const char * const name, Rboolean *canCache, R_ObjectTable *);
typedef SEXP     (*Rdb_get)(const char * const name, Rboolean *canCache, R_ObjectTable *);
typedef int      (*Rdb_remove)(const char * const name, R_ObjectTable *);
typedef SEXP     (*Rdb_assign)(const char * const name, SEXP value, R_ObjectTable *);
typedef SEXP     (*Rdb_objects)(R_ObjectTable *);
typedef Rboolean (*Rdb_canCache)(const char * const name, R_ObjectTable *);

typedef void     (*Rdb_onDetach)(R_ObjectTable *);
typedef void     (*Rdb_onAttach)(R_ObjectTable *);

struct  _R_ObjectTable{
  int       type;
  char    **cachedNames;
  Rboolean  active;

  Rdb_exists   exists;
  Rdb_get      get;
  Rdb_remove   remove;
  Rdb_assign   assign;
  Rdb_objects  objects;
  Rdb_canCache canCache;

  Rdb_onDetach onDetach;
  Rdb_onAttach onAttach;
  
  void     *privateData;
};


#ifdef __cplusplus
}
#endif

#endif /* R_OBJECTTABLE_H */
