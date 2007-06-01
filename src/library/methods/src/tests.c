/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002-2005   The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  A copy of the GNU General Public License is available via WWW at
 *  http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 *  writing to the Free Software Foundation, Inc., 51 Franklin Street
 *  Fifth Floor, Boston, MA 02110-1301  USA.
 */

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include "methods.h"

SEXP R_methods_test_MAKE_CLASS(SEXP className)
{
  const char *class;
  class = CHAR(asChar(className));
  return MAKE_CLASS(class);
}

SEXP R_methods_test_NEW(SEXP className)
{
  SEXP clDef;
  const char *class;
  class = CHAR(asChar(className));
  clDef = MAKE_CLASS(class);
  return NEW_OBJECT(clDef);
}


