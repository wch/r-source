
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include "methods.h"

SEXP R_methods_test_MAKE_CLASS(SEXP className)
{
  char *class;
  class = CHAR(asChar(className));
  return MAKE_CLASS(class);
}

SEXP R_methods_test_NEW(SEXP className)
{
  SEXP clDef;
  char *class;
  class = CHAR(asChar(className));
  clDef = MAKE_CLASS(class);
  return NEW_OBJECT(clDef);
}


