#include "Rinternals.h"

static void doCtestExample();
static void doMVAExample();
extern int Rf_initEmbeddedR(int argc, char *argv[]);


int
main(int argc, char *argv[])
{
  Rf_initEmbeddedR(argc, argv);
  doCtestExample();
  doMVAExample();
  return(0);
}

static void
doCtestExample()
{
  SEXP e, tmp;
  int errorOccurred;

  PROTECT(e = allocVector(LANGSXP, 2));
  SETCAR(e, Rf_install("example"));
  SETCAR(CDR(e),  tmp = allocVector(STRSXP, 1));
  SET_STRING_ELT(tmp, 0, mkChar("t.test"));
   
  Test_tryEval(e, &errorOccurred);

  UNPROTECT(1);
}

static void
doMVAExample()
{
  SEXP e, tmp;
 
  PROTECT(e = allocVector(LANGSXP, 2));
  SETCAR(e, Rf_install("library"));
  SETCAR(CDR(e), tmp = allocVector(STRSXP, 1));
  SET_STRING_ELT(tmp, 0, mkChar("mva"));
 
  Test_tryEval(e, NULL);
  UNPROTECT(1);

  PROTECT(e = allocVector(LANGSXP, 2));
  SETCAR(e, Rf_install("example"));
  SETCAR(CDR(e),  tmp = allocVector(STRSXP, 1));
  SET_STRING_ELT(tmp, 0, mkChar("dist"));
   
  Rf_PrintValue(e);

  Test_tryEval(e, NULL);
  UNPROTECT(1);
}
