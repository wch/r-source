#include <Rinternals.h>
#include "embeddedRCall.h"

static void doSplinesExample();
extern int Rf_initEmbeddedR(int argc, char *argv[]);


int
main(int argc, char *argv[])
{
  Rf_initEmbeddedR(argc, argv);
  doSplinesExample();
  return(0);
}

static void
doSplinesExample()
{
  SEXP e;
  int errorOccurred;

  PROTECT(e = allocVector(LANGSXP, 2));
  SETCAR(e, Rf_install("library"));
  SETCAR(CDR(e), mkString("splines"));

  R_tryEval(e, R_GlobalEnv, NULL);
  UNPROTECT(1);

  PROTECT(e = allocVector(LANGSXP, 2));
  SETCAR(e, Rf_install("example"));
  SETCAR(CDR(e), mkString("ns"));
   
  R_tryEval(e, R_GlobalEnv, &errorOccurred);

  UNPROTECT(1);
}
