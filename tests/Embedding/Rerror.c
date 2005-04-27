/*
 Tests the R error handling in an application
 which embeds R. Here, we generate the error by calling stop().
 We also provide an on.exit() also.
 */

#include <R.h>
#include <Rdefines.h>

#include "Startup.h"

#include "embeddedRCall.h"


int
main(int argc, char *argv[])
{
  SEXP fun, e, arg;
  int errorOccurred;
  char *localArgs[] = {"R", "--silent"};
  init_R(sizeof(localArgs)/sizeof(localArgs[0]), localArgs);

  /*
     Evaluates the two expressions:
       source("error.R")
     and then calls foo()  twice
     where foo is defined in the file error.R
   */
  PROTECT(fun = Rf_findFun(Rf_install("source"),  R_GlobalEnv));  
  PROTECT(e = allocVector(LANGSXP, 2));
  PROTECT(arg = NEW_CHARACTER(1));
  SET_STRING_ELT(arg, 0, COPY_TO_USER_STRING("error.R"));
  SETCAR(e, fun);
  SETCAR(CDR(e), arg);
  
  Test_tryEval(e, &errorOccurred);

  UNPROTECT(2);

    fun = Rf_findFun(Rf_install("foo"),  R_GlobalEnv);
    PROTECT(fun);
    PROTECT(arg = NEW_INTEGER(10));
    e = allocVector(LANGSXP, 1);
    PROTECT(e);
    SETCAR(e, fun);

    Test_tryEval(e, &errorOccurred);

    fprintf(stderr, "Trying again (yes it will fail also!)\n");fflush(stderr);

    Test_tryEval(e, &errorOccurred);
    UNPROTECT(2);

    R_CleanUp(SA_NOSAVE, 0, FALSE);

  return(0);
}

