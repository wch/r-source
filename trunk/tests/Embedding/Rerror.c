/*
 Tests the R error handling in an application
 which embeds R. Here, we generate the error by calling stop().
 We also provide an on.exit() also.
 */

#include "embeddedRCall.h"
#include "R_ext/RStartup.h"


int
main(int argc, char *argv[])
{
    SEXP e;
    int errorOccurred;
    char *localArgs[] = {"R", "--silent"};
    init_R(sizeof(localArgs)/sizeof(localArgs[0]), localArgs);

    /*
      Evaluates the two expressions:
      source("error.R")
      and then calls foo()  twice
      where foo is defined in the file error.R
    */
    PROTECT(e = lang2(install("source"), mkString("error.R")));
    R_tryEval(e, R_GlobalEnv, &errorOccurred);
    UNPROTECT(1);

    PROTECT(e = lang1(install("foo")));
    R_tryEval(e, R_GlobalEnv, &errorOccurred);
    fprintf(stderr, "Trying again (yes it will fail also!)\n");fflush(stderr);
    R_tryEval(e, R_GlobalEnv, &errorOccurred);
    UNPROTECT(1);

    end_R();

    return(0);
}
