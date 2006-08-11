#include "embeddedRCall.h"
#include "R_ext/RStartup.h"

static void doSplinesExample();
extern int Rf_initEmbeddedR(int argc, char *argv[]);

int
main(int argc, char *argv[])
{
    Rf_initEmbeddedR(argc, argv);
    doSplinesExample();
    end_Rmainloop();
    return(0);
}

static void
doSplinesExample()
{
    SEXP e;
    int errorOccurred;

    PROTECT(e = lang2(install("library"), mkString("splines")));
    R_tryEval(e, R_GlobalEnv, NULL);
    UNPROTECT(1);

    PROTECT(e = lang2(install("options"), ScalarLogical(0)));
    SET_TAG(CDR(e), install("example.ask"));
    PrintValue(e);
    R_tryEval(e, R_GlobalEnv, NULL);
    UNPROTECT(1);

    PROTECT(e = lang2(install("example"), mkString("ns")));
    R_tryEval(e, R_GlobalEnv, &errorOccurred);
    UNPROTECT(1);
}
