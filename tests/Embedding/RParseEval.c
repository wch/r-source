#include "embeddedRCall.h"
#include <R_ext/Parse.h>

int
main(int argc, char *argv[])
{
    SEXP e, tmp;
    int hadError;
    ParseStatus status;

    init_R(argc, argv);

    PROTECT(tmp = mkString("{plot(1:10, pch=\"+\"); print(1:10)}"));
    PROTECT(e = R_ParseVector(tmp, 1, &status, R_NilValue));
    PrintValue(e);
    R_tryEval(VECTOR_ELT(e,0), R_GlobalEnv, &hadError);
    UNPROTECT(2);

    end_R();
    return(0);
}
