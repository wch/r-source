#include <Rdefines.h>

int 
main(int argc, char *argv[])
{

    SEXP e, val;
    int errorOccurred;
    argv[0] = "R.bin";
    Rf_initEmbeddedR(argc, argv);

    PROTECT(e = allocVector(LANGSXP, 2));
    SETCAR(e, Rf_install("sqrt"));
    SETCAR(CDR(e), NEW_CHARACTER(1));
    val = R_tryEval(e, NULL, &errorOccurred); 
    if(errorOccurred) {
	fprintf(stderr, "Caught an error calling sqrt(). Try again with a different argument.\n");fflush(stderr);
    }
    SETCAR(CDR(e), val = NEW_INTEGER(1));
    INTEGER(val)[0] = 9;
    val = R_tryEval(e, NULL, &errorOccurred); 
    if(errorOccurred) {
	fprintf(stderr, "Caught another error calling sqrt()\n");fflush(stderr);
    } else {
	Rf_PrintValue(val);
    }
   
    UNPROTECT(1);
    
    return(0);
}

