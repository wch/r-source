#include <Rinternals.h>
#include <Rembedded.h>

int 
main(int argc, char *argv[])
{
    SEXP e, val;
    int errorOccurred;
    argv[0] = "R";
    Rf_initEmbeddedR(argc, argv);

    PROTECT(e = lang2(install("sqrt"), mkString("")));
    val = R_tryEval(e, NULL, &errorOccurred); 
    if(errorOccurred) {
	fprintf(stderr, "Caught an error calling sqrt(). Try again with a different argument.\n");fflush(stderr);
    }
    SETCAR(CDR(e), ScalarInteger(9));
    val = R_tryEval(e, NULL, &errorOccurred); 
    if(errorOccurred) {
	fprintf(stderr, "Caught another error calling sqrt()\n");fflush(stderr);
    } else {
	Rf_PrintValue(val);
    }
   
    UNPROTECT(1);
    
    return(0);
}

